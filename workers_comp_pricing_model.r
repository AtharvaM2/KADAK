library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(pscl)
library(MASS)
library(ismev)

set.seed(123)

# =============================
# Configuration
# =============================
expense_ratio <- 0.3
profit_ratio  <- 0.07
risk_ratio    <- 0.05
n_sims        <- 200000
exposure_assumption <- 1
vuong_sample_n <- 50000
tail_threshold <- 50000

# =============================
# Helpers
# =============================
clean_text_levels <- function(df) {
  df %>%
    mutate(across(
      where(~ is.character(.x) || is.factor(.x)),
      ~ str_squish(str_remove(as.character(.x), "_.*$"))
    ))
}

get_beta <- function(coefs, var, level = NULL) {
  if (is.null(level)) {
    if (var %in% names(coefs)) return(unname(coefs[[var]]))
    return(0)
  }

  nm <- paste0(var, level)
  if (nm %in% names(coefs)) return(unname(coefs[[nm]]))
  0
}

representative_value <- function(x) {
  x <- x[!is.na(x)]
  u <- sort(unique(x))
  if (length(u) <= 2 && all(u %in% c(0, 1))) return(1)
  mean(x)
}

normalize_occ <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

safe_weighted_mean <- function(x, w) {
  if (all(is.na(x)) || all(is.na(w)) || sum(w, na.rm = TRUE) <= 0) {
    return(NA_real_)
  }
  weighted.mean(x, w, na.rm = TRUE)
}

# =============================
# 1) Load and clean claims data
# =============================
worker_freq_raw <- read_excel("srcsc-2026-claims-workers-comp.xlsx", sheet = 1)
worker_sev_raw  <- read_excel("srcsc-2026-claims-workers-comp.xlsx", sheet = 2)

worker_freq <- worker_freq_raw %>%
  clean_text_levels() %>%
  filter(!is.na(claim_count), !is.na(exposure), exposure > 0) %>%
  mutate(
    claim_count = round(claim_count),
    occupation = factor(occupation),
    solar_system = factor(solar_system),
    employment_type = factor(employment_type)
  )

worker_sev <- worker_sev_raw %>%
  clean_text_levels() %>%
  filter(!is.na(claim_amount), claim_amount > 0) %>%
  mutate(
    occupation = factor(occupation, levels = levels(worker_freq$occupation)),
    solar_system = factor(solar_system, levels = levels(worker_freq$solar_system)),
    employment_type = factor(employment_type, levels = levels(worker_freq$employment_type))
  )

freq_fields <- c(
  "occupation", "solar_system", "employment_type",
  "accident_history_flag", "psych_stress_index",
  "safety_training_index", "exposure", "claim_count"
)

sev_fields <- c(
  "occupation", "solar_system", "employment_type",
  "accident_history_flag", "psych_stress_index",
  "supervision_level", "protective_gear_quality", "base_salary", "claim_length", "claim_amount"
)

worker_freq <- worker_freq %>% filter(!if_any(all_of(freq_fields), is.na))
worker_sev  <- worker_sev %>% filter(!if_any(all_of(sev_fields), is.na))

historical_total_loss <- sum(worker_sev$claim_amount, na.rm = TRUE)
historical_severity_mean <- mean(worker_sev$claim_amount, na.rm = TRUE)
historical_zero_rate <- mean(worker_freq$claim_count == 0)

# =============================
# 2) Frequency models: ZINB vs NB
# =============================
freq_formula_count <- claim_count ~ occupation + accident_history_flag + psych_stress_index +
  safety_training_index + solar_system

zinb_model <- zeroinfl(
  claim_count ~ occupation + accident_history_flag + psych_stress_index +
    safety_training_index + solar_system |
    employment_type,
  data = worker_freq,
  dist = "negbin",
  offset = log(exposure)
)

nb_model <- glm.nb(
  claim_count ~ occupation + accident_history_flag + psych_stress_index +
    safety_training_index + solar_system + employment_type + offset(log(exposure)),
  data = worker_freq
)

# Zero-probability diagnostics
zinb_pi <- predict(zinb_model, type = "zero")
zinb_mu <- predict(zinb_model, type = "count")
zinb_zero_rate_pred <- mean(zinb_pi + (1 - zinb_pi) * dnbinom(0, size = zinb_model$theta, mu = zinb_mu))

nb_mu <- fitted(nb_model)
nb_zero_rate_pred <- mean(dnbinom(0, size = nb_model$theta, mu = nb_mu))

# Vuong test on a sample for stability/runtime
vuong_z <- NA_real_
vuong_p <- NA_real_
vuong_note <- "Vuong not available"

freq_vuong_data <- worker_freq %>% slice_sample(n = min(nrow(worker_freq), vuong_sample_n))

zinb_vuong <- tryCatch(
  zeroinfl(
    claim_count ~ occupation + accident_history_flag + psych_stress_index +
      safety_training_index + solar_system |
      employment_type,
    data = freq_vuong_data,
    dist = "negbin",
    offset = log(exposure)
  ),
  error = function(e) NULL
)

nb_vuong <- tryCatch(
  glm.nb(
    claim_count ~ occupation + accident_history_flag + psych_stress_index +
      safety_training_index + solar_system + employment_type + offset(log(exposure)),
    data = freq_vuong_data
  ),
  error = function(e) NULL
)

if (!is.null(zinb_vuong) && !is.null(nb_vuong)) {
  vu <- tryCatch(vuong(zinb_vuong, nb_vuong), error = function(e) NULL)
  if (!is.null(vu)) {
    if (!is.null(vu$statistic) && "Raw" %in% names(vu$statistic)) vuong_z <- unname(vu$statistic["Raw"])
    if (!is.null(vu$p) && "Raw" %in% names(vu$p)) vuong_p <- unname(vu$p["Raw"])
    vuong_note <- "Vuong computed on sample"
  }
}

zinb_aic <- AIC(zinb_model)
nb_aic <- AIC(nb_model)
zinb_bic <- BIC(zinb_model)
nb_bic <- BIC(nb_model)

zinb_zero_error <- abs(zinb_zero_rate_pred - historical_zero_rate)
nb_zero_error <- abs(nb_zero_rate_pred - historical_zero_rate)

selected_frequency <- "ZINB"
if (!is.na(vuong_z)) {
  if (vuong_z < -1.96) selected_frequency <- "NB"
  if (vuong_z > 1.96) selected_frequency <- "ZINB"
}

if (is.na(vuong_z) || abs(vuong_z) <= 1.96) {
  if ((nb_aic + 2 < zinb_aic) && (nb_zero_error <= zinb_zero_error * 1.10)) {
    selected_frequency <- "NB"
  }
}

if (selected_frequency == "ZINB") {
  count_coef <- zinb_model$coefficients$count
  zero_coef  <- zinb_model$coefficients$zero
  count_intercept <- unname(count_coef[1])
  zero_intercept  <- unname(zero_coef[1])
  base_count_mean <- exp(count_intercept)
  base_nonzero_prob <- 1 - plogis(zero_intercept)
  freq_theta <- zinb_model$theta
} else {
  count_coef <- coef(nb_model)
  zero_coef  <- c("(Intercept)" = -Inf)
  count_intercept <- unname(count_coef[1])
  zero_intercept <- NA_real_
  base_count_mean <- exp(count_intercept)
  base_nonzero_prob <- 1
  freq_theta <- nb_model$theta
}

baseline_frequency <- base_count_mean * base_nonzero_prob * exposure_assumption

frequency_model_diagnostics <- tibble(
  metric = c(
    "historical_zero_rate",
    "zinb_predicted_zero_rate",
    "nb_predicted_zero_rate",
    "zinb_aic",
    "nb_aic",
    "zinb_bic",
    "nb_bic",
    "vuong_raw_z",
    "vuong_raw_p"
  ),
  value = c(
    historical_zero_rate,
    zinb_zero_rate_pred,
    nb_zero_rate_pred,
    zinb_aic,
    nb_aic,
    zinb_bic,
    nb_bic,
    vuong_z,
    vuong_p
  )
) %>%
  bind_rows(tibble(metric = "selected_frequency_model", value = NA_real_)) %>%
  mutate(
    value_text = case_when(
      metric == "selected_frequency_model" ~ selected_frequency,
      metric == "vuong_raw_z" ~ vuong_note,
      TRUE ~ NA_character_
    )
  )

# =============================
# 3) Severity models: Gamma vs LogNormal + Spliced Tail
# =============================
severity_formula <- claim_amount ~ occupation + solar_system + employment_type +
  accident_history_flag + psych_stress_index + supervision_level +
  protective_gear_quality + base_salary + claim_length

gamma_sev <- glm(
  severity_formula,
  family = Gamma(link = "log"),
  data = worker_sev
)

lognorm_sev <- glm(
  log(claim_amount) ~ occupation + solar_system + employment_type +
    accident_history_flag + psych_stress_index + supervision_level +
    protective_gear_quality + base_salary + claim_length,
  family = gaussian(link = "identity"),
  data = worker_sev
)

aic_gamma <- AIC(gamma_sev)
aic_lognorm <- AIC(lognorm_sev)
selected_severity <- ifelse(aic_lognorm + 2 < aic_gamma, "LOGNORMAL", "GAMMA")

if (selected_severity == "LOGNORMAL") {
  sev_model <- lognorm_sev
  sev_coef <- coef(lognorm_sev)
  # For glm(..., gaussian), dispersion is the residual variance on log-scale.
  sev_sigma2 <- summary(lognorm_sev)$dispersion
  sev_intercept <- unname(sev_coef[1])
  baseline_severity_raw <- exp(sev_intercept + 0.5 * sev_sigma2)
} else {
  sev_model <- gamma_sev
  sev_coef <- coef(gamma_sev)
  sev_sigma2 <- NA_real_
  sev_intercept <- unname(sev_coef[1])
  baseline_severity_raw <- exp(sev_intercept)
}

# Spliced severity: Gamma bulk (<= threshold) + GPD tail (> threshold)
sev_values <- worker_sev$claim_amount
bulk_values <- sev_values[sev_values <= tail_threshold]
excess_values <- sev_values[sev_values > tail_threshold] - tail_threshold

p_tail <- length(excess_values) / length(sev_values)

bulk_mean <- mean(bulk_values)
bulk_var <- var(bulk_values)
bulk_shape <- bulk_mean^2 / pmax(bulk_var, 1e-6)
bulk_scale <- bulk_var / pmax(bulk_mean, 1e-6)

if (length(excess_values) >= 30) {
  gpd_fit <- gpd.fit(excess_values, threshold = 0, show = FALSE)
  gpd_scale <- pmax(unname(gpd_fit$mle[1]), 1e-6)
  gpd_shape <- unname(gpd_fit$mle[2])
} else {
  gpd_scale <- pmax(mean(excess_values), 1)
  gpd_shape <- 0
}

# Keep finite-variance tail for stable TVaR simulation
if (!is.finite(gpd_shape)) gpd_shape <- 0
if (gpd_shape >= 0.49) gpd_shape <- 0.49
if (gpd_shape <= -0.4) gpd_shape <- -0.4

mean_excess <- if (abs(gpd_shape) < 1e-8) {
  gpd_scale
} else {
  gpd_scale / (1 - gpd_shape)
}

var_excess <- if (gpd_shape < 0.49) {
  gpd_scale^2 / ((1 - gpd_shape)^2 * (1 - 2 * gpd_shape))
} else {
  (10 * mean_excess)^2
}

shape_excess_gamma <- mean_excess^2 / pmax(var_excess, 1e-6)
scale_excess_gamma <- var_excess / pmax(mean_excess, 1e-6)

splice_base_mean <- (1 - p_tail) * (bulk_shape * bulk_scale) + p_tail * (tail_threshold + mean_excess)

severity_model_diagnostics <- tibble(
  metric = c(
    "historical_severity_mean",
    "historical_severity_max",
    "gamma_aic",
    "lognormal_aic",
    "tail_threshold",
    "tail_probability",
    "bulk_gamma_shape",
    "bulk_gamma_scale",
    "gpd_scale",
    "gpd_shape",
    "splice_base_mean"
  ),
  value = c(
    historical_severity_mean,
    max(sev_values, na.rm = TRUE),
    aic_gamma,
    aic_lognorm,
    tail_threshold,
    p_tail,
    bulk_shape,
    bulk_scale,
    gpd_scale,
    gpd_shape,
    splice_base_mean
  )
) %>%
  bind_rows(tibble(metric = "selected_severity_model", value = NA_real_)) %>%
  mutate(
    value_text = case_when(
      metric == "selected_severity_model" ~ selected_severity,
      TRUE ~ NA_character_
    )
  )

# =============================
# 4) Loading factors table
# =============================
label_map <- c(
  occupation = "Occupation",
  solar_system = "Solar system",
  employment_type = "Employment type",
  accident_history_flag = "Accident history flag",
  psych_stress_index = "Psychological stress index",
  safety_training_index = "Safety training index",
  supervision_level = "Supervision level",
  protective_gear_quality = "Protective gear quality",
  base_salary = "Base salary",
  claim_length = "Claim length",
  exposure = "Exposure"
)
pretty_label <- function(v) ifelse(v %in% names(label_map), label_map[[v]], v)

make_categorical_rows <- function(var, levels_vec, selected_frequency_model) {
  base_level <- levels_vec[1]

  bind_rows(lapply(levels_vec, function(lv) {
    count_beta <- get_beta(count_coef, var, lv)
    sev_beta <- get_beta(sev_coef, var, lv)

    freq_loading <- exp(count_beta)

    if (var == "employment_type" && selected_frequency_model == "ZINB") {
      beta_base <- get_beta(zero_coef, var, base_level)
      beta_lv <- get_beta(zero_coef, var, lv)
      pi_base <- plogis(zero_intercept + beta_base)
      pi_lv <- plogis(zero_intercept + beta_lv)
      freq_loading <- freq_loading * ((1 - pi_lv) / (1 - pi_base))
    }

    sev_loading <- exp(sev_beta)

    tibble(
      predictor = pretty_label(var),
      predictor_key = var,
      predictor_type = "categorical",
      level = lv,
      is_baseline = lv == base_level,
      representative_value = NA_real_,
      frequency_loading = freq_loading,
      severity_loading = sev_loading,
      premium_loading = freq_loading * sev_loading,
      contribution_label = paste0(pretty_label(var), ": ", lv)
    )
  }))
}

make_numeric_row <- function(var, df, use_offset = FALSE) {
  rep_val <- representative_value(df[[var]])

  # Salary is applied on scaled units for pricing stability
  effective_val <- rep_val
  if (var == "base_salary") effective_val <- rep_val / 10000

  count_beta <- get_beta(count_coef, var)
  sev_beta <- get_beta(sev_coef, var)

  freq_loading <- exp(count_beta * effective_val)
  if (use_offset && var == "exposure") freq_loading <- rep_val

  sev_loading <- exp(sev_beta * effective_val)

  tibble(
    predictor = pretty_label(var),
    predictor_key = var,
    predictor_type = "numeric",
    level = "representative_value",
    is_baseline = FALSE,
    representative_value = rep_val,
    frequency_loading = freq_loading,
    severity_loading = sev_loading,
    premium_loading = freq_loading * sev_loading,
    contribution_label = paste0(pretty_label(var), " at ", round(rep_val, 4))
  )
}

predictor_loading_table <- bind_rows(
  make_categorical_rows("occupation", levels(worker_freq$occupation), selected_frequency),
  make_categorical_rows("solar_system", levels(worker_freq$solar_system), selected_frequency),
  make_categorical_rows("employment_type", levels(worker_freq$employment_type), selected_frequency),
  make_numeric_row("accident_history_flag", worker_freq),
  make_numeric_row("psych_stress_index", worker_freq),
  make_numeric_row("safety_training_index", worker_freq),
  make_numeric_row("supervision_level", worker_sev),
  make_numeric_row("protective_gear_quality", worker_sev),
  make_numeric_row("base_salary", worker_sev),
  make_numeric_row("claim_length", worker_sev),
  make_numeric_row("exposure", worker_freq, use_offset = TRUE)
) %>%
  mutate(
    frequency_loading = round(frequency_loading, 6),
    severity_loading = round(severity_loading, 6),
    premium_loading = round(premium_loading, 6),
    representative_value = round(representative_value, 6)
  ) %>%
  arrange(predictor, predictor_type, level)

# =============================
# 5) Personnel data and occupation-level loadings
# =============================
personnel_raw <- read_excel(
  "srcsc-2026-cosmic-quarry-personnel.xlsx",
  sheet = "Personnel",
  col_names = FALSE
)

names(personnel_raw) <- c("name", "employees", "full_time", "contract", "avg_salary", "avg_age")

personnel_clean <- personnel_raw %>%
  transmute(
    name = str_squish(as.character(name)),
    employees = suppressWarnings(as.numeric(employees)),
    full_time = suppressWarnings(as.numeric(full_time)),
    contract = suppressWarnings(as.numeric(contract)),
    avg_salary = suppressWarnings(as.numeric(avg_salary)),
    avg_age = suppressWarnings(as.numeric(avg_age))
  )

ignore_rows <- c(
  "Cosmic Quarry Mining Corporation Personnel Summary",
  "Number of Employees",
  "Full-Time Employees",
  "Contract Employees",
  "Average Annualized Salary (Đ)",
  "Average Age"
)

current_department <- NA_character_
out_rows <- vector("list", nrow(personnel_clean))
out_idx <- 0

for (i in seq_len(nrow(personnel_clean))) {
  nm <- personnel_clean$name[i]
  if (is.na(nm) || nm == "" || nm %in% ignore_rows) next

  if (is.na(personnel_clean$employees[i])) {
    current_department <- nm
    next
  }

  out_idx <- out_idx + 1
  out_rows[[out_idx]] <- tibble(
    department = current_department,
    occupation_personnel = nm,
    employees = coalesce(personnel_clean$employees[i], 0),
    full_time = coalesce(personnel_clean$full_time[i], 0),
    contract = coalesce(personnel_clean$contract[i], 0),
    avg_salary = personnel_clean$avg_salary[i],
    avg_age = personnel_clean$avg_age[i]
  )
}

personnel_occ <- bind_rows(out_rows[seq_len(out_idx)]) %>%
  filter(!is.na(occupation_personnel), employees > 0)

occupation_loading_lookup <- predictor_loading_table %>%
  filter(predictor_key == "occupation") %>%
  transmute(
    occupation_model = level,
    occupation_norm = normalize_occ(level),
    occ_freq_loading = frequency_loading,
    occ_sev_loading = severity_loading,
    occ_premium_loading = premium_loading
  )

occ_alias <- tibble(
  occupation_norm = c("drilling operators", "engineers", "maintenance"),
  occupation_norm_model = c("drill operator", "engineer", "maintenance staff")
)

personnel_occ <- personnel_occ %>%
  mutate(occupation_norm = normalize_occ(occupation_personnel)) %>%
  left_join(occ_alias, by = "occupation_norm") %>%
  mutate(occupation_join_key = coalesce(occupation_norm_model, occupation_norm))

portfolio_occ <- personnel_occ %>%
  left_join(occupation_loading_lookup, by = c("occupation_join_key" = "occupation_norm")) %>%
  mutate(
    loading_source = if_else(!is.na(occ_premium_loading), "model", NA_character_),
    occupation_model = coalesce(occupation_model, NA_character_)
  )

department_fallback <- portfolio_occ %>%
  filter(loading_source == "model") %>%
  group_by(department) %>%
  summarise(
    dept_occ_freq_loading = safe_weighted_mean(occ_freq_loading, employees),
    dept_occ_sev_loading = safe_weighted_mean(occ_sev_loading, employees),
    dept_occ_premium_loading = safe_weighted_mean(occ_premium_loading, employees),
    .groups = "drop"
  )

overall_fallback <- occupation_loading_lookup %>%
  summarise(
    overall_occ_freq_loading = mean(occ_freq_loading, na.rm = TRUE),
    overall_occ_sev_loading = mean(occ_sev_loading, na.rm = TRUE),
    overall_occ_premium_loading = mean(occ_premium_loading, na.rm = TRUE)
  )

portfolio_occ <- portfolio_occ %>%
  left_join(department_fallback, by = "department") %>%
  mutate(
    occ_freq_loading = case_when(
      !is.na(occ_freq_loading) ~ occ_freq_loading,
      !is.na(dept_occ_freq_loading) ~ dept_occ_freq_loading,
      TRUE ~ overall_fallback$overall_occ_freq_loading
    ),
    occ_sev_loading = case_when(
      !is.na(occ_sev_loading) ~ occ_sev_loading,
      !is.na(dept_occ_sev_loading) ~ dept_occ_sev_loading,
      TRUE ~ overall_fallback$overall_occ_sev_loading
    ),
    occ_premium_loading = case_when(
      !is.na(occ_premium_loading) ~ occ_premium_loading,
      !is.na(dept_occ_premium_loading) ~ dept_occ_premium_loading,
      TRUE ~ overall_fallback$overall_occ_premium_loading
    ),
    loading_source = case_when(
      loading_source == "model" ~ "model",
      is.na(loading_source) & !is.na(dept_occ_premium_loading) ~ "department_average",
      TRUE ~ "overall_average"
    )
  )

# Employment-type loadings
emp_loading_lookup <- predictor_loading_table %>%
  filter(predictor_key == "employment_type") %>%
  transmute(employment_type = level, emp_premium_loading = premium_loading, emp_freq_loading = frequency_loading, emp_sev_loading = severity_loading)

get_emp_loading <- function(level_name, col_name) {
  v <- emp_loading_lookup %>% filter(str_to_lower(employment_type) == str_to_lower(level_name)) %>% pull(.data[[col_name]])
  if (length(v) == 0) return(1)
  v[1]
}

emp_contract_prem <- get_emp_loading("contract", "emp_premium_loading")
emp_fulltime_prem <- get_emp_loading("full-time", "emp_premium_loading")
emp_contract_freq <- get_emp_loading("contract", "emp_freq_loading")
emp_fulltime_freq <- get_emp_loading("full-time", "emp_freq_loading")
emp_contract_sev <- get_emp_loading("contract", "emp_sev_loading")
emp_fulltime_sev <- get_emp_loading("full-time", "emp_sev_loading")

# Common representative loadings
num_row <- function(key) {
  predictor_loading_table %>% filter(predictor_key == key) %>% slice(1)
}

common_freq_loading <-
  num_row("accident_history_flag")$frequency_loading *
  num_row("psych_stress_index")$frequency_loading *
  num_row("safety_training_index")$frequency_loading

common_sev_loading <-
  num_row("accident_history_flag")$severity_loading *
  num_row("psych_stress_index")$severity_loading *
  num_row("supervision_level")$severity_loading *
  num_row("protective_gear_quality")$severity_loading *
  num_row("claim_length")$severity_loading

base_salary_beta <- get_beta(sev_coef, "base_salary")

portfolio_occ <- portfolio_occ %>%
  mutate(
    ft_share = if_else(employees > 0, full_time / employees, 0),
    ct_share = if_else(employees > 0, contract / employees, 0),
    share_total = pmax(ft_share + ct_share, 1e-9),
    ft_share = ft_share / share_total,
    ct_share = ct_share / share_total,

    employment_mix_premium_loading = ft_share * emp_fulltime_prem + ct_share * emp_contract_prem,
    employment_mix_sev_loading = ft_share * emp_fulltime_sev + ct_share * emp_contract_sev,
    employment_mix_freq_loading = ft_share * emp_fulltime_freq + ct_share * emp_contract_freq,

    salary_for_loading = coalesce(avg_salary, median(worker_sev$base_salary, na.rm = TRUE)) / 10000,
    salary_loading = exp(base_salary_beta * salary_for_loading),
    other_loading = common_freq_loading * common_sev_loading
  )

total_employees <- sum(portfolio_occ$employees, na.rm = TRUE)

# =============================
# 6) Aggregate simulation (raw) with heavy-tail splice
# =============================
if (selected_frequency == "ZINB") {
  q_contract <- 1 - plogis(zero_intercept + get_beta(zero_coef, "employment_type", "Contract"))
  q_fulltime <- 1 - plogis(zero_intercept + get_beta(zero_coef, "employment_type", "Full-time"))

  if (!is.finite(q_contract) || q_contract <= 0) q_contract <- base_nonzero_prob
  if (!is.finite(q_fulltime) || q_fulltime <= 0) q_fulltime <- base_nonzero_prob
} else {
  q_contract <- 1
  q_fulltime <- 1
}

total_loss_raw <- numeric(n_sims)

theta_nb <- freq_theta

for (i in seq_len(nrow(portfolio_occ))) {
  row <- portfolio_occ[i, ]
  n_emp <- as.integer(row$employees)
  if (is.na(n_emp) || n_emp <= 0) next

  # Count-part mean per active employee
  lambda_count_emp <- base_count_mean * row$occ_freq_loading * common_freq_loading * exposure_assumption

  if (selected_frequency == "NB") {
    # NB has employment in count part directly
    lambda_count_emp <- lambda_count_emp * row$employment_mix_freq_loading
  }

  # Preserve structural-zero simulation only when using ZINB
  q_mix <- if (selected_frequency == "ZINB") {
    row$ft_share * q_fulltime + row$ct_share * q_contract
  } else {
    1
  }

  active_n <- rbinom(n_sims, size = n_emp, prob = pmin(pmax(q_mix, 0), 1))

  claims_occ <- integer(n_sims)
  idx_active <- active_n > 0
  if (any(idx_active)) {
    claims_occ[idx_active] <- rnbinom(
      sum(idx_active),
      size = pmax(theta_nb * active_n[idx_active], 1e-9),
      mu = pmax(lambda_count_emp * active_n[idx_active], 0)
    )
  }

  # Occupation-specific target mean severity from base GLM loading structure
  sev_mean_occ_raw <- baseline_severity_raw * row$occ_sev_loading * common_sev_loading * row$employment_mix_sev_loading * row$salary_loading
  sev_scale_factor <- sev_mean_occ_raw / pmax(splice_base_mean, 1e-9)

  tail_n <- rbinom(n_sims, size = claims_occ, prob = p_tail)
  bulk_n <- claims_occ - tail_n

  bulk_loss <- numeric(n_sims)
  idx_bulk <- bulk_n > 0
  if (any(idx_bulk)) {
    bulk_loss[idx_bulk] <- rgamma(
      sum(idx_bulk),
      shape = pmax(bulk_shape * bulk_n[idx_bulk], 1e-9),
      scale = bulk_scale
    )
  }

  tail_excess_loss <- numeric(n_sims)
  idx_tail <- tail_n > 0
  if (any(idx_tail)) {
    tail_excess_loss[idx_tail] <- rgamma(
      sum(idx_tail),
      shape = pmax(shape_excess_gamma * tail_n[idx_tail], 1e-9),
      scale = scale_excess_gamma
    )
  }

  tail_loss <- tail_threshold * tail_n + tail_excess_loss
  occ_loss_raw <- sev_scale_factor * (bulk_loss + tail_loss)

  total_loss_raw <- total_loss_raw + occ_loss_raw
}

# Re-anchor severity so simulated mean matches historical total loss
sim_mean_raw <- mean(total_loss_raw)
severity_anchor_factor <- ifelse(sim_mean_raw > 0, historical_total_loss / sim_mean_raw, 1)
severity_intercept_shift <- log(severity_anchor_factor)

total_loss <- total_loss_raw * severity_anchor_factor

# =============================
# 7) Final baseline premium (anchored)
# =============================
baseline_severity <- baseline_severity_raw * severity_anchor_factor
baseline_pure_premium <- baseline_frequency * baseline_severity
risk_margin <- baseline_pure_premium * risk_ratio
baseline_technical_premium <- (baseline_pure_premium + risk_margin) / (1 - expense_ratio - profit_ratio)
tech_loading_factor <- baseline_technical_premium / baseline_pure_premium

# Add adjusted premium columns now that baseline is anchored
predictor_loading_table <- predictor_loading_table %>%
  mutate(
    adjusted_pure_premium = round(baseline_pure_premium * premium_loading, 6),
    adjusted_technical_premium = round(baseline_technical_premium * premium_loading, 6)
  )

baseline_premium_summary <- tibble(
  metric = c(
    "historical_total_loss",
    "historical_severity_mean",
    "baseline_count_mean",
    "baseline_nonzero_probability",
    "baseline_frequency_per_exposure",
    "baseline_severity_raw",
    "severity_anchor_factor",
    "severity_intercept_shift",
    "baseline_severity_anchored",
    "baseline_pure_premium",
    "risk_margin",
    "expense_ratio",
    "profit_ratio",
    "risk_ratio",
    "baseline_technical_premium"
  ),
  value = c(
    historical_total_loss,
    historical_severity_mean,
    base_count_mean,
    base_nonzero_prob,
    baseline_frequency,
    baseline_severity_raw,
    severity_anchor_factor,
    severity_intercept_shift,
    baseline_severity,
    baseline_pure_premium,
    risk_margin,
    expense_ratio,
    profit_ratio,
    risk_ratio,
    baseline_technical_premium
  )
)

# =============================
# 8) Portfolio pricing by occupation (anchored premiums)
# =============================
portfolio_premium_by_occupation <- portfolio_occ %>%
  mutate(
    pure_premium_per_employee = baseline_pure_premium * occ_premium_loading * employment_mix_premium_loading * salary_loading * other_loading,
    premium_per_employee = baseline_technical_premium * occ_premium_loading * employment_mix_premium_loading * salary_loading * other_loading,
    total_pure_premium = pure_premium_per_employee * employees,
    total_premium = premium_per_employee * employees
  ) %>%
  dplyr::select(
    department,
    occupation = occupation_personnel,
    occupation_model,
    loading_source,
    employees,
    premium_per_employee,
    total_premium,
    pure_premium_per_employee,
    total_pure_premium,
    occ_premium_loading,
    employment_mix_premium_loading,
    salary_loading,
    other_loading
  ) %>%
  arrange(desc(total_premium))

total_portfolio_pure <- sum(portfolio_premium_by_occupation$total_pure_premium, na.rm = TRUE)
total_portfolio_premium <- sum(portfolio_premium_by_occupation$total_premium, na.rm = TRUE)

# =============================
# 9) Risk metrics + validation
# =============================
var95 <- unname(quantile(total_loss, 0.95))
var99 <- unname(quantile(total_loss, 0.99))
tvar99 <- mean(total_loss[total_loss >= var99])

loss_validation <- tibble(
  simulated_mean_loss = mean(total_loss),
  model_expected_loss = total_portfolio_pure,
  difference = mean(total_loss) - total_portfolio_pure,
  relative_error_vs_historical = (mean(total_loss) - historical_total_loss) / historical_total_loss
)

tail_gap_99 <- max(tvar99 - mean(total_loss), 0)
current_risk_margin_amount <- risk_ratio * total_portfolio_pure
risk_adequate <- current_risk_margin_amount >= tail_gap_99
suggested_risk_ratio <- ifelse(
  total_portfolio_pure > 0,
  max(risk_ratio, tail_gap_99 / total_portfolio_pure),
  risk_ratio
)

portfolio_risk_metrics <- tibble(
  metric = c(
    "mean_loss", "sd_loss", "var_95", "var_99", "tvar_99",
    "historical_total_loss", "mean_minus_historical",
    "model_expected_loss", "simulation_minus_model_expected",
    "tail_gap_99", "current_risk_margin_amount", "suggested_risk_ratio"
  ),
  value = c(
    mean(total_loss), sd(total_loss), var95, var99, tvar99,
    historical_total_loss, mean(total_loss) - historical_total_loss,
    total_portfolio_pure, mean(total_loss) - total_portfolio_pure,
    tail_gap_99, current_risk_margin_amount, suggested_risk_ratio
  )
)

portfolio_premium_summary <- tibble(
  total_employees = total_employees,
  selected_frequency_model = selected_frequency,
  selected_severity_model = selected_severity,
  total_portfolio_pure_premium = total_portfolio_pure,
  total_portfolio_technical_premium = total_portfolio_premium,
  weighted_average_premium_per_employee = ifelse(total_employees > 0, total_portfolio_premium / total_employees, NA_real_),
  historical_total_loss = historical_total_loss,
  expected_loss_simulated = mean(total_loss),
  var99_simulated = var99,
  tvar99_simulated = tvar99,
  tail_gap_99 = tail_gap_99,
  loss_validation_difference = loss_validation$difference,
  risk_loading_adequate = risk_adequate,
  suggested_risk_ratio = suggested_risk_ratio
)

# =============================
# 10) 10-year economic projection (anchored baseline)
# =============================
econ_raw <- read_excel("srcsc-2026-interest-and-inflation.xlsx", skip = 2)

econ_forecast_hw <- function(df, col_name, horizon = 10) {
  hist <- df %>% filter(!is.na(.data[[col_name]]))
  y <- hist[[col_name]]
  hw <- HoltWinters(y, gamma = FALSE)
  pred <- as.numeric(predict(hw, n.ahead = horizon))

  tibble(
    year = (max(hist$Year) + 1):(max(hist$Year) + horizon),
    series = col_name,
    value = pred
  )
}

econ_projection <- bind_rows(
  econ_forecast_hw(econ_raw, "Inflation", 10),
  econ_forecast_hw(econ_raw, "1-Year Risk Free Annual Spot Rate", 10)
) |>
  pivot_wider(names_from = series, values_from = value) |>
  rename(
    inflation = Inflation,
    rf_1y = `1-Year Risk Free Annual Spot Rate`
  ) |>
  mutate(
    inflation = pmax(inflation, -0.02),
    rf_1y = pmax(rf_1y, 0),
    severity_trend_factor = cumprod(1 + inflation),
    discount_factor = 1 / cumprod(1 + rf_1y),
    trended_frequency = baseline_frequency,
    trended_severity = baseline_severity * severity_trend_factor,
    trended_pure_premium = trended_frequency * trended_severity,
    trended_technical_premium = (trended_pure_premium + trended_pure_premium * risk_ratio) / (1 - expense_ratio - profit_ratio),
    pv_trended_technical_premium = trended_technical_premium * discount_factor,
    portfolio_trended_technical_premium = total_portfolio_premium * severity_trend_factor,
    portfolio_pv_trended_technical_premium = portfolio_trended_technical_premium * discount_factor
  )

premium_projection_10yr <- econ_projection %>%
  dplyr::select(
    year, inflation, rf_1y,
    severity_trend_factor, discount_factor,
    trended_frequency, trended_severity,
    trended_pure_premium, trended_technical_premium, pv_trended_technical_premium,
    portfolio_trended_technical_premium, portfolio_pv_trended_technical_premium
  )

# =============================
# 11) Validation plots
# =============================
dir.create("WC_outputs", showWarnings = FALSE)

png("WC_outputs/portfolio_loss_distribution.png", width = 1200, height = 800)
hist(
  total_loss,
  breaks = 100,
  main = "Simulated Portfolio Loss Distribution",
  xlab = "Total Loss",
  col = "lightblue"
)
abline(v = var95, col = "orange", lwd = 2)
abline(v = var99, col = "red", lwd = 2)
abline(v = historical_total_loss, col = "darkgreen", lwd = 2)
dev.off()

# QQ plot: observed severities vs baseline simulated severities from spliced model
n_obs <- nrow(worker_sev)
qq_p <- ppoints(n_obs)

sim_bulk <- rgamma(n_obs, shape = bulk_shape, scale = bulk_scale)
sim_excess <- rgamma(n_obs, shape = shape_excess_gamma, scale = scale_excess_gamma)
sim_tail_flag <- rbinom(n_obs, size = 1, prob = p_tail)
sim_sev_baseline <- ifelse(sim_tail_flag == 1, tail_threshold + sim_excess, sim_bulk)
sim_sev_anchored <- sim_sev_baseline * severity_anchor_factor

obs_q <- quantile(worker_sev$claim_amount, probs = qq_p, na.rm = TRUE)
sim_q <- quantile(sim_sev_anchored, probs = qq_p, na.rm = TRUE)

png("WC_outputs/severity_qq_plot.png", width = 1200, height = 800)
plot(
  obs_q, sim_q,
  pch = 16, cex = 0.6,
  xlab = "Observed Severity Quantiles",
  ylab = "Simulated Severity Quantiles",
  main = "QQ Plot: Observed vs Simulated Claim Severity"
)
abline(0, 1, col = "red", lwd = 2)
dev.off()

# =============================
# 12) Write outputs
# =============================
write.csv(baseline_premium_summary, "WC_outputs/baseline_premium_summary.csv", row.names = FALSE)
write.csv(predictor_loading_table, "WC_outputs/predictor_loading_table.csv", row.names = FALSE)
write.csv(portfolio_premium_by_occupation, "WC_outputs/portfolio_premium_by_occupation.csv", row.names = FALSE)
write.csv(portfolio_premium_summary, "WC_outputs/portfolio_premium_summary.csv", row.names = FALSE)
write.csv(premium_projection_10yr, "WC_outputs/premium_projection_10yr.csv", row.names = FALSE)
write.csv(portfolio_risk_metrics, "WC_outputs/portfolio_risk_metrics.csv", row.names = FALSE)

# Supplemental diagnostics
write.csv(frequency_model_diagnostics, "WC_outputs/frequency_model_diagnostics.csv", row.names = FALSE)
write.csv(severity_model_diagnostics, "WC_outputs/severity_model_diagnostics.csv", row.names = FALSE)
write.csv(loss_validation, "WC_outputs/loss_validation.csv", row.names = FALSE)

# Human-readable model choice notes
cat("\nSelected frequency model:", selected_frequency, "\n")
cat("Selected severity model:", selected_severity, "\n")
cat("Vuong note:", vuong_note, "\n")
cat("Severity anchor factor:", round(severity_anchor_factor, 4), "\n")
cat("Historical total loss:", round(historical_total_loss, 2), "\n")
cat("Simulated mean loss (anchored):", round(mean(total_loss), 2), "\n")

cat("\nPricing workflow complete. Files written to WC_outputs/:\n")
cat("- baseline_premium_summary.csv\n")
cat("- predictor_loading_table.csv\n")
cat("- portfolio_premium_by_occupation.csv\n")
cat("- portfolio_premium_summary.csv\n")
cat("- premium_projection_10yr.csv\n")
cat("- portfolio_risk_metrics.csv\n")
cat("- frequency_model_diagnostics.csv\n")
cat("- severity_model_diagnostics.csv\n")
cat("- loss_validation.csv\n")
cat("- portfolio_loss_distribution.png\n")
cat("- severity_qq_plot.png\n")
