
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(pscl)
library(ismev)

# pscl loads MASS as a dependency; MASS::select() masks dplyr::select().
# Restore dplyr precedence explicitly.
select <- dplyr::select

set.seed(42)
dir.create("outputs", showWarnings = FALSE)


# =============================================================================
# PART A — PARAMETERS
# =============================================================================

EXPENSE_LOAD  <- 0.12
PROFIT_LOAD   <- 0.07
RISK_LOAD     <- 0.05
N_SIMS        <- 200000
VUONG_N       <- 50000
SPLICE_THRESH <- 100000


# =============================================================================
# PART B — UTILITY FUNCTIONS
# =============================================================================

# Strip random ID suffixes (e.g. "Epsilon_???4070" -> "Epsilon") 
strip_id_suffixes <- function(df) {
  df %>% mutate(across(
    where(\(x) is.character(x) || is.factor(x)),
    \(x) str_squish(str_remove(as.character(x), "_.*$"))
  ))
}

coef_lookup <- function(coef_vec, predictor, lvl = NULL) {
  key <- if (is.null(lvl)) predictor else paste0(predictor, lvl)
  if (key %in% names(coef_vec)) unname(coef_vec[key]) else 0
}

central_value <- function(x) {
  x <- x[!is.na(x)]
  vals <- sort(unique(x))
  if (length(vals) <= 2 && all(vals %in% 0:1)) return(1)
  mean(x)
}

wt_mean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok) || sum(w[ok]) == 0) return(NA_real_)
  weighted.mean(x[ok], w[ok])
}

# Lowercase + punctuation removal for fuzzy string matching.
normalise_str <- function(x) {
  str_squish(str_replace_all(str_to_lower(x), "[^a-z0-9 ]", " "))
}


# =============================================================================
# PART C — DATA INGESTION AND VALIDATION
# =============================================================================
# All numeric fields are validated against the data dictionary ranges below.

DICT_BOUNDS <- list(
  equipment_age   = c(0,     10),
  maintenance_int = c(100,   5000),
  usage_int       = c(0,     24),
  exposure        = c(1e-9,  1),
  claim_count     = c(0,     3),
  claim_amount    = c(11000, 790000)
)

apply_bounds <- function(df, fields) {
  for (fld in fields) {
    if (!fld %in% names(DICT_BOUNDS)) next
    lo <- DICT_BOUNDS[[fld]][1]
    hi <- DICT_BOUNDS[[fld]][2]
    df <- df[!is.na(df[[fld]]) & df[[fld]] >= lo & df[[fld]] <= hi, ]
  }
  df
}

raw_freq <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "freq")
raw_sev  <- read_excel("srcsc-2026-claims-equipment-failure.xlsx", sheet = "sev")

freq_required <- c("equipment_type", "solar_system", "equipment_age",
                   "maintenance_int", "usage_int", "exposure", "claim_count")

sev_required  <- c("equipment_type", "solar_system", "equipment_age",
                   "maintenance_int", "usage_int", "claim_amount")

df_freq <- raw_freq %>%
  strip_id_suffixes() %>%
  mutate(claim_count = as.integer(suppressWarnings(round(as.numeric(claim_count))))) %>%
  filter(if_all(all_of(freq_required), \(x) !is.na(x))) %>%
  apply_bounds(freq_required) %>%
  mutate(
    equipment_type = factor(equipment_type),
    solar_system   = factor(solar_system)
  )

df_sev <- raw_sev %>%
  strip_id_suffixes() %>%
  filter(if_all(all_of(sev_required), \(x) !is.na(x))) %>%
  apply_bounds(sev_required) %>%
  mutate(
    equipment_type = factor(equipment_type, levels = levels(df_freq$equipment_type)),
    solar_system   = factor(solar_system,   levels = levels(df_freq$solar_system))
  )

cat(sprintf("Frequency data: %d raw rows -> %d after validation\n",
            nrow(raw_freq), nrow(df_freq)))
cat(sprintf("Severity data:  %d raw rows -> %d after validation\n",
            nrow(raw_sev),  nrow(df_sev)))

obs_total_loss    <- sum(df_sev$claim_amount)
obs_mean_severity <- mean(df_sev$claim_amount)
obs_zero_rate     <- mean(df_freq$claim_count == 0)

cat(sprintf("Observed zero rate: %.4f | Mean severity: %.0f\n",
            obs_zero_rate, obs_mean_severity))


# =============================================================================
# PART D — FREQUENCY MODEL SELECTION (ZIP vs Poisson)
# =============================================================================
freq_rhs <- "equipment_type + solar_system + equipment_age + maintenance_int + usage_int"

zip_fit <- zeroinfl(
  as.formula(paste("claim_count ~", freq_rhs, "| 1")),
  data   = df_freq,
  dist   = "poisson",
  offset = log(exposure)
)

pois_fit <- glm(
  as.formula(paste("claim_count ~", freq_rhs, "+ offset(log(exposure))")),
  data   = df_freq,
  family = poisson
)

zip_pred_pi   <- predict(zip_fit, type = "zero")
zip_pred_mu   <- predict(zip_fit, type = "count")
zip_zero_hat  <- mean(zip_pred_pi + (1 - zip_pred_pi) * dpois(0, zip_pred_mu))
pois_zero_hat <- mean(dpois(0, fitted(pois_fit)))

vuong_stat <- NA_real_
vuong_pval <- NA_real_
vuong_msg  <- "Vuong test not run"

subsample <- df_freq %>% slice_sample(n = min(nrow(df_freq), VUONG_N))

zip_sub <- tryCatch(
  zeroinfl(as.formula(paste("claim_count ~", freq_rhs, "| 1")),
           data = subsample, dist = "poisson", offset = log(exposure)),
  error = \(e) NULL
)
pois_sub <- tryCatch(
  glm(as.formula(paste("claim_count ~", freq_rhs, "+ offset(log(exposure))")),
      data = subsample, family = poisson),
  error = \(e) NULL
)

if (!is.null(zip_sub) && !is.null(pois_sub)) {
  vt <- tryCatch(vuong(zip_sub, pois_sub), error = \(e) NULL)
  if (!is.null(vt) && "Raw" %in% names(vt$statistic)) {
    vuong_stat <- unname(vt$statistic["Raw"])
    vuong_pval <- unname(vt$p["Raw"])
    vuong_msg  <- "Vuong run on subsample"
  }
}

zip_aic  <- AIC(zip_fit);  pois_aic <- AIC(pois_fit)
zip_bic  <- BIC(zip_fit);  pois_bic <- BIC(pois_fit)

zip_zero_err  <- abs(zip_zero_hat  - obs_zero_rate)
pois_zero_err <- abs(pois_zero_hat - obs_zero_rate)

freq_winner <- "ZIP"
if (!is.na(vuong_stat) && vuong_stat < -1.96) freq_winner <- "POISSON"
if (is.na(vuong_stat) || abs(vuong_stat) <= 1.96) {
  if (pois_aic + 2 < zip_aic && pois_zero_err <= zip_zero_err * 1.10)
    freq_winner <- "POISSON"
}

if (freq_winner == "ZIP") {
  cnt_coefs      <- zip_fit$coefficients$count
  zero_coefs     <- zip_fit$coefficients$zero
  cnt_intercept  <- unname(cnt_coefs[1])
  zero_intercept <- unname(zero_coefs[1])
  lambda0        <- exp(cnt_intercept)
  pi0            <- 1 - plogis(zero_intercept)
} else {
  cnt_coefs      <- coef(pois_fit)
  zero_coefs     <- c("(Intercept)" = -Inf)
  cnt_intercept  <- unname(cnt_coefs[1])
  zero_intercept <- NA_real_
  lambda0        <- exp(cnt_intercept)
  pi0            <- 1
}

baseline_freq <- lambda0 * pi0

freq_diagnostics <- tibble(
  metric = c("observed_zero_rate", "zip_zero_rate", "poisson_zero_rate",
             "zip_aic", "poisson_aic", "zip_bic", "poisson_bic",
             "vuong_statistic", "vuong_pvalue"),
  value  = c(obs_zero_rate, zip_zero_hat, pois_zero_hat,
             zip_aic, pois_aic, zip_bic, pois_bic,
             vuong_stat, vuong_pval)
) %>%
  bind_rows(tibble(metric = "selected_model", value = NA_real_)) %>%
  mutate(label = case_when(
    metric == "selected_model"   ~ freq_winner,
    metric == "vuong_statistic"  ~ vuong_msg,
    TRUE                         ~ NA_character_
  ))


# =============================================================================
# PART E — SEVERITY MODEL SELECTION (Gamma vs Log-Normal + GPD tail splice)
# =============================================================================

sev_rhs <- "equipment_type + solar_system + equipment_age + maintenance_int + usage_int"

gamma_fit <- glm(
  as.formula(paste("claim_amount ~", sev_rhs)),
  family = Gamma(link = "log"),
  data   = df_sev
)

lognorm_fit <- glm(
  as.formula(paste("log(claim_amount) ~", sev_rhs)),
  family = gaussian(link = "identity"),
  data   = df_sev
)

gamma_aic   <- AIC(gamma_fit)
lognorm_aic <- AIC(lognorm_fit)
sev_winner  <- if (lognorm_aic + 2 < gamma_aic) "LOGNORMAL" else "GAMMA"

if (sev_winner == "LOGNORMAL") {
  sev_coefs        <- coef(lognorm_fit)
  sigma2_resid     <- summary(lognorm_fit)$dispersion
  sev_intercept    <- unname(sev_coefs[1])
  baseline_sev_raw <- exp(sev_intercept + 0.5 * sigma2_resid)
} else {
  sev_coefs        <- coef(gamma_fit)
  sigma2_resid     <- NA_real_
  sev_intercept    <- unname(sev_coefs[1])
  baseline_sev_raw <- exp(sev_intercept)
}

# GPD splice ---------------------------------------------------------------

claim_vals  <- df_sev$claim_amount
bulk_vals   <- claim_vals[claim_vals <= SPLICE_THRESH]
excess_vals <- claim_vals[claim_vals >  SPLICE_THRESH] - SPLICE_THRESH
tail_prob   <- length(excess_vals) / length(claim_vals)

bulk_mu    <- mean(bulk_vals)
bulk_var   <- var(bulk_vals)
bulk_alpha <- bulk_mu^2 / pmax(bulk_var, 1e-9)
bulk_beta  <- bulk_var  / pmax(bulk_mu,  1e-9)

gpd_mom_fit <- function(x) {
  mu <- mean(x); v <- var(x)
  list(shape = 0.5 * (1 - mu^2 / v),
       scale = pmax(0.5 * mu * (1 + mu^2 / v), 1e-6))
}

if (length(excess_vals) >= 30) {
  gpd_result <- tryCatch(
    withCallingHandlers(
      gpd.fit(excess_vals, threshold = 0, show = FALSE),
      warning = \(w) {
        if (grepl("NaN", conditionMessage(w))) invokeRestart("muffleWarning")
      }
    ),
    error = \(e) NULL
  )
  if (!is.null(gpd_result) &&
      is.finite(gpd_result$mle[1]) && gpd_result$mle[1] > 0 &&
      is.finite(gpd_result$mle[2])) {
    gpd_xi    <- unname(gpd_result$mle[2])
    gpd_sigma <- unname(gpd_result$mle[1])
  } else {
    mom_est   <- gpd_mom_fit(excess_vals)
    gpd_xi    <- mom_est$shape
    gpd_sigma <- mom_est$scale
  }
} else {
  mom_est   <- gpd_mom_fit(excess_vals)
  gpd_xi    <- mom_est$shape
  gpd_sigma <- mom_est$scale
}

gpd_xi <- max(min(gpd_xi, 0.49), -0.40)

gpd_mean_excess <- if (abs(gpd_xi) < 1e-8) gpd_sigma else gpd_sigma / (1 - gpd_xi)
gpd_var_excess  <- if (gpd_xi < 0.49) {
  gpd_sigma^2 / ((1 - gpd_xi)^2 * (1 - 2 * gpd_xi))
} else {
  (10 * gpd_mean_excess)^2
}

tail_gamma_alpha <- gpd_mean_excess^2 / pmax(gpd_var_excess,  1e-9)
tail_gamma_beta  <- gpd_var_excess    / pmax(gpd_mean_excess, 1e-9)

splice_mean <- (1 - tail_prob) * (bulk_alpha * bulk_beta) +
  tail_prob       * (SPLICE_THRESH + gpd_mean_excess)

sev_diagnostics <- tibble(
  metric = c("observed_mean_severity", "observed_max_severity",
             "gamma_aic", "lognormal_aic",
             "splice_threshold", "tail_probability",
             "bulk_gamma_shape", "bulk_gamma_scale",
             "gpd_shape", "gpd_scale", "splice_mean"),
  value  = c(obs_mean_severity, max(claim_vals),
             gamma_aic, lognorm_aic,
             SPLICE_THRESH, tail_prob,
             bulk_alpha, bulk_beta,
             gpd_xi, gpd_sigma, splice_mean)
) %>%
  bind_rows(tibble(metric = "selected_model", value = NA_real_)) %>%
  mutate(label = if_else(metric == "selected_model", sev_winner, NA_character_))


# =============================================================================
# PART F — RELATIVITIES TABLE
# =============================================================================

var_labels <- c(
  equipment_type  = "Equipment Type",
  solar_system    = "Solar System",
  equipment_age   = "Equipment Age (yrs)",
  maintenance_int = "Maintenance Interval (hrs)",
  usage_int       = "Usage Intensity (hrs/day)"
)

build_cat_relativities <- function(var, lvls) {
  ref <- lvls[1]
  lapply(lvls, function(lv) {
    f_rel <- exp(coef_lookup(cnt_coefs, var, lv))
    s_rel <- exp(coef_lookup(sev_coefs, var, lv))
    tibble(
      variable     = var_labels[var],
      variable_key = var,
      type         = "categorical",
      level        = lv,
      is_reference = lv == ref,
      rep_value    = NA_real_,
      freq_rel     = f_rel,
      sev_rel      = s_rel,
      combined_rel = f_rel * s_rel
    )
  }) %>% bind_rows()
}

build_num_relativity <- function(var, df) {
  rv    <- central_value(df[[var]])
  f_rel <- exp(coef_lookup(cnt_coefs, var) * rv)
  s_rel <- exp(coef_lookup(sev_coefs, var) * rv)
  tibble(
    variable     = var_labels[var],
    variable_key = var,
    type         = "numeric",
    level        = "representative",
    is_reference = FALSE,
    rep_value    = rv,
    freq_rel     = f_rel,
    sev_rel      = s_rel,
    combined_rel = f_rel * s_rel
  )
}

relativities <- bind_rows(
  build_cat_relativities("equipment_type", levels(df_freq$equipment_type)),
  build_cat_relativities("solar_system",   levels(df_freq$solar_system)),
  build_num_relativity("equipment_age",   df_freq),
  build_num_relativity("maintenance_int", df_freq),
  build_num_relativity("usage_int",       df_freq)
) %>%
  mutate(across(c(freq_rel, sev_rel, combined_rel, rep_value), \(x) round(x, 6))) %>%
  arrange(variable_key, type, level)


# =============================================================================
# PART G — INVENTORY PARSING
# =============================================================================

inv <- read_excel("srcsc-2026-cosmic-quarry-inventory.xlsx",
                  sheet = "Equipment", col_names = FALSE)

systems  <- c("Helionis Cluster", "Bayesian System", "Oryn Delta")
eq_types <- c("Quantum Bores", "Graviton Extractors", "Fexstram Carriers",
              "ReglAggregators", "Flux Riders", "Ion Pulverizers")

# G1 — Unit counts
cnt_block        <- inv[5:10, 1:4]
names(cnt_block) <- c("eq_label", systems)

unit_counts <- cnt_block %>%
  mutate(eq_label = str_squish(as.character(eq_label))) %>%
  filter(!is.na(eq_label)) %>%
  pivot_longer(-eq_label, names_to = "system", values_to = "units") %>%
  mutate(units = suppressWarnings(as.numeric(units)))

# G2 — Age distribution
parse_age_block <- function(start_row, system_name) {
  blk <- inv[start_row:(start_row + 4), 1:7]
  names(blk) <- c("band", eq_types)
  blk %>%
    mutate(
      band     = c("<5", "5-9", "10-14", "15-19", "20+"),
      midpoint = c(2.5, 7, 12, 17, 22),
      system   = system_name
    ) %>%
    pivot_longer(all_of(eq_types), names_to = "eq_label", values_to = "n") %>%
    mutate(n = suppressWarnings(as.numeric(n)))
}

age_distributions <- bind_rows(
  parse_age_block(14, "Helionis Cluster"),
  parse_age_block(22, "Bayesian System"),
  parse_age_block(30, "Oryn Delta")
)

weighted_ages <- age_distributions %>%
  group_by(eq_label, system) %>%
  summarise(mean_age = wt_mean(midpoint, n), .groups = "drop")


usage_block        <- inv[41:46, 1:7]
names(usage_block) <- c("eq_label",
                        "pct_op_HC", "maint_hrs_HC",
                        "pct_op_BS", "maint_hrs_BS",
                        "pct_op_OD", "maint_hrs_OD")

usage_schedule <- usage_block %>%
  mutate(eq_label = str_squish(as.character(eq_label))) %>%
  filter(!is.na(eq_label)) %>%
  pivot_longer(
    cols          = -eq_label,
    names_to      = c(".value", "sys_code"),
    names_pattern = "^(.+)_(HC|BS|OD)$"
  ) %>%
  mutate(
    system    = case_when(
      sys_code == "HC" ~ "Helionis Cluster",
      sys_code == "BS" ~ "Bayesian System",
      sys_code == "OD" ~ "Oryn Delta"
    ),
    pct_op    = suppressWarnings(as.numeric(pct_op)),
    maint_hrs = suppressWarnings(as.numeric(maint_hrs))
  ) %>%
  dplyr::select(eq_label, system, pct_op, maint_hrs)

# G4 — Risk index
risk_block        <- inv[51:56, 1:4]
names(risk_block) <- c("eq_label", systems)

risk_index <- risk_block %>%
  mutate(eq_label = str_squish(as.character(eq_label))) %>%
  filter(!is.na(eq_label)) %>%
  pivot_longer(-eq_label, names_to = "system", values_to = "risk_idx") %>%
  mutate(risk_idx = suppressWarnings(as.numeric(risk_idx)))

inventory <- unit_counts %>%
  left_join(weighted_ages,  by = c("eq_label", "system")) %>%
  left_join(usage_schedule, by = c("eq_label", "system")) %>%
  left_join(risk_index,     by = c("eq_label", "system")) %>%
  filter(!is.na(units), units > 0)


# =============================================================================
# PART H — MAP INVENTORY TO MODEL FACTOR LEVELS
# =============================================================================

inv_to_model_eq <- tibble(
  inv_norm   = normalise_str(c("Quantum Bores", "Graviton Extractors",
                               "Fexstram Carriers", "ReglAggregators",
                               "Flux Riders", "Ion Pulverizers")),
  model_norm = normalise_str(c("Quantum Bore", "Graviton Extractor",
                               "FexStram Carrier", "ReglAggregators",
                               "Flux Rider", "Ion Pulverizer"))
)

eq_rel_lookup <- relativities %>%
  filter(variable_key == "equipment_type") %>%
  transmute(
    model_norm   = normalise_str(level),
    eq_freq_rel  = freq_rel,
    eq_sev_rel   = sev_rel,
    eq_comb_rel  = combined_rel
  )

sol_rel_lookup <- relativities %>%
  filter(variable_key == "solar_system") %>%
  transmute(
    system       = level,
    sol_freq_rel = freq_rel,
    sol_sev_rel  = sev_rel,
    sol_comb_rel = combined_rel
  )

inventory <- inventory %>%
  mutate(inv_norm = normalise_str(eq_label)) %>%
  left_join(inv_to_model_eq, by = "inv_norm") %>%
  left_join(eq_rel_lookup,   by = "model_norm") %>%
  left_join(sol_rel_lookup,  by = "system")

sys_avg <- inventory %>%
  filter(!is.na(eq_comb_rel)) %>%
  group_by(system) %>%
  summarise(across(c(eq_freq_rel, eq_sev_rel, eq_comb_rel),
                   \(x) wt_mean(x, units)),
            .groups = "drop") %>%
  rename_with(\(n) paste0("sys_", n), -system)

global_avg <- eq_rel_lookup %>%
  summarise(across(ends_with("_rel"), \(x) mean(x, na.rm = TRUE)))

inventory <- inventory %>%
  left_join(sys_avg, by = "system") %>%
  mutate(
    eq_freq_rel  = coalesce(eq_freq_rel,  sys_eq_freq_rel,  global_avg$eq_freq_rel),
    eq_sev_rel   = coalesce(eq_sev_rel,   sys_eq_sev_rel,   global_avg$eq_sev_rel),
    eq_comb_rel  = coalesce(eq_comb_rel,  sys_eq_comb_rel,  global_avg$eq_comb_rel),
    sol_freq_rel = coalesce(sol_freq_rel, mean(sol_rel_lookup$sol_freq_rel)),
    sol_sev_rel  = coalesce(sol_sev_rel,  mean(sol_rel_lookup$sol_sev_rel)),
    sol_comb_rel = coalesce(sol_comb_rel, mean(sol_rel_lookup$sol_comb_rel)),
    match_source = case_when(
      !is.na(model_norm)      ~ "exact",
      !is.na(sys_eq_comb_rel) ~ "system_average",
      TRUE                    ~ "global_average"
    )
  )

# Numeric predictor adjustments from inventory-level characteristics
age_freq_b   <- coef_lookup(cnt_coefs, "equipment_age")
age_sev_b    <- coef_lookup(sev_coefs, "equipment_age")
maint_freq_b <- coef_lookup(cnt_coefs, "maintenance_int")
maint_sev_b  <- coef_lookup(sev_coefs, "maintenance_int")
usage_freq_b <- coef_lookup(cnt_coefs, "usage_int")
usage_sev_b  <- coef_lookup(sev_coefs, "usage_int")

ref_age   <- central_value(df_freq$equipment_age)
ref_maint <- central_value(df_freq$maintenance_int)
ref_usage <- central_value(df_freq$usage_int)

inventory <- inventory %>%
  mutate(
    eff_age   = coalesce(mean_age, ref_age),
    eff_maint = coalesce(maint_hrs, ref_maint),
    eff_usage = coalesce(
      pct_op * ref_usage / mean(usage_schedule$pct_op, na.rm = TRUE),
      ref_usage
    ),
    num_freq_adj = exp(age_freq_b   * eff_age  +
                         maint_freq_b * eff_maint +
                         usage_freq_b * eff_usage),
    num_sev_adj  = exp(age_sev_b    * eff_age  +
                         maint_sev_b  * eff_maint +
                         usage_sev_b  * eff_usage),
    num_comb_adj = num_freq_adj * num_sev_adj
  )

total_units <- sum(inventory$units)


# =============================================================================
# PART I — MONTE CARLO LOSS SIMULATION
# =============================================================================

q_active <- if (freq_winner == "ZIP") pi0 else 1

agg_loss_raw <- numeric(N_SIMS)

for (i in seq_len(nrow(inventory))) {
  row <- inventory[i, ]
  n   <- as.integer(row$units)
  if (is.na(n) || n == 0L) next
  
  lam <- lambda0 * row$eq_freq_rel * row$sol_freq_rel * row$num_freq_adj
  
  active   <- rbinom(N_SIMS, size = n, prob = pmin(pmax(q_active, 0), 1))
  n_claims <- integer(N_SIMS)
  hit      <- active > 0L
  if (any(hit))
    n_claims[hit] <- rpois(sum(hit), lambda = pmax(lam * active[hit], 0))
  
  sev_mean_hat  <- baseline_sev_raw * row$eq_sev_rel * row$sol_sev_rel * row$num_sev_adj
  sev_scale_adj <- sev_mean_hat / pmax(splice_mean, 1e-9)
  
  n_tail <- rbinom(N_SIMS, size = n_claims, prob = tail_prob)
  n_bulk <- n_claims - n_tail
  
  loss_bulk <- numeric(N_SIMS)
  has_bulk  <- n_bulk > 0L
  if (any(has_bulk))
    loss_bulk[has_bulk] <- rgamma(sum(has_bulk),
                                  shape = pmax(bulk_alpha * n_bulk[has_bulk], 1e-9),
                                  scale = bulk_beta)
  
  loss_tail_excess <- numeric(N_SIMS)
  has_tail <- n_tail > 0L
  if (any(has_tail))
    loss_tail_excess[has_tail] <- rgamma(sum(has_tail),
                                         shape = pmax(tail_gamma_alpha * n_tail[has_tail], 1e-9),
                                         scale = tail_gamma_beta)
  
  row_loss     <- sev_scale_adj * (loss_bulk + SPLICE_THRESH * n_tail + loss_tail_excess)
  agg_loss_raw <- agg_loss_raw + row_loss
}

anchor_factor     <- if (mean(agg_loss_raw) > 0) obs_total_loss / mean(agg_loss_raw) else 1
agg_loss          <- agg_loss_raw * anchor_factor
sev_intercept_adj <- log(anchor_factor)


# =============================================================================
# PART J — BASELINE PREMIUM DERIVATION
# =============================================================================

baseline_sev     <- baseline_sev_raw * anchor_factor
baseline_pp      <- baseline_freq * baseline_sev
risk_margin_pp   <- baseline_pp * RISK_LOAD
baseline_tech_pp <- (baseline_pp + risk_margin_pp) / (1 - EXPENSE_LOAD - PROFIT_LOAD)

relativities <- relativities %>%
  mutate(
    pure_premium      = round(baseline_pp      * combined_rel, 6),
    technical_premium = round(baseline_tech_pp * combined_rel, 6)
  )

baseline_summary <- tibble(
  metric = c("obs_total_loss", "obs_mean_severity",
             "baseline_lambda", "baseline_pi", "baseline_frequency",
             "baseline_severity_raw", "anchor_factor", "sev_intercept_adj",
             "baseline_severity",
             "baseline_pure_premium", "risk_margin",
             "expense_load", "profit_load", "risk_load",
             "baseline_technical_premium"),
  value  = c(obs_total_loss, obs_mean_severity,
             lambda0, pi0, baseline_freq,
             baseline_sev_raw, anchor_factor, sev_intercept_adj,
             baseline_sev,
             baseline_pp, risk_margin_pp,
             EXPENSE_LOAD, PROFIT_LOAD, RISK_LOAD,
             baseline_tech_pp)
)


# =============================================================================
# PART K — PORTFOLIO PRICING BY INVENTORY UNIT
# =============================================================================

portfolio_by_unit <- inventory %>%
  mutate(
    unit_pure_prem = baseline_pp      * eq_comb_rel * sol_comb_rel * num_comb_adj,
    unit_tech_prem = baseline_tech_pp * eq_comb_rel * sol_comb_rel * num_comb_adj,
    total_pure     = unit_pure_prem * units,
    total_tech     = unit_tech_prem * units
  ) %>%
  dplyr::select(
    solar_system    = system,
    equipment_type  = eq_label,
    match_source,
    units,
    mean_age        = eff_age,
    pct_operational = pct_op,
    risk_index      = risk_idx,
    unit_tech_prem,
    total_tech,
    unit_pure_prem,
    total_pure,
    eq_comb_rel,
    sol_comb_rel,
    num_comb_adj
  ) %>%
  arrange(desc(total_tech))

port_total_pure <- sum(portfolio_by_unit$total_pure, na.rm = TRUE)
port_total_tech <- sum(portfolio_by_unit$total_tech, na.rm = TRUE)


# =============================================================================
# PART L — RISK METRICS
# =============================================================================

VaR95  <- unname(quantile(agg_loss, 0.95))
VaR99  <- unname(quantile(agg_loss, 0.99))
TVaR99 <- mean(agg_loss[agg_loss >= VaR99])

tail_gap            <- max(TVaR99 - mean(agg_loss), 0)
curr_risk_marg      <- RISK_LOAD * port_total_pure
risk_is_adequate    <- curr_risk_marg >= tail_gap
suggested_risk_load <- if (port_total_pure > 0)
  max(RISK_LOAD, tail_gap / port_total_pure) else RISK_LOAD

loss_validation <- tibble(
  sim_mean_loss       = mean(agg_loss),
  model_expected_loss = port_total_pure,
  sim_vs_model_diff   = mean(agg_loss) - port_total_pure,
  sim_vs_observed_pct = (mean(agg_loss) - obs_total_loss) / obs_total_loss
)

risk_metrics <- tibble(
  metric = c("mean_loss", "sd_loss", "VaR_95", "VaR_99", "TVaR_99",
             "obs_total_loss", "sim_minus_obs",
             "model_expected", "sim_minus_model",
             "tail_gap_99", "current_risk_margin", "suggested_risk_load"),
  value  = c(mean(agg_loss), sd(agg_loss), VaR95, VaR99, TVaR99,
             obs_total_loss, mean(agg_loss) - obs_total_loss,
             port_total_pure, mean(agg_loss) - port_total_pure,
             tail_gap, curr_risk_marg, suggested_risk_load)
)

portfolio_summary <- tibble(
  total_units                 = total_units,
  freq_model                  = freq_winner,
  sev_model                   = sev_winner,
  portfolio_pure_premium      = port_total_pure,
  portfolio_technical_premium = port_total_tech,
  avg_tech_premium_per_unit   = port_total_tech / total_units,
  obs_total_loss              = obs_total_loss,
  sim_mean_loss               = mean(agg_loss),
  VaR99                       = VaR99,
  TVaR99                      = TVaR99,
  tail_gap_99                 = tail_gap,
  sim_vs_model_diff           = loss_validation$sim_vs_model_diff,
  risk_load_adequate          = risk_is_adequate,
  suggested_risk_load         = suggested_risk_load
)


# =============================================================================
# PART M — 10-YEAR ECONOMIC PROJECTION
# =============================================================================

econ_raw <- read_excel("srcsc-2026-interest-and-inflation.xlsx", skip = 2)

hw_forecast <- function(df, col, n = 10) {
  series <- df[[col]][!is.na(df[[col]])]
  fcast  <- predict(HoltWinters(series, gamma = FALSE), n.ahead = n)
  tibble(
    year   = seq(max(df$Year[!is.na(df[[col]])]) + 1, length.out = n),
    series = col,
    value  = as.numeric(fcast)
  )
}

projection <- bind_rows(
  hw_forecast(econ_raw, "Inflation"),
  hw_forecast(econ_raw, "1-Year Risk Free Annual Spot Rate")
) %>%
  pivot_wider(names_from = series, values_from = value) %>%
  rename(inflation = Inflation,
         rf_rate   = `1-Year Risk Free Annual Spot Rate`) %>%
  mutate(
    inflation       = pmax(inflation, -0.02),
    rf_rate         = pmax(rf_rate, 0),
    trend_factor    = cumprod(1 + inflation),
    discount_factor = 1 / cumprod(1 + rf_rate),
    trended_freq    = baseline_freq,
    trended_sev     = baseline_sev * trend_factor,
    trended_pp      = trended_freq * trended_sev,
    trended_tech_pp = (trended_pp * (1 + RISK_LOAD)) / (1 - EXPENSE_LOAD - PROFIT_LOAD),
    pv_tech_pp      = trended_tech_pp * discount_factor,
    port_trended_tech = port_total_tech * trend_factor,
    port_pv_tech      = port_total_tech * trend_factor * discount_factor
  ) %>%
  dplyr::select(year, inflation, rf_rate, trend_factor, discount_factor,
                trended_freq, trended_sev, trended_pp,
                trended_tech_pp, pv_tech_pp,
                port_trended_tech, port_pv_tech)


# =============================================================================
# PART N — DIAGNOSTIC PLOTS
# =============================================================================

# Colour palette
COL_HIST    <- "#C8E6C9"   # mint — histogram bars
COL_BORDER  <- "white"   
COL_POINTS  <- "#F4B8C1"   # baby pink — QQ scatter points
COL_VAR95   <- "#F57373"   # soft red — VaR 95% line
COL_VAR99   <- "#C62828"   # deep red — VaR 99% line
COL_OBS     <- "#1888fc"   # raspberry — observed total loss line
COL_QQLINE  <- "#B71C1C"   # dark red — QQ 45-degree reference line

png("outputs/portfolio_loss_distribution_equip.png", width = 1200, height = 800)
par(bg = COL_BORDER)
hist(agg_loss, breaks = 100,
     main = "Simulated Aggregate Loss Distribution — Equipment Failure",
     xlab = "Aggregate Annual Loss",
     col  = COL_HIST, border = COL_BORDER)
abline(v = VaR95,          col = COL_VAR95, lwd = 2, lty = 2)
abline(v = VaR99,          col = COL_VAR99, lwd = 2, lty = 1)
abline(v = obs_total_loss, col = COL_OBS,   lwd = 2, lty = 3)
legend("topright",
       legend = c("VaR 95%", "VaR 99%", "Observed Total Loss"),
       col    = c(COL_VAR95, COL_VAR99, COL_OBS),
       lwd = 2, lty = c(2, 1, 3),
       bg  = COL_BORDER)
dev.off()

n_qq  <- nrow(df_sev)
probs <- ppoints(n_qq)

sim_bulk_qq <- rgamma(n_qq, shape = bulk_alpha,       scale = bulk_beta)
sim_tail_qq <- rgamma(n_qq, shape = tail_gamma_alpha, scale = tail_gamma_beta)
is_tail_qq  <- rbinom(n_qq, size = 1, prob = tail_prob) == 1
sim_sev_qq  <- ifelse(is_tail_qq, SPLICE_THRESH + sim_tail_qq, sim_bulk_qq) * anchor_factor

png("outputs/severity_qq_plot_equip.png", width = 1000, height = 1000)
par(bg = COL_BORDER)
plot(
  quantile(df_sev$claim_amount, probs),
  quantile(sim_sev_qq,          probs),
  pch = 16, cex = 0.5,
  col  = COL_POINTS,
  xlab = "Observed Claim Amount Quantiles",
  ylab = "Simulated Claim Amount Quantiles",
  main = "Severity QQ Plot — Observed vs Spliced Model"
)
abline(0, 1, col = COL_QQLINE, lwd = 2)
dev.off()


# =============================================================================
# PART O — WRITE OUTPUTS
# =============================================================================

write.csv(baseline_summary,  "outputs/ef_baseline_premium_summary.csv",    row.names = FALSE)
write.csv(relativities,      "outputs/ef_predictor_loading_table.csv",      row.names = FALSE)
write.csv(portfolio_by_unit, "outputs/ef_portfolio_premium_by_unit.csv",    row.names = FALSE)
write.csv(portfolio_summary, "outputs/ef_portfolio_premium_summary.csv",    row.names = FALSE)
write.csv(projection,        "outputs/ef_premium_projection_10yr.csv",      row.names = FALSE)
write.csv(risk_metrics,      "outputs/ef_portfolio_risk_metrics.csv",       row.names = FALSE)
write.csv(freq_diagnostics,  "outputs/ef_frequency_model_diagnostics.csv",  row.names = FALSE)
write.csv(sev_diagnostics,   "outputs/ef_severity_model_diagnostics.csv",   row.names = FALSE)
write.csv(loss_validation,   "outputs/ef_loss_validation.csv",              row.names = FALSE)

cat("\n--- Equipment Failure Pricing Complete ---\n")
cat(sprintf("Frequency model       : %s\n", freq_winner))
cat(sprintf("Severity model        : %s\n", sev_winner))
cat(sprintf("Anchor factor         : %.4f\n", anchor_factor))
cat(sprintf("Observed total loss   : %s\n", format(round(obs_total_loss), big.mark = ",")))
cat(sprintf("Simulated mean loss   : %s\n", format(round(mean(agg_loss)),  big.mark = ",")))
cat(sprintf("Portfolio tech premium: %s\n", format(round(port_total_tech), big.mark = ",")))
cat(sprintf("VaR99 / TVaR99        : %s / %s\n",
            format(round(VaR99),  big.mark = ","),
            format(round(TVaR99), big.mark = ",")))

