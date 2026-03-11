# =============================================================
# Business Interruption Pricing Model
# Adapted from workers_comp_pricing_model.r
# =============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)
library(pscl)
library(tseries)
library(forecast)

set.seed(123)

# =============================
# Configuration
# =============================
expense_ratio <- 0.12
profit_ratio  <- 0.07
risk_ratio    <- 0.05
n_sims        <- 10000
exposure_assumption <- 1

stress_factor_freq <- 1.3
stress_factor_sev  <- 1.2

# =============================
# Helpers
# =============================
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

range_metrics <- function(x) {
  x <- x[is.finite(x)]
  tibble(
    expected_value = mean(x),
    variance = var(x),
    sd = sd(x),
    var_95 = unname(quantile(x, 0.95)),
    var_99 = unname(quantile(x, 0.99)),
    tvar_99 = mean(x[x >= quantile(x, 0.99)]),
    var_995 = unname(quantile(x, 0.995)),
    var_999 = unname(quantile(x, 0.999))
  )
}

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

extract_inflation_forecast <- function(horizon = 10) {
  inflation_fc <- NULL

  if (file.exists("inflation_forecast.R")) {
    tryCatch({
      source("inflation_forecast.R", local = TRUE)
      if (exists("inflation_forecast")) {
        fc_obj <- inflation_forecast
        if (is.numeric(fc_obj)) {
          inflation_fc <- as.numeric(fc_obj)
        } else {
          tb <- tryCatch(as_tibble(fc_obj), error = function(e) NULL)
          if (!is.null(tb)) {
            if (".mean" %in% names(tb)) inflation_fc <- as.numeric(tb$.mean)
            if ("mean" %in% names(tb)) inflation_fc <- as.numeric(tb$mean)
            if ("value" %in% names(tb)) inflation_fc <- as.numeric(tb$value)
            if ("Inflation" %in% names(tb)) inflation_fc <- as.numeric(tb$Inflation)
          }
          if (is.null(inflation_fc) && "mean" %in% names(fc_obj)) {
            inflation_fc <- as.numeric(fc_obj$mean)
          }
        }
      }
    }, error = function(e) NULL)
  }

  if (is.null(inflation_fc) || length(inflation_fc) < horizon) {
    econ_raw <- read_excel("srcsc-2026-interest-and-inflation.xlsx", skip = 2)
    inflation_hw <- econ_forecast_hw(econ_raw, "Inflation", horizon)
    inflation_fc <- inflation_hw$value
  }

  inflation_fc[seq_len(horizon)]
}

# =============================
# 1) Load and clean claims data
# =============================
bi_freq_raw <- read_excel("srcsc-2026-claims-business-interruption.xlsx", sheet = 1)
bi_sev_raw <- read_excel("srcsc-2026-claims-business-interruption.xlsx", sheet = 2)

bi_freq_raw <- bi_freq_raw %>%
  mutate(solar_system = case_when(
    str_detect(solar_system, "^Zeta") ~ "Zeta",
    str_detect(solar_system, "^Epsilon") ~ "Epsilon",
    str_detect(solar_system, "^Helionis") ~ "Helionis Cluster",
    TRUE ~ solar_system
  ))

bi_sev_raw <- bi_sev_raw %>%
  mutate(solar_system = case_when(
    str_detect(solar_system, "^Zeta") ~ "Zeta",
    str_detect(solar_system, "^Epsilon") ~ "Epsilon",
    str_detect(solar_system, "^Helionis") ~ "Helionis Cluster",
    TRUE ~ solar_system
  ))

bi_freq_clean <- bi_freq_raw %>%
  filter(
    !is.na(policy_id) & policy_id != "",
    !is.na(station_id) & station_id != "",
    solar_system %in% c("Helionis Cluster", "Epsilon", "Zeta"),
    production_load >= 0 & production_load <= 1,
    energy_backup_score %in% 1:5,
    supply_chain_index >= 0 & supply_chain_index <= 1,
    avg_crew_exp >= 1 & avg_crew_exp <= 30,
    maintenance_freq >= 0 & maintenance_freq <= 6,
    safety_compliance %in% 1:5,
    exposure > 0 & exposure <= 1,
    claim_count >= 0 & claim_count <= 4
  ) %>%
  mutate(
    solar_system = factor(solar_system)
  )

bi_sev_clean <- bi_sev_raw %>%
  filter(
    !is.na(policy_id) & policy_id != "",
    !is.na(station_id) & station_id != "",
    solar_system %in% c("Helionis Cluster", "Epsilon", "Zeta"),
    production_load >= 0 & production_load <= 1,
    energy_backup_score %in% 1:5,
    safety_compliance %in% 1:5,
    exposure > 0 & exposure <= 1,
    claim_amount >= 28000 & claim_amount <= 1426000
  ) %>%
  mutate(
    solar_system = factor(solar_system, levels = levels(bi_freq_clean$solar_system))
  )

historical_total_loss <- sum(bi_sev_clean$claim_amount, na.rm = TRUE)
historical_severity_mean <- mean(bi_sev_clean$claim_amount, na.rm = TRUE)

# =============================
# 2) Frequency model (NB) aligned to BI Monte Carlo
# =============================
bi_freq_model <- bi_freq_clean %>%
  mutate(log_exposure = log(exposure))

freq_formula <- claim_count ~ solar_system + production_load +
  energy_backup_score + supply_chain_index +
  avg_crew_exp + maintenance_freq + safety_compliance +
  offset(log_exposure)

nb_model <- glm.nb(freq_formula, data = bi_freq_model)

lambda_hat <- predict(nb_model, type = "response")
theta_hat <- nb_model$theta
base_count_mean <- exp(unname(coef(nb_model)[1]))
baseline_frequency <- base_count_mean * exposure_assumption

# =============================
# 3) Severity model (Gamma) aligned to BI Monte Carlo
# =============================
bi_sev_model <- bi_sev_clean
bi_sev_model$claim_amount[bi_sev_model$claim_amount <= 0] <- 0.01

gamma_sev <- glm(
  claim_amount ~ solar_system + production_load +
    energy_backup_score + safety_compliance + exposure,
  data = bi_sev_model,
  family = Gamma(link = "log")
)

sev_coef <- coef(gamma_sev)
sev_intercept <- unname(sev_coef[1])
baseline_severity_raw <- exp(sev_intercept)

# Gamma parameters used in your BI Monte Carlo
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(bi_sev_clean$claim_amount, na.rm = TRUE) / sev_shape

# Policy-level severity prediction using frequency dataset
sev_hat_policy_raw <- predict(gamma_sev, newdata = bi_freq_model, type = "response")
pure_premium_policy_raw <- lambda_hat * sev_hat_policy_raw

# Anchor so total expected loss aligns to historical total loss
portfolio_pure_premium_raw <- sum(pure_premium_policy_raw, na.rm = TRUE)

severity_anchor_factor <- historical_total_loss / portfolio_pure_premium_raw
baseline_severity <- baseline_severity_raw 
sev_hat_policy <- sev_hat_policy_raw * severity_anchor_factor
pure_premium_policy <- lambda_hat * sev_hat_policy

total_portfolio_pure <- sum(pure_premium_policy, na.rm = TRUE)
risk_margin <- total_portfolio_pure * risk_ratio
total_portfolio_technical <- (total_portfolio_pure + risk_margin) /
  (1 - expense_ratio - profit_ratio)

baseline_pure_premium <- baseline_frequency * baseline_severity
baseline_technical_premium <- (baseline_pure_premium + baseline_pure_premium * risk_ratio) /
  (1 - expense_ratio - profit_ratio)

# =============================
# 4) Predictor loading table (frequency + severity)
# =============================
make_categorical_rows <- function(var, levels_vec) {
  base_level <- levels_vec[1]
  bind_rows(lapply(levels_vec, function(lv) {
    count_beta <- get_beta(coef(nb_model), var, lv)
    sev_beta <- get_beta(sev_coef, var, lv)
    freq_loading <- exp(count_beta)
    sev_loading <- exp(sev_beta)
    tibble(
      predictor = var,
      predictor_key = var,
      predictor_type = "categorical",
      level = lv,
      is_baseline = lv == base_level,
      representative_value = NA_real_,
      frequency_loading = freq_loading,
      severity_loading = sev_loading,
      premium_loading = freq_loading * sev_loading
    )
  }))
}

make_numeric_row <- function(var, df, use_offset = FALSE) {
  rep_val <- representative_value(df[[var]])
  count_beta <- get_beta(coef(nb_model), var)
  sev_beta <- get_beta(sev_coef, var)

  freq_loading <- exp(count_beta * rep_val)
  if (use_offset && var == "exposure") freq_loading <- rep_val
  sev_loading <- exp(sev_beta * rep_val)

  tibble(
    predictor = var,
    predictor_key = var,
    predictor_type = "numeric",
    level = "representative_value",
    is_baseline = FALSE,
    representative_value = rep_val,
    frequency_loading = freq_loading,
    severity_loading = sev_loading,
    premium_loading = freq_loading * sev_loading
  )
}

predictor_loading_table <- bind_rows(
  make_categorical_rows("solar_system", levels(bi_freq_clean$solar_system)),
  make_numeric_row("production_load", bi_freq_clean),
  make_numeric_row("energy_backup_score", bi_freq_clean),
  make_numeric_row("supply_chain_index", bi_freq_clean),
  make_numeric_row("avg_crew_exp", bi_freq_clean),
  make_numeric_row("maintenance_freq", bi_freq_clean),
  make_numeric_row("safety_compliance", bi_freq_clean),
  make_numeric_row("exposure", bi_freq_clean, use_offset = TRUE)
) %>%
  mutate(
    frequency_loading = round(frequency_loading, 6),
    severity_loading = round(severity_loading, 6),
    premium_loading = round(premium_loading, 6),
    representative_value = round(representative_value, 6)
  ) %>%
  arrange(predictor, predictor_type, level)

# =============================
# 5) Portfolio pricing by policy
# =============================
portfolio_premium_by_policy <- bi_freq_model %>%
  mutate(
    predicted_frequency = lambda_hat,
    predicted_severity = sev_hat_policy,
    pure_premium = predicted_frequency * predicted_severity,
    technical_premium = (pure_premium + pure_premium * risk_ratio) /
      (1 - expense_ratio - profit_ratio),
    expense_loading = technical_premium * expense_ratio,
    profit_loading = technical_premium * profit_ratio
  ) %>%
  dplyr::select(
    policy_id, station_id, solar_system, exposure,
    predicted_frequency, predicted_severity,
    pure_premium, technical_premium,
    expense_loading, profit_loading
  ) %>%
  arrange(desc(technical_premium))

# =============================
# 6) Aggregate simulation (aligned with BI Monte Carlo)
# =============================
n_pol <- length(lambda_hat)
aggregate_loss_raw <- numeric(n_sims)

for (s in 1:n_sims) {
  freq_sim <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat)
  total_claims <- sum(freq_sim)

  if (total_claims > 0) {
    severities <- rgamma(total_claims, shape = sev_shape, scale = sev_scale)
    aggregate_loss_raw[s] <- sum(severities)
  } else {
    aggregate_loss_raw[s] <- 0
  }
}
aggregate_loss <- aggregate_loss_raw * severity_anchor_factor

loss_metrics <- range_metrics(aggregate_loss) %>%
  mutate(metric = "baseline_costs")

# =============================
# 7) Stress tests
# =============================
aggregate_loss_stress1 <- numeric(n_sims)  # Severity stress
aggregate_loss_stress2 <- numeric(n_sims)  # Frequency stress
aggregate_loss_stress3 <- numeric(n_sims)  # Catastrophic (both)

for (s in 1:n_sims) {
  freq_sim <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat)
  total_claims <- sum(freq_sim)

  if (total_claims > 0) {
    severities_stress <- rgamma(total_claims, shape = sev_shape, scale = sev_scale * stress_factor_sev)
    aggregate_loss_stress1[s] <- sum(severities_stress)
  } else {
    aggregate_loss_stress1[s] <- 0
  }

  freq_stress <- round(freq_sim * stress_factor_freq)
  total_claims_stress <- sum(freq_stress)

  if (total_claims_stress > 0) {
    severities_freq_stress <- rgamma(total_claims_stress, shape = sev_shape, scale = sev_scale)
    aggregate_loss_stress2[s] <- sum(severities_freq_stress)
  } else {
    aggregate_loss_stress2[s] <- 0
  }

  if (total_claims_stress > 0) {
    severities_cat <- rgamma(total_claims_stress, shape = sev_shape, scale = sev_scale * stress_factor_sev)
    aggregate_loss_stress3[s] <- sum(severities_cat)
  } else {
    aggregate_loss_stress3[s] <- 0
  }
}

stress_ranges <- bind_rows(
  range_metrics(aggregate_loss) %>% mutate(scenario = "Baseline"),
  range_metrics(aggregate_loss_stress1) %>% mutate(scenario = "Severity Stress"),
  range_metrics(aggregate_loss_stress2) %>% mutate(scenario = "Frequency Stress"),
  range_metrics(aggregate_loss_stress3) %>% mutate(scenario = "Catastrophic")
) %>%
  dplyr::select(scenario, everything())


aggregate_loss_stress1 <- aggregate_loss_stress1 * severity_anchor_factor
aggregate_loss_stress2 <- aggregate_loss_stress2 * severity_anchor_factor
aggregate_loss_stress3 <- aggregate_loss_stress3 * severity_anchor_factor

# =============================
# 8) Short-term and long-term ranges
# =============================
short_term_costs <- stress_ranges %>%
  mutate(category = "Costs")

short_term_returns <- tibble(
  scenario = "Baseline",
  expected_value = total_portfolio_technical,
  variance = 0,
  sd = 0,
  var_95 = total_portfolio_technical,
  var_99 = total_portfolio_technical,
  tvar_99 = total_portfolio_technical,
  var_995 = total_portfolio_technical,
  var_999 = total_portfolio_technical,
  category = "Returns"
)

net_revenue_baseline <- total_portfolio_technical * (1 - expense_ratio) - aggregate_loss
net_revenue_stress1 <- total_portfolio_technical * (1 - expense_ratio) - aggregate_loss_stress1
net_revenue_stress2 <- total_portfolio_technical * (1 - expense_ratio) - aggregate_loss_stress2
net_revenue_stress3 <- total_portfolio_technical * (1 - expense_ratio) - aggregate_loss_stress3

short_term_net_revenue <- bind_rows(
  range_metrics(net_revenue_baseline) %>% mutate(scenario = "Baseline"),
  range_metrics(net_revenue_stress1) %>% mutate(scenario = "Severity Stress"),
  range_metrics(net_revenue_stress2) %>% mutate(scenario = "Frequency Stress"),
  range_metrics(net_revenue_stress3) %>% mutate(scenario = "Catastrophic")
) %>%
  mutate(category = "Net Revenue")

short_term_ranges <- bind_rows(short_term_costs, short_term_returns, short_term_net_revenue) %>%
  arrange(category, scenario)

# -------------------------------
# Long-term: 10-year projection using inflation + discounting
# -------------------------------

library(readxl)
library(here)

econ_raw <- read_excel(here("srcsc-2026-interest-and-inflation.xlsx"), skip = 2)

# Modify the function to accept a dataset

extract_inflation_forecast <- function(data, horizon = 10) {
  
  # Remove missing inflation values
  infl <- na.omit(data$Inflation)
  
  ts_infl <- ts(infl, frequency = 1)
  
  # Holt-Winters without seasonality (annual data)
  hw_fit <- HoltWinters(ts_infl, gamma = FALSE)
  
  fc <- forecast::forecast(hw_fit, h = horizon)
  
  data.frame(
    Year = (max(data$Year) + 1):(max(data$Year) + horizon),
    Forecast = as.numeric(fc$mean)
  )
}

inflation_projection <- extract_inflation_forecast(econ_raw, 10)



# Long-term: 10-year projection using inflation + discounting

econ_raw <- read_excel("C:/Users/khush/OneDrive - UNSW/Desktop/ACTL4001/KADAK/srcsc-2026-interest-and-inflation.xlsx", skip = 2)

rf_projection <- econ_forecast_hw(
  econ_raw,
  "1-Year Risk Free Annual Spot Rate",
  10
)

inflation_projection <- extract_inflation_forecast(
  econ_raw,
  10
)

econ_projection <- tibble(
  year = rf_projection$year,
  inflation = pmax(inflation_projection, -0.02),
  rf_1y = pmax(rf_projection$value, 0)
) %>%
  mutate(
    severity_trend_factor = cumprod(1 + inflation),
    discount_factor = 1 / cumprod(1 + rf_1y),
    trended_pure_premium = total_portfolio_pure * severity_trend_factor,
    trended_technical_premium = (trended_pure_premium + trended_pure_premium * risk_ratio) /
      (1 - expense_ratio - profit_ratio),
    pv_trended_technical_premium = trended_technical_premium * discount_factor
  )

# Long-term loss distribution: resample short-term losses and trend by inflation
years <- nrow(econ_projection)
loss_samples <- matrix(sample(aggregate_loss, n_sims * years, replace = TRUE), nrow = n_sims)
severity <- as.numeric(econ_projection$severity_trend_factor$Forecast)
loss_scaled <- sweep(loss_samples, 2, severity, "*")
long_term_loss <- rowSums(loss_scaled)

annual_premium <- as.numeric(econ_projection$trended_technical_premium$Forecast)

annual_expense <- annual_premium * expense_ratio

net_premium <- annual_premium - annual_expense

annual_net_revenue_samples <- sweep(loss_scaled, 2, net_premium, "-")
long_term_net_revenue <- rowSums(annual_net_revenue_samples)

long_term_costs <- range_metrics(long_term_loss) %>% mutate(category = "Costs", horizon = "10-year")
long_term_returns <- tibble(
  category = "Returns",
  horizon = "10-year",
  expected_value = sum(annual_premium),
  variance = 0,
  sd = 0,
  var_95 = sum(annual_premium),
  var_99 = sum(annual_premium),
  tvar_99 = sum(annual_premium),
  var_995 = sum(annual_premium),
  var_999 = sum(annual_premium)
)
long_term_net_revenue_summary <- range_metrics(long_term_net_revenue) %>%
  mutate(category = "Net Revenue", horizon = "10-year")

long_term_ranges <- bind_rows(long_term_costs, long_term_returns, long_term_net_revenue_summary) %>%
  arrange(category)

# Discounted (PV) long-term net revenue and returns
pv_premium_total <- sum(annual_premium * econ_projection$discount_factor)
pv_expense_total <- sum(annual_expense * econ_projection$discount_factor)
pv_loss_samples <- sweep(loss_scaled, 2, econ_projection$discount_factor, "*")
pv_long_term_loss <- rowSums(pv_loss_samples)
pv_long_term_net_revenue <- pv_premium_total - pv_expense_total - pv_long_term_loss

long_term_ranges_pv <- bind_rows(
  range_metrics(pv_long_term_loss) %>% mutate(category = "Costs", horizon = "10-year (PV)"),
  tibble(
    category = "Returns",
    horizon = "10-year (PV)",
    expected_value = pv_premium_total,
    variance = 0,
    sd = 0,
    var_95 = pv_premium_total,
    var_99 = pv_premium_total,
    tvar_99 = pv_premium_total,
    var_995 = pv_premium_total,
    var_999 = pv_premium_total
  ),
  range_metrics(pv_long_term_net_revenue) %>% mutate(category = "Net Revenue", horizon = "10-year (PV)")
)

# =============================
# 9) Summaries and validation
# =============================
loss_validation <- tibble(
  simulated_mean_loss_raw = mean(aggregate_loss_raw),
  simulated_mean_loss_anchored = mean(aggregate_loss),
  historical_total_loss = historical_total_loss,
  portfolio_pure_premium_raw = portfolio_pure_premium_raw,
  portfolio_pure_premium_anchored = total_portfolio_pure,
  severity_anchor_factor = severity_anchor_factor
)

portfolio_premium_summary <- tibble(
  total_policies = nrow(bi_freq_model),
  baseline_frequency = baseline_frequency,
  baseline_severity_raw = baseline_severity_raw,
  baseline_severity <- baseline_severity_raw * severity_anchor_factor,
  baseline_pure_premium = baseline_pure_premium,
  baseline_technical_premium = baseline_technical_premium,
  total_portfolio_pure_premium = total_portfolio_pure,
  total_portfolio_technical_premium = total_portfolio_technical,
  historical_total_loss = historical_total_loss,
  expected_loss_simulated = mean(aggregate_loss),
  var99_simulated = unname(quantile(aggregate_loss, 0.99)),
  tvar99_simulated = mean(aggregate_loss[aggregate_loss >= quantile(aggregate_loss, 0.99)])
)

baseline_premium_summary <- tibble(
  metric = c(
    "historical_total_loss",
    "historical_severity_mean",
    "baseline_count_mean",
    "baseline_frequency_per_exposure",
    "baseline_severity_raw",
    "severity_anchor_factor",
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
    baseline_frequency,
    baseline_severity_raw,
    severity_anchor_factor,
    baseline_severity,
    baseline_pure_premium,
    baseline_pure_premium * risk_ratio,
    expense_ratio,
    profit_ratio,
    risk_ratio,
    baseline_technical_premium
  )
)

# =============================
# 10) Write outputs
# =============================

# Set working directory
setwd("C:/Users/khush/OneDrive - UNSW/Desktop/ACTL4001/KADAK")

# Define outputs folder
output_dir <- "BI_outputs"  # this will create ACTL4001/KADAK/outputs

# Create the folder if it doesn't exist
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# Write CSVs to the folder using file.path()
write.csv(baseline_premium_summary, file.path(output_dir, "bi_baseline_premium_summary.csv"), row.names = FALSE)
write.csv(predictor_loading_table, file.path(output_dir, "bi_predictor_loading_table.csv"), row.names = FALSE)
write.csv(portfolio_premium_summary, file.path(output_dir, "bi_portfolio_premium_summary.csv"), row.names = FALSE)
write.csv(loss_validation, file.path(output_dir, "bi_loss_validation.csv"), row.names = FALSE)
write.csv(short_term_ranges, file.path(output_dir, "bi_short_term_ranges.csv"), row.names = FALSE)
write.csv(long_term_ranges, file.path(output_dir, "bi_long_term_ranges.csv"), row.names = FALSE)
write.csv(long_term_ranges_pv, file.path(output_dir, "bi_long_term_ranges_pv.csv"), row.names = FALSE)
write.csv(econ_projection, file.path(output_dir, "bi_econ_projection_10yr.csv"), row.names = FALSE)
