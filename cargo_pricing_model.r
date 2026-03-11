library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(MASS)
library(ggplot2)
library(VineCopula)
library(ismev)

set.seed(123)

# =============================
# Configuration
# =============================
expense_ratio <- 0.3
profit_ratio <- 0.07
risk_ratio <- 0.05
n_sims <- 50000
short_horizon <- 3
long_horizon <- 10
stress_sims <- 30000

# =============================
# Utility helpers
# =============================
clean_text_levels <- function(df) {
  df %>%
    mutate(across(
      where(~ is.character(.x) || is.factor(.x)),
      ~ str_squish(str_remove(as.character(.x), "_.*$"))
    ))
}

clamp <- function(x, min_val, max_val) {
  pmax(min_val, pmin(x, max_val))
}

summarize_upper <- function(x) {
  q95 <- as.numeric(quantile(x, 0.95, na.rm = TRUE))
  q99 <- as.numeric(quantile(x, 0.99, na.rm = TRUE))
  tvar99 <- mean(x[x >= q99], na.rm = TRUE)

  tibble(
    expected_value = mean(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    p95 = q95,
    p99 = q99,
    tail_mean_99 = tvar99,
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

summarize_lower <- function(x) {
  q05 <- as.numeric(quantile(x, 0.05, na.rm = TRUE))
  q01 <- as.numeric(quantile(x, 0.01, na.rm = TRUE))
  es01 <- mean(x[x <= q01], na.rm = TRUE)

  tibble(
    expected_value = mean(x, na.rm = TRUE),
    variance = var(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    p05 = q05,
    p01 = q01,
    downside_tail_mean_01 = es01,
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE)
  )
}

simulate_return_matrix <- function(
    n_sims,
    premium_cashflow,
    rf_base,
    rf_vol = 0.005,
    premium_vol = 0.03
) {
  horizon <- length(premium_cashflow)

  rate_shock <- matrix(rnorm(n_sims * horizon, mean = 0, sd = rf_vol), nrow = n_sims)
  premium_shock <- matrix(
    rlnorm(
      n_sims * horizon,
      meanlog = -0.5 * premium_vol^2,
      sdlog = premium_vol
    ),
    nrow = n_sims
  )

  premium_matrix <- matrix(rep(premium_cashflow, each = n_sims), nrow = n_sims) * premium_shock
  rf_matrix <- matrix(rep(rf_base, each = n_sims), nrow = n_sims) + rate_shock
  rf_matrix <- pmax(rf_matrix, -0.02)

  premium_matrix * (1 + rf_matrix)
}

fit_spliced_tail <- function(sev_values, tail_prob = 0.95) {
  sev_values <- sev_values[is.finite(sev_values) & sev_values > 0]
  threshold <- as.numeric(quantile(sev_values, tail_prob, na.rm = TRUE))

  bulk_values <- sev_values[sev_values <= threshold]
  excess_values <- sev_values[sev_values > threshold] - threshold

  p_tail <- length(excess_values) / length(sev_values)

  bulk_mean <- mean(bulk_values)
  bulk_var <- var(bulk_values)
  bulk_shape <- bulk_mean^2 / pmax(bulk_var, 1e-6)
  bulk_scale <- bulk_var / pmax(bulk_mean, 1e-6)

  gpd_fit <- NULL
  if (length(excess_values) >= 50) {
    gpd_fit <- tryCatch(
      suppressWarnings(gpd.fit(excess_values, threshold = 0, show = FALSE)),
      error = function(e) NULL
    )
  }

  if (!is.null(gpd_fit)) {
    gpd_scale <- pmax(unname(gpd_fit$mle[1]), 1e-6)
    gpd_shape <- unname(gpd_fit$mle[2])
  } else {
    gpd_scale <- pmax(mean(excess_values), 1)
    gpd_shape <- 0
  }

  if (!is.finite(gpd_shape)) gpd_shape <- 0
  if (gpd_shape >= 0.49) gpd_shape <- 0.49
  if (gpd_shape <= -0.4) gpd_shape <- -0.4

  list(
    threshold = threshold,
    p_tail = p_tail,
    bulk_shape = bulk_shape,
    bulk_scale = bulk_scale,
    gpd_scale = gpd_scale,
    gpd_shape = gpd_shape
  )
}

# =============================
# Data + model pipeline
# =============================
load_cargo_claims <- function(exclude_types = character()) {
  cargo_freq_raw <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 1)
  cargo_sev_raw <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 2)

  cargo_freq <- cargo_freq_raw %>%
    clean_text_levels() %>%
    filter(
      !is.na(distance) & distance >= 1 & distance <= 100,
      !is.na(cargo_value) & cargo_value >= 50000 & cargo_value <= 680000000,
      !is.na(transit_duration) & transit_duration >= 1 & transit_duration <= 60,
      !is.na(route_risk) & route_risk >= 1 & route_risk <= 5,
      !is.na(pilot_experience) & pilot_experience >= 1 & pilot_experience <= 30,
      !is.na(vessel_age) & vessel_age >= 1 & vessel_age <= 50,
      !is.na(weight) & weight >= 1500 & weight <= 250000,
      !is.na(solar_radiation) & solar_radiation >= 0 & solar_radiation <= 1,
      !is.na(debris_density) & debris_density >= 0 & debris_density <= 1,
      !is.na(exposure) & exposure > 0 & exposure <= 1,
      !is.na(claim_count) & claim_count >= 0 & claim_count <= 5
    )

  cargo_sev <- cargo_sev_raw %>%
    clean_text_levels() %>%
    filter(
      !is.na(distance) & distance >= 1 & distance <= 100,
      !is.na(cargo_value) & cargo_value >= 50000 & cargo_value <= 680000000,
      !is.na(transit_duration) & transit_duration >= 1 & transit_duration <= 60,
      !is.na(route_risk) & route_risk >= 1 & route_risk <= 5,
      !is.na(pilot_experience) & pilot_experience >= 1 & pilot_experience <= 30,
      !is.na(vessel_age) & vessel_age >= 1 & vessel_age <= 50,
      !is.na(weight) & weight >= 1500 & weight <= 250000,
      !is.na(solar_radiation) & solar_radiation >= 0 & solar_radiation <= 1,
      !is.na(debris_density) & debris_density >= 0 & debris_density <= 1,
      !is.na(exposure) & exposure > 0 & exposure <= 1,
      !is.na(claim_amount) & claim_amount >= 31000 & claim_amount <= 678000000
    )

  if (length(exclude_types) > 0) {
    cargo_freq <- cargo_freq %>% filter(!(cargo_type %in% exclude_types))
    cargo_sev <- cargo_sev %>% filter(!(cargo_type %in% exclude_types))
  }

  list(freq = cargo_freq, sev = cargo_sev)
}

fit_cargo_models <- function(cargo_freq, cargo_sev) {
  model_cargo_freq <- cargo_freq %>%
    mutate(
      claim_count = round(claim_count),
      cargo_type = factor(cargo_type),
      container_type = factor(container_type)
    )

  model_cargo_sev_raw <- cargo_sev %>%
    mutate(
      cargo_type = factor(cargo_type, levels = levels(model_cargo_freq$cargo_type)),
      container_type = factor(container_type, levels = levels(model_cargo_freq$container_type)),
      claim_amount = pmax(claim_amount, 0.01)
    ) %>%
    filter(!is.na(cargo_type), !is.na(container_type))

  freq_formula <- claim_count ~ route_risk + pilot_experience + solar_radiation + debris_density +
    container_type + cargo_type + cargo_value + offset(log(exposure))

  freq_model_poisson <- glm(freq_formula, family = poisson(link = "log"), data = model_cargo_freq)
  freq_model_nb <- glm.nb(freq_formula, data = model_cargo_freq)

  numeric_vars <- c(
    "route_risk", "solar_radiation", "debris_density", "cargo_value", "weight",
    "vessel_age", "distance", "transit_duration", "pilot_experience"
  )

  sev_centers <- sapply(model_cargo_sev_raw[numeric_vars], mean, na.rm = TRUE)
  sev_scales <- sapply(model_cargo_sev_raw[numeric_vars], sd, na.rm = TRUE)
  sev_scales[sev_scales <= 0 | is.na(sev_scales)] <- 1

  model_cargo_sev <- model_cargo_sev_raw
  for (v in numeric_vars) {
    model_cargo_sev[[v]] <- (model_cargo_sev[[v]] - sev_centers[[v]]) / sev_scales[[v]]
  }

  sev_model <- glm(
    claim_amount ~ route_risk + solar_radiation + debris_density +
      cargo_value + weight + vessel_age + distance + transit_duration +
      pilot_experience + container_type + cargo_type,
    family = Gamma(link = "log"),
    data = model_cargo_sev
  )

  lambda_hat <- predict(freq_model_nb, type = "response")
  theta_model <- freq_model_nb$theta

  # Moment-matched NB for total claims: aligns with cargo_loss.r structure but efficient.
  mu_total <- sum(lambda_hat)
  var_total <- sum(lambda_hat + (lambda_hat^2) / theta_model)
  theta_eff <- ifelse(var_total > mu_total, mu_total^2 / (var_total - mu_total), 1e6)
  theta_eff <- pmax(theta_eff, 1e-6)

  sev_shape <- 1 / summary(sev_model)$dispersion
  sev_scale <- mean(model_cargo_sev_raw$claim_amount, na.rm = TRUE) / sev_shape

  tail_fit <- fit_spliced_tail(model_cargo_sev_raw$claim_amount, tail_prob = 0.95)

  list(
    model_cargo_freq = model_cargo_freq,
    model_cargo_sev_raw = model_cargo_sev_raw,
    model_cargo_sev = model_cargo_sev,
    freq_model_poisson = freq_model_poisson,
    freq_model_nb = freq_model_nb,
    sev_model = sev_model,
    numeric_vars = numeric_vars,
    sev_centers = sev_centers,
    sev_scales = sev_scales,
    lambda_hat = lambda_hat,
    theta_model = theta_model,
    mu_total = mu_total,
    var_total = var_total,
    theta_eff = theta_eff,
    sev_shape = sev_shape,
    sev_scale = sev_scale,
    tail_fit = tail_fit,
    total_exposure = sum(model_cargo_freq$exposure, na.rm = TRUE)
  )
}

fit_frequency_severity_copula <- function(fit_obj, max_obs = 60000) {
  freq_pos <- fit_obj$model_cargo_freq$claim_count
  freq_pos <- freq_pos[freq_pos > 0]

  sev_vals <- fit_obj$model_cargo_sev_raw$claim_amount
  n <- min(length(freq_pos), length(sev_vals), max_obs)

  dep_data <- data.frame(
    freq = freq_pos[seq_len(n)],
    sev = sev_vals[seq_len(n)]
  )

  u <- rank(dep_data$freq) / (n + 1)
  v <- rank(dep_data$sev) / (n + 1)

  fit_cop <- tryCatch(
    BiCopSelect(u, v, familyset = NA),
    error = function(e) NULL
  )

  if (!is.null(fit_cop)) {
    list(
      method = "VineCopula",
      family = fit_cop$family,
      family_name = fit_cop$familyname,
      par = fit_cop$par,
      par2 = fit_cop$par2,
      tau = fit_cop$tau,
      u = u,
      v = v
    )
  } else {
    rho <- cor(qnorm(u), qnorm(v), use = "complete.obs")
    list(
      method = "GaussianFallback",
      family = NA,
      family_name = "GaussianFallback",
      par = rho,
      par2 = NA_real_,
      tau = cor(u, v, method = "kendall"),
      u = u,
      v = v
    )
  }
}

simulate_uv_from_copula <- function(copula_obj, n_sims) {
  if (copula_obj$method == "VineCopula") {
    BiCopSim(n_sims, copula_obj$family, copula_obj$par, copula_obj$par2)
  } else {
    rho <- clamp(copula_obj$par, -0.95, 0.95)
    z <- MASS::mvrnorm(n_sims, mu = c(0, 0), Sigma = matrix(c(1, rho, rho, 1), 2))
    cbind(pnorm(z[, 1]), pnorm(z[, 2]))
  }
}

simulate_aggregate_losses <- function(
    fit_obj,
    n_sims,
    freq_multiplier = 1,
    sev_multiplier = 1,
    uv = NULL,
    catastrophe_prob = 0,
    catastrophe_freq_mult = 1,
    catastrophe_sev_mult = 1
) {
  mu <- pmax(fit_obj$mu_total * freq_multiplier, 1e-9)
  theta <- pmax(fit_obj$theta_eff, 1e-6)

  if (is.null(uv)) {
    claim_counts <- rnbinom(n_sims, size = theta, mu = mu)
    sev_mult_sim <- rep(sev_multiplier, n_sims)
  } else {
    claim_counts <- qnbinom(uv[, 1], size = theta, mu = mu)
    sev_dep <- qgamma(uv[, 2], shape = fit_obj$sev_shape, scale = 1 / fit_obj$sev_shape)
    sev_mult_sim <- sev_multiplier * sev_dep
  }

  if (catastrophe_prob > 0) {
    flag <- rbinom(n_sims, size = 1, prob = clamp(catastrophe_prob, 0, 1))
    claim_counts <- round(claim_counts * (1 + (catastrophe_freq_mult - 1) * flag))
    sev_mult_sim <- sev_mult_sim * (1 + (catastrophe_sev_mult - 1) * flag)
  }

  agg <- numeric(n_sims)
  idx <- claim_counts > 0

  if (any(idx)) {
    # Equivalent to summing Gamma severities across total claims, but vectorized.
    agg[idx] <- rgamma(
      sum(idx),
      shape = pmax(claim_counts[idx] * fit_obj$sev_shape, 1e-9),
      scale = fit_obj$sev_scale * sev_mult_sim[idx]
    )
  }

  agg
}

build_horizon_summary <- function(cost_matrix, return_matrix, annual_premium, scenario_name, short_horizon, long_horizon) {
  summarize_h <- function(h, h_label) {
    costs <- rowSums(cost_matrix[, 1:h, drop = FALSE])
    returns <- rowSums(return_matrix[, 1:h, drop = FALSE])
    net <- returns - costs

    bind_rows(
      summarize_upper(costs) %>% mutate(stream = "costs"),
      summarize_upper(returns) %>% mutate(stream = "returns"),
      summarize_lower(net) %>% mutate(stream = "net_revenue")
    ) %>%
      mutate(
        scenario = scenario_name,
        horizon = h_label,
        annual_technical_premium = annual_premium,
        .before = 1
      )
  }

  bind_rows(
    summarize_h(short_horizon, "short_term_3y"),
    summarize_h(long_horizon, "long_term_10y")
  )
}

run_stress_suite <- function(fit_obj, output_prefix, n_sim = stress_sims) {
  test_levels <- list(
    Frequency = c(0.85, 1.00, 1.15, 1.30, 1.50),
    Severity = c(0.85, 1.00, 1.15, 1.30, 1.50),
    Route_Risk = c(0.90, 1.00, 1.10, 1.20, 1.35),
    Solar_Radiation = c(0.85, 1.00, 1.15, 1.30, 1.45),
    Debris_Density = c(0.85, 1.00, 1.15, 1.30, 1.45),
    Inflation = c(1.00, 1.05, 1.10, 1.20, 1.30, 1.40),
    Dependence_Rho = c(0.00, 0.20, 0.40, 0.60, 0.80),
    Catastrophe_Probability = c(0.000, 0.005, 0.010, 0.020, 0.030)
  )

  summarize_stress <- function(losses, test, stress_factor) {
    var_99 <- as.numeric(quantile(losses, 0.99, na.rm = TRUE))

    tibble(
      test = test,
      stress_factor = stress_factor,
      mean_loss = mean(losses, na.rm = TRUE),
      variance_loss = var(losses, na.rm = TRUE),
      VaR_95 = as.numeric(quantile(losses, 0.95, na.rm = TRUE)),
      VaR_99 = var_99,
      TVaR_99 = mean(losses[losses >= var_99], na.rm = TRUE)
    )
  }

  results <- list()

  results$Frequency <- bind_rows(lapply(test_levels$Frequency, function(f) {
    losses <- simulate_aggregate_losses(fit_obj, n_sim, freq_multiplier = f, sev_multiplier = 1)
    summarize_stress(losses, "Frequency", f)
  }))

  results$Severity <- bind_rows(lapply(test_levels$Severity, function(s) {
    losses <- simulate_aggregate_losses(fit_obj, n_sim, freq_multiplier = 1, sev_multiplier = s)
    summarize_stress(losses, "Severity", s)
  }))

  results$Route_Risk <- bind_rows(lapply(test_levels$Route_Risk, function(r) {
    losses <- simulate_aggregate_losses(
      fit_obj,
      n_sim,
      freq_multiplier = r,
      sev_multiplier = 1 + 0.25 * (r - 1)
    )
    summarize_stress(losses, "Route Risk", r)
  }))

  results$Solar_Radiation <- bind_rows(lapply(test_levels$Solar_Radiation, function(s) {
    losses <- simulate_aggregate_losses(
      fit_obj,
      n_sim,
      freq_multiplier = 1 + 0.35 * (s - 1),
      sev_multiplier = 1 + 0.20 * (s - 1)
    )
    summarize_stress(losses, "Solar Radiation", s)
  }))

  results$Debris_Density <- bind_rows(lapply(test_levels$Debris_Density, function(d) {
    losses <- simulate_aggregate_losses(
      fit_obj,
      n_sim,
      freq_multiplier = 1 + 0.45 * (d - 1),
      sev_multiplier = 1 + 0.25 * (d - 1)
    )
    summarize_stress(losses, "Debris Density", d)
  }))

  results$Inflation <- bind_rows(lapply(test_levels$Inflation, function(i) {
    losses <- simulate_aggregate_losses(fit_obj, n_sim, freq_multiplier = 1, sev_multiplier = i)
    summarize_stress(losses, "Claims Inflation", i)
  }))

  results$Dependence_Rho <- bind_rows(lapply(test_levels$Dependence_Rho, function(rho) {
    z <- MASS::mvrnorm(n_sim, mu = c(0, 0), Sigma = matrix(c(1, rho, rho, 1), 2))
    uv <- cbind(pnorm(z[, 1]), pnorm(z[, 2]))
    losses <- simulate_aggregate_losses(fit_obj, n_sim, uv = uv)
    summarize_stress(losses, "Freq-Sev Dependence", rho)
  }))

  results$Catastrophe_Probability <- bind_rows(lapply(test_levels$Catastrophe_Probability, function(cp) {
    losses <- simulate_aggregate_losses(
      fit_obj,
      n_sim,
      uv = simulate_uv_from_copula(fit_obj$copula_fit, n_sim),
      catastrophe_prob = cp,
      catastrophe_freq_mult = 2.2,
      catastrophe_sev_mult = 2.8
    )
    summarize_stress(losses, "Catastrophe Probability", cp)
  }))

  stress_results <- bind_rows(results)

  stress_ranges <- stress_results %>%
    group_by(test) %>%
    summarise(
      min_mean_loss = min(mean_loss, na.rm = TRUE),
      max_mean_loss = max(mean_loss, na.rm = TRUE),
      min_VaR_99 = min(VaR_99, na.rm = TRUE),
      max_VaR_99 = max(VaR_99, na.rm = TRUE),
      min_TVaR_99 = min(TVaR_99, na.rm = TRUE),
      max_TVaR_99 = max(TVaR_99, na.rm = TRUE),
      .groups = "drop"
    )

  write.csv(stress_results, paste0("CL_outputs/", output_prefix, "_stress_test_results.csv"), row.names = FALSE)
  write.csv(stress_ranges, paste0("CL_outputs/", output_prefix, "_stress_test_ranges.csv"), row.names = FALSE)

  png(paste0("CL_outputs/", output_prefix, "_stress_var99.png"), width = 1400, height = 900)
  ggplot(stress_results, aes(x = stress_factor, y = VaR_99, color = test)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.8) +
    labs(
      title = paste0("", output_prefix, " Stress Testing Impact on VaR (99%)"),
      x = "Stress Factor",
      y = "VaR 99%"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom")
  dev.off()

  list(results = stress_results, ranges = stress_ranges)
}

# =============================
# Full cargo pricing run
# =============================
run_cargo_pipeline <- function(
    exclude_types = character(),
    output_prefix = "cargo",
    coverage_note = "",
    run_stress = TRUE
) {
  dir.create("CL_outputs", showWarnings = FALSE)

  claims <- load_cargo_claims(exclude_types = exclude_types)
  fit_obj <- fit_cargo_models(claims$freq, claims$sev)

  copula_fit <- fit_frequency_severity_copula(fit_obj)
  fit_obj$copula_fit <- copula_fit

  baseline_losses <- simulate_aggregate_losses(fit_obj, n_sims = n_sims)

  # Alignment check versus cargo_loss.r style Monte Carlo.
  # This uses per-policy NB simulation of counts before aggregation.
  n_check <- 200
  exact_style_losses <- numeric(n_check)
  n_pol <- length(fit_obj$lambda_hat)

  for (i in seq_len(n_check)) {
    n_total <- sum(rnbinom(n_pol, size = fit_obj$theta_model, mu = fit_obj$lambda_hat))
    if (n_total > 0) {
      exact_style_losses[i] <- rgamma(
        1,
        shape = pmax(n_total * fit_obj$sev_shape, 1e-9),
        scale = fit_obj$sev_scale
      )
    }
  }

  # Dependency scenario from fitted copula
  uv_cop <- simulate_uv_from_copula(copula_fit, n_sims)
  copula_losses <- simulate_aggregate_losses(fit_obj, n_sims = n_sims, uv = uv_cop)

  annual_expected_cost <- mean(baseline_losses)
  pure_premium_rate <- annual_expected_cost / fit_obj$total_exposure
  technical_premium_rate <- (pure_premium_rate * (1 + risk_ratio)) / (1 - expense_ratio - profit_ratio)

  annual_pure_premium <- pure_premium_rate * fit_obj$total_exposure
  annual_technical_premium <- technical_premium_rate * fit_obj$total_exposure

  source("inflation_forecast_cargo.R")
  econ_projection <- forecast_inflation_and_rates(
    path = "srcsc-2026-interest-and-inflation.xlsx",
    horizon = long_horizon
  )

  econ_projection <- econ_projection %>%
    mutate(
      severity_trend_factor = cumprod(1 + inflation),
      discount_factor = 1 / cumprod(1 + rf_1y),
      premium_cashflow = annual_technical_premium * severity_trend_factor,
      return_cashflow = premium_cashflow * (1 + rf_1y),
      expected_cost_cashflow = annual_expected_cost * severity_trend_factor,
      expected_net_revenue = return_cashflow - expected_cost_cashflow,
      pv_expected_net_revenue = expected_net_revenue * discount_factor,
      horizon_bucket = if_else(row_number() <= short_horizon, "short_term", "long_term")
    )

  return_rate_vol <- pmax(sd(econ_projection$rf_1y, na.rm = TRUE), 0.005)

  cost_matrix <- sweep(
    matrix(baseline_losses, nrow = n_sims, ncol = long_horizon),
    MARGIN = 2,
    STATS = econ_projection$severity_trend_factor,
    FUN = "*"
  )

  return_matrix <- simulate_return_matrix(
    n_sims = n_sims,
    premium_cashflow = econ_projection$premium_cashflow,
    rf_base = econ_projection$rf_1y,
    rf_vol = return_rate_vol,
    premium_vol = 0.03
  )

  baseline_horizon_summary <- build_horizon_summary(
    cost_matrix,
    return_matrix,
    annual_premium = annual_technical_premium,
    scenario_name = "Baseline",
    short_horizon = short_horizon,
    long_horizon = long_horizon
  )

  scenario_def <- tibble(
    scenario = c("Best_Case", "Moderate_Case", "Worst_Case"),
    freq_mult = c(0.85, 1.00, 1.40),
    sev_mult = c(0.90, 1.00, 1.50),
    cat_prob = c(0.00, 0.005, 0.020),
    cat_freq_mult = c(1.00, 1.50, 2.50),
    cat_sev_mult = c(1.00, 1.50, 3.00)
  )

  scenario_ranges <- bind_rows(lapply(seq_len(nrow(scenario_def)), function(i) {
    sc <- scenario_def[i, ]

    uv <- simulate_uv_from_copula(copula_fit, n_sims)

    sc_losses <- simulate_aggregate_losses(
      fit_obj,
      n_sims = n_sims,
      freq_multiplier = sc$freq_mult,
      sev_multiplier = sc$sev_mult,
      uv = uv,
      catastrophe_prob = sc$cat_prob,
      catastrophe_freq_mult = sc$cat_freq_mult,
      catastrophe_sev_mult = sc$cat_sev_mult
    )

    sc_premium <- annual_technical_premium * sc$freq_mult * sc$sev_mult

    sc_econ <- econ_projection %>%
      mutate(premium_cashflow = sc_premium * severity_trend_factor)

    sc_cost_matrix <- sweep(
      matrix(sc_losses, nrow = n_sims, ncol = long_horizon),
      MARGIN = 2,
      STATS = sc_econ$severity_trend_factor,
      FUN = "*"
    )

    sc_return_matrix <- simulate_return_matrix(
      n_sims = n_sims,
      premium_cashflow = sc_econ$premium_cashflow,
      rf_base = sc_econ$rf_1y,
      rf_vol = return_rate_vol,
      premium_vol = 0.03
    )

    build_horizon_summary(
      sc_cost_matrix,
      sc_return_matrix,
      annual_premium = sc_premium,
      scenario_name = sc$scenario,
      short_horizon = short_horizon,
      long_horizon = long_horizon
    )
  }))

  sev_total <- sum(fit_obj$model_cargo_sev_raw$claim_amount, na.rm = TRUE)
  precious_share <- fit_obj$model_cargo_sev_raw %>%
    filter(cargo_type %in% c("gold", "platinum")) %>%
    summarise(share = sum(claim_amount, na.rm = TRUE) / sev_total) %>%
    pull(share)

  baseline_pricing_summary <- tibble(
    metric = c(
      "records_frequency", "records_severity", "total_exposure",
      "expected_claim_count_portfolio", "var_claim_count_portfolio",
      "theta_model", "theta_effective",
      "sev_shape", "sev_scale", "mean_severity",
      "expected_annual_cost", "annual_pure_premium", "annual_technical_premium",
      "pure_premium_rate_per_exposure", "technical_premium_rate_per_exposure",
      "expense_ratio", "profit_ratio", "risk_ratio",
      "tail_threshold_95", "tail_probability", "gpd_shape", "gpd_scale",
      "gold_platinum_severity_share"
    ),
    value = c(
      nrow(fit_obj$model_cargo_freq),
      nrow(fit_obj$model_cargo_sev_raw),
      fit_obj$total_exposure,
      fit_obj$mu_total,
      fit_obj$var_total,
      fit_obj$theta_model,
      fit_obj$theta_eff,
      fit_obj$sev_shape,
      fit_obj$sev_scale,
      mean(fit_obj$model_cargo_sev_raw$claim_amount, na.rm = TRUE),
      annual_expected_cost,
      annual_pure_premium,
      annual_technical_premium,
      pure_premium_rate,
      technical_premium_rate,
      expense_ratio,
      profit_ratio,
      risk_ratio,
      fit_obj$tail_fit$threshold,
      fit_obj$tail_fit$p_tail,
      fit_obj$tail_fit$gpd_shape,
      fit_obj$tail_fit$gpd_scale,
      precious_share
    )
  )

  mc_alignment_check <- tibble(
    metric = c(
      "frequency_mean_exact",
      "frequency_mean_effective_nb",
      "frequency_var_exact",
      "frequency_var_effective_nb",
      "loss_mean_exact_style_small_sim",
      "loss_mean_effective_nb_main_sim",
      "loss_mean_relative_gap"
    ),
    value = c(
      fit_obj$mu_total,
      fit_obj$mu_total,
      fit_obj$var_total,
      fit_obj$mu_total + fit_obj$mu_total^2 / fit_obj$theta_eff,
      mean(exact_style_losses, na.rm = TRUE),
      mean(baseline_losses, na.rm = TRUE),
      (mean(baseline_losses, na.rm = TRUE) - mean(exact_style_losses, na.rm = TRUE)) /
        pmax(mean(exact_style_losses, na.rm = TRUE), 1e-9)
    )
  )

  frequency_model_diagnostics <- tibble(
    metric = c("poisson_aic", "nb_aic", "poisson_bic", "nb_bic", "observed_zero_rate", "predicted_zero_rate_nb"),
    value = c(
      AIC(fit_obj$freq_model_poisson),
      AIC(fit_obj$freq_model_nb),
      BIC(fit_obj$freq_model_poisson),
      BIC(fit_obj$freq_model_nb),
      mean(fit_obj$model_cargo_freq$claim_count == 0, na.rm = TRUE),
      mean(dnbinom(0, size = fit_obj$theta_model, mu = fit_obj$lambda_hat), na.rm = TRUE)
    )
  )

  severity_model_diagnostics <- tibble(
    metric = c("gamma_dispersion", "observed_mean_severity", "simulated_mean_loss", "simulated_var99", "simulated_tvar99", "copula_mean_loss", "copula_var99", "copula_tvar99"),
    value = c(
      summary(fit_obj$sev_model)$dispersion,
      mean(fit_obj$model_cargo_sev_raw$claim_amount, na.rm = TRUE),
      mean(baseline_losses),
      quantile(baseline_losses, 0.99),
      mean(baseline_losses[baseline_losses >= quantile(baseline_losses, 0.99)]),
      mean(copula_losses),
      quantile(copula_losses, 0.99),
      mean(copula_losses[copula_losses >= quantile(copula_losses, 0.99)])
    )
  )

  copula_diagnostics <- tibble(
    metric = c("method", "family", "par", "par2", "tau"),
    value_text = c(
      copula_fit$method,
      copula_fit$family_name,
      as.character(copula_fit$par),
      as.character(copula_fit$par2),
      as.character(copula_fit$tau)
    )
  )

  copula_loss_metrics <- summarize_upper(copula_losses)

  solar_system_risk_mapping <- tibble(
    solar_system = c("Helionis", "Bayesia", "Oryn Delta"),
    resources_profile = c(
      "High-density metallic asteroid clusters; stable terrestrial and rocky operations",
      "Mapped asteroid belt with stable routes and high-gravity mining planet",
      "Rare-metal extraction in broad asymmetric asteroid ring"
    ),
    enc_challenges = c(
      "Erratic asteroid drift, micro-collisions, shifting debris clouds, relay repositioning",
      "Radiation spikes from binary-star alignments, ambient radiation, temperature extremes",
      "Low-visibility environment, sporadic flare risk, orbital shear and gravitational gradients"
    ),
    cargo_pricing_implications = c(
      "Debris-density and route-risk trigger bands drive deductible/loading",
      "Radiation-trigger endorsements and temporary hazard surcharges",
      "Highest catastrophe loading, stricter routing controls, higher co-insurance"
    )
  )

  precious_exclusion_text <- ifelse(
    length(exclude_types) > 0,
    paste0("Excluded from base cover: ", paste(exclude_types, collapse = ", ")),
    "Base cover includes all cargo types; optional precious-metals rider recommended"
  )

  product_design_features <- tibble(
    solar_system = c("Helionis", "Bayesia", "Oryn Delta"),
    benefit_structure = c(
      "Base cargo indemnity with debris-tier deductible and route-risk loading",
      "Cargo indemnity with radiation-event sublimit and short-term hazard surcharge",
      "Layered indemnity: attritional layer + catastrophe excess layer"
    ),
    coverage_triggers = c(
      "Debris-density band jump, route-risk tier jump, transit duration breach",
      "Binary-star radiation spike index, thin-magnetosphere alert threshold",
      "Orbital-shear alert, gravitational-gradient spike, communication blackout escalation"
    ),
    exclusions = c(
      "Unapproved route changes through unstable debris corridors",
      "Non-compliant shielding during known radiation windows",
      "Deep-ring missions without relay/drone telemetry compliance"
    ),
    scalability_adaptation = c(
      "Quarterly repricing from rolling debris/routing telemetry",
      "Dynamic radiation surcharge tied to forecasted alignment windows",
      "Adjust catastrophe limits as rare-metal extraction footprint expands"
    ),
    precious_metals_position = precious_exclusion_text,
    source = "ENC + Case Study"
  )

  if (run_stress) {
    stress_out <- run_stress_suite(fit_obj, output_prefix = output_prefix, n_sim = stress_sims)
  } else {
    stress_out <- list(results = tibble(), ranges = tibble())
  }

  threat_table <- stress_out$ranges %>%
    mutate(
      impact_ratio_var99 = max_VaR_99 / min_VaR_99,
      rank = dense_rank(desc(impact_ratio_var99))
    ) %>%
    arrange(rank, desc(max_VaR_99)) %>%
    transmute(
      rank,
      threat = test,
      min_var99 = min_VaR_99,
      max_var99 = max_VaR_99,
      impact_ratio_var99
    )

  case_study_checklist <- tibble(
    deliverable_item = c(
      "Aggregate loss distributions",
      "Short/long-term costs returns net revenue ranges",
      "Expected/variance/tail statistics",
      "Stress testing for extreme scenarios",
      "Dependency-based correlated scenarios",
      "Best/moderate/worst case scenario table",
      "Threat table ranking top threats",
      "Solar-system risk narrative mapping",
      "Product design: benefits/triggers/exclusions/scalability"
    ),
    cargo_output_file = c(
      paste0(output_prefix, "_baseline_pricing_summary.csv"),
      paste0(output_prefix, "_horizon_summary_baseline.csv"),
      paste0(output_prefix, "_horizon_summary_baseline.csv"),
      paste0(output_prefix, "_stress_test_results.csv"),
      paste0(output_prefix, "_copula_diagnostics.csv"),
      paste0(output_prefix, "_scenario_ranges.csv"),
      paste0(output_prefix, "_threat_table.csv"),
      paste0(output_prefix, "_solar_system_risk_mapping.csv"),
      paste0(output_prefix, "_product_design_features.csv")
    )
  )

  four_hazard_integration_template <- tibble(
    hazard_area = c("Cargo Loss", "Workers' Compensation", "Equipment Failure", "Business Interruption"),
    recommended_product_status = c(
      ifelse(length(exclude_types) > 0, "Cargo base cover excluding precious metals", "Cargo base cover with optional precious-metal rider"),
      "Use existing workers model outputs",
      "Model still to be built",
      "Model still to be built"
    ),
    pricing_distribution_file = c(
      paste0(output_prefix, "_baseline_pricing_summary.csv"),
      "portfolio_risk_metrics.csv",
      "pending",
      "pending"
    ),
    stress_testing_file = c(
      paste0(output_prefix, "_stress_test_results.csv"),
      "(existing worker stress results)",
      "pending",
      "pending"
    ),
    product_design_file = c(
      paste0(output_prefix, "_product_design_features.csv"),
      "(workers product design section in report)",
      "pending",
      "pending"
    ),
    seamless_report_link = c(
      "Map route/debris/radiation drivers to system-specific terms",
      "Map safety/training/psych drivers to same solar-system context",
      "Use same short/long cost-return-net structure",
      "Use same short/long cost-return-net structure"
    )
    
  )
  
  # =============================
  # Portfolio Risk Metrics (Cargo)
  # =============================
  
  var95 <- unname(quantile(baseline_losses, 0.95))
  var99 <- unname(quantile(baseline_losses, 0.99))
  tvar99 <- mean(baseline_losses[baseline_losses >= var99])
  
  loss_validation <- tibble(
    simulated_mean_loss = mean(baseline_losses),
    model_expected_loss = annual_expected_cost,
    difference = mean(baseline_losses) - annual_expected_cost
  )
  
  tail_gap_99 <- max(tvar99 - mean(baseline_losses), 0)
  
  current_risk_margin_amount <- risk_ratio * annual_pure_premium
  
  risk_adequate <- current_risk_margin_amount >= tail_gap_99
  
  suggested_risk_ratio <- ifelse(
    annual_pure_premium > 0,
    max(risk_ratio, tail_gap_99 / annual_pure_premium),
    risk_ratio
  )
  
  cargo_portfolio_risk_metrics <- tibble(
    metric = c(
      "mean_loss",
      "sd_loss",
      "var_95",
      "var_99",
      "tvar_99",
      "model_expected_loss",
      "simulation_minus_model_expected",
      "tail_gap_99",
      "current_risk_margin_amount",
      "risk_margin_adequate",
      "suggested_risk_ratio"
    ),
    value = c(
      mean(baseline_losses),
      sd(baseline_losses),
      var95,
      var99,
      tvar99,
      annual_expected_cost,
      mean(baseline_losses) - annual_expected_cost,
      tail_gap_99,
      current_risk_margin_amount,
      as.numeric(risk_adequate),
      suggested_risk_ratio
    )
  )

  # -----------------------------
  # Write outputs
  # -----------------------------
  write.csv(baseline_pricing_summary, paste0("CL_outputs/", output_prefix, "_baseline_pricing_summary.csv"), row.names = FALSE)
  write.csv(frequency_model_diagnostics, paste0("CL_outputs/", output_prefix, "_frequency_model_diagnostics.csv"), row.names = FALSE)
  write.csv(severity_model_diagnostics, paste0("CL_outputs/", output_prefix, "_severity_model_diagnostics.csv"), row.names = FALSE)
  write.csv(copula_diagnostics, paste0("CL_outputs/", output_prefix, "_copula_diagnostics.csv"), row.names = FALSE)
  write.csv(copula_loss_metrics, paste0("CL_outputs/", output_prefix, "_copula_loss_metrics.csv"), row.names = FALSE)
  write.csv(econ_projection, paste0("CL_outputs/", output_prefix, "_cashflow_projection_10yr.csv"), row.names = FALSE)
  write.csv(baseline_horizon_summary, paste0("CL_outputs/", output_prefix, "_horizon_summary_baseline.csv"), row.names = FALSE)
  write.csv(scenario_ranges, paste0("CL_outputs/", output_prefix, "_scenario_ranges.csv"), row.names = FALSE)
  write.csv(product_design_features, paste0("CL_outputs/", output_prefix, "_product_design_features.csv"), row.names = FALSE)
  write.csv(threat_table, paste0("CL_outputs/", output_prefix, "_threat_table.csv"), row.names = FALSE)
  write.csv(solar_system_risk_mapping, paste0("CL_outputs/", output_prefix, "_solar_system_risk_mapping.csv"), row.names = FALSE)
  write.csv(case_study_checklist, paste0("CL_outputs/", output_prefix, "_case_study_checklist.csv"), row.names = FALSE)
  write.csv(four_hazard_integration_template, paste0("CL_outputs/", output_prefix, "_four_hazard_integration_template.csv"), row.names = FALSE)
  write.csv(mc_alignment_check, paste0("CL_outputs/", output_prefix, "_mc_alignment_check.csv"), row.names = FALSE)
  write.csv(cargo_portfolio_risk_metrics, paste0("CL_outputs/", output_prefix, "_portfolio_risk_metrics.csv"), row.names = FALSE)

  # Keep default cargo filenames updated for report use
  if (output_prefix == "cargo") {
    write.csv(product_design_features, "CL_outputs/cargo_product_design_features.csv", row.names = FALSE)
  }

  png(paste0("CL_outputs/", output_prefix, "_portfolio_loss_distribution.png"), width = 1200, height = 800)
  hist(
    baseline_losses,
    breaks = 100,
    main = paste0(output_prefix, " Aggregate Loss Distribution (MC aligned to cargo_loss.r)"),
    xlab = "Aggregate Loss",
    col = "lightblue"
  )
  abline(v = quantile(baseline_losses, 0.95), col = "orange", lwd = 2)
  abline(v = quantile(baseline_losses, 0.99), col = "red", lwd = 2)
  dev.off()

  png(paste0("CL_outputs/", output_prefix, "_copula_loss_distribution.png"), width = 1200, height = 800)
  hist(
    copula_losses,
    breaks = 100,
    main = paste0(output_prefix, " Copula-Dependent Loss Distribution"),
    xlab = "Aggregate Loss",
    col = "lightgreen"
  )
  abline(v = quantile(copula_losses, 0.95), col = "orange", lwd = 2)
  abline(v = quantile(copula_losses, 0.99), col = "red", lwd = 2)
  dev.off()

  list(
    fit = fit_obj,
    baseline_losses = baseline_losses,
    copula_losses = copula_losses,
    baseline_pricing_summary = baseline_pricing_summary,
    horizon_summary = baseline_horizon_summary,
    scenario_ranges = scenario_ranges,
    stress_results = stress_out$results,
    stress_ranges = stress_out$ranges
  )
}

if (sys.nframe() == 0) {
  cargo_results <- run_cargo_pipeline(
    exclude_types = character(),
    output_prefix = "cargo",
    coverage_note = "Base run",
    run_stress = TRUE
  )

  cat("\nCargo pricing workflow complete. Outputs written to CL_outputs/ with prefix cargo_\n")
}
