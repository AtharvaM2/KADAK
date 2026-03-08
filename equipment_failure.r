library(ggplot2)
library(readxl)
equip_data <- read_excel("KADAK/srcsc-2026-claims-equipment-failure.xlsx")

library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(MASS)
library(pscl)
library(copula)

# upload  ───────────────────────────────────
equip_freq <- read_excel("KADAK/srcsc-2026-claims-equipment-failure.xlsx", sheet = 1)
equip_sev  <- read_excel("KADAK/srcsc-2026-claims-equipment-failure.xlsx", sheet = 2)

cat("Rows loaded — freq:", nrow(equip_freq), " | sev:", nrow(equip_sev), "\n")

##################################################################
# DATA CLEANING
##################################################################

# drop rows missing solar_system ───────────────────────────────────
freq_before <- nrow(equip_freq)
sev_before  <- nrow(equip_sev)

equip_freq <- equip_freq |> filter(!is.na(solar_system)) |> arrange(solar_system)
equip_sev  <- equip_sev  |> filter(!is.na(solar_system)) |> arrange(solar_system)

cat("After dropping NA solar_system — freq dropped:", freq_before - nrow(equip_freq),
    " | sev dropped:", sev_before - nrow(equip_sev), "\n")

# enforce data dictionary bounds BEFORE joining ─────────────────────
freq_before <- nrow(equip_freq)
sev_before  <- nrow(equip_sev)

equip_freq <- equip_freq |>
  filter(
    equipment_age   >= 0   & equipment_age   <= 10,
    maintenance_int >= 100 & maintenance_int <= 5000,
    usage_int >= 0   & usage_int <= 24,
    exposure        >= 0   & exposure        <= 1,
    claim_count     >= 0   & claim_count     <= 3
  )

equip_sev <- equip_sev |>
  filter(
    equipment_age   >= 0   & equipment_age   <= 10,
    maintenance_int >= 100 & maintenance_int <= 5000,
    usage_int >= 0   & usage_int <= 24,
    exposure        >= 0   & exposure        <= 1,
    claim_amount    >= 11000
  )

cat("After data dictionary range filters — freq dropped:", freq_before - nrow(equip_freq),
    " | sev dropped:", sev_before - nrow(equip_sev), "\n")

# join claim_count into severity dataset AFTER filtering ────────────
sev_before <- nrow(equip_sev)

equip_sev <- equip_sev %>%
  inner_join(
    equip_freq %>% dplyr::select(policy_id, equipment_id, claim_count),
    by = c("policy_id", "equipment_id")
  )

cat("After inner_join (claim_count) — sev rows dropped:",
    sev_before - nrow(equip_sev),
    " | sev rows remaining:", nrow(equip_sev), "\n")

# strip trailing suffixes from string/factor columns ───────────────
equip_freq <- equip_freq |>
  mutate(across(where(~ is.character(.x) | is.factor(.x)),
                ~ str_remove(as.character(.x), "_.*$")))

equip_sev <- equip_sev |>
  mutate(across(where(~ is.character(.x) | is.factor(.x)),
                ~ str_remove(as.character(.x), "_.*$")))

cat("Clean dataset sizes — freq:", nrow(equip_freq), " | sev:", nrow(equip_sev), "\n")

############################################################
# EDA
############################################################

##################################
# 1. Distribution of Claim Amounts
##################################

p1 <- ggplot(equip_sev, aes(x = claim_amount)) +
  geom_histogram(binwidth = 10000, fill = "white", color = "#f9a86c", alpha = 0.9) +
  geom_density(aes(y = after_stat(count)), color = "#8c3a0f", linewidth = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Claim Amount", x = "Claim Amount", y = "Count") +
  theme_minimal()

p1_log <- ggplot(equip_sev, aes(x = log(claim_amount))) +
  geom_histogram(binwidth = 0.2, fill = "#f9a86c", color = "white", alpha = 0.9) +
  geom_density(aes(y = after_stat(count)), color = "#8c3a0f", linewidth = 1) +
  labs(title = "Distribution of Log Claim Amount", x = "Log(Claim Amount)", y = "Count") +
  theme_minimal()

grid.arrange(p1, p1_log, ncol = 2)

##################################
# 2. Equipment Type vs Solar System
##################################

ggplot(equip_sev, aes(x = equipment_type, fill = solar_system)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("#fdd9b5", "#f9a86c", "#f07d3a")) +
  labs(title = "Number of Equipment Claims by Type and Solar System",
       x = "Equipment Type", y = "Count", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

##################################
# 3. Frequency of Claims by Equipment Type and Solar System
##################################

claims_summary <- equip_freq |>
  group_by(equipment_type, solar_system) |>
  summarise(total_claims = sum(claim_count, na.rm = TRUE), .groups = "drop")

ggplot(claims_summary, aes(x = equipment_type, y = total_claims, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = c("#fdd9b5", "#f9a86c", "#f07d3a")) +
  labs(title = "Frequency of Claims by Equipment Type and Solar System",
       x = "Equipment Type", y = "Total Claim Frequency", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank())

##################################
# 4. Exposure-Adjusted Claim Rate by Equipment Type and Solar System
##################################

claim_rate_summary <- equip_freq |>
  group_by(equipment_type, solar_system) |>
  summarise(
    total_claims   = sum(claim_count, na.rm = TRUE),
    total_exposure = sum(exposure,    na.rm = TRUE),
    claim_rate     = total_claims / total_exposure,
    .groups = "drop"
  )

ggplot(claim_rate_summary, aes(x = equipment_type, y = claim_rate, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = c("#fdd9b5", "#f9a86c", "#f07d3a")) +
  labs(title = "Claim Frequency Rate (Exposure Adjusted)",
       x = "Equipment Type", y = "Claims per Exposure Unit", fill = "Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# 5. Average Claim Amount by Equipment Type
##################################

equip_summary <- equip_sev |>
  group_by(equipment_type) |>
  summarise(mean_claim = mean(claim_amount, na.rm = TRUE))

ggplot(equip_summary, aes(x = fct_reorder(equipment_type, mean_claim), y = mean_claim)) +
  geom_col(fill = "#f07d3a") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Amount by Equipment Type",
       x = "Equipment Type", y = "Mean Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# 6. Claim Frequency Rate by Equipment Type
##################################

equip_freq_rate <- equip_freq |>
  group_by(equipment_type) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(equip_freq_rate, aes(x = fct_reorder(equipment_type, claim_rate), y = claim_rate)) +
  geom_col(fill = "#f07d3a") +
  geom_text(aes(label = round(claim_rate, 3)), vjust = -0.4, size = 3.5) +
  labs(title = "Claim Frequency Rate by Equipment Type",
       x = "Equipment Type", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# 7. Equipment Age vs Claim Frequency (banded)
##################################

equip_freq <- equip_freq |>
  mutate(age_band = cut(equipment_age, breaks = seq(0, 10, by = 2),
                        include.lowest = TRUE))

age_freq <- equip_freq |>
  group_by(age_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(age_freq, aes(x = age_band, y = claim_rate)) +
  geom_col(fill = "#f07d3a") +
  labs(title = "Claim Frequency Rate by Equipment Age Band",
       x = "Equipment Age (Earth Years)", y = "Claims per Exposure Year") +
  theme_minimal()

##################################
# 8. Equipment Age vs Claim Severity (banded)
##################################

equip_sev <- equip_sev |>
  mutate(age_band = cut(equipment_age, breaks = seq(0, 10, by = 2),
                        include.lowest = TRUE))

age_sev <- equip_sev |>
  group_by(age_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(age_sev, aes(x = age_band, y = mean_severity)) +
  geom_col(fill = "#c95b1a") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Equipment Age Band",
       x = "Equipment Age (Earth Years)", y = "Average Claim Amount") +
  theme_minimal()

##################################
# 9. Maintenance Interval vs Claim Frequency (banded)
##################################

equip_freq <- equip_freq |>
  mutate(maint_band = cut(maintenance_int,
                          breaks = c(100, 1000, 2000, 3000, 4000, 5000),
                          include.lowest = TRUE))

maint_freq <- equip_freq |>
  group_by(maint_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(maint_freq, aes(x = maint_band, y = claim_rate)) +
  geom_col(fill = "#f9a86c") +
  labs(title = "Claim Frequency Rate by Maintenance Interval Band",
       x = "Maintenance Interval (Earth Hours)", y = "Claims per Exposure Year") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# 10. Maintenance Interval vs Claim Severity (banded)
##################################

equip_sev <- equip_sev |>
  mutate(maint_band = cut(maintenance_int,
                          breaks = c(100, 1000, 2000, 3000, 4000, 5000),
                          include.lowest = TRUE))

maint_sev <- equip_sev |>
  group_by(maint_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(maint_sev, aes(x = maint_band, y = mean_severity)) +
  geom_col(fill = "#c95b1a") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Maintenance Interval Band",
       x = "Maintenance Interval (Earth Hours)", y = "Average Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# 11. Usage Intensity vs Claim Frequency (banded)
##################################

equip_freq <- equip_freq |>
  mutate(usage_band = cut(usage_int,
                          breaks = c(0, 6, 12, 18, 24),
                          include.lowest = TRUE))

usage_freq <- equip_freq |>
  group_by(usage_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure), .groups = "drop")

ggplot(usage_freq, aes(x = usage_band, y = claim_rate)) +
  geom_col(fill = "#f07d3a") +
  labs(title = "Claim Frequency Rate by Daily Usage Intensity Band",
       x = "Usage Intensity (Earth Hours/Day)", y = "Claims per Exposure Year") +
  theme_minimal()

##################################
# 12. Usage Intensity vs Claim Severity (banded)
##################################

equip_sev <- equip_sev |>
  mutate(usage_band = cut(usage_int,
                          breaks = c(0, 6, 12, 18, 24),
                          include.lowest = TRUE))

usage_sev <- equip_sev |>
  group_by(usage_band) |>
  summarise(mean_severity = mean(claim_amount, na.rm = TRUE), .groups = "drop")

ggplot(usage_sev, aes(x = usage_band, y = mean_severity)) +
  geom_col(fill = "#c95b1a") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Daily Usage Intensity Band",
       x = "Usage Intensity (Earth Hours/Day)", y = "Average Claim Amount") +
  theme_minimal()

##################################
# 13. Heatmap: Equipment Type vs Solar System (claim count)
##################################

heatmap_counts <- equip_sev |> count(equipment_type, solar_system)

ggplot(heatmap_counts,
       aes(x = fct_reorder(equipment_type, n, .fun = sum),
           y = fct_reorder(solar_system,   n, .fun = sum),
           fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#e8f5f0", high = "#c95b1a") +
  labs(title = "Heatmap: Equipment Type vs Solar System",
       x = "Equipment Type", y = "Solar System", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##################################
# 14. Equipment Type Distribution Across Solar Systems
##################################

ggplot(equip_sev,
       aes(x = fct_reorder(equipment_type, equipment_type, .fun = length),
           fill = equipment_type)) +
  geom_bar(color = "white", linewidth = 0.2) +
  facet_wrap(~ solar_system) +
  scale_fill_manual(values = c("#e8f5f0", "#b8e0d2", "#80c9b5", "#4db89a",
                               "#f9a86c", "#f07d3a")) +
  labs(title = "Equipment Type Distribution Across Solar Systems",
       x = "Equipment Type", y = "Number of Claims", fill = "Equipment Type") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"))

##################################
# 15. Exposure vs Equipment Age
##################################

ggplot(equip_freq, aes(x = exposure, y = equipment_age)) +
  geom_point(alpha = 0.4, colour = "#f07d3a") +
  labs(title = "Exposure vs Equipment Age",
       x = "Exposure (Proportion of Year)", y = "Equipment Age (Earth Years)") +
  theme_minimal()

############################################################
# MODELLING
############################################################

##################################
# Data Preparation
##################################

model_equip_freq <- equip_freq
model_equip_sev  <- equip_sev

# Fix exposure: replace zeros/NAs with small positive value for offset
model_equip_freq$exposure[model_equip_freq$exposure <= 0 |
                            is.na(model_equip_freq$exposure)] <- 0.001
model_equip_freq$claim_count  <- round(model_equip_freq$claim_count)
model_equip_freq$log_exposure <- log(model_equip_freq$exposure)
model_equip_freq <- model_equip_freq[!is.na(model_equip_freq$claim_count), ]

# Actual predictors from the data dictionary
predictors <- c("equipment_type", "solar_system", "equipment_age",
                "maintenance_int", "usage_int", "exposure")

# ── Drop rows with NA in key predictors — report count ───────────────────────
freq_before <- nrow(model_equip_freq)

model_equip_freq <- model_equip_freq %>%
  dplyr::filter(!if_any(all_of(predictors[predictors %in% names(model_equip_freq)]), is.na))

cat("Modelling freq — rows dropped for NA in predictors:",
    freq_before - nrow(model_equip_freq),
    " | rows remaining:", nrow(model_equip_freq), "\n")

##################################
# Forward & Backward Stepwise Selection
##################################

null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_equip_freq
)

full_freq <- glm(
  claim_count ~ equipment_type + solar_system + equipment_age +
    maintenance_int + usage_int +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_equip_freq
)

summary(full_freq)

step_backward <- stepAIC(full_freq, direction = "backward")
summary(step_backward)

upper_formula <- claim_count ~ equipment_type + solar_system + equipment_age +
  maintenance_int + usage_int +
  offset(log(exposure))

step_forward <- stepAIC(
  null_freq,
  scope = list(lower = ~1, upper = upper_formula),
  direction = "forward"
)

summary(step_forward)
AIC(full_freq, step_forward, step_backward)

##################################
# Frequency GLM — Poisson
##################################

glm_freq <- glm(
  claim_count ~ equipment_type + solar_system + equipment_age +
    maintenance_int + usage_int +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data   = model_equip_freq
)

summary(glm_freq)

##################################
# Frequency GLM — Negative Binomial
##################################

freq_model_nb <- glm.nb(
  claim_count ~ equipment_type + solar_system + equipment_age +
    maintenance_int + usage_int +
    offset(log(exposure)),
  data = model_equip_freq
)

summary(freq_model_nb)


##################################
# Severity GLM — Gamma
##################################

model_equip_sev$claim_count  <- round(model_equip_sev$claim_count)
model_equip_sev$claim_amount[model_equip_sev$claim_amount <= 0] <- 0.01

# Drop NA only in the columns the severity model actually uses.
# na.omit() on the full dataset was wiping all rows because the joined
# claim_count column introduced NAs for unmatched policies.
sev_model_cols <- c("claim_amount", "equipment_type", "solar_system",
                    "equipment_age", "maintenance_int", "usage_int")

sev_before <- nrow(model_equip_sev)
model_equip_sev <- model_equip_sev %>%
  dplyr::filter(!if_any(all_of(sev_model_cols[sev_model_cols %in%
                                                names(model_equip_sev)]), is.na))

cat("Severity model — rows dropped for NA in model columns:",
    sev_before - nrow(model_equip_sev),
    " | rows remaining:", nrow(model_equip_sev), "\n")

# Scale numeric predictors
numeric_vars_sev <- c("equipment_age", "maintenance_int", "usage_int", "exposure")
existing_numeric <- intersect(numeric_vars_sev, names(model_equip_sev))
model_equip_sev[existing_numeric] <- scale(model_equip_sev[existing_numeric])

gamma_sev <- glm(
  claim_amount ~ equipment_type + solar_system + equipment_age +
    maintenance_int + usage_int,
  family = Gamma(link = "log"),
  data   = model_equip_sev
)

summary(gamma_sev)

############################################################
# MONTE CARLO SIMULATION — Simple (Single Policy Average)
############################################################

set.seed(123)
n_sim <- 500000

lambda_hat  <- predict(zinb_model, type = "response")
lambda_mean <- mean(lambda_hat)

freq_sim <- rnbinom(n_sim, size = zinb_model$theta, mu = lambda_mean)

sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(model_equip_sev$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

for (i in 1:n_sim) {
  if (freq_sim[i] > 0) {
    aggregate_loss[i] <- sum(rgamma(freq_sim[i], shape = sev_shape, scale = sev_scale))
  }
}

EF_VaR_99    <- quantile(aggregate_loss, 0.99)
EF_TVaR_99   <- mean(aggregate_loss[aggregate_loss > EF_VaR_99])
EF_mean_loss <- mean(aggregate_loss)
EF_sd_loss   <- sd(aggregate_loss)

EF_VaR_99
EF_TVaR_99
EF_mean_loss
EF_sd_loss

############################################################
# MONTE CARLO SIMULATION — Policy-Level (Full Portfolio)
############################################################

set.seed(123)
n_sim <- 10000

lambda_hat <- predict(zinb_model, type = "response")
theta_hat  <- zinb_model$theta
n_pol      <- length(lambda_hat)

sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(model_equip_sev$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

for (s in 1:n_sim) {
  freq_sim_s   <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat)
  total_claims <- sum(freq_sim_s)
  if (total_claims > 0) {
    severities        <- rgamma(total_claims, shape = sev_shape, scale = sev_scale)
    aggregate_loss[s] <- sum(severities)
  }
}

EF_mean_loss <- mean(aggregate_loss)
EF_sd_loss   <- sd(aggregate_loss)
EF_VaR_99    <- quantile(aggregate_loss, 0.99)
EF_TVaR_99   <- mean(aggregate_loss[aggregate_loss > EF_VaR_99])

EF_mean_loss
EF_sd_loss
EF_VaR_99
EF_TVaR_99

############################################################
# TAIL DEPENDENCE — Copula Analysis
############################################################

freq_raw <- equip_freq$claim_count
freq_pos <- freq_raw[freq_raw > 0]
sev_raw  <- equip_sev$claim_amount
n        <- min(length(freq_pos), length(sev_raw))

dep_data <- data.frame(freq_dep = freq_pos[1:n], sev_dep = sev_raw[1:n])
u        <- rank(dep_data$freq_dep) / (n + 1)
v        <- rank(dep_data$sev_dep)  / (n + 1)
uv_data  <- cbind(u, v)

# Fit t-copula
t_cop   <- tCopula(dim = 2)
fit_cop <- fitCopula(t_cop, uv_data, method = "ml")
summary(fit_cop)

# Compare multiple copula families
fit_norm <- fitCopula(normalCopula(param = 0.5, dim = 2), uv_data, method = "ml")
fit_clay <- fitCopula(claytonCopula(param = 2,   dim = 2), uv_data, method = "ml")
fit_gumb <- fitCopula(gumbelCopula(param = 2,   dim = 2), uv_data, method = "ml")
fit_t2   <- fitCopula(tCopula(param = 0.5, df = 4, dim = 2), uv_data, method = "ml")

n_plot   <- 5000
sim_norm <- rCopula(n_plot, fit_norm@copula)
sim_clay <- rCopula(n_plot, fit_clay@copula)
sim_gumb <- rCopula(n_plot, fit_gumb@copula)
sim_t2   <- rCopula(n_plot, fit_t2@copula)

par(mfrow = c(2, 2))
plot(sim_norm, main = "Gaussian Copula", xlab = "Frequency", ylab = "Severity",
     pch = 20, col = rgb(0.973, 0.659, 0.424, 0.3))
plot(sim_clay, main = "Clayton Copula",  xlab = "Frequency", ylab = "Severity",
     pch = 20, col = rgb(0.784, 0.357, 0.098, 0.3))
plot(sim_gumb, main = "Gumbel Copula",   xlab = "Frequency", ylab = "Severity",
     pch = 20, col = rgb(0.502, 0.788, 0.710, 0.3))
plot(sim_t2,   main = "t-Copula",        xlab = "Frequency", ylab = "Severity",
     pch = 20, col = rgb(0.941, 0.490, 0.227, 0.3))
par(mfrow = c(1, 1))

# Copula-based aggregate loss simulation
set.seed(123)
n_sim   <- 500000
cop_sim <- rCopula(n_sim, fit_cop@copula)
u_sim   <- cop_sim[, 1]
v_sim   <- cop_sim[, 2]

lambda_hat  <- predict(zinb_model, type = "response")
lambda_mean <- mean(lambda_hat)

freq_sim_cop <- qnbinom(u_sim,
                        size = zinb_model$theta,
                        mu   = sample(lambda_hat, n_sim, replace = TRUE))

sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(model_equip_sev$claim_amount) / sev_shape

aggregate_loss_cop <- numeric(n_sim)

for (i in 1:n_sim) {
  if (freq_sim_cop[i] > 0) {
    aggregate_loss_cop[i] <- sum(rgamma(freq_sim_cop[i],
                                        shape = sev_shape,
                                        scale = sev_scale))
  }
}

EF_VaR_99_cop  <- quantile(aggregate_loss_cop, 0.99)
EF_TVaR_99_cop <- mean(aggregate_loss_cop[aggregate_loss_cop > EF_VaR_99_cop])

EF_VaR_99_cop
EF_TVaR_99_cop

quantile(aggregate_loss_cop, 0.995)  # Solvency risk
quantile(aggregate_loss_cop, 0.999)  # Catastrophic risk

lambda(fit_cop@copula)

############################################################
# STRESS TESTING
############################################################

# Stress level vectors
freq_stress_levels    <- c(1.25, 1.50, 1.75, 2.00)
sev_stress_levels     <- c(1.25, 1.50, 1.75, 2.00)
length_stress_levels  <- c(1.25, 1.50, 1.75, 2.00)
exposure_levels       <- c(1.25, 1.50, 1.75, 2.00)

# usage_int: 0–24 hrs/day; stress multipliers up to ~2x midpoint
usage_levels          <- c(1.25, 1.50, 2.00)

# maintenance_int: 100–5000 hrs; longer interval = less maintenance = more claims
# We model this as a frequency multiplier (inverse relationship)
maint_int_levels      <- c(1.4, 1.2, 1.0, 0.8, 0.6, 0.4)

# equipment_age: 0–10 yrs; older equipment → more claims
age_stress_levels     <- c(0.5, 0.75, 1.0, 1.25, 1.50, 1.75, 2.00)

inflation_levels      <- c(1.00, 1.05, 1.10, 1.15, 1.20, 1.30, 1.35, 1.40, 1.45, 1.50)

# Re-generate freq_sim for stress tests (n_sim = 500000 from copula section)
set.seed(123)
freq_sim <- rnbinom(n_sim, size = zinb_model$theta, mu = lambda_mean)

##################################
# Frequency Stress Test
##################################

freq_results <- data.frame()

for (f in freq_stress_levels) {
  freq_temp <- round(freq_sim * f)
  loss_temp <- sapply(freq_temp, function(n) {
    if (n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  VaR <- quantile(loss_temp, 0.99)
  freq_results <- rbind(freq_results,
                        data.frame(stress_factor = f,
                                   mean_loss = mean(loss_temp),
                                   VaR_99    = VaR,
                                   TVaR_99   = mean(loss_temp[loss_temp > VaR])))
}

freq_results

##################################
# Severity Stress Test
##################################

sev_results <- data.frame()

for (s in sev_stress_levels) {
  loss_temp <- sapply(freq_sim, function(n) {
    if (n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale * s))
  })
  VaR <- quantile(loss_temp, 0.99)
  sev_results <- rbind(sev_results,
                       data.frame(stress_factor = s,
                                  mean_loss = mean(loss_temp),
                                  VaR_99    = VaR,
                                  TVaR_99   = mean(loss_temp[loss_temp > VaR])))
}

sev_results

##################################
# Downtime / Claim Length Stress Test
##################################

daily_cost     <- mean(equip_sev$claim_amount) / 30
downtime_proxy <- model_equip_sev$claim_amount / mean(model_equip_sev$claim_amount)
downtime_proxy <- na.omit(downtime_proxy)

length_results <- data.frame()

for (l in length_stress_levels) {
  severity_temp <- downtime_proxy * l * daily_cost * 30
  loss_temp <- sapply(freq_sim, function(n) {
    if (n == 0) return(0)
    sum(sample(severity_temp, n, replace = TRUE))
  })
  VaR <- quantile(loss_temp, 0.99)
  length_results <- rbind(length_results,
                          data.frame(stress_factor = l,
                                     mean_loss = mean(loss_temp),
                                     VaR_99    = VaR,
                                     TVaR_99   = mean(loss_temp[loss_temp > VaR])))
}

length_results

##################################
# Combine & Plot: Frequency, Severity, Downtime
##################################

freq_results$test_type   <- "Frequency"
sev_results$test_type    <- "Severity"
length_results$test_type <- "Downtime Length"

stress_results <- rbind(freq_results, sev_results, length_results)

ggplot(stress_results, aes(x = stress_factor, y = VaR_99, color = test_type)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Frequency"       = "#f07d3a",
    "Severity"        = "#4db89a",
    "Downtime Length" = "#c95b1a"
  )) +
  labs(title = "Stress Testing Impact on VaR (99%) — Equipment Failure",
       x = "Stress Multiplier", y = "VaR 99%", color = "Stress Type") +
  theme_minimal()

##################################
# Operational Stress Tests
# (Exposure, Usage Intensity, Maintenance Interval, Equipment Age)
##################################

# Exposure stress
exposure_results <- data.frame()
for (e in exposure_levels) {
  freq_temp <- round(freq_sim * e)
  loss_temp <- sapply(freq_temp, function(n) {
    if (n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  VaR <- quantile(loss_temp, 0.99)
  exposure_results <- rbind(exposure_results,
                            data.frame(stress_factor = e,
                                       mean_loss = mean(loss_temp),
                                       VaR_99    = VaR,
                                       TVaR_99   = mean(loss_temp[loss_temp > VaR])))
}

# Usage intensity stress (higher usage → higher freq)
usage_results <- data.frame()
for (h in usage_levels) {
  freq_temp <- round(freq_sim * h)
  loss_temp <- sapply(freq_temp, function(n) {
    if (n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  VaR <- quantile(loss_temp, 0.99)
  usage_results <- rbind(usage_results,
                         data.frame(stress_factor = h,
                                    mean_loss = mean(loss_temp),
                                    VaR_99    = VaR,
                                    TVaR_99   = mean(loss_temp[loss_temp > VaR])))
}

# Maintenance interval stress (longer gap between maintenance → more claims)
# Factor < 1 means maintenance is less frequent, so freq multiplied up
maint_results <- data.frame()
for (m in maint_int_levels) {
  freq_temp <- round(freq_sim * (1 + (1 - m)))
  loss_temp <- sapply(freq_temp, function(n) {
    if (n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  VaR <- quantile(loss_temp, 0.99)
  maint_results <- rbind(maint_results,
                         data.frame(stress_factor = m,
                                    mean_loss = mean(loss_temp),
                                    VaR_99    = VaR,
                                    TVaR_99   = mean(loss_temp[loss_temp > VaR])))
}

# Equipment age stress (older equipment → higher freq and higher severity)
age_results <- data.frame()
for (g in age_stress_levels) {
  freq_stress_age  <- rnbinom(n_sim, size = zinb_model$theta, mu = lambda_mean * g)
  sev_scale_stress <- sev_scale * g
  loss_temp <- sapply(freq_stress_age, function(n) {
    if (n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale_stress))
  })
  age_results <- rbind(age_results,
                       data.frame(stress_factor = g,
                                  mean_loss = mean(loss_temp),
                                  VaR_99    = quantile(loss_temp, 0.99),
                                  TVaR_99   = mean(loss_temp[loss_temp >
                                                               quantile(loss_temp, 0.99)])))
}

exposure_results$test <- "Exposure"
usage_results$test    <- "Usage Intensity"
maint_results$test    <- "Maintenance Interval"
age_results$test      <- "Equipment Age"

stress_results1 <- rbind(exposure_results, usage_results, maint_results, age_results)

ggplot(stress_results1, aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    "Exposure"             = "#f07d3a",
    "Usage Intensity"      = "#4db89a",
    "Maintenance Interval" = "#c95b1a",
    "Equipment Age"        = "#f9a86c"
  )) +
  labs(title = "Operational Stress Tests — Equipment Failure Losses",
       x = "Stress Level", y = "VaR 99%", color = "Stress Type") +
  theme_minimal()

##################################
# Claims Inflation Stress Test
##################################

inflation_results <- data.frame()

for (i in inflation_levels) {
  loss_temp <- freq_sim * (sev_scale * sev_shape) * i
  inflation_results <- rbind(inflation_results,
                             data.frame(stress_factor = i,
                                        mean_loss = mean(loss_temp),
                                        VaR_99    = quantile(loss_temp, 0.99),
                                        TVaR_99   = mean(loss_temp[loss_temp >
                                                                      quantile(loss_temp, 0.99)])))
}

inflation_results$test <- "Claims Inflation"

ggplot(inflation_results, aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Claims Inflation" = "#c95b1a")) +
  labs(title = "Claims Inflation Stress Test — Equipment Failure",
       x = "Inflation Multiplier", y = "VaR 99%", color = "") +
  theme_minimal()

##################################
# Correlated Multi-Solar System Shock
# Solar systems per data dictionary: Helionis Cluster, Epsilon, Zeta
##################################

set.seed(2174)
n_sim_shock <- 100000

systems <- c("Helionis", "Epsilon", "Zeta")

sys_params <- list(
  Helionis = list(lambda = 8,  shape = 2.0, scale = sev_scale),
  Epsilon  = list(lambda = 12, shape = 1.5, scale = sev_scale * 1.2),
  Zeta     = list(lambda = 10, shape = 1.2, scale = sev_scale * 1.5)
)

solar_storm_occurs <- rbinom(n_sim_shock, 1, 0.01)

run_simulation_ef <- function(stress_freq = 1.0, stress_sev = 1.0,
                               correlated_event = FALSE) {
  total_losses <- numeric(n_sim_shock)
  for (s in systems) {
    p      <- sys_params[[s]]
    freq_s <- rpois(n_sim_shock, p$lambda * stress_freq)
    if (correlated_event) {
      freq_s <- freq_s + (solar_storm_occurs * rpois(n_sim_shock, p$lambda * 5))
    }
    sys_loss <- sapply(freq_s, function(f) {
      if (f > 0) sum(rgamma(f, shape = p$shape, scale = p$scale * stress_sev)) else 0
    })
    total_losses <- total_losses + sys_loss
  }
  return(total_losses)
}

baseline_ef     <- run_simulation_ef()
sev_stress_ef   <- run_simulation_ef(stress_sev  = 1.4)
freq_stress_ef  <- run_simulation_ef(stress_freq = 1.5)
catastrophic_ef <- run_simulation_ef(stress_freq = 1.5, stress_sev = 1.4,
                                      correlated_event = TRUE)

calc_metrics_ef <- function(losses, name) {
  var_99  <- quantile(losses, 0.99)
  tvar_99 <- mean(losses[losses > var_99])
  data.frame(Scenario = name,
             Mean     = mean(losses),
             VaR_99   = var_99,
             TVaR_99  = tvar_99,
             Max_Loss = max(losses))
}

summary_table_ef <- rbind(
  calc_metrics_ef(baseline_ef,     "Baseline"),
  calc_metrics_ef(sev_stress_ef,   "Severity Stress (+40%)"),
  calc_metrics_ef(freq_stress_ef,  "Frequency Stress (+50%)"),
  calc_metrics_ef(catastrophic_ef, "Systemic Catastrophe (Correlated)")
)

print(summary_table_ef)

hist(catastrophic_ef, breaks = 100,
     col    = rgb(0.941, 0.490, 0.227, 0.6),
     border = "white",
     main   = "Aggregate Loss Distribution: Catastrophic Scenario — Equipment Failure",
     xlab   = "Loss (Đ)",
     xlim   = c(0, quantile(catastrophic_ef, 0.999)))
abline(v = summary_table_ef$VaR_99[4], col = "#4db89a", lwd = 2, lty = 2)
legend("topright", legend = "99% VaR", col = "#4db89a", lty = 2, lwd = 2)

