# =============================================================
# Business Interruption Claims Analysis
# ACTL4001 Assignment
# =============================================================

# =============================================================
# 1. Setup
# =============================================================

# -----------------------------
# 1.1 Libraries
# -----------------------------
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

# -----------------------------
# 1.2 Working Directory & Data Import
# -----------------------------
setwd("C:/Users/khush/OneDrive - UNSW/Desktop/ACTL4001")

bi_freq_raw <- read_excel("KADAK/srcsc-2026-claims-business-interruption.xlsx", sheet = 1)
bi_sev_raw  <- read_excel("KADAK/srcsc-2026-claims-business-interruption.xlsx", sheet = 2)

# =============================================================
# 2. Data Cleaning
# =============================================================
# -----------------------------
# 2.1 Standardise Solar System Names
# -----------------------------
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

# -----------------------------
# 2.2 Clean Frequency & Severity Data
# -----------------------------
bi_freq_clean <- bi_freq_raw %>%
  filter(!is.na(policy_id) & policy_id != "",
         !is.na(station_id) & station_id != "",
         solar_system %in% c("Helionis Cluster", "Epsilon", "Zeta"),
         production_load >= 0 & production_load <= 1,
         energy_backup_score %in% 1:5,
         supply_chain_index >= 0 & supply_chain_index <= 1,
         avg_crew_exp >= 1 & avg_crew_exp <= 30,
         maintenance_freq >= 0 & maintenance_freq <= 6,
         safety_compliance %in% 1:5,
         exposure > 0 & exposure <= 1,
         claim_count >= 0 & claim_count <= 4)

bi_sev_clean <- bi_sev_raw %>%
  filter(!is.na(policy_id) & policy_id != "",
         !is.na(station_id) & station_id != "",
         solar_system %in% c("Helionis Cluster", "Epsilon", "Zeta"),
         production_load >= 0 & production_load <= 1,
         energy_backup_score %in% 1:5,
         safety_compliance %in% 1:5,
         exposure > 0 & exposure <= 1,
         claim_amount >= 28000 & claim_amount <= 1426000)

# =============================================================
# 3. Dataset Construction
# =============================================================
# -----------------------------
# 3.1 Aggregate Severity Data
# -----------------------------
bi_sev_agg <- bi_sev_clean %>%
  group_by(policy_id) %>%
  summarise(total_claim_amount = sum(claim_amount, na.rm = TRUE),
            avg_claim_amount   = mean(claim_amount, na.rm = TRUE))

# -----------------------------
# 3.2 Combine Frequency and Severity
# -----------------------------
bi_full <- bi_freq_clean %>%
  left_join(bi_sev_agg, by = "policy_id") %>%
  mutate(total_claim_amount = coalesce(total_claim_amount, 0),
         avg_claim_amount   = coalesce(avg_claim_amount, 0))

# =============================================================
# 4. Exploratory Data Analysis (EDA)
# =============================================================
# -----------------------------
# 4.1 Setup Colors
# -----------------------------
blue_pal <- c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")

# -----------------------------
# 4.2 Frequency Analysis
# -----------------------------
# Number of Policies by Solar System
ggplot(bi_freq_clean, aes(x=solar_system, fill=solar_system)) +
  geom_bar() +
  scale_fill_manual(values=blue_pal) +
  labs(title="Number of Policies by Solar System", x="Solar System", y="Count") +
  theme_minimal()

# Total Claim Frequency by Solar System
freq_summary <- bi_freq_clean %>%
  group_by(solar_system) %>%
  summarise(total_claims = sum(claim_count, na.rm=TRUE), .groups="drop")

ggplot(freq_summary, aes(x=solar_system, y=total_claims, fill=solar_system)) +
  geom_col() +
  labs(title="Total Claim Frequency by Solar System", x="Solar System", y="Total Claims") +
  theme_minimal()

# Heatmap: Total Claims by Production Load & Solar System
heatmap_summary <- bi_full %>%
  group_by(solar_system, production_load_bin = cut(production_load, breaks=seq(0,1,0.1))) %>%
  summarise(total_claims = sum(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(heatmap_summary, aes(x=production_load_bin, y=solar_system, fill=total_claims)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  labs(title="Heatmap: Total Claims by Production Load and Solar System", x="Production Load Bin", y="Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# -----------------------------
# 4.3 Severity Analysis
# -----------------------------
p1 <- ggplot(bi_sev_clean, aes(x=claim_amount)) +
  geom_histogram(binwidth=50000, fill="white", color=blue_pal[2]) +
  geom_density(aes(y=..count..), color=blue_pal[5], linewidth=1) +
  scale_x_continuous(labels=scales::comma) +
  labs(title="Distribution of BI Claim Amounts", x="Claim Amount", y="Count") +
  theme_minimal()

p1_log <- ggplot(bi_sev_clean, aes(x=log(claim_amount+1))) +
  geom_histogram(binwidth=0.3, fill=blue_pal[2], color="white") +
  geom_density(aes(y=..count..), color=blue_pal[5], linewidth=1) +
  labs(title="Log-Transformed Claim Amounts", x="Log(Claim Amount)", y="Count") +
  theme_minimal()

grid.arrange(p1, p1_log, ncol=2)

# -----------------------------
# 4.4 Portfolio Analysis
# -----------------------------
claim_summary <- bi_full %>%
  group_by(solar_system) %>%
  summarise(avg_total_claim = mean(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(claim_summary, aes(x=reorder(solar_system, avg_total_claim), y=avg_total_claim, fill=solar_system)) +
  geom_col() + coord_flip() +
  labs(title="Average Total Claim Amount per Policy", x="Solar System", y="Avg Total Claim Amount") +
  theme_minimal()

bi_full <- bi_full %>%
  mutate(exposure_bin = cut(exposure, breaks=seq(0,1,0.1), include.lowest=TRUE))

freq_by_exposure <- bi_full %>%
  group_by(exposure_bin, solar_system) %>%
  summarise(avg_claim_rate = mean(claim_count/exposure, na.rm=TRUE),
            total_policies = n(), .groups="drop")

ggplot(freq_by_exposure, aes(x=exposure_bin, y=avg_claim_rate, fill=solar_system)) +
  geom_col(position=position_dodge(width=0.8)) +
  labs(title="Average Claim Rate by Exposure Bin", x="Exposure Bin", y="Avg Claim Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

heatmap_summary2 <- bi_full %>%
  group_by(solar_system, maintenance_freq) %>%
  summarise(total_claims=sum(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(heatmap_summary2, aes(x=as.factor(maintenance_freq), y=solar_system, fill=total_claims)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  labs(title="Heatmap: Total Claims by Maintenance Frequency", x="Maintenance Frequency", y="Solar System") +
  theme_minimal()

# =============================================================
# 5. Frequency Modelling
# =============================================================
bi_freq_model <- bi_freq_clean %>%
  mutate(log_exposure = log(exposure))

full_formula <- claim_count ~ solar_system + production_load +
  energy_backup_score + supply_chain_index +
  avg_crew_exp + maintenance_freq + safety_compliance +
  offset(log_exposure)

poisson_model <- glm(full_formula, family = poisson(link = "log"), data = bi_freq_model)

dispersion <- sum(residuals(poisson_model, type="pearson")^2) / poisson_model$df.residual

nb_model <- glm.nb(full_formula, data = bi_freq_model)

zinb_model <- zeroinfl(
  claim_count ~ solar_system + production_load +
    energy_backup_score + supply_chain_index +
    avg_crew_exp + maintenance_freq +
    safety_compliance + offset(log_exposure) | 1,
  data = bi_freq_model,
  dist = "negbin"
)

AIC(poisson_model, nb_model, zinb_model)

# =============================================================
# 6. Severity Modelling
# =============================================================
bi_sev_model <- bi_sev_clean
bi_sev_model$claim_amount[bi_sev_model$claim_amount <= 0] <- 0.01

gamma_sev <- glm(
  claim_amount ~ solar_system + production_load +
    energy_backup_score + safety_compliance + exposure,
  data = bi_sev_model,
  family = Gamma(link = "log")
)

gamma_sev_step <- stepAIC(gamma_sev, direction="both")

# =============================================================
# 7. Monte Carlo Portfolio Simulation & Risk Measures
# =============================================================
set.seed(123)
n_sim <- 10000

# Frequency & severity parameters
lambda_hat <- predict(nb_model, type = "response")
theta_hat  <- nb_model$theta
n_pol      <- length(lambda_hat)
sev_shape  <- 1 / summary(gamma_sev)$dispersion
sev_scale  <- mean(bi_sev_clean$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

for(s in 1:n_sim){
  freq_sim <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat)
  total_claims <- sum(freq_sim)
  if(total_claims > 0){
    severities <- rgamma(total_claims, shape = sev_shape, scale = sev_scale)
    aggregate_loss[s] <- sum(severities)
  } else {
    aggregate_loss[s] <- 0
  }
}

aggregate_loss <- aggregate_loss * 0.3380428

# Risk Measures
BI_mean_loss_mc <- mean(aggregate_loss)
BI_sd_loss_mc   <- sd(aggregate_loss)
BI_VaR_99_mc   <- quantile(aggregate_loss, 0.99)
BI_TVaR_99_mc  <- mean(aggregate_loss[aggregate_loss > BI_VaR_99_mc])
BI_VaR_995_mc  <- quantile(aggregate_loss, 0.995)
BI_VaR_999_mc  <- quantile(aggregate_loss, 0.999)

risk_summary_mc <- data.frame(
  Metric = c("Mean Loss", "Std Dev", "VaR 99", "TVaR 99", "VaR 99.5", "VaR 99.9"),
  Value  = c(BI_mean_loss_mc, BI_sd_loss_mc, BI_VaR_99_mc, BI_TVaR_99_mc,
             BI_VaR_995_mc, BI_VaR_999_mc)
)

print(risk_summary_mc)

# =============================================================
# 8. Stress Testing
# =============================================================
# 8.1 Stress Levels
freq_levels <- c(1.25,1.5,1.75,2)
sev_levels  <- c(1.25,1.5,1.75,2)

production_levels  <- c(1.1,1.25,1.5,1.75,2)
energy_levels      <- c(1.1,1.25,1.5,1.75,2)
supply_levels      <- c(1.1,1.25,1.5,1.75,2)
maintenance_levels <- c(1.1,1.25,1.5,1.75,2)

crew_levels   <- c(0.6,0.8,1,1.2,1.4)
safety_levels <- c(0.8,1,1.2,1.4,1.6)

exposure_levels   <- c(1.25,1.5,1.75,2)
inflation_levels  <- c(1,1.05,1.1,1.15,1.2,1.3,1.4,1.5)

# 8.2 Stress Test Function
run_stress_test <- function(stress_levels, variable_name, category) {
  stress_levels <- c(1, stress_levels)
  results <- data.frame()
  
  for(s in stress_levels){
    freq_temp <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat * s)
    n_iter <- 5
    aggregate_losses <- numeric(n_iter)
    
    for(i in 1:n_iter){
      loss <- sum(sapply(freq_temp, function(n){
        if(n == 0) return(0)
        sum(rgamma(n, shape = sev_shape, scale = sev_scale))
      }))
      aggregate_losses[i] <- loss
    }
    
    aggregate_losses <- aggregate_losses * 0.3380428
    results <- rbind(results,
                     data.frame(`Risk Factor` = paste(category, "-", variable_name),
                                `Stress Factor` = s,
                                `Mean Loss ($)` = mean(aggregate_losses),
                                `VaR 99% ($)`  = quantile(aggregate_losses, 0.99)))
  }
  
  return(results)
}

# 8.3 Run Stress Tests by Category
core_freq_results <- run_stress_test(freq_levels, "Claim Frequency", "Core Insurance Risk")
core_sev_results  <- run_stress_test(sev_levels, "Claim Severity", "Core Insurance Risk")
oper_prod_results <- run_stress_test(production_levels, "Production Load", "Operational Risk")
oper_energy_results <- run_stress_test(energy_levels, "Energy Backup Score", "Operational Risk")
oper_supply_results <- run_stress_test(supply_levels, "Supply Chain Index", "Operational Risk")
oper_maint_results  <- run_stress_test(maintenance_levels, "Maintenance Frequency", "Operational Risk")
work_crew_results   <- run_stress_test(crew_levels, "Crew Experience", "Workforce Risk")
work_safety_results <- run_stress_test(safety_levels, "Safety Compliance", "Workforce Risk")
port_exposure_results <- run_stress_test(exposure_levels, "Exposure Growth", "Portfolio Risk")
fin_inflation_results <- run_stress_test(inflation_levels, "Claims Inflation", "Financial Risk")

# 8.4 Combine Stress Test Results
stress_results_final <- rbind(
  core_freq_results, core_sev_results,
  oper_prod_results, oper_energy_results, oper_supply_results, oper_maint_results,
  work_crew_results, work_safety_results,
  port_exposure_results,
  fin_inflation_results
)

colnames(stress_results_final) <- c("Risk_Factor", "Stress_Factor", "Mean_Loss", "VaR99")
print(stress_results_final)

# 8.5 Stress Test Plot
ggplot(stress_results_final,
       aes(x = Stress_Factor, y = VaR99, group = Risk_Factor, color = Risk_Factor)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~Risk_Factor, scales = "free_x") +
  theme_minimal() +
  labs(title = "Business Interruption Stress Testing by Risk Factor",
       x = "Stress Factor", y = "VaR 99% ($)") +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none")

# =============================================================
# 9. Scenario Testing (Portfolio-Level)
# =============================================================
scenario_results <- data.frame(
  `Risk Factor`   = character(),
  `Stress Factor` = character(),
  `Mean Loss ($)` = numeric(),
  `VaR 99% ($)`   = numeric(),
  stringsAsFactors = FALSE
)

scenarios <- data.frame(
  scenario = c("Best Case", "Moderate Case", "Worst Case"),
  freq     = c(0.75, 1, 2),
  sev      = c(0.9, 1, 2)
)

n_iter <- 50

for(i in 1:nrow(scenarios)){
  row <- scenarios[i,]
  freq_temp <- rnbinom(n_pol, size = theta_hat, mu = lambda_hat * row$freq)
  sev_scale_stress <- sev_scale * row$sev
  aggregate_losses <- numeric(n_iter)
  
  for(j in 1:n_iter){
    loss <- sum(sapply(freq_temp, function(n){
      if(n == 0) return(0)
      sum(rgamma(n, shape = sev_shape, scale = sev_scale_stress))
    }))
    aggregate_losses[j] <- loss
  }
  
  aggregate_losses <- aggregate_losses * 0.3380428
  
  scenario_results <- rbind(scenario_results,
                            data.frame(`Risk Factor` = "Portfolio Scenario",
                                       `Stress Factor` = row$scenario,
                                       `Mean Loss ($)` = mean(aggregate_losses),
                                       `VaR 99% ($)`   = quantile(aggregate_losses,0.99)))
}

print(scenario_results)

# =============================================================
# 10. Tail Dependence Analysis & Copulas
# =============================================================
freq <- bi_freq_clean$claim_count
freq_pos <- freq[freq>0]
sev <- bi_sev_clean$claim_amount
n <- min(length(freq_pos), length(sev))

freq_dep <- freq_pos[1:n]
sev_dep  <- sev[1:n]
dep_data <- data.frame(freq_dep, sev_dep)
u <- rank(dep_data$freq_dep) / (n + 1)
v <- rank(dep_data$sev_dep)  / (n + 1)
uv_data <- cbind(u, v)

# Fit t-Copula
t_cop <- tCopula(dim = 2)
fit_cop <- fitCopula(t_cop, uv_data, method = "ml")
summary(fit_cop)

# Simulate copula for tail analysis
set.seed(123)
n_sim <- 500000
cop_sim <- rCopula(n_sim, fit_cop@copula)
u_sim <- cop_sim[,1]
v_sim <- cop_sim[,2]

# Aggregate losses via copula
lambda_hat <- predict(zinb_model, type = "response")
lambda_mean <- mean(lambda_hat)
freq_sim <- qnbinom(u_sim, size = zinb_model$theta, mu = sample(lambda_hat, n_sim, replace = TRUE))
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(bi_sev_clean$claim_amount) / sev_shape
sev_sim <- qgamma(v_sim, shape = sev_shape, scale = sev_scale)

aggregate_loss <- freq_sim * sev_sim
for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    aggregate_loss[i] <- sum(rgamma(freq_sim[i], shape = sev_shape, scale = sev_scale))
  }
}

BI_mean_loss <- mean(aggregate_loss)
BI_sd_loss   <- sd(aggregate_loss)
BI_VaR_99   <- quantile(aggregate_loss, 0.99)
BI_TVaR_99  <- mean(aggregate_loss[aggregate_loss > BI_VaR_99])

# Fit and plot multiple copulas
norm_cop <- normalCopula(param = 0.5, dim = 2)
clay_cop <- claytonCopula(param = 2, dim = 2)
gumb_cop <- gumbelCopula(param = 2, dim = 2)
t_cop    <- tCopula(param = 0.5, df = 4, dim = 2)

fit_norm <- fitCopula(norm_cop, uv_data, method = "ml")
fit_clay <- fitCopula(clay_cop, uv_data, method = "ml")
fit_gumb <- fitCopula(gumb_cop, uv_data, method = "ml")
fit_t    <- fitCopula(t_cop, uv_data, method = "ml")

n_sim <- 5000
sim_norm <- rCopula(n_sim, fit_norm@copula)
sim_clay <- rCopula(n_sim, fit_clay@copula)
sim_gumb <- rCopula(n_sim, fit_gumb@copula)
sim_t    <- rCopula(n_sim, fit_t@copula)

par(mfrow = c(2,2))
plot(sim_norm, main = "Gaussian Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_clay, main = "Clayton Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_gumb, main = "Gumbel Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_t, main = "t-Copula", xlab = "Frequency", ylab = "Severity")

# Tail dependence plots
copula_df <- data.frame(u = u_sim, v = v_sim)

ggplot(copula_df, aes(u, v)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0.9,1), ylim = c(0.9,1)) +
  labs(title = "Upper Tail Dependence", x = "Frequency (u)", y = "Severity (v)") +
  theme_minimal()

ggplot(copula_df, aes(u, v)) +
  geom_point(alpha = 0.3) +
  coord_cartesian(xlim = c(0,0.1), ylim = c(0,0.1)) +
  labs(title = "Lower Tail Dependence", x = "Frequency Rank (u)", y = "Severity Rank (v)") +
  theme_minimal()

# Tail dependence coefficient
mean(u_sim > 0.95 & v_sim > 0.95)