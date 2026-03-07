# =============================================================
# Business Interruption Claims Analysis
# ACTL4001 Assignment
# =============================================================

# -----------------------------
# 1. Setup
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

setwd("C:/Users/khush/OneDrive - UNSW/Desktop/ACTL4001")


# -----------------------------
# 2. Data Import
# -----------------------------

bi_freq_raw <- read_excel(
  "KADAK/srcsc-2026-claims-business-interruption.xlsx",
  sheet = 1
)

bi_sev_raw <- read_excel(
  "KADAK/srcsc-2026-claims-business-interruption.xlsx",
  sheet = 2
)


# -----------------------------
# 3. Data Cleaning
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
  )


# -----------------------------
# 4. Dataset Construction
# -----------------------------

bi_sev_agg <- bi_sev_clean %>%
  group_by(policy_id) %>%
  summarise(
    total_claim_amount = sum(claim_amount, na.rm = TRUE),
    avg_claim_amount   = mean(claim_amount, na.rm = TRUE)
  )

bi_full <- bi_freq_clean %>%
  left_join(bi_sev_agg, by = "policy_id") %>%
  mutate(
    total_claim_amount = coalesce(total_claim_amount, 0),
    avg_claim_amount   = coalesce(avg_claim_amount, 0)
  )


# -----------------------------
# 5. Exploratory Data Analysis
# -----------------------------

blue_pal <- c("#c6dbef","#9ecae1","#6baed6","#3182bd","#08519c")

# 5a. Frequency
ggplot(bi_freq_clean, aes(x=solar_system, fill=solar_system)) +
  geom_bar() +
  scale_fill_manual(values=blue_pal) +
  labs(title="Number of Policies by Solar System", x="Solar System", y="Count") +
  theme_minimal()

freq_summary <- bi_freq_clean %>%
  group_by(solar_system) %>%
  summarise(total_claims = sum(claim_count, na.rm=TRUE), .groups="drop")

ggplot(freq_summary, aes(x=solar_system, y=total_claims, fill=solar_system)) +
  geom_col() +
  labs(title="Total Claim Frequency by Solar System", x="Solar System", y="Total Claims") +
  theme_minimal()

heatmap_summary <- bi_full %>%
  group_by(solar_system, production_load_bin = cut(production_load, breaks=seq(0,1,0.1))) %>%
  summarise(total_claims = sum(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(heatmap_summary, aes(x=production_load_bin, y=solar_system, fill=total_claims)) +
  geom_tile(color="white") +
  scale_fill_gradient(low="#deebf7", high="#3182bd") +
  labs(title="Heatmap: Total Claims by Production Load and Solar System", x="Production Load Bin", y="Solar System") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# 5b. Severity
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

# 5c. Portfolio Analysis
claim_summary <- bi_full %>%
  group_by(solar_system) %>%
  summarise(avg_total_claim=mean(total_claim_amount, na.rm=TRUE), .groups="drop")

ggplot(claim_summary, aes(x=reorder(solar_system, avg_total_claim), y=avg_total_claim, fill=solar_system)) +
  geom_col() + coord_flip() +
  labs(title="Average Total Claim Amount per Policy", x="Solar System", y="Avg Total Claim Amount") +
  theme_minimal()

bi_full <- bi_full %>%
  mutate(exposure_bin=cut(exposure, breaks=seq(0,1,0.1), include.lowest=TRUE))

freq_by_exposure <- bi_full %>%
  group_by(exposure_bin, solar_system) %>%
  summarise(avg_claim_rate=mean(claim_count/exposure, na.rm=TRUE), total_policies=n(), .groups="drop")

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

# -----------------------------
# 6. Frequency Modelling
# -----------------------------

bi_freq_model <- bi_freq_clean %>%
  mutate(log_exposure = log(exposure))

full_formula <- claim_count ~ solar_system + production_load +
  energy_backup_score + supply_chain_index +
  avg_crew_exp + maintenance_freq + safety_compliance +
  offset(log_exposure)

poisson_model <- glm(full_formula,
                     family = poisson(link = "log"),
                     data = bi_freq_model)

dispersion <- sum(residuals(poisson_model,type="pearson")^2) /
  poisson_model$df.residual

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


# -----------------------------
# 7. Severity Modelling
# -----------------------------

bi_sev_model <- bi_sev_clean
bi_sev_model$claim_amount[bi_sev_model$claim_amount <= 0] <- 0.01

gamma_sev <- glm(
  claim_amount ~ solar_system + production_load +
    energy_backup_score + safety_compliance + exposure,
  data = bi_sev_model,
  family = Gamma(link = "log")
)

gamma_sev_step <- stepAIC(gamma_sev, direction="both")


# -----------------------------
# 8. Monte Carlo Portfolio Simulation
# -----------------------------

set.seed(123)

n_sim <- 50000

lambda_hat <- predict(zinb_model, type = "response")
theta_hat <- zinb_model$theta
n_pol <- length(lambda_hat)

sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_mean <- mean(bi_sev_clean$claim_amount)
sev_scale <- sev_mean / sev_shape

aggregate_loss <- numeric(n_sim)

for(s in 1:n_sim){
  
  freq_sim <- rnbinom(n_pol,
                      size = theta_hat,
                      mu = lambda_hat)
  
  total_claims <- sum(freq_sim)
  
  if(total_claims > 0){
    
    severities <- rgamma(total_claims,
                         shape = sev_shape,
                         scale = sev_scale)
    
    aggregate_loss[s] <- sum(severities)
  }
}

BI_mean_loss <- mean(aggregate_loss)
BI_sd_loss <- sd(aggregate_loss)
BI_VaR_99 <- quantile(aggregate_loss,0.99)
BI_TVaR_99 <- mean(aggregate_loss[aggregate_loss > BI_VaR_99])


# -----------------------------
# 9. Copula Dependence Modelling
# -----------------------------

freq <- bi_freq_clean$claim_count
freq_pos <- freq[freq > 0]

sev <- bi_sev_clean$claim_amount

n <- min(length(freq_pos), length(sev))

freq_dep <- freq_pos[1:n]
sev_dep <- sev[1:n]

u <- rank(freq_dep)/(n+1)
v <- rank(sev_dep)/(n+1)

uv_data <- cbind(u,v)

norm_cop <- normalCopula(param=0.5,dim=2)
clay_cop <- claytonCopula(param=2,dim=2)
gumb_cop <- gumbelCopula(param=2,dim=2)
t_cop <- tCopula(param=0.5,df=4,dim=2)

fit_norm <- fitCopula(norm_cop,uv_data,method="ml")
fit_clay <- fitCopula(clay_cop,uv_data,method="ml")
fit_gumb <- fitCopula(gumb_cop,uv_data,method="ml")
fit_t <- fitCopula(t_cop,uv_data,method="ml")

AIC(fit_norm,fit_clay,fit_gumb,fit_t)


# -----------------------------
# 10. Copula Simulation
# -----------------------------

set.seed(123)

n_sim <- 500000

cop_sim <- rCopula(n_sim, fit_norm@copula)

u_sim <- cop_sim[,1]
v_sim <- cop_sim[,2]

freq_sim <- qnbinom(
  u_sim,
  size = zinb_model$theta,
  mu = sample(lambda_hat,n_sim,replace=TRUE)
)

sev_sim <- qgamma(
  v_sim,
  shape = sev_shape,
  scale = sev_scale
)

aggregate_loss <- numeric(n_sim)

for(i in 1:n_sim){
  
  if(freq_sim[i] > 0){
    
    severities <- rgamma(freq_sim[i],
                         shape = sev_shape,
                         scale = sev_scale)
    
    aggregate_loss[i] <- sum(severities)
  }
}


# -----------------------------
# 11. Extreme Risk Measures
# -----------------------------

BI_mean_loss <- mean(aggregate_loss)
BI_sd_loss <- sd(aggregate_loss)

BI_VaR_99 <- quantile(aggregate_loss,0.99)
BI_TVaR_99 <- mean(aggregate_loss[aggregate_loss > BI_VaR_99])

quantile(aggregate_loss,0.995)
quantile(aggregate_loss,0.999)

# -----------------------------
# Stress Test 1: Severity Inflation
# -----------------------------

stress_scale <- sev_scale * 1.2

aggregate_loss_stress1 <- numeric(n_sim)

for(i in 1:n_sim){
  
  if(freq_sim[i] > 0){
    
    severities <- rgamma(freq_sim[i],
                         shape = sev_shape,
                         scale = stress_scale)
    
    aggregate_loss_stress1[i] <- sum(severities)
  }
}

quantile(aggregate_loss_stress1, 0.99)
mean(aggregate_loss_stress1[aggregate_loss_stress1 > quantile(aggregate_loss_stress1,0.99)])

# -----------------------------
# Stress Test 2: Frequency Surge
# -----------------------------

freq_stress <- round(freq_sim * 1.3)

aggregate_loss_stress2 <- numeric(n_sim)

for(i in 1:n_sim){
  
  if(freq_stress[i] > 0){
    
    severities <- rgamma(freq_stress[i],
                         shape = sev_shape,
                         scale = sev_scale)
    
    aggregate_loss_stress2[i] <- sum(severities)
  }
}

quantile(aggregate_loss_stress2, 0.99)

# -----------------------------
# Stress Test 3: Catastrophic Scenario
# -----------------------------

aggregate_loss_stress3 <- numeric(n_sim)

for(i in 1:n_sim){
  
  if(freq_stress[i] > 0){
    
    severities <- rgamma(freq_stress[i],
                         shape = sev_shape,
                         scale = stress_scale)
    
    aggregate_loss_stress3[i] <- sum(severities)
  }
}

quantile(aggregate_loss_stress3,0.99)
quantile(aggregate_loss_stress3,0.999)

data.frame(
  Scenario = c("Baseline","Severity Stress","Frequency Stress","Catastrophic"),
  VaR99 = c(
    quantile(aggregate_loss,0.99),
    quantile(aggregate_loss_stress1,0.99),
    quantile(aggregate_loss_stress2,0.99),
    quantile(aggregate_loss_stress3,0.99)
  )
)

hist(aggregate_loss_stress3,
     breaks=100,
     main="Catastrophic Stress Loss Distribution")