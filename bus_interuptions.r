library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)
library(stringr)
library(MASS)

# -----------------------------
# Load Data
# -----------------------------

setwd("C:/Users/khush/OneDrive - UNSW/Desktop/ACTL4001")

bus_freq_raw <- read_excel("KADAK/srcsc-2026-claims-business-interruption.xlsx", sheet = 1)
bus_sev_raw <- read_excel("KADAK/srcsc-2026-claims-business-interruption.xlsx", sheet = 2)

# -----------------------------
# Clean Data
# -----------------------------

bus_freq_raw <- bus_freq_raw |>
  mutate(
    solar_system = case_when(
      str_detect(solar_system, "^Zeta") ~ "Zeta",
      str_detect(solar_system, "^Epsilon") ~ "Epsilon",
      str_detect(solar_system, "^Helionis") ~ "Helionis Cluster",
      TRUE ~ solar_system
    )
  )

bus_sev_raw <- bus_sev_raw |>
  mutate(
    solar_system = case_when(
      str_detect(solar_system, "^Zeta") ~ "Zeta",
      str_detect(solar_system, "^Epsilon") ~ "Epsilon",
      str_detect(solar_system, "^Helionis") ~ "Helionis Cluster",
      TRUE ~ solar_system
    )
  )     


bus_freq_clean <- bus_freq_raw |>
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

bus_sev_clean <- bus_sev_raw |>
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
# Combine Data
# -----------------------------

bus_sev_agg <- bus_sev_clean %>%
  group_by(policy_id) %>%
  summarise(
    total_claim_amount = sum(claim_amount, na.rm = TRUE),
    avg_claim_amount   = mean(claim_amount, na.rm = TRUE),
  )

bus_full <- bus_freq_clean %>%
  left_join(bus_sev_agg, by = "policy_id") %>%
  mutate(
    total_claim_amount = coalesce(total_claim_amount, 0),
    avg_claim_amount   = coalesce(avg_claim_amount, 0)
  )
     

# -----------------------------
# EDA for Frequency
# -----------------------------

# 1. Safety Compliance vs Frequency

bus_freq_clean <- bus_freq_clean |>
  mutate(freq = claim_count / exposure)

ggplot(bus_freq_clean, aes(x = factor(safety_compliance), y = freq)) +
  geom_boxplot(fill = "#6baed6", alpha = 0.6) +
  labs(title = "Claim Frequency by Safety Compliance",
       x = "Safety Compliance Score",
       y = "Claim Frequency (per year)") +
  theme_minimal()

# 2. Solar System vs Frequency

ggplot(bus_freq_clean, aes(x = solar_system, y = freq)) +
  geom_boxplot(fill = "#3182bd", alpha = 0.7) +
  labs(title = "Claim Frequency by Solar System",
       x = "Solar System",
       y = "Claim Frequency") +
  theme_minimal()

# -----------------------------
# EDA for Severity
# -----------------------------

# 1.Log Severity Histogram

bus_sev_clean <- bus_sev_clean |>
  mutate(log_claim_amount = log(claim_amount))

ggplot(bus_sev_clean, aes(x = claim_amount)) +
  geom_histogram(fill = "#3182bd", bins = 40) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Claim Amount Distribution (Log Scale)",
       x = "Claim Amount (log scale)",
       y = "Count") +
  theme_minimal()

# 2. QQ Plot (Log Severity)

ggplot(bus_sev_clean, aes(sample = log_claim_amount)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Log Claim Amount")

# -----------------------------
# EDA for Full
# -----------------------------

# 1. Aggregate Loss by Solar System

agg_solar <- bus_full |>
  group_by(solar_system) |>
  summarise(
    loss_per_exposure = sum(total_claim_amount) / sum(exposure),
    .groups = "drop"
  )

ggplot(agg_solar, aes(x = solar_system, y = loss_per_exposure)) +
  geom_col(fill = "#08519c") +
  labs(title = "Aggregate Loss per Exposure by Solar System",
       x = "Solar System",
       y = "Loss per Exposure") +
  theme_minimal()


# -----------------------------
# Frequency Model
# -----------------------------

freq_model <- glm(
  claim_count ~ solar_system +
    production_load +
    energy_backup_score +
    supply_chain_index +
    avg_crew_exp +
    maintenance_freq +
    safety_compliance,
  family = poisson(link = "log"),
  offset = log(exposure),
  data = bus_freq_clean
)

summary(freq_model)


freq_model_nb <- glm.nb(
  claim_count ~ solar_system +
    production_load +
    energy_backup_score +
    supply_chain_index +
    avg_crew_exp +
    maintenance_freq +
    safety_compliance +
    offset(log(exposure)),
  data = bus_freq_clean
)

summary(freq_model_nb)

# -----------------------------
# Severity Model
# -----------------------------

sev_model <- glm(
  claim_amount ~ solar_system +
    production_load +
    energy_backup_score +
    supply_chain_index +
    avg_crew_exp +
    maintenance_freq +
    safety_compliance,
  family = Gamma(link = "log"),
  data = bus_sev_clean
)

summary(sev_model)



# -----------------------------
# Combined Model
# -----------------------------

bus_full <- bus_full |>
  mutate(
    pure_premium = total_claim_amount / exposure
  )

bus_full_pos <- bus_full |>
  filter(pure_premium > 0)

pp_model <- glm(
  pure_premium ~ solar_system +
    production_load +
    energy_backup_score +
    supply_chain_index +
    avg_crew_exp +
    maintenance_freq +
    safety_compliance,
  family = Gamma(link = "log"),
  data = bus_full_pos
)

summary(pp_model)





            