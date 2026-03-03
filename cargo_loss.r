library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)
library(stringr)
library(MASS)
install.packages("fpp3")
library(fpp3)

# -----------------------------
# Load Data
# -----------------------------

cargo_freq <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 1)
cargo_sev  <- read_excel("srcsc-2026-claims-cargo.xlsx", sheet = 2)

cargo_full <- cargo_freq |>
  left_join(cargo_sev,
            by = c("policy_id", "shipment_id"))

cargo_full <- cargo_freq |>
  inner_join(cargo_sev,
            by = c("policy_id", "shipment_id"))

#clean data to be consistent with given value range
cargo_freq <- cargo_freq |>
  filter(
    !is.na(distance) & distance >= 1 & distance <= 100,
    !is.na(cargo_value) & cargo_value >= 50000 & cargo_value <= 680000000,
    !is.na(transit_duration) & transit_duration >= 1 & transit_duration <= 60,
    !is.na(route_risk) & route_risk >= 1 & route_risk <= 5,
    pilot_experience >= 1 & pilot_experience <= 30,
    vessel_age >= 1 & vessel_age <= 50,
    weight >= 1500 & weight <= 250000,
    solar_radiation >= 0 & solar_radiation <= 1,
    debris_density >= 0 & debris_density <= 1,
    exposure >= 0 & exposure <= 1,
    claim_count >= 0 & claim_count <= 5
  )

cargo_sev <- cargo_sev |>
  filter(
    !is.na(distance) & distance >= 1 & distance <= 100,
    !is.na(cargo_value) & cargo_value >= 50000 & cargo_value <= 680000000,
    !is.na(transit_duration) & transit_duration >= 1 & transit_duration <= 60,
    !is.na(route_risk) & route_risk >= 1 & route_risk <= 5,
    pilot_experience >= 1 & pilot_experience <= 30,
    vessel_age >= 1 & vessel_age <= 50,
    weight >= 1500 & weight <= 250000,
    solar_radiation >= 0 & solar_radiation <= 1,
    debris_density >= 0 & debris_density <= 1,
    exposure >= 0 & exposure <= 1,
    claim_amount >= 31000 & claim_amount <= 678000000
  )
 
# Clean variable names (removes unwanted characters from strings _???XXXX)
cargo_freq <- cargo_freq |>
  mutate(across(
    where(is.character),
    ~ str_trim(str_remove(.x, "_.*"))
  ))

cargo_sev <- cargo_sev |>
  mutate(across(
    where(is.character),
    ~ str_trim(str_remove(.x, "_.*"))
  ))


###################################################################### EDA
# 1. Claim Amount vs Route Risk
blue_palette <- c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c",
                  "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

# Ensure exposure-adjusted rate exists
cargo_freq <- cargo_freq |>
  mutate(claim_rate = claim_count / exposure)

ggplot(cargo_freq, aes(x = factor(route_risk), y = claim_count,
                       fill = factor(route_risk))) +
  geom_col() +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Route Risk",
    y = "Claim Count",
    fill = "Route Risk",
    title = "Claim Count by Route Risk"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 2. Container Type vs Cargo Type Heatmap

# Filter out unwanted container types and cargo types
counts <- cargo_freq |>
  filter(
    !is.na(container_type),
    !is.na(cargo_type)
  ) |>
  count(cargo_type, container_type) |>
  complete(cargo_type = unique(cargo_type), container_type =
             unique(container_type),fill = list(n = 0)) |>
  mutate(
    cargo_type = factor(cargo_type, levels = sort(unique(cargo_type))),
    container_type = factor(container_type,
                            levels = sort(unique(container_type)))
  )

# Plot heatmap
heatmap_palette <- c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
                     "#3182bd", "#08519c")

ggplot(counts, aes(x = cargo_type, y = container_type, fill = n)) +
  geom_tile(color = "white", size = 0.4) +
  scale_fill_gradientn(
    colours = heatmap_palette,
    breaks = pretty_breaks(n = 6)
  ) +
  coord_fixed() +
  labs(
    x = "Cargo Type",
    y = "Container Type",
    fill = "Count",
    title = "Heatmap of Cargo Type vs Container Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# 3. Distance vs Transit Time
ggplot(cargo_freq, aes(x = distance, y = transit_duration)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", col = "red") +
  labs(
    x = "Distance (km)",
    y = "Transit Duration (days)",
    title = "Distance vs Transit Duration"
  ) +
  theme_minimal()

######################################################################
# 4. Frequency Exploration

theme_set(theme_minimal(base_size = 14))
blue_fill <- "#9ecae1"

# Route Risk vs Frequency of Claims
freq_summary <- cargo_freq |>
  group_by(route_risk) |>
  summarise(
    avg_claim_count = mean(claim_count),
    total_exposure = sum(exposure),
    claim_rate = sum(claim_count) / sum(exposure)
  )

ggplot(freq_summary, aes(x = factor(route_risk), y = claim_rate)) +
  geom_col(fill = "#9ecae1", width = 0.6) +
  geom_text(aes(label = round(claim_rate, 3)),
            vjust = -0.5,
            size = 4) +
  labs(title = "Claim Frequency by Route Risk",
       subtitle = "Exposure-adjusted claim rate",
       x = "Route Risk Level",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14)

# Route Risk vs Severity of Claims
route_sev <- cargo_sev |>
  group_by(route_risk) |>
  summarise(avg_severity = mean(claim_amount))

ggplot(route_sev, aes(x = factor(route_risk), y = avg_severity)) +
  geom_col(fill = blue_fill, width = 0.6) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Route Risk",
       x = "Route Risk",
       y = "Average Claim Amount")

# Solar Radiation vs Frequency
cargo_freq <- cargo_freq |>
  mutate(radiation_band = cut(solar_radiation,
                              breaks = seq(0, 1, by = 0.1)))

rad_summary <- cargo_freq |>
  group_by(radiation_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(rad_summary, aes(x = radiation_band, y = claim_rate)) +
  geom_col(fill = "#41B6C4") +
  labs(title = "Claim Frequency by Solar Radiation Band",
       x = "Solar Radiation Band",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14)

# Solar Radiation vs Claim Count
ggplot(cargo_freq, aes(x = factor(radiation_band), y = claim_count,
                       fill = factor(radiation_band))) +
  geom_col() +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Radiation Band",
    y = "Claim Count",
    fill = "Radiation Band",
    title = "Claim Count by Radiation Band"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Solar Radiation vs Claim Severity
cargo_sev <- cargo_sev |>
  mutate(radiation_band = cut(solar_radiation,
                              breaks = seq(0, 1, by = 0.1)))

rad_sev <- cargo_sev |>
  group_by(radiation_band) |>
  summarise(avg_severity = mean(claim_amount))

ggplot(rad_sev, aes(x = radiation_band, y = avg_severity)) +
  geom_col(fill = blue_fill) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Severity by Solar Radiation",
       x = "Solar Radiation Band",
       y = "Average Claim Amount")

# Debris Density vs Frequency
cargo_freq <- cargo_freq |>
  mutate(debris_band = cut(debris_density,
                           breaks = seq(0, 1, by = 0.1)))

debris_summary <- cargo_freq |>
  group_by(debris_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(debris_summary, aes(x = debris_band, y = claim_rate)) +
  geom_col(fill = "#225EA8") +
  labs(title = "Claim Frequency by Debris Density Band",
       x = "Debris Density Band",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14)

# Debris Density vs Claim Count
ggplot(cargo_freq, aes(x = factor(debris_band), y = claim_count,
                       fill = factor(debris_band))) +
  geom_col() +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Debris Density",
    y = "Claim Count",
    fill = "Debris Density",
    title = "Claim Count by Debris Density"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Debris Density vs Claim Severity
cargo_sev <- cargo_sev |>
  mutate(debris_band = cut(debris_density,
                           breaks = seq(0, 1, by = 0.1)))

debris_sev <- cargo_sev |>
  group_by(debris_band) |>
  summarise(avg_severity = mean(claim_amount))

ggplot(debris_sev, aes(x = debris_band, y = avg_severity)) +
  geom_col(fill = blue_fill) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Severity by Debris Density",
       x = "Debris Density Band",
       y = "Average Claim Amount")

# Cargo Type vs Frequency 
cargo_type_freq <- cargo_freq |>
  group_by(cargo_type) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(cargo_type_freq, aes(x = reorder(cargo_type, claim_rate),
                            y = claim_rate)) +
  geom_text(aes(label = round(claim_rate, 3)),
            vjust = -0.5,
            size = 4) +
  geom_col(fill = blue_fill) +
  labs(title = "Claim Frequency by Cargo Type",
       x = "Cargo Type",
       y = "Claims per Exposure Year")

# Cargo Type vs Claim Count
ggplot(cargo_freq, aes(x = reorder(cargo_type, claim_count), y = claim_count,
                       fill = factor(cargo_type))) +
  geom_col() +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Cargo Type",
    y = "Claim Count",
    fill = "Cargo Type",
    title = "Claim Count by Cargo Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Cargo Severity
cargo_type_sev <- cargo_sev |>
  group_by(cargo_type) |>
  summarise(avg_severity = mean(claim_amount))

ggplot(cargo_type_sev,
       aes(x = reorder(cargo_type, avg_severity),
           y = avg_severity)) +
  geom_text(aes(label = round(avg_severity, 3)),
            vjust = -0.5,
            size = 4) +
  geom_col(fill = blue_fill) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Cargo Type",
       x = "Cargo Type",
       y = "Average Claim Amount")

# Cargo Type vs Cargo Value
ggplot(cargo_freq, aes(x = factor(cargo_type), y = cargo_value,
                       fill = factor(cargo_type))) +
  geom_col() +
  scale_y_continuous(labels = scales::comma) + 
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Cargo Type",
    y = "Cargo Value",
    fill = "Cargo Type",
    title = "Cargo Value by Cargo Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Container Type vs Frequency 
freq_summary_container <- cargo_freq |>
  group_by(container_type) |>
  summarise(
    avg_claim_count = mean(claim_count),
    total_exposure = sum(exposure),
    claim_rate = sum(claim_count) / sum(exposure)
  )

ggplot(freq_summary_container, aes(x = factor(container_type), y = claim_rate)) +
  geom_col(fill = "#9ecae1", width = 0.6) +
  geom_text(aes(label = round(claim_rate, 3)),
            vjust = -0.5,
            size = 4) +
  labs(title = "Claim Frequency by Container Type",
       subtitle = "Exposure-adjusted claim rate",
       x = "Container type",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14)

# Container Type vs Claim Count
ggplot(cargo_freq, aes(x = factor(container_type), y = claim_count,
                       fill = factor(container_type))) +
  geom_col() +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Container Type",
    y = "Claim Count",
    fill = "Container Type",
    title = "Claim Count by Cargo Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Container Type vs Claim Severity
container_sev <- cargo_sev |>
  group_by(container_type) |>
  summarise(avg_severity = mean(claim_amount))

ggplot(container_sev,
       aes(x = reorder(container_type, avg_severity),
           y = avg_severity)) +
  geom_text(aes(label = round(avg_severity, 3)),
            vjust = -0.5,
            size = 4) +
  geom_col(fill = blue_fill) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Container Type",
       x = "Container Type",
       y = "Average Claim Amount")


# Create sensible bands for numeric rating variables
cargo_freq <- cargo_freq |>
  mutate(
    distance_band = cut(distance, breaks = seq(0, 100, by = 10)),
    duration_band = cut(transit_duration, breaks = seq(0, 60, by = 5)),
    pilot_band    = cut(pilot_experience, breaks = seq(0, 30, by = 5)),
    vessel_band   = cut(vessel_age, breaks = seq(0, 50, by = 5))
  )

cargo_sev <- cargo_sev |>
  mutate(
    distance_band = cut(distance, breaks = seq(0, 100, by = 10)),
    duration_band = cut(transit_duration, breaks = seq(0, 60, by = 5)),
    pilot_band    = cut(pilot_experience, breaks = seq(0, 30, by = 5)),
    vessel_band   = cut(vessel_age, breaks = seq(0, 50, by = 5))
  )

# Claim frequency vs Distance
dist_freq <- cargo_freq |>
  group_by(distance_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(dist_freq, aes(distance_band, claim_rate)) +
  geom_col(fill = "#3182bd") +
  labs(title = "Claim Frequency by Distance",
       x = "Distance (km)",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Claim Count vs Distance
dist_count <- cargo_freq |>
  group_by(distance_band) |>
  summarise(total_claims = sum(claim_count))

ggplot(dist_count, aes(distance_band, total_claims)) +
  geom_col(fill = "#6baed6") +
  labs(title = "Total Claim Count by Distance",
       x = "Distance (km)",
       y = "Total Claims") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Claim Severity vs Distance
dist_sev <- cargo_sev |>
  group_by(distance_band) |>
  summarise(mean_severity = mean(claim_amount))

ggplot(dist_sev, aes(distance_band, mean_severity)) +
  geom_col(fill = "#08519c") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Distance",
       x = "Distance (km)",
       y = "Average Claim Amount") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transit Duration vs Claim Frequency
dur_freq <- cargo_freq |>
  group_by(duration_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(dur_freq, aes(duration_band, claim_rate)) +
  geom_col(fill = "#2b8cbe") +
  labs(title = "Claim Frequency by Transit Duration",
       x = "Transit Duration (days)",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transit Duration vs Claim Count
dur_count <- cargo_freq |>
  group_by(duration_band) |>
  summarise(total_claims = sum(claim_count))

ggplot(dur_count, aes(duration_band, total_claims)) +
  geom_col(fill = "#74a9cf") +
  labs(title = "Total Claim Count by Transit Duration",
       x = "Transit Duration (days)",
       y = "Total Claims") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Transit Duration vs Claim Severity
dur_sev <- cargo_sev |>
  group_by(duration_band) |>
  summarise(mean_severity = mean(claim_amount))

ggplot(dur_sev, aes(duration_band, mean_severity)) +
  geom_col(fill = "#045a8d") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Transit Duration",
       x = "Transit Duration (days)",
       y = "Average Claim Amount") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pilot Experience vs Claim Frequency
pilot_freq <- cargo_freq |>
  group_by(pilot_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(pilot_freq, aes(pilot_band, claim_rate)) +
  geom_col(fill = "#41b6c4") +
  labs(title = "Claim Frequency by Pilot Experience",
       x = "Pilot Experience (Years)",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pilot Experience vs Claim Count
pilot_count <- cargo_freq |>
  group_by(pilot_band) |>
  summarise(total_claims = sum(claim_count))

ggplot(pilot_count, aes(pilot_band, total_claims)) +
  geom_col(fill = "#7fcdbb") +
  labs(title = "Total Claim Count by Pilot Experience",
       x = "Pilot Experience (Years)",
       y = "Total Claims") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pilot Experience vs Claim Severity
pilot_sev <- cargo_sev |>
  group_by(pilot_band) |>
  summarise(mean_severity = mean(claim_amount))

ggplot(pilot_sev, aes(pilot_band, mean_severity)) +
  geom_col(fill = "#0868ac") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Pilot Experience",
       x = "Pilot Experience (Years)",
       y = "Average Claim Amount") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vessel Age vs Claim Frequency
vessel_freq <- cargo_freq |>
  group_by(vessel_band) |>
  summarise(claim_rate = sum(claim_count) / sum(exposure))

ggplot(vessel_freq, aes(vessel_band, claim_rate)) +
  geom_col(fill = "#225ea8") +
  labs(title = "Claim Frequency by Vessel Age",
       x = "Vessel Age (Years)",
       y = "Claims per Exposure Year") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vessel Age vs Claim Count
vessel_count <- cargo_freq |>
  group_by(vessel_band) |>
  summarise(total_claims = sum(claim_count))

ggplot(vessel_count, aes(vessel_band, total_claims)) +
  geom_col(fill = "#6baed6") +
  labs(title = "Total Claim Count by Vessel Age",
       x = "Vessel Age (Years)",
       y = "Total Claims") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Vessel Age vs Claim Severity
vessel_sev <- cargo_sev |>
  group_by(vessel_band) |>
  summarise(mean_severity = mean(claim_amount))

ggplot(vessel_sev, aes(vessel_band, mean_severity)) +
  geom_col(fill = "#08306b") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Average Claim Severity by Vessel Age",
       x = "Vessel Age (Years)",
       y = "Average Claim Amount") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

######################################################################
# 5. Forward Stepwise and Backwards Stepwise Functions

quantile(cargo_sev$claim_amount, probs = c(0.95,0.99,0.995))

full_nb <- glm.nb(
  claim_count ~ route_risk + solar_radiation +
    debris_density + distance + transit_duration +
    pilot_experience + vessel_age + weight +
    offset(log(exposure)),
  data = cargo_freq
)

null_nb <- glm.nb(
  claim_count ~ 1 + offset(log(exposure)),
  data = cargo_freq
)

step_forward <- stepAIC(null_nb,
                        scope = list(lower = null_nb,
                                     upper = full_nb),
                        direction = "forward")
summary(step_forward)

step_backward <- stepAIC(full_nb,
                         direction = "backward")
summary(step_backward)


AIC(freq_model_nb, step_forward, step_backward)

######################################################################
# 6. Frequency GLM

freq_model <- glm(
  claim_count ~ route_risk + solar_radiation + debris_density +
    distance + transit_duration + pilot_experience +
    vessel_age + weight,
  family = poisson(link = "log"),
  offset = log(exposure),
  data = cargo_freq
)

summary(freq_model)

### Negative binomial model
freq_model_nb <- glm.nb(
  claim_count ~ route_risk + solar_radiation + debris_density +
    distance + transit_duration + pilot_experience +
    vessel_age + weight + offset(log(exposure)),
  data = cargo_freq
)

summary(freq_model_nb)


freq_model_nb_2 <- glm.nb(
  claim_count ~ route_risk + solar_radiation + debris_density +
    distance + transit_duration + pilot_experience + container_type + 
    vessel_age + weight + cargo_type + cargo_value + offset(log(exposure)),
  data = cargo_freq
)

summary(freq_model_nb_2)

######################################################################
# 7. Severity Model

sev_model <- glm(
  claim_amount ~ route_risk + solar_radiation + debris_density +
    cargo_value + weight + vessel_age +  distance + transit_duration + pilot_experience + container_type + 
    cargo_type + cargo_value,
  family = Gamma(link = "log"),
  data = cargo_sev
)

summary(sev_model)


######################################################################
# 8. Monte Carlo Simulation for Aggregate Losses
set.seed(123)
n_sim <- 50000

# Predict mean frequency
lambda_hat <- predict(step_backward, type = "response")

# Use average lambda
lambda_mean <- mean(lambda_hat)

# Simulate frequency
freq_sim <- rnbinom(n_sim,
                    size = step_backward$theta,
                    mu = lambda_mean)

# Severity parameters
sev_shape <- 1 / summary(sev_model)$dispersion
sev_scale <- mean(cargo_sev$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    aggregate_loss[i] <-
      sum(rgamma(freq_sim[i],
                 shape = sev_shape,
                 scale = sev_scale))
  }
}


VaR_99 <- quantile(aggregate_loss, 0.99)

TVaR_99 <- mean(aggregate_loss[aggregate_loss > VaR_99])

mean_loss <- mean(aggregate_loss)
sd_loss   <- sd(aggregate_loss)


