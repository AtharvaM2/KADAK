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

cargo_freq <- read_excel("KADAK/srcsc-2026-claims-cargo.xlsx", sheet = 1)
cargo_sev  <- read_excel("KADAK/srcsc-2026-claims-cargo.xlsx", sheet = 2)

cargo_full <- cargo_freq |>
  left_join(cargo_sev,
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
    claim_amount >= 30000 & claim_amount <= 678000
  )

###################################################################### EDA
# 1. Claim Amount vs Route Risk
blue_palette <- c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")

ggplot(cargo_freq, aes(x = factor(route_risk), y = claim_count,
                       fill = factor(route_risk))) +
  geom_col() +
  scale_fill_manual(values = blue_palette) +
  labs(
    x = "Route Risk",
    y = "Claim Count",
    fill = "Route Risk",
    title = "Claim Amount by Route Risk"
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
    !is.na(cargo_type),
    !str_detect(container_type, "\\d"),
    !str_detect(cargo_type, "\\d")
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

# Claim frequency per exposure
cargo_freq<- cargo_freq |>
  mutate(freq = claim_count / exposure)

# Route Risk vs Frequency
ggplot(cargo_freq, aes(x = factor(route_risk), y = freq)) +
  geom_boxplot(fill = "#6baed6", alpha = 0.6) +
  labs(title = "Claim Frequency by Route Risk",
       x = "Route Risk",
       y = "Claim Frequency (per year)") +
  theme_minimal()

# Solar Radiation vs Frequency
ggplot(cargo_freq, aes(x = solar_radiation, y = freq)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
  labs(title = "Claim Frequency vs Solar Radiation",
       x = "Solar Radiation Index",
       y = "Claim Frequency") +
  theme_minimal()

# Debris Density vs Frequency
ggplot(cargo_freq, aes(x = debris_density, y = freq)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "lm", col = "darkblue", se = FALSE) +
  labs(title = "Claim Frequency vs Debris Density",
       x = "Debris Density Index",
       y = "Claim Frequency") +
  theme_minimal()


######################################################################
# 5. Severity Exploration

# Log transform (heavy-tailed)
cargo_sev <- cargo_sev |>
  mutate(log_claim_amount = log(claim_amount))

cargo_sev <- cargo_sev |>
  filter(!is.na(claim_amount))

ggplot(cargo_sev, aes(x = claim_amount)) +
  geom_histogram(fill = "#3182bd", bins = 40) +
  scale_x_log10(labels = scales::comma) +
  labs(title = "Claim Amount Distribution (Log Scale)",
       x = "Claim Amount (log scale)",
       y = "Count") +
  theme_minimal()

ggplot(cargo_sev, aes(sample = log_claim_amount)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of Log Claim Amount")


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


######################################################################
# 7. Severity Model

sev_model <- glm(
  claim_amount ~ route_risk + solar_radiation + debris_density +
    cargo_value + weight + vessel_age,
  family = Gamma(link = "log"),
  data = cargo_sev
)

summary(sev_model)


hello its anuva 