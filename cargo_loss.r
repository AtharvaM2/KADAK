library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(scales)
library(tidyr)
library(stringr)
library(MASS)
library(fpp3)
library(pscl)
library(ggplot2)
library(dplyr)
library(scales)
library(VineCopula)

# -----------------------------
# Load Data
# -----------------------------

cargo_freq <- read_excel("KADAK/srcsc-2026-claims-cargo.xlsx", sheet = 1)
cargo_sev  <- read_excel("KADAK/srcsc-2026-claims-cargo.xlsx", sheet = 2)

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


# Cargo Severity Pie Chart 
ggplot(cargo_type_sev, aes(x = "", y = avg_severity, fill = reorder(cargo_type, -avg_severity))) +
  geom_col(width = 1, color = "white") + 
  coord_polar(theta = "y") +             
  theme_void() +                         # Remove axis, grid, background
  labs(title = "Average Claim Severity by Cargo Type",
       fill = "Cargo Type") +
  theme(plot.title = element_text(hjust = 0.5))

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


# Separate Data in Cargo Types
plot_data <- cargo_sev %>%
  mutate(cargo_type = tolower(cargo_type)) %>%
  filter(cargo_type %in% c("supplies", "titanium", "rare_earths", "cobalt", 
                           "lithium", "gold", "platinum", "na"))

# 2. Smooth Filled Plot with Claim Counts
ggplot(plot_data, aes(x = claim_amount, fill = cargo_type, color = cargo_type)) +
  geom_density(aes(y = after_stat(count)), alpha = 0.3, size = 0.8) + 
  
  scale_x_log10(labels = scales::label_comma(), 
                breaks = c(1000, 10000, 100000, 1000000, 10000000, 100000000, 500000000)) +
  
  scale_fill_manual(values = c(
    "gold" = "#D4AF37", "platinum" = "#7F7F7F", "lithium" = "#8E44AD",
    "cobalt" = "#2980B9", "rare_earths" = "#27AE60", "titanium" = "#E67E22",
    "supplies" = "#95A5A6", "na" = "#34495E"
  )) +
  scale_color_manual(values = c(
    "gold" = "#D4AF37", "platinum" = "#7F7F7F", "lithium" = "#8E44AD",
    "cobalt" = "#2980B9", "rare_earths" = "#27AE60", "titanium" = "#E67E22",
    "supplies" = "#95A5A6", "na" = "#34495E"
  )) +
  
  labs(
    title = "Smooth Distribution of Claim Counts",
    subtitle = "Relative frequency of claims by dollar amount (Log Scale)",
    x = "Claim Amount",
    y = "Number of Claims",
    fill = "Cargo Type",
    color = "Cargo Type"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )


# Create the Summary Table
cargo_summary_table <- plot_data |>
  group_by(Cargo = toupper(cargo_type)) |>
  summarise(
    `Total Claims` = n(),
    `Average Severity` = mean(claim_amount),
    `Median Severity` = median(claim_amount),
    `Min Claim` = min(claim_amount),
    `Max Claim` = max(claim_amount),
    `Total Loss Value` = sum(claim_amount)
  ) |>
  arrange(desc(`Average Severity`))

# To view the table 
print(cargo_summary_table)

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
######################################################################

# Data Cleaning
quantile(cargo_sev$claim_amount, probs = c(0.95,0.99,0.995))

model_cargo_freq <- cargo_freq
model_cargo_sev <- cargo_sev


# Fix exposure: replace zeros or NAs
model_cargo_freq$exposure[model_cargo_freq$exposure <= 0 | is.na(model_cargo_freq$exposure)] <- 0.001
model_cargo_freq$claim_count <- round(model_cargo_freq$claim_count)
model_cargo_freq$exposure[model_cargo_freq$exposure <= 0] <- 0.001
model_cargo_freq$log_exposure <- log(model_cargo_freq$exposure)
model_cargo_freq <- model_cargo_freq[!is.na(model_cargo_freq$claim_count), ]


predictors <- c("cargo_type","cargo_value","weight",
                "route_risk","distance",
                "transit_duration","pilot_experience","vessel_age",
                "container_type","solar_radiation","debris_density",
                "exposure")

# Remove any rows with NA in these columns
model_cargo_freq <- model_cargo_freq %>%
  dplyr::filter(!if_any(all_of(predictors), is.na))


#######################################
# Forward Stepwise and Backwards Stepwise Functions 
#######################################
null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_cargo_freq
)

full_freq <- glm(
  claim_count ~ cargo_type + cargo_value + weight + route_risk + solar_radiation +
    container_type + debris_density + distance + transit_duration +
    pilot_experience + vessel_age + weight +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_cargo_freq
)
summary(full_freq)

step_backward <- stepAIC(full_freq,
                         direction = "backward")
summary(step_backward)

upper_formula <- claim_count ~ cargo_type + cargo_value + weight + route_risk + solar_radiation +
  container_type + debris_density + distance + transit_duration +
  pilot_experience + vessel_age + weight +
  offset(log(exposure))

# Forward stepwise selection
step_forward <- stepAIC(
  null_freq,
  scope = list(
    lower = ~1,
    upper = upper_formula
  ),
  direction = "forward"
)

summary(step_forward)

AIC(full_freq, step_forward, step_backward)

######################################################################
# 6. Frequency GLM

freq_model <- glm(
  claim_count ~ route_risk + pilot_experience + solar_radiation + debris_density +
    container_type + cargo_type + cargo_value + offset(log(exposure)),
  family = poisson(link = "log"),
  offset = log(exposure),
  data = model_cargo_freq
)

summary(freq_model)

### Negative binomial model
freq_model_nb <- glm.nb(
  claim_count ~ route_risk + pilot_experience + solar_radiation + debris_density +
    container_type + cargo_type + cargo_value + offset(log(exposure)),
  data = model_cargo_freq
)

summary(freq_model_nb)

AIC(freq_model_nb, step_forward, step_backward)

######################################################################
# 7. Severity Model

model_cargo_sev$claim_amount[model_cargo_sev$claim_amount <= 0] <- 0.01
model_cargo_sev <- na.omit(model_cargo_sev)

numeric_vars <- c("route_risk", "solar_radiation", "debris_density",
                  "cargo_value", "weight", "vessel_age",
                  "distance", "transit_duration", "pilot_experience")

model_cargo_sev[numeric_vars] <- scale(model_cargo_sev[numeric_vars])

sev_model <- glm(
  claim_amount ~ route_risk + solar_radiation + debris_density +
    cargo_value + weight + vessel_age +  distance + transit_duration + pilot_experience + container_type + 
    cargo_type,
  family = Gamma(link = "log"),
  data = model_cargo_sev
)

summary(sev_model)

######################################################################
# 8. Monte Carlo Simulation for Aggregate Losses
set.seed(123)
n_sim <- 50000

# Predict mean frequency
lambda_hat <- predict(freq_model_nb, type = "response")

# Use average lambda
lambda_mean <- mean(lambda_hat)

# Simulate frequency
freq_sim <- rnbinom(n_sim,
                    size = freq_model_nb$theta,
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


CL_VaR_99 <- quantile(aggregate_loss, 0.99)
CL_VaR_99

CL_TVaR_99 <- mean(aggregate_loss[aggregate_loss > VaR_99])
CL_TVaR_99

CL_mean_loss <- mean(aggregate_loss)
CL_mean_loss

CL_sd_loss   <- sd(aggregate_loss)
CL_sd_loss


#######################################
# Monte Carlo Simulation Pt. 2
#######################################

set.seed(123)

n_sim <- 10000   # 10k is usually enough for stable 99% VaR

# --- 1. Frequency model inputs ---
lambda_hat_sev <- predict(freq_model_nb, type = "response")
theta_hat_sev  <- freq_model_nb$theta

n_pol <- length(lambda_hat_sev)

# --- 2. Severity model inputs ---
# Severity parameters
sev_shape <- 1 / summary(sev_model)$dispersion
sev_scale <- mean(cargo_sev$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

# --- 3. Monte Carlo Simulation ---
aggregate_loss <- numeric(n_sim)

for(s in 1:n_sim){
  
  # Simulate frequency for ALL policies
  freq_sim <- rnbinom(n_pol,
                      size = theta_hat_sev,
                      mu   = lambda_hat_sev)
  
  total_claims <- sum(freq_sim)
  
  if(total_claims > 0){
    
    # Simulate all severities in one vectorised draw
    severities <- rgamma(total_claims,
                         shape = sev_shape,
                         scale = sev_scale)
    
    aggregate_loss[s] <- sum(severities)
  }
}

# --- 4. Risk Metrics ---
sev_mean_loss <- mean(aggregate_loss)
sev_sd_loss   <- sd(aggregate_loss)
sev_VaR_99    <- quantile(aggregate_loss, 0.99)
sev_TVaR_99   <- mean(aggregate_loss[aggregate_loss > sev_VaR_99])

sev_mean_loss
sev_sd_loss
sev_VaR_99
sev_TVaR_99

total_sev <- sum(cargo_sev$claim_amount)
gold_sev <- sum(cargo_sev$claim_amount) - sum(cargo_sev$claim_amount[cargo_sev$cargo_type != "gold"], na.rm = TRUE)
platinum_sev <- sum(cargo_sev$claim_amount) - sum(cargo_sev$claim_amount[cargo_sev$cargo_type != "platinum"], na.rm = TRUE)

gold_sev/total_sev
platinum_sev/total_sev
1 - (gold_sev + platinum_sev)/total_sev


total_freq <- sum(cargo_freq$claim_count)
gold_freq <- sum(cargo_freq$claim_count) - sum(cargo_freq$claim_count[cargo_freq$cargo_type != "gold"], na.rm = TRUE)
platinum_freq <- sum(cargo_freq$claim_count) - sum(cargo_freq$claim_count[cargo_freq$cargo_type != "platinum"], na.rm = TRUE)

gold_freq/total_freq
platinum_freq/total_freq
1 - (gold_freq + platinum_freq)/total_freq
##################################################################
# 9. Correlated Multi-Solar System Shock Model
##################################################################
common_shock <- rlnorm(n_sim, meanlog = 0, sdlog = 0.25)

portfolio_loss <-
  (agg_cargo +
     agg_equipment +
     agg_wc +
     agg_bi) * common_shock

VaR_portfolio_99 <- quantile(portfolio_loss, 0.99)

VaR_sum <- VaR_cargo + VaR_equipment + VaR_wc + VaR_bi

diversification_benefit <- VaR_sum - VaR_portfolio_99 

##################################################################
# 10. Tail Behaviour Observations
##################################################################
library(evir)

# Hill estimator
hill_est <- hill(cargo_sev$claim_amount, start = 100)
plot(hill_est)

# Mean Excess Plot
meplot(cargo_sev$claim_amount)

# Fit Generalized Pareto to upper tail
threshold <- quantile(cargo_sev$claim_amount, 0.95)

gpd_fit <- gpd(cargo_sev$claim_amount, threshold)
summary(gpd_fit)


#######################################
# Tail Dependence
####################################### 

# Install if needed: install.packages("VineCopula")
library(VineCopula)

freq <- cargo_freq$claim_count
freq_pos <- freq[freq > 0]
sev <- cargo_sev$claim_amount
n <- min(length(freq_pos), length(sev))

# Prepare data
dep_data <- data.frame(freq_dep = freq_pos[1:n], sev_dep = sev[1:n])
u <- rank(dep_data$freq_dep) / (n + 1)
v <- rank(dep_data$sev_dep)  / (n + 1)

# --- 1. Automatic Copula Selection and Fitting ---
# This replaces fitCopula. It will test Gaussian, t, Clayton, Gumbel, etc., 
# and pick the best one based on AIC/BIC.
fit_cop <- BiCopSelect(u, v, familyset = NA) # NA tests all available families

summary(fit_cop)

# --- 2. Simulation ---
set.seed(123)
n_sim <- 500000

# Simulate from the fitted bivariate copula
sim_data <- BiCopSim(n_sim, fit_cop$family, fit_cop$par, fit_cop$par2)
u_sim <- sim_data[,1]
v_sim <- sim_data[,2]

# --- 3. Marginal Transformations ---
lambda_hat <- predict(freq_model_nb, type = "response")

# Frequency simulation
freq_sim <- qnbinom(u_sim,
                    size = freq_model_nb$theta,
                    mu = sample(lambda_hat, n_sim, replace = TRUE))

# Severity simulation
sev_shape <- 1 / summary(sev_model)$dispersion
sev_scale <- mean(cargo_sev$claim_amount) / sev_shape

# --- 4. Aggregate Loss Calculation ---
# Optimization: Using a vectorized approach for Gamma simulation is much faster than a loop
aggregate_loss <- sapply(freq_sim, function(n_claims) {
  if(n_claims > 0) {
    return(sum(rgamma(n_claims, shape = sev_shape, scale = sev_scale)))
  } else {
    return(0)
  }
})

# --- 5. Risk Metrics ---
CL_VaR_99_copula <- quantile(aggregate_loss, 0.99)
CL_TVaR_99_copula <- mean(aggregate_loss[aggregate_loss > CL_VaR_99_copula])

# --- 6. Plotting Different Copulas (Comparison) ---
# To manually simulate specific families for your plots:
# Family codes: 1 = Gaussian, 2 = t, 3 = Clayton, 4 = Gumbel
par(mfrow = c(2,2))

# Gaussian (Family 1)
fit_norm <- BiCopEst(u, v, family = 1)
sim_norm <- BiCopSim(5000, 1, fit_norm$par)
plot(sim_norm, main = "Gaussian Copula", xlab = "U", ylab = "V", pch=20, col=rgb(0,0,1,0.2))

# Clayton (Family 3)
fit_clay <- BiCopEst(u, v, family = 3)
sim_clay <- BiCopSim(5000, 3, fit_clay$par)
plot(sim_clay, main = "Clayton Copula", xlab = "U", ylab = "V", pch=20, col=rgb(1,0,0,0.2))

# Gumbel (Family 4)
fit_gumb <- BiCopEst(u, v, family = 4)
sim_gumb <- BiCopSim(5000, 4, fit_gumb$par)
plot(sim_gumb, main = "Gumbel Copula", xlab = "U", ylab = "V", pch=20, col=rgb(0,1,0,0.2))

# t-Copula (Family 2)
fit_t <- BiCopEst(u, v, family = 2)
sim_t <- BiCopSim(5000, 2, fit_t$par, fit_t$par2)
plot(sim_t, main = "t-Copula", xlab = "U", ylab = "V", pch=20, col=rgb(0.5,0,0.5,0.2))


#extreme scenario risk 
quantile(aggregate_loss, 0.995) ##solvency risk 
quantile(aggregate_loss, 0.999) ##catastrophic risk 


#############################################################################
# Separate Model for Gold and Platinum
# --- Segment 1: High-Value (Gold & Platinum) ---
freq_high_value <- model_cargo_freq %>% 
  filter(cargo_type %in% c("gold", "platinum"))

sev_high_value <- model_cargo_sev %>% 
  filter(cargo_type %in% c("gold", "platinum"))

# --- Segment 2: Attritional (Everything Else) ---
freq_attritional <- model_cargo_freq %>% 
  filter(!(cargo_type %in% c("gold", "platinum")))

sev_attritional <- model_cargo_sev %>% 
  filter(!(cargo_type %in% c("gold", "platinum")))


# Fit High-Value Severity
sev_model_high <- glm(claim_amount ~ route_risk + solar_radiation + debris_density +
                        cargo_value + weight + vessel_age +  distance + transit_duration + pilot_experience + container_type + 
                        cargo_type,
                      family = Gamma(link = "log"), data = sev_high_value)
summary(sev_model_high)

# Fit Attritional Severity
sev_model_attr <- glm(claim_amount ~ route_risk + solar_radiation + debris_density +
                        cargo_value + weight + vessel_age +  distance + transit_duration + pilot_experience + container_type + 
                        cargo_type,
                      family = Gamma(link = "log"), data = sev_attritional)
summary(sev_model_attr)

# Get parameters for Simulation
shape_high <- 1 / summary(sev_model_high)$dispersion
scale_high <- mean(sev_high_value$claim_amount) / shape_high

shape_attr <- 1 / summary(sev_model_attr)$dispersion
scale_attr <- mean(sev_attritional$claim_amount) / shape_attr


set.seed(123)
n_sim <- 50000
agg_high <- numeric(n_sim)
agg_attr <- numeric(n_sim)

# Predict averages for each segment
mu_high <- mean(predict(freq_model_nb, newdata = freq_high_value, type = "response"))
mu_attr <- mean(predict(freq_model_nb, newdata = freq_attritional, type = "response"))

for(s in 1:n_sim){
  # 1. Simulate High-Value (Gold/Platinum)
  f_high <- rnbinom(1, size = freq_model_nb$theta, mu = mu_high * nrow(freq_high_value))
  if(f_high > 0) agg_high[s] <- sum(rgamma(f_high, shape = shape_high, scale = scale_high))
  
  # 2. Simulate Attritional
  f_attr <- rnbinom(1, size = freq_model_nb$theta, mu = mu_attr * nrow(freq_attritional))
  if(f_attr > 0) agg_attr[s] <- sum(rgamma(f_attr, shape = shape_attr, scale = scale_attr))
}

# --- Risk Metrics Comparison ---
results <- data.frame(
  Metric = c("Mean Loss", "99% VaR", "TVaR (Tail Risk)"),
  High_Value_Gold_Plat = c(mean(agg_high), quantile(agg_high, 0.99), mean(agg_high[agg_high > quantile(agg_high, 0.99)])),
  Attritional_Cargo = c(mean(agg_attr), quantile(agg_attr, 0.99), mean(agg_attr[agg_attr > quantile(agg_attr, 0.99)]))
)
print(results)


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


# ---------------------------------------------------------
# 1. SETUP & SYSTEM-SPECIFIC PARAMETERS
# ---------------------------------------------------------
set.seed(2174) # Reference year
n_sim <- 100000

# Define systems based on Case Study profiles
systems <- c("Helionis", "Bayesia", "Oryn_Delta")

# Base Frequency (λ) and Severity (Gamma params) per system
# Oryn Delta has higher severity due to 'asymmetric asteroid ring' risks
sys_params <- list(
  Helionis    = list(lambda = 12, shape = 2.0, scale = 50), 
  Bayesia     = list(lambda = 18, shape = 1.5, scale = 70), # Radiation spikes
  Oryn_Delta  = list(lambda = 15, shape = 1.2, scale = 120) # High-risk gravity shear
)

# ---------------------------------------------------------
# 2. CORRELATED RISK SCENARIO: The "Great Flare" (Stress 4)
# ---------------------------------------------------------
# Actuarial requirement: Dependency-based methods for multi-system events
solar_storm_occurs <- rbinom(n_sim, 1, 0.01) # 1-in-100 year event

# ---------------------------------------------------------
# 3. MULTI-SYSTEM SIMULATION ENGINE
# ---------------------------------------------------------
run_simulation <- function(stress_freq = 1.0, stress_sev = 1.0, correlated_event = FALSE) {
  total_losses <- numeric(n_sim)
  
  for(s in systems) {
    p <- sys_params[[s]]
    
    # Simulate Frequency (Poisson)
    freq <- rpois(n_sim, p$lambda * stress_freq)
    
    # Apply Correlated Event Impact
    if(correlated_event) {
      # If a solar storm hits, frequency increases by 5x for that year
      freq <- freq + (solar_storm_occurs * rpois(n_sim, p$lambda * 5))
    }
    
    # Simulate Severity (Gamma)
    sys_loss <- sapply(freq, function(f) {
      if(f > 0) sum(rgamma(f, shape = p$shape, scale = p$scale * stress_sev)) else 0
    })
    
    total_losses <- total_losses + sys_loss
  }
  return(total_losses)
}

# ---------------------------------------------------------
# 4. EXECUTE SCENARIOS
# ---------------------------------------------------------
baseline     <- run_simulation()
sev_stress   <- run_simulation(stress_sev = 1.4) # Inflation/Tech failure
freq_stress  <- run_simulation(stress_freq = 1.5) # Regulatory/Safety lapse
catastrophic <- run_simulation(stress_freq = 1.5, stress_sev = 1.4, correlated_event = TRUE)

# ---------------------------------------------------------
# 5. METRICS & TAIL BEHAVIOR (VaR and TVaR)
# ---------------------------------------------------------
calc_metrics <- function(losses, name) {
  var_99  <- quantile(losses, 0.99)
  tvar_99 <- mean(losses[losses > var_99]) # Tail Value at Risk
  
  data.frame(
    Scenario = name,
    Mean = mean(losses),
    VaR_99 = var_99,
    TVaR_99 = tvar_99,
    Max_Loss = max(losses)
  )
}

summary_table <- rbind(
  calc_metrics(baseline, "Baseline"),
  calc_metrics(sev_stress, "Severity Stress (+40%)"),
  calc_metrics(freq_stress, "Frequency Stress (+50%)"),
  calc_metrics(catastrophic, "Systemic Catastrophe (Correlated)")
)

print(summary_table)

# ---------------------------------------------------------
# 6. VISUALIZING THE TAIL (For the Report)
# ---------------------------------------------------------
hist(catastrophic, breaks = 100, col = rgb(1,0,0,0.5), 
     main = "Aggregate Loss Distribution: Catastrophic Scenario",
     xlab = "Loss (Đ Millions)", xlim = c(0, quantile(catastrophic, 0.999)))
abline(v = summary_table$VaR_99[4], col = "blue", lwd = 2, lty = 2)
legend("topright", legend = c("99% VaR"), col = "blue", lty = 2)
