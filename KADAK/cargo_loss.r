library(ggplot2)
library(readxl)
library(dplyr)
cargo_data <- read_excel("KADAK/srcsc-2026-claims-cargo.xlsx")

#clean data to be consistent with given value range
cargo_data <- cargo_data |>
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