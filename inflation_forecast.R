library(forecast)
library(tsibble)
library(dplyr)
library(fable)
library(feasts)
library(readxl)
library(ggplot2)
library(tidyr)

# Load Excel
inflation <- read_excel("srcsc-2026-interest-and-inflation.xlsx")
colnames(inflation) <- as.character(inflation[2, ])
inflation <- inflation[-c(1,2), ]

# Convert numeric
inflation$Year <- as.numeric(inflation$Year)
inflation$Inflation <- as.numeric(inflation$Inflation)
inflation$`Overnight Bank Lending Rate` <- as.numeric(inflation$`Overnight Bank Lending Rate`)
inflation$`1-Year RF Annual Spot Rate` <- as.numeric(inflation$`1-Year Risk Free Annual Spot Rate`)
inflation$`10-Year RF Annual Spot Rate` <- as.numeric(inflation$`10-Year Risk Free Annual Spot Rate`)

# Reshape to long format
infl_long <- inflation %>%
  select(Year, Inflation, `Overnight Bank Lending Rate`, `1-Year RF Annual Spot Rate`, `10-Year RF Annual Spot Rate`) %>%
  pivot_longer(-Year, names_to = "Metric", values_to = "Value")

# Convert to tsibble
infl_ts <- infl_long %>%
  as_tsibble(key = Metric, index = Year)

# Fit ARIMA model for each series
infl_models <- infl_ts %>%
  model(ARIMA(Value))

# Forecast 10 years ahead
infl_forecasts <- infl_models %>%
  forecast(h = 10)

# Extract forecast data as a data.frame for plotting
forecast_df <- infl_forecasts %>%
  as_tibble() %>%
  select(Year, Metric, .mean)

# Plot
ggplot(forecast_df, aes(x = Year, y = .mean, color = Metric)) +
  geom_line(linewidth = 1) +
  labs(
    title = "10-Year Forecast: Inflation and Interest Rates",
    x = "Year",
    y = "Rate / Inflation",
    color = "Metric"
  ) +
  theme_minimal() +
  theme(text = element_text(size = 12))