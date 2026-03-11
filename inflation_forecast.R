library(forecast)
library(tsibble)
library(dplyr)
library(fable)
library(feasts)
inflation <- read_excel("srcsc-2026-interest-and-inflation.xlsx")
colnames(inflation) <- as.character(inflation[2, ])
inflation <- inflation[- c(1,2), ]

inflation

inflation$Year <- as.numeric(inflation$Year)
inflation$Inflation <- as.numeric(inflation$Inflation)

#time-series 
inflation_ts <- inflation %>%
  as_tsibble(index = Year)

inflation_ts

inflation_model <- inflation_ts %>%
  model(ARIMA(Inflation))

report(inflation_model)

#10-year forecast
inflation_forecast <- inflation_model %>%
  forecast(h = 10)
inflation_forecast