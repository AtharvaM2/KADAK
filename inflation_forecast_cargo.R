library(readxl)
library(dplyr)

forecast_series_hw <- function(x, horizon) {
  x <- as.numeric(stats::na.omit(x))
  if (length(x) < 3) {
    return(rep(tail(x, 1), horizon))
  }

  fit <- stats::HoltWinters(x, gamma = FALSE)
  as.numeric(stats::predict(fit, n.ahead = horizon))
}

forecast_inflation_and_rates <- function(
    path = "srcsc-2026-interest-and-inflation.xlsx",
    horizon = 10
) {
  econ <- read_excel(path, skip = 2)

  required_cols <- c(
    "Year",
    "Inflation",
    "1-Year Risk Free Annual Spot Rate"
  )

  missing_cols <- setdiff(required_cols, names(econ))
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing required columns in interest/inflation file:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  econ <- econ %>%
    filter(!is.na(Year)) %>%
    mutate(
      Year = as.numeric(Year),
      Inflation = as.numeric(Inflation),
      `1-Year Risk Free Annual Spot Rate` =
        as.numeric(`1-Year Risk Free Annual Spot Rate`)
    )

  max_year <- max(econ$Year, na.rm = TRUE)

  infl_fc <- forecast_series_hw(econ$Inflation, horizon)
  rf_fc <- forecast_series_hw(econ$`1-Year Risk Free Annual Spot Rate`, horizon)

  tibble(
    year = (max_year + 1):(max_year + horizon),
    inflation = pmax(infl_fc, -0.03),
    rf_1y = pmax(rf_fc, 0)
  )
}
