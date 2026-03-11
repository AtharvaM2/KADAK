# 10-year financial sustainability model for WC portfolio
# Inputs: current premium, expected loss, financial.xlsx, inflation/risk-free forecasts

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forecast)
})

# =============================
# User inputs (override here)
# =============================
current_technical_premium <- 24853481.54
expected_annual_loss <- 14912088.92

# Simulated loss stats (from pricing model)
loss_stats <- list(
  mean_loss = 14922374.87,
  sd_loss = 1240718.07,
  var_95 = 17010761.46,
  var_99 = 17943892.88,
  tvar_99 = 18432316.88
)

financial_path <- "financial.xlsx"
inflation_source_path <- "inflation_forecast.R"
interest_inflation_path <- "srcsc-2026-interest-and-inflation.xlsx"

# =============================
# Load financials (3 years)
# =============================
financial_raw <- read_excel(financial_path, sheet = 1)
colnames(financial_raw)[1] <- "line_item"

financial_long <- financial_raw %>%
  pivot_longer(-line_item, names_to = "year", values_to = "value") %>%
  mutate(
    year = as.integer(year),
    line_item = str_squish(line_item)
  )

# Normalize line labels
normalize_line <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("\u00a0", " ") %>%
    str_squish()
}

financial_long <- financial_long %>%
  mutate(line_norm = normalize_line(line_item))

get_line <- function(pattern) {
  financial_long %>%
    filter(str_detect(line_norm, pattern)) %>%
    dplyr::select(year, value)
}

net_revenue <- get_line("net revenue")
oper_exp <- get_line("operating expenses")
oper_income <- get_line("operating income")
other_income <- get_line("other income")
pre_tax <- get_line("income before tax")
income_tax <- get_line("income tax")
net_income <- get_line("net income")

fin_join <- net_revenue %>%
  rename(net_revenue = value) %>%
  left_join(oper_exp %>% rename(oper_expenses = value), by = "year") %>%
  left_join(oper_income %>% rename(oper_income = value), by = "year") %>%
  left_join(other_income %>% rename(other_income = value), by = "year") %>%
  left_join(pre_tax %>% rename(pre_tax = value), by = "year") %>%
  left_join(income_tax %>% rename(income_tax = value), by = "year") %>%
  left_join(net_income %>% rename(net_income = value), by = "year")

# Ratios (use last 3 years average)
#expense_ratio <- mean(abs(fin_join$oper_expenses) / fin_join$net_revenue, na.rm = TRUE)
expense_ratio <- 0.3
profit_margin <- mean(fin_join$net_income / fin_join$net_revenue, na.rm = TRUE)
oper_income_margin <- mean(fin_join$oper_income / fin_join$net_revenue, na.rm = TRUE)
other_income_margin <- mean(fin_join$other_income / fin_join$net_revenue, na.rm = TRUE)

# =============================
# Load inflation and risk-free forecasts
# =============================
# Expect inflation_forecast.R to define inflation_forecast and risk_free_forecast.
# If not, compute forecasts from srcsc-2026-interest-and-inflation.xlsx

inflation_fc <- NULL
risk_free_fc <- NULL

if (file.exists(inflation_source_path)) {
  try(source(inflation_source_path), silent = TRUE)
}

extract_forecast <- function(obj) {
  if (is.null(obj)) return(NULL)
  if (inherits(obj, "fable")) {
    df <- as.data.frame(obj)
    if ("mean" %in% names(df)) return(df$mean)
    if (".mean" %in% names(df)) return(df$.mean)
  }
  if (is.numeric(obj)) return(obj)
  if (is.data.frame(obj) && "mean" %in% names(obj)) return(obj$mean)
  NULL
}

if (exists("inflation_forecast")) inflation_fc <- extract_forecast(inflation_forecast)
if (exists("risk_free_forecast")) risk_free_fc <- extract_forecast(risk_free_forecast)

if (is.null(inflation_fc) || length(inflation_fc) < 10 || is.null(risk_free_fc) || length(risk_free_fc) < 10) {
  # Fallback: forecast from interest/inflation workbook
  infl_raw <- read_excel(interest_inflation_path, skip = 2)
  infl_raw <- infl_raw %>%
    mutate(
      Year = as.integer(Year),
      Inflation = as.numeric(Inflation),
      rf_10y = as.numeric(`10-Year Risk Free Annual Spot Rate`)
    ) %>%
    filter(!is.na(Inflation), !is.na(rf_10y))

  infl_ts <- ts(infl_raw$Inflation, frequency = 1)
  rf_ts <- ts(infl_raw$rf_10y, frequency = 1)

  infl_fit <- auto.arima(infl_ts)
  rf_fit <- auto.arima(rf_ts)

  inflation_fc <- as.numeric(forecast(infl_fit, h = 10)$mean)
  risk_free_fc <- as.numeric(forecast(rf_fit, h = 10)$mean)
}

inflation_fc <- inflation_fc[1:10]
risk_free_fc <- risk_free_fc[1:10]

# =============================
# 10-year projections
# =============================
last_year <- max(fin_join$year, na.rm = TRUE)
years <- (last_year + 1):(last_year + 10)

inflation_fc <- pmax(inflation_fc, -0.02)
risk_free_fc <- pmax(risk_free_fc, 0)

inflation_factor <- cumprod(1 + inflation_fc)
discount_factor <- 1 / cumprod(1 + risk_free_fc)

premium_forecast <- current_technical_premium * inflation_factor
loss_forecast <- expected_annual_loss * inflation_factor

# Financial statement projections using ratios
expense_forecast <- premium_forecast * expense_ratio
oper_income_forecast <- premium_forecast * oper_income_margin
net_income_forecast <- premium_forecast * profit_margin

# PVs
pv_premium <- premium_forecast * discount_factor
pv_loss <- loss_forecast * discount_factor
pv_expense <- expense_forecast * discount_factor
pv_net_income <- net_income_forecast * discount_factor

projection_table <- tibble(
  year = years,
  inflation = inflation_fc,
  risk_free_rate = risk_free_fc,
  premium_forecast = premium_forecast,
  loss_forecast = loss_forecast,
  operating_expenses = expense_forecast,
  operating_income = oper_income_forecast,
  net_income = net_income_forecast,
  discount_factor = discount_factor,
  pv_premium = pv_premium,
  pv_loss = pv_loss,
  pv_expense = pv_expense,
  pv_net_income = pv_net_income
)

# =============================
# Reserve requirements
# =============================
expected_plus_margin <- expected_annual_loss + (loss_stats$tvar_99 - loss_stats$mean_loss)
reserve_candidates <- tibble(
  reserve_basis = c("Expected loss + TVaR margin", "VaR(99%)", "TVaR(99%)"),
  reserve_level = c(expected_plus_margin, loss_stats$var_99, loss_stats$tvar_99)
)

recommended_reserve <- reserve_candidates %>%
  arrange(desc(reserve_level)) %>%
  slice(1)

# =============================
# Sustainability assessment
# =============================
summary_totals <- tibble(
  metric = c("PV premiums", "PV losses", "PV expenses", "PV net income"),
  value = c(sum(pv_premium), sum(pv_loss), sum(pv_expense), sum(pv_net_income))
)

pv_profit = sum(pv_premium) - sum(pv_loss) - sum(pv_expense)
solvent_with_reserve <- pv_profit > recommended_reserve$reserve_level

sustainability_summary <- tibble(
  pv_profit = pv_profit,
  recommended_reserve = recommended_reserve$reserve_level,
  solvent_with_reserve = solvent_with_reserve
)

# =============================
# Output
# =============================
dir.create("WC_outputs", showWarnings = FALSE)
write.csv(projection_table, "WC_outputs/financial_projection_10yr.csv", row.names = FALSE)
write.csv(summary_totals, "WC_outputs/financial_projection_totals.csv", row.names = FALSE)
write.csv(reserve_candidates, "WC_outputs/reserve_candidates.csv", row.names = FALSE)
write.csv(sustainability_summary, "WC_outputs/sustainability_summary.csv", row.names = FALSE)

actuarial_summary <- paste(
  "Summary:",
  paste0("- PV premiums: ", round(sum(pv_premium), 2)),
  paste0("- PV losses: ", round(sum(pv_loss), 2)),
  paste0("- PV expenses: ", round(sum(pv_expense), 2)),
  paste0("- PV profit: ", round(pv_profit, 2)),
  paste0("- Recommended reserve: ", round(recommended_reserve$reserve_level, 2),
         " (", recommended_reserve$reserve_basis, ")"),
  paste0("- Solvent with reserve: ", ifelse(solvent_with_reserve, "YES", "NO")),
  sep = "\n"
)

writeLines(actuarial_summary, "WC_outputs/actuarial_summary.txt")

cat("\n10-year financial projection complete. WC_outputs:\n")
cat("- WC_outputs/financial_projection_10yr.csv\n")
cat("- WC_outputs/financial_projection_totals.csv\n")
cat("- WC_outputs/reserve_candidates.csv\n")
cat("- WC_outputs/sustainability_summary.csv\n")
cat("- WC_outputs/actuarial_summary.txt\n")
