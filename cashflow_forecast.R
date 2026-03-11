# 10-year financial sustainability model for multiple lines
# Core logic unchanged; refactored into a reusable function.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(forecast)
})

# =============================
# Shared inputs
# =============================
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
# NOTE: Kept as-is from the current script.
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
# Reference risk metrics (documented)
# =============================
# =============================
# Reference risk metrics (updated for all five lines)
# =============================
reference_metrics <- list(
  workers_comp = list(
    mean_loss = 14922374.87,
    tail_gap_99 = 18432316.88 - 14922374.87,  # TVaR margin
    current_risk_margin_amount = 18432316.88,
    suggested_risk_ratio = 0.05
  ),
  business_interruption = list(
    historical_total_loss = 2650772899,
    tail_gap_99 = 108621033,
    current_risk_margin_amount = 132538645,
    suggested_risk_ratio = 0.05
  ),
  cargo_loss_no_gold_platinum = list(
    mean_loss = 15382229485.9018,
    tail_gap_99 = 15920024318.8906 - 15382229485.9018,
    current_risk_margin_amount = 15920024318.8906,
    suggested_risk_ratio = 0.05
  ),
  cargo_loss = list(
    mean_loss = 413430638181.031,
    tail_gap_99 = 425463256005.725 - 413430638181.031,
    current_risk_margin_amount = 425463256005.725,
    suggested_risk_ratio = 0.05
  ),
  equipment_failure = list(
    obs_total_loss = 253820921.7,
    tail_gap_99 = 277122741.6 - 253820921.7,
    current_risk_margin_amount = 277122741.6,
    suggested_risk_ratio = 0.092302224
  )
)

# =============================
# Projection function
# =============================
run_cashflow_projection <- function(line_name, current_technical_premium, expected_annual_loss, loss_stats) {
  # 10-year projections
  last_year <- max(fin_join$year, na.rm = TRUE)
  years <- (last_year + 1):(last_year + 10)

  infl <- pmax(inflation_fc, -0.02)
  rf <- pmax(risk_free_fc, 0)

  inflation_factor <- cumprod(1 + infl)
  discount_factor <- 1 / cumprod(1 + rf)

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
    inflation = infl,
    risk_free_rate = rf,
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

  # Reserve requirements
  expected_plus_margin <- expected_annual_loss + (loss_stats$tvar_99 - loss_stats$mean_loss)
  reserve_candidates <- tibble(
    reserve_basis = c("Expected loss + TVaR margin", "VaR(99%)", "TVaR(99%)"),
    reserve_level = c(expected_plus_margin, loss_stats$var_99, loss_stats$tvar_99)
  )

  recommended_reserve <- reserve_candidates %>%
    arrange(desc(reserve_level)) %>%
    slice(1)

  # Sustainability assessment
  summary_totals <- tibble(
    metric = c("PV premiums", "PV losses", "PV expenses", "PV net income"),
    value = c(sum(pv_premium), sum(pv_loss), sum(pv_expense), sum(pv_net_income))
  )

  pv_profit <- sum(pv_premium) - sum(pv_loss) - sum(pv_expense)
  solvent_with_reserve <- pv_profit > recommended_reserve$reserve_level

  sustainability_summary <- tibble(
    pv_profit = pv_profit,
    recommended_reserve = recommended_reserve$reserve_level,
    solvent_with_reserve = solvent_with_reserve
  )

  # Output
  output_dir <- file.path("outputs", line_name)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  write.csv(projection_table, file.path(output_dir, "financial_projection_10yr.csv"), row.names = FALSE)
  write.csv(summary_totals, file.path(output_dir, "financial_projection_totals.csv"), row.names = FALSE)
  write.csv(reserve_candidates, file.path(output_dir, "reserve_candidates.csv"), row.names = FALSE)
  write.csv(sustainability_summary, file.path(output_dir, "sustainability_summary.csv"), row.names = FALSE)

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

  if (line_name %in% names(reference_metrics)) {
    ref <- reference_metrics[[line_name]]
    ref_lines <- paste0("- ", names(ref), ": ", unlist(ref))
    actuarial_summary <- paste(actuarial_summary, "Reference metrics:", paste(ref_lines, collapse = "\n"), sep = "\n")
  }

  writeLines(actuarial_summary, file.path(output_dir, "actuarial_summary.txt"))

  cat("\n10-year financial projection complete for", line_name, "->", output_dir, "\n")
}

# =============================
# Line inputs and runs
# =============================
wc_loss_stats <- list(
  mean_loss = 14922374.87,
  sd_loss = 1240718.07,
  var_95 = 17010761.46,
  var_99 = 17943892.88,
  tvar_99 = 18432316.88
)

bi_loss_stats <- list(
  mean_loss = 2652217518,
  sd_loss = 41057286.04,
  var_95 = 2720695646,
  var_99 = 2747419062,
  tvar_99 = 2760838551
)

cargo_loss_no_gold_platinum_stats <- list(
  mean_loss = 15382229485.9018,
  sd_loss = 200424933.638281,
  var_95 = 15713036725.7887,
  var_99 = 15857120598.3118,
  tvar_99 = 15920024318.8906
)

cargo_loss_stats <- list(
  mean_loss = 413430638181.031,
  sd_loss = 4473208157.56146,
  var_95 = 420831399130.527,
  var_99 = 423931983729.25,
  tvar_99 = 425463256005.725
)

equipment_loss_stats <- list(
  mean_loss = 253820921.7,
  sd_loss = 8562815.498,
  var_95 = 268060092.8,
  var_99 = 274108775.7,
  tvar_99 = 277122741.6
)

run_cashflow_projection(
  "workers_comp",
  current_technical_premium <- 24853481.54,
  expected_annual_loss <- 14912088.92,
  loss_stats = wc_loss_stats
)

run_cashflow_projection(
  "business_interruption",
  current_technical_premium = 4417954832,
  expected_annual_loss = 2652217518,
  loss_stats = bi_loss_stats
)

run_cashflow_projection(
  "cargo_loss_no_gold_platinum",
  current_technical_premium = 25637049143.1697,
  expected_annual_loss = 15382229485.9018,
  loss_stats = cargo_loss_no_gold_platinum_stats
)

run_cashflow_projection(
  "cargo_loss",
  current_technical_premium = 689051063635.052,
  expected_annual_loss = 413430638181.031,
  loss_stats = cargo_loss_stats
)


run_cashflow_projection(
  "equipment_failure",
  current_technical_premium = 420752228.3,
  expected_annual_loss = 253820921.7,
  loss_stats = equipment_loss_stats
)

library(dplyr)
library(knitr)

# Base data with updated Tail Gap values
data <- data.frame(
  LineOfBusiness = c("Business Interruptions", "Cargo Loss (no gold/platinum)", 
                     "Equipment Failure", "Workers Comp"),
  PV_Premiums = c(41009976588.54, 237977712570.86, 3905662164.58, 230704191.16),
  PV_Losses = c(24619395728.79, 142786627542.52, 2356110565.28, 138422514.66),
  PV_Expenses = c(12302992976.56, 71393313771.26, 1171698649.37, 69211257.35),
  PV_Profit = c(4087587883.2, 23797771257.09, 377852949.93, 23070419.15),
  Recommended_Reserve = c(2760838551, 15920024318.89, 277122741.6, 18432316.88),
  Tail_Gap_99 = c(108621033, 537794832.9888, 23301819.9, 3509942.01)
)

# Convert Tail_Gap_99 to character so total row can use "N/A"
data <- data %>%
  mutate(
    Tail_Gap_99 = as.character(round(Tail_Gap_99, 0)),
    Profit_Margin = round(PV_Profit / PV_Premiums, 4),
    Reserve_Proportion = round(Recommended_Reserve / sum(Recommended_Reserve), 4),
    PV_Premiums = round(PV_Premiums, 0),
    PV_Losses = round(PV_Losses, 0),
    PV_Expenses = round(PV_Expenses, 0),
    PV_Profit = round(PV_Profit, 0),
    Recommended_Reserve = round(Recommended_Reserve, 0)
  )

# Total row
total_row <- data.frame(
  LineOfBusiness = "TOTAL",
  PV_Premiums = sum(as.numeric(data$PV_Premiums)),
  PV_Losses = sum(as.numeric(data$PV_Losses)),
  PV_Expenses = sum(as.numeric(data$PV_Expenses)),
  PV_Profit = sum(as.numeric(data$PV_Profit)),
  Recommended_Reserve = sum(as.numeric(data$Recommended_Reserve)),
  Tail_Gap_99 = "N/A",
  Profit_Margin = round(sum(as.numeric(data$PV_Profit)) / sum(as.numeric(data$PV_Premiums)), 4),
  Reserve_Proportion = 1
)

# Combine
final_table <- bind_rows(data, total_row)

# Display
kable(final_table, caption = "Enhanced Summary of Metrics by Line of Business with Tail Gap")