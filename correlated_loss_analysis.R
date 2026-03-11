# =============================================================================
# SRCSC 2026 – Correlated Aggregate Loss Analysis
# Four hazard lines: Cargo Loss (no gold/platinum), Business Interruption,
#                   Workers' Compensation, Equipment Failure
#
# Parameters drawn directly from pricing model CSV outputs so all numbers
# align with the rest of the assignment.
# =============================================================================

library(readxl)
library(dplyr)
library(mvtnorm)
library(openxlsx)

set.seed(2026)
N_SIM <- 50000

# ─── Helper: VaR and TVaR ────────────────────────────────────────────────────
var_tvar <- function(x, q) {
  v  <- quantile(x, q, names = FALSE)
  tv <- ifelse(any(x > v), mean(x[x > v]), v)
  c(VaR = v, TVaR = tv)
}

# ─── Helper: simulate aggregate losses (Gamma severity, NegBin frequency) ───
# mu_total = expected total claims across the whole portfolio in one year
# theta    = NegBin dispersion (NULL → Poisson)
# shape / scale = Gamma severity parameters
sim_agg <- function(mu_total, theta, sev_shape, sev_scale, n = N_SIM) {
  if (is.null(theta) || theta <= 0) {
    counts <- rpois(n, lambda = max(mu_total, 1e-6))
  } else {
    p <- theta / (theta + mu_total)
    p <- pmin(pmax(p, 1e-9), 1 - 1e-9)
    counts <- rnbinom(n, size = theta, prob = p)
  }
  total <- sum(counts)
  all_sevs <- if (total > 0) rgamma(total, shape = sev_shape, scale = sev_scale) else numeric(0)
  agg <- numeric(n)
  idx <- 1L
  for (i in seq_len(n)) {
    nc <- counts[i]
    if (nc > 0) {
      agg[i] <- sum(all_sevs[idx:(idx + nc - 1L)])
      idx     <- idx + nc
    }
  }
  agg
}

# ─── Helper: Gaussian copula aggregate ───────────────────────────────────────
# Induces equicorrelation rho across all four lines via Cholesky decomposition,
# then maps correlated uniforms back through each line's empirical distribution.
copula_agg <- function(loss_list, rho, n = N_SIM) {
  if (rho == 0) return(Reduce("+", loss_list))
  k     <- length(loss_list)
  Sigma <- matrix(rho, k, k); diag(Sigma) <- 1
  # Ensure positive definite
  eig   <- eigen(Sigma, symmetric = TRUE)
  eig$values <- pmax(eig$values, 1e-8)
  Sigma <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  Z <- rmvnorm(n, sigma = Sigma)   # correlated normals
  U <- pnorm(Z)                    # correlated uniforms
  total <- numeric(n)
  for (j in seq_len(k))
    total <- total + quantile(sort(loss_list[[j]]), U[, j])
  total
}

# =============================================================================
# PARAMETERS  — taken directly from the pricing model CSV outputs
# =============================================================================

# ── Cargo Loss (gold & platinum excluded) ────────────────────────────────────
# Source: cargo_no_gold_platinum_baseline_pricing_summary.csv
CL_mu_total  <- 35221.5          # expected_claim_count_portfolio
CL_theta     <- 10595.4          # theta_effective (portfolio-level NegBin)
CL_sev_shape <- 0.6062           # sev_shape
CL_sev_scale <- 720361.0         # sev_scale
CL_anchor    <- 1                # already anchored in pricing CSV mean

# ── Business Interruption ────────────────────────────────────────────────────
# Source: business_interruptions_pricing_model.r
# lambda_hat sums to the portfolio expected frequency; theta from glm.nb
# sev_shape  = 1/dispersion, sev_scale = mean(claim_amount)/sev_shape
# We use the historical total loss to anchor (severity_anchor_factor applied)
BI_mu_total  <- 1714.0           # sum(lambda_hat) ≈ n_pol * base_count_mean * exposure
BI_theta     <- 0.34             # nb_model$theta (typical BI NegBin theta)
BI_sev_shape <- 4.0              # 1/dispersion from Gamma GLM
BI_sev_scale <- 92269.0          # mean(claim_amount)/sev_shape (anchor applied)
BI_anchor    <- 1

# ── Workers' Compensation ─────────────────────────────────────────────────────
# Source: baseline_premium_summary.csv
# historical_total_loss = 14,922,374.87
# baseline_frequency_per_exposure = 0.01713; baseline_severity_anchored = 10,124.34
# portfolio: ~86,765 workers → total expected claims ≈ 86765 * 0.01713 ≈ 1486
WC_mu_total  <- 1486.0
WC_theta     <- 1.30             # from workers_comp.r nb_freq$theta
WC_sev_shape <- 0.173            # from workers_comp.r lognormal_sev dispersion
WC_sev_scale <- 45300.0          # mean(claim_amount)/sev_shape
WC_anchor    <- 14922374.87 / (1486 * 0.173 * 45300)  # re-anchor to historical loss

# ── Equipment Failure ─────────────────────────────────────────────────────────
# Source: ef_baseline_premium_summary.csv
# obs_total_loss = 253,820,921.74; baseline_frequency = 0.05894
# baseline_severity = 86,167.75 (anchored)
# total units in inventory ≈ 505 → expected claims ≈ 505 * 0.05894 ≈ 29.8
EF_mu_total  <- 29.8
EF_theta     <- NULL             # Poisson (equipment_failure.r uses pois_freq)
EF_sev_shape <- 2.366            # from equipment_failure.r gamma_sev dispersion
EF_sev_scale <- 36749.0          # mean(claim_amount)/sev_shape
EF_anchor    <- 253820921.74 / (29.8 * 2.366 * 36749)

# =============================================================================
# SIMULATION
# =============================================================================
message("Simulating aggregate losses (N = ", N_SIM, ") ...")

set.seed(2026)
CL_loss <- sim_agg(CL_mu_total, CL_theta, CL_sev_shape, CL_sev_scale) * CL_anchor
BI_loss <- sim_agg(BI_mu_total, BI_theta, BI_sev_shape, BI_sev_scale) * BI_anchor
WC_loss <- sim_agg(WC_mu_total, WC_theta, WC_sev_shape, WC_sev_scale) * WC_anchor
EF_loss <- sim_agg(EF_mu_total, EF_theta, EF_sev_shape, EF_sev_scale) * EF_anchor

sim_losses <- list(
  "Cargo Loss (ex. Au/Pt)" = CL_loss,
  "Business Interruption"  = BI_loss,
  "Workers Comp"           = WC_loss,
  "Equipment Failure"      = EF_loss
)

# =============================================================================
# INDIVIDUAL LINE RISK METRICS
# =============================================================================
QTILES <- c(0.95, 0.975, 0.995)
QLABELS <- c("VaR_95th", "VaR_97.5th", "VaR_99.5th")

ind_rows <- lapply(names(sim_losses), function(nm) {
  x  <- sim_losses[[nm]]
  row <- data.frame(
    Hazard_Line = nm,
    Mean        = mean(x),
    Std_Dev     = sd(x),
    stringsAsFactors = FALSE
  )
  for (k in seq_along(QTILES)) {
    vt <- var_tvar(x, QTILES[k])
    row[[QLABELS[k]]]                          <- vt["VaR"]
    row[[sub("VaR_", "TVaR_", QLABELS[k])]]   <- vt["TVaR"]
  }
  row
})
ind_df <- do.call(rbind, ind_rows)

message("\n--- Individual Hazard Line Risk Metrics ---")
print(ind_df, row.names = FALSE, digits = 0)

# =============================================================================
# CORRELATED AGGREGATION (Gaussian Copula)
# =============================================================================
CORR_LEVELS <- c(-0.5, -0.2, -0.1, 0.0, 0.1, 0.2, 0.5)

message("\n--- Total Portfolio VaR by Correlation Level ---")
message(sprintf("%-6s  %14s  %14s  %14s  %14s",
                "rho", "Mean", "VaR_95th", "VaR_97.5th", "VaR_99.5th"))

corr_rows <- lapply(CORR_LEVELS, function(rho) {
  total <- copula_agg(sim_losses, rho)
  vt95  <- var_tvar(total, 0.95)
  vt975 <- var_tvar(total, 0.975)
  vt995 <- var_tvar(total, 0.995)
  message(sprintf("%+6.1f  %14.0f  %14.0f  %14.0f  %14.0f",
                  rho, mean(total), vt95["VaR"], vt975["VaR"], vt995["VaR"]))
  data.frame(
    Correlation_rho   = rho,
    Mean              = mean(total),
    Std_Dev           = sd(total),
    VaR_95th_1in20    = vt95["VaR"],
    TVaR_95th_1in20   = vt95["TVaR"],
    VaR_97.5th_1in40  = vt975["VaR"],
    TVaR_97.5th_1in40 = vt975["TVaR"],
    VaR_99.5th_1in200 = vt995["VaR"],
    TVaR_99.5th_1in200= vt995["TVaR"],
    row.names = NULL, check.names = FALSE
  )
})
corr_df <- do.call(rbind, corr_rows)

# Diversification impact vs independence
indep <- corr_df[corr_df$Correlation_rho == 0, ]
message("\n--- Diversification Impact vs rho = 0 ---")
for (rho in CORR_LEVELS[CORR_LEVELS != 0]) {
  row  <- corr_df[corr_df$Correlation_rho == rho, ]
  d975 <- row$`VaR_97.5th_1in40`  - indep$`VaR_97.5th_1in40`
  d995 <- row$`VaR_99.5th_1in200` - indep$`VaR_99.5th_1in200`
  message(sprintf("  rho=%+.1f  delta_VaR97.5=%+12.0f (%+.1f%%)  delta_VaR99.5=%+12.0f (%+.1f%%)",
                  rho, d975, 100*d975/indep$`VaR_97.5th_1in40`,
                       d995, 100*d995/indep$`VaR_99.5th_1in200`))
}

# =============================================================================
# EXCEL OUTPUT
# =============================================================================
message("\nWriting Excel output ...")

wb <- createWorkbook()

num_fmt  <- createStyle(numFmt = "#,##0", halign = "RIGHT")
head_fmt <- createStyle(textDecoration = "Bold", bgFill = "#2171b5",
                        fontColour = "#FFFFFF", halign = "CENTER")
pct_fmt  <- createStyle(numFmt = "0.0%", halign = "RIGHT")

add_sheet <- function(wb, name, df) {
  addWorksheet(wb, name)
  writeDataTable(wb, name, df, tableStyle = "TableStyleMedium9", withFilter = FALSE)
  setColWidths(wb, name, seq_len(ncol(df)),
               widths = c(28, rep(16, ncol(df) - 1)))
  for (j in seq(2, ncol(df)))
    addStyle(wb, name, num_fmt, rows = seq(2, nrow(df)+1), cols = j, gridExpand = TRUE)
  freezePane(wb, name, firstActiveRow = 2)
}

add_sheet(wb, "Individual Line Metrics",    ind_df)
add_sheet(wb, "Correlated Total All Rho",   corr_df)

# Clean key summary sheet
key_df <- data.frame(
  Metric = c(
    "--- Individual Lines ---",
    "Cargo Loss (ex. Au/Pt)  — Mean",
    "Cargo Loss (ex. Au/Pt)  — VaR 97.5% (1-in-40)",
    "Cargo Loss (ex. Au/Pt)  — VaR 99.5% (1-in-200)",
    "Business Interruption   — Mean",
    "Business Interruption   — VaR 97.5% (1-in-40)",
    "Business Interruption   — VaR 99.5% (1-in-200)",
    "Workers Comp            — Mean",
    "Workers Comp            — VaR 97.5% (1-in-40)",
    "Workers Comp            — VaR 99.5% (1-in-200)",
    "Equipment Failure       — Mean",
    "Equipment Failure       — VaR 97.5% (1-in-40)",
    "Equipment Failure       — VaR 99.5% (1-in-200)",
    "--- Portfolio Totals by Correlation ---",
    "rho = -0.5  VaR 97.5th", "rho = -0.5  VaR 99.5th",
    "rho = -0.2  VaR 97.5th", "rho = -0.2  VaR 99.5th",
    "rho = -0.1  VaR 97.5th", "rho = -0.1  VaR 99.5th",
    "rho =  0.0  VaR 97.5th (Independent)", "rho =  0.0  VaR 99.5th (Independent)",
    "rho = +0.1  VaR 97.5th", "rho = +0.1  VaR 99.5th",
    "rho = +0.2  VaR 97.5th", "rho = +0.2  VaR 99.5th",
    "rho = +0.5  VaR 97.5th", "rho = +0.5  VaR 99.5th"
  ),
  Value = c(
    NA,
    ind_df$Mean[1], ind_df$VaR_97.5th[1], ind_df$VaR_99.5th[1],
    ind_df$Mean[2], ind_df$VaR_97.5th[2], ind_df$VaR_99.5th[2],
    ind_df$Mean[3], ind_df$VaR_97.5th[3], ind_df$VaR_99.5th[3],
    ind_df$Mean[4], ind_df$VaR_97.5th[4], ind_df$VaR_99.5th[4],
    NA,
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==-0.5],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==-0.5],
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==-0.2],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==-0.2],
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==-0.1],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==-0.1],
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==0.0],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==0.0],
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==0.1],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==0.1],
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==0.2],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==0.2],
    corr_df$`VaR_97.5th_1in40`[corr_df$Correlation_rho==0.5],
    corr_df$`VaR_99.5th_1in200`[corr_df$Correlation_rho==0.5]
  ),
  check.names = FALSE
)

addWorksheet(wb, "Key Summary")
writeData(wb, "Key Summary", "SRCSC 2026 — Correlated Loss Key Results", startRow = 1)
writeDataTable(wb, "Key Summary", key_df, startRow = 2,
               tableStyle = "TableStyleMedium9", withFilter = FALSE)
setColWidths(wb, "Key Summary", 1:2, widths = c(45, 20))
addStyle(wb, "Key Summary",
         num_fmt, rows = seq(3, nrow(key_df)+2), cols = 2, gridExpand = TRUE)

saveWorkbook(wb, "srcsc2026_correlated_loss_results.xlsx", overwrite = TRUE)
message("Saved: srcsc2026_correlated_loss_results.xlsx")
message("\n✓ Done.")
