#######################################
# Baseline Aggregate Loss
#######################################

baseline_loss <- aggregate_loss

baseline_VaR99  <- quantile(baseline_loss, 0.99)
baseline_TVaR99 <- mean(baseline_loss[baseline_loss > baseline_VaR99])

baseline_mean <- mean(baseline_loss)
baseline_sd   <- sd(baseline_loss)

#######################################
# Frequency Stress Test
#######################################

freq_stress <- round(freq_sim * 1.3)

agg_loss_freq <- sapply(freq_stress, function(n){
  if(n == 0) return(0)
  sum(rgamma(n, shape = sev_shape, scale = sev_scale))
})

VaR_freq <- quantile(agg_loss_freq, 0.99)
TVaR_freq <- mean(agg_loss_freq[agg_loss_freq > VaR_freq])

#######################################
# Severity Stress Test
#######################################

sev_scale_stress <- sev_scale * 1.25

agg_loss_sev <- sapply(freq_sim, function(n){
  if(n == 0) return(0)
  sum(rgamma(n, shape = sev_shape, scale = sev_scale_stress))
})

VaR_sev <- quantile(agg_loss_sev, 0.99)
TVaR_sev <- mean(agg_loss_sev[agg_loss_sev > VaR_sev])

#######################################
# Correlated Stress Scenario
#######################################

rho <- 0.2

Sigma <- matrix(c(1, rho, rho, 1), 2, 2)

library(MASS)

Z <- mvrnorm(n_sim, mu = c(0,0), Sigma = Sigma)

freq_corr <- round(freq_sim * exp(0.2 * Z[,1]))
sev_corr  <- sev_scale * exp(0.15 * Z[,2])

agg_loss_corr <- sapply(1:n_sim, function(i){
  if(freq_corr[i] == 0) return(0)
  sum(rgamma(freq_corr[i], shape = sev_shape, scale = sev_corr[i]))
})

VaR_corr <- quantile(agg_loss_corr, 0.99)
TVaR_corr <- mean(agg_loss_corr[agg_loss_corr > VaR_corr])

#######################################
# Inflation Stress
#######################################

inflation_rate <- 0.08

sev_scale_infl <- sev_scale * (1 + inflation_rate)

agg_loss_infl <- sapply(freq_sim, function(n){
  if(n == 0) return(0)
  sum(rgamma(n, shape = sev_shape, scale = sev_scale_infl))
})

VaR_infl <- quantile(agg_loss_infl, 0.99)

#######################################
# Discount Rate Stress
#######################################

discount_rate <- 0.03

pv_losses <- baseline_loss / (1 + discount_rate)

mean_pv <- mean(pv_losses)

#######################################
# Workforce Stress Scenario
#######################################

stress_factor <- 1.2

freq_workforce <- round(freq_sim * stress_factor)

agg_loss_workforce <- sapply(freq_workforce, function(n){
  if(n == 0) return(0)
  sum(rgamma(n, shape = sev_shape, scale = sev_scale))
})

#######################################
# Capital Requirement
#######################################

capital_required <- quantile(aggregate_loss, 0.995)

expected_loss <- mean(aggregate_loss)

risk_margin <- capital_required - expected_loss

#######################################
# Sensitivity Analysis
#######################################

stress_levels <- seq(1.0, 1.5, by = 0.1)

VaR_sensitivity <- sapply(stress_levels, function(f){
  
  freq_temp <- round(freq_sim * f)
  
  loss_temp <- sapply(freq_temp, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  
  quantile(loss_temp, 0.99)
})

plot(stress_levels, VaR_sensitivity, type="b",
     xlab="Frequency Stress Multiplier",
     ylab="VaR 99%",
     main="Sensitivity of VaR to Frequency Stress")