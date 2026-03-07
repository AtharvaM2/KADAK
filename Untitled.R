#######################################
# Stress Levels
#######################################
#frequency & severity 
freq_stress_levels <- c(1.25, 1.5, 1.75, 2.0)   
sev_stress_levels  <- c(1.25, 1.5, 1.75, 2.0)        
length_stress_levels <- c(1.25, 1.5, 1.75, 2.0)   

#operational risk 
exposure_levels <- c(1.25, 1.5, 1.75, 2.0)
hours_levels <- c(1.5, 2.0, 2.5)         
supervision_levels <- c(1.4, 1.2, 1, 0.8, 0.6, 0.4)
experience_levels <- c(1.4, 1.2, 1, 0.8, 0.6, 0.4)

#safety and training 
training_stress_levels <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)
gravity_stress_levels <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)

#psychological impact
psych_stress_levels <- c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0)
correlation_levels <- c(0.1, 0.3, 0.5, 0.7, 0.9, 1)

#occupation 
occ_levels <- c(0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)
worker_levels <- c(0.6, 0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0)

#financial / economic
inflation_levels <- c(1, 1.05, 1.10, 1.15, 1.20, 1.30, 1.35, 1.4, 1.45, 1.5)
salary_levels <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5)  

#######################################
# Frequency Stress Test
#######################################

freq_results <- data.frame()

for(f in freq_stress_levels){
  
  freq_temp <- round(freq_sim * f)
  
  loss_temp <- sapply(freq_temp, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  
  freq_results <- rbind(freq_results,
                        data.frame(
                          stress_factor = f,
                          mean_loss = mean(loss_temp),
                          VaR_99 = quantile(loss_temp, 0.99),
                          TVaR_99 = mean(loss_temp[loss_temp > quantile(loss_temp,0.99)])
                        ))
}

freq_results

#######################################
# Severity Stress Test
#######################################

sev_results <- data.frame()

for(s in sev_stress_levels){
  
  sev_scale_stress <- sev_scale * s
  
  loss_temp <- sapply(freq_sim, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale_stress))
  })
  
  sev_results <- rbind(sev_results,
                       data.frame(
                         stress_factor = s,
                         mean_loss = mean(loss_temp),
                         VaR_99 = quantile(loss_temp, 0.99),
                         TVaR_99 = mean(loss_temp[loss_temp > quantile(loss_temp,0.99)])
                       ))
}

sev_results

#######################################
# Claim Length Stress Test
#######################################

claim_length <- na.omit(worker_data_sev$claim_length)

daily_wage <- mean(worker_data_sev$claim_amount) / mean(claim_length)

length_results <- data.frame()

for(l in length_stress_levels){
  
  length_stress <- claim_length * l
  
  severity_temp <- length_stress * daily_wage
  
  loss_temp <- sapply(freq_sim, function(n){
    if(n == 0) return(0)
    sum(sample(severity_temp, n, replace = TRUE))
  })
  
  VaR <- quantile(loss_temp, 0.99)
  
  length_results <- rbind(length_results,
                          data.frame(
                            stress_factor = l,
                            mean_loss = mean(loss_temp),
                            VaR_99 = VaR,
                            TVaR_99 = mean(loss_temp[loss_temp > VaR])
                          ))
}

length_results

#######################################
# Combine Results
#######################################

freq_results$test_type <- "Frequency"
sev_results$test_type  <- "Severity"
length_results$test_type <- "Claim Length"

stress_results <- rbind(freq_results, sev_results, length_results)

stress_results

#######################################
# Plot Results
#######################################

ggplot(stress_results,
       aes(x = stress_factor,
           y = VaR_99,
           color = test_type)) +
  geom_line() +
  geom_point() +
  labs(title = "Stress Testing Impact on VaR (99%)",
       x = "Stress Multiplier",
       y = "VaR 99%")

#######################################
# Exposure Stress Test
#######################################
exposure_results <- data.frame()

for(e in exposure_levels){
  
  freq_temp <- round(freq_sim * e)
  
  loss_temp <- sapply(freq_temp, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  
  VaR <- quantile(loss_temp, 0.99)
  
  exposure_results <- rbind(exposure_results,
                            data.frame(
                              stress_factor = e,
                              mean_loss = mean(loss_temp),
                              VaR_99 = VaR,
                              TVaR_99 = mean(loss_temp[loss_temp > VaR])
                            ))
}

exposure_results

#######################################
# Hours Worked Stress Test
#######################################

hours_results <- data.frame()

for(h in hours_levels){
  
  freq_temp <- round(freq_sim * h)
  
  loss_temp <- sapply(freq_temp, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  
  VaR <- quantile(loss_temp, 0.99)
  
  hours_results <- rbind(hours_results,
                         data.frame(
                           stress_factor = h,
                           mean_loss = mean(loss_temp),
                           VaR_99 = VaR,
                           TVaR_99 = mean(loss_temp[loss_temp > VaR])
                         ))
}

hours_results

#######################################
# Supervision Stress Test
#######################################

supervision_results <- data.frame()

for(s in supervision_levels){
  
  freq_temp <- round(freq_sim * (1 + (1 - s)))
  
  loss_temp <- sapply(freq_temp, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  
  VaR <- quantile(loss_temp, 0.99)
  
  supervision_results <- rbind(supervision_results,
                               data.frame(
                                 supervision_factor = s,
                                 mean_loss = mean(loss_temp),
                                 VaR_99 = VaR,
                                 TVaR_99 = mean(loss_temp[loss_temp > VaR])
                               ))
}

supervision_results

#######################################
# Experience Stress Test
#######################################

experience_results <- data.frame()

for(x in experience_levels){
  
  freq_temp <- round(freq_sim * (1 + (1 - x)))
  
  loss_temp <- sapply(freq_temp, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale))
  })
  
  VaR <- quantile(loss_temp, 0.99)
  
  experience_results <- rbind(experience_results,
                              data.frame(
                                experience_factor = x,
                                mean_loss = mean(loss_temp),
                                VaR_99 = VaR,
                                TVaR_99 = mean(loss_temp[loss_temp > VaR])
                              ))
}

experience_results

#######################################
# Combine Results
#######################################

# Standardize column names
exposure_results <- exposure_results %>% rename(stress_factor = stress_factor)
hours_results    <- hours_results %>% rename(stress_factor = stress_factor)
supervision_results <- supervision_results %>% rename(stress_factor = supervision_factor)
experience_results  <- experience_results %>% rename(stress_factor = experience_factor)

# Add test labels
exposure_results$test <- "Exposure"
hours_results$test <- "Hours"
supervision_results$test <- "Supervision"
experience_results$test <- "Experience"

# Now rbind will work
stress_results1 <- rbind(exposure_results,
                         hours_results,
                         supervision_results,
                         experience_results)

stress_results1

ggplot(stress_results1,
       aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line() +
  geom_point() +
  labs(title = "Operational Stress Tests on Workers' Compensation Losses",
       x = "Stress Level",
       y = "VaR 99%")

#######################################
#Training / Protective Gear
#######################################
training_results <- data.frame()

for(t in training_stress_levels){
  
  sev_shape_stress <- sev_shape
  sev_scale_stress <- sev_scale * t
  
  loss_temp <- sapply(freq_sim, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape_stress, scale = sev_scale_stress))
  })
  
  training_results <- rbind(training_results,
                            data.frame(
                              stress_factor = t,
                              mean_loss = mean(loss_temp),
                              VaR_99 = quantile(loss_temp,0.99),
                              TVaR_99 = mean(loss_temp[loss_temp > quantile(loss_temp,0.99)])
                            ))
}

#######################################
#Gravity / Hazard Level
#######################################
gravity_results <- data.frame()

for(g in gravity_stress_levels){
  
  freq_stress <- rnbinom(n_sim,
                         size = zinb_model$theta,
                         mu = lambda_mean * g)
  
  sev_scale_stress <- sev_scale * g
  
  loss_temp <- sapply(freq_stress, function(n){
    if(n == 0) return(0)
    sum(rgamma(n, shape = sev_shape, scale = sev_scale_stress))
  })
  
  gravity_results <- rbind(gravity_results,
                           data.frame(
                             stress_factor = g,
                             mean_loss = mean(loss_temp),
                             VaR_99 = quantile(loss_temp,0.99),
                             TVaR_99 = mean(loss_temp[loss_temp > quantile(loss_temp,0.99)])
                           ))
}

training_results$test <- "Training/PPE"
gravity_results$test <- "Gravity Hazard Level"

stress_results2 <- rbind(training_results, gravity_results)

ggplot(stress_results2,
       aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Operational Risk Stress Tests",
       x = "Stress Level",
       y = "VaR (99%)") +
  theme_minimal()

#######################################
# Stress Test: Psychological Stress
#######################################
psych_results <- data.frame()

for(s in psych_stress_levels){
  
  # Increase frequency for stressed workers
  freq_stress <- rnbinom(n_sim,
                         size = zinb_model$theta,
                         mu = lambda_mean * s)
  
  # Increase severity slightly
  sev_scale_stress <- sev_scale * (1 + (s - 1) * 0.5)
  
  loss_temp <- sapply(freq_stress, function(n){
    if(n == 0) return(0)
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress))
  })
  
  psych_results <- rbind(psych_results,
                         data.frame(
                           stress_factor = s,
                           mean_loss = mean(loss_temp),
                           VaR_99 = quantile(loss_temp,0.99),
                           TVaR_99 = mean(loss_temp[loss_temp > quantile(loss_temp,0.99)])
                         ))
}

psych_results$test <- "Psychological Stress"

#######################################
# Stress Test: Psychological-Physical Correlation
#######################################

library(copula)

corr_results <- data.frame()

for(rho in correlation_levels){
  
  # Gaussian copula with higher dependence
  cop <- normalCopula(param = rho, dim = 2)
  
  u <- rCopula(n_sim, cop)
  
  # Convert to frequency and severity drivers
  freq_driver <- qnorm(u[,1])
  sev_driver  <- qnorm(u[,2])
  
  freq_stress <- rnbinom(n_sim,
                         size = zinb_model$theta,
                         mu = lambda_mean * exp(0.2 * freq_driver))
  
  sev_scale_stress <- sev_scale * exp(0.2 * sev_driver)
  
  loss_temp <- sapply(1:n_sim, function(i){
    n <- freq_stress[i]
    if(n == 0) return(0)
    sum(rgamma(n,
               shape = sev_shape,
               scale = sev_scale_stress[i]))
  })
  
  corr_results <- rbind(corr_results,
                        data.frame(
                          stress_factor = rho,
                          mean_loss = mean(loss_temp),
                          VaR_99 = quantile(loss_temp,0.99),
                          TVaR_99 = mean(loss_temp[loss_temp > quantile(loss_temp,0.99)])
                        ))
}

corr_results$test <- "Psych-Physical Correlation"


stress_results3 <- rbind(psych_results, corr_results)

ggplot(stress_results3,
       aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Psychological Risk Stress Tests",
       x = "Stress Level",
       y = "VaR (99%)") +
  theme_minimal()


#######################################
# Occupation Risk Stress Test
#######################################
high_risk <- c("miners", "scientists")

occupation_results <- data.frame()

for(s in occ_levels){
  
  stressed_freq <- freq_sim
  
  high_risk_index <- model_worker_freq$occupation %in% high_risk
  
  stressed_freq[high_risk_index] <- stressed_freq[high_risk_index] * s
  
  aggregate_stress <- stressed_freq * sev_sim
  
  occupation_results <- rbind(occupation_results,
                              data.frame(
                                stress_factor = s,
                                VaR_99 = quantile(aggregate_stress,0.99),
                                TVaR_99 = mean(aggregate_stress[aggregate_stress >
                                                                  quantile(aggregate_stress,0.99)])
                              ))
}

occupation_results

#######################################
# Worker Type Stress Test
#######################################

worker_results <- data.frame()

for(s in worker_levels){
  
  stressed_freq <- freq_sim
  
  contract_index <- model_worker_freq$employment_type == "contract"
  
  stressed_freq[contract_index] <- stressed_freq[contract_index] * s
  
  aggregate_stress <- stressed_freq * sev_sim
  
  worker_results <- rbind(worker_results,
                          data.frame(
                            stress_factor = s,
                            VaR_99 = quantile(aggregate_stress,0.99),
                            TVaR_99 = mean(aggregate_stress[aggregate_stress >
                                                              quantile(aggregate_stress,0.99)])
                          ))
}

worker_results

occupation_results$test <- "Occupation Risk"
worker_results$test <- "Worker Type"

all_stress <- rbind(occupation_results,
                    worker_results)

ggplot(all_stress,
       aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line() +
  geom_point() +
  labs(title = "Workers Compensation Stress Testing",
       x = "Stress Level",
       y = "VaR 99%")

#######################################
# Claims Inflation Stress Test
#######################################
inflation_results <- data.frame()

for(i in inflation_levels){
  
  # Inflate severity
  sev_stress <- sev_sim * i
  
  # Compute aggregate loss
  aggregate_stress <- freq_sim * sev_stress
  
  # Store results
  inflation_results <- rbind(inflation_results,
                             data.frame(
                               stress_factor = i,
                               mean_loss = mean(aggregate_stress),
                               VaR_99 = quantile(aggregate_stress,0.99),
                               TVaR_99 = mean(aggregate_stress[aggregate_stress >
                                                                 quantile(aggregate_stress,0.99)])
                             ))
}

inflation_results$test <- "Claims Inflation"
inflation_results

#######################################
# Salary-Linked Claims Stress Test
#######################################
salary_results <- data.frame()

avg_salary <- mean(worker_data_sev$base_salary, na.rm = TRUE)

for(s in salary_levels){
  
  # Scale claim severity according to salary increase
  stressed_salary <- avg_salary * s
  salary_multiplier <- stressed_salary / avg_salary
  sev_stress <- sev_sim * salary_multiplier
  
  # Compute aggregate loss
  aggregate_stress <- freq_sim * sev_stress
  
  # Store results
  salary_results <- rbind(salary_results,
                          data.frame(
                            stress_factor = s,
                            mean_loss = mean(aggregate_stress),
                            VaR_99 = quantile(aggregate_stress,0.99),
                            TVaR_99 = mean(aggregate_stress[aggregate_stress >
                                                              quantile(aggregate_stress,0.99)])
                          ))
}

salary_results$test <- "Salary-Linked Claims"
salary_results


#######################################
# Combine and Plot Inflation & Salary Stress
#######################################

stress_results_final <- rbind(inflation_results, salary_results)

ggplot(stress_results_final,
       aes(x = stress_factor, y = VaR_99, color = test)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(title = "Claims Inflation & Salary-Linked Stress Tests",
       x = "Stress Multiplier",
       y = "VaR 99%") +
  theme_minimal()