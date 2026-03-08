library(ggplot2)
library(readxl)
library(dplyr)
library(forcats)
library(stringr)
library(gridExtra)
library(scales)
library(tidyr)
library(MASS)
library(pscl)
library(copula)

# Import datasets
worker_data_freq <- read_excel("srcsc-2026-claims-workers-comp.xlsx", sheet = 1)
worker_data_sev <- read_excel("srcsc-2026-claims-workers-comp.xlsx", sheet = 2)

##################################################################
# DATA CLEANING
##################################################################
clean_data <- function(df) {
  df |>
    filter(!is.na(solar_system), !is.na(occupation)) |>
    arrange(solar_system)
}

worker_data_freq <- clean_data(worker_data_freq)
worker_data_sev  <- clean_data(worker_data_sev)

## add claim_count to sev dataset
worker_data_sev <- worker_data_sev %>%
  left_join(worker_data_freq %>%
              dplyr::select(policy_id, worker_id, claim_count),
            by = c("policy_id", "worker_id"))

# Align all variables with provided value range / levels
worker_data_sev <- worker_data_sev |>
  filter(
    experience_yrs >= 0,
    accident_history_flag >= 0,
    psych_stress_index >= 1,
    hours_per_week >= 20,
    supervision_level >= 0,
    gravity_level >= 0.75,
    safety_training_index >= 1,
    protective_gear_quality >= 1,
    base_salary >= 20000,
    exposure >= 0,
    claim_count >= 0,
    claim_length >= 3,
    claim_amount >= 5
  )

worker_data_sev <- worker_data_sev |>
  mutate(across(
    where(~ is.character(.x) | is.factor(.x)),
    ~ str_remove(as.character(.x), "_.*$")
  ))

worker_data_freq <- worker_data_freq |>
  mutate(across(
    where(~ is.character(.x) | is.factor(.x)),
    ~ str_remove(as.character(.x), "_.*$")
  ))

############################################################
# EDA
############################################################

#Colour Themes
blue_palette <- c("#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c")
blue_palette1 <- c("#c6dbef", "#9ecae1", "#6baed6")
blue_palette7 <- c("#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd",
                   "#08519c", "#08306b")

##################################
#1. Distibution of Claim Amounts
##################################

#claim amount
p1 <- ggplot(worker_data_sev, aes(x = claim_amount)) +
  geom_histogram(binwidth = 1000, fill = "white", color = blue_palette[2],
                 alpha = 0.9) +
  geom_density(aes(y = ..count..), color = blue_palette[5], linewidth = 1) +
  scale_x_continuous(labels = scales::comma) +
  labs(title = "Distribution of Claim Amount",
       x = "Claim Amount", y = "Count") +
  theme_minimal()

#log-claims
p1_log <- ggplot(worker_data_sev, aes(x = log(claim_amount))) +
  geom_histogram(binwidth = 0.2, fill = blue_palette[2], color = "white",
                 alpha = 0.9) +
  geom_density(aes(y = ..count..), color = blue_palette[5], linewidth = 1) +
  labs(title = "Distribution of Log Claim Amount",
       x = "Log(Claim Amount)", y = "Count") +
  theme_minimal()

grid.arrange(p1, p1_log, ncol = 2)

##################################
#2. Occupation vs Solar System
##################################
ggplot(worker_data_sev, aes(x = occupation, fill = solar_system)) +
  geom_bar(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = blue_palette) +
  labs(
    title = "Number of Workers by Occupation and Solar System",
    x = "Occupation",
    y = "Count of Workers",
    fill = "Solar System"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

########################################
#3. Frequency of Claims vs Solar System
########################################

claims_summary <- worker_data_freq |>
  group_by(occupation, solar_system) |>
  summarise(
    total_claims = sum(claim_count, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(claims_summary,
       aes(x = occupation, y = total_claims, fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = blue_palette1) +
  labs(
    title = "Frequency of Claims by Occupation and Solar System",
    x = "Occupation",
    y = "Total Claim Frequency",
    fill = "Solar System"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

########################################
#3. Frequency of Claims adjusted per unit of exposure
########################################
# Error here saying object 'occupation' not found
claim_rate_summary <- worker_data_freq |>
  group_by(occupation, solar_system) |>
  summarise(
    total_claims = sum(claim_count, na.rm = TRUE),
    total_exposure = sum(exposure, na.rm = TRUE),
    claim_rate = total_claims / total_exposure,
    .groups = "drop"
  )

ggplot(claim_rate_summary,
       aes(x = occupation,
           y = claim_rate,
           fill = solar_system)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.7) +
  scale_fill_manual(values = blue_palette1) +
  labs(
    title = "Claim Frequency Rate (Exposure Adjusted)",
    x = "Occupation",
    y = "Claims per Exposure Unit",
    fill = "Solar System"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
#4. Average base salary vs occupation
#######################################

salary_summary <- worker_data_freq |>
  group_by(occupation) |>
  summarise(
    avg_salary = mean(base_salary, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(avg_salary))

ggplot(salary_summary,
       aes(x = reorder(occupation, avg_salary),
           y = avg_salary)) +
  geom_col(fill = blue_palette[4]) +
  coord_flip() +
  labs(
    title = "Average Base Salary by Occupation",
    x = "Occupation",
    y = "Average Base Salary"
  ) +
  theme_minimal()

#######################################
#5. Average claim amount vs injury type
#######################################

worker_summary <- worker_data_sev |>
  group_by(injury_type) |>
  summarise(mean_claim = mean(claim_amount, na.rm = TRUE))

ggplot(worker_summary, aes(x = fct_reorder(injury_type, mean_claim),
                           y = mean_claim)) +
  geom_col(fill = blue_palette[4]) +
  labs(title = "Average Claim Amount by Injury Type",
       x = "Injury Type", y = "Mean Claim Amount") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
#6. Injury type vs occupation
#######################################
injury_occupation_counts <- worker_data_sev |>
  count(occupation, injury_type)

ggplot(injury_occupation_counts, aes(x = fct_reorder(occupation, n, .fun = sum),
                                     y = fct_reorder(injury_type, n, .fun = sum),
                                     fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = blue_palette[1], high = blue_palette[5]) +
  labs(title = "Heatmap: Injury Type vs Occupation",
       x = "Occupation", y = "Injury Type", fill = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
#7. injury type vs solar system
#######################################
ggplot(worker_data_sev,
       aes(x = fct_reorder(occupation, occupation, .fun = length),
           fill = injury_type)) +
  geom_bar(color = "white", linewidth = 0.2) + facet_wrap(~ solar_system) +
  scale_fill_manual(values = blue_palette7) +

  labs(
    title = "Injury Type Distribution by Occupation Across Solar Systems",
    x = "Occupation",
    y = "Number of Claims",
    fill = "Injury Type"
  ) +

  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

#######################################
#8. Exposure vs gravity level
#######################################

ggplot(worker_data_freq,
       aes(x = exposure, y = gravity_level)) +
  geom_point(alpha = 0.5, colour = "#3182bd") +
  labs(
    title = "Exposure vs Gravity Level",
    x = "Exposure (Proportion of Year)",
    y = "Gravity Level"
  ) +
  theme_minimal()


############################################################
# Modelling
############################################################

#######################################
# Data Cleaning
#######################################

model_worker_freq <- worker_data_freq
model_worker_sev <- worker_data_sev

# Fix exposure: replace zeros or NAs
model_worker_freq$exposure[model_worker_freq$exposure <= 0 | is.na(model_worker_freq$exposure)] <- 0.001
model_worker_freq$claim_count <- round(model_worker_freq$claim_count)
model_worker_freq$exposure[model_worker_freq$exposure <= 0] <- 0.001
model_worker_freq$log_exposure <- log(model_worker_freq$exposure)
model_worker_freq <- model_worker_freq[!is.na(model_worker_freq$claim_count), ]

predictors <- c("occupation","employment_type","experience_yrs",
                "accident_history_flag","psych_stress_index",
                "hours_per_week","supervision_level","gravity_level",
                "safety_training_index","protective_gear_quality","base_salary",
                "exposure")

# Remove any rows with NA in these columns
model_worker_freq <- model_worker_freq %>%
  dplyr::filter(!if_any(all_of(predictors), is.na))

#######################################
# Forward Stepwise and Backwards Stepwise Functions 
#######################################
null_freq <- glm(
  claim_count ~ 1 + offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_worker_freq
)

full_freq <- glm(
  claim_count ~ occupation + employment_type + experience_yrs +
    accident_history_flag + psych_stress_index + hours_per_week +
    supervision_level + gravity_level + safety_training_index +
    protective_gear_quality + base_salary +
    offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_worker_freq
)
summary(full_freq)

step_backward <- stepAIC(full_freq,
                         direction = "backward")
summary(step_backward)

upper_formula <- claim_count ~ occupation + employment_type + experience_yrs +
  accident_history_flag + psych_stress_index + hours_per_week +
  supervision_level + gravity_level + safety_training_index +
  protective_gear_quality + base_salary +
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


#######################################
# Frequency
#######################################
# GLM
glm_freq <- glm(
  claim_count ~ occupation + solar_system + accident_history_flag + psych_stress_index +
    safety_training_index + offset(log(exposure)),
  family = poisson(link = "log"),
  data = model_worker_freq
)

summary(glm_freq)


### Negative binomial model
freq_model_nb <- glm.nb(
  claim_count ~ occupation + solar_system + accident_history_flag + psych_stress_index +
    safety_training_index + offset(log(exposure)),
  data = model_worker_freq
)

summary(freq_model_nb)

# ZINB Model
# Leading model
zinb_model <- zeroinfl(
  claim_count ~ occupation + accident_history_flag + psych_stress_index + safety_training_index + solar_system |
    employment_type,  # simpler zero-inflation part
  data = model_worker_freq,
  dist = "negbin",
  offset = log(exposure + 0.001)
)
summary(zinb_model)

#######################################
# Severity GLM
#######################################
model_worker_sev$claim_count <- round(model_worker_sev$claim_count)
worker_data_sev$claim_amount[worker_data_sev$claim_amount <= 0] <- 0.01
worker_data_sev <- na.omit(worker_data_sev)

numeric_vars <- c("experience_yrs", "psych_stress_index", "hours_per_week",
                  "supervision_level", "gravity_level", "safety_training_index",
                  "protective_gear_quality", "base_salary", "exposure")

worker_data_sev[numeric_vars] <- scale(worker_data_sev[numeric_vars])

gamma_sev <- glm(
  claim_amount ~ occupation + solar_system + employment_type +
    accident_history_flag + psych_stress_index + supervision_level + 
    protective_gear_quality + base_salary + claim_length,
  family = Gamma(link = "log"),
  data = worker_data_sev
)

summary(gamma_sev)

#######################################
# Monte Carlo Simulation
#######################################
set.seed(123)
n_sim <- 500000

# Predict mean frequency
lambda_hat <- predict(zinb_model, type = "response")

# Use average lambda
lambda_mean <- mean(lambda_hat)

# Simulate frequency
freq_sim <- rnbinom(n_sim,
                    size = zinb_model$theta,
                    mu = lambda_mean)

# Severity parameters
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(worker_data_sev$claim_amount) / sev_shape

aggregate_loss <- numeric(n_sim)

for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    aggregate_loss[i] <-
      sum(rgamma(freq_sim[i],
                 shape = sev_shape,
                 scale = sev_scale))
  }
}


WC_VaR_99 <- quantile(aggregate_loss, 0.99)
WC_VaR_99

WC_TVaR_99 <- mean(aggregate_loss[aggregate_loss > WC_VaR_99])
WC_TVaR_99

WC_mean_loss <- mean(aggregate_loss)
WC_mean_loss

WC_sd_loss   <- sd(aggregate_loss)
WC_sd_loss

#######################################
# Monte Carlo Simulation Pt. 2
#######################################

set.seed(123)

n_sim <- 10000   # 10k is usually enough for stable 99% VaR

# --- 1. Frequency model inputs ---
lambda_hat <- predict(zinb_model, type = "response")
theta_hat  <- zinb_model$theta

n_pol <- length(lambda_hat)

# --- 2. Severity model inputs ---
sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_mean  <- mean(worker_data_sev$claim_amount)
sev_scale <- sev_mean / sev_shape

# --- 3. Monte Carlo Simulation ---
aggregate_loss <- numeric(n_sim)

for(s in 1:n_sim){
  
  # Simulate frequency for ALL policies
  freq_sim <- rnbinom(n_pol,
                      size = theta_hat,
                      mu   = lambda_hat)
  
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
WC_mean_loss <- mean(aggregate_loss)
WC_sd_loss   <- sd(aggregate_loss)
WC_VaR_99    <- quantile(aggregate_loss, 0.99)
WC_TVaR_99   <- mean(aggregate_loss[aggregate_loss > WC_VaR_99])

WC_mean_loss
WC_sd_loss
WC_VaR_99
WC_TVaR_99

#######################################
# Tail Dependence
#######################################
library(copula)
freq <- worker_data_freq$claim_count
freq_pos <- freq[freq > 0]
sev <- worker_data_sev$claim_amount
n <- min(length(freq_pos), length(sev))

freq_dep <- freq_pos[1:n]
sev_dep  <- sev[1:n]
dep_data <- data.frame(freq_dep, sev_dep)
u <- rank(dep_data$freq_dep) / (n + 1)
v <- rank(dep_data$sev_dep)  / (n + 1)

uv_data <- cbind(u, v)

t_cop <- tCopula(dim = 2)

fit_cop <- fitCopula(t_cop, uv_data, method = "ml")

summary(fit_cop)

set.seed(123)

n_sim <- 500000

cop_sim <- rCopula(n_sim, fit_cop@copula)

u_sim <- cop_sim[,1]
v_sim <- cop_sim[,2]

lambda_hat <- predict(zinb_model, type = "response")
lambda_mean <- mean(lambda_hat)

freq_sim <- qnbinom(u_sim,
                    size = zinb_model$theta,
                    mu = sample(lambda_hat, n_sim, replace = TRUE))

sev_shape <- 1 / summary(gamma_sev)$dispersion
sev_scale <- mean(worker_data_sev$claim_amount) / sev_shape

sev_sim <- qgamma(v_sim,
                  shape = sev_shape,
                  scale = sev_scale)

aggregate_loss <- numeric(n_sim)

for(i in 1:n_sim){
  if(freq_sim[i] > 0){
    aggregate_loss[i] <- sum(rgamma(freq_sim[i],
                                    shape = sev_shape,
                                    scale = sev_scale))
  }
}

WC_mean_loss <- mean(aggregate_loss)
WC_sd_loss   <- sd(aggregate_loss)

WC_VaR_99  <- quantile(aggregate_loss, 0.99)
WC_TVaR_99 <- mean(aggregate_loss[aggregate_loss > WC_VaR_99])
lambda(fit_cop@copula)

##Testing different copula 

#intitial parameter guesses 
norm_cop <- normalCopula(param = 0.5, dim = 2)     # Gaussian
clay_cop <- claytonCopula(param = 2, dim = 2)      # Lower tail dependence
gumb_cop <- gumbelCopula(param = 2, dim = 2)       # Upper tail dependence
t_cop    <- tCopula(param = 0.5, df = 4, dim = 2)  # t-copula, symmetric tails

fit_norm <- fitCopula(norm_cop, uv_data, method = "ml")
fit_clay <- fitCopula(clay_cop, uv_data, method = "ml")
fit_gumb <- fitCopula(gumb_cop, uv_data, method = "ml")
fit_t    <- fitCopula(t_cop, uv_data, method = "ml")

n_sim <- 5000  # smaller for plotting

sim_norm <- rCopula(n_sim, fit_norm@copula)
sim_clay <- rCopula(n_sim, fit_clay@copula)
sim_gumb <- rCopula(n_sim, fit_gumb@copula)
sim_t    <- rCopula(n_sim, fit_t@copula)

#plots of copula 
par(mfrow = c(2,2)) 

plot(sim_norm, main = "Gaussian Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_clay, main = "Clayton Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_gumb, main = "Gumbel Copula", xlab = "Frequency", ylab = "Severity")
plot(sim_t, main = "t-Copula", xlab = "Frequency", ylab = "Severity")


#extreme scenario risk 
quantile(aggregate_loss, 0.995) ##solvency risk 
quantile(aggregate_loss, 0.999) ##catastrophic risk 

#######################################
# Stress testing
#######################################
##stress levels 
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