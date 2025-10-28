##############################################
# Bayesian Analysis: Global Warming Trend
# Author: Imelda Trejo Lorenzo
# Class example from the book "An Introduction to Data Analysis", Michael Franke
# https://michael-franke.github.io/intro-data-analysis/
#
# Objective:
#   Is the annual average world surface temperature increasing over time?
#  
##############################################

rm(list = ls())

# --- Libraries ---

library(dplyr)     # data manipulation
library(ggplot2)  # data visuals  

source("lineal_regression_functions.R")

#for reproducibility
set.seed(1)

#Extract the data
# Read directly from the raw GitHub URL
url <- "https://raw.githubusercontent.com/michael-franke/intro-data-analysis/master/data_sets/average-world-temperature.csv"
data_temperature <- read.csv(url)

# Inspect
head(data_temperature)
str(data_temperature)
summary(data_temperature)

data_temperature %>%
  ggplot(aes(x = year, y = avg_temp)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    y = "temperature (degrees Celsius)",
    title = "Annual average surface land temperature"
  )

# lm tools
fit <- lm(data_temperature$avg_temp~data_temperature$year)
# Extraer parámetros
intercept <- coef(fit)[1]
beta <- coef(fit)[2]

# Mostrar resultados
cat("Intercepto (β_0):", intercept, "\n")
cat("Tasa de crecimiento (β_1):", beta, "\n")

# --- Bayesian Linear Model ---
# Model: avg_temp ~ year, assuming normal residuals- 

data <-data.frame(X=data_temperature$year, Y=data_temperature$avg_temp)

n <- length(data$Y)
suma_y <- sum(data$Y)

cat("Número de observaciones:", n, "\n")
cat("Media muestral de temperatura:", round( suma_y/ n, 2), "\n\n")


# --------------------------------------------------------
# Run the MCMC for a Normal model and non informative prior 
#  classical lm
# --------------------------------------------------------

num_iteration <- 100000
burn_in_percentage <- 0.2

initial_param = c(0, 0, 1) #beta_0, beta_1, logSigma^2 

#log_posterior_lm_nopriors(initial_param,data)

#covariance matrix for the Random Walk
correlation_coefficient <- 0.5
dimension <- 3
cov_mat <- matrix(correlation_coefficient, nrow = dimension, ncol = dimension)
# Set diagonal elements to 1 (unit variance)
diag(cov_mat) <- 1

result <- mcmc_lm_noinformative(data, n_iter = num_iteration, cov_matrix = cov_mat, burn_in = burn_in_percentage)


#Trace plots para múltiples parámetros
par(mfrow = c(2, 3))
plot(result$samples[, 1], type = "l", main = "Trace Plot - beta_0")
plot(result$samples[, 2], type = "l", main = "Trace Plot - beta_1")
plot(result$samples[, 3], type = "l", main = "Trace Plot - logsSigma^2")
hist(result$samples[, 1], main = "Posterior - beta_0", breaks = 30)
hist(result$samples[, 2], main = "Posterior - beta_1", breaks = 30)
hist(result$samples[, 3], main = "Posterior - logSigma^2", breaks = 30)

#cadena 1
mean_beta0 <- mean(result$samples[, 1])
mean_beta1 <- mean(result$samples[, 2])
mean_logSigma2 <- mean(result$samples[, 3])

initial_param = c(mean_beta0, mean_beta1, mean_logSigma2) #beta_0, beta_1, logSigma^2 
result2 <- mcmc_lm_noinformative(data, n_iter = num_iteration, cov_matrix = cov_mat, burn_in = burn_in_percentage)

#Trace plots para múltiples parámetros
par(mfrow = c(2, 3))
plot(result2$samples[, 1], type = "l", main = "Trace Plot - beta_0")
plot(result2$samples[, 2], type = "l", main = "Trace Plot - beta_1")
plot(result2$samples[, 3], type = "l", main = "Trace Plot - logsSigma^2")
hist(result2$samples[, 1], main = "Posterior - beta_0", breaks = 30)
hist(result2$samples[, 2], main = "Posterior - beta_1", breaks = 30)
hist(result2$samples[, 3], main = "Posterior - logSigma^2", breaks = 30)

# --- Interpretation ---
mean_beta1 <- mean(result2$samples[, 2])

cat("\nPosterior mean of β1 (slope):", round(mean_beta1, 5))

