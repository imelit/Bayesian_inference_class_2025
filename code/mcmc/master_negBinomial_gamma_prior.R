# Master Script: Develop an MCMC with a NegBionmial model
# Improper prior (uniform on positive reals)
# Author: Imelda Trejo
# Last update: Sep 25 2025
# ========================================================

rm(list = ls())

# Set working directory (adjust if needed)
#setwd("C:/Users/Imelda/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")

setwd("C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")
# Load functions
source("functions.R")

#for reproducibility
set.seed(1)

# --------------------------------------------------------
# Data: accidentes fatales por año (ejemplo)
# --------------------------------------------------------
datos <- c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22)
n <- length(datos)
suma_y <- sum(datos)

cat("Número de observaciones:", n, "\n")
cat("Suma de accidentes:", suma_y, "\n")
cat("Media muestral:", round(suma_y / n, 2), "\n\n")


# --------------------------------------------------------
# Run the MCMC for a NegBionomial model and non informative prior 
# --------------------------------------------------------

num_iteration <- 100000
burn_in_percentage <- 0.2

initial_param = c(23, 1)

#covariance matrix for the Random Walk
cov_mat <- matrix(c(1.0, 0.5,0.5, 1.0),
                  nrow = 2, byrow = TRUE)

#hyperparameters 
a <- 3
b <- 2

result <- mcmc_negBinomial_prior_gamma(datos,a, b, n_iter = num_iteration, cov_matrix = cov_mat, burn_in = burn_in_percentage)

# --------------------------------------------------------
# Diagnostic
# --------------------------------------------------------


#Trace plots para múltiples parámetros
par(mfrow = c(2, 2))
plot(result$samples[, 1], type = "l", main = "Trace Plot - μ")
plot(result$samples[, 2], type = "l", main = "Trace Plot - r")
hist(result$samples[, 1], main = "Posterior - μ", breaks = 30)
hist(result$samples[, 2], main = "Posterior - r", breaks = 30)

#Note that the r- is to big, what does this mean in terms of the data
#we need to impose a prior for the dispersion parameter and work with adjusting step size

# --------------------------------------------------------
# Basic Posterior Summaries
# --------------------------------------------------------

posterior_mean <- mean(result$samples[, 1])
posterior_median <- median(result$samples[, 1])
posterior_var <- var(result$samples[, 1])
credible_interval <- quantile(result$samples[, 1], probs = c(0.025, 0.975))

cat("Posterior mean:", posterior_mean, "\n")
cat("Posterior median:", posterior_median, "\n")
cat("Posterior variance:", posterior_var, "\n")
cat("95% credible interval:", credible_interval, "\n\n")


