# ========================================================
# Analysis Script: Posterior summaries from Poisson MCMC
# ========================================================
rm(list = ls())


# Set working directory (adjust if needed)
#setwd("C:/Users/Imelda/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")
setwd("C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")


# --------------------------------------------------------
# Load MCMC results (saved by master_poisson.R)
# --------------------------------------------------------
samples <- scan("parameter_samples_noninformative_prior.txt")
acceptance <- scan("acceptance_record_noninformative_prior.txt")

n_iter <- length(samples)


cat("MCMC chain loaded successfully!\n")
cat("Number of samples:", n_iter, "\n")
cat("Acceptance rate:", round(mean(acceptance) * 100, 2), "%\n\n")

# --------------------------------------------------------
# Basic Posterior Summaries
# --------------------------------------------------------

posterior_mean <- mean(samples)
posterior_median <- median(samples)
posterior_var <- var(samples)
credible_interval <- quantile(samples, probs = c(0.025, 0.975))

cat("Posterior mean:", posterior_mean, "\n")
cat("Posterior median:", posterior_median, "\n")
cat("Posterior variance:", posterior_var, "\n")
cat("95% credible interval:", credible_interval, "\n\n")

# --------------------------------------------------------
# Diagnostic Plots
# --------------------------------------------------------
par(mfrow = c(2, 2))

# Trace plot
plot(samples, type = "l", col = "blue",
     main = "Trace Plot",
     xlab = "Iteration", ylab = "Lambda")

# Histogram
hist(samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Histogram",
     xlab = "Lambda", freq = FALSE)

par(mfrow = c(1, 1))  # reset layout



#### COMPARAR LAS DOS DISTRIBUSIONES POSTERIORES

# Results with a gamma prior(3,2)

#x <- seq(min(samples), max(samples), length.out = 500)


x <- seq(0, 20, length.out = 500)

# First curve (gamma prior)
plot(x, dgamma(x, shape = 3, rate = 2), 
     col = "red", lwd = 2, type = "l",
     ylab = "Density", xlab = "x",
     ylim = c(0, 1.2))  # <- Aquí se fija el rango del eje Y


# Second curve (flat prior = 1)
lines(x, rep(1, length(x)), col = "blue", lwd = 2)

legend("topright", legend = c("Gamma(3,2)", "Flat prior = 1"),
       col = c("red", "blue"), lwd = 2)


samples_gamma <- scan("parameter_samples_gamma_prior.txt")

# Posterior histogram
hist(samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Distribution with a noninformative vs gamma prior)",
     xlab = "Parameter value", freq = FALSE, xlim = c(10, 40), ylim = c(0, .4))

# Agregamos curva de densidad empírica (suavizada)
lines(density(samples), col = "blue", lwd = 2)
# Agregamos curva de densidad empírica (suavizada)
lines(density(samples_gamma), col = "red", lwd = 2)
legend("topright", legend = c("Posterior con noinformativo", "Posterior con Gamma(3,2)"),
       col = c("blue","red"), lwd = 2)


## Posterior density (smooth curve)
#plot(density(samples), col = "red", lwd = 2,
 #    main = "Posterior Density Estimate",
#     xlab = "Lambda")


# --------------------------------------------------------
# Basic Posterior Summaries
# --------------------------------------------------------

posterior_mean <- mean(samples_gamma)
posterior_median <- median(samples_gamma)
posterior_var <- var(samples_gamma)
credible_interval <- quantile(samples_gamma, probs = c(0.025, 0.975))

cat("Posterior mean:", posterior_mean, "\n")
cat("Posterior median:", posterior_median, "\n")
cat("Posterior variance:", posterior_var, "\n")
cat("95% credible interval:", credible_interval, "\n\n")
