# ========================================================
# Analysis Script: Posterior summaries from Poisson MCMC
# ========================================================

# Set working directory (adjust if needed)
setwd("C:/Users/Imelda/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")

# --------------------------------------------------------
# Load MCMC results (saved by master_poisson.R)
# --------------------------------------------------------
samples <- scan("parameter_samples.txt")
acceptance <- scan("acceptance_record.txt")

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

# Posterior density (smooth curve)
plot(density(samples), col = "red", lwd = 2,
     main = "Posterior Density Estimate",
     xlab = "Lambda")

# Autocorrelation function
acf(samples, main = "Autocorrelation of Chain")

par(mfrow = c(1, 1))  # reset layout
