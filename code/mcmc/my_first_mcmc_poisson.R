# ========================================================
# Master Script: Develop my first MCMC for a Poisson model
# Improper prior (uniform on positive reals)
# Author: Imelda Trejo
# Last update: Sep 23 2025
# ========================================================

# Set working directory (adjust if needed)
setwd("C:/Users/Imelda/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")

# Load functions
source("functions.R")

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
# Step 1: Testing log posterior function
# --------------------------------------------------------
rate_par_init <- suma_y / n
log_post_init <- log_posterior_poisson(rate_par_init, datos)
cat("Log-posterior at mean estimate:", log_post_init, "\n\n")

# Visualize posterior shape on a grid
grid_param <- seq(from = 0.01, to = 150, length = 300)
log_post_vec <- sapply(grid_param, log_posterior_poisson, data = datos)

plot(grid_param, log_post_vec, type = "l", col = "darkred", lwd = 2,
     main = "Log-Posterior vs Parameter",
     xlab = "Rate parameter (theta)",
     ylab = "Log-Posterior")

# --------------------------------------------------------
# Step 2: Testing random walk proposal
# --------------------------------------------------------
epsilon <- 0.3
proposal_test <- random_walk_proposal(rate_par_init, epsilon)
cat("Proposal generated from random walk:", proposal_test, "\n\n")

# --------------------------------------------------------
# Step 3: Run the MCMC
# --------------------------------------------------------

num_iteration <- 20000

result <- mcmc_poisson(data = datos,
                       n_iter = num_iteration ,
                       initial_param = 1 , #rate_par_init
                       step_size = 0.1)

# --------------------------------------------------------
# Step 4: Diagnostics (extra visualization)
# --------------------------------------------------------
par(mfrow = c(1, 2))  # two plots side by side

# Trace plot
plot(result$samples, type = "l", col = "blue",
     main = "Trace Plot",
     xlab = "Iteration", ylab = "Parameter value")

# Histogram (posterior distribution)
hist(result$samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Distribution",
     xlab = "Parameter value", freq = FALSE)

# ---------------------------
# Burn in concept
# ---------------------------

burn_in <- 1000     # periodo de burn-in

# Descartamos burn-in sampled values

samples <- result$samples[(burn_in+1):num_iteration]
log_post <- result$log_post_values[(burn_in+1):num_iteration]
acceptance <- result$acceptance[(burn_in+1):num_iteration]



# Trace plot (después del burn-in)
plot(samples, type = "l", col = "blue",
     main = paste("Trace Plot (burn-in =", burn_in, ")"),
     xlab = "Iteration", ylab = "Parameter value")

# Posterior histogram
hist(samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Distribution (after burn-in)",
     xlab = "Parameter value", freq = FALSE)

par(mfrow = c(1, 1))

#Guardar la muestra despues de eliminar las primeras iteraciones


# Optinal: Save results when is not to heavy

write.table(samples, "parameter_samples.txt", row.names = FALSE, col.names = FALSE)
write.table(log_post, "log_posterior.txt", row.names = FALSE, col.names = FALSE)
write.table(acceptance, "acceptance_record.txt", row.names = FALSE, col.names = FALSE)

cat("Files saved: parameter_samples.txt, log_posterior.txt, acceptance_record.txt\n")
