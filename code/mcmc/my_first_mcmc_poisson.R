# ========================================================
# Master Script: Develop my first MCMC for a Poisson model
# Improper prior (uniform on positive reals)
# Author: Imelda Trejo
# Last update: Oct 02 2025
# ========================================================

rm(list = ls())

# Set working directory (adjust if needed)
setwd("C:/Users/Imelda/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")

#setwd("C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/Documentos")
# Load functions


source("functions.R")

#for reproducibility
set.seed(111)

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
# Step 3: Run the MCMC poisson con prior igual a 1
# --------------------------------------------------------

num_iteration <- 200000
burn_in_percentage <- 0.1

result <- mcmc_poisson(data = datos,
                       n_iter = num_iteration ,
                       initial_param = suma_y/n,
                       step_size = 0.25,
                       burn_in= burn_in_percentage)

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

#explore diferent initial conditions, what are yor conclusion?

#-----------------------------------------------------------------
# comparing the convergence of the mcmc method with the 
# analytical parameter posterior distribution
#-------------------------------------------------------------------- 

par(mfrow = c(1, 1))  # two plots side by side

# Posterior analítica.
#Gamma(sum_yi + 1,n)

shapePost <- 1 +  suma_y    
ratePost  <-  n       

x <- seq(min(result$samples), max(result$samples), length.out = 500)


# Posterior histogram
hist(result$samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Distribution (after burn-in)",
     xlab = "Parameter value", freq = FALSE)

# Agregamos curva de densidad empírica (suavizada)
lines(density(result$samples), col = "blue", lwd = 2)
# Agregamos la curva de la densidad teórica
lines(x, dgamma(x, shape = shapePost, rate=ratePost),
      col = "red", lwd = 2)
legend("topright", legend = c("Histograma (simulación)",
                              "Densidad empírica",
                              "Densidad teórica"),
       col = c("skyblue", "blue", "red"), lwd = c(10, 2, 2),
       bty = "n")



# Optional: Save results when is not too heavy

write.table(samples, "parameter_samples_noninformative_prior.txt", row.names = FALSE, col.names = FALSE)
write.table(log_post, "log_posterior_noninformative_prior.txt", row.names = FALSE, col.names = FALSE)
write.table(acceptance, "acceptance_record_noninformative_prior.txt", row.names = FALSE, col.names = FALSE)

cat("Files saved: parameter_samples.txt, log_posterior.txt, acceptance_record.txt\n")




