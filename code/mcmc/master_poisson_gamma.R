# ========================================================
# Master Script: Develop an MCMC for a Poisson model
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
# Step 3: Run the MCMC poisson con prior igual a 1
# --------------------------------------------------------

num_iteration <- 20000
burn_in_percentage <- 0.1

#hyperparameters 
a <- 3
b <- 2

result <- mcmc_poisson_gamma(data = datos,
                             shape_prior=a,
                             rate_prior=b,
                             n_iter = num_iteration ,
                             initial_param = suma_y/n,
                             step_size = 0.25,
                             burn_in= burn_in_percentage
                       )


# ---------------------------
# Comparación estimacion Monte Carlo (MCMC) con la posterior analitica 
# ---------------------------
#Posterior=Gamma(sum_yi + a,n+b)

shapePost <- a + suma_y    
ratePost  <- b + n       
x <- seq(min(result$samples), max(result$samples), length.out = 500)


# Posterior histogram
hist(result$samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Distribution (after burn-in)",
     xlab = "Parameter value", freq = FALSE)

# Agregamos curva de densidad empírica (suavizada)
lines(density(result$samples), col = "blue", lwd = 2)
# Agregamos la curva de la densidad teórica
lines(x, dgamma(x, shape = shapePost, rate = ratePost),
      col = "red", lwd = 2)
legend("topright", legend = c("Histograma (simulación Monte Carlo)",
                              "Densidad empírica",
                              "Densidad análitica"),
       col = c("skyblue", "blue", "red"), lwd = c(10, 2, 2),
       bty = "n")


####
# Optional: Save results when is not too heavy

write.table(result$samples, "parameter_samples_gamma_prior.txt", row.names = FALSE, col.names = FALSE)
write.table(result$log_post, "log_posterior_gamma_prior.txt", row.names = FALSE, col.names = FALSE)
write.table(result$acceptance, "acceptance_record_gamma_prior.txt", row.names = FALSE, col.names = FALSE)

cat("Files saved")


