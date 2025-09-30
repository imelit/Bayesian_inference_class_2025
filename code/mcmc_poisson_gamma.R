# ========================================================
# Master Script: Develop an MCMC for a Poisson model
# Improper prior (uniform on positive reals)
# Author: Imelda Trejo
# Last update: Sep 25 2025
# ========================================================

rm(list = ls())

# Set working directory (adjust if needed)
#setwd("C:/Users/Imelda/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Teaching/Bayessian_inference/laboratorio/MCMC")

setwd("C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/Documentos")
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
# Step 3: Run the MCMC poisson con prior igual a 1
# --------------------------------------------------------

num_iteration <- 20000

#hyperparameters 

a <- 3
1/b <- 2


result <- mcmc_poisson(data = datos,
                       n_iter = num_iteration ,
                       initial_param = suma_y/n,
                       step_size = 0.25,
                       shape_prior=a,
                      rate_prior=b)
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

# Agregamos curva de densidad empírica (suavizada)
lines(density(samples), col = "blue", lwd = 2)

# ---------------------------
# Comparación estimacion Monte Carlo (MCMC) con la posterior te'orica 
# ---------------------------


#Posterior=Gamma(sum_yi + 1,n)

shapePost <- a + suma_y    
ratePost  <- 1/b + n       
x <- seq(min(samples), max(samples), length.out = 500)


# Posterior histogram
hist(samples, breaks = 30, col = "lightblue", border = "white",
     main = "Posterior Distribution (after burn-in)",
     xlab = "Parameter value", freq = FALSE)

# Agregamos curva de densidad empírica (suavizada)
lines(density(samples), col = "blue", lwd = 2)
# Agregamos la curva de la densidad teórica
lines(x, dgamma(x, shape = shapePost, rate = ratePost),
      col = "red", lwd = 2)
legend("topright", legend = c("Histograma (simulación)",
                              "Densidad empírica",
                              "Densidad teórica"),
       col = c("skyblue", "blue", "red"), lwd = c(10, 2, 2),
       bty = "n")


####


