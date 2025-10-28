#-----------------------------------------------------
# MCMC sampler with Metropolis-Hasting method
# auxiliary functions
#--------------------------------------------------------

# Librerías requeridas
library(MASS)  # Para generación de distribuciones normales multivariadas

# random walk
multivariate_random_walk <- function(current_param, covariance_matrix) {
  dimension <- length(current_param)
  proposed_param <- current_param + mvrnorm(1, mu = rep(0, dimension), Sigma = covariance_matrix) 
  return(proposed_param)
}

#posterior function with no-informative prior
log_posterior_lm_nopriors <- function(param, data) {
  
  beta0 <- param[1]       # beta0 
  beta1 <- param[2]       # beta0 
  sigma2 <- exp(param[3])   #log_sigma^2 
  mu <-beta0 + beta1*data$X       #vector  
  
  n <- length(data)
  
  # Log-verosimilitud de la Normal(mu,sigma^2)
  
  log_likelihood <- -n*log(2*pi*sigma2)/2- sum((data$Y-mu)*(data$Y-mu))/(2*sigma2)
  
  # Prior no informativo
  log_prior <- 0
  
  return(log_likelihood + log_prior)
}

#MCMC with Metropolis-Hastings Method

mcmc_lm_noinformative <- function(data, n_iter = 5000, initial_param = c(0,0, 1), 
                             cov_matrix = diag(3) * 0.1, burn_in = 0.2) {
  
  total_params <- length(initial_param)  # Número de parámetros (2 en este caso)
  
  # Almacenamiento: matriz para múltiples parámetros
  samples <- matrix(NA, nrow = n_iter, ncol = total_params)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Inicialización
  current_param <- initial_param
  current_log_post <- log_posterior_lm_nopriors(current_param, data)
  
  samples[1, ] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameters: beta0 =", initial_param[1], 
      ", beta1 =", initial_param[2],", log sigma^2 =", initial_param[3], "\n\n")
  
  # Loop MCMC
  for (index in 2:n_iter) {
    
    # Propuesta multivariada
    proposed_param <- multivariate_random_walk(current_param, cov_matrix)
    proposed_log_post <- log_posterior_lm_nopriors(proposed_param, data)
    
    # Razón de aceptación
    log_accept_ratio <- proposed_log_post - current_log_post
    
    # Aceptar/Rechazar
    if (log_accept_ratio >= 0 || runif(1) < exp(log_accept_ratio)) {
      current_param <- proposed_param
      current_log_post <- proposed_log_post
      acceptance[index] <- 1
    }
    
    # Almacenar (nota: samples[index, ] para matrices)
    samples[index, ] <- current_param
    log_post_values[index] <- current_log_post
    
    # Progreso
    if (index %% 1000 == 0) {
      cat(sprintf("Iteration %d | Current Log-Posterior = %.3f | Acceptance rate = %.2f%%\n",
                  index, current_log_post, mean(acceptance[1:index]) * 100))
    }
  }
  
  # Aplicar burn-in
  init <- floor(burn_in * n_iter) + 1
  samples_after_burn_in <- samples[init:n_iter, ]
  log_post_after_burn_in <- log_post_values[init:n_iter]
  acceptance_after_burn_in <- acceptance[init:n_iter]
  
  cat("\nMCMC Finished\n")
  cat("Final Acceptance Rate:", round(mean(acceptance) * 100, 2), "%\n")
  cat("Acceptance Rate (after burn-in):", 
      round(mean(acceptance_after_burn_in) * 100, 2), "%\n")
  
  # Para matrices, agregar nombres a las columnas
  colnames(samples_after_burn_in) <- c("beta0", "beta1","logSigma2")
  
  return(list(samples = samples_after_burn_in,
              log_post_values = log_post_after_burn_in,
              acceptance = acceptance_after_burn_in,
              acceptance_rate = mean(acceptance_after_burn_in)))
}

