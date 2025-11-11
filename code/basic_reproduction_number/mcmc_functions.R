#----------------------------------------------------------
#Random walk
#----------------------------------------------------------

random_walk <- function(param, step_size) {
  pi_param <- param[1]
  size <- param[2]
  
  # Transformación al espacio real
  log_current_pi <- log(pi_param / (1 - pi_param))
  log_current_size <- log(size)
  
  # Propuesta con una gaussiana
  log_proposed_pi <- log_current_pi + rnorm(1, mean  = 0, sd =step_size)
  log_proposed_size <- log_current_size + rnorm(1, mean  = 0, sd =step_size)
  
  # Transformación inversa
  proposed_param_pi <- exp(log_proposed_pi) / (1 + exp(log_proposed_pi))
  proposed_param_size <-exp(log_proposed_size)
  
  return(c(proposed_param_pi,proposed_param_size))
}


#----------------------------------------------------------
#log_posterior usando NegBinomial(mu,size), 
#----------------------------------------------------------

log_posterior_nbinomial <- function(param,alpha_prior,beta_prior,data) {
  
  mu <- data$Population*param[1] # vector (μi)
  size <- param[2] #dispersion scalar 
  
  log_likelihood <- sum(lgamma(data$Cases+size)-lgamma(size)+
        data$Cases*log(mu/(mu+size))+size*log(size/(size+mu)))
  
  log_prior <- (alpha_prior - 1) * log(param[1]) + (beta_prior - 1) * log(1 - param[1])
  
  L <-log_likelihood + log_prior
  
  return(L)
}

#----------------------------------------------------------
# MCMC
#----------------------------------------------------------

mcmc_nbinomial <- function(data, alpha_prior, beta_prior, n_iter = 5000, initial_param = c(0.3,5), 
                            step_size = 0.1, burn_in = 0.2) {
  # Almacenamiento
  
  totalParN <-length(initial_param)
  samples <- matrix(NA, nrow = n_iter, ncol =totalParN)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Inicialización
  current_param <- initial_param
  current_log_post <- log_posterior_nbinomial(current_param,alpha_prior,beta_prior, data)
  
  samples[1, ] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameters: pi =", initial_param[1], "\n\n")
  
  # Loop MCMC
  for (index in 2:n_iter) {
    
    # Propuesta multivariada
    proposed_param <- random_walk(current_param, step_size)
    proposed_log_post <- log_posterior_nbinomial(proposed_param,alpha_prior,beta_prior, data)
    
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
  #colnames(samples_after_burn_in) <- c("mu")
  
  return(list(samples = samples_after_burn_in,
              log_post_values = log_post_after_burn_in,
              acceptance = acceptance_after_burn_in,
              acceptance_rate = mean(acceptance_after_burn_in)))
}

