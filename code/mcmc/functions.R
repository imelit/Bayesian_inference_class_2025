# ============================================
# MCMC Sampler for Poisson Model (Metropolis-Hastings)
# auxiliary functions
# ============================================

# Load required libraries
library(MASS)  # For multivariate normal distribution generation


# Log-Posterior Density Function
log_posterior_poisson <- function(rate_param, data) {
  
  if (rate_param <= 0) return(-Inf)  # Ensure positivity
  
  n <- length(data)
  sum_y <- sum(data)
  
  log_likelihood <- sum_y * log(rate_param) - n * rate_param
  log_prior <- 0  # Improper non-informative prior: log(1)=0
  
  return(log_likelihood + log_prior)
}

# Log-Posterior Density Function
log_posterior_poisson_gamma <- function(rate_param, data, shape_prior, rate_prior) {
  
  if (rate_param <= 0) return(-Inf)  # Ensure positivity
  
  n <- length(data)
  sum_y <- sum(data)
  
  log_likelihood <- sum_y * log(rate_param) - n * rate_param
  log_prior <- (shape_prior-1)*log(rate_param)-rate_param*rate_prior  # gamma(shape,rate)
  #log_prior <- dgamma(rate_param, shape = shape_prior, rate = rate_prior)   
  
  return(log_likelihood + log_prior)
}


# Random Walk Proposal Function (log scale)
random_walk_proposal <- function(current_param, step_size) {
  log_current <- log(current_param)
  log_proposed <- log_current + rnorm(1, mean = 0, sd = step_size)
  proposed_param <- exp(log_proposed)
  return(proposed_param)
}


# Metropolis-Hastings MCMC Sampler
mcmc_poisson <- function(data, n_iter = 5000, initial_param = 1, step_size = 0.1, shape_prior=3,rate_prior=2, burn_in=0.2) {
  
  # Storage vectors
  samples <- numeric(n_iter)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Initialize
  current_param <- initial_param
  #current_log_post <- log_posterior_poisson(current_param, data)
  current_log_post <- log_posterior_poisson_gamma(current_param, data,shape_prior,rate_prior)

  samples[1] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameter:", initial_param, " | Step size:", step_size, "\n\n")
  
  # MCMC Loop
  for (index in 2:n_iter) {
    
    proposed_param <- random_walk_proposal(current_param, step_size)
    #proposed_log_post <- log_posterior_poisson(proposed_param, data)
    proposed_log_post <- log_posterior_poisson_gamma (proposed_param, data,shape_prior ,rate_prior )
    log_accept_ratio <- proposed_log_post - current_log_post
    
    # Accept/reject step
    if (log_accept_ratio >= 0 || runif(1) < exp(log_accept_ratio)) {
      current_param <- proposed_param
      current_log_post <- proposed_log_post
      acceptance[index] <- 1
    }
    
    # Store results
    samples[index] <- current_param
    log_post_values[index] <- current_log_post
    
    # Print progress every 100 iterations
    if (index %% 1000 == 0) {
      cat(sprintf("Iteration %d | Current Log-Posterior = %.3f | Acceptance rate = %.2f%%\n",
                  index, current_log_post, mean(acceptance[1:index]) * 100))
    }
  }
  
  init <- floor(burn_in*n_iter)
  samples_after_burn_in  <- samples[init:num_iteration]
  log_post_after_burn_in  <- log_post_values[init:num_iteration]
  acceptance_after_burn_in <- acceptance[init:num_iteration]
  
  cat("\nMCMC Finished\n")
  cat("Final Acceptance Rate:", round(mean(acceptance) * 100, 2), "%\n")
  
  return(list(samples=samples_after_burn_in,
              log_post_values=log_post_after_burn_in,
              acceptance=acceptance_after_burn_in))
  
}


#--------------------------------------------------------------
# mcmc for Negegativa Binomial
#----------------------------------------------------------------------


# Log-Posterior Density Function
log_posterior_negBinomial <- function(param, data) {
  
  #expected_param <-param[1] #mu
  
  p <-param[2]/(param[2]+param[1]) #success
  r <- param[2] #dispersion_param
  n <-lenght(Data)
  
  if (expected_param <= 0 || dispersion_param ) return(-Inf)  # Ensure positivity
  
  log_likelihood <- sum(lgamma(data + r) + y * log(1-p)) -n*lgamma(r) + r * log(p) 
  
  log_prior <- 0  # Improper non-informative prior: log(1)=0
  
  return(log_likelihood + log_prior)
}


multivariate_random_walk <- function(current_param, covariance_matrix) {
  
  # Get dimension of the problem
  dimension <- length(current_param)
  log_current <- log(current_param)
  log_proposed <- log_current + mvrnorm(1, mu = rep(0, dimension), Sigma = covariance_matrix) 
  proposed_param <- exp(log_proposed)
  return(proposed_param)
}


# Metropolis-Hastings MCMC Sampler
mcmc_negBionomial <- function(data,n_iter,initial_param, covMat, burn_in){
      

  totalParN = lenght(initial_param)  # number of parameters

  # Storage vectors and matrix
  # The matrix save the sampled parameters
  # row = iterations, columns = parameters

  samples <- matrix(NA, nrow = n_iter, ncol = totalParN)
  
  log_post_values <- numeric(n_iter)
  
  acceptance <- numeric(n_iter)
  
  # Initialize
  current_param <- initial_param
  current_log_post <- log_posterior_negBinomial(current_param, data)
  
  samples[1,] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameter:", initial_param, " | Step size:", step_size, "\n\n")
  
  # MCMC Loop
  for (index in 2:n_iter) {
    
    proposed_param <- multivariate_random_walk(current_param, covMat)
    proposed_log_post <- log_posterior_negBinomial (proposed_param, data)
    log_accept_ratio <- proposed_log_post - current_log_post
    
    # Accept/reject step
    if (log_accept_ratio >= 0 || runif(1) < exp(log_accept_ratio)) {
      current_param <- proposed_param
      current_log_post <- proposed_log_post
      acceptance[index] <- 1
    }
    
    # Store results
    samples[index] <- current_param
    log_post_values[index] <- current_log_post
    
    # Print progress every 100 iterations
    if (index %% 1000 == 0) {
      cat(sprintf("Iteration %d | Current Log-Posterior = %.3f | Acceptance rate = %.2f%%\n",
                  index, current_log_post, mean(acceptance[1:index]) * 100))
    }
  }
  
  init <- floor(burn_in*n_iter)
  samples_after_burn_in  <- samples[init:num_iteration]
  log_post_after_burn_in  <- log_post_values[init:num_iteration]
  acceptance_after_burn_in <- acceptance[init:num_iteration]
  
  cat("\nMCMC Finished\n")
  cat("Final Acceptance Rate:", round(mean(acceptance) * 100, 2), "%\n")
  
  return(list(samples=samples_after_burn_in,
              log_post_values=log_post_after_burn_in,
              acceptance=acceptance_after_burn_in))

  }



