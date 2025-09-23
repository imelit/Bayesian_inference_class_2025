# ============================================
# MCMC Sampler for Poisson Model (Metropolis-Hastings)
# auxiliary functions
# ============================================

# Log-Posterior Density Function
log_posterior_poisson <- function(rate_param, data) {
  
  if (rate_param <= 0) return(-Inf)  # Ensure positivity
  
  n <- length(data)
  sum_y <- sum(data)
  
  log_likelihood <- sum_y * log(rate_param) - n * rate_param
  log_prior <- 0  # Improper non-informative prior: log(1)=0
  
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
mcmc_poisson <- function(data, n_iter = 5000, initial_param = 1, step_size = 0.1) {
  
  # Storage vectors
  samples <- numeric(n_iter)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Initialize
  current_param <- initial_param
  current_log_post <- log_posterior_poisson(current_param, data)
  samples[1] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameter:", initial_param, " | Step size:", step_size, "\n\n")
  
  # MCMC Loop
  for (index in 2:n_iter) {
    
    proposed_param <- random_walk_proposal(current_param, step_size)
    proposed_log_post <- log_posterior_poisson(proposed_param, data)
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
    if (index %% 100 == 0) {
      cat(sprintf("Iteration %d | Current Log-Posterior = %.3f | Acceptance rate = %.2f%%\n",
                  index, current_log_post, mean(acceptance[1:index]) * 100))
    }
  }
  
  cat("\nMCMC Finished\n")
  cat("Final Acceptance Rate:", round(mean(acceptance) * 100, 2), "%\n")
  
  return(list(samples = samples,
              log_post = log_post_values,
              acceptance = acceptance))
}

# =========================
# Example usage:
# data <- rpois(50, lambda = 3)
# result <- mcmc_poisson(data, n_iter = 2000, initial_param = 2, step_size = 0.1)
# =========================
