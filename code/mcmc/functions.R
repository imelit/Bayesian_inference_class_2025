# =============================================================
# MCMC Sampler con Metropolis-Hastings
# Conjunto de samplers para diferentes modelos bayesianos
# Curso: Inferencia Bayesiana
# Autor: Imelda Trejo
# Last update: October 2nd, 2025
# =============================================================

# Librerías requeridas
library(MASS)  # Para generación de distribuciones normales multivariadas


#=============================================================
# FUNCIONES DE PROPUESTA (PROPOSAL FUNCTIONS)
#=============================================================

# Propuesta Random Walk en UNA dimensión
# Trabaja en escala logarítmica para mantener positividad

random_walk_proposal <- function(current_param, step_size) {
  log_current <- log(current_param)
  log_proposed <- log_current + rnorm(1, mean = 0, sd = step_size)
  proposed_param <- exp(log_proposed)
  return(proposed_param)
}

# Propuesta Random Walk MULTIVARIADA
# Trabaja en escala logarítmica para mantener positividad de todos los parámetros
# Retorna: vector de nuevos valores propuestos (todos positivos)

multivariate_random_walk <- function(current_param, covariance_matrix) {
  dimension <- length(current_param)
  log_current <- log(current_param)
  log_proposed <- log_current + mvrnorm(1, mu = rep(0, dimension), Sigma = covariance_matrix) 
  proposed_param <- exp(log_proposed)
  return(proposed_param)
}


#=============================================================
# MODELO 1: POISSON CON PRIOR NO INFORMATIVO
#=============================================================

# Función de Log-Posterior para Poisson con prior no informativo
#
# MODELO:
#   Y_i ~ Poisson(λ)  para i = 1, ..., n
#   π(λ) ∝ 1  (prior no informativo, log(1) = 0)
#
# Parámetros:
#   rate_param: parámetro λ de la distribución Poisson (tasa o media)
#   data: vector de observaciones
#
# Retorna: log-posterior = log-likelihood + log-prior
log_posterior_poisson <- function(rate_param, data) {
  
  if (rate_param <= 0) return(-Inf)  # Garantiza positividad
  
  n <- length(data)
  sum_y <- sum(data)
  
  # Log-verosimilitud: Σ[y_i * log(λ) - λ]
  log_likelihood <- sum_y * log(rate_param) - n * rate_param
  
  # Log-prior no informativo: log(1) = 0
  log_prior <- 0
  
  return(log_likelihood + log_prior)
}


# MCMC Sampler para Poisson con prior no informativo
#
# Parámetros:
#   data: vector de observaciones Poisson
#   n_iter: número total de iteraciones MCMC
#   initial_param: valor inicial de λ
#   step_size: tamaño de paso para la propuesta (controla aceptación)
#   burn_in: proporción de iteraciones a descartar (ej: 0.2 = 20%)
#
# Retorna: lista con samples, log_post_values, y acceptance (después de burn-in)
mcmc_poisson <- function(data, n_iter = 5000, initial_param = 1, 
                         step_size = 0.1, burn_in = 0.2) {
  
  # Vectores de almacenamiento
  samples <- numeric(n_iter)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Inicialización
  current_param <- initial_param
  current_log_post <- log_posterior_poisson(current_param, data)
  
  samples[1] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameter:", initial_param, " | Step size:", step_size, "\n\n")
  
  # Loop MCMC
  for (index in 2:n_iter) {
    
    # Paso 1: Proponer nuevo valor
    proposed_param <- random_walk_proposal(current_param, step_size)
    proposed_log_post <- log_posterior_poisson(proposed_param, data)
    
    # Paso 2: Calcular razón de aceptación (en log)
    log_accept_ratio <- proposed_log_post - current_log_post
    
    # Paso 3: Aceptar/Rechazar
    if (log_accept_ratio >= 0 || runif(1) < exp(log_accept_ratio)) {
      current_param <- proposed_param
      current_log_post <- proposed_log_post
      acceptance[index] <- 1
    }
    
    # Almacenar resultados
    samples[index] <- current_param
    log_post_values[index] <- current_log_post
    
    # Progreso cada 1000 iteraciones
    if (index %% 1000 == 0) {
      cat(sprintf("Iteration %d | Current Log-Posterior = %.3f | Acceptance rate = %.2f%%\n",
                  index, current_log_post, mean(acceptance[1:index]) * 100))
    }
  }
  
  # Aplicar burn-in
  init <- floor(burn_in * n_iter) + 1
  samples_after_burn_in <- samples[init:n_iter]
  log_post_after_burn_in <- log_post_values[init:n_iter]
  acceptance_after_burn_in <- acceptance[init:n_iter]
  
  cat("\nMCMC Finished\n")
  cat("Final Acceptance Rate:", round(mean(acceptance) * 100, 2), "%\n")
  cat("Acceptance Rate (after burn-in):", 
      round(mean(acceptance_after_burn_in) * 100, 2), "%\n")
  
  return(list(samples = samples_after_burn_in,
              log_post_values = log_post_after_burn_in,
              acceptance = acceptance_after_burn_in,
              acceptance_rate = mean(acceptance_after_burn_in)))
}


#=============================================================
# MODELO 2: POISSON CON PRIOR GAMMA
#=============================================================

# Función de Log-Posterior para Poisson con prior Gamma
#
# MODELO:
#   Y_i ~ Poisson(λ)  para i = 1, ..., n
#   λ ~ Gamma(a, b)  donde a = shape, b = rate
#   π(λ) ∝ λ^(a-1) * exp(-b*λ)
#
# Parámetros:
#   rate_param: parámetro λ de la distribución Poisson
#   data: vector de observaciones
#   shape_prior: parámetro a de la Gamma (shape)
#   rate_prior: parámetro b de la Gamma (rate)
#
# Retorna: log-posterior = log-likelihood + log-prior
log_posterior_poisson_gamma <- function(rate_param, data, shape_prior, rate_prior) {
  
  if (rate_param <= 0) return(-Inf)
  
  n <- length(data)
  sum_y <- sum(data)
  
  # Log-verosimilitud
  log_likelihood <- sum_y * log(rate_param) - n * rate_param
  
  # Log-prior: Gamma(a, b) -> log[π(λ)] = (a-1)*log(λ) - b*λ + constante
  log_prior <- (shape_prior - 1) * log(rate_param) - rate_param * rate_prior
  
  return(log_likelihood + log_prior)
}


# MCMC Sampler para Poisson con prior Gamma
#
# Parámetros:
#   data: vector de observaciones Poisson
#   shape_prior: parámetro a de la Gamma prior
#   rate_prior: parámetro b de la Gamma prior
#   n_iter: número total de iteraciones MCMC
#   initial_param: valor inicial de λ
#   step_size: tamaño de paso para la propuesta
#   burn_in: proporción de iteraciones a descartar
#
# Retorna: lista con samples, log_post_values, y acceptance (después de burn-in)
mcmc_poisson_gamma <- function(data, shape_prior, rate_prior, 
                               n_iter = 5000, initial_param = 1, 
                               step_size = 0.1, burn_in = 0.2) {
  
  # Vectores de almacenamiento
  samples <- numeric(n_iter)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Inicialización
  current_param <- initial_param
  current_log_post <- log_posterior_poisson_gamma(current_param, data, 
                                                  shape_prior, rate_prior)
  
  samples[1] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Prior: Gamma(shape =", shape_prior, ", rate =", rate_prior, ")\n")
  cat("Initial parameter:", initial_param, " | Step size:", step_size, "\n\n")
  
  # Loop MCMC
  for (index in 2:n_iter) {
    
    # Proponer nuevo valor
    proposed_param <- random_walk_proposal(current_param, step_size)
    proposed_log_post <- log_posterior_poisson_gamma(proposed_param, data, 
                                                     shape_prior, rate_prior)
    
    # Calcular razón de aceptación
    log_accept_ratio <- proposed_log_post - current_log_post
    
    # Aceptar/Rechazar
    if (log_accept_ratio >= 0 || runif(1) < exp(log_accept_ratio)) {
      current_param <- proposed_param
      current_log_post <- proposed_log_post
      acceptance[index] <- 1
    }
    
    # Almacenar
    samples[index] <- current_param
    log_post_values[index] <- current_log_post
    
    # Progreso
    if (index %% 1000 == 0) {
      cat(sprintf("Iteration %d | Current Log-Posterior = %.3f | Acceptance rate = %.2f%%\n",
                  index, current_log_post, mean(acceptance[1:index]) * 100))
    }
  }
  
  # Aplicar burn-in
  init <- floor(burn_in * n_iter) + 1
  samples_after_burn_in <- samples[init:n_iter]
  log_post_after_burn_in <- log_post_values[init:n_iter]
  acceptance_after_burn_in <- acceptance[init:n_iter]
  
  cat("\nMCMC Finished\n")
  cat("Final Acceptance Rate:", round(mean(acceptance) * 100, 2), "%\n")
  cat("Acceptance Rate (after burn-in):", 
      round(mean(acceptance_after_burn_in) * 100, 2), "%\n")
  
  return(list(samples = samples_after_burn_in,
              log_post_values = log_post_after_burn_in,
              acceptance = acceptance_after_burn_in,
              acceptance_rate = mean(acceptance_after_burn_in)))
}


#=============================================================
# MODELO 3: BINOMIAL NEGATIVA (CASO MULTIVARIADO)
#=============================================================

# Función de Log-Posterior para Binomial Negativa
#
# MODELO:
#   Y_i ~ NegBin(μ, r)  para i = 1, ..., n
#   donde μ = media, r = parámetro de dispersión
#   Parametrización: p = r/(r + μ), r = r
#   π(μ, r) ∝ 1  (prior no informativo)
#
# Parámetros:
#   param: vector c(μ, r) donde μ = media, r = dispersión
#   data: vector de observaciones
#
# Retorna: log-posterior = log-likelihood + log-prior
log_posterior_negBinomial <- function(param, data) {
  
  mean_param <- param[1]       # μ (media)
  dispersion_param <- param[2] # r (dispersión)
  
  # Validar parámetros positivos
  if (mean_param <= 0 || dispersion_param <= 0) return(-Inf)
  
  n <- length(data)
  r <- dispersion_param
  p <- dispersion_param / (dispersion_param + mean_param)
  
  # Log-verosimilitud de la Binomial Negativa
 
  log_likelihood <- sum(lgamma(data + r) +log(1 - p)*data) +
    n * (r*log(p) - lgamma(r))
  
  # Prior no informativo
  log_prior <- 0
  
  return(log_likelihood + log_prior)
}


# MCMC Sampler para Binomial Negativa
#
# Parámetros:
#   data: vector de observaciones
#   n_iter: número de iteraciones MCMC
#   initial_param: vector inicial c(μ, r)
#   cov_matrix: matriz de covarianza 2x2 para la propuesta multivariada
#   burn_in: proporción de burn-in
#
# Retorna: lista con samples (matriz), log_post_values, y acceptance
mcmc_negBinomial <- function(data, n_iter = 5000, initial_param = c(1, 1), 
                             cov_matrix = diag(2) * 0.1, burn_in = 0.2) {
  
  total_params <- length(initial_param)  # Número de parámetros (2 en este caso)
  
  # Almacenamiento: matriz para múltiples parámetros
  samples <- matrix(NA, nrow = n_iter, ncol = total_params)
  log_post_values <- numeric(n_iter)
  acceptance <- numeric(n_iter)
  
  # Inicialización
  current_param <- initial_param
  current_log_post <- log_posterior_negBinomial(current_param, data)
  
  samples[1, ] <- current_param
  log_post_values[1] <- current_log_post
  acceptance[1] <- 0
  
  cat("Starting MCMC with", n_iter, "iterations\n")
  cat("Initial parameters: μ =", initial_param[1], 
      ", r =", initial_param[2], "\n\n")
  
  # Loop MCMC
  for (index in 2:n_iter) {
    
    # Propuesta multivariada
    proposed_param <- multivariate_random_walk(current_param, cov_matrix)
    proposed_log_post <- log_posterior_negBinomial(proposed_param, data)
    
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
  colnames(samples_after_burn_in) <- c("mu", "phi")
  
  return(list(samples = samples_after_burn_in,
              log_post_values = log_post_after_burn_in,
              acceptance = acceptance_after_burn_in,
              acceptance_rate = mean(acceptance_after_burn_in)))
}


#=============================================================
# EJEMPLO DE USO
#=============================================================

# # Ejemplo 1: Poisson con prior no informativo
# set.seed(123)
# data_poisson <- rpois(100, lambda = 5)
# result1 <- mcmc_poisson(data_poisson, n_iter = 10000, initial_param = 3, 
#                         step_size = 0.2, burn_in = 0.2)
# 
# # Diagnósticos
# plot(result1$samples, type = "l", main = "Trace Plot - Lambda")
# hist(result1$samples, main = "Posterior Distribution - Lambda", breaks = 30)
# 
# 
# # Ejemplo 2: Poisson con prior Gamma
# result2 <- mcmc_poisson_gamma(data_poisson, shape_prior = 2, rate_prior = 0.5,
#                               n_iter = 10000, initial_param = 3, 
#                               step_size = 0.2, burn_in = 0.2)
# 
# 
# # Ejemplo 3: Binomial Negativa
# data_nb <- rnbinom(100, mu = 5, size = 2)
# cov_mat <- matrix(c(0.1, 0, 0, 0.1), nrow = 2)
# result3 <- mcmc_negBinomial(data_nb, n_iter = 10000, 
#                             initial_param = c(4, 1.5),
#                             cov_matrix = cov_mat, burn_in = 0.2)
# 
# # Trace plots para múltiples parámetros
# par(mfrow = c(2, 2))
# plot(result3$samples[, 1], type = "l", main = "Trace Plot - μ")
# plot(result3$samples[, 2], type = "l", main = "Trace Plot - r")
# hist(result3$samples[, 1], main = "Posterior - μ", breaks = 30)
# hist(result3$samples[, 2], main = "Posterior - r", breaks = 30)