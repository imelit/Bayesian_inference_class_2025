# =============================================================================
# Random Walk Implementation for Educational Purposes
# =============================================================================
# Author: Imelda Trejo, using ai to clean the codes
# Date: Sep-18-2025
# Purpose: Demonstrate univariate and multivariate random walk simulations
# =============================================================================

# Load required libraries
library(MASS)  # For multivariate normal distribution generation

# =============================================================================
# PART 1: UNIVARIATE RANDOM WALK
# =============================================================================

#' Simulate a univariate random walk
#' 
#' This function generates a random walk in one dimension where each step
#' follows a normal distribution with mean 0 and specified standard deviation.
#' 
#' @param num_steps Integer: Number of steps in the random walk
#' @param initial_value Numeric: Starting value of the walk
#' @param step_size Numeric: Standard deviation of each step (sigma)
#' @return Numeric vector of length (num_steps + 1) containing the walk trajectory
#' 
#' Mathematical formula: theta[t+1] = theta[t] + epsilon[t]
#' where epsilon[t] ~ N(0, sigma^2)
univariate_random_walk <- function(num_steps, initial_value, step_size) {
  
  # Initialize trajectory vector
  trajectory <- numeric(num_steps + 1)
  trajectory[1] <- initial_value
  
  # Generate random walk
  for (step in 1:num_steps) {
    # Generate random step from normal distribution
    random_step <- rnorm(1, mean = 0, sd = step_size)
    # Update trajectory
    trajectory[step + 1] <- trajectory[step] + random_step
  }
  
  return(trajectory)
}

# Simulation parameters
set.seed(123)  # For reproducibility
num_iterations <- 1000
initial_parameter <- 0
small_sigma <- 0.1  # Small step size
large_sigma <- 1.0  # Large step size

# Generate two random walks with different step sizes
small_steps_walk <- univariate_random_walk(num_iterations, initial_parameter, small_sigma)
large_steps_walk <- univariate_random_walk(num_iterations, initial_parameter, large_sigma)

# Create visualization
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Plot small steps walk
plot(0:num_iterations, small_steps_walk, 
     type = "l", col = "blue", lwd = 2,
     main = expression(paste("Small Steps (", sigma, " = 0.1)")),
     xlab = "Iteration", 
     ylab = expression(theta),
     cex.main = 1.2, cex.lab = 1.1)
grid(col = "gray90")

# Plot large steps walk
plot(0:num_iterations, large_steps_walk, 
     type = "l", col = "red", lwd = 2,
     main = expression(paste("Large Steps (", sigma, " = 1.0)")),
     xlab = "Iteration", 
     ylab = expression(theta),
     cex.main = 1.2, cex.lab = 1.1)
grid(col = "gray90")

# Reset plotting parameters
par(mfrow = c(1, 1))

# =============================================================================
# PART 2: MULTIVARIATE RANDOM WALK
# =============================================================================

#' Simulate a multivariate random walk
#' 
#' This function generates a random walk in multiple dimensions where each step
#' follows a multivariate normal distribution with specified covariance matrix.
#' 
#' @param num_steps Integer: Number of steps in the random walk
#' @param initial_values Numeric vector: Starting values for each dimension
#' @param covariance_matrix Matrix: Covariance matrix for the random steps
#' @return Matrix with (num_steps + 1) rows and d columns, where d is the dimension
#' 
#' Mathematical formula: theta[t+1] = theta[t] + epsilon[t]
#' where epsilon[t] ~ MVN(0, Sigma)
multivariate_random_walk <- function(num_steps, initial_values, covariance_matrix) {
  
  # Get dimension of the problem
  dimension <- length(initial_values)
  
  # Initialize trajectory matrix
  trajectory <- matrix(0, nrow = num_steps + 1, ncol = dimension)
  trajectory[1, ] <- initial_values
  
  # Generate multivariate random walk
  for (step in 1:num_steps) {
    # Generate multivariate random step
    random_step <- mvrnorm(1, mu = rep(0, dimension), Sigma = covariance_matrix)
    # Update trajectory
    trajectory[step + 1, ] <- trajectory[step, ] + random_step
  }
  
  return(trajectory)
}

#' Create a d-dimensional covariance matrix
#' 
#' This utility function creates a covariance matrix with specified correlation
#' coefficient between all pairs of variables and unit variance on the diagonal.
#' 
#' @param correlation Numeric: Correlation coefficient (-1 to 1)
#' @param dimension Integer: Dimension of the covariance matrix
#' @return Symmetric covariance matrix of size dimension x dimension
create_covariance_matrix <- function(correlation, dimension) {
  # Create matrix with correlation on off-diagonal elements
  cov_matrix <- matrix(correlation, nrow = dimension, ncol = dimension)
  # Set diagonal elements to 1 (unit variance)
  diag(cov_matrix) <- 1
  
  return(cov_matrix)
}

# =============================================================================
# EXAMPLE 1: 2D Random Walk with Positive Correlation
# =============================================================================

set.seed(123)  # For reproducibility

# Define initial parameters
initial_values <- c(0, 0)
correlation_coefficient <- 0.1
dimension <- 2

# Create covariance matrix manually
covariance_matrix_1 <- matrix(c(1.0, 0.5,
                                0.5, 1.0), 
                              nrow = 2, byrow = TRUE)

# Alternative: use the utility function
covariance_matrix_2 <- create_covariance_matrix(correlation_coefficient, dimension)

# Generate 2D random walk
trajectory_2d <- multivariate_random_walk(num_iterations, initial_values, covariance_matrix_1)

# Visualize 2D trajectory
plot(trajectory_2d[, 1], trajectory_2d[, 2], 
     type = "l", col = "blue", lwd = 2,
     main = "2D Random Walk (Positive Correlation = 0.5)",
     xlab = expression(theta[1]), 
     ylab = expression(theta[2]),
     cex.main = 1.2, cex.lab = 1.1)

# Add starting point
points(initial_values[1], initial_values[2], 
       col = "red", pch = 19, cex = 2)

# Add legend
legend("topleft", 
       legend = c("Trajectory", "Starting Point"), 
       col = c("blue", "red"), 
       lty = c(1, NA), 
       pch = c(NA, 19),
       cex = 1.1)

grid(col = "gray90")

# =============================================================================
# EXAMPLE 2: 2D Random Walk with Negative Correlation
# =============================================================================

# Parameters for negative correlation example
negative_correlation <- -0.8
dimension <- 2

# Create covariance matrix with negative correlation
negative_cov_matrix <- create_covariance_matrix(negative_correlation, dimension)

# Generate 2D random walk with negative correlation
negative_corr_trajectory <- multivariate_random_walk(num_iterations, 
                                                     initial_values, 
                                                     negative_cov_matrix)

# Visualize negative correlation trajectory
plot(negative_corr_trajectory[, 1], negative_corr_trajectory[, 2], 
     type = "l", col = "darkgreen", lwd = 2,
     main = "2D Random Walk (Negative Correlation = -0.8)",
     xlab = expression(theta[1]), 
     ylab = expression(theta[2]),
     cex.main = 1.2, cex.lab = 1.1)

# Add starting point
points(initial_values[1], initial_values[2], 
       col = "red", pch = 19, cex = 2)

# Add legend
legend("topleft", 
       legend = c("Trajectory", "Starting Point"), 
       col = c("darkgreen", "red"), 
       lty = c(1, NA), 
       pch = c(NA, 19),
       cex = 1.1)

grid(col = "gray90")

# =============================================================================
# COMPARISON: DIFFERENT CORRELATION STRUCTURES
# =============================================================================

# Create side-by-side comparison
par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Positive correlation plot
plot(trajectory_2d[, 1], trajectory_2d[, 2], 
     type = "l", col = "blue", lwd = 2,
     main = "Positive Correlation (ρ = 0.5)",
     xlab = expression(theta[1]), 
     ylab = expression(theta[2]),
     cex.main = 1.2, cex.lab = 1.1)
points(initial_values[1], initial_values[2], col = "red", pch = 19, cex = 1.5)
grid(col = "gray90")

# Negative correlation plot
plot(negative_corr_trajectory[, 1], negative_corr_trajectory[, 2], 
     type = "l", col = "darkgreen", lwd = 2,
     main = "Negative Correlation (ρ = -0.8)",
     xlab = expression(theta[1]), 
     ylab = expression(theta[2]),
     cex.main = 1.2, cex.lab = 1.1)
points(initial_values[1], initial_values[2], col = "red", pch = 19, cex = 1.5)
grid(col = "gray90")

# Reset plotting parameters
par(mfrow = c(1, 1))

# =============================================================================
# EXERCISES FOR STUDENTS
# =============================================================================

cat("\n=== EXERCISES FOR STUDENTS ===\n")
cat("1. Modify initial_parameter to see how it affects the random walk\n")
cat("2. Try different values of small_sigma and large_sigma\n")
cat("3. Change correlation_coefficient to values between -1 and 1\n")
cat("4. Experiment with different dimensions (3D, 4D, etc.)\n")
cat("5. Compare the behavior of independent vs correlated random walks\n")
cat("\n=== END OF CODE ===\n")

# =============================================================================
# SUMMARY STATISTICS (Optional)
# =============================================================================

# Calculate some basic statistics
cat("\n=== SUMMARY STATISTICS ===\n")
cat("Univariate Random Walk (Small Steps):\n")
cat("  Final value:", round(tail(small_steps_walk, 1), 3), "\n")
cat("  Range:", round(range(small_steps_walk), 3), "\n")
cat("  Standard deviation:", round(sd(small_steps_walk), 3), "\n\n")

cat("Multivariate Random Walk (Positive Correlation):\n")
cat("  Final values:", round(trajectory_2d[nrow(trajectory_2d), ], 3), "\n")
cat("  Correlation between dimensions:", round(cor(trajectory_2d)[1,2], 3), "\n")




#Ejercicio 2

log_density_poisson_model_prior_1(Y,parameter){
  n <- length(Y)
  theta <- parameter
  sumY=sum(Y)
  log_likelihood_times_prior_propotional= n*theta+sumY*log(theta)  
}


