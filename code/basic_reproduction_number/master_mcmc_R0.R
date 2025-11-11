#-------------------------------------------------------------
# Objective: MCMC to estimate the basic reproduction number
#-------------------------------------------------------------
library(HDInterval)

rm(list = ls())

library(ggplot2)

load("US_h1n1_2009.RData") # a data frame

total_states<- length(data$State)
cat("Número de observaciones:", total_states, "\n")

# Load functions
source("mcmc_functions.R")

# MCMC initial values

param0 <- c(0.9,2)  # 0<pi<1 and dispersion >0  
sd_random_walk <- 0.3

#testing aux function
#random_walk(param0,sd_random_walk) #sd
# log_posterior_nbinomial(param0,alpha_prior,beta_prior, data)

#-----------------------------------------------
# set priors
#------------------------------------------------
# Definir la función
#R0 <-1.2 #muy alto
#R0 <-1.1 #muy alto
R0 <-1.01
f <- function(x) 1-exp(-R0*x) - x 
# Buscar una raíz en el intervalo [0, 1]
root1 <- uniroot(f, c(0.0001, 1))
print(root1$root)


#hyperparameters (1,1) non-informative priors 

#mean 1.21 covering (1.1,1.3)

alpha_prior <-20 #shape
beta_prior <-40 #35

#mean 0.05, covering ()
alpha_prior <- 50
beta_prior <- 5500

# Generar valores aleatorior para R0

samples <- rbeta(10000, shape1 = alpha_prior, shape2 = beta_prior)
hist(samples)
min(samples)
max(samples)

mean(samples)
R0_prior <- (-log(1 - samples)) / samples
mean(R0_prior)
min(R0_prior)
max(R0_prior)
hist(R0_prior)


#MCMC run

num_iteration <- 100000
burn_in_percentage <- 0.2
sd_random_walk <- 0.4

result <- mcmc_nbinomial(data,alpha_prior,beta_prior, n_iter = num_iteration,initial_param =param0,step_size =sd_random_walk, burn_in = burn_in_percentage)

# --------------------------------------------------------
# Diagnostic
# --------------------------------------------------------
pi_samples <- result$samples[,1]
size_samples <-result$samples[,2]

#Trace plots para múltiples parámetros
par(mfrow = c(3, 2))
plot(size_samples, type = "l", main = "Trace Plot - size")
hist(size_samples, main = "Posterior - size", breaks = 30)
#pi-fraction of infection
plot(pi_samples, type = "l", main = "Trace Plot - pi")
hist(pi_samples, main = "Posterior - pi", breaks = 30)
#R0
R0_posterior <- (-log(1 - pi_samples)) / pi_samples
plot(R0_posterior, type = "l", main = "Trace Plot - R0")
hist(R0_posterior, main = "Posterior - R0", breaks = 30)
min(R0_posterior)


# --------------------------------------------------------
# histogramas prior vs posterior
# --------------------------------------------------------

par(mfrow = c(1,1))
df <- data.frame(
  value = c(R0_prior, R0_posterior),
  type = rep(c("Prior", "Posterior"),
             c(length(R0_prior), length(R0_posterior)))
)

ggplot(df, aes(x = value, fill = type)) +
  geom_histogram(aes(y = ..density..), position = "identity", alpha = 0.5, bins = 50) +
  scale_fill_manual(values = c("skyblue", "tomato")) +
  theme_minimal() +
  labs(title = "Distribución de R0: Prior vs Posterior", x = "R0", y = "Densidad")

median(R0_prior)
median(R0_posterior)

# --------------------------------------------------------
# Basic Posterior Summaries
# --------------------------------------------------------

posterior_mean <- mean(R0_posterior)
posterior_median <- median(R0_posterior)
posterior_var <- var(R0_posterior)
credible_interval <- quantile(R0_posterior, probs = c(0.025, 0.975))

cat("Posterior mean R0:", posterior_mean, "\n")
cat("Posterior median R0:", posterior_median, "\n")
cat("Posterior variance R0:", posterior_var, "\n")
cat("95% credible interval R0:", credible_interval, "\n\n")


##Predicciones
X <- matrix(0, length(pi_samples), total_states )
for ( k in 1:length(pi_samples)){
  mu <- data$Population*pi_samples[k]
  X[k,] <- rnbinom(total_states, mu, size_samples[k])
}


par(mfrow=c(5,6))


for (k in 1:total_states) {
  
  # Valores simulados y caso observado
  x_vals <- X[, k]
  case_val <- data$Cases[k]
  
  # Ajustar los límites del eje X para que siempre incluya data$Cases[k]
  x_min <- min(x_vals, case_val)
  x_max <- max(x_vals, case_val)
  
  hist(x_vals,
       nclass = 100,
       xlab = "Total infection",
       main = "Predictive distribution",
       sub = unique(data$State)[k],
       xlim = c(x_min, x_max)  # ← aquí aseguramos que la línea roja se vea
  )
  
  # Línea vertical con valor observado
  abline(v = case_val, lwd = 3, col = 2)
}

par(mfrow = c(1,1))

##Total infected people in Mexico

#


Y <- numeric(length(pi_samples))
for ( k in 1:length(pi_samples)){
  mu <- sum(data$Population)*pi_samples[k]
  Y[k] <- rnbinom(1, mu, size_samples[k])
}

credible_interval <- quantile(Y, probs = c(0.025, 0.975))
HDI <- hdi(Y, credMass=0.95)
hist(Y)

# Valores simulados y caso observado
y_vals <- Y
case_val <- sum(data$Cases)

# Ajustar los límites del eje X para que siempre incluya data$Cases[k]
y_min <- min(y_vals, case_val)
y_max <- max(y_vals, case_val)

hist(y_vals,
     nclass = 100,
     xlab = "Total infection",
     main = "Predictive distribution",
     xlim = c(y_min, y_max)  # ← aquí aseguramos que la línea roja se vea
)

# Línea vertical con valor observado
abline(v = case_val, lwd = 3, col = 2)

