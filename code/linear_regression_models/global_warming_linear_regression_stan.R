##############################################
# Bayesian Analysis: Global Warming Trend
# Author: Imelda Trejo Lorenzo
# Objective:
#   Is the annual average world surface temperature increasing over time?
##############################################
rm(list = ls())

# --- Libraries ---
library(dplyr)
library(ggplot2)
library(rstanarm)   # for Bayesian regression (Stan backend)
library(bayesplot)  # for visualization of posterior distributions
library(posterior)  # for summarizing posteriors

# --- Data Acquisition ---
url <- "https://raw.githubusercontent.com/michael-franke/intro-data-analysis/master/data_sets/average-world-temperature.csv"
data_temperature <- read.csv(url)

# --- Data Inspection ---
head(data_temperature)
summary(data_temperature)
str(data_temperature)

# --- Exploratory Plot ---
ggplot(data_temperature, aes(x = year, y = avg_temp)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Annual Average World Surface Temperature",
    y = "Temperature (°C)",
    x = "Year"
  ) +
  theme_minimal(base_size = 14)

# --- Bayesian Linear Model ---
# Model: avg_temp ~ year, assuming normal residuals
set.seed(1234)
fit_bayes <- stan_glm(
  avg_temp ~ year,
  data = data_temperature,
  family = gaussian(),
  prior = normal(0, 10, autoscale = TRUE),
  prior_intercept = normal(0, 10, autoscale = TRUE),
  prior_aux = exponential(1),
  chains = 4, iter = 4000, warmup = 1000, seed = 42
)

# --- Model Summary ---
print(fit_bayes, digits = 3)
#posterior_summary(fit_bayes)

# --- Check convergence diagnostics ---
mcmc_trace(as.matrix(fit_bayes), pars = c("(Intercept)", "year"))

# --- Posterior Visualization ---
mcmc_areas(as.matrix(fit_bayes),
           pars = c("(Intercept)"),
           prob = 0.95) +
  ggtitle("Posterior Distributions of Model Parameters")

mcmc_areas(as.matrix(fit_bayes),
           pars = c("year"),
           prob = 0.95) +
  ggtitle("Posterior Distributions of Model Parameters")


# --- Interpretation ---
posterior_draws <- as_draws_df(fit_bayes)
mean_beta1 <- mean(posterior_draws$year)
prob_beta1_positive <- mean(posterior_draws$year > 0)

cat("\nPosterior mean of β1 (slope):", round(mean_beta1, 5))
cat("\nProbability that β1 > 0:", round(prob_beta1_positive, 4), "\n")

if (prob_beta1_positive > 0.95) {
  message("→ Strong Bayesian evidence that temperature is increasing over time.")
} else {
  message("→ No strong Bayesian evidence for an increasing temperature trend.")
}

# --- Posterior Predictive Check ---
pp_check(fit_bayes, plotfun = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Model Fit")

##############################################
# End of script
##############################################
