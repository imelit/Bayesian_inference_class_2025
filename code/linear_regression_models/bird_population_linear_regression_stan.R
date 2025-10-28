########################################################################
# Bayesian Analysis: Calidris canutus abundance over time in France
# Author: Imelda Trejo Lorenzo with the help of Ben
# Objective:
#   Is the Calidris canutus (red_knot birds) pupulation increasing over time?
#
# This is an example proposed in the tutorial introduction to the R package brms:
# https://ourcodingclub.github.io/tutorials/brms/
#  
#
####################################

rm(list = ls())


# --- Libraries ---
library(dplyr)
library(ggplot2)
library(rstanarm)   # for Bayesian regression (Stan backend)
library(bayesplot)  # for visualization of posterior distributions
library(posterior)  # for summarizing posteriors
library(readr)      # read an csv file

France <- read_csv("red_knot.csv")

head(France)  # to get the first observations in each column
str(France)  # what type of variables do we have, table
             # pop and year variables of interest   

#Data distribution
(hist_france <- ggplot(France, aes(x = pop)) +
    geom_histogram(colour = "#8B5A00", fill = "#CD8500", bins = 10) +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nCalidris canutus abundance") +  # latin name for red knot
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))   

unique(France$year) #1976-2010

# --- Exploratory Plot ---
ggplot(France, aes(x = year, y = pop)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  labs(
    title = "Annual Average World Surface Temperature",
    y = "Population",
    x = "Year"
  ) +
  theme_minimal(base_size = 14)


# --- Bayesian Linear Model ---
#y = pop, x=year
#pop ~ year
#Model pop=b0+b1*year 
#Poission model with mean population rate: pop=b0+b1*year 

fit_bayes <- stan_glm(
  pop ~ year,
  data = France,
  family = poisson(),
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
  message("→ Strong Bayesian evidence that bird population is increasing over time.")
} else {
  message("→ No strong Bayesian evidence for an increasing bird population trend.")
}


# --- Posterior Predictive Check ---
pp_check(fit_bayes, plotfun = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Model Fit")


##### improve or estimates
unique(France$Location.of.population)  # observations come from 2 locations
(boxplot_location <- ggplot(France, aes(Location.of.population, pop)) +
    geom_boxplot() +  # could be a significant effect between locations so should look at that
    theme_bw() +
    xlab("Location\n") +
    ylab("\nCalidris canutus abundance") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))

fit_bayes <- stan_glm(
  pop ~ year+ Location.of.population,
  data = France,
  family = poisson(),
  prior = normal(0, 10, autoscale = TRUE),
  prior_intercept = normal(0, 10, autoscale = TRUE),
  prior_aux = exponential(1),
  chains = 4, iter = 4000, warmup = 1000, seed = 42
)

# --- Posterior Predictive Check ---
pp_check(fit_bayes, plotfun = "dens_overlay") +
  ggtitle("Posterior Predictive Check: Model Fit")


