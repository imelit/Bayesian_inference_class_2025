# Ejemplo 2D: Normal con σ conocida

set.seed(123)
n <- 20
theta_true <- 3
sigma <- 1
y <- rnorm(n, mean = theta_true, sd = sigma)

# Log-verosimilitud como función de mu
loglik_mu <- function(mu) {
  sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
}

# Valores de mu
mu_vals <- seq(0, 6, length.out = 100)
loglik_vals <- sapply(mu_vals, loglik_mu)

# Graficar
plot(mu_vals, loglik_vals, type="l",
     main="Log-verosimilitud vs μ (σ conocida)",
     xlab=expression(mu), ylab="Log-verosimilitud")
abline(v = mean(y), col="red", lty=2) # MLE
legend("topright", legend=c("MLE"), col="red", lty=2)


########################
# cuando ambos parametros son desconocidos


# Ejemplo: Verosimilitud para una Normal con varianza conocida

# Simulamos datos
set.seed(123)
n <- 20
theta_true <- 3
y <- rnorm(n, mean = theta_true, sd = 1)

# Definimos la función de log-verosimilitud para mu y sigma
loglik <- function(mu, sigma) {
  sum(dnorm(y, mean = mu, sd = sigma, log = TRUE))
}

# Grid de valores
mu_vals <- seq(0, 6, length.out = 100)
sigma_vals <- seq(0.5, 2, length.out = 100)

z <- outer(mu_vals, sigma_vals, Vectorize(function(mu, sigma) exp(loglik(mu, sigma))))


# Gráfico 3D
persp(mu_vals, sigma_vals, z,
      theta = 40, phi = 25,
      xlab = "Media (mu)", ylab = "Desviación (sigma)",
      zlab = "Log-Verosimilitud", col = "lightblue")

# --- Gráfica curvas de nivel ---
library(plotly)

plot_ly(x = mu_vals, y = sigma_vals, z = z) %>%
  add_surface() %>%
  layout(
    title = "Superficie de Verosimilitud",
    scene = list(
      xaxis = list(title = "μ"),
      yaxis = list(title = "σ"),
      zaxis = list(title = "L(μ,σ)")
    )
  )

# --- Contour plot ---
contour(mu_vals, sigma_vals, z,
        xlab = expression(mu),
        ylab = expression(sigma),
        main = "Contour plot de la función de verosimilitud")
points(mean(y), 1, col = "red", pch = 19) # estimador MLE

