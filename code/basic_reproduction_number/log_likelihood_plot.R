#posterior plot
# Definimos la función de log-verosimilitud para mu y sigma

rm(list = ls())

# --- Gráfica curvas de nivel ---
library(plotly)


load("MX_h1n1_2009.RData") # a data frame


loglik <- function(pi, size) {
  #size >0
  mu=data$Population*pi
  sum(lgamma(data$Cases+size)-lgamma(size)+
    data$Cases*log(mu/(mu+size))+size*log(size/(size+mu)))
}

loglik(.00001,1)
loglik(.0001,1)
loglik(.00041,1)
loglik(.00041,2)
loglik(.00041,5)


# Grid de valores
#pi_vals <- seq(0.0001, 0.003, length.out = 500)
#size_vals <- seq(0.1, 3, length.out = 500)

pi_vals <- seq(0.0001, 0.008, length.out = 500)
size_vals <- seq(0.00005,6, length.out = 500)


#z <- outer(pi_vals, size_vals, Vectorize(function(pi, size) exp(loglik(pi, size))))
z <- outer(pi_vals, size_vals, Vectorize(function(pi, size) loglik(pi, size)))


plot_ly(x = pi_vals, y = size_vals, z = z) %>%
  add_surface() %>%
  layout(
    title = "Likelihood Superface",
    scene = list(
      xaxis = list(title = "pi"),
      yaxis = list(title = "r"),
      zaxis = list(title = "L(pi,r)")
    )
  )

# --- Contour plot ---
contour(pi_vals, size_vals, z,
        xlab = expression(pi),
        ylab = expression(size),
        main = "Contour plot Likelihood")
#points(mean(y), 1, col = "red", pch = 19) # estimador MLE

max(z)

