library(HDInterval)
library(ggplot2)

set.seed(123)

# Datos Poisson con pocos eventos
n <- 5
lambda_true <- 1.2
y <- rpois(n, lambda_true)

# Prior débilmente informativo Gamma(a,b)
a <- 1; b <- 1
post_a <- a + sum(y)
post_b <- b + n

# Posterior Gamma

post_samples <- rgamma(1e5, shape=post_a, rate=post_b)

# CrI (cuantiles)
CrI <- quantile(post_samples, c(0.025, 0.975))

# HDI
HDI <- hdi(post_samples, credMass=0.95)

# Plot

df <- data.frame(x=post_samples)
ggplot(df, aes(x)) +
  geom_density(fill="lightblue", alpha=0.4) +
  geom_vline(xintercept=CrI, color="red", linetype="dashed", size=1, alpha=0.8) +
  geom_vline(xintercept=HDI, color="darkgreen", linetype="solid", size=1) +
  annotate("text", x=CrI[1], y=0, label="CrI lower", vjust=-1, hjust=0, color="red") +
  annotate("text", x=CrI[2], y=0, label="CrI upper", vjust=-1, hjust=1, color="red") +
  annotate("text", x=HDI[1], y=0.1, label="HDI lower", vjust=-1, hjust=0, color="darkgreen") +
  annotate("text", x=HDI[2], y=0.1, label="HDI upper", vjust=-1, hjust=1, color="darkgreen") +
  labs(title="Asymmetric Posterior: CrI vs HDI",
       subtitle=paste("n =",n,", sum(y) =",sum(y)),
       x=expression(lambda), y="Density")

#######################################################
# Una función posderior bimodal simulado 
# El hdi no funciona para este caso.


# Posterior bimodal simulada como mezcla de dos normales para mayor claridad visual
n_samples <- 1e5
w <- 0.4  # peso del primer pico
post_samples <- c(
  rnorm(n_samples * w, mean=1, sd=0.3),   # primer pico
  rnorm(n_samples * (1 - w), mean=4, sd=0.3) # segundo pico
)

# CrI (cuantil)
CrI <- quantile(post_samples, c(0.025, 0.975))

# HDI (puede ser disjunto)

HDI <- hdi(post_samples, credMass=0.95)

# Plot
library(ggplot2)
df <- data.frame(x=post_samples)
ggplot(df, aes(x)) +
  geom_density(fill="lightblue", alpha=0.4, adjust=1) +
  geom_vline(xintercept=CrI, color="red", linetype="dashed", size=1, alpha=0.8) +
  geom_vline(xintercept=HDI, color="darkgreen", linetype="solid", size=1) +
  annotate("text", x=CrI[1], y=0, label="CrI lower", vjust=-1, hjust=0, color="red") +
  annotate("text", x=CrI[2], y=0, label="CrI upper", vjust=-1, hjust=1, color="red") +
  labs(title="Bimodal Posterior: CrI vs HDI",
       subtitle="HDI covers the highest-density regions (may be disjoint)",
       x=expression(theta), y="Density")


##----El ejemplo anterior no funcionó---##
# Abajo hay un código que si muestra el HDI

# Generar posterior bimodal
n_samples <- 1e5
w <- 0.4
post_samples <- c(
  rnorm(n_samples * w, mean=1, sd=0.3),
  rnorm(n_samples * (1 - w), mean=4, sd=0.3)
)

#  Calcular densidad kernel
dens <- density(post_samples, n=2^12)
x <- dens$x
y <- dens$y

#  Determinar el umbral de densidad que define el HDI

credMass <- 0.95
sorted_y <- sort(y, decreasing = TRUE)
cumulative_prob <- cumsum(sorted_y) / sum(sorted_y)
threshold_index <- which(cumulative_prob >= credMass)[1]
c <- sorted_y[threshold_index]

#  Identificar regiones de HDI
in_HDI <- y >= c
# Convertir a intervalos disjuntos
HDI_intervals <- data.frame()
start <- NULL
for(i in 1:length(in_HDI)){
  if(in_HDI[i] & is.null(start)) start <- x[i]
  if((!in_HDI[i] | i==length(in_HDI)) & !is.null(start)){
    end <- x[i-1]
    HDI_intervals <- rbind(HDI_intervals, data.frame(lower=start, upper=end))
    start <- NULL
  }
}

HDI_intervals

# Extraer muestras que caen dentro de cada intervalo
HDI_samples <- unlist(lapply(1:nrow(HDI_intervals), function(i){
  post_samples[post_samples >= HDI_intervals$lower[i] &
                 post_samples <= HDI_intervals$upper[i]]
}))

# Calcular CrI para comparación
CrI <- quantile(post_samples, c(0.025, 0.975))

# Graficar posterior con HDI sombreado

ggplot() +
  geom_histogram(aes(x=post_samples, y=..density..), bins=100,
                 color="black", fill="grey80") +
  geom_histogram(aes(x=HDI_samples, y=..density..), bins=100,
                 fill="lightblue", alpha=0.7) +
  geom_vline(xintercept=CrI, color="red", linetype="dashed", size=1) +
  labs(title="Posterior Bimodal: HDI real vs CrI",
       subtitle="HDI sombreado en azul (intervalos disjuntos)\nCrI en líneas rojas",
       x=expression(theta), y="Density") +
  theme_minimal()



