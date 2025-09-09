library(ggplot2)

# En este laboratorio vamos a usar el ejemplo estimar el n'umero promedio de hijos 
# que tiene el grupo 1 y 2 de la ecuesta en estudio: theta1  y theta2
# para 
# (1) estimar la posterior de cada uno 
# (2) estimar la posterior de P(theta1>theta1) y el valor esperado junto con los intervaos de credibilidad


set.seed(2025) # para reproducibilidad

# 1. Datos y parámetros del prior
n1 <- 111; sum1 <- 217
n2 <- 44;  sum2 <- 66
a <- 2;    b <- 1

# 2. Parámetros de las posteriors (Poisson-Gamma conjugado)
shape1 <- a + sum1    # 219
rate1  <- b + n1      # 112 
shape2 <- a + sum2    # 68
rate2  <- b + n2      # 45



# -------------------------------------------------------------------------
# MONTE CARLO para estimar la distribuci'on posterior de P(theta2|datos_2)
# -------------------------------------------------------------------------


# Generar simulación Monte Carlo

S <- 1000    # número de simulaciones

theta2 <- rgamma(S, shape = shape2, rate = rate2)


# Graficamos el histograma de la muestra (densidad empírica)
hist(theta2, breaks = 50, probability = TRUE,
     col = "skyblue", border = "white",
     main = expression(paste("Posterior de ", theta[2])),
     xlab = expression(theta[2]))

# Agregamos curva de densidad empírica (suavizada)
lines(density(theta2), col = "blue", lwd = 2)


# Creamos una secuencia de valores en el rango de la muestra
x <- seq(min(theta2), max(theta2), length.out = 500)
# Agregamos la curva de la densidad teórica
lines(x, dgamma(x, shape = shape2, rate = rate2),
      col = "red", lwd = 2)

legend("topright", legend = c("Histograma (simulación)",
                              "Densidad empírica",
                              "Densidad teórica"),
       col = c("skyblue", "blue", "red"), lwd = c(10, 2, 2),
       bty = "n")


# -------------------------------------------------------------------------
# MONTE CARLO para estimar la distribuci'on posterior de P(theta1|datos_1)
# -------------------------------------------------------------------------










# ----------------------------
# MONTE CARLO para P(theta1 > theta2)
# ----------------------------

# 3. Simulación Monte Carlo
set.seed(2025)

S <- 200000
theta1 <- rgamma(S, shape = shape1, rate = rate1)
theta2 <- rgamma(S, shape = shape2, rate = rate2)

# 4. Estimar probabilidad
p_hat <- mean(theta1 > theta2)
se <- sqrt(p_hat * (1 - p_hat) / S)
z <- 1.96
ci <- c(p_hat - z * se, p_hat + z * se)

cat("Estimación Monte Carlo:\n")
cat(sprintf(" P(theta1 > theta2) ≈ %.6f\n", p_hat))
cat(sprintf(" MC Error Std      ≈ %.6f\n", se))
cat(sprintf(" 95%% CI           ≈ [%.6f, %.6f]\n", ci[1], ci[2]))

# 5. Graficar distribución muestral de (theta1 - theta2)

df <- data.frame(Diff = theta1 - theta2)
ggplot(df, aes(x = Diff)) +
  geom_density(fill = "steelblue", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = expression(paste("Distribución Monte Carlo de ", theta[1] - theta[2])),
    x = expression(theta[1] - theta[2]),
    y = "Densidad"
  ) +
  theme_minimal(base_size = 14)


#Notas
#1. Observemos que la distribuci'on' es simetrica
#2. Que la curva esta lejos de 0, tenemos alta certeza de que θ_1>θ_2

### Paso 2: Crear data.frame para graficar
df <- data.frame(theta1 = theta1, theta2 = theta2,
                 condicion = ifelse(theta1 > theta2, "θ1 > θ2", "θ1 ≤ θ2"))

### Paso 3: Graficar la distribución conjunta

ggplot(df, aes(x = theta1, y = theta2, color = condicion)) +
  geom_point(alpha = 0.3, size = 0.7) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(title = expression(paste("Distribución conjunta de (", theta[1], ", ", theta[2], ")")),
       x = expression(theta[1]), y = expression(theta[2]),
       color = "Condición") +
  theme_minimal() +
  scale_color_manual(values = c("θ1 > θ2" = "darkgreen", "θ1 ≤ θ2" = "gray70"))



