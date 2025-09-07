# LAb 3: usando datos 
# (1) vamos a estimar el parámetro theta del Modelo de Poission usando dos muestras
# (2) comparar las graficas de las dos distribuciones posteriores
# Nota: theta es una tasa, numero de hijos por mujer, su dominio es (0,infinito)

library(ggplot2)

#Datos:

n1<- 111
n2<- 44

sum1 <- 217
sum2 <- 66

#Parametros de la distribuci'on prior 

a <- 2
b <-1

# prior densidad gamma

parameter_space <- seq(0,5,0.01) #valores del dominio de la distribución gamma

prior <- dgamma(parameter_space, shape = a , rate=b )

plot(parameter_space,prior)

#posterior1

shape1 <- a+sum1
rate1 <- (n1+b)

posterior1 <- dgamma(parameter_space,shape = shape1, rate =rate1)
plot(parameter_space,posterior1)

#posterior 2

shape2 <- a+sum2
rate2 <- (n2+b)

posterior2 <- dgamma(parameter_space,shape = shape2, rate =rate2)
plot(parameter_space,posterior2)

#combining all posteiror in one data frame

df <- data.frame(Parameter_values=parameter_space,Prior=prior,Posterior1=posterior1,Posterior2=posterior2)

# --- Graficar sin pivot_longer ---
ggplot(df, aes(x = Parameter_values)) +
  geom_line(aes(y = Prior, color = "Prior"), size = 1) +
  geom_line(aes(y = Posterior1, color = "Posterior1"), size = 1) +
  geom_line(aes(y = Posterior2, color = "Posterior2"), size = 1) +
  labs(
    title = "Prior y Posteriores",
    x = "Parameter values: media de natalidad",
    y = "Density",
    color = "Distribución"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

#expected mean

print(c("valor esperado del número de hijos para el grupo 1 ",shape1/rate1))

print(c("valor esperado del número de hijos para el grupo 2 ",shape2/rate2))


print("intervalo de credibilidad para el parametro $\theta_1$")
qgamma(c(0.05 ,.95 ), shape=a+sum2 , rate=b+n2 )

print("intervalo de credibilidad para el parametro $\theta_2$")
qgamma(c(0.05 ,.95 ), shape=a+sum2 , rate=b+n2 )

