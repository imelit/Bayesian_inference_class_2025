#===============================================================================
#                    ANÁLISIS EXPLORATORIO - INFLUENZA ESTACIONAL MÉXICO
#===============================================================================
# Objetivo: Explorar datos de influenza para el proyecto de estadística descriptiva y analisis de datos
# Fuente: https://www.gob.mx/salud/en/documentos/datos-abiertos-152127
# Autor: [Tu nombre]
# Fecha de creación: Agosto 11, 2025
# Ultma actualizacón: Agosto 11, 2025
# ===============================================================================

# Limpiar entorno y configuración inicial
rm(list = ls())  # Limpiar memoria
set.seed(1)      # Reproducibilidad

# ===============================================================================
#                            LIBRERÍAS NECESARIAS
# ===============================================================================
#library(readxl)      # Leer archivos Excel
library(readr)       # Leer archivos CSV
library(ggplot2)     # Visualizaciones
#library(viridis)     # Paletas de colores
#library(tidyverse)   # Manipulación de datos
library(dplyr)       # Manipulación de datos
#library(lubridate)   # Manejo de fechas
#library(kableExtra)  # Tablas elegantes
#library(plotly)      # Gráficas interactivas
#library(corrplot)    # Matriz de correlación

# ===============================================================================
#                       CONFIGURACIÓN DE DIRECTORIO
# ===============================================================================
# Definir ruta de trabajo (ajustar según tu sistema)

influenza_data <- read_csv("https://raw.githubusercontent.com/imelit/Bayesian_inference_class_2025/refs/heads/main/data/COVID19MEXICOAug112025.csv",
  locale = locale(encoding = "UTF-8")
)


# Summary of your data 

summary(influenza_data)

#How many variables have NA's? 
#It is imporat to know the leading causes of NA's and their meanig. For missig data you an use MICE packages
#

# What is the percentage

sum(is.na(influenza_data$FECHA_DEF)) #total
1-sum(is.na(influenza_data$FECHA_DEF))/dim(influenza_data[1])[1] #3% de defunciones

#data type look at the environment
#work with variable SEXO



class(influenza_data$ENTIDAD_UM)

#Look at the catalogo in the repo
#https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fraw.githubusercontent.com%2Fimelit%2FBayesian_inference_class_2025%2Frefs%2Fheads%2Fmain%2Fdata%2F240708%2520Catalogos.xlsx&wdOrigin=BROWSELINK

#is this numeric or categorical

length(unique(influenza_data$ENTIDAD_UM)) #ctrl+enter

#redefinir la variable ENTIDAD_UM como variable categórica con sus respectivos niveles

influenza_data$ENTIDAD_UM <- as.factor(influenza_data$ENTIDAD_UM)

summary(influenza_data)

#Lista de variables categoricas formater a tipo facor


## Resultado del laboratorio

unique(influenza_data$TIPO_PACIENTE)

unique(influenza_data$RESULTADO_PCR)


#=====================================
# cleaning the data
#=======================================

#1. Change binary levels from 1 and 2 to  0 and 1

#redefinir varibles categoricas con sus nuevos niveles "etiquetas"

# usando chat gtp

# Vector con las claves en el orden deseado
niveles <- c(
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "13", "14", "17",
  "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "32", "33",
  "34", "35", "36", "37", "41", "997", "998", "999"
)

# Vector con las etiquetas descriptivas en el mismo orden
etiquetas <- c(
  "INFLUENZA AH1N1 PMD",
  "INFLUENZA A H1",
  "INFLUENZA A H3",
  "INFLUENZA B",
  "NEGATIVO",
  "MUESTRA NO ADECUADA",
  "ADENOVIRUS",
  "PARAINFLUENZA 1",
  "PARAINFLUENZA 2",
  "PARAINFLUENZA 3",
  "VIRUS SINCICIAL RESPIRATORIO",
  "INFLUENZA A NO SUBTIPIFICADA",
  "INFLUENZA A H5",
  "MUESTRA RECHAZADA",
  "VIRUS SINCICIAL RESPIRATORIO A",
  "VIRUS SINCICIAL RESPIRATORIO B",
  "CORONA 229E",
  "CORONA OC43",
  "CORONA SARS",
  "CORONA NL63",
  "CORONA HKU1",
  "MUESTRA QUE NO AMPLIFICO",
  "ENTEROV//RHINOVIRUS",
  "METAPNEUMOVIRUS",
  "MUESTRA SIN AISLAMIENTO",
  "PARAINFLUENZA 4",
  "MUESTRA SIN CELULAS",
  "SARS-CoV-2",
  "MERS-CoV",
  "SARS-CoV",
  "BOCAVIRUS",
  "MUESTRA NO RECIBIDA",
  "NO APLICA (CASO SIN MUESTRA)",
  "SIN COINFECCIÓN",
  "PENDIENTE"
)

# Convertir RESULTADO_pcr en factor con esos niveles y etiquetas

influenza_data$RESULTADO_PCR <- factor(
  as.character(influenza_data$RESULTADO_PCR),  # por si es numérica, convertir a caracter primero
  levels = niveles,
  labels = etiquetas,
  exclude = NULL # para que no elimine otros valores que no estén en niveles (opcional)
)

#========================================================
# Exploración y análisis de la variable RESULTADO_PCR
#============================================================

#SUMARRY
summary(influenza_data$RESULTADO_PCR)

#TABLA DE PROPORCIONES
tabla <- table(influenza_data$RESULTADO_PCR)
prop <- prop.table(tabla)

# reporte con la tabla de frecuencias

infecciones_respiratorias_frecuencia <- data.frame(Categoria = names(tabla), Frecuencia = as.vector(tabla), Proporcion = round(as.vector(prop), 3))

sum(infecciones_respiratorias_frecuencia$Frecuencia) #si es el total de datos reportados

#GRAFICA DE BARRAS

barplot(tabla, las=2, col="steelblue", main="Frecuencia de Resultados PCR", ylab="Cantidad")

ggplot(influenza_data, aes(x = RESULTADO_PCR)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotar 45 grados
  labs(title = "Frecuencia de Resultados PCR", x = "Resultado PCR", y = "Cantidad")


#describir los covariables


# Mostrar ambas
data.frame(Categoria = names(tabla), Frecuencia = as.vector(tabla), Proporcion = round(as.vector(prop), 3))



#SEXO

#pre-cleanig all data

#----
#Variables binarias codificadas como 0/1: Muy útil para regresión logística, tablas de contingencia, y otros análisis.
#----

# comorbidades
#todos los datos provienen de USMER #ORIGEN=factor(ORIGEN)=1 USMER
# "TOMA_MUESTRA_ANTIGENO" solo para sars-cov-2
# dataNew$RESULTADO_ANTIGENO no plica

dataSubset <- influenza_data %>% select("ID_REGISTRO", "SECTOR", "SEXO", "ENTIDAD_RES", "TIPO_PACIENTE", "FECHA_INGRESO", "FECHA_DEF", "INTUBADO",                
         "NEUMONIA", "EDAD", "EMBARAZO", "HABLA_LENGUA_INDIG" ,   "INDIGENA",
         "DIABETES", "EPOC", "ASMA", "INMUSUPR", "HIPERTENSION", "OTRA_COM" , "CARDIOVASCULAR", "OBESIDAD",
         "RENAL_CRONICA", "TABAQUISMO", "RESULTADO_PCR", "RESULTADO_PCR_COINFECCION",
          "CLASIFICACION_FINAL_FLU", "UCI")  

dataNew <-  dataSubset  %>% mutate(SEXO = case_when(
  SEXO == 1 ~ 1, #yes
  SEXO == 2 ~ 0), #no
  NEUMONIA = case_when(
  NEUMONIA == 1 ~ 1, #yes
  NEUMONIA == 2 ~ 0), #no
  TIPO_PACIENTE = case_when(
    TIPO_PACIENTE == 1 ~ 1, #yes
    TIPO_PACIENTE == 2 ~ 0), #no
  SEXO=factor(SEXO),
  NEUMONIA = factor(NEUMONIA),
  TIPO_PACIENTE = factor(TIPO_PACIENTE),
  CLASIFICACION_FINAL_FLU = factor(CLASIFICACION_FINAL_FLU),
  SECTOR = factor(SECTOR),
  ENTIDAD_RES = factor(ENTIDAD_RES),
  INDIGENA = factor(INDIGENA),
  DIABETES =factor(DIABETES),
  EPOC = factor(EPOC),         #Enfermedad Pulmonar Obstructiva Crónica.
  ASMA = factor(ASMA),         
  INMUSUPR =factor(INMUSUPR),
  HIPERTENSION  =factor(HIPERTENSION),
  OTRA_COM =factor(OTRA_COM),
  CARDIOVASCULAR =factor(CARDIOVASCULAR),
  OBESIDAD =factor(OBESIDAD),                 
  RENAL_CRONICA =factor(RENAL_CRONICA),
  TABAQUISMO=factor(TABAQUISMO),
  UCI=factor(UCI),
  INTUBADO=factor(INTUBADO),
  EMBARAZO=factor(EMBARAZO),
  HABLA_LENGUA_INDIG=factor(HABLA_LENGUA_INDIG),
  RESULTADO_PCR=factor(RESULTADO_PCR),
  RESULTADO_PCR_COINFECCION=factor(RESULTADO_PCR_COINFECCION)
  )


ggplot(dataNew, aes(x = CLASIFICACION_FINAL_FLU)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotar 45 grados
  labs(title = "Frecuencia de Resultados casos FLU", x = "Resultado", y = "Cantidad")


#3	CASO DE INFLUENZA CONFIRMADO
#4	INVÁLIDO POR LABORATORIO
#5	NO REALIZADO POR LABORATORIO
#6	CASO SOSPECHOSO
#7	NEGATIVO A INFLUENZA

dataNew$CLASIFICACION_FINAL_FLU <- with(dataNew,
  factor(CLASIFICACION_FINAL_FLU,
    levels = c('3', '4', '5', '6', '7'),
    labels = c("INFLUENZA CONFIRMADO", "INVÁLIDADO POR LAB","NO REALIZADO POR EL LAB",
      "SOSPECHOSO", "NEGATIVOS")
  )
)
                                     
                                     
ggplot(dataNew, aes(x = CLASIFICACION_FINAL_FLU)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotar 45 grados
  labs(title = "Frecuencia de casos de Influenza", x = "Resultado", y = "Cantidad")



tabla_cruzada <- dataNew %>%
  tabyl(CLASIFICACION_FINAL_FLU, RESULTADO_PCR)

clasificacion_y_resultado_laboratorio<-as.data.frame(tabla_cruzada)

#============================================
#
# Analizar la variabe SECTOR
#
#==========================================================




#ordinal categorical
#dataNew$educ <- with(dataNew, factor(educacion, 
                                  #   levels = c('2','3','4'), 
                                  #   labels = c("HS/GED","some college","college+"),
                                  #   ordered = TRUE))



####=========================================================================================
#Colcusión esta base de datos nos proporciona información de los usuarios que asisten SISVER
#que tipo de población es, cuales son sus caracteristicas?
#Es esta base de datos representativa a nivel nacional?
# que diferencias hay entre la poblacion diagnosticados positivo vs los que no
#==================================================================================================


Flu_clean_data <- dataNew %>% filter(CLASIFICACION_FINAL_FLU=="INFLUENZA CONFIRMADO")

#Guardar base limpia, tamben podriamos anexar una nueva variable binaria "defuncion si/no"

#save(dataFlu,file = paste0(myPath1,"data/Flu_clean_data.Rdata")) #Save ALL of the model parameters

ggplot(Flu_clean_data, aes(x = RESULTADO_PCR)) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # rotar 45 grados
  labs(title = "Frecuencia de casos de Influenza", x = "Resultado", y = "Cantidad")

#Siguente contar los casosconfirmados por dia para infleunza, hacer series de tiempo.


