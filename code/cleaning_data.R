#===============================================================================
#                    DATA CLEANING - SEASONAL INFLUENZA MEXICO
#===============================================================================
# Objective: Clean influenza data for data analysis
# Source: https://www.gob.mx/salud/en/documentos/datos-abiertos-152127
# Author: Imelda Trejo and  Andrei 
# Creation date: August 14, 2025
# Last update: August 14, 2025
# ===============================================================================

# Clear environment and initial setup
rm(list = ls())  # Clear memory
set.seed(1)      # Reproducibility

# ===============================================================================
#                            REQUIRED LIBRARIES
# ===============================================================================
library(readxl)      # Read Excel files
library(readr)       # Read CSV files
library(ggplot2)     # Visualizations
library(dplyr)       # Data manipulation
library(purrr)

# ===============================================================================
#                       DIRECTORY CONFIGURATION
# ===============================================================================

# Define working directory (adjust according to your system)
my_url <- "C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Research/Salud_publica/Influenza_Ejes_2025/"

web_url <- "https://raw.githubusercontent.com/imelit/Bayesian_inference_class_2025/refs/heads/main/"

# ===============================================================================
#                       Reading the data sets
# ===============================================================================

data_path1 <- paste0(web_url,"data/COVID19MEXICOAug112025.csv")

flu_data <- read_csv(data_path1,locale = locale(encoding = "UTF-8"))

data_path2 <- paste0(my_url,"data/240708 Catalogos.xlsx")

catalogo_list <- excel_sheets(data_path2) %>%
  set_names() %>%                 # set the sheet name
  map(~ read_excel(data_path2, sheet = .x))


#==== Cheeking when the data were downloaded
#
# "2025-08-05"

unique(flu_data$FECHA_ACTUALIZACION)

#===============================================================================
#drop unnecessary variables/columns 
#===============================================================================

new_data <- flu_data %>%
  select(-c("FECHA_ACTUALIZACION","MUNICIPIO_RES", "ENTIDAD_NAC", 
            "PAIS_NACIONALIDAD", "PAIS_ORIGEN"
  ))

# ===============================================================================
#                       categorical variables cleaning
# ===============================================================================


clean_data <- new_data %>%
  mutate(
    #  "ORIGEN" 
    ORIGEN = factor(ORIGEN,
             levels = as.character(catalogo_list$`Catálogo ORIGEN`$CLAVE),
             labels = catalogo_list$`Catálogo ORIGEN`$DESCRIPCIÓN,
             exclude = NULL # para que no elimine otros valores que no estén en niveles (opcional)              
    ),
    
    # SECTOR
    SECTOR = factor(SECTOR,
                    levels = as.character(catalogo_list$`Catálogo SECTOR`$CLAVE),
                    labels = catalogo_list$`Catálogo SECTOR`$DESCRIPCIÓN,
                    exclude = NULL # para que no elimine otros valores que no estén en niveles (opcional)              
                   ),
    
    #"ENTIDAD_UM" 
    ENTIDAD_UM = factor(ENTIDAD_UM,
                    levels = as.character(catalogo_list$`Catálogo de ENTIDADES`$CLAVE_ENTIDAD),
                    labels = catalogo_list$`Catálogo de ENTIDADES`$ENTIDAD_FEDERATIVA,
                    exclude = NULL # para que no elimine otros valores que no estén en niveles (opcional)              
    ),
    
    #"ENTIDAD_RES"  
    ENTIDAD_RES = factor(ENTIDAD_RES,
                        levels = as.character(catalogo_list$`Catálogo de ENTIDADES`$CLAVE_ENTIDAD),
                        labels = catalogo_list$`Catálogo de ENTIDADES`$ENTIDAD_FEDERATIVA,
                        exclude = NULL # para que no elimine otros valores que no estén en niveles (opcional)              
    ),
    

    #PAIS_NACIONALIDAD 
    NACIONALIDAD = factor(NACIONALIDAD,
                               levels = as.character(catalogo_list$`Catálogo NACIONALIDAD`$CLAVE),
                               labels = catalogo_list$`Catálogo NACIONALIDAD`$DESCRIPCIÓN,
                               exclude = NULL # para que no elimine otros valores que no estén en niveles (opcional)              
    ),
    
    
    #"TIPO_PACIENTE"
    TIPO_PACIENTE = factor(TIPO_PACIENTE,
                      levels = as.character(catalogo_list$`Catálogo TIPO_PACIENTE`$CLAVE),
                      labels = catalogo_list$`Catálogo TIPO_PACIENTE`$DESCRIPCIÓN,
    ),
    
    #"NEUMONIA"                 
    NEUMONIA = factor(NEUMONIA,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN,
                      ),
    
    #"EMBARAZO"     
    EMBARAZO = factor(EMBARAZO,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN,
    ),
    
    #"HABLA_LENGUA_INDIG"        
    HABLA_LENGUA_INDIG = factor(HABLA_LENGUA_INDIG,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN,
    ),
    
    #"INDIGENA"                 
    INDIGENA = factor(INDIGENA,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN,
    ),
    
    #"DIABETES"                  
    DIABETES  = factor(DIABETES,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    
    # EPOC
    EPOC = factor(EPOC,
                  levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                  labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # ASMA
    ASMA = factor(ASMA,
                  levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                  labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # INMUSUPR
    INMUSUPR = factor(INMUSUPR,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # HIPERTENSION
    HIPERTENSION = factor(HIPERTENSION,
                          levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                          labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # OTRA_COM
    OTRA_COM = factor(OTRA_COM,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # CARDIOVASCULAR
    CARDIOVASCULAR = factor(CARDIOVASCULAR,
                            levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                            labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # OBESIDAD
    OBESIDAD = factor(OBESIDAD,
                      levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # RENAL_CRONICA
    RENAL_CRONICA = factor(RENAL_CRONICA,
                           levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                           labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # TABAQUISMO
    TABAQUISMO = factor(TABAQUISMO,
                        levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                        labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # OTRO_CASO
    OTRO_CASO = factor(OTRO_CASO,
                       levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                       labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # TOMA_MUESTRA_LAB
    TOMA_MUESTRA_LAB = factor(TOMA_MUESTRA_LAB,
                              levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                              labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),      
  
   #TOMA_MUESTRA_ANTIGENO 
   TOMA_MUESTRA_ANTIGENO = factor(TOMA_MUESTRA_ANTIGENO,
                          levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                          labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),      

   #ICU 
   UCI= factor(UCI,
               levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
               labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),      
   
   #MIGRANTE 
   MIGRANTE = factor(MIGRANTE,
                     levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                     labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),      
   
   
   
   # RESULTADO_PCR
   RESULTADO_PCR = factor(RESULTADO_PCR,
                          levels = as.character(catalogo_list$`Catálogo RESULTADO_PCR`$CLAVE),
                          labels = catalogo_list$`Catálogo RESULTADO_PCR`$DESCRIPCIÓN,
                          exclude = NULL),
   
   # RESULTADO_PCR_COINFECCION
   RESULTADO_PCR_COINFECCION = factor(RESULTADO_PCR_COINFECCION,
                                      levels = as.character(catalogo_list$`Catálogo RESULTADO_PCR`$CLAVE),
                                      labels = catalogo_list$`Catálogo RESULTADO_PCR`$DESCRIPCIÓN,
                                      exclude = NULL),
   
   # RESULTADO_ANTIGENO
   RESULTADO_ANTIGENO = factor(RESULTADO_ANTIGENO,
                               levels = as.character(catalogo_list$`Catálogo RESULTADO_ANTIGENO`$CLAVE),
                               labels = catalogo_list$`Catálogo RESULTADO_ANTIGENO`$DESCRIPCIÓN,
                               exclude = NULL),
   
   # CLASIFICACION_FINAL_COVID
   CLASIFICACION_FINAL_COVID = factor(CLASIFICACION_FINAL_COVID,
                                      levels = as.character(catalogo_list$`Cat CLASIFICACION_FINAL_COVID`$CLAVE),
                                      labels = catalogo_list$`Cat CLASIFICACION_FINAL_COVID`$CLASIFICACIÓN,
                                      exclude = NULL),
   
   # CLASIFICACION_FINAL_FLU
   CLASIFICACION_FINAL_FLU = factor(CLASIFICACION_FINAL_FLU,
                                    levels = as.character(catalogo_list$`Cat CLASIFICACION_FINAL_FLU`$CLAVE),
                                    labels = catalogo_list$`Cat CLASIFICACION_FINAL_FLU`$CLASIFICACIÓN,
                                    exclude = NULL),
   
   # SEXO
   SEXO = factor(SEXO,
                 levels = as.character(catalogo_list$`Catálogo SEXO`$CLAVE),
                 labels = catalogo_list$`Catálogo SEXO`$DESCRIPCIÓN,
                 exclude = NULL),  
   # INTUBADO
   INTUBADO = factor(INTUBADO,
                 levels = as.character(catalogo_list$`Catálogo SI_NO`$CLAVE),
                 labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN,
                 exclude = NULL)  
   
  )

summary(clean_data)

#save( ,file = paste0(myPath1,"data/Flu_clean_data.Rdata")) #Save ALL of the model parameters


