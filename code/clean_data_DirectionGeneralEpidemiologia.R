#===============================================================================
#                    DATA CLEANING - RESPIRATORY INFECTIOUS DISEASES (MEXICO)
#===============================================================================
# Objective   : Clean RESPIRATORY dataset for analysis 
# Data Source : https://www.gob.mx/salud/en/documentos/datos-abiertos-152127
# Author      : Imelda Trejo
# Creation    : August 14, 2025
# Last update : August 20, 2025
#===============================================================================

#------------------------------------------------------------------------------
# Clear environment and initial setup
#------------------------------------------------------------------------------
rm(list = ls())    # Clear memory
set.seed(1)        # Reproducibility

#------------------------------------------------------------------------------
# Required libraries
#------------------------------------------------------------------------------
library(readxl)    # Read Excel files
library(readr)     # Read CSV files
library(ggplot2)   # Visualizations
library(dplyr)     # Data manipulation
library(purrr)     # Functional programming

#------------------------------------------------------------------------------
# Directory configuration
#------------------------------------------------------------------------------
# Adjust paths according to your local system
local_path <- "C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Research/Salud_publica/Influenza_Ejes_2025/"
web_path   <- "https://raw.githubusercontent.com/imelit/Bayesian_inference_class_2025/refs/heads/main/"

#------------------------------------------------------------------------------
# Read datasets
#------------------------------------------------------------------------------
# Main data (national influenza/COVID dataset)
data_path1 <- paste0(web_path, "data/COVID19MEXICOAug112025.csv")
flu_data   <- read_csv(data_path1, locale = locale(encoding = "UTF-8"))

# Catalog files (metadata dictionaries)
data_path2    <- paste0(local_path, "data/240708 Catalogos.xlsx")
catalogo_list <- excel_sheets(data_path2) %>%
  set_names() %>%
  map(~ read_excel(data_path2, sheet = .x))

#------------------------------------------------------------------------------
# Dataset metadata
#------------------------------------------------------------------------------
date_analysis <- unique(flu_data$FECHA_ACTUALIZACION)
year_analysis <- 2025

#------------------------------------------------------------------------------
# Drop unnecessary variables
#------------------------------------------------------------------------------
new_data <- flu_data %>%
  select(-c("FECHA_ACTUALIZACION", "MUNICIPIO_RES", 
            "ENTIDAD_NAC", "PAIS_NACIONALIDAD", "PAIS_ORIGEN"))

#------------------------------------------------------------------------------
# Clean categorical variables using catalog reference tables
#------------------------------------------------------------------------------
clean_data <- new_data %>%
  mutate(
    # Origin of case report
    ORIGEN = factor(ORIGEN,
                    levels = as.character(catalogo_list$`Catálogo ORIGEN`$CLAVE),
                    labels = catalogo_list$`Catálogo ORIGEN`$DESCRIPCIÓN,
                    exclude = NULL),
    
    # Health sector
    SECTOR = factor(SECTOR,
                    levels = as.character(catalogo_list$`Catálogo SECTOR`$CLAVE),
                    labels = catalogo_list$`Catálogo SECTOR`$DESCRIPCIÓN,
                    exclude = NULL),
    
    # State of medical unit
    ENTIDAD_UM = factor(ENTIDAD_UM,
                        levels = as.character(catalogo_list$`Catálogo de ENTIDADES`$CLAVE_ENTIDAD),
                        labels = catalogo_list$`Catálogo de ENTIDADES`$ENTIDAD_FEDERATIVA,
                        exclude = NULL),
    
    # State of residence
    ENTIDAD_RES = factor(ENTIDAD_RES,
                         levels = as.character(catalogo_list$`Catálogo de ENTIDADES`$CLAVE_ENTIDAD),
                         labels = catalogo_list$`Catálogo de ENTIDADES`$ENTIDAD_FEDERATIVA,
                         exclude = NULL),
    
    # Nationality
    NACIONALIDAD = factor(NACIONALIDAD,
                          levels = as.character(catalogo_list$`Catálogo NACIONALIDAD`$CLAVE),
                          labels = catalogo_list$`Catálogo NACIONALIDAD`$DESCRIPCIÓN,
                          exclude = NULL),
    
    # Patient type
    TIPO_PACIENTE = factor(TIPO_PACIENTE,
                           levels = as.character(catalogo_list$`Catálogo TIPO_PACIENTE`$CLAVE),
                           labels = catalogo_list$`Catálogo TIPO_PACIENTE`$DESCRIPCIÓN),
    
    # Health conditions and people in risk (Yes/No)
    NEUMONIA     = factor(NEUMONIA,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    EMBARAZO     = factor(EMBARAZO,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    HABLA_LENGUA_INDIG = factor(HABLA_LENGUA_INDIG, levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    INDIGENA     = factor(INDIGENA,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    DIABETES     = factor(DIABETES,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    EPOC         = factor(EPOC,         levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    ASMA         = factor(ASMA,         levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    INMUSUPR     = factor(INMUSUPR,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    HIPERTENSION = factor(HIPERTENSION, levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    OTRA_COM     = factor(OTRA_COM,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    CARDIOVASCULAR = factor(CARDIOVASCULAR, levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    OBESIDAD     = factor(OBESIDAD,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    RENAL_CRONICA = factor(RENAL_CRONICA, levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    TABAQUISMO   = factor(TABAQUISMO,   levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    OTRO_CASO    = factor(OTRO_CASO,    levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    TOMA_MUESTRA_LAB = factor(TOMA_MUESTRA_LAB, levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    TOMA_MUESTRA_ANTIGENO = factor(TOMA_MUESTRA_ANTIGENO, levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    UCI          = factor(UCI,          levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    MIGRANTE     = factor(MIGRANTE,     levels = catalogo_list$`Catálogo SI_NO`$CLAVE, labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN),
    
    # Test results
    RESULTADO_PCR   = factor(RESULTADO_PCR, levels = catalogo_list$`Catálogo RESULTADO_PCR`$CLAVE, labels = catalogo_list$`Catálogo RESULTADO_PCR`$DESCRIPCIÓN, exclude = NULL),
    RESULTADO_PCR_COINFECCION = factor(RESULTADO_PCR_COINFECCION, levels = catalogo_list$`Catálogo RESULTADO_PCR`$CLAVE, labels = catalogo_list$`Catálogo RESULTADO_PCR`$DESCRIPCIÓN, exclude = NULL),
    RESULTADO_ANTIGENO = factor(RESULTADO_ANTIGENO, levels = catalogo_list$`Catálogo RESULTADO_ANTIGENO`$CLAVE, labels = catalogo_list$`Catálogo RESULTADO_ANTIGENO`$DESCRIPCIÓN, exclude = NULL),
    
    # Case classification
    CLASIFICACION_FINAL_COVID = factor(CLASIFICACION_FINAL_COVID,
                                       levels = catalogo_list$`Cat CLASIFICACION_FINAL_COVID`$CLAVE,
                                       labels = catalogo_list$`Cat CLASIFICACION_FINAL_COVID`$CLASIFICACIÓN,
                                       exclude = NULL),
    CLASIFICACION_FINAL_FLU = factor(CLASIFICACION_FINAL_FLU,
                                     levels = catalogo_list$`Cat CLASIFICACION_FINAL_FLU`$CLAVE,
                                     labels = catalogo_list$`Cat CLASIFICACION_FINAL_FLU`$CLASIFICACIÓN,
                                     exclude = NULL),
    
    # Demographics
    SEXO = factor(SEXO,
                  levels = catalogo_list$`Catálogo SEXO`$CLAVE,
                  labels = catalogo_list$`Catálogo SEXO`$DESCRIPCIÓN,
                  exclude = NULL),
    # disease severity
    INTUBADO = factor(INTUBADO,
                      levels = catalogo_list$`Catálogo SI_NO`$CLAVE,
                      labels = catalogo_list$`Catálogo SI_NO`$DESCRIPCIÓN,
                      exclude = NULL)
  )

#------------------------------------------------------------------------------
# Derived variables
#------------------------------------------------------------------------------
clean_data <- clean_data %>%
  mutate(
    # Death status
    DEFUNCION = if_else(is.na(FECHA_DEF), 0, 1),
    DEFUNCION = factor(DEFUNCION, levels = c(0, 1), labels = c("NO", "YES")),
    
    # Age groups
    AGE_GROUP = case_when(
      EDAD < 5           ~ "0-4 years",
      EDAD >= 5 & EDAD < 20  ~ "5-19 years",
      EDAD >= 20 & EDAD < 65 ~ "20-64 years",
      EDAD >= 65         ~ "65+ years"
    ),
    AGE_GROUP = factor(AGE_GROUP, levels = c("0-4 years", "5-19 years", "20-64 years", "65+ years"))
  )

#------------------------------------------------------------------------------
# Save cleaned dataset
#------------------------------------------------------------------------------
data_resp <- as.data.frame(clean_data)
data_resp$YEAR <- year_analysis

summary(data_resp)

save_path <- paste0(local_path, "data/clean_respiratory_diseases_", date_analysis, ".Rdata")
save(data_resp, file = save_path)

#------------------------------------------------------------------------------
# Influenza subset
#------------------------------------------------------------------------------
data_flu <- data_resp %>%
  filter(CLASIFICACION_FINAL_FLU == "CASO DE INFLUENZA CONFIRMADO")

save_path <- paste0(local_path, "data/clean_flu_", date_analysis, ".Rdata")
save(data_flu, file = save_path)

