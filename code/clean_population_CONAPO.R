#===============================================================================
#                    Mexican Population data CLEANING 
#===============================================================================
# Objective: Clean Mexican population for the influenza data analysis
# Author: Imelda Trejo
# Creation date: August 15, 2025
# Last update: August 15, 2025
# ===============================================================================

# Clear environment and initial setup
rm(list = ls())  # Clear memory
set.seed(1)      # Reproducibility

# ===============================================================================
#                            REQUIRED LIBRARIES
# ===============================================================================
library(readxl)      # Read Excel files
library(dplyr)       # Data manipulation

#function

grouping_age<- function(age_breaks,age_labels,population){
  
  #stratify the population by groups and return their population size
  
  population_grouped <-population %>%
    group_by(EDAD)%>%
    mutate(
      AGE_GROUP = cut(
        EDAD,
        breaks = age_breaks,
        right = FALSE,
        labels = age_labels,
        include.lowest = TRUE
      )
    )%>%
    group_by(AGE_GROUP) %>%
    summarise(POPULATION = sum(POBLACION, na.rm = TRUE)) 
}


# ===============================================================================
#                       DIRECTORY CONFIGURATION
# ===============================================================================

# Define working directory (adjust according to your system)
my_url <- "C:/Users/Imelda Trejo/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/CCM_UNAM/Research/Salud_publica/Influenza_Ejes_2025/data/"

# ===============================================================================
#                       Reading the data sets
# ===============================================================================

#population 

data_path <-paste0(my_url,"0_Pob_Inicio_1950_2070 (proyeccion conapo para 2025).xlsx")

year_analysis_selected <- c("2009", "2023", "2024", "2025")  # numeric, not character

# Read and filter data
population_all <- read_excel(data_path) %>%
  filter(CVE_GEO == 0, AÑO %in% year_analysis_selected)

# Total number of years
number_of_years <- length(unique(population_all$AÑO))

# Age grouping parameters
age_group_labels <- c("0-4 years", "5-19 years", "20-64 years", "65+ years")
age_group_breaks <- c(0, 5, 20, 65, 200)

# Store results in a list
results_list <- list()

for (i in seq_len(number_of_years)) {
  
  # Filter for one year
  population_year <- population_all %>% 
    filter(AÑO == year_analysis_selected[i])
  
  # Apply your custom age grouping function
  new_pop <- grouping_age(age_group_breaks, age_group_labels, population_year)
  
  # Add the year info in new_pop
  new_pop <- new_pop %>% mutate(Year = year_analysis_selected[i])
  
  # Store in list
  results_list[[i]] <- new_pop
}

# Combine into one data frame
population <- bind_rows(results_list)

population <-as.data.frame(population)

print(population)

save(population,file =paste0(my_url,"clean_population.RData")) #Save ALL of the model parameters
