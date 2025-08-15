# Andrei/students task 

library(dplyr)
library(tidyr)
library(ggplot2)

#==========================================================================================
# Explore relationship between respiratory diseases and people at risk
# 1. Include all comorbidities, plus other variables related with risk factors
# 2. Compare counts for each risk factor
#==========================================================================================

# Clear environment and initial setup
rm(list = ls())  
set.seed(1)

# Read the data
urlfile <- "https://github.com/imelit/Bayesian_inference_class_2025/raw/main/data/respiratory_data_2025-08-05.Rdata"
load(url(urlfile))

# List of variables for people at risk
risk_vars <- c(
  "DIABETES", "EPOC", "ASMA", "INMUSUPR",
  "HIPERTENSION", "OTRA_COM", "CARDIOVASCULAR",
  "OBESIDAD", "RENAL_CRONICA",
  "INDIGENA", "EMBARAZO"
)

# Convert to long format
risk_long <- clean_data %>%
  select(all_of(risk_vars)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "RiskFactor",
    values_to = "Status"
  )

# Count frequency
risk_counts <- risk_long %>%
  group_by(RiskFactor, Status) %>%
  summarise(n = n(), .groups = "drop")

# Plot
ggplot(risk_counts, aes(x = RiskFactor, y = n, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "People at Risk in Respiratory Diseases Dataset",
       x = "Risk Factor",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
