

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



# Function to group ages
group_age <- function(df, age_var, set = c("CDC", "Custom")) {
  
  set <- match.arg(set)  # Force user to choose one of the allowed options
  
  case_expr <- if (set == "CDC") {
    case_when(
      {{ age_var }} >= 0  & {{ age_var }} <= 4   ~ "0-4 years",
      {{ age_var }} >= 5  & {{ age_var }} <= 17  ~ "5-17 years",
      {{ age_var }} >= 18 & {{ age_var }} <= 49  ~ "18-49 years",
      {{ age_var }} >= 50 & {{ age_var }} <= 64  ~ "50-64 years",
      {{ age_var }} >= 65                        ~ "65+ years",
      TRUE ~ "Not specified"
    )
  } else { #MX (0–4, 5–19, 20–64, y 65+ a ̃nos)
    case_when(
      {{ age_var }} < 5                ~ "0-5 years",
      {{ age_var }} >= 5 & {{ age_var }} < 20 ~ "5-19 years",
      {{ age_var }} >= 20 & {{ age_var }} < 64 ~ "20-64 years", 
      {{ age_var }} >= 65                      ~ "65+ years",
      TRUE ~ "Not specified"
    )
  }
  
  levels_set <- if (set == "CDC") {
    c("0-4 years", "5-17 years", "18-49 years", "50-64 years", "65+ years", "Not specified")
  } else {
    c("0-4 years", "5-19 years", "20-64 years", "65+ years", "Not specified")
  }
  
  #add new variable
  df %>%
    mutate(AGE_GROUP = factor(case_expr, levels = levels_set))
}
  