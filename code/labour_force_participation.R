library(tidyverse)
library(here)

#Load labour force participation raw data
load_labour_force_with_educ_data <- function() {
  read.csv(here("data", "raw", "ilo_labour_force_participation_and_education.csv.gz"))
}


#Function to prepare raw data
prepare_lfpr_data <- function(data) {
  data <- data %>% select(
    country = ref_area.label,
    data_source = source.label,
    year    = time,
    education = classif1.label,
    sex     = sex.label,
    rate   = obs_value
    )  #Select and rename relevant variables
  data <- data %>% filter(str_detect(education, "ISCED-11"), #Only one education scale
                          sex %in% c("Male", "Female"), #Only male/female rates
                          !is.na(rate), #No NA´s for labour force particpation rate
                          )
  data
}



#Prepare intermediate data for first analysis - five countries in year 2024
prepare_lfpr_total_2024_comp <- function(data) {
  relevant_countries <- c(
    "Tanzania, United Republic of" ,
    "Germany",
    "United States of America",
    "Iran (Islamic Republic of)",
    "Australia"
  ) #Select relevant countries

  data <- data %>% filter(country %in% relevant_countries, #Only selected countries
                          str_detect(education, "Total"), #No education levels considered
                          year == 2024 #Only data for 2024
  ) %>%
    mutate(country = recode(country,
                            "Tanzania, United Republic of" = "Tanzania" ,
                            "Germany" = "Germany",
                            "United States of America" = "USA",
                            "Iran (Islamic Republic of)" = "Iran",
                            "Australia" = "Australia"))
  data
}



#Function to prepare intermediate data for second analysis - Germany from 2010-2024
prepare_lfpr_GER_2010_to_2024 <- function(data) {
  data <- data%>% filter(country == "Germany", #Only Germany
                         str_detect(education, "Total"), #No education levels considered
                         year >= 2010 #From year 2010 onwards
  )
  data
}



#Function to prepare intermediate data for third analysis - LFPR by education level
prepare_lfpr_2024_by_education <- function(data) {
  data <- data %>% filter(year == 2024, #Only year 2024
                          when_any(country == "Germany", #Only Germany and Iran
                                   country == "Iran (Islamic Republic of)"),
                          !str_detect(education, #Drop certain education levels
                                      "Total|No schooling|Early childhood|Post-secondary|Not elsewhere")) %>%
    mutate(country = recode(country, #Rename countries
                            "Germany" = "Germany",
                            "Iran (Islamic Republic of)" = "Iran")) %>%
    mutate(education = str_remove(education, "^Education \\(ISCED-11\\):")) #Rename education levels

  data
}

#Function to group education levels into four categories and calculate adjusted lfpr
group_educ_and_recalculate_rates <- function(data) {
  data <- data %>% mutate(education_new = case_when( #Regroup education levels
      str_detect(education, "equivalent|Short-cycle") ~ " 4. Tertiary education",
      TRUE ~ education
    )) %>%
    group_by(country, sex, education_new) %>%
    summarise(rate = mean(rate, na.rm = TRUE),
              .groups = "drop") #Calculate aggregated lfpr for new education levels by country and sex

  data
}



#Function to prepare intermediate data for third analysis - LFPR for tertiary education
tertiary_educ_detailed <- function(data) {
  data <- data %>% filter(str_detect(education,  #Filter for tertiary education
                                     "Short-cycle|equivalent level")) %>%
    mutate(education = ifelse(str_detect(education, "level"),
                             str_remove(education, " level"),
                             education)) #Rename education levels
  data
}
