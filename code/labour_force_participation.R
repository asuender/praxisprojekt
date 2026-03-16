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
    mutate(country = ifelse(country %in% relevant_countries,
                            c("Tanzania", "Germany", "USA", "Iran", "Australia"),
                            country))
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
