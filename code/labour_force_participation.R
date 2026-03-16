library(tidyverse)
library(here)

load_labour_force_with_educ_data <- function() {
  read.csv(here("data", "raw", "ilo_labour_force_participation_and_education.csv.gz"))
}


prepare_lfpr_2024_comparison <- function(data) {
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
                          year == 2024 #Only data for 2024
                          )

  relevant_countries <- c(
    "Tanzania" = "Tanzania, United Republic of" ,
    "Germany" = "Germany",
    "USA" = "United States of America",
    "Iran" = "Iran (Islamic Republic of)",
    "Australia" = "Australia"
  ) #Selection of countries

  data <- data %>% filter(country %in% relevant_countries, #Only selected countries
                          str_detect(education, "Total") #No education levels considered
                          )
 }
