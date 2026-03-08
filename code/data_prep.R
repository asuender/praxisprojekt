library(tidyverse)


data_labour_force <- read.csv("data/raw/ilo_labour_force_participation.csv.gz")

#Umbenennen relevanter Variablen
data_labour_force <- rename(data_labour_force,
       country = ref_area.label,
       sex = sex.label,
       age_group = classif1.label,
       year = time,
       rate = obs_value)

#Selektieren relevanter Variablen
data_labour_force <- select(data_labour_force,
                            country, sex, age_group, year, rate)
