library(tidyverse)

labour_force_with_educ <- read.csv("data/raw/ilo_labour_force_participation_and_education.csv.gz")

#Umbenennen relevanter Variablen
labour_force_with_educ <- rename(labour_force_with_educ,
                                 country = ref_area.label,
                                 sex = sex.label,
                                 education = classif1.label,
                                 year = time,
                                 rate = obs_value)

#Selektieren relevanter Variablen
labour_force_with_educ <- select(labour_force_with_educ,
                                 country, sex, education, year, rate)

#Filtern nach einer Skala der Bildung
labour_force_with_educ <- filter(labour_force_with_educ,
                                 str_detect(education, "ISCED-11"))
