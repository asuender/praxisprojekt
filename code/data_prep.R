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


#Filtern der Daten, nur eine/die gesamte Altersgruppe
data_lf_all_age <- filter(data_labour_force, age_group == "Age (Youth, adults): 15+")

#Filtern der Daten nur nach Jahr 2025
data_lf_all_age_2025 <- filter(data_lf_all_age, year == 2025)


#Grafik: LFPR im Jahr 2025 von USA, Spanien, Japan, Pakistan
data_lf_all_age_2025|> filter(when_any(country == "United States of America",
                                       country == "Spain",
                                       country == "Japan",
                                       country == "Pakistan")) |>
  ggplot(aes(x = country, y = rate, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge")


#Grafik: LFPR in Deutschland von 2010-2024
data_lf_all_age |> filter(country == "Germany") |>
  filter(year >= 2010) |>
  ggplot(aes(x = year, y = rate, colour = sex)) + geom_line()
