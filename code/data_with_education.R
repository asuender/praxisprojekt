library(tidyverse)

labour_force_with_educ <- read.csv("data/raw/ilo_labour_force_participation_and_education.csv.gz")

#Umbenennen relevanter Variablen
labour_force_with_educ <- rename(labour_force_with_educ,
                                 country = ref_area.label,
                                 data_source = source.label ,
                                 sex = sex.label,
                                 education = classif1.label,
                                 year = time,
                                 rate = obs_value)

#Selektieren relevanter Variablen
labour_force_with_educ <- select(labour_force_with_educ,
                                 country, data_source, sex, education, year, rate)

#Filtern nach einer Skala der Bildung
labour_force_with_educ <- filter(labour_force_with_educ,
                                 str_detect(education, "ISCED-11"))

#Filtern nach Sex, nur Männer und Frauen
labour_force_with_educ <- filter(labour_force_with_educ,
                                 when_any(sex == "Female", sex == "Male"))


#Filtern - keine NAs für Labour Force Participation Rate
labour_force_with_educ <- filter(labour_force_with_educ, !is.na(rate))

#Übersicht - Labour Force Participation Rate im Jahr 2024 (Total)
labour_force_total_2024 <- filter(labour_force_with_educ,
                                  year == 2024, str_detect(education, "Total"))

#Untersuchen der Variable LFPR für Frauen im Jahr 2024
labour_force_total_2024 |> filter(sex == "Female") |>
  summarize(Median = median(rate), Mean =  mean(rate), Min = min(rate),
            Max = max(rate))
labour_force_total_2024 |> filter(sex == "Female",
                                  when_any(rate == 14.106, rate == 81.691))

#Grafik: LFPR Männer vs. Frauen im Jahr 2024 für Deutschland, USA, Tansania, Iran, Australien
labour_force_total_2024 |> filter(when_any(country == "Tanzania, United Republic of",
                                           country == "Germany",
                                           country == "United States of America",
                                           country == "Iran (Islamic Republic of)",
                                           country == "Australia")) |>
  ggplot(aes(x = country, y = rate, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Labour Force Participation Rate in 2024 by Gender",
        x = "Country",
        y = "Labour Force Participation Rate in %",
        fill = "Biological Gender") +
  scale_y_continuous(limits = c(0,90), breaks = seq(0,90,20))

#Grafik - LFPR Männer vs. Frauen in Deutschland von 2014 bis 2024
labour_force_with_educ |> filter(str_detect(education, "Total"),
                                 country == "Germany", year >= 2014) |>
  ggplot(aes(x = year, y = rate, colour = sex)) + geom_line() +
  labs(title = "Labour Force Participation Rate in Germany between 2014 and 2024",
       x = "Year",
       y = "Labour Force Participation Rate in %",
       colour = "Biological Gender") +
  scale_y_continuous(limits = c(0,70), breaks = seq(0,70,10))
