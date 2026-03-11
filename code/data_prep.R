owid <- read.csv("data/raw/ilo_avg_hourly_wages_usd.csv.gz")

load_data <- function() {
  # TODO: implement data loading
}
library(dplyr)
library(ggplot2)

data <- read.csv("data/raw/ilo_avg_hourly_wages_usd.csv.gz")

data_filtered <- data %>%
  filter(
    classif1.label == "Currency: U.S. dollars",
    sex.label %in% c("Male", "Female"),
    time >=2010,
    time <= 2025
  )

head(data)

male <- data_filtered %>%
  filter(sex.label == "Male")
female <- data_filtered %>%
  filter(sex.label == "Female")

merged <- merge(
  male,
  female,
  by = c("ref_area.label" , "time"),
  suffixes = c("_male", "_female")
)

merged$wage_gap <- (merged$obs_value_male - merged$obs_value_female) / merged$obs_value_male

avg_gap <- merged %>%
  group_by(time) %>%
  summarise(wage_gap = mean(wage_gap, na.rm = TRUE))

head(merged)


ggplot(avg_gap, aes(x = time, y = wage_gap)) +
  geom_line(color = "blue" , linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Gender Wage Gap Over Time",
    x = "Year",
    y = "Gender Wage Gap"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

avg_wage <-data_filtered %>%
  group_by(sex.label) %>%
  summarise(avg_wage = mean(obs_value, na.rm = TRUE))

ggplot(avg_wage, aes(x = sex.label, y= avg_wage, fill =sex.label)) +
  geom_col() +
  labs(
    title = "Average Hourly Wage by Gender",
    x = "Gender",
    y = "Average Wage (USD)"
  ) +
  theme_minimal() +
  guides(fill = "none")

country_gap <- merged %>%
  group_by(ref_area.label) %>%
  summarise(wage_gap = mean(wage_gap, na.rm =TRUE)) %>%
  arrange(desc(wage_gap)) %>%
  slice(1:10)

ggplot(country_gap, aes(x = reorder(ref_area.label, wage_gap), y =wage_gap)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Top Countries by Gender Wage Gap",
    x = "Country",
    y = "Gender Wage Gap"
  ) +
  theme_minimal()

