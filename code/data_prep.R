load_data <- function() {
  # TODO: implement data loading
}
data <- read.csv("data/raw/wage_data.csv")
head(data)
data_gender <- data[data$sex.label %in% c("Male" , "Female") , ]
male <- data_gender[data_gender$sex.label == "Male" , ]
female <- data_gender[data_gender$sex.label == "Female" , ]
merged <- merge(
  male,
  female,
  by = c("ref_area.label" , "time"),
  suffixes = c("_male", "_female")
)
merged$wage_gap <- (merged$obs_value_male - merged$obs_value_female) / merged$obs_value_male
head(merged)

library(dplyr)

avg_gap <- merged %>%
  group_by(time) %>%
  summarise(wage_gap = mean(wage_gap, na.rm = TRUE))

library(ggplot2)

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
