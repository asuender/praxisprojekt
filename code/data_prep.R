library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

data <- read.csv("data/raw/ilo_avg_hourly_wages_usd.csv.gz")

data_filtered <- data %>%
  filter(
    classif1.label == "Currency: U.S. dollars",
    sex.label %in% c("Male", "Female"),
    time >= 2010,
    time <= 2023
  )

wage_wide <- data_filtered %>%
  select(ref_area.label, time, sex.label, obs_value) %>%
  pivot_wider(
    names_from = sex.label,
    values_from = obs_value
  ) %>%
  filter(!is.na(Male), !is.na(Female))

wage_wide <- wage_wide %>%
  mutate(
    wage_gap = (Male - Female) / Male
  )

summary(wage_wide$wage_gap)


country_count <- wage_wide %>%
  group_by(time) %>%
  summarise(
    n_countries = n_distinct(ref_area.label),
    .groups = "drop"
  )

print(country_count)

ggplot(country_count, aes(x = time, y = n_countries)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  labs(
    title = "Number of Countries in Dataset per Year",
    x = "Year",
    y = "Number of Countries"
  ) +
  theme_minimal()


trend_gap <- wage_wide %>%
  group_by(time) %>%
  summarise(
    median_wage_gap = median(wage_gap, na.rm = TRUE),
    mean_wage_gap = mean(wage_gap, na.rm = TRUE),
    .groups = "drop"
  )

print(trend_gap)

ggplot(trend_gap, aes(x = time, y = median_wage_gap)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Median Gender Wage Gap Over Time",
    x = "Year",
    y = "Median Wage Gap"
  ) +
  theme_minimal()


year_change <- trend_gap %>%
  arrange(time) %>%
  mutate(
    previous_year = lag(time),
    change = median_wage_gap - lag(median_wage_gap),
    abs_change = abs(change)
  ) %>%
  filter(!is.na(change)) %>%
  arrange(desc(abs_change))

print(year_change)


largest_change <- year_change %>%
  slice(1)

print(largest_change)

year1 <- largest_change$previous_year
year2 <- largest_change$time

cat("Largest change is between", year1, "and", year2, "\n")

# =========================
# 8. Keep only the two years with the largest change
# =========================
gap_selected <- wage_wide %>%
  filter(time %in% c(year1, year2))


country_change <- gap_selected %>%
  select(ref_area.label, time, wage_gap) %>%
  pivot_wider(names_from = time, values_from = wage_gap) %>%
  filter(!is.na(.data[[as.character(year1)]]),
         !is.na(.data[[as.character(year2)]])) %>%
  mutate(
    change = .data[[as.character(year2)]] - .data[[as.character(year1)]],
    abs_change = abs(change)
  ) %>%
  arrange(desc(abs_change))

print(country_change)


top10 <- country_change %>%
  slice(1:10)

print(top10)

# Plot Top 10 country changes
# red   = increase
# blue  = decrease

ggplot(top10, aes(x = reorder(ref_area.label, change), y = change, fill = change > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue")) +
  labs(
    title = paste("Top 10 Countries Driving the Wage Gap Change (", year1, "–", year2, ")", sep = ""),
    x = "Country",
    y = "Change in Wage Gap"
  ) +
  theme_minimal() +
  guides(fill = "none")

selected_country_count <- country_count %>%
  filter(time %in% c(year1, year2))

print(selected_country_count)
