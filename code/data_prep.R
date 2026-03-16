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

all_years <- sort(unique(wage_wide$time))
n_total_years <- length(all_years)

country_years <- wage_wide %>%
  group_by(ref_area.label) %>%
  summarise(
    n_years = n_distinct(time),
    .groups = "drop"
  )

balanced_countries <- country_years %>%
  filter(n_years == n_total_years)

print(balanced_countries)

wage_balanced <- wage_wide %>%
  filter(ref_area.label %in% balanced_countries$ref_area.label)

wage_balanced <- wage_balanced %>%
  mutate(
    wage_gap = (Male - Female) / Male
  )

summary(wage_balanced$wage_gap)

n_balanced_countries <- wage_balanced %>%
  summarise(n = n_distinct(ref_area.label))

print(n_balanced_countries)

trend_gap <- wage_balanced %>%
  group_by(time) %>%
  summarise(
    median_wage_gap = median(wage_gap, na.rm = TRUE),
    mean_wage_gap = mean(wage_gap, na.rm = TRUE),
    n_countries = n_distinct(ref_area.label),
    .groups = "drop"
  )

print(trend_gap)

ggplot(trend_gap, aes(x = time, y = median_wage_gap)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  geom_point(size = 2.5, color = "steelblue") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Global Median Gender Wage Gap Over Time (Balanced Countries)",
    x = "Year",
    y = "Median Gender Wage Gap"
  ) +
  theme_minimal()

latest_year <- max(wage_balanced$time)

latest_gap <- wage_balanced %>%
  filter(time == latest_year) %>%
  arrange(desc(wage_gap)) %>%
  slice(1:10)

print(latest_gap)

ggplot(latest_gap, aes(x = reorder(ref_area.label, wage_gap), y = wage_gap)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = paste("Top 10 Countries with Highest Gender Wage Gap (", latest_year, ")", sep = ""),
    x = "Country",
    y = "Gender Wage Gap"
  ) +
  theme_minimal()

first_year <- min(wage_balanced$time)
last_year  <- max(wage_balanced$time)

country_change <- wage_balanced %>%
  filter(time %in% c(first_year, last_year)) %>%
  select(ref_area.label, time, wage_gap) %>%
  pivot_wider(names_from = time, values_from = wage_gap) %>%
  filter(
    !is.na(.data[[as.character(first_year)]]),
    !is.na(.data[[as.character(last_year)]])
  ) %>%
  mutate(
    change = .data[[as.character(last_year)]] - .data[[as.character(first_year)]],
    abs_change = abs(change)
  ) %>%
  arrange(desc(abs_change))

top10_change <- country_change %>%
  slice(1:10)

print(top10_change)

ggplot(top10_change,
       aes(x = reorder(ref_area.label, change), y = change, fill = change > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = paste("Top 10 Countries with Largest Change in Gender Wage Gap (",
                  first_year, "–", last_year, ")", sep = ""),
    x = "Country",
    y = "Change in Gender Wage Gap"
  ) +
  theme_minimal() +
  guides(fill = "none")

year1 <- 2019
year2 <- 2020

gap_selected <- wage_balanced %>%
  filter(time %in% c(year1, year2))

country_change_1920 <- gap_selected %>%
  select(ref_area.label, time, wage_gap) %>%
  pivot_wider(names_from = time, values_from = wage_gap) %>%
  filter(
    !is.na(`2019`),
    !is.na(`2020`)
  ) %>%
  mutate(
    change = `2020` - `2019`,
    abs_change = abs(change)
  ) %>%
  arrange(desc(abs_change))

top10_change_1920 <- country_change_1920 %>%
  slice(1:10)

print(top10_change_1920)

ggplot(top10_change_1920,
       aes(x = reorder(ref_area.label, change),
           y = change,
           fill = change > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "tomato",
                               "FALSE" = "steelblue")) +
  labs(
    title = "Top Countries Driving the Change in Gender Wage Gap (2019–2020)",
    x = "Country",
    y = "Change in Gender Wage Gap"
  ) +
  theme_minimal() +
  guides(fill = "none")
