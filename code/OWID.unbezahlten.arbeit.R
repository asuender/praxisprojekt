### OWID domestic work data interpreted.
# The lack of data for individual countries has led to taking the median of
# all countries in each year for a general view of whether the inequality in
# unpaid work exists throughout time.

owid_domestic_work_time <- read_csv("data/raw/owid_domestic_work_time.csv")
names(owid_domestic_work_time)

FDWT <- function(data) {
  data <- as.data.table(data)
  data <- data[, .(
    country = entity,
    year = year,
    female = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__female`,
    male   = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__male`
  )]
  data <- data[!is.na(female) & !is.na(male)]
  dt_year <- data[
    , .(
      female_med = median(female, na.rm = TRUE),
      male_med   = median(male, na.rm = TRUE)
    ),
    by = year
  ]
  dt_year[, gender_gap := female_med - male_med]
  dt_year[, ratio := female_med / male_med]
  return(dt_year)
}

filtered_domestic_work_time <- FDWT(owid_domestic_work_time)

# bar graph for ratio throughout years
ggplot(filtered_domestic_work_time, aes(x = factor(year), y = ratio)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") +
  labs(
    title = "Female-to-Male Ratio of Time Spent on Unpaid Domestic Work",
    x = "Year",
    y = "Female / Male Ratio"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# bar graph for gender gap throughout years
ggplot(filtered_domestic_work_time, aes(x = factor(year), y = gender_gap)) +
  geom_col(fill = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Gender Gap in Time Spent on Unpaid Domestic Work",
    x = "Year",
    y = "Female − Male (% of day)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )
