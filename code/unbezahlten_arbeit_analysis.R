
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
  geom_col(fill = "steelblue") +
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
  geom_col(fill = "tomato") +
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



### ILOSTAT care share interpreted
# The value interprets, among all people who are outside the labour force,
# what percentage say that care responsibilities are the reason.
# Lets first interpret the median values along the years for male and female.

care_responsibilities_share <- read_csv("data/raw/care_responsbility_share.csv.gz")
names(care_responsibilities_share)

dt.crs <- as.data.table(care_responsibilities_share)
dt.crs <- dt.crs[, .(
  country = ref_area.label,
  year = time,
  sex = sex.label,
  value = obs_value
)]
dt.crs <- dt.crs[sex %in% c("Male", "Female")]
dt.crs <- dt.crs[!is.na(value)]
dt_crs_year <- dt.crs[
  , .(value = median(value, na.rm = TRUE)),
  by = .(year, sex)
]

# plot median value for male and female as stacked bar graph along years.
ggplot(dt_crs_year, aes(x = factor(year), y = value, fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
  labs(
    title = "Share of People Outside Labour Force Due to Care Responsibilities",
    x = "Year",
    y = "Median Share (%)",
    fill = "Sex"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )



### ILOSTAT care share interpreted
# now lets interpret the share in individual countries throughout the years.
# Dependent on function input we graph the change in share for both male
# and female for a given country (from 2000 onwards).

years_available <- dt.crs[, .(years_available = uniqueN(year)), by = country][order(-years_available)]

plot_care_country <- function(data, country_name) {
  dt_country <- data[
    country == country_name & year >= 2000
  ]
  ggplot(dt_country, aes(x = factor(year), y = value, fill = sex)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Female" = "tomato", "Male" = "steelblue")) +
    labs(
      title = paste("Share Outside Labour Force Due to Care –", country_name),
      x = "Year",
      y = "Share (%)",
      fill = "Sex"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(face = "bold")
    )
}

# It seems that the values for individual countries seem to differ by a lot
# compared to the median of all.

plot_care_country(dt.crs, "Canada")

