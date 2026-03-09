### ILOSTAT care share interpreted
# The value interprets, among all people who are outside the labour force,
# what percentage say that care responsibilities are the reason.
# Lets first interpret the median values along the years for male and female.

care_responsibilities_share <- read_csv("data/raw/care_responsbility_share.csv.gz")
names(care_responsibilities_share)

dt.crs <- as.data.table(care_responsibilities_share)
# simplify names
dt.crs <- dt.crs[, .(
  country = ref_area.label,
  year = time,
  sex = sex.label,
  value = obs_value
)]
# extract only male and female (remove total)
dt.crs <- dt.crs[sex %in% c("Male", "Female")]
# remove NA values
dt.crs <- dt.crs[!is.na(value)]
# median share amongst gender along the years
dt_crs_year <- dt.crs[
  , .(value = median(value, na.rm = TRUE)),
  by = .(year, sex)
]

# plot median value for male and female as stacked bar graph along years.
ggplot(dt_crs_year, aes(x = factor(year), y = value, fill = sex)) +
  geom_col() +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
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
# side by side bar graph.
ggplot(dt_crs_year, aes(x = factor(year), y = value, fill = sex)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
  labs(
    title = "Share Outside the Labour Force Due to Care Responsibilities",
    subtitle = "Median across countries by year",
    x = "Year",
    y = "Median share (%)",
    fill = "Sex"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "top"
  )



### ILOSTAT care share interpreted
# now lets interpret the share in individual countries throughout the years with a function.
# Dependent on function input we graph the change in share for both male
# and female for a given country (from 2000 onwards).

years_available <- dt.crs[, .(years_available = uniqueN(year)), by = country][order(-years_available)]

plot_care_country <- function(data, country_name) {
  dt_country <- data[
    country == country_name & year >= 2000
  ]
  ggplot(dt_country, aes(x = factor(year), y = value, fill = sex)) +
    geom_col(position = "dodge") +
    scale_fill_manual(values = c("Female" = "red", "Male" = "blue")) +
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


### ILOSTAT no. of volunteers interpreted
volunteer_count <- read_csv("data/raw/ilo_volunteers_count.csv.gz")
names(volunteer_count)

setDT(volunteer_count)
vc.clean <- volunteer_count[sex.label %in% c("Male", "Female")]

vc.clean[, obs_value := as.numeric(obs_value)]
vc.clean <- vc.clean[!is.na(obs_value)]

vc.clean.collapsed <- vc.clean[, .(
  total_volunteers = sum(obs_value, na.rm = TRUE)
), by = .(ref_area.label, time, sex.label)]

vc.final <- vc.clean.collapsed[, if(.N == 2) .SD, by = .(ref_area.label, time)]
unique(vc.final[, .N, by = .(ref_area.label, time)]$N)

# Female-to-male ratio of volunteers by country
vc.ratio <- vc.final[, .(
  ratio = total_volunteers[sex.label == "Female"] / total_volunteers[sex.label == "Male"]
), by = .(ref_area.label, time)]

# Global trend over time
vc.med <- vc.ratio[, .(median_ratio = median(ratio, na.rm = TRUE)), by = time]
ggplot(vc.med, aes(x = factor(time), y = median_ratio)) +
  geom_col(fill = "blue") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Female/Male Volunteer Ratio Over Time", x = "Year", y = "Ratio") +
  theme_minimal()
