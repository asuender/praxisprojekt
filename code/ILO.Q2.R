# We know utlize a ILOSTAT dataset that has more data points to analyze the
# difference of care responsibilities in specific countries and along the
# time line. We utilize facet wrapping of line plot portraying values given in data.


# load ILOSTAT data that gives values of "among all people outside labour force,
# what percentage says it is due to care responsibilities by gender and country.
care_responsibilities_share <- read.csv("data/raw/care_responsbility_share.csv.gz")

# We first interpret which country has most values.
dt <- as.data.table(care_responsibilities_share)
dt[sex.label %in% c("Male", "Female") & !is.na(obs_value),
   .(n_years = uniqueN(time)),
   by = ref_area.label][order(-n_years)] |> head(30)


# plot and facet wrap.
plot_care_share_facet <- function(data) {
  dt <- as.data.table(data)
  dt <- dt[, .(
    country = ref_area.label,
    year    = time,
    sex     = sex.label,
    value   = obs_value
  )]
  dt <- dt[sex %in% c("Male", "Female")]
  dt <- dt[!is.na(value)]
  dt <- dt[year >= 2005]

  country_map <- c(
    "Australia"                  = "Australia",
    "Austria"                    = "Austria",
    "Switzerland"                = "Switzerland",
    "United States of America"   = "United States",
    "South Africa"               = "South Africa",
    "Chile"                      = "Chile",
    "Republic of Korea"          = "South Korea",
    "Iran (Islamic Republic of)" = "Iran",
    "Canada"                     = "Canada"
  )
  dt <- dt[country %in% names(country_map)]
  dt[, country := country_map[country]]
  dt[, country := factor(country, levels = unname(country_map))]
  dt[, sex := factor(ifelse(sex == "Female", "Women", "Men"),
                     levels = c("Women", "Men"))]

  ggplot(dt, aes(x = year, y = value, color = sex, group = sex)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(values = c("Women" = "red", "Men" = "blue")) +
    facet_wrap(~ country, scales = "free_y", ncol = 3) +
    labs(
      title    = "Care Responsibilities as Reason for Labour Market Inactivity",
      subtitle = "Share of people outside the labour force due to care responsibilities (%) | from 2005",
      x        = NULL,
      y        = "Share (%)",
      color    = NULL,
      caption  = "Source: ILOSTAT."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey", size = 9),
      plot.caption    = element_text(color = "grey", size = 7),
      legend.position = "top",
      strip.text      = element_text(face = "bold", size = 10),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      panel.spacing   = unit(1, "lines")
    )
}

# facet wrapped graphic
plot_care_share_facet(care_responsibilities_share)



# Let us analyze the gap in care responsibilities between genders for
# extreme cases. Which countries show clear inequality in share towards women
# and which don't ?

plot_care_gap_ranking <- function(data) {
  dt <- as.data.table(data)[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, value = obs_value)
  ]

  dt_last <- dt[, .SD[which.max(year)], by = .(country, sex)]
  dt_wide <- dcast(dt_last, country ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, gap := Female - Male]
  setorder(dt_wide, gap)

  selected <- c(head(dt_wide$country, 5), tail(dt_wide$country, 5))
  dt_plot  <- dt_wide[country %in% selected]
  dt_plot[, group := ifelse(gap %in% tail(sort(gap), 5), "Top 5 Largest Gap", "Bottom 5 Smallest Gap")]

  ggplot(dt_plot, aes(x = reorder(country, gap), y = gap, fill = group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = round(gap, 1)),
              hjust = -0.2, size = 3, color = "grey30") +
    coord_flip() +
    scale_fill_manual(values = c("Top 5 Largest Gap" = "red", "Bottom 5 Smallest Gap" = "steelblue")) +
    labs(
      title    = "Gender Gap in Care Responsibilities by Country",
      subtitle = "Most recent available year | Female minus Male (pp) | Top & Bottom 5",
      x        = NULL,
      y        = "Gap (percentage points)",
      fill     = NULL,
      caption  = "Source: ILOSTAT."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey", size = 9),
      plot.caption  = element_text(color = "grey", size = 7),
      legend.position = "top",
      axis.text.y   = element_text(size = 10)
    )
}

# plot bar graph
plot_care_gap_ranking(care_responsibilities_share)




# Now lets look at the gap amongst european countries (discuss if we should include)
plot_care_gap_ranking_europe <- function(data) {
  europe <- c(
    "Albania", "Austria", "Belarus", "Belgium", "Bosnia and Herzegovina",
    "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark", "Estonia",
    "Finland", "France", "Germany", "Greece", "Hungary", "Iceland",
    "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
    "Moldova", "Montenegro", "Netherlands", "North Macedonia", "Norway",
    "Poland", "Portugal", "Romania", "Russian Federation", "Serbia",
    "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland",
    "Ukraine", "United Kingdom of Great Britain and Northern Ireland"
  )

  dt <- as.data.table(data)[
    sex.label %in% c("Male", "Female") & !is.na(obs_value) & ref_area.label %in% europe,
    .(country = ref_area.label, year = time, sex = sex.label, value = obs_value)
  ]

  dt_last <- dt[, .SD[which.max(year)], by = .(country, sex)]
  dt_wide <- dcast(dt_last, country ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, gap := Female - Male]
  setorder(dt_wide, gap)

  selected <- c(head(dt_wide$country, 5), tail(dt_wide$country, 5))
  dt_plot  <- dt_wide[country %in% selected]
  dt_plot[, group := ifelse(gap %in% tail(sort(gap), 5), "Top 5 Largest Gap", "Bottom 5 Smallest Gap")]

  ggplot(dt_plot, aes(x = reorder(country, gap), y = gap, fill = group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = round(gap, 1)),
              hjust = -0.2, size = 3, color = "grey30") +
    coord_flip() +
    scale_fill_manual(values = c("Top 5 Largest Gap" = "red", "Bottom 5 Smallest Gap" = "steelblue")) +
    labs(
      title    = "Gender Gap in Care Responsibilities — Europe",
      subtitle = "Most recent available year | Female minus Male (pp) | Top & Bottom 5",
      x        = NULL,
      y        = "Gap (percentage points)",
      fill     = NULL,
      caption  = "Source: ILOSTAT."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey", size = 9),
      plot.caption    = element_text(color = "grey", size = 7),
      legend.position = "top",
      axis.text.y     = element_text(size = 10)
    )
}

# plot bar graph
plot_care_gap_ranking_europe(care_responsibilities_share)




### Ignore this. Research purpose only.

plot_care_share_trend <- function(data, country_name) {
  dt <- as.data.table(data)
  dt <- dt[, .(
    country = ref_area.label,
    year    = time,
    sex     = sex.label,
    value   = obs_value
  )]
  dt <- dt[sex %in% c("Male", "Female")]
  dt <- dt[!is.na(value)]
  dt <- dt[country == country_name]

  ggplot(dt, aes(x = year, y = value, color = sex, group = sex)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    scale_color_manual(
      values = c("Female" = "red", "Male" = "blue"),
      labels = c("Frauen", "Männer")
    ) +
    labs(
      title    = paste0("Sorgepflichten als Grund für Nichterwerbstätigkeit – ", country_name),
      subtitle = "Anteil der Personen außerhalb der Erwerbsbevölkerung (%)",
      x        = "Jahr",
      y        = "Anteil (%)",
      color    = NULL,
      caption  = "Quelle: ILOSTAT."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey", size = 9),
      plot.caption    = element_text(color = "grey", size = 7),
      legend.position = "top"
    )
}

dt <- as.data.table(care_responsibilities_share)
dt[sex.label %in% c("Male", "Female") & !is.na(obs_value),
   .N,
   by = ref_area.label][order(-N)] |> head(50)

# use countries with long time series
plot_care_share_trend(care_responsibilities_share, "South Africa")
plot_care_share_trend(care_responsibilities_share, "Chile")
plot_care_share_trend(care_responsibilities_share, "Australia")
plot_care_share_trend(care_responsibilities_share, "Canada")

###
