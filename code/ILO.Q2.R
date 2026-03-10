### ILOSTAT care share interpreted
# The value interprets, among all people who are outside the labour force,
# what percentage saya that care responsibilities are the reason.
# Lets first interpret the median values along the years for male and female.
care_responsibilities_share <- read.csv("data/raw/care_responsbility_share.csv.gz")

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
plot_care_share_trend(care_responsibilities_share, "Austria")




### For presentation
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

  selected <- c("South Africa", "Chile", "Australia", "Austria")
  dt <- dt[country %in% selected]

  dt[, sex_de := ifelse(sex == "Female", "Frauen", "M\u00E4nner")]
  dt[, sex_de := factor(sex_de, levels = c("Frauen", "M\u00E4nner"))]

  dt[, country_en := fcase(
    country == "South Africa", "South Africa",
    country == "Chile",     "Chile",
    country == "Australia", "Australia",
    country == "Austria",   "Austria"
  )]

  dt[, country_en := factor(country_en, levels = c(
    "South Africa", "Chile", "Australia", "Austria"
  ))]

  ggplot(dt, aes(x = year, y = value, color = sex_de, group = sex_de)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(
      values = c("Frauen" = "red", "M\u00E4nner" = "blue")
    ) +
    facet_wrap(~ country_en, scales = "free_y", ncol = 2) +
    labs(
      title    = "Sorgepflichten als Grund f\u00FCr Nichterwerbst\u00E4tigkeit",
      subtitle = "Anteil der Personen au\u00DFerhalb der Erwerbsbev\u00F6lkerung aufgrund von Sorgepflichten (%) | ab 2005",
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
      legend.position = "top",
      strip.text      = element_text(face = "bold", size = 10),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      panel.spacing   = unit(1, "lines")
    )
}

plot_care_share_facet(care_responsibilities_share)


# TODO downward trend analysis for each country. analyze each one.
# is the majority of countries on a downward trend ?
