### ILOSTAT care share interpreted
# The value interprets, among all people who are outside the labour force,
# what percentage say that care responsibilities are the reason.
# Lets first interpret the median values along the years for male and female.

care_responsibilities_share <- read_csv("data/raw/care_responsbility_share.csv.gz")

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

# use countries with long time series
plot_care_share_trend(care_responsibilities_share, "United States of America")
plot_care_share_trend(care_responsibilities_share, "South Africa")
plot_care_share_trend(care_responsibilities_share, "Zambia")
plot_care_share_trend(care_responsibilities_share, "Germany")
