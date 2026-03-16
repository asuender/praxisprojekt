library(here)

# load ILOSTAT data that gives values of "among all people outside labour force,
# what percentage says it is due to care responsibilities by gender and country.
load_care_resp_share_data <- function(data) {
  fread(here("data", "raw", "care_responsbility_share.csv.gz"))
}

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
    levels = c("Women", "Men")
  )]

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


# Country-level dot plot: Female vs Male care responsibility share
# Most recent available year per country (with over 5 values), ordered by female share.
# A dumbbell plot cleanly shows both absolute level and gap simultaneously.

plot_care_country_dumbbell <- function(data, min_years = 5, n_countries = 20) {
  dt <- as.data.table(data)[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, value = obs_value)
  ]

  # keep countries with at least min_years of data
  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= min_years, country]]

  # take most recent year per country
  dt_last <- dt[, .SD[which.max(year)], by = .(country, sex)]
  dt_wide <- dcast(dt_last, country ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, gap := Female - Male]

  # select top and bottom n_countries
  setorder(dt_wide, gap)
  selected <- c(head(dt_wide$country, n_countries / 2),
                tail(dt_wide$country, n_countries / 2))
  dt_plot <- dt_wide[country %in% selected]

  # long format for ggplot
  dt_long <- melt(dt_plot,
                  id.vars       = c("country", "gap"),
                  measure.vars  = c("Female", "Male"),
                  variable.name = "sex",
                  value.name    = "value")

  dt_long[, sex := factor(sex, levels = c("Male", "Female"))]

  ggplot() +
    geom_segment(data = dt_plot,
                 aes(x = Male, xend = Female,
                     y = reorder(country, gap),
                     yend = reorder(country, gap)),
                 color = "grey70", linewidth = 0.8) +
    geom_point(data = dt_long,
               aes(x = value,
                   y = reorder(country, gap),
                   color = sex),
               size = 3) +
    geom_text(data = dt_plot,
              aes(x = (Female + Male) / 2,
                  y = reorder(country, gap),
                  label = paste0("+", round(gap, 1))),
              vjust = -0.7, size = 2.6, color = "grey40") +
    scale_color_manual(values = c("Female" = "red", "Male" = "blue")) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    labs(
      title    = "Gender Gap in Care Responsibilities by Country",
      subtitle = paste0("Share outside labour force due to care (%) | Most recent year | ",
                        "Top & bottom ", n_countries / 2, " countries by gap"),
      x        = "Share outside labour force due to care responsibilities (%)",
      y        = NULL,
      color    = NULL,
      caption  = "Source: ILOSTAT. Countries with fewer than 5 years of data excluded."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey50", size = 9),
      plot.caption    = element_text(color = "grey50", size = 7),
      legend.position = "top",
      axis.text.y     = element_text(size = 9),
      panel.grid.major.y = element_line(color = "grey"),
      panel.grid.major.x = element_line(color = "grey"),
      panel.grid.minor   = element_blank()
    ) +
    scale_y_discrete(expand = expansion(add = c(0.5, 1.2)))
}


### Ignore from here (research purpose only)

# for individual countries in facet
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


plot_care_gap_ranking <- function(dt) {
  dt <- dt[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, value = obs_value)
  ]

  dt_last <- dt[, .SD[which.max(year)], by = .(country, sex)]
  dt_wide <- dcast(dt_last, country ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, gap := Female - Male]
  setorder(dt_wide, gap)

  selected <- c(head(dt_wide$country, 5), tail(dt_wide$country, 5))
  dt_plot <- dt_wide[country %in% selected]
  dt_plot[, group := ifelse(gap %in% tail(sort(gap), 5), "Top 5 Largest Gap", "Bottom 5 Smallest Gap")]

  ggplot(dt_plot, aes(x = reorder(country, gap), y = gap, fill = group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = round(gap, 1)),
      hjust = -0.2, size = 3, color = "grey30"
    ) +
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
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(color = "grey", size = 9),
      plot.caption = element_text(color = "grey", size = 7),
      legend.position = "top",
      axis.text.y = element_text(size = 10)
    )
}

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
  dt_plot <- dt_wide[country %in% selected]
  dt_plot[, group := ifelse(gap %in% tail(sort(gap), 5), "Top 5 Largest Gap", "Bottom 5 Smallest Gap")]

  ggplot(dt_plot, aes(x = reorder(country, gap), y = gap, fill = group)) +
    geom_col(width = 0.7) +
    geom_text(aes(label = round(gap, 1)),
      hjust = -0.2, size = 3, color = "grey30"
    ) +
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

plot_care_share_gii_correlation <- function(dt) {
  gii <- fread(here("data", "raw", "owid_gii.csv"))

  dt.crs <- dt[, .(
    country = ref_area.label,
    year    = time,
    sex     = sex.label,
    value   = obs_value
  )]
  dt.crs <- dt.crs[sex %in% c("Male", "Female") & !is.na(value)]

  dt_country <- dt.crs[, .(
    value = mean(value, na.rm = TRUE)
  ), by = .(country, sex)]

  dt_wide <- dcast(dt_country, country ~ sex, value.var = "value")
  dt_wide[, gap := Female - Male]
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide <- dt_wide[!grepl("Egypt", country)]

  dt_gii <- as.data.table(gii)[!is.na(gii), .(
    gii = mean(gii, na.rm = TRUE)
  ), by = .(country = entity)]

  merged <- merge(dt_wide, dt_gii, by = "country")

  # ── Correlation ───────────────────────────────────────────────────────────────
  pearson <- round(cor(merged$gii, merged$gap, use = "complete.obs"), 3)
  spearman <- round(cor(merged$gii, merged$gap, method = "spearman", use = "complete.obs"), 3)

  cat("Pearson:  ", pearson, "\n")
  cat("Spearman: ", spearman, "\n")
  cat("N:        ", nrow(merged), "\n")

  # ── Scatterplot ───────────────────────────────────────────────────────────────
  ggplot(merged, aes(x = gii, y = gap)) +
    geom_point(size = 2.5, alpha = 0.6, color = "steelblue") +
    geom_smooth(
      method    = "loess",
      se        = TRUE,
      color     = "black",
      linewidth = 0.8,
      alpha     = 0.15
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    annotate(
      "text",
      x = 0.95,
      y = max(merged$gap, na.rm = TRUE),
      label = paste0("Pearson:  ", pearson, "\nSpearman: ", spearman),
      hjust = 1, vjust = 1,
      size = 3.2,
      color = "grey20"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title = "Zusammenhang zwischen GII und Gender Gap in Sorgepflichten",
      subtitle = paste0(
        "Ein Punkt = ein Land | Mittelwert \u00FCber verf\u00FCgbare Jahre | n = ",
        nrow(merged), " L\u00E4nder"
      ),
      x = "Gender Inequality Index (GII) \u2013 h\u00F6here Werte = mehr Ungleichheit",
      y = "Gender Gap (Frauen \u2212 M\u00E4nner, %)",
      caption = paste0(
        "Quelle: ILOSTAT, OWID/UNDP. ",
        "LOESS-Gl\u00E4ttung mit 95%-Konfidenzband. ",
        "\u00C4gypten ausgeschlossen (extremer Ausrei\u00DFer)."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey", size = 9),
      plot.caption     = element_text(color = "grey", size = 7),
      panel.grid.minor = element_blank()
    )
}

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
