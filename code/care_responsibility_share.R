library(here)
library(data.table)

# load ILOSTAT data that gives values of "among all people outside labour force,
# what percentage says it is due to care responsibilities by gender and country.

load_care_resp_share_data <- function() {
  fread(here("data", "raw", "care_responsbility_share.csv.gz"))
}

load_lfp_data <- function() {
  fread(here("data", "raw", "ilo_labour_force_participation.csv.gz"))
}

lfp <- load_lfp_data()

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

care_responsibilities_share <- load_care_resp_share_data()


#####

# this is a lollipop plot that compares gaps of inequality for
# the most recent years for the top and bottom 10
# countries in values. It also removes countries that have less than 5
# data points for valid analysis.

plot_care_country_gap <- function(data, n_countries = 20) {
  dt <- as.data.table(data)[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label,
      value = obs_value)
  ]

  # remove countries with only one year of data
  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= 2, country]]

  # most recent year where both sexes observed in same year, capped at 2023
  dt_paired <- dt[year <= 2023, if (all(c("Male", "Female") %in% sex)) .SD,
                  by = .(country, year)]
  dt_last_year <- dt_paired[, .(last_year = max(year)), by = country]
  dt_paired <- dt_paired[dt_last_year, on = "country"][year == last_year]

  # keep only countries with most recent paired observation from 2020 or later
  dt_paired <- dt_paired[year >= 2020]

  # wide format and gap
  dt_wide <- dcast(dt_paired, country + year ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, gap := Female - Male]

  # top and bottom 10 by gap
  setorder(dt_wide, gap)
  selected <- c(head(dt_wide$country, n_countries / 2),
                tail(dt_wide$country, n_countries / 2))
  dt_plot <- dt_wide[country %in% selected]

  # country label with year appended
  dt_plot[, country_label := paste0(country, " (", year, ")")]

  ggplot(dt_plot, aes(x = gap,
                      y = reorder(country_label, gap),
                      color = gap)) +
    geom_vline(xintercept = 0, color = "grey50", linetype = "dashed") +
    geom_point(size = 3) +
    geom_segment(aes(x = 0, xend = gap,
                     y = reorder(country_label, gap),
                     yend = reorder(country_label, gap)),
                 linewidth = 0.5, alpha = 0.5) +
    scale_color_gradient(low = "#f5a623", high = "#c0392b") +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       expand = expansion(mult = c(0.05, 0.08))) +
    labs(
      title    = "Gender Gap in Care Driven Labour Inactivity",
      subtitle = paste0("Gap = Female minus Male share outside labour force due to care (%) | ",
                        "Top & bottom 10 countries | Most recent paired year 2020–2023"),
      x        = "Gender gap (percentage points)",
      y        = NULL,
      caption  = paste0(
        "Source: ILOSTAT. Countries with only one year of data excluded. ",
        "Most recent paired year between 2020 and 2023 used per country. ",
        "2024 data excluded due to incomplete provisional releases. ",
        "Year shown in parentheses."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey50", size = 9),
      plot.caption       = element_text(color = "grey50", size = 7),
      legend.position    = "none",
      axis.text.y        = element_text(size = 8),
      panel.grid.major.y = element_line(color = "grey92"),
      panel.grid.major.x = element_line(color = "grey92"),
      panel.grid.minor   = element_blank()
    )
}

# Correlation analysis function for care share and LFP
plot_care_lfp_correlation <- function(data, lfp_data) {
  dt <- as.data.table(data)[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, value = obs_value)
  ]

  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= 2, country]]

  dt_paired <- dt[year <= 2023, if (all(c("Male", "Female") %in% sex)) .SD,
                  by = .(country, year)]
  dt_last <- dt_paired[, .(last_year = max(year)), by = country]
  dt_paired <- dt_paired[dt_last, on = "country"][year == last_year]
  dt_paired <- dt_paired[year >= 2020]

  dt_wide <- dcast(dt_paired, country + year ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, care_gap := Female - Male]

  dt_lfp <- as.data.table(lfp_data)[
    classif1.label == "Age (Youth, adults): 15+" &
      sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, lfp = obs_value)
  ]
  dt_lfp_wide <- dcast(dt_lfp, country + year ~ sex, value.var = "lfp")
  dt_lfp_wide[, lfp_gap := Male - Female]

  merged <- merge(dt_wide[, .(country, year, care_gap)],
                  dt_lfp_wide[, .(country, year, lfp_gap)],
                  by = c("country", "year"))

  sp_rho <- round(cor(merged$care_gap, merged$lfp_gap,
                      method = "spearman", use = "complete.obs"), 3)
  annot  <- paste0("Spearman \u03C1 = ", sp_rho, "\nn = ", nrow(merged))

  cat("Spearman rho: ", sp_rho, "\n")
  cat("N:            ", nrow(merged), "\n")

  ggplot(merged, aes(x = lfp_gap, y = care_gap)) +
    geom_point(size = 2.5, alpha = 0.6, color = "steelblue") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    annotate(
      "text",
      x     = max(merged$lfp_gap, na.rm = TRUE),
      y     = max(merged$care_gap, na.rm = TRUE),
      label = annot,
      hjust = 1, vjust = 1,
      size  = 3.2,
      color = "grey20"
    ) +
    labs(
      title    = "Care Inactivity Gap vs Labour Force Participation Gap",
      subtitle = paste0("One point = one country | Most recent paired year 2020\u20132023 | ",
                        "n = ", nrow(merged), " countries"),
      x        = "Labour Force Participation gap (Male minus Female, pp)",
      y        = "Care inactivity gap (Women minus Men, pp)",
      caption  = "Source: ILOSTAT. Data: Most recent paired year between 2020 and 2023 per country with more than 1 data point."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey50", size = 9),
      plot.caption     = element_text(color = "grey50", size = 7),
      panel.grid.minor = element_blank()
    )
}

care_responsibilities_share <- load_care_resp_share_data()
lfp <- load_lfp_data()

### Ignore from here (research purpose only)

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



plot_care_ratio_gii_correlation <- function(data) {
  gii <- fread(here("data", "raw", "owid_gii.csv"))

  dt <- as.data.table(data)[, .(
    country = entity,
    year    = year,
    female  = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__female`,
    male    = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__male`
  )][!is.na(female) & !is.na(male)]

  # country mean across all available years then ratio
  dt_country <- dt[, .(
    female = mean(female, na.rm = TRUE),
    male   = mean(male,   na.rm = TRUE)
  ), by = country]
  dt_country[, ratio := female / male]

  # most recent GII per country
  dt_gii <- as.data.table(gii)[
    !is.na(gii),
    .SD[which.max(year)],
    by = .(country = entity)
  ]

  merged <- merge(dt_country, dt_gii[, .(country, gii)], by = "country")

  # spearman with significance test
  sp_test  <- cor.test(merged$gii, merged$ratio, method = "spearman")
  sp_rho   <- round(sp_test$estimate, 3)
  sp_pval  <- signif(sp_test$p.value, 3)
  sig_label <- ifelse(sp_pval < 0.001, "p < 0.001", paste0("p = ", sp_pval))
  annot_label <- paste0("Spearman \u03C1 = ", sp_rho, "\n", sig_label, "\nn = ", nrow(merged))

  cat("Spearman rho: ", sp_rho, "\n")
  cat("p-value:      ", sp_pval, "\n")
  cat("N:            ", nrow(merged), "\n")

  ggplot(merged, aes(x = gii, y = ratio)) +
    geom_point(size = 2.5, alpha = 0.6, color = "steelblue") +
    geom_smooth(
      method    = "loess",
      se        = TRUE,
      color     = "black",
      linewidth = 0.8,
      alpha     = 0.15
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    annotate(
      "text",
      x     = 0.95,
      y     = max(merged$ratio, na.rm = TRUE),
      label = annot_label,
      hjust = 1, vjust = 1,
      size  = 3.2,
      color = "grey20"
    ) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    labs(
      title    = "Correlation between GII and Female-to-Male Domestic Work Time Ratio",
      subtitle = paste0(
        "One point = one country | Country mean over all available years | ",
        "n = ", nrow(merged), " countries"
      ),
      x       = "Gender Inequality Index (GII) \u2013 higher values = more inequality",
      y       = "Female-to-Male time ratio in domestic work",
      caption = paste0(
        "Source: OWID/ILO (domestic work time), OWID/UNDP (GII). ",
        "Ratio = female mean / male mean across all available years. ",
        "LOESS smoothing with 95% confidence band. ",
        "Dashed line at ratio = 1 indicates parity."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey50", size = 9),
      plot.caption     = element_text(color = "grey50", size = 7),
      panel.grid.minor = element_blank()
    )
}



plot_care_country_gap_2 <- function(data, lfp_data, n_countries = 10) {
  dt <- as.data.table(data)[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label,
      value = obs_value)
  ]

  # remove countries with only one year of data
  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= 2, country]]

  # most recent year where both sexes observed in same year, capped at 2023
  dt_paired <- dt[year <= 2023, if (all(c("Male", "Female") %in% sex)) .SD,
                  by = .(country, year)]
  dt_last_year <- dt_paired[, .(last_year = max(year)), by = country]
  dt_paired <- dt_paired[dt_last_year, on = "country"][year == last_year]

  # keep only countries with most recent paired observation from 2020 or later
  dt_paired <- dt_paired[year >= 2020]

  # wide format and care gap
  dt_wide <- dcast(dt_paired, country + year ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, care_gap := Female - Male]

  # top and bottom 5 by care gap
  setorder(dt_wide, care_gap)
  selected <- c(head(dt_wide$country, n_countries / 2),
                tail(dt_wide$country, n_countries / 2))
  dt_plot <- dt_wide[country %in% selected]

  # LFP gap for matching country-year
  dt_lfp <- as.data.table(lfp_data)[
    classif1.label == "Age (Youth, adults): 15+" &
      sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, lfp = obs_value)
  ]

  dt_lfp_wide <- dcast(dt_lfp, country + year ~ sex, value.var = "lfp")
  dt_lfp_wide[, lfp_gap := Male - Female]

  # merge LFP gap
  dt_plot <- merge(dt_plot, dt_lfp_wide[, .(country, year, lfp_gap)],
                   by = c("country", "year"), all.x = TRUE)

  # country label with year
  dt_plot[, country_label := paste0(country, "\n(", year, ")")]

  # create long format with both gaps as separate rows per country
  dt_long <- melt(
    dt_plot,
    id.vars      = c("country", "country_label", "year", "care_gap", "lfp_gap"),
    measure.vars = c("care_gap", "lfp_gap"),
    variable.name = "gap_type",
    value.name    = "gap"
  )
  dt_long[, gap_type := factor(
    ifelse(gap_type == "care_gap",
           "Care Inactivity Gap (Women - Men)",
           "LFP Gap (Men - Women)"),
    levels = c("Care Inactivity Gap (Women - Men)", "LFP Gap (Men - Women)")
  )]

  # preserve country order by care gap
  country_order <- dt_plot[order(care_gap), country_label]
  dt_long[, country_label := factor(country_label, levels = country_order)]

  ggplot(dt_long, aes(x = gap,
                      y = country_label,
                      color = gap_type)) +
    geom_vline(xintercept = 0, color = "grey40", linetype = "dashed") +
    geom_point(size = 3) +
    geom_segment(aes(x = 0, xend = gap,
                     y = country_label,
                     yend = country_label),
                 linewidth = 0.5, alpha = 0.6) +
    scale_color_manual(
      values = c("Care Inactivity Gap (Women - Men)" = "#c0392b",
                 "LFP Gap (Men - Women)"             = "#2471A3"),
      name = NULL
    ) +
    facet_wrap(~ gap_type, ncol = 2, scales = "free_x") +
    labs(
      title    = "Care Inactivity Gap vs Labour Force Participation Gap",
      subtitle = paste0("Top & bottom 5 countries by care gap | Most recent paired year 2020–2023"),
      x        = "Gap (percentage points)",
      y        = NULL,
      caption  = paste0(
        "Source: ILOSTAT. Care gap = Female minus Male share outside labour force due to care. ",
        "LFP gap = Male minus Female labour force participation rate (15+), same year. ",
        "Countries with only one year of data excluded. 2024 data excluded."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey50", size = 9),
      plot.caption       = element_text(color = "grey50", size = 7),
      legend.position    = "none",
      axis.text.y        = element_text(size = 9),
      strip.text         = element_text(face = "bold", size = 10),
      panel.grid.major.y = element_line(color = "grey92"),
      panel.grid.major.x = element_line(color = "grey92"),
      panel.grid.minor   = element_blank()
    )
}

