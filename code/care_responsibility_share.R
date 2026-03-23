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
    facet_wrap(~ country, ncol = 3) +
    labs(
      title    = "Care Responsibilities as Reason for Labour Market Inactivity",
      subtitle = "Share of people outside the labour force due to care responsibilities (%) | from 2005 onwards",
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
plot_care_share_facet(care_responsibilities_share)


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
                        "Top & Bottom 10 countries | Most recent paired year 2020–2023"),
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


### Correlation analysis for graphics no.2
# Correlation analysis function for care share and LFP

plot_care_lfp_correlation <- function(data, lfp_data, n_countries = 20) {
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

  setorder(dt_wide, care_gap)
  low_gap_countries  <- head(dt_wide$country, n_countries / 2)
  high_gap_countries <- tail(dt_wide$country, n_countries / 2)

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

  merged[, highlight := fcase(
    country %in% high_gap_countries, "High care gap (top 10)",
    country %in% low_gap_countries,  "Low care gap (bottom 10)",
    default = "Other"
  )]

  top10_merged <- merged[highlight == "High care gap (top 10)"]
  setorder(top10_merged, lfp_gap)
  label_countries <- head(top10_merged$country, 2)

  sp_rho <- round(cor(merged$care_gap, merged$lfp_gap,
                      method = "spearman", use = "complete.obs"), 3)
  annot  <- paste0("Spearman \u03C1 = ", sp_rho)


  ggplot(merged, aes(x = lfp_gap, y = care_gap, color = highlight)) +
    geom_point(aes(size  = ifelse(highlight == "Other", 2.0, 2.8),
                   alpha = ifelse(highlight == "Other", 0.4, 0.9))) +
    scale_size_identity() +
    scale_alpha_identity() +
    ggrepel::geom_text_repel(
      data          = merged[country %in% label_countries],
      aes(label     = country),
      color         = "#c0392b",
      size          = 3.0,
      fontface      = "bold",
      box.padding   = 0.5,
      point.padding = 0.3,
      segment.color = "grey60",
      segment.size  = 0.3,
      show.legend   = FALSE
    ) +
    scale_color_manual(
      values = c(
        "High care gap (top 10)"   = "#c0392b",
        "Low care gap (bottom 10)" = "#f5a623",
        "Other"                    = "grey70"
      ),
      name = NULL
    ) +
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
      title    = "Care Related Labour Inactivity Gap vs Labour Force Participation Gap with 85 countries",
      subtitle = "One point = one country | Most recent paired year 2020\u20132023 | Highlighted = top & bottom 10 countries from previous slide",
      x        = "Labour Force Participation gap (Male minus Female)",
      y        = "Care Related Labour inactivity gap (Female minus Male)",
      caption  = "Source: ILOSTAT. Data: Most recent paired year between 2020 and 2023 per country with more than 1 data point. Gap taken respectively as percentage points."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey50", size = 9),
      plot.caption     = element_text(color = "grey50", size = 7),
      legend.position  = "bottom",
      panel.grid.minor = element_blank()
    )
}

care_responsibilities_share <- load_care_resp_share_data()
lfp <- load_lfp_data()
plot_care_country_gap(care_responsibilities_share)
plot_care_lfp_correlation(care_responsibilities_share, lfp)
