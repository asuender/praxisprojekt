library(here)
library(checkmate)
library(data.table)

#' Load ILOSTAT care responsibility inactivity data
#'
#' Reads the ILOSTAT dataset on the share of people outside the labor force due
#' to care responsibilities.
#'
#' @return A \code{data.table} with country-year-sex observations from ILOSTAT.
load_care_resp_share_data <- function() {
  fread(here("data", "raw", "care_responsibility_share.csv.gz"))
}

#' Load ILOSTAT labour force participation data
#'
#' Reads the raw ILOSTAT labour force participation dataset from
#' \code{data/raw/}.
#'
#' @return A \code{data.table} with labour force participation observations.
load_lfp_data <- function() {
  fread(here("data", "raw", "ilo_labour_force_participation.csv.gz"))
}

#' Plot care-related inactivity shares for selected countries
#'
#' Filters the care responsibility dataset to selected countries and plots female
#' and male shares over time in faceted panels.
#'
#' @param data A \code{data.table} returned by
#'   \code{load_care_resp_share_data()}.
#' @return A \code{ggplot} object.
plot_care_share_facet <- function(dt) {
  assert_data_table(dt)

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
  dt[, sex := factor(sex, levels = c("Female", "Male"))]
  ggplot(dt, aes(x = year, y = value, color = sex, group = sex)) +
    geom_line() +
    geom_point(size = 1.5) +
    ylim(0, 100) +
    scale_color_sex() +
    facet_wrap(~country, ncol = 3) +
    labs(
      title    = "Share of people outside the labor force due to care responsibilities",
      subtitle = "Selected countries | 2005 onward",
      x        = NULL,
      y        = "Share (%)",
      color    = "Sex",
      caption  = "Source: ILOSTAT."
    ) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      panel.spacing   = unit(1, "lines")
    )
}


#' Plot country-level care inactivity gaps
#'
#' Computes the latest paired female-male care responsibility gap per country,
#' keeps the countries with the lowest and highest gaps, and displays them as a
#' lollipop chart.
#'
#' @param dt A \code{data.table} returned by
#'   \code{load_care_resp_share_data()}.
#' @param n_countries Number of countries to show in total.
#' @return A \code{ggplot} object.
plot_care_country_gap <- function(dt, n_countries = 20) {
  assert_data_table(dt)
  assert_count(n_countries)

  dt <- dt[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(
      country = ref_area.label, year = time, sex = sex.label,
      value = obs_value
    )
  ]

  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= 2, country]]

  dt_paired <- dt[year <= 2023, if (all(c("Male", "Female") %in% sex)) .SD,
    by = .(country, year)
  ]
  dt_last_year <- dt_paired[, .(last_year = max(year)), by = country]
  dt_paired <- dt_paired[dt_last_year, on = "country"][year == last_year]

  dt_paired <- dt_paired[year >= 2020]

  dt_wide <- dcast(dt_paired, country + year ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, gap := Female - Male]

  setorder(dt_wide, gap)
  selected <- c(
    head(dt_wide$country, n_countries / 2),
    tail(dt_wide$country, n_countries / 2)
  )
  dt_plot <- dt_wide[country %in% selected]
  dt_plot[, highlight := fcase(
    country %in% tail(dt_wide$country, n_countries / 2), "High care gap (top 10)",
    country %in% head(dt_wide$country, n_countries / 2), "Low care gap (bottom 10)"
  )]

  dt_plot[, country_label := paste0(country, " (", year, ")")]

  ggplot(dt_plot, aes(
    x = gap,
    y = reorder(country_label, gap),
    color = highlight
  )) +
    geom_vline(xintercept = 0, color = "grey50", linetype = "dashed") +
    geom_point(
      aes(fill = highlight),
      size = 3.2,
      shape = 21,
      stroke = 0.45,
      color = unname(config.palette.presentation$ink)
    ) +
    geom_segment(
      aes(
        x = 0, xend = gap,
        y = reorder(country_label, gap),
        yend = reorder(country_label, gap)
      ),
      linewidth = 0.5, alpha = 0.5
    ) +
    scale_color_manual(values = config.palette.care.highlight, name = NULL) +
    scale_fill_manual(values = config.palette.care.highlight, name = NULL) +
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.05, 0.08))
    ) +
    labs(
      title    = "Gender gap in care-related labour inactivity",
      subtitle = "Selected countries | Most recent paired year in 2020-2023",
      x        = "Female minus male share (%)",
      y        = NULL,
      caption  = "Source: ILOSTAT."
    ) +
    theme(
      legend.position    = "none",
      axis.text.y        = element_text(size = 12),
      panel.grid.major.y = element_line(color = "grey92"),
      panel.grid.major.x = element_line(color = "grey92"),
      panel.grid.minor   = element_blank()
    )
}


#' Plot the relationship between care gaps and labour force gaps
#'
#' Combines the latest paired care-related inactivity gaps with labour force
#' participation gaps and highlights countries with especially low or high care
#' gaps.
#'
#' @param dt A \code{data.table} returned by
#'   \code{load_care_resp_share_data()}.
#' @param lfp_data A \code{data.table} returned by
#'   \code{load_lfp_data()}.
#' @param n_countries Number of highlighted countries used to define the high and
#'   low gap groups.
#' @return A \code{ggplot} object.
plot_care_lfp_correlation <- function(dt, lfp_data, n_countries = 20) {
  assert_data_table(dt)
  assert_data_table(lfp_data)
  assert_count(n_countries)

  dt <- dt[
    sex.label %in% c("Male", "Female") & !is.na(obs_value),
    .(country = ref_area.label, year = time, sex = sex.label, value = obs_value)
  ]
  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= 2, country]]
  dt_paired <- dt[year <= 2023, if (all(c("Male", "Female") %in% sex)) .SD,
    by = .(country, year)
  ]
  dt_last <- dt_paired[, .(last_year = max(year)), by = country]
  dt_paired <- dt_paired[dt_last, on = "country"][year == last_year]
  dt_paired <- dt_paired[year >= 2020]
  dt_wide <- dcast(dt_paired, country + year ~ sex, value.var = "value")
  dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
  dt_wide[, care_gap := Female - Male]

  setorder(dt_wide, care_gap)
  low_gap_countries <- head(dt_wide$country, n_countries / 2)
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
    by = c("country", "year")
  )

  merged[, highlight := fcase(
    country %in% high_gap_countries, "High care gap (top 10)",
    country %in% low_gap_countries,  "Low care gap (bottom 10)",
    default = "Other"
  )]

  top10_merged <- merged[highlight == "High care gap (top 10)"]
  setorder(top10_merged, lfp_gap)
  label_countries <- head(top10_merged$country, 2)

  sp_rho <- round(cor(merged$care_gap, merged$lfp_gap,
    method = "spearman", use = "complete.obs"
  ), 3)

  ggplot(merged, aes(x = lfp_gap, y = care_gap)) +
    geom_point(
      aes(
        fill = highlight,
        size = ifelse(highlight == "Other", 2.0, 2.8),
        alpha = ifelse(highlight == "Other", 0.4, 0.9)
      ),
      shape = 21,
      color = unname(config.palette.presentation$ink),
      stroke = 0.45
    ) +
    scale_size_identity() +
    scale_alpha_identity() +
    ggrepel::geom_text_repel(
      data = merged[country %in% label_countries],
      aes(label = country),
      color = unname(config.palette.care.highlight["High care gap (top 10)"]),
      size = 4.5,
      fontface = "bold",
      box.padding = 0.5,
      point.padding = 0.3,
      segment.color = "grey60",
      segment.size = 0.3,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = config.palette.care.highlight,
      name = NULL
    ) +
    guides(fill = guide_legend(override.aes = list(
      size = 4.5,
      alpha = 1,
      shape = 21,
      stroke = 0.45,
      color = unname(config.palette.presentation$ink)
    ))) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title = "Care-related labour inactivity gap vs labour force participation rate gap",
      subtitle = paste0(
        "Most recent paired year, 2020-2023 | One point = one country | Spearman rho = ",
        sp_rho
      ),
      x = "Labour force participation rate gap\n(Male minus Female)",
      y = "Care-related labour inactivity gap\n(Female minus Male)",
      caption = "Source: ILOSTAT."
    ) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}
