library(ggplot2)
library(data.table)
library(here)
library(countrycode)

#' Load OWID gender wage gap data
#'
#' Reads the raw Our World in Data wage gap dataset from \code{data/raw/}.
#'
#' @return A \code{data.table} with country-year wage gap observations.
load_gender_wage_gap_data <- function() {
  fread(here("data", "raw", "owid_gender_wage_gap.csv"))
}


#' Prepare regional wage gap summaries
#'
#' Collapses the OWID wage gap series to country medians and then computes
#' regional medians and interquartile ranges.
#'
#' @param owid_data A data frame or \code{data.table} containing the raw OWID
#'   wage gap data.
#' @param min_years Minimum number of yearly observations required per country.
#'   Currently unused.
#' @return A \code{data.table} with one row per region and summary statistics
#'   for the wage gap.
prepare_wage_gap_regional_data <- function(owid_data, min_years = 1) {
  dt <- as.data.table(owid_data)[, .(
    country = entity,
    country_code = code,
    year = year,
    gap = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total
  )]
  dt <- dt[!is.na(gap) & year >= 2000 & year <= 2025]

  # Add region using countrycode
  dt[, region := countrycode(country_code, origin = "iso3c", destination = "continent")]
  dt <- dt[!is.na(region)]

  # Calculate per-country medians
  dt_country <- dt[, .(
    gap = median(gap, na.rm = TRUE),
    region = region[1]
  ), by = country]

  # Calculate regional statistics
  dt_region <- dt_country[, .(
    gap_med = median(gap, na.rm = TRUE),
    gap_q1 = quantile(gap, 0.25, na.rm = TRUE),
    gap_q3 = quantile(gap, 0.75, na.rm = TRUE),
    n_countries = uniqueN(country)
  ), by = region]

  dt_region[, region_label := paste0(region, "\nn = ", n_countries)]
  dt_region
}


#' Plot the regional wage gap distribution
#'
#' Visualises median regional wage gaps together with interquartile ranges based
#' on country-level medians.
#'
#' @param owid_data A data frame or \code{data.table} containing the raw OWID
#'   wage gap data.
#' @return A \code{ggplot} object.
plot_wage_gap_distribution <- function(owid_data) {
  dt_region <- prepare_wage_gap_regional_data(owid_data)

  # Custom order: Asia, Africa, Oceania, Europe, Americas
  custom_order <- c("Asia", "Africa", "Oceania", "Europe", "Americas")
  dt_region[, region_label := factor(
    region_label,
    levels = dt_region[match(custom_order, region), region_label]
  )]

  n_countries <- sum(dt_region$n_countries)

  # Add color column based on sign
  dt_region[, bar_color := ifelse(gap_med >= 0,
    config.palette.presentation$male,
    config.palette.presentation$female
  )]

  ggplot(dt_region, aes(x = region_label, y = gap_med)) +
    geom_col(
      aes(fill = bar_color),
      width = 0.6,
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    geom_errorbar(
      aes(ymin = gap_q1, ymax = gap_q3),
      width = 0.25,
      linewidth = 0.5,
      color = "black"
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_text(
      aes(label = paste0(round(gap_med, 1), "%"), y = gap_q3),
      vjust = -1.5,
      size = 4.5,
      fontface = "bold",
      color = "grey20"
    ) +
    scale_fill_identity() +
    scale_y_continuous(
      labels = function(x) paste0(x, "%"),
      expand = expansion(mult = c(0.05, 0.15))
    ) +
    labs(
      title    = "Gender wage gap by region",
      subtitle = paste0(n_countries, " countries | By region | Median of country medians | 2000-2025"),
      x        = NULL,
      y        = "Wage gap (%)",
      caption  = "Source: Our World in Data.",
    ) +
    theme(
      axis.text.x        = element_text(angle = 35, hjust = 1, face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position    = "none"
    )
}

#' Plot wage gap trends for selected countries
#'
#' Filters the OWID wage gap data to a fixed set of countries and shows their
#' annual time series in small multiples.
#'
#' @param owid_data A data frame or \code{data.table} containing the raw OWID
#'   wage gap data.
#' @return A \code{ggplot} object.
plot_wage_gap_facet <- function(owid_data) {
  dt <- as.data.table(owid_data)[, .(
    country = entity,
    year    = year,
    gap     = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total
  )]
  dt <- dt[!is.na(gap) & year >= 2000 & year <= 2025]

  country_map <- c(
    "United States" = "United States",
    "United Kingdom" = "United Kingdom",
    "Portugal" = "Portugal",
    "Mexico" = "Mexico",
    "Egypt" = "Egypt",
    "Colombia" = "Colombia",
    "South Korea" = "South Korea",
    "Philippines" = "Philippines",
    "South Africa" = "South Africa"
  )

  dt <- dt[country %in% names(country_map)]
  dt[, country := factor(country, levels = unname(country_map))]

  ggplot(dt, aes(x = year, y = gap)) +
    geom_line(color = config.palette.presentation$economic) +
    geom_point(size = 1.5, color = config.palette.presentation$economic) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    facet_wrap(~country, ncol = 3) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 6)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Gender wage gap over time",
      subtitle = "Selected countries | 2000-2025",
      x        = NULL,
      y        = "Anual wage gap (%)",
      caption  = "Source: Our World in Data."
    ) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1),
      panel.spacing   = grid::unit(1, "lines")
    )
}

#' Plot the relationship between wage gap and GII
#'
#' Joins the most recent wage gap observations with the most recent gender
#' inequality index values and visualises their cross-country association.
#'
#' @param dt_gii A data frame or \code{data.table} with gender inequality index
#'   values.
#' @param owid_data A data frame or \code{data.table} containing the raw OWID
#'   wage gap data.
#' @return A \code{ggplot} object.
plot_wage_gap_gii_correlation <- function(dt_gii, owid_data) {
  dt_gap <- as.data.table(owid_data)[
    !is.na(gender_wage_gap_by_occupation__classif1_occupation__skill_level__total) &
      year >= 2020 & year <= 2025
  ]
  setnames(dt_gap, "entity", "country")
  dt_gap <- dt_gap[, .SD[which.max(year)], by = country][, .(country, gap = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total)]

  dt_gii <- dt_gii[
    !is.na(value) & year >= 2020 & year <= 2025
  ][, .SD[which.max(year)], by = country][, .(country, gii = value)]

  merged <- merge(dt_gap, dt_gii, by = "country")

  sp_test <- cor.test(merged$gap, merged$gii, method = "spearman", exact = FALSE)
  sp_rho <- round(sp_test$estimate, 3)

  ggplot(merged, aes(x = gii, y = gap)) +
    geom_point(
      aes(fill = gap > 0),
      size = 2.5,
      alpha = 0.75,
      shape = 21,
      color = unname(config.palette.presentation$ink),
      stroke = 0.45
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_fill_presentation_binary(
      negative_label = "Women earn more",
      positive_label = "Women earn less",
      negative = config.palette.presentation$female,
      positive = config.palette.presentation$male,
      name = NULL
    ) +
    guides(fill = guide_legend(override.aes = list(
      size = 4.5,
      alpha = 1,
      shape = 21,
      stroke = 0.45,
      color = unname(config.palette.presentation$ink)
    ))) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title = "Gender wage gap vs gender inequality index",
      subtitle = paste0(
        "79 countries | Most recent year, 2020-2025 | Spearman rho = ",
        sp_rho
      ),
      x = "Gender inequality index",
      y = "Wage gap (%)",
      caption = "Source: Our World in Data."
    ) +
    theme(
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}
