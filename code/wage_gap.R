library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

#' Load ILO hourly wage data and build a balanced panel with wage gap
#'
#' Reads the ILO average hourly wages CSV, filters to USD wages for Male/Female
#' between 2010 and 2023, pivots to wide format, and retains only countries
#' with data for every year in the range. Computes the gender wage gap as
#' (Male - Female) / Male.
#'
#' @return A data frame with columns: ref_area.label, time, Male, Female, wage_gap.
load_wage_gap_data <- function() {
  data <- read.csv(here("data", "raw", "ilo_avg_hourly_wages_usd.csv.gz"))

  data_filtered <- data %>%
    filter(
      classif1.label == "Currency: U.S. dollars",
      sex.label %in% c("Male", "Female"),
      time >= 2010,
      time <= 2023
    )

  wage_wide <- data_filtered %>%
    select(ref_area.label, time, sex.label, obs_value) %>%
    pivot_wider(names_from = sex.label, values_from = obs_value) %>%
    filter(!is.na(Male), !is.na(Female))

  all_years <- sort(unique(wage_wide$time))
  n_total_years <- length(all_years)

  balanced_countries <- wage_wide %>%
    group_by(ref_area.label) %>%
    summarise(n_years = n_distinct(time), .groups = "drop") %>%
    filter(n_years == n_total_years)

  wage_wide %>%
    filter(ref_area.label %in% balanced_countries$ref_area.label) %>%
    mutate(wage_gap = (Male - Female) / Male)
}

#' Aggregate wage gap trend over time
#'
#' @param data A data frame returned by \code{load_wage_gap_data}.
#' @return A data frame with columns: time, median_wage_gap, mean_wage_gap, n_countries.
prepare_wage_gap_trend <- function(data) {
  data %>%
    group_by(time) %>%
    summarise(
      median_wage_gap = median(wage_gap, na.rm = TRUE),
      mean_wage_gap = mean(wage_gap, na.rm = TRUE),
      n_countries = n_distinct(ref_area.label),
      .groups = "drop"
    )
}

#' Select top N countries by wage gap for a given year
#'
#' @param data A data frame returned by \code{load_wage_gap_data}.
#' @param year The year to filter on.
#' @param n Number of countries to return (default 10).
#' @return A data frame with the top N rows ordered by descending wage_gap.
prepare_wage_gap_top_countries <- function(data, year, n = 10) {
  data %>%
    filter(time == year) %>%
    arrange(desc(wage_gap)) %>%
    slice_head(n = n)
}

#' Compute wage gap change between two years
#'
#' @param data A data frame returned by \code{load_wage_gap_data}.
#' @param year1 Start year.
#' @param year2 End year.
#' @param n Number of countries to return (default 10), ordered by absolute change.
#' @return A wide data frame with columns for each year's gap, change, and abs_change.
prepare_wage_gap_change <- function(data, year1, year2, n = 10) {
  data %>%
    filter(time %in% c(year1, year2)) %>%
    select(ref_area.label, time, wage_gap) %>%
    pivot_wider(names_from = time, values_from = wage_gap) %>%
    filter(
      !is.na(.data[[as.character(year1)]]),
      !is.na(.data[[as.character(year2)]])
    ) %>%
    mutate(
      change = .data[[as.character(year2)]] - .data[[as.character(year1)]],
      abs_change = abs(change)
    ) %>%
    arrange(desc(abs_change)) %>%
    slice_head(n = n)
}

#' Plot median wage gap trend over time
#'
#' @param trend_data A data frame returned by \code{prepare_wage_gap_trend}.
plot_wage_gap_trend <- function(trend_data) {
  ggplot(trend_data, aes(x = time, y = median_wage_gap)) +
    geom_line(linewidth = 1.2, color = "steelblue") +
    geom_point(size = 2.5, color = "steelblue") +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = "Global Median Gender Wage Gap Over Time (Balanced Countries)",
      x = "Year",
      y = "Median Gender Wage Gap"
    ) +
    theme_minimal()
}

#' Bar chart of countries with the highest wage gap in a given year
#'
#' @param data A data frame returned by \code{prepare_wage_gap_top_countries}.
#' @param year The year (used in the plot title).
plot_wage_gap_top_countries <- function(data, year) {
  ggplot(data, aes(x = reorder(ref_area.label, wage_gap), y = wage_gap)) +
    geom_col(fill = "tomato") +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste0("Top 10 Countries with Highest Gender Wage Gap (", year, ")"),
      x = "Country",
      y = "Gender Wage Gap"
    ) +
    theme_minimal()
}

#' Bar chart of countries with the largest change in wage gap
#'
#' @param data A data frame returned by \code{prepare_wage_gap_change}.
#' @param year1 Start year (used in the plot title).
#' @param year2 End year (used in the plot title).
plot_wage_gap_change <- function(data, year1, year2) {
  ggplot(data, aes(x = reorder(ref_area.label, change), y = change, fill = change > 0)) +
    geom_col() +
    coord_flip() +
    scale_fill_manual(values = c("TRUE" = "tomato", "FALSE" = "steelblue")) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      title = paste0(
        "Top 10 Countries with Largest Change in Gender Wage Gap (",
        year1, "-", year2, ")"
      ),
      x = "Country",
      y = "Change in Gender Wage Gap"
    ) +
    theme_minimal() +
    guides(fill = "none")
}
