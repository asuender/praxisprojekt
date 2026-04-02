library(here)
library(checkmate)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

#' Load labour force participation and education data
#'
#' Reads the raw ILOSTAT dataset that combines labour force participation rates
#' with education categories.
#'
#' @return A \code{data.frame} with raw ILOSTAT observations.
load_labour_force_with_educ_data <- function() {
  read.csv(here("data", "raw", "ilo_labour_force_participation_and_education.csv.gz"))
}


#' Prepare labour force participation data
#'
#' Selects the relevant variables, restricts the dataset to ISCED-11 education
#' categories and binary sex labels, and removes missing rates.
#'
#' @param data A data frame returned by \code{load_labour_force_with_educ_data()}.
#' @return A data frame with harmonised labour force participation variables.
prepare_lfpr_data <- function(data) {
  assert_data_frame(data)

  data <- data %>% select(
    country = ref_area.label,
    data_source = source.label,
    year = time,
    education = classif1.label,
    sex = sex.label,
    rate = obs_value
  ) # Select and rename relevant variables
  data <- data %>% filter(
    str_detect(education, "ISCED-11"), # Only one education scale
    sex %in% c("Male", "Female"), # Only male/female rates
    !is.na(rate), # No NA´s for labor force particpation rate
  )
  data
}


#' Prepare 2024 totals for selected countries
#'
#' Filters the prepared labour force participation data to five countries, keeps
#' total education observations only, and renames countries for presentation.
#'
#' @param data A data frame created by \code{prepare_lfpr_data()}.
#' @return A data frame with one row per country-sex observation for 2024.
prepare_lfpr_total_2024_comp <- function(data) {
  assert_data_frame(data)

  relevant_countries <- c(
    "Tanzania, United Republic of",
    "Germany",
    "United States of America",
    "Iran (Islamic Republic of)",
    "Australia"
  ) # Select relevant countries

  data <- data %>%
    filter(
      country %in% relevant_countries, # Only selected countries
      str_detect(education, "Total"), # No education levels considered
      year == 2024 # Only data for 2024
    ) %>%
    mutate(country = recode(country,
      "Tanzania, United Republic of" = "Tanzania",
      "Germany" = "Germany",
      "United States of America" = "USA",
      "Iran (Islamic Republic of)" = "Iran",
      "Australia" = "Australia"
    ))
  data
}


#' Prepare Germany time series for total participation rates
#'
#' Filters the prepared labour force participation data to Germany, total
#' education observations, and the period from 2010 onward.
#'
#' @param data A data frame created by \code{prepare_lfpr_data()}.
#' @return A data frame with Germany observations by year and sex.
prepare_lfpr_GER_2010_to_2024 <- function(data) {
  assert_data_frame(data)

  data <- data %>% filter(
    country == "Germany", # Only Germany
    str_detect(education, "Total"), # No education levels considered
    year >= 2010 # From year 2010 onwards
  )
  data
}


#' Prepare 2024 participation rates by education level
#'
#' Filters the prepared labour force participation data to Germany and Iran in
#' 2024, removes broad or residual education categories, and standardises the
#' education labels.
#'
#' @param data A data frame created by \code{prepare_lfpr_data()}.
#' @return A data frame with country-, sex-, and education-specific rates.
prepare_lfpr_2024_by_education <- function(data) {
  assert_data_frame(data)

  data <- data %>%
    filter(
      year == 2024, # Only year 2024
      when_any(
        country == "Germany", # Only Germany and Iran
        country == "Iran (Islamic Republic of)"
      ),
      !str_detect(
        education, # Drop certain education levels
        "Total|No schooling|Early childhood|Post-secondary|Not elsewhere"
      )
    ) %>%
    mutate(country = recode(country, # Rename countries
      "Germany" = "Germany",
      "Iran (Islamic Republic of)" = "Iran"
    )) %>%
    mutate(
      education = str_remove(education, "^Education \\(ISCED-11\\):"),
      education = str_remove(education, "^\\s*\\d+\\.\\s*"),
      education = str_remove(education, " level$"),
      education = factor(
        education,
        levels = c(
          "Primary education",
          "Lower secondary education",
          "Upper secondary education",
          "Short-cycle tertiary education",
          "Bachelor's or equivalent",
          "Master's or equivalent",
          "Doctoral or equivalent"
        )
      )
    )

  data
}

#' Group education levels and recalculate participation rates
#'
#' Collapses detailed tertiary categories into one combined category and averages
#' labour force participation rates within each country-sex-education group.
#'
#' @param data A data frame created by \code{prepare_lfpr_2024_by_education()}.
#' @return A data frame with regrouped education categories and averaged rates.
group_educ_and_recalculate_rates <- function(data) {
  assert_data_frame(data)

  data <- data %>%
    mutate(education_new = case_when( # Regroup education levels
      str_detect(as.character(education), "equivalent|Short-cycle") ~ "Tertiary education",
      TRUE ~ education
    )) %>%
    mutate(education_new = factor(
      education_new,
      levels = c(
        "Primary education",
        "Lower secondary education",
        "Upper secondary education",
        "Tertiary education"
      )
    )) %>%
    group_by(country, sex, education_new) %>%
    summarise(
      rate = mean(rate, na.rm = TRUE),
      .groups = "drop"
    ) # Calculate aggregated lfpr for new education levels by country and sex

  data
}


#' Prepare detailed tertiary education observations
#'
#' Filters the prepared education-specific labour force participation data to
#' tertiary education categories only.
#'
#' @param data A data frame created by \code{prepare_lfpr_2024_by_education()}.
#' @return A data frame with tertiary education observations only.
tertiary_educ_detailed <- function(data) {
  assert_data_frame(data)

  data <- data %>%
    filter(str_detect(
      as.character(education), # Filter for tertiary education
      "Short-cycle|equivalent"
    )) %>%
    mutate(education = factor(
      education,
      levels = c(
        "Short-cycle tertiary education",
        "Bachelor's or equivalent",
        "Master's or equivalent",
        "Doctoral or equivalent"
      )
    ))
  data
}

#' Plot selected-country participation rates
#'
#' Creates a grouped bar chart for total labour force participation rates in the
#' selected countries for 2024.
#'
#' @param lf_total_2024_comparison A data frame created by
#'   \code{prepare_lfpr_total_2024_comp()}.
#' @return A \code{ggplot} object.
plot_lfpr_selected_countries <- function(lf_total_2024_comparison) {
  assert_data_frame(lf_total_2024_comparison)

  bar_width <- 0.7
  country_levels <- unique(lf_total_2024_comparison$country)

  plot_data <- lf_total_2024_comparison %>%
    mutate(
      country = factor(country, levels = country_levels),
      sex = factor(sex, levels = c("Female", "Male"))
    )

  ratio_data <- plot_data %>%
    select(country, sex, rate) %>%
    pivot_wider(names_from = sex, values_from = rate) %>%
    mutate(
      ratio = round(Male / Female, 2),
      label_y = Male + 4
    )

  upper_limit <- max(90, ceiling(max(ratio_data$label_y + 2, na.rm = TRUE) / 10) * 10)

  plot_data %>%
    ggplot(aes(x = country, y = rate, fill = sex)) +
    geom_col(
      position = "dodge",
      width = bar_width,
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    geom_text(
      data = ratio_data,
      aes(
        x = as.numeric(country) + bar_width / 4,
        y = label_y,
        label = paste0(ratio, "x")
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 4.5,
      color = "grey20"
    ) +
    labs(
      title = "Labor force participation rates",
      subtitle = "Selected countries | By sex | 2024",
      x = "Country",
      y = "Labor force participation rate (%)",
      fill = "Sex",
      caption = "Source: ILOSTAT."
    ) +
    scale_fill_sex() +
    scale_y_continuous(
      limits = c(0, upper_limit),
      breaks = seq(0, upper_limit, 20),
      expand = expansion(mult = c(0, 0.08))
    )
}

#' Plot Germany labour force participation over time
#'
#' Creates a time series plot of Germany's total labour force participation rate
#' by sex from 2010 onward.
#'
#' @param lf_GER_2010_to_2024 A data frame created by
#'   \code{prepare_lfpr_GER_2010_to_2024()}.
#' @return A \code{ggplot} object.
plot_lfpr_germany_time <- function(lf_GER_2010_to_2024) {
  assert_data_frame(lf_GER_2010_to_2024)

  lf_GER_2010_to_2024 %>%
    ggplot(aes(x = year, y = rate, color = sex)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Labor force participation rate",
      subtitle = "Germany | By sex | 2010-2024",
      x = "Year",
      y = "Labor force participation rate (%)",
      color = "Sex",
      caption = "Source: ILOSTAT."
    ) +
    scale_color_sex() +
    scale_x_continuous(limits = c(2010, 2025), breaks = c(2010, 2015, 2020, 2025)) +
    scale_y_continuous(limits = c(0, 70), breaks = seq(0, 70, 10))
}

#' Plot participation rates by grouped education level
#'
#' Creates a faceted bar chart of labour force participation rates by sex across
#' grouped education levels for one country.
#'
#' @param lf_2024_educ_grouped A data frame created by
#'   \code{group_educ_and_recalculate_rates()}.
#' @param country_name The country to plot.
#' @return A \code{ggplot} object.
plot_lfpr_by_education <- function(lf_2024_educ_grouped, country) {
  assert_data_frame(lf_2024_educ_grouped)
  assert_string(country)

  lf_2024_educ_grouped %>%
    filter(country == country) %>%
    ggplot(aes(x = country, y = rate, fill = sex)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    labs(
      title = "Labor force participation rate",
      subtitle = paste(country, "| By sex and education level | 2024"),
      x = NULL,
      y = "Labor force participation rate (%)",
      fill = "Sex",
      caption = "Source: ILOSTAT."
    ) +
    scale_fill_sex() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~education_new)
}

#' Plot participation rates across tertiary education levels
#'
#' Creates a faceted bar chart of labour force participation rates by sex across
#' detailed tertiary education categories for one country.
#'
#' @param lf_2024_tert_educ A data frame created by \code{tertiary_educ_detailed()}.
#' @param country_name The country to plot.
#' @return A \code{ggplot} object.
plot_lfpr_tertiary <- function(lf_2024_tert_educ, country) {
  assert_data_frame(lf_2024_tert_educ)

  lf_2024_tert_educ %>%
    filter(country == country) %>%
    ggplot(aes(x = country, y = rate, fill = sex)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    labs(
      title = "Labor force participation rate",
      subtitle = paste(country, "| By sex and tertiary education | 2024"),
      x = NULL,
      y = "Labor force participation rate (%)",
      fill = "Sex",
      caption = "Source: ILOSTAT."
    ) +
    scale_fill_sex() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~education)
}
