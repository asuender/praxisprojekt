library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(here)

#Load labor force participation raw data
load_labour_force_with_educ_data <- function() {
  read.csv(here("data", "raw", "ilo_labour_force_participation_and_education.csv.gz"))
}


#Function to prepare raw data
prepare_lfpr_data <- function(data) {
  data <- data %>% select(
    country = ref_area.label,
    data_source = source.label,
    year    = time,
    education = classif1.label,
    sex     = sex.label,
    rate   = obs_value
    )  #Select and rename relevant variables
  data <- data %>% filter(str_detect(education, "ISCED-11"), #Only one education scale
                          sex %in% c("Male", "Female"), #Only male/female rates
                           !is.na(rate), #No NA´s for labor force particpation rate
                          )
  data
}



#Prepare intermediate data for first analysis - five countries in year 2024
prepare_lfpr_total_2024_comp <- function(data) {
  relevant_countries <- c(
    "Tanzania, United Republic of" ,
    "Germany",
    "United States of America",
    "Iran (Islamic Republic of)",
    "Australia"
  ) #Select relevant countries

  data <- data %>% filter(country %in% relevant_countries, #Only selected countries
                          str_detect(education, "Total"), #No education levels considered
                          year == 2024 #Only data for 2024
  ) %>%
    mutate(country = recode(country,
                            "Tanzania, United Republic of" = "Tanzania" ,
                            "Germany" = "Germany",
                            "United States of America" = "USA",
                            "Iran (Islamic Republic of)" = "Iran",
                            "Australia" = "Australia"))
  data
}



#Function to prepare intermediate data for second analysis - Germany from 2010-2024
prepare_lfpr_GER_2010_to_2024 <- function(data) {
  data <- data%>% filter(country == "Germany", #Only Germany
                         str_detect(education, "Total"), #No education levels considered
                         year >= 2010 #From year 2010 onwards
  )
  data
}



#Function to prepare intermediate data for third analysis - LFPR by education level
prepare_lfpr_2024_by_education <- function(data) {
  data <- data %>% filter(year == 2024, #Only year 2024
                          when_any(country == "Germany", #Only Germany and Iran
                                   country == "Iran (Islamic Republic of)"),
                          !str_detect(education, #Drop certain education levels
                                      "Total|No schooling|Early childhood|Post-secondary|Not elsewhere")) %>%
    mutate(country = recode(country, #Rename countries
                            "Germany" = "Germany",
                            "Iran (Islamic Republic of)" = "Iran")) %>%
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

#Function to group education levels into four categories and calculate adjusted lfpr
group_educ_and_recalculate_rates <- function(data) {
  data <- data %>% mutate(education_new = case_when( #Regroup education levels
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
    summarise(rate = mean(rate, na.rm = TRUE),
              .groups = "drop") #Calculate aggregated lfpr for new education levels by country and sex

  data
}



#Function to prepare intermediate data for third analysis - LFPR for tertiary education
tertiary_educ_detailed <- function(data) {
  data <- data %>% filter(str_detect(as.character(education),  #Filter for tertiary education
                                     "Short-cycle|equivalent")) %>%
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

plot_lfpr_selected_countries <- function(lf_total_2024_comparison) {
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
      size = 3.2,
      color = "grey20"
    ) +
    labs(
      title = "Labor force participation rates",
      subtitle = "Selected countries | By sex | 2024",
      x = "Country",
      y = "Labor force participation rate (%)",
      fill = "Sex",
      caption = paste0(
        "Source: ILOSTAT.\n",
        "Labels above the male bars show the male-to-female participation rate ratio."
      )
    ) +
    scale_fill_sex() +
    scale_y_continuous(
      limits = c(0, upper_limit),
      breaks = seq(0, upper_limit, 20),
      expand = expansion(mult = c(0, 0.08))
    )
}

plot_lfpr_germany_time <- function(lf_GER_2010_to_2024) {
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

plot_lfpr_by_education <- function(lf_2024_educ_grouped, country_name) {
  lf_2024_educ_grouped %>%
    filter(country == country_name) %>%
    ggplot(aes(x = country, y = rate, fill = sex)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    labs(
      title = "Labor force participation rate",
      subtitle = paste(country_name, "| By sex and education level | 2024"),
      x = NULL,
      y = "Labor force participation rate (%)",
      fill = "Sex",
      caption = "Source: ILOSTAT."
    ) +
    scale_fill_sex() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~ education_new)
}

plot_lfpr_tertiary <- function(lf_2024_tert_educ, country_name) {
  lf_2024_tert_educ %>%
    filter(country == country_name) %>%
    ggplot(aes(x = country, y = rate, fill = sex)) +
    geom_bar(
      stat = "identity",
      position = "dodge",
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    labs(
      title = "Labor force participation rate",
      subtitle = paste(country_name, "| By sex and tertiary education | 2024"),
      x = NULL,
      y = "Labor force participation rate (%)",
      fill = "Sex",
      caption = "Source: ILOSTAT.\nRestricted to detailed tertiary education categories."
    ) +
    scale_fill_sex() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    facet_wrap(~ education)
}
