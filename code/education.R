library(data.table)
library(checkmate)
library(here)
library(countrycode)

#' Load OWID Gender Inequality Index data
#'
#' Reads the raw OWID GII CSV and standardises column names.
#'
#' @return A \code{data.table} with columns: country, countryCode, year, value
#'   (the GII score).
load_owid_gii_data <- function() {
  dt <- fread(here("data", "raw", "owid_gender_inequality_index.csv"))
  setnames(dt, c("entity", "code", "gii"), c("country", "country_code", "value"))
  dt
}

#' Prepare raw UIS indicator data from ZIP archives
#'
#' Extracts and row-binds the \code{data.csv} files contained in each ZIP
#' archive. Derives a \code{sex} factor from the list names and maps ISO-3
#' country codes to readable country names via \pkg{countrycode}.
#'
#' @param files A named list where names encode \code{<indicator>_<sex>} and
#'   values are filenames of ZIP archives located in \code{data/raw/}.
#' @return A \code{data.table} with an \code{indicator} column (from list
#'   names), \code{sex} factor, \code{countryCode}, \code{country}, and all
#'   original CSV columns.
prepare_uis_data <- function(files) {
  assert_list(files, names = "named")

  dt <- rbindlist(lapply(files, function(f) {
    fread(cmd = paste("unzip -p", here("data", "raw", f), "'*/data.csv'"))
  }), idcol = "indicator")

  dt[, sex := sub(".*_", "", indicator)]
  dt[, sex := factor(sex, levels = c("female", "male"), labels = c("Female", "Male"))]

  setnames(dt, "geoUnit", "country_code")
  dt[, country := countrycode(country_code, origin = "iso3c", destination = "country.name")]

  dt
}

#' Load UIS completion rates by education level and sex
#'
#' Reads six ZIP archives (primary, lower secondary, upper secondary - each for
#' female and male) via \code{\link{prepare_uis_data}}, derives the education
#' level from the indicator name, and keeps only country-year-level
#' combinations where both sexes are present.
#'
#' @return A \code{data.table} with columns: education_level, country_code,
#'   country, sex, year, value (completion rate in percent).
load_uis_completion_rates <- function() {
  files <- list(
    primary_female   = "uis_completion_rate_primary_female.zip",
    primary_male     = "uis_completion_rate_primary_male.zip",
    lower_sec_female = "uis_completion_rate_lower_secondary_female.zip",
    lower_sec_male   = "uis_completion_rate_lower_secondary_male.zip",
    upper_sec_female = "uis_completion_rate_upper_secondary_female.zip",
    upper_sec_male   = "uis_completion_rate_upper_secondary_male.zip"
  )

  dt <- prepare_uis_data(files)

  dt[, education_level := sub("_[^_]+$", "", indicator)]
  dt[, education_level := factor(fcase(
    education_level == "primary",   "Primary",
    education_level == "lower_sec", "Lower secondary",
    education_level == "upper_sec", "Upper secondary"
  ), levels = c("Primary", "Lower secondary", "Upper secondary"))]
  dt <- dt[, if (.N == 2) .SD, by = .(country, year, education_level)]

  dt[, .(education_level, country_code, country, sex, year, value)]
}

#' Load OWID gross enrollment data and compute the Gender Parity Index
#'
#' Reads OWID gross enrollment CSVs for four education levels (primary, lower
#' secondary, upper secondary, tertiary), identifies the female and male
#' columns heuristically, and computes the GPI as \code{female / male}.
#' Although OWID's data is also based on UIS, they modified certain columns
#' during their own data cleaning process, which makes it necessary to handle
#' that separately (hence we cannot use the \code{\link{prepare_uis_data}} function).
#'
#' @return A \code{data.table} with columns: education_level, country_code,
#'   country, year, value (GPI ratio).
load_owid_gpi_data <- function() {
  files <- list(
    "Primary"         = "owid_gross_enrolment_primary.csv",
    "Lower secondary" = "owid_gross_enrolment_lower_secondary.csv",
    "Upper secondary" = "owid_gross_enrolment_upper_secondary.csv",
    "Tertiary"        = "owid_gross_enrolment_tertiary.csv"
  )

  dt <- rbindlist(lapply(names(files), function(level) {
    dt <- fread(here("data", "raw", files[[level]]))
    cols <- names(dt)
    col_female <- grep("fe", cols, ignore.case = TRUE, value = TRUE)
    col_male <- setdiff(cols[4:5], col_female)

    setnames(dt, c(cols[1:2], col_female, col_male), c("country", "country_code", "female", "male"))
    dt[, level := level]
    dt
  }), fill = TRUE)

  dt[, value := female / male]
  dt[, education_level := factor(level, levels = c(
    "Pre-primary", "Primary", "Lower secondary", "Upper secondary", "Tertiary"
  ))]

  dt[, .(education_level, country_code, country, year, value)]
}

#' Prepare world map data for the gender inequality index
#'
#' Filters the gender inequality index data to a given year and joins it to a
#' world geometry object by ISO-3 country code.
#'
#' @param gii A \code{data.table} with GII values.
#' @param world An \code{sf} object with an \code{iso_a3} column.
#' @param year The year to keep.
#' @return An \code{sf} object with joined GII values.
prepare_gii_world_map_data <- function(gii, world, year = 2023) {
  assert_data_table(gii)
  assert_class(world, "sf")
  assert_count(year)

  target_year <- year
  gii_latest <- gii[year == target_year]
  merge(world, gii_latest, by.x = "iso_a3", by.y = "country_code", all.x = TRUE)
}

#' Prepare global completion averages for the latest year
#'
#' Keeps countries with complete female and male observations across all three
#' education levels in the selected year and averages completion rates by sex and
#' level.
#'
#' @param completion A \code{data.table} with completion rate observations.
#' @param year The year to summarise.
#' @return A \code{data.table} with average completion rates by education level
#'   and sex.
prepare_completion_global_latest <- function(completion, year = 2021) {
  assert_data_table(completion)
  assert_count(year)

  target_year <- year

  balanced_countries <- completion[year == target_year,
    if (uniqueN(education_level) == 3 && uniqueN(sex) == 2) .SD,
    by = country
  ]$country |> unique()

  completion[year == target_year & country %in% balanced_countries,
    .(value = mean(value)),
    by = .(education_level, sex)
  ]
}

#' Prepare completion trends for selected countries
#'
#' Averages completion rates across education levels for selected countries over
#' a given year range.
#'
#' @param completion A \code{data.table} with completion rate observations.
#' @param countries A character vector of country names to include.
#' @param year_range A numeric vector of length two giving the start and end
#'   year.
#' @return A \code{data.table} with country-year-sex averages.
prepare_completion_countries_time <- function(
  completion,
  countries,
  year_range = c(2005, 2020)
) {
  assert_data_table(completion)
  assert_character(countries, any.missing = FALSE)
  assert_integerish(year_range, lower = 0, len = 2, any.missing = FALSE)

  completion_countries_time <- completion[
    between(year, year_range[1], year_range[2]) & country %in% countries,
    .(value = mean(value)),
    by = .(country, year, sex)
  ]

  completion_countries_time[, country := factor(country, levels = countries)]
  completion_countries_time
}

#' Plot a global map of the gender inequality index
#'
#' Visualises country-level gender inequality index values on a world map.
#'
#' @param gii_world An \code{sf} object produced by
#'   \code{prepare_gii_world_map_data()}.
#' @return A \code{ggplot} object.
plot_gii_map <- function(gii_world) {
  assert_class(gii_world, "sf")

  ggplot(gii_world) +
    geom_sf(aes(fill = value), color = "grey30", linewidth = 0.1) +
    scale_fill_presentation_sequential(
      name = "GII", na.value = "grey80",
      limits = c(0, 1)
    ) +
    coord_sf(ylim = c(-56, 90)) +
    labs(
      title = "Gender inequality index",
      subtitle = "Global coverage | 2023",
      caption = "Source: UNDP via Our World in Data.\nHigher values indicate greater inequality towards women."
    ) +
    theme(legend.text = element_text(size = 12), axis.text = element_blank(), axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
}

#' Plot a global map of the gender parity index
#'
#' Filters the gross enrolment-based gender parity index to the latest available
#' year in 2020-2021 for one education level and plots it on a world map.
#'
#' @param gpi A \code{data.table} with GPI values.
#' @param world An \code{sf} object with an \code{iso_a3} column.
#' @param level The education level to plot.
#' @return A \code{ggplot} object.
plot_gpi_map <- function(gpi, world, level) {
  assert_data_table(gpi)
  assert_class(world, "sf")
  assert_string(level)

  gpi_filtered <- gpi[year %in% c(2020, 2021) & education_level == level]
  gpi_filtered <- gpi_filtered[, .SD[which.max(year)], by = .(country_code, country)]
  world_merged <- merge(world, gpi_filtered, by.x = "iso_a3", by.y = "country_code", all.x = TRUE)

  ggplot(world_merged) +
    geom_sf(aes(fill = value), color = "grey30", linewidth = 0.1) +
    scale_fill_presentation_diverging(
      name = "GPI", na.value = "grey80",
      low = unname(config.palette.sex["Female"]),
      high = unname(config.palette.sex["Male"]),
      midpoint = 1,
      limits = c(0.5, 1.5), oob = scales::squish
    ) +
    coord_sf(ylim = c(-56, 90)) +
    labs(
      title = "Gender parity index",
      subtitle = "Latest available year in 2020-2021 | Female-to-male gross enrollment ratio"
    ) +
    theme(
      legend.title = element_text(vjust = 0.75),
      legend.text = element_text(size = 12),
      axis.ticks = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank()
    )
}

#' Plot one education-level GPI map panel
#'
#' Wraps \code{plot_gpi_map()} for use in the combined multi-panel layout.
#'
#' @param gpi A \code{data.table} with GPI values.
#' @param world An \code{sf} object with an \code{iso_a3} column.
#' @param level The education level to plot.
#' @return A \code{ggplot} object.
plot_gpi_subplot <- function(gpi, world, level) {
  assert_data_table(gpi)
  assert_class(world, "sf")
  assert_string(level)

  plot_gpi_map(gpi, world, level) +
    labs(title = level, subtitle = NULL) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 3)),
      plot.margin = margin(t = 8, r = 2, b = 2, l = 2)
    )
}

#' Plot combined GPI maps across education levels
#'
#' Arranges four education-specific GPI maps into a shared layout with a common
#' legend and annotation.
#'
#' @param gpi A \code{data.table} with GPI values.
#' @param world An \code{sf} object with an \code{iso_a3} column.
#' @return A patchwork plot object.
plot_gpi_combined <- function(gpi, world) {
  assert_data_table(gpi)
  assert_class(world, "sf")

  (plot_gpi_subplot(gpi, world, "Primary") +
    plot_gpi_subplot(gpi, world, "Lower secondary")) /
    (plot_gpi_subplot(gpi, world, "Upper secondary") +
      plot_gpi_subplot(gpi, world, "Tertiary")) +
    plot_layout(guides = "collect") +
    plot_annotation(
      title = "Gender parity index",
      subtitle = "Global coverage | By education level | Latest available year in 2020-2021",
      caption = paste(
        "Source: Our World in Data.\n",
        "Values show the female-to-male gross enrollment ratio; 1 indicates parity."
      )
    ) &
    theme(
      legend.position = "bottom",
      legend.key.width = unit(2, "cm")
    )
}

#' Plot average completion rates by education level and sex
#'
#' Creates a grouped bar chart from the output of
#' \code{prepare_completion_global_latest()}.
#'
#' @param completion_global_latest A \code{data.table} with average completion
#'   rates by education level and sex.
#' @return A \code{ggplot} object.
plot_completion_global <- function(completion_global_latest) {
  assert_data_table(completion_global_latest)

  ggplot(completion_global_latest, aes(x = education_level, y = value, fill = sex)) +
    geom_bar(
      position = "dodge",
      stat = "identity",
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    ylim(0, 100) +
    scale_fill_sex() +
    labs(
      title = "Average completion rates",
      subtitle = "49 countries | By sex and education level | 2021",
      x = "Education level",
      y = "Average completion rate (%)",
      fill = "Sex",
      caption = "Source: UNESCO Institute of Statistics."
    )
}

#' Plot completion rate trends for selected countries
#'
#' Creates faceted time series of completion rates averaged across education
#' levels.
#'
#' @param completion_countries_time A \code{data.table} created by
#'   \code{prepare_completion_countries_time()}.
#' @return A \code{ggplot} object.
plot_completion_countries_time <- function(completion_countries_time) {
  assert_data_table(completion_countries_time)

  ggplot(completion_countries_time, aes(x = year, y = value, color = sex)) +
    geom_line() +
    geom_point() +
    ylim(40, 100) +
    scale_color_sex() +
    facet_wrap(~country, nrow = 2) +
    theme(panel.spacing.x = unit(2.5, "lines")) +
    labs(
      title = "Completion rates",
      subtitle = "Selected countries | Averaged across education levels | 2005-2020",
      x = "Year",
      y = "Average completion rate (%)",
      color = "Sex",
      caption = "Source: UNESCO Institute of Statistics."
    )
}
