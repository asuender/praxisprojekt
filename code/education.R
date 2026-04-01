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
  DT <- fread(here("data", "raw", "owid_gender_inequality_index.csv"))
  setnames(DT, c("entity", "code", "gii"), c("country", "countryCode", "value"))
  DT
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
  DT <- rbindlist(lapply(files, function(f) {
    fread(cmd = paste("unzip -p", here("data", "raw", f), "'*/data.csv'"))
  }), idcol = "indicator")

  DT[, sex := sub(".*_", "", indicator)]
  DT[, sex := factor(sex, levels = c("female", "male"), labels = c("Female", "Male"))]

  setnames(DT, "geoUnit", "countryCode")
  DT[, country := countrycode(countryCode, origin = "iso3c", destination = "country.name")]

  DT
}

#' Load UIS completion rates by education level and sex
#'
#' Reads six ZIP archives (primary, lower secondary, upper secondary - each for
#' female and male) via \code{\link{prepare_uis_data}}, derives the education
#' level from the indicator name, and keeps only country-year-level
#' combinations where both sexes are present.
#'
#' @return A \code{data.table} with columns: educationLevel, countryCode,
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

  DT <- prepare_uis_data(files)

  DT[, educationLevel := sub("_[^_]+$", "", indicator)]
  DT[, educationLevel := factor(fcase(
    educationLevel == "primary",   "Primary",
    educationLevel == "lower_sec", "Lower secondary",
    educationLevel == "upper_sec", "Upper secondary"
  ), levels = c("Primary", "Lower secondary", "Upper secondary"))]
  DT <- DT[, if (.N == 2) .SD, by = .(country, year, educationLevel)]

  DT[, .(educationLevel, countryCode, country, sex, year, value)]
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
#' @return A \code{data.table} with columns: educationLevel, countryCode,
#'   country, year, value (GPI ratio).
load_owid_gpi_data <- function() {
  files <- list(
    "Primary"         = "owid_gross_enrolment_primary.csv",
    "Lower secondary" = "owid_gross_enrolment_lower_secondary.csv",
    "Upper secondary" = "owid_gross_enrolment_upper_secondary.csv",
    "Tertiary"        = "owid_gross_enrolment_tertiary.csv"
  )

  DT <- rbindlist(lapply(names(files), function(level) {
    dt <- fread(here("data", "raw", files[[level]]))
    cols <- names(dt)
    col_female <- grep("fe", cols, ignore.case = TRUE, value = TRUE)
    col_male <- setdiff(cols[4:5], col_female)

    setnames(dt, c(cols[1:2], col_female, col_male), c("country", "countryCode", "female", "male"))
    dt[, level := level]
    dt
  }), fill = TRUE)

  DT[, value := female / male]
  DT[, educationLevel := factor(level, levels = c(
    "Pre-primary", "Primary", "Lower secondary", "Upper secondary", "Tertiary"
  ))]

  DT[, .(educationLevel, countryCode, country, year, value)]
}

prepare_gii_world_map_data <- function(gii, world, year = 2023) {
  gii_latest <- gii[year == year]
  merge(world, gii_latest, by.x = "iso_a3", by.y = "countryCode", all.x = TRUE)
}

prepare_completion_global_latest <- function(completion, year = 2021) {
  balanced_countries <- completion[year == year,
    if (uniqueN(educationLevel) == 3 && uniqueN(sex) == 2) .SD,
    by = country
  ]$country |> unique()

  completion[year == year & country %in% balanced_countries,
    .(value = mean(value)), by = .(educationLevel, sex)
  ]
}

prepare_completion_countries_time <- function(
    completion,
    countries,
    year_range = c(2005, 2020)
) {
  completion_countries_time <- completion[
    between(year, year_range[1], year_range[2]) & country %in% countries,
    .(value = mean(value)),
    by = .(country, year, sex)
  ]

  completion_countries_time[, country := factor(country, levels = countries)]
  completion_countries_time
}

plot_gii_map <- function(gii_world) {
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

plot_gpi_map <- function(gpi, world, level) {
  gpi_filtered <- gpi[year %in% c(2020, 2021) & educationLevel == level]
  gpi_filtered <- gpi_filtered[, .SD[which.max(year)], by = .(countryCode, country)]
  world_merged <- merge(world, gpi_filtered, by.x = "iso_a3", by.y = "countryCode", all.x = TRUE)

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

plot_gpi_subplot <- function(gpi, world, level) {
  plot_gpi_map(gpi, world, level) +
    labs(title = level, subtitle = NULL) +
    theme(
      plot.title = element_text(size = 12, hjust = 0.5, margin = margin(b = 3)),
      plot.margin = margin(t = 8, r = 2, b = 2, l = 2)
    )
}

plot_gpi_combined <- function(gpi, world) {
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

plot_completion_global <- function(completion_global_latest) {
  ggplot(completion_global_latest, aes(x = educationLevel, y = value, fill = sex)) +
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

plot_completion_countries_time <- function(completion_countries_time) {
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
