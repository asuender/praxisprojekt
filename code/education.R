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
  DT[, sex := factor(sex, levels = c("female", "male"))]

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

#' Load UIS literacy rates by age group and sex
#'
#' Reads four ZIP archives (youth and adult, each for female and male) via
#' \code{\link{prepare_uis_data}}, maps indicator names to age-group labels
#' ("15-24", "25-64"), and retains only country-year combinations where both
#' sexes are present.
#'
#' @return A \code{data.table} with columns: countryCode, country, ageGroup,
#'   sex, year, value (literacy rate in percent).
load_uis_literacy_rates <- function() {
  files <- list(
    youth_male   = "uis_literacy_rate_youth_male.zip",
    youth_female = "uis_literacy_rate_youth_female.zip",
    adult_male   = "uis_literacy_rate_adult_male.zip",
    adult_female = "uis_literacy_rate_adult_female.zip"
  )

  DT <- prepare_uis_data(files)

  DT[, ageGroup := sub("_[^_]+$", "", indicator)]
  DT[, ageGroup := fcase(
    ageGroup == "youth", "15-24",
    ageGroup == "adult", "25-64"
  )]
  DT <- DT[, if (.N == 2) .SD, by = .(country, year)]

  DT[, .(countryCode, country, ageGroup, sex, year, value)]
}
