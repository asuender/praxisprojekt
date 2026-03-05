library(data.table)
library(checkmate)
library(here)
library(countrycode)

load_data <- function() {
  # TODO: implement data loading
}

load_ilo_stwt <- function() {
  fread(here("data", "raw", "ilo_school_to_work_transitions.csv.gz"))
}

load_uis_completion_rates <- function() {
  files <- list(
    primary_female   = "uis_completion_rate_primary_female.zip",
    primary_male     = "uis_completion_rate_primary_male.zip",
    lower_sec_female = "uis_completion_rate_lower_secondary_female.zip",
    lower_sec_male   = "uis_completion_rate_lower_secondary_male.zip",
    upper_sec_female = "uis_completion_rate_upper_secondary_female.zip",
    upper_sec_male   = "uis_completion_rate_upper_secondary_male.zip"
  )

  dt <- rbindlist(lapply(files, function(f) {
    fread(cmd = paste("unzip -p", here("data", "raw", f), "'*/data.csv'"))
  }), idcol = "indicator")

  dt[, sex := sub(".*_", "", indicator)]
  dt[, indicator := sub("_[^_]+$", "", indicator)]

  dt[, indicator := factor(fcase(
    indicator == "primary",   "Primary",
    indicator == "lower_sec", "Lower secondary",
    indicator == "upper_sec", "Upper secondary"
  ), levels = c("Upper secondary", "Lower secondary", "Primary"))]
  dt[, sex := factor(fcase(
    sex == "female", "Female",
    sex == "male",   "Male"
  ), levels = c("Female", "Male"))]
  dt[, country := countrycode(geoUnit, origin = "iso3c", destination = "country.name")]

  dt
}

load_uis_literacy_rates <- function() {
  files <- list(
    youth_male   = "uis_literacy_rate_youth_male.zip",
    youth_female = "uis_literacy_rate_youth_female.zip",
    adult_male   = "uis_literacy_rate_adult_male.zip",
    adult_female = "uis_literacy_rate_adult_female.zip"
  )

  dt <- rbindlist(lapply(files, function(f) {
    fread(cmd = paste("unzip -p", here("data", "raw", f), "'*/data.csv'"))
  }), idcol = "indicator")

  dt[, sex := sub(".*_", "", indicator)]
  dt[, age_group := sub("_[^_]+$", "", indicator)]

  dt[, age_group := fcase(
    age_group == "youth", "15-24",
    age_group == "adult", "25-64"
  )]
  dt[, sex := factor(fcase(
    sex == "female", "Female",
    sex == "male",   "Male"
  ), levels = c("Female", "Male"))]
  dt[, country := countrycode(geoUnit, origin = "iso3c", destination = "country.name")]

  dt[, indicator := NULL]
  dt
}
