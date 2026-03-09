library(data.table)
library(checkmate)
library(here)
library(countrycode)

load_owid_gii_data <- function() {
  DT <- fread(here("data", "raw", "owid_gender_inequality_index.csv"))

  setnames(DT, c("entity", "gii"), c("country", "value"))

  DT
}

load_ilo_stwt <- function() {
  fread(here("data", "raw", "ilo_school_to_work_transitions.csv.gz"))
}

prepare_uis_data <- function(files) {
  DT <- rbindlist(lapply(files, function(f) {
    fread(cmd = paste("unzip -p", here("data", "raw", f), "'*/data.csv'"))
  }), idcol = "indicator")

  DT[, sex := sub(".*_", "", indicator)]
  DT[, sex := factor(sex, levels = c("female", "male"))]

  setnames(DT, "geoUnit", "code")
  DT[, country := countrycode(code, origin = "iso3c", destination = "country.name")]

  DT
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

  DT <- prepare_uis_data(files)

  DT[, indicator := sub("_[^_]+$", "", indicator)]
  DT[, indicator := factor(fcase(
    indicator == "primary",   "Primary",
    indicator == "lower_sec", "Lower secondary",
    indicator == "upper_sec", "Upper secondary"
  ), levels = c("Upper secondary", "Lower secondary", "Primary"))]

  DT
}

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

  DT[, `:=`(indicator = NULL, indicatorId = NULL, qualifier = NULL, magnitude = NULL)]

  DT
}

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

    setnames(dt, c(cols[1:2], col_female, col_male), c("entity", "code", "female", "male"))
    dt[, level := level]
    dt
  }), fill = TRUE)

  DT[, value := female / male]
  DT[, `:=`(female = NULL, male = NULL, entity = NULL)]
  DT[, country := countrycode(code, origin = "iso3c", destination = "country.name")]
  DT[, educationLevel := factor(level, levels = c(
    "Pre-primary", "Primary", "Lower secondary", "Upper secondary", "Tertiary"
  ))]

  DT[, .(code, country, year, educationLevel, value)]
}
