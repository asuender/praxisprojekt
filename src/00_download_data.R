owid_download <- function(dataset, out_name) {
  url <- paste0("https://ourworldindata.org/grapher/", dataset, ".csv")
  download.file(url, file.path("data/raw", out_name), mode = "wb")
}

### Gender pay gap data
owid_download("gender-gap-in-average-wages-ilo", "gender_gap_wages_ilo.csv")

### Unpaid care work
owid_download("female-to-male-ratio-of-time-devoted-to-unpaid-care-work", "unpaid_care_ratio_f_to_m.csv")
owid_download("time-spend-in-domestic-work-female-vs-male", "time_domestic_work_f_vs_m.csv")



### labour force data (ILOSTAT)
# Run this once :
library(Rilostat)
library(dplyr)
library(readr)
dir.create("data/raw/ilostat", recursive = TRUE, showWarnings = FALSE)

# Choose by text (you can change these two lines)
# indicator name from column 1 on ILOSTAT and database_name from column 3.
# make sure indicator_name string and database_name string combined is unique.
indicator_name <- "labour force participation rate"
database_name  <- "Modelled"

# Get catalogue and pick the FIRST match
toc <- get_ilostat_toc()
chosen_id <- toc %>%
  filter(grepl(indicator_name, indicator.label, ignore.case = TRUE),
         grepl(database_name,  database.label,  ignore.case = TRUE)) %>%
  pull(id) %>%
  .[1]

# Download + save
lfp_raw <- get_ilostat(id = chosen_id)
# change "labour_force_participation_raw" naming dependent on what is being imported.
write_csv(lfp_raw, "data/raw/ilostat/labour_force_participation_raw.csv")



