library(readr)

### Gender pay gap data
gender_gap <- read_csv("data/raw/gender_gap_wages_ilo.csv")

### Unpaid care work
unpaid_care <- read_csv("data/raw/unpaid_care_ratio_f_to_m.csv")
domestic_work <- read_csv("data/raw/time_domestic_work_f_vs_m.csv")

### labor force data (ILOSTAT)
# change "labour_force_participation_raw" dependent on how it is named in download_data.
lfpr_raw <- read_csv("data/raw/ilostat/labour_force_participation_raw.csv")
