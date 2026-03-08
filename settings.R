config.dataset.urls <- list(
  # Gender wage gap
  list(
    url = "https://ourworldindata.org/grapher/gender-gap-in-average-wages-ilo.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_gender_wage_gap.csv"
  ),
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EAR_EMTA_SEX_NB_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_avg_monthly_wages.csv.gz"
  ),

  # Unpaid care work
  list(
    url = "https://ourworldindata.org/grapher/female-to-male-ratio-of-time-devoted-to-unpaid-care-work.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_unpaid_care_ratio.csv"
  ),
  list(
    url = "https://ourworldindata.org/grapher/time-spend-in-domestic-work-female-vs-male.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_domestic_work_time.csv"
  ),
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=FOW_TVOL_SEX_VOL_NB_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_volunteers_count.csv.gz"
  ),
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=FOW_TVOL_SEX_VOL_RT_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_volunteer_rate.csv.gz"
  ),

  # Labour force participation
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EAP_DWAP_SEX_AGE_RT_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_labour_force_participation.csv.gz"
  ),

  #Labour Force Participation and Education
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EAP_DWAP_SEX_EDU_RT_A&timefrom=1970&timeto=2025&type=label&format=.csv.gz",
    filename = "ilo_labour_force_participation_and_education.csv.gz"
  ),

  # Human Development Index
  list(
    url = "https://ourworldindata.org/grapher/human-development-index.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_hdi.csv"
  )
)
