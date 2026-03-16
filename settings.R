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
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EAR_EHRA_SEX_CUR_NB_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_avg_hourly_wages_usd.csv.gz"
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
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EIP_RCAR_SEX_RT_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "care_responsbility_share.csv.gz"
  ),
  list(
    url      = "https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_gii.csv"
  ),

  # Labour force participation
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EAP_DWAP_SEX_AGE_RT_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_labour_force_participation.csv.gz"
  ),

  # Labour Force Participation and Education
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=EAP_DWAP_SEX_EDU_RT_A&timefrom=1970&timeto=2025&type=label&format=.csv.gz",
    filename = "ilo_labour_force_participation_and_education.csv.gz"
  ),

  # World poppulation OWID
  list(
    url = "https://ourworldindata.org/grapher/population.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_pop.csv"
  ),
  # Human Development Index
  list(
    url = "https://ourworldindata.org/grapher/human-development-index.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_hdi.csv"
  ),

  # School to work transition
  list(
    url = "https://rplumber.ilo.org/data/indicator/?id=POP_3TED_SEX_STE_NB_A&lang=en&type=label&format=.csv.gz&channel=ilostat",
    filename = "ilo_school_to_work_transitions.csv.gz"
  ),

  # Gender Inequality Index (GII)
  list(
    url = "https://ourworldindata.org/grapher/gender-inequality-index-from-the-human-development-report.csv?v=1&csvType=full&useColumnShortNames=true",
    filename = "owid_gender_inequality_index.csv"
  ),

  # Education (completion rate by level of education and by sex)
  # UIS API returns zip archives containing a CSV
  list(
    url = "https://api.uis.unesco.org/api/public/data/indicators/export?indicator=CR.1.F&start=2000&end=2024&indicatorMetadata=true&footnotes=true&version=20260224-aef8b145&format=csv",
    filename = "uis_completion_rate_primary_female.zip"
  ),
  list(
    url = "https://api.uis.unesco.org/api/public/data/indicators/export?indicator=CR.1.M&start=2000&end=2024&indicatorMetadata=false&footnotes=false&version=20260224-aef8b145&format=csv",
    filename = "uis_completion_rate_primary_male.zip"
  ),
  list(
    url = "https://api.uis.unesco.org/api/public/data/indicators/export?indicator=CR.2.F&start=2000&end=2024&indicatorMetadata=false&footnotes=false&version=20260224-aef8b145&format=csv",
    filename = "uis_completion_rate_lower_secondary_female.zip"
  ),
  list(
    url = "https://api.uis.unesco.org/api/public/data/indicators/export?indicator=CR.2.M&start=2000&end=2024&indicatorMetadata=false&footnotes=false&version=20260224-aef8b145&format=csv",
    filename = "uis_completion_rate_lower_secondary_male.zip"
  ),
  list(
    url = "https://api.uis.unesco.org/api/public/data/indicators/export?indicator=CR.3.F&start=2000&end=2024&indicatorMetadata=false&footnotes=false&version=20260224-aef8b145&format=csv",
    filename = "uis_completion_rate_upper_secondary_female.zip"
  ),
  list(
    url = "https://api.uis.unesco.org/api/public/data/indicators/export?indicator=CR.3.M&start=2000&end=2024&indicatorMetadata=false&footnotes=false&version=20260224-aef8b145&format=csv",
    filename = "uis_completion_rate_upper_secondary_male.zip"
  ),

  # School enrollment rates
  list(
    url = "https://ourworldindata.org/grapher/school-enrolment.csv?v=1&csvType=full&useColumnShortNames=true&enrolment_type=gross_enrolment&level=primary&sex=sex_side_by_side",
    filename = "owid_gross_enrolment_primary.csv"
  ),
  list(
    url = "https://ourworldindata.org/grapher/school-enrolment.csv?v=1&csvType=full&useColumnShortNames=true&enrolment_type=gross_enrolment&level=lower_secondary&sex=sex_side_by_side",
    filename = "owid_gross_enrolment_lower_secondary.csv"
  ),
  list(
    url = "https://ourworldindata.org/grapher/school-enrolment.csv?v=1&csvType=full&useColumnShortNames=true&enrolment_type=gross_enrolment&level=upper_secondary&sex=sex_side_by_side",
    filename = "owid_gross_enrolment_upper_secondary.csv"
  ),
  list(
    url = "https://ourworldindata.org/grapher/school-enrolment.csv?v=1&csvType=full&useColumnShortNames=true&enrolment_type=gross_enrolment&level=tertiary&sex=sex_side_by_side",
    filename = "owid_gross_enrolment_tertiary.csv"
  )
)
