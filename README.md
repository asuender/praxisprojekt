# praxisprojekt

The base for our statistical project at LMU concerning gender inequality data.

Find out about the current status of the project [here](https://github.com/asuender/praxisprojekt/issues/1).

## Overview

TODO (will be replaced once we agreed on the scope of the final presentation)

## Quick Start

Let's keep it simple, stupid. To get started with this repository, follow the steps below:

```
# TODO (will be filled in once a first report is ready)
```

## Project Structure

```
praxisprojekt/
├── code/                           # R scripts for data handling and analysis
│   ├── data_download.R             # Download raw data from external sources
│   ├── models.R                    # Statistical models and computations
│   ├── education.R                 # Education data analysis (completion rates, GPI)
│   ├── labour_force_participation.R # Labour force participation analysis
│   ├── care_responsibility_share.R  # Care responsibilities analysis
│   ├── domestic_work_time.R        # Unpaid domestic work analysis
│   └── wage_gap.R                  # Gender wage gap analysis
├── data/
│   ├── raw/                        # Original, unmodified datasets (ZIP, CSV, CSV.GZ)
│   └── intermediate/               # Cleaned/transformed data (currently unused)
├── analysis.qmd                    # Local experimentation and exploratory analysis
├── report.qmd                      # DIN A4 Executive Summary (PDF output)
├── presentation.qmd                # Final presentation slides
├── settings.R                      # Project-wide settings, URLs, and ggplot theme
├── source_all.R                    # Sources all R scripts in code/ and settings.R
└── renv/                           # Local renv library and infrastructure
```

## Glossary

### Completion rate (primary education, lower secondary education, upper secondary education)

**Definition:** SDG Indicator 4.1.2: Percentage of a cohort of children or young people aged 3-5 years above the intended age for the last grade of each level of education who have completed that grade.

The intended age for the last grade of each level of education is the age at which pupils would enter the grade if they had started school at the official primary entrance age, had studied full-time and had progressed without repeating or skipping a grade.

For example, if the official age of entry into primary education is 6 years, and if primary education has 6 grades, the intended age for the last grade of primary education is 11 years. In this case, 14-16 years (11 + 3 = 14 and 11 + 5 = 16) would be the reference age group for calculation of the primary completion rate.

**Calculation method:** The number of persons in the relevant age group who have completed the last grade of the given level of education is expressed as a percentage of the total population (in the survey sample) of the same age group. As with attendance rates, individuals are assigned completion age group based on actual or assumed age at the beginning of the school year.

## Contributing

Please refer to [CONTRIBUTING.md](./CONTRIBUTING.md).

## License

MIT
