# praxisprojekt

The base for our statistical project at LMU concerning gender inequality data.

## Overview

This project is part of the "Statistisches Praktikum" course at LMU Munich. We study gender inequality across four dimensions using public macro-level datasets from Our World in Data, ILOSTAT, and UNESCO UIS:

- **Education** - Gender parity in enrollment (GPI) and completion rates by sex across education levels
- **Care work** - Unpaid domestic work time, share of people outside the labor force due to care responsibilities, and the link between care gaps and labor force participation
- **Labor force participation** - Cross-country comparison of participation rates, with focus on Germany and breakdowns by education level
- **Income** - Gender wage gaps across regions and their correlation with the Gender Inequality Index (GII)

## Quick Start

To get started with this repository, follow the steps below:

```r
# 1. Restore R packages
renv::restore()

# 2. Source all scripts and settings
source("source_all.R")

# 3. Download datasets into data/raw/
download_data()

# 4. Render the presentation
#    & executive summary
quarto render presentation.qmd
quarto render report.qmd
```

Note: to render `report.qmd`, you need to have a LaTeX distribution installed on your system. Alternatively, you may also use `quarto install tinytex` to install a bare minimal distribution just for R.

## Project Structure

```
praxisprojekt/
├── code/                                 # R scripts for data handling and analysis
│   ├── data_download.R                   # Download raw data from external sources
│   ├── education.R                       # Education data analysis (completion rates, GPI)
│   ├── labour_force_participation.R      # Labour force participation analysis
│   ├── care_responsibility_share.R       # Care responsibilities analysis
│   ├── domestic_work_time.R              # Unpaid domestic work analysis
│   └── wage_gap.R                        # Gender wage gap analysis
├── data/
│   ├── raw/                              # Original, unmodified datasets (ZIP, CSV, CSV.GZ)
│   └── intermediate/                     # Cleaned/transformed data (currently unused)
├── assets/                               # Static assets (logos, diagrams)
├── presentation.qmd                      # Final presentation slides (revealjs)
├── presentation.css                      # Custom styles for presentation
├── report.qmd                            # DIN A4 Executive Summary (PDF output)
├── settings.R                            # Project-wide settings, URLs, and ggplot theme
└── source_all.R                          # Sources all R scripts in code/ and settings.R
```

## Contributing

Please refer to [CONTRIBUTING.md](./CONTRIBUTING.md).

## License

MIT
