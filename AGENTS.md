# Agent Instructions for praxisprojekt

R-based statistical analysis project studying gender inequality data. Uses R, data.table, tidyverse, and Quarto.

## Commands

```bash
# Package management
renv::restore()           # Restore packages from renv.lock
renv::snapshot()          # Snapshot current package state
renv::install("pkg")      # Install a new package

# Linting
lintr::lint_dir()          # Lint all R files (excludes renv/)
lintr::lint("code/file.R") # Lint specific file

# Running code
source("source_all.R")     # Source all R scripts in code/ and settings.R
download_data()            # Download all datasets from settings.R

# Quarto documents
quarto::quarto_render("report.qmd")
quarto::quarto_render("presentation.qmd")

# Tests (no formal test framework yet - use testthat if adding tests)
testthat::test_dir("tests/")
testthat::test_file("tests/testthat/test-file.R")
```

## Code Style

**Line length**: 120 max | **Indentation**: 2 spaces | **Encoding**: UTF-8

### Naming
- Functions: `snake_case` (e.g., `load_owid_gii_data`)
- Variables: `snake_case` or `dotted.case`
- Constants: `camelCase` or `SCREAMING_SNAKE_CASE` (e.g., `config.dataset.urls`)

### Imports
Library dependencies at top of each file:
```r
library(data.table)
library(here)
library(dplyr)
library(ggplot2)
library(checkmate)
```
Key packages: `data.table`, `dplyr`/`tidyr`, `ggplot2` (theme_minimal base), `here` (always use for paths), `checkmate` (assertions).

### Function Documentation
Roxygen2-style:
```r
#' Load OWID Gender Inequality Index data
#'
#' @return A data.table with columns: country, countryCode, year, value.
load_owid_gii_data <- function() { }
```

### Error Handling
Use `checkmate` for validation:
```r
my_function <- function(data, n = 10) {
  assert_data_frame(data)
  assert_number(n, lower = 1)
}
```
For data processing errors: `if (!condition) stop("message")`

### Data Paths
Always use `here()`:
```r
fread(here("data", "raw", "filename.csv"))  # Good
read.csv("data/raw/filename.csv")              # Bad
```

### Contributing
1. Create feature branch
2. Make changes and commit
3. Open PR - CI runs automated formatting
4. After review, merge to main

### Data Files
- **Do NOT commit** files in `data/` directory
- Raw data downloaded via `download_data()` from URLs in `settings.R`

### Package Versions
Managed via `renv.lock`. Run `renv::restore()` to install exact versions.
