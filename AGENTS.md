# Agent Instructions for praxisprojekt

R-based statistical analysis project studying gender inequality data. Uses R, data.table, parts of tidyverse, and Quarto.

## Commands

```r
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
```

After finishing your edits, if necessary, render the presentation:

```sh
quarto render presentation.qmd
```

## Style

For labels, use sentence case:

```text
bad: Labor Force Participation
good: Labor force participation
```

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

### Package Versions

Managed via `renv.lock`. Run `renv::restore()` to install exact versions.
