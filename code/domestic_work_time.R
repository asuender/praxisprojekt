library(ggplot2)
library(readr)
library(data.table)
library(gt)
library(here)

#' Load OWID unpaid domestic work time data
#'
#' Reads the raw Our World in Data file on unpaid domestic work time from
#' \code{data/raw/}.
#'
#' @return A \code{data.table} with country-year observations and sex-specific
#'   time-use variables.
load_care_time_share_data <- function() {
  fread(here("data", "raw", "owid_domestic_work_time.csv"))
}

#' Plot unpaid domestic work time by region and sex
#'
#' Aggregates country-level median time shares into regional summaries and plots
#' female and male medians with interquartile ranges.
#'
#' @param data A data frame or \code{data.table} returned by
#'   \code{load_care_time_share_data()}.
#' @return A \code{ggplot} object.
plot_domestic_work_region <- function(data) {
  dt <- as.data.table(data)[, .(
    country = entity,
    year    = year,
    region  = owid_region,
    female  = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__female`,
    male    = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__male`
  )]
  dt <- dt[!is.na(female) & !is.na(male)]
  dt_country <- dt[, .(
    female = median(female, na.rm = TRUE),
    male   = median(male, na.rm = TRUE)
  ), by = .(country, region)]
  dt_region <- dt_country[, .(
    female_med  = median(female, na.rm = TRUE),
    female_q1   = quantile(female, 0.25, na.rm = TRUE),
    female_q3   = quantile(female, 0.75, na.rm = TRUE),
    male_med    = median(male, na.rm = TRUE),
    male_q1     = quantile(male, 0.25, na.rm = TRUE),
    male_q3     = quantile(male, 0.75, na.rm = TRUE),
    n_countries = uniqueN(country)
  ), by = region]
  dt_region[, ratio := round(female_med / male_med, 2)]
  dt_region[, region_label := paste0(region, "\nn = ", n_countries)]
  dt_long <- melt(
    dt_region,
    id.vars = c("region", "region_label", "n_countries", "ratio"),
    measure.vars = list(
      time_spent = c("female_med", "male_med"),
      q1         = c("female_q1", "male_q1"),
      q3         = c("female_q3", "male_q3")
    ),
    variable.name = "sex"
  )
  dt_long[, sex := factor(ifelse(sex == 1, "Female", "Male"), levels = c("Female", "Male"))]
  custom_order <- c("Asia", "Africa", "South America", "North America", "Oceania", "Europe")
  dt_long[, region_label := factor(
    region_label,
    levels = dt_region[match(custom_order, region), region_label]
  )]
  ggplot(dt_long, aes(x = region_label, y = time_spent, fill = sex)) +
    geom_col(
      position = "dodge",
      width = 0.7,
      color = unname(config.palette.presentation$ink),
      linewidth = 0.5
    ) +
    geom_errorbar(
      aes(ymin = q1, ymax = q3),
      position = position_dodge(width = 0.7),
      width = 0.25,
      linewidth = 0.5,
      color = "black"
    ) +
    geom_text(
      data = dt_long[sex == "Female"],
      aes(
        x = as.numeric(region_label) - 0.175,
        y = q3 + 1.5,
        label = paste0(ratio, "x")
      ),
      inherit.aes = FALSE,
      fontface = "bold",
      size = 4.5,
      color = "grey20"
    ) +
    scale_fill_sex() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = "Time spent in unpaid domestic work",
      subtitle = paste0(uniqueN(dt$country), " countries | ", "By region and sex | Median of country medians | ", min(dt$year), "-", max(dt$year)),
      x = NULL,
      y = "Time (% of day)",
      fill = "Sex",
      caption = "Source: Our World in Data."
    ) +
    theme(
      axis.text.x        = element_text(angle = 35, hjust = 1, face = "bold"),
      panel.grid.major.x = element_blank()
    )
}
