# In this task, I utilize the OWID data for percentage of day used on unpaid and domestic work.
# Weights are not used as we are dealing with sparse data where not all countries at all times
# are covered. We simply want to identify if there is a differnce in amount of care work between
# male and female. We take the mean value across all years for each country and then the median
# from the means in each continent and plot a bar graph to identify the differnce.
# A more numerical analysis is done within the gt table with gaps and ratios.

library(ggplot2)
library(readr)
library(data.table)
library(gt)

# load OWID with values of % of day used in care and domestic work.
owid_domestic_work_time <- read_csv("data/raw/owid_domestic_work_time.csv")

# plot graphic function
plot_domestic_work_region <- function(data) {
  dt <- as.data.table(data)[, .(
    country = entity,
    year    = year,
    region  = owid_region,
    female  = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__female`,
    male    = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__male`
  )]
  dt <- dt[!is.na(female) & !is.na(male)]

  # calculate mean across all years by each country
  dt_country <- dt[, .(
    female = mean(female, na.rm = TRUE),
    male   = mean(male,   na.rm = TRUE)
  ), by = .(country, region)]

  # find regional median from means extracted.
  dt_region <- dt_country[, .(
    female_med  = median(female, na.rm = TRUE),
    male_med    = median(male,   na.rm = TRUE),
    n_countries = uniqueN(country)
  ), by = region]

  # convert to long format so that each gender has a row for each region.
  dt_long <- melt(dt_region,
                  id.vars       = c("region", "n_countries"),
                  measure.vars  = c("female_med", "male_med"),
                  variable.name = "sex",
                  value.name    = "time_spent")

  # portray female before male
  dt_long[, sex := factor(ifelse(sex == "female_med", "Women", "Men"),
                          levels = c("Women", "Men"))]

  # plot
  ggplot(dt_long, aes(x = reorder(region, -time_spent), y = time_spent, fill = sex)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(aes(label = paste0(round(time_spent, 1), "%")),
              position = position_dodge(width = 0.7),
              vjust = -0.4, size = 2.8, color = "grey") +
    scale_fill_manual(values = c("Women" = "red", "Men" = "blue")) +
    labs(
      title    = "Unpaid Domestic Work by Region and Gender",
      subtitle = paste0("Median of country means | ",
                        uniqueN(dt$country), " countries | ",
                        min(dt$year), "\u2013", max(dt$year)),
      x       = NULL,
      y       = "Time (% of day)",
      fill    = "Gender",
      caption = "Source: OWID. Country mean over available years, then regional median."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x        = element_text(angle = 35, hjust = 1, face = "bold", size = 9),
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey", size = 9),
      plot.caption       = element_text(color = "grey", size = 7),
      legend.position    = "top",
      panel.grid.major.x = element_blank()
    )
}


# table graphic function
# we may utilize this if necessary (in decision making).
table_domestic_work_region <- function(data) {
  dt <- as.data.table(data)[, .(
    country = entity,
    year    = year,
    region  = owid_region,
    female  = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__female`,
    male    = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__male`
  )]
  dt <- dt[!is.na(female) & !is.na(male)]

  dt_country <- dt[, .(
    female = mean(female, na.rm = TRUE),
    male   = mean(male,   na.rm = TRUE)
  ), by = .(country, region)]

  dt_region <- dt_country[, .(
    Women     = round(median(female, na.rm = TRUE), 2),
    Men       = round(median(male,   na.rm = TRUE), 2),
    Gap       = round(median(female - male, na.rm = TRUE), 2),
    Ratio     = round(median(female / male, na.rm = TRUE), 2),
    Countries = uniqueN(country)
  ), by = region]

  setnames(dt_region, "region", "Region")
  setorder(dt_region, -Gap)

  dt_region |>
    as.data.frame() |>
    gt() |>
    tab_header(
      title    = "Gender Differences in Unpaid Domestic Work",
      subtitle = "Median of country means over all available years"
    ) |>
    cols_label(
      Women     = "Women (% day)",
      Men       = "Men (% day)",
      Gap       = "\u0394 Gender Gap",
      Ratio     = "F/M Ratio",
      Countries = "No. of Countries"
    ) |>
    tab_style(cell_text(weight = "bold"),
              cells_body(rows = Gap == max(Gap))) |>
    tab_footnote(
      footnote  = "\u0394 Gap = median (Women) \u2212 median (Men) as % of day. Ratio = Women / Men.",
      locations = cells_column_labels(columns = Gap)
    ) |>
    tab_source_note("Source: OWID. Country mean over available years, then regional median.") |>
    tab_options(
      table.font.size            = 13,
      heading.title.font.size    = 15,
      heading.subtitle.font.size = 11,
      column_labels.font.weight  = "bold",
      table.border.top.color     = "black",
      table.border.bottom.color  = "black"
    )
}

# execute function for owid_domestic_work_time
plot_domestic_work_region(owid_domestic_work_time)
table_domestic_work_region(owid_domestic_work_time)
