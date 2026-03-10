library(ggplot2)
library(readr)
library(data.table)
install.packages("gt")
library(gt)

### OWID domestic work data interpreted.
# The lack of data for individual countries has led to taking the median of
# all countries in each year for a general view of whether the inequality in
# unpaid work exists throughout time.

# extract data of time values (% of 24 hour day)
owid_domestic_work_time <- read_csv("data/raw/owid_domestic_work_time.csv")

# countries counted
country.count <- as.data.table(owid_domestic_work_time)
country.count[, uniqueN(entity)] # 63

# Function
# We want to plot the median percentage of mean (of each country) time spent
# in domestic work for male and female among the time span of 2000 till 2024
# by region to see if the inequality exists.

plot_domestic_work_region <- function(data) {
  dt <- as.data.table(data)
  dt <- dt[, .(
    country = entity,
    year = year,
    region = owid_region,
    female = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__female`,
    male   = `_5_4_1__sl_dom_tspd__15_years_old_and_over__all_areas__male`
  )]
  dt <- dt[!is.na(female) & !is.na(male)]

  # Mittelwert je Land über alle verfügbaren Jahre
  dt_country <- dt[, .(
    female = mean(female, na.rm = TRUE),
    male   = mean(male,   na.rm = TRUE)
  ), by = .(country, region)]

  # Median der Ländermittelwerte je Region
  dt_region <- dt_country[, .(
    female_med  = median(female, na.rm = TRUE),
    male_med    = median(male,   na.rm = TRUE),
    n_countries = uniqueN(country)
  ), by = region]

  # Region labels auf Deutsch
  dt_region[, region_de := fcase(
    region == "Europe",         "Europa",
    region == "Asia",           "Asien",
    region == "Africa",         "Afrika",
    region == "North America",  "Nordamerika",
    region == "South America",  "S\u00FCdamerika",
    region == "Oceania",        "Ozeanien",
    default = region
  )]

  dt_long <- melt(
    dt_region,
    id.vars      = c("region", "region_de", "n_countries"),
    measure.vars = c("female_med", "male_med"),
    variable.name = "sex",
    value.name    = "time_spent"
  )

  dt_long[, sex_de := ifelse(sex == "female_med", "Frauen", "M\u00E4nner")]
  dt_long[, sex_de := factor(sex_de, levels = c("Frauen", "M\u00E4nner"))]

  ggplot(dt_long, aes(x = reorder(region_de, -time_spent), y = time_spent, fill = sex_de)) +
    geom_col(position = "dodge", width = 0.7) +
    geom_text(
      aes(label = paste0(round(time_spent, 1), "%")),
      position = position_dodge(width = 0.7),
      vjust = -0.4,
      size = 2.8,
      color = "grey30"
    ) +
    scale_fill_manual(
      values = c("Frauen" = "red", "M\u00E4nner" = "blue")
    ) +
    labs(
      title    = "Unbezahlte Hausarbeit nach Region und Geschlecht",
      subtitle = paste0(
        "Median der L\u00E4ndermittelwerte | ",
        uniqueN(dt$country), " L\u00E4nder | ",
        min(dt$year), "\u2013", max(dt$year)
      ),
      x       = NULL,
      y       = "Zeit (% des Tages)",
      fill    = "Geschlecht",
      caption = "Quelle: OWID. Je Land Mittelwert \u00FCber verf\u00FCgbare Jahre, dann regionaler Median."
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

table_domestic_work_region <- function(data) {
  dt <- as.data.table(data)
  dt <- dt[, .(
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
    Frauen   = round(median(female, na.rm = TRUE), 2),
    Maenner  = round(median(male,   na.rm = TRUE), 2),
    Gap      = round(median(female - male, na.rm = TRUE), 2),
    Ratio    = round(median(female / male, na.rm = TRUE), 2),
    Laender  = uniqueN(country)
  ), by = region]

  dt_region[, region := fcase(
    region == "Europe",        "Europa",
    region == "Asia",          "Asien",
    region == "Africa",        "Afrika",
    region == "North America", "Nordamerika",
    region == "South America", "S\u00FCdamerika",
    region == "Oceania",       "Ozeanien",
    default = region
  )]

  setnames(dt_region, "region", "Region")
  setorder(dt_region, -Gap)

  dt_region |>
    as.data.frame() |>
    gt() |>
    tab_header(
      title    = "Geschlechtsspezifische Unterschiede in unbezahlter Hausarbeit",
      subtitle = "Median der L\u00E4ndermittelwerte \u00FCber alle verf\u00FCgbaren Jahre"
    ) |>
    cols_label(
      Frauen  = "Frauen (% Tag)",
      Maenner = "M\u00E4nner (% Tag)",
      Gap     = "\u0394 Gender Gap",
      Ratio   = "F/M Verh\u00E4ltnis",
      Laender = "Anzahl L\u00E4nder"
    ) |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(rows = Gap == max(Gap))
    ) |>
    tab_footnote(
      footnote  = "\u0394 Unterschied = Median (Frauen) \u2212 Median (M\u00E4nner) in % des Tages. Verh\u00E4ltnis = Frauen / M\u00E4nner.",
      locations = cells_column_labels(columns = Gap)
    ) |>
    tab_source_note(
      source_note = "Quelle: OWID. Je Land Mittelwert \u00FCber verf\u00FCgbare Jahre, dann regionaler Median."
    ) |>
    tab_options(
      table.font.size            = 13,
      heading.title.font.size    = 15,
      heading.subtitle.font.size = 11,
      column_labels.font.weight  = "bold",
      table.border.top.color     = "black",
      table.border.bottom.color  = "black"
    )
}

plot_domestic_work_region(owid_domestic_work_time)
table_domestic_work_region(owid_domestic_work_time)
