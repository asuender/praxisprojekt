library(ggplot2)
library(data.table)
library(here)


load_gender_wage_gap_data <- function() {
  fread(here("data", "raw", "owid_gender_wage_gap.csv"))
}


prepare_wage_gap_distribution_data <- function(owid_data, min_years = 1) {
  dt <- as.data.table(owid_data)[, .(
    country = entity,
    year    = year,
    gap     = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total
  )]
  dt <- dt[!is.na(gap) & year >= 2000 & year <= 2025]

  dt_counts <- dt[, .(n_years = uniqueN(year)), by = country]
  dt <- dt[country %in% dt_counts[n_years >= min_years, country]]

  dt[, .(
    gap     = mean(gap, na.rm = TRUE),
    n_years = uniqueN(year)
  ), by = country]
}


plot_wage_gap_distribution <- function(owid_data) {
  dt_country <- prepare_wage_gap_distribution_data(owid_data, min_years = 2)

  pct_positive <- round(100 * mean(dt_country$gap > 0), 1)
  med_gap      <- round(median(dt_country$gap, na.rm = TRUE), 1)
  n_countries  <- nrow(dt_country)

  ggplot(dt_country, aes(x = gap)) +
    geom_histogram(
      aes(fill = after_stat(x > 0)),
      binwidth  = 3,
      color     = unname(config.palette.presentation$ink),
      linewidth = 0.3
    ) +
    scale_fill_presentation_binary(
      negative_label = "Women earn more",
      positive_label = "Women earn less",
      name = NULL
    ) +
    geom_vline(xintercept = 0,       linetype = "solid",  color = "black",  linewidth = 0.7) +
    geom_vline(xintercept = med_gap, linetype = "dashed", color = "grey20", linewidth = 0.6) +
    annotate(
      "text", x = med_gap + 0.8, y = Inf,
      label    = paste0("Median: ", med_gap, "%"),
      hjust    = 0, vjust = 1.6,
      size     = 3.2, color = "grey20", fontface = "bold"
    ) +
    annotate(
      "text", x = max(dt_country$gap, na.rm = TRUE) * 0.60, y = Inf,
      label    = paste0(pct_positive, "% of countries\nshow a positive gap"),
      hjust    = 0, vjust = 1.6,
      size     = 3.4, color = unname(config.palette.sex["Female"]), fontface = "bold"
    ) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title    = "Gender wage gap distribution across countries",
      subtitle = paste0(n_countries, " countries | 2000-2025 | Country means"),
      x       = "Wage gap (%)",
      y       = "Number of countries",
      caption = paste0(
        "Source: Our World in Data.\n",
        "Countries with fewer than 2 observations are excluded.\n",
        "The wage gap is defined as (male - female) / male * 100."
      )
    ) +
    theme(
      legend.position = "top",
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank()
    )
}

plot_wage_gap_facet <- function(owid_data) {

  dt <- as.data.table(owid_data)[, .(
    country = entity,
    year    = year,
    gap     = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total
  )]
  dt <- dt[!is.na(gap) & year >= 2000 & year <= 2025]

  country_map <- c(
    "United States" = "United States",
    "United Kingdom" = "United Kingdom",
    "Portugal"       = "Portugal",
    "Mexico"         = "Mexico",
    "Egypt"         = "Egypt",
    "Colombia"       = "Colombia",
    "South Korea"    = "South Korea",
    "Philippines"    = "Philippines",
    "South Africa"   = "South Africa"
  )

  dt <- dt[country %in% names(country_map)]
  dt[, country := factor(country, levels = unname(country_map))]

  ggplot(dt, aes(x = year, y = gap)) +
    geom_line(color = unname(config.palette.sex["Female"]), linewidth = 1) +
    geom_point(color = unname(config.palette.sex["Female"]), size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    facet_wrap(~ country, ncol = 3) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 6)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Gender wage gap over time",
      subtitle = "Selected countries | 2000-2025",
      x        = NULL,
      y        = "Anual wage gap",
      caption  = paste(
        "Source: Our World in Data.\n",
        "Positive values indicate women earn less than men.\n",
        "The wage gap is defined as (male - female) / male * 100."
      )
    ) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      panel.spacing   = grid::unit(1, "lines")
    )
}

plot_wage_gap_gii_correlation <- function(owid_data) {

  gii <- fread(here("data", "raw", "owid_gii.csv"))

  dt_gap <- as.data.table(owid_data)[
    !is.na(gender_wage_gap_by_occupation__classif1_occupation__skill_level__total) &
      year >= 2020 & year <= 2025
  ]
  setnames(dt_gap, "entity", "country")
  dt_gap <- dt_gap[, .SD[which.max(year)], by = country
  ][, .(country, gap = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total)]

  dt_gii <- as.data.table(gii)[
    !is.na(gii) & year >= 2020 & year <= 2025
  ][, .SD[which.max(year)], by = .(country = entity)
  ][, .(country, gii)]

  merged <- merge(dt_gap, dt_gii, by = "country")

  sp_test <- cor.test(merged$gap, merged$gii, method = "spearman")
  sp_rho  <- round(sp_test$estimate, 3)

  ggplot(merged, aes(x = gii, y = gap)) +
    geom_point(
      aes(color = gap > 0),
      size  = 2.5,
      alpha = 0.75
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_color_presentation_binary(
      negative_label = "Women earn more",
      positive_label = "Women earn less",
      name = NULL
    ) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Gender wage gap vs gender inequality index",
      subtitle = paste0(
        "Most recent year, 2020-2025 | One point = one country | Spearman rho = ",
        sp_rho
      ),
      x       = "Gender inequality index",
      y       = "Gender wage gap (%)",
      caption = paste(
        "Source: Our World in Data.\n",
        "The wage gap is defined as (male - female) / male * 100.\n",
        "The most recent available year per country within 2020-2025 is used for both series."
      )
    ) +
    theme(
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}
