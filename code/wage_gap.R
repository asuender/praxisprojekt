library(ggplot2)
library(data.table)
library(here)


load_gender_wage_gap_data <- function() {
  fread(here("data", "raw", "owid_gender_wage_gap.csv"))
}


plot_wage_gap_distribution <- function(owid_data) {

  dt <- as.data.table(owid_data)[, .(
    country = entity,
    year    = year,
    gap     = gender_wage_gap_by_occupation__classif1_occupation__skill_level__total
  )]
  dt <- dt[!is.na(gap) & year >= 2000 & year <= 2025]

  dt_country <- dt[, .(
    gap = mean(gap, na.rm = TRUE)
  ), by = country]

  pct_positive <- round(100 * mean(dt_country$gap > 0), 1)
  med_gap      <- round(median(dt_country$gap, na.rm = TRUE), 1)
  n_countries  <- nrow(dt_country)

  ggplot(dt_country, aes(x = gap)) +
    geom_histogram(
      aes(fill = after_stat(x > 0)),
      binwidth  = 3,
      color     = "white",
      linewidth = 0.3
    ) +
    scale_fill_manual(
      values = c("FALSE" = "blue", "TRUE" = "red"),
      labels = c("FALSE" = "Women earn more", "TRUE" = "Women earn less"),
      name   = NULL
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
      size     = 3.4, color = "red", fontface = "bold"
    ) +
    scale_x_continuous(labels = function(x) paste0(x, "%")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
    labs(
      title    = "Gender Wage Gap Distribution Across Countries",
      subtitle = paste0(
        n_countries, " countries | country means over available years | 2000\u20132025"
      ),
      x       = "Gender Wage Gap (male \u2212 female avg. wage, % of male wage)",
      y       = "Number of Countries",
      caption = paste0(
        "Source: ILO via Our World in Data.  ",
        "Country means computed over all available years within 2000\u20132025.\n",
        "Gap defined as (male \u2212 female) / male \u00d7 100 at the total occupation level.  ",
        "Each bar represents a 3 percentage point interval (binwidth = 3); "
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title         = element_text(face = "bold", size = 13),
      plot.subtitle      = element_text(color = "grey40", size = 9),
      plot.caption       = element_text(color = "grey50", size = 7),
      legend.position    = "top",
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
    geom_line(color = "red", linewidth = 1) +
    geom_point(color = "red", size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.5) +
    facet_wrap(~ country, ncol = 3) +
    scale_x_continuous(breaks = seq(2000, 2024, by = 6)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Gender Wage Gap Over Time Across Selected Countries",
      subtitle = "Annual gap (male \u2212 female avg. wage, % of male wage) | 2000\u20132025 ",
      x        = NULL,
      y        = "Gender Wage Gap (%)",
      caption  = paste0(
        "Source: ILO via Our World in Data.  ",
        "Positive values indicate women earn less than men.  ",
        "Gap defined as (male \u2212 female) / male \u00d7 100 at total occupation level."
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      plot.subtitle   = element_text(color = "grey40", size = 9),
      plot.caption    = element_text(color = "grey50", size = 7),
      strip.text      = element_text(face = "bold", size = 10),
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

  cat("Spearman rho:", sp_rho, "\n")
  cat("N countries: ", nrow(merged), "\n")

  ggplot(merged, aes(x = gii, y = gap)) +
    geom_point(
      aes(color = gap > 0),
      size  = 2.5,
      alpha = 0.75
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_color_manual(
      values = c("FALSE" = "steelblue", "TRUE" = "#C0392B"),
      labels = c("FALSE" = "Women earn more", "TRUE" = "Women earn less"),
      name   = NULL
    ) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(
      title    = "Gender Wage Gap vs. Gender Inequality Index",
      subtitle = paste0(
        "One point = one country | Most recent year 2020\u20132025 | ",
        "n = ", nrow(merged), " | ",
        "Spearman \u03C1 = ", sp_rho
      ),
      x       = "Gender Inequality Index (GII)  |  0 = fully equal, 1 = fully unequal",
      y       = "Gender Wage Gap (%)",
      caption = paste0(
        "Source: ILO & UNDP via Our World in Data.  ",
        "Wage gap = (male \u2212 female) / male \u00d7 100.  ",
        "GII captures reproductive health, empowerment and labour market dimensions.\n",
        "Most recent available year per country within 2020\u20132025.  "
      )
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 13),
      plot.subtitle    = element_text(color = "grey40", size = 9),
      plot.caption     = element_text(color = "grey50", size = 7),
      legend.position  = "top",
      panel.grid.minor = element_blank()
    )
}
