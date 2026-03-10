care_responsibilities_share <- read.csv("data/raw/care_responsbility_share.csv.gz")
gii <- read_csv("data/raw/owid_gii.csv")

dt.crs <- as.data.table(care_responsibilities_share)[, .(
  country = ref_area.label,
  year    = time,
  sex     = sex.label,
  value   = obs_value
)]
dt.crs <- dt.crs[sex %in% c("Male", "Female") & !is.na(value)]

dt_country <- dt.crs[, .(
  value = mean(value, na.rm = TRUE)
), by = .(country, sex)]

dt_wide <- dcast(dt_country, country ~ sex, value.var = "value")
dt_wide[, gap := Female - Male]
dt_wide <- dt_wide[!is.na(Female) & !is.na(Male)]
dt_wide <- dt_wide[!grepl("Egypt", country)]

dt_gii <- as.data.table(gii)[!is.na(gii), .(
  gii = mean(gii, na.rm = TRUE)
), by = .(country = entity)]

merged <- merge(dt_wide, dt_gii, by = "country")

# ── Correlation ───────────────────────────────────────────────────────────────
pearson  <- round(cor(merged$gii, merged$gap, use = "complete.obs"), 3)
spearman <- round(cor(merged$gii, merged$gap, method = "spearman", use = "complete.obs"), 3)

cat("Pearson:  ", pearson,  "\n")
cat("Spearman: ", spearman, "\n")
cat("N:        ", nrow(merged), "\n")

# ── Scatterplot ───────────────────────────────────────────────────────────────
ggplot(merged, aes(x = gii, y = gap)) +
  geom_point(size = 2.5, alpha = 0.6, color = "steelblue") +
  geom_smooth(
    method    = "loess",
    se        = TRUE,
    color     = "black",
    linewidth = 0.8,
    alpha     = 0.15
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  annotate(
    "text",
    x     = 0.95,
    y     = max(merged$gap, na.rm = TRUE),
    label = paste0("Pearson:  ", pearson, "\nSpearman: ", spearman),
    hjust = 1, vjust = 1,
    size  = 3.2,
    color = "grey20"
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  labs(
    title    = "Zusammenhang zwischen GII und Gender Gap in Sorgepflichten",
    subtitle = paste0(
      "Ein Punkt = ein Land | Mittelwert \u00FCber verf\u00FCgbare Jahre | n = ",
      nrow(merged), " L\u00E4nder"
    ),
    x       = "Gender Inequality Index (GII) \u2013 h\u00F6here Werte = mehr Ungleichheit",
    y       = "Gender Gap (Frauen \u2212 M\u00E4nner, %)",
    caption = paste0(
      "Quelle: ILOSTAT, OWID/UNDP. ",
      "LOESS-Gl\u00E4ttung mit 95%-Konfidenzband. ",
      "\u00C4gypten ausgeschlossen (extremer Ausrei\u00DFer)."
    )
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "grey", size = 9),
    plot.caption     = element_text(color = "grey", size = 7),
    panel.grid.minor = element_blank()
  )
