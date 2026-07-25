# ============================================================
# 08_Figure_Revenue.R
# ============================================================

# ---- 1. SETTINGS ------------------------------------------------
REGIME_LEVELS <- c("Conventional (T1–T2)", "Organic (T3–T5)")

scenario_map <- tibble(
  scenario_var = c("revenue_whole", "revenue_direct"),
  channel      = c("Wholesale",     "Direct Marketing")
) %>%
  mutate(
    channel = factor(channel, levels = c("Wholesale", "Direct Marketing"))
  )

treat_to_regime <- function(trt_chr) {
  dplyr::case_when(
    trt_chr %in% c("1", "2")     ~ REGIME_LEVELS[1],
    trt_chr %in% c("3", "4", "5") ~ REGIME_LEVELS[2],
    TRUE                          ~ NA_character_
  )
}

# ---- 2. SUMMARISE MEAN ± SE -------------------------------------
rev_summ <- dat %>%
  mutate(
    year = dplyr::case_when(
      as.character(year) == "2022" ~ "Year 1",
      as.character(year) == "2023" ~ "Year 2",
      as.character(year) == "2024" ~ "Year 3"
    ),
    year      = factor(year, levels = YEAR_LEVELS),
    treatment = as.character(treatment),
    regime    = factor(treat_to_regime(treatment), levels = REGIME_LEVELS)
  ) %>%
  filter(!is.na(year), !is.na(regime)) %>%
  pivot_longer(
    cols      = all_of(scenario_map$scenario_var),
    names_to  = "scenario_var",
    values_to = "revenue"
  ) %>%
  left_join(scenario_map, by = "scenario_var") %>%
  filter(!is.na(revenue), !is.na(channel)) %>%
  group_by(channel, year, regime) %>%
  summarise(
    mean    = mean(revenue, na.rm = TRUE),
    n       = sum(!is.na(revenue)),
    se      = sd(revenue, na.rm = TRUE) / sqrt(n),
    lower   = mean - se,
    upper   = mean + se,
    .groups = "drop"
  )

# ---- 3. PLOT ----------------------------------------------------
revenue_plot <- ggplot(
  rev_summ,
  aes(
    x      = year,
    y      = mean,
    colour = regime,
    fill   = regime,
    group  = regime
  )
) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha       = 0.22,
    colour      = NA,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 1.42) +
  geom_point(
    size   = 2.3,
    stroke = 0.9,
    colour = "black"
  ) +
  facet_wrap(~ channel, ncol = 1, strip.position = "top") +
  scale_y_continuous(labels = scales::label_dollar(accuracy = 1)) +
  labs(x = NULL, y = "Gross Revenue ($/bed)") +
  scale_colour_grey(start = 0.35, end = 0.65) +
  scale_fill_grey(start  = 0.15, end = 0.65) +
  guides(
    colour = guide_legend(
      title        = "Price Used",
      override.aes = list(linewidth = 1.3, shape = 16)
    ),
    fill = "none"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.major.y  = element_line(colour = "grey85", linewidth = 0.6),
    panel.border        = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    panel.spacing.y     = unit(1.0, "lines"),
    strip.placement     = "outside",
    strip.background    = element_rect(fill = "grey92", colour = "black", linewidth = 0.8),
    strip.text          = element_text(face = "bold", size = 16),
    axis.text.x         = element_text(size = 13, colour = "black"),
    axis.text.y         = element_text(size = 13, colour = "black"),
    axis.title.y        = element_text(size = 14, margin = margin(r = 10)),
    legend.position     = "right",
    legend.box          = "vertical",
    legend.title        = element_text(size = 14, face = "bold"),
    legend.text         = element_text(size = 13),
    legend.key.height   = unit(0.9, "lines"),
    legend.spacing.y    = unit(6, "pt")
  )

revenue_plot

# ---- 4. SAVE ----------------------------------------------------
ggsave(
  filename = "Figure4_Revenue.png",
  plot     = revenue_plot,
  path     = CONFIG$figs_dir,
  width    = 9.0,
  height   = 5.2,
  units    = "in",
  dpi      = 1200
)
