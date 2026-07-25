# ============================================================
# 12_Figure_Minimum_Acreage.R
# ============================================================

# ---- 1. SETTINGS ------------------------------------------------
BENCHMARK_ACRES <- 2.6

# ---- 2. SUMMARISE MEAN ± SE -------------------------------------
scaling <- dat %>%
  filter(as.character(year) %in% c("2022", "2023", "2024")) %>%
  mutate(
    year = factor(
      case_when(
        as.character(year) == "2022" ~ "Year 1",
        as.character(year) == "2023" ~ "Year 2",
        as.character(year) == "2024" ~ "Year 3"
      ),
      levels = YEAR_LEVELS
    ),
    treatment = factor(as.character(treatment), levels = TRT_LEVELS)
  ) %>%
  group_by(treatment, year) %>%
  summarise(
    mean_wholesale = mean(revenue_whole, na.rm = TRUE),
    mean_direct    = mean(revenue_direct, na.rm = TRUE),
    se_wholesale   = sd(revenue_whole, na.rm = TRUE) /
      sqrt(sum(!is.na(revenue_whole))),
    se_direct      = sd(revenue_direct, na.rm = TRUE) /
      sqrt(sum(!is.na(revenue_direct))),
    .groups = "drop"
  ) %>%
  mutate(
    acres_wholesale    = (100000 / mean_wholesale) * (7.5 / 4046.86),
    acres_direct       = (100000 / mean_direct)    * (7.5 / 4046.86),
    acres_se_wholesale = acres_wholesale * (se_wholesale / mean_wholesale),
    acres_se_direct    = acres_direct    * (se_direct    / mean_direct)
  )

# ---- 3. RESHAPE TO LONG FORMAT ----------------------------------
scaling_long <- scaling %>%
  select(treatment, year,
         acres_wholesale, acres_direct,
         acres_se_wholesale, acres_se_direct) %>%
  pivot_longer(
    cols      = c(acres_wholesale, acres_direct,
                  acres_se_wholesale, acres_se_direct),
    names_to  = "key",
    values_to = "value"
  ) %>%
  mutate(
    Channel = case_when(
      grepl("wholesale", key) ~ "Wholesale",
      grepl("direct",    key) ~ "Direct Marketing"
    ),
    metric = case_when(
      grepl("se", key) ~ "se",
      TRUE             ~ "acres"
    )
  ) %>%
  select(-key) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(
    Channel   = factor(Channel, levels = c("Wholesale", "Direct Marketing")),
    treatment = factor(treatment, levels = TRT_LEVELS),
    year      = factor(year, levels = YEAR_LEVELS)
  )

# ---- 4. COLOUR PALETTE ------------------------------------------
greys <- RColorBrewer::brewer.pal(7, "Greys")[2:6]
names(greys) <- TRT_LEVELS

# ---- 5. PLOT ----------------------------------------------------
plot_scaling <- ggplot(
  scaling_long,
  aes(
    x     = year,
    y     = acres,
    fill  = treatment,
    group = treatment
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width    = 0.65
  ) +
  geom_errorbar(
    aes(ymin = acres - se, ymax = acres + se),
    position  = position_dodge(width = 0.75),
    width     = 0.20,
    linewidth = 0.5,
    colour    = "grey30"
  ) +
  geom_hline(
    yintercept = BENCHMARK_ACRES,
    linetype   = "dashed",
    linewidth  = 0.8,
    colour     = "black"
  ) +
  facet_wrap(~ Channel, ncol = 1, strip.position = "top") +
  scale_fill_manual(
    values = greys,
    labels = TRT_LABELS,
    name   = "Treatment"
  ) +
  scale_y_continuous(
    breaks = seq(0, 10, by = 2),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    x = NULL,
    y = "Acres Required to Reach $100K Revenue"
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
    legend.title        = element_text(size = 14, face = "bold", colour = "black"),
    legend.text         = element_text(size = 13, colour = "black"),
    legend.key.height   = unit(0.9, "lines"),
    legend.spacing.y    = unit(6, "pt")
  )

plot_scaling

# ---- 6. SAVE ----------------------------------------------------
ggsave(
  filename = "Figure7_Minimum_Acreage.png",
  plot     = plot_scaling,
  path     = CONFIG$figs_dir,
  width    = 9.0,
  height   = 5.8,
  units    = "in",
  dpi      = 1200
)
