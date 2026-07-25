# ============================================================
# 06_Figure_TIP.R
# ============================================================

# ---- 1. SETTINGS ------------------------------------------------
TIP_FILE  <- CONFIG$data_file
TIP_SHEET <- "TIP Benchmarks (R)"

REVENUE_LEVELS <- c("<$50k", "$50–100k", "$100–500k", "$500k–1M", ">$1M")

# ---- 2. IMPORT AND CLEAN ----------------------------------------
tip <- read_excel(
  path  = TIP_FILE,
  sheet = TIP_SHEET,
  na    = "na"
) |>
  clean_names() |>
  rename(
    revenue_class  = class,
    total_expenses = expenses,
    net_margin     = margin
  ) |>
  mutate(
    revenue_class = case_when(
      as.character(revenue_class) == "0-50"    ~ "<$50k",
      as.character(revenue_class) == "50-100"  ~ "$50–100k",
      as.character(revenue_class) == "100-500" ~ "$100–500k",
      as.character(revenue_class) == "500-1M"  ~ "$500k–1M",
      as.character(revenue_class) == "1M+"     ~ ">$1M"
    ),
    revenue_class = factor(revenue_class, levels = REVENUE_LEVELS),
    year = case_when(
      as.character(year) == "2018-2022" ~ "2018–2022",
      as.character(year) == "2023"      ~ "2023"
    )
  )

# ---- 3. PREPARE PLOTTING DATA -----------------------------------
margin_plot <- tip |>
  transmute(
    revenue_class,
    year,
    value = net_margin * 100
  )

# ---- 4. PLOT ----------------------------------------------------
p_tip_margin <- ggplot(
  margin_plot,
  aes(x = revenue_class, y = value, group = year)
) +
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
           fill = "grey85", alpha = 0.25) +
  annotate("rect",
           xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
           fill = "#F7C6C6", alpha = 0.15) +
  geom_hline(yintercept = 0, linewidth = 0.7, color = "grey20") +
  geom_line(aes(linetype = year), linewidth = 1.15, color = "grey15") +
  geom_point(
    size   = 2.8,
    shape  = 21,
    fill   = "grey15",
    color  = "white",
    stroke = 0.6
  ) +
  scale_y_continuous(
    labels = label_percent(scale = 1),
    limits = c(-150, 30),
    breaks = seq(-150, 25, by = 25),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  scale_linetype_manual(
    values = c("2018–2022" = "solid", "2023" = "22")
  ) +
  labs(
    x        = "Farm Revenue Class",
    y        = "Net Margin (% of Gross Farming Income)",
    linetype = "Benchmark Period"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.grid.major.y  = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor    = element_blank(),
    axis.title.x        = element_text(margin = margin(t = 10)),
    axis.title.y        = element_text(margin = margin(r = 12)),
    legend.position     = c(0.80, 0.18),
    legend.background   = element_rect(fill = "white", color = "black", linewidth = 0.35),
    legend.key          = element_blank(),
    legend.title        = element_text(face = "bold"),
    legend.text         = element_text(size = 12)
  )

p_tip_margin

# ---- 5. SAVE ----------------------------------------------------
ggsave(
  filename = "Figure1_TIP_Margin.png",
  plot     = p_tip_margin,
  path     = CONFIG$figs_dir,
  width    = 7.2,
  height   = 4.2,
  units    = "in",
  dpi      = 1200
)
