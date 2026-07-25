# ============================================================
# 09_Figure_Costs.R
# ============================================================

# ---- 1. BASE THEME ----------------------------------------------
theme_cost_base <- function(show_x_axis = TRUE) {
  t <- theme_minimal(base_size = 18) +
    theme(
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      panel.grid.major.y  = element_line(colour = "grey85", linewidth = 0.6),
      panel.border        = element_rect(colour = "black", fill = NA, linewidth = 0.8),
      panel.spacing.x     = unit(0.8, "lines"),
      strip.placement     = "outside",
      strip.background    = element_rect(fill = "grey92", colour = "black", linewidth = 0.8),
      strip.text          = element_text(face = "bold", size = 16),
      axis.text.y         = element_text(size = 16, colour = "black"),
      axis.title.y        = element_text(size = 16, margin = margin(r = 10)),
      axis.title.x        = element_blank(),
      legend.position     = "none"
    )
  
  if (show_x_axis) {
    t <- t + theme(
      axis.text.x  = element_text(size = 16, colour = "black"),
      axis.ticks.x = element_line(colour = "black")
    )
  } else {
    t <- t + theme(
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
  }
  t
}

# ---- 2. SUMMARISE MEAN ± SE -------------------------------------
summarise_cost <- function(data, cost_vars, channel_labels) {
  
  COST_MAP <- tibble(
    cost_var = cost_vars,
    channel  = channel_labels
  ) %>%
    mutate(channel = factor(channel, levels = c("Wholesale", "Direct Marketing")))
  
  data %>%
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
    pivot_longer(
      cols      = all_of(cost_vars),
      names_to  = "cost_var",
      values_to = "cost"
    ) %>%
    left_join(COST_MAP, by = "cost_var") %>%
    filter(!is.na(cost), !is.na(channel), !is.na(year), !is.na(treatment)) %>%
    group_by(channel, year, treatment) %>%
    summarise(
      mean    = mean(cost, na.rm = TRUE),
      n       = sum(!is.na(cost)),
      se      = sd(cost, na.rm = TRUE) / sqrt(n),
      lower   = mean - se,
      upper   = mean + se,
      .groups = "drop"
    )
}

# ---- 3. BUILD TWO-PANEL PLOT ------------------------------------
build_plot <- function(summ_data, y_label, show_x_axis = TRUE) {
  ggplot(
    summ_data,
    aes(x = year, y = mean, group = treatment,
        colour = treatment, shape = treatment)
  ) +
    geom_line(linewidth = 1.35) +
    geom_point(size = 3, stroke = 1.0) +
    facet_wrap(~ channel, ncol = 2, strip.position = "top") +
    scale_y_continuous(labels = label_dollar(accuracy = 1)) +
    labs(y = y_label) +
    scale_colour_manual(
      name   = "Treatment",
      values = TRT_COLOURS,
      labels = TRT_LABELS,
      breaks = TRT_LEVELS
    ) +
    scale_shape_manual(
      name   = "Treatment",
      values = TRT_SHAPES,
      labels = TRT_LABELS,
      breaks = TRT_LEVELS
    ) +
    guides(
      colour = guide_legend(
        title        = "Treatment",
        override.aes = list(linewidth = 1.6, size = 3.5)
      )
    ) +
    theme_cost_base(show_x_axis = show_x_axis)
}

# ---- 4. BUILD SUMMARIES -----------------------------------------
summ_variable <- summarise_cost(
  dat,
  cost_vars      = c("cost_whole", "cost_direct"),
  channel_labels = c("Wholesale", "Direct Marketing")
)

summ_labour <- summarise_cost(
  dat,
  cost_vars      = c("labour_whole", "labour_direct"),
  channel_labels = c("Wholesale", "Direct Marketing")
)

# ---- 5. BUILD INDIVIDUAL PLOTS ----------------------------------
p_variable <- build_plot(
  summ_variable,
  y_label     = "Variable Costs ($/bed)",
  show_x_axis = FALSE
)

p_labour <- build_plot(
  summ_labour,
  y_label     = "Labour Costs ($/bed)",
  show_x_axis = TRUE
)

# ---- 6. COMBINE WITH PATCHWORK ----------------------------------
p_combined <- (p_variable / p_labour) +
  plot_layout(guides = "collect") &
  theme(
    legend.position   = "right",
    legend.box        = "vertical",
    legend.title      = element_text(size = 16, face = "bold"),
    legend.text       = element_text(size = 16),
    legend.key.height = unit(0.9, "lines")
  )

p_combined

# ---- 7. SAVE ----------------------------------------------------
ggsave(
  filename = "Figure5_Costs.png",
  plot     = p_combined,
  path     = CONFIG$figs_dir,
  width    = 12.0,
  height   = 6.0,
  units    = "in",
  dpi      = 1200
)
