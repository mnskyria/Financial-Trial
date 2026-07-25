# ============================================================
# 11_Figure_Margins.R
# ============================================================

# ---- 1. SETTINGS ------------------------------------------------
MARGIN_VARS <- c(
  margins_whole  = "Wholesale",
  margins_direct = "Direct Marketing"
)

# ---- 2. HELPER: TREATMENT LETTERS VIA EMMEANS -------------------
get_trt_letters_emm <- function(mod, adjust = "tukey") {
  emm  <- emmeans(mod, ~ treatment)
  pw   <- contrast(emm, method = "pairwise", adjust = adjust) %>% as.data.frame()
  pvals        <- pw$p.value
  names(pvals) <- pw$contrast
  L <- multcompView::multcompLetters(pvals)$Letters
  tibble(
    treatment = names(L),
    .group    = stringr::str_trim(unname(L))
  )
}

# ---- 3. HELPER: LM PREDICTIONS WITH CI --------------------------
predict_lm_grid <- function(mod, newdat, level = 0.95) {
  pr   <- predict(mod, newdata = newdat, se.fit = TRUE)
  crit <- qnorm(1 - (1 - level) / 2)
  newdat %>%
    mutate(
      fit   = as.numeric(pr$fit),
      se    = as.numeric(pr$se.fit),
      lower = fit - crit * se,
      upper = fit + crit * se
    )
}

# ---- 4. HELPER: BUILD ONE PANEL ---------------------------------
plot_margins_panel <- function(
    data,
    response_var,
    scenario_title       = NULL,
    year_levels          = YEAR_LEVELS,
    treat_levels         = TRT_LEVELS,
    treat_labels         = TRT_LABELS,
    adjust               = "tukey",
    letter_y_offset_frac = 0.06,
    raw_alpha            = 0.25,
    raw_size             = 1.6,
    pred_size            = 3.0,
    dodge_width          = 0.55,
    y_limits             = c(-120, 30),
    y_break_by           = 30,
    show_legend          = FALSE
) {
  
  d <- data %>%
    mutate(
      year = factor(
        case_when(
          as.character(year) == "2022" ~ "Year 1",
          as.character(year) == "2023" ~ "Year 2",
          as.character(year) == "2024" ~ "Year 3"
        ),
        levels = year_levels
      ),
      treatment = factor(as.character(treatment), levels = treat_levels)
    ) %>%
    filter(!is.na(.data[[response_var]]), !is.na(year), !is.na(treatment)) %>%
    rename(y = all_of(response_var))
  
  if (is.null(scenario_title)) scenario_title <- response_var
  
  mod <- lm(y ~ treatment * year, data = d)
  
  newdat <- expand_grid(
    year      = factor(year_levels, levels = year_levels),
    treatment = factor(treat_levels, levels = treat_levels)
  )
  
  preds      <- predict_lm_grid(mod, newdat, level = 0.95)
  letters_df <- get_trt_letters_emm(mod, adjust = adjust) %>%
    mutate(treatment = factor(treatment, levels = treat_levels))
  preds <- preds %>% left_join(letters_df, by = "treatment")
  
  rng   <- range(c(preds$lower, preds$upper, d$y), na.rm = TRUE)
  y_pad <- letter_y_offset_frac * diff(rng)
  preds <- preds %>% mutate(letter_y = upper + y_pad)
  
  ggplot() +
    annotate("rect",
             xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0,
             fill = "#F7C6C6", alpha = 0.15) +
    annotate("rect",
             xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf,
             fill = "grey85", alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = "solid",
               colour = "grey15", linewidth = 0.4) +
    geom_point(
      data     = d,
      aes(x = year, y = y, fill = treatment),
      position = position_jitterdodge(jitter.width = 0.08,
                                      dodge.width  = dodge_width),
      alpha  = raw_alpha,
      size   = raw_size,
      shape  = 21,
      stroke = 0.4,
      color  = "grey40"
    ) +
    geom_errorbar(
      data = preds,
      aes(x = year, ymin = lower, ymax = upper, group = treatment),
      width     = 0.10,
      linewidth = 0.8,
      position  = position_dodge(width = dodge_width)
    ) +
    geom_point(
      data     = preds,
      aes(x = year, y = fit, fill = treatment, group = treatment),
      position = position_dodge(width = dodge_width),
      size   = pred_size,
      shape  = 21,
      stroke = 0.8,
      color  = "black"
    ) +
    geom_text(
      data     = preds,
      aes(x = year, y = letter_y, label = .group, group = treatment),
      position = position_dodge(width = dodge_width),
      vjust = 0,
      size  = 6
    ) +
    scale_fill_grey(
      start  = 0.15,
      end    = 0.85,
      name   = "Treatment",
      breaks = names(treat_labels),
      labels = treat_labels
    ) +
    guides(
      fill = guide_legend(
        override.aes = list(alpha = 1, size = 4, shape = 21, color = "black")
      )
    ) +
    scale_y_continuous(
      limits = y_limits,
      breaks = seq(y_limits[1], y_limits[2], by = y_break_by),
      expand = expansion(mult = c(0, 0.05)),
      labels = label_dollar(accuracy = 1, style_negative = "minus")
    ) +
    labs(x = NULL, y = NULL, title = scenario_title) +
    theme_minimal(base_size = 18) +
    theme(
      plot.title          = element_text(hjust = 0.5, size = 16,
                                         face = "bold", margin = margin(b = 5)),
      panel.grid.minor    = element_blank(),
      panel.grid.major.x  = element_blank(),
      axis.text.x         = element_text(size = 14, margin = margin(t = 10),
                                         colour = "black"),
      axis.title.y        = element_blank(),
      axis.text.y         = element_text(size = 14, colour = "black"),
      legend.position     = if (show_legend) "right" else "none",
      legend.title        = element_text(size = 14),
      legend.text         = element_text(size = 13)
    )
}

# ---- 5. BUILD PANELS --------------------------------------------
p_wholesale <- plot_margins_panel(
  dat,
  response_var   = "margins_whole",
  scenario_title = "Wholesale",
  show_legend    = FALSE
) + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

p_direct <- plot_margins_panel(
  dat,
  response_var   = "margins_direct",
  scenario_title = "Direct Marketing",
  show_legend    = TRUE
)

# ---- 6. COMBINE WITH PATCHWORK ----------------------------------
combined_panels <- (p_wholesale / p_direct) +
  plot_layout(guides = "collect") &
  theme(
    legend.position  = "right",
    legend.key.width = unit(0.7, "lines"),
    legend.spacing.y = unit(2, "pt")
  )

y_lab <- grid::textGrob(
  "Net Margin ($/bed)",
  rot = 90,
  gp  = grid::gpar(fontsize = 14, col = "black")
)

combined_margins <- patchwork::wrap_elements(full = y_lab) + combined_panels +
  plot_layout(widths = c(0.07, 1))

combined_margins

# ---- 7. SAVE ----------------------------------------------------
ggsave(
  filename = "Figure6_Margins.png",
  plot     = combined_margins,
  path     = CONFIG$figs_dir,
  width    = 8.5,
  height   = 5.5,
  units    = "in",
  dpi      = 1200
)
