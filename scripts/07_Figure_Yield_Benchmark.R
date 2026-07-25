# ============================================================
# 07_Figure_Yield_Benchmark.R
# ============================================================

# ---- 1. SETTINGS ------------------------------------------------
YEAR_LEVELS_RAW <- c("2022", "2023", "2024")
benchmarks_plot <- tibble(
  crop      = c("Bean", "Carrot", "Kale"),
  benchmark = c(2948, 14969, 7257)
)

# ---- 2. RESHAPE TO LONG FORMAT ----------------------------------
yield_long <- dat %>%
  select(treatment, year, kgAcre_bean, kgAcre_carrot, kgAcre_kale) %>%
  pivot_longer(
    cols      = starts_with("kgAcre_"),
    names_to  = "crop",
    values_to = "yield_kg_acre"
  ) %>%
  mutate(
    crop = case_when(
      crop == "kgAcre_bean"   ~ "Bean",
      crop == "kgAcre_carrot" ~ "Carrot",
      crop == "kgAcre_kale"   ~ "Kale"
    ),
    treatment = factor(as.character(treatment), levels = TRT_LEVELS),
    year_raw  = as.character(year),
    year      = factor(
      case_when(
        year_raw == YEAR_LEVELS_RAW[1] ~ YEAR_LEVELS[1],
        year_raw == YEAR_LEVELS_RAW[2] ~ YEAR_LEVELS[2],
        year_raw == YEAR_LEVELS_RAW[3] ~ YEAR_LEVELS[3]
      ),
      levels = YEAR_LEVELS
    )
  ) %>%
  filter(!is.na(yield_kg_acre))

# ---- 3. MODEL PREDICTIONS: BEAN AND CARROT ----------------------
yield_long_bc <- yield_long %>% filter(crop %in% c("Bean", "Carrot"))

preds_list_bc <- lapply(split(yield_long_bc, yield_long_bc$crop), function(df_crop) {
  
  mod <- lm(yield_kg_acre ~ treatment * year_raw,
            data      = df_crop,
            na.action = na.omit)
  
  newdat <- expand.grid(
    treatment = levels(df_crop$treatment),
    year_raw  = YEAR_LEVELS_RAW
  )
  
  p <- predict(mod, newdata = newdat, se.fit = TRUE)
  
  tibble(
    crop      = unique(df_crop$crop),
    treatment = factor(newdat$treatment, levels = TRT_LEVELS),
    year_raw  = newdat$year_raw,
    fit       = as.numeric(p$fit),
    se        = as.numeric(p$se.fit),
    lower     = fit - 1.96 * se,
    upper     = fit + 1.96 * se
  ) %>%
    mutate(year = factor(
      case_when(
        year_raw == YEAR_LEVELS_RAW[1] ~ YEAR_LEVELS[1],
        year_raw == YEAR_LEVELS_RAW[2] ~ YEAR_LEVELS[2],
        year_raw == YEAR_LEVELS_RAW[3] ~ YEAR_LEVELS[3]
      ),
      levels = YEAR_LEVELS
    ))
})

yield_preds_bc <- bind_rows(preds_list_bc)

# ---- 4. MODEL PREDICTIONS: KALE (YEARS 2-3 ONLY) ----------------
yield_long_kale <- yield_long %>%
  filter(crop == "Kale", year_raw != YEAR_LEVELS_RAW[1])

kale_year_levels <- sort(unique(yield_long_kale$year_raw))

mod_kale <- lm(yield_kg_acre ~ treatment * year_raw,
               data      = yield_long_kale,
               na.action = na.omit)

newdat_kale <- expand.grid(
  treatment = levels(yield_long_kale$treatment),
  year_raw  = kale_year_levels
)

p_kale <- predict(mod_kale, newdata = newdat_kale, se.fit = TRUE)

yield_preds_kale <- tibble(
  crop      = "Kale",
  treatment = factor(newdat_kale$treatment, levels = TRT_LEVELS),
  year_raw  = newdat_kale$year_raw,
  fit       = as.numeric(p_kale$fit),
  se        = as.numeric(p_kale$se.fit),
  lower     = fit - 1.96 * se,
  upper     = fit + 1.96 * se
) %>%
  mutate(year = factor(
    case_when(
      year_raw == YEAR_LEVELS_RAW[2] ~ YEAR_LEVELS[2],
      year_raw == YEAR_LEVELS_RAW[3] ~ YEAR_LEVELS[3]
    ),
    levels = YEAR_LEVELS
  ))

# ---- 5. COMBINE PREDICTIONS -------------------------------------
yield_est <- bind_rows(
  yield_preds_bc,
  yield_preds_kale
)

# ---- 6. PLOT ----------------------------------------------------
crop_benchmark <- ggplot() +
  geom_hline(
    data      = benchmarks_plot,
    aes(yintercept = benchmark),
    linetype  = "dashed",
    linewidth = 0.4,
    alpha     = 0.35
  ) +
  geom_point(
    data     = yield_long,
    aes(x = treatment, y = yield_kg_acre),
    position = position_jitter(width = 0.10),
    alpha    = 0.35,
    size     = 1.6,
    color    = "grey40"
  ) +
  geom_errorbar(
    data = yield_est,
    aes(x = treatment, ymin = lower, ymax = upper),
    width     = 0.15,
    linewidth = 0.6
  ) +
  geom_point(
    data  = yield_est,
    aes(x = treatment, y = fit),
    size  = 2.6,
    color = "black"
  ) +
  facet_grid(crop ~ year, scales = "free_y") +
  labs(
    x = "Treatment",
    y = expression("Yield (kg acre"^-1*")")
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.spacing.x     = unit(1.1, "lines"),
    panel.spacing.y     = unit(1.1, "lines"),
    panel.border        = element_rect(color = "grey30", fill = NA, linewidth = 0.5),
    strip.background.x  = element_blank(),
    strip.text.x        = element_text(face = "bold", size = 12, color = "black"),
    strip.background.y  = element_rect(fill = "grey92", color = "grey30", linewidth = 0.5),
    strip.text.y        = element_text(face = "bold", size = 12, angle = 0, color = "black"),
    axis.text.x         = element_text(color = "black"),
    axis.text.y         = element_text(color = "black"),
    panel.grid.minor    = element_blank(),
    panel.grid.major.x  = element_blank()
  )

crop_benchmark

# ---- 7. SAVE ----------------------------------------------------
ggsave(
  filename = "Figure3_Yield_Benchmark.png",
  plot     = crop_benchmark,
  path     = CONFIG$figs_dir,
  width    = 7.2,
  height   = 5.2,
  units    = "in",
  dpi      = 1200
)
