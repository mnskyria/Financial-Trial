# ============================================================
# 05_Figure_Model_Outputs.R
# ============================================================

# ----------------- 1. INDICATOR SETS AND LABELS -----------------
YIELD_INDICATORS_CH3 <- c("totalYield", "bean", "carrot")

MARGIN_INDICATORS <- c("margins_whole", "margins_direct")

ACREAGE_INDICATORS <- c("acres_wholesale", "acres_direct")

YLABS_CH3 <- c(
  totalYield      = "Total Yield (g)",
  bean            = "Bean Yield (g)",
  carrot          = "Carrot Yield (g)",
  kale            = "Kale Yield (g)",
  margins_whole   = "Net Margin - Wholesale ($/bed)",
  margins_direct  = "Net Margin - Direct Marketing ($/bed)",
  acres_whole     = "Acres Required - Wholesale",
  acres_direct    = "Acres Required - Direct Marketing"
)

# ----------------- 2. PLOT FUNCTION: TREATMENT X YEAR -----------------
plot_with_raw_ch3 <- function(model, data, response_var,
                              x           = "treatment",
                              facet_by    = "year",
                              y_label     = NULL,
                              year_labels = NULL,
                              point_alpha = 0.4,
                              point_size  = 2) {
  
  if (is.null(y_label)) {
    if (exists("YLABS_CH3") && response_var %in% names(YLABS_CH3)) {
      y_label <- YLABS_CH3[[response_var]]
    } else {
      y_label <- response_var
    }
  }
  
  year_levels <- sort(unique(data$year))
  
  if (is.null(year_labels)) {
    year_labels <- paste("Year", seq_along(year_levels))
  }
  
  newdat <- expand_grid(
    treatment = unique(data$treatment),
    year      = unique(data$year)
  )
  
  p <- predict(model, newdata = newdat, se.fit = TRUE, re.form = NA)
  
  preds <- newdat %>%
    mutate(
      fit   = p$fit,
      se    = p$se.fit,
      lower = fit - 1.96 * se,
      upper = fit + 1.96 * se,
      year  = factor(year, levels = year_levels, labels = year_labels)
    )
  
  raw <- data %>%
    select(treatment, year, all_of(response_var)) %>%
    rename(y = all_of(response_var)) %>%
    mutate(year = factor(year, levels = year_levels, labels = year_labels))
  
  ggplot() +
    geom_jitter(
      data  = raw,
      aes_string(x = x, y = "y"),
      width = 0.1, alpha = point_alpha,
      size  = point_size, color = "grey40"
    ) +
    geom_errorbar(
      data = preds,
      aes_string(x = x, ymin = "lower", ymax = "upper"),
      width = 0.15, size = 0.7
    ) +
    geom_point(
      data = preds,
      aes_string(x = x, y = "fit"),
      size = 3
    ) +
    facet_wrap(as.formula(paste("~", facet_by))) +
    labs(x = "Treatment", y = y_label, title = NULL) +
    theme_bw(base_size = 14) +
    theme(strip.text = element_text(face = "bold"))
}

# ----------------- 3. BATCH RUNNER: TREATMENT X YEAR -----------------
run_lm_plots_ch3 <- function(
    indicators,
    data,
    formula_type = "interaction",   # "interaction" = * year, "additive" = + year
    out_dir,
    year_labels = NULL,
    width       = 7,
    height      = 4,
    dpi         = 300
) {
  
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  for (response_var in indicators) {
    message("Processing: ", response_var)
    
    tryCatch({
      
      # Switch between interaction and additive year term
      if (formula_type == "interaction") {
        fml <- as.formula(paste0(response_var, " ~ treatment * year"))
      } else {
        fml <- as.formula(paste0(response_var, " ~ treatment + year"))
      }
      
      mod <- lm(fml, data = data, na.action = na.omit)
      
      p <- plot_with_raw_ch3(
        model        = mod,
        data         = data,
        response_var = response_var,
        year_labels  = year_labels
      )
      
      ggsave(
        filename = file.path(out_dir, paste0(response_var, "_LM.png")),
        plot     = p,
        width    = width,
        height   = height,
        dpi      = dpi,
        units    = "in"
      )
      
      message("  ✓ ", response_var)
      
    }, error = function(e) {
      message("  ✗ Error for ", response_var, ": ", e$message)
    })
  }
  
  message("✓ Completed: ", out_dir)
}

# ----------------- 4. RUN: YIELD MODELS (treatment * year) -----------------
run_lm_plots_ch3(
  indicators   = YIELD_INDICATORS_CH3,
  data         = dat,
  formula_type = "interaction",
  out_dir      = file.path(CONFIG$figs_dir, "LM Outputs", "Yield Models"),
  year_labels  = c("Year 1", "Year 2", "Year 3")
)

# Kale: Years 2-3 only
run_lm_plots_ch3(
  indicators   = "kale",
  data         = dat_kale,
  formula_type = "interaction",
  out_dir      = file.path(CONFIG$figs_dir, "LM Outputs", "Kale Model"),
  year_labels  = c("Year 2", "Year 3")
)

# ----------------- 5. RUN: NET MARGIN MODELS (treatment * year) -----------------
run_lm_plots_ch3(
  indicators   = MARGIN_INDICATORS,
  data         = dat,
  formula_type = "interaction",
  out_dir      = file.path(CONFIG$figs_dir, "LM Outputs", "Net Margin Models"),
  year_labels  = c("Year 1", "Year 2", "Year 3")
)

# ---------- 6. RUN ALL MODEL GROUPS ---------------------------------------
message("Running yield models...")
res_yield <- run_models(
  vars         = c("totalYield", "bean", "carrot"),
  data         = recode_dat(dat),
  formula_list = ch3_formulas,
  year_levels  = c("Year 1", "Year 2", "Year 3")
)

message("Running kale model...")
res_kale <- run_models(
  vars         = "kale",
  data         = dat_kale,
  formula_list = ch3_formulas,
  year_levels  = c("Year 2", "Year 3")
)

message("Running margin models...")
res_margins <- run_models(
  vars         = c("margins_whole", "margins_direct"),
  data         = recode_dat(dat),
  formula_list = ch3_formulas,
  year_levels  = c("Year 1", "Year 2", "Year 3")
)

message("Running acreage models...")
res_acres <- run_models(
  vars         = c("acres_whole", "acres_direct"),
  data         = recode_dat(dat),
  formula_list = ch3_formulas,
  year_levels  = c("Year 1", "Year 2", "Year 3")
)

# ---------- 7. COMBINE MAIN RESULTS (excluding kale) ----------------------
all_summary <- bind_rows(res_yield$summary, res_margins$summary, res_acres$summary)
all_coef    <- bind_rows(res_yield$coef,    res_margins$coef,    res_acres$coef)
all_pairs   <- bind_rows(res_yield$pairs,   res_margins$pairs,   res_acres$pairs)
all_log     <- bind_rows(res_yield$log,     res_margins$log,     res_acres$log)

message("Rows — Summary: ", nrow(all_summary),
        " | Coef: ",  nrow(all_coef),
        " | Pairs: ", nrow(all_pairs))

# ---------- 8. WRITE WORKBOOK ---------------------------------------------
wb <- createWorkbook()

# Main model sheets
wb_add_tbl(wb, "Model_Summary", all_summary)
wb_add_tbl(wb, "Coefficients",  all_coef)
wb_add_tbl(wb, "Pairs",         all_pairs)
wb_add_tbl(wb, "Run_Log",       all_log)

# Kale separate sheets
wb_add_tbl(wb, "Kale_Summary", res_kale$summary)
wb_add_tbl(wb, "Kale_Coef",    res_kale$coef)
wb_add_tbl(wb, "Kale_Pairs",   res_kale$pairs)
wb_add_tbl(wb, "Kale_Log",     res_kale$log)

outfile <- file.path(CONFIG$model_dir, "Financial_LM_All_Models.xlsx")
saveWorkbook(wb, outfile, overwrite = TRUE)
message("✓ Saved: ", normalizePath(outfile, winslash = "/"))
