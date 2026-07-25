# ============================================================
# 04_Residual_Diagnostics.R
# ============================================================

# ----------------- 1. YEAR RECODE HELPER -----------------
recode_years_ch3 <- function(data) {
  data$year <- factor(
    as.character(droplevels(factor(as.character(data$year)))),
    levels = c("2022", "2023", "2024"),
    labels = c("Year 1", "Year 2", "Year 3")
  )
  data
}

# ----------------- 2. SHAPIRO EXCEL WRITER -----------------
write_shapiro_excel <- function(results, outfile) {
  wb <- createWorkbook()
  addWorksheet(wb, "Shapiro")
  
  df <- data.frame(
    Variable = names(results),
    W        = sapply(results, function(x) round(unname(x$shapiro$statistic), 4)),
    p_value  = sapply(results, function(x) round(unname(x$shapiro$p.value), 4)),
    stringsAsFactors = FALSE
  )
  
  writeDataTable(wb, "Shapiro", df)
  saveWorkbook(wb, outfile, overwrite = TRUE)
  message("✓ Shapiro results saved: ", outfile)
}

# ----------------- 3. WRAPPER -----------------
run_all_diagnostics <- function(vars, data, formula_list = NULL,
                                fig_dir, width = 10, height = 6, dpi = 300) {
  
  dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
  results <- list()
  
  for (v in vars) {
    message("Running diagnostics: ", v)
    tryCatch({
      
      # Use custom formula if provided, otherwise default to treatment * year
      if (!is.null(formula_list) && v %in% names(formula_list)) {
        fml <- formula_list[[v]]
      } else {
        fml <- as.formula(paste(v, "~ treatment * year"))
      }
      
      mod <- lm(fml, data = data, na.action = na.omit)
      outfile <- file.path(fig_dir, paste0("diagnostics_", v, ".png"))
      png(outfile, width = width, height = height, units = "in", res = dpi)
      results[[v]] <- check_diagnostics(mod, data = data)
      dev.off()
      message("  ✓ ", v)
      
    }, error = function(e) {
      message("  ✗ Error for ", v, ": ", e$message)
      try(dev.off(), silent = TRUE)
    })
  }
  
  invisible(results)
}

# ----------------- 4. CUSTOM FORMULA LIST -----------------
ch3_formulas <- list(
  # Yield models — treatment * year
  totalYield      = totalYield      ~ treatment * year,
  bean            = bean            ~ treatment * year,
  carrot          = carrot          ~ treatment * year,
  kale            = kale            ~ treatment * year,
  
  # Net margin models — treatment * year
  margins_whole   = margins_whole   ~ treatment * year,
  margins_direct  = margins_direct  ~ treatment * year,
  
  # Viable acreage models — treatment + year (no interaction)
  acres_whole     = acres_whole     ~ treatment + year,
  acres_direct    = acres_direct    ~ treatment + year
)

# ----------------- 5. PREPARE DATA -----------------

# Full dataset recoded
dat_ch3 <- recode_years_ch3(dat)

# Kale Years 2-3 only
dat_kale_ch3 <- dat_ch3 %>%
  filter(year %in% c("Year 2", "Year 3"))

# ----------------- 6. RUN YIELD DIAGNOSTICS -----------------

# Total yield, bean, carrot — all three years
diag_yield_ch3 <- run_all_diagnostics(
  vars         = c("totalYield", "bean", "carrot"),
  data         = dat_ch3,
  formula_list = ch3_formulas,
  fig_dir      = file.path(CONFIG$diag_dir, "Yield Models")
)
write_shapiro_excel(
  diag_yield_ch3,
  file.path(CONFIG$diag_dir, "Yield Models", "shapiro_yield_ch3.xlsx")
)

# Kale — Years 2-3 only
diag_kale_ch3 <- run_all_diagnostics(
  vars         = "kale",
  data         = dat_kale_ch3,
  formula_list = ch3_formulas,
  fig_dir      = file.path(CONFIG$diag_dir, "Kale Model")
)
write_shapiro_excel(
  diag_kale_ch3,
  file.path(CONFIG$diag_dir, "Kale Model", "shapiro_kale_ch3.xlsx")
)

# ----------------- 7. RUN NET MARGIN DIAGNOSTICS -----------------

diag_margins_ch3 <- run_all_diagnostics(
  vars         = c("margins_whole", "margins_direct"),
  data         = dat_ch3,
  formula_list = ch3_formulas,
  fig_dir      = file.path(CONFIG$diag_dir, "Margin Models")
)
write_shapiro_excel(
  diag_margins_ch3,
  file.path(CONFIG$diag_dir, "Margin Models", "shapiro_margins_ch3.xlsx")
)

# ----------------- 8. RUN VIABLE ACREAGE DIAGNOSTICS -----------------

diag_acres_ch3 <- run_all_diagnostics(
  vars         = c("acres_whole", "acres_direct"),
  data         = dat_ch3,
  formula_list = ch3_formulas,
  fig_dir      = file.path(CONFIG$diag_dir, "Acreage Models")
)
write_shapiro_excel(
  diag_acres_ch3,
  file.path(CONFIG$diag_dir, "Acreage Models", "shapiro_acres_ch3.xlsx")
)

message("\n✓ Chapter 3 diagnostics complete.")
