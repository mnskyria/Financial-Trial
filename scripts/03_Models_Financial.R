# ============================================================
# 03_Models_Financial.R
# ============================================================

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------- 1. WORKBOOK HELPERS -----------------------------------------
sanitize_sheet <- function(x) {
  x <- gsub("[:\\\\/\\?\\*\\[\\]]", "_", x)
  substr(x, 1, 31)
}

wb_add_tbl <- function(wb, sheet, tbl) {
  sn <- sanitize_sheet(sheet)
  if (sn %in% sheets(wb)) removeWorksheet(wb, sn)
  addWorksheet(wb, sn)
  writeData(wb, sn, tbl)
  
  num_cols <- which(vapply(tbl, is.numeric, TRUE))
  if (length(num_cols)) {
    addStyle(
      wb, sn, createStyle(numFmt = "0.000"),
      rows = 2:(nrow(tbl) + 1),
      cols = num_cols,
      gridExpand = TRUE
    )
  }
  setColWidths(wb, sn, cols = 1:ncol(tbl), widths = "auto")
}

# ---------- 2. MAIN RUNNER ----------------------------------------
run_financial_lm_to_excel <- function(
    vars,
    data,
    outfile,
    formula_list = NULL,   # named list of formulas; defaults to treatment * year
    year_levels  = c("Year 1", "Year 2", "Year 3"),
    emm_adjust   = "tukey"
) {
  
  dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
  wb <- createWorkbook()
  
  model_summary <- list()
  coef_tbl      <- list()
  pairs_tbl     <- list()
  run_log <- tibble(
    var    = character(),
    n_used = integer(),
    ok     = logical(),
    notes  = character()
  )
  
  for (v in vars) {
    
    # Use custom formula if provided, otherwise default to treatment * year
    if (!is.null(formula_list) && v %in% names(formula_list)) {
      fml <- formula_list[[v]]
    } else {
      fml <- as.formula(paste(v, "~ treatment * year"))
    }
    
    needed <- c(v, "treatment", "year")
    if (!all(needed %in% names(data))) {
      run_log <- add_row(
        run_log, var = v, n_used = 0L, ok = FALSE,
        notes = "missing required columns"
      )
      next
    }
    
    dat <- data |>
      select(all_of(needed)) |>
      drop_na()
    
    if (nrow(dat) < 5) {
      run_log <- add_row(
        run_log, var = v, n_used = nrow(dat), ok = FALSE,
        notes = "too few observations"
      )
      next
    }
    
    dat$year <- factor(as.character(dat$year), levels = year_levels)
    
    mod <- try(lm(fml, data = dat), silent = TRUE)
    if (inherits(mod, "try-error")) {
      run_log <- add_row(
        run_log, var = v, n_used = 0L, ok = FALSE,
        notes = as.character(mod)
      )
      next
    }
    
    n_used <- nobs(mod)
    sm     <- summary(mod)
    
    # ---- MODEL SUMMARY
    model_summary[[v]] <- tibble(
      indicator = v,
      formula   = deparse(fml),
      n_used    = n_used,
      r2        = sm$r.squared,
      adj_r2    = sm$adj.r.squared,
      f_stat    = unname(sm$fstatistic[1]),
      df1       = unname(sm$fstatistic[2]),
      df2       = unname(sm$fstatistic[3]),
      p_value   = pf(
        sm$fstatistic[1],
        sm$fstatistic[2],
        sm$fstatistic[3],
        lower.tail = FALSE
      )
    )
    
    # ---- COEFFICIENTS
    coef_tbl[[v]] <-
      broom::tidy(mod) |>
      mutate(indicator = v, n_used = n_used, .before = 1)
    
    # ---- EMMEANS PAIRWISE (treatment within year)
    emm <- try(emmeans(mod, ~ treatment | year), silent = TRUE)
    
    if (!inherits(emm, "try-error")) {
      pairs_tbl[[v]] <-
        pairs(emm, adjust = emm_adjust) |>
        as_tibble() |>
        mutate(indicator = v, .before = 1)
      
      run_log <- add_row(
        run_log, var = v, n_used = n_used, ok = TRUE, notes = ""
      )
    } else {
      run_log <- add_row(
        run_log, var = v, n_used = n_used, ok = FALSE,
        notes = "emmeans failed"
      )
    }
  }
  
  # ---- WRITE WORKBOOK
  wb_add_tbl(wb, "Model_Summary",        bind_rows(model_summary))
  wb_add_tbl(wb, "Coefficients",         bind_rows(coef_tbl))
  wb_add_tbl(wb, "Pairs_trt_within_year", bind_rows(pairs_tbl))
  wb_add_tbl(wb, "Run_Log",              run_log)
  
  saveWorkbook(wb, outfile, overwrite = TRUE)
  message("✓ Wrote financial LM summary: ", normalizePath(outfile, winslash = "/"))
  
  invisible(list(
    model_summary = bind_rows(model_summary),
    coefficients  = bind_rows(coef_tbl),
    pairs         = bind_rows(pairs_tbl),
    log           = run_log,
    path          = outfile
  ))
}

# ---------- 3. FORMULA LIST -----------------------------------------------
ch3_formulas <- list(
  totalYield     = totalYield     ~ treatment * year,
  bean           = bean           ~ treatment * year,
  carrot         = carrot         ~ treatment * year,
  kale           = kale           ~ treatment * year,
  margins_whole  = margins_whole  ~ treatment * year,
  margins_direct = margins_direct ~ treatment * year,
  acres_whole    = acres_whole    ~ treatment + year,
  acres_direct   = acres_direct   ~ treatment + year
)

# ---------- 4. RUNNER — RETURNS RESULTS DIRECTLY -------------------------

run_models <- function(vars, data, formula_list, year_levels,
                       emm_adjust = "tukey") {
  
  summary_list <- list()
  coef_list    <- list()
  pairs_list   <- list()
  log_list     <- list()
  
  for (v in vars) {
    
    fml <- if (!is.null(formula_list) && v %in% names(formula_list)) {
      formula_list[[v]]
    } else {
      as.formula(paste(v, "~ treatment * year"))
    }
    
    needed <- c(v, "treatment", "year")
    if (!all(needed %in% names(data))) {
      message("  ✗ Missing columns: ", v)
      log_list[[v]] <- tibble(var = v, ok = FALSE, notes = "missing columns")
      next
    }
    
    dat_v <- data |>
      select(all_of(needed)) |>
      drop_na() |>
      mutate(year = factor(as.character(year), levels = year_levels))
    
    if (nrow(dat_v) < 5) {
      message("  ✗ Too few rows: ", v)
      log_list[[v]] <- tibble(var = v, ok = FALSE, notes = "too few rows")
      next
    }
    
    mod <- try(lm(fml, data = dat_v), silent = TRUE)
    if (inherits(mod, "try-error")) {
      message("  ✗ Model failed: ", v)
      log_list[[v]] <- tibble(var = v, ok = FALSE, notes = as.character(mod))
      next
    }
    
    sm     <- summary(mod)
    n_used <- nobs(mod)
    
    summary_list[[v]] <- tibble(
      indicator = v,
      formula   = deparse(fml),
      n_used    = n_used,
      r2        = sm$r.squared,
      adj_r2    = sm$adj.r.squared,
      f_stat    = unname(sm$fstatistic[1]),
      df1       = unname(sm$fstatistic[2]),
      df2       = unname(sm$fstatistic[3]),
      p_value   = pf(sm$fstatistic[1], sm$fstatistic[2],
                     sm$fstatistic[3], lower.tail = FALSE)
    )
    
    coef_list[[v]] <- broom::tidy(mod) |>
      mutate(indicator = v, n_used = n_used, .before = 1)
    
    emm <- try(emmeans(mod, ~ treatment | year), silent = TRUE)
    if (!inherits(emm, "try-error")) {
      pairs_list[[v]] <- pairs(emm, adjust = emm_adjust) |>
        as_tibble() |>
        mutate(indicator = v, .before = 1)
      log_list[[v]] <- tibble(var = v, ok = TRUE, notes = "")
    } else {
      log_list[[v]] <- tibble(var = v, ok = FALSE, notes = "emmeans failed")
    }
    
    message("  ✓ ", v)
  }
  
  list(
    summary = bind_rows(summary_list),
    coef    = bind_rows(coef_list),
    pairs   = bind_rows(pairs_list),
    log     = bind_rows(log_list)
  )
}

# ---------- 5. RECODE HELPER ----------------------------------------------
recode_dat <- function(data) {
  data |> mutate(year = dplyr::recode(as.character(year),
                                      "2022" = "Year 1",
                                      "2023" = "Year 2",
                                      "2024" = "Year 3"
  ))
}

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

# ---------- 7. COMBINE ALL RESULTS ----------------------------------------
all_summary <- bind_rows(res_yield$summary, res_kale$summary,
                         res_margins$summary, res_acres$summary)
all_coef    <- bind_rows(res_yield$coef,    res_kale$coef,
                         res_margins$coef,   res_acres$coef)
all_pairs   <- bind_rows(res_yield$pairs,   res_kale$pairs,
                         res_margins$pairs,  res_acres$pairs)
all_log     <- bind_rows(res_yield$log,     res_kale$log,
                         res_margins$log,    res_acres$log)

# Check results before writing
message("Rows — Summary: ", nrow(all_summary),
        " | Coef: ",  nrow(all_coef),
        " | Pairs: ", nrow(all_pairs))

# ---------- 8. WRITE WORKBOOK ---------------------------------------------
wb <- createWorkbook()
wb_add_tbl(wb, "Model_Summary", all_summary)
wb_add_tbl(wb, "Coefficients",  all_coef)
wb_add_tbl(wb, "Pairs",         all_pairs)
wb_add_tbl(wb, "Run_Log",       all_log)

outfile <- file.path(CONFIG$model_dir, "Financial_LM_All_Models.xlsx")
saveWorkbook(wb, outfile, overwrite = TRUE)
message("✓ Saved: ", normalizePath(outfile, winslash = "/"))
