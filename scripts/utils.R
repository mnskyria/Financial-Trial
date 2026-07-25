# ============================================================
# utils.R
# ============================================================

# ---- 1. SHARED PLOT CONSTANTS -----------------------------------
YEAR_LEVELS     <- c("Year 1", "Year 2", "Year 3")
YEAR_LEVELS_RAW <- c("2022", "2023", "2024")
TRT_LEVELS      <- c("1", "2", "3", "4", "5")

TRT_LABELS <- c(
  "1" = "1: T + F",
  "2" = "2: T + F + Cc",
  "3" = "3: T + C + Cc",
  "4" = "4: T + C + Cc + Gz",
  "5" = "5: NT + C + Cc + Gz"
)

TRT_SHAPES <- c(
  "1" = 16,
  "2" = 15,
  "3" = 17,
  "4" = 18,
  "5" = 25
)

TRT_COLOURS <- c(
  "1" = "grey20",
  "2" = "grey35",
  "3" = "grey50",
  "4" = "grey65",
  "5" = "grey80"
)

# ---- 2. KALE DATASET --------------------------------------------
dat_kale <- dat %>%
  mutate(
    year = factor(
      case_when(
        as.character(year) == YEAR_LEVELS_RAW[1] ~ YEAR_LEVELS[1],
        as.character(year) == YEAR_LEVELS_RAW[2] ~ YEAR_LEVELS[2],
        as.character(year) == YEAR_LEVELS_RAW[3] ~ YEAR_LEVELS[3]
      ),
      levels = YEAR_LEVELS
    ),
    treatment = factor(as.character(treatment), levels = TRT_LEVELS)
  ) %>%
  filter(year != YEAR_LEVELS[1]) %>%
  filter(!is.na(kale))

# ---- 3. RESIDUAL DIAGNOSTICS ------------------------------------
check_diagnostics <- function(model, data = NULL) {
  
  if (inherits(model, "formula")) {
    if (is.null(data)) stop("If providing a formula, you must supply data.")
    model <- lm(model, data = data, na.action = na.exclude)
  }
  
  if (!inherits(model, "lm"))
    stop("Model must be an lm object or a formula.")
  
  mf <- model.frame(model)
  df <- data.frame(
    resid  = residuals(model),
    fitted = fitted(model),
    mf
  )
  df <- df[!is.na(df$resid), ]
  
  if (!is.null(data)) {
    treatment_names <- c("treatment", "Treatment", "trt", "Trt")
    trt_col <- treatment_names[treatment_names %in% names(data)][1]
    
    if (!is.na(trt_col)) {
      df$treatment <- factor(data[[trt_col]][match(rownames(df), rownames(data))])
    }
    
    if ("year" %in% names(data)) {
      df$year <- factor(data$year[match(rownames(df), rownames(data))])
    }
    
    if ("treatment" %in% names(df) && "year" %in% names(df)) {
      df$trt_year <- interaction(df$treatment, df$year, sep = "_")
    }
  }
  
  group_vars <- c("treatment", "year", "trt_year")
  group_vars <- group_vars[group_vars %in% names(df)]
  
  shapiro_result <- shapiro.test(df$resid)
  cat("Shapiro-Wilk test for residuals:\n")
  cat("W =", round(shapiro_result$statistic, 4),
      "| p-value =", round(shapiro_result$p.value, 4), "\n\n")
  
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(2, 3), mar = c(4, 4, 2, 1))
  
  car::qqPlot(df$resid, main = "Q-Q Plot",
              ylab = "Residuals", col = "black", pch = 19)
  
  hist(df$resid, breaks = 30, freq = FALSE,
       main = "Residuals", xlab = "Residuals", col = "grey")
  lines(density(df$resid), col = "red", lwd = 2)
  
  plot(df$fitted, df$resid,
       xlab = "Fitted values", ylab = "Residuals",
       main = "Residuals vs Fitted", pch = 19)
  abline(h = 0, lty = 2)
  
  slots_remaining <- 3
  i <- 1
  while (i <= slots_remaining) {
    if (i <= length(group_vars)) {
      gv <- group_vars[i]
      boxplot(df$resid ~ df[[gv]],
              main = paste("Residuals by", gv),
              xlab = gv, ylab = "Residuals")
      abline(h = 0, lty = 2)
    } else {
      plot.new()
    }
    i <- i + 1
  }
  
  invisible(list(
    model      = model,
    shapiro    = shapiro_result,
    residual_df = df
  ))
}