# ============================================================
# 02_Summaries.R
# ============================================================

# ---- 1. HELPER: MEAN ± SE -------------------------------------
mean_se <- function(x, digits = 0) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  m  <- mean(x)
  se <- sd(x) / sqrt(length(x))
  paste0(round(m, digits), " ± ", round(se, digits))
}

# ---- 2. HELPER: MEAN ± SE AS PERCENT --------------------------
mean_se_pct <- function(x, digits = 1) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  m  <- mean(x) * 100
  se <- sd(x) / sqrt(length(x)) * 100
  paste0(round(m, digits), "% ± ", round(se, digits), "%")
}

# ---- 3. FINANCIAL SUMMARY -------------------------------------
financial_summary <- dat |>
  filter(year %in% c("2022", "2023", "2024")) |>
  group_by(treatment, year) |>
  summarise(
    `Bean Harvest (kg/Acre)`              = mean_se(kgAcre_bean),
    `Carrot Harvest (kg/Acre)`            = mean_se(kgAcre_carrot),
    `Kale Harvest (kg/Acre)`              = mean_se(kgAcre_kale),
    
    `Wholesale Revenue ($)`               = mean_se(revenue_whole),
    `Direct Marketing Revenue ($)`        = mean_se(revenue_direct),
    
    `Wholesale Costs ($)`                 = mean_se(cost_whole),
    `Direct Marketing Costs ($)`          = mean_se(cost_direct),
    
    `Wholesale Labour (% of Revenue)`     = mean_se_pct(labour_whole),
    `Direct Marketing Labour (% of Revenue)` = mean_se_pct(labour_direct),
    
    `Net Margins - Wholesale ($/bed)`     = mean_se(margins_whole),
    `Net Margins - Direct ($/bed)`        = mean_se(margins_direct),
    
    `Acres Required - Wholesale (acres)`  = mean_se(acres_whole, digits = 2),
    `Acres Required - Direct (acres)`     = mean_se(acres_direct, digits = 2),
    
    .groups = "drop"
  ) |>
  arrange(treatment, year)

# ---- 5. WIDE TABLE: ROWS = SCENARIO X YEAR, COLS = TREATMENT --
all_financial_table <- financial_summary %>%
  tidyr::pivot_longer(
    cols      = -c(treatment, year),
    names_to  = "Scenario",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::case_when(
      stringr::str_detect(Scenario, "Harvest")  ~ "Yield",
      stringr::str_detect(Scenario, "Revenue")  ~ "Revenue",
      stringr::str_detect(Scenario, "Costs|Labour") ~ "Costs",
      stringr::str_detect(Scenario, "Margins|Acres") ~ "Margins & Viability",
      TRUE ~ "Other"
    ),
    Metric = factor(Metric, levels = c("Yield", "Revenue", "Costs", "Margins & Viability")),
    
    Scenario = factor(
      Scenario,
      levels = c(
        # Yield
        "Bean Harvest (kg/Acre)",
        "Carrot Harvest (kg/Acre)",
        "Kale Harvest (kg/Acre)",
        # Revenue
        "Wholesale Revenue ($)",
        "Direct Marketing Revenue ($)",
        # Costs
        "Wholesale Costs ($)",
        "Direct Marketing Costs ($)",
        "Wholesale Labour (% of Revenue)",
        "Direct Marketing Labour (% of Revenue)",
        # Margins & Viability
        "Net Margins - Wholesale ($/bed)",
        "Net Margins - Direct ($/bed)",
        "Acres Required - Wholesale (acres)",
        "Acres Required - Direct (acres)"
      )
    ),
    year = factor(year, levels = c("2022", "2023", "2024"))
  ) %>%
  dplyr::arrange(Metric, Scenario, year, treatment) %>%
  dplyr::mutate(Row = paste0(as.character(Scenario), " — ", as.character(year))) %>%
  dplyr::select(Row, treatment, Value) %>%
  tidyr::pivot_wider(
    names_from   = treatment,
    values_from  = Value,
    names_prefix = "T"
  )

all_financial_table

# ---- 6. SAVE TO EXCEL -----------------------------------------
openxlsx::write.xlsx(
  all_financial_table,
  file      = file.path(CONFIG$summ_dir, "Summary_Financial.xlsx"),
  overwrite = TRUE
)
