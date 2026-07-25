# scripts/run_all.R
suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(lmerTest)
  library(emmeans)
  library(multcompView)
  library(openxlsx)
  library(scales)
  library(utils)
  library(janitor)
})

CONFIG <- list(
  data_file     = file.path("data", "ch3-final-data-3.xlsx"),
  sheet         = "Financial Data (R)",
  na_val        = "na",
  
  # outputs
  out_root      = "outputs",
  summ_dir = file.path("outputs", "Data Summaries"),
  diag_dir    = file.path("outputs", "Residual Diagnostics"),
  model_dir     = file.path("outputs", "Model Summaries"),
  figs_dir      = file.path("outputs", "Figures")
)


# ensure output folders
dirs <- CONFIG[c(
  "out_root", "summ_dir", "diag_dir", "model_dir", "figs_dir"
)]

# ensure output folders
invisible(lapply(CONFIG[c("out_root","summ_dir","diag_dir","model_dir","figs_dir")],
                 dir.create, recursive = TRUE, showWarnings = FALSE))


# 1) load + clean
source(file.path("scripts","01_Load_Clean.R"))
source(file.path("scripts","utils.R"))

# 2) summaries
source(file.path("scripts","02_Summaries.R"))

# 3) LMMs + EMMs
source(file.path("scripts","03_Models_Financial.R"))

# 4) Residual Diagnostics
source(file.path("scripts","04_Residual_Diagnostics.R"))

# 5) Figures
source(file.path("scripts","05_Figure_Model_Outputs.R"))
source(file.path("scripts","06_Figure_TIP.R"))
source(file.path("scripts","07_Figure_Yield_Benchmark.R"))
source(file.path("scripts","08_Figure_Revenue.R"))
source(file.path("scripts","09_Figure_Costs.R"))
source(file.path("scripts","11_Figure_Margins.R"))
source(file.path("scripts","12_Figure_Minimum_Acreage.R"))

message("All tasks completed. Check ./outputs for CSVs/PNGs.")
