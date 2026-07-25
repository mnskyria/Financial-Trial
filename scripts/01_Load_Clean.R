# ============================================================
# 01_Load_Clean.R
# ============================================================
load_data <- function(cfg = CONFIG) {
  raw <- readxl::read_excel(
    path  = cfg$data_file,
    sheet = cfg$sheet,
    na    = cfg$na_val
  )
  
  d <- raw |>
    dplyr::mutate(
      treatment             = factor(as.character(`Treatment`), levels = c("1","2","3","4","5")),
      year                  = factor(as.character(`Year`), levels = c("T0","2022","2023","2024")),
      
      bean                  = `Bean Harvest (g)`,
      carrot                = `Carrot Gr. 1 Harvest (g)`,
      kale                  = `Kale Harvest (g)`,
      totalYield            = `Total Harvest (g)`,
      
      kgAcre_bean           = bean * ((4046.86 / 2.5) / 1000),
      kgAcre_carrot         = carrot * ((4046.86 / 2.5) / 1000),
      kgAcre_kale           = kale * ((4046.86 / 2.5) / 1000),
      
      revenue_whole         = `Total Revenue - Wholesale ($)`,
      revenue_direct        = `Total Revenue - Direct Marketing ($)`,
      cost_whole            = `Total Annual Variable Cost - Wholesale ($)`,
      cost_direct           = `Total Annual Variable Cost - Direct Marketing ($)`,
      labour_whole          = `Total Annual Labour Cost - Wholesale ($)`,
      labour_direct         = `Total Annual Labour Cost - Direct Marketing ($)`,
      
      margins_whole         = `Net Returns Over Variable Cost - Wholesale ($)`,
      margins_direct        = `Net Returns Over Variable Cost - Direct Marketing ($)`,
      
      acres_whole           = (100000 / revenue_whole)  * (7.5 / 4046.86),
      acres_direct          = (100000 / revenue_direct) * (7.5 / 4046.86)
      
    ) |>
    dplyr::select(treatment:acres_direct)
  
  d
}

dat <- load_data(CONFIG)