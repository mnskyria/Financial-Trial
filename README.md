------------------------------------------------------------------------

# Quick Start

1.  Place the Excel data file in `data/` using the expected filename and sheet names.

2.  Open `Chapter 2 - Soil-Yields.Rproj` in RStudio

3.  Run `scripts/run_all.R` - outputs (models, figures, summaries) will appear in `outputs/` Note: the raw data file is not included in this repository. Contact the author for access.

------------------------------------------------------------------------

# General Information

1.  **Title of Dataset:** Comparable Yields,Unviable Economics: Financial Feasibility of Vegetable Production in an Agricultural Reclamation Trial (Data)

2.  **Author Information**

    ```         
     Name: Matthew Kyriakides
     ORCID:0009-0002-3073-3646
     Institution: Ecogastronomy Research Group, School of Environmental Studies, University of Victoria
     Address: Turpin B156. 3800 Finnerty Rd, Victoria, BC V8N 4V3
     Email: mnskyria@uvic.ca
    ```

3.  **Date of data collection:**

    - Ongoing from 2022 to 2024

4.  **Geographic location of data collection:** Sandown Centre for Regenerative Agriculture. 1810 Glamorgan Road, North Saanich, BC V8L 5S9. (48°39’34.23” N, 123°25’39.28” W). W̱SÍ¸ḴEM traditional territory.

5.  **Dataset Description:** This repository documents a multi-year (2022–2024) field trial at the Sandown Centre for Regenerative Agriculture conducted as part of PhD research in the Ecogastronomy Research Group at the University of Victoria. The dataset integrates soil-health indicators, crop yields, and management practices across a five-treatment input-systems gradient. All analyses are performed in R using a fully reproducible workflow for data cleaning, linear mixed-effects modeling, post-hoc comparisons, model selection, and visualization. **TL;DR to Reproduce Analyses**

    1.  Put the Excel data in `data/` using the expected filename and sheet names.

    2.  Open R (or RStudio) at the project root.

    3.  Run `scripts/run_all.R`. Outputs (models, EMMs, AIC, DHARMa, figures) will appear in `outputs/`.

------------------------------------------------------------------------

## SHARING/ACCESS INFORMATION

1.  Licenses/restrictions placed on the data:

2.  Links to publications that cite or use the data:

3.  Links to other publicly accessible locations of the data:

4.  Links/relationships to ancillary data sets:

    - Companion agronomic dataset (Chapter 2): soil health indicators, bulk density, and soil organic carbon across the same experimental treatments and years.

5.  Was data derived from another source? no

6.  Recommended citation for this dataset: \*\*Kyriakides, M. (2026). Comparable Yields, Unviable Economics: Financial Feasibility of Vegetable Production in an Agricultural ReclamationTrial (Data)\*. University of Victoria, Ecogastronomy Research Group.

------------------------------------------------------------------------

## DATA & FILE OVERVIEW

1.  **File List:**

    1.  **Repository Structure:**

        ```         
          ├── Field-Trial_Financial.Rproj       # RStudio project (open this)
          ├── README.md                         # Project documentation
          ├── .Rprofile                         # auto-activates renv
          ├── renv.lock                         # reproducible package versions
          │
          ├── data/                             # input data (private)
          ├── outputs/
          │   ├── Data summaries/               # Summary tables (xlsx)
          │   ├── Figures/                      # Publication figures (png)
          │       ├── LM/                       # model outputs (png)
          │           ├── Acreage Models        # acreage model outputs (png)
          │           ├── Kale Model/           # kale model output (png)
          │           ├── Net Margin Models/    # margin model outputs (png)
          │           ├── Yield Models/         # yield model outputs (png)
          │   ├── Model Summaries/              # LM outputs (xlsx)
          │   └── Residual Diagnostics/         # Diagnostic plots (png)
          │       ├── Acreage Models            # acreage model diagnostics (png)
          │       ├── Kale Model/               # kale model diagnostics (png)
          │       ├── Net Margin Models/        # margin model diagnostics (png)
          |       ├── Yield Models/             # yield model diagnostics (png)
          └── scripts/
              ├── run_all.R                     # Master script — run this
              ├── utils.R                       # Shared constants and helpers
              ├── 01_Load_Clean.R               # Data import and cleaning
              ├── 02_Summaries                  # Summary statistics
              ├── 03_Models_Financial.R         # Linear models
              ├── 04_Residual_Diagnostics.R     # Residual diagnostics
              ├── 05_Figure_Model_Outputs.R     # Model output figures
              ├── 06_Figure_TIP.R               # Figure 1: TIP revenue
              ├── 07_Figure_Yield_Benchmark.R   # Figure 3: Yield benchmarks
              ├── 08_Figure_Revenue.R           # Figure 4: Revenues
              ├── 09_Figure_Costs.R             # Figure 5: Costs
              ├── 10_Figure_Margins.R           # Figure 6: Margins
              ├── 11_Figure_Minimum_Acreage     # Figure 7: Minimum acreage
        ```

    **README for R scripts:**

    1.  run_all.R — Master pipeline; sources all scripts in order
    2.  utils.R — Shared constants (treatment/year levels, labels, colours), dat_kale, and check_diagnostics()
    3.  01_Load_Clean.R — Loads and cleans raw Excel data; derives yield, revenue, cost, and acreage variables; creates dat
    4.  02_Summaries.R — Mean ± SE summaries of yield, revenue, costs, and margins by year and treatment; exports to Excel
    5.  03_Models_Financial.R — LMs for yield, margins, and viable acreage \~ treatment × year; coefficients and pairwise contrasts exported to Excel
    6.  04_Residual_Diagnostics.R — Residual diagnostic plots and Shapiro-Wilk tests for all financial and yield models
    7.  05_Figure_Model_Outputs.R — Model prediction plots with raw data for yield, kale, and net margin models
    8.  06_Figure_TIP.R — Figure 1: TIP (Towards Increased Profits) benchmark comparison
    9.  07_Figure_Yield_Benchmark.R — Figure 3: Yield vs. BC provincial benchmarks
    10. 08_Figure_Revenue.R — Figure 4: Revenue by treatment and year
    11. 09_Figure_Costs.R — Figure 5: Variable and labour costs by treatment and year
    12. 10_Figure_Margins.R — Figure 6: Net margins by treatment and year
    13. 11_Figure_Minimum_Acreage.R — Figure 7: Minimum viable acreage by treatment

2.  **Relationship between files, if important:** `scripts/run_all.R` sources the numbered scripts in order and writes outputs into `outputs/`. Helper functions in `utils` are imported by analysis scripts.

3.  **Additional related data collected that was not included in the current data package:** n/a

4.  **Are there multiple versions of the dataset?** A. If yes, name of file(s) that was updated: B. Why was the file updated? C. When was the file updated?

------------------------------------------------------------------------

## METHODOLOGICAL INFORMATION

1.  **Description of methods used for collection/generation of data:** Field trials were conducted from 2022-2024 at the Sandown Centre for Regenerative Agriculture (North Saanich, BC, Canada) on a former horse racing track undergoing agricultural reclamation. Five management treatments were established along an input-systems (I-S) continuum, replicated four times (n = 20 plots per year; n = 60 total). Each treatment combined varying levels of tillage, inorganic fertilizer, compost, cover crops, and livestock grazing. Carrots (Daucus carota), bush beans (Phaseolus vulgaris), and kale (Brassica oleracea) were grown in each plot. Marketable crop biomass was harvested at maturity, weighed, and recorded as fresh weight per bed. All labour hours and input costs were tracked throughout the trial. Crop prices were compiled from the Vancouver Terminal Market (conventional) and Organic BC price lists (organic). See Kyriakides et al. (2026) for full methods.

2.  **Methods for processing the data:** Raw financial and yield data were compiled in Microsoft Excel. Data were imported into R and cleaned using the tidyverse package. An enterprise budgeting approach was used to calculate gross revenues, variable costs, and net margins under wholesale and direct marketing scenarios. Summary statistics (mean ± standard error) were calculated for revenues, costs, and net margins for each treatment-year combination to characterize treatment-level trends and interannual variability. Linear models (LMs) were used to evaluate treatment and year effects on yield, net margins, and acreage required to reach \$100,000 in gross revenue. Kale Year 1 data were excluded from modelling due to establishment failure across all treatments. A type II Anova was used to evaluate treatment effect on total yield. Model assumptions were assessed by examining residuals using Shapiro-Wilk tests and visual diagnostics (QQ plots, histograms, and residuals-versus-fitted plots, including checks for residual patterns across treatments and years). Models adequately met assumptions, though acreage requirement models exhibited mild positive skew. Results are interpreted accordingly and full diagnostic outputs are provided in Appendix 1. Table 5 summarizes all the models that were used.

    Pairwise *post-hoc* comparisons among treatments within each year were conducted using the *emmeans* package (Lenth, 2025). These identify which specific treatments differ from one another, rather than whether treatment effects exist in aggregate. Model-estimated means (± 95% confidence intervals) were visualized alongside raw observations to facilitate interpretation of both modeled effects and underlying data variability. All visualizations were produced using the *tidyverse* package (Wickham et al., 2019), and complete model outputs are provided in Appendix 2. Supplementary AI coding support was provided by ChatGPT models GPT-4 and 5.\

    AI use: OpenAI’s ChatGPT (GPT-4 and 5) and Anthropic’s Claude (Sonnet 4.6 and Opus 4.8) were used to support statistical analysis and editorial feedback during manuscript preparation (accessed January 2026 to July 2026). All outputs were verified, and all interpretations and final decisions were made by the author. This use was supplementary and conducted in accordance with institutional ethical guidelines.

3.  **Instrument- or software-specific information needed to interpret the data:** All analyses were conducted in RStudio (v.2025.05.0) for Windows 11 using R (v4.5.1). Required packages: `tidyverse`, `readxl`, `openxlsx`, `emmeans`, `multcompView`, `broom`, `scales`, `patchwork`, `car`, `janitor`.

4.  **Standards and calibration information, if appropriate:** Crop prices were standardized to Canadian dollars per kilogram. Direct marketing prices were calculated by multiplying wholesale conventional prices by 1.7 and wholesale organic prices by 1.8, consistent with Hardesty & Leff (2010). Labour costs were valued at BC provincial minimum wage rates (\$15.65 hr-1 i n 2022, \$16.75 hr-1 in 2023, \$17.85 hr-1 in 2024).

5.  **Environmental/experimental conditions:** The experiment took place over three years (2022-2024) in a temperate coastal climate on southern Vancouver Island (mean annual temperature approximately 9.5°C; annual precipitation approximately 880 mm). The site features coarse-textured soils on a reclaimed horse racing track. See Kyriakides et al. (2026) for full site description.

6.  **Describe any quality-assurance procedures performed on the data:** All financial data were compiled in a master Excel file and cross-checked against field records and purchase receipts. Values were reviewed for transcription errors using descriptive statistics and visual inspection in R. Residual diagnostics (QQ plots, Shapiro-Wilk tests, residuals vs fitted plots) were used to validate model assumptions.

7.  **People involved with sample collection, processing, analysis and/or submission:**

- **2022:** Matthew Kyriakides
- **2023:** Matthew Kyriakides, P
- **2024:** Matthew Kyriakides, T, E

|                                               |
|-----------------------------------------------|
| DATA-SPECIFIC INFORMATION FOR: financial data |

| **Type of Variable** | **Type of Data** | R Label | **n** | **Absent Data** |
|---------------|---------------|---------------|---------------|---------------|
| Treatment | Categorical, ordinal (5 levels: 1-5) | treatment | 60 | None absent |
| Year | Categorical, discrete (4 levels: 4 times) | year | 60 | None absent |
| **Response Variable: Yield** |  |  |  |  |
| Bean Harvest (g) | Numerical, continuous | bean | 60 | None absent |
| Carrot Harvest (g) | Numerical, continuous | carrot | 60 | None absent |
| Kale Harvest (g) | Numerical, continuous | kale | 40 | 2023: all absent |
| Total Harvest (g) | Numerical, continuous | totalYield | 60 | None absent |
| **Revenue Variables** |  |  |  |  |
| Gross revenue (wholesale) | Numerical, continuous | revenue_whole | 60 | None absent |
| Gross revenue (direct) | Numerical, continuous | revenue_direct | 60 | None absent |
| **Cost Variables** |  |  | 60 | None absent |
| Costs (wholesale) | Numerical, continuous | costs_whole | 60 | None absent |
| Costs (direct) | Numerical, continuous | costs_direct | 60 | None absent |
| Labour (wholesale) | Numerical, continuous | labour_whole | 60 | None absent |
| Labour (direct) | Numerical, continuous | labour_direct | 60 | None absent |
| **Net Margin Variables** |  |  | 60 | None absent |
| Net margin (wholesale) | Numerical, continuous | margins_whole | 60 | None absent |
| Net margin (direct) | Numerical, continuous | margins_direct | 60 | None absent |
| **Viable Acreage Variables** |  |  | 60 | None absent |
| Viable acreage (wholesale) | Numerical, continuous | acres_whole | 60 | None absent |
| Viable acreage (direct) | Numerical, continuous | acres_direct | 60 | None absent |

**Missing data codes**: NAs listed above; all labelled as na in the document and then coded to mean NA in R using na = "na".

**Specialized formats or other abbreviations used:** `I-S continuum` Input-systems continuum (management gradient from input-oriented to systems-oriented practices) `TIP` Towards Increased Profits (BC Ministry of Agriculture benchmark program) `T + F` Tillage + Fertilizer `T + F + Cc` Tillage + Fertilizer + Cover Crops `T + C + Cc` Tillage + Compost + Cover Crops `T + C + Cc + Gz` Tillage + Compost + Cover Crops + Grazing `NT + C + Cc + Gz` No-Till + Compost + Cover Crops + Grazing
