# Low-grade albuminuria and mortality across blood-pressure and treatment states (NHANES 1999-2018)
mitools 2.4, MASS 7.3.65, Hmisc 5.2.5, ggplot2 4.0.3, ragg 1.5.2, scales 1.4.0,
dplyr 1.2.1, and haven 2.5.5 for the ingestion step. The landmark figures also
need cowplot 1.2.0, and the Word builders need Python with python-docx 1.2.0.

## What is here

```
06_R_Code/                    analysis scripts
07_R_Data/                    datasets the scripts read and write
08_Analysis_Outputs/          results
NHANES_UACR_Analysis.Rproj    RStudio project; opening it sets the working directory
CITATION.cff                  how to cite this
LICENSE                       MIT
```

Paths inside the scripts are relative to the repository root, and the folder
numbers mirror the submitted package, so please keep both as they are.

### 06_R_Code

| File | What it does |
|---|---|
| `00_README_CODE_INDEX.md` | Maps every script to the table or figure it produces. Start here to find one specific number. |
| `00_config.R` | Packages, random seed, paths, and the Model 1-5 covariate sets. Sourced by every other script. |
| `00_ingest_nhanes.R` | Stacks the 162 NHANES `.xpt` files into `raw_domains.rds`. Run once, only if rebuilding from the raw downloads. |
| `01_derive_cohort.R` | Applies the eligibility criteria and writes the analytic cohort. |
| `00_run_all.R` | Runs the whole pipeline in dependency order. |
| `test_*.R` | One statistical analysis each, named for the table it fills. |
| `figure_*.R` | One figure each. |
| `99_verify_outputs.R` | Recomputes the headline estimates and checks them against the shipped outputs. Prints a single pass/fail line. |
| `view_figures.R` | Draws a saved figure in the RStudio Plots pane. |
| `helpers/` | The CKD-EPI 2021 eGFR equation. |
| `supplementary_figure_generators/` | Figures that use the legacy column schema; see the README in that folder. |
| `sync_submission_figures.R` | Copies finished figures into the journal-facing folders. |
| `build_docx_tables.py`, `build_supplementary_materials.py` | Turn the result CSVs into Word documents. Formatting only; they compute nothing. |

### 07_R_Data

| File | What it holds |
|---|---|
| `raw_domains.rds` | The 25 NHANES domains stacked across cycles. At 29 MB, it is excluded from Git; rebuild it from the public-use NHANES source files using `00_ingest_nhanes.R`. |
| `mortality.rds` | The 2019 public-use linked mortality file. |
| `crp_harmonized.rds` | C-reactive protein, kept separate because the assay changed between cycles. |
| `cohort_primary_v2.rds` | The analytic cohort. Every test script reads this. |
| `cohort_for_figures.rds` | The same 29,951 participants under the column names used by the scripts in `supplementary_figure_generators/`. Figure 4 and Supplementary Figures S1-S5, S7 and S8 are drawn from it. |
