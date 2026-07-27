# Supplementary-figure generators (S1–S8)

These scripts reproduce Supplementary Figures **S1–S5, S7, S8** from the current
cohort. They are the original reanalysis generators, with input paths repointed
to this package's `07_R_Data/`. (Figures 1–5, S6, S9, S10 and all tables are
reproduced by the main scripts in `06_R_Code/`.)

Run each from the **package root**, e.g.:
`Rscript 06_R_Code/supplementary_figure_generators/gen_S4_landmark_KM_allcause.R`
Outputs are written to `06_R_Code/supplementary_figure_generators/output/`.

| Script | Reproduces | Reads |
|--------|-----------|-------|
| gen_S1_S2_S3_attrition_missingness_prevalence.R | S1 attrition, S2 missingness, S3 UACR prevalence | cohort_primary_v2.rds, cohort_for_figures.rds, absrisk_v2.rds |
| gen_S4_landmark_KM_allcause.R | S4 landmark KM (all-cause) | cohort_for_figures.rds |
| gen_S5_landmark_BPxUACR_allcause.R | S5 landmark by BP×UACR (all-cause) | cohort_for_figures.rds |
| gen_S7_rcs_continuous.R | **Figure 4 (all-cause) AND S7 (heart-disease)** — matched RCS pair, identical survey-weighted-density panel; writes to 08_Analysis_Outputs/figures. The pipeline's `figure_4_rcs_continuous.R` is a thin wrapper that sources this. | cohort_for_figures.rds, mortality.rds |
| gen_S8_landmark_KM_heartdisease.R | S8 landmark KM (heart-disease) | cohort_for_figures.rds, mortality.rds |
| gen_extra_landmark_BPxUACR_heartdisease.R | (supporting) landmark BP×UACR heart-disease | cohort_for_figures.rds, mortality.rds |
| build_cohort_for_figures.R | builds cohort_for_figures.rds from cohort_primary_v2.rds | cohort_primary_v2.rds, mortality.rds, crp_harmonized.rds |
| gen_main_figures_v2_reference.R | reference copy of the main-figure pipeline | cohort_primary_v2.rds |

## Data note
`cohort_for_figures.rds` is the **same current analytic cohort (N=29,951)** as
`cohort_primary_v2.rds`, re-expressed under the column names these scripts use (its non-elevated-BP × UACR cell counts are 7,818/1,723/490 = the
current v2 counts). All generators therefore use the **current** analysis, not any
older cohort.

## Verified reproduction (2026-07-27)
Re-running the generators reproduces S4, S5 and S8 pixel for pixel, as it does
Figure 4 and S7 from `gen_S7_rcs_continuous.R`. gen_S4 prints low-grade HR
**1.34 (1.19-1.51)** and albuminuria **2.06 (1.81-2.35)**, matching shipped S4
and the 2-year-landmark row of `S8_sensitivity_complete_case.csv`.

S1, S2 and S3 are the exception. gen_S1_S2_S3 reproduces the same numbers —
S3 still shows survey-weighted within-BP prevalence 80/16/4 … 54/29/17 — but not
the same image: it draws S1 and S2 at different dimensions and adds a title and
subtitle that the deposited panels do not carry. The deposited S1, S2 and S3 came
from the earlier codebase and are the versions used in the manuscript.

Run these from a UTF-8 locale. Under a C/ASCII locale R cannot read the en-dash
and >= characters in the scripts and silently renders them as an ellipsis.

- Shared theme/save helpers are in `helpers/04_theme_jacc.R`.
Output filenames use the original reanalysis names; the identical-content, renamed
copies used in the manuscript are the ones shipped in `03_Main_Figures/` and inside
`04_Supplementary_Materials/Supplementary_Materials.docx`.
