# R code index (organized one file per test and per figure)

Every statistical test and every figure that has source code lives in its own
file. Each file is self-contained: it sources `00_config.R`, reads inputs from
`07_R_Data`, runs one analysis, and writes its output to `08_Analysis_Outputs`.
You can run any single file on its own, or run everything with:

```
Rscript 06_R_Code/00_run_all.R
```

Primary analysis is complete-case, survey-weighted Cox regression. Multiple
imputation is a sensitivity analysis. The pipeline is deterministic (seed
20260629); re-running reproduces the shipped outputs byte-for-byte.

## Shared files
| File | Purpose |
|---|---|
| `00_config.R` | Packages, options, seed, paths, model definitions, shared helpers. |
| `helpers/01_ckdepi.R` | CKD-EPI 2021 race-free eGFR equation. |
| `00_ingest_nhanes.R` | Stacks the 162 NHANES `.xpt` files into `07_R_Data/raw_domains.rds`. |
| `01_derive_cohort.R` | Derives the primary cohort (N=29,951). |
| `00_run_all.R` | Runs derive -> tests -> figures -> verify in order. |
| `99_verify_outputs.R` | Recomputes and checks the headline numbers (PASS/FAIL). |
| `build_docx_tables.py` | Regenerates the Word (.docx) main and supplementary tables from the CSV outputs. |

`00_ingest_nhanes.R` is a one-off step and is not part of `00_run_all.R`, because
`raw_domains.rds` is shipped with the package. Run it only to rebuild that file
from the raw downloads; it reproduces the shipped cohort exactly.

## Statistical tests (one file each)
| File | Analysis | Output |
|---|---|---|
| `test_table1_baseline.R` | Baseline characteristics by UACR | Table1_baseline.csv |
| `test_primary_model_hierarchy.R` | Complete-case Models 1-5 (all-cause) | model_hierarchy_complete_case.csv |
| `test_multiple_imputation.R` | MI sensitivity (m=20), Models 3-5 | model_hierarchy_mi.csv |
| `test_joint_bp_uacr_risk.R` | Joint 18-cell risks + within-category contrasts | joint_18_cells.csv, within_bp_contrasts.csv |
| `test_S1_cohort_derivation.R` | Cohort derivation flow | S1_cohort_derivation.csv |
| `test_S2_incidence.R` | Incidence rates by UACR | S2_incidence.csv |
| `test_S2_absolute_risk.R` | Standardized 5/10-year absolute risk | S2_absolute_risk.csv |
| `test_S4_dose_response_spline.R` | Continuous log2(UACR) + spline | S4_functional_form.csv |
| `test_S4_knot_sensitivity.R` | 3/4/5-knot spline sensitivity | S4_knot_sensitivity.csv |
| `test_S5_measurement_error.R` | Hypothetical reliability-ratio disattenuation sensitivity | S5_measurement_error.csv |
| `test_S6_proportional_hazards.R` | Proportional-hazards diagnostics | S6_ph_diagnostics.csv |
| `test_S7_discrimination.R` | Survey-weighted Harrell C-statistic and categorical NRI with stratified PSU jackknife uncertainty | S7_discrimination.csv; S19_ipcw_nri.csv; S7_S19_survey_prediction_metrics.csv; S7_S19_survey_prediction_replicates.csv |
| `test_S8_sensitivity_analyses.R` | Sensitivity analyses set | S8_sensitivity_complete_case.csv |
| `test_S8_evalues.R` | E-values | S8_evalues.csv |
| `test_S9_egfr_interaction.R` | eGFR strata and interaction | S9_egfr_interaction.csv |
| `test_S10_heartdisease_hierarchy.R` | Heart-disease model hierarchy | S10_heart_disease_hierarchy.csv |
| `test_S11_competing_risk.R` | Cause-specific and Fine-Gray | S11_competing_risk.csv |
| `test_S12_crp_availability.R` | CRP assay availability | S12_crp_availability.csv |
| `test_S13_crp_distribution.R` | CRP distribution by BP and UACR | S13_crp_distribution.csv |
| `test_S14_crp_associations.R` | UACR-log(CRP) associations + geomeans | S14_log_crp_models.csv, S14_crp_adjusted_geomean_figure.csv |
| `test_S15_crp_mortality.R` | Mortality models adding log(CRP) | S15_mortality_crp.csv |
| `test_S16_covariate_omission.R` | Covariate-omission robustness | S16_covariate_omission.csv |
| `test_S17_endpoint_sensitivity.R` | Alternative endpoint definition, common 1999-2014 cycles | S17_endpoint_sensitivity.csv |
| `test_S18_lowrange_uacr.R` | Expanded low-range UACR | S18_low_range_uacr.csv |
| `test_S19_nri_and_decision_curve.R` | Decision-curve data retained as a non-submission diagnostic; NRI is generated with S7 | S12_decision_curve.csv |
| `test_S20_fasting_subsample_sensitivity.R` | Fasting-subsample-weighted sensitivity using fasting-expanded diabetes and dyslipidemia definitions, paired with primary definitions on identical samples | S20_fasting_subsample_sensitivity.csv; S20_fasting_subsample_design_audit.csv |

Notes on grouping: the joint 18-cell risks and within-category contrasts share
one fitted model and one seeded coefficient-draw set, so they are in one file;
likewise the NRI and decision-curve share the IPCW machinery.

## Figures (one file each)
| File | Figure |
|---|---|
| `figure_1_cohort_derivation.R` | Figure 1 |
| `figure_2_heatmap_bpxuacr.R` | Figure 2 |
| `figure_graphical_abstract.R` | Graphical abstract (same panel as Figure 2) |
| `figure_3_within_category_ard.R` | Figure 3 |
| `figure_4_rcs_continuous.R` | Figure 4 |
| `figure_5_subgroup_forest_subclinical.R` | Figure 5 |
| `figure_S6_forest_albuminuria_allcause.R` | Supplementary Figure S6 |
| `figure_S14_crp_by_uacr.R` | Supplementary Figure S9 after final renumbering |
| `figure_S15_riskprofile_bpxuacr.R` | Supplementary Figure S10 after final renumbering |

## Supplementary figures built by the figure generators
Supplementary Figures S1-S5, S7 and S8 are drawn by the scripts in
`supplementary_figure_generators/`, which read `cohort_for_figures.rds`. See that
folder.s README for the script-to-figure map.

| Figure | Generator |
|---|---|
| S1 Participant attrition | `gen_S1_S2_S3_attrition_missingness_prevalence.R` |
| S2 Missingness by cycle | `gen_S1_S2_S3_attrition_missingness_prevalence.R` |
| S3 UACR prevalence | `gen_S1_S2_S3_attrition_missingness_prevalence.R` |
| S4 Landmark KM, all-cause | `gen_S4_landmark_KM_allcause.R` |
| S5 Landmark BP x UACR, all-cause | `gen_S5_landmark_BPxUACR_allcause.R` |
| S7 Continuous UACR, heart disease (RCS) | `gen_S7_rcs_continuous.R` (also draws Figure 4) |
| S8 Landmark KM, heart disease | `gen_S8_landmark_KM_heartdisease.R` |

## Final-audit corrections (2026-07-03)

- `test_S17_endpoint_sensitivity.R` restricts both endpoint definitions to
  NHANES 1999-2014 because cerebrovascular mortality code 005 is unavailable in
  the 2015-2018 public-use files.
- Figure 5 and Supplementary Figure S6 calculate displayed N and event counts
  from the complete-case data actually used in each fitted subgroup model and
  display section-level interaction P values.
- At author request, prior Supplementary Figures S8, S9, S11, S12, and S13 were
  removed from the submission. Their analysis scripts remain available for audit
  but are no longer called by `00_run_all.R`. Retained figures were renumbered
  sequentially S1-S10.
- The redundant cohort-derivation Supplementary Table was removed; submission
  tables were consolidated and renumbered S1-S9. CSV and test-script names retain their original
  analysis identifiers so the reproducible evidence trail is not broken.
