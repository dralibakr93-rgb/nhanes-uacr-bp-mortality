# Master runner for the per-test / per-figure analysis folder.
# Run from the submission-package root:  Rscript 06_R_Code/00_run_all.R
# Stages: derive cohorts -> statistical tests -> figures -> verify.
# Each script is self-contained (sources 00_config.R and reads 07_R_Data),
# so any single file can also be run on its own.

root <- "06_R_Code"

# Stage 0 is not run here. 07_R_Data/raw_domains.rds ships with the package;
# rebuild it from the raw .xpt downloads with 00_ingest_nhanes.R if needed.
#
# build_cohort_for_figures.R re-expresses the cohort under the column names the
# figure generators use.
# It has to run here, before the figures: the restricted-cubic-spline generator
# behind Figure 4 reads that file, as do the landmark generators below.
derive <- c("01_derive_cohort.R",
            "supplementary_figure_generators/build_cohort_for_figures.R")

tests <- c(
  "test_table1_baseline.R",
  "test_primary_model_hierarchy.R",
  "test_multiple_imputation.R",
  "test_joint_bp_uacr_risk.R",
  "test_S1_cohort_derivation.R",
  "test_S2_incidence.R",
  "test_S2_absolute_risk.R",
  "test_S4_dose_response_spline.R",
  "test_S4_knot_sensitivity.R",
  "test_S5_measurement_error.R",
  "test_S6_proportional_hazards.R",
  "test_S7_discrimination.R",
  "test_S8_sensitivity_analyses.R",
  "test_S8_evalues.R",
  "test_S9_egfr_interaction.R",
  "test_S10_heartdisease_hierarchy.R",
  "test_S11_competing_risk.R",
  "test_S12_crp_availability.R",
  "test_S13_crp_distribution.R",
  "test_S14_crp_associations.R",
  "test_S15_crp_mortality.R",
  "test_S16_covariate_omission.R",
  "test_S17_endpoint_sensitivity.R",
  "test_S18_lowrange_uacr.R",
  "test_S19_nri_and_decision_curve.R",
  "test_S20_fasting_subsample_sensitivity.R"
)

figures <- c(
  "figure_1_cohort_derivation.R",
  "figure_2_heatmap_bpxuacr.R",
  "figure_graphical_abstract.R",
  "figure_3_within_category_ard.R",
  "figure_4_rcs_continuous.R",
  "figure_5_subgroup_forest_subclinical.R",
  "figure_S6_forest_albuminuria_allcause.R",
  "figure_S14_crp_by_uacr.R",
  "figure_S15_riskprofile_bpxuacr.R"
)

# Supplementary Figures S1-S5, S7 and S8 come from the figure generators.
# S7 is already drawn by figure_4_rcs_continuous.R above, which shares its
# generator, so it is not repeated here.
supplementary <- file.path("supplementary_figure_generators", c(
  "gen_S1_S2_S3_attrition_missingness_prevalence.R",
  "gen_S4_landmark_KM_allcause.R",
  "gen_S5_landmark_BPxUACR_allcause.R",
  "gen_S8_landmark_KM_heartdisease.R"
))

verify <- c("99_verify_outputs.R")

run <- function(files, stage) {
  cat("\n========== STAGE:", stage, "==========\n")
  for (f in files) {
    cat(">>", f, "...\n")
    source(file.path(root, f))
  }
}

run(derive,        "derive cohorts")
run(tests,         "statistical tests (26)")
run(figures,       "main and pipeline figures (9)")
run(supplementary, "supplementary figures S1-S5, S8")
run(verify,        "verification")
cat("\nALL STAGES COMPLETE.\n")
