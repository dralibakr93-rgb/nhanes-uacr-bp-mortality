# Verify the main cohort, key estimates, and required output files.

source("06_R_Code/00_config.R")

cohort <- readRDS(file.path(paths$data, "cohort_primary_v2.rds"))
stopifnot(
  nrow(cohort) == 29951,
  sum(cohort$death_allcause) == 3432,
  sum(cohort$death_hd) == 702,
  identical(as.integer(table(cohort$uacr_cat)), c(19995L, 6865L, 3091L)),
  identical(as.integer(table(cohort$bp_stage)),
            c(10031L, 3509L, 4755L, 3491L, 3334L, 4831L))
)

complete_case <- read.csv(
  file.path(paths$tables, "model_hierarchy_complete_case.csv"),
  stringsAsFactors = FALSE
)
model3 <- complete_case[complete_case$model == "M3", ]
stopifnot(
  model3$N == 29675,
  model3$events == 3347,
  model3$subclinical == "1.32 (1.18-1.48)",
  model3$albuminuria == "2.09 (1.84-2.38)"
)

joint <- read.csv(file.path(paths$tables, "joint_18_cells.csv"))
stopifnot(
  nrow(joint) == 18,
  sum(joint$N) == 29675,
  sum(joint$deaths) == 3347,
  round(min(joint$adjusted_10y_risk_percent), 1) == 4.7,
  round(max(joint$adjusted_10y_risk_percent), 1) == 13.7
)

interaction_test <- read.csv(
  file.path(paths$tables, "bp_uacr_interaction_test.csv")
)
stopifnot(
  nrow(interaction_test) == 3,
  identical(interaction_test$numerator_df, c(10L, 5L, 5L)),
  identical(interaction_test$denominator_df, c(125L, 131L, 131L)),
  max(abs(interaction_test$p - c(
    0.780950625273916,
    0.714654691664622,
    0.8089499412352))) < 1e-12
)

required_outputs <- c(
  "Table1_baseline.csv", "model_hierarchy_complete_case.csv",
  "model_hierarchy_mi.csv", "joint_18_cells.csv",
  "bp_uacr_interaction_test.csv",
  "within_bp_contrasts.csv", "S1_cohort_derivation.csv",
  "S2_incidence.csv", "S2_absolute_risk.csv",
  "S4_functional_form.csv", "S4_knot_sensitivity.csv",
  "S5_measurement_error.csv", "S6_ph_diagnostics.csv",
  "S7_discrimination.csv", "S8_sensitivity_complete_case.csv",
  "S8_evalues.csv", "S9_egfr_interaction.csv",
  "S10_heart_disease_hierarchy.csv", "S11_competing_risk.csv",
  "S12_crp_availability.csv", "S12_decision_curve.csv",
  "S13_crp_distribution.csv", "S14_log_crp_models.csv",
  "S14_crp_adjusted_geomean_figure.csv", "S15_mortality_crp.csv",
  "S16_covariate_omission.csv", "S17_endpoint_sensitivity.csv",
  "S18_low_range_uacr.csv", "S19_ipcw_nri.csv"
)
output_files <- list.files(paths$tables)
for (required in required_outputs) {
  if (!(required %in% output_files)) {
    stop("Missing required output: ", required)
  }
}

code_files <- list.files("06_R_Code", pattern = "\\.(R|py|md)$",
                         recursive = TRUE, full.names = TRUE)
code_files <- setdiff(code_files, "06_R_Code/99_verify_outputs.R")
code_text <- unlist(lapply(code_files, readLines, warn = FALSE), use.names = FALSE)
forbidden <- c("/Users/", "#>", "ONE-LINE")
for (pattern in forbidden) {
  stopifnot(!any(grepl(pattern, code_text, fixed = TRUE)))
}

cat("Verification passed: cohort, key estimates, risks, outputs, and code paths.\n")
