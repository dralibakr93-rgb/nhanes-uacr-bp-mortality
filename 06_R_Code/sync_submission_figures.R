# Synchronize generated figure files with the manuscript submission folders.
# Statistical scripts write first to 08_Analysis_Outputs; this step keeps the
# clean, journal-facing folders aligned with those machine-generated outputs.

copy_set <- function(source_dir, source_stub, destination_dir, destination_stub = source_stub) {
  dir.create(destination_dir, recursive = TRUE, showWarnings = FALSE)
  for (extension in c("png", "pdf", "tiff")) {
    source_file <- file.path(source_dir, paste0(source_stub, ".", extension))
    destination_file <- file.path(destination_dir, paste0(destination_stub, ".", extension))
    if (!file.exists(source_file)) stop("Missing generated figure: ", source_file)
    if (!file.copy(source_file, destination_file, overwrite = TRUE)) {
      stop("Could not copy ", source_file, " to ", destination_file)
    }
  }
}

generated <- "08_Analysis_Outputs/figures"
main <- "03_Main_Figures"
for (stub in c(
  "Figure_1_Cohort_Derivation",
  "Figure_2_Heatmap_BPxUACR_10yr_Risk",
  "Figure_3_WithinCategory_ARD",
  "Figure_4_RCS_Continuous_UACR",
  "Figure_5_Subgroup_Forest_Low-grade",
  "Graphical_Abstract_BPxUACR")) {
  copy_set(generated, stub, main)
}

supplement <- "05_Supplementary_Figures"
for (stub in c(
  "Supplementary_Figure_S1_Participant_Attrition",
  "Supplementary_Figure_S2_Missingness_byCycle",
  "Supplementary_Figure_S3_UACR_Prevalence",
  "Supplementary_Figure_S4_Landmark_KM_AllCause",
  "Supplementary_Figure_S5_Landmark_BPxUACR_AllCause",
  "Supplementary_Figure_S6_Forest_Albuminuria_AllCause",
  "Supplementary_Figure_S7_RCS_Continuous_HeartDisease",
  "Supplementary_Figure_S8_Landmark_KM_HeartDisease",
  "Supplementary_Figure_S9_CRP_by_UACR",
  "Supplementary_Figure_S10_RiskProfile_BPxUACR")) {
  copy_set(generated, stub, supplement)
}

cat("Submission figure folders synchronized: 6 main, 10 supplementary.\n")
