# Canonical wrapper for the two-year landmark all-cause figure. The maintained
# generator displays pre-landmark follow-up and restarts the curves at zero at
# the landmark among two-year survivors.
source(paste0(
  "06_R_Code/supplementary_figure_generators/supplementary_figure_generators/",
  "gen_S4_landmark_KM_allcause.R"))

generated <- file.path(
  "06_R_Code/supplementary_figure_generators/output",
  paste0("KM_2yrLandmark_UACR_AllCause.", c("png", "pdf", "tiff")))
destinations <- c(
  file.path("08_Analysis_Outputs/figures", paste0(
    "Supplementary_Figure_S4_Landmark_KM_AllCause.", c("png", "pdf", "tiff"))))
for (destination in destinations) {
  source_file <- generated[match(tools::file_ext(destination), tools::file_ext(generated))]
  file.copy(source_file, destination, overwrite = TRUE)
}
