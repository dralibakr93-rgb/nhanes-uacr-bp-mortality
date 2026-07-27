# Canonical wrapper for the hybrid two-year landmark heart-disease figure.
source(paste0(
  "06_R_Code/supplementary_figure_generators/supplementary_figure_generators/",
  "gen_S8_landmark_KM_heartdisease.R"))

generated <- file.path(
  "06_R_Code/supplementary_figure_generators/output",
  paste0("KM_2yrLandmark_UACR_HeartDisease.", c("png", "pdf", "tiff")))
destinations <- c(
  file.path("08_Analysis_Outputs/figures", paste0(
    "Supplementary_Figure_S8_Landmark_KM_HeartDisease.", c("png", "pdf", "tiff"))))
for (destination in destinations) {
  source_file <- generated[match(tools::file_ext(destination), tools::file_ext(generated))]
  file.copy(source_file, destination, overwrite = TRUE)
}
