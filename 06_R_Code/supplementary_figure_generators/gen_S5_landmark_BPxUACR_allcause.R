# Canonical wrapper for the hybrid two-year landmark BP-by-UACR figure.
source(paste0(
  "06_R_Code/supplementary_figure_generators/supplementary_figure_generators/",
  "gen_S5_landmark_BPxUACR_allcause.R"))

generated <- file.path(
  "06_R_Code/supplementary_figure_generators/output",
  paste0("Supplementary_Figure_Landmark2yr_BPxUACR.", c("png", "pdf", "tiff")))
destinations <- c(
  file.path("08_Analysis_Outputs/figures", paste0(
    "Supplementary_Figure_S5_Landmark_BPxUACR_AllCause.", c("png", "pdf", "tiff"))))
for (destination in destinations) {
  source_file <- generated[match(tools::file_ext(destination), tools::file_ext(generated))]
  file.copy(source_file, destination, overwrite = TRUE)
}
