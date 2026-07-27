# Figure 4: continuous UACR and all-cause mortality (restricted cubic spline).
#
# Figure 4 (all-cause) and Supplementary Figure S7 (heart-disease) are a MATCHED
# restricted-cubic-spline pair drawn with an identical survey-weighted-density
# panel (harmonized 2026-07-09). Both are produced by the RCS generator below,
# which writes:
#   08_Analysis_Outputs/figures/Figure_4_RCS_Continuous_UACR.{png,pdf,tiff}
#   08_Analysis_Outputs/figures/Supplementary_Figure_S7_RCS_Continuous_HeartDisease.{...}
#
# (The earlier stand-alone version of this script drew the density as an embedded
# strip; it is preserved in the project backups. This wrapper keeps the pipeline
# and the shipped figures consistent.)

source("06_R_Code/supplementary_figure_generators/gen_S7_rcs_continuous.R")
