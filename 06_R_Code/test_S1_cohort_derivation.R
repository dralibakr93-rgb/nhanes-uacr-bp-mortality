# Supplementary Table S1: sequential cohort derivation flow.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
# Supplementary Table S1: cohort derivation.
flow <- read.csv(file.path(paths$data, "cohort_flow_v2.csv"),
                 stringsAsFactors = FALSE)
write.csv(flow, file.path(paths$tables, "S1_cohort_derivation.csv"),
          row.names = FALSE)
