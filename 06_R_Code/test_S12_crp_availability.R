# Supplementary Table S12 (CRP availability): CRP assay availability by cycle.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
crp <- readRDS(file.path(paths$data, "crp_harmonized.rds"))
crp_index <- match(cohort$SEQN, crp$SEQN)
cohort$crp_mg_l <- crp$crp_mgL[crp_index]
cohort$crp_assay <- crp$crp_assay[crp_index]
cohort$log_crp <- log(cohort$crp_mg_l)

# CBC fields are rebuilt directly from the supplied legacy and contemporary
# laboratory domains.
raw_domains <- readRDS(file.path(paths$data, "raw_domains.rds"))
cbc_columns <- c("SEQN", "LBXHGB", "LBXWBCSI", "LBXPLTSI", "LBXRDW")
cbc <- rbind(
  raw_domains$cbc_legacy[, cbc_columns],
  raw_domains$cbc[, cbc_columns]
)
cbc <- cbc[!duplicated(cbc$SEQN), ]
cbc_index <- match(cohort$SEQN, cbc$SEQN)
cohort$hemoglobin <- cbc$LBXHGB[cbc_index]
cohort$wbc <- cbc$LBXWBCSI[cbc_index]
cohort$platelets <- cbc$LBXPLTSI[cbc_index]
cohort$rdw <- cbc$LBXRDW[cbc_index]
full_design <- make_design(cohort)
# Supplementary Table S12: CRP availability.
crp_availability <- as.data.frame(table(
  factor(
    ifelse(is.na(cohort$crp_mg_l), "Not measured", cohort$crp_assay),
    levels = c("standard CRP", "hs-CRP", "Not measured")
  )
))
names(crp_availability) <- c("assay", "N")
write.csv(crp_availability, file.path(paths$tables, "S12_crp_availability.csv"),
          row.names = FALSE)
