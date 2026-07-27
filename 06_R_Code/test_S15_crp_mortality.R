# Supplementary Table S15: mortality models before and after adding log(CRP).
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
# Supplementary Table S15: mortality models before and after CRP adjustment.
crp_mortality_rows <- list()
for (assay in c("standard CRP", "hs-CRP")) {
  assay_rows <- cohort$crp_assay == assay & !is.na(cohort$log_crp)
  assay_design <- full_design[which(assay_rows), ]
  formulas <- list(
    "Model 3" = model_formula("M3"),
    "Model 3 + log CRP" = update(model_formula("M3"), . ~ . + log_crp)
  )
  crp_mortality_rows[[assay]] <- do.call(rbind, lapply(names(formulas), function(label) {
    fit <- quietly(svycoxph(formulas[[label]], design = assay_design, model = TRUE))
    estimates <- extract_uacr(fit)
    data.frame(
      assay = assay,
      model = label,
      N = fit$n,
      deaths = sum(model.response(model.frame(fit))[, "status"]),
      subclinical_HR = estimates$HR[1],
      subclinical_lower = estimates$lower[1],
      subclinical_upper = estimates$upper[1],
      albuminuria_HR = estimates$HR[2],
      albuminuria_lower = estimates$lower[2],
      albuminuria_upper = estimates$upper[2],
      stringsAsFactors = FALSE
    )
  }))
}
write.csv(do.call(rbind, crp_mortality_rows),
          file.path(paths$tables, "S15_mortality_crp.csv"), row.names = FALSE)
