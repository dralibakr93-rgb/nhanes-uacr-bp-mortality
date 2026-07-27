# Supplementary Table S13: CRP distribution within BP and UACR categories.
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
# Supplementary Table S13: CRP distribution within BP and UACR categories.
crp_distribution_rows <- list()
row_index <- 0
for (assay in c("standard CRP", "hs-CRP")) {
  assay_rows <- cohort$crp_assay == assay &
    !is.na(cohort$crp_mg_l) & cohort$crp_mg_l > 0
  assay_data <- cohort[which(assay_rows), ]
  assay_design <- full_design[which(assay_rows), ]
  for (bp in levels(cohort$bp_stage)) {
    for (uacr_level in levels(cohort$uacr_cat)) {
      selected <- subset(
        assay_design, bp_stage == bp & uacr_cat == uacr_level
      )
      selected_data <- assay_data[
        assay_data$bp_stage == bp & assay_data$uacr_cat == uacr_level,
      ]
      log_mean <- tryCatch(
        as.numeric(coef(quietly(svymean(~log_crp, selected, na.rm = TRUE)))),
        error = function(error) NA_real_
      )
      quantiles <- tryCatch(
        as.numeric(quietly(svyquantile(
          ~crp_mg_l, selected, c(0.25, 0.50, 0.75), na.rm = TRUE,
          ci = FALSE
        ))[[1]]),
        error = function(error) rep(NA_real_, 3)
      )
      row_index <- row_index + 1
      crp_distribution_rows[[row_index]] <- data.frame(
        assay = assay,
        bp_category = bp,
        uacr_category = uacr_level,
        N = nrow(selected_data),
        geometric_mean_mg_l = exp(log_mean),
        median_mg_l = quantiles[2],
        q1_mg_l = quantiles[1],
        q3_mg_l = quantiles[3],
        stringsAsFactors = FALSE
      )
    }
  }
}
write.csv(do.call(rbind, crp_distribution_rows),
          file.path(paths$tables, "S13_crp_distribution.csv"), row.names = FALSE)
