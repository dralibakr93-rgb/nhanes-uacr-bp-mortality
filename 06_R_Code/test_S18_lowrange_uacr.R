# Supplementary Table S18: expanded low-range UACR categories and trend.
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
# Supplementary Table S18: expanded low-range UACR categories.
cohort$uacr_four <- cut(
  cohort$uacr, breaks = c(-Inf, 5, 10, 30, Inf), right = FALSE,
  labels = c("<5 mg/g", "5-9 mg/g", "10-29 mg/g", ">=30 mg/g")
)
low_range_formula <- as.formula(paste0(
  "Surv(ftime_years, death_allcause) ~ uacr_four + ",
  paste(model_terms$M3, collapse = " + ")
))
low_range_design <- make_design(cohort)
low_range_fit <- quietly(svycoxph(low_range_formula, design = low_range_design, model = TRUE))
low_range_summary <- quietly(summary(low_range_fit))
low_range_indices <- grep("^uacr_four", rownames(low_range_summary$conf.int))
low_range_rows <- list(data.frame(
  uacr_category = "<5 mg/g", N = sum(cohort$uacr_four == "<5 mg/g"),
  deaths = sum(cohort$death_allcause[cohort$uacr_four == "<5 mg/g"]),
  HR = 1, lower = NA, upper = NA, p = NA
))
for (index in low_range_indices) {
  level <- sub("^uacr_four", "", rownames(low_range_summary$conf.int)[index])
  selected <- cohort$uacr_four == level
  low_range_rows[[length(low_range_rows) + 1]] <- data.frame(
    uacr_category = level,
    N = sum(selected),
    deaths = sum(cohort$death_allcause[selected]),
    HR = low_range_summary$conf.int[index, 1],
    lower = low_range_summary$conf.int[index, 3],
    upper = low_range_summary$conf.int[index, 4],
    p = low_range_summary$coefficients[index, ncol(low_range_summary$coefficients)]
  )
}
cohort$uacr_four_score <- as.numeric(cohort$uacr_four) - 1
trend_design <- quietly(update(
  low_range_design, uacr_four_score = as.numeric(uacr_four) - 1
))
trend_fit <- quietly(svycoxph(
  update(model_formula("M3"), . ~ uacr_four_score + . - uacr_cat),
  design = trend_design
))
trend_summary <- quietly(summary(trend_fit))
trend_p <- trend_summary$coefficients[
  "uacr_four_score", ncol(trend_summary$coefficients)
]
low_range_results <- do.call(rbind, low_range_rows)
low_range_results$p_trend <- trend_p
write.csv(low_range_results,
          file.path(paths$tables, "S18_low_range_uacr.csv"), row.names = FALSE)
