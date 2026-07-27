# Supplementary Table S16: covariate-omission and inflammatory-marker robustness.
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
# Supplementary Table S16: covariate-omission and inflammatory-marker models.
omission_sets <- c(
  "Model 3 (full)" = NA,
  "Omit BMI" = "bmi",
  "Omit smoking" = "smoking",
  "Omit diabetes" = "dm_main",
  "Omit BP-treatment category" = "bp_stage",
  "Omit eGFR" = "egfr"
)
model3_frame <- complete.cases(cohort[, unique(c(
  "ftime_years", "death_allcause", "uacr_cat", model_terms$M3
))])

fit_exploratory_model <- function(label, terms, rows, block) {
  formula <- as.formula(paste0(
    "Surv(ftime_years, death_allcause) ~ uacr_cat + ",
    paste(terms, collapse = " + ")
  ))
  fit_design <- full_design[which(rows), ]
  fit <- quietly(svycoxph(formula, design = fit_design, model = TRUE))
  estimates <- extract_uacr(fit)
  data.frame(
    block = block,
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
}

omission_rows <- lapply(names(omission_sets), function(label) {
  omitted <- omission_sets[[label]]
  terms <- if (is.na(omitted)) model_terms$M3 else setdiff(model_terms$M3, omitted)
  fit_exploratory_model(label, terms, model3_frame,
                        "Covariate-omission robustness")
})
omission_rows[[length(omission_rows) + 1]] <- fit_exploratory_model(
  "Minimally adjusted (Model 2)", model_terms$M2, model3_frame,
  "Covariate-omission robustness"
)

cbc_terms <- c(model_terms$M3, "hemoglobin", "wbc", "platelets", "rdw")
cbc_frame <- model3_frame & complete.cases(
  cohort[, c("hemoglobin", "wbc", "platelets", "rdw")]
)
omission_rows[[length(omission_rows) + 1]] <- fit_exploratory_model(
  "Model 3 + CBC", cbc_terms, cbc_frame,
  "Inflammatory-marker sensitivity"
)
for (assay in c("standard CRP", "hs-CRP")) {
  assay_frame <- cbc_frame & cohort$crp_assay == assay & !is.na(cohort$log_crp)
  omission_rows[[length(omission_rows) + 1]] <- fit_exploratory_model(
    paste0("Model 3 + CBC + log ", assay), c(cbc_terms, "log_crp"),
    assay_frame, "Inflammatory-marker sensitivity"
  )
}
write.csv(do.call(rbind, omission_rows),
          file.path(paths$tables, "S16_covariate_omission.csv"), row.names = FALSE)
