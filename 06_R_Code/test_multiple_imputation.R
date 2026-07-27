# Multiple-imputation sensitivity (m=20), Models 3-5. Outputs: model_hierarchy_mi.csv, multiple_imputation_m20.rds
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
suppressPackageStartupMessages({
  library(mice)
  library(mitools)
})
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
# Impute all covariates required through Model 5. Design variables are included
# as predictors but are not imputed.
imputation_variables <- unique(c(
  "ftime_years", "death_allcause", "uacr_cat", unlist(model_terms),
  "wt_pooled", "strata", "psu"
))
imputation_data <- cohort[, imputation_variables]
na_fit <- survfit(Surv(ftime_years, death_allcause) ~ 1, data = cohort)
imputation_data$nelson_aalen <- approx(
  na_fit$time, na_fit$cumhaz, xout = cohort$ftime_years, rule = 2
)$y

methods <- mice::make.method(imputation_data)
methods[c("ftime_years", "death_allcause", "uacr_cat", "age", "sex_female",
          "race_eth", "cycle", "dm_main", "bp_stage", "egfr",
          "dyslipidemia_main", "wt_pooled", "strata", "psu",
          "nelson_aalen")] <- ""
predictors <- mice::make.predictorMatrix(imputation_data)
diag(predictors) <- 0
predictors[, c("psu", "strata")] <- 1

imputations <- mice(
  imputation_data,
  m = 20,
  maxit = 10,
  method = methods,
  predictorMatrix = predictors,
  seed = 20260629,
  printFlag = FALSE
)

pool_model <- function(model) {
  fits <- vector("list", imputations$m)
  for (i in seq_len(imputations$m)) {
    completed <- complete(imputations, i)
    completed$psu <- cohort$psu
    completed$strata <- cohort$strata
    completed$wt_pooled <- cohort$wt_pooled
    fit_design <- make_design(completed)
    fits[[i]] <- svycoxph(model_formula(model), design = fit_design)
  }
  pooled <- MIcombine(
    results = lapply(fits, coef),
    variances = lapply(fits, vcov)
  )
  idx <- grep("^uacr_cat", names(coef(pooled)))
  estimate <- coef(pooled)[idx]
  variance <- diag(vcov(pooled))[idx]
  df <- pooled$df[idx]
  critical <- qt(0.975, df = df)
  p_value <- 2 * pt(-abs(estimate / sqrt(variance)), df = df)
  data.frame(
    analysis = "Multiple imputation",
    model = model,
    N = nrow(cohort),
    events = sum(cohort$death_allcause),
    subclinical = fmt_ci(exp(estimate[1]),
                         exp(estimate[1] - critical[1] * sqrt(variance[1])),
                         exp(estimate[1] + critical[1] * sqrt(variance[1]))),
    subclinical_p = fmt_p(p_value[1]),
    albuminuria = fmt_ci(exp(estimate[2]),
                        exp(estimate[2] - critical[2] * sqrt(variance[2])),
                        exp(estimate[2] + critical[2] * sqrt(variance[2]))),
    albuminuria_p = fmt_p(p_value[2]),
    stringsAsFactors = FALSE
  )
}

mi_results <- do.call(rbind, lapply(c("M3", "M4", "M5"), pool_model))
write.csv(mi_results, file.path(paths$tables, "model_hierarchy_mi.csv"),
          row.names = FALSE)
saveRDS(imputations, file.path(paths$output, "multiple_imputation_m20.rds"))
