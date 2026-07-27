# Supplementary Table S11: cause-specific and Fine-Gray competing-risk models (heart disease).
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))

fit_uacr_design <- function(fit_design, formula, label) {
  fit <- svycoxph(formula, design = fit_design, model = TRUE)
  estimates <- extract_uacr(fit)
  model_frame <- model.frame(fit)
  event_count <- sum(model.response(model_frame)[, "status"])
  data.frame(
    analysis = label,
    N = fit$n,
    events = event_count,
    subclinical_HR = estimates$HR[1],
    subclinical_lower = estimates$lower[1],
    subclinical_upper = estimates$upper[1],
    subclinical_p = estimates$p[1],
    albuminuria_HR = estimates$HR[2],
    albuminuria_lower = estimates$lower[2],
    albuminuria_upper = estimates$upper[2],
    albuminuria_p = estimates$p[2],
    stringsAsFactors = FALSE
  )
}

full_design <- make_design(cohort)
m3_required <- unique(c(
  "ftime_years", "death_allcause", "uacr_cat", model_terms$M3,
  "wt_pooled", "strata", "psu"
))
m3_cc <- cohort[complete.cases(cohort[, m3_required]), ]
# Supplementary Table S11: cause-specific and Fine-Gray models.
heart_cc <- m3_cc
cause_weighted <- fit_uacr_design(
  full_design, model_formula("M3", outcome = "death_hd"),
  "Cause-specific, survey-weighted"
)

heart_cc$competing_event <- ifelse(
  heart_cc$death_hd == 1, 1,
  ifelse(heart_cc$death_allcause == 1, 2, 0)
)
heart_cc$competing_factor <- factor(
  heart_cc$competing_event, levels = 0:2,
  labels = c("Censored", "Heart disease", "Other death")
)
finegray_variables <- unique(c(
  "SEQN", "ftime_years", "competing_factor", "uacr_cat", model_terms$M3,
  "wt_pooled", "psu", "strata"
))
fg_data <- finegray(
  Surv(ftime_years, competing_factor) ~ .,
  data = heart_cc[, finegray_variables], etype = "Heart disease"
)
fg_data$analysis_weight <- fg_data$fgwt * fg_data$wt_pooled
fg_design <- svydesign(
  ids = ~psu, strata = ~strata, weights = ~analysis_weight,
  nest = TRUE, data = fg_data
)
fg_formula <- as.formula(paste0(
  "Surv(fgstart, fgstop, fgstatus) ~ uacr_cat + ",
  paste(model_terms$M3, collapse = " + ")
))
fg_weighted_fit <- svycoxph(fg_formula, design = fg_design)
fg_weighted_estimates <- extract_uacr(fg_weighted_fit)

cause_unweighted_fit <- coxph(
  model_formula("M3", outcome = "death_hd"), data = heart_cc
)
cause_unweighted_estimates <- extract_uacr(cause_unweighted_fit)
fg_unweighted_fit <- coxph(
  fg_formula, data = fg_data, weights = fgwt, robust = TRUE,
  cluster = SEQN
)
fg_unweighted_estimates <- extract_uacr(fg_unweighted_fit)

competing_row <- function(label, estimates) {
  data.frame(
    analysis = label,
    subclinical_HR = estimates$HR[1],
    subclinical_lower = estimates$lower[1],
    subclinical_upper = estimates$upper[1],
    subclinical_p = estimates$p[1],
    albuminuria_HR = estimates$HR[2],
    albuminuria_lower = estimates$lower[2],
    albuminuria_upper = estimates$upper[2],
    albuminuria_p = estimates$p[2],
    stringsAsFactors = FALSE
  )
}
finegray_weighted_row <- competing_row(
  "Fine-Gray, survey-weighted", fg_weighted_estimates
)
common_columns <- names(finegray_weighted_row)
competing_results <- rbind(
  cause_weighted[, common_columns],
  finegray_weighted_row,
  competing_row("Cause-specific, unweighted", cause_unweighted_estimates),
  competing_row("Fine-Gray, unweighted", fg_unweighted_estimates)
)
write.csv(competing_results,
          file.path(paths$tables, "S11_competing_risk.csv"), row.names = FALSE)
