# Supplementary Table S10: heart-disease mortality model hierarchy (exploratory).
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
# Supplementary Table S10: heart-disease mortality model hierarchy.
heart_rows <- lapply(names(model_terms), function(model) {
  result <- fit_uacr_design(
    full_design, model_formula(model, outcome = "death_hd"),
    paste("Model", substring(model, 2))
  )
  result$model <- model
  result
})
heart_results <- do.call(rbind, heart_rows)
write.csv(heart_results,
          file.path(paths$tables, "S10_heart_disease_hierarchy.csv"),
          row.names = FALSE)
