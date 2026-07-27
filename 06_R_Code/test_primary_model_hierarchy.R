# Primary complete-case model hierarchy (Models 1-5). Output: model_hierarchy_complete_case.csv
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
design <- make_design(cohort)

complete_case_rows <- list()
fits_complete_case <- list()

for (model in names(model_terms)) {
  fit <- svycoxph(model_formula(model), design = design)
  fits_complete_case[[model]] <- fit
  estimates <- extract_uacr(fit)
  model_frame <- model.frame(fit)
  events <- sum(model.response(model_frame)[, "status"])
  complete_case_rows[[model]] <- data.frame(
    analysis = "Complete-case",
    model = model,
    N = fit$n,
    events = events,
    subclinical = fmt_ci(estimates$HR[1], estimates$lower[1], estimates$upper[1]),
    subclinical_p = fmt_p(estimates$p[1]),
    albuminuria = fmt_ci(estimates$HR[2], estimates$lower[2], estimates$upper[2]),
    albuminuria_p = fmt_p(estimates$p[2]),
    stringsAsFactors = FALSE
  )
}
complete_case_results <- do.call(rbind, complete_case_rows)
write.csv(complete_case_results,
          file.path(paths$tables, "model_hierarchy_complete_case.csv"),
          row.names = FALSE)
