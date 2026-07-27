# Supplementary Table S8 (E-values): E-values for the primary Model 3 associations.
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
primary_m3_fit <- svycoxph(model_formula("M3"), design = full_design)
primary_m3_estimates <- extract_uacr(primary_m3_fit)
e_value <- function(ratio) ratio + sqrt(ratio * (ratio - 1))
evalue_results <- data.frame(
  exposure = c("Low-grade albuminuria", "Albuminuria"),
  HR = primary_m3_estimates$HR,
  confidence_limit_closest_to_null = primary_m3_estimates$lower,
  E_value_point = e_value(primary_m3_estimates$HR),
  E_value_confidence_limit = e_value(primary_m3_estimates$lower)
)
write.csv(evalue_results, file.path(paths$tables, "S8_evalues.csv"),
          row.names = FALSE)
