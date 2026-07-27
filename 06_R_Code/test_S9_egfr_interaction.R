# Supplementary Table S9: UACR associations across preserved-eGFR strata and interaction.
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
# Supplementary Table S9: preserved-eGFR strata and interaction.
cohort$egfr_group <- cut(
  cohort$egfr, breaks = c(60, 75, 90, Inf), right = FALSE,
  labels = c("60-74", "75-89", ">=90")
)
egfr_adjustment <- setdiff(model_terms$M3, "egfr")
egfr_formula <- as.formula(paste0(
  "Surv(ftime_years, death_allcause) ~ uacr_cat + ",
  paste(egfr_adjustment, collapse = " + ")
))
egfr_full_design <- make_design(cohort)
egfr_rows <- lapply(levels(cohort$egfr_group), function(group) {
  fit_uacr_design(
    subset(egfr_full_design, egfr_group == group), egfr_formula,
    paste("eGFR", group)
  )
})
interaction_design <- egfr_full_design
interaction_base <- as.formula(paste0(
  "Surv(ftime_years, death_allcause) ~ uacr_cat + egfr_group + ",
  paste(egfr_adjustment, collapse = " + ")
))
interaction_fit <- svycoxph(
  update(interaction_base, . ~ . + uacr_cat:egfr_group),
  design = interaction_design
)
interaction_test <- regTermTest(interaction_fit, ~uacr_cat:egfr_group)
egfr_results <- do.call(rbind, egfr_rows)
egfr_results$interaction_p <- rep(as.numeric(interaction_test$p),
                                  nrow(egfr_results))
write.csv(egfr_results, file.path(paths$tables, "S9_egfr_interaction.csv"),
          row.names = FALSE)
