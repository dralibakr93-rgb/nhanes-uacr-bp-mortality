# Supplementary Table S6: scaled Schoenfeld-residual proportional-hazards diagnostics.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
m3_required <- unique(c(
  "ftime_years", "death_allcause", "uacr_cat", model_terms$M3,
  "wt_pooled", "strata", "psu"
))
# Supplementary Table S6: unweighted Schoenfeld-residual diagnostics.
m3_cc <- cohort[complete.cases(cohort[, m3_required]), ]
ph_fit <- coxph(model_formula("M3"), data = m3_cc, x = TRUE)
ph_test <- cox.zph(ph_fit, terms = TRUE, global = TRUE)
ph_table <- data.frame(
  term = rownames(ph_test$table),
  chisq = ph_test$table[, "chisq"],
  df = ph_test$table[, "df"],
  p = ph_test$table[, "p"],
  method = "Unweighted Schoenfeld-residual test"
)
write.csv(ph_table, file.path(paths$tables, "S6_ph_diagnostics.csv"),
          row.names = FALSE)
