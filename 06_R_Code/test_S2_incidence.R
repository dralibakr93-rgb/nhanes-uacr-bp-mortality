# Supplementary Table S2 (incidence): design-based mortality incidence rates by UACR.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
# Supplementary Table S2, panel A: design-based incidence rates.
incidence_design <- update(make_design(cohort), events_1000 = 1000 * death_allcause)
incidence_rows <- lapply(levels(cohort$uacr_cat), function(level) {
  subset_design <- subset(incidence_design, uacr_cat == level)
  ratio <- svyratio(~events_1000, ~ftime_years, subset_design)
  limits <- confint(ratio)
  rows <- cohort$uacr_cat == level
  data.frame(
    uacr_category = level,
    N = sum(rows),
    deaths = sum(cohort$death_allcause[rows]),
    person_years_unweighted = sum(cohort$ftime_years[rows]),
    incidence_rate_per_1000_py = as.numeric(coef(ratio)),
    rate_lower = limits[1],
    rate_upper = limits[2],
    stringsAsFactors = FALSE
  )
})
write.csv(do.call(rbind, incidence_rows),
          file.path(paths$tables, "S2_incidence.csv"), row.names = FALSE)
