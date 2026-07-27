# Fasting-subsample sensitivity analysis for diabetes and dyslipidemia.
#
# This analysis uses the least-inclusive NHANES fasting subsample weight.
# For pooled NHANES 1999-2018, the 1999-2002 four-year fasting weight is
# divided by 5 and each 2003-2018 two-year fasting weight is divided by 10.
# Paired models are fit on identical complete-case samples so differences
# isolate the expanded fasting biomarker definitions rather than sample loss.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
raw_domains <- readRDS(file.path(paths$data, "raw_domains.rds"))

modern_fasting_weights <- transform(
  raw_domains$trigly[, c("SEQN", "WTSAF2YR")],
  WTSAF4YR = NA_real_
)
legacy_fasting_weights <- raw_domains$lipid_legacy_tg[
  , c("SEQN", "WTSAF2YR", "WTSAF4YR")
]
fasting_weights <- rbind(modern_fasting_weights, legacy_fasting_weights)
fasting_weights <- fasting_weights[!duplicated(fasting_weights$SEQN), ]

fasting_index <- match(cohort$SEQN, fasting_weights$SEQN)
cohort$wt_fast2 <- fasting_weights$WTSAF2YR[fasting_index]
cohort$wt_fast4 <- fasting_weights$WTSAF4YR[fasting_index]
cohort$wt_fast_pooled <- ifelse(
  cohort$cycle %in% c(1, 2),
  cohort$wt_fast4 / 5,
  cohort$wt_fast2 / 10
)
cohort$wt_fast_pooled[
  !is.finite(cohort$wt_fast_pooled) | cohort$wt_fast_pooled <= 0
] <- NA_real_

# Fasting glucose and triglycerides must be observed. Calculated LDL may be
# missing when triglycerides are high; all such participants in this cohort
# already satisfy the fasting dyslipidemia definition through TG >=200 mg/dL.
fasting_eligible <- with(
  cohort,
  !is.na(wt_fast_pooled) & !is.na(fpg) & !is.na(tg) &
    !is.na(dm_fasting) & !is.na(dyslipidemia_fasting)
)
fasting_cohort <- droplevels(cohort[fasting_eligible, ])

m3_primary_formula <- Surv(ftime_years, death_allcause) ~
  uacr_cat + age + sex_female + race_eth + cycle + bmi + smoking +
  dm_main + bp_stage + egfr
m3_fasting_formula <- update(
  m3_primary_formula, . ~ . - dm_main + dm_fasting
)
m5_primary_formula <- update(
  m3_primary_formula, . ~ . + pir + educ + dyslipidemia_main
)
m5_fasting_formula <- update(
  m5_primary_formula,
  . ~ . - dm_main - dyslipidemia_main + dm_fasting + dyslipidemia_fasting
)

extract_fit <- function(fit, analysis, comparison_set, diabetes_definition,
                        dyslipidemia_definition) {
  estimates <- extract_uacr(fit)
  model_frame <- model.frame(fit)
  outcome <- model.response(model_frame)
  data.frame(
    analysis = analysis,
    comparison_set = comparison_set,
    diabetes_definition = diabetes_definition,
    dyslipidemia_definition = dyslipidemia_definition,
    N = fit$n,
    events = sum(outcome[, "status"]),
    weighted_population = sum(weights(fit$survey.design, "sampling")),
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

fit_pair <- function(data, primary_formula, fasting_formula, comparison_set,
                     primary_dyslipidemia, fasting_dyslipidemia) {
  required <- unique(c(
    all.vars(primary_formula), all.vars(fasting_formula),
    "wt_fast_pooled", "strata", "psu"
  ))
  # Domain (subpopulation) analysis: build the design once on the full fasting-
  # eligible sample and subset to the paired complete cases, so design-based
  # standard errors retain the full strata/PSU structure (NCHS subpopulation
  # guidance) instead of rebuilding the design on deleted records.
  data$in_paired_sample <- complete.cases(data[, required])
  full_design <- make_design(data, weight = "wt_fast_pooled")
  paired_design <- subset(full_design, in_paired_sample)
  primary_fit <- svycoxph(primary_formula, design = paired_design, model = TRUE)
  fasting_fit <- svycoxph(fasting_formula, design = paired_design, model = TRUE)
  rbind(
    extract_fit(
      primary_fit,
      paste0(comparison_set, ": primary definitions"),
      comparison_set,
      "HbA1c, diagnosis, or medication",
      primary_dyslipidemia
    ),
    extract_fit(
      fasting_fit,
      paste0(comparison_set, ": fasting-expanded definitions"),
      comparison_set,
      "Primary definition plus fasting glucose >=126 mg/dL",
      fasting_dyslipidemia
    )
  )
}

m3_results <- fit_pair(
  fasting_cohort,
  m3_primary_formula,
  m3_fasting_formula,
  "Fasting-subsample Model 3",
  "Not included in Model 3",
  "Not included in Model 3"
)
m5_results <- fit_pair(
  fasting_cohort,
  m5_primary_formula,
  m5_fasting_formula,
  "Fasting-subsample Model 5",
  "Medication, total cholesterol, or low HDL",
  paste0(
    "Primary definition plus LDL >=160 mg/dL or ",
    "fasting triglycerides >=200 mg/dL"
  )
)

fasting_results <- rbind(m3_results, m5_results)
fasting_results$weight <- paste0(
  "WTSAF4YR/5 (1999-2002); WTSAF2YR/10 (2003-2018)"
)
write.csv(
  fasting_results,
  file.path(paths$tables, "S20_fasting_subsample_sensitivity.csv"),
  row.names = FALSE
)

design_audit <- data.frame(
  metric = c(
    "Eligible fasting cohort before model-specific complete cases",
    "All-cause deaths in eligible fasting cohort",
    "Observed PSU-stratum cells",
    "Observed strata",
    "Design degrees of freedom",
    "LDL missing with fasting TG observed",
    "LDL missing and fasting TG >=200 mg/dL"
  ),
  value = c(
    nrow(fasting_cohort),
    sum(fasting_cohort$death_allcause),
    nrow(unique(fasting_cohort[, c("strata", "psu")])),
    length(unique(fasting_cohort$strata)),
    nrow(unique(fasting_cohort[, c("strata", "psu")])) -
      length(unique(fasting_cohort$strata)),
    sum(is.na(fasting_cohort$ldl)),
    sum(is.na(fasting_cohort$ldl) & fasting_cohort$tg >= 200)
  ),
  stringsAsFactors = FALSE
)
write.csv(
  design_audit,
  file.path(paths$tables, "S20_fasting_subsample_design_audit.csv"),
  row.names = FALSE
)
