# Supplementary Table S8: complete-case sensitivity analyses (landmark, subgroups, treatment definitions).
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))

# Serum albumin (LBXSAL, g/dL) from the standard biochemistry profile across all
# cycles (biopro 2003-2018; scr_legacy 1999-2002), for a sensitivity analysis
# matching the covariate set used by Claudel et al.
.rd <- readRDS(file.path(paths$data, "raw_domains.rds"))
.alb <- rbind(.rd$biopro[!is.na(.rd$biopro$LBXSAL), c("SEQN", "LBXSAL")],
              .rd$scr_legacy[!is.na(.rd$scr_legacy$LBXSAL), c("SEQN", "LBXSAL")])
.alb <- .alb[!duplicated(.alb$SEQN), ]
cohort$serum_albumin <- .alb$LBXSAL[match(cohort$SEQN, .alb$SEQN)]

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
# Supplementary Table S8: complete-case sensitivity analyses.
rhs_m3 <- paste(c("uacr_cat", model_terms$M3), collapse = " + ")
sensitivity_rows <- list()

landmark_design <- update(full_design, time_after_2y = ftime_years - 2)
landmark_design <- subset(landmark_design, ftime_years > 2)
landmark_formula <- as.formula(paste0(
  "Surv(time_after_2y, death_allcause) ~ ", rhs_m3
))
sensitivity_rows[["landmark"]] <- fit_uacr_design(
  landmark_design, landmark_formula, "2-year landmark"
)

no_diabetes_formula <- as.formula(paste0(
  "Surv(ftime_years, death_allcause) ~ uacr_cat + ",
  paste(setdiff(model_terms$M3, "dm_main"), collapse = " + ")
))
sensitivity_rows[["no_diabetes"]] <- fit_uacr_design(
  subset(full_design, dm_main == 0), no_diabetes_formula, "No diabetes"
)

sex_formula <- as.formula(paste0(
  "Surv(ftime_years, death_allcause) ~ uacr_cat + ",
  paste(setdiff(model_terms$M3, "sex_female"), collapse = " + ")
))
sensitivity_rows[["women"]] <- fit_uacr_design(
  subset(full_design, sex_female == 1), sex_formula, "Women"
)
sensitivity_rows[["men"]] <- fit_uacr_design(
  subset(full_design, sex_female == 0), sex_formula, "Men"
)
bpq_formula <- update(model_formula("M3"), . ~ . - bp_stage + bp_stage_bpq)
sensitivity_rows[["bpq"]] <- fit_uacr_design(
  full_design, bpq_formula, "Self-report BP-treatment definition"
)
sensitivity_rows[["medication"]] <- fit_uacr_design(
  subset(full_design, rx_uacr == 0), model_formula("M3"),
  "Exclude UACR-lowering medication users"
)
albumin_formula <- update(model_formula("M3"), . ~ . + serum_albumin)
sensitivity_rows[["serum_albumin"]] <- fit_uacr_design(
  full_design, albumin_formula, "Additional adjustment for serum albumin"
)
sensitivity_results <- do.call(rbind, sensitivity_rows)
write.csv(sensitivity_results,
          file.path(paths$tables, "S8_sensitivity_complete_case.csv"),
          row.names = FALSE)
