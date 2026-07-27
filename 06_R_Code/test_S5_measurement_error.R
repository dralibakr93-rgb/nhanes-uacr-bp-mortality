# Supplementary Table S5: regression-calibration correction for single-sample UACR error.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
continuous_formula <- update(model_formula("M3"), . ~ uacr_log2 + . - uacr_cat)
continuous_fit <- svycoxph(continuous_formula, design = make_design(cohort),
                           model = TRUE)
continuous_beta <- coef(continuous_fit)["uacr_log2"]
continuous_se <- sqrt(vcov(continuous_fit)["uacr_log2", "uacr_log2"])
reliability <- c(1.0, 0.8, 0.7, 0.6)
measurement_error <- data.frame(
  reliability = reliability,
  corrected_HR = exp(continuous_beta / reliability),
  lower = exp((continuous_beta - qnorm(0.975) * continuous_se) / reliability),
  upper = exp((continuous_beta + qnorm(0.975) * continuous_se) / reliability)
)
write.csv(measurement_error,
          file.path(paths$tables, "S5_measurement_error.csv"), row.names = FALSE)
