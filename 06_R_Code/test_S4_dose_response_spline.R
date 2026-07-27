# Supplementary Table S4 (functional form): continuous log2(UACR) and restricted cubic spline.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
# Supplementary Tables S4-S5: continuous functional form and measurement error.
continuous_formula <- update(model_formula("M3"), . ~ uacr_log2 + . - uacr_cat)
continuous_fit <- svycoxph(continuous_formula, design = make_design(cohort),
                           model = TRUE)
continuous_summary <- quietly(summary(continuous_fit))
continuous_index <- match("uacr_log2", rownames(continuous_summary$conf.int))
continuous_row <- continuous_summary$conf.int[continuous_index, ]
continuous_p <- continuous_summary$coefficients[
  continuous_index, ncol(continuous_summary$coefficients)
]

knots <- as.numeric(quantile(cohort$uacr_log2, c(0.05, 0.35, 0.65, 0.95),
                             na.rm = TRUE))
rcs_basis <- Hmisc::rcspline.eval(cohort$uacr_log2, knots = knots, inclx = TRUE)
colnames(rcs_basis) <- c("rcs_linear", "rcs_nonlinear1", "rcs_nonlinear2")
rcs_data <- cbind(cohort, rcs_basis)
rcs_formula <- Surv(ftime_years, death_allcause) ~
  rcs_linear + rcs_nonlinear1 + rcs_nonlinear2 + age + sex_female +
  race_eth + cycle + bmi + smoking + dm_main + bp_stage + egfr
rcs_fit <- svycoxph(rcs_formula, design = make_design(rcs_data))
overall_test <- regTermTest(
  rcs_fit, ~rcs_linear + rcs_nonlinear1 + rcs_nonlinear2
)
nonlinear_test <- regTermTest(
  rcs_fit, ~rcs_nonlinear1 + rcs_nonlinear2
)
spline_names <- c("rcs_linear", "rcs_nonlinear1", "rcs_nonlinear2")
spline_beta <- coef(rcs_fit)[spline_names]
spline_variance <- vcov(rcs_fit)[spline_names, spline_names]
spline_contrast <- function(values_mg_g, reference_mg_g = 10) {
  value_basis <- Hmisc::rcspline.eval(
    log2(values_mg_g), knots = knots, inclx = TRUE
  )
  reference_basis <- Hmisc::rcspline.eval(
    log2(reference_mg_g), knots = knots, inclx = TRUE
  )
  basis_difference <- sweep(value_basis, 2, reference_basis)
  linear_predictor <- drop(basis_difference %*% spline_beta)
  standard_error <- sqrt(pmax(
    0, rowSums((basis_difference %*% spline_variance) * basis_difference)
  ))
  data.frame(
    HR = exp(linear_predictor),
    lower = exp(linear_predictor - qnorm(0.975) * standard_error),
    upper = exp(linear_predictor + qnorm(0.975) * standard_error)
  )
}
spline_15 <- spline_contrast(15)
spline_30 <- spline_contrast(30)
functional_form <- data.frame(
  HR_per_doubling = continuous_row[1],
  lower = continuous_row[3],
  upper = continuous_row[4],
  p_overall_linear = continuous_p,
  p_overall_spline = overall_test$p,
  p_nonlinearity = nonlinear_test$p,
  HR_at_15_mg_g = spline_15$HR,
  lower_at_15_mg_g = spline_15$lower,
  upper_at_15_mg_g = spline_15$upper,
  HR_at_30_mg_g = spline_30$HR,
  lower_at_30_mg_g = spline_30$lower,
  upper_at_30_mg_g = spline_30$upper,
  knot_1_mg_g = 2^knots[1],
  knot_2_mg_g = 2^knots[2],
  knot_3_mg_g = 2^knots[3],
  knot_4_mg_g = 2^knots[4]
)
write.csv(functional_form, file.path(paths$tables, "S4_functional_form.csv"),
          row.names = FALSE)
