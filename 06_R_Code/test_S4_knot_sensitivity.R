# Supplementary Table S4 (knots): spline nonlinearity across 3/4/5-knot specifications.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
knot_probability_sets <- list(
  "3 knots (10th, 50th, 90th percentiles)" = c(0.10, 0.50, 0.90),
  "4 knots (5th, 35th, 65th, 95th percentiles)" = c(0.05, 0.35, 0.65, 0.95),
  "5 knots (5th, 27.5th, 50th, 72.5th, 95th percentiles)" =
    c(0.05, 0.275, 0.50, 0.725, 0.95)
)
knot_sensitivity <- do.call(rbind, lapply(names(knot_probability_sets), function(label) {
  knot_values <- as.numeric(quantile(
    cohort$uacr_log2, knot_probability_sets[[label]], na.rm = TRUE
  ))
  basis <- Hmisc::rcspline.eval(
    cohort$uacr_log2, knots = knot_values, inclx = TRUE
  )
  basis_names <- paste0("spline_basis_", seq_len(ncol(basis)))
  colnames(basis) <- basis_names
  spline_data <- cbind(cohort, basis)
  spline_formula <- as.formula(paste0(
    "Surv(ftime_years, death_allcause) ~ ",
    paste(c(basis_names, model_terms$M3), collapse = " + ")
  ))
  spline_fit <- svycoxph(spline_formula, design = make_design(spline_data))
  overall <- regTermTest(
    spline_fit, as.formula(paste0("~", paste(basis_names, collapse = " + ")))
  )$p
  nonlinear <- regTermTest(
    spline_fit,
    as.formula(paste0("~", paste(basis_names[-1], collapse = " + ")))
  )$p
  data.frame(
    specification = label,
    overall_p = overall,
    nonlinearity_p = nonlinear,
    knot_locations_mg_g = paste(round(2^knot_values, 2), collapse = ", "),
    stringsAsFactors = FALSE
  )
}))
write.csv(knot_sensitivity,
          file.path(paths$tables, "S4_knot_sensitivity.csv"), row.names = FALSE)
