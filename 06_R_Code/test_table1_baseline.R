# Table 1: survey-weighted baseline characteristics by UACR category.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
cohort$age_group <- cut(
  cohort$age, c(30, 45, 60, 75, Inf), right = FALSE,
  labels = c("30-44", "45-59", "60-74", ">=75")
)
cohort$bmi_group <- cut(
  cohort$bmi, c(-Inf, 18.5, 25, 30, Inf), right = FALSE,
  labels = c("Underweight (<18.5)", "Normal (18.5-<25)",
             "Overweight (25-<30)", "Obesity (>=30)")
)
cohort$education_group <- factor(
  ifelse(cohort$educ %in% c(1, 2), "< High school",
         ifelse(cohort$educ == 3, "High school / GED",
                ifelse(cohort$educ == 4, "Some college / AA",
                       ifelse(cohort$educ == 5, "College graduate +", NA)))),
  levels = c("< High school", "High school / GED", "Some college / AA",
             "College graduate +")
)
cohort$non_hdl <- cohort$tc - cohort$hdl
cohort$diabetes_label <- factor(
  cohort$dm_main, levels = c(0, 1), labels = c("No", "Yes")
)
cohort$glycemic_status <- factor(
  ifelse(cohort$dm_main == 1, "Diabetes",
         ifelse(cohort$hba1c >= 5.7, "Prediabetes", "Normoglycemia")),
  levels = c("Normoglycemia", "Prediabetes", "Diabetes")
)
cohort$bp_treatment_label <- factor(
  cohort$on_bp_treat, levels = c(0, 1), labels = c("No", "Yes")
)
cohort$lipid_treatment_label <- factor(
  cohort$on_lipid_lowering, levels = c(0, 1), labels = c("No", "Yes")
)
cohort$antiplatelet_label <- factor(
  cohort$on_antiplatelet_rx, levels = c(0, 1), labels = c("No", "Yes")
)

design <- make_design(cohort)
uacr_levels <- levels(cohort$uacr_cat)
group_designs <- c(list(Overall = design), lapply(uacr_levels, function(level) {
  subset(design, uacr_cat == level)
}))
names(group_designs) <- c("Overall", uacr_levels)

weighted_mean_sd <- function(variable, subset_design) {
  formula <- as.formula(paste0("~", variable))
  mean_value <- as.numeric(coef(svymean(formula, subset_design, na.rm = TRUE)))
  sd_value <- sqrt(as.numeric(coef(svyvar(formula, subset_design, na.rm = TRUE))))
  sprintf("%.1f (%.1f)", mean_value, sd_value)
}

weighted_median_iqr <- function(variable, subset_design, digits = 1) {
  formula <- as.formula(paste0("~", variable))
  quantiles <- as.numeric(svyquantile(
    formula, subset_design, c(0.25, 0.50, 0.75), na.rm = TRUE,
    ci = FALSE
  )[[1]])
  format_string <- paste0("%.", digits, "f [%.", digits, "f-%.", digits, "f]")
  sprintf(format_string, quantiles[2], quantiles[1], quantiles[3])
}

continuous_p <- function(variable) {
  fit <- svyglm(as.formula(paste0(variable, " ~ uacr_cat")), design = design)
  regTermTest(fit, ~uacr_cat)$p
}

categorical_p <- function(variable) {
  test <- svychisq(
    as.formula(paste0("~", variable, " + uacr_cat")), design = design,
    statistic = "F"
  )
  test$p.value
}

continuous_smd_pair <- function(variable, group_a, group_b) {
  design_a <- subset(design, uacr_cat == uacr_levels[group_a])
  design_b <- subset(design, uacr_cat == uacr_levels[group_b])
  formula <- as.formula(paste0("~", variable))
  mean_a <- as.numeric(coef(svymean(formula, design_a, na.rm = TRUE)))
  mean_b <- as.numeric(coef(svymean(formula, design_b, na.rm = TRUE)))
  variance_a <- as.numeric(coef(svyvar(formula, design_a, na.rm = TRUE)))
  variance_b <- as.numeric(coef(svyvar(formula, design_b, na.rm = TRUE)))
  abs(mean_a - mean_b) / sqrt((variance_a + variance_b) / 2)
}

categorical_smd_pair <- function(variable, group_a, group_b) {
  design_a <- subset(design, uacr_cat == uacr_levels[group_a])
  design_b <- subset(design, uacr_cat == uacr_levels[group_b])
  formula <- as.formula(paste0("~factor(", variable, ")"))
  proportion_a <- coef(svymean(formula, design_a, na.rm = TRUE))
  proportion_b <- coef(svymean(formula, design_b, na.rm = TRUE))
  if (length(proportion_a) < 2) return(NA_real_)
  use <- seq_len(length(proportion_a) - 1)
  difference <- proportion_a[use] - proportion_b[use]
  covariance_a <- diag(as.numeric(proportion_a[use]), nrow = length(use)) -
    outer(proportion_a[use], proportion_a[use])
  covariance_b <- diag(as.numeric(proportion_b[use]), nrow = length(use)) -
    outer(proportion_b[use], proportion_b[use])
  pooled <- (covariance_a + covariance_b) / 2
  sqrt(max(0, drop(t(difference) %*% MASS::ginv(pooled) %*% difference)))
}

maximum_smd <- function(variable, categorical = FALSE) {
  pairs <- list(c(1, 2), c(1, 3), c(2, 3))
  values <- vapply(pairs, function(pair) {
    if (categorical) {
      categorical_smd_pair(variable, pair[1], pair[2])
    } else {
      continuous_smd_pair(variable, pair[1], pair[2])
    }
  }, numeric(1))
  max(values, na.rm = TRUE)
}

row_store <- list()
row_number <- 0
add_row <- function(section, characteristic, values, p = NA, smd = NA,
                    row_type = "data") {
  row_number <<- row_number + 1
  row_store[[row_number]] <<- data.frame(
    order = row_number,
    section = section,
    row_type = row_type,
    characteristic = characteristic,
    overall = values[1],
    normal = values[2],
    subclinical = values[3],
    albuminuria = values[4],
    p = p,
    max_pairwise_smd = smd,
    stringsAsFactors = FALSE
  )
}

add_continuous <- function(section, label, variable) {
  values <- vapply(group_designs, function(item) {
    weighted_mean_sd(variable, item)
  }, character(1))
  add_row(section, label, values, continuous_p(variable),
          maximum_smd(variable))
}

add_median <- function(section, label, variable, digits = 1) {
  values <- vapply(group_designs, function(item) {
    weighted_median_iqr(variable, item, digits)
  }, character(1))
  add_row(section, label, values, continuous_p(variable),
          maximum_smd(variable))
}

add_categorical <- function(section, label, variable, display_levels = NULL) {
  p_value <- categorical_p(variable)
  smd <- maximum_smd(variable, categorical = TRUE)
  add_row(section, label, rep("", 4), p_value, smd, "category")
  observed_levels <- levels(factor(cohort[[variable]]))
  if (is.null(display_levels)) display_levels <- observed_levels
  for (index in seq_along(observed_levels)) {
    level <- observed_levels[index]
    values <- vapply(group_designs, function(item) {
      estimate <- svymean(
        as.formula(paste0("~I(as.numeric(", variable, " == ",
                          deparse(level), "))")),
        item, na.rm = TRUE
      )
      sprintf("%.1f", 100 * as.numeric(coef(estimate))[1])
    }, character(1))
    add_row(section, paste0("   ", display_levels[index]), values,
            row_type = "level")
  }
}

add_binary <- function(section, label, variable, positive_level) {
  values <- vapply(group_designs, function(item) {
    estimate <- svymean(
      as.formula(paste0("~I(as.numeric(", variable, " == ",
                        deparse(positive_level), "))")),
      item, na.rm = TRUE
    )
    sprintf("%.1f", 100 * as.numeric(coef(estimate))[1])
  }, character(1))
  add_row(section, label, values, categorical_p(variable),
          maximum_smd(variable, categorical = TRUE))
}

population <- c(
  sum(weights(design, "sampling")),
  vapply(uacr_levels, function(level) {
    sum(weights(subset(design, uacr_cat == level), "sampling"))
  }, numeric(1))
)
sample_size <- c(nrow(cohort), as.numeric(table(cohort$uacr_cat)))
add_row("Sample size", "Unweighted analytic N",
        format(sample_size, big.mark = ",", scientific = FALSE), row_type = "count")
add_row("Sample size", "Weighted population estimate",
        format(round(population), big.mark = ",", scientific = FALSE),
        row_type = "count")

add_continuous("Demographics", "Age, y", "age")
add_categorical("Demographics", "Age category, %", "age_group")
add_binary("Demographics", "Female sex, %", "sex_female", 1)
add_categorical("Demographics", "Race or ethnicity, %", "race_eth")
add_categorical("Demographics", "Educational attainment, %", "education_group")
add_continuous("Demographics", "Family income-to-poverty ratio", "pir")

add_continuous("Anthropometry and blood pressure", "Body-mass index, kg/m2", "bmi")
add_categorical("Anthropometry and blood pressure", "BMI category, %", "bmi_group")
add_continuous("Anthropometry and blood pressure", "Systolic blood pressure, mm Hg", "sbp")
add_continuous("Anthropometry and blood pressure", "Diastolic blood pressure, mm Hg", "dbp")
add_categorical("Anthropometry and blood pressure", "Blood-pressure phenotype, %", "bp_stage")

add_categorical("Lifestyle", "Smoking status, %", "smoking")

add_median("Glycemia and kidney function", "HbA1c, %", "hba1c")
add_categorical("Glycemia and kidney function", "Glycemic status, %", "glycemic_status")
add_continuous("Glycemia and kidney function", "Serum creatinine, mg/dL", "scr")
add_continuous("Glycemia and kidney function", "eGFR, mL/min/1.73 m2", "egfr")
add_median("Glycemia and kidney function", "UACR, mg/g", "uacr")

add_continuous("Lipids", "Total cholesterol, mg/dL", "tc")
add_continuous("Lipids", "HDL cholesterol, mg/dL", "hdl")
add_continuous("Lipids", "Non-HDL cholesterol, mg/dL", "non_hdl")
add_binary("Lipids", "Dyslipidemia, %", "dyslipidemia_main", 1)

add_binary("Cardiometabolic medications", "Antihypertensive medication use, %",
           "bp_treatment_label", "Yes")
add_binary("Cardiometabolic medications", "Lipid-lowering therapy, %",
           "lipid_treatment_label", "Yes")
add_binary("Cardiometabolic medications", "Prescription-recorded aspirin or antiplatelet use, %",
           "antiplatelet_label", "Yes")

table1 <- do.call(rbind, row_store)
write.csv(table1, file.path(paths$tables, "Table1_baseline.csv"),
          row.names = FALSE, na = "")
cat("Table 1 data completed.\n")
