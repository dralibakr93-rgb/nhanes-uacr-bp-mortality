# Shared configuration for the submitted analysis.

required_packages <- c(
  "survey", "survival", "mice", "mitools", "MASS", "Hmisc",
  "ggplot2", "ragg", "scales"
)
missing_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing_packages)) {
  stop("Missing R packages: ", paste(missing_packages, collapse = ", "))
}

suppressPackageStartupMessages({
  library(survey)
  library(survival)
})

options(survey.lonely.psu = "adjust", stringsAsFactors = FALSE)
set.seed(20260629)

paths <- list(
  data = "07_R_Data",
  output = "08_Analysis_Outputs",
  tables = "08_Analysis_Outputs/tables",
  figures = "08_Analysis_Outputs/figures"
)
invisible(lapply(paths[c("output", "tables", "figures")], dir.create,
                 recursive = TRUE, showWarnings = FALSE))

fmt_ci <- function(hr, lo, hi) sprintf("%.2f (%.2f-%.2f)", hr, lo, hi)
fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
}

quietly <- function(expr) {
  value <- NULL
  suppressMessages(capture.output(value <- eval.parent(substitute(expr))))
  value
}

prepare_cohort <- function(data) {
  data$uacr_cat <- droplevels(data$uacr_cat)
  data$bp_stage <- droplevels(data$bp_stage)
  data$smoking <- droplevels(data$smoking)
  data$race_eth <- droplevels(data$race_eth)
  data$educ <- factor(data$educ)
  data
}

make_design <- function(data, weight = "wt_pooled") {
  quietly(survey::svydesign(
    ids = ~psu,
    strata = ~strata,
    weights = as.formula(paste0("~", weight)),
    nest = TRUE,
    data = data
  ))
}

model_terms <- list(
  M1 = character(),
  M2 = c("age", "sex_female", "race_eth", "cycle"),
  M3 = c("age", "sex_female", "race_eth", "cycle", "bmi", "smoking",
         "dm_main", "bp_stage", "egfr"),
  M4 = c("age", "sex_female", "race_eth", "cycle", "bmi", "smoking",
         "dm_main", "bp_stage", "egfr", "pir", "educ"),
  M5 = c("age", "sex_female", "race_eth", "cycle", "bmi", "smoking",
         "dm_main", "bp_stage", "egfr", "pir", "educ",
         "dyslipidemia_main")
)

model_formula <- function(model, outcome = "death_allcause", exposure = "uacr_cat",
                          time = "ftime_years") {
  rhs <- c(exposure, model_terms[[model]])
  as.formula(sprintf("Surv(%s, %s) ~ %s", time, outcome,
                     paste(rhs, collapse = " + ")))
}

extract_uacr <- function(fit) {
  s <- quietly(summary(fit))
  rows <- grep("^uacr_cat", rownames(s$conf.int))
  data.frame(
    level = c("Low-grade", "Albuminuria"),
    HR = s$conf.int[rows, 1],
    lower = s$conf.int[rows, 3],
    upper = s$conf.int[rows, 4],
    p = s$coefficients[rows, ncol(s$coefficients)],
    stringsAsFactors = FALSE
  )
}
