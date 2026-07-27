# Supplementary Tables S7 and S19: survey-design-based discrimination and NRI.
#
# Point estimates use pooled MEC sampling weights. Variance estimation uses
# stratified delete-1 PSU jackknife (JKn) replicate weights, preserving the
# NHANES strata and PSU structure. Each replicate refits the Cox models,
# re-estimates the 10-year baseline hazard, and re-estimates the censoring
# distribution used for IPCW NRI.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
prediction_variables <- unique(c(
  "ftime_years", "death_allcause", "uacr_cat", "uacr_log2",
  model_terms$M3, "wt_pooled", "strata", "psu"
))
prediction_data <- cohort[complete.cases(cohort[, prediction_variables]), ]

clinical_formula <- Surv(ftime_years, death_allcause) ~
  age + sex_female + race_eth + cycle + bmi + smoking + dm_main +
  bp_stage + egfr
categorical_formula <- update(clinical_formula, . ~ . + uacr_cat)
continuous_formula <- update(clinical_formula, . ~ . + uacr_log2)

predict_10y_risk <- function(fit, data) {
  baseline <- basehaz(fit, centered = FALSE)
  eligible <- which(baseline$time <= 10)
  if (!length(eligible)) {
    stop("No event time at or before 10 years in a replicate fit.")
  }
  baseline_10 <- baseline$hazard[max(eligible)]
  linear_predictor <- predict(
    fit, newdata = data, type = "lp", reference = "zero"
  )
  1 - exp(-baseline_10 * exp(linear_predictor))
}

weighted_harrell_c <- function(data, weights, linear_predictor) {
  unname(concordance(
    Surv(ftime_years, death_allcause) ~ linear_predictor,
    data = data,
    weights = weights,
    reverse = TRUE,
    timewt = "n"
  )$concordance)
}

ipcw_nri <- function(data, weights, old_risk, new_risk) {
  censor_times <- sort(unique(data$ftime_years[
    data$death_allcause == 0 & data$ftime_years <= 10
  ]))
  g_after <- numeric(length(censor_times))
  g_current <- 1
  if (length(censor_times)) {
    for (index in seq_along(censor_times)) {
      current_time <- censor_times[index]
      at_risk <- sum(weights[data$ftime_years >= current_time])
      censored <- sum(weights[
        data$ftime_years == current_time & data$death_allcause == 0
      ])
      g_current <- g_current * (1 - censored / at_risk)
      g_after[index] <- g_current
    }
  }
  g_before <- function(time) {
    index <- if (length(censor_times)) max(which(censor_times < time), 0) else 0
    if (index == 0) 1 else g_after[index]
  }
  g_10 <- if (length(censor_times) && any(censor_times <= 10)) {
    g_after[max(which(censor_times <= 10))]
  } else {
    1
  }

  status_10 <- ifelse(
    data$death_allcause == 1 & data$ftime_years <= 10, 1,
    ifelse(data$ftime_years > 10, 0, NA)
  )
  ipcw <- numeric(nrow(data))
  event_rows <- which(status_10 == 1)
  nonevent_rows <- which(status_10 == 0)
  ipcw[event_rows] <- 1 / vapply(
    data$ftime_years[event_rows], g_before, numeric(1)
  )
  ipcw[nonevent_rows] <- 1 / g_10
  final_weights <- weights * ipcw

  risk_bands <- c(0, 0.05, 0.10, 0.20, Inf)
  old_band <- as.numeric(cut(
    old_risk, risk_bands, right = FALSE, include.lowest = TRUE
  ))
  new_band <- as.numeric(cut(
    new_risk, risk_bands, right = FALSE, include.lowest = TRUE
  ))
  movement <- sign(new_band - old_band)
  is_event <- !is.na(status_10) & status_10 == 1
  is_nonevent <- !is.na(status_10) & status_10 == 0

  movement_component <- function(rows, favorable_direction) {
    denominator <- sum(final_weights[rows])
    favorable <- sum(final_weights[rows & movement == favorable_direction])
    unfavorable <- sum(final_weights[rows & movement == -favorable_direction])
    (favorable - unfavorable) / denominator
  }
  event_nri <- movement_component(is_event, 1)
  nonevent_nri <- movement_component(is_nonevent, -1)
  c(event = event_nri, nonevent = nonevent_nri,
    total = event_nri + nonevent_nri)
}

replicate_counter <- 0L
prediction_metrics <- function(weights, data) {
  replicate_counter <<- replicate_counter + 1L
  if (replicate_counter %% 25L == 0L) {
    message("Completed prediction replicate ", replicate_counter - 1L)
  }

  keep <- is.finite(weights) & weights > 0
  analysis_data <- droplevels(data[keep, , drop = FALSE])
  analysis_weights <- weights[keep]
  analysis_weights <- analysis_weights / mean(analysis_weights)
  analysis_data$.prediction_weight <- analysis_weights

  clinical_fit <- coxph(
    clinical_formula, data = analysis_data, weights = .prediction_weight,
    robust = FALSE, model = TRUE, x = TRUE, y = TRUE
  )
  categorical_fit <- coxph(
    categorical_formula, data = analysis_data, weights = .prediction_weight,
    robust = FALSE, model = TRUE, x = TRUE, y = TRUE
  )
  continuous_fit <- coxph(
    continuous_formula, data = analysis_data, weights = .prediction_weight,
    robust = FALSE, model = TRUE, x = TRUE, y = TRUE
  )

  clinical_lp <- clinical_fit$linear.predictors
  categorical_lp <- categorical_fit$linear.predictors
  continuous_lp <- continuous_fit$linear.predictors
  c_clinical <- weighted_harrell_c(
    analysis_data, analysis_weights, clinical_lp
  )
  c_categorical <- weighted_harrell_c(
    analysis_data, analysis_weights, categorical_lp
  )
  c_continuous <- weighted_harrell_c(
    analysis_data, analysis_weights, continuous_lp
  )

  clinical_risk <- predict_10y_risk(clinical_fit, analysis_data)
  categorical_risk <- predict_10y_risk(categorical_fit, analysis_data)
  nri <- ipcw_nri(
    analysis_data, analysis_weights, clinical_risk, categorical_risk
  )

  c(
    c_clinical = c_clinical,
    c_categorical = c_categorical,
    c_continuous = c_continuous,
    delta_c_categorical = c_categorical - c_clinical,
    delta_c_continuous = c_continuous - c_clinical,
    nri_event = nri[["event"]],
    nri_nonevent = nri[["nonevent"]],
    nri_total = nri[["total"]]
  )
}

prediction_design <- make_design(prediction_data)
prediction_rep_design <- as.svrepdesign(
  prediction_design, type = "JKn", mse = TRUE
)
prediction_result <- withReplicates(
  prediction_rep_design,
  prediction_metrics,
  return.replicates = TRUE
)

point_estimates <- prediction_result$theta
variance_matrix <- attr(point_estimates, "var")
standard_errors <- sqrt(diag(variance_matrix))
design_df <- degf(prediction_rep_design)
critical_value <- qt(0.975, df = design_df)
lower_limits <- point_estimates - critical_value * standard_errors
upper_limits <- point_estimates + critical_value * standard_errors

metric_summary <- data.frame(
  metric = names(point_estimates),
  estimate = as.numeric(point_estimates),
  SE = as.numeric(standard_errors),
  lower = as.numeric(lower_limits),
  upper = as.numeric(upper_limits),
  design_df = design_df,
  method = "Pooled MEC-weighted estimate; stratified delete-1 PSU jackknife (JKn)",
  stringsAsFactors = FALSE
)
write.csv(
  metric_summary,
  file.path(paths$tables, "S7_S19_survey_prediction_metrics.csv"),
  row.names = FALSE
)

discrimination <- data.frame(
  model = c("Clinical model", "+ UACR category", "+ log2(UACR)"),
  C_statistic = point_estimates[c(
    "c_clinical", "c_categorical", "c_continuous"
  )],
  SE = standard_errors[c(
    "c_clinical", "c_categorical", "c_continuous"
  )],
  lower = lower_limits[c(
    "c_clinical", "c_categorical", "c_continuous"
  )],
  upper = upper_limits[c(
    "c_clinical", "c_categorical", "c_continuous"
  )],
  change_in_C = c(
    NA, point_estimates["delta_c_categorical"],
    point_estimates["delta_c_continuous"]
  ),
  change_SE = c(
    NA, standard_errors["delta_c_categorical"],
    standard_errors["delta_c_continuous"]
  ),
  change_lower = c(
    NA, lower_limits["delta_c_categorical"],
    lower_limits["delta_c_continuous"]
  ),
  change_upper = c(
    NA, upper_limits["delta_c_categorical"],
    upper_limits["delta_c_continuous"]
  ),
  method = "Survey-weighted Harrell C; JKn design-based 95% CI",
  stringsAsFactors = FALSE
)
write.csv(
  discrimination,
  file.path(paths$tables, "S7_discrimination.csv"),
  row.names = FALSE
)

nri_results <- data.frame(
  component = c("Events", "Non-events", "Total"),
  NRI = point_estimates[c("nri_event", "nri_nonevent", "nri_total")],
  SE = standard_errors[c("nri_event", "nri_nonevent", "nri_total")],
  lower = lower_limits[c("nri_event", "nri_nonevent", "nri_total")],
  upper = upper_limits[c("nri_event", "nri_nonevent", "nri_total")],
  method = "Survey- and IPCW-weighted categorical NRI; JKn design-based 95% CI",
  stringsAsFactors = FALSE
)
write.csv(
  nri_results,
  file.path(paths$tables, "S19_ipcw_nri.csv"),
  row.names = FALSE
)

replicate_output <- data.frame(
  replicate = seq_len(nrow(prediction_result$replicates)),
  prediction_result$replicates,
  check.names = FALSE
)
write.csv(
  replicate_output,
  file.path(paths$tables, "S7_S19_survey_prediction_replicates.csv"),
  row.names = FALSE
)
