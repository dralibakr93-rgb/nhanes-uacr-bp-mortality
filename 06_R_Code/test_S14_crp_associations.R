# Supplementary Table S14: UACR-log(CRP) associations and adjusted geometric means.
# Auto-organized from the verified pipeline; logic unchanged.

source("06_R_Code/00_config.R")

cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
crp <- readRDS(file.path(paths$data, "crp_harmonized.rds"))
crp_index <- match(cohort$SEQN, crp$SEQN)
cohort$crp_mg_l <- crp$crp_mgL[crp_index]
cohort$crp_assay <- crp$crp_assay[crp_index]
cohort$log_crp <- log(cohort$crp_mg_l)

# CBC fields are rebuilt directly from the supplied legacy and contemporary
# laboratory domains.
raw_domains <- readRDS(file.path(paths$data, "raw_domains.rds"))
cbc_columns <- c("SEQN", "LBXHGB", "LBXWBCSI", "LBXPLTSI", "LBXRDW")
cbc <- rbind(
  raw_domains$cbc_legacy[, cbc_columns],
  raw_domains$cbc[, cbc_columns]
)
cbc <- cbc[!duplicated(cbc$SEQN), ]
cbc_index <- match(cohort$SEQN, cbc$SEQN)
cohort$hemoglobin <- cbc$LBXHGB[cbc_index]
cohort$wbc <- cbc$LBXWBCSI[cbc_index]
cohort$platelets <- cbc$LBXPLTSI[cbc_index]
cohort$rdw <- cbc$LBXRDW[cbc_index]
full_design <- make_design(cohort)
# Supplementary Table S14: association of UACR with log CRP, by assay.
crp_association_rows <- list()
for (assay in c("standard CRP", "hs-CRP")) {
  assay_rows <- cohort$crp_assay == assay & !is.na(cohort$log_crp)
  assay_design <- full_design[which(assay_rows), ]
  fit <- quietly(svyglm(
    as.formula(paste0(
      "log_crp ~ uacr_cat + ", paste(model_terms$M3, collapse = " + ")
    )),
    design = assay_design
  ))
  interaction_fit <- quietly(svyglm(
    as.formula(paste0(
      "log_crp ~ uacr_cat * bp_stage + ",
      paste(setdiff(model_terms$M3, "bp_stage"), collapse = " + ")
    )),
    design = assay_design
  ))
  interaction_p <- quietly(regTermTest(
    interaction_fit, ~uacr_cat:bp_stage
  ))$p
  coefficients <- summary(fit)$coefficients
  indices <- grep("^uacr_cat", rownames(coefficients))
  crp_association_rows[[assay]] <- do.call(rbind, lapply(indices, function(index) {
    beta <- coefficients[index, 1]
    standard_error <- coefficients[index, 2]
    data.frame(
      assay = assay,
      contrast = ifelse(grepl("Low-grade", rownames(coefficients)[index]),
                        "Low-grade vs normal", "Albuminuria vs normal"),
      geometric_mean_ratio = exp(beta),
      # CI on the same reference distribution (t with the model's residual design
      # df) as the survey p-value, so the interval and p-value are internally
      # consistent even in the small high-sensitivity-CRP subsample.
      lower = exp(beta - qt(0.975, df.residual(fit)) * standard_error),
      upper = exp(beta + qt(0.975, df.residual(fit)) * standard_error),
      p = coefficients[index, ncol(coefficients)],
      uacr_by_bp_interaction_p = interaction_p,
      stringsAsFactors = FALSE
    )
  }))
}
write.csv(do.call(rbind, crp_association_rows),
          file.path(paths$tables, "S14_log_crp_models.csv"), row.names = FALSE)

# Adjusted geometric means for the exploratory CRP figure. The standard-CRP
# assay block is used, matching the original prespecified figure.
crp_figure_terms <- setdiff(model_terms$M3, "bp_stage")
crp_figure_formula <- as.formula(paste0(
  "log_crp ~ uacr_cat * bp_stage + ",
  paste(crp_figure_terms, collapse = " + ")
))
standard_rows <- cohort$crp_assay == "standard CRP" & !is.na(cohort$log_crp)
standard_indices <- which(standard_rows)
standard_complete <- complete.cases(cohort[standard_indices, all.vars(crp_figure_formula)])
standard_indices <- standard_indices[standard_complete]
standard_data <- cohort[standard_indices, ]
standard_design <- full_design[standard_indices, ]
crp_figure_fit <- quietly(svyglm(crp_figure_formula, design = standard_design))
standard_weights <- weights(standard_design, "sampling")
standard_weights <- standard_weights / sum(standard_weights)
crp_prediction_rows <- list()
prediction_index <- 0
for (bp in levels(cohort$bp_stage)) {
  for (uacr in levels(cohort$uacr_cat)) {
    prediction_data <- standard_data
    prediction_data$bp_stage <- factor(
      bp, levels = levels(cohort$bp_stage)
    )
    prediction_data$uacr_cat <- factor(
      uacr, levels = levels(cohort$uacr_cat)
    )
    prediction_matrix <- model.matrix(
      delete.response(terms(crp_figure_fit)), prediction_data
    )
    prediction_matrix <- prediction_matrix[, names(coef(crp_figure_fit)), drop = FALSE]
    average_design_row <- colSums(prediction_matrix * standard_weights)
    estimate_log <- sum(average_design_row * coef(crp_figure_fit))
    standard_error <- sqrt(drop(
      t(average_design_row) %*% vcov(crp_figure_fit) %*% average_design_row
    ))
    prediction_index <- prediction_index + 1
    crp_prediction_rows[[prediction_index]] <- data.frame(
      bp_category = bp,
      uacr_category = uacr,
      adjusted_geometric_mean_mg_l = exp(estimate_log),
      lower = exp(estimate_log - qnorm(0.975) * standard_error),
      upper = exp(estimate_log + qnorm(0.975) * standard_error),
      stringsAsFactors = FALSE
    )
  }
}
write.csv(do.call(rbind, crp_prediction_rows),
          file.path(paths$tables, "S14_crp_adjusted_geomean_figure.csv"),
          row.names = FALSE)
