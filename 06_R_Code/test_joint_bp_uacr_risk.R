# Joint BP x UACR standardized 10-year risks and within-category contrasts.
# Outputs: joint_18_cells.csv, within_bp_contrasts.csv, bp_uacr_interaction_test.csv, absolute_risk_model.rds
#
# HRs and the interaction test use the survey-weighted Cox model. Standardized
# 10-year risks and within-BP absolute risk differences use marginal
# standardization; their 95% CIs use a stratified delete-1 PSU jackknife (JKn)
# that refits the model, re-estimates the baseline hazard, and re-standardizes
# in every replicate, so the intervals include baseline-hazard uncertainty.

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
required <- c("ftime_years", "death_allcause", "uacr_cat", "bp_stage", "age",
              "sex_female", "race_eth", "cycle", "bmi", "smoking", "dm_main",
              "egfr", "wt_pooled", "strata", "psu")
analysis_data <- cohort[complete.cases(cohort[, required]), ]
analysis_data$joint <- interaction(analysis_data$bp_stage, analysis_data$uacr_cat,
                                   sep = " | ", drop = TRUE)
joint_levels <- levels(analysis_data$joint)
bp_levels <- levels(analysis_data$bp_stage)
uacr_levels <- levels(analysis_data$uacr_cat)
design <- make_design(analysis_data)

covariate_rhs <- "age + sex_female + race_eth + cycle + bmi + smoking + dm_main + egfr"
joint_formula <- as.formula(paste0("Surv(ftime_years, death_allcause) ~ joint + ", covariate_rhs))
joint_fit <- svycoxph(joint_formula, design = design, x = TRUE, model = TRUE)

# Design-based BP-treatment-by-UACR interaction tests.  The omnibus test uses
# all three UACR categories (10 df).  The two contrast-specific tests restrict
# the analysis to the normal category and one elevated category (5 df each),
# matching the exposure contrast displayed in the corresponding forest plot.
interaction_fit <- svycoxph(
  as.formula(paste0("Surv(ftime_years, death_allcause) ~ bp_stage * uacr_cat + ", covariate_rhs)),
  design = design)
interaction_test <- regTermTest(interaction_fit, ~bp_stage:uacr_cat)
interaction_result <- data.frame(
  test = "Overall BP-treatment group by three-level UACR category",
  numerator_df = unname(interaction_test$df), denominator_df = unname(interaction_test$ddf),
  F = unname(interaction_test$Ftest), p = unname(interaction_test$p), stringsAsFactors = FALSE)

contrast_interaction <- function(exposure_level, label) {
  contrast_design <- subset(design, uacr_cat %in% c(uacr_levels[1], exposure_level))
  contrast_design <- update(
    contrast_design,
    uacr_contrast = droplevels(factor(uacr_cat, levels = c(uacr_levels[1], exposure_level))))
  contrast_fit <- svycoxph(
    as.formula(paste0(
      "Surv(ftime_years, death_allcause) ~ bp_stage * uacr_contrast + ", covariate_rhs)),
    design = contrast_design)
  contrast_test <- regTermTest(contrast_fit, ~bp_stage:uacr_contrast)
  data.frame(
    test = label,
    numerator_df = unname(contrast_test$df),
    denominator_df = unname(contrast_test$ddf),
    F = unname(contrast_test$Ftest),
    p = unname(contrast_test$p),
    stringsAsFactors = FALSE)
}

interaction_result <- rbind(
  interaction_result,
  contrast_interaction(
    uacr_levels[2],
    "Low-grade UACR elevation vs normal across BP-treatment groups"),
  contrast_interaction(
    uacr_levels[3],
    "Albuminuria vs normal across BP-treatment groups"))

# HR of each joint cell vs the global reference (non-elevated BP, normal UACR).
beta <- coef(joint_fit); V <- vcov(joint_fit)
joint_coef <- grep("^joint", names(beta))
linear_contrast <- function(index_a, index_b = 1L) {
  contrast <- rep(0, length(beta))
  if (index_a > 1) contrast[joint_coef[index_a - 1]] <- 1
  if (index_b > 1) contrast[joint_coef[index_b - 1]] <- -1
  est <- sum(contrast * beta); se <- sqrt(drop(t(contrast) %*% V %*% contrast))
  c(HR = exp(est), lower = exp(est - qnorm(0.975) * se),
    upper = exp(est + qnorm(0.975) * se), p = 2 * pnorm(-abs(est / se)))
}

# ---- Marginal standardization metric (point + jackknife) --------------------
ard_names <- unlist(lapply(bp_levels, function(bp)
  paste0("ARD::", bp, " | ", uacr_levels[-1])))
standardize <- function(weights, data) {
  keep <- is.finite(weights) & weights > 0
  d <- data[keep, , drop = FALSE]; w <- weights[keep]; w <- w / mean(w)
  d$joint <- factor(interaction(d$bp_stage, d$uacr_cat, sep = " | "), levels = joint_levels)
  d$.w <- w
  fit <- coxph(as.formula(paste0("Surv(ftime_years, death_allcause) ~ joint + ", covariate_rhs)),
               data = d, weights = .w, x = TRUE)
  b <- coef(fit); X <- fit$x
  jc <- grep("^joint", colnames(X)); cc <- setdiff(seq_len(ncol(X)), jc)
  cov_lp <- drop(X[, cc, drop = FALSE] %*% b[cc])
  bh <- basehaz(fit, centered = FALSE); H0 <- bh$hazard[max(which(bh$time <= 10))]
  eff <- setNames(rep(0, length(joint_levels)), joint_levels)
  bj <- b[jc]; names(bj) <- sub("^joint", "", names(bj)); bj[is.na(bj)] <- 0
  eff[names(bj)] <- bj
  risk <- vapply(eff, function(e) sum(w * (1 - exp(-H0 * exp(cov_lp + e)))) / sum(w), numeric(1))
  names(risk) <- paste0("RISK::", joint_levels)
  ard <- unlist(lapply(bp_levels, function(bp) {
    ref <- risk[[paste0("RISK::", bp, " | ", uacr_levels[1])]]
    vapply(uacr_levels[-1], function(u) risk[[paste0("RISK::", bp, " | ", u)]] - ref, numeric(1))
  }))
  names(ard) <- ard_names
  c(risk * 100, ard * 100)
}

rep_design <- as.svrepdesign(design, type = "JKn", mse = TRUE)
jk <- withReplicates(rep_design, standardize)
point <- as.numeric(jk); names(point) <- names(jk)
SE <- sqrt(diag(attr(jk, "var")))
tcrit <- qt(0.975, degf(rep_design))
risk_key <- paste0("RISK::", joint_levels)

# ---- assemble 18-cell table -------------------------------------------------
event_time <- analysis_data$ftime_years; event_status <- analysis_data$death_allcause
cell_rows <- lapply(seq_along(joint_levels), function(i) {
  lvl <- joint_levels[i]; rows <- analysis_data$joint == lvl
  parts <- strsplit(lvl, " \\| ")[[1]]
  wr <- 1000 * sum(design$prob[rows]^-1 * event_status[rows]) /
        sum(design$prob[rows]^-1 * event_time[rows])
  hr <- linear_contrast(i)
  data.frame(bp_category = parts[1], uacr_category = parts[2],
             N = sum(rows), deaths = sum(event_status[rows]),
             incidence_rate_per_1000_py = wr,
             adjusted_10y_risk_percent = point[[paste0("RISK::", lvl)]],
             risk_lower = point[[paste0("RISK::", lvl)]] - tcrit * SE[[paste0("RISK::", lvl)]],
             risk_upper = point[[paste0("RISK::", lvl)]] + tcrit * SE[[paste0("RISK::", lvl)]],
             HR_vs_global_reference = hr["HR"], HR_lower = hr["lower"],
             HR_upper = hr["upper"], p_vs_global_reference = hr["p"],
             stringsAsFactors = FALSE)
})
joint_results <- do.call(rbind, cell_rows)

# ---- within-BP contrasts ----------------------------------------------------
within_rows <- lapply(bp_levels, function(bp) {
  idx <- which(startsWith(joint_levels, paste0(bp, " | ")))
  normal <- idx[endsWith(joint_levels[idx], uacr_levels[1])]
  lapply(idx[joint_levels[idx] != joint_levels[normal]], function(i) {
    hr <- linear_contrast(i, normal)
    u <- strsplit(joint_levels[i], " \\| ")[[1]][2]
    key <- paste0("ARD::", bp, " | ", u)
    data.frame(bp_category = bp, comparison = paste0(u, " vs ", uacr_levels[1]),
               HR = hr["HR"], HR_lower = hr["lower"], HR_upper = hr["upper"], p = hr["p"],
               ARD_percentage_points = point[[key]],
               ARD_lower = point[[key]] - tcrit * SE[[key]],
               ARD_upper = point[[key]] + tcrit * SE[[key]], stringsAsFactors = FALSE)
  })
})
within_results <- do.call(rbind, unlist(within_rows, recursive = FALSE))

write.csv(joint_results, file.path(paths$tables, "joint_18_cells.csv"), row.names = FALSE)
write.csv(within_results, file.path(paths$tables, "within_bp_contrasts.csv"), row.names = FALSE)
write.csv(interaction_result, file.path(paths$tables, "bp_uacr_interaction_test.csv"), row.names = FALSE)
saveRDS(list(fit = joint_fit, risks = joint_results, within_bp = within_results,
             interaction_test = interaction_result,
             method = "Survey-weighted Cox; marginal standardization; JKn design-based 95% CI"),
        file.path(paths$output, "absolute_risk_model.rds"))
cat("joint risk (JKn) written. df =", degf(rep_design),
    "| risk range", round(min(joint_results$adjusted_10y_risk_percent), 1),
    "-", round(max(joint_results$adjusted_10y_risk_percent), 1), "%\n")
