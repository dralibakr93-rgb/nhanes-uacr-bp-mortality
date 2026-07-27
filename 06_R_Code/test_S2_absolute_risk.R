# Supplementary Table S1 (panel B): marginally standardized 5- and 10-year risks
# from Model 3, with full design-based uncertainty.
#
# Point estimates use pooled MEC sampling weights and a tie-corrected weighted
# Breslow baseline hazard. Confidence intervals use a stratified delete-1 PSU
# jackknife (JKn); every replicate refits the Cox model, re-estimates the
# baseline hazard, and re-standardizes, so the intervals include baseline-hazard
# uncertainty (not only coefficient uncertainty).

source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
required <- unique(c("ftime_years", "death_allcause", "uacr_cat", model_terms$M3,
                     "wt_pooled", "strata", "psu"))
analysis <- cohort[complete.cases(cohort[, required]), ]
uacr_levels <- levels(analysis$uacr_cat)

# Marginal standardization from a weighted Cox fit: assign every participant to
# each UACR category in turn (keeping their own covariates) and average the
# predicted horizon-year risk.
standardize <- function(weights, data) {
  keep <- is.finite(weights) & weights > 0
  d <- droplevels(data[keep, , drop = FALSE])
  w <- weights[keep]; w <- w / mean(w)
  d$.w <- w
  fit <- coxph(model_formula("M3"), data = d, weights = .w, x = TRUE, model = TRUE)
  X <- fit$x; beta <- coef(fit)
  exposure_cols <- grep("^uacr_cat", colnames(X))
  covariate_lp <- drop(X[, -exposure_cols, drop = FALSE] %*% beta[-exposure_cols])
  effects <- c(0, beta[exposure_cols])            # normal is the reference
  bh <- basehaz(fit, centered = FALSE)
  out <- numeric(0)
  for (horizon in c(5, 10)) {
    H0 <- bh$hazard[max(which(bh$time <= horizon))]
    risk <- vapply(effects, function(e)
      sum(w * (1 - exp(-H0 * exp(covariate_lp + e)))) / sum(w), numeric(1))
    names(risk) <- paste0("risk_y", horizon, "_", seq_along(risk))
    ard <- (risk[-1] - risk[1]); names(ard) <- paste0("ard_y", horizon, "_", 2:3)
    out <- c(out, risk * 100, ard * 100)
  }
  out
}

design <- make_design(analysis)
rep_design <- as.svrepdesign(design, type = "JKn", mse = TRUE)
result <- withReplicates(rep_design, standardize)
point <- result; SE <- sqrt(diag(attr(result, "var")))
tcrit <- qt(0.975, degf(rep_design))

rows <- list()
for (horizon in c(5, 10)) {
  for (i in seq_along(uacr_levels)) {
    rk <- paste0("risk_y", horizon, "_", i)
    ad <- paste0("ard_y", horizon, "_", i)
    ard_point <- if (i == 1) 0 else point[[ad]]
    ard_lo <- if (i == 1) NA_real_ else point[[ad]] - tcrit * SE[[ad]]
    ard_hi <- if (i == 1) NA_real_ else point[[ad]] + tcrit * SE[[ad]]
    rows[[length(rows) + 1]] <- data.frame(
      horizon_years = horizon, uacr_category = uacr_levels[i],
      adjusted_risk_percent = point[[rk]],
      risk_lower = point[[rk]] - tcrit * SE[[rk]],
      risk_upper = point[[rk]] + tcrit * SE[[rk]],
      ARD_percentage_points = ard_point, ARD_lower = ard_lo, ARD_upper = ard_hi,
      stringsAsFactors = FALSE)
  }
}
risk_summary <- do.call(rbind, rows)
write.csv(risk_summary, file.path(paths$tables, "S2_absolute_risk.csv"), row.names = FALSE)
cat("S2 absolute risk (JKn) written. design df =", degf(rep_design), "\n")
print(risk_summary[risk_summary$horizon_years == 10, ], row.names = FALSE)
