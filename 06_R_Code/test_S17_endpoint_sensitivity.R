# Supplementary Table S17: alternative cardiovascular-endpoint definition.
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
# Supplementary Table S17: alternative cardiovascular endpoint definition.
# Cerebrovascular cause code 005 is unavailable in the 2015-2018 public-use
# linked mortality cycles. Restrict both endpoint definitions to 1999-2014 so
# that their ascertainment windows are directly comparable.
mortality <- readRDS(file.path(paths$data, "mortality.rds"))
mortality_index <- match(cohort$SEQN, mortality$SEQN)
cohort$death_hd_stroke <- as.integer(
  cohort$death_allcause == 1 &
    mortality$UCOD_LEADING[mortality_index] %in% c("001", "005")
)
endpoint_cohort <- droplevels(cohort[cohort$cycle <= 8, ])
endpoint_rows <- lapply(
  list(
    "Heart disease only (UCOD 001)" = "death_hd",
    "Heart disease or cerebrovascular disease (UCOD 001 or 005)" = "death_hd_stroke"
  ),
  function(outcome) {
    fit <- quietly(svycoxph(
      model_formula("M3", outcome = outcome),
      design = make_design(endpoint_cohort), model = TRUE
    ))
    estimates <- extract_uacr(fit)
    data.frame(
      events = sum(model.response(model.frame(fit))[, "status"]),
      subclinical_HR = estimates$HR[1],
      subclinical_lower = estimates$lower[1],
      subclinical_upper = estimates$upper[1],
      albuminuria_HR = estimates$HR[2],
      albuminuria_lower = estimates$lower[2],
      albuminuria_upper = estimates$upper[2]
    )
  }
)
endpoint_results <- do.call(rbind, endpoint_rows)
endpoint_results$endpoint <- names(endpoint_rows)
endpoint_results$cycles <- "NHANES 1999-2014"
endpoint_results <- endpoint_results[, c("endpoint", setdiff(names(endpoint_results), "endpoint"))]
write.csv(endpoint_results,
          file.path(paths$tables, "S17_endpoint_sensitivity.csv"), row.names = FALSE)
