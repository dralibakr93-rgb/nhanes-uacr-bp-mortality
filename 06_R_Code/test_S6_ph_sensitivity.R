# Supplementary Table S6 (sensitivity): UACR HRs when the Cox baseline is
# stratified by the nonproportional covariates. UACR itself satisfies the PH
# assumption (p=0.18), so its HR should be stable across these specifications.
source("06_R_Code/00_config.R")
cohort <- prepare_cohort(readRDS(file.path(paths$data, "cohort_primary_v2.rds")))
cohort$age_cat  <- cut(cohort$age,  c(-Inf,50,65,Inf))
cohort$egfr_cat <- cut(cohort$egfr, c(60,75,90,Inf), right=FALSE)
req <- unique(c("ftime_years","death_allcause","uacr_cat",model_terms$M3,
                "age_cat","egfr_cat","wt_pooled","strata","psu"))
cc <- cohort[complete.cases(cohort[,req]),]
d  <- make_design(cc)
uhr <- function(f){ e <- extract_uacr(svycoxph(f, design=d)); c(
  subclinical=sprintf("%.2f (%.2f-%.2f)", e$HR[1],e$lower[1],e$upper[1]),
  albuminuria=sprintf("%.2f (%.2f-%.2f)", e$HR[2],e$lower[2],e$upper[2])) }
S <- function(...) paste(c(...), collapse=" + ")
base <- "Surv(ftime_years, death_allcause) ~ uacr_cat"
rows <- rbind(
  data.frame(model="Primary Model 3 (unstratified)",
             t(uhr(as.formula(paste0(base," + ", S(model_terms$M3)))))),
  data.frame(model="Baseline stratified by BP-treatment group",
             t(uhr(as.formula(paste0(base," + ", S(setdiff(model_terms$M3,"bp_stage")), " + strata(bp_stage)"))))),
  data.frame(model="Baseline stratified by nonproportional categorical covariates (sex, smoking, BP group)",
             t(uhr(as.formula(paste0(base," + ", S(setdiff(model_terms$M3,c("sex_female","smoking","bp_stage"))), " + strata(sex_female, smoking, bp_stage)"))))),
  data.frame(model="Baseline stratified by all nonproportional covariates (sex, smoking, BP group, and categorized age and eGFR), continuous age and eGFR retained for adjustment",
             t(uhr(as.formula(paste0(base," + ", S(setdiff(model_terms$M3,c("sex_female","smoking","bp_stage"))), " + strata(age_cat, sex_female, smoking, bp_stage, egfr_cat)")))))
)
write.csv(rows, file.path(paths$tables,"S6_ph_sensitivity.csv"), row.names=FALSE)
print(rows, row.names=FALSE)
