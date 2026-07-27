# =============================================================================
# 08_build_cohort_for_figures.R -- map the v2 reanalysis cohort onto the PREVIOUS
# submission's column schema so the original JACC-style design scripts (44_rcs,
# 46_forest, KM/prevalence/CRP supplements, table builders) run unchanged on the
# new numbers. Non-destructive: writes a NEW file, never overwrites old cohorts.
# Run from project root.
# =============================================================================
suppressWarnings(suppressMessages({library(dplyr)}))
v <- readRDS("07_R_Data/cohort_primary_v2.rds")
m <- readRDS("07_R_Data/mortality.rds")
ucod <- m$UCOD_LEADING[match(v$SEQN, m$SEQN)]
crp <- tryCatch(readRDS("07_R_Data/crp_harmonized.rds"), error=function(e) NULL)
crp_mgL <- if(!is.null(crp)) crp$crp_mgL[match(v$SEQN, crp$SEQN)] else NA_real_
crp_assay <- if(!is.null(crp)) crp$crp_assay[match(v$SEQN, crp$SEQN)] else NA_character_

bmi_cat <- cut(v$bmi, c(-Inf,18.5,25,30,Inf),
  labels=c("Underweight (<18.5)","Normal (18.5-<25)","Overweight (25-<30)","Obesity (>=30)"), right=FALSE)
age_cat <- cut(v$age, c(-Inf,45,60,75,Inf), labels=c("30-44","45-59","60-74","75+"), right=FALSE)

o <- tibble(
  SEQN=v$SEQN,
  age_years=v$age, age_cat=age_cat,
  sex=factor(ifelse(v$sex_female,"Female","Male"), levels=c("Male","Female")),
  race_eth=v$race_eth, race_eth_v3=v$race_eth,
  cycle_num=as.integer(v$cycle), cycle_suffix=NA_character_, cycle_years=NA_character_,
  bmi_kgm2=v$bmi, bmi_cat=bmi_cat,
  sbp_mmhg=v$sbp, dbp_mmhg=v$dbp,
  smoking_3cat=v$smoking,
  dm_composite=v$dm_main, dm_med=v$dm_med, dm_self=v$dm_self,
  dyslipidemia=v$dyslipidemia_main,
  bp_stage=v$bp_stage,
  egfr_2021=v$egfr,
  scr_mgdl=v$scr, hba1c_pct=v$hba1c, fpg_mgdl=v$fpg,
  hdl_mgdl=v$hdl, ldl_mgdl=v$ldl, tc_mgdl=v$tc, tg_mgdl=v$tg, non_hdl=v$tc-v$hdl,
  uacr_mgg=v$uacr, uacr_log2=v$uacr_log2,
  uacr_cat=droplevels(v$uacr_cat),
  uacr_cat_short=factor(as.integer(v$uacr_cat), levels=1:3,
    labels=c("Normal UACR","Low-grade albuminuria","Albuminuria")),
  uacr_legend=factor(as.integer(v$uacr_cat), levels=1:3,
    labels=c("Normal","Low-grade","Albuminuria")),
  uacr_idx=as.integer(v$uacr_cat),
  pir=v$pir, education=factor(v$educ),
  on_anti_htn=v$on_bp_treat, on_statin=v$on_lipid_lowering, lipid_lowering_tx=v$on_lipid_lowering,
  on_aspirin=v$on_antiplatelet_rx,
  prev_cvd=v$prev_cvd,
  ftime_years=v$ftime_years, ftime_months=v$ftime_years*12,
  death_allcause=v$death_allcause, died=v$death_allcause, death_hd=v$death_hd,
  death_cvd=v$death_hd,
  ucod=ucod,
  crp_mgL=crp_mgL, crp_log=log(crp_mgL), crp_high=as.integer(crp_mgL>=3),
  crp_assay=crp_assay,
  hgb_gdl=NA_real_, wbc_k=NA_real_, plt_k=NA_real_, waist_cm=NA_real_,
  compete_event=ifelse(v$death_hd==1,1L, ifelse(v$death_allcause==1,2L,0L)),
  elig=v$elig, preserved_kidney=1L,
  wt_pooled=v$wt_pooled, MEC_wt_pooled_10=v$wt_pooled,
  WT_fasting_pooled_10=v$wt_pooled,
  psu=v$psu, strata=v$strata
)
o <- as.data.frame(o)
saveRDS(o, "07_R_Data/cohort_for_figures.rds")
cat("Saved cohort_for_figures.rds  N=",nrow(o)," cols=",ncol(o),"\n")
cat("bp_stage:",paste(levels(o$bp_stage),collapse=" | "),"\n")
cat("all-cause=",sum(o$death_allcause)," HD=",sum(o$death_hd)," other-death=",sum(o$compete_event==2),"\n")
