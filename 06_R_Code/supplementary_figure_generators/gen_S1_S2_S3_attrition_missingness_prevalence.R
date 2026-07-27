# =============================================================================
# 11_supp_figures_extra_v2.R -- remaining supplementary figures in JACC design:
# S1 covariate-attrition waterfall, S2 missingness by cycle, S3 UACR-prevalence
# stacked bar, S12 decision-curve net benefit, S13 risk-gradient ranking,
# S14 CRP by UACR. Run from project root. Each block is tryCatch-guarded.
# =============================================================================
suppressWarnings(suppressMessages({library(survey);library(survival);library(ggplot2);library(ragg)}))
options(survey.lonely.psu="adjust")
source("06_R_Code/supplementary_figure_generators/helpers/04_theme_jacc.R")
PAL <- c("Normal"="#1F4E79","Low-grade"="#E08E2A","Albuminuria"="#C0392B")
OUT <- "06_R_Code/supplementary_figure_generators/output"; dir.create(OUT,showWarnings=FALSE)
coh <- readRDS("07_R_Data/cohort_primary_v2.rds")
coh$uacr_cat<-droplevels(coh$uacr_cat); coh$bp_stage<-droplevels(coh$bp_stage)
coh$smoking<-droplevels(coh$smoking); coh$race_eth<-droplevels(coh$race_eth)
des <- svydesign(ids=~psu,strata=~strata,weights=~wt_pooled,nest=TRUE,data=coh)
exp3 <- function(p,stub,w,h){ ggsave(file.path(OUT,paste0(stub,".pdf")),p,width=w,height=h,device=grDevices::pdf)
  ggsave(file.path(OUT,paste0(stub,".png")),p,width=w,height=h,dpi=300,device=ragg::agg_png,bg="white")
  ggsave(file.path(OUT,paste0(stub,".tiff")),p,width=w,height=h,dpi=600,device=ragg::agg_tiff,compression="lzw",bg="white")
  cat("  wrote",stub,"\n") }
ulab <- c("Normal","Low-grade","Albuminuria")
BPo <- c("Non-elevated BP","Elevated BP","Stage 1 hypertension","Stage 2 hypertension","Treated - Controlled","Treated - Uncontrolled")
bpshort <- c("Non-elevated BP","Elevated BP","Untreated stage 1 HTN","Untreated stage 2 HTN","Treated-controlled HTN","Treated-uncontrolled HTN")

## ---- S3: UACR prevalence stacked bar by BP category -------------------------
tryCatch({
  pv <- svyby(~uacr_cat, ~bp_stage, des, svymean, na.rm=TRUE)
  m <- do.call(rbind, lapply(seq_len(nrow(pv)), function(i)
    data.frame(bp=pv$bp_stage[i], uacr=ulab, pct=100*as.numeric(pv[i,2:4]))))
  m$bp <- factor(bpshort[match(m$bp,BPo)], levels=rev(bpshort))
  m$uacr <- factor(m$uacr, levels=ulab)
  p <- ggplot(m, aes(pct, bp, fill=uacr)) +
    geom_col(width=0.72, color="white", linewidth=0.3) +
    geom_text(aes(label=ifelse(pct>=4,sprintf("%.1f%%",pct),"")), position=position_stack(vjust=0.5),
              size=3.1, color=ifelse(m$uacr=="Normal","white","grey10")) +
    scale_fill_manual(values=PAL, name="UACR category") +
    scale_x_continuous(expand=expansion(mult=c(0,0.02))) +
    labs(title="Survey-weighted UACR-category prevalence by BP-treatment category",
         subtitle="Distribution of normal UACR, low-grade UACR elevation, and albuminuria within each BP-treatment category.",
         x="Weighted prevalence (%)", y=NULL) + theme_jacc() + theme(legend.position="bottom")
  exp3(p,"Supplement_UACR_Prevalence_StackedBar",9.0,5.6)
}, error=function(e) cat("S3 FAILED:",conditionMessage(e),"\n"))

## ---- S14: CRP geometric mean by UACR category, by assay ---------------------
tryCatch({
  cc <- readRDS("07_R_Data/cohort_for_figures.rds")
  cc <- cc[!is.na(cc$crp_mgL) & cc$crp_mgL>0 & !is.na(cc$crp_assay),]
  cc$lcrp <- log(cc$crp_mgL)
  d2 <- svydesign(ids=~psu,strata=~strata,weights=~wt_pooled,nest=TRUE,data=cc)
  gm <- svyby(~lcrp, ~uacr_cat+crp_assay, d2, svymean, na.rm=TRUE)
  gm$geomean <- exp(gm$lcrp); gm$lo <- exp(gm$lcrp-1.96*gm$se); gm$hi <- exp(gm$lcrp+1.96*gm$se)
  gm$ua <- factor(ulab[as.integer(gm$uacr_cat)], levels=ulab)
  gm$assay <- factor(gm$crp_assay)
  p <- ggplot(gm, aes(ua, geomean, fill=ua)) +
    geom_col(width=0.7, color="grey25", linewidth=0.3) +
    geom_errorbar(aes(ymin=lo,ymax=hi), width=0.2) +
    geom_text(aes(label=sprintf("%.2f",geomean)), vjust=-0.6, size=3) +
    facet_wrap(~assay, nrow=1) +
    scale_fill_manual(values=PAL, guide="none") +
    labs(title="Survey-weighted geometric-mean C-reactive protein by UACR category",
         subtitle="Exploratory; standard and high-sensitivity assays shown separately (not pooled).",
         x=NULL, y="Geometric-mean CRP (mg/L)") + theme_jacc()
  exp3(p,"Supplement_CRP_by_UACR",9.0,5.2)
}, error=function(e) cat("S14 FAILED:",conditionMessage(e),"\n"))

## ---- S2: missingness by cycle (key covariates) -----------------------------
tryCatch({
  vars <- c(pir="Income-to-poverty",educ="Education",bmi="BMI",smoking="Smoking",
            hba1c="HbA1c",ldl="LDL (fasting)",tg="Triglycerides (fasting)")
  mm <- do.call(rbind, lapply(names(vars), function(v) data.frame(
    cycle=tapply(is.na(coh[[v]]),coh$cycle,mean), cyc=sort(unique(coh$cycle)), var=vars[v])))
  cyclab <- c("1999-00","2001-02","2003-04","2005-06","2007-08","2009-10","2011-12","2013-14","2015-16","2017-18")
  mm$cycf <- factor(cyclab[mm$cyc], levels=cyclab); mm$pct <- 100*mm$cycle
  p <- ggplot(mm, aes(cycf, var, fill=pct)) + geom_tile(color="white", linewidth=0.6) +
    geom_text(aes(label=sprintf("%.0f",pct)), size=3, color=ifelse(mm$pct>40,"white","grey15")) +
    scale_fill_gradient(low="#EAF2F8", high="#1F4E79", name="% missing") +
    labs(title="Covariate missingness by NHANES cycle (analytic cohort)",
         subtitle="Percent missing within each 2-year cycle; fasting-subsample biomarkers are missing by design in non-fasting participants.",
         x=NULL, y=NULL) + theme_jacc() +
    theme(axis.text.x=element_text(angle=45,hjust=1), panel.grid=element_blank())
  exp3(p,"Supplement_Missingness_byCycle",9.4,4.8)
}, error=function(e) cat("S2 FAILED:",conditionMessage(e),"\n"))

## ---- S1: covariate-attrition waterfall (complete-case) ----------------------
tryCatch({
  seq_vars <- list("Analytic cohort"=character(0),
    "+ demographics"=c("age","sex_female","race_eth","cycle"),
    "+ BMI, smoking"=c("bmi","smoking"),
    "+ diabetes, BP, eGFR (Model 3)"=c("dm_main","bp_stage","egfr"),
    "+ income, education (Model 4)"=c("pir","educ"),
    "+ dyslipidemia (Model 5)"=c("dyslipidemia_main"))
  cum <- character(0); rows <- data.frame()
  for(nm in names(seq_vars)){ cum <- c(cum, seq_vars[[nm]])
    n <- if(length(cum)==0) nrow(coh) else sum(complete.cases(coh[,cum,drop=FALSE]))
    rows <- rbind(rows, data.frame(step=nm, n=n)) }
  rows$step <- factor(rows$step, levels=rev(rows$step))
  p <- ggplot(rows, aes(n, step)) +
    geom_col(fill="#5499C7", width=0.66, color="grey25", linewidth=0.3) +
    geom_text(aes(label=format(n,big.mark=",")), hjust=-0.12, size=3.2) +
    scale_x_continuous(limits=c(0,32000), expand=expansion(mult=c(0,0.04))) +
    labs(title="Complete-case sample size across the adjustment hierarchy",
         subtitle="Unweighted N with non-missing covariates as each model adds terms (primary analysis is complete-case).",
         x="Participants with complete covariates (N)", y=NULL) + theme_jacc()
  exp3(p,"Supplement_CovariateAttrition_Waterfall",9.0,4.6)
}, error=function(e) cat("S1 FAILED:",conditionMessage(e),"\n"))

## ---- S12: decision-curve net benefit (exploratory) -------------------------
tryCatch({
  vv <- c("ftime_years","death_allcause","uacr_cat","age","sex_female","race_eth","cycle","bmi","smoking","dm_main","bp_stage","egfr")
  d <- coh[complete.cases(coh[,vv]),]
  clin <- coxph(Surv(ftime_years,death_allcause)~age+sex_female+race_eth+cycle+bmi+smoking+dm_main+bp_stage+egfr, data=d)
  full <- update(clin, .~.+uacr_cat)
  r10 <- function(fit){ s<-survfit(fit,newdata=d); ti<-which.min(abs(s$time-10)); 1-summary(s,times=s$time[ti])$surv }
  p0<-r10(clin); p1<-r10(full)
  st <- ifelse(d$death_allcause==1 & d$ftime_years<=10,1L, ifelse(d$ftime_years>=10,0L,NA))
  k <- !is.na(st); ev<-st[k]==1
  thr <- seq(0.02,0.25,by=0.005); prev <- mean(ev)
  nb <- function(p){ p<-p[k]; sapply(thr, function(t){ tp<-mean(p>t & st[k]==1); fp<-mean(p>t & st[k]==0); tp - fp*(t/(1-t)) }) }
  nb_all <- sapply(thr, function(t) prev - (1-prev)*(t/(1-t)))
  D <- rbind(data.frame(thr=thr,nb=nb(p0),model="Clinical model"),
             data.frame(thr=thr,nb=nb(p1),model="Clinical + UACR"),
             data.frame(thr=thr,nb=nb_all,model="Treat all"),
             data.frame(thr=thr,nb=0,model="Treat none"))
  p <- ggplot(D, aes(thr*100, nb, color=model, linetype=model)) + geom_line(linewidth=0.8) +
    scale_color_manual(values=c("Clinical model"="#1F4E79","Clinical + UACR"="#C0392B","Treat all"="grey55","Treat none"="grey20"), name=NULL) +
    scale_linetype_manual(values=c("Clinical model"=1,"Clinical + UACR"=1,"Treat all"=2,"Treat none"=3), name=NULL) +
    coord_cartesian(ylim=c(-0.01, max(D$nb)*1.05)) +
    labs(title="Decision-curve analysis: net benefit of adding UACR (exploratory)",
         subtitle="10-year all-cause mortality; complete-case, no IPCW. Interpret as exploratory context, not a validated tool.",
         x="Threshold probability (%)", y="Net benefit") + theme_jacc() + theme(legend.position="bottom")
  exp3(p,"Supplement_DCA_NetBenefit",8.8,5.4)
}, error=function(e) cat("S12 FAILED:",conditionMessage(e),"\n"))

## Copy the three submitted panels into 08_Analysis_Outputs/figures under their
## final names, the way the landmark generators do; everything downstream reads
## from there. The other panels drawn above are diagnostics and stay put.
FINAL <- "08_Analysis_Outputs/figures"
dir.create(FINAL, recursive=TRUE, showWarnings=FALSE)
submitted <- c(
  Supplement_CovariateAttrition_Waterfall = "Supplementary_Figure_S1_Participant_Attrition",
  Supplement_Missingness_byCycle          = "Supplementary_Figure_S2_Missingness_byCycle",
  Supplement_UACR_Prevalence_StackedBar   = "Supplementary_Figure_S3_UACR_Prevalence")
for (stub in names(submitted)) for (ext in c("png","pdf","tiff")) {
  src <- file.path(OUT, paste0(stub, ".", ext))
  if (file.exists(src)) file.copy(src, file.path(FINAL, paste0(submitted[[stub]], ".", ext)),
                                  overwrite=TRUE)
}
cat("\nDone (supplement extra figures).\n")
