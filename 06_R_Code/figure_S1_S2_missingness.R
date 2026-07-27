# Supplementary Figures S1 (covariate-missingness waterfall) and S2 (per-variable x
# per-cycle missingness heatmap), COMPREHENSIVE design, on the CALIBRATED cohort.
# Reader-friendly variable labels. Missingness is unaffected by creatinine calibration;
# this regenerates the "before" comprehensive layout with the calibrated N and labels.

suppressPackageStartupMessages({library(ggplot2)})
D <- "07_R_Data"; OUT <- "08_Analysis_Outputs/figures"
coh <- readRDS(file.path(D, "cohort_primary_v2.rds"))          # calibrated, N = 29,951
rd  <- readRDS(file.path(D, "raw_domains.rds"))
crp <- readRDS(file.path(D, "crp_harmonized.rds"))

# merge CRP + CBC and derive non-HDL / anemia so the audit is comprehensive
coh$crp <- crp$crp_mgL[match(coh$SEQN, crp$SEQN)]
cbc <- rbind(rd$cbc_legacy[, c("SEQN","LBXHGB","LBXWBCSI")], rd$cbc[, c("SEQN","LBXHGB","LBXWBCSI")])
cbc <- cbc[!duplicated(cbc$SEQN), ]
coh$hgb <- cbc$LBXHGB[match(coh$SEQN, cbc$SEQN)]
coh$wbc <- cbc$LBXWBCSI[match(coh$SEQN, cbc$SEQN)]
coh$non_hdl <- coh$tc - coh$hdl
coh$anemia  <- ifelse(is.na(coh$hgb), NA_integer_,
                      as.integer((coh$sex_female & coh$hgb < 12) | (!coh$sex_female & coh$hgb < 13)))

label <- c(age="Age", sex_female="Sex", race_eth="Race/ethnicity", educ="Education",
  pir="Income-to-poverty ratio", sbp="Systolic BP", dbp="Diastolic BP", bmi="Body-mass index",
  smoking="Smoking status", scr="Serum creatinine", egfr="eGFR", uacr="UACR", hba1c="HbA1c",
  fpg="Fasting glucose", dm_main="Diabetes", tc="Total cholesterol", hdl="HDL cholesterol",
  ldl="LDL cholesterol (fasting)", tg="Triglycerides (fasting)", non_hdl="Non-HDL cholesterol",
  dyslipidemia_main="Dyslipidemia", crp="C-reactive protein", hgb="Hemoglobin",
  wbc="White-cell count", anemia="Anemia", on_bp_treat="Antihypertensive use",
  on_lipid_lowering="Lipid-lowering use", bp_stage="BP-treatment group",
  death_allcause="Vital status", ftime_years="Follow-up time")
structural <- c("crp","hgb","wbc","anemia","fpg","ldl","tg","non_hdl","tc","hdl")
vars <- intersect(names(label), names(coh))
N <- nrow(coh)

theme_pub <- function() theme_classic(base_size = 12) + theme(
  plot.title = element_text(face="bold"), plot.subtitle = element_text(color="grey30"),
  axis.text = element_text(color="black"), panel.grid = element_blank(),
  plot.background = element_rect(fill="white", colour=NA),
  panel.background = element_rect(fill="white", colour=NA))
save3 <- function(p, base, w, h) {
  ggsave(file.path(OUT, paste0(base,".pdf")), p, width=w, height=h, device=grDevices::pdf, bg="white")
  ggsave(file.path(OUT, paste0(base,".png")), p, width=w, height=h, dpi=300, device=ragg::agg_png, bg="white")
  ggsave(file.path(OUT, paste0(base,".tiff")), p, width=w, height=h, dpi=600, device=ragg::agg_tiff, compression="lzw", bg="white")
}

# ---- S1: participants missing each covariate (waterfall bars) ----------------
order1 <- c("age","sex_female","race_eth","sbp","dbp","bmi","smoking","dm_main","uacr",
            "egfr","bp_stage","pir","educ","dyslipidemia_main","crp","hgb","wbc","fpg","ldl","tg")
order1 <- intersect(order1, vars)
s1 <- data.frame(v=order1,
  miss=sapply(order1, function(v) sum(is.na(coh[[v]]))),
  grp=ifelse(order1 %in% structural, "Subsample / cycle-limited", "Routine (MEC)"))
s1$lab <- factor(label[s1$v], levels=label[order1])
p1 <- ggplot(s1, aes(lab, miss, fill=grp)) +
  geom_col(width=0.7) +
  geom_text(aes(label=formatC(miss, format="d", big.mark=",")), vjust=-0.3, size=3, fontface="bold") +
  scale_fill_manual(values=c("Routine (MEC)"="#8AA9C4","Subsample / cycle-limited"="#C0392B"), name=NULL) +
  scale_y_continuous(labels=scales::comma, expand=expansion(mult=c(0,0.12))) +
  labs(title=NULL,
       subtitle=NULL,
       x=NULL, y="Participants missing this variable") +
  theme_pub() + theme(axis.text.x=element_text(angle=35, hjust=1), legend.position="top")
save3(p1, "Supplementary_Figure_S1_Participant_Attrition", 12, 5.6)

# ---- S2: per-variable x per-cycle % missing (heatmap, sorted) -----------------
cyclab <- c("1999-2000","2001-2002","2003-2004","2005-2006","2007-2008","2009-2010",
            "2011-2012","2013-2014","2015-2016","2017-2018")
mm <- do.call(rbind, lapply(vars, function(v) {
  pct <- tapply(is.na(coh[[v]]), coh$cycle, mean) * 100
  data.frame(v=v, cyc=as.integer(names(pct)), pct=as.numeric(pct))
}))
overall <- sapply(vars, function(v) mean(is.na(coh[[v]]))*100)
mm$lab  <- factor(label[mm$v], levels=label[names(sort(overall))])   # sorted asc -> highest on top after rev
mm$cycf <- factor(cyclab[mm$cyc], levels=cyclab)
p2 <- ggplot(mm, aes(cycf, lab, fill=pct)) +
  geom_tile(color="white", linewidth=0.4) +
  geom_text(aes(label=ifelse(pct>=1, sprintf("%.0f", pct), "")), size=2.6,
            color=ifelse(mm$pct>45,"white","grey15")) +
  scale_fill_gradientn(colors=c("#F4F8FC","#FDEBD0","#E8A87C","#C0392B","#641E16"),
    values=scales::rescale(c(0,5,25,50,100)), limits=c(0,100), name="% missing") +
  labs(title=NULL,
       subtitle=NULL,
       x=NULL, y=NULL) +
  theme_pub() + theme(axis.text.x=element_text(angle=35, hjust=1), legend.position="right")
save3(p2, "Supplementary_Figure_S2_Missingness_byCycle", 10.5, 9.5)
cat("S1 + S2 regenerated on calibrated cohort (N =", N, ") with friendly labels.\n")
