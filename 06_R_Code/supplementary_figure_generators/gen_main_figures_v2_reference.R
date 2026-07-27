# =============================================================================
# 07_figures_v2.R -- Regenerate all 5 main figures on the reanalysis cohort.
# Fig 1 cohort flow; Fig 2 BPxUACR heatmap (10-yr risk); Fig 3 within-category
# ARD; Fig 4 RCS spline (HR vs continuous UACR, ref 10 mg/g); Fig 5 subgroup
# forest (subclinical 10-29 vs <10). Outputs PNG+PDF+TIFF(600dpi). Run from root.
# =============================================================================
suppressWarnings(suppressMessages({library(ggplot2);library(ragg);library(survey);library(survival);library(rms)}))
options(survey.lonely.psu="adjust"); set.seed(20260627)
OUT<-"06_R_Code/supplementary_figure_generators/output"; dir.create(OUT,showWarnings=FALSE)
expall<-function(p,stub,w,h){
  ggsave(file.path(OUT,paste0(stub,".pdf")),p,width=w,height=h,device=grDevices::pdf)
  ggsave(file.path(OUT,paste0(stub,".png")),p,width=w,height=h,dpi=300,device=ragg::agg_png,bg="white")
  ggsave(file.path(OUT,paste0(stub,".tiff")),p,width=w,height=h,dpi=600,device=ragg::agg_tiff,compression="lzw",bg="white")
  cat("  wrote",stub,"\n")}
coh<-readRDS("07_R_Data/cohort_primary_v2.rds")
coh$uacr_cat<-droplevels(coh$uacr_cat);coh$bp_stage<-droplevels(coh$bp_stage)
coh$smoking<-droplevels(coh$smoking);coh$race_eth<-droplevels(coh$race_eth)
BP_ORDER<-c("Non-elevated BP","Elevated BP","Stage 1 hypertension","Stage 2 hypertension","Treated - Controlled","Treated - Uncontrolled")

## ---- FIGURE 1: cohort flow --------------------------------------------------
cat("Figure 1 (flow)...\n")
fl<-read.csv("REANALYSIS_2026-06-27/cohort_flow_v2.csv")
main<-c("All NHANES 1999-2018 participants"=101316,"Aged >=30 years"=45524,
  "Valid UACR"=42085,"Eligible for mortality linkage"=41995,
  "Preserved eGFR (>=60 mL/min/1.73 m2)"=36095,"No clinical cardiovascular disease"=32387,
  "Classifiable BP-treatment category"=30516,"Final analytic cohort (not pregnant)"=30063)
excl<-c("Aged <30 years: 55,792","No valid UACR: 3,439","Mortality-ineligible: 90",
  "eGFR <60: 5,900","Clinical CVD: 3,708","Unclassifiable BP: 1,871","Pregnant: 453")
n<-length(main); ys<-seq(n,1)
mdf<-data.frame(y=ys,lab=sprintf("%s\nN = %s",names(main),format(unname(main),big.mark=",")))
edf<-data.frame(y=ys[-n]-0.5,lab=excl)
p1<-ggplot()+
  geom_rect(data=mdf,aes(xmin=0,xmax=4,ymin=y-0.34,ymax=y+0.34),fill="#EAF2F8",color="#21618C",linewidth=0.7)+
  geom_text(data=mdf,aes(x=2,y=y,label=lab),size=3.2,lineheight=0.9)+
  geom_segment(data=data.frame(y=ys[-n]),aes(x=2,xend=2,y=y-0.34,yend=y-0.66),
    arrow=arrow(length=unit(0.16,"cm"),type="closed"),linewidth=0.5,color="#21618C")+
  geom_rect(data=edf,aes(xmin=4.6,xmax=8.4,ymin=y-0.22,ymax=y+0.22),fill="#FDEDEC",color="#C0392B",linewidth=0.5)+
  geom_text(data=edf,aes(x=6.5,y=y,label=lab),size=2.7)+
  geom_segment(data=edf,aes(x=2,xend=4.6,y=y,yend=y),linetype="dotted",linewidth=0.4,color="#C0392B")+
  annotate("text",x=2,y=0.30,label="Final cohort by UACR:  Normal 20,182  |  Low-grade 6,817  |  Albuminuria 3,064",size=2.9,fontface="italic")+
  coord_cartesian(xlim=c(0,8.6),ylim=c(0,n+0.6),clip="off")+theme_void()
expall(p1,"Figure1_CohortFlow_v2",8.6,9.0)

## ---- FIGURE 2: heatmap ------------------------------------------------------
cat("Figure 2 (heatmap)...\n")
T3<-read.csv("REANALYSIS_2026-06-27/T3_joint_v2.csv",check.names=FALSE)
T3$risk<-as.numeric(sub("%.*","",T3$risk))
T3$uacr<-factor(T3$UACR,levels=c("Normal UACR (<10 mg/g)","Low-grade albuminuria (10-29 mg/g)","Albuminuria (>=30 mg/g)"),
  labels=c("Normal\n(<10 mg/g)","Low-grade\n(10-29 mg/g)","Albuminuria\n(>=30 mg/g)"))
T3$bp<-factor(T3$BP,levels=rev(BP_ORDER))
T3$lab<-sprintf("%.1f%%",T3$risk)
T3$txt<-ifelse(T3$risk>11,"white","grey10")
ref<-T3[T3$BP=="Non-elevated BP" & grepl("Normal",T3$uacr),]
p2<-ggplot(T3,aes(uacr,bp,fill=risk))+
  geom_tile(color="white",linewidth=1.1)+
  geom_tile(data=ref,fill=NA,color="black",linewidth=1.4)+
  geom_text(aes(label=lab,color=txt),fontface="bold",size=4.6)+scale_color_identity()+
  scale_fill_gradient(low="#FFF5EC",high="#9E2A1B",name="Adjusted 10-yr\nrisk (%)",limits=c(4,15),breaks=c(5,8,11,14))+
  scale_x_discrete(position="top")+
  labs(title="Adjusted 10-year all-cause mortality by BP-treatment category and UACR",
    subtitle="Survey-weighted, marginally standardized complete-case Model 3. Boxed cell = global reference.",
    x="Urinary albumin-to-creatinine ratio (mg/g)",y=NULL)+
  theme_minimal(base_size=12)+
  theme(plot.title=element_text(face="bold",size=12.5),plot.subtitle=element_text(size=9.3,color="grey30",margin=margin(b=8)),
    axis.text.y=element_text(size=11,color="black"),axis.text.x=element_text(size=10.5,color="black"),
    panel.grid=element_blank())
expall(p2,"Figure2_Heatmap_BPxUACR_10yrRisk_v2",9.0,6.2)

## ---- FIGURE 3: within-category ARD ------------------------------------------
cat("Figure 3 (ARD)...\n")
T4<-read.csv("REANALYSIS_2026-06-27/T4_ard_v2.csv",check.names=FALSE)
pa<-function(s){m<-regmatches(s,regexec("\\+?([0-9.]+) \\(([0-9.-]+) to ([0-9.-]+)\\)",s))[[1]];as.numeric(m[2:4])}
rows<-do.call(rbind,lapply(1:nrow(T4),function(i){
  s<-pa(T4$subclinical_ARD[i]);a<-pa(T4$albuminuria_ARD[i])
  rbind(data.frame(bp=T4$BP[i],grp="Low-grade (10-29)",est=s[1],lo=s[2],hi=s[3]),
        data.frame(bp=T4$BP[i],grp="Albuminuria (>=30)",est=a[1],lo=a[2],hi=a[3]))}))
rows$bp<-factor(rows$bp,levels=rev(BP_ORDER))
rows$grp<-factor(rows$grp,levels=c("Low-grade (10-29)","Albuminuria (>=30)"))
p3<-ggplot(rows,aes(est,bp,color=grp))+
  geom_vline(xintercept=0,linetype="dashed",color="grey50")+
  geom_errorbarh(aes(xmin=lo,xmax=hi),height=0.25,position=position_dodge(width=0.55),linewidth=0.7)+
  geom_point(position=position_dodge(width=0.55),size=2.8)+
  scale_color_manual(values=c("Low-grade (10-29)"="#E67E22","Albuminuria (>=30)"="#922B21"),name=NULL)+
  labs(title="Within-category 10-year absolute risk difference vs normal UACR",
    subtitle="Survey-weighted complete-case Model 3; within-category reference. Bars = 95% CI (coefficient resampling).",
    x="Absolute risk difference (percentage points)",y=NULL)+
  theme_minimal(base_size=12)+theme(legend.position="top",plot.title=element_text(face="bold",size=12.5),
    plot.subtitle=element_text(size=9.2,color="grey30",margin=margin(b=8)),axis.text.y=element_text(size=11,color="black"))
expall(p3,"Figure3_ARD_within_category_v2",8.6,6.0)

## ---- FIGURE 4: RCS spline (HR vs continuous UACR, ref 10) -------------------
cat("Figure 4 (spline)...\n")
des<-svydesign(ids=~psu,strata=~strata,weights=~wt_pooled,nest=TRUE,data=coh)
KQ<-c(.05,.35,.65,.95); knots<-as.numeric(quantile(coh$uacr_log2,KQ,na.rm=TRUE)); kn_mgg<-2^knots
kexpr<-paste0("c(",paste(sprintf("%.10g",knots),collapse=","),")")
ADJ<-"age+sex_female+race_eth+cycle+bmi+smoking+dm_main+bp_stage+egfr"
fit<-svycoxph(as.formula(sprintf("Surv(ftime_years,death_allcause)~rms::rcs(uacr_log2,%s)+%s",kexpr,ADJ)),design=des)
fitl<-svycoxph(as.formula(sprintf("Surv(ftime_years,death_allcause)~uacr_log2+%s",ADJ)),design=des)
prng<-as.numeric(quantile(coh$uacr,c(.01,.99),na.rm=TRUE)); xlo<-max(1,prng[1]); xhi<-min(400,prng[2])
ug<-exp(seq(log(xlo),log(xhi),length.out=400))
prof<-coh[rep(1,length(ug)),]; prof$uacr_log2<-log2(ug)
refrow<-prof[1,]; refrow$uacr_log2<-log2(10)
mm<-model.matrix(delete.response(terms(fit)),data=rbind(refrow,prof))
sp<-grep("uacr_log2",colnames(mm)); mmd<-sweep(mm[-1,sp,drop=FALSE],2,mm[1,sp]); b<-coef(fit)[colnames(mm)[sp]]; V<-vcov(fit)[colnames(mm)[sp],colnames(mm)[sp]]
lp<-as.numeric(mmd%*%b); se<-sqrt(pmax(0,rowSums((mmd%*%V)*mmd)))
sdf<-data.frame(uacr=ug,HR=exp(lp),lo=exp(lp-1.96*se),hi=exp(lp+1.96*se))
bd<-coef(fitl)["uacr_log2"]; hrdbl<-sprintf("%.2f (%.2f-%.2f)",exp(bd),exp(bd-1.96*sqrt(vcov(fitl)["uacr_log2","uacr_log2"])),exp(bd+1.96*sqrt(vcov(fitl)["uacr_log2","uacr_log2"])))
ticks<-c(1,2,3,5,10,30,100,300); ymax<-min(4,max(sdf$hi)*1.05)
p4<-ggplot(sdf,aes(uacr,HR))+
  annotate("rect",xmin=10,xmax=30,ymin=0.5,ymax=ymax,fill="#FDF2E9",alpha=0.7)+
  geom_hline(yintercept=1,linetype="dashed",color="grey50")+
  geom_vline(xintercept=c(10,30),linetype="dotted",color="grey55")+
  geom_ribbon(aes(ymin=lo,ymax=hi),fill="#5499C7",alpha=0.30)+
  geom_line(color="#1F4E79",linewidth=1.0)+
  annotate("point",x=kn_mgg[kn_mgg>=xlo&kn_mgg<=xhi],y=1,shape=124,size=3,color="grey40")+
  annotate("text",x=xhi,y=ymax,hjust=1,vjust=1,size=3.2,label=sprintf("HR per doubling: %s",hrdbl))+
  annotate("text",x=c(sqrt(xlo*10),sqrt(10*30),sqrt(30*xhi)),y=0.55,label=c("normal\n(<10)","subclinical\n(10-29)","clinical\n(>=30)"),size=2.9,color="grey35")+
  scale_x_continuous(trans="log10",breaks=ticks,labels=ticks)+
  coord_cartesian(ylim=c(0.5,ymax))+
  labs(title="Adjusted hazard ratio for all-cause mortality across continuous UACR",
    subtitle="Survey-weighted complete-case Model 3, restricted cubic spline (4 knots); reference 10 mg/g.",
    x="UACR (mg/g, log scale)",y="Adjusted hazard ratio (95% CI)")+
  theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold",size=12.5),
    plot.subtitle=element_text(size=9.2,color="grey30",margin=margin(b=8)),panel.grid.minor=element_blank())
expall(p4,"Figure4_RCS_UACR_AllCause_v2",8.6,5.8)

## ---- FIGURE 5: subgroup forest (subclinical 10-29 vs <10) -------------------
cat("Figure 5 (subgroup forest)...\n")
coh$agec<-factor(ifelse(coh$age<60,"<60 years",">=60 years"),levels=c("<60 years",">=60 years"))
coh$dmc<-factor(ifelse(coh$dm_main==1,"Diabetes","No diabetes"),levels=c("No diabetes","Diabetes"))
coh$sexc<-factor(ifelse(coh$sex_female==1,"Women","Men"),levels=c("Men","Women"))
coh$egfrc<-factor(ifelse(coh$egfr>=90,"eGFR >=90","eGFR 60-89"),levels=c("eGFR 60-89","eGFR >=90"))
coh$obc<-factor(ifelse(coh$bmi>=30,"Obese","Non-obese"),levels=c("Non-obese","Obese"))
FULL<-"age+sex_female+race_eth+cycle+bmi+smoking+dm_main+bp_stage+egfr"
hr_sub<-function(dat,adj){
  d<-svydesign(ids=~psu,strata=~strata,weights=~wt_pooled,nest=TRUE,data=dat)
  f<-tryCatch(svycoxph(as.formula(sprintf("Surv(ftime_years,death_allcause)~uacr_cat+%s",adj)),design=d,singular.ok=TRUE),error=function(e)NULL)
  if(is.null(f))return(c(NA,NA,NA,NA))
  ci<-summary(f)$conf.int; r<-ci[grep("^uacr_cat",rownames(ci))[1],c(1,3,4)]; c(r,sum(dat$death_allcause))}
pint<-function(col,adj){   # interaction tested on the SAME dichotomy shown in the figure
  d<-svydesign(ids=~psu,strata=~strata,weights=~wt_pooled,nest=TRUE,data=coh)
  f<-tryCatch(svycoxph(as.formula(sprintf("Surv(ftime_years,death_allcause)~uacr_cat*%s+%s",col,adj)),design=d,singular.ok=TRUE),error=function(e)NULL)
  if(is.null(f))return(NA)
  rt<-tryCatch(survey::regTermTest(f,as.formula(sprintf("~uacr_cat:%s",col))),error=function(e)NULL)
  if(is.null(rt))NA else as.numeric(rt$p)}
specs<-list(
  list("Overall",NULL,NULL,FULL),
  list("Sex","sexc","sex_female","age+race_eth+cycle+bmi+smoking+dm_main+bp_stage+egfr"),
  list("Age","agec","age","sex_female+race_eth+cycle+bmi+smoking+dm_main+bp_stage+egfr"),
  list("Diabetes","dmc","dm_main","age+sex_female+race_eth+cycle+bmi+smoking+bp_stage+egfr"),
  list("eGFR","egfrc","egfr","age+sex_female+race_eth+cycle+bmi+smoking+dm_main+bp_stage"),
  list("Obesity","obc","bmi","age+sex_female+race_eth+cycle+smoking+dm_main+bp_stage+egfr"))
R<-list()
for(s in specs){lbl<-s[[1]];col<-s[[2]];vint<-s[[3]];adj<-s[[4]]
  if(is.null(col)){r<-hr_sub(coh,adj);R[[length(R)+1]]<-data.frame(grp=lbl,lvl="Overall",HR=r[1],lo=r[2],hi=r[3],dz=r[4],pint=NA,header=FALSE)}
  else{p<-pint(col,adj);R[[length(R)+1]]<-data.frame(grp=lbl,lvl=lbl,HR=NA,lo=NA,hi=NA,dz=NA,pint=p,header=TRUE)
    for(lv in levels(coh[[col]])){r<-hr_sub(droplevels(coh[which(coh[[col]]==lv),]),adj)
      R[[length(R)+1]]<-data.frame(grp=lbl,lvl=paste0("   ",lv),HR=r[1],lo=r[2],hi=r[3],dz=r[4],pint=NA,header=FALSE)}}}
F<-do.call(rbind,R); F$ord<-rev(seq_len(nrow(F))); F$y<-F$ord
F$txt<-ifelse(F$header,"",ifelse(is.na(F$HR),"-",sprintf("%.2f (%.2f-%.2f)",F$HR,F$lo,F$hi)))
F$ptxt<-ifelse(!is.na(F$pint),sprintf("P-int=%.2f",F$pint),"")
xmax<-2.6
p5<-ggplot(F,aes(HR,y))+
  geom_vline(xintercept=1,linetype="dashed",color="grey55")+
  geom_errorbarh(data=subset(F,!is.na(HR)),aes(xmin=lo,xmax=hi),height=0,linewidth=0.6,color="#1F4E79")+
  geom_point(data=subset(F,!is.na(HR)),aes(size=dz),color="#1F4E79")+scale_size(range=c(1.6,4),guide="none")+
  geom_text(aes(x=-1.05,label=ifelse(header,as.character(lvl),lvl)),hjust=0,size=3.0,fontface=ifelse(F$header,"bold","plain"))+
  geom_text(aes(x=xmax+0.05,label=txt),hjust=0,size=2.9)+
  geom_text(aes(x=xmax+0.05,label=ptxt),hjust=0,size=2.7,color="grey30")+
  scale_x_continuous(limits=c(-1.05,xmax+0.9),breaks=c(0.8,1,1.3,1.6,2),expand=c(0,0))+
  labs(title="Low-grade albuminuria (10-29 vs <10 mg/g) and all-cause mortality by subgroup",
    subtitle="Stratum-specific complete-case Model 3 hazard ratios (stratifying covariate excluded). Subgroup analyses are exploratory.",
    x="Adjusted hazard ratio (95% CI)",y=NULL)+
  theme_minimal(base_size=12)+theme(plot.title=element_text(face="bold",size=12),
    plot.subtitle=element_text(size=8.8,color="grey30",margin=margin(b=8)),
    axis.text.y=element_blank(),panel.grid=element_blank(),axis.ticks.y=element_blank())
expall(p5,"Figure5_Forest_Subclinical_AllCause_v2",9.2,7.0)
cat("\nAll 5 figures written to",OUT,"\n")
