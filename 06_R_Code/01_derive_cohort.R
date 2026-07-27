# Derive the primary NHANES 1999-2018 analytic cohort.
# Run from the submission root. Inputs and outputs are in 07_R_Data.
suppressWarnings(suppressMessages({library(dplyr)}))
source("06_R_Code/helpers/01_ckdepi.R")
rd  <- readRDS("07_R_Data/raw_domains.rds")
mort<- readRDS("07_R_Data/mortality.rds")
N<-function(x)suppressWarnings(as.numeric(x))
comb2 <- function(a,b) bind_rows(a,b) %>% filter(!is.na(val)) %>% distinct(SEQN,.keep_all=TRUE)  # modern+legacy

## NHANES special-missing values
na7 <- function(x, codes=c(7,9)) { x[x %in% codes] <- NA; x }
demo<-rd$demo; bpx<-rd$bpx
bpq <- rd$bpq %>% mutate(BPQ020=na7(BPQ020), BPQ040A=na7(BPQ040A), BPQ050A=na7(BPQ050A))
diq <- rd$diq %>% mutate(DIQ010=na7(DIQ010), DIQ050=na7(DIQ050), DIQ070=na7(DIQ070))
mcq <- rd$mcq %>% mutate(across(c(MCQ160B,MCQ160C,MCQ160D,MCQ160E,MCQ160F), na7))
smq <- rd$smq %>% mutate(SMQ020=na7(SMQ020), SMQ040=na7(SMQ040))
demo<- demo %>% mutate(DMDEDUC2=na7(DMDEDUC2))

P <- demo %>% transmute(SEQN, age=RIDAGEYR, sex_female=(RIAGENDR==2), race=RIDRETH1,
  educ=DMDEDUC2, pir=INDFMPIR, ridexprg=RIDEXPRG, cycle=SDDSRVYR,
  wt2=WTMEC2YR, wt4=WTMEC4YR, strata=SDMVSTRA, psu=SDMVPSU)

## Blood pressure: discard the first valid reading and average the rest
avg_drop_first <- function(...){ v<-c(...); v<-v[!is.na(v)]; if(length(v)>=2) mean(v[-1]) else NA_real_ }
bp <- bpx %>% rowwise() %>% mutate(
  sbp=avg_drop_first(BPXSY1,BPXSY2,BPXSY3,BPXSY4),
  dbp=avg_drop_first(BPXDI1,BPXDI2,BPXDI3,BPXDI4)) %>% ungroup() %>% select(SEQN,sbp,dbp)

## Prescription-medication flags
ANTIHTN <- paste0("LISINOPRIL|ENALAPRIL|RAMIPRIL|BENAZEPRIL|CAPTOPRIL|QUINAPRIL|FOSINOPRIL|LOSARTAN|VALSARTAN|IRBESARTAN|OLMESARTAN|TELMISARTAN|CANDESARTAN|AZILSARTAN|AMLODIPINE|NIFEDIPINE|DILTIAZEM|VERAPAMIL|FELODIPINE|NICARDIPINE|METOPROLOL|ATENOLOL|CARVEDILOL|BISOPROLOL|PROPRANOLOL|LABETALOL|NEBIVOLOL|HYDROCHLOROTHIAZIDE|CHLORTHALIDONE|FUROSEMIDE|BUMETANIDE|SPIRONOLACTONE|INDAPAMIDE|TORSEMIDE|TRIAMTERENE|AMILORIDE|METOLAZONE|EPLERENONE|DOXAZOSIN|TERAZOSIN|PRAZOSIN|CLONIDINE|METHYLDOPA|HYDRALAZINE|MINOXIDIL|ALISKIREN")
UACRDRUG <- paste0("LISINOPRIL|ENALAPRIL|RAMIPRIL|BENAZEPRIL|CAPTOPRIL|QUINAPRIL|FOSINOPRIL|LOSARTAN|VALSARTAN|IRBESARTAN|OLMESARTAN|TELMISARTAN|CANDESARTAN|AZILSARTAN|ALISKIREN|SPIRONOLACTONE|EPLERENONE|FINERENONE|CANAGLIFLOZIN|DAPAGLIFLOZIN|EMPAGLIFLOZIN|ERTUGLIFLOZIN")
LIPIDRX <- paste0("ATORVASTATIN|SIMVASTATIN|ROSUVASTATIN|PRAVASTATIN|LOVASTATIN|FLUVASTATIN|PITAVASTATIN|EZETIMIBE|GEMFIBROZIL|FENOFIBRATE|FENOFIBRIC|CHOLESTYRAMINE|COLESEVELAM|COLESTIPOL|NIACIN|ALIROCUMAB|EVOLOCUMAB|BEMPEDOIC|INCLISIRAN")
APLT <- "ASPIRIN|ACETYLSALICYLIC|CLOPIDOGREL|TICAGRELOR|PRASUGREL|DIPYRIDAMOLE|TICLOPIDINE"
rxq <- rd$rxq; rxq$dn <- toupper(as.character(rxq$RXDDRUG))
RX <- rxq %>% group_by(SEQN) %>% summarise(
  rx_bp = as.integer(any(grepl(ANTIHTN,dn),na.rm=TRUE)),
  rx_uacr = as.integer(any(grepl(UACRDRUG,dn),na.rm=TRUE)),
  on_lipid_lowering = as.integer(any(grepl(LIPIDRX,dn),na.rm=TRUE)),
  on_antiplatelet_rx = as.integer(any(grepl(APLT,dn),na.rm=TRUE)), .groups="drop")

## Antihypertensive treatment status
BPM <- bpq %>% transmute(SEQN, on_med_bpq = case_when(
  BPQ050A==1~1L, BPQ050A==2~0L, BPQ040A==2~0L, BPQ020==2~0L, TRUE~NA_integer_))
P <- P %>% left_join(bp,by="SEQN") %>% left_join(BPM,by="SEQN") %>% left_join(RX,by="SEQN") %>%
  mutate(across(c(rx_bp,rx_uacr,on_lipid_lowering,on_antiplatelet_rx), ~coalesce(.,0L)),
    on_bp_treat = case_when(on_med_bpq==1 | rx_bp==1 ~ 1L, on_med_bpq==0 & rx_bp==0 ~ 0L, TRUE ~ NA_integer_),
    on_bp_treat_bpq = on_med_bpq)

## Six BP-treatment categories; both systolic and diastolic BP are required
six <- c("Non-elevated BP","Elevated BP","Stage 1 hypertension","Stage 2 hypertension","Treated - Controlled","Treated - Uncontrolled")
stage6 <- function(sbp,dbp,on){ s<-rep(NA_character_,length(sbp)); ok<-!is.na(sbp)&!is.na(dbp)&!is.na(on)
  tr<-ok&on==1L; un<-ok&on==0L
  s[tr & sbp<130 & dbp<80] <- "Treated - Controlled"; s[tr & !(sbp<130&dbp<80)] <- "Treated - Uncontrolled"
  s[un & sbp<120 & dbp<80] <- "Non-elevated BP"; s[un & sbp>=120 & sbp<=129 & dbp<80] <- "Elevated BP"
  s[un & !(sbp<120&dbp<80) & !(sbp>=120&sbp<=129&dbp<80) & ((sbp>=130&sbp<=139)|(dbp>=80&dbp<=89))] <- "Stage 1 hypertension"
  s[un & (sbp>=140|dbp>=90)] <- "Stage 2 hypertension"; factor(s, levels=six) }
P$bp_stage     <- stage6(P$sbp,P$dbp,P$on_bp_treat)
P$bp_stage_bpq <- stage6(P$sbp,P$dbp,P$on_bp_treat_bpq)

## Laboratory measures, including legacy 1999-2004 files
scr <- comb2(rd$biopro%>%transmute(SEQN,val=N(LBXSCR)), rd$scr_legacy%>%transmute(SEQN,val=coalesce(N(LBXSCR),N(LBDSCR))))%>%rename(scr=val)
# Urine albumin + creatinine kept as separate analytes so urine creatinine can be
# calibrated (Jaffe 1999-2006 -> enzymatic-comparable 2007+) before forming UACR.
urine <- bind_rows(rd$alb_cr%>%transmute(SEQN,uma=N(URXUMA),ucr=N(URXUCR)),
                   rd$urine_alb_legacy%>%transmute(SEQN,uma=N(URXUMA),ucr=N(URXUCR))) %>%
  filter(!is.na(uma)&!is.na(ucr)&ucr>0) %>% distinct(SEQN,.keep_all=TRUE)
# NHANES ALB_CR analytic-note piecewise adjustment (sqrt scale), mg/dL.
adj_ucr <- function(x) ifelse(is.na(x),NA_real_,
  ifelse(x<75,(1.02*sqrt(x)-0.36)^2, ifelse(x<250,(1.05*sqrt(x)-0.74)^2,(1.01*sqrt(x)-0.10)^2)))
hba1c<-comb2(rd$ghb%>%transmute(SEQN,val=N(LBXGH)), rd$hba1c_legacy%>%transmute(SEQN,val=N(LBXGH)))%>%rename(hba1c=val)
fpg <- comb2(rd$glu%>%transmute(SEQN,val=N(LBXGLU)), rd$glu_legacy%>%transmute(SEQN,val=N(LBXGLU)))%>%rename(fpg=val)
tc  <- comb2(rd$tchol%>%transmute(SEQN,val=N(LBXTC)), rd$lipid_legacy_tc%>%transmute(SEQN,val=N(LBXTC)))%>%rename(tc=val)
hdl <- comb2(rd$hdl%>%transmute(SEQN,val=N(LBDHDD)), rd$lipid_legacy_tc%>%transmute(SEQN,val=coalesce(N(LBXHDD),N(LBDHDL))))%>%rename(hdl=val)
tg  <- comb2(rd$trigly%>%transmute(SEQN,val=N(LBXTR)), rd$lipid_legacy_tg%>%transmute(SEQN,val=N(LBXTR)))%>%rename(tg=val)
ldl <- comb2(rd$trigly%>%transmute(SEQN,val=N(LBDLDL)), rd$lipid_legacy_tg%>%transmute(SEQN,val=N(LBDLDL)))%>%rename(ldl=val)
P <- P %>% left_join(scr,by="SEQN") %>%
  # Selvin 2007 serum-creatinine calibration to standardized creatinine, NHANES 1999-2000 only.
  mutate(scr = ifelse(cycle==1 & !is.na(scr), 0.147 + 1.013*scr, scr),
         egfr = ckdepi_2021(scr,age,sex_female)) %>%
  left_join(urine,by="SEQN") %>%
  # Urine-creatinine method calibration for Jaffe cycles (1999-2006), then form UACR.
  mutate(ucr_adj = ifelse(cycle %in% c(1,2,3,4) & !is.na(ucr), adj_ucr(ucr), ucr),
         uacr = ifelse(!is.na(uma)&!is.na(ucr_adj)&ucr_adj>0, uma/ucr_adj*100, NA_real_)) %>%
  left_join(hba1c,by="SEQN") %>% left_join(fpg,by="SEQN") %>%
  left_join(tc,by="SEQN") %>% left_join(hdl,by="SEQN") %>% left_join(tg,by="SEQN") %>% left_join(ldl,by="SEQN")

P <- P %>% mutate(uacr_cat=cut(uacr,c(-Inf,10,30,Inf),right=FALSE,
  labels=c("Normal UACR (<10 mg/g)","Low-grade albuminuria (10-29 mg/g)","Albuminuria (>=30 mg/g)")),
  uacr_log2=ifelse(uacr>0,log2(uacr),NA_real_))

## BMI, smoking, diabetes, and dyslipidemia
P <- P %>% left_join(rd$bmx%>%transmute(SEQN,bmi=N(BMXBMI)),by="SEQN") %>%
  left_join(smq%>%transmute(SEQN, smoking=case_when(SMQ020==2~"Never",
    SMQ020==1&SMQ040==3~"Former", SMQ020==1&SMQ040%in%c(1,2)~"Current", TRUE~NA_character_)),by="SEQN") %>%
  left_join(diq%>%transmute(SEQN, dm_self=case_when(DIQ010==1~TRUE,DIQ010%in%c(2,3)~FALSE,TRUE~NA),
    dm_med=case_when(DIQ050==1|DIQ070==1~TRUE, DIQ050==2&DIQ070==2~FALSE, TRUE~NA)),by="SEQN") %>%
  mutate(smoking=factor(smoking,levels=c("Never","Former","Current")))
anypos<-function(...){m<-cbind(...);ap<-apply(m,1,function(r)any(r,na.rm=TRUE));an<-apply(m,1,function(r)all(is.na(r)));o<-as.integer(ap);o[an]<-NA_integer_;o}
lowhdl <- (P$sex_female & P$hdl<50) | (!P$sex_female & P$hdl<40)
P <- P %>% mutate(
  dm_main    = anypos(hba1c>=6.5, dm_self, dm_med),
  dm_fasting = anypos(hba1c>=6.5, dm_self, dm_med, fpg>=126),
  dyslipidemia_main    = anypos(on_lipid_lowering==1, tc>=240, lowhdl),
  dyslipidemia_fasting = anypos(on_lipid_lowering==1, tc>=240, lowhdl, ldl>=160, tg>=200))

## Prevalent cardiovascular disease
P <- P %>% left_join(mcq%>%transmute(SEQN, prev_cvd=anypos(MCQ160B==1,MCQ160C==1,MCQ160D==1,MCQ160E==1,MCQ160F==1)),by="SEQN")

## Mortality outcomes
M <- mort %>% transmute(SEQN, elig=ELIGSTAT, ftime_years=coalesce(PERMTH_EXM,PERMTH_INT)/12,
  death_allcause=ifelse(ELIGSTAT==1, as.integer(MORTSTAT==1), NA_integer_),
  death_hd=case_when(ELIGSTAT!=1~NA_integer_, MORTSTAT!=1~0L, UCOD_LEADING=="001"~1L, TRUE~0L))
P <- P %>% left_join(M,by="SEQN")

## Pooled examination weights and race/ethnicity
P <- P %>% mutate(wt_pooled=case_when(cycle%in%c(1,2)&!is.na(wt4)~wt4/5, !is.na(wt2)~wt2/10, TRUE~NA_real_),
  race_eth=factor(case_when(race==3~"Non-Hispanic White",race==4~"Non-Hispanic Black",race==1~"Mexican American",
    race==2~"Other Hispanic",race==5~"Other / Multiracial",TRUE~NA_character_),
    levels=c("Non-Hispanic White","Non-Hispanic Black","Mexican American","Other Hispanic","Other / Multiracial")))

## Eligibility flow
flow<-data.frame(step=character(),criterion=character(),n=integer()); add<-function(s,c,d)flow<<-rbind(flow,data.frame(step=s,criterion=c,n=nrow(d)))
s<-P;                                          add("0","All NHANES 1999-2018",s)
s<-s[!is.na(s$age)&s$age>=30,];                add("1","Age >=30",s)
s<-s[!is.na(s$uacr),];                         add("2","Valid UACR",s)
s<-s[!is.na(s$elig)&s$elig==1,];               add("3","Mortality-eligible",s)
s<-s[!is.na(s$egfr)&s$egfr>=60,];              add("4","Preserved eGFR >=60",s)
s<-s[!is.na(s$prev_cvd)&s$prev_cvd==0,];       add("5","No clinical CVD (prev_cvd==0)",s)
s<-s[!is.na(s$bp_stage),];                     add("6","Classifiable BP (6 groups)",s)
s<-s[!is.na(s$race_eth)&!is.na(s$sex_female),];add("7","Complete demographics",s)
s<-s[is.na(s$ridexprg)|s$ridexprg!=1,];        add("8","Not pregnant (RIDEXPRG!=1)",s)
cohort<-droplevels(s)
print(flow); cat("\nNEW PRIMARY COHORT N =",nrow(cohort)," deaths(all-cause) =",sum(cohort$death_allcause)," HD deaths =",sum(cohort$death_hd),"\n")
cat("BP categories:\n");print(table(cohort$bp_stage)); cat("UACR:\n");print(table(cohort$uacr_cat))
cat("pregnant excluded:",sum(P$age>=30 & !is.na(P$ridexprg) & P$ridexprg==1, na.rm=TRUE),"\n")
saveRDS(cohort,"07_R_Data/cohort_primary_v2.rds"); write.csv(flow,"07_R_Data/cohort_flow_v2.csv",row.names=FALSE)
cat("Saved cohort_primary_v2.rds + cohort_flow_v2.csv\n")
