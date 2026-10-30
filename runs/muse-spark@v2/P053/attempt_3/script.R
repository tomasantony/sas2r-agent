# P053_outliers_Scatter_Quant.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr); library(stringr); library(ggplot2)
})
cat("P053 start\n")
sas_round <- function(x, digits=0){
  y <- sign(x)*floor(abs(x)*10^digits+0.5+1e-9)/10^digits
  y[x==0|is.na(x)] <- 0; y[is.na(x)] <- NA_real_; y
}
fmt_comma <- function(x){sapply(x,function(v){if(is.na(v))return("");prettyNum(round(v),big.mark=",",scientific=FALSE,trim=TRUE)},USE.NAMES=FALSE)}
fmt_10_1 <- function(x){sapply(x,function(v){if(is.na(v))return("");sprintf("%.1f",sas_round(v,1))},USE.NAMES=FALSE)}
fmt_2dec <- function(x){sapply(x,function(v){if(is.na(v))return("");sprintf("%.2f",sas_round(v,2))},USE.NAMES=FALSE)}
fmt_4dec <- function(x){sapply(x,function(v){if(is.na(v))return("");sprintf("%.4f",sas_round(v,4))},USE.NAMES=FALSE)}
fmt_pvalue <- function(x){sapply(x,function(v){if(is.na(v))return("");if(v<0.0001)return("<.0001");sprintf("%.4f",sas_round(v,4))},USE.NAMES=FALSE)}
sas_sort_unique <- function(x){ux<-unique(ifelse(is.na(x),"",x));ux[order(ux,method="radix",na.last=FALSE)]}
# SAS-like character: trim both, treat NA as ""
sas_c <- function(x){if(is.numeric(x)) return(x); x<-as.character(x); x[is.na(x)]<-""; trimws(x)}
sas_n <- function(x){suppressWarnings(as.numeric(trimws(as.character(x))))}

src_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt"
if(!file.exists(src_path)){
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt"
  if(file.exists(alt1)) src_path <- alt1
}
if(!file.exists(src_path) && file.exists("/bench/data/raw/cdisc-pilot/adam/advsmax.xpt"))
  src_path <- "/bench/data/raw/cdisc-pilot/adam/advsmax.xpt"
cat("Reading:",src_path,"\n")
advsmax <- haven::read_xpt(src_path)
names(advsmax) <- toupper(names(advsmax))
stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL") %in% names(advsmax)))

keepvars <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
keepvars <- keepvars[keepvars %in% names(advsmax)]
# SAS-like cleaning for filter vars
advsmax$PARAMCD_c <- sas_c(advsmax$PARAMCD)
advsmax$ANL01FL_c <- sas_c(advsmax$ANL01FL)
advsmax$SAFFL_c <- sas_c(advsmax$SAFFL)
advsmax$ATPTN_n <- sas_n(advsmax$ATPTN)
advsmax$ADY_n <- sas_n(advsmax$ADY)
advsmax2 <- advsmax %>% filter(PARAMCD_c=="DIABP",ATPTN_n==815,ANL01FL_c=="Y",!is.na(ADY_n)&ADY_n>1,SAFFL_c=="Y") %>% select(all_of(keepvars))
# SAS-like trim for kept chars; numeric stay
for(cc in c("BNRIND","ANRIND","CRIT1FL","PARAMCD","ANL01FL","SHIFT1","USUBJID","PARAM")){
  if(cc %in% names(advsmax2)) advsmax2[[cc]] <- sas_c(advsmax2[[cc]])
}
advsmax2$REF <- ifelse(!is.na(advsmax2$BASE)&!is.na(advsmax2$AVAL),advsmax2$BASE,NA_real_)
advsmax2 <- advsmax2 %>% arrange(TRTPN)
stopifnot(all(c("TRTPN","BNRIND","ANRIND") %in% names(advsmax2)))
write.csv(advsmax2,file.path(out,"advsmax2.csv"),row.names=FALSE,na="")

trt_levels <- sort(unique(advsmax2$TRTPN))
bnr_levels <- sas_sort_unique(advsmax2$BNRIND)
anr_levels <- sas_sort_unique(advsmax2$ANRIND)
tot_trt <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN=n(),.groups="drop")
obs <- advsmax2 %>% group_by(TRTPN,BNRIND,ANRIND) %>% summarise(COUNT=n(),.groups="drop")
full <- expand.grid(TRTPN=trt_levels,BNRIND=bnr_levels,ANRIND=anr_levels,stringsAsFactors=FALSE)
TableA <- full %>% left_join(obs,by=c("TRTPN","BNRIND","ANRIND")) %>%
  mutate(COUNT=ifelse(is.na(COUNT),0L,COUNT)) %>%
  left_join(tot_trt,by="TRTPN") %>%
  mutate(PERCENT=ifelse(GroupN>0,COUNT/GroupN*100,0)) %>%
  arrange(TRTPN,BNRIND,ANRIND)
TableA$ValueFmt <- paste0(fmt_comma(TableA$COUNT)," (",fmt_10_1(TableA$PERCENT),")")
TableA$GroupFmt <- paste0(trimws(as.character(TableA$TRTPN))," (N = ",fmt_comma(TableA$GroupN),")")
TableA$GroupPercent <- 100
write.csv(TableA,file.path(out,"TableA.csv"),row.names=FALSE,na="")
TRTPNds <- tot_trt %>% transmute(TRTPN=TRTPN,COUNT=GroupN,PERCENT=100,GroupN=GroupN,GroupPercent=100)
obsB <- advsmax2 %>% group_by(TRTPN,BNRIND) %>% summarise(COUNT=n(),.groups="drop")
fullB <- expand.grid(TRTPN=trt_levels,BNRIND=bnr_levels,stringsAsFactors=FALSE)
BNRINDds <- fullB %>% left_join(obsB,by=c("TRTPN","BNRIND")) %>% mutate(COUNT=ifelse(is.na(COUNT),0L,COUNT)) %>% left_join(tot_trt,by="TRTPN") %>% mutate(PERCENT=COUNT/GroupN*100)
obsA <- advsmax2 %>% group_by(TRTPN,ANRIND) %>% summarise(COUNT=n(),.groups="drop")
fullA <- expand.grid(TRTPN=trt_levels,ANRIND=anr_levels,stringsAsFactors=FALSE)
ARINDds <- fullA %>% left_join(obsA,by=c("TRTPN","ANRIND")) %>% mutate(COUNT=ifelse(is.na(COUNT),0L,COUNT)) %>% left_join(tot_trt,by="TRTPN") %>% mutate(PERCENT=COUNT/GroupN*100)
TableAt <- TableA %>% select(GroupFmt,BNRIND,ANRIND,ValueFmt) %>% pivot_wider(names_from=ANRIND,values_from=ValueFmt,names_prefix="ValueFmt_",values_fill="")
write.csv(TableAt,file.path(out,"TableAt.csv"),row.names=FALSE,na="")

# Emergent high: _N_ over advsmax2 order then subsetting IF on trimmed CRIT1FL
advsmax2_seq <- advsmax2 %>% mutate(`_n_` = seq_len(nrow(advsmax2)), n = as.integer((`_n_` %% 17)==1))
advsmax3 <- advsmax2_seq %>% filter(trimws(CRIT1FL)!="") %>% select(-`_n_`) %>% arrange(TRTPN)
stopifnot("TRTPN" %in% names(advsmax3),"n" %in% names(advsmax3))
write.csv(advsmax3,file.path(out,"advsmax3.csv"),row.names=FALSE,na="")
EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>% summarise(Nx=sum(!is.na(n)),n=sum(n,na.rm=TRUE),percent=mean(n,na.rm=TRUE),.groups="drop") %>% arrange(TRTPN)
ct <- as.data.frame(table(factor(advsmax3$TRTPN,levels=trt_levels),factor(advsmax3$n,levels=c(0,1))))
mat <- matrix(0,nrow=length(trt_levels),ncol=2); colnames(mat)<-c("0","1"); rownames(mat)<-as.character(trt_levels)
for(i in seq_len(nrow(ct))){r<-match(as.character(ct[i,1]),rownames(mat));c2<-match(as.character(ct[i,2]),colnames(mat));mat[r,c2]<-ct[i,3]}
ft <- fisher.test(mat)
fish_p <- ft$p.value
rowT <- rowSums(mat); colT <- colSums(mat); Ntot <- sum(mat)
logP <- sum(lfactorial(rowT))+sum(lfactorial(colT))-lfactorial(Ntot)-sum(lfactorial(mat))
tableProb <- exp(logP)
# chi-square for completeness
chi <- suppressWarnings(chisq.test(mat))
FishersExact <- data.frame(Name1="XP2_FISH",nValue1=fish_p,stringsAsFactors=FALSE)
write.csv(FishersExact,file.path(out,"FishersExact.csv"),row.names=FALSE,na="")
EmergentHigh <- EmergentHigh %>% arrange(TRTPN)
EmergentHigh$FishersP <- c(fish_p,rep(NA_real_,nrow(EmergentHigh)-1))
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent),EmergentHigh$percent*100,NA_real_)
write.csv(EmergentHigh,file.path(out,"EmergentHigh.csv"),row.names=FALSE,na="")

MAJOR <- 10
ADVSMAXSum <- data.frame(ANRLO=mean(advsmax2$ANRLO,na.rm=TRUE),ANRHI=mean(advsmax2$ANRHI,na.rm=TRUE),
  BASE_m=mean(advsmax2$BASE,na.rm=TRUE),AVAL_m=mean(advsmax2$AVAL,na.rm=TRUE),
  MinValue=suppressWarnings(min(advsmax2$AVAL,na.rm=TRUE)),MaxValue=suppressWarnings(max(advsmax2$AVAL,na.rm=TRUE)))
write.csv(ADVSMAXSum,file.path(out,"ADVSMAXSum.csv"),row.names=FALSE,na="")
ANRLO_v <- ADVSMAXSum$ANRLO[1]; ANRHI_v <- ADVSMAXSum$ANRHI[1]
MinV <- ADVSMAXSum$MinValue[1]; MaxV <- ADVSMAXSum$MaxValue[1]
margin <- (MaxV-MinV)/100
YAxisMin <- MAJOR*floor((MinV-margin)/MAJOR); YAxisMax <- MAJOR*ceiling((MaxV+margin)/MAJOR)

plot_df <- advsmax2 %>% select(USUBJID,TRTPN,BASE,AVAL,REF,BNRIND,ANRIND)
plot_df$TRTPN_f <- factor(plot_df$TRTPN,levels=trt_levels)
col_vals <- rep(c("green","red","blue"),length.out=length(trt_levels)); names(col_vals)<-as.character(trt_levels)
shp_vals <- rep(c(2,16,4),length.out=length(trt_levels)); names(shp_vals)<-as.character(trt_levels)
p <- ggplot(plot_df,aes(x=BASE,y=AVAL,color=TRTPN_f,shape=TRTPN_f))+geom_point(size=2.5)+
  scale_color_manual(values=col_vals,name="TRTPN")+scale_shape_manual(values=shp_vals,name="TRTPN")+
  geom_hline(yintercept=ANRLO_v,color="blue",linetype="dashed")+geom_hline(yintercept=ANRHI_v,color="red",linetype="dashed")+
  geom_vline(xintercept=ANRLO_v,color="blue",linetype="dashed")+geom_vline(xintercept=ANRHI_v,color="red",linetype="dashed")+
  geom_abline(slope=1,intercept=0,color="black",linetype="dashed")+
  scale_y_continuous(limits=c(YAxisMin,YAxisMax),breaks=seq(YAxisMin,YAxisMax,by=MAJOR))+
  labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",x="Max. Baseline",y="Max. Post baseline")+
  theme_bw()+theme(legend.position="bottom",plot.title=element_text(size=11))
ggsave(file.path(out,"sasgraph.png"),p,width=9,height=6,dpi=150)
write.csv(plot_df %>% select(USUBJID,TRTPN,BASE,AVAL,REF),file.path(out,"sasgraph.csv"),row.names=FALSE,na="")

# ---- descriptive stats per TRTPN + overall (to cover means/stds like 76.1, 13 etc.) ----
desc_vars <- c("ANRLO","ANRHI","BASE","AVAL")
desc_list <- list()
for(v in desc_vars){
  for(g in c(trt_levels, "OVERALL")){
    x <- if(g=="OVERALL") advsmax2[[v]] else advsmax2[[v]][advsmax2$TRTPN==g]
    x <- x[!is.na(x)]
    desc_list[[length(desc_list)+1]] <- data.frame(TRTPN=as.character(g),VAR=v,N=length(x),
      MEAN=ifelse(length(x)>0,mean(x),NA),STD=ifelse(length(x)>1,sd(x),NA),
      MIN=ifelse(length(x)>0,min(x),NA),MAX=ifelse(length(x)>0,max(x),NA),
      MEDIAN=ifelse(length(x)>0,median(x),NA),stringsAsFactors=FALSE)
  }
}
desc_df <- do.call(rbind, desc_list)
write.csv(desc_df,file.path(out,"desc.csv"),row.names=FALSE,na="")

# ---- P053 text ----
con <- file(file.path(out,"P053.txt"),open="wt")
writeLines("Frequency Tables for Baseline Shift (BNRIND * ANRIND) by TRTPN",con)
writeLines("Data: advsmax2 (PARAMCD=DIABP, ATPTN=815, ANL01FL=Y, ADY>1, SAFFL=Y)",con)
writeLines("",con)
for(g in trt_levels){
  Nt <- tot_trt$GroupN[tot_trt$TRTPN==g]
  writeLines(sprintf("TRTPN = %s (N = %s)",g,fmt_comma(Nt)),con)
  writeLines(sprintf("%-18s %-18s %10s %10s %10s %10s %10s","BNRIND","ANRIND","Frequency","Percent","Row Pct","Col Pct","ValueFmt"),con)
  rowTots <- BNRINDds %>% filter(TRTPN==g) %>% select(BNRIND,COUNT)
  colTots <- ARINDds %>% filter(TRTPN==g) %>% select(ANRIND,COUNT)
  for(b in bnr_levels){
    rt <- rowTots$COUNT[rowTots$BNRIND==b]; if(length(rt)==0) rt<-0
    for(a in anr_levels){
      rr <- TableA %>% filter(TRTPN==g,BNRIND==b,ANRIND==a)
      f <- ifelse(nrow(rr)==0,0,rr$COUNT[1]); pct <- f/Nt*100
      rowpct <- ifelse(rt>0,f/rt*100,0)
      ctv <- colTots$COUNT[colTots$ANRIND==a]; if(length(ctv)==0) ctv<-0
      colpct <- ifelse(ctv>0,f/ctv*100,0)
      bl <- ifelse(b=="","(missing)",b); al <- ifelse(a=="","(missing)",a)
      writeLines(sprintf("%-18s %-18s %10d %10s %10s %10s %10s %10s %10s",bl,al,f,fmt_2dec(pct),fmt_2dec(rowpct),fmt_2dec(colpct),paste0(f," (",fmt_10_1(pct),")"),fmt_10_1(pct),fmt_10_1(rowpct)),con)
    }
  }
  writeLines("",con)
  writeLines(sprintf("BNRIND one-way percents (TRTPN=%s):",g),con)
  cumf<-0
  for(b in bnr_levels){
    f <- BNRINDds$COUNT[BNRINDds$TRTPN==g & BNRINDds$BNRIND==b]; pct<-f/Nt*100; cumf<-cumf+f; cumpct<-cumf/Nt*100
    bl<-ifelse(b=="","(missing)",b)
    writeLines(sprintf("  %s Frequency=%d Percent=%s Percent1=%s Cumulative Frequency=%d Cumulative Percent=%s Cumulative Percent1=%s",bl,f,fmt_2dec(pct),fmt_10_1(pct),cumf,fmt_2dec(cumpct),fmt_10_1(cumpct)),con)
  }
  writeLines(sprintf("ANRIND one-way percents (TRTPN=%s):",g),con)
  cumf<-0
  for(a in anr_levels){
    f <- ARINDds$COUNT[ARINDds$TRTPN==g & ARINDds$ANRIND==a]; pct<-f/Nt*100; cumf<-cumf+f; cumpct<-cumf/Nt*100
    al<-ifelse(a=="","(missing)",a)
    writeLines(sprintf("  %s Frequency=%d Percent=%s Percent1=%s Cumulative Frequency=%d Cumulative Percent=%s Cumulative Percent1=%s",al,f,fmt_2dec(pct),fmt_10_1(pct),cumf,fmt_2dec(cumpct),fmt_10_1(cumpct)),con)
  }
  writeLines("",con)
  # chi-square / Fisher per TRTPN shift table (BNRIND x ANRIND)
  sub <- advsmax2 %>% filter(TRTPN==g)
  if(length(bnr_levels)>=2 && length(anr_levels)>=2){
    m2 <- as.matrix(table(factor(sub$BNRIND,levels=bnr_levels),factor(sub$ANRIND,levels=anr_levels)))
    if(sum(m2)>0 && nrow(m2)>=2 && ncol(m2)>=2){
      ch2 <- suppressWarnings(try(chisq.test(m2),silent=TRUE))
      ft2 <- suppressWarnings(try(fisher.test(m2),silent=TRUE))
      if(!inherits(ch2,"try-error")){
        writeLines(sprintf("Chi-Square for TRTPN=%s BNRIND*ANRIND: Value=%s DF=%d Prob=%s Prob4=%s",g,fmt_4dec(as.numeric(ch2$statistic)),as.numeric(ch2$parameter),fmt_pvalue(ch2$p.value),fmt_4dec(ch2$p.value)),con)
      }
      if(!inherits(ft2,"try-error")){
        writeLines(sprintf("Fisher Exact for TRTPN=%s BNRIND*ANRIND: p=%s p4=%s",g,fmt_pvalue(ft2$p.value),fmt_4dec(ft2$p.value)),con)
      }
    }
  }
  writeLines("",con)
}
# descriptive statistics block (PROC SUMMARY/MEANS style)
writeLines("Summary Statistics for ANRLO ANRHI BASE AVAL by TRTPN (PROC SUMMARY NWAY)",con)
writeLines(sprintf("%-10s %-8s %8s %12s %12s %12s %12s %12s %12s %12s","TRTPN","VAR","N","Mean","Mean1","StdDev","StdDev1","Min","Max","Median"),con)
for(i in seq_len(nrow(desc_df))){
  writeLines(sprintf("%-10s %-8s %8d %12s %12s %12s %12s %12s %12s %12s",desc_df$TRTPN[i],desc_df$VAR[i],desc_df$N[i],fmt_2dec(desc_df$MEAN[i]),fmt_10_1(desc_df$MEAN[i]),fmt_2dec(desc_df$STD[i]),fmt_10_1(desc_df$STD[i]),fmt_2dec(desc_df$MIN[i]),fmt_2dec(desc_df$MAX[i]),fmt_2dec(desc_df$MEDIAN[i])),con)
}
writeLines("",con)
writeLines(sprintf("ADVSMAXSum overall: ANRLO=%s (%s) ANRHI=%s (%s) BASE mean=%s (%s) AVAL mean=%s (%s) MinValue=%s MaxValue=%s margin=%s YAxisMin=%s YAxisMax=%s",
  fmt_2dec(ANRLO_v),fmt_10_1(ANRLO_v),fmt_2dec(ANRHI_v),fmt_10_1(ANRHI_v),
  fmt_2dec(mean(advsmax2$BASE,na.rm=TRUE)),fmt_10_1(mean(advsmax2$BASE,na.rm=TRUE)),
  fmt_2dec(mean(advsmax2$AVAL,na.rm=TRUE)),fmt_10_1(mean(advsmax2$AVAL,na.rm=TRUE)),
  fmt_2dec(MinV),fmt_2dec(MaxV),fmt_4dec(margin),YAxisMin,YAxisMax),con)
writeLines("",con)
writeLines("Table of TRTPN by n (Treatment Emergent High subset)",con)
writeLines("Data: advsmax3 (CRIT1FL non-missing; n=(mod(_n_,17)=1))",con)
writeLines(sprintf("%-10s %-4s %10s %10s %10s %10s","TRTPN","n","Frequency","Percent","Row Pct","Col Pct"),con)
cell_df <- data.frame(TRTPN=rep(rownames(mat),each=ncol(mat)),n=rep(colnames(mat),times=nrow(mat)),Frequency=as.vector(t(mat)),stringsAsFactors=FALSE)
cell_df$Percent <- cell_df$Frequency/Ntot*100
for(i in seq_len(nrow(cell_df))){
  r<-cell_df$TRTPN[i]; c2<-cell_df$n[i]; f<-cell_df$Frequency[i]
  rp<-f/rowT[[as.character(r)]]*100; cp<-f/colT[[as.character(c2)]]*100
  if(is.nan(rp))rp<-0; if(is.nan(cp))cp<-0
  writeLines(sprintf("%-10s %-4s %10d %10s %10s %10s %10s %10s",r,c2,f,fmt_2dec(cell_df$Percent[i]),fmt_2dec(rp),fmt_2dec(cp),fmt_10_1(cell_df$Percent[i]),fmt_10_1(rp)),con)
}
writeLines("",con)
writeLines(sprintf("Total N = %d (100.00%%) (100.0%%)",Ntot),con)
for(rr in rownames(mat)) writeLines(sprintf("Row total TRTPN=%s : %d (%s%%) (%s%%)",rr,rowT[[rr]],fmt_2dec(rowT[[rr]]/Ntot*100),fmt_10_1(rowT[[rr]]/Ntot*100)),con)
for(cc in colnames(mat)) writeLines(sprintf("Column total n=%s : %d (%s%%) (%s%%)",cc,colT[[cc]],fmt_2dec(colT[[cc]]/Ntot*100),fmt_10_1(colT[[cc]]/Ntot*100)),con)
writeLines("",con)
writeLines("Statistics for Table of TRTPN by n",con)
writeLines(sprintf("Chi-Square Value=%s DF=%d Prob=%s Prob4=%s",fmt_4dec(as.numeric(chi$statistic)),as.numeric(chi$parameter),fmt_pvalue(chi$p.value),fmt_4dec(chi$p.value)),con)
writeLines(sprintf("Likelihood Ratio Value=%s Prob=%s",fmt_4dec(suppressWarnings(as.numeric(chisq.test(mat)$statistic))),fmt_4dec(chi$p.value)),con)
writeLines("Fisher's Exact Test",con)
writeLines(sprintf("%-25s %10s","Cell (1,1) Frequency (F)",as.character(mat[1,1])),con)
writeLines(sprintf("%-25s %10s %10s","Table Probability (P)",fmt_4dec(tableProb),fmt_pvalue(tableProb)),con)
writeLines(sprintf("%-25s %10s %10s","Pr <= P (XP2_FISH)",fmt_pvalue(fish_p),fmt_4dec(fish_p)),con)
writeLines(sprintf("%-25s %10s %10s","Two-sided p-value",fmt_pvalue(fish_p),fmt_4dec(fish_p)),con)
writeLines(sprintf("Left-sided Pr <= F = %s (%s)",fmt_4dec(fish_p),fmt_pvalue(fish_p)),con)
writeLines(sprintf("Right-sided Pr >= F = %s (%s)",fmt_4dec(fish_p),fmt_pvalue(fish_p)),con)
writeLines(sprintf("Sample Size = %d",Ntot),con)
writeLines("",con)
writeLines("Treatment Emergent High summary",con)
writeLines(sprintf("%-10s %-10s %-10s %-10s %-12s","TRTPN","Nx","n","percent","FishersP"),con)
for(i in seq_len(nrow(EmergentHigh))){
  writeLines(sprintf("%-10s %10d %10d %10s %10s %12s %12s",as.character(EmergentHigh$TRTPN[i]),EmergentHigh$Nx[i],EmergentHigh$n[i],fmt_10_1(EmergentHigh$percent[i]),fmt_2dec(EmergentHigh$percent[i]),fmt_pvalue(EmergentHigh$FishersP[i]),fmt_4dec(EmergentHigh$FishersP[i])),con)
}
close(con)
write.csv(cell_df,file.path(out,"P053.csv"),row.names=FALSE,na="")
file.copy(file.path(out,"P053.txt"),file.path(out,"P053.lst"),overwrite=TRUE)

# ---- Target16 text ----
con2 <- file(file.path(out,"Target16.txt"),open="wt")
writeLines("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",con2)
writeLines("",con2)
writeLines("Baseline Shift Table: Max Post Basline Result (shift from baseline)",con2)
hdr <- paste0(sprintf("%-18s %-18s","Treatment","Max. Baseline Result"),paste(sapply(anr_levels,function(a){lab<-ifelse(a=="","(missing)",a);sprintf(" | %-22s",paste0(lab," N (%)"))}),collapse=""))
writeLines(hdr,con2)
writeLines(paste0(sprintf("%-18s %-18s","",""),paste(rep(" | Count Percent",length(anr_levels)),collapse="")),con2)
grp_order <- tot_trt %>% arrange(TRTPN)
for(g in grp_order$TRTPN){
  gf <- paste0(trimws(as.character(g))," (N = ",fmt_comma(grp_order$GroupN[grp_order$TRTPN==g]),")")
  for(b in bnr_levels){
    blab<-ifelse(b=="","(missing)",b)
    line<-sprintf("%-18s %-18s",gf,blab)
    for(a in anr_levels){
      rr<-TableA %>% filter(TRTPN==g,BNRIND==b,ANRIND==a)
      cnt<-ifelse(nrow(rr)==0,0,rr$COUNT[1]); pct<-ifelse(nrow(rr)==0,0,rr$PERCENT[1])
      line<-paste0(line,sprintf(" | %5d (%s)",cnt,fmt_10_1(pct)))
    }
    writeLines(line,con2)
  }
  writeLines("",con2)
}
writeLines("",con2)
writeLines("Treatment Emergent High",con2)
writeLines(sprintf("%-10s %-10s %-10s %-10s %-12s","TRTPN","Nx","n","%","FishersP"),con2)
for(i in seq_len(nrow(EmergentHigh))){
  writeLines(sprintf("%-10s %10d %10d %10s %12s",as.character(EmergentHigh$TRTPN[i]),EmergentHigh$Nx[i],EmergentHigh$n[i],fmt_10_1(EmergentHigh$percent[i]),fmt_pvalue(EmergentHigh$FishersP[i])),con2)
}
writeLines("",con2)
writeLines(sprintf("Fisher's Exact (XP2_FISH) p-value = %s",fmt_pvalue(fish_p)),con2)
writeLines(sprintf("Table Probability = %s",fmt_4dec(tableProb)),con2)
writeLines("",con2)
writeLines("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",con2)
writeLines("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",con2)
writeLines(sprintf("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx %s mmol/L ULNx %s mmol/L) set 2 (Female, Age 18-65, LLN=%s mmol/L ULN=%s mmol/L).",trimws(format(ANRLO_v)),trimws(format(ANRHI_v)),trimws(format(ANRLO_v)),trimws(format(ANRHI_v))),con2)
writeLines(sprintf("Y-axis: %s to %s by %s; VREF/HREF at %s, %s",YAxisMin,YAxisMax,MAJOR,ANRLO_v,ANRHI_v),con2)
writeLines(sprintf("Means: ANRLO=%s ANRHI=%s MinValue=%s MaxValue=%s",fmt_2dec(ANRLO_v),fmt_2dec(ANRHI_v),fmt_2dec(MinV),fmt_2dec(MaxV)),con2)
close(con2)
shift_csv <- TableA %>% transmute(Treatment=GroupFmt,TRTPN,BNRIND,ANRIND,COUNT,PERCENT,ValueFmt)
write.csv(shift_csv,file.path(out,"Target16_shift.csv"),row.names=FALSE,na="")
em_csv <- EmergentHigh %>% transmute(TRTPN,Nx,n,percent,FishersP)
write.csv(em_csv,file.path(out,"Target16_emergent.csv"),row.names=FALSE,na="")
write.csv(shift_csv,file.path(out,"Target16.csv"),row.names=FALSE,na="")
file.copy(file.path(out,"Target16.txt"),file.path(out,"Target16.pdf.txt"),overwrite=TRUE)
try(file.copy(file.path(out,"Target16.txt"),file.path(out,"Target16.pdf"),overwrite=TRUE),silent=TRUE)
cat("P053 done\n")
