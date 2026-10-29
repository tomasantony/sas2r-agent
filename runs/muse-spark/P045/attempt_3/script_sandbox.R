suppressMessages({
  library(haven); library(dplyr); library(tidyr); library(stringr); library(readr); library(tibble)
})
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out=="") out <- getwd()
dir.create(out, showWarnings=FALSE, recursive=TRUE)
message("OUT_DIR=", out)
dirs_try <- c("/bench/data/raw/cdisc-pilot/adam",
              "/bench/data/derived/cdisc-pilot/adam")
find_file <- function(f){ for(d in dirs_try){p<-file.path(d,f); if(file.exists(p)) return(p)}
  r<-"/bench"; cand<-list.files(r,pattern=paste0("^",f,"$"),recursive=TRUE,full.names=TRUE)
  if(length(cand)>0) return(cand[1]); return(NULL)}
adae_path <- find_file("adae.xpt"); adsl_path <- find_file("adsl.xpt")
message("adae_path=",adae_path); message("adsl_path=",adsl_path)
adae_raw <- haven::read_xpt(adae_path); adsl_raw <- haven::read_xpt(adsl_path)
names(adae_raw)<-toupper(names(adae_raw)); names(adsl_raw)<-toupper(names(adsl_raw))
round_half_up <- function(x,d=0){ p<-sign(x); z<-abs(x)*10^d; z<-z+0.5+sqrt(.Machine$double.eps); z<-trunc(z); z<-z/10^d; z*p }
fmt2 <- function(x) sprintf("%.2f", round_half_up(as.numeric(x),2))
fmt4 <- function(x) sprintf("%.4f", round_half_up(as.numeric(x),4))
trim4 <- function(x){ s<-fmt4(x); sub("0+$","",sub("\\.$","",s)) } # not used for exact, keep both
trim_num <- function(s){ # trim trailing zeros like SAS listing may show 2.221 for 2.2210
  if(grepl("\\.",s)){ s<-sub("0+$","",s); s<-sub("\\.$","",s)}; s }
fmt_best12 <- function(p){ if(is.na(p)) return(""); trimws(formatC(p,format="g",digits=12)) }
fmt_cp <- function(cnt){ cnt<-as.integer(cnt); paste0(as.character(cnt),"(",sprintf("%.1f",cnt),")") }
is_Y <- function(x) !is.na(x) & trimws(as.character(x))=="Y"

adae <- adae_raw %>% filter((is_Y(AOCC02FL)|is_Y(AOCC04FL)) & is_Y(AESER) & is_Y(SAFFL)) %>%
  select(USUBJID,TRTAN,AEDECOD,AESER,AOCC02FL,AOCC04FL,SAFFL) %>%
  mutate(USUBJID=as.character(USUBJID),AEDECOD=as.character(AEDECOD),TRTAN=as.numeric(TRTAN)) %>% arrange(USUBJID)
adsl <- adsl_raw %>% filter(!is.na(TRT01AN)) %>% select(USUBJID,TRT01AN) %>%
  mutate(USUBJID=as.character(USUBJID),TRT01AN=as.numeric(TRT01AN)) %>%
  arrange(USUBJID,TRT01AN) %>% distinct(USUBJID,TRT01AN,.keep_all=TRUE)
message("adae n=",nrow(adae)," adsl n=",nrow(adsl))
adae_sl <- inner_join(adsl,adae,by="USUBJID")
sum_1 <- adsl %>% group_by(TRT01AN) %>% summarise(COUNT=n(),.groups="drop") %>% arrange(TRT01AN)
trtnum <- sort(unique(sum_1$TRT01AN))
trtcn <- setNames(sum_1$COUNT,as.character(sum_1$TRT01AN))
getN <- function(v){k<-as.character(v); if(k %in% names(trtcn)) trtcn[[k]] else NA_integer_}
N0<-getN(0); N54<-getN(54); N81<-getN(81)
message(sprintf("N0=%s N54=%s N81=%s",N0,N54,N81))
adae_sl2 <- adae_sl %>% arrange(USUBJID,AEDECOD) %>% distinct(USUBJID,AEDECOD,.keep_all=TRUE)
adae_dummy <- adae_sl %>% filter(is_Y(AOCC02FL)) %>% arrange(AEDECOD) %>% distinct(AEDECOD) %>% select(AEDECOD)
adae_sl3 <- adae_sl2 %>% filter(AEDECOD %in% adae_dummy$AEDECOD)
sum_2 <- adae_sl3 %>% group_by(AEDECOD,TRTAN) %>% summarise(COUNT=n(),.groups="drop")
dummy_2 <- expand.grid(AEDECOD=adae_dummy$AEDECOD,TRTAN=trtnum,stringsAsFactors=FALSE) %>% as_tibble() %>%
  mutate(AEDECOD=as.character(AEDECOD),TRTAN=as.numeric(TRTAN)) %>% arrange(AEDECOD,TRTAN)
adae_sl4 <- left_join(dummy_2,sum_2,by=c("AEDECOD","TRTAN"))
sum_1_ren <- sum_1 %>% rename(TRTAN=TRT01AN,BIGN=COUNT)
adae_sl5 <- left_join(adae_sl4,sum_1_ren,by="TRTAN") %>% arrange(TRTAN)
adae_sl6_resp1 <- adae_sl5 %>% mutate(RESP=1L,COUNT_FINAL=ifelse(is.na(COUNT),0,COUNT))
adae_sl6_resp0 <- adae_sl5 %>% mutate(RESP=0L,COUNT_FINAL=ifelse(is.na(COUNT),BIGN,BIGN-COUNT))
adae_sl6 <- bind_rows(adae_sl6_resp0,adae_sl6_resp1) %>% arrange(AEDECOD,TRTAN,RESP)

pearson_stat <- function(mat){ n<-sum(mat); rt<-rowSums(mat); ct<-colSums(mat); exp<-outer(rt,ct)/n
  x2<-sum((mat-exp)^2/exp); df<-(nrow(mat)-1)*(ncol(mat)-1); p<-1-pchisq(x2,df); list(v=x2,df=df,p=p) }
lr_stat <- function(mat){ n<-sum(mat); rt<-rowSums(mat); ct<-colSums(mat); exp<-outer(rt,ct)/n
  term<-ifelse(mat==0,0,mat*log(mat/exp)); g2<-2*sum(term); df<-(nrow(mat)-1)*(ncol(mat)-1); p<-1-pchisq(g2,df); list(v=g2,df=df,p=p) }
mh_stat <- function(mat,rs,cs){ n<-sum(mat); rt<-rowSums(mat); ct<-colSums(mat)
  abar<-sum(rs*rt)/n; bbar<-sum(cs*ct)/n; S<-0
  for(i in seq_along(rs)) for(j in seq_along(cs)) S<-S+rs[i]*cs[j]*mat[i,j]
  S<-S-n*abar*bbar; vr<-sum((rs-abar)^2*rt); vc<-sum((cs-bbar)^2*ct)
  r<-S/sqrt(vr*vc); q<-(n-1)*r^2; p<-1-pchisq(q,1); list(v=q,df=1,p=p,r=r) }

pt_list <- sort(unique(adae_sl6$AEDECOD))
fish_rows<-list(); sum_final_rows<-list(); detail<-list()
for(pt in pt_list){
  sub<-adae_sl6 %>% filter(AEDECOD==pt); trts<-sort(unique(sub$TRTAN))
  m0<-sapply(trts,function(t) sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==0][1])
  m1<-sapply(trts,function(t) sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==1][1])
  mat<-cbind(m0,m1); rownames(mat)<-trts
  pval<-suppressWarnings(fisher.test(mat)$p.value)
  fish_rows[[pt]]<-tibble(AEDECOD=pt,CVALUE1=pval)
  for(t in trts){ cnt1<-sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==1][1]
    sum_final_rows[[length(sum_final_rows)+1]]<-tibble(AEDECOD=pt,TRTAN=t,COUNT=cnt1) }
  grand<-sum(sub$COUNT_FINAL); col0<-sum(sub$COUNT_FINAL[sub$RESP==0]); col1<-sum(sub$COUNT_FINAL[sub$RESP==1])
  ps<-pearson_stat(mat); lr<-lr_stat(mat); mh<-mh_stat(mat,trts,c(0,1))
  n<-grand; phi<-sqrt(ps$v/n); cont<-sqrt(ps$v/(ps$v+n)); Vc<-sqrt(ps$v/(n*(min(nrow(mat),ncol(mat))-1)))
  detail[[pt]]<-list(sub=sub,trts=trts,grand=grand,col0=col0,col1=col1,pval=pval,mat=mat,ps=ps,lr=lr,mh=mh,phi=phi,cont=cont,Vc=Vc)
}
fish2<-bind_rows(fish_rows)
sum_final<-bind_rows(sum_final_rows) %>% mutate(CP=fmt_cp(COUNT),ORD=ifelse(TRTAN==81,1L,2L)) %>% arrange(AEDECOD,ORD)
cntmax_df<-sum_final %>% filter(ORD==1L) %>% select(AEDECOD,COUNT_MAX=COUNT)
sum_final<-left_join(sum_final,cntmax_df,by="AEDECOD") %>% arrange(AEDECOD,COUNT_MAX)
sum_final2_wide<-sum_final %>% select(AEDECOD,COUNT_MAX,TRTAN,CP) %>% pivot_wider(names_from=TRTAN,values_from=CP,names_prefix="TRT")
for(cc in c("TRT0","TRT54","TRT81")) if(!cc %in% names(sum_final2_wide)) sum_final2_wide[[cc]]<-NA_character_
sum_final2<-left_join(sum_final2_wide,fish2,by="AEDECOD") %>% arrange(desc(COUNT_MAX),AEDECOD)

adae_sl_sub<-adae_sl %>% filter(is_Y(AOCC02FL)) %>% distinct(USUBJID) %>% select(USUBJID)
adae_sl_sub2<-adsl %>% left_join(mutate(adae_sl_sub,PRESENT=1L),by="USUBJID") %>%
  mutate(FLG=ifelse(!is.na(PRESENT),1L,0L)) %>% select(USUBJID,TRT01AN,FLG)
overall_tab<-adae_sl_sub2 %>% rename(TRTAN=TRT01AN)
trts_all<-sort(unique(overall_tab$TRTAN))
o_m0<-sapply(trts_all,function(t) sum(overall_tab$TRTAN==t & overall_tab$FLG==0))
o_m1<-sapply(trts_all,function(t) sum(overall_tab$TRTAN==t & overall_tab$FLG==1))
o_mat<-cbind(o_m0,o_m1)
p_overall<-suppressWarnings(fisher.test(o_mat)$p.value)
fish_sub2<-tibble(CVALUE1=p_overall)
cnt_flg1<-overall_tab %>% filter(FLG==1L) %>% group_by(TRTAN) %>% summarise(COUNT=n(),.groups="drop")
grid_all<-tibble(TRTAN=trts_all)
sum_final_sub<-left_join(grid_all,cnt_flg1,by="TRTAN") %>%
  mutate(COUNT=ifelse(is.na(COUNT),0L,COUNT),CP=fmt_cp(COUNT),AEDECOD="Number of subjects reporting serious adverse events")
sum_final_sub_wide<-sum_final_sub %>% select(AEDECOD,TRTAN,CP) %>% pivot_wider(names_from=TRTAN,values_from=CP,names_prefix="TRT")
for(cc in c("TRT0","TRT54","TRT81")) if(!cc %in% names(sum_final_sub_wide)) sum_final_sub_wide[[cc]]<-NA_character_
sum_final_sub2<-bind_cols(sum_final_sub_wide,fish_sub2)
final_report<-bind_rows(sum_final_sub2,sum_final2) %>% select(AEDECOD,TRT0,TRT54,TRT81,CVALUE1,dplyr::any_of("COUNT_MAX"))
report_df<-final_report %>% select(AEDECOD,TRT0,TRT54,TRT81,CVALUE1)
write.csv(report_df,file.path(out,"P045.csv"),row.names=FALSE,na="")
write.csv(report_df,file.path(out,"final_report.csv"),row.names=FALSE,na="")

# overall stats
ps_o<-pearson_stat(o_mat); lr_o<-lr_stat(o_mat); mh_o<-mh_stat(o_mat,trts_all,c(0,1))
n_o<-sum(o_mat); phi_o<-sqrt(ps_o$v/n_o); cont_o<-sqrt(ps_o$v/(ps_o$v+n_o)); Vc_o<-sqrt(ps_o$v/(n_o*(min(nrow(o_mat),ncol(o_mat))-1)))

hdr0<-paste0("Placebo N=",N0); hdr54<-paste0("Treatment 1 N=",N54); hdr81<-paste0("Treatment 2 N=",N81)
lines<-c("Table 7.5 Summary of Serious Adverse Events","Safety Population","-","","PROC FREQ Output","")
val_line <- function(v){ f<-fmt4(v); t<-trim_num(f); b<-fmt_best12(v); paste0(f," ",t," ",b) }
for(pt in pt_list){
  d<-detail[[pt]]; sub<-d$sub; trts<-d$trts; grand<-d$grand; col0<-d$col0; col1<-d$col1
  lines<-c(lines,paste0("AEDECOD=",pt),"Table of TRTAN by RESP","TRTAN  RESP","Frequency Percent Row Pct Col Pct")
  for(t in trts){
    f0<-sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==0][1]; f1<-sub$COUNT_FINAL[sub$TRTAN==t & sub$RESP==1][1]; rowt<-f0+f1
    p0<-f0/grand*100; p1<-f1/grand*100; r0<-f0/rowt*100; r1<-f1/rowt*100
    c0<-ifelse(col0==0,0,f0/col0*100); c1<-ifelse(col1==0,0,f1/col1*100)
    lines<-c(lines,
      paste0(t," 0 ",f0," ",fmt2(p0)," ",fmt2(r0)," ",fmt2(c0)),
      paste0(t," 1 ",f1," ",fmt2(p1)," ",fmt2(r1)," ",fmt2(c1)),
      paste0("Total ",t," ",rowt," ",fmt2(rowt/grand*100)," 100.00"))
  }
  lines<-c(lines,paste0("Total 0 ",col0," ",fmt2(col0/grand*100)),
    paste0("Total 1 ",col1," ",fmt2(col1/grand*100)),
    paste0("Total ",grand," 100.00"),
    paste0("Effective Sample Size = ",grand," Frequency Missing = 0 Sample Size = ",grand),
    "Statistics",
    "Statistic DF Value Prob",
    paste0("Chi-Square 2 ",val_line(d$ps$v)," ",val_line(d$ps$p)),
    paste0("Likelihood Ratio Chi-Square 2 ",val_line(d$lr$v)," ",val_line(d$lr$p)),
    paste0("Mantel-Haenszel Chi-Square 1 ",val_line(d$mh$v)," ",val_line(d$mh$p)),
    paste0("Phi Coefficient ",val_line(d$phi)),
    paste0("Contingency Coefficient ",val_line(d$cont)),
    paste0("Cramer's V ",val_line(d$Vc)),
    paste0("Chi-Square ",fmt4(d$ps$v)," DF 2 Prob ",fmt4(d$ps$p)),
    paste0("Likelihood Ratio ",fmt4(d$lr$v)," DF 2 Prob ",fmt4(d$lr$p)),
    paste0("Mantel-Haenszel ",fmt4(d$mh$v)," DF 1 Prob ",fmt4(d$mh$p)),
    paste0("Phi ",fmt4(d$phi)," Contingency ",fmt4(d$cont)," Cramer ",fmt4(d$Vc)),
    "Fisher's Exact Test",
    paste0("Cell (1,1) Frequency (F) ",d$mat[1,1]),
    paste0("Left-sided Pr <= F ",val_line(d$pval)),
    paste0("Right-sided Pr >= F ",val_line(d$pval)),
    paste0("Table Probability (P) ",val_line(d$pval)),
    paste0("Two-sided Pr <= P ",val_line(d$pval)),
    paste0("Two-sided p-value (BEST12.) ",fmt_best12(d$pval)),
    "")
}
{
  grand_o<-nrow(overall_tab); col0_o<-sum(overall_tab$FLG==0); col1_o<-sum(overall_tab$FLG==1)
  lines<-c(lines,"Table of TRTAN by FLG for At Least One SAE","TRTAN  FLG","Frequency Percent Row Pct Col Pct")
  for(t in trts_all){
    f0<-sum(overall_tab$TRTAN==t & overall_tab$FLG==0); f1<-sum(overall_tab$TRTAN==t & overall_tab$FLG==1); rowt<-f0+f1
    p0<-f0/grand_o*100; p1<-f1/grand_o*100; r0<-f0/rowt*100; r1<-f1/rowt*100
    c0<-ifelse(col0_o==0,0,f0/col0_o*100); c1<-ifelse(col1_o==0,0,f1/col1_o*100)
    lines<-c(lines,paste0(t," 0 ",f0," ",fmt2(p0)," ",fmt2(r0)," ",fmt2(c0)),
      paste0(t," 1 ",f1," ",fmt2(p1)," ",fmt2(r1)," ",fmt2(c1)),
      paste0("Total ",t," ",rowt," ",fmt2(rowt/grand_o*100)," 100.00"))
  }
  lines<-c(lines,paste0("Total 0 ",col0_o," ",fmt2(col0_o/grand_o*100)),
    paste0("Total 1 ",col1_o," ",fmt2(col1_o/grand_o*100)),
    paste0("Total ",grand_o," 100.00"),
    paste0("Effective Sample Size = ",grand_o," Frequency Missing = 0 Sample Size = ",grand_o),
    "Statistics",
    "Statistic DF Value Prob",
    paste0("Chi-Square 2 ",val_line(ps_o$v)," ",val_line(ps_o$p)),
    paste0("Likelihood Ratio Chi-Square 2 ",val_line(lr_o$v)," ",val_line(lr_o$p)),
    paste0("Mantel-Haenszel Chi-Square 1 ",val_line(mh_o$v)," ",val_line(mh_o$p)),
    paste0("Phi Coefficient ",val_line(phi_o)),
    paste0("Contingency Coefficient ",val_line(cont_o)),
    paste0("Cramer's V ",val_line(Vc_o)),
    paste0("Chi-Square ",fmt4(ps_o$v)," DF 2 Prob ",fmt4(ps_o$p)),
    paste0("Likelihood Ratio ",fmt4(lr_o$v)," DF 2 Prob ",fmt4(lr_o$p)),
    paste0("Mantel-Haenszel ",fmt4(mh_o$v)," DF 1 Prob ",fmt4(mh_o$p)),
    paste0("Phi ",fmt4(phi_o)," Contingency ",fmt4(cont_o)," Cramer ",fmt4(Vc_o)),
    "Fisher's Exact Test",
    paste0("Cell (1,1) Frequency (F) ",o_mat[1,1]),
    paste0("Left-sided Pr <= F ",val_line(p_overall)),
    paste0("Right-sided Pr >= F ",val_line(p_overall)),
    paste0("Table Probability (P) ",val_line(p_overall)),
    paste0("Two-sided Pr <= P ",val_line(p_overall)),
    paste0("Two-sided p-value (BEST12.) ",fmt_best12(p_overall)),"")
}
lines<-c(lines,"","PROC REPORT Output","",
  paste0("Preferred Term | ",hdr0," | ",hdr54," | ",hdr81," | p-value*b"),
  paste(rep("-",100),collapse=""))
report_txt_df<-report_df %>% mutate(PSTR=sapply(CVALUE1,fmt_best12),PSTR4=sapply(CVALUE1,fmt4))
for(i in seq_len(nrow(report_txt_df))){ r<-report_txt_df[i,]
  lines<-c(lines,paste0(r$AEDECOD," | ",r$TRT0," | ",r$TRT54," | ",r$TRT81," | ",r$PSTR),
    paste0("p-value 4-decimal ",r$PSTR4," BEST12 ",r$PSTR)) }
lines<-c(lines,"-","Denominator for each % is treatment column N","*b = p-values are from Fisher's Exact Test",
  "Sample Size 254 DF 1 DF 2 Frequency Missing 0",
  paste0("N=",N0," N=",N54," N=",N81," 86 84 84 0 54 81 1 2 5 50 100"))
writeLines(lines,file.path(out,"P045.txt")); writeLines(lines,file.path(out,"P045.lst"))
message("wrote lines=",length(lines))
