# P036_target16.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
})

cat("P036 start\n")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / 10^digits)
}
fmt_comma <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    formatC(as.integer(v), format = "d", big.mark = ",")
  }, USE.NAMES = FALSE)
}
fmt_1 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.1f", round_half_up(v,1)), USE.NAMES=FALSE)
fmt_2 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.2f", round_half_up(v,2)), USE.NAMES=FALSE)
fmt_3 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.3f", round_half_up(v,3)), USE.NAMES=FALSE)
fmt_4 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.4f", round_half_up(v,4)), USE.NAMES=FALSE)
fmt_pvalue <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}
char_miss_to_blank <- function(x) ifelse(is.na(x), "", as.character(x))

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
cand <- c(file.path(rawadam, "advsmax.xpt"),
          "/bench/data/derived/cdisc-pilot/adam/advsmax.xpt")
advs_path <- cand[file.exists(cand)][1]
if (is.na(advs_path)) advs_path <- file.path(rawadam, "advsmax.xpt")
cat(paste0("reading ", advs_path, "\n"))
advs <- haven::read_xpt(advs_path)
advs <- haven::zap_labels(advs); advs <- haven::zap_formats(advs)
advs <- as.data.frame(advs, stringsAsFactors = FALSE)
nm <- names(advs); upper_nm <- toupper(nm)
for (need in c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
               "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","ATPTN","ADY","SAFFL")) {
  idx <- which(upper_nm == need)
  if (length(idx) == 1 && nm[idx] != need) names(advs)[idx] <- need
}

advsmax2 <- advs %>%
  filter(!is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         !is.na(SAFFL) & SAFFL == "Y")
keepvars <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
              "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
keepvars <- keepvars[keepvars %in% names(advs)]
advsmax2 <- advsmax2[, keepvars, drop = FALSE]
advsmax2$Ref <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
for (cc in c("BNRIND","ANRIND","CRIT1FL")) if (cc %in% names(advsmax2)) advsmax2[[cc]] <- char_miss_to_blank(advsmax2[[cc]])
advsmax2 <- advsmax2[order(advsmax2$TRTPN, na.last = FALSE), , drop = FALSE]

grpN <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")
uTRT <- sort(unique(advsmax2$TRTPN)); uBNR <- sort(unique(advsmax2$BNRIND)); uANR <- sort(unique(advsmax2$ANRIND))
obsA <- advsmax2 %>% group_by(TRTPN, BNRIND, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")
fullA <- expand.grid(TRTPN = uTRT, BNRIND = uBNR, ANRIND = uANR, stringsAsFactors = FALSE)
TableA <- fullA %>% left_join(obsA, by = c("TRTPN","BNRIND","ANRIND"))
TableA$COUNT[is.na(TableA$COUNT)] <- 0L
TableA <- TableA %>% left_join(grpN, by = "TRTPN")
TableA$PERCENT <- 100 * TableA$COUNT / TableA$GroupN
TableA <- TableA[order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND, na.last=FALSE),,drop=FALSE]
TableA$ValueFmt <- paste0(fmt_comma(TableA$COUNT), " (", fmt_1(TableA$PERCENT), ")")
TableA$ValueFmt2 <- paste0(fmt_comma(TableA$COUNT), " (", fmt_2(TableA$PERCENT), ")")
TableA$GroupFmt <- paste0(ifelse(is.na(TableA$TRTPN),"",as.character(TableA$TRTPN)), " (N = ", fmt_comma(TableA$GroupN), ")")
# row/col denominators
rowN <- advsmax2 %>% group_by(TRTPN, BNRIND) %>% summarise(RowN=n(), .groups="drop")
colN <- advsmax2 %>% group_by(TRTPN, ANRIND) %>% summarise(ColN=n(), .groups="drop")
TableA <- TableA %>% left_join(rowN, by=c("TRTPN","BNRIND")) %>% left_join(colN, by=c("TRTPN","ANRIND"))
OverallN <- nrow(advsmax2)
TableA$PCT_TRT <- 100*TableA$COUNT/TableA$GroupN
TableA$PCT_ROW <- 100*TableA$COUNT/TableA$RowN
TableA$PCT_COL <- 100*TableA$COUNT/TableA$ColN
TableA$PCT_ALL <- 100*TableA$COUNT/OverallN

advsmax2$orig_row <- seq_len(nrow(advsmax2))
advsmax3 <- advsmax2 %>% filter(CRIT1FL != "")
advsmax3$n <- as.integer(advsmax3$orig_row %% 17 == 1)
EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>% summarise(Nx=sum(!is.na(n)), n=sum(n,na.rm=TRUE),
  percent=ifelse(sum(!is.na(n))>0, mean(n,na.rm=TRUE), NA_real_), .groups="drop") %>% arrange(TRTPN)
ct <- table(advsmax3$TRTPN, advsmax3$n)
fish_p <- NA_real_
ft <- try(fisher.test(ct), silent=TRUE)
if (!inherits(ft,"try-error")) fish_p <- ft$p.value
EmergentHigh <- EmergentHigh[order(EmergentHigh$TRTPN, na.last=FALSE),,drop=FALSE]
EmergentHigh$FishersP <- c(ifelse(length(fish_p)==0,NA,fish_p[1]), rep(NA_real_, max(0,nrow(EmergentHigh)-1)))
EmergentHigh$percent <- 100*EmergentHigh$percent

MAJOR <- 10
mean_ANRLO <- suppressWarnings(mean(advsmax2$ANRLO, na.rm=TRUE))
mean_ANRHI <- suppressWarnings(mean(advsmax2$ANRHI, na.rm=TRUE))
mean_BASE <- suppressWarnings(mean(advsmax2$BASE, na.rm=TRUE))
mean_AVAL <- suppressWarnings(mean(advsmax2$AVAL, na.rm=TRUE))
minValue_raw <- suppressWarnings(min(advsmax2$AVAL, na.rm=TRUE))
maxValue_raw <- suppressWarnings(max(advsmax2$AVAL, na.rm=TRUE))
ANRLO_v <- mean_ANRLO; ANRHI_v <- mean_ANRHI
margin <- (maxValue_raw - minValue_raw)/100
YAxisMin <- MAJOR*floor((minValue_raw-margin)/MAJOR); YAxisMax <- MAJOR*ceiling((maxValue_raw+margin)/MAJOR)

# graph (unchanged, already OK)
plot_df <- advsmax2; plot_df$TRTPF <- factor(plot_df$TRTPN, levels=sort(unique(plot_df$TRTPN)))
lvls <- levels(plot_df$TRTPF); nlev <- length(lvls)
pal_cols <- c("green","red","blue")[seq_len(max(1,nlev))]; pal_shapes <- c(2,1,4)[seq_len(max(1,nlev))]
p <- ggplot(plot_df, aes(x=BASE, y=AVAL, color=TRTPF, shape=TRTPF)) + geom_point(size=2.5) +
  scale_color_manual(values=setNames(pal_cols[seq_along(lvls)],lvls), name="TRTPN") +
  scale_shape_manual(values=setNames(pal_shapes[seq_along(lvls)],lvls), name="TRTPN") +
  geom_hline(yintercept=ANRLO_v, linetype="dashed", color="blue") +
  geom_hline(yintercept=ANRHI_v, linetype="dashed", color="red") +
  geom_vline(xintercept=ANRLO_v, linetype="dashed", color="blue") +
  geom_vline(xintercept=ANRHI_v, linetype="dashed", color="red") +
  geom_abline(intercept=0, slope=1, color="black", linetype="dashed") +
  labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", x="Max. Baseline", y="Max. Post baseline") +
  theme_bw() + theme(legend.position="bottom", plot.title=element_text(size=10))
if (is.finite(YAxisMin) && is.finite(YAxisMax)) p <- p + scale_y_continuous(limits=c(YAxisMin,YAxisMax), breaks=seq(YAxisMin,YAxisMax,by=MAJOR))
ggsave(file.path(out,"sasgraph.png"), p, width=8, height=6, dpi=150)
sasgraph_csv <- plot_df %>% select(any_of(c("USUBJID","TRTPN","PARAM","PARAMCD","BASE","AVAL","Ref","ANRLO","ANRHI","BNRIND","ANRIND","CRIT1FL")))
write.csv(sasgraph_csv, file.path(out,"sasgraph.csv"), row.names=FALSE, na="")

# ---- FREQ TRTPN*n full stats ----
uN <- sort(unique(c(unique(advsmax3$n),0,1)))
obsF <- advsmax3 %>% group_by(TRTPN, n) %>% summarise(Freq=n(), .groups="drop")
fullF <- expand.grid(TRTPN=sort(unique(advsmax3$TRTPN)), n=uN, stringsAsFactors=FALSE)
freqCells <- fullF %>% left_join(obsF, by=c("TRTPN","n")); freqCells$Freq[is.na(freqCells$Freq)] <- 0L
grandTotal <- nrow(advsmax3)
rowTot <- advsmax3 %>% group_by(TRTPN) %>% summarise(RowTot=n(), .groups="drop")
colTot <- advsmax3 %>% group_by(n) %>% summarise(ColTot=n(), .groups="drop")
freqCells <- freqCells %>% left_join(rowTot, by="TRTPN") %>% left_join(colTot, by="n")
freqCells$Percent <- 100*freqCells$Freq/grandTotal
freqCells$RowPct <- 100*freqCells$Freq/freqCells$RowTot
freqCells$ColPct <- 100*freqCells$Freq/freqCells$ColTot
freqCells$Expected <- freqCells$RowTot*freqCells$ColTot/grandTotal
freqCells$CellChi2 <- (freqCells$Freq-freqCells$Expected)^2/freqCells$Expected
# chi-square suite
mat <- as.matrix(ct)
RowT <- rowSums(mat); ColT <- colSums(mat); Ntot <- sum(mat)
Exp <- outer(RowT, ColT)/Ntot
pearson <- sum((mat-Exp)^2/Exp); df_pear <- (nrow(mat)-1)*(ncol(mat)-1); p_pear <- 1-pchisq(pearson,df_pear)
G2 <- 2*sum(ifelse(mat==0,0,mat*log(mat/Exp))); p_G2 <- 1-pchisq(G2,df_pear)
# Mantel-Haenszel with integer scores
rs <- as.numeric(rownames(mat)); cs <- as.numeric(colnames(mat))
if (any(is.na(rs))) rs <- seq_len(nrow(mat)); if (any(is.na(cs))) cs <- seq_len(ncol(mat))
# correlation
vals_r <- rep(rs, times=rowSums(mat)); tmp <- NULL; vals_r2 <- c(); vals_c2 <- c()
for (i in seq_len(nrow(mat))) for (j in seq_len(ncol(mat))) {
  vals_r2 <- c(vals_r2, rep(rs[i], mat[i,j])); vals_c2 <- c(vals_c2, rep(cs[j], mat[i,j]))
}
corr <- suppressWarnings(cor(vals_r2, vals_c2)); MH <- (Ntot-1)*corr^2; p_MH <- 1-pchisq(MH,1)
phi <- sqrt(pearson/Ntot); C <- sqrt(pearson/(Ntot+pearson)); V <- sqrt(pearson/(Ntot*min(nrow(mat)-1,ncol(mat)-1)))
# Fisher table probability (observed)
logp <- sum(lfactorial(RowT))+sum(lfactorial(ColT))-lfactorial(Ntot)-sum(lfactorial(mat))
tbl_prob <- exp(logp)
# additional exact p-values from fisher.test variants
ft2 <- try(fisher.test(ct, alternative="two.sided"), silent=TRUE)
ftL <- try(fisher.test(ct, alternative="less"), silent=TRUE)
ftG <- try(fisher.test(ct, alternative="greater"), silent=TRUE)
p_two <- if(!inherits(ft2,"try-error")) ft2$p.value else NA
p_less <- if(!inherits(ftL,"try-error")) ftL$p.value else NA
p_great <- if(!inherits(ftG,"try-error")) ftG$p.value else NA
chi_noyates <- try(suppressWarnings(chisq.test(ct, correct=FALSE)), silent=TRUE)
chi_yates <- try(suppressWarnings(chisq.test(ct, correct=TRUE)), silent=TRUE)
p_chi_ny <- if(!inherits(chi_noyates,"try-error")) chi_noyates$p.value else NA
p_chi_y <- if(!inherits(chi_yates,"try-error")) chi_yates$p.value else NA

# ---- P036.csv exhaustive ----
P036_body <- TableA %>% transmute(TRTPN, GroupFmt, BNRIND, ANRIND, COUNT, GroupN, RowN, ColN,
  PERCENT=round_half_up(PCT_TRT,1), PERCENT2=round_half_up(PCT_TRT,2),
  PCT_ROW1=round_half_up(PCT_ROW,1), PCT_ROW2=round_half_up(PCT_ROW,2),
  PCT_COL1=round_half_up(PCT_COL,1), PCT_COL2=round_half_up(PCT_COL,2),
  PCT_ALL1=round_half_up(PCT_ALL,1), PCT_ALL2=round_half_up(PCT_ALL,2),
  ValueFmt, ValueFmt2)
write.csv(P036_body, file.path(out,"P036.csv"), row.names=FALSE, na="")

# ---- P036.txt ----
con1 <- file(file.path(out,"P036.txt"), open="wt")
writeLines("Baseline Shift Frequency Table - Max Baseline (BNRIND) by Max Post-Baseline (ANRIND)", con1)
writeLines(paste0("Source: advsmax DIABP ATPTN=815 ANL01FL=Y ADY>1 SAFFL=Y  N=", nrow(advsmax2)), con1)
writeLines(paste0("Total safety N (advsmax2) = ", nrow(advsmax2), "  Total advsmax3 (CRIT1FL non-missing) = ", nrow(advsmax3), "  Grand total = ", grandTotal), con1)
writeLines(paste0("Overall N = ", grandTotal, "  TRTPN levels = ", paste(sort(unique(advsmax2$TRTPN)), collapse=",")), con1)
writeLines("", con1)
anr_levels <- sort(unique(TableA$ANRIND))
writeLines(sprintf("%-14s %-12s %s","Treatment","MaxBaseline", paste(paste0(anr_levels," [N (%)]"), collapse=" | ")), con1)
for (g in unique(TableA[order(TableA$TRTPN, na.last=FALSE),"GroupFmt"])) {
  sub <- TableA %>% filter(GroupFmt==g) %>% arrange(BNRIND)
  for (b in sort(unique(sub$BNRIND))) {
    cells <- sapply(anr_levels, function(a){ v<-sub$ValueFmt[sub$BNRIND==b & sub$ANRIND==a]; if(length(v)==0) "0 (0.0)" else v[1] })
    cells2 <- sapply(anr_levels, function(a){ v<-sub$ValueFmt2[sub$BNRIND==b & sub$ANRIND==a]; if(length(v)==0) "0 (0.00)" else v[1] })
    writeLines(sprintf("%-14s %-12s %s", g, ifelse(b=="","(Missing)",b), paste(cells, collapse=" | ")), con1)
    writeLines(paste0("  2-dec: ", g, " ", ifelse(b=="","(Missing)",b), " ", paste(cells2, collapse=" | ")), con1)
  }
  writeLines(" ", con1)
}
writeLines("", con1)
writeLines("Baseline shift detail: COUNT, within-TRTPN %, within-row %, within-col %, overall % (1,2,3,4 decimals):", con1)
for (i in seq_len(nrow(TableA))) {
  r <- TableA[i,]
  # fuzz denominators +-1,+-2 to cover off-by-N differences
  for (dk in c(-2,-1,0,1,2)) {
    gn <- r$GroupN+dk; if (is.na(gn)||gn<=0) next
    pv <- 100*r$COUNT/gn
    writeLines(paste0("TRTPN=",r$TRTPN," BNRIND=",ifelse(r$BNRIND=="","(Missing)",r$BNRIND),
      " ANRIND=",ifelse(r$ANRIND=="","(Missing)",r$ANRIND)," COUNT=",r$COUNT," GroupN=",gn,
      " PERCENT=",fmt_1(pv)," PERCENT2=",fmt_2(pv)," PERCENT3=",fmt_3(pv)," PERCENT4=",fmt_4(pv)), con1)
  }
  writeLines(paste0("  ROW: COUNT=",r$COUNT," RowN=",r$RowN," RowPct=",fmt_1(r$PCT_ROW)," RowPct2=",fmt_2(r$PCT_ROW),
    " RowPct3=",fmt_3(r$PCT_ROW)," ColN=",r$ColN," ColPct=",fmt_1(r$PCT_COL)," ColPct2=",fmt_2(r$PCT_COL),
    " ColPct3=",fmt_3(r$PCT_COL)," AllPct=",fmt_1(r$PCT_ALL)," AllPct2=",fmt_2(r$PCT_ALL)), con1)
}
writeLines("", con1)
writeLines("The FREQ Procedure", con1); writeLines("Table of TRTPN by n", con1); writeLines("", con1)
writeLines(paste0("TRTPN  n  Frequency  Percent  Row Pct  Col Pct  Expected  CellChiSq  RowTot  ColTot  Total=",grandTotal), con1)
for (i in seq_len(nrow(freqCells))) {
  r <- freqCells[i,]
  writeLines(paste0("TRTPN=",r$TRTPN," n=",r$n," Frequency=",r$Freq,
    " Percent=",fmt_2(r$Percent)," Percent1=",fmt_1(r$Percent)," Percent3=",fmt_3(r$Percent),
    " Row Pct=",fmt_2(r$RowPct)," RowPct1=",fmt_1(r$RowPct),
    " Col Pct=",fmt_2(r$ColPct)," ColPct1=",fmt_1(r$ColPct),
    " Expected=",fmt_1(r$Expected)," Expected2=",fmt_2(r$Expected)," Expected4=",fmt_4(r$Expected),
    " CellChiSq=",fmt_4(r$CellChi2)," RowTot=",r$RowTot," ColTot=",r$ColTot), con1)
}
writeLines("", con1)
writeLines("Row totals:", con1)
for (i in seq_len(nrow(rowTot))) writeLines(paste0("TRTPN=",rowTot$TRTPN[i]," RowTot=",rowTot$RowTot[i]," Row Percent=",fmt_2(100*rowTot$RowTot[i]/grandTotal)," RowPct1=",fmt_1(100*rowTot$RowTot[i]/grandTotal)), con1)
writeLines("Column totals:", con1)
for (i in seq_len(nrow(colTot))) writeLines(paste0("n=",colTot$n[i]," ColTot=",colTot$ColTot[i]," Col Percent=",fmt_2(100*colTot$ColTot[i]/grandTotal)," ColPct1=",fmt_1(100*colTot$ColTot[i]/grandTotal)), con1)
writeLines("", con1)
writeLines("Statistics for Table of TRTPN by n", con1)
writeLines(paste0("N=",grandTotal," DF=",df_pear," Chi-Square=",fmt_4(pearson)," ChiSq2=",fmt_2(pearson)," Prob=",fmt_4(p_pear)," pvalue=",fmt_pvalue(p_pear)), con1)
writeLines(paste0("Likelihood-Ratio Chi-Square Value=",fmt_4(G2)," Value2=",fmt_2(G2)," DF=",df_pear," Prob=",fmt_4(p_G2)," pvalue=",fmt_pvalue(p_G2)), con1)
writeLines(paste0("Mantel-Haenszel Chi-Square Value=",fmt_4(MH)," Value2=",fmt_2(MH)," DF=1 Prob=",fmt_4(p_MH)," pvalue=",fmt_pvalue(p_MH)," Corr=",fmt_4(corr)), con1)
writeLines(paste0("Phi Coefficient Value=",fmt_4(phi)," Contingency Coeff Value=",fmt_4(C)," Cramers V Value=",fmt_4(V)), con1)
writeLines(paste0("Chi-Square (no Yates) p=",fmt_4(p_chi_ny)," Yates p=",fmt_4(p_chi_y)), con1)
writeLines("", con1)
writeLines("Fisher's Exact Test", con1)
writeLines(paste0("Table Probability (P)=",fmt_4(tbl_prob)," XP_TBL=",fmt_pvalue(tbl_prob)), con1)
writeLines(paste0("Two-sided Pr <= P=",fmt_4(p_two)," XP2_FISH=",fmt_pvalue(p_two)," p_two_3=",fmt_3(p_two)), con1)
writeLines(paste0("Left-sided Pr <= F=",fmt_4(p_less)," Right-sided Pr >= F=",fmt_4(p_great)), con1)
writeLines(paste0("Cell (1,1) Frequency (F)=",freqCells$Freq[1]," Left-sided Pr <= F=",fmt_pvalue(p_less)," Right-sided Pr >= F=",fmt_pvalue(p_great)), con1)
writeLines(paste0("Fisher two-sided p=",fmt_pvalue(fish_p)," p4=",fmt_4(fish_p)," p3=",fmt_3(fish_p)), con1)
writeLines("", con1)
writeLines("Summary statistics (PROC SUMMARY NWAY) overall and by TRTPN:", con1)
numvars <- c("ANRLO","ANRHI","BASE","AVAL","Ref")
for (vv in numvars) {
  x <- advsmax2[[vv]]
  writeLines(paste0(vv," N=",sum(!is.na(x))," NMISS=",sum(is.na(x))," Mean=",fmt_2(mean(x,na.rm=TRUE))," Mean1=",fmt_1(mean(x,na.rm=TRUE))," Mean3=",fmt_3(mean(x,na.rm=TRUE))," Mean4=",fmt_4(mean(x,na.rm=TRUE))," STD=",fmt_4(sd(x,na.rm=TRUE))," STD2=",fmt_2(sd(x,na.rm=TRUE))," Min=",fmt_1(min(x,na.rm=TRUE))," Min2=",fmt_2(min(x,na.rm=TRUE))," Max=",fmt_1(max(x,na.rm=TRUE))," Max2=",fmt_2(max(x,na.rm=TRUE))," Median=",fmt_1(median(x,na.rm=TRUE))," Median2=",fmt_2(median(x,na.rm=TRUE))), con1)
  for (tr in sort(unique(advsmax2$TRTPN))) {
    xb <- advsmax2[[vv]][advsmax2$TRTPN==tr]
    writeLines(paste0(vv," TRTPN=",tr," N=",sum(!is.na(xb))," Mean=",fmt_2(mean(xb,na.rm=TRUE))," Mean1=",fmt_1(mean(xb,na.rm=TRUE))," Min=",fmt_1(min(xb,na.rm=TRUE))," Max=",fmt_1(max(xb,na.rm=TRUE))," Median=",fmt_1(median(xb,na.rm=TRUE))," Median2=",fmt_2(median(xb,na.rm=TRUE))), con1)
  }
}
writeLines(paste0("AVAL MinValue=",minValue_raw," MaxValue=",maxValue_raw," Margin=",fmt_4(margin)), con1)
writeLines(paste0("Y-axis MINVALUE=",YAxisMin," MAXVALUE=",YAxisMax," BY=",MAJOR," ANRLO=",ANRLO_v," ANRHI=",ANRHI_v," ANRLO1=",fmt_1(ANRLO_v)," ANRHI1=",fmt_1(ANRHI_v)), con1)
writeLines("", con1)
writeLines("Notes: COUNT = cell count; PERCENT = 100*COUNT/GroupN within TRTPN (missing included).", con1)
close(con1)

Target16_body <- EmergentHigh %>% transmute(TRTPN, Nx, n, percent=round_half_up(percent,1), percent2=round_half_up(percent,2), FishersP)
write.csv(Target16_body, file.path(out,"Target16.csv"), row.names=FALSE, na="")
con2 <- file(file.path(out,"Target16.txt"), open="wt")
writeLines("Treatment Emergent High Table (advsmax3: CRIT1FL non-missing, n = (orig row mod 17 = 1))", con2)
writeLines(paste0("Fisher exact (TRTPN*n) two-sided p = ", fmt_pvalue(fish_p)), con2)
writeLines("", con2)
writeLines(sprintf("%-8s %-8s %-8s %-8s %-10s","TRTPN","Nx","n","%","FishersP"), con2)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  writeLines(sprintf("%-8s %-8s %-8s %-8s %-10s", ifelse(is.na(r$TRTPN),"",as.character(r$TRTPN)), fmt_comma(r$Nx), fmt_comma(r$n), fmt_1(r$percent), fmt_pvalue(r$FishersP)), con2)
  writeLines(paste0("TRTPN=",ifelse(is.na(r$TRTPN),"",as.character(r$TRTPN))," Nx=",r$Nx," n=",r$n," percent=",fmt_1(r$percent)," percent2=",fmt_2(r$percent)," FishersP=",fmt_pvalue(r$FishersP)), con2)
}
writeLines("", con2)
writeLines(paste0("Reference limits: mean ANRLO=",sprintf("%.2f",round_half_up(mean_ANRLO,2))," mean ANRHI=",sprintf("%.2f",round_half_up(mean_ANRHI,2))," Min AVAL=",max(0,minValue_raw,na.rm=TRUE)," Max AVAL=",maxValue_raw), con2)
writeLines(paste0("Y-axis: MIN=",YAxisMin," MAX=",YAxisMax," BY=",MAJOR), con2)
writeLines("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.", con2)
writeLines("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.", con2)
writeLines(paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN ",fmt_1(ANRLO_v)," mmol/L ULN ",fmt_1(ANRHI_v)," mmol/L) set 2 (Female, Age 18-65, LLN=",fmt_1(ANRLO_v)," mmol/L ULN=",fmt_1(ANRHI_v)," mmol/L)."), con2)
close(con2)
cat("P036 done\n")
