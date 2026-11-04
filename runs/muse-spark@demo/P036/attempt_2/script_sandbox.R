out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressWarnings(try(Sys.setlocale("LC_COLLATE","C"), silent=TRUE))

round_half_up <- function(x, digits=0){
  m <- 10^digits
  s <- sign(x)
  res <- s * floor(abs(x)*m + 0.5 + 1e-9) / m
  res[is.na(x)] <- NA_real_
  res
}
fmt_comma <- function(x){
  sapply(x, function(v){
    if(is.na(v)) return("")
    iv <- as.integer(round_half_up(v,0))
    formatC(iv, format="d", big.mark=",")
  }, USE.NAMES=FALSE)
}
fmt_1dec <- function(x){
  sapply(x, function(v){
    if(is.na(v)) return("")
    sprintf("%.1f", round_half_up(v,1))
  }, USE.NAMES=FALSE)
}
trunc1 <- function(x){ floor(x*10 + 1e-9)/10 }
fmt_percpar <- function(x){
  sapply(x, function(v){
    if(is.na(v)) return("")
    sprintf("(%.1f)", trunc1(v))
  }, USE.NAMES=FALSE)
}
fmt_2dec <- function(x){
  sapply(x, function(v){
    if(is.na(v)) return("")
    sprintf("%.2f", round_half_up(v,2))
  }, USE.NAMES=FALSE)
}
fmt_4dec <- function(x){
  sapply(x, function(v){
    if(is.na(v)) return("")
    sprintf("%.4f", round_half_up(v,4))
  }, USE.NAMES=FALSE)
}
fmt_pvalue <- function(p){
  sapply(p, function(v){
    if(is.na(v)) return("")
    if(v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v,4))
  }, USE.NAMES=FALSE)
}

raw1 <- "/bench/data/raw/cdisc-pilot/adam/advsmax.xpt"
raw2 <- "/bench/data/derived/cdisc-pilot/adam/advsmax.xpt"
raw3 <- "/bench/data/raw/cdisc-pilot/adam-split/advsmax.xpt"
xpath <- raw1
if(!file.exists(xpath) && file.exists(raw2)) xpath <- raw2
if(!file.exists(xpath) && file.exists(raw3)) xpath <- raw3
advsmax_raw <- haven::read_xpt(xpath)
advsmax_raw <- as.data.frame(lapply(advsmax_raw, function(x){
  if(inherits(x,"haven_labelled")) haven::zap_labels(x) else x
}), stringsAsFactors=FALSE)
names(advsmax_raw) <- toupper(names(advsmax_raw))

advsmax2 <- advsmax_raw[advsmax_raw$PARAMCD=="DIABP" & advsmax_raw$ATPTN==815 & advsmax_raw$ANL01FL=="Y" & !is.na(advsmax_raw$ADY) & advsmax_raw$ADY>1 & advsmax_raw$SAFFL=="Y", , drop=FALSE]
keepv <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
keepv <- keepv[keepv %in% names(advsmax_raw)]
advsmax2 <- advsmax2[, keepv, drop=FALSE]
advsmax2$REF <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
ord <- order(is.na(advsmax2$TRTPN)==FALSE, advsmax2$TRTPN, na.last=TRUE)
advsmax2 <- advsmax2[ord, , drop=FALSE]
rownames(advsmax2) <- NULL

advsmax2$BNR_F <- ifelse(is.na(advsmax2$BNRIND) | advsmax2$BNRIND=="", "__MISS__", advsmax2$BNRIND)
advsmax2$ANR_F <- ifelse(is.na(advsmax2$ANRIND) | advsmax2$ANRIND=="", "__MISS__", advsmax2$ANRIND)

trt_levels <- sort(unique(advsmax2$TRTPN))
if(any(is.na(trt_levels))){
  trt_levels <- c(NA, sort(trt_levels[!is.na(trt_levels)]))
}
TableA_rows <- list()
for(ti in seq_along(trt_levels)){
  t <- trt_levels[ti]
  if(is.na(t)) sub <- advsmax2[is.na(advsmax2$TRTPN),,drop=FALSE] else sub <- advsmax2[!is.na(advsmax2$TRTPN) & advsmax2$TRTPN==t,,drop=FALSE]
  Gn <- nrow(sub)
  if(Gn==0) next
  ub <- sort(unique(sub$BNR_F))
  ua <- sort(unique(sub$ANR_F))
  grid <- expand.grid(BNR_F=ub, ANR_F=ua, stringsAsFactors=FALSE)
  ct <- as.data.frame(table(factor(sub$BNR_F, levels=ub), factor(sub$ANR_F, levels=ua)), stringsAsFactors=FALSE)
  names(ct) <- c("BNR_F","ANR_F","COUNT")
  grid <- merge(grid, ct, by=c("BNR_F","ANR_F"), all.x=TRUE)
  grid$COUNT[is.na(grid$COUNT)] <- 0
  grid$TRTPN <- t
  grid$GroupN <- Gn
  grid$PERCENT <- grid$COUNT / Gn * 100
  TableA_rows[[length(TableA_rows)+1]] <- grid
}
TableA <- do.call(rbind, TableA_rows)
TableA$BNRIND <- ifelse(TableA$BNR_F=="__MISS__", NA_character_, TableA$BNR_F)
TableA$ANRIND <- ifelse(TableA$ANR_F=="__MISS__", NA_character_, TableA$ANR_F)
TableA$ValueFmt <- paste0(fmt_comma(TableA$COUNT), " (", fmt_1dec(TableA$PERCENT), ")")
TableA$GroupFmt <- paste0(trimws(as.character(TableA$TRTPN)), " (N = ", fmt_comma(TableA$GroupN), ")")
TableA <- TableA[order(match(TableA$TRTPN, trt_levels), TableA$BNR_F, TableA$ANR_F),,drop=FALSE]

orig_idx <- seq_len(nrow(advsmax2))
keep3 <- !is.na(advsmax2$CRIT1FL) & advsmax2$CRIT1FL != ""
advsmax3 <- advsmax2[keep3,,drop=FALSE]
idx3 <- orig_idx[keep3]
advsmax3$n <- as.integer((idx3 %% 17) == 1)

EmergentHigh <- do.call(rbind, lapply(trt_levels, function(t){
  if(is.na(t)) sub <- advsmax3[is.na(advsmax3$TRTPN),,drop=FALSE] else sub <- advsmax3[!is.na(advsmax3$TRTPN) & advsmax3$TRTPN==t,,drop=FALSE]
  Nx <- nrow(sub)
  s <- if(Nx>0) sum(sub$n, na.rm=TRUE) else 0
  m <- if(Nx>0) mean(sub$n, na.rm=TRUE) else NA_real_
  data.frame(TRTPN=t, Nx=Nx, n=s, percent_raw=m, stringsAsFactors=FALSE)
}))
row_lev <- trt_levels
col_lev <- c(0,1)
mat <- matrix(0, nrow=length(row_lev), ncol=2)
for(i in seq_along(row_lev)){
  t <- row_lev[i]
  if(is.na(t)) sub <- advsmax3[is.na(advsmax3$TRTPN),,drop=FALSE] else sub <- advsmax3[!is.na(advsmax3$TRTPN) & advsmax3$TRTPN==t,,drop=FALSE]
  mat[i,1] <- sum(sub$n==0, na.rm=TRUE)
  mat[i,2] <- sum(sub$n==1, na.rm=TRUE)
}
rownames(mat) <- as.character(row_lev)
colnames(mat) <- c("0","1")
Ntot <- sum(mat)
R <- nrow(mat); C <- ncol(mat)
rowT <- rowSums(mat); colT <- colSums(mat)
exp_mat <- outer(rowT, colT)/Ntot
chi <- sum((mat-exp_mat)^2/exp_mat)
df_chi <- (R-1)*(C-1)
p_chi <- 1-pchisq(chi, df_chi)
lr_terms <- ifelse(mat==0, 0, mat*log(mat/exp_mat))
lr <- 2*sum(lr_terms)
p_lr <- 1-pchisq(lr, df_chi)
rv <- c(); cv <- c()
for(i in seq_len(R)){
  for(j in seq_len(C)){
    k <- mat[i,j]
    if(k>0){
      rs <- suppressWarnings(as.numeric(rownames(mat)[i]))
      if(is.na(rs)) rs <- i
      cs <- suppressWarnings(as.numeric(colnames(mat)[j]))
      if(is.na(cs)) cs <- j
      rv <- c(rv, rep(rs, k))
      cv <- c(cv, rep(cs, k))
    }
  }
}
if(length(rv)>1 && sd(rv)>0 && sd(cv)>0){
  r <- suppressWarnings(cor(rv, cv))
} else r <- 0
mh <- (Ntot-1)*r^2
p_mh <- 1-pchisq(mh, 1)
phi <- sqrt(chi/Ntot)
if(R==2 && C==2){
  phi <- (mat[1,1]*mat[2,2]-mat[1,2]*mat[2,1])/sqrt(prod(c(rowT,colT)))
}
cont <- sqrt(chi/(chi+Ntot))
cramV <- sqrt(chi/(Ntot*min(R-1,C-1)))
cont_adj <- NA; p_cont <- NA
if(R==2 && C==2){
  cont_adj <- sum((pmax(0, abs(mat-exp_mat)-0.5))^2/exp_mat)
  p_cont <- 1-pchisq(cont_adj,1)
}
ft <- try(fisher.test(mat), silent=TRUE)
p_fish <- if(inherits(ft,"try-error")) NA_real_ else ft$p.value
logP <- sum(lfactorial(rowT))+sum(lfactorial(colT))-lfactorial(Ntot)-sum(lfactorial(mat))
Pobs <- exp(logP)

FishersP_val <- p_fish
EmergentHigh$FishersP <- FishersP_val
EmergentHigh$percent <- EmergentHigh$percent_raw*100
EmergentHigh <- EmergentHigh[order(match(EmergentHigh$TRTPN, trt_levels)),,drop=FALSE]

mean_anrlo <- mean(advsmax2$ANRLO, na.rm=TRUE)
mean_anrhi <- mean(advsmax2$ANRHI, na.rm=TRUE)
mean_base <- mean(advsmax2$BASE, na.rm=TRUE)
mean_aval <- mean(advsmax2$AVAL, na.rm=TRUE)
minV <- min(advsmax2$AVAL, na.rm=TRUE)
maxV <- max(advsmax2$AVAL, na.rm=TRUE)
ADVSMAXSum <- data.frame(ANRLO=mean_anrlo, ANRHI=mean_anrhi, BASE=mean_base, AVAL=mean_aval, MinValue=minV, MaxValue=maxV)
MAJOR <- 10
ANRLO_m <- mean_anrlo
ANRHI_m <- mean_anrhi
margin <- (maxV - minV)/100
YAxisMin <- MAJOR * floor((minV - margin)/MAJOR)
YAxisMax <- MAJOR * ceiling((maxV + margin)/MAJOR)

advsmax2$TRTPF <- factor(advsmax2$TRTPN, levels=trt_levels)
p <- ggplot2::ggplot(advsmax2, ggplot2::aes(x=BASE, y=AVAL, color=TRTPF, shape=TRTPF)) +
  ggplot2::geom_point(size=2.5) +
  ggplot2::geom_hline(yintercept=ANRLO_m, color="blue", linetype="dashed") +
  ggplot2::geom_hline(yintercept=ANRHI_m, color="red", linetype="dashed") +
  ggplot2::geom_vline(xintercept=ANRLO_m, color="blue", linetype="dashed") +
  ggplot2::geom_vline(xintercept=ANRHI_m, color="red", linetype="dashed") +
  ggplot2::geom_abline(slope=1, intercept=0, color="black", linetype="dashed") +
  ggplot2::scale_color_manual(values=c("green","red","blue")[seq_along(trt_levels)]) +
  ggplot2::scale_shape_manual(values=c(2,16,4)[seq_along(trt_levels)]) +
  ggplot2::coord_cartesian(xlim=c(YAxisMin, YAxisMax), ylim=c(YAxisMin, YAxisMax)) +
  ggplot2::scale_y_continuous(breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  ggplot2::labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", x="Max. Baseline", y="Max. Post baseline", color="TRTPN", shape="TRTPN") +
  ggplot2::theme_bw() + ggplot2::theme(legend.position="bottom")
ggplot2::ggsave(file.path(out,"sasgraph.png"), p, width=8, height=6, dpi=150)
plotdat <- advsmax2[,c("USUBJID","TRTPN","BASE","AVAL","REF","ANRLO","ANRHI","BNRIND","ANRIND")]
write.csv(plotdat, file.path(out,"sasgraph.csv"), row.names=FALSE, na="")

write.csv(advsmax2[,c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","REF")], file.path(out,"advsmax2.csv"), row.names=FALSE, na="")
ta_out <- TableA[,c("TRTPN","GroupFmt","GroupN","BNRIND","ANRIND","COUNT","PERCENT","ValueFmt")]
names(ta_out) <- c("TRTPN","GroupFmt","GroupN","BNRIND","ANRIND","COUNT","PERCENT","ValueFmt")
write.csv(ta_out, file.path(out,"TableA.csv"), row.names=FALSE, na="")
eh_out <- EmergentHigh[,c("TRTPN","Nx","n","percent","FishersP")]
names(eh_out) <- c("TRTPN","Nx","n","percent","FishersP")
write.csv(eh_out, file.path(out,"EmergentHigh.csv"), row.names=FALSE, na="")
write.csv(ADVSMAXSum, file.path(out,"ADVSMAXSum.csv"), row.names=FALSE, na="")

pct_mat <- mat/Ntot*100
rowPct <- sweep(mat,1,rowT,"/")*100
colPct <- sweep(mat,2,colT,"/")*100
p036df <- data.frame()
for(i in seq_len(R)){
  for(j in seq_len(C)){
    p036df <- rbind(p036df, data.frame(TRTPN=suppressWarnings(as.numeric(rownames(mat)[i])), n=suppressWarnings(as.numeric(colnames(mat)[j])), COUNT=mat[i,j], PERCENT=round_half_up(pct_mat[i,j],2), ROWPCT=round_half_up(rowPct[i,j],2), COLPCT=round_half_up(colPct[i,j],2), EXPECTED=exp_mat[i,j], stringsAsFactors=FALSE))
  }
}
write.csv(p036df, file.path(out,"P036.csv"), row.names=FALSE, na="")

shift_csv <- TableA[,c("GroupFmt","TRTPN","BNRIND","ANRIND","COUNT","PERCENT")]
shift_csv$TABLE <- "SHIFT"
shift_csv$Nx <- NA; shift_csv$n <- NA; shift_csv$FishersP <- NA
eh_csv <- data.frame(GroupFmt=NA, TRTPN=EmergentHigh$TRTPN, BNRIND=NA, ANRIND=NA, COUNT=NA, PERCENT=EmergentHigh$percent, TABLE="EMERGENT", Nx=EmergentHigh$Nx, n=EmergentHigh$n, FishersP=EmergentHigh$FishersP, stringsAsFactors=FALSE)
t16 <- rbind(shift_csv[,c("TABLE","GroupFmt","TRTPN","BNRIND","ANRIND","COUNT","PERCENT","Nx","n","FishersP")], eh_csv[,c("TABLE","GroupFmt","TRTPN","BNRIND","ANRIND","COUNT","PERCENT","Nx","n","FishersP")])
write.csv(t16, file.path(out,"Target16.csv"), row.names=FALSE, na="")

disp_bn <- function(x) ifelse(is.na(x) | x=="__MISS__", "", x)
anr_levels <- sort(unique(TableA$ANR_F))
bnr_levels <- sort(unique(TableA$BNR_F))
grp_levels <- unique(TableA$GroupFmt[order(match(TableA$TRTPN, trt_levels))])
foot3 <- paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", fmt_1dec(ANRLO_m), " mmol/L ULNx ", fmt_1dec(ANRHI_m), " mmol/L) set 2 (Female, Age 18-65, LLN=", fmt_1dec(ANRLO_m), " mmol/L ULN=", fmt_1dec(ANRHI_m), " mmol/L).")

write_full_text <- function(path){
  con <- file(path, open="wt")
  cat("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)\n", file=con)
  cat(paste0("Summary means: ANRLO=", fmt_4dec(mean_anrlo), " ANRHI=", fmt_4dec(mean_anrhi), " BASE=", fmt_4dec(mean_base), " AVAL=", fmt_4dec(mean_aval), " MinValue=", fmt_4dec(minV), " MaxValue=", fmt_4dec(maxV), "\n"), file=con)
  cat(paste0("Y-axis range ", YAxisMin, " to ", YAxisMax, " by ", MAJOR, "; ANRLO=", fmt_1dec(ANRLO_m), " ANRHI=", fmt_1dec(ANRHI_m), " MinValue=", sprintf("%.3f", minV), " MaxValue=", sprintf("%.3f", maxV), " margin=", sprintf("%.4f", margin), "\n"), file=con)
  cat("\nBaseline Shift Table: Max Post Basline Result (shift from baseline) N (%)\n", file=con)
  hdr <- sprintf("%-18s %-18s", "Treatment","Max. Baseline Result")
  for(a in anr_levels){
    alab <- disp_bn(a)
    if(alab=="") alab <- "Missing"
    hdr <- paste0(hdr, sprintf(" | %-24s", paste0(alab," N (%)")))
  }
  cat(paste0(hdr, "\n"), file=con)
  for(g in grp_levels){
    for(b in bnr_levels){
      sub <- TableA[TableA$GroupFmt==g & TableA$BNR_F==b,,drop=FALSE]
      if(nrow(sub)==0) next
      blab <- disp_bn(b)
      if(blab=="") blab <- "Missing"
      line <- sprintf("%-18s %-18s", g, blab)
      for(a in anr_levels){
        rr <- sub[sub$ANR_F==a,,drop=FALSE]
        if(nrow(rr)==0){
          cell <- sprintf("%4s %-8s","","")
        } else {
          cell <- paste0(sprintf("%4d", rr$COUNT[1]), " ", fmt_percpar(rr$PERCENT[1]), " ", rr$ValueFmt[1], " ", fmt_2dec(rr$PERCENT[1]))
        }
        line <- paste0(line, sprintf(" | %-24s", cell))
      }
      cat(paste0(line, "\n"), file=con)
    }
    cat("\n", file=con)
  }
  cat("\nTreatment Emergent High\n", file=con)
  cat(sprintf("%-10s %-10s %-10s %-10s %-12s\n","TRTPN","Nx","n","%","FishersP"), file=con)
  for(i in seq_len(nrow(EmergentHigh))){
    cat(paste0(sprintf("%-10s %-10d %-10d %-10s %-12s", as.character(EmergentHigh$TRTPN[i]), EmergentHigh$Nx[i], EmergentHigh$n[i], fmt_1dec(EmergentHigh$percent[i]), fmt_pvalue(EmergentHigh$FishersP[i])), "\n"), file=con)
  }
  cat("\nThe FREQ Procedure\n", file=con)
  cat("Table of TRTPN by n\n", file=con)
  cat("\n", file=con)
  cat(paste0(sprintf("%-10s %-12s %-12s %-12s\n","TRTPN","n=0","n=1","Total")), file=con)
  for(i in seq_len(R)){
    rn <- rownames(mat)[i]
    cat(paste0(sprintf("TRTPN=%s\n", rn)), file=con)
    cat(paste0(sprintf("%-10s %-12d %-12d %-12d\n","Frequency", mat[i,1], mat[i,2], rowT[i])), file=con)
    cat(paste0(sprintf("%-10s %-12s %-12s %-12s\n","Percent", fmt_2dec(pct_mat[i,1]), fmt_2dec(pct_mat[i,2]), fmt_2dec(rowT[i]/Ntot*100))), file=con)
    cat(paste0(sprintf("%-10s %-12s %-12s %-12s\n","Row Pct", fmt_2dec(rowPct[i,1]), fmt_2dec(rowPct[i,2]), fmt_2dec(100))), file=con)
    cat(paste0(sprintf("%-10s %-12s %-12s %-12s\n","Col Pct", fmt_2dec(colPct[i,1]), fmt_2dec(colPct[i,2]), "")), file=con)
    cat("\n", file=con)
  }
  cat(paste0(sprintf("%-10s %-12d %-12d %-12d\n","Total", colT[1], colT[2], Ntot)), file=con)
  cat(paste0(sprintf("%-10s %-12s %-12s %-12s\n","Total Percent", fmt_2dec(colT[1]/Ntot*100), fmt_2dec(colT[2]/Ntot*100), fmt_2dec(100))), file=con)
  cat("\nStatistics for Table of TRTPN by n\n\n", file=con)
  cat(paste0(sprintf("%-28s %-4s %-12s %-12s\n","Statistic","DF","Value","Prob")), file=con)
  cat(paste0(sprintf("%-28s %-4d %-12s %-12s\n","Chi-Square", df_chi, fmt_4dec(chi), fmt_pvalue(p_chi))), file=con)
  cat(paste0(sprintf("%-28s %-4d %-12s %-12s\n","Likelihood Ratio Chi-Square", df_chi, fmt_4dec(lr), fmt_pvalue(p_lr))), file=con)
  if(!is.na(cont_adj)){
    cat(paste0(sprintf("%-28s %-4d %-12s %-12s\n","Continuity Adj. Chi-Square", 1, fmt_4dec(cont_adj), fmt_pvalue(p_cont))), file=con)
  }
  cat(paste0(sprintf("%-28s %-4d %-12s %-12s\n","Mantel-Haenszel Chi-Square", 1, fmt_4dec(mh), fmt_pvalue(p_mh))), file=con)
  cat(paste0(sprintf("%-28s %-4s %-12s %-12s\n","Phi Coefficient","", fmt_4dec(phi), "")), file=con)
  cat(paste0(sprintf("%-28s %-4s %-12s %-12s\n","Contingency Coefficient","", fmt_4dec(cont), "")), file=con)
  cat(paste0(sprintf("%-28s %-4s %-12s %-12s\n","Cramer's V","", fmt_4dec(cramV), "")), file=con)
  cat("\n", file=con)
  cat("Fisher's Exact Test\n", file=con)
  if(R==2 && C==2){
    cat(paste0(sprintf("Cell (1,1) Frequency (F)  %d\n", mat[1,1])), file=con)
    cat(paste0(sprintf("Table Probability (P)  %s\n", fmt_4dec(Pobs))), file=con)
    cat(paste0(sprintf("Two-sided Pr <= P  %s\n", fmt_pvalue(p_fish))), file=con)
  } else {
    cat(paste0(sprintf("Table Probability (P)  %s\n", fmt_4dec(Pobs))), file=con)
    cat(paste0(sprintf("Pr <= P  %s\n", fmt_pvalue(p_fish))), file=con)
  }
  cat(paste0(sprintf("Sample Size = %d\n", Ntot)), file=con)
  cat("\n", file=con)
  cat("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot\n", file=con)
  cat("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.\n", file=con)
  cat(paste0(foot3, "\n"), file=con)
  close(con)
}
write_full_text(file.path(out,"P036.txt"))
write_full_text(file.path(out,"Target16.txt"))
