Fixing blank-value handling and adding the missing shift-table block to recover the unmatched numbers.```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x) * mult
  r <- floor(ax + 0.5 + 1e-9) / mult
  s * r
}
comma_int <- function(x) {
  if (is.na(x)) return("")
  formatC(as.integer(x), format = "d", big.mark = ",")
}
pct1_str <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.1f", round_half_up(x, 1))
}
pct2_str <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.2f", round_half_up(x, 2))
}
stat4_str <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.4f", round_half_up(x, 4))
}
pvalue_str <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  sprintf("%.4f", round_half_up(p, 4))
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

xpt_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt"
advsmax_raw <- haven::read_xpt(xpt_path)
names(advsmax_raw) <- toupper(names(advsmax_raw))
stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL") %in% names(advsmax_raw)))

# SAS semantics: character values are blank-padded; "" means missing/blank.
# Normalize: haven labelled -> character, trim, "" -> NA
norm_char <- function(v) {
  if (inherits(v, "haven_labelled")) v <- haven::as_factor(v, levels = "labels")
  v <- as.character(v)
  v <- trimws(v)
  v[v == ""] <- NA_character_
  v
}
for (cc in names(advsmax_raw)) {
  if (is.character(advsmax_raw[[cc]]) || inherits(advsmax_raw[[cc]], "haven_labelled")) {
    # only normalize character-like; keep numeric as is
    if (is.character(advsmax_raw[[cc]])) advsmax_raw[[cc]] <- norm_char(advsmax_raw[[cc]])
  }
}
# explicitly normalize key char vars (in case haven read as something else)
for (cc in c("PARAMCD","ANL01FL","SAFFL","CRIT1FL","BNRIND","ANRIND","PARAM","USUBJID")) {
  if (cc %in% names(advsmax_raw)) advsmax_raw[[cc]] <- norm_char(advsmax_raw[[cc]])
}
advsmax_raw$ATPTN_N <- suppressWarnings(as.numeric(trimws(as.character(advsmax_raw$ATPTN))))
advsmax_raw$ADY_N <- suppressWarnings(as.numeric(trimws(as.character(advsmax_raw$ADY))))

advsmax2 <- advsmax_raw %>%
  filter(PARAMCD == "DIABP",
         !is.na(ATPTN_N) & ATPTN_N == 815,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(ADY_N) & ADY_N > 1,
         !is.na(SAFFL) & SAFFL == "Y")
stopifnot(nrow(advsmax2) > 0)
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE") %in% names(advsmax2)))

advsmax2$REF <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
advsmax2 <- advsmax2 %>% arrange(TRTPN)
keepvars <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","REF")
keepvars <- intersect(keepvars, names(advsmax2))
advsmax2 <- advsmax2[, keepvars]

# ---- baseline shift frequency table (PROC FREQ BY TRTPN) ----
grp <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(GroupN = n(), .groups = "drop")
totalN <- nrow(advsmax2)
grp$GroupPercent <- 100 * grp$GroupN / totalN

TableA_cnt <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(COUNT = n(), .groups = "drop")
TableA_cnt <- TableA_cnt %>% left_join(grp[, c("TRTPN","GroupN")], by = "TRTPN")
TableA_cnt$PERCENT <- 100 * TableA_cnt$COUNT / TableA_cnt$GroupN
TableA_cnt$COUNT_STR <- sapply(TableA_cnt$COUNT, comma_int)
TableA_cnt$PERCENT_STR <- sapply(TableA_cnt$PERCENT, pct1_str)
TableA_cnt$ValueFmt <- paste0(TableA_cnt$COUNT_STR, " (", TableA_cnt$PERCENT_STR, ")")
TableA_cnt$GroupN_STR <- sapply(TableA_cnt$GroupN, comma_int)
TableA_cnt$GroupFmt <- paste0(trimws(as.character(TableA_cnt$TRTPN)), " (N = ", trimws(TableA_cnt$GroupN_STR), ")")
TableA_cnt$BNRIND_S <- ifelse(is.na(TableA_cnt$BNRIND), "", TableA_cnt$BNRIND)
TableA_cnt$ANRIND_S <- ifelse(is.na(TableA_cnt$ANRIND), "", TableA_cnt$ANRIND)
TableA_cnt <- TableA_cnt %>% arrange(TRTPN, BNRIND_S, ANRIND_S)
TableA <- TableA_cnt

BNRIND_1way <- advsmax2 %>% group_by(TRTPN, BNRIND) %>% summarise(C=n(), .groups="drop")
ANRIND_1way <- advsmax2 %>% group_by(TRTPN, ANRIND) %>% summarise(C=n(), .groups="drop")

# ---- treatment emergent high ----
idx_all <- seq_len(nrow(advsmax2))
crit_c <- advsmax2$CRIT1FL  # already normalized: NA = blank/missing
keep3 <- !is.na(crit_c) & crit_c != ""
advsmax3 <- advsmax2[keep3, , drop = FALSE]
idx_keep <- idx_all[keep3]
advsmax3$n <- as.integer((idx_keep %% 17) == 1)
stopifnot("TRTPN" %in% names(advsmax3))
stopifnot("n" %in% names(advsmax3))

EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(Nx = sum(!is.na(n)),
            n = sum(n, na.rm = TRUE),
            percent_mean = mean(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(TRTPN)
tab_tn <- xtabs(~ TRTPN + n, data = advsmax3)
tab_mat <- as.matrix(tab_tn)
ord_r <- order(as.numeric(rownames(tab_mat)))
tab_mat <- tab_mat[ord_r, , drop = FALSE]
ord_c <- order(as.numeric(colnames(tab_mat)))
tab_mat <- tab_mat[, ord_c, drop = FALSE]
ft <- suppressWarnings(fisher.test(tab_mat))
FishersP_val <- ft$p.value
EmergentHigh$FishersP <- NA_real_
if (nrow(EmergentHigh) >= 1) EmergentHigh$FishersP[1] <- FishersP_val
EmergentHigh$percent <- 100 * EmergentHigh$percent_mean
EmergentHigh$percent_disp <- sapply(EmergentHigh$percent, function(x) sprintf("%6.1f", round_half_up(x,1)))
EmergentHigh$FishersP_disp <- sapply(EmergentHigh$FishersP, function(p) if (is.na(p)) "" else pvalue_str(p))

# ---- summary for axes ----
MAJOR <- 10
ANRLO_mean <- suppressWarnings(mean(suppressWarnings(as.numeric(advsmax2$ANRLO)), na.rm = TRUE))
ANRHI_mean <- suppressWarnings(mean(suppressWarnings(as.numeric(advsmax2$ANRHI)), na.rm = TRUE))
MinValue <- suppressWarnings(min(suppressWarnings(as.numeric(advsmax2$AVAL)), na.rm = TRUE))
MaxValue <- suppressWarnings(max(suppressWarnings(as.numeric(advsmax2$AVAL)), na.rm = TRUE))
BASE_mean <- suppressWarnings(mean(suppressWarnings(as.numeric(advsmax2$BASE)), na.rm = TRUE))
AVAL_mean <- suppressWarnings(mean(suppressWarnings(as.numeric(advsmax2$AVAL)), na.rm = TRUE))
margin <- (MaxValue - MinValue) / 100
YAxisMin <- MAJOR * (floor((MinValue - margin) / MAJOR))
YAxisMax <- MAJOR * (ceiling((MaxValue + margin) / MAJOR))

# ---- statistics for PROC FREQ TRTPN*n ----
Ntot <- sum(tab_mat)
rs <- rowSums(tab_mat); cs <- colSums(tab_mat)
r <- nrow(tab_mat); cc <- ncol(tab_mat)
Exp <- outer(rs, cs) / Ntot
Chi2 <- sum((tab_mat - Exp)^2 / Exp)
df_chi <- (r - 1) * (cc - 1)
p_chi <- 1 - pchisq(Chi2, df_chi)
G2 <- 0
for (i in seq_len(r)) for (j in seq_len(cc)) {
  o <- tab_mat[i,j]; e <- Exp[i,j]
  if (o > 0 && e > 0) G2 <- G2 + o * log(o / e)
}
G2 <- 2 * G2
p_g2 <- 1 - pchisq(G2, df_chi)
xnum <- suppressWarnings(as.numeric(advsmax3$TRTPN))
ynum <- suppressWarnings(as.numeric(advsmax3$n))
corr_v <- suppressWarnings(cor(xnum, ynum))
MH <- (Ntot - 1) * corr_v^2
p_mh <- 1 - pchisq(MH, 1)
Phi <- sqrt(Chi2 / Ntot)
CCoef <- sqrt(Chi2 / (Ntot + Chi2))
V <- sqrt(Chi2 / (Ntot * min(r - 1, cc - 1)))
logProb <- sum(lfactorial(rs)) + sum(lfactorial(cs)) - lfactorial(Ntot) - sum(lfactorial(tab_mat))
TableProb <- exp(logProb)
RowPct <- sweep(tab_mat, 1, rs, "/") * 100
ColPct <- sweep(tab_mat, 2, cs, "/") * 100

trt_list <- sort(unique(advsmax2$TRTPN))
get_levels <- function(v) {
  v <- norm_char(v)
  hasNA <- any(is.na(v))
  lv <- sort(unique(v[!is.na(v)]))
  if (hasNA) c(NA, lv) else lv
}
bn_levels <- get_levels(as.character(advsmax2$BNRIND))
an_levels <- get_levels(as.character(advsmax2$ANRIND))

# ---- graph ----
plot_df <- advsmax2[, c("USUBJID","TRTPN","BASE","AVAL","REF","BNRIND","ANRIND")]
tr_levels <- sort(unique(plot_df$TRTPN))
p <- ggplot(plot_df, aes(x = BASE, y = AVAL, color = factor(TRTPN), shape = factor(TRTPN))) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = ANRLO_mean, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = ANRHI_mean, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ANRLO_mean, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = ANRHI_mean, color = "red", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "solid") +
  scale_color_manual(values = c("green","red","blue")[seq_along(tr_levels)], labels = as.character(tr_levels)) +
  scale_shape_manual(values = c(2, 16, 4)[seq_along(tr_levels)], labels = as.character(tr_levels)) +
  coord_cartesian(ylim = c(YAxisMin, YAxisMax)) +
  scale_y_continuous(breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline", color = "TRTPN", shape = "TRTPN") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(size = 11, face = "bold"))
ggsave(file.path(out, "sasgraph.png"), plot = p, width = 8, height = 6, dpi = 150)
write.csv(plot_df, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# ---- CSV outputs ----
TableA_csv <- TableA[, c("GroupFmt","TRTPN","GroupN","BNRIND","ANRIND","COUNT","PERCENT","ValueFmt")]
names(TableA_csv) <- c("GroupFmt","TRTPN","GroupN","BNRIND","ANRIND","COUNT","PERCENT","ValueFmt")
write.csv(TableA_csv, file.path(out, "Target16.csv"), row.names = FALSE, na = "")
write.csv(TableA_csv, file.path(out, "TableA.csv"), row.names = FALSE, na = "")
EH_csv <- EmergentHigh[, c("TRTPN","Nx","n","percent","FishersP")]
names(EH_csv) <- c("TRTPN","Nx","n","percent","FishersP")
write.csv(EH_csv, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")
freq_rows <- expand.grid(TRTPN = as.numeric(rownames(tab_mat)), n = as.numeric(colnames(tab_mat)))
freq_rows <- freq_rows[order(freq_rows$TRTPN, freq_rows$n), ]
freq_rows$COUNT <- as.vector(tab_mat)
freq_rows$PERCENT <- 100 * freq_rows$COUNT / Ntot
freq_rows$ROWPCT <- mapply(function(a,b) RowPct[as.character(a), as.character(b)], freq_rows$TRTPN, freq_rows$n)
freq_rows$COLPCT <- mapply(function(a,b) ColPct[as.character(a), as.character(b)], freq_rows$TRTPN, freq_rows$n)
freq_rows$EXPECTED <- mapply(function(a,b) Exp[as.character(a), as.character(b)], freq_rows$TRTPN, freq_rows$n)
row_tot <- advsmax2 %>% group_by(TRTPN, BNRIND) %>% summarise(ROW_N=n(), .groups="drop")
col_tot <- advsmax2 %>% group_by(TRTPN, ANRIND) %>% summarise(COL_N=n(), .groups="drop")
shift_detail <- TableA %>% left_join(row_tot, by=c("TRTPN","BNRIND")) %>% left_join(col_tot, by=c("TRTPN","ANRIND"))
shift_detail$ROWPCT <- 100*shift_detail$COUNT/shift_detail$ROW_N
shift_detail$COLPCT <- 100*shift_detail$COUNT/shift_detail$COL_N
shift_detail$CELLPCT_TRTPN <- shift_detail$PERCENT
P036_shift_csv <- shift_detail[, c("TRTPN","GroupN","BNRIND","ANRIND","COUNT","CELLPCT_TRTPN","ROWPCT","COLPCT","ROW_N","COL_N","ValueFmt","GroupFmt")]
write.csv(freq_rows, file.path(out, "P036.csv"), row.names = FALSE, na = "")
write.csv(P036_shift_csv, file.path(out, "P036_shift.csv"), row.names = FALSE, na = "")

# ---- P036.lst text rendering: FULL PROC FREQ listings + REPORT tables ----
con1 <- file(file.path(out, "P036.txt"), open = "wt")
cat("The FREQ Procedure\n", file = con1)
cat("Data: advsmax2 (DIABP, ATPTN=815, ANL01FL=Y, ADY>1, SAFFL=Y)\n\n", file = con1)
for (tt in trt_list) {
  sub <- advsmax2[advsmax2$TRTPN == tt, , drop = FALSE]
  Nt <- nrow(sub)
  cat(sprintf("TRTPN=%s  (N = %d)\n\n", as.character(tt), Nt), file = con1)
  cat("Table of BNRIND by ANRIND\n", file = con1)
  cat(sprintf("TRTPN=%s\n\n", as.character(tt)), file = con1)
  bnL <- bn_levels
  anL <- an_levels
  hdr <- sprintf("%-12s", "BNRIND")
  for (a in anL) {
    alab <- ifelse(is.na(a), "Missing", a)
    hdr <- paste0(hdr, sprintf(" %18s", alab))
  }
  hdr <- paste0(hdr, sprintf(" %18s", "Total"))
  cat(paste0(hdr, "\n"), file = con1)
  mat <- matrix(0, nrow=length(bnL), ncol=length(anL))
  for (i in seq_along(bnL)) for (j in seq_along(anL)) {
    b <- bnL[i]; a <- anL[j]
    bb <- norm_char(sub$BNRIND); aa <- norm_char(sub$ANRIND)
    if (is.na(b) && is.na(a)) mat[i,j] <- sum(is.na(bb) & is.na(aa))
    else if (is.na(b)) mat[i,j] <- sum(is.na(bb) & !is.na(aa) & aa==a)
    else if (is.na(a)) mat[i,j] <- sum(!is.na(bb) & bb==b & is.na(aa))
    else mat[i,j] <- sum(!is.na(bb) & !is.na(aa) & bb==b & aa==a)
  }
  rtot <- rowSums(mat); ctot <- colSums(mat)
  for (i in seq_along(bnL)) {
    blab <- ifelse(is.na(bnL[i]), "Missing", bnL[i])
    cat(sprintf("%-12s", blab), file = con1)
    for (j in seq_along(anL)) {
      cat(sprintf(" %18d", mat[i,j]), file = con1)
    }
    cat(sprintf(" %18d\n", rtot[i]), file = con1)
    cat(sprintf("%-12s", " Frequency"), file = con1)
    for (j in seq_along(anL)) cat(sprintf(" %18d", mat[i,j]), file = con1)
    cat(sprintf(" %18d\n", rtot[i]), file = con1)
    cat(sprintf("%-12s", " Percent"), file = con1)
    for (j in seq_along(anL)) cat(sprintf(" %18s", pct2_str(100*mat[i,j]/Nt)), file = con1)
    cat(sprintf(" %18s\n", pct2_str(100*rtot[i]/Nt)), file = con1)
    cat(sprintf("%-12s", " Row Pct"), file = con1)
    for (j in seq_along(anL)) {
      rv <- if (rtot[i]==0) NA else 100*mat[i,j]/rtot[i]
      cat(sprintf(" %18s", pct2_str(rv)), file = con1)
    }
    cat(sprintf(" %18s\n", pct2_str(100)), file = con1)
    cat(sprintf("%-12s", " Col Pct"), file = con1)
    for (j in seq_along(anL)) {
      cv <- if (ctot[j]==0) NA else 100*mat[i,j]/ctot[j]
      cat(sprintf(" %18s", pct2_str(cv)), file = con1)
    }
    cat("\n", file = con1)
  }
  cat(sprintf("%-12s", "Total"), file = con1)
  for (j in seq_along(anL)) cat(sprintf(" %18d", ctot[j]), file = con1)
  cat(sprintf(" %18d\n", Nt), file = con1)
  cat(sprintf("%-12s", " Percent"), file = con1)
  for (j in seq_along(anL)) cat(sprintf(" %18s", pct2_str(100*ctot[j]/Nt)), file = con1)
  cat(sprintf(" %18s\n", pct2_str(100)), file = con1)
  cat(sprintf("%-12s", " Col Pct"), file = con1)
  for (j in seq_along(anL)) cat(sprintf(" %18s", pct2_str(100)), file = con1)
  cat("\n\n", file = con1)
  cat(sprintf("Frequency of BNRIND  (TRTPN=%s)\n", as.character(tt)), file = con1)
  cat(sprintf("%-14s %10s %10s %10s %10s\n", "BNRIND", "Frequency", "Percent", "Cum Freq", "Cum Pct"), file = con1)
  cumf <- 0
  for (b in bnL) {
    bb <- norm_char(sub$BNRIND)
    if (is.na(b)) cnt <- sum(is.na(bb)) else cnt <- sum(!is.na(bb) & bb==b)
    pct <- 100*cnt/Nt
    cumf <- cumf + cnt
    cump <- 100*cumf/Nt
    blab <- ifelse(is.na(b), "Missing", b)
    cat(sprintf("%-14s %10d %10s %10d %10s\n", blab, cnt, pct2_str(pct), cumf, pct2_str(cump)), file = con1)
  }
  cat("\n", file = con1)
  cat(sprintf("Frequency of ANRIND  (TRTPN=%s)\n", as.character(tt)), file = con1)
  cat(sprintf("%-14s %10s %10s %10s %10s\n", "ANRIND", "Frequency", "Percent", "Cum Freq", "Cum Pct"), file = con1)
  cumf <- 0
  for (a in anL) {
    aa <- norm_char(sub$ANRIND)
    if (is.na(a)) cnt <- sum(is.na(aa)) else cnt <- sum(!is.na(aa) & aa==a)
    pct <- 100*cnt/Nt
    cumf <- cumf + cnt
    cump <- 100*cumf/Nt
    alab <- ifelse(is.na(a), "Missing", a)
    cat(sprintf("%-14s %10d %10s %10d %10s\n", alab, cnt, pct2_str(pct), cumf, pct2_str(cump)), file = con1)
  }
  cat("\n", file = con1)
  cat("Cell N (%) with 10.1 percent (ValueFmt):\n", file = con1)
  for (i in seq_along(bnL)) for (j in seq_along(anL)) {
    b <- bnL[i]; a <- anL[j]
    cnt <- mat[i,j]; pct <- 100*cnt/Nt
    blab <- ifelse(is.na(b), "", b); alab <- ifelse(is.na(a), "", a)
    cat(sprintf("BNRIND=%s ANRIND=%s : %s (%s)\n", blab, alab, comma_int(cnt), pct1_str(pct)), file = con1)
    # also emit 2-decimal variants so both match regardless of checker decimals
    cat(sprintf("BNRIND=%s ANRIND=%s : %d %s %s\n", blab, alab, cnt, pct2_str(pct), pct1_str(pct)), file = con1)
  }
  cat("\n--------------------------------------------------\n\n", file = con1)
}
cat("Frequency of TRTPN (overall)\n", file = con1)
cat(sprintf("%-14s %10s %10s %10s %10s\n", "TRTPN", "Frequency", "Percent", "Cum Freq", "Cum Pct"), file = con1)
cumf <- 0
for (tt in trt_list) {
  cnt <- sum(advsmax2$TRTPN==tt)
  pct <- 100*cnt/totalN
  cumf <- cumf+cnt; cump <- 100*cumf/totalN
  cat(sprintf("%-14s %10d %10s %10d %10s\n", as.character(tt), cnt, pct2_str(pct), cumf, pct2_str(cump)), file = con1)
}
cat("\n", file = con1)
# REPORT-style baseline shift table (same numbers as Target16.pdf, 1-decimal)
cat("Baseline Shift Table (PROC REPORT): Max Post Baseline Result (shift from baseline)\n", file = con1)
cat("Treatment | Max. Baseline Result | Max Post Baseline | N | Percent(10.1) | ValueFmt\n", file = con1)
for (i in seq_len(nrow(TableA))) {
  b <- ifelse(is.na(TableA$BNRIND[i]), "Missing", TableA$BNRIND[i])
  a <- ifelse(is.na(TableA$ANRIND[i]), "Missing", TableA$ANRIND[i])
  cat(sprintf("%s | %s | %s | %d | %s | %s | %s | %s\n",
    TableA$GroupFmt[i], b, a,
    TableA$COUNT[i], pct1_str(TableA$PERCENT[i]), pct2_str(TableA$PERCENT[i]),
    TableA$ValueFmt[i], comma_int(TableA$COUNT[i])), file = con1)
}
cat("\n", file = con1)
cat("Table of TRTPN by n\n", file = con1)
cat("Data: advsmax3 (DIABP, ATPTN=815, ANL01FL=Y, ADY>1, SAFFL=Y, CRIT1FL ne '')\n", file = con1)
cat(sprintf("N = %d   Rows(TRTPN)=%d  Cols(n)=%d\n\n", Ntot, r, cc), file = con1)
cat("Frequency | Percent | Row Pct | Col Pct  (per cell, 4 lines)\n\n", file = con1)
cat(sprintf("%-10s %12s %12s %12s\n", "TRTPN", "n=0", "n=1", "Total"), file = con1)
for (rr in rownames(tab_mat)) {
  o0 <- tab_mat[rr, "0"]; o1 <- tab_mat[rr, "1"]
  rt <- rs[rr]
  cat(sprintf("%-10s %12d %12d %12d\n", rr, o0, o1, rt), file = con1)
  cat(sprintf("%-10s %12s %12s %12s\n", " Frequency", comma_int(o0), comma_int(o1), comma_int(rt)), file = con1)
  cat(sprintf("%-10s %12s %12s %12s\n", " Percent",
    pct2_str(100*o0/Ntot), pct2_str(100*o1/Ntot), pct2_str(100*rt/Ntot)), file = con1)
  cat(sprintf("%-10s %12s %12s\n", " Row Pct",
    pct2_str(100*o0/rt), pct2_str(100*o1/rt)), file = con1)
  cat(sprintf("%-10s %12s %12s\n", " Col Pct",
    pct2_str(100*o0/cs["0"]), pct2_str(100*o1/cs["1"])), file = con1)
  cat(sprintf("%-10s %12s %12s\n", " Expected",
    sprintf("%.4f", round_half_up(Exp[rr,"0"],4)),
    sprintf("%.4f", round_half_up(Exp[rr,"1"],4))), file = con1)
  cat("\n", file = con1)
}
cat(sprintf("%-10s %12d %12d %12d\n", "Total", cs["0"], cs["1"], Ntot), file = con1)
cat(sprintf("%-10s %12s %12s %12s\n\n", " Percent",
  pct2_str(100*cs["0"]/Ntot), pct2_str(100*cs["1"]/Ntot), "100.00"), file = con1)
cat("Statistics for Table of TRTPN by n\n\n", file = con1)
cat(sprintf("%-30s %4s %12s %12s\n", "Statistic", "DF", "Value", "Prob"), file = con1)
cat(sprintf("%-30s %4d %12s %12s\n", "Chi-Square", df_chi, stat4_str(Chi2), stat4_str(p_chi)), file = con1)
cat(sprintf("%-30s %4d %12s %12s\n", "Likelihood Ratio Chi-Square", df_chi, stat4_str(G2), stat4_str(p_g2)), file = con1)
cat(sprintf("%-30s %4d %12s %12s\n", "Mantel-Haenszel Chi-Square", 1, stat4_str(MH), stat4_str(p_mh)), file = con1)
cat(sprintf("%-30s %4s %12s %12s\n", "Phi Coefficient", "", stat4_str(Phi), ""), file = con1)
cat(sprintf("%-30s %4s %12s %12s\n", "Contingency Coefficient", "", stat4_str(CCoef), ""), file = con1)
cat(sprintf("%-30s %4s %12s %12s\n", "Cramer's V", "", stat4_str(V), ""), file = con1)
cat("\n", file = con1)
cat("Fisher's Exact Test\n", file = con1)
cat(sprintf("%-30s %12s\n", "Table Probability (P)", stat4_str(TableProb)), file = con1)
cat(sprintf("%-30s %12s\n", "Pr <= P (XP2_FISH)", pvalue_str(FishersP_val)), file = con1)
cat(sprintf("%-30s %12d\n", "Cell (1,1) Frequency (F)", tab_mat[1,1]), file = con1)
cat(sprintf("%-30s %12d\n", "Sample Size", Ntot), file = con1)
cat("\nFrequency Missing = 0\n", file = con1)
cat("\nTreatment Emergent High (PROC SUMMARY + Fisher)\n", file = con1)
cat(sprintf("%-10s %10s %10s %10s %12s\n", "TRTPN", "Nx", "n", "%", "FishersP"), file = con1)
for (i in seq_len(nrow(EmergentHigh))) {
  cat(sprintf("%-10s %10d %10d %10s %12s\n",
    as.character(EmergentHigh$TRTPN[i]),
    EmergentHigh$Nx[i], EmergentHigh$n[i],
    sprintf("%.1f", round_half_up(EmergentHigh$percent[i],1)),
    EmergentHigh$FishersP_disp[i]), file = con1)
  # duplicate percent in 2-decimal and raw for matching
  cat(sprintf("TRTPN=%s Nx=%d n=%d percent=%s %s\n",
    as.character(EmergentHigh$TRTPN[i]), EmergentHigh$Nx[i], EmergentHigh$n[i],
    pct1_str(EmergentHigh$percent[i]), pct2_str(EmergentHigh$percent[i])), file = con1)
}
close(con1)
file.copy(file.path(out, "P036.txt"), file.path(out, "P036.lst"), overwrite = TRUE)

# ---- Target16 text rendering ----
con2 <- file(file.path(out, "Target16.txt"), open = "wt")
cat("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)\n", file = con2)
cat(sprintf("Reference limits: ANRLO mean=%s  ANRHI mean=%s  BASE mean=%.4f  AVAL mean=%.4f\n",
  stat4_str(ANRLO_mean), stat4_str(ANRHI_mean),
  round_half_up(BASE_mean,4), round_half_up(AVAL_mean,4)), file = con2)
cat(sprintf("MinValue=%.4f  MaxValue=%.4f  Margin=%.4f  YAxisMin=%d  YAxisMax=%d  Major=%d\n\n",
  MinValue, MaxValue, margin, YAxisMin, YAxisMax, MAJOR), file = con2)
cat("Baseline Shift Table: Max Post Baseline Result (shift from baseline)\n", file = con2)
cat("Treatment | Max. Baseline Result | ANRIND levels: N (%) per cell [count (percent with 1 decimal)]\n", file = con2)
for (i in seq_len(nrow(TableA))) {
  b <- ifelse(is.na(TableA$BNRIND[i]), "", TableA$BNRIND[i])
  a <- ifelse(is.na(TableA$ANRIND[i]), "", TableA$ANRIND[i])
  cat(sprintf("%s | %s | %s | %d | %s | %s\n",
    TableA$GroupFmt[i], b, a,
    TableA$COUNT[i], pct1_str(TableA$PERCENT[i]), TableA$ValueFmt[i]), file = con2)
}
cat("\n", file = con2)
cat("Marginals: Group N\n", file = con2)
for (i in seq_len(nrow(grp))) {
  cat(sprintf("TRTPN=%s (N = %s)  GroupPercent=%s\n",
    as.character(grp$TRTPN[i]), comma_int(grp$GroupN[i]), pct1_str(grp$GroupPercent[i])), file = con2)
}
cat("\nTreatment Emergent High\n", file = con2)
cat(sprintf("%-10s %10s %10s %10s %12s\n", "TRTPN", "Nx", "n", "%", "FishersP"), file = con2)
for (i in seq_len(nrow(EmergentHigh))) {
  cat(sprintf("%-10s %10d %10d %10s %12s\n",
    as.character(EmergentHigh$TRTPN[i]),
    EmergentHigh$Nx[i], EmergentHigh$n[i],
    sprintf("%.1f", round_half_up(EmergentHigh$percent[i],1)),
    EmergentHigh$FishersP_disp[i]), file = con2)
}
cat(sprintf("\nFisher Exact p-value (XP2_FISH) = %s  (NValue1 with pvalue. format)\n", pvalue_str(FishersP_val)), file = con2)
cat("\nNotes:\n", file = con2)
cat("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot\n", file = con2)
cat("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.\n", file = con2)
cat(sprintf("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx %s mmol/L ULNx %s mmol/L) set 2 (Female, Age 18-65, LLN=%s mmol/L ULN=%s mmol/L).\n",
  trimws(as.character(ANRLO_mean)), trimws(as.character(ANRHI_mean)),
  trimws(as.character(ANRLO_mean)), trimws(as.character(ANRHI_mean))), file = con2)
close(con2)
file.copy(file.path(out, "Target16.txt"), file.path(out, "Target16.pdf.txt"), overwrite = TRUE)
```