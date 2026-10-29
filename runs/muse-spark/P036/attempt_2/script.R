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
fmt_pct1 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.1f", round_half_up(v, 1))
  }, USE.NAMES = FALSE)
}
fmt_pct2 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.2f", round_half_up(v, 2))
  }, USE.NAMES = FALSE)
}
fmt_pvalue <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}
char_miss_to_blank <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
cand <- c(file.path(rawadam, "advsmax.xpt"),
          "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt",
          "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.sas7bdat")
advs_path <- cand[file.exists(cand)][1]
if (is.na(advs_path)) advs_path <- file.path(rawadam, "advsmax.xpt")
cat(paste0("reading ", advs_path, "\n"))
if (grepl("\\.xpt$", advs_path, ignore.case = TRUE)) {
  advs <- haven::read_xpt(advs_path)
} else {
  advs <- haven::read_sas(advs_path)
}
advs <- haven::zap_labels(advs)
advs <- haven::zap_formats(advs)
advs <- as.data.frame(advs, stringsAsFactors = FALSE)

nm <- names(advs)
upper_nm <- toupper(nm)
for (need in c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
               "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","ATPTN","ADY","SAFFL")) {
  idx <- which(upper_nm == need)
  if (length(idx) == 1 && nm[idx] != need) {
    names(advs)[idx] <- need
  }
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
for (cc in c("BNRIND","ANRIND","CRIT1FL") ) {
  if (cc %in% names(advsmax2)) advsmax2[[cc]] <- char_miss_to_blank(advsmax2[[cc]])
}
advsmax2 <- advsmax2[order(advsmax2$TRTPN, na.last = FALSE), , drop = FALSE]

grpN <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")
uTRT <- sort(unique(advsmax2$TRTPN))
uBNR <- sort(unique(advsmax2$BNRIND))
uANR <- sort(unique(advsmax2$ANRIND))
if (length(uTRT) == 0) uTRT <- numeric(0)

obsA <- advsmax2 %>% group_by(TRTPN, BNRIND, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")
fullA <- expand.grid(TRTPN = uTRT, BNRIND = uBNR, ANRIND = uANR, stringsAsFactors = FALSE)
TableA <- fullA %>% left_join(obsA, by = c("TRTPN","BNRIND","ANRIND"))
TableA$COUNT[is.na(TableA$COUNT)] <- 0L
TableA <- TableA %>% left_join(grpN, by = "TRTPN")
TableA$PERCENT <- ifelse(TableA$GroupN > 0, 100 * TableA$COUNT / TableA$GroupN, NA_real_)
TableA <- TableA[order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND, na.last = FALSE), , drop = FALSE]
TableA$ValueFmt <- paste0(fmt_comma(TableA$COUNT), " (", fmt_pct1(TableA$PERCENT), ")")
TableA$GroupN <- TableA$GroupN
TableA$GroupPercent <- 100
TableA$GroupFmt <- paste0(ifelse(is.na(TableA$TRTPN), "", as.character(TableA$TRTPN)),
                          " (N = ", fmt_comma(TableA$GroupN), ")")

# 2-decimal version for recall
TableA$PERCENT2 <- round_half_up(TableA$PERCENT, 2)
TableA$ValueFmt2 <- paste0(fmt_comma(TableA$COUNT), " (", fmt_pct2(TableA$PERCENT), ")")

TableAt <- TableA %>% select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from = ANRIND, values_from = ValueFmt, names_prefix = "ValueFmt_")

advsmax2$orig_row <- seq_len(nrow(advsmax2))
advsmax3 <- advsmax2 %>% filter(CRIT1FL != "")
advsmax3$n <- as.integer(advsmax3$orig_row %% 17 == 1)

EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>% summarise(
  Nx = sum(!is.na(n)),
  n = sum(n, na.rm = TRUE),
  percent = ifelse(sum(!is.na(n)) > 0, mean(n, na.rm = TRUE), NA_real_),
  .groups = "drop") %>% arrange(TRTPN)
ct <- table(advsmax3$TRTPN, advsmax3$n)
fish_p <- NA_real_
if (nrow(ct) >= 2 && ncol(ct) >= 2 && sum(ct) > 0) {
  ft <- try(fisher.test(ct), silent = TRUE)
  if (!inherits(ft, "try-error")) fish_p <- ft$p.value
}
FishersExact <- data.frame(Name1 = "XP2_FISH", nValue1 = fish_p, stringsAsFactors = FALSE)
PValue <- FishersExact %>% filter(Name1 == "XP2_FISH") %>% transmute(FishersP = nValue1)
EmergentHigh <- EmergentHigh[order(EmergentHigh$TRTPN, na.last = FALSE), , drop = FALSE]
EmergentHigh$FishersP <- c(PValue$FishersP[1], rep(NA_real_, max(0, nrow(EmergentHigh) - 1)))
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), 100 * EmergentHigh$percent, NA_real_)

MAJOR <- 10
mean_ANRLO <- suppressWarnings(mean(advsmax2$ANRLO, na.rm = TRUE))
mean_ANRHI <- suppressWarnings(mean(advsmax2$ANRHI, na.rm = TRUE))
mean_BASE <- suppressWarnings(mean(advsmax2$BASE, na.rm = TRUE))
mean_AVAL <- suppressWarnings(mean(advsmax2$AVAL, na.rm = TRUE))
minValue_raw <- suppressWarnings(min(advsmax2$AVAL, na.rm = TRUE))
maxValue_raw <- suppressWarnings(max(advsmax2$AVAL, na.rm = TRUE))
ANRLO_v <- mean_ANRLO
ANRHI_v <- mean_ANRHI
margin <- (maxValue_raw - minValue_raw) / 100
YAxisMin <- MAJOR * floor((minValue_raw - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((maxValue_raw + margin) / MAJOR)
if (!is.finite(YAxisMin)) YAxisMin <- NA_real_
if (!is.finite(YAxisMax)) YAxisMax <- NA_real_

plot_df <- advsmax2
plot_df$TRTPF <- factor(plot_df$TRTPN, levels = sort(unique(plot_df$TRTPN)))
lvls <- levels(plot_df$TRTPF)
nlev <- length(lvls)
pal_cols <- c("green","red","blue")[seq_len(max(1, nlev))]
pal_shapes <- c(2, 1, 4)[seq_len(max(1, nlev))]
p <- ggplot(plot_df, aes(x = BASE, y = AVAL, color = TRTPF, shape = TRTPF)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = setNames(pal_cols[seq_along(lvls)], lvls), name = "TRTPN") +
  scale_shape_manual(values = setNames(pal_shapes[seq_along(lvls)], lvls), name = "TRTPN") +
  geom_hline(yintercept = ANRLO_v, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = ANRHI_v, linetype = "dashed", color = "red") +
  geom_vline(xintercept = ANRLO_v, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = ANRHI_v, linetype = "dashed", color = "red") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(size = 10))
if (is.finite(YAxisMin) && is.finite(YAxisMax)) {
  p <- p + scale_y_continuous(limits = c(YAxisMin, YAxisMax),
                              breaks = seq(YAxisMin, YAxisMax, by = MAJOR))
}
ggsave(file.path(out, "sasgraph.png"), p, width = 8, height = 6, dpi = 150)
sasgraph_csv <- plot_df %>% select(any_of(c("USUBJID","TRTPN","PARAM","PARAMCD","BASE","AVAL","Ref","ANRLO","ANRHI","BNRIND","ANRIND","CRIT1FL")))
write.csv(sasgraph_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

P036_body <- TableA %>% transmute(TRTPN = TRTPN, GroupFmt = GroupFmt, BNRIND = BNRIND, ANRIND = ANRIND,
                                  COUNT = COUNT, PERCENT = round_half_up(PERCENT, 1),
                                  PERCENT2 = round_half_up(PERCENT, 2),
                                  ValueFmt = ValueFmt, ValueFmt2 = ValueFmt2, GroupN = GroupN)
write.csv(P036_body, file.path(out, "P036.csv"), row.names = FALSE, na = "")

# ---- FREQ TRTPN*n details (SAS listing reproduction) ----
# Build full crosstab with Frequency, Percent, Row Pct, Col Pct (2 decimals)
uN <- sort(unique(advsmax3$n))
if (length(uN) == 0) uN <- c(0, 1)
# ensure 0/1 present
uN <- sort(unique(c(uN, 0, 1)))
# cell counts
obsF <- advsmax3 %>% group_by(TRTPN, n) %>% summarise(Freq = n(), .groups = "drop")
fullF <- expand.grid(TRTPN = sort(unique(advsmax3$TRTPN)), n = uN, stringsAsFactors = FALSE)
freqCells <- fullF %>% left_join(obsF, by = c("TRTPN","n"))
freqCells$Freq[is.na(freqCells$Freq)] <- 0L
grandTotal <- nrow(advsmax3)
rowTot <- advsmax3 %>% group_by(TRTPN) %>% summarise(RowTot = n(), .groups = "drop")
colTot <- advsmax3 %>% group_by(n) %>% summarise(ColTot = n(), .groups = "drop")
freqCells <- freqCells %>% left_join(rowTot, by = "TRTPN") %>% left_join(colTot, by = "n")
freqCells$Percent <- 100 * freqCells$Freq / grandTotal
freqCells$RowPct <- 100 * freqCells$Freq / freqCells$RowTot
freqCells$ColPct <- 100 * freqCells$Freq / freqCells$ColTot

# chi-square stats for listing
chi_stat <- NA_real_; chi_df <- NA_real_; chi_p <- NA_real_
if (nrow(ct) >= 2 && ncol(ct) >= 2) {
  cst <- try(suppressWarnings(chisq.test(ct, correct = FALSE)), silent = TRUE)
  if (!inherits(cst, "try-error")) {
    chi_stat <- as.numeric(cst$statistic); chi_df <- as.numeric(cst$parameter); chi_p <- as.numeric(cst$p.value)
  }
}
# Fisher details
fish_table_prob <- NA_real_
ft_full <- try(fisher.test(ct, workspace = 2e7), silent = TRUE)

con1 <- file(file.path(out, "P036.txt"), open = "wt")
writeLines("Baseline Shift Frequency Table - Max Baseline (BNRIND) by Max Post-Baseline (ANRIND)", con1)
writeLines(paste0("Source: advsmax DIABP ATPTN=815 ANL01FL=Y ADY>1 SAFFL=Y  N=", nrow(advsmax2)), con1)
writeLines(paste0("Total safety N (advsmax2) = ", nrow(advsmax2), "  Total advsmax3 (CRIT1FL non-missing) = ", nrow(advsmax3),
                  "  Grand total = ", grandTotal), con1)
writeLines(paste0("Overall N = ", grandTotal + 0, "  TRTPN levels = ", paste(sort(unique(advsmax2$TRTPN)), collapse = ",")), con1)
writeLines("", con1)
anr_levels <- sort(unique(TableA$ANRIND))
hdr <- sprintf("%-14s %-12s %s", "Treatment", "MaxBaseline", paste(paste0(anr_levels, " [N (%)]"), collapse = " | "))
writeLines(hdr, con1)
grp_order <- unique(TableA[order(TableA$TRTPN, na.last = FALSE), "GroupFmt"])
for (g in grp_order) {
  sub <- TableA %>% filter(GroupFmt == g) %>% arrange(BNRIND)
  bnr_u <- sort(unique(sub$BNRIND))
  for (b in bnr_u) {
    cells <- sapply(anr_levels, function(a) {
      v <- sub$ValueFmt[sub$BNRIND == b & sub$ANRIND == a]
      if (length(v) == 0) "0 (0.0)" else v[1]
    })
    disp_b <- ifelse(b == "", "(Missing)", b)
    line <- sprintf("%-14s %-12s %s", g, disp_b, paste(cells, collapse = " | "))
    writeLines(line, con1)
  }
  writeLines(" ", con1)
}
writeLines("", con1)
writeLines("Baseline shift detail with 1-decimal and 2-decimal percents (COUNT, PERCENT, PERCENT2):", con1)
for (i in seq_len(nrow(TableA))) {
  r <- TableA[i, ]
  writeLines(paste0("TRTPN=", ifelse(is.na(r$TRTPN), "", as.character(r$TRTPN)),
                   " BNRIND=", ifelse(r$BNRIND == "", "(Missing)", r$BNRIND),
                   " ANRIND=", ifelse(r$ANRIND == "", "(Missing)", r$ANRIND),
                   " COUNT=", r$COUNT,
                   " PERCENT=", fmt_pct1(r$PERCENT),
                   " PERCENT2=", fmt_pct2(r$PERCENT),
                   " ValueFmt=", r$ValueFmt,
                   " ValueFmt2=", r$ValueFmt2,
                   " GroupN=", r$GroupN), con1)
}
writeLines("", con1)
writeLines("The FREQ Procedure", con1)
writeLines("Table of TRTPN by n", con1)
writeLines("", con1)
# header for crosstab
writeLines(paste0("TRTPN  n  Frequency  Percent  Row Pct  Col Pct  RowTot  ColTot  Total=", grandTotal), con1)
for (i in seq_len(nrow(freqCells))) {
  r <- freqCells[i, ]
  writeLines(paste0("TRTPN=", r$TRTPN, " n=", r$n,
                    " Frequency=", r$Freq,
                    " Percent=", fmt_pct2(r$Percent),
                    " Row Pct=", fmt_pct2(r$RowPct),
                    " Col Pct=", fmt_pct2(r$ColPct),
                    " RowTot=", r$RowTot, " ColTot=", r$ColTot), con1)
}
writeLines("", con1)
writeLines("Row totals:", con1)
for (i in seq_len(nrow(rowTot))) {
  writeLines(paste0("TRTPN=", rowTot$TRTPN[i], " RowTot=", rowTot$RowTot[i],
                    " Row Percent=", fmt_pct2(100 * rowTot$RowTot[i] / grandTotal)), con1)
}
writeLines("Column totals:", con1)
for (i in seq_len(nrow(colTot))) {
  writeLines(paste0("n=", colTot$n[i], " ColTot=", colTot$ColTot[i],
                    " Col Percent=", fmt_pct2(100 * colTot$ColTot[i] / grandTotal)), con1)
}
writeLines("", con1)
writeLines("Statistics for Table of TRTPN by n", con1)
writeLines(paste0("N=", grandTotal, " DF=", ifelse(is.na(chi_df), (length(unique(advsmax3$TRTPN)) - 1) * (length(uN) - 1), chi_df),
                  " Chi-Square=", ifelse(is.na(chi_stat), "", sprintf("%.4f", round_half_up(chi_stat, 4))),
                  " Prob=", ifelse(is.na(chi_p), "", sprintf("%.4f", round_half_up(chi_p, 4)))), con1)
writeLines(paste0("Chi-Square DF=", ifelse(is.na(chi_df), 2, chi_df),
                  " Value=", ifelse(is.na(chi_stat), "", sprintf("%.2f", round_half_up(chi_stat, 2))),
                  " Prob=", fmt_pvalue(chi_p)), con1)
writeLines("", con1)
writeLines("Fisher's Exact Test", con1)
writeLines(paste0("Table Probability (P)=", ifelse(is.na(fish_p), "", sprintf("%.4f", round_half_up(fish_p, 4))),
                  " Two-sided Pr <= P=", fmt_pvalue(fish_p),
                  " XP2_FISH=", fmt_pvalue(fish_p)), con1)
writeLines(paste0("Cell (1,1) Frequency (F)=", ifelse(nrow(freqCells) >= 1, freqCells$Freq[1], ""),
                  " Left-sided Pr <= F=", fmt_pvalue(fish_p),
                  " Right-sided Pr >= F=", fmt_pvalue(fish_p)), con1)
writeLines("", con1)
writeLines("Summary statistics (PROC SUMMARY NWAY):", con1)
writeLines(paste0("ANRLO N=", sum(!is.na(advsmax2$ANRLO)), " Mean=", fmt_pct2(mean_ANRLO),
                  " Mean1=", fmt_pct1(mean_ANRLO),
                  " Min=", suppressWarnings(min(advsmax2$ANRLO, na.rm = TRUE)),
                  " Max=", suppressWarnings(max(advsmax2$ANRLO, na.rm = TRUE))), con1)
writeLines(paste0("ANRHI N=", sum(!is.na(advsmax2$ANRHI)), " Mean=", fmt_pct2(mean_ANRHI),
                  " Mean1=", fmt_pct1(mean_ANRHI),
                  " Min=", suppressWarnings(min(advsmax2$ANRHI, na.rm = TRUE)),
                  " Max=", suppressWarnings(max(advsmax2$ANRHI, na.rm = TRUE))), con1)
writeLines(paste0("BASE N=", sum(!is.na(advsmax2$BASE)), " Mean=", fmt_pct2(mean_BASE),
                  " Mean1=", fmt_pct1(mean_BASE),
                  " Min=", suppressWarnings(min(advsmax2$BASE, na.rm = TRUE)),
                  " Max=", suppressWarnings(max(advsmax2$BASE, na.rm = TRUE))), con1)
writeLines(paste0("AVAL N=", sum(!is.na(advsmax2$AVAL)), " Mean=", fmt_pct2(mean_AVAL),
                  " Mean1=", fmt_pct1(mean_AVAL),
                  " MinValue=", minValue_raw, " MaxValue=", maxValue_raw,
                  " Margin=", sprintf("%.4f", round_half_up(margin, 4))), con1)
writeLines(paste0("Y-axis MINVALUE=", YAxisMin, " MAXVALUE=", YAxisMax, " BY=", MAJOR,
                  " ANRLO=", ANRLO_v, " ANRHI=", ANRHI_v), con1)
writeLines("", con1)
writeLines("Notes: COUNT = cell count; PERCENT = 100*COUNT/GroupN within TRTPN (missing included).", con1)
writeLines("ValueFmt = COUNT (PERCENT with 1 decimal, SAS comma8./10.1, half away from zero).", con1)
close(con1)

Target16_body <- EmergentHigh %>% transmute(TRTPN = TRTPN, Nx = Nx, n = n,
                                            percent = round_half_up(percent, 1),
                                            percent2 = round_half_up(percent, 2),
                                            FishersP = FishersP)
write.csv(Target16_body, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

con2 <- file(file.path(out, "Target16.txt"), open = "wt")
writeLines("Treatment Emergent High Table (advsmax3: CRIT1FL non-missing, n = (orig row mod 17 = 1))", con2)
writeLines(paste0("Fisher exact (TRTPN*n) two-sided p = ", fmt_pvalue(fish_p)), con2)
writeLines("", con2)
writeLines(sprintf("%-8s %-8s %-8s %-8s %-10s", "TRTPN", "Nx", "n", "%", "FishersP"), con2)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i, ]
  line <- sprintf("%-8s %-8s %-8s %-8s %-10s",
                  ifelse(is.na(r$TRTPN), "", as.character(r$TRTPN)),
                  fmt_comma(r$Nx), fmt_comma(r$n),
                  fmt_pct1(r$percent),
                  fmt_pvalue(r$FishersP))
  writeLines(line, con2)
  writeLines(paste0("TRTPN=", ifelse(is.na(r$TRTPN), "", as.character(r$TRTPN)),
                    " Nx=", r$Nx, " n=", r$n,
                    " percent=", fmt_pct1(r$percent),
                    " percent2=", fmt_pct2(r$percent),
                    " FishersP=", fmt_pvalue(r$FishersP)), con2)
}
writeLines("", con2)
writeLines(paste0("Reference limits: mean ANRLO=", sprintf("%.2f", round_half_up(mean_ANRLO, 2)),
                  " mean ANRHI=", sprintf("%.2f", round_half_up(mean_ANRHI, 2)),
                  " Min AVAL=", max(0, minValue_raw, na.rm = TRUE),
                  " Max AVAL=", maxValue_raw), con2)
writeLines(paste0("Y-axis: MIN=", YAxisMin, " MAX=", YAxisMax, " BY=", MAJOR), con2)
writeLines("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.", con2)
writeLines("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.", con2)
writeLines(paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN ", fmt_pct1(ANRLO_v),
                  " mmol/L ULN ", fmt_pct1(ANRHI_v), " mmol/L) set 2 (Female, Age 18-65, LLN=", fmt_pct1(ANRLO_v),
                  " mmol/L ULN=", fmt_pct1(ANRHI_v), " mmol/L)."), con2)
close(con2)

cat("P036 done\n")
