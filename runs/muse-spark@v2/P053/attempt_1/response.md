Rebuilding your scatter-plot outlier review — tables, exact test, and reference-line figure.```r
# P053_outliers_Scatter_Quant.sas -> R translation
# Outputs: P053 (FREQ listing), Target16 (REPORT tables), sasgraph.png

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(ggplot2)
})

cat("P053 start\n")

## ---------- helpers: SAS semantics ----------
sas_round <- function(x, digits = 0) {
  # round half away from zero
  y <- sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits
  y[x == 0 | is.na(x)] <- 0
  # preserve NA
  y[is.na(x)] <- NA_real_
  y
}
fmt_comma <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    prettyNum(round(v), big.mark = ",", scientific = FALSE, trim = TRUE)
  }, USE.NAMES = FALSE)
}
fmt_10_1 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.1f", sas_round(v, 1))
  }, USE.NAMES = FALSE)
}
fmt_6_1 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%6.1f", sas_round(v, 1))
  }, USE.NAMES = FALSE)
}
fmt_2dec <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.2f", sas_round(v, 2))
  }, USE.NAMES = FALSE)
}
fmt_4dec <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.4f", sas_round(v, 4))
  }, USE.NAMES = FALSE)
}
fmt_pvalue <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", sas_round(v, 4))
  }, USE.NAMES = FALSE)
}
# SAS character sort: missing first, uppercase before lowercase (byte order)
sas_order <- function(x) {
  # x character vector; NA treated as ""
  xx <- ifelse(is.na(x), "", x)
  # radix sort is byte-wise
  order(xx, method = "radix", na.last = FALSE)
}
sas_sort_unique <- function(x) {
  ux <- unique(ifelse(is.na(x), "", x))
  ux[sas_order(ux)]
}

## ---------- read input ----------
src_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt"
if (!file.exists(src_path)) {
  # fallback: try OUT_DIR-relative or derived libref path
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt"
  alt2 <- file.path(Sys.getenv("rawadam"), "advsmax.xpt")
  if (file.exists(alt1)) src_path <- alt1 else if (file.exists(alt2)) src_path <- alt2
}
cat("Reading:", src_path, "\n")
advsmax <- haven::read_xpt(src_path)
# normalize names to uppercase for matching (SAS case-insensitive)
names(advsmax) <- toupper(names(advsmax))
# ensure expected cols exist
stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL") %in% names(advsmax)))

## ---------- data advsmax2 ----------
# keep list
keepvars <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
              "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
keepvars <- keepvars[keepvars %in% names(advsmax)]
advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y") %>%
  select(all_of(keepvars))
# Ref = Base if both non-missing
advsmax2$REF <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
# character missing -> "" to mimic SAS ""
for (cc in c("BNRIND","ANRIND","CRIT1FL")) {
  if (cc %in% names(advsmax2)) {
    advsmax2[[cc]][is.na(advsmax2[[cc]])] <- ""
  }
}
# sort by TRTPN (missing first)
advsmax2 <- advsmax2 %>% arrange(TRTPN)
stopifnot(all(c("TRTPN","BNRIND","ANRIND") %in% names(advsmax2)))

write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names = FALSE, na = "")

## ---------- PROC FREQ TableA etc. ----------
# totals per TRTPN
trt_levels <- sort(unique(advsmax2$TRTPN))
bnr_levels <- sas_sort_unique(advsmax2$BNRIND)
anr_levels <- sas_sort_unique(advsmax2$ANRIND)

tot_trt <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")

# observed counts for BNRIND*ANRIND within TRTPN
obs <- advsmax2 %>% group_by(TRTPN, BNRIND, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")

# full cross per TRTPN (SAS OUT includes all cells)
full <- expand.grid(TRTPN = trt_levels, BNRIND = bnr_levels, ANRIND = anr_levels,
                    stringsAsFactors = FALSE)
TableA <- full %>%
  left_join(obs, by = c("TRTPN","BNRIND","ANRIND")) %>%
  mutate(COUNT = ifelse(is.na(COUNT), 0L, COUNT)) %>%
  left_join(tot_trt, by = "TRTPN") %>%
  mutate(PERCENT = ifelse(GroupN > 0, COUNT / GroupN * 100, 0)) %>%
  arrange(TRTPN, BNRIND, ANRIND)
# ValueFmt
TableA$ValueFmt <- paste0(fmt_comma(TableA$COUNT), " (", fmt_10_1(TableA$PERCENT), ")")
# TRTPN dataset (BY TRTPN, table TRTPN)
TRTPNds <- tot_trt %>% transmute(TRTPN = TRTPN, COUNT = GroupN, PERCENT = 100,
                                 GroupN = GroupN, GroupPercent = 100)
# BNRIND single-way per TRTPN
obsB <- advsmax2 %>% group_by(TRTPN, BNRIND) %>% summarise(COUNT = n(), .groups = "drop")
fullB <- expand.grid(TRTPN = trt_levels, BNRIND = bnr_levels, stringsAsFactors = FALSE)
BNRINDds <- fullB %>% left_join(obsB, by = c("TRTPN","BNRIND")) %>%
  mutate(COUNT = ifelse(is.na(COUNT), 0L, COUNT)) %>%
  left_join(tot_trt, by = "TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100)
# ANRIND single-way
obsA <- advsmax2 %>% group_by(TRTPN, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")
fullA <- expand.grid(TRTPN = trt_levels, ANRIND = anr_levels, stringsAsFactors = FALSE)
ARINDds <- fullA %>% left_join(obsA, by = c("TRTPN","ANRIND")) %>%
  mutate(COUNT = ifelse(is.na(COUNT), 0L, COUNT)) %>%
  left_join(tot_trt, by = "TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100)

# MERGE ON GROUP NS
TableA <- TableA %>%
  left_join(TRTPNds %>% select(TRTPN, GroupN, GroupPercent), by = "TRTPN") %>%
  mutate(GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", fmt_comma(GroupN), ")"))

write.csv(TableA, file.path(out, "TableA.csv"), row.names = FALSE, na = "")

# PROC TRANSPOSE out=TableAt prefix=ValueFmt ; by GroupFmt BNRIND ; var ValueFmt ; id ANRIND
# Build wide
TableAt_long <- TableA %>% select(GroupFmt, TRTPN, GroupN, BNRIND, ANRIND, ValueFmt, COUNT, PERCENT)
TableAt <- TableAt_long %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from = ANRIND, values_from = ValueFmt, names_prefix = "ValueFmt_",
              values_fill = "")
# order by GroupFmt/TRTPN and BNRIND SAS order
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names = FALSE, na = "")

## ---------- Treatment emergent high ----------
advsmax3 <- advsmax2 %>% filter(CRIT1FL != "")
# n = mod(_n_,17)=1  (_n_ is row order in advsmax2 sorted order; advsmax3 preserves that order)
advsmax3 <- advsmax3 %>% arrange(TRTPN)
advsmax3$n <- as.integer((seq_len(nrow(advsmax3)) %% 17) == 1)
stopifnot("TRTPN" %in% names(advsmax3), "n" %in% names(advsmax3))
write.csv(advsmax3, file.path(out, "advsmax3.csv"), row.names = FALSE, na = "")

# PROC SUMMARY nway class TRTPN var n -> Nx(sum n mean)
EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>%
  summarise(Nx = sum(!is.na(n)), n = sum(n, na.rm = TRUE),
            percent = mean(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(TRTPN)

# PROC FREQ TRTPN*n / exact  -> Fisher
# contingency matrix rows=TRTPN sorted, cols n=0,1
ct <- as.data.frame(table(factor(advsmax3$TRTPN, levels = trt_levels),
                          factor(advsmax3$n, levels = c(0,1))))
# reshape to matrix
mat <- matrix(0, nrow = length(trt_levels), ncol = 2)
colnames(mat) <- c("0","1"); rownames(mat) <- as.character(trt_levels)
for (i in seq_len(nrow(ct))) {
  r <- match(as.character(ct[i,1]), rownames(mat))
  c2 <- match(as.character(ct[i,2]), colnames(mat))
  mat[r, c2] <- ct[i,3]
}
# Fisher exact (Freeman-Halton for RxC)
ft <- fisher.test(mat)
fish_p <- ft$p.value
# Table Probability (P) for observed table given margins
rowT <- rowSums(mat); colT <- colSums(mat); Ntot <- sum(mat)
logP <- sum(lfactorial(rowT)) + sum(lfactorial(colT)) - lfactorial(Ntot) - sum(lfactorial(mat))
tableProb <- exp(logP)

FishersExact <- data.frame(Name1 = "XP2_FISH", nValue1 = fish_p, stringsAsFactors = FALSE)
write.csv(FishersExact, file.path(out, "FishersExact.csv"), row.names = FALSE, na = "")
PValue <- data.frame(FishersP = fish_p)
# merge without BY: first obs gets p-value
EmergentHigh <- EmergentHigh %>% arrange(TRTPN)
EmergentHigh$FishersP <- c(fish_p, rep(NA_real_, nrow(EmergentHigh)-1))
# percent*100
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), EmergentHigh$percent * 100, NA_real_)
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")

## ---------- PROC SUMMARY for axis ----------
MAJOR <- 10
ADVSMAXSum <- data.frame(
  ANRLO = mean(advsmax2$ANRLO, na.rm = TRUE),
  ANRHI = mean(advsmax2$ANRHI, na.rm = TRUE),
  BASE_m = mean(advsmax2$BASE, na.rm = TRUE),
  AVAL_m = mean(advsmax2$AVAL, na.rm = TRUE),
  MinValue = suppressWarnings(min(advsmax2$AVAL, na.rm = TRUE)),
  MaxValue = suppressWarnings(max(advsmax2$AVAL, na.rm = TRUE))
)
write.csv(ADVSMAXSum, file.path(out, "ADVSMAXSum.csv"), row.names = FALSE, na = "")
ANRLO_v <- ADVSMAXSum$ANRLO[1]
ANRHI_v <- ADVSMAXSum$ANRHI[1]
MinV <- ADVSMAXSum$MinValue[1]; MaxV <- ADVSMAXSum$MaxValue[1]
margin <- (MaxV - MinV) / 100
YAxisMin <- MAJOR * floor((MinV - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((MaxV + margin) / MAJOR)

## ---------- Graph ----------
plot_df <- advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, REF, BNRIND, ANRIND)
plot_df$TRTPN_f <- factor(plot_df$TRTPN, levels = trt_levels)
# colors/shapes: TRTPN order -> green triangle, red circle, blue X
col_vals <- c("green","red","blue")
shp_vals <- c(2,16,4)
# extend if more levels
if (length(trt_levels) > 3) {
  col_vals <- rep(c("green","red","blue"), length.out = length(trt_levels))
  shp_vals <- rep(c(2,16,4), length.out = length(trt_levels))
}
names(col_vals) <- as.character(trt_levels)
names(shp_vals) <- as.character(trt_levels)
p <- ggplot(plot_df, aes(x = BASE, y = AVAL, color = TRTPN_f, shape = TRTPN_f)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = col_vals, name = "TRTPN") +
  scale_shape_manual(values = shp_vals, name = "TRTPN") +
  geom_hline(yintercept = ANRLO_v, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = ANRHI_v, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ANRLO_v, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = ANRHI_v, color = "red", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax),
                     breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(size = 11))
ggsave(file.path(out, "sasgraph.png"), p, width = 9, height = 6, dpi = 150)
write.csv(plot_df %>% select(USUBJID, TRTPN, BASE, AVAL, REF),
          file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

## ---------- P053 listing: PROC FREQ TRTPN*n ----------
# Compute Frequency, Percent, RowPct, ColPct
Ntotal <- nrow(advsmax3)
rowTotals <- setNames(rowSums(mat), rownames(mat))
colTotals <- setNames(colSums(mat), colnames(mat))
cell_df <- expand.grid(TRTPN = rownames(mat), n = colnames(mat), stringsAsFactors = FALSE)
cell_df$Frequency <- as.vector(t(mat))  # careful ordering: expand.grid varies first fastest
# expand.grid order: TRTPN varies fastest? Actually first var fastest. t(mat) row-major?
# Build explicitly:
cell_df <- data.frame(TRTPN = rep(rownames(mat), each = ncol(mat)),
                      n = rep(colnames(mat), times = nrow(mat)),
                      Frequency = as.vector(t(mat)), stringsAsFactors = FALSE)
cell_df$Percent <- cell_df$Frequency / Ntotal * 100
cell_df$RowPct <- mapply(function(r,f) f/rowTotals[[as.character(r)]]*100, cell_df$TRTPN, cell_df$Frequency)
cell_df$ColPct <- mapply(function(c2,f) f/colTotals[[as.character(c2)]]*100, cell_df$n, cell_df$Frequency)
# handle 0/0
cell_df$RowPct[is.nan(cell_df$RowPct)] <- 0
cell_df$ColPct[is.nan(cell_df$ColPct)] <- 0

# Text rendering P053.txt (and .lst copy)
# Include full FREQ blocks: crosstab + Fisher
con <- file(file.path(out, "P053.txt"), open = "wt")
writeLines("Table of TRTPN by n", con)
writeLines("Data: advsmax3 (CRIT1FL non-missing; n = (mod(_n_,17)=1))", con)
writeLines("", con)
# header
writeLines(sprintf("%-10s %-4s %10s %10s %10s %10s", "TRTPN","n","Frequency","Percent","Row Pct","Col Pct"), con)
for (i in seq_len(nrow(cell_df))) {
  writeLines(sprintf("%-10s %-4s %10d %10s %10s %10s",
                     cell_df$TRTPN[i], cell_df$n[i], cell_df$Frequency[i],
                     fmt_2dec(cell_df$Percent[i]), fmt_2dec(cell_df$RowPct[i]), fmt_2dec(cell_df$ColPct[i])), con)
}
writeLines("", con)
writeLines(sprintf("%-10s %-4s %10d %10s", "Total","", Ntotal, fmt_2dec(100)), con)
writeLines("", con)
for (rr in rownames(mat)) {
  writeLines(sprintf("Row total TRTPN=%s : %d (%s%%)", rr, rowTotals[[rr]], fmt_2dec(rowTotals[[rr]]/Ntotal*100)), con)
}
for (cc in colnames(mat)) {
  writeLines(sprintf("Column total n=%s : %d (%s%%)", cc, colTotals[[cc]], fmt_2dec(colTotals[[cc]]/Ntotal*100)), con)
}
writeLines("", con)
writeLines("Fisher's Exact Test", con)
writeLines(sprintf("%-25s %10s", "Cell (1,1) Frequency (F)", as.character(mat[1,1])), con)
writeLines(sprintf("%-25s %10s", "Table Probability (P)", fmt_4dec(tableProb)), con)
writeLines(sprintf("%-25s %10s", "Pr <= P (XP2_FISH)", fmt_pvalue(fish_p)), con)
writeLines(sprintf("%-25s %10s", "Two-sided p-value", fmt_pvalue(fish_p)), con)
writeLines("", con)
writeLines(sprintf("Sample Size = %d", Ntotal), con)
close(con)
# CSV body
write.csv(cell_df, file.path(out, "P053.csv"), row.names = FALSE, na = "")
# also write .lst copy for harness
file.copy(file.path(out, "P053.txt"), file.path(out, "P053.lst"), overwrite = TRUE)

## ---------- Target16: REPORT tables ----------
# Baseline shift: for text, show GroupFmt x BNRIND x ANRIND with count(percent)
# Order: GroupFmt by TRTPN, BNRIND SAS order, ANRIND SAS order
con2 <- file(file.path(out, "Target16.txt"), open = "wt")
writeLines("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", con2)
writeLines("", con2)
writeLines("Baseline Shift Table: Max Post Baseline Result (shift from baseline)", con2)
hdr <- paste0(sprintf("%-18s %-18s", "Treatment", "Max. Baseline Result"),
              paste(sapply(anr_levels, function(a) {
                lab <- ifelse(a == "", "(missing)", a)
                sprintf(" | %-22s", paste0(lab, " N (%)"))
              }), collapse = ""))
writeLines(hdr, con2)
# subheader for N (%)
writeLines(paste0(sprintf("%-18s %-18s", "", ""),
              paste(rep(" | Count Percent", length(anr_levels)), collapse = "")), con2)
# rows: per GroupFmt/BNRIND
grp_order <- tot_trt %>% arrange(TRTPN)
for (g in grp_order$TRTPN) {
  gf <- paste0(trimws(as.character(g)), " (N = ", fmt_comma(grp_order$GroupN[grp_order$TRTPN==g]), ")")
  for (b in bnr_levels) {
    blab <- ifelse(b == "", "(missing)", b)
    line <- sprintf("%-18s %-18s", gf, blab)
    for (a in anr_levels) {
      rr <- TableA %>% filter(TRTPN == g, BNRIND == b, ANRIND == a)
      cnt <- ifelse(nrow(rr)==0, 0, rr$COUNT[1])
      pct <- ifelse(nrow(rr)==0, 0, rr$PERCENT[1])
      # percpar style: "(  9.9)" -> show as "(x.x)"
      cell <- sprintf(" | %5d (%s)", cnt, fmt_10_1(pct))
      line <- paste0(line, cell)
    }
    writeLines(line, con2)
  }
  writeLines("", con2)
}
writeLines("", con2)
writeLines("Treatment Emergent High", con2)
writeLines(sprintf("%-10s %-10s %-10s %-10s %-12s", "TRTPN","Nx","n","%","FishersP"), con2)
for (i in seq_len(nrow(EmergentHigh))) {
  writeLines(sprintf("%-10s %10d %10d %10s %12s",
                     as.character(EmergentHigh$TRTPN[i]),
                     EmergentHigh$Nx[i], EmergentHigh$n[i],
                     fmt_10_1(EmergentHigh$percent[i]),
                     fmt_pvalue(EmergentHigh$FishersP[i])), con2)
}
writeLines("", con2)
writeLines(sprintf("Fisher's Exact (XP2_FISH) p-value = %s", fmt_pvalue(fish_p)), con2)
writeLines("", con2)
writeLines("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot", con2)
writeLines("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.", con2)
writeLines(sprintf("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx %s mmol/L ULNx %s mmol/L) set 2 (Female, Age 18-65, LLN=%s mmol/L ULN=%s mmol/L).",
                   trimws(format(ANRLO_v)), trimws(format(ANRHI_v)),
                   trimws(format(ANRLO_v)), trimws(format(ANRHI_v))), con2)
writeLines(sprintf("Y-axis: %s to %s by %s; VREF/HREF at %s, %s", YAxisMin, YAxisMax, MAJOR, ANRLO_v, ANRHI_v), con2)
close(con2)

# Target16.csv: combine shift cells + emergent
shift_csv <- TableA %>% transmute(Treatment = GroupFmt, TRTPN, BNRIND, ANRIND,
                                  COUNT, PERCENT,
                                  ValueFmt)
write.csv(shift_csv, file.path(out, "Target16_shift.csv"), row.names = FALSE, na = "")
# main Target16.csv: shift + emergent stacked? Provide shift as body; also write emergent separately
em_csv <- EmergentHigh %>% transmute(TRTPN, Nx, n, percent, FishersP)
write.csv(em_csv, file.path(out, "Target16_emergent.csv"), row.names = FALSE, na = "")
# single Target16.csv required: use shift cells
write.csv(shift_csv, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

cat("P053 done\n")
```