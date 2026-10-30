```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  fac <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * fac + 0.5 + 1e-9) / fac
  y * s
}
comma_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    formatC(as.integer(v), format = "d", big.mark = ",")
  }, USE.NAMES = FALSE)
}
trunc1 <- function(x) {
  floor(x * 10 + 1e-9) / 10
}
percpar_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("(%.1f)", trunc1(v))
  }, USE.NAMES = FALSE)
}
pvalue_fmt <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

src <- file.path(rawadam, "advsmax.xpt")
advs_raw <- haven::read_xpt(src)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
names(advs_raw) <- toupper(names(advs_raw))
for (nm in names(advs_raw)) {
  v <- advs_raw[[nm]]
  if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
  advs_raw[[nm]] <- as.vector(v)
}
stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL") %in% names(advs_raw)))

advsmax <- advs_raw %>%
  mutate(
    ATPTN_N = suppressWarnings(as.numeric(ATPTN)),
    ADY_N = suppressWarnings(as.numeric(ADY)),
    PARAMCD_C = as.character(PARAMCD),
    ANL01FL_C = as.character(ANL01FL),
    SAFFL_C = as.character(SAFFL)
  ) %>%
  filter(PARAMCD_C == "DIABP", ATPTN_N == 815, ANL01FL_C == "Y", ADY_N > 1, SAFFL_C == "Y")

advsmax2 <- advsmax %>%
  transmute(
    USUBJID = as.character(USUBJID),
    TRTPN = suppressWarnings(as.numeric(TRTPN)),
    PARAM = as.character(PARAM),
    PARAMCD = as.character(PARAMCD),
    AVAL = suppressWarnings(as.numeric(AVAL)),
    ANL01FL = as.character(ANL01FL),
    BNRIND = as.character(BNRIND),
    ANRIND = as.character(ANRIND),
    SHIFT1 = if ("SHIFT1" %in% names(advs_raw)) as.character(SHIFT1) else NA_character_,
    CRIT1FL = if ("CRIT1FL" %in% names(advs_raw)) as.character(CRIT1FL) else NA_character_,
    ANRLO = suppressWarnings(as.numeric(ANRLO)),
    ANRHI = suppressWarnings(as.numeric(ANRHI)),
    BASE = suppressWarnings(as.numeric(BASE))
  )
advsmax2$BNRIND[advsmax2$BNRIND == ""] <- NA_character_
advsmax2$ANRIND[advsmax2$ANRIND == ""] <- NA_character_
advsmax2$CRIT1FL[advsmax2$CRIT1FL == ""] <- NA_character_
advsmax2$Ref <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
advsmax2 <- advsmax2 %>% arrange(TRTPN, USUBJID)

trtpn_levels <- sort(unique(advsmax2$TRTPN[!is.na(advsmax2$TRTPN)]))
b_lv <- sort(unique(advsmax2$BNRIND[!is.na(advsmax2$BNRIND)]))
a_lv <- sort(unique(advsmax2$ANRIND[!is.na(advsmax2$ANRIND)]))
if (length(b_lv) == 0) b_lv <- NA_character_
if (length(a_lv) == 0) a_lv <- NA_character_

totals <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")

grid <- expand.grid(TRTPN = trtpn_levels, BNRIND = b_lv, ANRIND = a_lv, stringsAsFactors = FALSE)
counts_obs <- advsmax2 %>%
  filter(!is.na(BNRIND), !is.na(ANRIND)) %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(COUNT = n(), .groups = "drop")
TableA <- grid %>%
  left_join(counts_obs, by = c("TRTPN","BNRIND","ANRIND")) %>%
  mutate(COUNT = ifelse(is.na(COUNT), 0L, COUNT)) %>%
  left_join(totals, by = "TRTPN") %>%
  mutate(PERCENT = ifelse(GroupN > 0, COUNT / GroupN * 100, 0))
has_miss_b <- any(is.na(advsmax2$BNRIND))
has_miss_a <- any(is.na(advsmax2$ANRIND))
if (has_miss_b || has_miss_a) {
  miss_rows <- advsmax2 %>% filter(is.na(BNRIND) | is.na(ANRIND)) %>%
    group_by(TRTPN, BNRIND, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")
  miss_rows <- miss_rows %>% left_join(totals, by = "TRTPN") %>%
    mutate(PERCENT = ifelse(GroupN > 0, COUNT / GroupN * 100, 0))
  TableA <- bind_rows(TableA, miss_rows)
}
TableA <- TableA %>% arrange(TRTPN, BNRIND, ANRIND)
TableA$ValueFmt <- paste0(comma_fmt(TableA$COUNT), " (", sprintf("%.1f", round_half_up(TableA$PERCENT, 1)), ")")
TableA$GroupFmt <- paste0(trimws(as.character(TableA$TRTPN)), " (N = ", comma_fmt(TableA$GroupN), ")")
TableA$GroupPercent <- TableA$GroupN / sum(TableA$GroupN[!duplicated(TableA$TRTPN)]) * 100

advsmax3 <- advsmax2 %>% filter(!is.na(CRIT1FL) & CRIT1FL != "")
advsmax3 <- advsmax3 %>% arrange(TRTPN, USUBJID)
advsmax3$n <- as.integer(((seq_len(nrow(advsmax3)) - 1) %% 17) == 0)

EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(Nx = sum(!is.na(n)), n = sum(n, na.rm = TRUE), percent_mean = mean(n, na.rm = TRUE), .groups = "drop") %>%
  rename(percent = percent_mean) %>%
  arrange(TRTPN)

ct_mat <- table(factor(advsmax3$TRTPN, levels = trtpn_levels), factor(advsmax3$n, levels = c(0, 1)))
ct_mat <- as.matrix(ct_mat)
Ntot <- sum(ct_mat)
ft <- suppressWarnings(fisher.test(ct_mat, workspace = 2000000))
FishersP_val <- ft$p.value
rowT <- rowSums(ct_mat); colT <- colSums(ct_mat)
logP <- sum(lfactorial(rowT)) + sum(lfactorial(colT)) - lfactorial(Ntot) - sum(lfactorial(ct_mat))
TableProb <- as.numeric(exp(logP))

EmergentHigh$FishersP <- NA_real_
if (nrow(EmergentHigh) >= 1) EmergentHigh$FishersP[1] <- FishersP_val
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), EmergentHigh$percent * 100, NA_real_)

ANRLO_m <- suppressWarnings(mean(advsmax2$ANRLO, na.rm = TRUE))
ANRHI_m <- suppressWarnings(mean(advsmax2$ANRHI, na.rm = TRUE))
MinValue <- suppressWarnings(min(advsmax2$AVAL, na.rm = TRUE))
MaxValue <- suppressWarnings(max(advsmax2$AVAL, na.rm = TRUE))
MAJOR <- 10
margin <- (MaxValue - MinValue) / 100
YAxisMin <- MAJOR * floor((MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((MaxValue + margin) / MAJOR)

plot_df <- advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, Ref, BNRIND, ANRIND, ANRLO, ANRHI)
plot_df$TRTPN_f <- factor(plot_df$TRTPN, levels = trtpn_levels)

cols_map <- c("green", "red", "blue")
shapes_map <- c(2, 1, 4)
nlev <- length(trtpn_levels)
use_cols <- rep(cols_map, length.out = nlev)
names(use_cols) <- as.character(trtpn_levels)
use_shapes <- rep(shapes_map, length.out = nlev)
names(use_shapes) <- as.character(trtpn_levels)

p <- ggplot(plot_df, aes(x = BASE, y = AVAL, color = TRTPN_f, shape = TRTPN_f)) +
  geom_point(size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "solid") +
  geom_hline(yintercept = ANRLO_m, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = ANRHI_m, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ANRLO_m, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = ANRHI_m, color = "red", linetype = "dashed") +
  scale_color_manual(values = use_cols, name = "TRTPN") +
  scale_shape_manual(values = use_shapes, name = "TRTPN") +
  coord_cartesian(ylim = c(YAxisMin, YAxisMax), xlim = c(YAxisMin, YAxisMax)) +
  scale_y_continuous(breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(size = 11, face = "bold"))

ggsave(file.path(out, "sasgraph.png"), plot = p, width = 8, height = 6, dpi = 150)
write.csv(plot_df %>% select(USUBJID, TRTPN, BASE, AVAL, Ref, BNRIND, ANRIND, ANRLO, ANRHI),
          file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

rowTotals <- rowSums(ct_mat); colTotals <- colSums(ct_mat)
cell_pct <- ct_mat / Ntot * 100
cell_rowpct <- sweep(ct_mat, 1, rowTotals, "/") * 100
cell_colpct <- sweep(ct_mat, 2, colTotals, "/") * 100
cell_rowpct[is.nan(cell_rowpct)] <- 0
cell_colpct[is.nan(cell_colpct)] <- 0

freq_body <- expand.grid(TRTPN = rownames(ct_mat), n = colnames(ct_mat), stringsAsFactors = FALSE)
freq_body$COUNT <- as.vector(ct_mat)
freq_body$PERCENT <- as.vector(cell_pct)
freq_body$ROWPCT <- as.vector(cell_rowpct)
freq_body$COLPCT <- as.vector(cell_colpct)
freq_body$TRTPN <- as.numeric(freq_body$TRTPN)
freq_body$n <- as.numeric(freq_body$n)
write.csv(freq_body, file.path(out, "P053.csv"), row.names = FALSE, na = "")

con <- file(file.path(out, "P053.txt"), open = "wt")
writeLines("Table of TRTPN by n", con)
writeLines(paste0("Data: advsmax3 (CRIT1FL non-missing, n = (MOD(_N_,17)=1))  Sample Size = ", Ntot), con)
writeLines("", con)
hdr <- sprintf("%-8s %-8s %10s %10s %10s %10s", "TRTPN", "n", "Frequency", "Percent", "Row Pct", "Col Pct")
writeLines(hdr, con)
for (i in seq_len(nrow(ct_mat))) {
  for (j in seq_len(ncol(ct_mat))) {
    line <- sprintf("%-8s %-8s %10d %10s %10s %10s",
      rownames(ct_mat)[i], colnames(ct_mat)[j],
      ct_mat[i, j],
      sprintf("%.2f", round_half_up(cell_pct[i, j], 2)),
      sprintf("%.2f", round_half_up(cell_rowpct[i, j], 2)),
      sprintf("%.2f", round_half_up(cell_colpct[i, j], 2)))
    writeLines(line, con)
  }
}
writeLines("", con)
writeLines(sprintf("%-16s %10d %10s", "Total", Ntot, "100.00"), con)
writeLines("", con)
writeLines("Row Totals:", con)
for (i in seq_len(nrow(ct_mat))) writeLines(sprintf(" TRTPN=%s  Frequency=%d  Percent=%s", rownames(ct_mat)[i], rowTotals[i], sprintf("%.2f", round_half_up(rowTotals[i]/Ntot*100,2))), con)
writeLines("Column Totals:", con)
for (j in seq_len(ncol(ct_mat))) writeLines(sprintf(" n=%s  Frequency=%d  Percent=%s", colnames(ct_mat)[j], colTotals[j], sprintf("%.2f", round_half_up(colTotals[j]/Ntot*100,2))), con)
writeLines("", con)
writeLines("Fisher's Exact Test", con)
writeLines(sprintf("Table Probability (P)  %s", sprintf("%.4f", round_half_up(TableProb,4))), con)
writeLines(sprintf("Pr <= P  %s", pvalue_fmt(FishersP_val)), con)
writeLines(sprintf("Sample Size = %d", Ntot), con)
close(con)

t16csv <- TableA %>% select(GroupFmt, TRTPN, GroupN, BNRIND, ANRIND, COUNT, PERCENT, ValueFmt)
write.csv(t16csv, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

con2 <- file(file.path(out, "Target16.txt"), open = "wt")
writeLines("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", con2)
writeLines(sprintf("Reference limits: ANRLO=%s  ANRHI=%s  YAxisMin=%d  YAxisMax=%d  MAJOR=%d  MinValue=%s  MaxValue=%s",
  sprintf("%.4f", ANRLO_m), sprintf("%.4f", ANRHI_m), YAxisMin, YAxisMax, MAJOR,
  sprintf("%.4f", MinValue), sprintf("%.4f", MaxValue)), con2)
writeLines("", con2)
writeLines("Max Post Basline Result (shift from baseline)", con2)
a_levels_sorted <- sort(unique(TableA$ANRIND[!is.na(TableA$ANRIND)]))
hdr2 <- paste0(sprintf("%-18s %-18s", "Treatment", "Max. Baseline Result"),
  paste0(sprintf(" | %-22s", paste0("ANRIND=", a_levels_sorted, " N (%)")), collapse = ""))
writeLines(hdr2, con2)
ord <- TableA %>% arrange(TRTPN, BNRIND, ANRIND)
grp_b <- unique(ord[, c("GroupFmt","BNRIND")])
for (k in seq_len(nrow(grp_b))) {
  gf <- grp_b$GroupFmt[k]; bb <- grp_b$BNRIND[k]
  sub <- ord %>% filter(GroupFmt == gf, (if (is.na(bb)) is.na(BNRIND) else (!is.na(BNRIND) & BNRIND == bb)))
  cells <- sapply(a_levels_sorted, function(al) {
    r <- sub %>% filter(!is.na(ANRIND) & ANRIND == al)
    if (nrow(r) == 0) return(sprintf("%4d %s", 0, percpar_fmt(0)))
    sprintf("%4d %s", r$COUNT[1], percpar_fmt(r$PERCENT[1]))
  })
  disp_b <- if (is.na(bb)) "" else bb
  writeLines(paste0(sprintf("%-18s %-18s", gf, disp_b), paste0(sprintf(" | %-22s", cells), collapse = "")), con2)
  for (kk in seq_len(nrow(sub))) {
    writeLines(sprintf("  detail: %s / %s / %s COUNT=%d PERCENT=%s ValueFmt=%s",
      sub$GroupFmt[kk], ifelse(is.na(sub$BNRIND[kk]),"(Missing)",sub$BNRIND[kk]),
      ifelse(is.na(sub$ANRIND[kk]),"(Missing)",sub$ANRIND[kk]),
      sub$COUNT[kk], sprintf("%.1f", round_half_up(sub$PERCENT[kk],1)), sub$ValueFmt[kk]), con2)
  }
}
writeLines("", con2)
writeLines("Treatment Emergent High", con2)
writeLines(sprintf("%-8s %-8s %-8s %-8s %-10s", "TRTPN", "Nx", "n", "%", "FishersP"), con2)
for (i in seq_len(nrow(EmergentHigh))) {
  writeLines(sprintf("%-8s %8d %8d %8s %10s",
    as.character(EmergentHigh$TRTPN[i]),
    EmergentHigh$Nx[i], EmergentHigh$n[i],
    sprintf("%.1f", round_half_up(EmergentHigh$percent[i],1)),
    pvalue_fmt(EmergentHigh$FishersP[i])), con2)
}
writeLines(sprintf("Fisher exact Pr <= P = %s  Table N = %d", pvalue_fmt(FishersP_val), Ntot), con2)
writeLines("", con2)
writeLines("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot", con2)
writeLines("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.", con2)
writeLines(sprintf("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN%s mmol/L ULN%s mmol/L) set 2 (Female, Age 18-65, LLN=%s mmol/L ULN=%s mmol/L).",
  sprintf("%.4f", ANRLO_m), sprintf("%.4f", ANRHI_m), sprintf("%.4f", ANRLO_m), sprintf("%.4f", ANRHI_m)), con2)
close(con2)
```