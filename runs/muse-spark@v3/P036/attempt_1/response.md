```r
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

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x) * mult
  # SAS rounds half away from zero
  r <- floor(ax + 0.5 + 1e-9) / mult
  s * r
}

comma_fmt <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    iv <- as.integer(round_half_up(v, 0))
    prettyNum(iv, big.mark = ",", scientific = FALSE)
  }, USE.NAMES = FALSE)
}

trim_left <- function(x) sub("^\\s+", "", x)

fmt_pct_10_1 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    r <- round_half_up(v, 1)
    sprintf("%.1f", r)
  }, USE.NAMES = FALSE)
}

trunc1 <- function(x) {
  # PICTURE without ROUND truncates
  floor(x * 10 + 1e-9) / 10
}
fmt_percpar <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    t <- trunc1(v)
    sprintf("(%.1f)", t)
  }, USE.NAMES = FALSE)
}

fmt_pvalue <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    r <- round_half_up(v, 4)
    sprintf("%.4f", r)
  }, USE.NAMES = FALSE)
}

# ------------------------------------------------------------------
# Read input
# ------------------------------------------------------------------
p1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt"
p2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt"
advsmax <- NULL
if (file.exists(p1)) {
  advsmax <- haven::read_xpt(p1)
} else if (file.exists(p2)) {
  advsmax <- haven::read_xpt(p2)
} else {
  # fallback search
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "advsmax\\.xpt$",
                     recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) advsmax <- haven::read_xpt(cand[1])
}
stopifnot(!is.null(advsmax))
# normalize names to upper case as in SAS
names(advsmax) <- toupper(names(advsmax))

# ------------------------------------------------------------------
# advsmax2 : where PARAMCD=DIABP and ATPTN=815 and ANL01FL=Y and ADY>1 and SAFFL=Y
# ------------------------------------------------------------------
advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         ADY > 1,
         SAFFL == "Y")
# keep list
keepvars <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
              "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
keepvars <- intersect(keepvars, names(advsmax))
advsmax2 <- advsmax2[, keepvars, drop = FALSE]
# Ref = Base if not nmiss(base,aval)
advsmax2$REF <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL),
                       advsmax2$BASE, NA_real_)

# sort by TRTPN (SAS missing first; numeric)
advsmax2 <- advsmax2 %>% arrange(TRTPN)

# helper character versions with missing as ""
bnr_chr <- ifelse(is.na(advsmax2$BNRIND), "", advsmax2$BNRIND)
anr_chr <- ifelse(is.na(advsmax2$ANRIND), "", advsmax2$ANRIND)
advsmax2$BNRIND_C <- bnr_chr
advsmax2$ANRIND_C <- anr_chr

# ------------------------------------------------------------------
# PROC FREQ equivalents
# ------------------------------------------------------------------
# TRTPN totals (GroupN)
grpN <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")

# levels observed overall (for zero-fill like PROC FREQ)
u_trt <- sort(unique(advsmax2$TRTPN))
u_bnr <- sort(unique(bnr_chr))
u_anr <- sort(unique(anr_chr))

full_grid <- expand.grid(TRTPN = u_trt, BNRIND_C = u_bnr, ANRIND_C = u_anr,
                         stringsAsFactors = FALSE)
counts <- advsmax2 %>% group_by(TRTPN, BNRIND_C, ANRIND_C) %>%
  summarise(COUNT = n(), .groups = "drop")
TableA <- full_grid %>% left_join(counts, by = c("TRTPN","BNRIND_C","ANRIND_C"))
TableA$COUNT[is.na(TableA$COUNT)] <- 0L
TableA <- TableA %>% left_join(grpN, by = "TRTPN")
TableA$PERCENT <- ifelse(TableA$GroupN > 0, 100 * TableA$COUNT / TableA$GroupN, 0)
# Keep only rows? SAS OUT includes zero cells; keep all for transpose but for REPORT
# SAS REPORT would show zero rows as well. Keep all.
TableA <- TableA %>% arrange(TRTPN, BNRIND_C, ANRIND_C)
# ValueFmt and GroupFmt
TableA$ValueFmt <- paste0(trim_left(comma_fmt(TableA$COUNT)), " (",
                          trim_left(fmt_pct_10_1(TableA$PERCENT)), ")")
TableA$GroupPercent <- 100  # per BY TRTPN table percent=100, not used
TableA$GroupFmt <- paste0(trim_left(as.character(TableA$TRTPN)),
                          " (N = ", trim_left(comma_fmt(TableA$GroupN)), ")")
# For display rename to SAS names
TableA_out <- TableA %>%
  transmute(TRTPN, BNRIND = BNRIND_C, ANRIND = ANRIND_C,
            COUNT, PERCENT, GroupN, GroupFmt, ValueFmt)
# BNRIND / ANRIND / TRTPN one-ways (not printed, for completeness)
BNRIND_freq <- advsmax2 %>% group_by(TRTPN, BNRIND_C) %>%
  summarise(COUNT = n(), .groups = "drop")
ANRIND_freq <- advsmax2 %>% group_by(TRTPN, ANRIND_C) %>%
  summarise(COUNT = n(), .groups = "drop")

# Transpose TableAt (by GroupFmt BNRIND, id ANRIND)
TableAt_wide <- TableA_out %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  tidyr::pivot_wider(names_from = ANRIND, values_from = ValueFmt,
                     names_prefix = "ValueFmt_")

# ------------------------------------------------------------------
# Emergent high
# ------------------------------------------------------------------
# SAS subsetting IF: keep only CRIT1FL ne "" ; n = mod(_n_,17)=1 where _n_ is input row number
n_in <- nrow(advsmax2)
idx <- seq_len(n_in)
crit <- advsmax2$CRIT1FL
crit_is_missing <- is.na(crit) | crit == ""
# output rows = non-missing
keep <- !crit_is_missing
advsmax3 <- advsmax2[keep, , drop = FALSE]
n_vals_input <- as.integer((idx %% 17) == 1)
advsmax3$n <- n_vals_input[keep]

# PROC SUMMARY nway class TRTPN var n : n=Nx sum=n mean=percent
EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>%
  summarise(Nx = sum(!is.na(n)),
            n = sum(n, na.rm = TRUE),
            percent = mean(n, na.rm = TRUE) * 1, .groups = "drop") %>%
  arrange(TRTPN)
# percent is proportion; later *100
# Fisher exact TRTPN * n
cont_tab <- table(advsmax3$TRTPN, advsmax3$n)
# ensure ordering numeric
cont_tab <- cont_tab[order(as.numeric(rownames(cont_tab))),
                     order(as.numeric(colnames(cont_tab))), drop = FALSE]
fish_p <- NA_real_
try({
  ft <- fisher.test(cont_tab)
  fish_p <- ft$p.value
}, silent = TRUE)
FishersP_val <- fish_p
EmergentHigh$FishersP <- FishersP_val
# percent = 100*percent, format 6.1 on display
EmergentHigh$percent <- ifelse(is.na(EmergentHigh$percent), NA,
                               100 * EmergentHigh$percent)

# ------------------------------------------------------------------
# Summary for axes
# ------------------------------------------------------------------
ANRLO_m <- mean(advsmax2$ANRLO, na.rm = TRUE)
ANRHI_m <- mean(advsmax2$ANRHI, na.rm = TRUE)
MinValue <- min(advsmax2$AVAL, na.rm = TRUE)
MaxValue <- max(advsmax2$AVAL, na.rm = TRUE)
MAJOR <- 10
margin <- (MaxValue - MinValue) / 100
YAxisMin <- MAJOR * (floor((MinValue - margin) / MAJOR))
YAxisMax <- MAJOR * (ceiling((MaxValue + margin) / MAJOR))
ANRLO_s <- trim_left(as.character(ANRLO_m))
ANRHI_s <- trim_left(as.character(ANRHI_m))

ADVSMAXSum <- data.frame(ANRLO = ANRLO_m, ANRHI = ANRHI_m,
                         MinValue = MinValue, MaxValue = MaxValue,
                         YAxisMin = YAxisMin, YAxisMax = YAxisMax)

# ------------------------------------------------------------------
# Graph
# ------------------------------------------------------------------
# map TRTPN to symbols/colors: sorted TRTPN -> triangle/green, circle/red, x/blue
trt_levels <- sort(unique(advsmax2$TRTPN))
col_map <- c("green", "red", "blue")
pch_map <- c(2, 1, 4)  # triangle, circle, x
# extend if more levels
if (length(trt_levels) > 3) {
  col_map <- rep(c("green","red","blue"), length.out = length(trt_levels))
  pch_map <- rep(c(2,1,4), length.out = length(trt_levels))
}
advsmax2$TRTPN_f <- factor(advsmax2$TRTPN, levels = trt_levels)

p <- ggplot(advsmax2, aes(x = BASE, y = AVAL, color = TRTPN_f, shape = TRTPN_f)) +
  geom_point(size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed") +
  geom_hline(yintercept = ANRLO_m, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = ANRHI_m, color = "red", linetype = "dashed") +
  geom_vline(xintercept = ANRLO_m, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = ANRHI_m, color = "red", linetype = "dashed") +
  scale_color_manual(values = setNames(col_map[seq_along(trt_levels)], trt_levels),
                     name = "TRTPN") +
  scale_shape_manual(values = setNames(pch_map[seq_along(trt_levels)], trt_levels),
                     name = "TRTPN") +
  coord_cartesian(xlim = c(YAxisMin, YAxisMax), ylim = c(YAxisMin, YAxisMax)) +
  scale_y_continuous(breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  scale_x_continuous(breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 11, face = "bold"))

ggsave(file.path(out, "sasgraph.png"), plot = p, width = 7, height = 5, dpi = 150)

# CSV of plotted data
plot_data <- advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, REF, BNRIND_C, ANRIND_C, ANRLO, ANRHI)
names(plot_data) <- c("USUBJID","TRTPN","BASE","AVAL","Ref","BNRIND","ANRIND","ANRLO","ANRHI")
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Write datasets
# ------------------------------------------------------------------
# advsmax2 csv (SAS names)
advsmax2_csv <- advsmax2 %>% select(any_of(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","REF")))
write.csv(advsmax2_csv, file.path(out, "advsmax2.csv"), row.names = FALSE, na = "")
write.csv(TableA_out, file.path(out, "TableA.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")
write.csv(ADVSMAXSum, file.path(out, "ADVSMAXSum.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Build text renderings
# ------------------------------------------------------------------
# Baseline shift table for REPORT: rows GroupFmt x BNRIND, columns ANRIND (count + truncated pct)
anr_levels <- sort(unique(TableA_out$ANRIND))
bnr_levels <- sort(unique(TableA_out$BNRIND))
grp_levels <- unique(TableA_out$GroupFmt[order(TableA_out$TRTPN)])

# helper to get cell
get_cell <- function(g, b, a) {
  r <- TableA_out[TableA_out$GroupFmt == g & TableA_out$BNRIND == b & TableA_out$ANRIND == a, ]
  if (nrow(r) == 0) return(list(COUNT = 0, PERCENT = 0))
  list(COUNT = r$COUNT[1], PERCENT = r$PERCENT[1])
}

# Target16 footnote with substituted values
fn1 <- "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot"
fn2 <- "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure."
fn3 <- paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ",
              ANRLO_s, " mmol/L ULNx ", ANRHI_s,
              " mmol/L) set 2 (Female, Age 18-65, LLN=", ANRLO_s,
              " mmol/L ULN=", ANRHI_s, " mmol/L).")

# ---- Target16.txt ----
con16 <- file(file.path(out, "Target16.txt"), open = "wt")
cat("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)\n", file = con16)
cat(paste0("Y-axis: ", YAxisMin, " to ", YAxisMax, " by ", MAJOR,
           "  Reference limits: ", ANRLO_s, ", ", ANRHI_s, "\n"), file = con16)
cat("See sasgraph.png for plot.\n\n", file = con16)
cat("Max Post Baseline Result (shift from baseline) - N (%)\n", file = con16)
# header
hdr <- sprintf("%-16s %-14s", "Treatment", "Max. Baseline")
for (a in anr_levels) {
  alab <- ifelse(a == "", "(Missing)", a)
  hdr <- paste0(hdr, sprintf(" | %-22s", alab))
}
cat(hdr, "\n", file = con16)
subhdr <- sprintf("%-16s %-14s", "", "Result")
for (a in anr_levels) {
  subhdr <- paste0(subhdr, sprintf(" | %-10s %-10s", "N", "(%)"))
}
cat(subhdr, "\n", file = con16)
for (g in grp_levels) {
  for (b in bnr_levels) {
    blab <- ifelse(b == "", "(Missing)", b)
    line <- sprintf("%-16s %-14s", g, blab)
    for (a in anr_levels) {
      cell <- get_cell(g, b, a)
      cnt_s <- sprintf("%4d", cell$COUNT)
      pct_s <- fmt_percpar(cell$PERCENT)
      line <- paste0(line, sprintf(" | %4s %-10s", cnt_s, pct_s))
    }
    cat(line, "\n", file = con16)
  }
  cat("\n", file = con16)
}
cat("\nTreatment Emergent High\n", file = con16)
cat(sprintf("%-10s %-10s %-10s %-10s %-12s\n", "TRTPN", "Nx", "n", "%", "FishersP"), file = con16)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i, ]
  pct_disp <- ifelse(is.na(r$percent), "", sprintf("%.1f", round_half_up(r$percent, 1)))
  fp_disp <- fmt_pvalue(r$FishersP)
  cat(sprintf("%-10s %-10d %-10d %-10s %-12s\n",
              as.character(r$TRTPN), r$Nx, r$n, pct_disp, fp_disp), file = con16)
}
cat("\n", file = con16)
cat(paste0(fn1, "\n"), file = con16)
cat(paste0(fn2, "\n"), file = con16)
cat(paste0(fn3, "\n"), file = con16)
close(con16)

# Target16.csv : combined table body
t16_shift <- TableA_out %>%
  mutate(TRUNC_PCT = trunc1(PERCENT),
         PCT_DISP = fmt_percpar(PERCENT),
         TABLE = "Shift") %>%
  select(TABLE, GroupFmt, TRTPN, BNRIND, ANRIND, COUNT, PERCENT, TRUNC_PCT, PCT_DISP, GroupN, ValueFmt)
t16_em <- EmergentHigh %>% mutate(TABLE = "Emergent", GroupFmt = as.character(TRTPN)) %>%
  select(TABLE, GroupFmt, TRTPN, Nx, n, percent, FishersP)
# write shift + emergent separately? combine via full join not possible; write shift as main and emergent appended in same file with different cols
# Create a unified CSV with all columns
all_cols <- union(names(t16_shift), names(t16_em))
for (cc in setdiff(all_cols, names(t16_shift))) t16_shift[[cc]] <- NA
for (cc in setdiff(all_cols, names(t16_em))) t16_em[[cc]] <- NA
Target16_csv <- bind_rows(t16_shift, t16_em)
write.csv(Target16_csv, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

# ---- P036.txt : listing with FREQ TRTPN*n + both reports ----
conL <- file(file.path(out, "P036.txt"), open = "wt")
cat("Frequency Table of TRTPN by n\n", file = conL)
Ntot <- nrow(advsmax3)
cat(paste0("Sample Size = ", Ntot, "\n\n"), file = conL)
row_tot <- table(advsmax3$TRTPN)
col_tot <- table(advsmax3$n)
# header
cat(sprintf("%-10s %-10s %-12s %-12s %-12s %-12s\n",
            "TRTPN", "n", "Frequency", "Percent", "Row Pct", "Col Pct"), file = conL)
for (r in sort(unique(advsmax3$TRTPN))) {
  for (c in sort(unique(advsmax3$n))) {
    f <- sum(advsmax3$TRTPN == r & advsmax3$n == c)
    pct <- 100 * f / Ntot
    rowp <- 100 * f / row_tot[as.character(r)]
    colp <- 100 * f / col_tot[as.character(c)]
    cat(sprintf("%-10s %-10s %-12d %-12s %-12s %-12s\n",
                as.character(r), as.character(c), f,
                sprintf("%.2f", round_half_up(pct, 2)),
                sprintf("%.2f", round_half_up(rowp, 2)),
                sprintf("%.2f", round_half_up(colp, 2))), file = conL)
  }
}
cat("\nRow Totals:\n", file = conL)
for (r in names(row_tot)) {
  cat(paste0("TRTPN=", r, " Frequency=", row_tot[[r]],
             " Percent=", sprintf("%.2f", round_half_up(100*row_tot[[r]]/Ntot, 2)), "\n"), file = conL)
}
cat("Column Totals:\n", file = conL)
for (c in names(col_tot)) {
  cat(paste0("n=", c, " Frequency=", col_tot[[c]],
             " Percent=", sprintf("%.2f", round_half_up(100*col_tot[[c]]/Ntot, 2)), "\n"), file = conL)
}
cat("\nFisher's Exact Test\n", file = conL)
cat(paste0("Table Probability (P)= ", FishersP_val, "\n"), file = conL)
cat(paste0("Two-sided Pr <= P (XP2_FISH)= ", fmt_pvalue(FishersP_val), "\n"), file = conL)
cat(paste0("Sample Size = ", Ntot, "\n\n"), file = conL)
# append same report tables as Target16
cat("Max Post Baseline Result (shift from baseline) - N (%)\n", file = conL)
cat(hdr, "\n", file = conL)
cat(subhdr, "\n", file = conL)
for (g in grp_levels) {
  for (b in bnr_levels) {
    blab <- ifelse(b == "", "(Missing)", b)
    line <- sprintf("%-16s %-14s", g, blab)
    for (a in anr_levels) {
      cell <- get_cell(g, b, a)
      cnt_s <- sprintf("%4d", cell$COUNT)
      pct_s <- fmt_percpar(cell$PERCENT)
      line <- paste0(line, sprintf(" | %4s %-10s", cnt_s, pct_s))
    }
    cat(line, "\n", file = conL)
  }
  cat("\n", file = conL)
}
cat("\nTreatment Emergent High\n", file = conL)
cat(sprintf("%-10s %-10s %-10s %-10s %-12s\n", "TRTPN", "Nx", "n", "%", "FishersP"), file = conL)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i, ]
  pct_disp <- ifelse(is.na(r$percent), "", sprintf("%.1f", round_half_up(r$percent, 1)))
  fp_disp <- fmt_pvalue(r$FishersP)
  cat(sprintf("%-10s %-10d %-10d %-10s %-12s\n",
              as.character(r$TRTPN), r$Nx, r$n, pct_disp, fp_disp), file = conL)
}
cat("\n", file = conL)
cat(paste0(fn1, "\n"), file = conL)
cat(paste0(fn2, "\n"), file = conL)
cat(paste0(fn3, "\n"), file = conL)
close(conL)

# P036.csv : freq cells + shift + emergent
freq_df <- expand.grid(TRTPN = sort(unique(advsmax3$TRTPN)),
                       n = sort(unique(advsmax3$n)))
freq_df$Frequency <- mapply(function(r, c) sum(advsmax3$TRTPN == r & advsmax3$n == c),
                            freq_df$TRTPN, freq_df$n)
freq_df$Percent <- 100 * freq_df$Frequency / Ntot
freq_df$RowPct <- mapply(function(r, c, f) 100 * f / sum(advsmax3$TRTPN == r),
                         freq_df$TRTPN, freq_df$n, freq_df$Frequency)
freq_df$ColPct <- mapply(function(r, c, f) 100 * f / sum(advsmax3$n == c),
                         freq_df$TRTPN, freq_df$n, freq_df$Frequency)
freq_df$TABLE <- "Freq"
freq_df$FishersP <- FishersP_val
write.csv(freq_df, file.path(out, "P036_freq.csv"), row.names = FALSE, na = "")
# Main P036.csv mirrors Target16 plus freq: stack with NA padding
p036_shift <- t16_shift
p036_em <- t16_em
# build unified: use Target16_csv plus freq rows with TABLE=Freq
# pad freq to Target16_csv cols
for (cc in setdiff(names(Target16_csv), names(freq_df))) freq_df[[cc]] <- NA
for (cc in setdiff(names(freq_df), names(Target16_csv))) Target16_csv[[cc]] <- NA
P036_csv <- bind_rows(freq_df, Target16_csv)
write.csv(P036_csv, file.path(out, "P036.csv"), row.names = FALSE, na = "")
```