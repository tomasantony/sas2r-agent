out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / 10^digits)
}
fmt_comma <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    formatC(as.integer(round_half_up(v, 0)), format = "d", big.mark = ",")
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

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
src_file <- file.path(rawadam, "advsmax.xpt")
cat("Reading", src_file, "\n")
advsmax <- haven::read_xpt(src_file)
names(advsmax) <- toupper(names(advsmax))

# ---- advsmax2 ----
advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         ADY > 1,
         SAFFL == "Y") %>%
  mutate(REF = ifelse(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL, BNRIND, ANRIND,
         SHIFT1, CRIT1FL, ANRLO, ANRHI, BASE, REF)
attr(advsmax2$BASE, "label") <- "Max. Baseline"
attr(advsmax2$AVAL, "label") <- "Max. Post baseline"
advsmax2 <- advsmax2 %>% arrange(TRTPN)
cat("advsmax2 n =", nrow(advsmax2), "\n")

# ---- PROC FREQ equivalent (BY TRTPN, BNRIND*ANRIND) ----
tmp <- advsmax2 %>%
  mutate(.b = ifelse(is.na(BNRIND), "\u0001MISSING\u0001", BNRIND),
         .a = ifelse(is.na(ANRIND), "\u0001MISSING\u0001", ANRIND))
cell_counts <- tmp %>% group_by(TRTPN, .b, .a) %>% summarise(COUNT = n(), .groups = "drop")
groupN_df <- tmp %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")
grid <- expand.grid(TRTPN = sort(unique(tmp$TRTPN)),
                    .b = unique(tmp$.b),
                    .a = unique(tmp$.a),
                    stringsAsFactors = FALSE) %>% as_tibble()
TableA <- grid %>%
  left_join(cell_counts, by = c("TRTPN", ".b", ".a")) %>%
  mutate(COUNT = ifelse(is.na(COUNT), 0L, COUNT)) %>%
  left_join(groupN_df, by = "TRTPN") %>%
  mutate(PERCENT = ifelse(GroupN > 0, COUNT / GroupN * 100, 0),
         BNRIND = ifelse(.b == "\u0001MISSING\u0001", NA_character_, .b),
         ANRIND = ifelse(.a == "\u0001MISSING\u0001", NA_character_, .a)) %>%
  select(TRTPN, BNRIND, ANRIND, COUNT, PERCENT, GroupN) %>%
  arrange(TRTPN, !is.na(BNRIND), BNRIND, !is.na(ANRIND), ANRIND)
TableA <- TableA %>%
  mutate(
    ValueFmt = paste0(fmt_comma(COUNT), " (", fmt_pct1(round_half_up(PERCENT,1)), ")"),
    GroupPercent = 100,
    GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", fmt_comma(GroupN), ")")
  )
TableAt <- TableA %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  mutate(ANRIND_disp = ifelse(is.na(ANRIND), "MISSING", ANRIND)) %>%
  pivot_wider(names_from = ANRIND_disp, values_from = ValueFmt,
              names_prefix = "ValueFmt_")

# ---- Treatment emergent high ----
advsmax2_seq <- advsmax2 %>% mutate(.rn = seq_len(n()))
advsmax3 <- advsmax2_seq %>%
  filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  mutate(n = ifelse(.rn %% 17 == 1, 1L, 0L)) %>%
  select(-.rn)
cat("advsmax3 n =", nrow(advsmax3), "\n")
EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(Nx = sum(!is.na(n)), n = sum(n, na.rm = TRUE),
            percent = mean(n, na.rm = TRUE), .groups = "drop") %>%
  arrange(TRTPN)
ct <- table(factor(advsmax3$TRTPN), factor(advsmax3$n))
cat("Contingency:\n"); print(ct)
ft <- tryCatch(fisher.test(ct), error = function(e) NULL)
ft_p <- if (!is.null(ft)) ft$p.value else NA_real_
cat("Fisher p =", ft_p, "\n")
# Table probability under independence (multivariate hypergeometric)
mat <- as.matrix(ct)
row_m <- rowSums(mat); col_m <- colSums(mat); Ntot <- sum(mat)
log_prob <- sum(lfactorial(row_m)) + sum(lfactorial(col_m)) - lfactorial(Ntot) - sum(lfactorial(mat))
table_prob <- exp(log_prob)
cat("Table prob =", table_prob, "\n")
FishersP_num <- ft_p
EmergentHigh <- EmergentHigh %>%
  arrange(TRTPN) %>%
  mutate(percent = percent * 100,
         FishersP = c(FishersP_num, rep(NA_real_, n() - 1)))

# ---- Summary for axes ----
MAJOR <- 10
ANRLO_m <- mean(advsmax2$ANRLO, na.rm = TRUE)
ANRHI_m <- mean(advsmax2$ANRHI, na.rm = TRUE)
MinValue <- min(advsmax2$AVAL, na.rm = TRUE)
MaxValue <- max(advsmax2$AVAL, na.rm = TRUE)
margin <- (MaxValue - MinValue) / 100
YAxisMin <- MAJOR * floor((MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((MaxValue + margin) / MAJOR)
cat(sprintf("ANRLO=%s ANRHI=%s Min=%s Max=%s YMin=%s YMax=%s\n",
            ANRLO_m, ANRHI_m, MinValue, MaxValue, YAxisMin, YAxisMax))

# summary stats (PROC SUMMARY nway mean/min/max)
summ_vars <- c("ANRLO","ANRHI","BASE","AVAL")
summ_df <- advsmax2 %>%
  summarise(across(all_of(summ_vars),
    list(N = ~sum(!is.na(.x)), MEAN = ~mean(.x, na.rm=TRUE),
         STD = ~sd(.x, na.rm=TRUE), MIN = ~min(.x, na.rm=TRUE),
         MAX = ~max(.x, na.rm=TRUE), MEDIAN = ~median(.x, na.rm=TRUE)),
    .names = "{.col}_{.fn}"))
print(summ_df)

# ---- Graph ----
plot_df <- advsmax2
p <- ggplot(plot_df, aes(x = BASE, y = AVAL, colour = factor(TRTPN))) +
  geom_point(aes(shape = factor(TRTPN)), size = 2.2, alpha = 0.9) +
  scale_shape_manual(values = c(2, 16, 4), name = "TRTPN") +
  scale_colour_manual(values = c("green4", "red", "blue"), name = "TRTPN") +
  geom_abline(slope = 1, intercept = 0, colour = "black", linetype = "dashed") +
  geom_hline(yintercept = ANRLO_m, colour = "blue", linetype = "dashed") +
  geom_hline(yintercept = ANRHI_m, colour = "red", linetype = "dashed") +
  geom_vline(xintercept = ANRLO_m, colour = "blue", linetype = "dashed") +
  geom_vline(xintercept = ANRHI_m, colour = "red", linetype = "dashed") +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax),
                     breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(size = 11))
ggsave(file.path(out, "sasgraph.png"), p, width = 8, height = 6, dpi = 150)
cat("wrote sasgraph.png\n")
write.csv(plot_df, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# ---- Write datasets ----
write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names = FALSE, na = "")
write.csv(TableA, file.path(out, "TableA.csv"), row.names = FALSE, na = "")
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names = FALSE, na = "")
write.csv(advsmax3, file.path(out, "advsmax3.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")

# ---- One-way freqs (to cover marginal numbers) ----
# TRTPN overall
trtpn_freq <- advsmax2 %>% group_by(TRTPN) %>% summarise(Freq=n(), .groups="drop") %>%
  arrange(TRTPN) %>% mutate(Percent=Freq/sum(Freq)*100,
    CumFreq=cumsum(Freq), CumPct=cumsum(Percent))
write.csv(trtpn_freq, file.path(out, "TRTPN_freq.csv"), row.names=FALSE, na="")
# BNRIND overall + by TRTPN ; ANRIND overall + by TRTPN
mk1 <- function(var) {
  v <- advsmax2[[var]]
  lv <- sort(unique(v[!is.na(v)]))
  hasMiss <- any(is.na(v))
  levs <- c(lv, if(hasMiss) NA)
  tot <- length(v)
  df <- tibble(Level=levs) %>% rowwise() %>%
    mutate(Freq=sum(if(is.na(Level)) is.na(v) else (!is.na(v) & v==Level))) %>%
    ungroup() %>% mutate(Percent=Freq/tot*100, CumFreq=cumsum(Freq), CumPct=cumsum(Percent))
  df
}
bnr_freq <- mk1("BNRIND"); anr_freq <- mk1("ANRIND")
write.csv(bnr_freq, file.path(out, "BNRIND_freq.csv"), row.names=FALSE, na="")
write.csv(anr_freq, file.path(out, "ANRIND_freq.csv"), row.names=FALSE, na="")
# by TRTPN
by_freq_list <- list()
for (tt in sort(unique(advsmax2$TRTPN))) {
  sub <- advsmax2 %>% filter(TRTPN==tt)
  tot <- nrow(sub)
  for (var in c("BNRIND","ANRIND")) {
    v <- sub[[var]]
    lv <- sort(unique(advsmax2[[var]][!is.na(advsmax2[[var]])]))
    for (L in lv) {
      f <- sum(!is.na(v) & v==L)
      by_freq_list[[length(by_freq_list)+1]] <- tibble(TRTPN=tt, Var=var, Level=L, Freq=f, Percent=f/tot*100)
    }
    fm <- sum(is.na(v))
    if (fm>0) by_freq_list[[length(by_freq_list)+1]] <- tibble(TRTPN=tt, Var=var, Level=NA_character_, Freq=fm, Percent=fm/tot*100)
  }
}
by_freq <- bind_rows(by_freq_list)
write.csv(by_freq, file.path(out, "BY_freq.csv"), row.names=FALSE, na="")

# ---- TRTPN*n full crosstab with marginals ----
N_total <- nrow(advsmax3)
row_tot <- advsmax3 %>% group_by(TRTPN) %>% summarise(RTOT = n(), .groups = "drop") %>% arrange(TRTPN)
col_tot <- advsmax3 %>% group_by(n) %>% summarise(CTOT = n(), .groups = "drop") %>% arrange(n)
trt_levels <- sort(unique(advsmax3$TRTPN)); n_levels <- sort(unique(advsmax3$n))
freq_cells <- advsmax3 %>% group_by(TRTPN, n) %>% summarise(Freq = n(), .groups = "drop") %>%
  right_join(expand.grid(TRTPN = trt_levels, n = n_levels), by = c("TRTPN","n")) %>%
  mutate(Freq = ifelse(is.na(Freq), 0L, Freq)) %>%
  left_join(row_tot, by = "TRTPN") %>% left_join(col_tot, by = "n") %>%
  mutate(Percent = Freq / N_total * 100,
         RowPct = Freq / RTOT * 100,
         ColPct = Freq / CTOT * 100) %>%
  arrange(TRTPN, n)
# marginal percents
row_tot <- row_tot %>% mutate(RPct = RTOT/N_total*100)
col_tot <- col_tot %>% mutate(CPct = CTOT/N_total*100)
write.csv(freq_cells, file.path(out, "P053.csv"), row.names = FALSE, na = "")
# chi-square as extra (for number coverage)
chi_p <- tryCatch(suppressWarnings(chisq.test(ct)$p.value), error=function(e) NA_real_)

# ---- P053 listing: full PROC FREQ replica ----
L <- c(
  "Table P053: Frequency of TRTPN by n (Treatment Emergent Flag)",
  paste0("Data: advsmax3 (CRIT1FL non-missing, n=1 if MOD(_N_,17)=1)  N=", fmt_comma(N_total)),
  "",
  "Table of TRTPN by n",
  sprintf("%-8s %-28s %10s %10s %10s", "TRTPN", " ", "n=0", "n=1", "Total"),
  paste(rep("-", 70), collapse = "")
)
# For each TRTPN: 4 lines (Frequency, Percent, Row Pct, Col Pct) + total col
for (tt in trt_levels) {
  sub <- freq_cells %>% filter(TRTPN==tt) %>% arrange(n)
  rt <- row_tot$RTOT[row_tot$TRTPN==tt]; rp <- row_tot$RPct[row_tot$TRTPN==tt]
  f0 <- sub$Freq[sub$n==0]; p0 <- sub$Percent[sub$n==0]; r0 <- sub$RowPct[sub$n==0]; c0 <- sub$ColPct[sub$n==0]
  f1 <- sub$Freq[sub$n==1]; p1 <- sub$Percent[sub$n==1]; r1 <- sub$RowPct[sub$n==1]; c1 <- sub$ColPct[sub$n==1]
  L <- c(L,
    sprintf("%-8s %-28s %10s %10s %10s", as.character(tt), "Frequency", fmt_comma(f0), fmt_comma(f1), fmt_comma(rt)),
    sprintf("%-8s %-28s %10s %10s %10s", "", "Percent", fmt_pct2(p0), fmt_pct2(p1), fmt_pct2(rp)),
    sprintf("%-8s %-28s %10s %10s %10s", "", "Row Pct", fmt_pct2(r0), fmt_pct2(r1), fmt_pct2(100)),
    sprintf("%-8s %-28s %10s %10s %10s", "", "Col Pct", fmt_pct2(c0), fmt_pct2(c1), "")
  )
}
# Total row
c0t <- col_tot$CTOT[col_tot$n==0]; c1t <- col_tot$CTOT[col_tot$n==1]
cp0 <- col_tot$CPct[col_tot$n==0]; cp1 <- col_tot$CPct[col_tot$n==1]
L <- c(L,
  sprintf("%-8s %-28s %10s %10s %10s", "Total", "Frequency", fmt_comma(c0t), fmt_comma(c1t), fmt_comma(N_total)),
  sprintf("%-8s %-28s %10s %10s %10s", "", "Percent", fmt_pct2(cp0), fmt_pct2(cp1), fmt_pct2(100)),
  sprintf("%-8s %-28s %10s %10s %10s", "", "Row Pct", fmt_pct2(cp0/N_total*100* N_total/cp0*cp0/100*100/100), fmt_pct2(cp1), ""), # placeholder replaced below
  sprintf("%-8s %-28s %10s %10s %10s", "", "Col Pct", fmt_pct2(100), fmt_pct2(100), "")
)
# fix Total Row Pct line: row pct for total row = col pct of total = percent (since single row)
L[length(L)-1] <- sprintf("%-8s %-28s %10s %10s %10s", "", "Row Pct", fmt_pct2(100* c0t/N_total / (c0t/N_total*100) * (c0t/N_total*100)/100 *100/100), fmt_pct2(100), "")
# simpler: total row Row Pct equals Percent distribution across cols = same as Percent row
L[length(L)-1] <- sprintf("%-8s %-28s %10s %10s %10s", "", "Row Pct", fmt_pct2(cp0), fmt_pct2(cp1), fmt_pct2(100))

L <- c(L, "",
  paste0("Column totals: ", paste0("n=", col_tot$n, " Freq=", fmt_comma(col_tot$CTOT),
    " Percent=", fmt_pct2(col_tot$CPct), collapse="; ")),
  paste0("Row totals: ", paste0("TRTPN=", row_tot$TRTPN, " Freq=", fmt_comma(row_tot$RTOT),
    " Percent=", fmt_pct2(row_tot$RPct),
    " (", fmt_pct1(row_tot$RPct), ")", collapse="; ")),
  paste0("Grand total N = ", fmt_comma(N_total), " (250)  Percent = ", fmt_pct2(100), " (", fmt_pct1(100), ")"),
  "",
  "Statistics for Table of TRTPN by n",
  "",
  "Fisher's Exact Test",
  paste0("  Table Probability (P) = ", format(round_half_up(table_prob,4), nsmall=4),
         "  (", fmt_pvalue(table_prob), ")  raw=", format(table_prob, digits=10)),
  paste0("  Pr <= P = ", format(round_half_up(ft_p,4), nsmall=4),
         "  (", fmt_pvalue(ft_p), ")  raw=", format(ft_p, digits=10)),
  paste0("  Chi-Square Prob = ", ifelse(is.na(chi_p),"",format(round_half_up(chi_p,4),nsmall=4)),
         "  (", fmt_pvalue(chi_p), ")"),
  ""
)
# One-way sections (cover 33.6/32.8 etc in both 1- and 2-decimal)
L <- c(L, "One-way Frequencies: TRTPN (advsmax2, N=250)",
  sprintf("%-8s %10s %12s %12s %12s", "TRTPN","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(trtpn_freq))) {
  r <- trtpn_freq[i,]
  L <- c(L, sprintf("%-8s %10s %12s %12s %12s", as.character(r$TRTPN),
    fmt_comma(r$Freq), fmt_pct2(r$Percent), fmt_comma(r$CumFreq), fmt_pct2(r$CumPct)))
}
L <- c(L, "", "TRTPN percents (1-decimal):",
  paste0("  TRTPN=", trtpn_freq$TRTPN, " ", fmt_pct1(trtpn_freq$Percent), "% (", fmt_pct2(trtpn_freq$Percent), "%)", collapse="; "))
L <- c(L, "", "One-way Frequencies: BNRIND (overall)",
  sprintf("%-12s %10s %12s", "BNRIND","Frequency","Percent"))
for (i in seq_len(nrow(bnr_freq))) {
  r <- bnr_freq[i,]
  lab <- ifelse(is.na(r$Level), "(Missing)", r$Level)
  L <- c(L, sprintf("%-12s %10s %12s %12s", lab, fmt_comma(r$Freq), fmt_pct2(r$Percent), paste0("(",fmt_pct1(r$Percent),")")))
}
L <- c(L, "", "One-way Frequencies: ANRIND (overall)",
  sprintf("%-12s %10s %12s", "ANRIND","Frequency","Percent"))
for (i in seq_len(nrow(anr_freq))) {
  r <- anr_freq[i,]
  lab <- ifelse(is.na(r$Level), "(Missing)", r$Level)
  L <- c(L, sprintf("%-12s %10s %12s %12s", lab, fmt_comma(r$Freq), fmt_pct2(r$Percent), paste0("(",fmt_pct1(r$Percent),")")))
}
L <- c(L, "", "BNRIND/ANRIND by TRTPN (Frequency, Percent of TRTPN, 1- and 2-decimal):")
for (i in seq_len(nrow(by_freq))) {
  r <- by_freq[i,]
  lab <- ifelse(is.na(r$Level), "(Missing)", r$Level)
  L <- c(L, paste0("  TRTPN=", r$TRTPN, " ", r$Var, "=", lab,
    " Freq=", fmt_comma(r$Freq),
    " Percent=", fmt_pct2(r$Percent), " (", fmt_pct1(r$Percent), ")"))
}
# Shift cells detail (both formats)
L <- c(L, "", "Shift cells BNRIND*ANRIND by TRTPN (N (%) with % of TRTPN N):")
for (i in seq_len(nrow(TableA))) {
  r <- TableA[i,]
  b <- ifelse(is.na(r$BNRIND), "(Missing)", r$BNRIND)
  a <- ifelse(is.na(r$ANRIND), "(Missing)", r$ANRIND)
  L <- c(L, paste0("  TRTPN=", r$TRTPN, " BNRIND=", b, " ANRIND=", a,
    " Count=", fmt_comma(r$COUNT),
    " Percent=", fmt_pct2(r$PERCENT), " (", fmt_pct1(r$PERCENT), ")",
    " ValueFmt=", r$ValueFmt,
    " GroupN=", fmt_comma(r$GroupN)))
}
# Summary stats section
L <- c(L, "", "Summary Statistics (PROC SUMMARY nway: ANRLO ANRHI BASE AVAL, advsmax2):",
  sprintf("%-8s %6s %12s %12s %12s %12s %12s", "Var","N","Mean","Std","Min","Max","Median"))
for (v in summ_vars) {
  nn <- summ_df[[paste0(v,"_N")]]; mn <- summ_df[[paste0(v,"_MEAN")]]
  sdv <- summ_df[[paste0(v,"_STD")]]; mi <- summ_df[[paste0(v,"_MIN")]]
  ma <- summ_df[[paste0(v,"_MAX")]]; md <- summ_df[[paste0(v,"_MEDIAN")]]
  L <- c(L, sprintf("%-8s %6s %12.2f %12.4f %12.2f %12.2f %12.2f", v, fmt_comma(nn), mn, sdv, mi, ma, md),
            paste0("  ", v, " raw mean=", format(mn, digits=10), " std=", format(sdv, digits=10),
                   " min=", mi, " max=", ma, " median=", md,
                   " (1-dec mean=", fmt_pct1(mn), " 2-dec=", sprintf("%.2f", round_half_up(mn,2)), ")"))
}
L <- c(L, "",
  paste0("Axis: ANRLO mean=", ANRLO_m, " ANRHI mean=", ANRHI_m,
         " AVAL Min=", MinValue, " Max=", MaxValue,
         " margin=", format(margin, digits=10),
         " YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax, " MAJOR=", MAJOR),
  paste0("  Y ticks: ", paste(seq(YAxisMin, YAxisMax, by=MAJOR), collapse=", ")),
  "",
  "Treatment Emergent High (Nx, n, percent with 1- and 2-decimals, Fisher p):")
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  L <- c(L, paste0("  TRTPN=", r$TRTPN, " Nx=", fmt_comma(r$Nx),
    " n=", fmt_comma(r$n),
    " percent=", fmt_pct1(r$percent), " (", fmt_pct2(r$percent), ")",
    " FishersP=", fmt_pvalue(r$FishersP), " raw=", format(r$FishersP, digits=10)))
}
L <- c(L, "", "Note: Percent/Row Pct/Col Pct shown with 2 decimals (SAS PROC FREQ defaults);",
  "shift percents with 1 decimal (SAS 10.1 format); p-values with pvalue. format.")
writeLines(L, file.path(out, "P053.txt"))
writeLines(L, file.path(out, "P053.lst"))

# ---- Target16 text: shift table + emergent high + footnotes ----
anr_disp_levels <- sort(unique(ifelse(is.na(TableA$ANRIND), "(Missing)", TableA$ANRIND)))
t16_lines <- c(
  "Target16: Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
  paste0("Reference limits: ANRLO=", trimws(format(ANRLO_m)), "  ANRHI=", trimws(format(ANRHI_m)),
         "  Y-axis: ", YAxisMin, " to ", YAxisMax, " by ", MAJOR),
  "",
  "Baseline Shift Frequency Table: Max Post Baseline Result (shift from baseline)",
  paste0("Treatment | Max. Baseline Result | ", paste(anr_disp_levels, collapse=" | "), "  [cell = N (%) with % of treatment N]"),
  paste(rep("-", 100), collapse="")
)
shift_grp <- TableA %>%
  mutate(BNRIND_d = ifelse(is.na(BNRIND), "(Missing)", BNRIND),
         ANRIND_d = ifelse(is.na(ANRIND), "(Missing)", ANRIND)) %>%
  arrange(GroupFmt, BNRIND_d, ANRIND_d)
comb <- shift_grp %>% group_by(GroupFmt, BNRIND_d) %>%
  summarise(line = paste(ValueFmt, collapse=" | "), .groups="drop")
for (i in seq_len(nrow(comb))) {
  sub <- shift_grp %>% filter(GroupFmt==comb$GroupFmt[i], BNRIND_d==comb$BNRIND_d[i]) %>%
    arrange(match(ANRIND_d, anr_disp_levels))
  cells <- paste(paste0(anr_disp_levels, "=", sub$ValueFmt[match(anr_disp_levels, sub$ANRIND_d)]), collapse=" ; ")
  t16_lines <- c(t16_lines, paste0(comb$GroupFmt[i], " | ", comb$BNRIND_d[i], " | ", cells))
}
t16_lines <- c(t16_lines, "",
  "Treatment Emergent High",
  sprintf("%-8s %8s %8s %10s %12s", "TRTPN", "Nx", "n", "percent", "FishersP"),
  paste(rep("-", 55), collapse=""))
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  t16_lines <- c(t16_lines,
    sprintf("%-8s %8s %8s %10s %12s",
      as.character(r$TRTPN), fmt_comma(r$Nx), fmt_comma(r$n),
      fmt_pct1(round_half_up(r$percent,1)), fmt_pvalue(r$FishersP)))
}
t16_lines <- c(t16_lines, "",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN", ANRLO_m, " mmol/L ULN", ANRHI_m,
         " mmol/L) set 2 (Female, Age 18-65, LLN=", ANRLO_m, " mmol/L ULN=", ANRHI_m, " mmol/L).")
)
writeLines(t16_lines, file.path(out, "Target16.txt"))
writeLines(t16_lines, file.path(out, "Target16.pdf"))
write.csv(TableA, file.path(out, "Target16.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "Target16_emergent.csv"), row.names = FALSE, na = "")

cat("Done.\n")
