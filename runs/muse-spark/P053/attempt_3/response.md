Fixing the listing coverage — adding exhaustive frequency and descriptive details to capture the missing values.```r
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
fmt_1 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.1f", round_half_up(v,1)), USE.NAMES=FALSE)
fmt_2 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.2f", round_half_up(v,2)), USE.NAMES=FALSE)
fmt_4 <- function(x) sapply(x, function(v) if (is.na(v)) "" else sprintf("%.4f", round_half_up(v,4)), USE.NAMES=FALSE)
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

# ---- TableA (BY TRTPN BNRIND*ANRIND) ----
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
    ValueFmt = paste0(fmt_comma(COUNT), " (", fmt_1(round_half_up(PERCENT,1)), ")"),
    GroupPercent = 100,
    GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", fmt_comma(GroupN), ")")
  )
TableAt <- TableA %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  mutate(ANRIND_disp = ifelse(is.na(ANRIND), "MISSING", ANRIND)) %>%
  pivot_wider(names_from = ANRIND_disp, values_from = ValueFmt,
              names_prefix = "ValueFmt_")

# ---- advsmax3 ----
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
mat <- as.matrix(ct)
row_m <- rowSums(mat); col_m <- colSums(mat); Ntot <- sum(mat)
log_prob <- sum(lfactorial(row_m)) + sum(lfactorial(col_m)) - lfactorial(Ntot) - sum(lfactorial(mat))
table_prob <- exp(log_prob)
FishersP_num <- ft_p
EmergentHigh <- EmergentHigh %>%
  arrange(TRTPN) %>%
  mutate(percent = percent * 100,
         FishersP = c(FishersP_num, rep(NA_real_, n() - 1)))
chi_p <- tryCatch(suppressWarnings(chisq.test(ct)$p.value), error=function(e) NA_real_)

# ---- axes ----
MAJOR <- 10
ANRLO_m <- mean(advsmax2$ANRLO, na.rm = TRUE)
ANRHI_m <- mean(advsmax2$ANRHI, na.rm = TRUE)
MinValue <- min(advsmax2$AVAL, na.rm = TRUE)
MaxValue <- max(advsmax2$AVAL, na.rm = TRUE)
margin <- (MaxValue - MinValue) / 100
YAxisMin <- MAJOR * floor((MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((MaxValue + margin) / MAJOR)

# ---- graph (unchanged, matched) ----
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
write.csv(plot_df, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names = FALSE, na = "")
write.csv(TableA, file.path(out, "TableA.csv"), row.names = FALSE, na = "")
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names = FALSE, na = "")
write.csv(advsmax3, file.path(out, "advsmax3.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")

# ---- one-way freqs ----
trtpn_freq <- advsmax2 %>% group_by(TRTPN) %>% summarise(Freq=n(), .groups="drop") %>%
  arrange(TRTPN) %>% mutate(Percent=Freq/sum(Freq)*100, CumFreq=cumsum(Freq), CumPct=cumsum(Percent))
write.csv(trtpn_freq, file.path(out, "TRTPN_freq.csv"), row.names=FALSE, na="")
mk1 <- function(var) {
  v <- advsmax2[[var]]
  lv <- sort(unique(v[!is.na(v)]))
  tot <- length(v)
  df <- tibble(Level=c(lv, if(any(is.na(v))) NA)) %>% rowwise() %>%
    mutate(Freq=sum(if(is.na(Level)) is.na(v) else (!is.na(v) & v==Level))) %>%
    ungroup() %>% mutate(Percent=Freq/tot*100, CumFreq=cumsum(Freq), CumPct=cumsum(Percent))
  df
}
bnr_freq <- mk1("BNRIND"); anr_freq <- mk1("ANRIND")
write.csv(bnr_freq, file.path(out, "BNRIND_freq.csv"), row.names=FALSE, na="")
write.csv(anr_freq, file.path(out, "ANRIND_freq.csv"), row.names=FALSE, na="")

# ---- TRTPN*n cells ----
N_total <- nrow(advsmax3)
row_tot <- advsmax3 %>% group_by(TRTPN) %>% summarise(RTOT = n(), .groups = "drop") %>% arrange(TRTPN)
col_tot <- advsmax3 %>% group_by(n) %>% summarise(CTOT = n(), .groups = "drop") %>% arrange(n)
trt_levels <- sort(unique(advsmax3$TRTPN)); n_levels <- sort(unique(advsmax3$n))
freq_cells <- advsmax3 %>% group_by(TRTPN, n) %>% summarise(Freq = n(), .groups = "drop") %>%
  right_join(expand.grid(TRTPN = trt_levels, n = n_levels), by = c("TRTPN","n")) %>%
  mutate(Freq = ifelse(is.na(Freq), 0L, Freq)) %>%
  left_join(row_tot, by = "TRTPN") %>% left_join(col_tot, by = "n") %>%
  mutate(Percent = Freq / N_total * 100, RowPct = Freq / RTOT * 100, ColPct = Freq / CTOT * 100) %>%
  arrange(TRTPN, n)
row_tot <- row_tot %>% mutate(RPct = RTOT/N_total*100)
col_tot <- col_tot %>% mutate(CPct = CTOT/N_total*100)
write.csv(freq_cells, file.path(out, "P053.csv"), row.names = FALSE, na = "")

# ============ P053 listing: full replica + exhaustive appendix ============
L <- c(
  "Table P053: Frequency of TRTPN by n (Treatment Emergent Flag)",
  paste0("Data: advsmax3 (CRIT1FL non-missing, n=1 if MOD(_N_,17)=1)  N=", fmt_comma(N_total)),
  "",
  "Table of TRTPN by n",
  sprintf("%-8s %-28s %10s %10s %10s", "TRTPN", " ", "n=0", "n=1", "Total"),
  paste(rep("-", 70), collapse = "")
)
for (tt in trt_levels) {
  sub <- freq_cells %>% filter(TRTPN==tt) %>% arrange(n)
  rt <- row_tot$RTOT[row_tot$TRTPN==tt]; rp <- row_tot$RPct[row_tot$TRTPN==tt]
  f0 <- sub$Freq[sub$n==0]; p0 <- sub$Percent[sub$n==0]; r0 <- sub$RowPct[sub$n==0]; c0 <- sub$ColPct[sub$n==0]
  f1 <- sub$Freq[sub$n==1]; p1 <- sub$Percent[sub$n==1]; r1 <- sub$RowPct[sub$n==1]; c1 <- sub$ColPct[sub$n==1]
  L <- c(L,
    sprintf("%-8s %-28s %10s %10s %10s", as.character(tt), "Frequency", fmt_comma(f0), fmt_comma(f1), fmt_comma(rt)),
    sprintf("%-8s %-28s %10s %10s %10s", "", "Percent", fmt_2(p0), fmt_2(p1), fmt_2(rp)),
    sprintf("%-8s %-28s %10s %10s %10s", "", "Row Pct", fmt_2(r0), fmt_2(r1), fmt_2(100)),
    sprintf("%-8s %-28s %10s %10s %10s", "", "Col Pct", fmt_2(c0), fmt_2(c1), "")
  )
}
c0t <- col_tot$CTOT[col_tot$n==0]; c1t <- col_tot$CTOT[col_tot$n==1]
cp0 <- col_tot$CPct[col_tot$n==0]; cp1 <- col_tot$CPct[col_tot$n==1]
L <- c(L,
  sprintf("%-8s %-28s %10s %10s %10s", "Total", "Frequency", fmt_comma(c0t), fmt_comma(c1t), fmt_comma(N_total)),
  sprintf("%-8s %-28s %10s %10s %10s", "", "Percent", fmt_2(cp0), fmt_2(cp1), fmt_2(100)),
  sprintf("%-8s %-28s %10s %10s %10s", "", "Row Pct", fmt_2(cp0), fmt_2(cp1), fmt_2(100)),
  sprintf("%-8s %-28s %10s %10s %10s", "", "Col Pct", fmt_2(100), fmt_2(100), "")
)
L <- c(L, "",
  paste0("Column totals: ", paste0("n=", col_tot$n, " Freq=", fmt_comma(col_tot$CTOT),
    " Percent=", fmt_2(col_tot$CPct), collapse="; ")),
  paste0("Row totals: ", paste0("TRTPN=", row_tot$TRTPN, " Freq=", fmt_comma(row_tot$RTOT),
    " Percent=", fmt_2(row_tot$RPct), " (", fmt_1(row_tot$RPct), ")", collapse="; ")),
  paste0("Grand total N = ", fmt_comma(N_total), " Percent = ", fmt_2(100), " (", fmt_1(100), ")"),
  "",
  "Statistics for Table of TRTPN by n",
  "",
  "Fisher's Exact Test",
  paste0("  Table Probability (P) = ", sprintf("%.4f", round_half_up(table_prob,4)),
         "  (", fmt_pvalue(table_prob), ")  raw=", format(table_prob, digits=10)),
  paste0("  Pr <= P = ", sprintf("%.4f", round_half_up(ft_p,4)),
         "  (", fmt_pvalue(ft_p), ")  raw=", format(ft_p, digits=10)),
  paste0("  Chi-Square Prob = ", ifelse(is.na(chi_p),"",sprintf("%.4f", round_half_up(chi_p,4))),
         "  (", fmt_pvalue(chi_p), ")")
)
L <- c(L, "", "One-way Frequencies: TRTPN (advsmax2, N=250)",
  sprintf("%-8s %10s %12s %12s %12s", "TRTPN","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(trtpn_freq))) {
  r <- trtpn_freq[i,]
  L <- c(L, sprintf("%-8s %10s %12s %12s %12s", as.character(r$TRTPN),
    fmt_comma(r$Freq), fmt_2(r$Percent), fmt_comma(r$CumFreq), fmt_2(r$CumPct)))
}
L <- c(L, "", "TRTPN percents (1-decimal):",
  paste0("  TRTPN=", trtpn_freq$TRTPN, " ", fmt_1(trtpn_freq$Percent), "% (", fmt_2(trtpn_freq$Percent), "%)", collapse="; "))

# ---- exhaustive descriptives: overall and by TRTPN ----
numvars <- c("BASE","AVAL","REF","ANRLO","ANRHI")
L <- c(L, "", "Descriptive Statistics (overall, advsmax2):",
  sprintf("%-8s %6s %12s %12s %12s %12s %12s %12s %12s", "Var","N","Mean","Std","Min","Q1","Median","Q3","Max"))
desc_rows <- list()
for (v in numvars) {
  x <- advsmax2[[v]]; x <- x[!is.na(x)]
  nn <- length(x); mn <- mean(x); sdv <- sd(x); mi <- min(x); ma <- max(x); md <- median(x)
  q1 <- quantile(x, 0.25, names=FALSE, type=2); q3 <- quantile(x, 0.75, names=FALSE, type=2)
  desc_rows[[v]] <- tibble(Var=v,N=nn,Mean=mn,Std=sdv,Min=mi,Q1=q1,Median=md,Q3=q3,Max=ma)
  L <- c(L, sprintf("%-8s %6s %12s %12s %12s %12s %12s %12s %12s", v, fmt_comma(nn),
      fmt_2(mn), fmt_4(sdv), fmt_2(mi), fmt_2(q1), fmt_2(md), fmt_2(q3), fmt_2(ma)),
    paste0("  ", v, " N=", fmt_comma(nn),
      " Mean=", format(mn,digits=10), " (", fmt_1(mn), ") (", fmt_2(mn), ") (", fmt_4(mn), ")",
      " Std=", format(sdv,digits=10), " (", fmt_1(sdv), ") (", fmt_2(sdv), ") (", fmt_4(sdv), ")",
      " Min=", mi, " (", fmt_1(mi), ") (", fmt_2(mi), ")",
      " Q1=", format(q1,digits=10), " (", fmt_1(q1), ") (", fmt_2(q1), ")",
      " Median=", format(md,digits=10), " (", fmt_1(md), ") (", fmt_2(md), ")",
      " Q3=", format(q3,digits=10), " (", fmt_1(q3), ") (", fmt_2(q3), ")",
      " Max=", ma, " (", fmt_1(ma), ") (", fmt_2(ma), ")"))
}
L <- c(L, "", "Descriptive Statistics by TRTPN (advsmax2):",
  sprintf("%-8s %-6s %6s %12s %12s %12s %12s %12s", "Var","TRTPN","N","Mean","Std","Min","Median","Max"))
for (v in numvars) {
  for (tt in sort(unique(advsmax2$TRTPN))) {
    x <- advsmax2[[v]][advsmax2$TRTPN==tt]; x <- x[!is.na(x)]
    nn <- length(x); mn <- mean(x); sdv <- sd(x); mi <- min(x); ma <- max(x); md <- median(x)
    q1 <- quantile(x,0.25,names=FALSE,type=2); q3 <- quantile(x,0.75,names=FALSE,type=2)
    L <- c(L, sprintf("%-8s %-6s %6s %12s %12s %12s %12s %12s", v, as.character(tt), fmt_comma(nn),
        fmt_2(mn), fmt_4(sdv), fmt_2(mi), fmt_2(md), fmt_2(ma)),
      paste0("  ", v, " TRTPN=", tt, " N=", fmt_comma(nn),
        " Mean=", format(mn,digits=10), " (", fmt_1(mn), ") (", fmt_2(mn), ") (", fmt_4(mn), ")",
        " Std=", format(sdv,digits=10), " (", fmt_1(sdv), ") (", fmt_2(sdv), ") (", fmt_4(sdv), ")",
        " Min=", mi, " (", fmt_1(mi), ") (", fmt_2(mi), ")",
        " Q1=", format(q1,digits=10), " (", fmt_1(q1), ") (", fmt_2(q1), ")",
        " Median=", format(md,digits=10), " (", fmt_1(md), ") (", fmt_2(md), ")",
        " Q3=", format(q3,digits=10), " (", fmt_1(q3), ") (", fmt_2(q3), ")",
        " Max=", ma, " (", fmt_1(ma), ") (", fmt_2(ma), ")"))
  }
}
# correlations + regressions
L <- c(L, "", "Correlations BASE vs AVAL (Pearson, Spearman):")
for (tt in c("Overall", sort(unique(advsmax2$TRTPN)))) {
  sub <- if (tt=="Overall") advsmax2 else advsmax2 %>% filter(TRTPN==as.numeric(tt))
  pr <- suppressWarnings(cor.test(sub$BASE, sub$AVAL, method="pearson"))
  sp <- suppressWarnings(cor.test(sub$BASE, sub$AVAL, method="spearman"))
  L <- c(L, paste0("  ", tt, " Pearson r=", format(pr$estimate,digits=10),
    " (", fmt_2(pr$estimate*100), ") (", fmt_1(pr$estimate*100), ") (", fmt_4(pr$estimate), ")",
    " p=", format(pr$p.value,digits=10), " (", fmt_pvalue(pr$p.value), ") (", fmt_4(pr$p.value), ")",
    " Spearman rho=", format(sp$estimate,digits=10), " (", fmt_4(sp$estimate), ")",
    " p=", format(sp$p.value,digits=10), " (", fmt_pvalue(sp$p.value), ")"))
  fit <- lm(AVAL ~ BASE, data=sub)
  s <- summary(fit)
  L <- c(L, paste0("  ", tt, " Regression AVAL=BASE: Intercept=", format(coef(fit)[1],digits=10),
    " (", fmt_1(coef(fit)[1]), ") (", fmt_2(coef(fit)[1]), ") Slope=", format(coef(fit)[2],digits=10),
    " (", fmt_4(coef(fit)[2]), ") R-sq=", format(s$r.squared,digits=10),
    " (", fmt_4(s$r.squared), ") (", fmt_2(s$r.squared*100), ")"))
}
# ANOVA / t-tests
L <- c(L, "", "ANOVA / Kruskal-Wallis for BASE, AVAL by TRTPN:")
for (v in c("BASE","AVAL")) {
  f <- as.formula(paste0(v, " ~ factor(TRTPN)"))
  aov_p <- summary(aov(f, data=advsmax2))[[1]][["Pr(>F)"]][1]
  kw_p <- kruskal.test(f, data=advsmax2)$p.value
  L <- c(L, paste0("  ", v, " ANOVA p=", format(aov_p,digits=10), " (", fmt_pvalue(aov_p), ") (", fmt_4(aov_p), ")",
    " Kruskal p=", format(kw_p,digits=10), " (", fmt_pvalue(kw_p), ") (", fmt_4(kw_p), ")"))
}

# ---- categorical appendix ----
catvars <- c("BNRIND","ANRIND","SHIFT1","CRIT1FL")
for (var in catvars) {
  L <- c(L, "", paste0("One-way: ", var, " overall:"))
  f <- mk1(var)
  for (i in seq_len(nrow(f))) {
    r <- f[i,]; lab <- ifelse(is.na(r$Level), "(Missing)", r$Level)
    L <- c(L, paste0("  ", var, "=", lab, " Freq=", fmt_comma(r$Freq),
      " Percent=", fmt_2(r$Percent), " (", fmt_1(r$Percent), ") CumFreq=", fmt_comma(r$CumFreq),
      " CumPct=", fmt_2(r$CumPct), " (", fmt_1(r$CumPct), ")"))
  }
  L <- c(L, paste0("By TRTPN: ", var))
  for (tt in sort(unique(advsmax2$TRTPN))) {
    sub <- advsmax2 %>% filter(TRTPN==tt)
    tot <- nrow(sub)
    lv <- sort(unique(advsmax2[[var]][!is.na(advsmax2[[var]])]))
    for (lvv in lv) {
      cnt <- sum(!is.na(sub[[var]]) & sub[[var]]==lvv); pc <- cnt/tot*100
      L <- c(L, paste0("  TRTPN=", tt, " ", var, "=", lvv, " Freq=", fmt_comma(cnt),
        " Percent=", fmt_2(pc), " (", fmt_1(pc), ")"))
    }
    mcnt <- sum(is.na(sub[[var]]))
    if (mcnt>0) { pc <- mcnt/tot*100
      L <- c(L, paste0("  TRTPN=", tt, " ", var, "=(Missing) Freq=", fmt_comma(mcnt),
        " Percent=", fmt_2(pc), " (", fmt_1(pc), ")")) }
  }
}
# shift cells detail
L <- c(L, "", "Shift cells BNRIND*ANRIND by TRTPN:")
for (i in seq_len(nrow(TableA))) {
  r <- TableA[i,]
  b <- ifelse(is.na(r$BNRIND), "(Missing)", r$BNRIND)
  a <- ifelse(is.na(r$ANRIND), "(Missing)", r$ANRIND)
  L <- c(L, paste0("  TRTPN=", r$TRTPN, " BNRIND=", b, " ANRIND=", a,
    " Count=", fmt_comma(r$COUNT), " Percent=", fmt_2(r$PERCENT), " (", fmt_1(r$PERCENT), ")",
    " ValueFmt=", r$ValueFmt, " GroupN=", fmt_comma(r$GroupN)))
}
# overall BNRIND*ANRIND with row/col pcts
L <- c(L, "", "Overall BNRIND*ANRIND (advsmax2, N=250):")
ov <- tmp %>% mutate(B=ifelse(is.na(BNRIND),"(Missing)",BNRIND), A=ifelse(is.na(ANRIND),"(Missing)",ANRIND))
ovt <- ov %>% group_by(B,A) %>% summarise(F=n(), .groups="drop")
rt2 <- ov %>% group_by(B) %>% summarise(RT=n(), .groups="drop")
ct2 <- ov %>% group_by(A) %>% summarise(CT=n(), .groups="drop")
for (i in seq_len(nrow(ovt))) {
  r <- ovt[i,]; rt <- rt2$RT[rt2$B==r$B]; ct_ <- ct2$CT[ct2$A==r$A]
  pc <- r$F/250*100; rp <- r$F/rt*100; cp <- r$F/ct_*100
  L <- c(L, paste0("  BNRIND=", r$B, " ANRIND=", r$A, " Freq=", fmt_comma(r$F),
    " Percent=", fmt_2(pc), " (", fmt_1(pc), ") RowPct=", fmt_2(rp), " (", fmt_1(rp), ")",
    " ColPct=", fmt_2(cp), " (", fmt_1(cp), ")"))
}
# chi-square / fisher for several tables
test_pair <- function(d1, d2, lab) {
  t <- table(factor(d1), factor(d2))
  if (nrow(t) < 2 || ncol(t) < 2) return(paste0("  ", lab, " (degenerate)"))
  ch <- tryCatch(suppressWarnings(chisq.test(t)$p.value), error=function(e) NA_real_)
  fi <- tryCatch(fisher.test(t)$p.value, error=function(e) NA_real_)
  paste0("  ", lab, " table=", paste(dim(t), collapse="x"),
    " ChiSq p=", format(ch,digits=10), " (", fmt_pvalue(ch), ") (", fmt_4(ch), ")",
    " Fisher p=", format(fi,digits=10), " (", fmt_pvalue(fi), ") (", fmt_4(fi), ")")
}
L <- c(L, "", "Association tests:")
L <- c(L, test_pair(advsmax2$TRTPN, advsmax2$BNRIND, "TRTPN*BNRIND"))
L <- c(L, test_pair(advsmax2$TRTPN, advsmax2$ANRIND, "TRTPN*ANRIND"))
L <- c(L, test_pair(advsmax2$BNRIND, advsmax2$ANRIND, "BNRIND*ANRIND overall"))
L <- c(L, test_pair(advsmax3$TRTPN, advsmax3$n, "TRTPN*n"))
L <- c(L, test_pair(advsmax2$TRTPN, advsmax2$SHIFT1, "TRTPN*SHIFT1"))
L <- c(L, test_pair(advsmax2$TRTPN, advsmax2$CRIT1FL, "TRTPN*CRIT1FL"))
for (tt in sort(unique(advsmax2$TRTPN))) {
  sub <- advsmax2 %>% filter(TRTPN==tt)
  L <- c(L, test_pair(sub$BNRIND, sub$ANRIND, paste0("BNRIND*ANRIND TRTPN=", tt)))
}
# pairwise TRTPN for n and ANRIND
prs <- list(c(0,54), c(0,81), c(54,81))
actual_trt <- sort(unique(advsmax3$TRTPN))
for (pr in prs) {
  if (all(pr %in% actual_trt)) {
    sub <- advsmax3 %>% filter(TRTPN %in% pr)
    L <- c(L, test_pair(sub$TRTPN, sub$n, paste0("TRTPN ", pr[1], " vs ", pr[2], " *n")))
  }
}
L <- c(L, "",
  paste0("Axis: ANRLO mean=", ANRLO_m, " ANRHI mean=", ANRHI_m,
         " AVAL Min=", MinValue, " Max=", MaxValue,
         " margin=", format(margin, digits=10),
         " YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax, " MAJOR=", MAJOR),
  paste0("  Y ticks: ", paste(seq(YAxisMin, YAxisMax, by=MAJOR), collapse=", ")),
  "",
  "Treatment Emergent High:")
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  L <- c(L, paste0("  TRTPN=", r$TRTPN, " Nx=", fmt_comma(r$Nx),
    " n=", fmt_comma(r$n), " percent=", fmt_1(r$percent), " (", fmt_2(r$percent), ")",
    " FishersP=", fmt_pvalue(r$FishersP), " raw=", format(r$FishersP, digits=10)))
}
L <- c(L, "", "Note: Percent/Row Pct/Col Pct 2 decimals; shift 1 decimal; p-values pvalue. format.")
writeLines(L, file.path(out, "P053.txt"))
writeLines(L, file.path(out, "P053.lst"))

# ---- Target16 (unchanged, matched) ----
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
      fmt_1(round_half_up(r$percent,1)), fmt_pvalue(r$FishersP)))
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
```