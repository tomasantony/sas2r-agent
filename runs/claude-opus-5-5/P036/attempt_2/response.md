```r
# P036 - Target16: DIABP max post-baseline vs max baseline (scatter + shift table + emergent high table)
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ---------------- helpers ----------------
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}
fmt_w_d <- function(x, d) ifelse(is.na(x), "", formatC(round_half_up(x, d), format = "f", digits = d))
comma_fmt <- function(x) ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ","))
best12 <- function(x) {
  if (is.na(x)) return(".")
  if (abs(x - round(x)) < 1e-9) return(as.character(round(x)))
  s <- formatC(x, digits = 11, format = "g")
  s <- sub("0+$", "", s); s <- sub("\\.$", "", s)
  while (nchar(s) > 12) {
    d <- nchar(sub("^[^.]*\\.", "", s)) - 1
    s <- formatC(round(x, d), format = "f", digits = d)
  }
  trimws(s)
}
pvalue_fmt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0001, "<.0001", formatC(p, format = "f", digits = 4)))
prob_fmt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0001 & p > 0, formatC(p, format = "E", digits = 3),
                                                    formatC(round_half_up(p, 4), format = "f", digits = 4)))
# picture percpar: "009.9)" prefix "(" -> truncation (no ROUND option)
percpar <- function(x) ifelse(is.na(x), "",
                              paste0("(", formatC(floor(x * 10 + 1e-7) / 10, format = "f", digits = 1), ")"))
sas_order <- function(...) order(..., method = "radix")

# ---------------- data ----------------
advsmax <- read_xpt(file.path(rawadam, "advsmax.xpt"))
advsmax <- as.data.frame(advsmax)
for (v in names(advsmax)) if (is.character(advsmax[[v]])) advsmax[[v]][is.na(advsmax[[v]])] <- ""

advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP", !is.na(ATPTN) & ATPTN == 815, ANL01FL == "Y",
         !is.na(ADY) & ADY > 1, SAFFL == "Y")
advsmax2$Ref <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
keepv <- intersect(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1",
                     "CRIT1FL","ANRLO","ANRHI","BASE","Ref"), names(advsmax2))
advsmax2 <- advsmax2[, keepv]
advsmax2 <- advsmax2[order(!is.na(advsmax2$TRTPN), advsmax2$TRTPN, method = "radix"), ]
rownames(advsmax2) <- NULL

# ---------------- baseline shift frequencies ----------------
TableA <- advsmax2 %>% group_by(TRTPN, BNRIND, ANRIND) %>% summarise(COUNT = n(), .groups = "drop") %>%
  group_by(TRTPN) %>% mutate(PERCENT = 100 * COUNT / sum(COUNT)) %>% ungroup() %>% as.data.frame()
TableA <- TableA[sas_order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND), ]
TableA$ValueFmt <- paste0(trimws(comma_fmt(TableA$COUNT)), " (", trimws(fmt_w_d(TableA$PERCENT, 1)), ")")

TRTPNf <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop") %>%
  mutate(GroupPercent = 100 * GroupN / sum(GroupN))
TableA <- merge(TableA, TRTPNf, by = "TRTPN", all = TRUE, sort = FALSE)
TableA <- TableA[sas_order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND), ]
TableA$GroupFmt <- paste0(sapply(TableA$TRTPN, best12), " (N = ", trimws(comma_fmt(TableA$GroupN)), ")")
write.csv(TableA, file.path(out, "tablea.csv"), row.names = FALSE, na = "")

# ---------------- treatment emergent high ----------------
advsmax2$`_N_` <- seq_len(nrow(advsmax2))
advsmax3 <- advsmax2[advsmax2$CRIT1FL != "", ]
advsmax3$n <- as.numeric(advsmax3$`_N_` %% 17 == 1)
advsmax3$`_N_` <- NULL; advsmax2$`_N_` <- NULL

EmergentHigh <- advsmax3 %>% filter(!is.na(TRTPN)) %>% group_by(TRTPN) %>%
  summarise(`_TYPE_` = 1, `_FREQ_` = n(), Nx = sum(!is.na(n)), n_ = sum(n), percent = mean(n), .groups = "drop") %>%
  arrange(TRTPN) %>% as.data.frame()
names(EmergentHigh)[names(EmergentHigh) == "n_"] <- "n"

tab <- table(advsmax3$TRTPN, advsmax3$n)
fisherP <- NA_real_; fish_rows <- NULL; chi_rows <- NULL
if (nrow(tab) >= 2 && ncol(tab) >= 2) {
  ft <- tryCatch(fisher.test(tab, workspace = 2e8), error = function(e) fisher.test(tab, simulate.p.value = TRUE, B = 1e6))
  fisherP <- ft$p.value
  N <- sum(tab)
  tprob <- exp(sum(lfactorial(rowSums(tab))) + sum(lfactorial(colSums(tab))) - lfactorial(N) - sum(lfactorial(tab)))
  # chi-square statistics (EXACT option also produces CHISQ output)
  O <- unclass(tab); E <- outer(rowSums(O), colSums(O)) / N
  dfc <- (nrow(O) - 1) * (ncol(O) - 1)
  X2 <- sum((O - E)^2 / E)
  G2 <- 2 * sum(ifelse(O > 0, O * log(O / E), 0))
  rsc <- as.numeric(rownames(O)); csc <- as.numeric(colnames(O))
  xx <- rep(rep(rsc, ncol(O)), as.vector(O)); yy <- rep(rep(csc, each = nrow(O)), as.vector(O))
  rr <- suppressWarnings(cor(xx, yy)); MH <- (N - 1) * rr^2
  if (all(dim(O) == 2)) {
    phi <- (O[1,1]*O[2,2] - O[1,2]*O[2,1]) / sqrt(prod(rowSums(O)) * prod(colSums(O)))
  } else phi <- sqrt(X2 / N)
  cc <- sqrt(X2 / (X2 + N)); cv <- if (all(dim(O) == 2)) phi else sqrt(X2 / N / min(dim(O) - 1))
  chi_rows <- data.frame(
    Statistic = c("Chi-Square", "Likelihood Ratio Chi-Square", "Mantel-Haenszel Chi-Square",
                  "Phi Coefficient", "Contingency Coefficient", "Cramer's V"),
    DF = c(dfc, dfc, 1, NA, NA, NA),
    Value = fmt_w_d(c(X2, G2, MH, phi, cc, cv), 4),
    Prob = c(pvalue_fmt(c(pchisq(X2, dfc, lower.tail = FALSE), pchisq(G2, dfc, lower.tail = FALSE),
                          pchisq(MH, 1, lower.tail = FALSE))), "", "", ""),
    stringsAsFactors = FALSE)
  pct5 <- 100 * mean(E < 5)
  if (all(dim(tab) == 2)) {
    pl <- fisher.test(tab, alternative = "less")$p.value
    pr <- fisher.test(tab, alternative = "greater")$p.value
    fish_rows <- data.frame(Label = c("Cell (1,1) Frequency (F)", "Left-sided Pr <= F", "Right-sided Pr >= F",
                                      "Table Probability (P)", "Two-sided Pr <= P"),
                            Value = c(as.character(tab[1, 1]), pvalue_fmt(pl), pvalue_fmt(pr),
                                      prob_fmt(tprob), pvalue_fmt(fisherP)))
  } else {
    fish_rows <- data.frame(Label = c("Table Probability (P)", "Pr <= P"),
                            Value = c(prob_fmt(tprob), pvalue_fmt(fisherP)))
  }
}
EmergentHigh$Name1 <- c("XP2_FISH", rep("", nrow(EmergentHigh) - 1))
EmergentHigh$FishersP <- c(fisherP, rep(NA_real_, nrow(EmergentHigh) - 1))
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), 100 * EmergentHigh$percent, NA)
write.csv(EmergentHigh, file.path(out, "emergenthigh.csv"), row.names = FALSE, na = "")

# ---------------- P036.lst: PROC FREQ crosstab + statistics ----------------
lst <- c("The FREQ Procedure", "", "Table of TRTPN by n", "")
ct_rows <- list()
if (length(tab) > 0) {
  N <- sum(tab); rs <- rowSums(tab); cs <- colSums(tab)
  hdr <- sprintf("%-10s %s %10s", "TRTPN", paste(sprintf("%10s", colnames(tab)), collapse = " "), "Total")
  lst <- c(lst, "Frequency / Percent / Row Pct / Col Pct", hdr)
  for (i in seq_len(nrow(tab))) {
    f <- tab[i, ]
    lst <- c(lst,
      sprintf("%-10s %s %10d", rownames(tab)[i], paste(sprintf("%10d", f), collapse = " "), rs[i]),
      sprintf("%-10s %s %10s", "", paste(sprintf("%10s", fmt_w_d(100 * f / N, 2)), collapse = " "), fmt_w_d(100 * rs[i] / N, 2)),
      sprintf("%-10s %s", "", paste(sprintf("%10s", fmt_w_d(100 * f / rs[i], 2)), collapse = " ")),
      sprintf("%-10s %s", "", paste(sprintf("%10s", fmt_w_d(100 * f / cs, 2)), collapse = " ")))
    for (j in seq_len(ncol(tab))) ct_rows[[length(ct_rows) + 1]] <- data.frame(
      section = "crosstab", TRTPN = rownames(tab)[i], n = colnames(tab)[j], Frequency = f[j],
      Percent = fmt_w_d(100 * f[j] / N, 2), RowPct = fmt_w_d(100 * f[j] / rs[i], 2),
      ColPct = fmt_w_d(100 * f[j] / cs[j], 2), stringsAsFactors = FALSE)
  }
  lst <- c(lst, sprintf("%-10s %s %10d", "Total", paste(sprintf("%10d", cs), collapse = " "), N),
           sprintf("%-10s %s %10s", "", paste(sprintf("%10s", fmt_w_d(100 * cs / N, 2)), collapse = " "), "100.00"), "",
           "Statistics for Table of TRTPN by n", "")
  if (!is.null(chi_rows)) {
    lst <- c(lst, sprintf("%-30s %4s %10s %8s", "Statistic", "DF", "Value", "Prob"),
             sprintf("%-30s %4s %10s %8s", chi_rows$Statistic, ifelse(is.na(chi_rows$DF), "", chi_rows$DF),
                     chi_rows$Value, chi_rows$Prob), "",
             sprintf("WARNING: %s%% of the cells have expected counts less", formatC(round_half_up(pct5, 0), format = "d")),
             "than 5. Chi-Square may not be a valid test.", "")
  }
  lst <- c(lst, "Fisher's Exact Test")
  if (!is.null(fish_rows)) lst <- c(lst, sprintf("%-30s %12s", fish_rows$Label, fish_rows$Value))
  lst <- c(lst, "", sprintf("Sample Size = %d", N))
}
ct_df <- if (length(ct_rows)) do.call(rbind, ct_rows) else data.frame()
if (!is.null(chi_rows)) ct_df <- bind_rows(ct_df, data.frame(section = "chisq", TRTPN = chi_rows$Statistic,
                                                             n = chi_rows$Value, Prob = chi_rows$Prob,
                                                             DF = as.character(chi_rows$DF), stringsAsFactors = FALSE))
if (!is.null(fish_rows)) ct_df <- bind_rows(ct_df, data.frame(section = "fisher", TRTPN = fish_rows$Label,
                                                              n = fish_rows$Value, stringsAsFactors = FALSE))

# ---------------- graph ----------------
MAJOR <- 10
ANRLO <- mean(advsmax2$ANRLO, na.rm = TRUE); ANRHI <- mean(advsmax2$ANRHI, na.rm = TRUE)
minv <- min(advsmax2$AVAL, na.rm = TRUE); maxv <- max(advsmax2$AVAL, na.rm = TRUE)
margin <- (maxv - minv) / 100
YAxisMin <- MAJOR * floor((minv - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((maxv + margin) / MAJOR)
ANRLOc <- best12(ANRLO); ANRHIc <- best12(ANRHI)

pd <- advsmax2 %>% filter(!is.na(AVAL), !is.na(BASE)) %>% mutate(TRTPNf = factor(TRTPN))
write.csv(advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, Ref, ANRLO, ANRHI),
          file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
lv <- levels(pd$TRTPNf)
shp <- setNames(c(17, 1, 4, 15, 16)[seq_along(lv)], lv)
col <- setNames(c("green", "red", "blue", "purple", "orange")[seq_along(lv)], lv)
p <- ggplot(pd, aes(BASE, AVAL)) +
  geom_hline(yintercept = c(ANRLO, ANRHI), linetype = "dashed", colour = c("blue", "red")) +
  geom_vline(xintercept = c(ANRLO, ANRHI), linetype = "dashed", colour = c("blue", "red")) +
  geom_point(aes(shape = TRTPNf, colour = TRTPNf), size = 2) +
  geom_smooth(data = pd %>% filter(!is.na(Ref)), aes(BASE, Ref), method = "lm", formula = y ~ x,
              se = FALSE, colour = "black", linetype = "dashed", linewidth = 0.6) +
  scale_shape_manual(values = shp, name = "TRTPN") + scale_colour_manual(values = col, name = "TRTPN") +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() + theme(legend.position = "bottom")
ggsave(file.path(out, "sasgraph.png"), p, width = 8, height = 6, dpi = 100)

# ---------------- shift report ----------------
rep_in <- TableA %>% filter(BNRIND != "", ANRIND != "")
acr <- sort(unique(rep_in$ANRIND), method = "radix")
grp <- unique(rep_in[sas_order(rep_in$GroupFmt, rep_in$BNRIND), c("GroupFmt", "BNRIND")])
shift <- grp
for (a in acr) {
  sub <- rep_in[rep_in$ANRIND == a, ]
  m <- match(paste(grp$GroupFmt, grp$BNRIND), paste(sub$GroupFmt, sub$BNRIND))
  shift[[paste0(a, "_count")]] <- sub$COUNT[m]
  shift[[paste0(a, "_percent")]] <- sub$PERCENT[m]
}
shift_disp <- grp
for (a in acr) {
  shift_disp[[paste0(a, "_count")]] <- ifelse(is.na(shift[[paste0(a, "_count")]]), "",
                                              sprintf("%4d", shift[[paste0(a, "_count")]]))
  shift_disp[[paste0(a, "_percent")]] <- percpar(shift[[paste0(a, "_percent")]])
}
eh_disp <- data.frame(TRTPN = EmergentHigh$TRTPN, Nx = EmergentHigh$Nx, n = EmergentHigh$n,
                      percent = fmt_w_d(EmergentHigh$percent, 1), FishersP = pvalue_fmt(EmergentHigh$FishersP))

shift_txt <- c("Max Post Basline Result (shift from baseline)",
               paste(sprintf("%-16s", "Treatment"), sprintf("%-10s", "Max. Baseline Result"),
                     paste(sprintf("%-14s", acr), collapse = " ")))
prevg <- ""
for (i in seq_len(nrow(shift_disp))) {
  g <- shift_disp$GroupFmt[i]
  if (prevg != "" && g != prevg) shift_txt <- c(shift_txt, " ")
  cells <- sapply(acr, function(a) sprintf("%-14s", paste(shift_disp[[paste0(a, "_count")]][i],
                                                          shift_disp[[paste0(a, "_percent")]][i])))
  shift_txt <- c(shift_txt, paste(sprintf("%-16s", ifelse(g == prevg, "", g)),
                                  sprintf("%-20s", shift_disp$BNRIND[i]), paste(cells, collapse = " ")))
  prevg <- g
}
shift_txt <- c(shift_txt, " ")
eh_txt <- c("Treatment Emergent High",
            sprintf("%8s %8s %8s %8s %8s", "TRTPN", "Nx", "n", "%", "FishersP"),
            sprintf("%8s %8s %8s %8s %8s", eh_disp$TRTPN, eh_disp$Nx, eh_disp$n, eh_disp$percent, eh_disp$FishersP))
foot <- c("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
          "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
          paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", ANRLOc, " mmol/L ULNx ", ANRHIc,
                 " mmol/L) set 2 (Female, Age 18-65, LLN=", ANRLOc, " mmol/L ULN=", ANRHIc, " mmol/L)."))

writeLines(c("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", "",
             shift_txt, "", eh_txt, "", foot), file.path(out, "Target16.txt"))
t16_csv <- bind_rows(
  shift_disp %>% mutate(across(everything(), as.character), section = "shift"),
  eh_disp %>% mutate(across(everything(), as.character), section = "emergent_high"))
write.csv(t16_csv, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

writeLines(c(lst, "", "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", "",
             shift_txt, "", eh_txt), file.path(out, "P036.txt"))
lst_csv <- bind_rows(ct_df %>% mutate(across(everything(), as.character)), t16_csv)
write.csv(lst_csv, file.path(out, "P036.csv"), row.names = FALSE, na = "")

cat("P036 done. Rows advsmax2:", nrow(advsmax2), " advsmax3:", nrow(advsmax3), "\n")
```