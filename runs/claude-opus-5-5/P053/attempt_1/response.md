```r
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rhu <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
fmt1 <- function(x) ifelse(is.na(x), "", formatC(rhu(x, 1), format = "f", digits = 1))
pvalfmt <- function(p) ifelse(is.na(p), "", ifelse(p < 0.0001, "<.0001",
                          formatC(rhu(p, 4), format = "f", digits = 4)))
num2c <- function(x) { s <- format(x, digits = 12, trim = TRUE); s }

# ---------------------------------------------------------------- data
advsmax <- read_xpt("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt")
advsmax <- as.data.frame(lapply(advsmax, function(v) { attributes(v) <- attributes(v)[intersect(names(attributes(v)), c("names"))]; v }),
                         stringsAsFactors = FALSE)
names(advsmax) <- toupper(names(advsmax))
chr <- sapply(advsmax, is.character)
advsmax[chr] <- lapply(advsmax[chr], function(v) ifelse(is.na(v), "", v))

keepv <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL",
           "ANRLO","ANRHI","BASE")
advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP", !is.na(ATPTN) & ATPTN == 815, ANL01FL == "Y",
         !is.na(ADY) & ADY > 1, SAFFL == "Y")
advsmax2 <- advsmax2[, names(advsmax2)[names(advsmax2) %in% keepv], drop = FALSE]
advsmax2$Ref <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)

# ---------------------------------------------------------------- shift table
advsmax2 <- advsmax2 %>% arrange(TRTPN)   # stable sort

TableA <- advsmax2 %>% group_by(TRTPN) %>% mutate(.tot = n()) %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(COUNT = n(), .tot = first(.tot), .groups = "drop") %>%
  mutate(PERCENT = 100 * COUNT / .tot) %>% select(-.tot) %>%
  arrange(TRTPN, BNRIND, ANRIND)
BNRIND_ds <- advsmax2 %>% group_by(TRTPN) %>% mutate(.tot = n()) %>% group_by(TRTPN, BNRIND) %>%
  summarise(COUNT = n(), PERCENT = 100 * n() / first(.tot), .groups = "drop")
ARIND_ds <- advsmax2 %>% group_by(TRTPN) %>% mutate(.tot = n()) %>% group_by(TRTPN, ANRIND) %>%
  summarise(COUNT = n(), PERCENT = 100 * n() / first(.tot), .groups = "drop")
TRTPN_ds <- advsmax2 %>% group_by(TRTPN) %>% summarise(COUNT = n(), PERCENT = 100, .groups = "drop")

TableA$ValueFmt <- paste0(format(TableA$COUNT, big.mark = ",", trim = TRUE), " (", fmt1(TableA$PERCENT), ")")
TableA <- TableA %>% left_join(TRTPN_ds %>% rename(GroupN = COUNT, GroupPercent = PERCENT), by = "TRTPN") %>%
  mutate(GroupFmt = paste0(num2c(TRTPN), " (N = ", format(GroupN, big.mark = ",", trim = TRUE), ")"))

TableAt <- TableA %>% arrange(GroupFmt, BNRIND) %>%
  mutate(.id = paste0("ValueFmt", ANRIND)) %>%
  select(GroupFmt, BNRIND, .id, ValueFmt) %>%
  pivot_wider(names_from = .id, values_from = ValueFmt) %>%
  mutate(`_NAME_` = "ValueFmt", .after = BNRIND)

# ---------------------------------------------------------------- emergent high
advsmax2$.n_ <- seq_len(nrow(advsmax2))
advsmax3 <- advsmax2 %>% filter(CRIT1FL != "") %>%
  mutate(n = as.numeric(.n_ %% 17 == 1)) %>% select(-.n_)
advsmax2$.n_ <- NULL

EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>%
  summarise(`_TYPE_` = 1, `_FREQ_` = n(), Nx = sum(!is.na(n)), n = sum(n), percent = mean(n), .groups = "drop")
# careful: mean computed after sum replaced n; recompute
EmergentHigh$percent <- (advsmax3 %>% group_by(TRTPN) %>% summarise(p = mean(n)))$p

ctab <- table(advsmax3$TRTPN, advsmax3$n)
FishersExact <- data.frame(Name1 = "XP2_FISH", nValue1 = NA_real_)
tabprob <- NA_real_; fp <- NA_real_
if (nrow(ctab) >= 2 && ncol(ctab) >= 2) {
  fp <- tryCatch(fisher.test(ctab, workspace = 2e7)$p.value, error = function(e)
    fisher.test(ctab, simulate.p.value = TRUE, B = 1e6)$p.value)
  N <- sum(ctab)
  tabprob <- exp(sum(lfactorial(rowSums(ctab))) + sum(lfactorial(colSums(ctab))) -
                 lfactorial(N) - sum(lfactorial(ctab)))
  FishersExact <- data.frame(Table = "Table TRTPN * n", Name1 = c("P_TABLE", "XP2_FISH"),
                             Label1 = c("Table Probability (P)", "Pr <= P"),
                             cValue1 = c(formatC(tabprob, format = "e", digits = 3), pvalfmt(fp)),
                             nValue1 = c(tabprob, fp))
}
PValue <- FishersExact %>% filter(Name1 == "XP2_FISH") %>% rename(FishersP = nValue1)
EmergentHigh$FishersP <- NA_real_
if (nrow(PValue) > 0) EmergentHigh$FishersP[1] <- PValue$FishersP[1]
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), 100 * EmergentHigh$percent, NA)

# ---------------------------------------------------------------- summary for graph
ADVSMAXSum <- data.frame(`_TYPE_` = 0, `_FREQ_` = nrow(advsmax2),
                         ANRLO = mean(advsmax2$ANRLO, na.rm = TRUE), ANRHI = mean(advsmax2$ANRHI, na.rm = TRUE),
                         BASE = mean(advsmax2$BASE, na.rm = TRUE), AVAL = mean(advsmax2$AVAL, na.rm = TRUE),
                         MinValue = min(advsmax2$AVAL, na.rm = TRUE), MaxValue = max(advsmax2$AVAL, na.rm = TRUE),
                         check.names = FALSE)
MAJOR <- 10
ANRLO <- ADVSMAXSum$ANRLO; ANRHI <- ADVSMAXSum$ANRHI
margin <- (ADVSMAXSum$MaxValue - ADVSMAXSum$MinValue) / 100
YMIN <- MAJOR * floor((ADVSMAXSum$MinValue - margin) / MAJOR)
YMAX <- MAJOR * ceiling((ADVSMAXSum$MaxValue + margin) / MAJOR)

pd <- advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, Ref) %>% mutate(TRTPN_f = factor(TRTPN))
lvl <- levels(pd$TRTPN_f)
shp <- setNames(c(17, 1, 4, 15, 16)[seq_along(lvl)], lvl)
cols <- setNames(c("green", "red", "blue", "orange", "purple")[seq_along(lvl)], lvl)
fitd <- pd %>% filter(!is.na(Ref), !is.na(BASE))
p <- ggplot(pd, aes(BASE, AVAL)) +
  geom_hline(yintercept = c(ANRLO, ANRHI), linetype = "dashed", colour = c("blue", "red")) +
  geom_vline(xintercept = c(ANRLO, ANRHI), linetype = "dashed", colour = c("blue", "red")) +
  geom_point(aes(shape = TRTPN_f, colour = TRTPN_f), na.rm = TRUE) +
  scale_shape_manual(values = shp, name = "TRTPN") + scale_colour_manual(values = cols, name = "TRTPN") +
  scale_y_continuous(limits = c(YMIN, YMAX), breaks = seq(YMIN, YMAX, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() + theme(legend.position = "bottom")
if (nrow(fitd) > 1) p <- p + geom_smooth(data = fitd, aes(BASE, Ref), method = "lm", formula = y ~ x,
                                         se = FALSE, colour = "black", linetype = "dashed", inherit.aes = FALSE)
ggsave(file.path(out, "sasgraph.png"), p, width = 8, height = 6, dpi = 100)
write.csv(pd %>% select(-TRTPN_f), file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------- report renderings
percpar <- function(x) ifelse(is.na(x), "", paste0("(", formatC(floor(x * 10 + 1e-9) / 10, format = "f", digits = 1), ")"))
anr <- sort(unique(TableA$ANRIND))
rep1 <- TableA %>% group_by(GroupFmt, BNRIND, ANRIND) %>%
  summarise(count = sum(COUNT), percent = sum(PERCENT), .groups = "drop") %>% arrange(GroupFmt, BNRIND)
rep1w <- rep1 %>% mutate(cell_c = formatC(count, width = 4), cell_p = percpar(percent)) %>%
  select(GroupFmt, BNRIND, ANRIND, cell_c, cell_p) %>%
  pivot_wider(names_from = ANRIND, values_from = c(cell_c, cell_p), names_glue = "{ANRIND}_{.value}")
ordcols <- c("GroupFmt", "BNRIND", unlist(lapply(anr, function(a) paste0(a, c("_cell_c", "_cell_p")))))
ordcols <- ordcols[ordcols %in% names(rep1w)]
rep1w <- rep1w[, ordcols]; rep1w[is.na(rep1w)] <- ""
names(rep1w) <- sub("_cell_c$", "_N", sub("_cell_p$", "_PCT", names(rep1w)))

rep2 <- data.frame(TRTPN = EmergentHigh$TRTPN, Nx = EmergentHigh$Nx, n = EmergentHigh$n,
                   percent = formatC(rhu(EmergentHigh$percent, 1), format = "f", digits = 1, width = 6),
                   FishersP = pvalfmt(EmergentHigh$FishersP), stringsAsFactors = FALSE)

render <- function(df) {
  m <- rbind(names(df), as.matrix(format(df)))
  w <- apply(m, 2, function(z) max(nchar(z)))
  apply(m, 1, function(r) paste(formatC(r, width = w), collapse = "  "))
}

# FREQ crosstab text
ftxt <- c("The FREQ Procedure", "", "Table of TRTPN by n", "")
if (length(ctab) > 0) {
  N <- sum(ctab)
  for (i in seq_len(nrow(ctab))) {
    for (j in seq_len(ncol(ctab))) {
      ftxt <- c(ftxt, sprintf("TRTPN=%s n=%s  Frequency=%d  Percent=%.2f  Row Pct=%.2f  Col Pct=%.2f",
                rownames(ctab)[i], colnames(ctab)[j], ctab[i, j], rhu(100 * ctab[i, j] / N, 2),
                rhu(100 * ctab[i, j] / sum(ctab[i, ]), 2), rhu(100 * ctab[i, j] / sum(ctab[, j]), 2)))
    }
    ftxt <- c(ftxt, sprintf("TRTPN=%s Total=%d  Percent=%.2f", rownames(ctab)[i], sum(ctab[i, ]), rhu(100 * sum(ctab[i, ]) / N, 2)))
  }
  for (j in seq_len(ncol(ctab))) ftxt <- c(ftxt, sprintf("n=%s Total=%d  Percent=%.2f", colnames(ctab)[j], sum(ctab[, j]), rhu(100 * sum(ctab[, j]) / N, 2)))
  ftxt <- c(ftxt, sprintf("Total=%d  Percent=100.00", N), "", "Fisher's Exact Test",
            sprintf("Table Probability (P)  %s", formatC(tabprob, format = "e", digits = 3)),
            sprintf("Pr <= P                %s", pvalfmt(fp)), "", sprintf("Sample Size = %d", N))
}

reptxt <- c("Max Post Basline Result (shift from baseline)", render(rep1w), "",
            "Treatment Emergent High", render(rep2), "",
            "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
            "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
            sprintf("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx %s mmol/L ULNx %s mmol/L) set 2 (Female, Age 18-65, LLN=%s mmol/L ULN=%s mmol/L).",
                    num2c(ANRLO), num2c(ANRHI), num2c(ANRLO), num2c(ANRHI)))

writeLines(c(ftxt, "", reptxt), file.path(out, "P053.txt"))
writeLines(reptxt, file.path(out, "Target16.txt"))
freqcsv <- as.data.frame(ctab, stringsAsFactors = FALSE)
if (nrow(freqcsv)) names(freqcsv) <- c("TRTPN", "n", "Frequency")
write.csv(freqcsv, file.path(out, "P053_freq.csv"), row.names = FALSE, na = "")
write.csv(rep1w, file.path(out, "P053.csv"), row.names = FALSE, na = "")
write.csv(rep1w, file.path(out, "Target16.csv"), row.names = FALSE, na = "")
write.csv(rep2, file.path(out, "Target16_emergent.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------- datasets
wcsv <- function(df, nm) write.csv(as.data.frame(df), file.path(out, paste0(nm, ".csv")), row.names = FALSE, na = "")
wcsv(advsmax2, "advsmax2"); wcsv(TableA, "tablea"); wcsv(TableAt, "tableat")
wcsv(advsmax3, "advsmax3"); wcsv(EmergentHigh, "emergenthigh"); wcsv(FishersExact, "fishersexact")
wcsv(PValue, "pvalue"); wcsv(ADVSMAXSum, "advsmaxsum"); wcsv(BNRIND_ds, "bnrind")
wcsv(ARIND_ds, "arind"); wcsv(TRTPN_ds, "trtpn")
cat("P053 done\n")
```