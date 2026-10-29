```r
# P057_pk_mean_conc.sas -> R translation
# Target13: Arithmetic mean concentration-time plot (ANAL1, MEANTYPE=A, SD=BOTH)

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

## SAS round half away from zero helper
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

## ---- locate adpc.xpt ----
candidates <- c(
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adpc.xpt"),
  "/data/raw/cdisc-pilot/adam/adpc.xpt"
)
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) {
  # last resort: search under OUT_DIR parent tree
  poss <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^adpc\\.xpt$",
                     recursive = TRUE, full.names = TRUE)
  if (length(poss) > 0) adpc_path <- poss[1]
}
if (is.null(adpc_path)) stop("adpc.xpt not found")
message("Reading: ", adpc_path)

adpc_all <- haven::read_xpt(adpc_path)

## Normalize names to upper case for matching (SAS case-insensitive, case-sensitive compare)
names(adpc_all) <- toupper(names(adpc_all))

## ---- data work.adpc : keep + where PCTESTCD="ANAL1" ----
# keep=ANLPRNT TRTAN ANLPRNT PCSPEC PCTESTL TRTA DOSREFID PCTESTCD PCORRESU PCORRES USUBJID PCORRESN EPLTM
PCTESTCD_val <- "ANAL1"
MEANTYPE <- "A"
SDOPT <- "BOTH"

keepvars <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
              "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keepvars <- keepvars[keepvars %in% names(adpc_all)]

adpc <- adpc_all %>%
  filter(PCTESTCD == PCTESTCD_val) %>%
  select(all_of(keepvars))

## PCTPT=input(EPLTM,best10.) : SAS character->numeric; if EPLTM is numeric keep as-is
## haven may return labelled; strip labels
unlabel_vec <- function(v) {
  if (inherits(v, "labelled")) v <- haven::zap_labels(v)
  v
}
adpc <- adpc %>% mutate(across(everything(), unlabel_vec))

if ("EPLTM" %in% names(adpc)) {
  e <- adpc$EPLTM
  if (is.numeric(e)) {
    adpc$PCTPT <- as.numeric(e)
  } else {
    # SAS INPUT strips blanks then best10.
    adpc$PCTPT <- suppressWarnings(as.numeric(trimws(as.character(e))))
  }
} else {
  stop("EPLTM not found")
}

## ORDERBY / BYVAL6 (kept for fidelity, not used in render except label)
if (all(c("ANLPRNT","PCSPEC","PCTESTL","TRTAN") %in% names(adpc))) {
  adpc$ORDERBY <- paste0(as.character(adpc$ANLPRNT), as.character(adpc$PCSPEC),
                         as.character(adpc$PCTESTL), as.character(adpc$TRTAN))
}
if (all(c("PCTESTCD","PCORRESU") %in% names(adpc))) {
  # compbl(propcase(pctestcd)||' Conc '||' ('||PCORRESU||')')
  propcase <- function(x) {
    # SAS PROPCASE: first letter upper, rest lower per word
    sapply(x, function(s) {
      if (is.na(s)) return(NA_character_)
      s <- tolower(s)
      gsub("\\b([a-z])", "\\U\\1", s, perl = TRUE)
    }, USE.NAMES = FALSE)
  }
  adpc$BYVAL6 <- trimws(gsub("\\s+", " ",
    paste0(propcase(as.character(adpc$PCTESTCD)), " Conc ",
           " (", as.character(adpc$PCORRESU), ")")))
}

## Ensure types: TRTAN numeric, TRTA character, PCORRESN numeric
if ("TRTAN" %in% names(adpc)) adpc$TRTAN <- suppressWarnings(as.numeric(as.character(adpc$TRTAN)))
if ("TRTA" %in% names(adpc)) adpc$TRTA <- as.character(adpc$TRTA)
adpc$PCORRESN <- suppressWarnings(as.numeric(as.character(adpc$PCORRESN)))

## ---- proc sort by TRTAN TRTA PCTPT (SAS: missing first, ASCII order) ----
# Missing first: arrange with is.na; ASCII (binary) order for TRTA
adpc$TRTA_sortkey <- sapply(adpc$TRTA, function(s) {
  if (is.na(s)) return("")
  paste(sprintf("%03d", utf8ToInt(s)), collapse = "-")
})
ord <- order(ifelse(is.na(adpc$TRTAN), -Inf, adpc$TRTAN),
             adpc$TRTA_sortkey,
             ifelse(is.na(adpc$PCTPT), -Inf, adpc$PCTPT),
             na.last = FALSE)
adpc <- adpc[ord, , drop = FALSE]
adpc$TRTA_sortkey <- NULL

## ---- proc univariate by TRTAN TRTA PCTPT var PCORRESN -> mean, std (sample, N-1) ----
# SAS PROC UNIVARIATE STD = sample std dev; missing excluded
pcmeans <- adpc %>%
  group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    PCMEAN = ifelse(sum(!is.na(PCORRESN)) > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
    PCSD = ifelse(sum(!is.na(PCORRESN)) > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(TRTAN, PCTPT)

## data MeanData; if PCMEAN=0 then PCMEAN=.; PCSD=.;
meandata <- pcmeans %>%
  mutate(
    PCMEAN = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCMEAN),
    PCSD = ifelse(!is.na(PCMEAN) & PCMEAN == 0, NA_real_, PCSD)
  )
# Note: after above, rows where mean was 0 now have PCMEAN NA; ensure PCSD NA too
meandata$PCSD[is.na(meandata$PCMEAN)] <- ifelse(is.na(meandata$PCMEAN) & meandata$N > 0 & !is.na(meandata$PCSD), meandata$PCSD, meandata$PCSD)
# Faithful: if original mean==0 set both missing
# Recompute correctly: need original mean before overwrite
# (redo to be exact)
pcmeans2 <- pcmeans
meandata <- pcmeans2
zero_idx <- !is.na(meandata$PCMEAN) & meandata$PCMEAN == 0
meandata$PCMEAN[zero_idx] <- NA_real_
meandata$PCSD[zero_idx] <- NA_real_

## Sort MeanData by TRTAN TRTA PCTPT for plotting
meandata <- meandata %>% arrange(TRTAN, TRTA, PCTPT)

## Y label as in SAS sgrender
ylabel <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
xlabel <- "Time (h)"
XMAX <- 100
TICKS <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)

## Error bars for SD=BOTH
meandata$LOWER <- meandata$PCMEAN - meandata$PCSD
meandata$UPPER <- meandata$PCMEAN + meandata$PCSD
# viewmin=0 clipping for display
meandata$LOWER_DISP <- pmax(meandata$LOWER, 0)

## ---- dataset CSV (SAS dataset counterpart) ----
# Keep SAS variable names; dates N/A
out_ds <- meandata %>% select(TRTAN, TRTA, PCTPT, N, PCMEAN, PCSD, LOWER, UPPER)
write.csv(out_ds, file.path(out, "target13.csv"), row.names = FALSE, na = "")

## ---- figure PNG (linear + semilog) ----
# Use base graphics via png() to allow two panels side-by-side
trts <- unique(meandata[, c("TRTAN", "TRTA")])
trts <- trts[order(trts$TRTAN), , drop = FALSE]
ntrt <- nrow(trts)
# black lines, distinct lty/pch (SAS used black for all)
ltys <- c(1, 2, 3, 4, 5, 6)
pchs <- c(1, 2, 0, 5, 6, 4)
cols <- rep("black", ntrt)

ymax_lin <- suppressWarnings(max(meandata$UPPER, na.rm = TRUE))
if (!is.finite(ymax_lin)) ymax_lin <- suppressWarnings(max(meandata$PCMEAN, na.rm = TRUE))
if (!is.finite(ymax_lin)) ymax_lin <- 1
ymax_lin <- ymax_lin * 1.1
if (ymax_lin <= 0) ymax_lin <- 1

# semilog ymax: max mean
ymax_log <- suppressWarnings(max(meandata$PCMEAN, na.rm = TRUE))
ymin_log <- suppressWarnings(min(meandata$PCMEAN[meandata$PCMEAN > 0], na.rm = TRUE))
if (!is.finite(ymax_log)) ymax_log <- 1
if (!is.finite(ymin_log)) ymin_log <- 0.1
ymax_log <- ymax_log * 1.3

png(file.path(out, "target13.png"), width = 2200, height = 1100, res = 150)
par(mfrow = c(1, 2), mar = c(5, 5, 5, 2), oma = c(0, 0, 4, 0))
# Overall titles like SAS title statements
# Linear view
plot(NA, xlim = c(0, XMAX), ylim = c(0, ymax_lin),
     xlab = xlabel, ylab = ylabel, main = "Linear view",
     xaxt = "n", cex.main = 1.1, cex.lab = 1)
axis(1, at = TICKS, labels = TICKS)
for (i in seq_len(ntrt)) {
  d <- meandata[meandata$TRTAN == trts$TRTAN[i], , drop = FALSE]
  d <- d[order(d$PCTPT), , drop = FALSE]
  # error bars
  idx <- !is.na(d$PCMEAN) & !is.na(d$PCSD)
  if (any(idx)) {
    arrows(d$PCTPT[idx], d$LOWER_DISP[idx], d$PCTPT[idx], d$UPPER[idx],
           angle = 90, code = 3, length = 0.05, col = "black", lwd = 1)
  }
  lines(d$PCTPT, d$PCMEAN, col = cols[i], lty = ltys[(i - 1) %% length(ltys) + 1], lwd = 1.5)
  points(d$PCTPT, d$PCMEAN, col = "black", bg = "black",
         pch = pchs[(i - 1) %% length(pchs) + 1], cex = 1.1)
}
# Semilogarithmic view
plot(NA, xlim = c(0, XMAX), ylim = c(ymin_log, ymax_log),
     xlab = xlabel, ylab = ylabel, main = "Semilogarithmic view",
     xaxt = "n", log = "y", cex.main = 1.1, cex.lab = 1)
axis(1, at = TICKS, labels = TICKS)
for (i in seq_len(ntrt)) {
  d <- meandata[meandata$TRTAN == trts$TRTAN[i], , drop = FALSE]
  d <- d[order(d$PCTPT), , drop = FALSE]
  lines(d$PCTPT, d$PCMEAN, col = cols[i], lty = ltys[(i - 1) %% length(ltys) + 1], lwd = 1.5)
  points(d$PCTPT, d$PCMEAN, col = "black", bg = "black",
         pch = pchs[(i - 1) %% length(pchs) + 1], cex = 1.1)
}
legend("topright", legend = trts$TRTA, col = "black",
       lty = ltys[seq_len(ntrt)], pch = pchs[seq_len(ntrt)], cex = 0.9, bg = "white")
mtext("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", outer = TRUE, line = 2, cex = 0.95, font = 2)
mtext("Analysis set: PK analysis set   |   PCTESTCD=ANAL1  Arithmetic mean (SD=BOTH)", outer = TRUE, line = 0.8, cex = 0.85)
dev.off()

## ---- table TXT rendering (same numbers as SAS output) ----
# Text rendering of MeanData table with titles/footnotes
fmt_num <- function(x, dec = 4) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # SAS-style half-away formatting
    r <- sas_round(v, dec)
    formatC(r, format = "f", digits = dec)
  }, USE.NAMES = FALSE)
}

txt_path <- file.path(out, "target13.txt")
con <- file(txt_path, open = "wt")
writeLines("SPONSOR/PROTOCOL/PRODUCT INFO", con)
writeLines("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", con)
writeLines("Analysis set: PK analysis set", con)
writeLines(paste0("PCTESTCD=", PCTESTCD_val, "  Mean type=Arithmetic  Error bars=", SDOPT), con)
writeLines("", con)
writeLines(paste0(xlabel, " vs ", ylabel), con)
writeLines(paste0("XMAX=", XMAX, "  XTICKS=", paste(TICKS, collapse = " ")), con)
writeLines("", con)
hdr <- sprintf("%-6s %-25s %8s %5s %14s %14s %14s %14s",
               "TRTAN", "TRTA", "PCTPT", "N", "PCMEAN", "PCSD", "LOWER", "UPPER")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)
od <- out_ds[order(out_ds$TRTAN, out_ds$PCTPT), , drop = FALSE]
for (r in seq_len(nrow(od))) {
  line <- sprintf("%-6s %-25s %8s %5s %14s %14s %14s %14s",
    ifelse(is.na(od$TRTAN[r]), "", as.character(od$TRTAN[r])),
    ifelse(is.na(od$TRTA[r]), "", substr(od$TRTA[r], 1, 25)),
    ifelse(is.na(od$PCTPT[r]), "", as.character(sas_round(od$PCTPT[r], 2))),
    ifelse(is.na(od$N[r]), "", as.character(od$N[r])),
    fmt_num(od$PCMEAN[r], 4),
    fmt_num(od$PCSD[r], 4),
    fmt_num(od$LOWER[r], 4),
    fmt_num(od$UPPER[r], 4))
  writeLines(line, con)
}
writeLines("", con)
writeLines("PATH DATA/PROGRAM/OUTPUT", con)
# also echo plotted-data range for traceability
writeLines(paste0("Linear YMAX(display)=", fmt_num(ymax_lin, 4)), con)
close(con)

message("Done. Wrote target13.csv / target13.png / target13.txt to ", out)
```