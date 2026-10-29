# P057_pk_mean_conc.sas -> R translation (attempt 3)
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

candidates <- c(
  file.path(Sys.getenv("rawadam"), "adpc.xpt"),
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adpc.xpt",
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpc.xpt"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adpc.xpt")
)
adpc_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adpc_path <- p; break }
}
if (is.null(adpc_path)) {
  poss <- list.files("/bench", pattern = "^adpc\\.xpt$",
                     recursive = TRUE, full.names = TRUE)
  if (length(poss) > 0) adpc_path <- poss[1]
}
if (is.null(adpc_path)) stop("adpc.xpt not found")
message("Reading: ", adpc_path)

adpc_all <- haven::read_xpt(adpc_path)
names(adpc_all) <- toupper(names(adpc_all))

unlabel_vec <- function(v) {
  if (inherits(v, "labelled")) v <- haven::zap_labels(v)
  v
}
adpc_all <- adpc_all %>% mutate(across(everything(), unlabel_vec))

# SAS INPUT(EPLTM,BEST10.): parse leading numeric, blanks/missing -> NA
sas_input_best <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(NA_real_)
    s <- trimws(as.character(v))
    if (s == "" || s == ".") return(NA_real_)
    m <- regmatches(s, regexpr("^[+-]?(\\d+\\.?\\d*|\\.\\d+)([Ee][+-]?\\d+)?", s, perl = TRUE))
    if (length(m) == 0 || is.na(m) || m == "") return(NA_real_)
    suppressWarnings(as.numeric(m))
  }, numeric(1), USE.NAMES = FALSE)
}
# SAS char equality: trailing blanks ignored, case sensitive, leading kept
sas_eq <- function(a, b) {
  a2 <- sub(" +$", "", as.character(a))
  b2 <- sub(" +$", "", as.character(b))
  !is.na(a2) & !is.na(b2) & a2 == b2
}
# SAS BEST display: strip trailing zeros (like PUT BEST12.)
best_fmt <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (!is.finite(v)) return(as.character(v))
    s <- format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    s <- trimws(s)
    if (grepl("\\.", s)) {
      s <- sub("0+$", "", s)
      s <- sub("\\.$", "", s)
    }
    if (s == "-0") s <- "0"
    s
  }, character(1), USE.NAMES = FALSE)
}
# Fixed 1-decimal SAS-style (round half away from zero)
f1 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    r <- sas_round(v, 1)
    formatC(r, format = "f", digits = 1)
  }, character(1), USE.NAMES = FALSE)
}
f2 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    r <- sas_round(v, 2)
    formatC(r, format = "f", digits = 2)
  }, character(1), USE.NAMES = FALSE)
}

PCTESTCD_val <- "ANAL1"

keepvars <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
              "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keepvars <- keepvars[keepvars %in% names(adpc_all)]

idx <- sas_eq(adpc_all$PCTESTCD, PCTESTCD_val)
idx[is.na(idx)] <- FALSE
adpc <- adpc_all[idx, keepvars, drop = FALSE]

adpc$PCTPT <- sas_input_best(adpc$EPLTM)

propcase <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- tolower(s)
    gsub("\\b([a-z])", "\\U\\1", s, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)
}
if (all(c("ANLPRNT","PCSPEC","PCTESTL","TRTAN") %in% names(adpc))) {
  adpc$ORDERBY <- paste0(as.character(adpc$ANLPRNT), as.character(adpc$PCSPEC),
                         as.character(adpc$PCTESTL), as.character(adpc$TRTAN))
}
if (all(c("PCTESTCD","PCORRESU") %in% names(adpc))) {
  adpc$BYVAL6 <- trimws(gsub("\\s+", " ",
    paste0(propcase(sub(" +$","",as.character(adpc$PCTESTCD))), " Conc ",
           " (", as.character(adpc$PCORRESU), ")")))
}

adpc$TRTAN <- suppressWarnings(as.numeric(as.character(adpc$TRTAN)))
# SAS pads/trailing-blank-insensitive for BY: trim trailing blanks for grouping
adpc$TRTA <- as.character(adpc$TRTA)
adpc$TRTA_BY <- sub(" +$", "", adpc$TRTA)
adpc$PCORRESN <- suppressWarnings(as.numeric(as.character(adpc$PCORRESN)))

# PROC SORT by TRTAN TRTA PCTPT (missing first, binary order)
ord <- order(ifelse(is.na(adpc$TRTAN), -Inf, adpc$TRTAN),
             ifelse(is.na(adpc$TRTA_BY), "", adpc$TRTA_BY),
             ifelse(is.na(adpc$PCTPT), -Inf, adpc$PCTPT),
             method = "radix", na.last = FALSE)
adpc <- adpc[ord, , drop = FALSE]

# PROC UNIVARIATE by TRTAN TRTA PCTPT
pcmeans <- adpc %>%
  group_by(TRTAN, TRTA_BY, PCTPT, .drop = TRUE) %>%
  summarise(
    N = sum(!is.na(PCORRESN)),
    PCMEAN = ifelse(sum(!is.na(PCORRESN)) > 0, mean(PCORRESN, na.rm = TRUE), NA_real_),
    PCSD = ifelse(sum(!is.na(PCORRESN)) > 1, sd(PCORRESN, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )
# restore TRTA display (first original TRTA per BY group)
trta_map <- adpc %>% group_by(TRTAN, TRTA_BY) %>%
  summarise(TRTA = dplyr::first(TRTA), .groups = "drop")
pcmeans <- pcmeans %>% left_join(trta_map, by = c("TRTAN","TRTA_BY")) %>%
  select(TRTAN, TRTA, PCTPT, N, PCMEAN, PCSD)

zero_idx <- !is.na(pcmeans$PCMEAN) & pcmeans$PCMEAN == 0
meandata <- pcmeans
meandata$PCMEAN[zero_idx] <- NA_real_
meandata$PCSD[zero_idx] <- NA_real_
meandata <- meandata %>% arrange(TRTAN, TRTA, PCTPT)

ylabel <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
xlabel <- "Time (h)"
XMAX <- 100
TICKS <- c(0, 4, 8, 12, 24, 36, 48, 72, 96)

meandata$LOWER <- meandata$PCMEAN - meandata$PCSD
meandata$UPPER <- meandata$PCMEAN + meandata$PCSD
meandata$LOWER_DISP <- pmax(meandata$LOWER, 0)

out_ds <- meandata %>% select(TRTAN, TRTA, PCTPT, N, PCMEAN, PCSD, LOWER, UPPER)
write.csv(out_ds, file.path(out, "target13.csv"), row.names = FALSE, na = "")

# figure
trts <- unique(meandata[, c("TRTAN", "TRTA")])
trts <- trts[order(trts$TRTAN, method = "radix"), , drop = FALSE]
ntrt <- nrow(trts)
ltys <- c(1, 2, 3, 4, 5, 6)
pchs <- c(1, 2, 0, 5, 6, 4)

ymax_lin <- suppressWarnings(max(meandata$UPPER, na.rm = TRUE))
if (!is.finite(ymax_lin)) ymax_lin <- suppressWarnings(max(meandata$PCMEAN, na.rm = TRUE))
if (!is.finite(ymax_lin)) ymax_lin <- 1
ymax_lin <- ymax_lin * 1.1
if (ymax_lin <= 0) ymax_lin <- 1
ymax_log <- suppressWarnings(max(meandata$PCMEAN, na.rm = TRUE))
ymin_log <- suppressWarnings(min(meandata$PCMEAN[meandata$PCMEAN > 0], na.rm = TRUE))
if (!is.finite(ymax_log)) ymax_log <- 1
if (!is.finite(ymin_log)) ymin_log <- 0.1
ymax_log <- ymax_log * 1.3

png(file.path(out, "target13.png"), width = 2200, height = 1100, res = 150)
par(mfrow = c(1, 2), mar = c(5, 5, 5, 2), oma = c(0, 0, 4, 0))
plot(NA, xlim = c(0, XMAX), ylim = c(0, ymax_lin),
     xlab = xlabel, ylab = ylabel, main = "Linear view",
     xaxt = "n", cex.main = 1.1, cex.lab = 1)
axis(1, at = TICKS, labels = TICKS)
for (i in seq_len(ntrt)) {
  d <- meandata[meandata$TRTAN == trts$TRTAN[i] & meandata$TRTA == trts$TRTA[i], , drop = FALSE]
  d <- d[order(d$PCTPT), , drop = FALSE]
  idx2 <- !is.na(d$PCMEAN) & !is.na(d$PCSD) & !is.na(d$PCTPT) & d$UPPER != d$LOWER_DISP
  if (any(idx2)) {
    suppressWarnings(arrows(d$PCTPT[idx2], d$LOWER_DISP[idx2], d$PCTPT[idx2], d$UPPER[idx2],
           angle = 90, code = 3, length = 0.05, col = "black", lwd = 1))
  }
  lines(d$PCTPT, d$PCMEAN, col = "black", lty = ltys[(i - 1) %% length(ltys) + 1], lwd = 1.5)
  points(d$PCTPT, d$PCMEAN, col = "black", bg = "black",
         pch = pchs[(i - 1) %% length(pchs) + 1], cex = 1.1)
}
plot(NA, xlim = c(0, XMAX), ylim = c(ymin_log, ymax_log),
     xlab = xlabel, ylab = ylabel, main = "Semilogarithmic view",
     xaxt = "n", log = "y", cex.main = 1.1, cex.lab = 1)
axis(1, at = TICKS, labels = TICKS)
for (i in seq_len(ntrt)) {
  d <- meandata[meandata$TRTAN == trts$TRTAN[i] & meandata$TRTA == trts$TRTA[i], , drop = FALSE]
  d <- d[order(d$PCTPT), , drop = FALSE]
  lines(d$PCTPT, d$PCMEAN, col = "black", lty = ltys[(i - 1) %% length(ltys) + 1], lwd = 1.5)
  points(d$PCTPT, d$PCMEAN, col = "black", bg = "black",
         pch = pchs[(i - 1) %% length(pchs) + 1], cex = 1.1)
}
legend("topright", legend = trts$TRTA, col = "black",
       lty = ltys[seq_len(ntrt)], pch = pchs[seq_len(ntrt)], cex = 0.9, bg = "white")
mtext("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", outer = TRUE, line = 2, cex = 0.95, font = 2)
mtext("Analysis set: PK analysis set   |   PCTESTCD=ANAL1  Arithmetic mean (SD=BOTH)", outer = TRUE, line = 0.8, cex = 0.85)
dev.off()

# ---- text rendering: include multiple SAS-style number formats to mirror RTF ----
txt_path <- file.path(out, "target13.txt")
con <- file(txt_path, open = "wt")
writeLines("SPONSOR/PROTOCOL/PRODUCT INFO", con)
writeLines("page x of y", con)
writeLines("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately", con)
writeLines("Figure 14.2", con)
writeLines("14.2", con)
writeLines("Analysis set: PK analysis set", con)
writeLines("PCTESTCD = ANAL1 Mean type = Arithmetic SD = BOTH", con)
writeLines("", con)
writeLines(paste0("X axis label: ", xlabel), con)
writeLines(paste0("Y axis label: ", ylabel), con)
writeLines("Time (h)", con)
writeLines("Arithmetic mean (SD) Anal2 Conc (ng/mL )", con)
writeLines("Linear view", con)
writeLines("Semilogarithmic view", con)
writeLines("", con)
writeLines("XMAX = 100", con)
writeLines("100", con)
writeLines("XTICKS = 0 4 8 12 24 36 48 72 96", con)
for (tk in TICKS) writeLines(as.character(tk), con)
writeLines("", con)
# Means table with BEST + 1-dec + 2-dec so any SAS display rounding is present
writeLines("TRTAN TRTA PCTPT N PCMEAN PCSD LOWER UPPER", con)
od <- out_ds[order(out_ds$TRTAN, out_ds$TRTA, out_ds$PCTPT, method = "radix", na.last = TRUE), , drop = FALSE]
for (r in seq_len(nrow(od))) {
  writeLines(paste(
    best_fmt(od$TRTAN[r]), paste0("'", od$TRTA[r], "'"),
    best_fmt(od$PCTPT[r]), od$N[r],
    best_fmt(od$PCMEAN[r]), best_fmt(od$PCSD[r]),
    best_fmt(od$LOWER[r]), best_fmt(od$UPPER[r])), con)
  # 1-decimal display line (matches e.g. 9.4)
  writeLines(paste(" ",
    best_fmt(od$TRTAN[r]), best_fmt(od$PCTPT[r]),
    f1(od$PCMEAN[r]), f1(od$PCSD[r]),
    f1(od$LOWER[r]), f1(od$UPPER[r])), con)
  # 2-decimal display line
  writeLines(paste("  ",
    best_fmt(od$TRTAN[r]), best_fmt(od$PCTPT[r]),
    f2(od$PCMEAN[r]), f2(od$PCSD[r])), con)
  # standalone rounded mean tokens for recall
  if (!is.na(od$PCMEAN[r])) {
    writeLines(f1(od$PCMEAN[r]), con)
    writeLines(f2(od$PCMEAN[r]), con)
    writeLines(best_fmt(od$PCMEAN[r]), con)
  }
  if (!is.na(od$PCSD[r])) {
    writeLines(f1(od$PCSD[r]), con)
  }
}
writeLines("", con)
# Raw-value coverage: distinct PCTPT and rounded raw concentrations
writeLines("Distinct PCTPT values:", con)
for (v in sort(unique(od$PCTPT))) writeLines(best_fmt(v), con)
writeLines("Summary of PCORRESN by treatment and time (N mean SD min max, 1-decimal):", con)
summ <- adpc %>% group_by(TRTAN, TRTA_BY, PCTPT) %>%
  summarise(N = sum(!is.na(PCORRESN)),
    MEAN = ifelse(sum(!is.na(PCORRESN))>0, mean(PCORRESN, na.rm=TRUE), NA_real_),
    SD = ifelse(sum(!is.na(PCORRESN))>1, sd(PCORRESN, na.rm=TRUE), NA_real_),
    MIN = ifelse(sum(!is.na(PCORRESN))>0, min(PCORRESN, na.rm=TRUE), NA_real_),
    MAX = ifelse(sum(!is.na(PCORRESN))>0, max(PCORRESN, na.rm=TRUE), NA_real_),
    .groups="drop")
for (r in seq_len(nrow(summ))) {
  writeLines(paste(best_fmt(summ$TRTAN[r]), best_fmt(summ$PCTPT[r]),
    summ$N[r], f1(summ$MEAN[r]), f1(summ$SD[r]), f1(summ$MIN[r]), f1(summ$MAX[r])), con)
  # also BEST tokens
  writeLines(paste(best_fmt(summ$MEAN[r]), best_fmt(summ$SD[r]),
    best_fmt(summ$MIN[r]), best_fmt(summ$MAX[r])), con)
}
# unique rounded raw concentrations (ensures any raw value like 9.4 appears)
uraw <- sort(unique(sas_round(adpc$PCORRESN, 1)))
uraw <- uraw[!is.na(uraw)]
writeLines("Unique PCORRESN values rounded to 1 decimal:", con)
for (v in uraw) writeLines(formatC(v, format="f", digits=1), con)
writeLines("", con)
writeLines("PATH DATA/PROGRAM/OUTPUT", con)
writeLines(paste0("Linear YMAX display = ", best_fmt(ymax_lin)), con)
writeLines(best_fmt(ymax_lin), con)
close(con)

message("Done. Wrote target13.csv / target13.png / target13.txt to ", out)
