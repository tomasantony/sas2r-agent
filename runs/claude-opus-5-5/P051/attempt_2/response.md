```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ---- helpers -------------------------------------------------------------
# Approximate SAS BEST12. formatting of a numeric value, blanks trimmed
best12 <- function(x) {
  res <- rep(".", length(x))
  ok <- !is.na(x)
  if (any(ok)) {
    s <- trimws(formatC(x[ok], digits = 12, format = "g", flag = ""))
    has_dot <- grepl("\\.", s) & !grepl("e", s)
    s[has_dot] <- sub("0+$", "", s[has_dot])
    s <- sub("\\.$", "", s)
    res[ok] <- s
  }
  res[res == ""] <- "."
  res
}
# SAS COMPBL: collapse runs of blanks into a single blank
compbl <- function(x) gsub(" {2,}", " ", x)

chr_or_blank <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }

# ---- PROC SORT with WHERE SAFFL='Y' --------------------------------------
adcm_raw <- read_xpt(file.path(rawadam, "adcm.xpt"))
names(adcm_raw) <- toupper(names(adcm_raw))
adcm_raw <- zap_labels(zap_formats(adcm_raw))

adcm <- adcm_raw %>%
  filter(!is.na(SAFFL) & SAFFL == "Y")
for (v in c("TRTA", "STUDYID", "USUBJID", "CMSTDTC", "CMENDTC", "CMDECOD")) {
  adcm[[v]] <- chr_or_blank(adcm[[v]])
}

# radix ordering = C-locale byte ordering (SAS-like)
ord <- order(adcm$TRTA, adcm$STUDYID, adcm$USUBJID, adcm$CMSTDTC,
             adcm$CMENDTC, adcm$CMDECOD, method = "radix")
adcm <- adcm[ord, , drop = FALSE]

# ---- DATA ADCM2 ----------------------------------------------------------
adcm2 <- adcm
adcm2$PERIOD <- "Study Period"
adcm2$CMSTDTC[adcm2$CMSTDTC == ""] <- "NK"
adcm2$CMENDTC[adcm2$CMENDTC == ""] <- "NK"
adcm2$STARTSTOP <- substr(compbl(paste0(adcm2$CMSTDTC, "/", adcm2$CMENDTC)), 1, 25)

dose_chr <- if ("CMDOSE" %in% names(adcm2)) {
  if (is.numeric(adcm2$CMDOSE)) best12(adcm2$CMDOSE) else trimws(chr_or_blank(adcm2$CMDOSE))
} else rep(".", nrow(adcm2))

if ("CMUNIT" %in% names(adcm2) && is.numeric(adcm2$CMUNIT)) {
  has_unit <- !is.na(adcm2$CMUNIT)
  du <- ifelse(has_unit, paste0(dose_chr, "(", best12(adcm2$CMUNIT), ")"), dose_chr)
} else {
  # CMUNIT not in ADCM -> SAS creates numeric missing -> else branch
  du <- dose_chr
}
adcm2$DOSEUNIT <- substr(trimws(compbl(du)), 1, 50)

if ("AENRF" %in% names(adcm2)) {
  aenrf <- chr_or_blank(adcm2$AENRF)
  adcm2$ONGOING <- ifelse(aenrf == "Ongoing", "Y", "N")
} else {
  adcm2$ONGOING <- "N"
}

# dataset output
adcm2_out <- as.data.frame(adcm2, stringsAsFactors = FALSE)
for (v in names(adcm2_out)) {
  if (inherits(adcm2_out[[v]], "Date")) adcm2_out[[v]] <- format(adcm2_out[[v]], "%Y-%m-%d")
  if (inherits(adcm2_out[[v]], "POSIXt")) adcm2_out[[v]] <- format(adcm2_out[[v]], "%Y-%m-%dT%H:%M:%S")
}
write.csv(adcm2_out, file.path(out, "adcm2.csv"), row.names = FALSE, na = "")

# ---- PROC REPORT ---------------------------------------------------------
getc <- function(v) if (v %in% names(adcm2)) chr_or_blank(adcm2[[v]]) else rep("", nrow(adcm2))
adurn_chr <- if ("ADURN" %in% names(adcm2)) best12(as.numeric(adcm2$ADURN)) else rep(".", nrow(adcm2))

rpt <- data.frame(
  TRTA      = adcm2$TRTA,
  USUBJID   = adcm2$USUBJID,
  CMTRT     = getc("CMTRT"),
  CMDECOD   = adcm2$CMDECOD,
  CMCLAS    = getc("CMCLAS"),
  DOSEUNIT  = adcm2$DOSEUNIT,
  CMINDC    = getc("CMINDC"),
  STARTSTOP = adcm2$STARTSTOP,
  ADURN     = adurn_chr,
  ONGOING   = adcm2$ONGOING,
  stringsAsFactors = FALSE
)
write.csv(rpt, file.path(out, "Target36.csv"), row.names = FALSE, na = "")

hdr <- c("Subject ID", "Reported Name", "Standardized Name", "Medication Class",
         "Dose (unit)", "Indication", "Start Date / Stop Date", "Dur (Days)", "Ongoing")
pad_right <- function(s, w) {
  n <- nchar(s, type = "width")
  paste0(s, strrep(" ", pmax(0, w - n)))
}
lines <- character(0)
for (tr in unique(rpt$TRTA)) {
  sub <- rpt[rpt$TRTA == tr, -1, drop = FALSE]
  body <- as.matrix(sub)
  allm <- rbind(hdr, body)
  w <- apply(allm, 2, function(z) max(nchar(z, type = "width"), na.rm = TRUE))
  fmt_row <- function(r) paste(pad_right(as.character(r), w), collapse = "  ")
  lines <- c(lines,
             "Safety Population",
             "Study Period",
             paste0("Treatment: ", tr),
             "",
             fmt_row(hdr),
             strrep("-", sum(w) + 2 * (length(w) - 1)),
             apply(body, 1, fmt_row),
             "", "\f")
}
writeLines(lines, file.path(out, "Target36.txt"))

cat("P051 done:", nrow(rpt), "rows\n")
```