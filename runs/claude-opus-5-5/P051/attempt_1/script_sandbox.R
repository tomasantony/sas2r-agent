suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# ---- helpers -------------------------------------------------------------
# Mimic SAS BEST12. formatting of a numeric value, with surrounding blanks trimmed
best12 <- function(x) {
  res <- ifelse(is.na(x), ".", {
    s <- formatC(x, digits = 12, format = "g", flag = "")
    s <- trimws(s)
    s
  })
  # drop trailing zeros in decimals, e.g. "2.50000" -> "2.5"
  res <- sub("(\\.[0-9]*?)0+$", "\\1", res)
  res <- sub("\\.$", "", res)
  res[res == ""] <- "."
  res
}
# SAS COMPBL: collapse runs of blanks into a single blank
compbl <- function(x) gsub(" {2,}", " ", x)

# ---- PROC SORT with WHERE SAFFL='Y' --------------------------------------
adcm_raw <- read_xpt(file.path(rawadam, "adcm.xpt"))
names(adcm_raw) <- toupper(names(adcm_raw))

chr_or_blank <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; v }

adcm <- adcm_raw %>%
  filter(!is.na(SAFFL) & SAFFL == "Y")
for (v in c("TRTA", "STUDYID", "USUBJID", "CMSTDTC", "CMENDTC", "CMDECOD")) {
  adcm[[v]] <- chr_or_blank(adcm[[v]])
}

old_loc <- Sys.getlocale("LC_COLLATE")
Sys.setlocale("LC_COLLATE", "C")
ord <- order(adcm$TRTA, adcm$STUDYID, adcm$USUBJID, adcm$CMSTDTC,
             adcm$CMENDTC, adcm$CMDECOD, method = "radix")
Sys.setlocale("LC_COLLATE", old_loc)
adcm <- adcm[ord, , drop = FALSE]

# ---- DATA ADCM2 ----------------------------------------------------------
adcm2 <- adcm
adcm2$PERIOD <- "Study Period"
adcm2$CMSTDTC[adcm2$CMSTDTC == ""] <- "NK"
adcm2$CMENDTC[adcm2$CMENDTC == ""] <- "NK"
adcm2$STARTSTOP <- substr(compbl(paste0(adcm2$CMSTDTC, "/", adcm2$CMENDTC)), 1, 25)

# CMDOSE: numeric in pilot ADCM -> BEST12. conversion
dose_chr <- if ("CMDOSE" %in% names(adcm2)) {
  if (is.numeric(adcm2$CMDOSE)) best12(adcm2$CMDOSE) else trimws(chr_or_blank(adcm2$CMDOSE))
} else rep(".", nrow(adcm2))

if ("CMUNIT" %in% names(adcm2) && is.numeric(adcm2$CMUNIT)) {
  has_unit <- !is.na(adcm2$CMUNIT)
  du <- ifelse(has_unit, paste0(dose_chr, "(", best12(adcm2$CMUNIT), ")"), dose_chr)
} else {
  # CMUNIT does not exist in ADCM -> SAS creates numeric missing -> else branch
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
adcm2_out <- adcm2
for (v in names(adcm2_out)) {
  if (inherits(adcm2_out[[v]], "Date")) adcm2_out[[v]] <- format(adcm2_out[[v]], "%Y-%m-%d")
  if (inherits(adcm2_out[[v]], "POSIXt")) adcm2_out[[v]] <- format(adcm2_out[[v]], "%Y-%m-%dT%H:%M:%S")
}
adcm2_out <- as.data.frame(lapply(adcm2_out, function(x) { attributes(x) <- attributes(x)[c("names","class")[c("names","class") %in% names(attributes(x))]]; if (inherits(x, "haven_labelled")) unclass(x) else x }), stringsAsFactors = FALSE)
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
lines <- character(0)
for (tr in unique(rpt$TRTA)) {
  sub <- rpt[rpt$TRTA == tr, -1]
  body <- as.matrix(sub)
  allm <- rbind(hdr, body)
  w <- apply(allm, 2, function(z) max(nchar(z), na.rm = TRUE))
  fmt_row <- function(r) paste(formatC(r, width = -w), collapse = "  ")
  lines <- c(lines,
             "Safety Population",
             "Study Period",
             paste0("Treatment: ", tr),
             "",
             fmt_row(hdr),
             paste(strrep("-", sum(w) + 2 * (length(w) - 1))),
             apply(body, 1, fmt_row),
             "", "\f")
}
writeLines(lines, file.path(out, "Target36.txt"))

cat("P051 done:", nrow(rpt), "rows\n")
