```r
# P076 - Listing 16.2.7.1 Listing of Adverse Events (translation of l_ae.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# Read an ADaM dataset, preferring .sas7bdat and falling back to .xpt
read_ds <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- read_sas(f1)
  } else if (file.exists(f2)) {
    d <- read_xpt(f2)
  } else {
    stop("Cannot find dataset ", name)
  }
  d <- zap_labels(zap_formats(d))
  names(d) <- toupper(names(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

# Convert to character; dates as YYYY-MM-DD, missing as ""
to_chr <- function(x) {
  if (inherits(x, "Date")) {
    x <- format(x, "%Y-%m-%d")
  } else if (inherits(x, "POSIXt")) {
    x <- format(x, "%Y-%m-%dT%H:%M:%S")
  }
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

# Character column that is all blank when the variable is absent
getc <- function(d, v) {
  if (v %in% names(d)) to_chr(d[[v]]) else rep("", nrow(d))
}

# SAS PROPCASE with default delimiters: blank, / - ( . tab
propcase <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || s == "") return("")
    ch <- strsplit(tolower(s), "")[[1]]
    prev_delim <- TRUE
    for (i in seq_along(ch)) {
      if (prev_delim && grepl("[a-z]", ch[i])) ch[i] <- toupper(ch[i])
      prev_delim <- ch[i] %in% c(" ", "/", "-", "(", ".", "\t")
    }
    paste(ch, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# Truncate to the SAS LENGTH of the target variable
trunc_len <- function(x, n) substr(x, 1, n)

adae <- read_ds("adae")
adsl <- read_ds("adsl")
cat("Read ADAE:", nrow(adae), "rows; ADSL:", nrow(adsl), "rows\n")

# Fall back to TRT01A if ADSL has no TRTA
if (!("TRTA" %in% names(adsl)) && "TRT01A" %in% names(adsl)) adsl$TRTA <- adsl$TRT01A

# ---- Step 1: PROC SQL inner join (safety population, treatment-emergent) ----
a <- data.frame(
  USUBJID      = getc(adae, "USUBJID"),
  SOC          = getc(adae, "AEBODSYS"),
  PT           = getc(adae, "AEDECOD"),
  VERBATIM     = getc(adae, "AETERM"),
  START_DATE   = getc(adae, "AESTDTC"),
  END_DATE     = getc(adae, "AEENDTC"),
  SEVERITY     = getc(adae, "AESEV"),
  RELATIONSHIP = getc(adae, "AEREL"),
  SERIOUS      = getc(adae, "AESER"),
  ACTION_TAKEN = getc(adae, "AEACN"),
  OUTCOME      = getc(adae, "AEOUT"),
  CTCAE_GRADE  = getc(adae, "AETOXGR"),
  TRTEMFL      = getc(adae, "TRTEMFL"),
  stringsAsFactors = FALSE
)
a$.ord <- seq_len(nrow(a))

b <- data.frame(
  USUBJID = getc(adsl, "USUBJID"),
  TRTA    = getc(adsl, "TRTA"),
  AGE     = if ("AGE" %in% names(adsl)) adsl$AGE else NA,
  SEX     = getc(adsl, "SEX"),
  RACE    = getc(adsl, "RACE"),
  SAFFL   = getc(adsl, "SAFFL"),
  stringsAsFactors = FALSE
)

ae_list <- inner_join(a, b, by = "USUBJID") %>%
  filter(SAFFL == "Y", TRTEMFL == "Y")

# ORDER BY with SAS (byte) character ordering; blanks sort first
ae_list <- ae_list[order(ae_list$TRTA, ae_list$USUBJID, ae_list$START_DATE,
                         ae_list$SOC, ae_list$PT, ae_list$.ord, method = "radix"), ]

ae_list <- ae_list %>%
  select(USUBJID, TRTA, AGE, SEX, RACE, SOC, PT, VERBATIM, START_DATE, END_DATE,
         SEVERITY, RELATIONSHIP, SERIOUS, ACTION_TAKEN, OUTCOME, CTCAE_GRADE, TRTEMFL)

# ---- Step 2: display values ----
ae_report <- ae_list
ae_report$severity_d <- trunc_len(propcase(ae_report$SEVERITY), 10)
ae_report$rel_d      <- trunc_len(propcase(ae_report$RELATIONSHIP), 15)
ae_report$serious_d  <- ifelse(ae_report$SERIOUS == "Y", "Yes", "No")

act <- toupper(ae_report$ACTION_TAKEN)
ae_report$action_d <- trunc_len(dplyr::case_when(
  act == "DRUG WITHDRAWN"   ~ "Drug Withdrawn",
  act == "DOSE REDUCED"     ~ "Dose Reduced",
  act == "DRUG INTERRUPTED" ~ "Drug Interrupted",
  act == "DOSE NOT CHANGED" ~ "Dose Not Changed",
  act == "NOT APPLICABLE"   ~ "Not Applicable",
  TRUE                      ~ propcase(ae_report$ACTION_TAKEN)
), 30)

oc <- toupper(ae_report$OUTCOME)
ae_report$outcome_d <- trunc_len(dplyr::case_when(
  oc == "RECOVERED/RESOLVED"               ~ "Resolved",
  oc == "RECOVERING/RESOLVING"             ~ "Resolving",
  oc == "NOT RECOVERED/NOT RESOLVED"       ~ "Not Resolved",
  oc == "RECOVERED/RESOLVED WITH SEQUELAE" ~ "Resolved w/ Sequelae",
  oc == "FATAL"                            ~ "Fatal",
  TRUE                                     ~ propcase(ae_report$OUTCOME)
), 30)

# ---- Step 3: PROC REPORT listing ----
# ORDER variables TRTA, USUBJID: report is sorted by them (stable within ties)
rep <- ae_report[order(ae_report$TRTA, ae_report$USUBJID, method = "radix"), ]

cols <- c("TRTA", "USUBJID", "SOC", "PT", "VERBATIM", "START_DATE", "END_DATE",
          "CTCAE_GRADE", "severity_d", "serious_d", "rel_d", "action_d", "outcome_d")
body <- rep[, cols]
write.csv(body, file.path(out, "l_ae.csv"), row.names = FALSE, na = "")

hdr <- c("Treatment", "Subject ID", "System Organ Class", "Preferred Term",
         "Verbatim Term", "Start Date", "End Date", "CTCAE Grade", "Severity",
         "SAE", "Relationship", "Action Taken", "Outcome")

# Left-pad helper (vectorised widths)
padr <- function(v, w) {
  v <- as.character(v)
  v[is.na(v)] <- ""
  paste0(v, strrep(" ", pmax(0, w - nchar(v))))
}

# Text rendering: blank repeated ORDER values; new page for each TRTA
lines <- character(0)
titles <- c("[Sponsor Name]", "Protocol: [Protocol Number]", "",
            "Listing 16.2.7.1", "Listing of Adverse Events", "Safety Analysis Set", "")
trts <- unique(body$TRTA)
for (t in trts) {
  sub <- body[body$TRTA == t, , drop = FALSE]
  disp <- sub
  disp$USUBJID[duplicated(sub$USUBJID)] <- ""
  if (nrow(disp) > 1) disp$TRTA[-1] <- ""
  w <- pmax(nchar(hdr), vapply(disp, function(x) max(c(0, nchar(as.character(x)))), numeric(1)))
  fmt <- function(v) paste(padr(v, w), collapse = "  ")
  if (length(lines) > 0) lines <- c(lines, "\f")
  lines <- c(lines, titles, fmt(hdr), strrep("-", sum(w) + 2 * (length(w) - 1)))
  for (i in seq_len(nrow(disp))) lines <- c(lines, fmt(unlist(disp[i, ], use.names = FALSE)))
  lines <- c(lines, "", "Data cutoff: [DDMONYYYY]", "Source: ADaM datasets")
}
writeLines(lines, file.path(out, "l_ae.txt"))

cat("NOTE: l_ae completed successfully;", nrow(body), "rows listed.\n")
```