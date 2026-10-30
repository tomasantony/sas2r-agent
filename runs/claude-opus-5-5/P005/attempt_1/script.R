suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

# Find a dataset in a directory by name, ignoring case.
# SAS7BDAT is preferred; XPT is used as a fallback.
read_ds <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  b <- tolower(basename(files))
  f <- files[b == paste0(tolower(name), ".sas7bdat")]
  if (length(f)) return(haven::read_sas(f[1]))
  f <- files[b == paste0(tolower(name), ".xpt")]
  if (length(f)) return(haven::read_xpt(f[1]))
  stop(paste("Dataset not found:", name, "in", dir))
}

# Convert a SAS date value to numeric days since 1970-01-01.
to_num_date <- function(x) {
  if (inherits(x, "Date")) return(as.numeric(x))
  if (inherits(x, "POSIXt")) return(as.numeric(as.Date(x)))
  as.numeric(x)
}

# Remove haven labels and formats so columns behave as plain R vectors.
zap <- function(df) {
  df <- haven::zap_labels(haven::zap_formats(df))
  as.data.frame(df, stringsAsFactors = FALSE)
}

adsl <- zap(read_ds(adam_dir, "adsl"))
lb   <- zap(read_ds(sdtm_dir, "lb"))
names(adsl) <- toupper(names(adsl))
names(lb)   <- toupper(names(lb))
message("Read ADSL: ", nrow(adsl), " rows; LB: ", nrow(lb), " rows")

# Missing character values are blank strings in SAS
for (v in names(lb))   if (is.character(lb[[v]]))   lb[[v]][is.na(lb[[v]])] <- ""
for (v in names(adsl)) if (is.character(adsl[[v]])) adsl[[v]][is.na(adsl[[v]])] <- ""

# ---------------------------------------------------------------
# 1. Sort and merge
# ---------------------------------------------------------------
adsl_sorted <- adsl[order(adsl$USUBJID, method = "radix"), , drop = FALSE]
lb_sorted <- lb[order(lb$USUBJID, lb$LBDTC, lb$LBTESTCD, method = "radix"), , drop = FALSE]

# In a SAS merge, variables common to both datasets take their values from LB
adsl_keep <- adsl_sorted[, c("USUBJID", setdiff(names(adsl_sorted), names(lb_sorted))), drop = FALSE]
adsl_keep <- adsl_keep[!duplicated(adsl_keep$USUBJID), , drop = FALSE]

m <- left_join(lb_sorted, adsl_keep, by = "USUBJID")   # if b

trtsdt <- if ("TRTSDT" %in% names(m)) to_num_date(m$TRTSDT) else rep(NA_real_, nrow(m))
trtedt <- if ("TRTEDT" %in% names(m)) to_num_date(m$TRTEDT) else rep(NA_real_, nrow(m))
trt01a <- if ("TRT01A" %in% names(m)) m$TRT01A else rep("", nrow(m))

# Convert the ISO date to a numeric analysis date.
# SAS LENGTH() returns 1 for a blank string, so blank LBDTC never qualifies.
lbdtc <- m$LBDTC
astdt <- rep(NA_real_, nrow(m))
ok <- nchar(lbdtc) >= 10
d <- suppressWarnings(as.Date(substr(lbdtc[ok], 1, 10), format = "%Y-%m-%d"))
astdt[ok] <- as.numeric(d)

# Analysis relative day (no day 0)
astdy <- ifelse(!is.na(astdt) & !is.na(trtsdt),
                ifelse(astdt >= trtsdt, astdt - trtsdt + 1, astdt - trtsdt),
                NA_real_)

adlb_draft <- data.frame(
  STUDYID  = m$STUDYID,
  USUBJID  = m$USUBJID,
  PARAMCD  = m$LBTESTCD,
  PARAM    = m$LBTEST,
  TRT01A   = trt01a,
  TRTSDT   = trtsdt,
  TRTEDT   = trtedt,
  LBSTRESU = m$LBSTRESU,
  AVAL     = as.numeric(m$LBSTRESN),
  AVISIT   = m$VISIT,
  AVISITN  = as.numeric(m$VISITNUM),
  SRCDOM   = "LB",
  SRCVAR   = "LBSEQ",
  SRCSEQ   = as.numeric(m$LBSEQ),
  ASTDT    = astdt,
  ASTDY    = astdy,
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------
# 2. Baseline candidates
# ---------------------------------------------------------------
# In SAS a missing value is smaller than any number, so missing ASTDT <= TRTSDT is true
le_sas <- function(a, b) {
  ifelse(is.na(a), TRUE, ifelse(is.na(b), FALSE, a <= b))
}
cand <- adlb_draft[le_sas(adlb_draft$ASTDT, adlb_draft$TRTSDT) & !is.na(adlb_draft$AVAL), , drop = FALSE]
cand <- cand[order(cand$USUBJID, cand$PARAMCD,
                   cand$ASTDT, cand$AVISITN, cand$SRCSEQ,
                   na.last = FALSE, method = "radix"), , drop = FALSE]

# Keep the last record within each USUBJID/PARAMCD (LAST.PARAMCD)
key <- paste(cand$USUBJID, cand$PARAMCD, sep = "\r")
last <- !duplicated(key, fromLast = TRUE)
base_flags <- data.frame(
  USUBJID  = cand$USUBJID[last],
  PARAMCD  = cand$PARAMCD[last],
  BASE     = cand$AVAL[last],
  BASE_DT  = cand$ASTDT[last],
  BASE_SEQ = cand$SRCSEQ[last],
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------
# 3. Merge baseline back
# ---------------------------------------------------------------
# Stable sort, matching PROC SORT
adlb_draft <- adlb_draft[order(adlb_draft$USUBJID, adlb_draft$PARAMCD, method = "radix"), , drop = FALSE]
adlb <- left_join(adlb_draft, base_flags, by = c("USUBJID", "PARAMCD"))

# SAS treats two missing values as equal
eq_sas <- (is.na(adlb$SRCSEQ) & is.na(adlb$BASE_SEQ)) |
  (!is.na(adlb$SRCSEQ) & !is.na(adlb$BASE_SEQ) & adlb$SRCSEQ == adlb$BASE_SEQ)
adlb$ABLFL <- ifelse(eq_sas, "Y", "")

adlb$CHG <- ifelse(!is.na(adlb$AVAL) & !is.na(adlb$BASE), adlb$AVAL - adlb$BASE, NA_real_)

# Abnormality indicator with internal unit conversion
lbnrind <- rep("", nrow(adlb))
for (i in seq_len(nrow(adlb))) {
  a <- adlb$AVAL[i]
  if (is.na(a)) next
  cv <- a
  u <- toupper(adlb$LBSTRESU[i])
  p <- adlb$PARAMCD[i]
  if (identical(p, "GLUC")) {
    if (identical(u, "MG/DL")) cv <- a * 0.0555
    lbnrind[i] <- if (cv < 3.9) "LOW" else if (cv > 5.6) "HIGH" else "NORMAL"
  } else if (identical(p, "HGB")) {
    if (identical(u, "G/DL")) cv <- a * 10
    lbnrind[i] <- if (cv < 120) "LOW" else if (cv > 175) "HIGH" else "NORMAL"
  } else {
    lbnrind[i] <- "UNKNOWN"
  }
}
adlb$LBNRIND <- lbnrind
adlb$BASE_DT <- NULL
adlb$BASE_SEQ <- NULL

# Final re-sort for presentation (stable, missing first)
adlb <- adlb[order(adlb$USUBJID, adlb$PARAMCD, adlb$ASTDT,
                   na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(adlb) <- NULL

# ---------------------------------------------------------------
# Write dataset
# ---------------------------------------------------------------
fmt_iso <- function(x) {
  r <- format(as.Date(x, origin = "1970-01-01"), "%Y-%m-%d")
  r[is.na(x)] <- ""
  r
}
adlb_out <- adlb
for (v in c("TRTSDT", "TRTEDT", "ASTDT")) adlb_out[[v]] <- fmt_iso(adlb_out[[v]])
write.csv(adlb_out, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
message("Wrote adlb.csv with ", nrow(adlb_out), " rows")

# ---------------------------------------------------------------
# 4. Visual audit (PROC PRINT obs=15)
# ---------------------------------------------------------------
# SAS DATE9. format, e.g. 02JAN2014
fmt_date9 <- function(x) {
  r <- toupper(format(as.Date(x, origin = "1970-01-01"), "%d%b%Y"))
  r[is.na(x)] <- "."
  r
}
# SAS BEST. style numeric display; missing shows as "."
fmt_num <- function(x) {
  r <- ifelse(is.na(x), ".", format(signif(x, 12), trim = TRUE, scientific = FALSE, drop0trailing = TRUE))
  as.character(r)
}

pr <- head(adlb, 15)
lst <- data.frame(
  Obs      = seq_len(nrow(pr)),
  USUBJID  = pr$USUBJID,
  PARAMCD  = pr$PARAMCD,
  AVISIT   = pr$AVISIT,
  ASTDT    = fmt_date9(pr$ASTDT),
  AVAL     = fmt_num(pr$AVAL),
  LBSTRESU = pr$LBSTRESU,
  BASE     = fmt_num(pr$BASE),
  CHG      = fmt_num(pr$CHG),
  ABLFL    = pr$ABLFL,
  LBNRIND  = pr$LBNRIND,
  stringsAsFactors = FALSE
)
write.csv(lst, file.path(out, "P005.csv"), row.names = FALSE, na = "")

# Fixed-width text rendering
cols <- names(lst)
w <- sapply(cols, function(cn) max(nchar(cn), nchar(as.character(lst[[cn]]))))
num_cols <- c("Obs", "AVAL", "BASE", "CHG")
pad <- function(s, n, right) if (right) formatC(s, width = n) else formatC(s, width = n, flag = "-")
hdr <- paste(mapply(function(cn, n) pad(cn, n, cn %in% num_cols), cols, w), collapse = "  ")
lines <- c("ADLB Audit - Laboratory Analysis with Clinical Indicators", "", hdr, "")
for (i in seq_len(nrow(lst))) {
  lines <- c(lines, paste(mapply(function(cn, n) pad(as.character(lst[[cn]][i]), n, cn %in% num_cols),
                                 cols, w), collapse = "  "))
}
writeLines(lines, file.path(out, "P005.txt"))
message("Wrote P005.txt / P005.csv")
