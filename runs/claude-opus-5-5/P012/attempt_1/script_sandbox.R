# P012: SDTM DS domain from raw_demog.csv
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
project_path <- "/bench/data/derived/c2c"
usubjid_length <- 40

# ---- PROC IMPORT (read as character, then mimic type guessing) ----
raw_ds <- read.csv(file.path(project_path, "data/raw/raw_demog.csv"),
                   colClasses = "character", na.strings = character(0),
                   check.names = FALSE, stringsAsFactors = FALSE)
names(raw_ds) <- toupper(trimws(names(raw_ds)))
raw_ds[] <- lapply(raw_ds, function(x) { x <- trimws(x); x[is.na(x)] <- ""; x })

# Is the column numeric in PROC IMPORT terms?
is_num_col <- function(x) {
  nz <- x[x != ""]
  length(nz) > 0 && all(!is.na(suppressWarnings(as.numeric(nz))))
}
# SAS BEST12. style for catx of numeric values
sas_num_chr <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  ifelse(is.na(v), "", trimws(formatC(v, digits = 12, format = "g")))
}

subj <- raw_ds$SUBJ_ID
if (is_num_col(subj)) subj <- sas_num_chr(subj)

# catx: strip and drop blanks
catx <- function(sep, ...) {
  args <- list(...)
  n <- max(sapply(args, length))
  sapply(seq_len(n), function(i) {
    parts <- sapply(args, function(a) trimws(as.character(a[(i - 1) %% length(a) + 1])))
    parts <- parts[!is.na(parts) & parts != ""]
    paste(parts, collapse = sep)
  })
}

# anydtdte-like parsing (DATESTYLE=MDY)
parse_any_date <- function(x) {
  x <- trimws(x)
  res <- as.Date(rep(NA_character_, length(x)))
  ok <- x != ""
  if (any(ok)) {
    p <- suppressWarnings(parse_date_time(x[ok],
           orders = c("Ymd", "mdY", "dbY", "dmY", "Ymd HMS", "mdY HMS", "dbY HMS", "mdy"),
           quiet = TRUE))
    res[ok] <- as.Date(p)
    # numeric SAS date values
    nn <- is.na(res) & ok & !is.na(suppressWarnings(as.numeric(x)))
    if (any(nn)) res[nn] <- as.Date(as.numeric(x[nn]), origin = "1960-01-01")
  }
  res
}

disp_reason <- if ("DISPOSITION_REASON" %in% names(raw_ds)) raw_ds$DISPOSITION_REASON else rep("", nrow(raw_ds))
disp        <- if ("DISPOSITION" %in% names(raw_ds)) raw_ds$DISPOSITION else rep("", nrow(raw_ds))
end_dat     <- if ("STUDY_END_DAT" %in% names(raw_ds)) raw_ds$STUDY_END_DAT else rep("", nrow(raw_ds))

STUDYID <- "CDISC-01"
dsdt <- parse_any_date(end_dat)
DSTERM <- substr(toupper(trimws(disp_reason)), 1, 80)

ds <- data.frame(
  STUDYID = rep(STUDYID, nrow(raw_ds)),
  DOMAIN  = "DS",
  USUBJID = substr(catx("-", STUDYID, subj), 1, usubjid_length),
  DSCAT   = "DISPOSITION EVENT",
  DSSCAT  = "STUDY PARTICIPATION",
  DSTERM  = DSTERM,
  DSDECOD = DSTERM,
  EPOCH   = ifelse(toupper(trimws(disp)) == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP"),
  DSSTDTC = ifelse(is.na(dsdt), "", format(dsdt, "%Y-%m-%d")),
  DSSEQ   = 1,
  stringsAsFactors = FALSE
)

# ---- PROC SORT by USUBJID DSSEQ (stable, byte ordering) ----
old_loc <- Sys.getlocale("LC_COLLATE")
invisible(Sys.setlocale("LC_COLLATE", "C"))
ds <- ds[order(ds$USUBJID, ds$DSSEQ, method = "radix"), , drop = FALSE]
invisible(Sys.setlocale("LC_COLLATE", old_loc))
rownames(ds) <- NULL

write.csv(ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT (obs=12) ----
pr <- head(ds, 12)
pr <- cbind(Obs = seq_len(nrow(pr)), pr)
write.csv(pr, file.path(out, "P012.csv"), row.names = FALSE, na = "")

pc <- as.data.frame(lapply(pr, function(x) {
  if (is.numeric(x)) format(x, trim = TRUE) else ifelse(is.na(x), "", x)
}), stringsAsFactors = FALSE)
widths <- sapply(names(pc), function(n) max(nchar(n), nchar(pc[[n]]), na.rm = TRUE))
fmt_row <- function(vals, right) {
  paste(mapply(function(v, w, r) if (r) formatC(v, width = w) else formatC(v, width = w, flag = "-"),
               vals, widths, right), collapse = "  ")
}
right <- sapply(pr, is.numeric)
lines <- c("DS Domain Audit", "",
           fmt_row(names(pc), right),
           apply(pc, 1, function(r) fmt_row(r, right)))
writeLines(lines, file.path(out, "P012.txt"))

cat("P012 done:", nrow(ds), "DS records\n")
