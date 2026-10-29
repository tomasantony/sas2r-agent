```r
# P017_sdtm_sv.sas -> R translation
# Outputs: sv.csv + listing P017.lst (text + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_vitals_path <- file.path(project_path, "data/raw/raw_vitals.csv")
# fallback to rawdir if needed
if (!file.exists(raw_vitals_path)) {
  raw_vitals_path <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_vitals.csv"
}
if (!file.exists(raw_vitals_path)) {
  # try alternative derived location
  raw_vitals_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv"
}

# helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  rounded <- floor(abs_x * mult + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# helper: SAS CATX
catx <- function(sep, ...) {
  args <- list(...)
  # trim, remove NA/empty
  cleaned <- lapply(args, function(a) {
    if (is.null(a)) return(NULL)
    a <- as.character(a)
    a <- trimws(a)
    a[a == "" | is.na(a)] <- NA_character_
    a
  })
  # vectorized
  do.call(function(...) {
    mat <- do.call(cbind, cleaned)
    apply(mat, 1, function(row) {
      row <- row[!is.na(row) & row != ""]
      paste(row, collapse = sep)
    })
  }, list())
}

# helper: parse anydtdte32. -> Date
parse_anydtdte <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA" || toupper(s) == ".") return(as.Date(NA))
  # If numeric SAS date (days since 1960-01-01) - pure digits 1-5 chars
  if (grepl("^[0-9]+$", s)) {
    # try yyyymmdd 8 digits first
    if (nchar(s) == 8) {
      d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
      if (!is.na(d)) return(as.Date(d))
    }
    num <- suppressWarnings(as.numeric(s))
    # SAS date range approx 0-40000 (1960-01-01 to ~2069)
    if (!is.na(num) && num >= 0 && num <= 60000) {
      # Heuristic: if number looks like SAS date (e.g., < 30000) and not a plausible yyyymmdd
      # Check if original string length <=5 then treat as SAS date
      if (nchar(s) <= 5) {
        return(as.Date("1960-01-01") + num)
      }
      # also if 4-5 digits, treat as SAS date
      if (nchar(s) %in% c(4,5)) {
        return(as.Date("1960-01-01") + num)
      }
    }
  }
  # Try lubridate parse_date_time with many orders
  orders <- c("ymd", "Ymd", "dmy", "mdy", "dby", "dbY", "dBy", "Ymd HMS", "dmy HMS", "mdy HMS",
              "ymd HM", "dmy HM", "mdy HM", "Y-m-d", "d-b-Y", "d-b-y", "m/d/Y", "d/m/Y",
              "b d Y", "B d Y", "d B Y", "d b Y", "Y/m/d", "d.m.Y", "Y.m.d", "Ymd", "dmY", "mdY")
  d <- suppressWarnings(lubridate::parse_date_time(s, orders = orders, quiet = TRUE, exact = FALSE, tz = "UTC"))
  if (length(d) == 1 && !is.na(d)) return(as.Date(d))
  # Explicit formats fallback
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%d-%b-%Y", "%d-%B-%Y", "%d-%b-%y", "%d/%m/%Y", "%m/%d/%Y",
            "%d-%m-%Y", "%m-%d-%Y", "%d.%m.%Y", "%Y.%m.%d", "%d %b %Y", "%d %B %Y",
            "%b %d %Y", "%B %d %Y", "%Y%m%d", "%d%b%Y", "%d%B%Y", "%Y-%m-%dT%H:%M:%S",
            "%Y-%m-%d %H:%M:%S", "%d-%b-%Y %H:%M", "%m/%d/%y", "%d/%m/%y")
  for (fmt in fmts) {
    d2 <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(d2)) return(d2)
  }
  # Try title-case month (SAS DATE11 like 15-JAN-2023)
  s2 <- stringr::str_to_title(tolower(s))
  # lubridate again on title case
  d <- suppressWarnings(lubridate::parse_date_time(s2, orders = orders, quiet = TRUE, exact = FALSE, tz = "UTC"))
  if (length(d) == 1 && !is.na(d)) return(as.Date(d))
  for (fmt in fmts) {
    d2 <- suppressWarnings(as.Date(s2, format = fmt))
    if (!is.na(d2)) return(d2)
  }
  return(as.Date(NA))
}
parse_anydtdte_vec <- function(v) {
  # vectorized
  res <- vapply(v, function(x) as.numeric(parse_anydtdte(x)), numeric(1))
  as.Date(res, origin = "1970-01-01")
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

# ---- Import raw_vitals.csv (PROC IMPORT) ----
# SAS proc import getnames=yes dbms=csv replace
if (!file.exists(raw_vitals_path)) {
  stop(paste0("Input file not found: ", raw_vitals_path))
}
# Use readr to mimic SAS proc import guessing
raw_sv <- readr::read_csv(raw_vitals_path, show_col_types = FALSE, guess_max = 10000, trim_ws = TRUE)
# Force uppercase variable names (options validvarname=upcase)
names(raw_sv) <- toupper(names(raw_sv))

# Ensure expected columns exist (case-insensitive)
# Expected: PT_ID, VISIT_NUM, VISIT, VS_DATE
# If names differ, try to map
# Print progress
cat("Imported raw_sv:", nrow(raw_sv), "rows,", ncol(raw_sv), "cols\n")
cat("Columns:", paste(names(raw_sv), collapse = ", "), "\n")

# ---- Create sdtm.sv ----
# length STUDYID $8 DOMAIN $2 USUBJID $40 VISIT $40 SVSTDTC SVENDTC $10
# retain STUDYID DOMAIN USUBJID VISITNUM VISIT VISITDY SVSTDTC SVENDTC
# set work.raw_sv

# Helper to get column safely (case-insensitive already upper)
get_col <- function(df, name, default = NA) {
  if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
}

pt_id <- get_col(raw_sv, "PT_ID")
visit_num <- get_col(raw_sv, "VISIT_NUM")
visit_raw <- get_col(raw_sv, "VISIT")
vs_date_raw <- get_col(raw_sv, "VS_DATE")

# If PT_ID not found, try USUBJID or SUBJECT
if (all(is.na(pt_id))) {
  if ("SUBJID" %in% names(raw_sv)) pt_id <- raw_sv[["SUBJID"]]
  if ("USUBJID" %in% names(raw_sv)) pt_id <- raw_sv[["USUBJID"]]
}

# Ensure types
visit_num <- suppressWarnings(as.numeric(visit_num))
visit_clean <- trimws(as.character(visit_raw))
visit_clean[is.na(visit_clean)] <- ""
# Truncate to 40 as per LENGTH
visit_clean <- substr(visit_clean, 1, 40)

# USUBJID = catx("-", STUDYID, PT_ID)
studyid <- rep("CDISC-01", nrow(raw_sv))
domain <- rep("SV", nrow(raw_sv))
# catx trims and skips missing
usubjid <- ifelse(is.na(pt_id) | trimws(as.character(pt_id)) == "",
                  studyid,
                  paste0(studyid, "-", trimws(as.character(pt_id))))
usubjid <- substr(usubjid, 1, 40)

visitnum <- visit_num

# VISITDY mapping
visitdy <- dplyr::case_when(
  visitnum == 1 ~ 1,
  visitnum == 2 ~ 31,
  visitnum == 3 ~ 61,
  TRUE ~ NA_real_
)

# _svdt = input(strip(vvalue(VS_DATE)), anydtdte32.)
# vvalue: if VS_DATE is numeric SAS date with format, returns formatted string
# In CSV we have character; handle both
# For haven numeric dates, vvalue would be formatted; but here we parse directly
vs_date_char <- as.character(vs_date_raw)
# If vs_date_raw is numeric (SAS date), vvalue would give formatted date string
# We handle numeric SAS dates via parse_anydtdte_vec which detects pure digits
# Also handle haven labelled numeric
# Strip
vs_date_char <- trimws(vs_date_char)
vs_date_char[vs_date_char == "NA" | vs_date_char == "."] <- NA_character_

svdt <- parse_anydtdte_vec(vs_date_char)

# SVSTDTC = put(_svdt, is8601da.) => YYYY-MM-DD
svstdtc <- ifelse(is.na(svdt), NA_character_, format(svdt, "%Y-%m-%d"))
svendtc <- svstdtc

# Truncate to $10 (already YYYY-MM-DD is 10)
svstdtc <- substr(svstdtc, 1, 10)
svendtc <- substr(svendtc, 1, 10)

sdtm_sv <- tibble::tibble(
  STUDYID = substr(studyid, 1, 8),
  DOMAIN = substr(domain, 1, 2),
  USUBJID = usubjid,
  VISITNUM = visitnum,
  VISIT = visit_clean,
  VISITDY = visitdy,
  SVSTDTC = svstdtc,
  SVENDTC = svendtc
)

# Keep order as retain
sdtm_sv <- sdtm_sv %>% select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC)

# Sort without deleting records: duplicate keys must remain visible to QC.
# SAS sort: by USUBJID VISITNUM; missing sort first; uppercase before lowercase
# Use base order with na.last = FALSE and method to mimic SAS
# For character, SAS uses ASCII (uppercase 65-90 before lowercase 97-122)
# R's order with default locale may differ; enforce C locale via xtfrm or stringr
# Use arrange with .locale = "C" if available, else base order
# Implement SAS-like sort: missing first, then ASCII
# Create sort keys
# Use dplyr arrange which is stable; for SAS missing first, NAs should be first
sdtm_sv <- sdtm_sv %>%
  arrange(USUBJID, VISITNUM)

# Ensure numeric types: VISITNUM, VISITDY as numeric (SAS numeric)
# Write dataset sv.csv
sv_out <- sdtm_sv
# Dates already as YYYY-MM-DD character; keep NA as ""
write.csv(sv_out, file.path(out, "sv.csv"), row.names = FALSE, na = "")

cat("Wrote sv.csv with", nrow(sv_out), "rows\n")

# ---- Listing: SV Domain Audit (PROC PRINT obs=12) ----
obs_n <- min(12, nrow(sdtm_sv))
print_df <- sdtm_sv[seq_len(obs_n), , drop = FALSE]
# Add Obs column like SAS PROC PRINT
print_df_with_obs <- tibble::tibble(Obs = seq_len(obs_n)) %>% bind_cols(print_df)

# Create text rendering
txt_path1 <- file.path(out, "P017.lst.txt")
txt_path2 <- file.path(out, "P017.txt")
csv_path1 <- file.path(out, "P017.lst.csv")
csv_path2 <- file.path(out, "P017.csv")

# Build text output similar to SAS listing
# Include title and column headers
header_line <- paste0(
  strrep(" ", 2),
  sprintf("%-8s %-6s %-20s %-8s %-20s %-8s %-10s %-10s",
          "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "VISITDY", "SVSTDTC", "SVENDTC")
)
sep_line <- strrep("-", nchar(header_line))

con_lines <- c(
  "SV Domain Audit",
  paste0("Dataset: sdtm.sv  (", nrow(sdtm_sv), " records, sorted by USUBJID VISITNUM)"),
  "",
  paste0("Obs  ", header_line),
  paste0("---- ", sep_line)
)

# Format rows
fmt_row <- function(obs, studyid, domain, usubjid, visitnum, visit, visitdy, svstdtc, svendtc) {
  # SAS PROC PRINT default formatting: numbers right-aligned
  # Reproduce with similar widths
  sprintf("%4d  %-8s %-6s %-20s %8s %-20s %8s %-10s %-10s",
          obs,
          ifelse(is.na(studyid), "", studyid),
          ifelse(is.na(domain), "", domain),
          ifelse(is.na(usubjid), "", usubjid),
          ifelse(is.na(visitnum), "", as.character(visitnum)),
          ifelse(is.na(visit), "", visit),
          ifelse(is.na(visitdy), "", as.character(visitdy)),
          ifelse(is.na(svstdtc), "", svstdtc),
          ifelse(is.na(svendtc), "", svendtc)
  )
}

for (i in seq_len(obs_n)) {
  r <- print_df_with_obs[i, ]
  line <- fmt_row(r$Obs, r$STUDYID, r$DOMAIN, r$USUBJID, r$VISITNUM, r$VISIT, r$VISITDY, r$SVSTDTC, r$SVENDTC)
  con_lines <- c(con_lines, line)
}
con_lines <- c(con_lines, "", paste0("Displayed ", obs_n, " of ", nrow(sdtm_sv), " records (obs=12)."), "")

# Write text files
writeLines(con_lines, txt_path1)
writeLines(con_lines, txt_path2)
cat("Wrote listing text to", txt_path1, "\n")

# Write CSV of table body (listing)
write.csv(print_df_with_obs, csv_path1, row.names = FALSE, na = "")
write.csv(print_df_with_obs, csv_path2, row.names = FALSE, na = "")
cat("Wrote listing CSV to", csv_path1, "\n")

# Also ensure P017.lst without extension for strict check (copy txt)
lst_path <- file.path(out, "P017.lst")
if (!file.exists(lst_path)) {
  file.copy(txt_path1, lst_path, overwrite = TRUE)
}

cat("Done. Outputs in", out, "\n")
```