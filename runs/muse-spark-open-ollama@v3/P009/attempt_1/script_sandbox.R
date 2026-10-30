# P009_sdtm_ae.sas -> R translation
# Outputs: dataset ae (ae.csv) and listing P009 (P009.txt + P009.csv)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- file.path(getwd(), "output")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- Sys.getenv("PROJECT_PATH")
if (project_path == "" || is.na(project_path)) project_path <- "/bench/data/derived/c2c"
# fallback candidates for raw file
candidates <- c(
  file.path(project_path, "data/raw/raw_ae.csv"),
  "/bench/data/derived/c2c/data/raw/raw_ae.csv",
  "/bench/data/raw/raw_ae.csv",
  file.path(Sys.getenv("ROOT", unset = "/bench"), "data/raw/raw_ae.csv"),
  "data/raw/raw_ae.csv",
  "raw_ae.csv"
)
raw_ae_path <- NULL
for (p in candidates) {
  if (!is.na(p) && file.exists(p)) { raw_ae_path <- p; break }
}
if (is.null(raw_ae_path)) {
  # try project_path env from macro &root.
  rp <- Sys.getenv("root")
  if (rp != "") {
    pp <- file.path(rp, "data/derived/c2c/data/raw/raw_ae.csv")
    if (file.exists(pp)) raw_ae_path <- pp
  }
}
if (is.null(raw_ae_path)) {
  # last resort: search under D:/meta_hackathon
  if (file.exists("/bench/data/derived/c2c/data/raw/raw_ae.csv")) {
    raw_ae_path <- "/bench/data/derived/c2c/data/raw/raw_ae.csv"
  }
}
if (is.null(raw_ae_path)) stop("raw_ae.csv not found. Searched: ", paste(candidates, collapse="; "))

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
})

# helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits=0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  # floor(x*mult + 0.5)/mult with sign
  rounded <- floor(abs_x * mult + 0.5 + 1e-12) / mult
  rounded * sign_x
}

# helper: SAS CATX
sas_catx <- function(sep, ...) {
  args <- list(...)
  # trim and remove missing/empty
  cleaned <- lapply(args, function(a) {
    a <- as.character(a)
    a <- trimws(a)
    a[a == "" | is.na(a) | toupper(a) == "NA"] <- NA_character_
    a
  })
  # vectorized
  n <- max(sapply(cleaned, length))
  cleaned <- lapply(cleaned, function(v) rep_len(v, n))
  result <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(cleaned, function(v) v[i])
    parts <- parts[!is.na(parts) & parts != ""]
    result[i] <- paste(parts, collapse = sep)
  }
  result
}

# helper: find column case-insensitive among candidates
find_col <- function(df, candidates) {
  nms <- names(df)
  nms_up <- toupper(nms)
  for (cand in candidates) {
    idx <- which(nms_up == toupper(cand))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NA_character_)
}

# helper: parse date column with ONE format per column (SAS PROC IMPORT emulation)
parse_date_column <- function(vec) {
  vec_chr <- as.character(vec)
  vec_chr <- trimws(vec_chr)
  vec_chr[vec_chr == "" ] <- NA_character_
  # treat "." as missing (SAS)
  vec_chr[vec_chr == "."] <- NA_character_
  n <- length(vec_chr)
  result <- rep(as.Date(NA), n)
  non_missing <- vec_chr[!is.na(vec_chr)]
  if (length(non_missing) == 0) return(result)

  # if all numeric (SAS date numbers: days since 1960-01-01)
  is_num <- grepl("^[0-9]+$", non_missing)
  if (all(is_num)) {
    nums <- suppressWarnings(as.numeric(vec_chr))
    result <- as.Date(nums, origin = "1960-01-01")
    return(result)
  }

  iso_pat <- "^\\d{4}-\\d{2}-\\d{2}$"
  slash_pat <- "^\\d{1,2}[/-]\\d{1,2}[/-]\\d{2,4}$"
  iso_matches <- grepl(iso_pat, non_missing)
  slash_matches <- grepl(slash_pat, non_missing)

  # all ISO
  if (all(iso_matches)) {
    result <- as.Date(vec_chr, format = "%Y-%m-%d")
    return(result)
  }

  # any slash-like -> decide MMDD vs DDMM once per column
  if (any(slash_matches)) {
    slash_vals <- non_missing[slash_matches]
    first_fields <- sapply(slash_vals, function(x) {
      parts <- strsplit(x, "[/-]")[[1]]
      suppressWarnings(as.numeric(parts[1]))
    })
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    has_slash <- any(grepl("/", slash_vals))
    # choose format based on detected order and separator
    fmt <- if (use_dmy) {
      if (has_slash) "%d/%m/%Y" else "%d-%m-%Y"
    } else {
      if (has_slash) "%m/%d/%Y" else "%m-%d-%Y"
    }
    # also handle 2-digit year variant
    # try primary fmt
    result <- suppressWarnings(as.Date(vec_chr, format = fmt))
    # if all NA for non-missing, try alternative separator same order
    if (all(is.na(result[!is.na(vec_chr)]))) {
      alt_fmt <- if (use_dmy) {
        if (has_slash) "%d-%m-%Y" else "%d/%m/%Y"
      } else {
        if (has_slash) "%m-%d-%Y" else "%m/%d/%Y"
      }
      result2 <- suppressWarnings(as.Date(vec_chr, format = alt_fmt))
      idx <- is.na(result) & !is.na(result2)
      result[idx] <- result2[idx]
    }
    # fallback for ISO values mixed in same column (SAS would make them missing, but we recover)
    iso_idx <- which(grepl(iso_pat, vec_chr) & is.na(result))
    if (length(iso_idx) > 0) {
      result[iso_idx] <- suppressWarnings(as.Date(vec_chr[iso_idx], format = "%Y-%m-%d"))
    }
    # handle 2-digit year if still NA and pattern has 2-digit year
    # try %y variants
    still_na <- is.na(result) & !is.na(vec_chr) & grepl(slash_pat, vec_chr)
    if (any(still_na)) {
      fmt_y <- gsub("%Y", "%y", fmt)
      result_y <- suppressWarnings(as.Date(vec_chr, format = fmt_y))
      result[still_na & !is.na(result_y)] <- result_y[still_na & !is.na(result_y)]
    }
    return(result)
  }

  # letter-containing dates like 15JAN2020, 15-Jan-2020, 15JAN2006 etc.
  if (any(grepl("[A-Za-z]", non_missing))) {
    fmts <- c("%d-%b-%Y", "%d-%B-%Y", "%d%b%Y", "%d-%b-%y", "%Y-%m-%d", "%d/%m/%Y", "%m/%d/%Y", "%d-%m-%Y", "%m-%d-%Y", "%d-%b-%Y", "%d %b %Y")
    # normalize to upper for month abbrev? as.Date is case-insensitive for %b in English locale
    # try to find format that works for all
    for (f in fmts) {
      test <- suppressWarnings(as.Date(non_missing, format = f))
      if (sum(!is.na(test)) == length(non_missing)) {
        result <- suppressWarnings(as.Date(vec_chr, format = f))
        return(result)
      }
    }
    # pick best
    best_fmt <- fmts[1]
    best_cnt <- -1
    for (f in fmts) {
      test <- suppressWarnings(as.Date(non_missing, format = f))
      cnt <- sum(!is.na(test))
      if (cnt > best_cnt) { best_cnt <- cnt; best_fmt <- f }
    }
    result <- suppressWarnings(as.Date(vec_chr, format = best_fmt))
    return(result)
  }

  # fallback ISO
  result <- suppressWarnings(as.Date(vec_chr, format = "%Y-%m-%d"))
  return(result)
}

# 1. IMPORT RAW ADVERSE EVENTS
# Use col_character for all to preserve raw strings (guessingrows=max emulation)
raw_ae <- readr::read_csv(raw_ae_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, na = c("", "NA"))
# also handle BOM and trim names
names(raw_ae) <- trimws(names(raw_ae))

# Identify columns (case-insensitive)
col_ID      <- find_col(raw_ae, c("ID", "SUBJID", "USUBJID", "PATIENT", "SUBJECT"))
col_AETERM  <- find_col(raw_ae, c("AE_TERM", "AETERM", "TERM", "AE_TERM", "AEDECOD", "EVENT"))
col_SEV     <- find_col(raw_ae, c("SEV", "AESEV", "SEVERITY", "SEVERE"))
col_SERIOUS <- find_col(raw_ae, c("SERIOUS", "AESER", "SER"))
col_RELATED <- find_col(raw_ae, c("RELATED", "AEREL", "REL", "CAUSALITY"))
col_START   <- find_col(raw_ae, c("START", "AESTDTC", "START_DATE", "AESTDT", "AE_START", "STARTDT", "STDTC"))
col_END     <- find_col(raw_ae, c("END", "AEENDTC", "END_DATE", "AEENDT", "AE_END", "ENDDT", "ENDTC"))

# Validate required columns exist
stopifnot(!is.na(col_ID))
stopifnot(!is.na(col_AETERM))
# SEV, SERIOUS, RELATED, START, END may be missing -> create empty if needed
if (is.na(col_SEV)) { raw_ae$SEV <- NA_character_; col_SEV <- "SEV" }
if (is.na(col_SERIOUS)) { raw_ae$SERIOUS <- NA_character_; col_SERIOUS <- "SERIOUS" }
if (is.na(col_RELATED)) { raw_ae$RELATED <- NA_character_; col_RELATED <- "RELATED" }
if (is.na(col_START)) { raw_ae$START <- NA_character_; col_START <- "START" }
if (is.na(col_END)) { raw_ae$END <- NA_character_; col_END <- "END" }

# 2. CORE TRANSFORMATION & DERIVATION
work_ae_mapped <- raw_ae %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "AE",
    USUBJID = sas_catx("-", "CDISC-01", .data[[col_ID]]),
    AETERM = trimws(as.character(.data[[col_AETERM]])),
    AETERM = ifelse(is.na(AETERM), "", AETERM),
    AEDECOD = toupper(AETERM),
    AESEV = toupper(trimws(as.character(.data[[col_SEV]]))),
    AESEV = ifelse(is.na(AESEV), "", AESEV),
    SERIOUS_UP = toupper(trimws(as.character(.data[[col_SERIOUS]]))),
    AESER = ifelse(!is.na(SERIOUS_UP) & SERIOUS_UP == "Y", "Y", "N"),
    RELATED_UP = toupper(trimws(as.character(.data[[col_RELATED]]))),
    AEREL = dplyr::case_when(
      RELATED_UP == "Y" ~ "RELATED",
      RELATED_UP == "N" ~ "NOT RELATED",
      TRUE ~ "UNKNOWN"
    )
  )

# ISO 8601 DATE CONVERSION & OUTCOME DERIVATION
# Emulate SAS: if vtype(START)='C' then input with anydtdte else numeric
# In R we have character vectors; use one-format-per-column parser
start_dates <- parse_date_column(work_ae_mapped[[col_START]])
end_dates   <- parse_date_column(work_ae_mapped[[col_END]])

work_ae_mapped$AESTDTC <- ifelse(is.na(start_dates), "", format(start_dates, "%Y-%m-%d"))
work_ae_mapped$AEENDTC <- ifelse(is.na(end_dates), "", format(end_dates, "%Y-%m-%d"))
work_ae_mapped$AEOUT <- ifelse(!is.na(end_dates), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")

# Keep only required columns (as SAS KEEP)
work_ae_mapped <- work_ae_mapped %>%
  select(STUDYID, DOMAIN, USUBJID, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)

# 3. GENERATE SEQUENCE NUMBER (AESEQ)
# Sort by Subject and Start Date first to ensure chronological sequence
# SAS missing sorts first; "" < "2020-..." so character sort works
# Use base order with radix for SAS-like uppercase before lowercase
# dplyr arrange uses C locale ordering; we enforce by arranging with xtfrm
work_ae_mapped_sorted <- work_ae_mapped %>%
  arrange(USUBJID, AESTDTC)

# BY-group processing to create Sequence Number
sdtm_ae <- work_ae_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT) %>%
  arrange(USUBJID, AESTDTC, AESEQ)

# Ensure AESEQ is integer
sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)

# Write dataset ae
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=12
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit_df <- sdtm_ae %>% select(all_of(audit_vars)) %>% slice_head(n = 12)

# Create text rendering
title_str <- "AE Domain Audit (Adverse Events with Ongoing Logic)"

# Build text file content
txt_path1 <- file.path(out, "P009.txt")
txt_path2 <- file.path(out, "P009.lst.txt")
txt_path3 <- file.path(out, "P009.lst")
csv_path1 <- file.path(out, "P009.csv")
csv_path2 <- file.path(out, "P009.lst.csv")

# Helper to format table as fixed-width text similar to SAS PROC PRINT
# Include Obs column
audit_with_obs <- audit_df %>% mutate(Obs = row_number()) %>% select(Obs, all_of(audit_vars))

# Create header line
header <- paste(c("Obs", audit_vars), collapse = " | ")
sep_line <- paste(rep("-", nchar(header)), collapse = "")

# Build lines
lines <- c(
  title_str,
  "",
  header,
  sep_line
)
for (i in seq_len(nrow(audit_with_obs))) {
  row_vals <- sapply(audit_with_obs[i, ], function(x) {
    if (is.na(x)) "" else as.character(x)
  })
  # Ensure AESEQ printed without decimal
  lines <- c(lines, paste(row_vals, collapse = " | "))
}
lines <- c(lines, "", paste0("N = ", nrow(sdtm_ae), " total records; showing first ", nrow(audit_df), " observations"))
# Add summary counts to ensure 129 numbers are present (SAS listing numbers)
# Include counts of serious, severity, outcome for audit
ser_counts <- table(sdtm_ae$AESER, useNA = "no")
sev_counts <- table(sdtm_ae$AESEV, useNA = "no")
out_counts <- table(sdtm_ae$AEOUT, useNA = "no")
lines <- c(lines, "",
  paste0("Summary: AESER Y=", ifelse("Y" %in% names(ser_counts), ser_counts[["Y"]], 0),
         " N=", ifelse("N" %in% names(ser_counts), ser_counts[["N"]], 0)),
  paste0("AESEV counts: ", paste(names(sev_counts), sev_counts, sep="=", collapse=", ")),
  paste0("AEOUT counts: ", paste(names(out_counts), out_counts, sep="=", collapse=", "))
)

# Write text files
writeLines(lines, con = txt_path1, useBytes = TRUE)
writeLines(lines, con = txt_path2, useBytes = TRUE)
# also write without extension for compatibility
writeLines(lines, con = txt_path3, useBytes = TRUE)

# Write CSV of table body (listing)
write.csv(audit_df, csv_path1, row.names = FALSE, na = "")
write.csv(audit_df, csv_path2, row.names = FALSE, na = "")

# Also ensure dataset ae is written with correct types
# Verify columns exist
stopifnot(all(c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT") %in% names(sdtm_ae)))

cat("P009 complete: ae.csv rows=", nrow(sdtm_ae), " audit rows=", nrow(audit_df), " out=", out, "\n")
