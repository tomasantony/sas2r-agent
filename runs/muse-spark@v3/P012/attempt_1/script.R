# P012_sdtm_ds.sas -> R translation
# Creates SDTM DS domain from raw_demog.csv and prints audit listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# SAS round half away from zero helper (not needed for this program but kept per spec)
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

# ---- Input path (absolute, forward slashes) ----
raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"
# fallback via project_path env if needed (same location)
if (!file.exists(raw_path)) {
  pp <- Sys.getenv("PROJECT_PATH")
  if (pp != "" && file.exists(file.path(pp, "data/raw/raw_demog.csv"))) {
    raw_path <- file.path(pp, "data/raw/raw_demog.csv")
  }
}
stopifnot(file.exists(raw_path))

# ---- PROC IMPORT emulation: read everything as character to preserve raw strings ----
# Use readr with all-character cols (GUESSINGROWS emulation done manually below)
df_raw <- readr::read_csv(
  raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE,
  progress = FALSE
)
df_raw <- as.data.frame(df_raw, stringsAsFactors = FALSE, check.names = FALSE)
# options validvarname=upcase -> uppercase names
names(df_raw) <- toupper(names(df_raw))

# Expected columns
stopifnot("SUBJ_ID" %in% names(df_raw))
stopifnot("DISPOSITION" %in% names(df_raw))
stopifnot("DISPOSITION_REASON" %in% names(df_raw))
stopifnot("STUDY_END_DAT" %in% names(df_raw))

# Helper: SAS STRIP (remove leading/trailing blanks), UPCASE
sas_strip <- function(x) {
  # x character vector; NA stays NA
  out <- trimws(as.character(x), which = "both")
  out[out == "" & !is.na(out)] <- ""
  out
}
sas_upcase_strip <- function(x) {
  s <- sas_strip(x)
  ifelse(is.na(s), NA_character_, toupper(s))
}
# Helper: SAS CATX(delimiter, items...) - strips, skips missing/blank
sas_catx <- function(delim, ...) {
  args <- list(...)
  n <- max(lengths(args))
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  vapply(seq_len(n), function(i) {
    parts <- vapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(v, which = "both")
      if (v == "") return(NA_character_)
      v
    }, character(1))
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, character(1))
}

n_obs <- nrow(df_raw)
SUBJ_ID_raw <- sas_strip(df_raw$SUBJ_ID)
DISPOSITION_raw <- df_raw$DISPOSITION
DISPOSITION_REASON_raw <- df_raw$DISPOSITION_REASON
STUDY_END_raw <- sas_strip(df_raw$STUDY_END_DAT)
# readr turns "" into NA already; normalize "" to NA for date logic
STUDY_END_raw[STUDY_END_raw == ""] <- NA_character_

# ---- STUDY_END_DAT informat decision (PROC IMPORT GUESSINGROWS emulation) ----
# Decide ONE format for the whole column from all its values, then parse every
# value with that single format.
vals_nm <- STUDY_END_raw[!is.na(STUDY_END_raw) & STUDY_END_raw != ""]
vals_nm_trim <- trimws(vals_nm)

decide_date_format <- function(vals) {
  if (length(vals) == 0) return(NA_character_)
  is_slash <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", vals)
  is_iso <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", vals)
  is_dmon <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", vals)
  is_dmon2 <- grepl("^[0-9]{1,2} [A-Za-z]{3} [0-9]{2,4}$", vals)
  if (all(is_slash)) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", vals)))
    year_parts <- sub("^[0-9]{1,2}/[0-9]{1,2}/", "", vals)
    use_y2 <- all(nchar(year_parts) == 2)
    if (any(first_fields > 12, na.rm = TRUE)) {
      fmt <- if (use_y2) "%m/%d/%y" else "%m/%d/%Y"
      # first field >12 means DDMMYY10. in SAS -> DMY order
      fmt <- if (use_y2) "%d/%m/%y" else "%d/%m/%Y"
    } else {
      fmt <- if (use_y2) "%m/%d/%y" else "%m/%d/%Y"
    }
    return(fmt)
  }
  if (all(is_iso)) return("%Y-%m-%d")
  if (all(is_dmon)) return("%d-%b-%Y")
  if (all(is_dmon2)) return("%d %b %Y")
  # Fallback (mixed): pick single format based on majority / SAS ANYDTDTE DATESTYLE logic.
  # If any slash present, decide DMY vs MDY once from slash subset.
  slash_vals <- vals[grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", vals)]
  if (length(slash_vals) > 0) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", slash_vals)))
    year_parts <- sub("^[0-9]{1,2}/[0-9]{1,2}/", "", slash_vals)
    use_y2 <- all(nchar(year_parts) == 2)
    if (any(first_fields > 12, na.rm = TRUE)) {
      return(if (use_y2) "%d/%m/%y" else "%d/%m/%Y")
    } else {
      return(if (use_y2) "%m/%d/%y" else "%m/%d/%Y")
    }
  }
  if (any(is_iso)) return("%Y-%m-%d")
  if (any(is_dmon)) return("%d-%b-%Y")
  return("%Y-%m-%d")
}

date_fmt <- decide_date_format(vals_nm_trim)

# Normalize month-abbrev case for %b parsing (e.g., "12-jan-2020" -> "12-Jan-2020")
normalize_for_fmt <- function(x, fmt) {
  if (is.na(fmt)) return(x)
  if (fmt %in% c("%d-%b-%Y", "%d %b %Y", "%d-%b-%y", "%d %b %y")) {
    vapply(x, function(v) {
      if (is.na(v) || v == "") return(v)
      # title-case the alpha month token
      m <- regmatches(v, regexpr("[A-Za-z]+", v))
      if (length(m) == 0 || m == "") return(v)
      tc <- paste0(toupper(substr(m, 1, 1)), tolower(substr(m, 2, nchar(m))))
      sub("[A-Za-z]+", tc, v)
    }, character(1), USE.NAMES = FALSE)
  } else {
    x
  }
}

STUDY_END_norm <- normalize_for_fmt(STUDY_END_raw, date_fmt)
if (is.na(date_fmt)) {
  `_dsdt` <- rep(as.Date(NA), n_obs)
} else {
  `_dsdt` <- as.Date(STUDY_END_norm, format = date_fmt)
  # as.Date with %y/%Y strict single format; no per-value fallback
}

# ---- DATA sdtm.ds emulation ----
STUDYID <- rep("CDISC-01", n_obs)
DOMAIN <- rep("DS", n_obs)
USUBJID <- sas_catx("-", STUDYID, SUBJ_ID_raw)
# LENGTH truncations
STUDYID <- substr(STUDYID, 1, 8)
DOMAIN <- substr(DOMAIN, 1, 2)
USUBJID <- substr(USUBJID, 1, 40)
USUBJID[is.na(USUBJID) | USUBJID == ""] <- NA_character_

DSSEQ <- rep(1, n_obs)
DSCAT <- rep("DISPOSITION EVENT", n_obs)
DSSCAT <- rep("STUDY PARTICIPATION", n_obs)
DSTERM <- sas_upcase_strip(DISPOSITION_REASON_raw)
DSTERM <- substr(DSTERM, 1, 80)
DSDECOD <- DSTERM

DISP_UP <- sas_upcase_strip(DISPOSITION_raw)
EPOCH <- ifelse(!is.na(DISP_UP) & DISP_UP == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")
EPOCH <- substr(EPOCH, 1, 20)

DSSTDTC <- ifelse(is.na(`_dsdt`), NA_character_, format(`_dsdt`, "%Y-%m-%d"))

sdtm_ds <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  DSSEQ = DSSEQ,
  DSCAT = DSCAT,
  DSSCAT = DSSCAT,
  DSTERM = DSTERM,
  DSDECOD = DSDECOD,
  EPOCH = EPOCH,
  DSSTDTC = DSSTDTC,
  stringsAsFactors = FALSE
)

# ---- PROC SORT by USUBJID DSSEQ (SAS: missing first, uppercase before lowercase) ----
# Use binary (C-locale) ordering to match SAS character ordering
ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE, method = "radix")
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]
rownames(sdtm_ds) <- NULL

# ---- Write dataset ds.csv ----
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# ---- PROC PRINT data=sdtm.ds(obs=12); title "DS Domain Audit"; ----
print_n <- min(12, nrow(sdtm_ds))
listing_df <- sdtm_ds[seq_len(print_n), , drop = FALSE]
listing_df <- cbind(Obs = seq_len(print_n), listing_df)

# CSV of the table body
write.csv(listing_df, file.path(out, "P012.csv"), row.names = FALSE, na = "")

# Plain-text rendering (SAS PROC PRINT style)
# Build fixed-width table
cols <- names(listing_df)
# compute widths
widths <- sapply(cols, function(cn) {
  vals <- ifelse(is.na(listing_df[[cn]]), "", as.character(listing_df[[cn]]))
  max(nchar(c(cn, vals)), na.rm = TRUE)
})
# header and rows
pad_right <- function(s, w) sprintf(paste0("%-", w, "s"), s)
pad_left_num <- function(s, w) sprintf(paste0("%", w, "s"), s)
header_cells <- mapply(pad_right, cols, widths, USE.NAMES = FALSE)
# Obs and DSSEQ right-aligned like SAS, others left-aligned
header_cells[cols == "Obs"] <- sprintf(paste0("%", widths[cols == "Obs"], "s"), "Obs")
header_cells[cols == "DSSEQ"] <- sprintf(paste0("%", widths[cols == "DSSEQ"], "s"), "DSSEQ")
header_line <- paste(header_cells, collapse = "  ")
sep_line <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), widths, USE.NAMES = FALSE), collapse = "  ")
body_lines <- vapply(seq_len(nrow(listing_df)), function(i) {
  cells <- sapply(cols, function(cn) {
    v <- listing_df[i, cn]
    s <- if (is.na(v)) "" else as.character(v)
    if (cn %in% c("Obs", "DSSEQ")) sprintf(paste0("%", widths[cn], "s"), s)
    else sprintf(paste0("%-", widths[cn], "s"), s)
  }, USE.NAMES = FALSE)
  paste(cells, collapse = "  ")
}, character(1))

txt_lines <- c(
  "DS Domain Audit",
  "",
  header_line,
  sep_line,
  body_lines,
  ""
)
txt_path1 <- file.path(out, "P012.txt")
txt_path2 <- file.path(out, "P012.lst")
writeLines(txt_lines, txt_path1, useBytes = TRUE)
writeLines(txt_lines, txt_path2, useBytes = TRUE)
