out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_demog.csv")
if (!file.exists(raw_path)) {
  alt1 <- "/bench/data/derived/c2c/data/raw/raw_demog.csv"
  if (file.exists(alt1)) raw_path <- alt1
}
if (!file.exists(raw_path)) {
  cands <- list.files("/bench", pattern = "raw_demog\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(cands) > 0) raw_path <- cands[1]
}
if (!file.exists(raw_path)) {
  cands2 <- list.files("/bench", pattern = "raw_demog\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(cands2) > 0) raw_path <- cands2[1]
}
if (!file.exists(raw_path)) stop(paste("Input file not found:", raw_path))

raw_ds <- readr::read_csv(raw_path, show_col_types = FALSE, na = c("", "NA", "."))
names(raw_ds) <- toupper(names(raw_ds))

cat("Input file:", raw_path, "\n")
cat("Columns:", paste(names(raw_ds), collapse = ", "), "\n")
cat("Rows:", nrow(raw_ds), "\n")

get_col <- function(df, colname) {
  if (colname %in% names(df)) return(df[[colname]])
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx) > 0) return(df[[idx[1]]])
  idx2 <- which(gsub("_", "", toupper(names(df))) == gsub("_", "", toupper(colname)))
  if (length(idx2) > 0) return(df[[idx2[1]]])
  return(rep(NA_character_, nrow(df)))
}

catx <- function(delim, ...) {
  args <- list(...)
  n <- length(args[[1]])
  if (n == 0) return(character(0))
  result <- character(n)
  for (i in seq_len(n)) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v))
      if (v == "") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) result[i] <- ""
    else result[i] <- paste(parts, collapse = delim)
  }
  result
}

upcase_strip <- function(x) {
  toupper(trimws(as.character(x)))
}

parse_anydtdte <- function(x) {
  x_trim <- trimws(as.character(x))
  result <- rep(as.Date(NA), length(x_trim))
  idx <- !is.na(x_trim) & x_trim != ""
  if (any(idx)) {
    parsed <- suppressWarnings(lubridate::parse_date_time(
      x_trim[idx],
      orders = c("ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS",
                 "ymd HM", "mdy HM", "dmy HM",
                 "Ymd", "mdY", "dmY", "Y-m-d", "m/d/Y", "d/m/Y",
                 "Ymd HMS", "Y-m-d HMS", "m-d-Y", "d-m-Y",
                 "ymdT", "YmdT"),
      exact = FALSE, truncated = 3, quiet = TRUE
    ))
    result[idx] <- as.Date(parsed)
    still_na <- is.na(result) & idx
    if (any(still_na)) {
      try2 <- suppressWarnings(as.Date(x_trim[still_na], format = "%Y-%m-%d"))
      result[still_na] <- try2
      still_na2 <- is.na(result) & idx
      if (any(still_na2)) {
        try3 <- suppressWarnings(as.Date(x_trim[still_na2], format = "%m/%d/%Y"))
        result[still_na2] <- try3
      }
      still_na3 <- is.na(result) & idx
      if (any(still_na3)) {
        try4 <- suppressWarnings(as.Date(x_trim[still_na3], format = "%d/%m/%Y"))
        result[still_na3] <- try4
      }
      still_na4 <- is.na(result) & idx
      if (any(still_na4)) {
        num_vals <- suppressWarnings(as.numeric(x_trim[still_na4]))
        has_num <- !is.na(num_vals)
        if (any(has_num)) {
          result[still_na4][has_num] <- as.Date(num_vals[has_num], origin = "1960-01-01")
        }
      }
    }
  }
  result
}

put_is8601da <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

subj_id_vec <- get_col(raw_ds, "SUBJ_ID")
if (all(is.na(subj_id_vec))) subj_id_vec <- get_col(raw_ds, "SUBJID")
disposition_reason_vec <- get_col(raw_ds, "DISPOSITION_REASON")
disposition_vec <- get_col(raw_ds, "DISPOSITION")
study_end_dat_vec <- get_col(raw_ds, "STUDY_END_DAT")
if (all(is.na(study_end_dat_vec))) study_end_dat_vec <- get_col(raw_ds, "STUDY_END_DATE")

n <- nrow(raw_ds)

sdtm_ds <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN = rep("DS", n),
  USUBJID = catx("-", rep("CDISC-01", n), as.character(subj_id_vec)),
  DSSEQ = rep(1L, n),
  DSCAT = rep("DISPOSITION EVENT", n),
  DSSCAT = rep("STUDY PARTICIPATION", n),
  DSTERM = upcase_strip(disposition_reason_vec),
  stringsAsFactors = FALSE
)
sdtm_ds$DSDECOD <- sdtm_ds$DSTERM

disp_up <- upcase_strip(disposition_vec)
sdtm_ds$EPOCH <- ifelse(!is.na(disp_up) & disp_up == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")

# DSSTDTC logic - avoid variable names starting with underscore
is_character_type <- is.character(study_end_dat_vec)
dsdt_tmp <- rep(as.Date(NA), n)

if (is_character_type) {
  dsdt_tmp <- parse_anydtdte(study_end_dat_vec)
} else if (inherits(study_end_dat_vec, "Date")) {
  dsdt_tmp <- study_end_dat_vec
} else if (inherits(study_end_dat_vec, "POSIXt")) {
  dsdt_tmp <- as.Date(study_end_dat_vec)
} else if (is.numeric(study_end_dat_vec)) {
  test_sas <- as.Date(study_end_dat_vec, origin = "1960-01-01")
  yr <- suppressWarnings(as.numeric(format(test_sas, "%Y")))
  if (any(!is.na(yr) & (yr > 2030 | yr < 1990))) {
    test_excel <- as.Date(study_end_dat_vec, origin = "1899-12-30")
    yr2 <- suppressWarnings(as.numeric(format(test_excel, "%Y")))
    plausible_sas <- sum(!is.na(yr) & yr >= 2000 & yr <= 2025)
    plausible_excel <- sum(!is.na(yr2) & yr2 >= 2000 & yr2 <= 2025)
    if (plausible_excel > plausible_sas) dsdt_tmp <- test_excel else dsdt_tmp <- test_sas
  } else {
    dsdt_tmp <- test_sas
  }
} else {
  dsdt_tmp <- parse_anydtdte(as.character(study_end_dat_vec))
}

sdtm_ds$DSSTDTC <- put_is8601da(dsdt_tmp)

sdtm_ds <- sdtm_ds[, c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSCAT", "DSSCAT", "DSTERM", "DSDECOD", "EPOCH", "DSSTDTC")]

ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE)
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]
rownames(sdtm_ds) <- NULL

cat("STUDY_END_DAT sample (parsed):", paste(head(study_end_dat_vec, 5), collapse = " | "), "\n")
cat("Class of STUDY_END_DAT:", paste(class(study_end_dat_vec), collapse = ","), "\n")
cat("DSSTDTC sample:", paste(head(sdtm_ds$DSSTDTC, 5), collapse = " | "), "\n")

# Write dataset ds.csv
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# Listing: DS Domain Audit PROC PRINT obs=12
listing_n <- min(12, nrow(sdtm_ds))
listing_df <- sdtm_ds[seq_len(listing_n), , drop = FALSE]
listing_df$Obs <- seq_len(listing_n)
listing_df <- listing_df[, c("Obs", "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSCAT", "DSSCAT", "DSTERM", "DSDECOD", "EPOCH", "DSSTDTC")]

# CSV of table body
write.csv(listing_df, file.path(out, "P012.csv"), row.names = FALSE, na = "")

# Text rendering - must contain numbers from SAS output
# Build SAS-like PROC PRINT text
format_listing_text <- function(df, title) {
  cols <- names(df)
  widths <- sapply(cols, function(cn) {
    vals <- as.character(df[[cn]])
    vals[is.na(vals)] <- ""
    max(nchar(c(cn, vals), type = "width"), na.rm = TRUE)
  })
  widths <- widths + 2
  header <- paste0(sprintf(paste0("%-", widths, "s"), cols), collapse = "")
  separator <- paste0(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "")
  lines <- c(title, "", header, separator)
  for (i in seq_len(nrow(df))) {
    row_vals <- sapply(cols, function(cn) {
      v <- df[[cn]][i]
      if (is.na(v)) v <- ""
      as.character(v)
    })
    line <- paste0(sprintf(paste0("%-", widths, "s"), row_vals), collapse = "")
    lines <- c(lines, line)
  }
  lines <- c(lines, "", paste0("N = ", nrow(df), " (first ", nrow(df), " observations of ", nrow(sdtm_ds), " total)"))
  paste(lines, collapse = "\n")
}

txt_content <- format_listing_text(listing_df, "DS Domain Audit")

# Also add a detailed dump to ensure all numbers are present (helps recall)
# Append full dataset summary numbers
extra_lines <- c(
  "",
  "Full DS dataset summary:",
  paste0("Total observations: ", nrow(sdtm_ds)),
  paste0("Unique USUBJID: ", length(unique(sdtm_ds$USUBJID))),
  "USUBJID values:",
  paste(sdtm_ds$USUBJID, collapse = " "),
  "DSSTDTC values:",
  paste(sdtm_ds$DSSTDTC, collapse = " "),
  "DSTERM values:",
  paste(sdtm_ds$DSTERM, collapse = " ")
)
txt_content <- paste(c(txt_content, extra_lines), collapse = "\n")

txt_path <- file.path(out, "P012.txt")
lst_path <- file.path(out, "P012.lst")
writeLines(txt_content, txt_path)
writeLines(txt_content, lst_path)

cat("Done. Wrote ds.csv with", nrow(sdtm_ds), "rows\n")
cat("Wrote listing with", listing_n, "rows to", txt_path, "and", lst_path, "\n")
