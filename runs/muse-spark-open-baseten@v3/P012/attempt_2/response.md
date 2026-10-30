```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_demog.csv")

candidates <- c(
  raw_file,
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_demog.csv",
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_demog.csv"
)
raw_file_found <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_file_found <- p; break }
}
if (is.null(raw_file_found)) {
  all <- list.files(project_path, pattern = "raw_demog\\.csv", recursive = TRUE, full.names = TRUE)
  if (length(all) > 0) raw_file_found <- all[1]
}
if (is.null(raw_file_found)) stop(paste("raw_demog.csv not found. Tried:", paste(candidates, collapse="; ")))
raw_file <- raw_file_found

df_raw <- NULL
if (requireNamespace("readr", quietly = TRUE)) {
  df_raw <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()), na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
  df_raw <- as.data.frame(df_raw, stringsAsFactors = FALSE)
} else {
  df_raw <- read.csv(raw_file, stringsAsFactors = FALSE, colClasses = "character", na.strings = c("", "NA"), check.names = FALSE)
}
names(df_raw) <- toupper(names(df_raw))

required <- c("SUBJ_ID", "DISPOSITION", "DISPOSITION_REASON", "STUDY_END_DAT")
for (col in required) {
  if (!col %in% names(df_raw)) {
    df_raw[[col]] <- NA_character_
  }
}

sas_strip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(x, which = "both"))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  n <- max(lengths(args))
  args <- lapply(args, function(a) rep(a, length.out = n))
  vapply(seq_len(n), function(i) {
    vals <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v), which="both")
      if (v == "") return(NA_character_)
      v
    })
    vals <- vals[!is.na(vals)]
    if (length(vals)==0) return(NA_character_)
    paste(vals, collapse = sep)
  }, character(1))
}

parse_study_end_dat <- function(x_char) {
  x_trim <- sas_strip(x_char)
  x_trim[x_trim == ""] <- NA_character_
  idx <- which(!is.na(x_trim))
  if (length(idx) == 0) return(rep(as.Date(NA), length(x_char)))
  vals <- x_trim[idx]
  is_iso <- grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}", vals)
  if (all(is_iso)) {
    if (any(grepl("-", vals, fixed=TRUE))) {
      return(suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d")))
    } else {
      return(suppressWarnings(as.Date(x_trim, format = "%Y/%m/%d")))
    }
  }
  has_slash <- grepl("/", vals, fixed = TRUE)
  if (any(has_slash)) {
    first_fields <- suppressWarnings(as.numeric(sub("/.*", "", vals[has_slash])))
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    if (use_dmy) {
      res <- suppressWarnings(as.Date(x_trim, format = "%d/%m/%Y"))
      return(res)
    } else {
      res <- suppressWarnings(as.Date(x_trim, format = "%m/%d/%Y"))
      return(res)
    }
  }
  has_alpha <- grepl("[A-Za-z]", vals)
  if (any(has_alpha)) {
    res <- suppressWarnings(as.Date(x_trim, format = "%d-%b-%Y"))
    miss <- is.na(res) & !is.na(x_trim)
    if (any(miss)) {
      res2 <- suppressWarnings(as.Date(x_trim[miss], format = "%d-%B-%Y"))
      res[miss] <- res2
    }
    miss <- is.na(res) & !is.na(x_trim)
    if (any(miss)) {
      res2 <- suppressWarnings(as.Date(x_trim[miss], format = "%d %b %Y"))
      res[miss] <- res2
    }
    return(res)
  }
  has_dash <- grepl("-", vals, fixed = TRUE)
  if (any(has_dash)) {
    first_fields <- suppressWarnings(as.numeric(sub("-.*", "", vals[has_dash])))
    use_dmy <- any(first_fields > 12, na.rm = TRUE)
    if (use_dmy) {
      return(suppressWarnings(as.Date(x_trim, format = "%d-%m-%Y")))
    } else {
      return(suppressWarnings(as.Date(x_trim, format = "%m-%d-%Y")))
    }
  }
  return(suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d")))
}

n <- nrow(df_raw)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("DS", n)
USUBJID <- sas_catx("-", STUDYID, df_raw$SUBJ_ID)
DSSEQ <- rep(1L, n)
DSCAT <- rep("DISPOSITION EVENT", n)
DSSCAT <- rep("STUDY PARTICIPATION", n)
DSTERM <- sas_upcase(sas_strip(df_raw$DISPOSITION_REASON))
DSDECOD <- DSTERM
disp_up <- sas_upcase(sas_strip(df_raw$DISPOSITION))
EPOCH <- ifelse(!is.na(disp_up) & disp_up == "SCREEN FAILURE", "SCREENING", "FOLLOW-UP")

tmp_dsdt <- parse_study_end_dat(df_raw$STUDY_END_DAT)
DSSTDTC <- ifelse(is.na(tmp_dsdt), NA_character_, format(tmp_dsdt, "%Y-%m-%d"))

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

ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE)
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]

write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

print_n <- min(12, nrow(sdtm_ds))
print_df <- sdtm_ds[seq_len(print_n), , drop = FALSE]
print_df_with_obs <- cbind(Obs = seq_len(print_n), print_df)

write.csv(print_df_with_obs, file.path(out, "P012.lst.csv"), row.names = FALSE, na = "")
write.csv(print_df_with_obs, file.path(out, "P012.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P012.lst.txt")
txt_path2 <- file.path(out, "P012.txt")
col_names <- names(print_df_with_obs)
col_widths <- sapply(seq_along(col_names), function(i) {
  col <- col_names[i]
  vals <- as.character(print_df_with_obs[[i]])
  vals[is.na(vals)] <- ""
  max(nchar(col, type="width"), max(nchar(vals, type="width"), na.rm=TRUE), na.rm=TRUE)
})
col_widths <- pmax(col_widths, 6)

format_row <- function(vals, widths) {
  formatted <- mapply(function(v, w, nm) {
    v <- ifelse(is.na(v), "", as.character(v))
    if (nm %in% c("Obs", "DSSEQ")) {
      sprintf(paste0("%", w, "s"), v)
    } else {
      sprintf(paste0("%-", w, "s"), v)
    }
  }, vals, widths, col_names, SIMPLIFY = TRUE)
  paste(formatted, collapse = "  ")
}

header_lines <- c(
  "DS Domain Audit",
  paste(rep("-", 80), collapse = ""),
  ""
)

for (tp in c(txt_path, txt_path2)) {
  con <- file(tp, open = "wt")
  writeLines(header_lines, con)
  hdr_line <- format_row(as.list(col_names), col_widths)
  sep_line <- paste(sapply(col_widths, function(w) paste(rep("-", w), collapse="")), collapse="  ")
  writeLines(hdr_line, con)
  writeLines(sep_line, con)
  for (i in seq_len(nrow(print_df_with_obs))) {
    row_vals <- as.list(print_df_with_obs[i, , drop=FALSE])
    line <- format_row(row_vals, col_widths)
    writeLines(line, con)
  }
  writeLines("", con)
  writeLines(paste0("NOTE: ", nrow(sdtm_ds), " observations in dataset sdtm.ds; first ", print_n, " displayed."), con)
  close(con)
}
```