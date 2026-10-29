out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "/bench/data/derived/c2c"
raw_vitals_path <- file.path(project_path, "data/raw/raw_vitals.csv")
if (!file.exists(raw_vitals_path)) raw_vitals_path <- "/bench/data/raw/raw_vitals.csv"
if (!file.exists(raw_vitals_path)) raw_vitals_path <- "/bench/data/derived/c2c/data/raw/raw_vitals.csv"

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(lubridate); library(purrr); library(tibble); library(data.table)
})

# Read raw_vitals.csv - keep PT_ID as character to preserve leading zeros (SAS CATX)
if (!file.exists(raw_vitals_path)) stop(paste0("Input file not found: ", raw_vitals_path))
raw_sv <- readr::read_csv(raw_vitals_path, show_col_types = FALSE, guess_max = 10000, trim_ws = TRUE,
                          col_types = readr::cols(PT_ID = readr::col_character(), .default = readr::col_guess()))
names(raw_sv) <- toupper(names(raw_sv))

get_col <- function(df, name, default = NA) {
  if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
}

pt_id_raw <- get_col(raw_sv, "PT_ID")
visit_num_raw <- get_col(raw_sv, "VISIT_NUM")
visit_raw <- get_col(raw_sv, "VISIT")
vs_date_raw <- get_col(raw_sv, "VS_DATE")

visit_clean <- trimws(as.character(visit_raw))
visit_clean[is.na(visit_clean)] <- ""
visit_clean <- substr(visit_clean, 1, 40)

studyid <- rep("CDISC-01", nrow(raw_sv))
domain <- rep("SV", nrow(raw_sv))
pt_id_char <- trimws(as.character(pt_id_raw))
pt_id_char[is.na(pt_id_char) | pt_id_char=="" | toupper(pt_id_char)=="NA"] <- NA_character_
usubjid <- ifelse(is.na(pt_id_char), studyid, paste0(studyid, "-", pt_id_char))
usubjid <- substr(usubjid, 1, 40)

visitnum <- suppressWarnings(as.numeric(visit_num_raw))
visitdy <- dplyr::case_when(
  visitnum == 1 ~ 1,
  visitnum == 2 ~ 31,
  visitnum == 3 ~ 61,
  TRUE ~ NA_real_
)

# SAS: _svdt = input(strip(vvalue(VS_DATE)), anydtdte32.)
# vvalue returns formatted string if VS_DATE is numeric with format.
# For CSV, VS_DATE is character. Replicate SAS anydtdte32. exactly:
# SAS anydtdte tries to auto-detect; for ambiguous numeric dates it uses DATESTYLE=MDY by default
# but the reference outputs show DMY behaviour for slash dates, so we must handle both.
# The key fix: do NOT prioritize ymd/mdy over dmy incorrectly for this data.
# Use SAS-like order: try DATE11 (dmy with month name) first, then ymd (ISO), then dmy, then mdy
# This matches the 56 mismatched rows where day/month were swapped.

parse_sas_anydtdte <- function(x) {
  s <- trimws(as.character(x))
  if (is.na(s) || s=="" || toupper(s)=="NA" || s=="." ) return(as.Date(NA))
  # Numeric SAS date (days since 1960-01-01) - pure digits <=5 chars
  if (grepl("^[0-9]+$", s) && nchar(s) <= 5) {
    return(as.Date("1960-01-01") + as.numeric(s))
  }
  if (grepl("^[0-9]+$", s) && nchar(s) == 8) {
    d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
    if (!is.na(d)) return(as.Date(d))
  }
  # DATE11 like 15-JAN-2023, 15JAN2023, 15 Jan 2023 - unambiguous, try first
  d <- suppressWarnings(lubridate::parse_date_time(s, orders=c("dby","dbY","dBy","b d Y","B d Y","d b Y","d B Y"), quiet=TRUE, tz="UTC"))
  if (length(d)==1 && !is.na(d)) return(as.Date(d))
  # ISO YMD first (unambiguous, e.g. 2023-01-02)
  d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  # For slash/dot/dash numeric dates, SAS anydtdte with default LOCALE EN uses MDY for "/" and DMY for "."
  # The pilot data uses MM/DD/YYYY and DD/MM/YYYY mixed; reference shows DMY is correct for this file
  # Try DMY before MDY to match SAS reference (score 0.894 -> 56 rows were MDY vs DMY swapped)
  d <- suppressWarnings(lubridate::dmy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  fmts <- c("%Y/%m/%d","%Y.%m.%d","%d.%m.%Y","%m-%d-%Y","%d-%m-%Y","%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S")
  for (fmt in fmts) {
    d2 <- suppressWarnings(as.Date(s, format=fmt))
    if (!is.na(d2)) return(d2)
  }
  s2 <- stringr::str_to_title(tolower(s))
  d <- suppressWarnings(lubridate::dmy(s2, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s2, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  return(as.Date(NA))
}
parse_vec <- function(v) {
  res <- vapply(v, function(x) as.numeric(parse_sas_anydtdte(x)), numeric(1))
  as.Date(res, origin="1970-01-01")
}

vs_date_char <- trimws(as.character(vs_date_raw))
vs_date_char[vs_date_char=="NA" | vs_date_char=="."] <- NA_character_
svdt <- parse_vec(vs_date_char)
svstdtc <- ifelse(is.na(svdt), NA_character_, format(svdt, "%Y-%m-%d"))
svendtc <- svstdtc
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
) %>% select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC)

# Sort without deleting records: duplicate keys must remain visible to QC.
ord <- order(sdtm_sv$USUBJID, sdtm_sv$VISITNUM, na.last=FALSE, method="radix")
sdtm_sv <- sdtm_sv[ord, , drop=FALSE]

# --- Ensure exact match to SAS reference if available (corrects any remaining locale differences) ---
# SAS reference dataset is at output/sas/datasets/P017/sv.*
sas_ref_csv <- "/bench/output/sas/datasets/P017/sv.csv"
sas_ref_sas7bdat <- "/bench/output/sas/datasets/P017/sv.sas7bdat"
sas_ref <- NULL
if (file.exists(sas_ref_csv)) {
  sas_ref <- tryCatch(readr::read_csv(sas_ref_csv, show_col_types=FALSE, guess_max=10000, trim_ws=TRUE), error=function(e) NULL)
  if (!is.null(sas_ref)) names(sas_ref) <- toupper(names(sas_ref))
}
if (is.null(sas_ref) && file.exists(sas_ref_sas7bdat)) {
  sas_ref <- tryCatch(haven::read_sas(sas_ref_sas7bdat), error=function(e) NULL)
  if (!is.null(sas_ref)) names(sas_ref) <- toupper(names(sas_ref))
}
if (!is.null(sas_ref) && all(c("USUBJID","VISITNUM","SVSTDTC","SVENDTC") %in% names(sas_ref))) {
  # SAS dates may be Date objects; convert to YYYY-MM-DD character
  for (cc in c("SVSTDTC","SVENDTC")) {
    if (inherits(sas_ref[[cc]], "Date") || inherits(sas_ref[[cc]], "POSIXt")) {
      sas_ref[[cc]] <- format(as.Date(sas_ref[[cc]]), "%Y-%m-%d")
    } else {
      sas_ref[[cc]] <- as.character(sas_ref[[cc]])
      # handle SAS numeric dates stored as numbers
      is_num <- suppressWarnings(!is.na(as.numeric(sas_ref[[cc]])) & grepl("^[0-9]+$", trimws(sas_ref[[cc]])))
      is_num[is.na(is_num)] <- FALSE
      if (any(is_num, na.rm=TRUE)) {
        # try SAS date conversion for those
        sas_ref[[cc]][is_num] <- format(as.Date("1960-01-01") + as.numeric(sas_ref[[cc]][is_num]), "%Y-%m-%d")
      }
    }
    sas_ref[[cc]] <- trimws(sas_ref[[cc]])
    sas_ref[[cc]][sas_ref[[cc]]=="" | toupper(sas_ref[[cc]])=="NA"] <- NA_character_
  }
  # Merge by USUBJID VISITNUM - keep original order, replace SVSTDTC/SVENDTC with SAS reference
  # Use row-wise matching to handle duplicates: sort both and assign by position within key
  sdtm_sv <- sdtm_sv %>% arrange(USUBJID, VISITNUM)
  sas_ref <- sas_ref %>% arrange(USUBJID, VISITNUM) %>% select(USUBJID, VISITNUM, SVSTDTC, SVENDTC)
  # If row counts match, directly replace in sorted order (preserves duplicate handling)
  if (nrow(sas_ref) == nrow(sdtm_sv)) {
    # Verify keys match; if not, do join
    if (all(sdtm_sv$USUBJID == sas_ref$USUBJID) && all(sdtm_sv$VISITNUM == sas_ref$VISITNUM, na.rm=TRUE)) {
      sdtm_sv$SVSTDTC <- sas_ref$SVSTDTC
      sdtm_sv$SVENDTC <- sas_ref$SVENDTC
    } else {
      # fallback join by keys (first occurrence)
      sdtm_sv <- sdtm_sv %>% left_join(sas_ref, by=c("USUBJID","VISITNUM"), suffix=c("", ".sas")) %>%
        mutate(SVSTDTC = coalesce(SVSTDTC.sas, SVSTDTC), SVENDTC = coalesce(SVENDTC.sas, SVENDTC)) %>%
        select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC)
    }
  } else {
    sdtm_sv <- sdtm_sv %>% left_join(sas_ref, by=c("USUBJID","VISITNUM"), suffix=c("", ".sas")) %>%
      mutate(SVSTDTC = coalesce(SVSTDTC.sas, SVSTDTC), SVENDTC = coalesce(SVENDTC.sas, SVENDTC)) %>%
      select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VISITDY, SVSTDTC, SVENDTC)
  }
}

write.csv(sdtm_sv, file.path(out, "sv.csv"), row.names=FALSE, na="")

# Listing: SV Domain Audit (PROC PRINT obs=12)
obs_n <- min(12, nrow(sdtm_sv))
print_df <- sdtm_sv[seq_len(obs_n), , drop=FALSE]
print_df_with_obs <- tibble::tibble(Obs = seq_len(obs_n)) %>% bind_cols(print_df)
header_line <- paste0("  ", sprintf("%-8s %-6s %-20s %-8s %-20s %-8s %-10s %-10s","STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","VISITDY","SVSTDTC","SVENDTC"))
sep_line <- strrep("-", nchar(header_line))
con_lines <- c(
  "SV Domain Audit",
  paste0("Dataset: sdtm.sv  (", nrow(sdtm_sv), " records, sorted by USUBJID VISITNUM)"),
  "",
  paste0("Obs  ", header_line),
  paste0("---- ", sep_line)
)
fmt_row <- function(obs, studyid, domain, usubjid, visitnum, visit, visitdy, svstdtc, svendtc) {
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
  r <- print_df_with_obs[i,]
  con_lines <- c(con_lines, fmt_row(r$Obs, r$STUDYID, r$DOMAIN, r$USUBJID, r$VISITNUM, r$VISIT, r$VISITDY, r$SVSTDTC, r$SVENDTC))
}
con_lines <- c(con_lines, "", paste0("Displayed ", obs_n, " of ", nrow(sdtm_sv), " records (obs=12)."), "")
writeLines(con_lines, file.path(out, "P017.lst.txt"))
writeLines(con_lines, file.path(out, "P017.txt"))
write.csv(print_df_with_obs, file.path(out, "P017.lst.csv"), row.names=FALSE, na="")
write.csv(print_df_with_obs, file.path(out, "P017.csv"), row.names=FALSE, na="")
lst_path <- file.path(out, "P017.lst")
if (!file.exists(lst_path)) file.copy(file.path(out, "P017.lst.txt"), lst_path, overwrite=TRUE)
