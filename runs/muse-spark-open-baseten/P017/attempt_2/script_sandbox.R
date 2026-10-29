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

# Read raw_vitals.csv preserving PT_ID as character to keep leading zeros like SAS CATX
# SAS PROC IMPORT guesses types; PT_ID is character in source, keep as character
if (!file.exists(raw_vitals_path)) stop(paste0("Input file not found: ", raw_vitals_path))

# Read with PT_ID forced to character, others guessed
raw_sv <- readr::read_csv(raw_vitals_path, show_col_types = FALSE, guess_max = 10000, trim_ws = TRUE,
                          col_types = readr::cols(PT_ID = readr::col_character(), .default = readr::col_guess()))
names(raw_sv) <- toupper(names(raw_sv))
cat("Imported raw_sv:", nrow(raw_sv), "rows,", ncol(raw_sv), "cols\n")
cat("Columns:", paste(names(raw_sv), collapse=", "), "\n")

get_col <- function(df, name, default = NA) {
  if (name %in% names(df)) df[[name]] else rep(default, nrow(df))
}

pt_id_raw <- get_col(raw_sv, "PT_ID")
visit_num_raw <- get_col(raw_sv, "VISIT_NUM")
visit_raw <- get_col(raw_sv, "VISIT")
vs_date_raw <- get_col(raw_sv, "VS_DATE")

# SAS semantics: VISIT = strip(VISIT); length $40
visit_clean <- trimws(as.character(visit_raw))
visit_clean[is.na(visit_clean)] <- ""
visit_clean <- substr(visit_clean, 1, 40)

# USUBJID = catx("-", STUDYID, PT_ID)  SAS catx strips and skips missing
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
# anydtdte32. in SAS defaults to MDY for ambiguous slash dates
# Replicate SAS anydtdte: prioritize YMD (ISO), then MDY, then DMY, then DATE11
parse_sas_anydtdte <- function(x) {
  s <- trimws(as.character(x))
  if (is.na(s) || s=="" || toupper(s)=="NA" || s=="." ) return(as.Date(NA))
  # Pure digits: SAS numeric date (days since 1960-01-01) when width <=5
  if (grepl("^[0-9]+$", s)) {
    if (nchar(s) <= 5) {
      # SAS date
      return(as.Date("1960-01-01") + as.numeric(s))
    }
    if (nchar(s) == 8) {
      d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
      if (!is.na(d)) return(as.Date(d))
    }
  }
  # ISO YMD first (unambiguous)
  d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  # MDY before DMY to match SAS default DATESTYLE=MDY
  d <- suppressWarnings(lubridate::mdy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::dmy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  # DATE11 like 15-JAN-2023, 15JAN2023, 15 Jan 2023
  d <- suppressWarnings(lubridate::parse_date_time(s, orders=c("dby","dbY","dBy","b d Y","B d Y","d b Y","d B Y"), quiet=TRUE, tz="UTC"))
  if (length(d)==1 && !is.na(d)) return(as.Date(d))
  # Explicit fallback formats
  fmts <- c("%d-%b-%Y","%d-%B-%Y","%d-%b-%y","%d-%B-%y","%Y/%m/%d","%Y.%m.%d","%d.%m.%Y","%m-%d-%Y","%d-%m-%Y","%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S")
  for (fmt in fmts) {
    d2 <- suppressWarnings(as.Date(s, format=fmt))
    if (!is.na(d2)) return(d2)
  }
  # Title-case month retry (SAS upcase month)
  s2 <- stringr::str_to_title(tolower(s))
  d <- suppressWarnings(lubridate::ymd(s2, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s2, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::dmy(s2, quiet=TRUE))
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
# SAS sort: by USUBJID VISITNUM; missing first; ASCII (uppercase before lowercase)
# Use base order with na.last=FALSE to match SAS missing first
ord <- order(sdtm_sv$USUBJID, sdtm_sv$VISITNUM, na.last=FALSE, method="radix")
sdtm_sv <- sdtm_sv[ord, , drop=FALSE]

write.csv(sdtm_sv, file.path(out, "sv.csv"), row.names=FALSE, na="")
cat("Wrote sv.csv with", nrow(sdtm_sv), "rows\n")

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

txt_path1 <- file.path(out, "P017.lst.txt")
txt_path2 <- file.path(out, "P017.txt")
csv_path1 <- file.path(out, "P017.lst.csv")
csv_path2 <- file.path(out, "P017.csv")
writeLines(con_lines, txt_path1)
writeLines(con_lines, txt_path2)
write.csv(print_df_with_obs, csv_path1, row.names=FALSE, na="")
write.csv(print_df_with_obs, csv_path2, row.names=FALSE, na="")
lst_path <- file.path(out, "P017.lst")
if (!file.exists(lst_path)) file.copy(txt_path1, lst_path, overwrite=TRUE)
cat("Done. Outputs in", out, "\n")
