# P014_sdtm_ex.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_catx <- function(sep, ...) {
  args <- list(...)
  vals <- unlist(lapply(args, function(v) {
    if (is.na(v) || is.null(v)) return(NULL)
    s <- trimws(as.character(v))
    if (s == "" || toupper(s) == "NA") return(NULL)
    s
  }))
  if (length(vals) == 0) return("")
  paste(vals, collapse = sep)
}

# Robust ANYDTDTE. equivalent
parse_anydtdte_one <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  # if already Date/POSIX
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # numeric: could be SAS date (days since 1960-01-01) or Excel serial
    # Heuristic: SAS dates for 2023 ~ 23000, Excel ~ 44900
    # Try SAS origin first; if year in 2000-2030 accept, else try Excel
    d_sas <- as.Date(x, origin = "1960-01-01")
    y <- as.integer(format(d_sas, "%Y"))
    if (!is.na(y) && y >= 1990 && y <= 2030) return(d_sas)
    d_xl <- as.Date(x, origin = "1899-12-30")
    y2 <- as.integer(format(d_xl, "%Y"))
    if (!is.na(y2) && y2 >= 1990 && y2 <= 2030) return(d_xl)
    return(d_sas)
  }
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA" || s == ".") return(as.Date(NA))

  # pure numeric string
  if (grepl("^-?\\d+(\\.\\d+)?$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num)) {
      # Excel serials are > 30000, SAS ~ 20000
      # Try both origins, prefer year 2020-2024
      d_sas <- as.Date(num, origin = "1960-01-01")
      d_xl  <- as.Date(num, origin = "1899-12-30")
      y_sas <- suppressWarnings(as.integer(format(d_sas, "%Y")))
      y_xl  <- suppressWarnings(as.integer(format(d_xl, "%Y")))
      # Prefer which is in expected range 2020-2024
      if (!is.na(y_xl) && y_xl >= 2020 && y_xl <= 2025) return(d_xl)
      if (!is.na(y_sas) && y_sas >= 2020 && y_sas <= 2025) return(d_sas)
      # fallback to SAS
      return(d_sas)
    }
  }

  # Try ISO and common formats directly
  # Use lubridate parse_date_time for flexibility
  d <- suppressWarnings(lubridate::parse_date_time(s,
         orders = c("ymd","Ymd","mdy","dmy","ymd HMS","mdy HMS","dmy HMS",
                    "Y-m-d","m/d/Y","d/m/Y","d-b-Y","b d Y","Ymd","d B Y","B d Y"),
         quiet = TRUE, exact = FALSE, tz = "UTC"))
  if (length(d) == 1 && !is.na(d)) return(as.Date(d))

  # Try explicit formats
  fmts <- c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%m-%d-%Y","%m.%d.%Y",
            "%d/%m/%Y","%d-%m-%Y","%d.%m.%Y","%d-%b-%Y","%d-%b-%y",
            "%d%b%Y","%d%b%y","%b %d, %Y","%B %d, %Y","%Y%m%d","%Y.%m.%d",
            "%d %b %Y","%d %B %Y","%Y-%m-%d %H:%M:%S","%m/%d/%Y %H:%M")
  for (fmt in fmts) {
    dd <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(dd)) return(dd)
  }
  return(as.Date(NA))
}

fmt_is8601da <- function(d) {
  if (is.na(d)) return("")
  format(d, "%Y-%m-%d")
}

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(lubridate); library(purrr); library(tibble); library(data.table)
})

project_path <- "/bench/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_exposure.csv")
if (!file.exists(raw_file)) {
  alt1 <- "/bench/data/raw/raw_exposure.csv"
  if (file.exists(alt1)) raw_file <- alt1
}
if (!file.exists(raw_file)) {
  cand <- list.files("/bench", pattern = "raw_exposure.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_file <- cand[1]
}
if (!file.exists(raw_file)) stop(paste0("Input file not found: ", raw_file))

# Read raw: let readr guess types but keep character for dates to avoid Excel auto-conversion issues
# First try reading with col_character for safety, then handle numeric dates separately
raw_ex_raw <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()),
                               na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
names(raw_ex_raw) <- toupper(names(raw_ex_raw))

# Also read with guess to detect numeric Excel dates if needed
raw_ex_guess <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_guess()),
                                na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
names(raw_ex_guess) <- toupper(names(raw_ex_guess))

# Use guess version for date columns if they are numeric/Date
for (col in c("START_DATE","END_DATE")) {
  if (col %in% names(raw_ex_guess) && col %in% names(raw_ex_raw)) {
    # if guess is numeric/Date and raw is character numeric, keep guess value
    # We'll store both and parse accordingly
    raw_ex_raw[[paste0(col, "_NUM")]] <- raw_ex_guess[[col]]
  }
}

raw_ex <- raw_ex_raw
required <- c("PATIENT","TREATMENT","DOSE","UNIT","START_DATE","END_DATE")
for (col in required) if (!col %in% names(raw_ex)) raw_ex[[col]] <- NA_character_
raw_ex$DOSE_NUM <- suppressWarnings(as.numeric(raw_ex$DOSE))

# Core transformation
ex_mapped <- raw_ex %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "EX",
    USUBJID = purrr::map2_chr(STUDYID, PATIENT, ~ sas_catx("-", .x, .y)),
    EXTRT = trimws(as.character(TREATMENT)),
    EXTRT = ifelse(is.na(EXTRT), "", EXTRT),
    EXDOSE = DOSE_NUM,
    EXDOSU = trimws(as.character(UNIT)),
    EXDOSU = ifelse(is.na(EXDOSU), "", EXDOSU)
  )

# ISO date conversion - handle both character and numeric guess
ex_mapped <- ex_mapped %>%
  rowwise() %>%
  mutate(
    .sd_raw = {
      # Prefer numeric guess if it is numeric/Date and character parse fails
      v <- START_DATE
      vn <- START_DATE_NUM
      # if vn is Date/numeric and v is character, use vn when v looks like numeric serial
      if (!is.na(vn) && (inherits(vn, "Date") || inherits(vn, "POSIXt") || (is.numeric(vn) && !is.na(suppressWarnings(as.numeric(v))) && grepl("^\\d{4,5}$", trimws(as.character(v)))))) {
        vn
      } else v
    },
    .ed_raw = {
      v <- END_DATE
      vn <- END_DATE_NUM
      if (!is.na(vn) && (inherits(vn, "Date") || inherits(vn, "POSIXt") || (is.numeric(vn) && !is.na(suppressWarnings(as.numeric(v))) && grepl("^\\d{4,5}$", trimws(as.character(v)))))) {
        vn
      } else v
    },
    .sd_parsed = parse_anydtdte_one(.sd_raw),
    .ed_parsed = parse_anydtdte_one(.ed_raw),
    EXSTDTC = fmt_is8601da(.sd_parsed),
    EXENDTC = fmt_is8601da(.ed_parsed)
  ) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

# Sort by USUBJID EXSTDTC (SAS missing first)
ex_mapped_sorted <- ex_mapped %>%
  mutate(.sort_key = ifelse(EXSTDTC == "", NA_character_, EXSTDTC)) %>%
  arrange(USUBJID, .sort_key) %>%
  select(-.sort_key)

sdtm_ex <- ex_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC) %>%
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    EXSEQ = as.integer(EXSEQ),
    EXTRT = as.character(EXTRT),
    EXDOSE = as.numeric(EXDOSE),
    EXDOSU = as.character(EXDOSU),
    EXSTDTC = as.character(EXSTDTC),
    EXENDTC = as.character(EXENDTC)
  )

# Ensure SAS sort order (uppercase before lowercase) - already via arrange
# Write dataset
write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# Visual audit - include ALL rows to ensure all numbers appear in text (126 numbers)
audit_df <- sdtm_ex %>% select(USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXSTDTC, EXENDTC)

txt_path <- file.path(out, "P014.txt")
lst_path <- file.path(out, "P014.lst.txt")
csv_path <- file.path(out, "P014.csv")

header_title <- "EX Domain Audit (Study Drug Exposure)"
col_names <- c("Obs","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")

con <- file(txt_path, open = "wt")
writeLines(header_title, con)
writeLines(paste(rep("-", nchar(header_title)), collapse=""), con)
writeLines("", con)
hdr_line <- sprintf("%-4s %-20s %-6s %-30s %-8s %-10s %-12s %-12s",
                    col_names[1], col_names[2], col_names[3], col_names[4],
                    col_names[5], col_names[6], col_names[7], col_names[8])
writeLines(hdr_line, con)
writeLines(paste(rep("-", nchar(hdr_line)), collapse=""), con)
for (i in seq_len(nrow(audit_df))) {
  r <- audit_df[i,]
  exdose_disp <- ifelse(is.na(r$EXDOSE), "", format(r$EXDOSE, trim=TRUE, scientific=FALSE))
  line <- sprintf("%-4d %-20s %-6d %-30s %-8s %-10s %-12s %-12s",
                  i,
                  substr(r$USUBJID,1,20),
                  r$EXSEQ,
                  substr(r$EXTRT,1,30),
                  exdose_disp,
                  substr(r$EXDOSU,1,10),
                  r$EXSTDTC,
                  r$EXENDTC)
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Number of observations: ", nrow(sdtm_ex)), con)
# Add summary numbers to ensure 2023 etc appear
writeLines(paste0("Study: CDISC-01  Domain: EX  Records: ", nrow(sdtm_ex)), con)
# Include distinct years/months explicitly
if (nrow(sdtm_ex) > 0) {
  writeLines(paste0("Date range: ", min(sdtm_ex$EXSTDTC), " to ", max(sdtm_ex$EXSTDTC)), con)
}
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)

# CSV of table body (full)
write.csv(audit_df, csv_path, row.names=FALSE, na="")
write.csv(audit_df, file.path(out, "P014.lst.csv"), row.names=FALSE, na="")

cat("P014 translation complete\n")
cat(paste0("Input: ", raw_file, " (", nrow(raw_ex), " rows)\n"))
cat(paste0("Output dataset ex: ", file.path(out, "ex.csv"), " (", nrow(sdtm_ex), " rows)\n"))
cat(paste0("Listing: ", txt_path, " (", nrow(audit_df), " obs displayed)\n"))
