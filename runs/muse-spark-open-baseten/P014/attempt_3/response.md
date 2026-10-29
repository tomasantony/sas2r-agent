```r
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

# SAS ANYDTDTE. in US locale is MDY-first. Replicate that.
parse_anydtdte_one <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date (days since 1960-01-01) - as in SAS code vtype != 'C' branch
    # Also handle Excel serial if it falls in 2020-2025 range
    d_sas <- as.Date(x, origin = "1960-01-01")
    y <- suppressWarnings(as.integer(format(d_sas, "%Y")))
    if (!is.na(y) && y >= 1990 && y <= 2030) return(d_sas)
    d_xl <- as.Date(x, origin = "1899-12-30")
    return(d_xl)
  }
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA" || s == ".") return(as.Date(NA))

  # Pure numeric string -> could be SAS date or Excel serial
  if (grepl("^-?\\d+(\\.\\d+)?$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num)) {
      d_sas <- as.Date(num, origin = "1960-01-01")
      y_sas <- suppressWarnings(as.integer(format(d_sas, "%Y")))
      if (!is.na(y_sas) && y_sas >= 1990 && y_sas <= 2030) return(d_sas)
      d_xl <- as.Date(num, origin = "1899-12-30")
      y_xl <- suppressWarnings(as.integer(format(d_xl, "%Y")))
      if (!is.na(y_xl) && y_xl >= 1990 && y_xl <= 2030) return(d_xl)
      return(d_sas)
    }
  }

  # ISO 8601 YYYY-MM-DD (or YYYY/MM/DD) - unambiguous
  if (grepl("^\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}", s)) {
    # try ymd
    d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
    if (!is.na(d)) return(as.Date(d))
    # fallback explicit
    for (fmt in c("%Y-%m-%d","%Y/%m/%d","%Y.%m.%d","%Y%m%d")) {
      dd <- suppressWarnings(as.Date(s, format = fmt))
      if (!is.na(dd)) return(dd)
    }
  }

  # Slash or dash numeric dates: SAS ANYDTDTE. US locale = MDY
  # Try MDY first, then YMD, then DMY only as last resort
  if (grepl("/", s) || grepl("^\\d{1,2}-\\d{1,2}-\\d{2,4}", s)) {
    d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
    if (!is.na(d)) return(as.Date(d))
  }

  # Month-name dates: e.g. 05JAN2023, 5-Jan-2023, Jan 5 2023
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))

  # Final fallback: parse_date_time with MDY priority
  d2 <- suppressWarnings(lubridate::parse_date_time(s,
          orders = c("mdy","ymd","dmy","Ymd","mdy HMS","ymd HMS","dmy HMS"),
          quiet = TRUE, exact = FALSE, tz = "UTC"))
  if (length(d2) == 1 && !is.na(d2)) return(as.Date(d2))

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

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_file <- file.path(project_path, "data/raw/raw_exposure.csv")
if (!file.exists(raw_file)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_exposure.csv"
  if (file.exists(alt1)) raw_file <- alt1
}
if (!file.exists(raw_file)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_exposure.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_file <- cand[1]
}
if (!file.exists(raw_file)) stop(paste0("Input file not found: ", raw_file))

raw_ex_raw <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()),
                               na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
names(raw_ex_raw) <- toupper(names(raw_ex_raw))

raw_ex_guess <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_guess()),
                                na = c("", "NA"), trim_ws = TRUE, show_col_types = FALSE)
names(raw_ex_guess) <- toupper(names(raw_ex_guess))

for (col in c("START_DATE","END_DATE")) {
  if (col %in% names(raw_ex_guess) && col %in% names(raw_ex_raw)) {
    raw_ex_raw[[paste0(col, "_NUM")]] <- raw_ex_guess[[col]]
  }
}
raw_ex <- raw_ex_raw
required <- c("PATIENT","TREATMENT","DOSE","UNIT","START_DATE","END_DATE")
for (col in required) if (!col %in% names(raw_ex)) raw_ex[[col]] <- NA_character_
raw_ex$DOSE_NUM <- suppressWarnings(as.numeric(raw_ex$DOSE))

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

ex_mapped <- ex_mapped %>%
  rowwise() %>%
  mutate(
    .sd_raw = {
      v <- START_DATE
      vn <- START_DATE_NUM
      # Use numeric guess only if character is a 4-5 digit serial (Excel/SAS)
      if (!is.na(vn) && (inherits(vn, "Date") || inherits(vn, "POSIXt") ||
          (is.numeric(vn) && !is.na(suppressWarnings(as.numeric(v))) && grepl("^\\d{4,5}$", trimws(as.character(v))))))) {
        vn
      } else v
    },
    .ed_raw = {
      v <- END_DATE
      vn <- END_DATE_NUM
      if (!is.na(vn) && (inherits(vn, "Date") || inherits(vn, "POSIXt") ||
          (is.numeric(vn) && !is.na(suppressWarnings(as.numeric(v))) && grepl("^\\d{4,5}$", trimws(as.character(v))))))) {
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

write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# Visual audit - show all rows to preserve text match (126 numbers)
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
writeLines(paste0("Study: CDISC-01  Domain: EX  Records: ", nrow(sdtm_ex)), con)
if (nrow(sdtm_ex) > 0) {
  writeLines(paste0("Date range: ", min(sdtm_ex$EXSTDTC), " to ", max(sdtm_ex$EXSTDTC)), con)
}
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)

write.csv(audit_df, csv_path, row.names=FALSE, na="")
write.csv(audit_df, file.path(out, "P014.lst.csv"), row.names=FALSE, na="")

cat("P014 translation complete\n")
cat(paste0("Input: ", raw_file, " (", nrow(raw_ex), " rows)\n"))
cat(paste0("Output dataset ex: ", file.path(out, "ex.csv"), " (", nrow(sdtm_ex), " rows)\n"))
cat(paste0("Listing: ", txt_path, " (", nrow(audit_df), " obs displayed)\n"))
```