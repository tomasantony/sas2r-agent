```r
out <- Sys.getenv("OUT_DIR")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

# SAS round half away from zero helper (w.d formats truncate unless ROUND)
round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

# SAS BEST32. formatting: up to 12 significant digits, strip, missing -> "."
fmt_best <- function(x) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(".")
    # format with 12 significant digits, no scientific unless needed
    s <- format(xi, digits = 12, scientific = FALSE, trim = TRUE, nsmall = 0, big.mark = "")
    s <- trimws(s)
    # remove trailing decimal point if any
    s <- sub("\\.$", "", s)
    s
  }, character(1), USE.NAMES = FALSE)
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ecg.csv"
if (!file.exists(raw_path)) {
  # fallback: try forward-slash variant via project_path env if present
  stop(paste0("Input file not found: ", raw_path))
}

# PROC IMPORT csv with GETNAMES=YES; force uppercase names (options validvarname=upcase)
raw_eg <- readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA"),
  show_col_types = FALSE,
  progress = FALSE)
names(raw_eg) <- toupper(names(raw_eg))

# stopifnot columns exist (after uppercasing)
stopifnot(all(c("SUBJECT","VISIT_NAM","VISIT_NUM","TEST_NAME","RESULT","UNIT","DATE") %in% names(raw_eg)))

df <- raw_eg %>%
  mutate(
    SUBJECT_c = ifelse(is.na(SUBJECT), "", trimws(SUBJECT)),
    VISIT_NAM_c = ifelse(is.na(VISIT_NAM), "", trimws(VISIT_NAM)),
    TEST_NAME_c = ifelse(is.na(TEST_NAME), "", trimws(TEST_NAME)),
    UNIT_c = ifelse(is.na(UNIT), "", trimws(UNIT)),
    VISIT_NUM_n = suppressWarnings(as.numeric(trimws(VISIT_NUM))),
    RESULT_n = suppressWarnings(as.numeric(trimws(RESULT))),
    DATE_c = trimws(as.character(DATE))
  )

# ---- DATE handling: mimic SAS vtype check + ANYDTDTE -> IS8601DA ----
# PROC IMPORT may infer DATE as numeric SAS date (days since 1960-01-01).
# Here we read as character; digit-only strings are treated as SAS numeric dates.
date_raw <- df$DATE_c
date_raw[date_raw == ""] <- NA_character_

is_num_date <- !is.na(date_raw) & grepl("^[0-9]+(\\.[0-9]+)?$", date_raw)

# character date part: strip time portion after space or T (for datetime strings)
date_part <- ifelse(is.na(date_raw), NA_character_, sub("[T ].*$", "", date_raw, perl = TRUE))
# for slash dates with time like "01/15/2013 10:30", above already cut; keep as is otherwise

# Detect ONE explicit strict format from first non-missing non-numeric value
cand <- date_part[!is.na(date_part) & date_part != "" & !is_num_date]
fmt <- "%Y-%m-%d"
sample1 <- if (length(cand) > 0) cand[1] else NA_character_
if (!is.na(sample1)) {
  if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", sample1)) {
    fmt <- "%Y-%m-%d"
  } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{4}$", sample1)) {
    fmt <- "%m/%d/%Y"
  } else if (grepl("^\\d{1,2}/\\d{1,2}/\\d{2}$", sample1)) {
    fmt <- "%m/%d/%y"
  } else if (grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", sample1)) {
    fmt <- "%Y/%m/%d"
  } else if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{4}$", sample1)) {
    fmt <- "%d-%b-%Y"
  } else if (grepl("^\\d{1,2}-[A-Za-z]{3}-\\d{2}$", sample1)) {
    fmt <- "%d-%b-%y"
  } else if (grepl("^\\d{1,2}-[0-9]{1,2}-\\d{4}$", sample1)) {
    fmt <- "%m-%d-%Y"
  } else if (grepl("^\\d{4}-\\d{1,2}-\\d{1,2}.*$", sample1)) {
    fmt <- "%Y-%m-%d"
  } else {
    fmt <- "%Y-%m-%d"
  }
}

# Normalize month abbreviation case for %b formats (e.g. JAN -> Jan)
date_part_norm <- date_part
if (fmt %in% c("%d-%b-%Y", "%d-%b-%y")) {
  date_part_norm <- vapply(date_part, function(z) {
    if (is.na(z) || z == "") return(z)
    # expect dd-Mon-yyyy
    parts <- strsplit(z, "-", fixed = TRUE)[[1]]
    if (length(parts) == 3) {
      mon <- paste0(toupper(substr(parts[2], 1, 1)), tolower(substr(parts[2], 2, nchar(parts[2]))))
      paste0(parts[1], "-", mon, "-", parts[3])
    } else z
  }, character(1), USE.NAMES = FALSE)
}

egdt <- rep(as.Date(NA), length(date_raw))
# numeric SAS dates
if (any(is_num_date, na.rm = TRUE)) {
  egdt[is_num_date] <- as.Date(as.numeric(date_raw[is_num_date]), origin = "1960-01-01")
}
# character dates with ONE explicit strict format
idx_c <- !is.na(date_part_norm) & date_part_norm != "" & !is_num_date
if (any(idx_c)) {
  parsed_c <- as.Date(date_part_norm[idx_c], format = fmt)
  egdt[idx_c] <- parsed_c
}
EGDTC_vec <- format(egdt, format = "%Y-%m-%d")
EGDTC_vec[is.na(egdt)] <- NA_character_

# ---- Build SDTM EG ----
# EGORRES / EGSTRESC via BEST32.
EGORRES_vec <- fmt_best(df$RESULT_n)
EGSTRESC_vec <- fmt_best(df$RESULT_n)

eg1 <- tibble::tibble(
  STUDYID = "CDISC-01",
  DOMAIN = "EG",
  USUBJID = paste0("CDISC-01-", df$SUBJECT_c),
  VISITNUM = df$VISIT_NUM_n,
  VISIT = df$VISIT_NAM_c,
  EGTEST = df$TEST_NAME_c,
  EGORRES = EGORRES_vec,
  EGORRESU = df$UNIT_c,
  EGSTRESN = df$RESULT_n,
  EGSTRESC = EGSTRESC_vec,
  EGSTRESU = df$UNIT_c,
  EGDTC = EGDTC_vec
)

# Assign short codes based on test name (case-sensitive)
eg1 <- eg1 %>%
  mutate(
    EGTESTCD = dplyr::case_when(
      EGTEST == "Heart Rate" ~ "HR",
      EGTEST == "QT Duration" ~ "QT",
      EGTEST == "RR Duration" ~ "RR",
      TRUE ~ "UNKNOWN"
    )
  )

# Keep order as first DATA step
eg1 <- eg1 %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, EGTESTCD, EGTEST,
         EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC)

# PROC SORT by USUBJID EGDTC EGTESTCD (missing first, uppercase before lowercase)
# All keys here are uppercase/ISO so binary order equals SAS; enforce missing-first for EGDTC
eg_sorted <- eg1 %>%
  arrange(USUBJID, !is.na(EGDTC), EGDTC, EGTESTCD)

# Second DATA step: RETAIN + BY USUBJID -> EGSEQ
eg_final <- eg_sorted %>%
  group_by(USUBJID) %>%
  mutate(EGSEQ = dplyr::row_number()) %>%
  ungroup()

# Retain / keep final column order
eg_final <- eg_final %>%
  select(STUDYID, DOMAIN, USUBJID, EGSEQ, VISITNUM, VISIT, EGTESTCD, EGTEST,
         EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC) %>%
  arrange(USUBJID, !is.na(EGDTC), EGDTC, EGTESTCD)

stopifnot(all(c("STUDYID","DOMAIN","USUBJID","EGSEQ","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC") %in% names(eg_final)))

write.csv(eg_final, file.path(out, "eg.csv"), row.names = FALSE, na = "")
```