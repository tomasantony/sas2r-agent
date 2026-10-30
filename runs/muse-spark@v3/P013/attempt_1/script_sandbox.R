out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, {
    m <- 10^digits
    sign(x) * floor(abs(x) * m + 0.5) / m
  })
}

best32 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) NA_character_
    else trimws(format(v, scientific = FALSE, trim = TRUE, digits = 15))
  }, character(1), USE.NAMES = FALSE)
}

sstrip <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
}

in_path <- "/bench/data/derived/c2c/data/raw/raw_ecg.csv"

raw_df <- readr::read_csv(in_path, col_types = readr::cols(.default = readr::col_character()), na = c("", "NA"), trim_ws = FALSE, progress = FALSE)
names(raw_df) <- toupper(names(raw_df))

stopifnot(all(c("SUBJECT", "VISIT_NAM", "VISIT_NUM", "TEST_NAME", "RESULT", "UNIT", "DATE") %in% names(raw_df)))

work_raw_eg <- tibble::as_tibble(raw_df)

# Emulate PROC IMPORT type inference: numeric columns
work_raw_eg$VISIT_NUM_N <- suppressWarnings(as.numeric(trimws(work_raw_eg$VISIT_NUM)))
work_raw_eg$RESULT_N <- suppressWarnings(as.numeric(trimws(work_raw_eg$RESULT)))

# Emulate PROC IMPORT GUESSINGROWS date order decision for DATE column (ONE format for whole column)
date_raw <- trimws(as.character(work_raw_eg$DATE))
date_raw[date_raw == ""] <- NA_character_
nonmiss <- date_raw[!is.na(date_raw) & date_raw != ""]

egdt <- rep(as.Date(NA), length(date_raw))

if (length(nonmiss) > 0L && all(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", nonmiss))) {
  first_fields <- suppressWarnings(as.integer(sub("/.*$", "", nonmiss)))
  dmy <- any(first_fields > 12L, na.rm = TRUE)
  year_parts <- sub("^.*/", "", nonmiss)
  use_2digit_year <- all(nchar(year_parts) == 2L)
  if (dmy) {
    fmt <- if (use_2digit_year) "%d/%m/%y" else "%d/%m/%Y"
  } else {
    fmt <- if (use_2digit_year) "%m/%d/%y" else "%m/%d/%Y"
  }
  egdt <- as.Date(date_raw, format = fmt)
} else if (length(nonmiss) > 0L && all(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", nonmiss))) {
  egdt <- as.Date(date_raw, format = "%Y-%m-%d")
} else if (length(nonmiss) > 0L && all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", nonmiss))) {
  # Normalize month abbrev to title case for %b
  norm <- gsub("([0-9]{1,2}-)([A-Za-z]{3})(-.*)", "\\1\\2\\3", date_raw)
  norm <- vapply(norm, function(v) {
    if (is.na(v)) NA_character_
    else sub("^(-?[0-9]+-)([A-Za-z]{3})(-.*)$", "\\1", v, perl = TRUE) -> pre
    v
  }, character(1), USE.NAMES = FALSE)
  # title-case month part
  norm2 <- ifelse(is.na(date_raw), NA_character_, {
    m <- regmatches(date_raw, regexec("^([0-9]{1,2}-)([A-Za-z]{3})(-[0-9]{2,4})$", date_raw))
    vapply(m, function(mm) {
      if (length(mm) == 0L || is.na(mm[1])) NA_character_
      else {
        mon <- paste0(toupper(substr(mm[3], 1, 1)), tolower(substr(mm[3], 2, 3)))
        paste0(mm[2], mon, mm[4])
      }
    }, character(1), USE.NAMES = FALSE)
  })
  year_parts2 <- sub("^.*-", "", nonmiss)
  use_2digit_year2 <- all(nchar(year_parts2) == 2L)
  fmt2 <- if (use_2digit_year2) "%d-%b-%y" else "%d-%b-%Y"
  egdt <- as.Date(norm2, format = fmt2)
} else if (length(nonmiss) > 0L) {
  # Fallback single ISO format
  egdt <- as.Date(date_raw, format = "%Y-%m-%d")
}

# Build SDTM EG (first data step)
n <- nrow(work_raw_eg)
STUDYID <- rep("CDISC-01", n)
STUDYID <- substr(STUDYID, 1, 8)
DOMAIN <- rep("EG", n)
DOMAIN <- substr(DOMAIN, 1, 2)
SUBJECT_c <- sstrip(work_raw_eg$SUBJECT)
USUBJID <- ifelse(is.na(SUBJECT_c) | SUBJECT_c == "", "CDISC-01", paste0("CDISC-01-", SUBJECT_c))
USUBJID <- substr(USUBJID, 1, 40)
VISIT <- sstrip(work_raw_eg$VISIT_NAM)
VISITNUM <- work_raw_eg$VISIT_NUM_N
EGTEST <- sstrip(work_raw_eg$TEST_NAME)
EGTEST <- substr(EGTEST, 1, 40)
EGTESTCD <- ifelse(!is.na(EGTEST) & EGTEST == "Heart Rate", "HR",
            ifelse(!is.na(EGTEST) & EGTEST == "QT Duration", "QT",
            ifelse(!is.na(EGTEST) & EGTEST == "RR Duration", "RR", "UNKNOWN")))
EGTESTCD <- substr(EGTESTCD, 1, 8)
EGORRES <- best32(work_raw_eg$RESULT_N)
EGORRES <- substr(ifelse(is.na(EGORRES), NA_character_, EGORRES), 1, 20)
EGORRESU <- sstrip(work_raw_eg$UNIT)
EGORRESU <- substr(EGORRESU, 1, 20)
EGSTRESN <- work_raw_eg$RESULT_N
EGSTRESC <- best32(EGSTRESN)
EGSTRESC <- substr(ifelse(is.na(EGSTRESC), NA_character_, EGSTRESC), 1, 20)
EGSTRESU <- sstrip(work_raw_eg$UNIT)
EGSTRESU <- substr(EGSTRESU, 1, 20)
EGDTC <- format(egdt, "%Y-%m-%d")
EGDTC[is.na(EGDTC)] <- NA_character_
EGDTC <- substr(EGDTC, 1, 10)

sdtm_eg <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  VISITNUM = VISITNUM,
  VISIT = VISIT,
  EGTESTCD = EGTESTCD,
  EGTEST = EGTEST,
  EGORRES = EGORRES,
  EGORRESU = EGORRESU,
  EGSTRESC = EGSTRESC,
  EGSTRESN = EGSTRESN,
  EGSTRESU = EGSTRESU,
  EGDTC = EGDTC,
  stringsAsFactors = FALSE
)

# PROC SORT by USUBJID EGDTC EGTESTCD (missing first, SAS binary order uppercase before lowercase)
o <- order(sdtm_eg$USUBJID, sdtm_eg$EGDTC, sdtm_eg$EGTESTCD, na.last = FALSE, method = "radix")
sdtm_eg <- sdtm_eg[o, , drop = FALSE]

# Second data step: RETAIN + BY USUBJID -> EGSEQ
sdtm_eg$EGSEQ <- ave(seq_len(nrow(sdtm_eg)), sdtm_eg$USUBJID, FUN = seq_along)

sdtm_eg <- sdtm_eg[, c("STUDYID", "DOMAIN", "USUBJID", "EGSEQ", "VISITNUM", "VISIT", "EGTESTCD", "EGTEST", "EGORRES", "EGORRESU", "EGSTRESC", "EGSTRESN", "EGSTRESU", "EGDTC"), drop = FALSE]
rownames(sdtm_eg) <- NULL

write.csv(sdtm_eg, file.path(out, "eg.csv"), row.names = FALSE, na = "")
