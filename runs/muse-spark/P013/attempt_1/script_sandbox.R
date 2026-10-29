out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

project_path <- "/bench/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_ecg.csv")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

# SAS BEST32. put helper (strip(put(x,best32.)))
sas_best32 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return(".")
    if (is.nan(v)) return(".")
    if (is.infinite(v)) {
      if (v > 0) return("Infinity") else return("-Infinity")
    }
    # integers within exact range: plain integer notation
    if (!is.na(suppressWarnings(as.numeric(v))) && v == floor(v) && abs(v) < 1e15) {
      return(format(v, scientific = FALSE, trim = TRUE, digits = 15))
    } else {
      s <- format(v, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
      return(trimws(s))
    }
  }, character(1), USE.NAMES = FALSE)
}

sas_strip <- function(x) {
  if (is.numeric(x) || is.integer(x)) return(x)
  x <- as.character(x)
  out <- trimws(x)
  out[is.na(x)] <- NA_character_
  out
}

parse_anydtdte_vec <- function(x) {
  # returns Date vector, NA where missing/unparseable
  # x is character vector (already trimmed); handles Date/POSIX/numeric upstream
  res <- rep(as.Date(NA), length(x))
  idx <- which(!is.na(x) & trimws(x) != "" & toupper(trimws(x)) != "." & toupper(trimws(x)) != "NA")
  if (length(idx) == 0) return(res)
  vals <- trimws(x[idx])
  # first try base as.Date with many tryFormats (SAS ANYDTDTE analogue)
  fmts <- c("%Y-%m-%d","%Y/%m/%d","%Y.%m.%d","%Y%m%d",
            "%m/%d/%Y","%m-%d-%Y","%m.%d.%Y","%m/%d/%y","%m-%d-%y",
            "%d/%m/%Y","%d-%m-%Y","%d.%m.%Y",
            "%d%b%Y","%d-%b-%Y","%d %b %Y","%d%b%y","%d-%b-%y","%d %b %y",
            "%b %d, %Y","%b %d %Y","%B %d, %Y",
            "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S",
            "%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M","%Y-%m-%dT%H:%M",
            "%Y-%m-%d %H:%M","%d%b%Y %H:%M:%S","%d-%b-%Y %H:%M:%S")
  parsed <- suppressWarnings(as.Date(vals, tryFormats = fmts))
  need <- which(is.na(parsed))
  if (length(need) > 0) {
    # fallback via lubridate parse_date_time then as.Date
    lub <- suppressWarnings(lubridate::parse_date_time(vals[need],
      orders = c("ymd HMS","ymd HM","ymd","Ymd HMS","Ymd",
                 "mdy HMS","mdy HM","mdy","dmy HMS","dmy HM","dmy",
                 "bdy HMS","bdy","dbY HMS","dbY","YbD"),
      quiet = TRUE, truncated = 5))
    lub_date <- suppressWarnings(as.Date(lub))
    parsed[need] <- lub_date
  }
  res[idx] <- parsed
  res
}

# ---- proc import ----
raw_eg <- readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 1000000, progress = FALSE)
names(raw_eg) <- toupper(names(raw_eg))

# Ensure expected columns exist
# SUBJECT, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, DATE
work_raw_eg <- raw_eg

# ---- data sdtm.eg ----
# Normalize types similar to PROC IMPORT + VALIDVARNAME=UPCASE
# RESULT should be numeric; coerce if read as character
if ("RESULT" %in% names(work_raw_eg)) {
  if (!is.numeric(work_raw_eg$RESULT)) {
    work_raw_eg$RESULT <- suppressWarnings(as.numeric(as.character(work_raw_eg$RESULT)))
  }
}
if ("VISIT_NUM" %in% names(work_raw_eg)) {
  if (!is.numeric(work_raw_eg$VISIT_NUM)) {
    work_raw_eg$VISIT_NUM <- suppressWarnings(as.numeric(as.character(work_raw_eg$VISIT_NUM)))
  }
}

df <- work_raw_eg %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "EG",
    .SUBJECT_c = sas_strip(as.character(SUBJECT)),
    USUBJID = dplyr::if_else(is.na(.SUBJECT_c) | .SUBJECT_c == "",
                             "CDISC-01",
                             paste0("CDISC-01-", .SUBJECT_c)),
    VISIT = sas_strip(as.character(VISIT_NAM)),
    VISITNUM = suppressWarnings(as.numeric(VISIT_NUM)),
    EGTEST = sas_strip(as.character(TEST_NAME))
  ) %>%
  mutate(
    EGTESTCD = dplyr::case_when(
      EGTEST == "Heart Rate" ~ "HR",
      EGTEST == "QT Duration" ~ "QT",
      EGTEST == "RR Duration" ~ "RR",
      TRUE ~ "UNKNOWN"
    ),
    EGORRES = vapply(RESULT, function(v) if (is.na(v)) "." else trimws(sas_best32(v)), character(1)),
    EGORRESU = sas_strip(as.character(UNIT)),
    EGSTRESN = suppressWarnings(as.numeric(RESULT)),
    EGSTRESU = sas_strip(as.character(UNIT))
  ) %>%
  mutate(
    EGSTRESC = vapply(EGSTRESN, function(v) if (is.na(v)) "." else trimws(sas_best32(v)), character(1))
  )

# DATE handling: vtype(DATE)='C' -> input anydtdte, else SAS date numeric
date_col <- df$DATE
egdt <- rep(as.Date(NA), nrow(df))
if (inherits(date_col, "Date")) {
  egdt <- as.Date(date_col)
} else if (inherits(date_col, "POSIXt")) {
  egdt <- as.Date(date_col)
} else if (is.numeric(date_col) && !is.character(date_col)) {
  # numeric SAS date (days since 1960-01-01); if values look like SAS dates use that origin
  # PROC IMPORT numeric DATE would be SAS date value
  egdt <- suppressWarnings(as.Date(date_col, origin = "1960-01-01"))
} else {
  date_char <- as.character(date_col)
  # readr may have produced NA for empty; keep NA
  egdt <- parse_anydtdte_vec(date_char)
}
df$`_egdt` <- egdt
df$EGDTC <- ifelse(!is.na(egdt), format(egdt, "%Y-%m-%d"), NA_character_)

# Apply LENGTH truncations (SAS LENGTH statement)
substr_na <- function(x, n) {
  x <- as.character(x)
  out <- substr(x, 1, n)
  out[is.na(x)] <- NA_character_
  # SAS blank for character missing stays NA here (written as "" later)
  out
}
df <- df %>%
  mutate(
    STUDYID = substr_na(STUDYID, 8),
    DOMAIN = substr_na(DOMAIN, 2),
    USUBJID = substr_na(USUBJID, 40),
    EGTESTCD = substr_na(EGTESTCD, 8),
    EGTEST = substr_na(EGTEST, 40),
    EGORRES = substr_na(EGORRES, 20),
    EGORRESU = substr_na(EGORRESU, 20),
    EGSTRESC = substr_na(EGSTRESC, 20),
    EGSTRESU = substr_na(EGSTRESU, 20),
    EGDTC = substr_na(EGDTC, 10)
  )

# keep STUDYID DOMAIN USUBJID VISITNUM VISIT EGTESTCD EGTEST EGORRES EGORRESU EGSTRESC EGSTRESN EGSTRESU EGDTC
df1 <- df %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, EGTESTCD, EGTEST,
         EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC)

# ---- proc sort by USUBJID EGDTC EGTESTCD (SAS: missing first, byte order) ----
# Use radix ordering for SAS-like byte order, NA first
ord <- order(df1$USUBJID, df1$EGDTC, df1$EGTESTCD, method = "radix", na.last = FALSE)
df1 <- df1[ord, , drop = FALSE]

# ---- second data step: EGSEQ by USUBJID ----
df1$EGSEQ <- NA_integer_
# FIRST./LAST. processing on USUBJID
usub <- df1$USUBJID
# treat NA as its own group; SAS missing sorts first and groups together
seqn <- integer(nrow(df1))
if (nrow(df1) > 0) {
  cur <- 0L
  prev <- NA_character_
  first <- TRUE
  for (i in seq_len(nrow(df1))) {
    cur_usub <- usub[i]
    is_first <- FALSE
    if (first) {
      is_first <- TRUE
    } else {
      if (is.na(cur_usub) && is.na(prev)) is_first <- FALSE
      else if (is.na(cur_usub) || is.na(prev)) is_first <- TRUE
      else if (cur_usub != prev) is_first <- TRUE
      else is_first <- FALSE
    }
    if (is_first) cur <- 1L else cur <- cur + 1L
    seqn[i] <- cur
    prev <- cur_usub
    first <- FALSE
  }
}
df1$EGSEQ <- seqn

# retain / keep order
eg <- df1 %>%
  select(STUDYID, DOMAIN, USUBJID, EGSEQ, VISITNUM, VISIT, EGTESTCD, EGTEST,
         EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC)

# Ensure types: EGSEQ integer/numeric, VISITNUM numeric, EGSTRESN numeric
eg$EGSEQ <- as.numeric(eg$EGSEQ)
eg$VISITNUM <- as.numeric(eg$VISITNUM)
eg$EGSTRESN <- as.numeric(eg$EGSTRESN)

write.csv(eg, file.path(out, "eg.csv"), row.names = FALSE, na = "")
