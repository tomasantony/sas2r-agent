```r
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

out <- Sys.getenv("OUT_DIR")
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40

# ---- PROC IMPORT (read as character, then mimic type guessing) ----
raw_eg <- read_csv(file.path(project_path, "data/raw/raw_ecg.csv"),
                   col_types = cols(.default = col_character()),
                   na = c("", "NA"), trim_ws = TRUE)
names(raw_eg) <- toupper(names(raw_eg))

is_all_numeric <- function(x) {
  nn <- x[!is.na(x) & x != ""]
  length(nn) > 0 && all(!is.na(suppressWarnings(as.numeric(nn))))
}

# SAS BEST32. style formatting
best32 <- function(x) {
  res <- ifelse(is.na(x), ".",
                trimws(formatC(x, digits = 15, format = "g", flag = "")))
  res <- sub("e\\+0*", "E", res)
  res <- sub("e-0*", "E-", res)
  res
}

# character from possibly-numeric column (strip(put(var,best12.)) behavior)
char_from <- function(x) {
  if (is_all_numeric(x)) {
    v <- suppressWarnings(as.numeric(x))
    ifelse(is.na(v), ".", best32(v))
  } else {
    ifelse(is.na(x), "", trimws(x))
  }
}

trunc_len <- function(x, n) substr(x, 1, n)

parse_anydt <- function(x) {
  x <- trimws(x)
  d <- suppressWarnings(as.Date(parse_date_time(x,
                orders = c("Ymd", "mdY", "dmY", "dbY", "Ymd HMS", "mdY HMS"),
                quiet = TRUE)))
  d
}

eg <- raw_eg %>%
  mutate(
    STUDYID  = "CDISC-01",
    DOMAIN   = "EG",
    .subj    = char_from(SUBJECT),
    USUBJID  = trunc_len(ifelse(.subj == "", STUDYID, paste(STUDYID, .subj, sep = "-")),
                         usubjid_length),
    VISIT    = char_from(VISIT_NAM),
    VISITNUM = suppressWarnings(as.numeric(VISIT_NUM)),
    EGTEST   = trunc_len(char_from(TEST_NAME), 40),
    EGTESTCD = case_when(
      EGTEST == "Heart Rate"  ~ "HR",
      EGTEST == "QT Duration" ~ "QT",
      EGTEST == "RR Duration" ~ "RR",
      TRUE ~ "UNKNOWN"),
    .res     = suppressWarnings(as.numeric(RESULT)),
    EGORRES  = trunc_len(best32(.res), 20),
    EGORRESU = trunc_len(char_from(UNIT), 20),
    EGSTRESN = .res,
    EGSTRESC = trunc_len(best32(.res), 20),
    EGSTRESU = trunc_len(char_from(UNIT), 20),
    .egdt    = parse_anydt(DATE),
    EGDTC    = ifelse(is.na(.egdt), "", format(.egdt, "%Y-%m-%d"))
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, EGTESTCD, EGTEST,
         EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC)

# ---- PROC SORT by USUBJID EGDTC EGTESTCD (stable, C-locale ordering) ----
ord <- order(enc2utf8(eg$USUBJID), eg$EGDTC, eg$EGTESTCD, method = "radix")
eg <- eg[ord, ]

# ---- EGSEQ by USUBJID ----
eg <- eg %>%
  group_by(USUBJID) %>%
  mutate(EGSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EGSEQ, VISITNUM, VISIT, EGTESTCD, EGTEST,
         EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC)

write.csv(as.data.frame(eg), file.path(out, "eg.csv"), row.names = FALSE, na = "")
message("EG written: ", nrow(eg), " rows")
```