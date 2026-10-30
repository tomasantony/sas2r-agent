suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(lubridate)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40

# ---- PROC IMPORT ----
raw_mh <- read_csv(file.path(project_path, "data/raw/raw_mh.csv"),
                   col_types = cols(.default = col_character()),
                   na = c("", "NA", "."), trim_ws = TRUE)
names(raw_mh) <- toupper(names(raw_mh))

# Mimic PROC IMPORT: SUBJECT read as numeric if all values numeric
subj <- raw_mh$SUBJECT
nonmiss <- subj[!is.na(subj)]
if (length(nonmiss) > 0 && all(!is.na(suppressWarnings(as.numeric(nonmiss))))) {
  num <- as.numeric(subj)
  # BEST12. style conversion
  subj_chr <- ifelse(is.na(num), "",
                     trimws(formatC(num, digits = 12, format = "g", flag = "")))
  subj_chr <- sub("\\.?0+$", "", ifelse(grepl("\\.", subj_chr), subj_chr, paste0(subj_chr, "")))
} else {
  subj_chr <- ifelse(is.na(subj), "", trimws(subj))
}

# ---- Date parsing (anydtdte. approximation, MDY preference) ----
parse_any_date <- function(x) {
  x <- trimws(x)
  res <- as.Date(rep(NA_character_, length(x)))
  ok <- !is.na(x) & x != ""
  if (any(ok)) {
    d <- suppressWarnings(parse_date_time(x[ok],
           orders = c("Ymd", "mdY", "dmY", "dbY", "bdY", "Ymd HMS", "mdY HMS", "dbY HMS"),
           quiet = TRUE))
    res[ok] <- as.Date(d)
  }
  res
}

mhdt <- parse_any_date(raw_mh$DIAGNOSIS_DATE)

catx_dash <- function(a, b) {
  ifelse(b == "", a, paste(a, b, sep = "-"))
}

mh <- tibble(
  STUDYID = "CDISC-01",
  DOMAIN  = "MH",
  USUBJID = substr(catx_dash("CDISC-01", subj_chr), 1, usubjid_length),
  MHTERM  = substr(ifelse(is.na(raw_mh$CONDITION), "", trimws(raw_mh$CONDITION)), 1, 60),
  MHSTDTC = ifelse(is.na(mhdt), "", format(mhdt, "%Y-%m-%d"))
)

# ---- PROC SORT by USUBJID MHSTDTC (stable, C-locale ordering) ----
ord <- order(mh$USUBJID, mh$MHSTDTC, method = "radix")
mh <- mh[ord, ]

# ---- MHSEQ by USUBJID ----
mh <- mh %>%
  group_by(USUBJID) %>%
  mutate(MHSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, MHSEQ, MHTERM, MHSTDTC)

write.csv(mh, file.path(out, "mh.csv"), row.names = FALSE, na = "")
cat("MH written:", nrow(mh), "rows\n")
