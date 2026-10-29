# P012_sdtm_ds.sas -> R translation
# Creates SDTM DS domain from raw_demog.csv and reproduces PROC PRINT listing
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(readr))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))

raw_path <- "/bench/data/derived/c2c/data/raw/raw_demog.csv"

# ---- SAS helpers ----
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  sign_x <- sign(x)
  z <- abs(x) * mult + 0.5 + 1e-10
  trunc(z) / mult * sign_x
}
sas_strip <- function(x) {
  # SAS STRIP: remove leading/trailing blanks; NA stays NA
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(as.character(x)))
}
sas_trunc <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(as.character(x), 1, len))
}
sas_catx <- function(delim, a, b) {
  # CATX("-", STUDYID, SUBJ_ID): strip, skip missing/blank, join
  a_c <- sas_strip(as.character(a))
  b_c <- sas_strip(as.character(b))
  a_miss <- is.na(a_c) | a_c == ""
  b_miss <- is.na(b_c) | b_c == ""
  res <- ifelse(a_miss & b_miss, NA_character_,
         ifelse(a_miss, b_c,
         ifelse(b_miss, a_c, paste0(a_c, delim, b_c))))
  res
}

parse_one_anydtdte <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  s <- trimws(as.character(s))
  if (s == "" || toupper(s) == "NA") return(as.Date(NA))
  if (s == ".") return(as.Date(NA))
  orders <- c("ymd", "Ymd", "ymd HMS", "ymd HM", "ymd H",
              "mdy", "dmy", "Ymd HMS", "mdy HMS", "dmy HMS",
              "BdY", "dBY", "bY", "BY", "ym", "my",
              "YmdT HMS", "YmdT HM", "Ymd HM")
  dt <- suppressWarnings(lubridate::parse_date_time(s, orders = orders, quiet = TRUE, truncated = 0))
  if (length(dt) == 1 && !is.na(dt)) return(as.Date(dt))
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%Y%m%d",
            "%m/%d/%Y", "%m-%d-%Y", "%m/%d/%y", "%m-%d-%y",
            "%d/%m/%Y", "%d-%m-%Y",
            "%d%b%Y", "%d-%b-%Y", "%d %b %Y",
            "%d%B%Y", "%d-%B-%Y", "%d %B %Y",
            "%b %d %Y", "%b %d, %Y", "%B %d, %Y", "%B %d %Y",
            "%Y-%b-%d", "%d-%b-%y", "%d%b%y",
            "%Y.%m.%d", "%d.%m.%Y", "%m.%d.%Y",
            "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d)) return(d)
    # try upper-cased month variant
    d2 <- suppressWarnings(as.Date(toupper(s), format = f))
    if (!is.na(d2)) return(d2)
  }
  s2 <- gsub(",", " ", s, fixed = FALSE)
  s2 <- gsub("  +", " ", trimws(s2))
  if (s2 != s) {
    dt2 <- suppressWarnings(lubridate::parse_date_time(s2, orders = orders, quiet = TRUE))
    if (length(dt2) == 1 && !is.na(dt2)) return(as.Date(dt2))
  }
  if (grepl("^[0-9]+$", s)) {
    num <- suppressWarnings(as.numeric(s))
    if (!is.na(num)) {
      if (nchar(s) == 8) {
        d8 <- suppressWarnings(as.Date(s, format = "%Y%m%d"))
        if (!is.na(d8)) return(d8)
      }
      if (nchar(s) <= 5 && num >= 0 && num <= 60000) {
        return(as.Date("1960-01-01") + num)
      }
      if (nchar(s) == 8) {
        d8 <- suppressWarnings(as.Date(s, format = "%Y%m%d"))
        if (!is.na(d8)) return(d8)
      }
    }
  }
  return(as.Date(NA))
}

parse_anydtdte_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, c("POSIXct", "POSIXlt", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x) || is.integer(x)) {
    xn <- as.numeric(x)
    return(as.Date("1960-01-01") + xn)
  }
  xc <- as.character(x)
  # vapply to Date numeric then convert
  dnum <- vapply(xc, function(s) {
    if (is.na(s)) return(NA_real_)
    d <- parse_one_anydtdte(s)
    as.numeric(d)
  }, numeric(1))
  as.Date(dnum, origin = "1970-01-01")
}

find_col <- function(df, candidates) {
  un <- toupper(names(df))
  for (cand in candidates) {
    idx <- which(un == cand)
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}

message("Reading raw_demog.csv ...")
raw_guess <- readr::read_csv(raw_path, guess_max = 10000, trim_ws = TRUE,
                             show_col_types = FALSE, na = c("", "NA"))
names(raw_guess) <- toupper(names(raw_guess))

subj_col   <- find_col(raw_guess, c("SUBJ_ID", "SUBJID", "SUBJ", "SUBJECT_ID", "USUBJID"))
disp_col   <- find_col(raw_guess, c("DISPOSITION", "DISP"))
reason_col <- find_col(raw_guess, c("DISPOSITION_REASON", "DISP_REASON", "DISPOSITIONREASON", "DSTERM", "REASON"))
enddat_col <- find_col(raw_guess, c("STUDY_END_DAT", "STUDY_END_DATE", "END_DAT", "DSSTDTC", "STUDYENDDAT", "STUDY_END_DT"))

if (is.na(subj_col) || is.na(disp_col) || is.na(reason_col) || is.na(enddat_col)) {
  stop(paste0("Required column not found. Available: ", paste(names(raw_guess), collapse = ", ")))
}

subj_raw   <- raw_guess[[subj_col]]
disp_raw   <- as.character(raw_guess[[disp_col]])
reason_raw <- as.character(raw_guess[[reason_col]])
end_raw    <- raw_guess[[enddat_col]]

n <- nrow(raw_guess)

# ---- data sdtm.ds ----
STUDYID <- rep("CDISC-01", n)
DOMAIN  <- rep("DS", n)

# USUBJID = catx("-", STUDYID, SUBJ_ID) length $40
USUBJID <- sas_catx("-", STUDYID, as.character(subj_raw))
USUBJID <- sas_trunc(USUBJID, 40)

DSSEQ <- rep(1, n)

DSCAT  <- rep("DISPOSITION EVENT", n)
DSSCAT <- rep("STUDY PARTICIPATION", n)
DSCAT  <- sas_trunc(DSCAT, 40)
DSSCAT <- sas_trunc(DSSCAT, 40)

DSTERM <- sas_upcase(sas_strip(reason_raw))
DSTERM <- sas_trunc(DSTERM, 80)
DSDECOD <- sas_trunc(DSTERM, 80)

disp_up <- sas_upcase(sas_strip(disp_raw))
is_screen <- !is.na(disp_up) & disp_up == "SCREEN FAILURE"
EPOCH <- ifelse(is_screen, "SCREENING", "FOLLOW-UP")
EPOCH <- sas_trunc(EPOCH, 20)

# _dsdt handling: if character -> input(anydtdte.) else numeric SAS date
dsdt_date <- parse_anydtdte_vec(end_raw)
DSSTDTC <- ifelse(is.na(dsdt_date), NA_character_, format(dsdt_date, "%Y-%m-%d"))
DSSTDTC <- sas_trunc(DSSTDTC, 10)

STUDYID <- sas_trunc(STUDYID, 8)
DOMAIN  <- sas_trunc(DOMAIN, 2)

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

# ---- proc sort by USUBJID DSSEQ (missing first, uppercase before lowercase) ----
ord <- order(sdtm_ds$USUBJID, sdtm_ds$DSSEQ, na.last = FALSE, method = "radix")
sdtm_ds <- sdtm_ds[ord, , drop = FALSE]

message(paste0("DS rows: ", nrow(sdtm_ds)))

# ---- write dataset ds ----
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# ---- proc print obs=12 title DS Domain Audit ----
nprint <- min(12L, nrow(sdtm_ds))
print_df <- sdtm_ds[seq_len(nprint), , drop = FALSE]
print_df_csv <- data.frame(Obs = seq_len(nprint), print_df, stringsAsFactors = FALSE, check.names = FALSE)

# CSV of table body
write.csv(print_df_csv, file.path(out, "P012.csv"), row.names = FALSE, na = "")

# Text rendering
options(width = 10000)
txt_path <- file.path(out, "P012.txt")
lst_path <- file.path(out, "P012.lst")
con <- file(txt_path, open = "wt")
writeLines("DS Domain Audit", con)
writeLines("", con)
hdr <- capture.output(print(print_df_csv, row.names = FALSE, right = TRUE, na.print = ""))
writeLines(hdr, con)
writeLines("", con)
writeLines(paste0("NOTE: ", nprint, " of ", nrow(sdtm_ds), " observations displayed (OBS=12)."), con)
close(con)
# exact copy for expected .lst name
file.copy(txt_path, lst_path, overwrite = TRUE)

message("Done.")
