out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_mh.csv")

cat("Reading:", raw_path, "\n")
raw_mh <- utils::read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE,
                          na.strings = c("", "NA"), strip.white = FALSE)
names(raw_mh) <- toupper(names(raw_mh))
cat("raw_mh rows:", nrow(raw_mh), " cols:", paste(names(raw_mh), collapse=","), "\n")

# Helper: SAS ANYDTDTE. -> R Date (character input)
sas_anydtdte_to_date <- function(x) {
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi)) next
    xi_t <- trimws(as.character(xi))
    if (xi_t == "" || xi_t == "." || toupper(xi_t) == "NA") next
    # SAS anydtdte can read ISO T separator; normalize to space
    xi_t2 <- gsub("T", " ", xi_t, fixed = FALSE)
    dt <- suppressWarnings(lubridate::parse_date_time(xi_t2,
      orders = c("ymd HMS","ymd HM","ymd",
                 "Ymd HMS","Ymd HM","Ymd",
                 "mdy HMS","mdy HM","mdy",
                 "dmy HMS","dmy HM","dmy",
                 "myd HMS","myd HM","myd",
                 "dym HMS","dym HM","dym",
                 "dbY HMS","dbY HM","dbY",
                 "dBY HMS","dBY HM","dBY",
                 "Ybd HMS","Ybd HM","Ybd",
                 "bdY HMS","bdY HM","bdY",
                 "bdy HMS","bdy HM","bdy",
                 "YbD HMS","YbD HM","YbD",
                 "ymd HMS p","mdy HMS p","dmy HMS p"),
      tz = "UTC", truncated = 0, quiet = TRUE))
    if (length(dt) == 0 || all(is.na(dt))) {
      # fallback: pure digits -> try SAS numeric date
      if (grepl("^[0-9]+$", xi_t)) {
        num <- suppressWarnings(as.numeric(xi_t))
        if (!is.na(num)) {
          res[i] <- as.Date(num, origin = "1960-01-01")
          next
        }
      }
      next
    }
    if (!is.na(dt[1])) {
      res[i] <- as.Date(dt[1], tz = "UTC")
    } else {
      if (grepl("^[0-9]+$", xi_t)) {
        num <- suppressWarnings(as.numeric(xi_t))
        if (!is.na(num)) res[i] <- as.Date(num, origin = "1960-01-01")
      }
    }
  }
  res
}

# ---- data sdtm.mh; ----
n <- nrow(raw_mh)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("MH", n)

# SUBJECT -> USUBJID via CATX('-', STUDYID, strip(SUBJECT))
subj_raw <- if ("SUBJECT" %in% names(raw_mh)) raw_mh[["SUBJECT"]] else rep(NA_character_, n)
subj_chr <- as.character(subj_raw)
subj_strip <- trimws(subj_chr)
subj_strip[is.na(subj_chr)] <- NA_character_
subj_strip[subj_strip == ""] <- NA_character_
USUBJID <- ifelse(is.na(subj_strip), STUDYID, paste0(STUDYID, "-", subj_strip))
USUBJID <- substr(USUBJID, 1, 40)

# CONDITION -> MHTERM = strip(CONDITION), length $60
cond_raw <- if ("CONDITION" %in% names(raw_mh)) raw_mh[["CONDITION"]] else rep(NA_character_, n)
cond_chr <- as.character(cond_raw)
MHTERM <- trimws(cond_chr)
MHTERM[is.na(cond_chr)] <- NA_character_
# SAS strip of "" stays missing; trimws("")=="" -> set to NA to write as ""
MHTERM[!is.na(MHTERM) & MHTERM == ""] <- NA_character_
MHTERM <- substr(MHTERM, 1, 60)

# DIAGNOSIS_DATE -> _mhdt -> MHSTDTC (is8601da. = YYYY-MM-DD)
dd <- if ("DIAGNOSIS_DATE" %in% names(raw_mh)) raw_mh[["DIAGNOSIS_DATE"]] else rep(NA, n)
mh_date <- rep(as.Date(NA), n)
if (inherits(dd, "Date")) {
  mh_date <- dd
} else if (inherits(dd, "POSIXct") || inherits(dd, "POSIXlt")) {
  mh_date <- as.Date(dd, tz = "UTC")
} else if (is.numeric(dd)) {
  # vtype = N : _mhdt = DIAGNOSIS_DATE (SAS date numeric)
  mh_date <- as.Date(dd, origin = "1960-01-01")
} else {
  # vtype = C : _mhdt = input(strip(DIAGNOSIS_DATE), anydtdte.)
  dd_chr <- as.character(dd)
  mh_date <- sas_anydtdte_to_date(dd_chr)
}
MHSTDTC <- ifelse(is.na(mh_date), NA_character_, format(mh_date, "%Y-%m-%d"))
MHSTDTC <- substr(MHSTDTC, 1, 10)
MHSTDTC[!is.na(MHSTDTC) & MHSTDTC == ""] <- NA_character_

mh <- data.frame(STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
                 MHTERM = MHTERM, MHSTDTC = MHSTDTC, stringsAsFactors = FALSE)

# ---- proc sort by USUBJID MHSTDTC (missing first, ASCII: uppercase before lowercase) ----
ord <- order(mh$USUBJID, mh$MHSTDTC, method = "radix", na.last = FALSE)
mh <- mh[ord, , drop = FALSE]
rownames(mh) <- NULL

# ---- second data step: retain + by USUBJID -> MHSEQ ----
MHSEQ <- integer(nrow(mh))
seqn <- 0L
prev <- NA_character_
for (i in seq_len(nrow(mh))) {
  cur <- mh$USUBJID[i]
  is_first <- FALSE
  if (i == 1L) {
    is_first <- TRUE
  } else if (is.na(cur) && is.na(prev)) {
    is_first <- FALSE
  } else if (is.na(cur) != is.na(prev)) {
    is_first <- TRUE
  } else if (!is.na(cur) && !is.na(prev) && cur != prev) {
    is_first <- TRUE
  } else {
    is_first <- FALSE
  }
  if (is_first) seqn <- 1L else seqn <- seqn + 1L
  MHSEQ[i] <- seqn
  prev <- cur
}
mh$MHSEQ <- MHSEQ

# keep STUDYID DOMAIN USUBJID MHSEQ MHTERM MHSTDTC
mh <- mh[, c("STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHSTDTC")]

cat("mh rows:", nrow(mh), "\n")
write.csv(mh, file.path(out, "mh.csv"), row.names = FALSE, na = "")
cat("Wrote", file.path(out, "mh.csv"), "\n")
