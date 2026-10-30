# P009_sdtm_ae.sas -> R translation
# SDTM AE mapping with AESEQ and ongoing logic

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
})

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv"
raw_ae_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv"

cat("Reading raw AE:", raw_ae_path, "\n")

raw_ae <- readr::read_csv(raw_ae_path,
  col_types = readr::cols(.default = readr::col_character()),
  guess_max = 1000000, show_col_types = FALSE)

names(raw_ae) <- toupper(names(raw_ae))
stopifnot(all(c("ID","AE_TERM","SEV","SERIOUS","RELATED","START","END") %in% names(raw_ae)))

sstrip <- function(x) {
  ifelse(is.na(x), NA_character_, stringr::str_trim(as.character(x)))
}

# SAS YEARCUTOFF default 1926: 00-26 -> 2000s, 27-99 -> 1900s
expand2yr <- function(y) {
  ifelse(is.na(y), NA_real_,
    ifelse(y < 100,
      ifelse(y <= 26, 2000 + y, 1900 + y), y))
}

mon_map <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)

# Emulate SAS ANYDTDTE with DATESTYLE=MDY (with disambiguation when month>12)
parse_anydtdte_mdy <- function(x) {
  n <- length(x)
  res <- as.Date(rep(NA_character_, n))
  for (i in seq_len(n)) {
    v <- x[i]
    if (is.na(v)) next
    t <- stringr::str_trim(as.character(v))
    if (is.na(t) || t == "" || t == ".") next
    # strip time portion for date detection but keep original for time-strip
    # pure numeric string (SAS date number or YYYYMMDD)
    if (grepl("^[0-9]+(\\.[0-9]+)?$", t)) {
      ti <- strsplit(t, ".", fixed=TRUE)[[1]][1]
      if (grepl("^[0-9]{8}$", ti)) {
        yr4 <- suppressWarnings(as.numeric(substr(ti,1,4)))
        if (!is.na(yr4) && yr4 >= 1800 && yr4 <= 2200) {
          d <- as.Date(ti, format="%Y%m%d")
          if (!is.na(d)) { res[i] <- d; next }
        }
      }
      num <- suppressWarnings(as.numeric(t))
      if (!is.na(num)) {
        # SAS date number: days since 1960-01-01 (floor like SAS)
        res[i] <- as.Date(floor(num), origin="1960-01-01")
        next
      } else next
    }
    tu <- toupper(t)
    # month-name present -> DATE9-like (DDMMMYYYY, DD-MMM-YYYY, MMM DD YYYY, etc.)
    if (grepl("[A-Z]", tu)) {
      # remove commas periods dashes slashes spaces for compact form, keep letters/digits
      # extract month abbreviation
      m <- regexpr("(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)", tu)
      if (m > 0) {
        mon <- substr(tu, m, m+2)
        mno <- unname(mon_map[mon])
        nums <- regmatches(tu, gregexpr("[0-9]+", tu))[[1]]
        nums <- suppressWarnings(as.numeric(nums))
        nums <- nums[!is.na(nums)]
        if (length(nums) >= 2) {
          # take day = first number that is <=31, year = number with 2-4 digits that looks like year
          # SAS: for DDMMMYY(YY) day is before month or after? Handle both orders:
          # find year as last number (2-4 digits), day as the other number
          yr <- nums[length(nums)]
          dy <- nums[1]
          # if 3 numbers (e.g. time included) take first and second? year is 4-digit one
          idx4 <- which(nums >= 1000 & nums <= 2200)
          if (length(idx4) > 0) {
            yr <- nums[idx4[1]]
            others <- nums[-idx4[1]]
            # day is number nearest month? use first other <=31
            cand <- others[others >= 1 & others <= 31]
            if (length(cand) > 0) dy <- cand[1] else dy <- others[1]
          }
          yr <- expand2yr(yr)
          dy <- as.integer(dy)
          if (!is.na(yr) && !is.na(dy) && !is.na(mno) && dy >= 1 && dy <= 31) {
            d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yr, mno, dy), format="%Y-%m-%d"))
            # validate no overflow (e.g. Feb 30 -> NA, SAS would be missing)
            if (!is.na(d)) {
              # verify components round-trip
              lt <- as.POSIXlt(d)
              if (lt$year+1900==yr && lt$mon+1==mno && lt$mday==dy) { res[i] <- d; next }
            }
          }
        } else if (length(nums)==1 && nchar(tu)>=4) {
          # year only? skip -> NA
        }
      }
      # month-name but unparseable -> missing (SAS would be missing)
      res[i] <- as.Date(NA); next
    }
    # isolate date part before space or T (time)
    td <- strsplit(t, " ", fixed=TRUE)[[1]][1]
    td <- strsplit(td, "T", fixed=TRUE)[[1]][1]
    td <- strsplit(td, "t", fixed=TRUE)[[1]][1]
    # year-first ISO with - or / : YYYY-MM-DD or YYYY/MM/DD
    if (grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}$", td)) {
      tdn <- gsub("/", "-", td, fixed=TRUE)
      parts <- strsplit(tdn, "-", fixed=TRUE)[[1]]
      y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
      if (!any(is.na(c(y,m2,d2)))) {
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
        if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
      }
      res[i] <- as.Date(NA); next
    }
    # YYYYMMDD with dashes already handled; compact YYYYMMDD handled above
    # slash dates
    if (grepl("/", td, fixed=TRUE)) {
      parts <- strsplit(td, "/", fixed=TRUE)[[1]]
      if (length(parts)==3) {
        # year first? (YYYY/M/D)
        if (nchar(parts[1])==4 && grepl("^[0-9]{4}$", parts[1])) {
          y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
          y <- expand2yr(y)
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
          res[i] <- as.Date(NA); next
        }
        m2 <- suppressWarnings(as.numeric(parts[1])); d2 <- suppressWarnings(as.numeric(parts[2])); y <- suppressWarnings(as.numeric(parts[3]))
        if (any(is.na(c(m2,d2,y)))) { res[i] <- as.Date(NA); next }
        y <- expand2yr(y)
        # SAS ANYDTDTE MDY disambiguation: if month>12 and day<=12, swap
        if (!is.na(m2) && !is.na(d2) && m2>12 && d2<=12) { tmp<-m2; m2<-d2; d2<-tmp }
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
        if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
        res[i] <- as.Date(NA); next
      } else { res[i] <- as.Date(NA); next }
    }
    # dash numeric dates: YYYY-MM-DD already handled; remaining MDY with dashes or DD-MON handled above
    if (grepl("-", td, fixed=TRUE)) {
      parts <- strsplit(td, "-", fixed=TRUE)[[1]]
      if (length(parts)==3 && all(grepl("^[0-9]+$", parts))) {
        if (nchar(parts[1])==4) {
          y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
          y <- expand2yr(y)
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
          res[i] <- as.Date(NA); next
        } else {
          m2 <- suppressWarnings(as.numeric(parts[1])); d2 <- suppressWarnings(as.numeric(parts[2])); y <- suppressWarnings(as.numeric(parts[3]))
          if (any(is.na(c(m2,d2,y)))) { res[i] <- as.Date(NA); next }
          y <- expand2yr(y)
          if (!is.na(m2) && !is.na(d2) && m2>12 && d2<=12) { tmp<-m2; m2<-d2; d2<-tmp }
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
          res[i] <- as.Date(NA); next
        }
      } else { res[i] <- as.Date(NA); next }
    }
    # compact YYYYMMDD already handled; dot dates DD.MM.YYYY
    if (grepl("\\.", td)) {
      parts <- strsplit(td, ".", fixed=TRUE)[[1]]
      parts <- parts[parts!=""]
      if (length(parts)==3 && all(grepl("^[0-9]+$", parts))) {
        if (nchar(parts[1])==4) {
          y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
        } else {
          # assume DMY? with MDY style month first? For dots SAS MDY? keep MDY with swap
          m2 <- suppressWarnings(as.numeric(parts[1])); d2 <- suppressWarnings(as.numeric(parts[2])); y <- suppressWarnings(as.numeric(parts[3]))
          if (!is.na(m2) && !is.na(d2) && m2>12 && d2<=12) { tmp<-m2; m2<-d2; d2<-tmp }
        }
        y <- expand2yr(y)
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
        if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
      }
      res[i] <- as.Date(NA); next
    }
    # fallback: 8-digit compact
    tc <- gsub("[^0-9]", "", td)
    if (nchar(tc)==8) {
      yr4 <- suppressWarnings(as.numeric(substr(tc,1,4)))
      if (!is.na(yr4) && yr4>=1800 && yr4<=2200) {
        d <- as.Date(tc, format="%Y%m%d"); if (!is.na(d)) { res[i] <- d; next }
      }
    }
    res[i] <- as.Date(NA)
  }
  res
}

ae_mapped <- raw_ae %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "AE",
    .ID_trim = sstrip(ID),
    USUBJID = ifelse(is.na(.ID_trim) | .ID_trim == "", STUDYID, paste0(STUDYID, "-", .ID_trim)),
    .AETERM_raw = sstrip(AE_TERM),
    AETERM = substr(ifelse(is.na(.AETERM_raw), NA_character_, .AETERM_raw), 1, 60),
    AEDECOD = substr(ifelse(is.na(AETERM), NA_character_, toupper(AETERM)), 1, 60),
    .SEV_trim = sstrip(SEV),
    AESEV = substr(ifelse(is.na(.SEV_trim), NA_character_, toupper(.SEV_trim)), 1, 10),
    .SER_trim = sstrip(SERIOUS),
    .SER_up = ifelse(is.na(.SER_trim), "", toupper(.SER_trim)),
    AESER = ifelse(.SER_up == "Y", "Y", "N"),
    .REL_trim = sstrip(RELATED),
    .REL_up = ifelse(is.na(.REL_trim), "", toupper(.REL_trim)),
    AEREL_raw = ifelse(.REL_up == "Y", "RELATED",
                ifelse(.REL_up == "N", "NOT RELATED", "UNKNOWN")),
    AEREL = substr(AEREL_raw, 1, 15)
  )

start_chr <- sstrip(ae_mapped$START)
end_chr <- sstrip(ae_mapped$END)

start_date <- parse_anydtdte_mdy(start_chr)
end_date <- parse_anydtdte_mdy(end_chr)

ae_mapped$AESTDTC <- ifelse(is.na(start_date), NA_character_, format(start_date, "%Y-%m-%d"))
ae_mapped$AEENDTC <- ifelse(is.na(end_date), NA_character_, format(end_date, "%Y-%m-%d"))
ae_mapped$AEOUT_raw <- ifelse(!is.na(end_date), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")
ae_mapped$AEOUT <- substr(ae_mapped$AEOUT_raw, 1, 30)

ae_mapped$STUDYID <- substr(ae_mapped$STUDYID, 1, 8)
ae_mapped$DOMAIN <- substr(ae_mapped$DOMAIN, 1, 2)
ae_mapped$USUBJID <- substr(ae_mapped$USUBJID, 1, 40)
ae_mapped$AESER <- substr(ae_mapped$AESER, 1, 1)

ae_mapped <- ae_mapped %>%
  select(STUDYID, DOMAIN, USUBJID, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)

# PROC SORT by USUBJID AESTDTC (missing first, SAS character order); stable with original order tie-break
ae_mapped$.seq <- seq_len(nrow(ae_mapped))
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, ae_mapped$.seq, na.last = FALSE, method = "radix")
ae_sorted <- ae_mapped[ord, , drop = FALSE]
ae_sorted$.seq <- NULL

ae_sorted$AESEQ <- ave(seq_len(nrow(ae_sorted)), ae_sorted$USUBJID,
  FUN = function(x) seq_along(x))

sdtm_ae <- ae_sorted[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]

sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)

cat("N AE records:", nrow(sdtm_ae), "\n")

write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

audit_title <- "AE Domain Audit (Adverse Events with Ongoing Logic)"
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit12 <- head(sdtm_ae[, audit_vars, drop = FALSE], 12)

write.csv(audit12, file.path(out, "P009.csv"), row.names = FALSE, na = "")

df_print <- data.frame(Obs = seq_len(nrow(audit12)), audit12, check.names = FALSE, stringsAsFactors = FALSE)
df_disp <- df_print
for (nm in names(df_disp)) {
  v <- df_disp[[nm]]
  v <- ifelse(is.na(v), "", as.character(v))
  df_disp[[nm]] <- v
}
widths <- sapply(names(df_disp), function(nm) max(nchar(c(nm, df_disp[[nm]])), na.rm = TRUE))
fmt_row <- function(vals, w) {
  paste(mapply(function(v, ww) sprintf(paste0("%-", ww, "s"), v), vals, w), collapse = "  ")
}
lines <- c(audit_title, "",
  fmt_row(names(df_disp), widths),
  fmt_row(sapply(widths, function(w) paste(rep("-", w), collapse = "")), widths)
)
for (i in seq_len(nrow(df_disp))) {
  lines <- c(lines, fmt_row(as.character(df_disp[i, ]), widths))
}
lines <- c(lines, "", paste0("N = ", nrow(audit12), " rows shown (first 12 of ", nrow(sdtm_ae), ")"))

txt_path <- file.path(out, "P009.txt")
writeLines(lines, txt_path)
writeLines(lines, file.path(out, "P009.lst"))

cat("Wrote:", file.path(out, "ae.csv"), "\n")
cat("Wrote:", txt_path, "and P009.csv\n")
