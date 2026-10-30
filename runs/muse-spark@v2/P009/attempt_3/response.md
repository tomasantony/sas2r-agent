Fixing the AE date derivations and sort order to match SAS — rebuilding the mapping.```r
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

# SAS default YEARCUTOFF=1940 -> window 1940-2039 : 00-39 => 2000s, 40-99 => 1900s
expand2yr_1940 <- function(y) {
  ifelse(is.na(y), NA_real_,
    ifelse(y < 100,
      ifelse(y < 40, 2000 + y, 1900 + y), y))
}

mon_map <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)

# Strict SAS ANYDTDTE with DATESTYLE=MDY (single fixed order, no swap)
# - month-name dates resolved by name position (unambiguous)
# - YYYY-first dates (YYYY-MM-DD, YYYY/MM/DD, YYYYMMDD)
# - slash/dash/dot numeric dates strictly MDY (m=part1,d=part2,y=part3); invalid -> missing
# - compact 6/8 digit numeric dates as MMDDYY / MMDDYYYY (MDY)
parse_anydtdte_mdy_strict <- function(x) {
  n <- length(x)
  res <- as.Date(rep(NA_character_, n))
  for (i in seq_len(n)) {
    v <- x[i]
    if (is.na(v)) next
    t <- stringr::str_trim(as.character(v))
    if (is.na(t) || t == "" || t == ".") next
    tu <- toupper(t)
    # month name present -> parse by name position
    if (grepl("(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)", tu)) {
      m <- regexpr("(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)", tu)
      mon <- substr(tu, m, m+2)
      mno <- unname(mon_map[mon])
      nums <- regmatches(tu, gregexpr("[0-9]+", tu))[[1]]
      nums <- suppressWarnings(as.numeric(nums))
      nums <- nums[!is.na(nums)]
      if (length(nums) >= 2) {
        # year = 4-digit number if present else last number; day = the other number <=31
        idx4 <- which(nums >= 1000 & nums <= 2200)
        if (length(idx4) > 0) {
          yr <- nums[idx4[1]]
          others <- nums[-idx4[1]]
          cand <- others[others >= 1 & others <= 31]
          if (length(cand) > 0) dy <- cand[1] else dy <- others[1]
        } else {
          yr <- nums[length(nums)]
          dy <- nums[1]
          if (length(nums) >= 3) {
            # e.g. time fragments included; pick first <=31 as day
            cand <- nums[nums >= 1 & nums <= 31]
            # day is number adjacent to month; approximate with first small number
            if (length(cand) > 0) dy <- cand[1]
          }
        }
        yr <- expand2yr_1940(yr)
        dy <- as.integer(dy)
        if (!is.na(yr) && !is.na(dy) && !is.na(mno) && dy >= 1 && dy <= 31) {
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yr, mno, dy), format="%Y-%m-%d"))
          if (!is.na(d)) {
            lt <- as.POSIXlt(d)
            if (lt$year+1900==yr && lt$mon+1==mno && lt$mday==dy) { res[i] <- d; next }
          }
        }
      }
      res[i] <- as.Date(NA); next
    }
    # isolate date part before blank or T (time)
    td <- strsplit(t, " ", fixed=TRUE)[[1]][1]
    td <- strsplit(td, "T", fixed=TRUE)[[1]][1]
    td <- strsplit(td, "t", fixed=TRUE)[[1]][1]
    # strip trailing time attached with colon without blank, e.g. 01/02/2020:00:00
    # keep date portion only: for slash/dash/dot forms handle below via year split on ":"
    # year-first ISO with - or / : YYYY-MM-DD or YYYY/MM/DD
    if (grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}$", td) ||
        grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}[:].*$", td)) {
      # cut off any :time suffix
      td2 <- strsplit(td, ":", fixed=TRUE)[[1]][1]
      tdn <- gsub("/", "-", td2, fixed=TRUE)
      parts <- strsplit(tdn, "-", fixed=TRUE)[[1]]
      if (length(parts)==3) {
        y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
        y <- expand2yr_1940(y)
        if (!any(is.na(c(y,m2,d2)))) {
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
        }
      }
      res[i] <- as.Date(NA); next
    }
    # slash dates strictly MDY
    if (grepl("/", td, fixed=TRUE)) {
      # remove :time suffix if attached
      td2 <- strsplit(td, ":", fixed=TRUE)[[1]][1]
      parts <- strsplit(td2, "/", fixed=TRUE)[[1]]
      if (length(parts)==3) {
        if (nchar(parts[1])==4 && grepl("^[0-9]{4}$", parts[1])) {
          y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
          y <- expand2yr_1940(y)
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
          res[i] <- as.Date(NA); next
        }
        m2 <- suppressWarnings(as.numeric(parts[1])); d2 <- suppressWarnings(as.numeric(parts[2]))
        # year part may contain trailing time already stripped; take leading digits
        ystr <- regmatches(parts[3], regexpr("^[0-9]+", parts[3]))
        if (length(ystr)==0 || is.na(ystr)) { res[i] <- as.Date(NA); next }
        y <- suppressWarnings(as.numeric(ystr))
        if (any(is.na(c(m2,d2,y)))) { res[i] <- as.Date(NA); next }
        y <- expand2yr_1940(y)
        # strict MDY: no swap; invalid month/day -> missing
        if (is.na(m2)||is.na(d2)||m2<1||m2>12||d2<1||d2>31) { res[i] <- as.Date(NA); next }
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
        if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
        res[i] <- as.Date(NA); next
      } else { res[i] <- as.Date(NA); next }
    }
    # dash numeric dates (no month name, already excluded): strictly MDY
    if (grepl("-", td, fixed=TRUE)) {
      td2 <- strsplit(td, ":", fixed=TRUE)[[1]][1]
      parts <- strsplit(td2, "-", fixed=TRUE)[[1]]
      if (length(parts)==3 && all(grepl("^[0-9]+$", parts))) {
        if (nchar(parts[1])==4) {
          y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
          y <- expand2yr_1940(y)
          if (!is.na(m2)&&!is.na(d2)&&(m2<1||m2>12||d2<1||d2>31)) { res[i] <- as.Date(NA); next }
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
          res[i] <- as.Date(NA); next
        } else {
          m2 <- suppressWarnings(as.numeric(parts[1])); d2 <- suppressWarnings(as.numeric(parts[2])); y <- suppressWarnings(as.numeric(parts[3]))
          if (any(is.na(c(m2,d2,y)))) { res[i] <- as.Date(NA); next }
          y <- expand2yr_1940(y)
          if (m2<1||m2>12||d2<1||d2>31) { res[i] <- as.Date(NA); next }
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
          res[i] <- as.Date(NA); next
        }
      } else { res[i] <- as.Date(NA); next }
    }
    # dot numeric dates strictly MDY
    if (grepl("\\.", td)) {
      td2 <- strsplit(td, ":", fixed=TRUE)[[1]][1]
      parts <- strsplit(td2, ".", fixed=TRUE)[[1]]
      parts <- parts[parts!=""]
      if (length(parts)==3 && all(grepl("^[0-9]+$", parts))) {
        if (nchar(parts[1])==4) {
          y <- suppressWarnings(as.numeric(parts[1])); m2 <- suppressWarnings(as.numeric(parts[2])); d2 <- suppressWarnings(as.numeric(parts[3]))
        } else {
          m2 <- suppressWarnings(as.numeric(parts[1])); d2 <- suppressWarnings(as.numeric(parts[2])); y <- suppressWarnings(as.numeric(parts[3]))
        }
        y <- expand2yr_1940(y)
        if (is.na(m2)||is.na(d2)||m2<1||m2>12||d2<1||d2>31) { res[i] <- as.Date(NA); next }
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
        if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
      }
      res[i] <- as.Date(NA); next
    }
    # compact digits only
    tc <- gsub("[^0-9]", "", td)
    if (tc != "" && grepl("^[0-9]+$", tc)) {
      if (nchar(tc)==8) {
        # try YYYYMMDD first if plausible year
        yr4 <- suppressWarnings(as.numeric(substr(tc,1,4)))
        if (!is.na(yr4) && yr4>=1800 && yr4<=2200) {
          d <- as.Date(tc, format="%Y%m%d")
          if (!is.na(d)) { res[i] <- d; next }
        }
        # else MMDDYYYY (MDY)
        m2 <- suppressWarnings(as.numeric(substr(tc,1,2))); d2 <- suppressWarnings(as.numeric(substr(tc,3,4))); y <- suppressWarnings(as.numeric(substr(tc,5,8)))
        y <- expand2yr_1940(y)
        if (!any(is.na(c(m2,d2,y))) && m2>=1 && m2<=12 && d2>=1 && d2<=31) {
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
        }
        res[i] <- as.Date(NA); next
      } else if (nchar(tc)==6) {
        # MMDDYY (MDY)
        m2 <- suppressWarnings(as.numeric(substr(tc,1,2))); d2 <- suppressWarnings(as.numeric(substr(tc,3,4))); y <- suppressWarnings(as.numeric(substr(tc,5,6)))
        y <- expand2yr_1940(y)
        if (!any(is.na(c(m2,d2,y))) && m2>=1 && m2<=12 && d2>=1 && d2<=31) {
          d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y,m2,d2), format="%Y-%m-%d"))
          if (!is.na(d)) { lt <- as.POSIXlt(d); if (lt$year+1900==y && lt$mon+1==m2 && lt$mday==d2) { res[i] <- d; next } }
        }
        res[i] <- as.Date(NA); next
      } else {
        res[i] <- as.Date(NA); next
      }
    }
    res[i] <- as.Date(NA)
  }
  res
}

# Mimic PROC IMPORT type inference for ID/START/END:
# if a column is all-numeric (pure numbers) SAS reads it numeric; else character.
is_all_numeric_col <- function(v) {
  vv <- sstrip(v)
  vv <- vv[!is.na(vv) & vv != "" & vv != "."]
  if (length(vv)==0) return(FALSE)
  all(grepl("^[0-9]+(\\.[0-9]+)?$", vv))
}

start_is_num <- is_all_numeric_col(raw_ae$START)
end_is_num <- is_all_numeric_col(raw_ae$END)
id_is_num <- is_all_numeric_col(raw_ae$ID)

ae_mapped <- raw_ae %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "AE",
    .ID_trim = sstrip(ID)
  )
# CATX("-",STUDYID,ID): if ID numeric in SAS, BEST12. strips leading zeros
if (id_is_num) {
  ae_mapped$.ID_trim <- ifelse(is.na(ae_mapped$.ID_trim) | ae_mapped$.ID_trim=="", NA_character_,
    as.character(suppressWarnings(as.numeric(ae_mapped$.ID_trim))))
  # remove trailing ".0" artifacts / scientific: format integers without decimals
  ae_mapped$.ID_trim <- ifelse(is.na(ae_mapped$.ID_trim), NA_character_,
    format(suppressWarnings(as.numeric(ae_mapped$.ID_trim)), scientific=FALSE, trim=TRUE))
  # BEST12. would show integers without decimal point
  ae_mapped$.ID_trim <- sub("\\.0+$", "", ae_mapped$.ID_trim)
}

ae_mapped <- ae_mapped %>%
  mutate(
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

if (start_is_num) {
  start_date <- suppressWarnings(as.Date(floor(suppressWarnings(as.numeric(start_chr))), origin="1960-01-01"))
  # non-numeric (should not happen when all-numeric) -> NA
  start_date[is.na(suppressWarnings(as.numeric(start_chr)))] <- as.Date(NA)
} else {
  start_date <- parse_anydtdte_mdy_strict(start_chr)
}
if (end_is_num) {
  end_date <- suppressWarnings(as.Date(floor(suppressWarnings(as.numeric(end_chr))), origin="1960-01-01"))
  end_date[is.na(suppressWarnings(as.numeric(end_chr)))] <- as.Date(NA)
} else {
  end_date <- parse_anydtdte_mdy_strict(end_chr)
}

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

# PROC SORT by USUBJID AESTDTC (missing first, SAS byte order); stable tie-break on input order
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
```