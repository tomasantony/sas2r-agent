# P014_sdtm_ex.sas -> R translation
# SDTM EX domain mapping

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
raw_path <- file.path(project_path, "data/raw/raw_exposure.csv")
if (!file.exists(raw_path)) {
  cand <- list.files("/bench", pattern = "raw_exposure\\.csv$",
                     recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_path <- cand[1]
}
cat("Reading:", raw_path, "\n")

# 1. IMPORT RAW EXPOSURE DATA (PROC IMPORT CSV GETNAMES=YES)
raw_ex <- suppressMessages(readr::read_csv(raw_path,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("NA"), trim_ws = TRUE, guess_max = 10000, show_col_types = FALSE))
names(raw_ex) <- toupper(names(raw_ex))
for (nm in names(raw_ex)) {
  v <- raw_ex[[nm]]
  v[trimws(as.character(v)) == ""] <- NA_character_
  raw_ex[[nm]] <- v
}
cat("Raw rows:", nrow(raw_ex), " cols:", paste(names(raw_ex), collapse=","), "\n")
print(utils::head(as.data.frame(raw_ex), 5))

get_col <- function(df, candidates) {
  for (c in candidates) if (c %in% names(df)) return(df[[c]])
  return(rep(NA_character_, nrow(df)))
}
PATIENT    <- get_col(raw_ex, c("PATIENT","SUBJID","SUBJECT","PT"))
TREATMENT  <- get_col(raw_ex, c("TREATMENT","TRT","EXTRT"))
DOSE       <- get_col(raw_ex, c("DOSE","EXDOSE"))
UNIT       <- get_col(raw_ex, c("UNIT","EXDOSU","DOSU"))
START_DATE <- get_col(raw_ex, c("START_DATE","STARTDATE","EXSTDTC","START"))
END_DATE   <- get_col(raw_ex, c("END_DATE","ENDDATE","EXENDTC","END"))

# --- helpers ---
mon_map <- c(JAN=1L,FEB=2L,MAR=3L,APR=4L,MAY=5L,JUN=6L,JUL=7L,AUG=8L,SEP=9L,OCT=10L,NOV=11L,DEC=12L)

expand_2dyr <- function(y) {
  if (length(y) != 1 || is.na(y)) return(NA_integer_)
  y <- as.integer(y)
  if (is.na(y)) return(NA_integer_)
  if (y >= 100) return(y)
  if (y <= 68) return(2000L + y) else return(1900L + y)
}

mkdate <- function(y, mo, d) {
  if (length(y)!=1 || length(mo)!=1 || length(d)!=1) return(as.Date(NA))
  if (is.na(suppressWarnings(as.numeric(y))) || is.na(suppressWarnings(as.numeric(mo))) || is.na(suppressWarnings(as.numeric(d)))) return(as.Date(NA))
  y <- suppressWarnings(as.integer(y)); mo <- suppressWarnings(as.integer(mo)); d <- suppressWarnings(as.integer(d))
  if (is.na(y) || is.na(mo) || is.na(d)) return(as.Date(NA))
  if (mo < 1 || mo > 12 || d < 1 || d > 31 || y < 1500 || y > 2500) return(as.Date(NA))
  s <- sprintf("%04d-%02d-%02d", y, mo, d)
  dd <- tryCatch(suppressWarnings(as.Date(s, format="%Y-%m-%d")), error=function(e) as.Date(NA))
  if (length(dd) != 1 || is.na(dd)) return(as.Date(NA))
  # guard overflow (e.g. Feb 30 -> Mar): re-format must match
  back <- format(dd, "%Y-%m-%d")
  if (is.na(back) || back != s) return(as.Date(NA))
  dd
}

safe_asDate <- function(s, fmt) {
  tryCatch(suppressWarnings(as.Date(s, format=fmt)), error=function(e) as.Date(NA))
}

parse_one <- function(s) {
  if (length(s) != 1 || is.na(s)) return(as.Date(NA))
  s <- trimws(as.character(s))
  if (is.na(s) || s == "") return(as.Date(NA))
  if (toupper(s) %in% c(".", "NA", "NULL", "MISSING")) return(as.Date(NA))
  # strip time portion when ':' present
  if (grepl(":", s, fixed=TRUE)) {
    tok <- strsplit(s, "[ T]+")[[1]][1]
    if (!is.na(tok) && nchar(trimws(tok)) >= 4) s <- trimws(tok)
  }
  s <- trimws(s)
  su <- toupper(s)
  su <- gsub(",", " ", su, fixed=FALSE)
  su <- gsub("\\.", "/", su)
  su <- trimws(gsub("\\s+", " ", su))

  # pure digits
  if (grepl("^[0-9]+$", su)) {
    if (grepl("^(19|20)[0-9]{6}$", su)) {
      dd <- mkdate(substr(su,1,4), substr(su,5,6), substr(su,7,8))
      if (!is.na(dd)) return(dd)
    }
    num <- suppressWarnings(as.numeric(su))
    if (!is.na(num) && num >= 5000 && num <= 60000 && nchar(su) %in% c(4,5)) {
      dd <- tryCatch(suppressWarnings(as.Date(num, origin="1960-01-01")), error=function(e) as.Date(NA))
      return(dd)
    }
    return(as.Date(NA))
  }

  # YYYY-MM-DD or YYYY/MM/DD
  m <- regmatches(su, regexec("^(\\d{4})[/\\-](\\d{1,2})[/\\-](\\d{1,2})$", su))[[1]]
  if (length(m) == 4) {
    dd <- mkdate(m[2], m[3], m[4])
    if (!is.na(dd)) return(dd)
  }
  # slash/dash numeric: DMY first (data are DD/MM/YYYY), then MDY fallback
  m <- regmatches(su, regexec("^(\\d{1,2})[/\\-](\\d{1,2})[/\\-](\\d{2,4})$", su))[[1]]
  if (length(m) == 4) {
    a <- suppressWarnings(as.integer(m[2])); b <- suppressWarnings(as.integer(m[3]))
    y <- expand_2dyr(suppressWarnings(as.integer(m[4])))
    # DMY: d=a, mo=b
    dd <- mkdate(y, b, a)
    if (!is.na(dd)) return(dd)
    # MDY fallback: mo=a, d=b
    dd2 <- mkdate(y, a, b)
    if (!is.na(dd2)) return(dd2)
    return(as.Date(NA))
  }
  # DDMMMYYYY variants
  m <- regmatches(su, regexec("^(\\d{1,2})[ /\\-]*(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[ /\\-]*((?:19|20)?\\d{2})$", su))[[1]]
  if (length(m) == 4) {
    d <- suppressWarnings(as.integer(m[2])); mo <- unname(mon_map[m[3]]); y <- expand_2dyr(suppressWarnings(as.integer(m[4])))
    dd <- mkdate(y, mo, d)
    if (!is.na(dd)) return(dd)
  }
  # MMM DD YYYY
  m <- regmatches(su, regexec("^(JAN|FEB|MAR|APR|MAY|JUN|JUL|AUG|SEP|OCT|NOV|DEC)[ /\\-]+(\\d{1,2})[ /\\-]+((?:19|20)?\\d{2})$", su))[[1]]
  if (length(m) == 4) {
    mo <- unname(mon_map[m[2]]); d <- suppressWarnings(as.integer(m[3])); y <- expand_2dyr(suppressWarnings(as.integer(m[4])))
    dd <- mkdate(y, mo, d)
    if (!is.na(dd)) return(dd)
  }
  # strict fallback formats
  fmts <- c("%Y-%m-%d","%Y/%m/%d","%d/%m/%Y","%m/%d/%Y","%d-%m-%Y","%m-%d-%Y",
            "%d/%m/%y","%m/%d/%y","%d-%m-%y","%m-%d-%y",
            "%d-%b-%Y","%d-%b-%y","%d%b%Y","%d%b%y","%d %b %Y","%d %b %y",
            "%b %d %Y","%b %d %y","%B %d %Y","%Y%m%d","%d/%m/%Y")
  for (f in fmts) {
    dd <- safe_asDate(s, f)
    if (!is.na(dd)) return(dd)
    dd <- safe_asDate(su, toupper(f))
    if (!is.na(dd)) return(dd)
  }
  return(as.Date(NA))
}

parse_vec <- function(x) {
  res <- rep(as.Date(NA), length(x))
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    res[i] <- parse_one(x[i])
  }
  res
}

n <- nrow(raw_ex)
STUDYID <- rep("CDISC-01", n)
DOMAIN  <- rep("EX", n)
pat_trim <- trimws(as.character(PATIENT))
pat_trim[is.na(pat_trim) | pat_trim == ""] <- NA_character_
USUBJID <- ifelse(is.na(pat_trim), "CDISC-01", paste0("CDISC-01-", pat_trim))
EXTRT <- trimws(as.character(TREATMENT))
EXTRT[EXTRT == ""] <- NA_character_
EXDOSE <- suppressWarnings(as.numeric(as.character(DOSE)))
EXDOSU <- trimws(as.character(UNIT))
EXDOSU[EXDOSU == ""] <- NA_character_

start_dt <- parse_vec(as.character(START_DATE))
end_dt   <- parse_vec(as.character(END_DATE))
EXSTDTC <- format(start_dt, "%Y-%m-%d")
EXENDTC <- format(end_dt, "%Y-%m-%d")

ex_mapped <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  EXTRT = EXTRT, EXDOSE = EXDOSE, EXDOSU = EXDOSU,
  EXSTDTC = EXSTDTC, EXENDTC = EXENDTC, stringsAsFactors = FALSE
)

# 3. GENERATE SEQUENCE NUMBER (EXSEQ): sort by USUBJID EXSTDTC (SAS: missing first, ASCII order)
ord <- order(ex_mapped$USUBJID, ex_mapped$EXSTDTC, na.last = FALSE, method = "radix")
ex_mapped <- ex_mapped[ord, , drop = FALSE]
EXSEQ <- integer(nrow(ex_mapped))
if (nrow(ex_mapped) > 0) {
  cur_id <- NA_character_; cnt <- 0L
  for (i in seq_len(nrow(ex_mapped))) {
    id <- ex_mapped$USUBJID[i]
    if (is.na(id)) id <- "<NA>"
    if (is.na(cur_id) || id != cur_id) { cnt <- 1L; cur_id <- id } else { cnt <- cnt + 1L }
    EXSEQ[i] <- cnt
  }
}
ex_mapped$EXSEQ <- EXSEQ
sdtm_ex <- ex_mapped[, c("STUDYID","DOMAIN","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")]
rownames(sdtm_ex) <- NULL

write.csv(sdtm_ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: proc print obs=10
audit <- head(sdtm_ex[, c("USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")], 10)
write.csv(audit, file.path(out, "P014.csv"), row.names = FALSE, na = "")
write.csv(audit, file.path(out, "P014.lst.csv"), row.names = FALSE, na = "")

audit_txt <- audit
for (cc in names(audit_txt)) audit_txt[[cc]][is.na(audit_txt[[cc]])] <- ""
hdr_line <- sprintf("%-4s %-18s %-5s %-20s %-8s %-10s %-10s %-10s",
  "Obs","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXSTDTC","EXENDTC")
lines <- c("EX Domain Audit (Study Drug Exposure)", "",
  hdr_line, paste(rep("-", nchar(hdr_line)), collapse=""))
for (i in seq_len(nrow(audit_txt))) {
  r <- audit_txt[i, , drop=FALSE]
  exdose_s <- ifelse(is.na(audit$EXDOSE[i]), "", as.character(audit$EXDOSE[i]))
  lines <- c(lines, sprintf("%-4d %-18s %-5s %-20s %-8s %-10s %-10s %-10s",
    i, as.character(r$USUBJID), as.character(r$EXSEQ), as.character(r$EXTRT),
    exdose_s, as.character(r$EXDOSU), as.character(r$EXSTDTC), as.character(r$EXENDTC)))
}
lines <- c(lines, "", sprintf("N = %d (first %d observations shown of %d in EX domain)",
  nrow(audit), nrow(audit), nrow(sdtm_ex)))
writeLines(lines, file.path(out, "P014.txt"))
writeLines(lines, file.path(out, "P014.lst.txt"))
writeLines(lines, file.path(out, "P014.lst"))
cat("Done. EX rows:", nrow(sdtm_ex), " audit rows:", nrow(audit), "\n")
