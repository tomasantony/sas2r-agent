out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
raw_path <- paste0(project_path, "/data/raw/raw_ae.csv")
if (!file.exists(raw_path)) {
  cands <- c(
    "/bench/data/derived/c2c/data/raw/raw_ae.csv",
    "/bench/data/raw/cdisc-pilot/sdtm/raw_ae.csv",
    "/bench/data/raw/raw_ae.csv"
  )
  for (a in cands) if (file.exists(a)) { raw_path <- a; break }
}

sas_trunc <- function(x, n) {
  v <- as.character(x)
  isna <- is.na(v)
  v[!isna] <- substr(v[!isna], 1, n)
  v
}
sas_strip <- function(x) {
  v <- as.character(x)
  v[is.na(v)] <- ""
  trimws(v)
}
sas_upcase_strip <- function(x) {
  toupper(sas_strip(x))
}

month_num <- function(s) {
  m <- tolower(s)
  m3 <- substr(m, 1, 3)
  map3 <- c(jan=1L,feb=2L,mar=3L,apr=4L,may=5L,jun=6L,jul=7L,aug=8L,sep=9L,oct=10L,nov=11L,dec=12L)
  if (m3 %in% names(map3)) return(unname(map3[m3]))
  mapf <- c(january=1L,february=2L,march=3L,april=4L,may=5L,june=6L,july=7L,
            august=8L,september=9L,october=10L,november=11L,december=12L,
            sept=9L)
  if (m %in% names(mapf)) return(unname(mapf[m]))
  return(NA_integer_)
}

parse_one_anydtdte <- function(s) {
  if (length(s) == 0) return(NA_real_)
  if (is.na(s)) return(NA_real_)
  s0 <- trimws(as.character(s))
  if (is.na(s0) || s0 == "") return(NA_real_)
  # pure integer string
  if (grepl("^[+-]?[0-9]+$", s0)) {
    # 8-digit yyyymmdd starting 19/20
    if (grepl("^(19|20)[0-9]{6}$", s0)) {
      d <- suppressWarnings(as.Date(s0, format = "%Y%m%d"))
      if (!is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
    }
    num <- suppressWarnings(as.numeric(s0))
    if (!is.na(num)) {
      # 4-digit year like 2023 -> treat as Jan 1 of that year (recent SAS dates are 5-digit)
      if (grepl("^(19|20)[0-9]{2}$", s0)) {
        d2 <- suppressWarnings(as.Date(paste0(s0, "-01-01"), format = "%Y-%m-%d"))
        if (!is.na(d2)) return(as.numeric(d2 - as.Date("1960-01-01")))
      }
      # plausible SAS date range -> treat as SAS date number
      if (!is.na(num) && num >= -5000 && num <= 80000) {
        # distinguish: 5-digit numbers in 10000-30000 are SAS dates; return directly
        return(num)
      }
      if (grepl("^[0-9]{8}$", s0)) {
        d <- suppressWarnings(as.Date(s0, format = "%Y%m%d"))
        if (!is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
      }
      # fallback: if integer in SAS range return it
      if (!is.na(num) && num >= -5000 && num <= 80000) return(num)
    }
    return(NA_real_)
  }
  # pure decimal number string -> NA (time not expected)
  if (grepl("^[+-]?[0-9]*\\.[0-9]+$", s0)) return(NA_real_)

  fmts <- c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y","%m-%d-%Y","%d-%m-%Y",
            "%Y%m%d","%Y.%m.%d","%m.%d.%Y","%d.%m.%Y",
            "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%Y/%m/%d %H:%M:%S",
            "%m/%d/%Y %H:%M:%S","%m/%d/%Y %H:%M","%Y-%m-%d %H:%M","%Y-%m-%dT%H:%M",
            "%d-%b-%Y","%d %b %Y","%d%b%Y","%d-%b-%y","%d %b %y","%d%b%y",
            "%b %d, %Y","%B %d, %Y","%b %d %Y","%B %d %Y","%d %B %Y",
            "%m/%d/%y","%d/%m/%y","%d-%b-%Y %H:%M:%S","%d %b %Y %H:%M:%S",
            "%Y-%m-%dT%H:%M","%d-%b-%Y:%H:%M:%S","%d%b%Y:%H:%M:%S")
  d <- suppressWarnings(as.Date(s0, tryFormats = fmts))
  if (length(d) == 1 && !is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
  # title-case variant for upper-case months (e.g. 05JAN2023)
  s_low <- tolower(s0)
  s_title <- tools::toTitleCase(s_low)
  if (!identical(s_title, s0)) {
    d <- suppressWarnings(as.Date(s_title, tryFormats = fmts))
    if (length(d) == 1 && !is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
  }
  # manual DATE9: dd-mon-yyyy
  m <- regexec("([0-9]{1,2})[^0-9A-Za-z]+([A-Za-z]{3,9})[^0-9A-Za-z]+([0-9]{2,4})", s0)
  parts <- regmatches(s0, m)[[1]]
  if (length(parts) == 4) {
    dd <- suppressWarnings(as.integer(parts[2]))
    mn <- month_num(parts[3])
    yy <- suppressWarnings(as.integer(parts[4]))
    if (!is.na(dd) && !is.na(mn) && !is.na(yy)) {
      if (yy < 100) yy <- ifelse(yy <= 30, 2000L + yy, 1900L + yy)
      d3 <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mn, dd)))
      if (!is.na(d3)) return(as.numeric(d3 - as.Date("1960-01-01")))
    }
  }
  m <- regexec("([0-9]{1,2})([A-Za-z]{3,9})([0-9]{2,4})", s0)
  parts <- regmatches(s0, m)[[1]]
  if (length(parts) == 4) {
    dd <- suppressWarnings(as.integer(parts[2]))
    mn <- month_num(parts[3])
    yy <- suppressWarnings(as.integer(parts[4]))
    if (!is.na(dd) && !is.na(mn) && !is.na(yy)) {
      if (yy < 100) yy <- ifelse(yy <= 30, 2000L + yy, 1900L + yy)
      d3 <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mn, dd)))
      if (!is.na(d3)) return(as.numeric(d3 - as.Date("1960-01-01")))
    }
  }
  # mon dd yyyy
  m2 <- regexec("([A-Za-z]{3,9})[^0-9A-Za-z]+([0-9]{1,2})[^0-9A-Za-z]+([0-9]{2,4})", s0)
  parts2 <- regmatches(s0, m2)[[1]]
  if (length(parts2) == 4) {
    mn <- month_num(parts2[2])
    dd <- suppressWarnings(as.integer(parts2[3]))
    yy <- suppressWarnings(as.integer(parts2[4]))
    if (!is.na(dd) && !is.na(mn) && !is.na(yy)) {
      if (yy < 100) yy <- ifelse(yy <= 30, 2000L + yy, 1900L + yy)
      d3 <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mn, dd)))
      if (!is.na(d3)) return(as.numeric(d3 - as.Date("1960-01-01")))
    }
  }
  # strip time part and retry date part
  s_datepart <- sub("[T ].*$", "", s0)
  if (!identical(s_datepart, s0) && nchar(s_datepart) >= 6) {
    d <- suppressWarnings(as.Date(s_datepart, tryFormats = fmts))
    if (length(d) == 1 && !is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
    s_t2 <- tools::toTitleCase(tolower(s_datepart))
    d <- suppressWarnings(as.Date(s_t2, tryFormats = fmts))
    if (length(d) == 1 && !is.na(d)) return(as.numeric(d - as.Date("1960-01-01")))
  }
  return(NA_real_)
}

parse_anydtdte_vec <- function(v) {
  v <- as.character(v)
  outv <- rep(NA_real_, length(v))
  for (i in seq_along(v)) outv[i] <- parse_one_anydtdte(v[i])
  outv
}

sas_to_iso <- function(num) {
  res <- rep("", length(num))
  ok <- !is.na(num)
  res[ok] <- format(as.Date("1960-01-01") + num[ok], "%Y-%m-%d")
  res
}

get_sas_num <- function(col) {
  if (is.numeric(col)) return(as.numeric(col))
  if (inherits(col, "Date")) return(as.numeric(col - as.Date("1960-01-01")))
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    return(as.numeric(as.Date(col, tz = "UTC") - as.Date("1960-01-01")))
  }
  if (is.logical(col)) return(rep(NA_real_, length(col)))
  parse_anydtdte_vec(as.character(col))
}

# 1. IMPORT RAW ADVERSE EVENTS (PROC IMPORT guessingrows=max)
raw_ae <- suppressWarnings(readr::read_csv(raw_path, guess_max = 1000000,
  show_col_types = FALSE, progress = FALSE))
names(raw_ae) <- toupper(names(raw_ae))

getcol <- function(df, nm) {
  if (nm %in% names(df)) return(df[[nm]])
  rep(NA_character_, nrow(df))
}
ID_raw <- getcol(raw_ae, "ID")
AE_TERM_raw <- getcol(raw_ae, "AE_TERM")
SEV_raw <- getcol(raw_ae, "SEV")
SERIOUS_raw <- getcol(raw_ae, "SERIOUS")
RELATED_raw <- getcol(raw_ae, "RELATED")
START_raw <- getcol(raw_ae, "START")
END_raw <- getcol(raw_ae, "END")

n <- nrow(raw_ae)

# 2. CORE TRANSFORMATION & DERIVATION
STUDYID_v <- rep(sas_trunc("CDISC-01", 8), n)
DOMAIN_v <- rep(sas_trunc("AE", 2), n)

if (is.numeric(ID_raw)) {
  ID_char <- vapply(ID_raw, function(x) if (is.na(x)) "" else trimws(format(x, scientific = FALSE, trim = TRUE)), character(1))
} else {
  ID_char <- sas_strip(ID_raw)
}
USUBJID_v <- ifelse(ID_char == "", STUDYID_v, paste0(STUDYID_v, "-", ID_char))
USUBJID_v <- sas_trunc(USUBJID_v, 40)

AETERM_v <- sas_trunc(sas_strip(AE_TERM_raw), 60)
AEDECOD_v <- sas_trunc(toupper(AETERM_v), 60)
AETERM_v[is.na(AETERM_v)] <- ""
AEDECOD_v[is.na(AEDECOD_v)] <- ""

AESEV_v <- sas_trunc(sas_upcase_strip(SEV_raw), 10)
SER_up <- sas_upcase_strip(SERIOUS_raw)
AESER_v <- ifelse(SER_up == "Y", "Y", "N")
AESER_v <- sas_trunc(AESER_v, 1)
REL_up <- sas_upcase_strip(RELATED_raw)
AEREL_v <- ifelse(REL_up == "Y", "RELATED",
  ifelse(REL_up == "N", "NOT RELATED", "UNKNOWN"))
AEREL_v <- sas_trunc(AEREL_v, 15)

start_num <- get_sas_num(START_raw)
end_num <- get_sas_num(END_raw)
AESTDTC_v <- sas_to_iso(start_num)
AEENDTC_v <- sas_to_iso(end_num)
AEOUT_v <- ifelse(is.na(end_num), "NOT RECOVERED/NOT RESOLVED", "RECOVERED/RESOLVED")
AEOUT_v <- sas_trunc(AEOUT_v, 30)

AESTDTC_v[is.na(AESTDTC_v)] <- ""
AEENDTC_v[is.na(AEENDTC_v)] <- ""
AESEV_v[is.na(AESEV_v)] <- ""
AESER_v[is.na(AESER_v)] <- ""
AEREL_v[is.na(AEREL_v)] <- ""
AEOUT_v[is.na(AEOUT_v)] <- ""

ae_mapped <- data.frame(
  STUDYID = STUDYID_v, DOMAIN = DOMAIN_v, USUBJID = USUBJID_v,
  AETERM = AETERM_v, AEDECOD = AEDECOD_v, AESEV = AESEV_v,
  AESER = AESER_v, AEREL = AEREL_v, AESTDTC = AESTDTC_v,
  AEENDTC = AEENDTC_v, AEOUT = AEOUT_v,
  stringsAsFactors = FALSE
)

# 3. GENERATE SEQUENCE NUMBER (AESEQ) - sort by USUBJID AESTDTC (SAS byte order, missing first)
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, na.last = FALSE, method = "radix")
ae_mapped <- ae_mapped[ord, , drop = FALSE]
AESEQ_v <- integer(nrow(ae_mapped))
if (nrow(ae_mapped) > 0) {
  cur_id <- NA_character_
  cnt <- 0L
  for (i in seq_len(nrow(ae_mapped))) {
    uid <- ae_mapped$USUBJID[i]
    if (is.na(uid)) uid <- ""
    if (i == 1 || !identical(uid, cur_id)) { cnt <- 1L; cur_id <- uid } else { cnt <- cnt + 1L }
    AESEQ_v[i] <- cnt
  }
}
ae_mapped$AESEQ <- AESEQ_v
sdtm_ae <- ae_mapped[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]

write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=12
print_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
audit_n <- min(12, nrow(sdtm_ae))
audit_df <- sdtm_ae[seq_len(audit_n), print_vars, drop = FALSE]
audit_df <- cbind(Obs = seq_len(audit_n), audit_df)
write.csv(audit_df, file.path(out, "P009.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P009.txt")
con <- file(txt_path, open = "wt")
writeLines("AE Domain Audit (Adverse Events with Ongoing Logic)", con)
writeLines(paste0("Dataset: SDTM.AE  (N=", nrow(sdtm_ae), ", showing first ", audit_n, " observations sorted by USUBJID AESTDTC)"), con)
writeLines("", con)
hdr <- sprintf("%-4s %-20s %-5s %-30s %-10s %-5s %-13s %-10s %-10s %-25s",
  "Obs","USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)
for (i in seq_len(audit_n)) {
  r <- audit_df[i, ]
  line <- sprintf("%-4d %-20s %-5d %-30s %-10s %-5s %-13s %-10s %-10s %-25s",
    as.integer(r$Obs), as.character(r$USUBJID), as.integer(r$AESEQ),
    substr(as.character(r$AETERM), 1, 30), as.character(r$AESEV),
    as.character(r$AESER), as.character(r$AEREL),
    as.character(r$AESTDTC), as.character(r$AEENDTC), as.character(r$AEOUT))
  writeLines(line, con)
}
close(con)
try({ file.copy(file.path(out, "P009.txt"), file.path(out, "P009.lst"), overwrite = TRUE) }, silent = TRUE)
try({ file.copy(file.path(out, "P009.csv"), file.path(out, "P009.lst.csv"), overwrite = TRUE) }, silent = TRUE)
