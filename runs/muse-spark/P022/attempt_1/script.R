# P022_ae.sas -> R translation
# SDTM AE mapping & derivation + PROC PRINT listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

raw_ae_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx"
raw_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
raw_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

# ---------- SAS helpers ----------
sas_trunc <- function(x, n) {
  # LENGTH $n truncation like SAS; NA stays NA
  v <- as.character(x)
  is_na <- is.na(v)
  v[!is_na] <- substr(v[!is_na], 1, n)
  v
}
sas_upcase <- function(x) {
  v <- as.character(x)
  ifelse(is.na(v), NA_character_, toupper(v))
}
sas_tranwrd <- function(x, from, to) {
  v <- as.character(x)
  ifelse(is.na(v), NA_character_, gsub(from, to, v, fixed = TRUE))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  # CATX strips leading/trailing blanks, skips missing/empty?
  # SAS CATX: treats missing as empty and strips; if all missing -> ""
  # Here implement per-element vectorized for two args, first constant
  # General: paste trimmed non-missing? SAS CATX skips empty? Actually CATX removes leading/trailing and inserts sep, missing treated as empty but sep still? No, CATX strips and omits extra seps for missing? Let's mimic: for USUBJID = catx("-", "ONCO-2026", SUBJID)
  a <- args[[1]]; b <- args[[2]]
  # recycle
  n <- max(length(a), length(b))
  a <- rep(a, length.out = n); b <- rep(b, length.out = n)
  trim <- function(z) ifelse(is.na(z), "", trimws(as.character(z)))
  ta <- trim(a); tb <- trim(b)
  # SAS CATX with 2 items: if one is empty, no delimiter? e.g. catx("-", "", "A") = "A"? Actually CATX removes leading/trailing blanks and inserts delimiter between non-blank? Let's implement that.
  res <- ifelse(ta == "" & tb == "", "",
         ifelse(ta == "", tb,
         ifelse(tb == "", ta, paste0(ta, sep, tb))))
  res
}

get_col <- function(df, target) {
  # case-insensitive column lookup; returns vector or NULL
  cn <- names(df)
  idx <- which(toupper(cn) == toupper(target))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
get_colname <- function(df, target) {
  cn <- names(df)
  idx <- which(toupper(cn) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  cn[idx[1]]
}

parse_date_char <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  s <- trimws(as.character(s))
  if (s == "" || toupper(s) %in% c(".", "NA", "NULL", "MISSING")) return(as.Date(NA))
  # Try lubridate ymd first (handles YYYY-MM-DD, YYYY/MM/DD, with time)
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE, truncated = 0))
  if (!is.na(d)) return(as.Date(d))
  # Try ymd_hms via parse_date_time for datetime strings
  # Try dmy (handles 01JAN2026, 01-JAN-2026, 01 JAN 2026, 01/01/2026 as dmy?)
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  # Title-case month abbreviations for base as.Date %b
  # Build title-case version: JAN->Jan etc.
  s2 <- s
  # replace month abbreviations case-insensitively with Title case
  # manual loop
  months_up <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","SEPT","OCT","NOV","DEC")
  months_tc <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Sept","Oct","Nov","Dec")
  s2_up <- toupper(s2)
  for (k in seq_along(months_up)) {
    s2 <- gsub(months_up[k], months_tc[k], s2, fixed = TRUE)
    # also handle already upper? gsub fixed is case-sensitive, so need to replace upper only; but s may be lower/mixed
    # do case-insensitive via regex
    s2 <- gsub(paste0("(?i)", months_up[k]), months_tc[k], s2, perl = TRUE)
  }
  fmts <- c("%d%b%Y","%d-%b-%Y","%d %b %Y","%d%b%y","%d-%b-%y","%d %b %y",
            "%d/%m/%Y","%m/%d/%Y","%Y-%m-%d","%Y/%m/%d","%d-%m-%Y","%d.%m.%Y",
            "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%d-%b-%Y %H:%M","%d%b%Y %H:%M")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s2, format = f))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d)) return(d)
  }
  return(as.Date(NA))
}

raw_vec_to_date <- function(vec) {
  # returns Date vector
  if (inherits(vec, "Date")) return(vec)
  if (inherits(vec, "POSIXt")) return(as.Date(vec))
  if (is.numeric(vec) && !is.character(vec)) {
    # Could be Excel numeric dates; distinguish small numbers (e.g., TXGR) vs Excel dates?
    # Only called for date columns, so treat numeric as Excel date
    return(suppressWarnings(as.Date(vec, origin = "1899-12-30")))
  }
  # character / factor / mixed (readxl may return POSIXct for dates)
  # Handle list? No
  v <- as.character(vec)
  # readxl sometimes returns numeric stored as character? keep
  res <- as.Date(rep(NA, length(v)))
  for (i in seq_along(v)) {
    vi <- v[i]
    if (is.na(vi)) { res[i] <- as.Date(NA); next }
    vi_t <- trimws(vi)
    if (vi_t == "") { res[i] <- as.Date(NA); next }
    # If vi looks like numeric and original was character numeric? Try Excel numeric? No, keep parse
    res[i] <- parse_date_char(vi_t)
  }
  res
}

raw_vec_to_iso <- function(vec) {
  if (is.null(vec)) return(rep(NA_character_, 0))
  if (inherits(vec, "Date")) {
    return(ifelse(is.na(vec), NA_character_, format(vec, "%Y-%m-%d")))
  }
  if (inherits(vec, "POSIXt")) {
    d <- as.Date(vec)
    return(ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d")))
  }
  if (is.numeric(vec)) {
    d <- suppressWarnings(as.Date(vec, origin = "1899-12-30"))
    return(ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d")))
  }
  v <- as.character(vec)
  d <- raw_vec_to_date(v)
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

iso_to_date <- function(vec) {
  # SAS input(..., yymmdd10.): read first 10 chars as YYYY-MM-DD; fallback to generic parse
  v <- as.character(vec)
  res <- as.Date(rep(NA, length(v)))
  for (i in seq_along(v)) {
    vi <- v[i]
    if (is.na(vi)) { res[i] <- as.Date(NA); next }
    vi_t <- trimws(vi)
    if (vi_t == "") { res[i] <- as.Date(NA); next }
    sub10 <- substr(vi_t, 1, 10)
    d <- suppressWarnings(as.Date(sub10, format = "%Y-%m-%d"))
    if (!is.na(d)) { res[i] <- d; next }
    # fallback generic
    res[i] <- parse_date_char(vi_t)
  }
  res
}

# ----------  Read edc_ae ----------
message("Reading edc_ae ...")
edc_ae <- readxl::read_excel(raw_ae_path, .name_repair = "minimal")
names(edc_ae) <- trimws(names(edc_ae))
message(paste0("edc_ae cols: ", paste(names(edc_ae), collapse=", "), " n=", nrow(edc_ae)))

# Extract raw columns case-insensitively
c_SUBJID <- get_col(edc_ae, "SUBJID")
c_STDT   <- get_col(edc_ae, "STDT")
c_ENDT   <- get_col(edc_ae, "ENDT")
c_TERM   <- get_col(edc_ae, "TERM")
c_DECOD  <- get_col(edc_ae, "DECOD")
c_BODSYS <- get_col(edc_ae, "BODSYS")
c_SEV    <- get_col(edc_ae, "SEV")
c_TXGR   <- get_col(edc_ae, "TXGR")
c_REL    <- get_col(edc_ae, "REL")
c_OUT    <- get_col(edc_ae, "OUT")
c_SER    <- get_col(edc_ae, "SER")
n_ae <- nrow(edc_ae)
if (is.null(c_SUBJID)) c_SUBJID <- rep(NA_character_, n_ae)
if (is.null(c_STDT)) c_STDT <- rep(NA_character_, n_ae)
if (is.null(c_ENDT)) c_ENDT <- rep(NA_character_, n_ae)
if (is.null(c_TERM)) c_TERM <- rep(NA_character_, n_ae)
if (is.null(c_DECOD)) c_DECOD <- rep(NA_character_, n_ae)
if (is.null(c_BODSYS)) c_BODSYS <- rep(NA_character_, n_ae)
if (is.null(c_SEV)) c_SEV <- rep(NA_character_, n_ae)
if (is.null(c_TXGR)) c_TXGR <- rep(NA_real_, n_ae)
if (is.null(c_REL)) c_REL <- rep(NA_character_, n_ae)
if (is.null(c_OUT)) c_OUT <- rep(NA_character_, n_ae)
if (is.null(c_SER)) c_SER <- rep(NA_character_, n_ae)

# ----------  dm_for_ae : try to locate sdtm_dm, else derive ----------
dm_for_ae <- NULL
# search for existing dm datasets
search_roots <- c("D:/meta_hackathon/sas2r-agent/output/sas/datasets",
                  "D:/meta_hackathon/sas2r-agent/data/derived",
                  "D:/meta_hackathon/sas2r-agent/data/raw")
found_dm_file <- NULL
for (rt in search_roots) {
  if (!dir.exists(rt)) next
  # list files recursively, look for dm.xlsx, sdtm_dm.*, dm.csv, dm.sas7bdat, dm.xpt
  fl <- tryCatch(list.files(rt, recursive = TRUE, full.names = TRUE), error = function(e) character(0))
  # prefer files with sdtm_dm or /dm. extension
  cand <- fl[grepl("(?i)(sdtm_dm|/dm\\.|/dm_|_dm\\.)", fl, perl = TRUE)]
  # exclude this program's own ae output
  cand <- cand[!grepl("(?i)/ae\\.", cand, perl = TRUE)]
  if (length(cand) > 0) {
    # prefer xlsx/csv
    ord <- order(!grepl("(?i)\\.xlsx$", cand), !grepl("(?i)\\.csv$", cand), nchar(cand))
    cand <- cand[ord]
    found_dm_file <- cand[1]
    break
  }
}
if (!is.null(found_dm_file) && !is.na(found_dm_file) && file.exists(found_dm_file)) {
  message(paste0("Found DM file: ", found_dm_file))
  dm_raw <- tryCatch({
    if (grepl("(?i)\\.xlsx$", found_dm_file)) readxl::read_excel(found_dm_file, .name_repair = "minimal")
    else if (grepl("(?i)\\.xls$", found_dm_file)) readxl::read_excel(found_dm_file, .name_repair = "minimal")
    else if (grepl("(?i)\\.csv$", found_dm_file)) readr::read_csv(found_dm_file, show_col_types = FALSE)
    else if (grepl("(?i)\\.xpt$", found_dm_file)) haven::read_xpt(found_dm_file)
    else if (grepl("(?i)\\.sas7bdat$", found_dm_file)) haven::read_sas(found_dm_file)
    else readxl::read_excel(found_dm_file, .name_repair = "minimal")
  }, error = function(e) { message(paste0("DM read failed: ", e$message)); NULL })
  if (!is.null(dm_raw)) {
    names(dm_raw) <- trimws(names(dm_raw))
    d_USUBJID <- get_col(dm_raw, "USUBJID")
    d_RFSTDTC <- get_col(dm_raw, "RFSTDTC")
    if (!is.null(d_USUBJID) && !is.null(d_RFSTDTC)) {
      # RFSTDTC may be Date/datetime/numeric/char -> convert to ISO YYYY-MM-DD
      if (inherits(d_RFSTDTC, "Date")) rf_iso <- ifelse(is.na(d_RFSTDTC), NA_character_, format(d_RFSTDTC, "%Y-%m-%d"))
      else if (inherits(d_RFSTDTC, "POSIXt")) rf_iso <- ifelse(is.na(d_RFSTDTC), NA_character_, format(as.Date(d_RFSTDTC), "%Y-%m-%d"))
      else if (is.numeric(d_RFSTDTC)) {
        # could be SAS date (days since 1960-01-01) or Excel date; haven labelled? Assume SAS date if values ~20000-30000?
        # Heuristic: if file is xpt/sas7bdat, numeric is SAS date
        if (grepl("(?i)\\.(xpt|sas7bdat)$", found_dm_file)) {
          d2 <- suppressWarnings(as.Date(d_RFSTDTC, origin = "1960-01-01"))
          rf_iso <- ifelse(is.na(d2), NA_character_, format(d2, "%Y-%m-%d"))
        } else {
          d2 <- suppressWarnings(as.Date(d_RFSTDTC, origin = "1899-12-30"))
          rf_iso <- ifelse(is.na(d2), NA_character_, format(d2, "%Y-%m-%d"))
        }
      } else rf_iso <- raw_vec_to_iso(d_RFSTDTC)
      dm_for_ae <- tibble::tibble(USUBJID = sas_trunc(as.character(d_USUBJID), 25),
                                  RFSTDTC = as.character(rf_iso))
      # SAS: RFSTDTC ne "" check -> keep as "" vs NA? Use NA for missing; handle both later
      dm_for_ae$RFSTDTC[is.na(dm_for_ae$RFSTDTC)] <- NA_character_
      # sort by USUBJID (missing first, ASCII)
      dm_for_ae <- dm_for_ae %>% arrange(ifelse(is.na(USUBJID), "", USUBJID))
      # dedupe to one per USUBJID (keep first) to mimic one-to-many merge
      dm_for_ae <- dm_for_ae %>% distinct(USUBJID, .keep_all = TRUE)
      message(paste0("dm_for_ae from file n=", nrow(dm_for_ae)))
    }
  }
}

if (is.null(dm_for_ae)) {
  message("Deriving dm_for_ae from edc_dm / edc_ex fallback ...")
  edc_dm <- tryCatch(readxl::read_excel(raw_dm_path, .name_repair = "minimal"), error = function(e) NULL)
  if (!is.null(edc_dm)) names(edc_dm) <- trimws(names(edc_dm))
  edc_ex <- tryCatch(readxl::read_excel(raw_ex_path, .name_repair = "minimal"), error = function(e) NULL)
  if (!is.null(edc_ex)) names(edc_ex) <- trimws(names(edc_ex))
  # USUBJID from edc_dm SUBJID if available, else from edc_ae distinct SUBJID
  dm_subjid <- NULL
  dm_rf <- NULL
  if (!is.null(edc_dm)) {
    message(paste0("edc_dm cols: ", paste(names(edc_dm), collapse=", ")))
    dm_subjid <- get_col(edc_dm, "SUBJID")
    # try RFSTDTC variants
    for (nm in c("RFSTDTC","RFSTDT","RFSTDAT","RANDDT","RANDDTC","ENRDT","ENRDTC","STARTDT","STDT","EXSTDT","TRTSDT","TRTSTDTC")) {
      v <- get_col(edc_dm, nm)
      if (!is.null(v)) { dm_rf <- v; message(paste0("Using edc_dm col for RFSTDTC: ", nm)); break }
    }
    # also try any column containing RFST
    if (is.null(dm_rf)) {
      for (cn in names(edc_dm)) {
        if (grepl("(?i)RFST", cn)) { dm_rf <- edc_dm[[cn]]; message(paste0("Using edc_dm col for RFSTDTC (fuzzy): ", cn)); break }
      }
    }
  }
  if (is.null(dm_subjid)) {
    dm_subjid <- unique(as.character(c_SUBJID))
  }
  # If RF still null, try edc_ex min date per subject
  ex_map <- NULL
  if (is.null(dm_rf) && !is.null(edc_ex)) {
    message(paste0("edc_ex cols: ", paste(names(edc_ex), collapse=", ")))
    ex_sub <- get_col(edc_ex, "SUBJID")
    # find date column in ex
    ex_date_vec <- NULL
    ex_date_name <- NA
    for (nm in c("EXSTDTC","EXSTDT","STDT","STARTDT","EXDT","DATE","EXSTDAT","TRTSDT")) {
      v <- get_col(edc_ex, nm)
      if (!is.null(v)) { ex_date_vec <- v; ex_date_name <- nm; break }
    }
    if (is.null(ex_date_vec)) {
      # pick first date-like column (contains DT or DATE)
      for (cn in names(edc_ex)) {
        if (grepl("(?i)(DT|DATE)", cn)) { ex_date_vec <- edc_ex[[cn]]; ex_date_name <- cn; break }
      }
    }
    if (!is.null(ex_sub) && !is.null(ex_date_vec)) {
      message(paste0("Deriving RFSTDTC from edc_ex col: ", ex_date_name))
      ex_iso <- raw_vec_to_iso(ex_date_vec)
      ex_df <- tibble::tibble(SUBJID_tmp = as.character(ex_sub), EXISO = ex_iso)
      ex_df <- ex_df %>% filter(!is.na(SUBJID_tmp) & trimws(SUBJID_tmp) != "" & !is.na(EXISO) & EXISO != "")
      if (nrow(ex_df) > 0) {
        ex_map <- ex_df %>% group_by(SUBJID_tmp) %>% summarise(MINDT = min(EXISO), .groups = "drop")
      }
    }
  }
  # Build dm_for_ae
  usub <- sas_catx("-", "ONCO-2026", as.character(dm_subjid))
  usub <- sas_trunc(usub, 25)
  if (!is.null(dm_rf)) {
    # dm_rf aligned with edc_dm rows; need to handle length mismatch if dm_subjid from elsewhere? Here dm_subjid from edc_dm so aligned
    rf_iso2 <- raw_vec_to_iso(dm_rf)
    dm_for_ae <- tibble::tibble(USUBJID = as.character(usub), RFSTDTC = as.character(rf_iso2))
  } else if (!is.null(ex_map)) {
    base <- tibble::tibble(SUBJID_tmp = as.character(dm_subjid))
    base$USUBJID <- sas_catx("-", "ONCO-2026", base$SUBJID_tmp)
    base$USUBJID <- sas_trunc(base$USUBJID, 25)
    base <- base %>% left_join(ex_map, by = "SUBJID_tmp")
    dm_for_ae <- tibble::tibble(USUBJID = base$USUBJID, RFSTDTC = base$MINDT)
  } else {
    # No RF source: all missing
    dm_for_ae <- tibble::tibble(USUBJID = as.character(usub), RFSTDTC = NA_character_)
  }
  dm_for_ae$RFSTDTC[is.na(dm_for_ae$RFSTDTC)] <- NA_character_
  dm_for_ae <- dm_for_ae %>% arrange(ifelse(is.na(USUBJID), "", USUBJID)) %>% distinct(USUBJID, .keep_all = TRUE)
  message(paste0("dm_for_ae fallback n=", nrow(dm_for_ae)))
}

# ---------- 2. Process Raw AE Data (ae_prep) ----------
# USUBJID = catx("-", "ONCO-2026", SUBJID); AESTDTC/AEENDTC ISO
usub_ae <- sas_trunc(sas_catx("-", "ONCO-2026", as.character(c_SUBJID)), 25)
aestdtc <- raw_vec_to_iso(c_STDT)
aeendtc <- raw_vec_to_iso(c_ENDT)
# SAS: if STDT ne "" then AESTDTC=... ; else missing ("" ). Our raw_vec_to_iso gives NA for missing -> keep NA, later treat "" and NA same
# Length $19 truncation (ISO 10 chars, but keep 19 for datetime compat)
aestdtc <- sas_trunc(aestdtc, 19)
aeendtc <- sas_trunc(aeendtc, 19)

ae_prep <- tibble::tibble(
  USUBJID = usub_ae,
  AESTDTC = aestdtc,
  AEENDTC = aeendtc,
  SUBJID_raw = as.character(c_SUBJID),
  TERM = as.character(c_TERM),
  DECOD = as.character(c_DECOD),
  BODSYS = as.character(c_BODSYS),
  SEV = as.character(c_SEV),
  TXGR_raw = c_TXGR,
  REL = as.character(c_REL),
  OUT = as.character(c_OUT),
  SER = as.character(c_SER)
)

# ---------- 3. Sort AE Data by USUBJID AESTDTC TERM (SAS: missing first, ASCII) ----------
ae_prep <- ae_prep %>% arrange(
  ifelse(is.na(USUBJID), "", USUBJID),
  ifelse(is.na(AESTDTC), "", AESTDTC),
  ifelse(is.na(TERM), "", TERM)
)

# ---------- 4. Final Mapping and Derivation (sdtm_ae1) ----------
# Merge ae_prep(in=a) dm_for_ae by USUBJID (left join)
sdtm_ae1 <- ae_prep %>% left_join(dm_for_ae, by = "USUBJID")

# Identifier + AE variables with LENGTH truncation
sdtm_ae1 <- sdtm_ae1 %>% mutate(
  STUDYID = sas_trunc("ONCO-2026", 10),
  DOMAIN = sas_trunc("AE", 2),
  USUBJID = sas_trunc(USUBJID, 25),
  AETERM = sas_trunc(TERM, 40),
  AEDECOD = sas_trunc(sas_upcase(DECOD), 40),
  AEBODSYS = sas_trunc(sas_tranwrd(BODSYS, "_", " "), 40),
  AESEV = sas_trunc(sas_upcase(SEV), 10),
  AETXGR = suppressWarnings(as.numeric(TXGR_raw)),
  AEREL = sas_trunc(sas_upcase(REL), 20),
  AEOUT = sas_trunc(sas_upcase(OUT), 20),
  AESER = sas_trunc(SER, 1)
)

# Derive AESTDY / AEENDY
stdt_d <- iso_to_date(sdtm_ae1$AESTDTC)
rf_d1  <- iso_to_date(sdtm_ae1$RFSTDTC)
endt_d <- iso_to_date(sdtm_ae1$AEENDTC)
rf_d2  <- rf_d1

AESTDY <- rep(NA_real_, nrow(sdtm_ae1))
AEENDY <- rep(NA_real_, nrow(sdtm_ae1))
for (i in seq_len(nrow(sdtm_ae1))) {
  a <- sdtm_ae1$AESTDTC[i]; r <- sdtm_ae1$RFSTDTC[i]
  a_miss <- is.na(a) || trimws(a) == ""
  r_miss <- is.na(r) || trimws(r) == ""
  if (!a_miss && !r_miss && !is.na(stdt_d[i]) && !is.na(rf_d1[i])) {
    diff <- as.numeric(stdt_d[i] - rf_d1[i])
    AESTDY[i] <- ifelse(stdt_d[i] >= rf_d1[i], diff + 1, diff)
  }
  ae2 <- sdtm_ae1$AEENDTC[i]
  ae_miss <- is.na(ae2) || trimws(ae2) == ""
  if (!ae_miss && !r_miss && !is.na(endt_d[i]) && !is.na(rf_d2[i])) {
    diff2 <- as.numeric(endt_d[i] - rf_d2[i])
    AEENDY[i] <- ifelse(endt_d[i] >= rf_d2[i], diff2 + 1, diff2)
  }
}
sdtm_ae1$AESTDY <- AESTDY
sdtm_ae1$AEENDY <- AEENDY
# Truncate DTCs to $19 (already)
sdtm_ae1$AESTDTC <- sas_trunc(sdtm_ae1$AESTDTC, 19)
sdtm_ae1$AEENDTC <- sas_trunc(sdtm_ae1$AEENDTC, 19)

# ---------- 5. Sort Final AE Dataset by USUBJID AESTDTC TERM ----------
sdtm_ae1 <- sdtm_ae1 %>% arrange(
  ifelse(is.na(USUBJID), "", USUBJID),
  ifelse(is.na(AESTDTC), "", AESTDTC),
  ifelse(is.na(TERM), "", TERM)
)

# ---------- 6. Derive AESEQ (by USUBJID) ----------
sdtm_ae1 <- sdtm_ae1 %>% group_by(USUBJID) %>% mutate(AESEQ = dplyr::row_number()) %>% ungroup()
# Ensure AESEQ restarts per USUBJID including NA group? SAS first.USUBJID with missing sorts first as one group - row_number per group handles (NA group together). Good.

# Keep Final SDTM Variables in order
sdtm_ae <- sdtm_ae1 %>% transmute(
  STUDYID = sas_trunc(STUDYID, 10),
  DOMAIN = sas_trunc(DOMAIN, 2),
  USUBJID = sas_trunc(USUBJID, 25),
  AESEQ = as.numeric(AESEQ),
  AETERM = sas_trunc(AETERM, 40),
  AEDECOD = sas_trunc(AEDECOD, 40),
  AEBODSYS = sas_trunc(AEBODSYS, 40),
  AESEV = sas_trunc(AESEV, 10),
  AETXGR = as.numeric(AETXGR),
  AEREL = sas_trunc(AEREL, 20),
  AEOUT = sas_trunc(AEOUT, 20),
  AESER = sas_trunc(AESER, 1),
  AESTDTC = sas_trunc(AESTDTC, 19),
  AEENDTC = sas_trunc(AEENDTC, 19),
  AESTDY = as.numeric(AESTDY),
  AEENDY = as.numeric(AEENDY)
) %>% arrange(
  ifelse(is.na(USUBJID), "", USUBJID),
  ifelse(is.na(AESTDTC), "", AESTDTC),
  ifelse(is.na(AETERM), "", AETERM)
)

# ---------- 7/8. PROC PRINT + Export ----------
# Write dataset ae.csv
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# Build text rendering of PROC PRINT (all obs)
# SAS PROC PRINT prints Obs + all variables
print_df <- as.data.frame(sdtm_ae)
print_df_display <- print_df
# For display, replace NA with "" (SAS missing shows blank)
print_df_display[is.na(print_df_display)] <- ""

# Create listing text
hdr <- c("PROC PRINT of SDTM_AE (SDTM AE MAPPING & DERIVATION)", paste0("Rows: ", nrow(print_df_display)))
col_line <- paste(c("Obs", names(print_df_display)), collapse = " | ")
sep_line <- paste(rep("-", nchar(col_line)), collapse = "")
body_lines <- c()
for (i in seq_len(nrow(print_df_display))) {
  vals <- c(as.character(i), as.character(unlist(print_df_display[i, , drop = TRUE])))
  # vals already character; keep numeric formatting unrounded (as stored)
  body_lines <- c(body_lines, paste(vals, collapse = " | "))
}
if (length(body_lines) == 0) body_lines <- "(no observations)"
txt_lines <- c(hdr, "", col_line, sep_line, body_lines, "", paste0("Total observations: ", nrow(print_df_display)))
txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst")
writeLines(txt_lines, txt_path)
writeLines(txt_lines, lst_path)

# CSV of table body (same as listing body, with Obs)
p022_body <- data.frame(Obs = seq_len(nrow(print_df)), print_df, check.names = FALSE, stringsAsFactors = FALSE)
write.csv(p022_body, file.path(out, "P022.csv"), row.names = FALSE, na = "")

message(paste0("Done. ae n=", nrow(sdtm_ae), " -> ", file.path(out, "ae.csv")))
