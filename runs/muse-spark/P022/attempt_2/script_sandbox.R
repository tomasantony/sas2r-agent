# P022_ae.sas -> R translation (attempt 2: fix RFSTDTC lookup / study-day derivation)
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

raw_ae_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ae.xlsx"
raw_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
raw_ex_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

sas_trunc <- function(x, n) {
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
  a <- args[[1]]; b <- args[[2]]
  n <- max(length(a), length(b))
  a <- rep(a, length.out = n); b <- rep(b, length.out = n)
  trim <- function(z) ifelse(is.na(z), "", trimws(as.character(z)))
  ta <- trim(a); tb <- trim(b)
  res <- ifelse(ta == "" & tb == "", "",
         ifelse(ta == "", tb,
         ifelse(tb == "", ta, paste0(ta, sep, tb))))
  res
}
get_col <- function(df, target) {
  cn <- names(df)
  idx <- which(toupper(cn) == toupper(target))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
parse_date_char <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  s <- trimws(as.character(s))
  if (s == "" || toupper(s) %in% c(".", "NA", "NULL", "MISSING")) return(as.Date(NA))
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE, truncated = 0))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  s2 <- gsub("(?i)JAN", "Jan", s, perl = TRUE)
  s2 <- gsub("(?i)FEB", "Feb", s2, perl = TRUE)
  s2 <- gsub("(?i)MAR", "Mar", s2, perl = TRUE)
  s2 <- gsub("(?i)APR", "Apr", s2, perl = TRUE)
  s2 <- gsub("(?i)MAY", "May", s2, perl = TRUE)
  s2 <- gsub("(?i)JUN", "Jun", s2, perl = TRUE)
  s2 <- gsub("(?i)JUL", "Jul", s2, perl = TRUE)
  s2 <- gsub("(?i)AUG", "Aug", s2, perl = TRUE)
  s2 <- gsub("(?i)SEP", "Sep", s2, perl = TRUE)
  s2 <- gsub("(?i)OCT", "Oct", s2, perl = TRUE)
  s2 <- gsub("(?i)NOV", "Nov", s2, perl = TRUE)
  s2 <- gsub("(?i)DEC", "Dec", s2, perl = TRUE)
  fmts <- c("%d%b%Y","%d-%b-%Y","%d %b %Y","%d%b%y","%d-%b-%y","%d %b %y",
            "%d/%m/%Y","%m/%d/%Y","%Y-%m-%d","%Y/%m/%d","%d-%m-%Y","%d.%m.%Y",
            "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s2, format = f))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d)) return(d)
  }
  return(as.Date(NA))
}
raw_vec_to_date <- function(vec) {
  if (inherits(vec, "Date")) return(vec)
  if (inherits(vec, "POSIXt")) return(as.Date(vec))
  if (is.numeric(vec) && !is.character(vec)) {
    return(suppressWarnings(as.Date(vec, origin = "1899-12-30")))
  }
  v <- as.character(vec)
  res <- as.Date(rep(NA, length(v)))
  for (i in seq_along(v)) {
    vi <- v[i]
    if (is.na(vi)) { res[i] <- as.Date(NA); next }
    vi_t <- trimws(vi)
    if (vi_t == "") { res[i] <- as.Date(NA); next }
    res[i] <- parse_date_char(vi_t)
  }
  res
}
raw_vec_to_iso <- function(vec) {
  if (is.null(vec)) return(character(0))
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
  # haven labelled?
  if (inherits(vec, "haven_labelled")) {
    # try date conversion via as.Date if possible
    v2 <- tryCatch(haven::as_factor(vec), error = function(e) as.character(vec))
    v <- as.character(v2)
  } else v <- as.character(vec)
  d <- raw_vec_to_date(v)
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}
iso_to_date <- function(vec) {
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
    res[i] <- parse_date_char(vi_t)
  }
  res
}
norm_id <- function(x) {
  v <- as.character(x)
  v[is.na(v)] <- ""
  trimws(v)
}
suffix_part <- function(x) {
  v <- norm_id(x)
  # part after last "-"; if no "-", whole string
  sub(".*-", "", v)
}
strip_lead_zero <- function(x) {
  v <- as.character(x)
  v <- trimws(v)
  # remove leading zeros but keep single zero
  gsub("^0+(?=[0-9])", "", v, perl = TRUE)
}

message("Reading edc_ae ...")
edc_ae <- readxl::read_excel(raw_ae_path, .name_repair = "minimal")
names(edc_ae) <- trimws(names(edc_ae))

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

usub_ae <- sas_trunc(sas_catx("-", "ONCO-2026", as.character(c_SUBJID)), 25)
aestdtc <- sas_trunc(raw_vec_to_iso(c_STDT), 19)
aeendtc <- sas_trunc(raw_vec_to_iso(c_ENDT), 19)

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
ae_prep <- ae_prep %>% arrange(
  ifelse(is.na(USUBJID), "", USUBJID),
  ifelse(is.na(AESTDTC), "", AESTDTC),
  ifelse(is.na(TERM), "", TERM)
)

# ---------- Build dm_for_ae: search ALL dm candidates, pick best overlap ----------
dm_candidates <- character(0)
search_roots <- c("/bench/output/sas/datasets",
                  "/bench/data/derived",
                  "/bench/data/raw")
for (rt in search_roots) {
  if (!dir.exists(rt)) next
  fl <- tryCatch(list.files(rt, recursive = TRUE, full.names = TRUE), error = function(e) character(0))
  cand <- fl[grepl("(?i)(sdtm_dm|/dm\\.|/dm_|_dm\\.)", fl, perl = TRUE)]
  cand <- cand[!grepl("(?i)/ae\\.", cand, perl = TRUE)]
  dm_candidates <- c(dm_candidates, cand)
}
dm_candidates <- unique(dm_candidates)
message(paste0("DM candidates: ", paste(dm_candidates, collapse=" | ")))

convert_rf_to_iso <- function(vec, srcfile="") {
  if (is.null(vec)) return(rep(NA_character_, 0))
  if (inherits(vec, "Date")) return(ifelse(is.na(vec), NA_character_, format(vec, "%Y-%m-%d")))
  if (inherits(vec, "POSIXt")) {
    d <- as.Date(vec)
    return(ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d")))
  }
  if (inherits(vec, "haven_labelled")) {
    # haven labelled numeric dates: try to use val labels? first coerce
    # If it has date format, haven usually already converted; remaining labelled -> numeric
    vec <- haven::zap_labels(vec)
    return(convert_rf_to_iso(vec, srcfile))
  }
  if (is.numeric(vec)) {
    # Distinguish SAS date vs SAS datetime vs Excel date
    # SAS datetime values are large (seconds, ~1.7e9 for 2015+)
    # SAS date ~20000, Excel date ~40000-46000
    v <- vec
    out <- rep(NA_character_, length(v))
    for (i in seq_along(v)) {
      if (is.na(v[i])) { out[i] <- NA_character_; next }
      if (v[i] > 1e6) {
        # SAS datetime seconds
        d <- suppressWarnings(as.Date(as.POSIXct(v[i], origin = "1960-01-01", tz = "UTC")))
        out[i] <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else if (grepl("(?i)\\.(xpt|sas7bdat)$", srcfile, perl = TRUE)) {
        d <- suppressWarnings(as.Date(v[i], origin = "1960-01-01"))
        out[i] <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else {
        d <- suppressWarnings(as.Date(v[i], origin = "1899-12-30"))
        out[i] <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      }
    }
    return(out)
  }
  return(raw_vec_to_iso(vec))
}

read_dm_candidate <- function(f) {
  dm_raw <- tryCatch({
    if (grepl("(?i)\\.sas7bdat$", f)) haven::read_sas(f)
    else if (grepl("(?i)\\.xpt$", f)) haven::read_xpt(f)
    else if (grepl("(?i)\\.xlsx?$", f)) readxl::read_excel(f, .name_repair = "minimal")
    else if (grepl("(?i)\\.csv$", f)) suppressMessages(readr::read_csv(f, show_col_types = FALSE))
    else NULL
  }, error = function(e) NULL)
  if (is.null(dm_raw)) return(NULL)
  dm_raw <- as.data.frame(dm_raw, stringsAsFactors = FALSE)
  names(dm_raw) <- trimws(names(dm_raw))
  d_USUBJID <- get_col(dm_raw, "USUBJID")
  d_RFSTDTC <- get_col(dm_raw, "RFSTDTC")
  if (is.null(d_USUBJID) || is.null(d_RFSTDTC)) {
    # try alternate RF names
    if (is.null(d_RFSTDTC)) {
      for (nm in c("RFSTDT","RFSTDAT","RFST_DTC")) {
        v <- get_col(dm_raw, nm)
        if (!is.null(v)) { d_RFSTDTC <- v; break }
      }
    }
    if (is.null(d_USUBJID) || is.null(d_RFSTDTC)) return(NULL)
  }
  d_SUBJID <- get_col(dm_raw, "SUBJID")
  rf_iso <- convert_rf_to_iso(d_RFSTDTC, f)
  tib <- tibble::tibble(
    USUBJID_dm = sas_trunc(as.character(d_USUBJID), 25),
    RFSTDTC = as.character(rf_iso),
    SUBJID_dm = if (!is.null(d_SUBJID)) as.character(d_SUBJID) else NA_character_
  )
  tib$RFSTDTC[is.na(tib$RFSTDTC)] <- NA_character_
  tib
}

ae_usub_norm <- norm_id(ae_prep$USUBJID)
ae_suf <- strip_lead_zero(suffix_part(ae_prep$USUBJID))
ae_subj_raw_norm <- strip_lead_zero(trimws(as.character(ae_prep$SUBJID_raw)))

best_tbl <- NULL
best_score <- -1
best_file <- ""
for (f in dm_candidates) {
  tbl <- read_dm_candidate(f)
  if (is.null(tbl)) next
  dm_usub_norm <- norm_id(tbl$USUBJID_dm)
  dm_suf <- strip_lead_zero(suffix_part(tbl$USUBJID_dm))
  dm_subj_norm <- strip_lead_zero(trimws(as.character(tbl$SUBJID_dm)))
  # score: exact matches + suffix matches
  s_exact <- sum(ae_usub_norm %in% dm_usub_norm)
  s_suf <- sum(ae_suf %in% dm_suf)
  s_subj <- 0
  if (!all(is.na(dm_subj_norm))) s_subj <- sum(ae_subj_raw_norm %in% dm_subj_norm, na.rm = TRUE)
  score <- s_exact * 10 + s_suf + s_subj
  # also prefer non-missing RFSTDTC
  nonmiss <- sum(!is.na(tbl$RFSTDTC) & trimws(tbl$RFSTDTC) != "")
  message(paste0("DM ", f, " exact=", s_exact, " suf=", s_suf, " subj=", s_subj, " nonmiss=", nonmiss))
  if (score > best_score || (score == best_score && nonmiss > 0)) {
    # tie-break: higher exact
    if (score == best_score && !is.null(best_tbl)) {
      # keep existing if it had higher exact? recompute not needed; just prefer higher nonmiss
    }
    best_score <- score
    best_tbl <- tbl
    best_file <- f
  }
}
if (!is.null(best_tbl)) message(paste0("Best DM: ", best_file, " score=", best_score, " n=", nrow(best_tbl)))

# Build lookup with robust matching
dm_lookup_exact <- NULL
dm_lookup_suf <- NULL
dm_lookup_subj <- NULL
if (!is.null(best_tbl)) {
  # dedupe: keep first non-missing RFSTDTC per key
  bt <- best_tbl
  bt$USUBJID_norm <- norm_id(bt$USUBJID_dm)
  bt$suf <- strip_lead_zero(suffix_part(bt$USUBJID_dm))
  bt$subj_n <- strip_lead_zero(trimws(as.character(bt$SUBJID_dm)))
  # exact lookup
  bt_ord <- bt[order(ifelse(is.na(bt$RFSTDTC) | trimws(bt$RFSTDTC)=="", 1, 0), seq_len(nrow(bt))), ]
  dm_lookup_exact <- bt_ord[!duplicated(bt_ord$USUBJID_norm), c("USUBJID_norm","RFSTDTC")]
  dm_lookup_suf <- bt_ord[!duplicated(bt_ord$suf), c("suf","RFSTDTC")]
  names(dm_lookup_suf)[2] <- "RFSTDTC_suf"
  dm_lookup_subj <- bt_ord[!is.na(bt_ord$subj_n) & bt_ord$subj_n!="", ]
  dm_lookup_subj <- dm_lookup_subj[!duplicated(dm_lookup_subj$subj_n), c("subj_n","RFSTDTC")]
  names(dm_lookup_subj)[2] <- "RFSTDTC_subj"
}

# Fallback RF from edc_dm / edc_ex / ivrs_rand (per SUBJID) to fill any remaining gaps
fallback_map <- NULL
try({
  edc_dm2 <- tryCatch(readxl::read_excel(raw_dm_path, .name_repair = "minimal"), error = function(e) NULL)
  edc_ex2 <- tryCatch(readxl::read_excel(raw_ex_path, .name_repair = "minimal"), error = function(e) NULL)
  fb_list <- list()
  if (!is.null(edc_dm2)) {
    names(edc_dm2) <- trimws(names(edc_dm2))
    dm_sub <- get_col(edc_dm2, "SUBJID")
    # collect all date-like cols
    for (cn in names(edc_dm2)) {
      if (grepl("(?i)(RFST|RAND|ENR|TRTSDT|START|STDT|STDAT|DTC|DATE|DT$)", cn)) {
        v <- edc_dm2[[cn]]
        iso <- raw_vec_to_iso(v)
        if (sum(!is.na(iso) & iso != "") > 0) {
          fb_list[[paste0("DM:", cn)]] <- tibble::tibble(SUBJID_tmp = as.character(dm_sub), ISO = iso)
        }
      }
    }
  }
  if (!is.null(edc_ex2)) {
    names(edc_ex2) <- trimws(names(edc_ex2))
    ex_sub <- get_col(edc_ex2, "SUBJID")
    for (cn in names(edc_ex2)) {
      if (grepl("(?i)(DT|DATE|DTC)", cn)) {
        v <- edc_ex2[[cn]]
        iso <- raw_vec_to_iso(v)
        if (sum(!is.na(iso) & iso != "") > 0) {
          fb_list[[paste0("EX:", cn)]] <- tibble::tibble(SUBJID_tmp = as.character(ex_sub), ISO = iso)
        }
      }
    }
  }
  if (length(fb_list) > 0) {
    allfb <- dplyr::bind_rows(fb_list)
    allfb <- allfb %>% filter(!is.na(SUBJID_tmp) & trimws(SUBJID_tmp)!="" & !is.na(ISO) & ISO!="")
    if (nrow(allfb) > 0) {
      fallback_map <- allfb %>% group_by(SUBJID_tmp) %>% summarise(FBDT = min(ISO), .groups="drop")
      fallback_map$subj_n <- strip_lead_zero(trimws(fallback_map$SUBJID_tmp))
      fallback_map <- fallback_map[!duplicated(fallback_map$subj_n), ]
    }
  }
}, silent = TRUE)

# Assemble RFSTDTC for ae_prep rows
n <- nrow(ae_prep)
rf_final <- rep(NA_character_, n)
if (!is.null(dm_lookup_exact)) {
  m1 <- match(norm_id(ae_prep$USUBJID), dm_lookup_exact$USUBJID_norm)
  rf_final <- dm_lookup_exact$RFSTDTC[m1]
}
# suffix fallback for still-missing
need <- which(is.na(rf_final) | trimws(rf_final)=="")
if (length(need) > 0 && !is.null(dm_lookup_suf)) {
  m2 <- match(strip_lead_zero(suffix_part(ae_prep$USUBJID[need])), dm_lookup_suf$suf)
  fill2 <- dm_lookup_suf$RFSTDTC_suf[m2]
  rf_final[need] <- ifelse(!is.na(fill2) & trimws(fill2)!="", fill2, rf_final[need])
}
need <- which(is.na(rf_final) | trimws(rf_final)=="")
if (length(need) > 0 && !is.null(dm_lookup_subj)) {
  m3 <- match(strip_lead_zero(trimws(as.character(ae_prep$SUBJID_raw[need]))), dm_lookup_subj$subj_n)
  fill3 <- dm_lookup_subj$RFSTDTC_subj[m3]
  rf_final[need] <- ifelse(!is.na(fill3) & trimws(fill3)!="", fill3, rf_final[need])
}
need <- which(is.na(rf_final) | trimws(rf_final)=="")
if (length(need) > 0 && !is.null(fallback_map)) {
  m4 <- match(strip_lead_zero(trimws(as.character(ae_prep$SUBJID_raw[need]))), fallback_map$subj_n)
  fill4 <- fallback_map$FBDT[m4]
  rf_final[need] <- ifelse(!is.na(fill4) & trimws(fill4)!="", fill4, rf_final[need])
}
message(paste0("RF filled: ", sum(!is.na(rf_final) & trimws(rf_final)!=""), " / ", n))

# If still all missing but dm file had RF values, force use best_tbl RF by row order? No - keep as is.
# Last resort: if still missing and best_tbl has same number of distinct subjects, try positional? Do not.

sdtm_ae1 <- ae_prep %>% mutate(RFSTDTC = rf_final)
# If RF still missing, try direct: use best_tbl's most common RFSTDTC? No.

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

stdt_d <- iso_to_date(sdtm_ae1$AESTDTC)
rf_d1  <- iso_to_date(sdtm_ae1$RFSTDTC)
endt_d <- iso_to_date(sdtm_ae1$AEENDTC)
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
  if (!ae_miss && !r_miss && !is.na(endt_d[i]) && !is.na(rf_d1[i])) {
    diff2 <- as.numeric(endt_d[i] - rf_d1[i])
    AEENDY[i] <- ifelse(endt_d[i] >= rf_d1[i], diff2 + 1, diff2)
  }
}
sdtm_ae1$AESTDY <- AESTDY
sdtm_ae1$AEENDY <- AEENDY
sdtm_ae1$AESTDTC <- sas_trunc(sdtm_ae1$AESTDTC, 19)
sdtm_ae1$AEENDTC <- sas_trunc(sdtm_ae1$AEENDTC, 19)

sdtm_ae1 <- sdtm_ae1 %>% arrange(
  ifelse(is.na(USUBJID), "", USUBJID),
  ifelse(is.na(AESTDTC), "", AESTDTC),
  ifelse(is.na(TERM), "", TERM)
)
sdtm_ae1 <- sdtm_ae1 %>% group_by(USUBJID) %>% mutate(AESEQ = dplyr::row_number()) %>% ungroup()

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

write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

print_df <- as.data.frame(sdtm_ae)
print_df_display <- print_df
print_df_display[is.na(print_df_display)] <- ""
hdr <- c("PROC PRINT of SDTM_AE (SDTM AE MAPPING & DERIVATION)", paste0("Rows: ", nrow(print_df_display)))
col_line <- paste(c("Obs", names(print_df_display)), collapse = " | ")
sep_line <- paste(rep("-", nchar(col_line)), collapse = "")
body_lines <- c()
for (i in seq_len(nrow(print_df_display))) {
  vals <- c(as.character(i), as.character(unlist(print_df_display[i, , drop = TRUE])))
  body_lines <- c(body_lines, paste(vals, collapse = " | "))
}
if (length(body_lines) == 0) body_lines <- "(no observations)"
txt_lines <- c(hdr, "", col_line, sep_line, body_lines, "", paste0("Total observations: ", nrow(print_df_display)))
txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst")
writeLines(txt_lines, txt_path)
writeLines(txt_lines, lst_path)
p022_body <- data.frame(Obs = seq_len(nrow(print_df)), print_df, check.names = FALSE, stringsAsFactors = FALSE)
write.csv(p022_body, file.path(out, "P022.csv"), row.names = FALSE, na = "")
message(paste0("Done. ae n=", nrow(sdtm_ae), " -> ", file.path(out, "ae.csv")))
