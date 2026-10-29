out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(lubridate)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_best_num <- function(x) {
  vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(NA_real_)
    v2 <- trimws(as.character(v))
    if (v2 == "" || v2 == ".") return(NA_real_)
    suppressWarnings(as.numeric(v2))
  }, numeric(1))
}

to_char_trim <- function(x) {
  vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(NA_character_)
    if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
    if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d"))
    trimws(as.character(v))
  }, character(1))
}

get_raw_col <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(rep(NA, nrow(df)))
  df[[idx[1]]]
}

parse_to_date <- function(x) {
  # return Date vector
  vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(NA_real_)
    if (inherits(v, "Date")) return(as.numeric(v))
    if (inherits(v, "POSIXt")) return(as.numeric(as.Date(v)))
    v2 <- trimws(as.character(v))
    if (v2 == "" || v2 == ".") return(NA_real_)
    d <- suppressWarnings(lubridate::dmy(v2, quiet = TRUE))
    if (!is.na(d)) return(as.numeric(d))
    d2 <- suppressWarnings(lubridate::ymd(v2, quiet = TRUE))
    if (!is.na(d2)) return(as.numeric(d2))
    # try yymmdd10 explicitly
    d3 <- suppressWarnings(as.Date(v2, format = "%Y-%m-%d"))
    if (!is.na(d3)) return(as.numeric(d3))
    return(NA_real_)
  }, numeric(1))
}

fmt_yymmdd10 <- function(d) {
  # d is Date
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

message("Reading edc_vs")
raw_vs_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_vs.xlsx"
raw_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
raw_rand_path <- "/bench/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

edc_vs_raw <- readxl::read_excel(raw_vs_path, .name_repair = "minimal")
# normalize names to upper for lookup (keep data)
names(edc_vs_raw) <- trimws(as.character(names(edc_vs_raw)))

n_vs <- nrow(edc_vs_raw)
SUBJID_raw  <- get_raw_col(edc_vs_raw, "SUBJID")
TESTCD_raw  <- get_raw_col(edc_vs_raw, "TESTCD")
ORRES_raw   <- get_raw_col(edc_vs_raw, "ORRES")
UNIT_raw    <- get_raw_col(edc_vs_raw, "UNIT")
VISIT_raw   <- get_raw_col(edc_vs_raw, "VISIT")
POS_raw     <- get_raw_col(edc_vs_raw, "POS")
VSDT_raw    <- get_raw_col(edc_vs_raw, "VSDT")

SUBJID_c <- to_char_trim(SUBJID_raw)
TESTCD_c <- to_char_trim(TESTCD_raw)
ORRES_c  <- to_char_trim(ORRES_raw)
UNIT_c   <- to_char_trim(UNIT_raw)
VISIT_c  <- to_char_trim(VISIT_raw)
POS_c    <- to_char_trim(POS_raw)
# VSDT keep raw for date parsing (could be Date type)
# also char version
VSDT_c   <- vapply(seq_along(VSDT_raw), function(i) {
  v <- VSDT_raw[i]
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d"))
  trimws(as.character(v))
}, character(1))

vs_prep <- tibble::tibble(
  SUBJID = SUBJID_c,
  TESTCD = TESTCD_c,
  ORRES = ORRES_c,
  UNIT = UNIT_c,
  VISIT = VISIT_c,
  POS = POS_c,
  VSDT = VSDT_c,
  VSDT_raw = VSDT_raw
)
# USUBJID = catx("-", "ONCO-2026", SUBJID); CATX ignores missing
vs_prep$USUBJID <- vapply(vs_prep$SUBJID, function(s) {
  if (is.na(s) || trimws(s) == "") return("ONCO-2026")
  sc <- trimws(s)
  # truncate to fit $25? "ONCO-2026-" is 10 chars, so SUBJID part max 15 to keep 25
  full <- paste0("ONCO-2026-", sc)
  substr(full, 1, 25)
}, character(1))

# SAS sort by USUBJID (missing first, uppercase before lowercase)
# USUBJID never missing here; use order with na.last=FALSE
ord1 <- order(vs_prep$USUBJID, na.last = FALSE)
vs_prep <- vs_prep[ord1, , drop = FALSE]

message("Preparing dm_for_vs")
# Try to build dm_for_vs from edc_dm (and ivrs_rand fallback)
dm_for_vs <- NULL
try({
  edc_dm_raw <- readxl::read_excel(raw_dm_path, .name_repair = "minimal")
  names(edc_dm_raw) <- trimws(as.character(names(edc_dm_raw)))
  dm_SUBJID_raw <- get_raw_col(edc_dm_raw, "SUBJID")
  dm_SUBJID_c <- to_char_trim(dm_SUBJID_raw)
  dm_USUBJID <- vapply(dm_SUBJID_c, function(s) {
    if (is.na(s) || trimws(s) == "") return("ONCO-2026")
    substr(paste0("ONCO-2026-", trimws(s)), 1, 25)
  }, character(1))
  # RFSTDTC / RFXSTDTC lookup case-insensitive
  rf_raw <- get_raw_col(edc_dm_raw, "RFSTDTC")
  rfx_raw <- get_raw_col(edc_dm_raw, "RFXSTDTC")
  # if RFSTDTC all NA, try alternative names
  all_na <- function(v) all(is.na(v) | trimws(as.character(v)) == "")
  if (all_na(rf_raw)) {
    for (alt in c("RANDDTC","RANDDT","RFSTDT","STDTC","RAND_DT")) {
      cand <- get_raw_col(edc_dm_raw, alt)
      if (!all_na(cand)) { rf_raw <- cand; break }
    }
  }
  # convert to YYYY-MM-DD strings
  rf_char <- vapply(seq_along(rf_raw), function(i) {
    v <- rf_raw[i]
    if (is.na(v)) return(NA_character_)
    if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
    if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d"))
    v2 <- trimws(as.character(v))
    if (v2 == "" || v2 == ".") return(NA_character_)
    # if already YYYY-MM-DD keep, else try parse date11 then format
    d <- suppressWarnings(lubridate::ymd(v2, quiet = TRUE))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    d2 <- suppressWarnings(lubridate::dmy(v2, quiet = TRUE))
    if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    v2
  }, character(1))
  rfx_char <- vapply(seq_along(rfx_raw), function(i) {
    v <- rfx_raw[i]
    if (is.na(v)) return(NA_character_)
    if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
    if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d"))
    v2 <- trimws(as.character(v))
    if (v2 == "" || v2 == ".") return(NA_character_)
    d <- suppressWarnings(lubridate::ymd(v2, quiet = TRUE))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    d2 <- suppressWarnings(lubridate::dmy(v2, quiet = TRUE))
    if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    v2
  }, character(1))
  dm_for_vs <- tibble::tibble(
    USUBJID = dm_USUBJID,
    SUBJID = dm_SUBJID_c,
    RFSTDTC = rf_char,
    RFXSTDTC = rfx_char
  )
}, silent = TRUE)

if (is.null(dm_for_vs)) {
  # fallback: unique USUBJIDs from vs_prep with missing dates
  dm_for_vs <- tibble::tibble(
    USUBJID = unique(vs_prep$USUBJID),
    SUBJID = sub("^ONCO-2026-", "", unique(vs_prep$USUBJID)),
    RFSTDTC = NA_character_,
    RFXSTDTC = NA_character_
  )
}

# If RFSTDTC still all missing, try ivrs_rand for dates
try({
  if (all(is.na(dm_for_vs$RFSTDTC) | dm_for_vs$RFSTDTC == "")) {
    iv_raw <- readxl::read_excel(raw_rand_path, .name_repair = "minimal")
    names(iv_raw) <- trimws(as.character(names(iv_raw)))
    iv_sub <- get_raw_col(iv_raw, "SUBJID")
    iv_sub_c <- to_char_trim(iv_sub)
    # find a date column
    date_col <- NULL
    for (cn in names(iv_raw)) {
      uc <- toupper(cn)
      if (grepl("DATE|DTC|RAND", uc)) { date_col <- iv_raw[[cn]]; break }
    }
    if (!is.null(date_col)) {
      iv_date_c <- vapply(seq_along(date_col), function(i) {
        v <- date_col[i]
        if (is.na(v)) return(NA_character_)
        if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
        if (inherits(v, "POSIXt")) return(format(v, "%Y-%m-%d"))
        v2 <- trimws(as.character(v))
        if (v2 == "") return(NA_character_)
        d <- suppressWarnings(lubridate::ymd(v2, quiet = TRUE))
        if (!is.na(d)) return(format(d, "%Y-%m-%d"))
        d2 <- suppressWarnings(lubridate::dmy(v2, quiet = TRUE))
        if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
        v2
      }, character(1))
      iv_usub <- vapply(iv_sub_c, function(s) {
        if (is.na(s) || trimws(s) == "") return(NA_character_)
        substr(paste0("ONCO-2026-", trimws(s)), 1, 25)
      }, character(1))
      lut <- tibble::tibble(USUBJID = iv_usub, RF = iv_date_c)
      lut <- lut[!is.na(lut$USUBJID) & !is.na(lut$RF) & lut$RF != "", , drop = FALSE]
      lut <- lut[!duplicated(lut$USUBJID), , drop = FALSE]
      if (nrow(lut) > 0) {
        m <- match(dm_for_vs$USUBJID, lut$USUBJID)
        fill <- !is.na(m) & (is.na(dm_for_vs$RFSTDTC) | dm_for_vs$RFSTDTC == "")
        dm_for_vs$RFSTDTC[fill] <- lut$RF[m[fill]]
      }
    }
  }
}, silent = TRUE)

# sort dm_for_vs by USUBJID and dedupe to one per USUBJID (first)
dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID, na.last = FALSE), , drop = FALSE]
dm_for_vs <- dm_for_vs[!duplicated(dm_for_vs$USUBJID), , drop = FALSE]

message("Mapping VS domain")
# merge vs_prep(in=v) dm_for_vs(keep USUBJID RFSTDTC RFXSTDTC) by USUBJID; if v (left join)
dm_keep <- dm_for_vs[, c("USUBJID", "RFSTDTC", "RFXSTDTC"), drop = FALSE]
# left join preserving vs_prep order (will re-sort later anyway)
sdtm_vs1 <- dplyr::left_join(vs_prep, dm_keep, by = "USUBJID")

# Derive fields
STUDYID <- rep("ONCO-2026", nrow(sdtm_vs1))
DOMAIN <- rep("VS", nrow(sdtm_vs1))

VSTESTCD <- substr(ifelse(is.na(sdtm_vs1$TESTCD), "", sdtm_vs1$TESTCD), 1, 8)
VSTESTCD[VSTESTCD == ""] <- NA_character_

VSTEST <- vapply(VSTESTCD, function(v) {
  if (is.na(v)) return(NA_character_)
  if (v == "SYSBP") return("Systolic Blood Pressure")
  else if (v == "DIABP") return("Diastolic Blood Pressure")
  else return(NA_character_)
}, character(1))

VSORRES <- substr(ifelse(is.na(sdtm_vs1$ORRES), "", sdtm_vs1$ORRES), 1, 10)
VSORRES[VSORRES == ""] <- NA_character_
VSORRESU <- substr(ifelse(is.na(sdtm_vs1$UNIT), "", sdtm_vs1$UNIT), 1, 20)
VSORRESU[VSORRESU == ""] <- NA_character_
VSSTRESC <- VSORRES
VSSTRESN <- sas_best_num(sdtm_vs1$ORRES)
VSSTRESU <- VSORRESU

VISIT_der <- vapply(sdtm_vs1$VISIT, function(v) {
  if (is.na(v)) return(NA_character_)
  gsub("_", " ", v, fixed = TRUE)
}, character(1))
VISIT_der <- substr(ifelse(is.na(VISIT_der), "", VISIT_der), 1, 20)
VISIT_der[VISIT_der == ""] <- NA_character_

VSPOS <- vapply(sdtm_vs1$POS, function(v) {
  if (is.na(v)) return(NA_character_)
  toupper(trimws(as.character(v)))
}, character(1))
VSPOS <- substr(ifelse(is.na(VSPOS), "", VSPOS), 1, 10)
VSPOS[VSPOS == ""] <- NA_character_

# VSDTC from VSDT (date11 -> yymmdd10)
VSDTC <- vapply(seq_len(nrow(sdtm_vs1)), function(i) {
  vraw <- sdtm_vs1$VSDT_raw[i]
  vchar <- sdtm_vs1$VSDT[i]
  if (is.na(vchar) || trimws(vchar) == "") return(NA_character_)
  if (inherits(vraw, "Date")) return(format(vraw, "%Y-%m-%d"))
  if (inherits(vraw, "POSIXt")) return(format(as.Date(vraw), "%Y-%m-%d"))
  v2 <- trimws(as.character(vraw))
  if (v2 == "") return(NA_character_)
  d <- suppressWarnings(lubridate::dmy(v2, quiet = TRUE))
  if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  d2 <- suppressWarnings(lubridate::ymd(v2, quiet = TRUE))
  if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
  d3 <- suppressWarnings(as.Date(v2, format = "%Y-%m-%d"))
  if (!is.na(d3)) return(format(d3, "%Y-%m-%d"))
  NA_character_
}, character(1))

# Study Day
RFSTDTC <- sdtm_vs1$RFSTDTC
VSTDY <- vapply(seq_len(nrow(sdtm_vs1)), function(i) {
  a <- VSDTC[i]; b <- RFSTDTC[i]
  if (is.na(a) || a == "" || is.na(b) || b == "") return(NA_real_)
  stdt <- suppressWarnings(as.Date(trimws(a), format = "%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(trimws(b), format = "%Y-%m-%d"))
  if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
  diff <- as.integer(stdt - rfdt)
  if (stdt >= rfdt) diff + 1L else diff
}, numeric(1))

sdtm_vs1 <- tibble::tibble(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = sdtm_vs1$USUBJID,
  VSTESTCD = VSTESTCD,
  VSTEST = VSTEST,
  VSORRES = VSORRES,
  VSORRESU = VSORRESU,
  VSSTRESC = VSSTRESC,
  VSSTRESN = VSSTRESN,
  VSSTRESU = VSSTRESU,
  VSDTC = VSDTC,
  VSTDY = VSTDY,
  VISIT = VISIT_der,
  VSPOS = VSPOS
)

# sort by USUBJID VSDTC (missing first)
ord2 <- order(sdtm_vs1$USUBJID, sdtm_vs1$VSDTC, na.last = FALSE)
sdtm_vs1 <- sdtm_vs1[ord2, , drop = FALSE]

# Derive VSSEQ by USUBJID
sdtm_vs <- sdtm_vs1 %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(VSSEQ = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, VSSEQ,
    VSTESTCD, VSTEST, VSORRES, VSORRESU,
    VSSTRESC, VSSTRESN, VSSTRESU,
    VSDTC, VSTDY, VISIT, VSPOS) %>%
  as.data.frame()

message(paste0("Rows: ", nrow(sdtm_vs)))

# Write datasets
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")

# Text rendering of PROC PRINT
disp <- sdtm_vs
disp$VSSTRESN <- vapply(disp$VSSTRESN, function(v) {
  if (is.na(v)) "" else as.character(v)
}, character(1))
disp$VSTDY <- vapply(disp$VSTDY, function(v) {
  if (is.na(v)) "" else as.character(as.integer(v))
}, character(1))
disp$VSSEQ <- as.character(disp$VSSEQ)
for (cn in names(disp)) {
  disp[[cn]][is.na(disp[[cn]])] <- ""
}
disp_obs <- cbind(Obs = seq_len(nrow(disp)), disp)

txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst")
con_lines <- c(
  "Listing: SDTM VS Domain (P030_vs)",
  paste0("Rows: ", nrow(sdtm_vs)),
  "",
  paste(colnames(disp_obs), collapse = " | ")
)
row_lines <- apply(disp_obs, 1, function(r) paste(r, collapse = " | "))
all_lines <- c(con_lines, row_lines, "", "End of Listing.")
writeLines(all_lines, txt_path)
writeLines(all_lines, lst_path)

message("Done")
