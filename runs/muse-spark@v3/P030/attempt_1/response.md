```r
# P030_vs.sas -> R translation
# SDTM VS mapping & derivation + PROC PRINT listing

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# SAS round half away from zero helper (not needed here but for fidelity)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

edc_vs_path    <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_vs.xlsx"
edc_dm_path    <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_ex_path    <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
ivrs_rand_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

# ---------- helpers ----------
find_col_name <- function(df, candidates) {
  nm <- names(df)
  low <- tolower(nm)
  for (cand in candidates) {
    idx <- which(low == tolower(cand))
    if (length(idx) > 0) return(nm[idx[1]])
  }
  return(NA_character_)
}
get_col_vec <- function(df, candidates) {
  cn <- find_col_name(df, candidates)
  if (is.na(cn)) return(rep(NA_character_, nrow(df)))
  v <- df[[cn]]
  # convert factors / haven labelled to character
  if (inherits(v, "labelled")) v <- haven::as_factor(v)
  as.character(v)
}
get_raw_col <- function(df, candidates) {
  cn <- find_col_name(df, candidates)
  if (is.na(cn)) return(NULL)
  df[[cn]]
}

# normalize month case for %b parsing: "01-JAN-2024" -> "01-Jan-2024"
normalize_b_month <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || trimws(v) == "") return(v)
    m <- regmatches(v, regexpr("[A-Za-z]+", v, perl = TRUE))
    if (length(m) == 0 || m == "") return(v)
    title <- paste0(toupper(substr(m, 1, 1)), tolower(substr(m, 2, nchar(m))))
    sub("[A-Za-z]+", title, v, perl = TRUE)
  }, character(1), USE.NAMES = FALSE)
}

# strict parse character vector with ONE format
parse_char_strict <- function(x, fmt) {
  x <- as.character(x)
  res <- rep(as.Date(NA), length(x))
  idx <- which(!is.na(x) & trimws(x) != "")
  if (length(idx) == 0) return(res)
  vals <- trimws(x[idx])
  if (fmt == "%d-%b-%Y") vals <- normalize_b_month(vals)
  # for yymmdd10. take first 10 chars (SAS reads YYYY-MM-DD part)
  if (fmt == "%Y-%m-%d") vals <- substr(vals, 1, 10)
  res[idx] <- suppressWarnings(as.Date(vals, format = fmt))
  res
}

# generic column -> Date with ONE format decision per column (PROC IMPORT guessing)
col_to_date_guess <- function(col) {
  if (is.null(col)) return(rep(as.Date(NA), 0))
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) return(as.Date(col))
  if (inherits(col, "POSIXlt")) return(as.Date(col))
  if (is.numeric(col)) {
    # Excel serial dates
    out <- rep(as.Date(NA), length(col))
    idx <- which(!is.na(col) & col > 20000 & col < 60000)
    out[idx] <- as.Date(col[idx], origin = "1899-12-30")
    return(out)
  }
  x <- as.character(col)
  # Date objects read as character? handle
  nonmiss <- trimws(x[!is.na(x) & trimws(x) != ""])
  if (length(nonmiss) == 0) return(rep(as.Date(NA), length(x)))
  # decide ONE format for whole column
  is_iso <- grepl("^\\d{4}-\\d{1,2}-\\d{1,2}", nonmiss)
  is_dmon <- grepl("^\\d{1,2}-[A-Za-z]{3,9}-\\d{2,4}", nonmiss)
  is_slash <- grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}", nonmiss)
  is_yslash <- grepl("^\\d{4}/\\d{1,2}/\\d{1,2}", nonmiss)
  fmt <- "%Y-%m-%d"
  if (all(is_iso)) fmt <- "%Y-%m-%d"
  else if (all(is_dmon)) fmt <- "%d-%b-%Y"
  else if (all(is_slash)) {
    first_field <- suppressWarnings(as.integer(sub("/.*", "", nonmiss)))
    # PROC IMPORT: if any first field > 12 then DMY else MDY
    if (any(!is.na(first_field) & first_field > 12)) fmt <- "%d/%m/%Y"
    else fmt <- "%m/%d/%Y"
  } else if (all(is_yslash)) fmt <- "%Y/%m/%d"
  else if (any(is_iso)) fmt <- "%Y-%m-%d"
  else if (any(is_dmon)) fmt <- "%d-%b-%Y"
  else fmt <- "%Y-%m-%d"
  parse_char_strict(x, fmt)
}

col_to_iso <- function(col) {
  d <- col_to_date_guess(col)
  format(d, "%Y-%m-%d")
}

# SAS CATX("-", "ONCO-2026", SUBJID)
catx_usubjid <- function(subjid) {
  subjid <- as.character(subjid)
  vapply(subjid, function(s) {
    s <- ifelse(is.na(s), "", trimws(s))
    if (s == "") "ONCO-2026" else paste0("ONCO-2026-", s)
  }, character(1), USE.NAMES = FALSE)
}

# ---------- read raw ----------
edc_vs <- readxl::read_excel(edc_vs_path, .name_repair = "minimal")
edc_dm <- readxl::read_excel(edc_dm_path, .name_repair = "minimal")
edc_ex <- readxl::read_excel(edc_ex_path, .name_repair = "minimal")
ivrs_rand <- readxl::read_excel(ivrs_rand_path, .name_repair = "minimal")

# ---------- derive dm_for_vs (sdtm_dm upstream) ----------
# Need USUBJID, SUBJID, RFSTDTC, RFXSTDTC
# Try direct RFSTDTC/RFXSTDTC in edc_dm first
dm_subjid_raw <- get_raw_col(edc_dm, c("SUBJID","SUBJ_ID","SUBJECTID","SUBID","SUBJECT","ID"))
dm_subjid <- if (is.null(dm_subjid_raw)) rep(NA_character_, nrow(edc_dm)) else as.character(dm_subjid_raw)
dm_usubjid <- catx_usubjid(dm_subjid)

# direct RFSTDTC / RFXSTDTC if present
dm_rf_raw <- get_raw_col(edc_dm, c("RFSTDTC"))
dm_rfx_raw <- get_raw_col(edc_dm, c("RFXSTDTC"))
if (!is.null(dm_rf_raw)) {
  dm_rf_iso <- col_to_iso(dm_rf_raw)
  # col_to_iso already ISO; ensure NA -> NA_character_
  dm_rf_iso[is.na(dm_rf_iso)] <- NA_character_
} else dm_rf_iso <- rep(NA_character_, nrow(edc_dm))
if (!is.null(dm_rfx_raw)) {
  dm_rfx_iso <- col_to_iso(dm_rfx_raw)
  dm_rfx_iso[is.na(dm_rfx_iso)] <- NA_character_
} else dm_rfx_iso <- rep(NA_character_, nrow(edc_dm))

# randomization dates from ivrs_rand
rand_subjid_raw <- get_raw_col(ivrs_rand, c("SUBJID","SUBJ_ID","SUBJECTID","SUBID","SUBJECT","ID","SUBJECT_ID"))
rand_subjid <- if (is.null(rand_subjid_raw)) rep(NA_character_, nrow(ivrs_rand)) else as.character(rand_subjid_raw)
rand_usubjid <- catx_usubjid(rand_subjid)
# find rand date column: prefer RAND* then DATE/DT/DTC
rand_date_colname <- find_col_name(ivrs_rand, c("RANDDTC","RANDDT","RAND_DATE","RANDOMDT","RANDOMDTC","RAND_DT"))
if (is.na(rand_date_colname)) {
  # search any name containing RAND
  nm <- names(ivrs_rand); low <- tolower(nm)
  cand <- nm[grepl("rand", low)]
  if (length(cand) > 0) rand_date_colname <- cand[1]
}
if (is.na(rand_date_colname)) {
  nm <- names(ivrs_rand); low <- tolower(nm)
  cand <- nm[grepl("date|dtc|^dt$|_dt|dat", low)]
  # avoid birth date
  cand <- cand[!grepl("birth|brth|dob", tolower(cand))]
  if (length(cand) > 0) rand_date_colname <- cand[1]
}
if (!is.na(rand_date_colname)) {
  rand_dates <- col_to_date_guess(ivrs_rand[[rand_date_colname]])
  rand_iso <- format(rand_dates, "%Y-%m-%d")
} else {
  rand_iso <- rep(NA_character_, nrow(ivrs_rand))
}
rand_df <- tibble(USUBJID = rand_usubjid, RAND_ISO = rand_iso) %>%
  filter(!is.na(USUBJID) & USUBJID != "") %>%
  group_by(USUBJID) %>%
  summarise(RAND_ISO = suppressWarnings(min(RAND_ISO[!is.na(RAND_ISO)], na.rm = TRUE)), .groups = "drop")
rand_df$RAND_ISO[is.infinite(rand_df$RAND_ISO)] <- NA_character_
# min of character ISO = earliest; handle Inf case
rand_df <- rand_df %>% mutate(RAND_ISO = ifelse(is.na(RAND_ISO) | RAND_ISO == "Inf", NA_character_, RAND_ISO))

# exposure first dates from edc_ex
ex_subjid_raw <- get_raw_col(edc_ex, c("SUBJID","SUBJ_ID","SUBJECTID","SUBID","SUBJECT","ID"))
ex_subjid <- if (is.null(ex_subjid_raw)) rep(NA_character_, nrow(edc_ex)) else as.character(ex_subjid_raw)
ex_usubjid <- catx_usubjid(ex_subjid)
ex_date_colname <- find_col_name(edc_ex, c("EXSTDTC","EXSTDT","EXDAT","EX_DT","EXDATE","STDTC","STDT"))
if (is.na(ex_date_colname)) {
  nm <- names(edc_ex); low <- tolower(nm)
  cand <- nm[grepl("exstd|exdat|stdtc|stdate|^date|_dt|dat", low)]
  cand <- cand[!grepl("end|enrt|birth|brth|dob", tolower(cand))]
  if (length(cand) > 0) ex_date_colname <- cand[1]
}
if (is.na(ex_date_colname)) {
  nm <- names(edc_ex); low <- tolower(nm)
  cand <- nm[grepl("date|dtc|^dt$", low)]
  cand <- cand[!grepl("birth|brth|dob|end", tolower(cand))]
  if (length(cand) > 0) ex_date_colname <- cand[1]
}
if (!is.na(ex_date_colname)) {
  ex_dates <- col_to_date_guess(edc_ex[[ex_date_colname]])
  ex_iso <- format(ex_dates, "%Y-%m-%d")
} else {
  ex_iso <- rep(NA_character_, nrow(edc_ex))
}
ex_df <- tibble(USUBJID = ex_usubjid, EX_ISO = ex_iso) %>%
  filter(!is.na(USUBJID) & USUBJID != "") %>%
  group_by(USUBJID) %>%
  summarise(EX_ISO = suppressWarnings(min(EX_ISO[!is.na(EX_ISO)], na.rm = TRUE)), .groups = "drop")
ex_df$EX_ISO[is.infinite(ex_df$EX_ISO)] <- NA_character_
ex_df <- ex_df %>% mutate(EX_ISO = ifelse(is.na(EX_ISO) | EX_ISO == "Inf", NA_character_, EX_ISO))

# dm enrollment / other date fallback
dm_date_colname <- find_col_name(edc_dm, c("ENROLLDT","ENRDT","ENR_DTC","RANDDTC","RANDDT","RFSTDTC","BRTHDTC"))
if (is.na(dm_date_colname)) {
  nm <- names(edc_dm); low <- tolower(nm)
  cand <- nm[grepl("enroll|rand|consent|screen|rfstd|date|dtc", low)]
  cand <- cand[!grepl("birth|brth|dob|death|dth", tolower(cand))]
  if (length(cand) > 0) dm_date_colname <- cand[1]
}
if (!is.na(dm_date_colname) && is.na(find_col_name(edc_dm, c("RFSTDTC")))) {
  dm_fallback <- format(col_to_date_guess(edc_dm[[dm_date_colname]]), "%Y-%m-%d")
} else {
  dm_fallback <- rep(NA_character_, nrow(edc_dm))
}

dm_base <- tibble(USUBJID = dm_usubjid, SUBJID = trimws(as.character(dm_subjid)),
                  DM_RF = dm_rf_iso, DM_RFX = dm_rfx_iso, DM_FB = dm_fallback)
# collapse to one row per USUBJID (first non-missing)
dm_base <- dm_base %>%
  group_by(USUBJID) %>%
  summarise(
    SUBJID = first(SUBJID[!is.na(SUBJID) & trimws(SUBJID) != ""]),
    DM_RF = first(DM_RF[!is.na(DM_RF) & DM_RF != ""]),
    DM_RFX = first(DM_RFX[!is.na(DM_RFX) & DM_RFX != ""]),
    DM_FB = first(DM_FB[!is.na(DM_FB) & DM_FB != ""]),
    .groups = "drop"
  )
# first() on empty returns NA? handle: if all NA, first of empty is character(0) -> need fix
# Rebuild safely
fix_first <- function(x) { y <- x[!is.na(x) & x != ""]; if (length(y)==0) NA_character_ else y[1] }
dm_base <- tibble(USUBJID = dm_usubjid, SUBJID = trimws(as.character(dm_subjid)),
                  DM_RF = dm_rf_iso, DM_RFX = dm_rfx_iso, DM_FB = dm_fallback) %>%
  group_by(USUBJID) %>%
  summarise(SUBJID = fix_first(SUBJID), DM_RF = fix_first(DM_RF),
            DM_RFX = fix_first(DM_RFX), DM_FB = fix_first(DM_FB), .groups = "drop")

dm_for_vs <- dm_base %>%
  left_join(rand_df, by = "USUBJID") %>%
  left_join(ex_df, by = "USUBJID") %>%
  mutate(
    RFSTDTC = dplyr::coalesce(DM_RF, RAND_ISO, EX_ISO, DM_FB),
    RFXSTDTC = dplyr::coalesce(DM_RFX, EX_ISO)
  ) %>%
  select(USUBJID, SUBJID, RFSTDTC, RFXSTDTC)

# SAS sort by USUBJID (binary: uppercase before lowercase) - use radix
dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID, method = "radix", na.last = TRUE), , drop = FALSE]

# ---------- 1. vs_prep ----------
vs_subjid <- get_col_vec(edc_vs, c("SUBJID","SUBJ_ID","SUBJECTID","SUBID","SUBJECT","ID"))
vs_testcd <- get_col_vec(edc_vs, c("TESTCD","VSTESTCD","TEST","PARAMCD","VSTEST"))
vs_orres  <- get_col_vec(edc_vs, c("ORRES","VSORRES","RESULT","VALUE","VAL","AVALC"))
vs_unit   <- get_col_vec(edc_vs, c("UNIT","VSORRESU","UNITS","ORRESU","VSSTRESU"))
vs_visit  <- get_col_vec(edc_vs, c("VISIT","VISITNAME","VISIT_NAM","VISITNUM"))
vs_pos_raw<- get_col_vec(edc_vs, c("POS","VSPOS","POSITION"))
# VSDT raw (keep original type for date handling)
vsdt_colname <- find_col_name(edc_vs, c("VSDT","VSDTC","VSDATE","VS_DAT","VS_DT","DATE","DAT","VSDT_C","EXAMDATE"))
vsdt_raw <- if (is.na(vsdt_colname)) rep(NA_character_, nrow(edc_vs)) else edc_vs[[vsdt_colname]]

vs_prep <- tibble(
  SUBJID = trimws(as.character(vs_subjid)),
  TESTCD = trimws(as.character(vs_testcd)),
  ORRES  = trimws(as.character(vs_orres)),
  UNIT   = trimws(as.character(vs_unit)),
  VISIT  = as.character(vs_visit),
  POS    = as.character(vs_pos_raw)
)
vs_prep$VSDT_raw <- vsdt_raw  # may be Date or character; keep as list-col? store separately
# USUBJID = catx("-", "ONCO-2026", SUBJID)
vs_prep$USUBJID <- catx_usubjid(vs_prep$SUBJID)
# normalize empty strings to NA for character cols
for (cc in c("TESTCD","ORRES","UNIT","VISIT","POS")) {
  vs_prep[[cc]][!is.na(vs_prep[[cc]]) & trimws(vs_prep[[cc]]) == ""] <- NA_character_
}
vs_prep <- vs_prep[order(vs_prep$USUBJID, method = "radix", na.last = TRUE), , drop = FALSE]

# ---------- 2. Map VS domain (merge) ----------
# Before merging, drop every column present in both tables except keys
# dm_for_vs has USUBJID, SUBJID, RFSTDTC, RFXSTDTC; vs_prep has SUBJID too -> drop from dm side
dm_join <- dm_for_vs %>% select(USUBJID, RFSTDTC, RFXSTDTC)
# left join (SAS merge if v)
sdtm_vs1 <- vs_prep %>% left_join(dm_join, by = "USUBJID")
stopifnot("RFSTDTC" %in% names(sdtm_vs1))
stopifnot("USUBJID" %in% names(sdtm_vs1))

# Derive fields
n <- nrow(sdtm_vs1)
STUDYID <- rep("ONCO-2026", n)
DOMAIN <- rep("VS", n)
VSTESTCD <- substr(ifelse(is.na(sdtm_vs1$TESTCD), NA_character_, sdtm_vs1$TESTCD), 1, 8)
VSTEST <- rep(NA_character_, n)
VSTEST[!is.na(VSTESTCD) & VSTESTCD == "SYSBP"] <- "Systolic Blood Pressure"
VSTEST[!is.na(VSTESTCD) & VSTESTCD == "DIABP"] <- "Diastolic Blood Pressure"
VSORRES <- substr(ifelse(is.na(sdtm_vs1$ORRES), NA_character_, sdtm_vs1$ORRES), 1, 10)
VSORRESU <- substr(ifelse(is.na(sdtm_vs1$UNIT), NA_character_, sdtm_vs1$UNIT), 1, 20)
VSSTRESC <- substr(ifelse(is.na(sdtm_vs1$ORRES), NA_character_, sdtm_vs1$ORRES), 1, 10)
VSSTRESN <- suppressWarnings(as.numeric(trimws(sdtm_vs1$ORRES)))
VSSTRESU <- substr(ifelse(is.na(sdtm_vs1$UNIT), NA_character_, sdtm_vs1$UNIT), 1, 20)
VISIT <- sdtm_vs1$VISIT
VISIT <- ifelse(is.na(VISIT), NA_character_, gsub("_", " ", VISIT, fixed = TRUE))
VISIT <- substr(VISIT, 1, 20)
VSPOS <- ifelse(is.na(sdtm_vs1$POS), NA_character_, toupper(sdtm_vs1$POS))
VSPOS <- substr(VSPOS, 1, 10)

# VSDTC = put(input(VSDT, date11.), yymmdd10.)  -- ONE strict format %d-%b-%Y
vsdt_col <- sdtm_vs1$VSDT_raw
if (inherits(vsdt_col, "Date")) {
  vsdt_date <- vsdt_col
} else if (inherits(vsdt_col, "POSIXct") || inherits(vsdt_col, "POSIXt")) {
  vsdt_date <- as.Date(vsdt_col)
} else if (is.numeric(vsdt_col)) {
  vsdt_date <- rep(as.Date(NA), length(vsdt_col))
  idxn <- which(!is.na(vsdt_col) & vsdt_col > 20000 & vsdt_col < 60000)
  vsdt_date[idxn] <- as.Date(vsdt_col[idxn], origin = "1899-12-30")
} else {
  vsdt_char <- as.character(vsdt_col)
  vsdt_char[trimws(vsdt_char) == ""] <- NA_character_
  vsdt_date <- parse_char_strict(vsdt_char, "%d-%b-%Y")
}
VSDTC <- format(vsdt_date, "%Y-%m-%d")
VSDTC[is.na(VSDTC)] <- NA_character_
VSDTC <- substr(VSDTC, 1, 19)

# Study Day: input(VSDTC, yymmdd10.) and input(RFSTDTC, yymmdd10.)
rf_char <- as.character(sdtm_vs1$RFSTDTC)
rf_char[!is.na(rf_char) & trimws(rf_char) == ""] <- NA_character_
stdt_date <- parse_char_strict(VSDTC, "%Y-%m-%d")
rfdt_date <- parse_char_strict(rf_char, "%Y-%m-%d")
VSTDY <- rep(NA_real_, n)
idxd <- which(!is.na(stdt_date) & !is.na(rfdt_date))
diffd <- as.integer(stdt_date[idxd] - rfdt_date[idxd])
VSTDY[idxd] <- ifelse(stdt_date[idxd] >= rfdt_date[idxd], diffd + 1, diffd)

sdtm_vs1 <- tibble(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = sdtm_vs1$USUBJID,
  VSTESTCD = VSTESTCD, VSTEST = VSTEST,
  VSORRES = VSORRES, VSORRESU = VSORRESU,
  VSSTRESC = VSSTRESC, VSSTRESN = VSSTRESN, VSSTRESU = VSSTRESU,
  VSDTC = VSDTC, VSTDY = VSTDY,
  VISIT = VISIT, VSPOS = VSPOS
)

# ---------- 3. sort + VSSEQ ----------
# SAS sort by USUBJID VSDTC (missing first)
sdtm_vs1$VSDTC_key <- ifelse(is.na(sdtm_vs1$VSDTC), "", sdtm_vs1$VSDTC)
ord <- order(sdtm_vs1$USUBJID, sdtm_vs1$VSDTC_key, method = "radix", na.last = FALSE)
sdtm_vs1 <- sdtm_vs1[ord, , drop = FALSE]

sdtm_vs <- sdtm_vs1 %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VSTESTCD, VSTEST,
         VSORRES, VSORRESU, VSSTRESC, VSSTRESN, VSSTRESU,
         VSDTC, VSTDY, VISIT, VSPOS) %>%
  arrange(USUBJID, ifelse(is.na(VSDTC), "", VSDTC))

sdtm_vs <- as.data.frame(sdtm_vs)

# ---------- outputs ----------
# dataset CSV
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "P030.lst.csv"), row.names = FALSE, na = "")

# PROC PRINT text rendering
fmt_cell <- function(val, is_num) {
  if (is.na(val)) return(ifelse(is_num, ".", ""))
  as.character(val)
}
header <- c("Obs","STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST",
            "VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")
lines <- c("PROC PRINT DATA=SDTM_VS;", "",
           paste(header, collapse = " | "),
           paste(rep("-", 150), collapse = ""))
for (i in seq_len(nrow(sdtm_vs))) {
  r <- sdtm_vs[i, ]
  vals <- c(
    as.character(i),
    fmt_cell(r$STUDYID, FALSE), fmt_cell(r$DOMAIN, FALSE), fmt_cell(r$USUBJID, FALSE),
    fmt_cell(r$VSSEQ, TRUE), fmt_cell(r$VSTESTCD, FALSE), fmt_cell(r$VSTEST, FALSE),
    fmt_cell(r$VSORRES, FALSE), fmt_cell(r$VSORRESU, FALSE), fmt_cell(r$VSSTRESC, FALSE),
    fmt_cell(r$VSSTRESN, TRUE), fmt_cell(r$VSSTRESU, FALSE), fmt_cell(r$VSDTC, FALSE),
    fmt_cell(r$VSTDY, TRUE), fmt_cell(r$VISIT, FALSE), fmt_cell(r$VSPOS, FALSE)
  )
  lines <- c(lines, paste(vals, collapse = " | "))
}
lines <- c(lines, "", paste0("N = ", nrow(sdtm_vs)))

writeLines(lines, file.path(out, "P030.txt"))
writeLines(lines, file.path(out, "P030.lst.txt"))
# CSV of table body already written as P030.csv
```