# P018_sdtm_vs.sas -> R translation
# Vital Signs SDTM domain mapping: horizontal to vertical unpivot

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("out")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, {
    mult <- 10^digits
    sign_x <- sign(x)
    abs_x <- abs(x)
    sign_x * floor(abs_x * mult + 0.5) / mult
  })
}

sas_best <- function(x) {
  if (is.na(x)) return(NA_character_)
  out <- formatC(x, format = "g", digits = 12)
  trimws(out)
}
sas_best_vec <- function(v) vapply(v, sas_best, character(1), USE.NAMES = FALSE)

parse_vsdtc <- function(vs_date_vec) {
  vapply(vs_date_vec, function(x) {
    if (is.na(x) || trimws(as.character(x)) == "" || toupper(trimws(as.character(x))) %in% c("NA",".")) return(NA_character_)
    x_trim <- trimws(as.character(x))
    # pure integer -> possible SAS date numeric (days since 1960-01-01)
    if (grepl("^[0-9]+$", x_trim)) {
      nch <- nchar(x_trim)
      num <- suppressWarnings(as.numeric(x_trim))
      if (!is.na(num)) {
        if (nch <= 5) {
          d <- as.Date(num, origin = "1960-01-01")
          return(format(d, "%Y-%m-%d"))
        } else if (nch == 8) {
          d <- suppressWarnings(as.Date(x_trim, format = "%Y%m%d"))
          if (!is.na(d)) return(format(d, "%Y-%m-%d"))
          d2 <- as.Date(num, origin = "1960-01-01")
          if (!is.na(d2) && d2 > as.Date("1900-01-01") && d2 < as.Date("2100-01-01")) return(format(d2, "%Y-%m-%d"))
        }
      }
    }
    fmts <- c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y","%d-%b-%Y","%d-%B-%Y",
              "%d%b%Y","%d %b %Y","%b %d, %Y","%Y%m%d","%m-%d-%Y","%d-%m-%Y",
              "%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S","%d-%b-%y","%m/%d/%y",
              "%d-%m-%y","%Y-%m-%dT%H:%M","%d/%m/%y","%Y.%m.%d")
    x_up <- toupper(x_trim)
    for (fmt in fmts) {
      d <- suppressWarnings(as.Date(x_trim, format = fmt))
      if (!is.na(d)) return(format(d, "%Y-%m-%d"))
      d2 <- suppressWarnings(as.Date(x_up, format = toupper(fmt)))
      if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    }
    # try ISO date-time then take date part
    d <- suppressWarnings(as.Date(substr(x_trim,1,10)))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    return(NA_character_)
  }, character(1), USE.NAMES = FALSE)
}

# locate raw_vitals.csv -------------------------------------------------
possible_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_vitals.csv",
  file.path(Sys.getenv("project_path"), "data/raw/raw_vitals.csv"),
  file.path(Sys.getenv("PROJECT_PATH"), "data/raw/raw_vitals.csv"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_vitals.csv"),
  "data/derived/c2c/data/raw/raw_vitals.csv"
)
raw_path <- NULL
for (p in possible_paths) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { raw_path <- p; break }
}
if (is.null(raw_path)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_vitals.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_path <- cand[1]
}
if (is.null(raw_path) || !file.exists(raw_path)) {
  # fallback search from current working directory
  cand2 <- list.files(".", pattern = "raw_vitals.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand2) > 0) raw_path <- cand2[1]
}
stopifnot(!is.null(raw_path), file.exists(raw_path))

# 1. IMPORT RAW VITALS --------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidyr)
})

if (requireNamespace("readr", quietly = TRUE)) {
  raw_vs_raw <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE)
} else {
  raw_vs_raw <- read.csv(raw_path, stringsAsFactors = FALSE, colClasses = "character")
}
names(raw_vs_raw) <- toupper(names(raw_vs_raw))
# standardize expected columns if alternative names exist
if (!"PT_ID" %in% names(raw_vs_raw)) {
  for (a in c("PATIENT_ID","PATID","SUBJID","USUBJID","PTID","SUBJECT")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$PT_ID <- raw_vs_raw[[a]]; break }
}
if (!"VISIT_NUM" %in% names(raw_vs_raw)) {
  for (a in c("VISITNUM","VISIT_NUMBER","VISITNO","VISIT_N")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$VISIT_NUM <- raw_vs_raw[[a]]; break }
}
if (!"VS_DATE" %in% names(raw_vs_raw)) {
  for (a in c("VSDTC","DATE","VSDATE","VS_DTC","VSTESTDT","VSDT")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$VS_DATE <- raw_vs_raw[[a]]; break }
}
if (!"VISIT" %in% names(raw_vs_raw)) raw_vs_raw$VISIT <- NA_character_
if (!"SYS_BP" %in% names(raw_vs_raw)) {
  for (a in c("SYSBP","SBP","SYSTOLIC")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$SYS_BP <- raw_vs_raw[[a]]; break }
}
if (!"DIA_BP" %in% names(raw_vs_raw)) {
  for (a in c("DIABP","DBP","DIASTOLIC")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$DIA_BP <- raw_vs_raw[[a]]; break }
}
if (!"HR_BPM" %in% names(raw_vs_raw)) {
  for (a in c("HR","PULSE","HRBPM","HEART_RATE")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$HR_BPM <- raw_vs_raw[[a]]; break }
}
if (!"WEIGHT_KG" %in% names(raw_vs_raw)) {
  for (a in c("WEIGHT","WT","WEIGHTKG","WEIGHT_KG_")) if (a %in% names(raw_vs_raw)) { raw_vs_raw$WEIGHT_KG <- raw_vs_raw[[a]]; break }
}
# ensure columns exist
for (cn in c("PT_ID","VISIT_NUM","VISIT","VS_DATE","SYS_BP","DIA_BP","HR_BPM","WEIGHT_KG")) {
  if (!cn %in% names(raw_vs_raw)) raw_vs_raw[[cn]] <- NA_character_
}

raw_vs <- raw_vs_raw %>%
  mutate(
    PT_ID = trimws(as.character(PT_ID)),
    VISIT = trimws(as.character(VISIT)),
    VISIT_NUM = suppressWarnings(as.numeric(VISIT_NUM)),
    SYS_BP = suppressWarnings(as.numeric(SYS_BP)),
    DIA_BP = suppressWarnings(as.numeric(DIA_BP)),
    HR_BPM = suppressWarnings(as.numeric(HR_BPM)),
    WEIGHT_KG = suppressWarnings(as.numeric(WEIGHT_KG))
  )

# compute derived SDTM variables per SAS logic
# USUBJID = catx("-", STUDYID, PT_ID); STUDYID="CDISC-01"; DOMAIN="VS"
# VSDTC from VS_DATE via anydtdte -> is8601da
raw_vs <- raw_vs %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "VS",
    USUBJID = ifelse(is.na(PT_ID) | PT_ID == "", STUDYID, paste0(STUDYID, "-", PT_ID)),
    VISITNUM = VISIT_NUM,
    VSDTC = parse_vsdtc(VS_DATE)
  )

# 2. HORIZONTAL TO VERTICAL TRANSFORMATION (UNPIVOT) --------------------
# helper to build each test block
base_cols <- c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","VSDTC")

mk_block <- function(df, filter_col, testcd, test, unit) {
  df %>%
    filter(!is.na(.data[[filter_col]])) %>%
    mutate(
      VSTESTCD = testcd,
      VSTEST = test,
      VSORRES = sas_best_vec(.data[[filter_col]]),
      VSSTRESN = .data[[filter_col]],
      VSORRESU = unit,
      VSSTRESU = unit
    ) %>%
    select(all_of(c(base_cols, "VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESN","VSSTRESU")))
}

vs_sys <- mk_block(raw_vs, "SYS_BP", "SYSBP", "Systolic Blood Pressure", "mmHg")
vs_dia <- mk_block(raw_vs, "DIA_BP", "DIABP", "Diastolic Blood Pressure", "mmHg")
vs_pulse <- mk_block(raw_vs, "HR_BPM", "PULSE", "Pulse Rate", "beats/min")
vs_wt <- mk_block(raw_vs, "WEIGHT_KG", "WEIGHT", "Weight", "kg")

vs_mapped <- bind_rows(vs_sys, vs_dia, vs_pulse, vs_wt) %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU)

# 3. GENERATE SEQUENCE NUMBER (VSSEQ) -----------------------------------
# proc sort by USUBJID VSDTC VSTESTCD; then by USUBJID retain VSSEQ
vs_mapped_sorted <- vs_mapped %>%
  arrange(USUBJID, VSDTC, VSTESTCD)

sdtm_vs <- vs_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(VSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU) %>%
  arrange(USUBJID, VSDTC, VSTESTCD)

# write dataset vs.csv --------------------------------------------------
write.csv(sdtm_vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - listing obs=12 --------------------------------------
listing_df <- sdtm_vs %>%
  select(USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSSTRESN, VSSTRESU) %>%
  head(12)

# text rendering
title_txt <- "VS Domain Audit (Unpivoted Format with VSSEQ)"
header_line <- sprintf("%-3s %-20s %5s %8s %-20s %-10s %-8s %8s %-10s",
                       "Obs","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
sep_line <- sprintf("%-3s %-20s %5s %8s %-20s %-10s %-8s %8s %-10s",
                    "---","--------------------","-----","--------","--------------------","----------","--------","--------","----------")
lines <- c(title_txt, "", header_line, sep_line)
for (i in seq_len(nrow(listing_df))) {
  r <- listing_df[i, ]
  lines <- c(lines, sprintf("%3d %-20s %5s %8s %-20s %-10s %-8s %8s %-10s",
    i,
    ifelse(is.na(r$USUBJID), "", substr(r$USUBJID,1,20)),
    ifelse(is.na(r$VSSEQ), "", as.character(r$VSSEQ)),
    ifelse(is.na(r$VISITNUM), "", as.character(r$VISITNUM)),
    ifelse(is.na(r$VISIT), "", substr(r$VISIT,1,20)),
    ifelse(is.na(r$VSDTC), "", r$VSDTC),
    ifelse(is.na(r$VSTESTCD), "", r$VSTESTCD),
    ifelse(is.na(r$VSSTRESN), "", sas_best(r$VSSTRESN)),
    ifelse(is.na(r$VSSTRESU), "", r$VSSTRESU)
  ))
}
lines <- c(lines, "", paste0("N = ", nrow(sdtm_vs), " total records in VS domain; ", nrow(listing_df), " displayed above."))

writeLines(lines, file.path(out, "P018.txt"))
writeLines(lines, file.path(out, "P018.lst"))
write.csv(listing_df, file.path(out, "P018.csv"), row.names = FALSE, na = "")
