# P005_adam_adlb.sas -> R translation
# ADLB derivation: merge ADSL + LB, baseline, CHG, LBNRIND, audit listing

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find and read a dataset by lib path + name
read_dataset <- function(libpath, dsname) {
  # try exact libpath first
  candidates <- c()
  if (dir.exists(libpath)) {
    files <- list.files(libpath, recursive = FALSE, full.names = TRUE)
    # match dsname with any extension
    pat <- paste0("^", dsname, "\\.")
    matched <- files[grepl(pat, basename(files), ignore.case = TRUE)]
    candidates <- c(candidates, matched)
  }
  # also try alternative roots if not found
  if (length(candidates) == 0) {
    alt_roots <- c("/bench/data/derived/c2c",
                   "/bench",
                   ".", "./data/derived/c2c", "/tmp")
    for (r in alt_roots) {
      for (sub in c(paste0("/data/", tolower(basename(libpath))),
                     paste0("/data/derived/c2c/data/", tolower(basename(libpath))),
                     paste0("/data/", tolower(basename(libpath))))) {
        p <- file.path(r, sub)
        if (dir.exists(p)) {
          files <- list.files(p, full.names = TRUE)
          pat <- paste0("^", dsname, "\\.")
          matched <- files[grepl(pat, basename(files), ignore.case = TRUE)]
          if (length(matched) > 0) candidates <- c(candidates, matched)
        }
      }
    }
    # recursive search under project_path if still not found
    if (length(candidates) == 0) {
      for (r in c("/bench", ".", getwd())) {
        if (dir.exists(r)) {
          allf <- list.files(r, recursive = TRUE, full.names = TRUE)
          idx <- grepl(paste0("/", dsname, "\\."), allf, ignore.case = TRUE)
          if (any(idx)) candidates <- c(candidates, allf[idx])
        }
      }
    }
  }
  candidates <- unique(candidates)
  if (length(candidates) == 0) {
    stop(paste0("Dataset not found: ", libpath, "/", dsname, " candidates searched"))
  }
  # prefer sas7bdat > xpt > csv > parquet
  order_ext <- function(f) {
    ext <- tolower(tools::file_ext(f))
    match(ext, c("sas7bdat", "xpt", "csv", "parquet", "rds"), nomatch = 99)
  }
  candidates <- candidates[order(sapply(candidates, order_ext))]
  f <- candidates[1]
  message("Reading ", dsname, " from ", f)
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else if (ext == "parquet") {
    df <- arrow::read_parquet(f)
  } else if (ext == "rds") {
    df <- readRDS(f)
  } else {
    # try haven as fallback
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <<- haven::read_xpt(f))
  }
  # convert haven_labelled to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # uppercase names to emulate validvarname=upcase
  names(df) <- toupper(names(df))
  df
}

# helper: convert SAS numeric date (days since 1960-01-01) to Date
to_Date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS dates are typically 10000-30000 for 1987-2042; also could be 0-50000
    # Heuristic: if all non-NA values are between 0 and 60000 and no value looks like year
    # treat as SAS date. Also check if already Date-like numeric with origin 1970 would be ~18000 for 2019, similar.
    # We distinguish by checking if values are integers and plausible SAS dates.
    # ADSL TRTSDT/TRTEDT are SAS dates.
    # Convert using 1960-01-01 origin
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # try ISO YYYY-MM-DD
    x_trim <- trimws(x)
    x_trim[x_trim == ""] <- NA
    # SAS yymmdd10. is YYYY-MM-DD
    d <- suppressWarnings(as.Date(x_trim, format = "%Y-%m-%d"))
    # fallback: try YYYY/MM/DD or DDMMMYYYY etc but keep strict
    miss <- is.na(d) & !is.na(x_trim)
    if (any(miss)) {
      # try yymmdd with dashes already, else try date9
      d2 <- suppressWarnings(as.Date(x_trim[miss], format = "%d%b%Y"))
      d[miss] <- d2
    }
    return(d)
  }
  # haven labelled numeric
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  return(suppressWarnings(as.Date(as.character(x))))
}

# helper: format Date as DATE9. e.g., 01JAN2020
format_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}

# ------------------------------------------------------------------
# 1. Read ADSL and LB
# ------------------------------------------------------------------
adam_path <- "/bench/data/derived/c2c/data/adam"
sdtm_path <- "/bench/data/derived/c2c/data/sdtm"
raw_path  <- "/bench/data/derived/c2c/data/raw"

adsl <- read_dataset(adam_path, "adsl")
lb   <- read_dataset(sdtm_path, "lb")

# Ensure required columns exist (case-insensitive already upper)
# ADSL expected: STUDYID, USUBJID, TRT01A, TRTSDT, TRTEDT
# LB expected: USUBJID, LBDTC, LBTESTCD, LBTEST, LBSTRESN, LBSTRESU, VISIT, VISITNUM, LBSEQ, STUDYID

# Normalize types
# Convert TRTSDT/TRTEDT to Date
if ("TRTSDT" %in% names(adsl)) adsl$TRTSDT <- to_Date(adsl$TRTSDT)
if ("TRTEDT" %in% names(adsl)) adsl$TRTEDT <- to_Date(adsl$TRTEDT)

# Ensure character columns are character
char_cols_adsl <- intersect(c("STUDYID","USUBJID","TRT01A"), names(adsl))
for (cn in char_cols_adsl) adsl[[cn]] <- as.character(adsl[[cn]])

char_cols_lb <- intersect(c("STUDYID","USUBJID","LBDTC","LBTESTCD","LBTEST","LBSTRESU","VISIT"), names(lb))
for (cn in char_cols_lb) lb[[cn]] <- as.character(lb[[cn]])

# Numeric columns
if ("LBSTRESN" %in% names(lb)) lb$LBSTRESN <- suppressWarnings(as.numeric(lb$LBSTRESN))
if ("VISITNUM" %in% names(lb)) lb$VISITNUM <- suppressWarnings(as.numeric(lb$VISITNUM))
if ("LBSEQ" %in% names(lb)) lb$LBSEQ <- suppressWarnings(as.numeric(lb$LBSEQ))

# Sort as SAS does
adsl_sorted <- adsl %>% arrange(USUBJID)
lb_sorted   <- lb %>% arrange(USUBJID, LBDTC, LBTESTCD)

# ------------------------------------------------------------------
# 2. MERGE ADSL AND LB -> work.adlb_draft
# ------------------------------------------------------------------
# SAS: merge work.adsl_sorted(in=a) work.lb_sorted(in=b) by USUBJID; if b;
# This is left join LB with ADSL (many-to-one)
# Handle overlapping columns except BY: drop/rename to avoid .x/.y
# Overlapping non-BY: STUDYID (and possibly others)
# We will keep LB's STUDYID if present, else ADSL's

# Prepare ADSL for join: keep only needed columns, rename STUDYID to avoid clash
adsl_for_join <- adsl_sorted %>%
  select(any_of(c("USUBJID","STUDYID","TRT01A","TRTSDT","TRTEDT")))

# If both have STUDYID, we will coalesce later; rename ADSL's to STUDYID_ADSL
if ("STUDYID" %in% names(adsl_for_join) && "STUDYID" %in% names(lb_sorted)) {
  names(adsl_for_join)[names(adsl_for_join)=="STUDYID"] <- "STUDYID_ADSL"
}

adlb_draft <- lb_sorted %>%
  left_join(adsl_for_join, by = "USUBJID")

# Verify join produced rows (stopifnot filter exists)
stopifnot(nrow(adlb_draft) > 0)
stopifnot("USUBJID" %in% names(adlb_draft))

# Derive core analysis variables
# PARAMCD = LBTESTCD, PARAM = LBTEST, AVAL = LBSTRESN, AVISIT = VISIT, AVISITN = VISITNUM
# SRCDOM='LB', SRCVAR='LBSEQ', SRCSEQ=LBSEQ
# ASTDT = input(substr(LBDTC,1,10), yymmdd10.) if length>=10
# ASTDY calculation

adlb_draft <- adlb_draft %>%
  mutate(
    PARAMCD = as.character(LBTESTCD),
    PARAM   = as.character(LBTEST),
    AVAL    = suppressWarnings(as.numeric(LBSTRESN)),
    AVISIT  = as.character(VISIT),
    AVISITN = suppressWarnings(as.numeric(VISITNUM)),
    SRCDOM  = "LB",
    SRCVAR  = "LBSEQ",
    SRCSEQ  = suppressWarnings(as.numeric(LBSEQ)),
    LBSTRESU = as.character(LBSTRESU)
  )

# STUDYID handling: coalesce LB STUDYID and ADSL STUDYID_ADSL
if ("STUDYID_ADSL" %in% names(adlb_draft)) {
  if ("STUDYID" %in% names(adlb_draft)) {
    adlb_draft$STUDYID <- ifelse(!is.na(adlb_draft$STUDYID) & adlb_draft$STUDYID != "",
                                 adlb_draft$STUDYID, adlb_draft$STUDYID_ADSL)
    adlb_draft$STUDYID_ADSL <- NULL
  } else {
    adlb_draft$STUDYID <- adlb_draft$STUDYID_ADSL
    adlb_draft$STUDYID_ADSL <- NULL
  }
}

# ASTDT derivation: if length(LBDTC) >=10 then input(substr(LBDTC,1,10), yymmdd10.)
adlb_draft$ASTDT <- as.Date(NA)
idx_len <- !is.na(adlb_draft$LBDTC) & nchar(adlb_draft$LBDTC) >= 10
if (any(idx_len)) {
  substr10 <- substr(adlb_draft$LBDTC[idx_len], 1, 10)
  # SAS yymmdd10. expects YYYY-MM-DD (with dashes)
  adlb_draft$ASTDT[idx_len] <- suppressWarnings(as.Date(substr10, format = "%Y-%m-%d"))
  # For any that failed but length>=10, try alternative strict? Keep NA as SAS would
}

# ASTDY
adlb_draft$ASTDY <- NA_real_
# TRTSDT already Date
has_both <- !is.na(adlb_draft$ASTDT) & !is.na(adlb_draft$TRTSDT)
if (any(has_both)) {
  ge <- adlb_draft$ASTDT[has_both] >= adlb_draft$TRTSDT[has_both]
  diff_days <- as.numeric(adlb_draft$ASTDT[has_both] - adlb_draft$TRTSDT[has_both])
  adlb_draft$ASTDY[has_both] <- ifelse(ge, diff_days + 1, diff_days)
}

# Keep only required columns (as SAS keep)
keep_cols <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL",
               "AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ")
# Ensure all exist, create NA if missing
for (kc in keep_cols) if (!kc %in% names(adlb_draft)) adlb_draft[[kc]] <- NA
adlb_draft <- adlb_draft %>% select(all_of(keep_cols))

# ------------------------------------------------------------------
# 3. IDENTIFY BASELINE RECORD
# ------------------------------------------------------------------
# proc sort data=work.adlb_draft out=work.adlb_base_candidates;
#   by USUBJID PARAMCD ASTDT AVISITN SRCSEQ;
#   where ASTDT <= TRTSDT and not missing(AVAL);
adlb_base_candidates <- adlb_draft %>%
  filter(!is.na(ASTDT) & !is.na(TRTSDT) & ASTDT <= TRTSDT & !is.na(AVAL)) %>%
  arrange(USUBJID, PARAMCD, ASTDT, AVISITN, SRCSEQ)

# data work.base_flags; set work.adlb_base_candidates; by USUBJID PARAMCD;
# if last.PARAMCD then do; BASE=AVAL; BASE_DT=ASTDT; BASE_SEQ=SRCSEQ; output; end;
# keep USUBJID PARAMCD BASE BASE_DT BASE_SEQ;

# For each USUBJID+PARAMCD, take last row
base_flags <- adlb_base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  arrange(ASTDT, AVISITN, SRCSEQ) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(USUBJID, PARAMCD, BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ)

# ------------------------------------------------------------------
# 4. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE + LBNRIND
# ------------------------------------------------------------------
# proc sort data=work.adlb_draft; by USUBJID PARAMCD;
adlb_draft_sorted <- adlb_draft %>% arrange(USUBJID, PARAMCD)

# merge work.adlb_draft(in=a) work.base_flags(in=b) by USUBJID PARAMCD; if a;
adlb <- adlb_draft_sorted %>%
  left_join(base_flags, by = c("USUBJID","PARAMCD"))

# Flag exact source record
adlb <- adlb %>%
  mutate(
    ABLFL = ifelse(!is.na(SRCSEQ) & !is.na(BASE_SEQ) & SRCSEQ == BASE_SEQ, "Y", ""),
    CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_)
  )

# Derive LBNRIND
# length $10, if not missing AVAL then _calc_val = AVAL; select PARAMCD
adlb$LBNRIND <- NA_character_
# Ensure LBSTRESU exists
if (!"LBSTRESU" %in% names(adlb)) adlb$LBSTRESU <- NA_character_

for (i in seq_len(nrow(adlb))) {
  aval <- adlb$AVAL[i]
  if (is.na(aval)) next
  paramcd <- adlb$PARAMCD[i]
  if (is.na(paramcd)) {
    adlb$LBNRIND[i] <- "UNKNOWN"
    next
  }
  calc_val <- aval
  lb_unit <- adlb$LBSTRESU[i]
  if (is.na(lb_unit)) lb_unit <- ""
  lb_unit_up <- toupper(trimws(lb_unit))
  if (paramcd == "GLUC") {
    if (lb_unit_up == "MG/DL") calc_val <- aval * 0.0555
    if (calc_val < 3.9) adlb$LBNRIND[i] <- "LOW"
    else if (calc_val > 5.6) adlb$LBNRIND[i] <- "HIGH"
    else adlb$LBNRIND[i] <- "NORMAL"
  } else if (paramcd == "HGB") {
    if (lb_unit_up == "G/DL") calc_val <- aval * 10
    if (calc_val < 120) adlb$LBNRIND[i] <- "LOW"
    else if (calc_val > 175) adlb$LBNRIND[i] <- "HIGH"
    else adlb$LBNRIND[i] <- "NORMAL"
  } else {
    adlb$LBNRIND[i] <- "UNKNOWN"
  }
}

# drop BASE_DT BASE_SEQ (and _calc_val not stored)
adlb$BASE_DT <- NULL
adlb$BASE_SEQ <- NULL

# Re-sort for final presentation: by USUBJID PARAMCD ASTDT
adlb <- adlb %>% arrange(USUBJID, PARAMCD, ASTDT)

# Retain order: STUDYID USUBJID PARAMCD PARAM ...
retain_order <- c("STUDYID","USUBJID","PARAMCD","PARAM","TRT01A","TRTSDT","TRTEDT",
                  "AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ",
                  "BASE","CHG","ABLFL","LBNRIND")
# Keep only those that exist, plus any extra in original keep
final_cols <- intersect(retain_order, names(adlb))
# Add any remaining columns not in retain_order at end
remaining <- setdiff(names(adlb), final_cols)
adlb <- adlb %>% select(all_of(c(final_cols, remaining)))

# ------------------------------------------------------------------
# 5. Write dataset adlb.csv
# ------------------------------------------------------------------
# Dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS, numeric unrounded
adlb_out <- adlb
# Convert Date columns to character ISO for CSV
date_cols <- c("TRTSDT","TRTEDT","ASTDT")
for (dc in date_cols) {
  if (dc %in% names(adlb_out)) {
    adlb_out[[dc]] <- ifelse(is.na(adlb_out[[dc]]), "",
                             format(as.Date(adlb_out[[dc]]), "%Y-%m-%d"))
  }
}
# Ensure BASE, CHG, AVAL, ASTDY, AVISITN, SRCSEQ are numeric as is
write.csv(adlb_out, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 6. VISUAL AUDIT - listing
# ------------------------------------------------------------------
# title "ADLB Audit - Laboratory Analysis with Clinical Indicators";
# proc print data=adam.adlb(obs=15); var USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND;

audit_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
audit_df <- adlb %>% select(any_of(audit_vars)) %>% head(15)

# Prepare display version with DATE9 formatting for ASTDT
audit_display <- audit_df %>%
  mutate(
    ASTDT_DISP = format_date9(ASTDT),
    AVAL_DISP = ifelse(is.na(AVAL), "", format(AVAL, trim = TRUE, scientific = FALSE)),
    BASE_DISP = ifelse(is.na(BASE), "", format(BASE, trim = TRUE, scientific = FALSE)),
    CHG_DISP  = ifelse(is.na(CHG), "", format(CHG, trim = TRUE, scientific = FALSE))
  )

# Create text rendering
txt_path <- file.path(out, "P005.txt")
lst_path <- file.path(out, "P005.lst.txt")
# Also create P005.lst as text per expected output name
lst_alt <- file.path(out, "P005.lst")

con <- file(txt_path, open = "wt")
writeLines("ADLB Audit - Laboratory Analysis with Clinical Indicators", con)
writeLines(paste(rep("-", 80), collapse = ""), con)
writeLines("", con)
# Header
header <- sprintf("%-12s %-8s %-12s %-10s %8s %-8s %8s %8s %-5s %-8s",
                  "USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse = ""), con)
for (i in seq_len(nrow(audit_display))) {
  row <- audit_display[i, ]
  line <- sprintf("%-12s %-8s %-12s %-10s %8s %-8s %8s %8s %-5s %-8s",
                  substr(ifelse(is.na(row$USUBJID),"",row$USUBJID),1,12),
                  substr(ifelse(is.na(row$PARAMCD),"",row$PARAMCD),1,8),
                  substr(ifelse(is.na(row$AVISIT),"",row$AVISIT),1,12),
                  substr(row$ASTDT_DISP,1,10),
                  substr(row$AVAL_DISP,1,8),
                  substr(ifelse(is.na(row$LBSTRESU),"",row$LBSTRESU),1,8),
                  substr(row$BASE_DISP,1,8),
                  substr(row$CHG_DISP,1,8),
                  substr(ifelse(is.na(row$ABLFL),"",row$ABLFL),1,5),
                  substr(ifelse(is.na(row$LBNRIND),"",row$LBNRIND),1,8)
  )
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Obs displayed: ", nrow(audit_df), " of ", nrow(adlb), " total records"), con)
close(con)

# copy to alternative names
file.copy(txt_path, lst_path, overwrite = TRUE)
file.copy(txt_path, lst_alt, overwrite = TRUE)

# CSV of table body (same as proc print vars, with ISO dates)
audit_csv <- audit_df %>%
  mutate(
    ASTDT = ifelse(is.na(ASTDT), "", format(as.Date(ASTDT), "%Y-%m-%d"))
  )
# Write P005.csv as listing CSV
write.csv(audit_csv, file.path(out, "P005.csv"), row.names = FALSE, na = "")
# Also write P005.lst.csv for completeness
write.csv(audit_csv, file.path(out, "P005.lst.csv"), row.names = FALSE, na = "")

message("Done. Outputs written to ", out)
message("adlb.csv rows: ", nrow(adlb_out))
message("P005 audit rows: ", nrow(audit_csv))
