# P006 adam_adsl.sas -> R translation
# ADSL derivation: merge SDTM DM + EX, flags, dates, age, duration + audit listing

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sdtm_dir <- "/bench/data/derived/c2c/data/sdtm"
adam_dir <- "/bench/data/derived/c2c/data/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
  library(tibble)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

is_sas_miss_char <- function(x) {
  is.na(x) | trimws(as.character(x), which = "both") == ""
}

sas_len <- function(x) {
  # SAS LENGTH strips trailing blanks; missing "" -> 0/1 -> treat as 0
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(0L)
    s2 <- sub(" +$", "", s)
    nchar(s2)
  }, integer(1), USE.NAMES = FALSE)
}

parse_yymmdd10 <- function(x) {
  # input(substr(x,1,10), yymmdd10.) with length>=10 guard
  vapply(as.character(x), function(s) {
    if (is.na(s)) return(NA_character_)
    # SAS length check on original
    slen <- nchar(sub(" +$", "", s))
    if (is.na(slen) || slen < 10) return(NA_character_)
    sub10 <- substr(s, 1, 10)
    d <- suppressWarnings(as.Date(sub10, format = "%Y-%m-%d"))
    if (is.na(d)) return(NA_character_)
    as.character(d)
  }, character(1), USE.NAMES = FALSE) |> suppressWarnings() |> as.Date(format = "%Y-%m-%d")
}

calc_age_yrdif <- function(brthdt, rficdt) {
  # SAS YRDIF(brthdt, rficdt, 'AGE') + floor => complete years
  mapply(function(b, r) {
    if (is.na(b) || is.na(r)) return(NA_real_)
    b <- as.Date(b, origin = "1970-01-01")
    r <- as.Date(r, origin = "1970-01-01")
    if (r < b) return(NA_real_)
    yb <- as.integer(format(b, "%Y")); mb <- as.integer(format(b, "%m")); db <- as.integer(format(b, "%d"))
    yr <- as.integer(format(r, "%Y")); mr <- as.integer(format(r, "%m")); dr <- as.integer(format(r, "%d"))
    age <- yr - yb
    if ((mr < mb) || (mr == mb && dr < db)) age <- age - 1L
    as.numeric(age)
  }, brthdt, rficdt, USE.NAMES = FALSE)
}

sas_bin_key <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s)) s <- ""
    if (s == "") return("\001")
    codes <- utf8ToInt(s)
    paste(sprintf("%03d", codes), collapse = " ")
  }, character(1), USE.NAMES = FALSE)
}

clean_df <- function(df) {
  if (is.null(df)) return(NULL)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # zap haven labels/formats
  for (nm in names(df)) {
    col <- df[[nm]]
    if (haven::is.labelled(col)) col <- haven::zap_labels(col)
    # remove remaining attributes that could interfere
    attributes(col) <- attributes(col)[intersect(names(attributes(col)), c("names","class"))]
    if (is.factor(col)) col <- as.character(col)
    if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) col <- as.character(col)
    # haven Date? keep as Date if Date else character
    df[[nm]] <- col
  }
  # convert list-cols (if any) to character
  for (nm in names(df)) {
    if (is.list(df[[nm]])) df[[nm]] <- sapply(df[[nm]], function(v) if (is.null(v) || length(v)==0 || all(is.na(v))) NA_character_ else as.character(v[1]))
  }
  tibble::as_tibble(df)
}

read_sas_dataset <- function(dir, memname) {
  if (!dir.exists(dir)) stop(paste0("Directory not found: ", dir))
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match basename sans ext case-insensitive
  target <- tolower(memname)
  matched <- files[tolower(tools::file_path_sans_ext(basename(files))) == target]
  if (length(matched) == 0) {
    # try recursive
    files2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
    matched <- files2[tolower(tools::file_path_sans_ext(basename(files2))) == target]
  }
  if (length(matched) == 0) stop(paste0("Dataset ", memname, " not found in ", dir, ". Files: ", paste(basename(files), collapse="; ")))
  f <- matched[1]
  ext <- tolower(tools::file_ext(f))
  message("Reading ", memname, " from ", f)
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- suppressMessages(readr::read_csv(f, col_types = readr::cols(.default = "c"), na = c("", "NA")))
    df <- as.data.frame(df)
  } else if (ext %in% c("xls","xlsx")) {
    df <- as.data.frame(readxl::read_excel(f, col_types = "text"))
  } else {
    # try sas then xpt
    df <- tryCatch(haven::read_sas(f), error = function(e) haven::read_xpt(f))
  }
  clean_df(df)
}

ensure_char_col <- function(df, nm) {
  if (!nm %in% names(df)) {
    df[[nm]] <- NA_character_
  } else {
    col <- df[[nm]]
    if (is.factor(col)) col <- as.character(col)
    if (inherits(col, "Date")) col <- format(col, "%Y-%m-%d")
    if (inherits(col, "POSIXct")) col <- format(as.Date(col), "%Y-%m-%d")
    if (is.numeric(col)) col <- as.character(col)
    # haven labelled numeric already zapped; ensure character
    df[[nm]] <- as.character(col)
  }
  df
}

message("Step 1-2: read SDTM DM and EX")
dm <- read_sas_dataset(sdtm_dir, "dm")
ex <- read_sas_dataset(sdtm_dir, "ex")

# Ensure needed columns exist
for (cn in c("STUDYID","USUBJID","SUBJID","SITEID","ARMCD","ARM","BRTHDTC","RFICDTC","RFPENDTC","SEX","RACE")) dm <- ensure_char_col(dm, cn)
for (cn in c("USUBJID","EXTRT","EXSTDTC","EXENDTC")) ex <- ensure_char_col(ex, cn)

# 1. GET FIRST EXPOSURE RECORD PER SUBJECT : sort by USUBJID EXSTDTC (missing first, SAS binary)
message("Sorting EX by USUBJID EXSTDTC")
ex_first <- ex %>%
  mutate(.k1 = sas_bin_key(USUBJID),
         .k2 = sas_bin_key(ifelse(is_sas_miss_char(EXSTDTC), "", EXSTDTC)),
         .miss = is_sas_miss_char(EXSTDTC)) %>%
  arrange(.k1, .miss, .k2) %>%
  select(-.k1, -.k2, -.miss)

# keep only initial dosing record per USUBJID (FIRST.USUBJID)
ex_trt <- ex_first %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  select(USUBJID, EXTRT, EXSTDTC, EXENDTC)

# 2. SORT DM BEFORE MERGING
message("Sorting DM by USUBJID")
dm_sorted <- dm %>%
  mutate(.k = sas_bin_key(USUBJID)) %>%
  arrange(.k) %>%
  select(-.k)

# 3. MERGE DEMOGRAPHICS AND EXPOSURE
message("Merging DM + EX to create ADSL draft")
# indicator b: in ex_trt
ex_keys <- unique(ex_trt$USUBJID)
dm_sorted <- dm_sorted %>% mutate(.in_b = USUBJID %in% ex_keys)
adsl_draft <- dm_sorted %>%
  left_join(ex_trt, by = "USUBJID", suffix = c("", ".ex")) %>%
  # if a (all dm kept) - left join already keeps all
  mutate(
    # TRT01P
    TRT01P = ifelse(!is_sas_miss_char(ARMCD) & ARMCD == "SCRNFAIL", "UNPLANNED",
             ifelse(is_sas_miss_char(ARMCD) & FALSE, "UNPLANNED",
               ifelse(tolower(ARMCD) == tolower("SCRNFAIL") & !is.na(ARMCD), "UNPLANNED", ARM))),
    # Correct SAS: if ARMCD ne 'SCRNFAIL' then TRT01P=ARM else UNPLANNED
    # Recompute strictly: missing ARMCD (NA/"") ne SCRNFAIL => TRUE => ARM
    TRT01P = ifelse(is.na(ARMCD) | trimws(as.character(ARMCD)) != "SCRNFAIL", as.character(ARM), "UNPLANNED"),
    # TRT01A
    TRT01A = ifelse(.in_b & !is_sas_miss_char(EXTRT), as.character(EXTRT), "NOT TREATED"),
    # ITTFL
    ITTFL = ifelse(is.na(ARMCD) | trimws(as.character(ARMCD)) != "SCRNFAIL", "Y", "N"),
    # SAFFL
    SAFFL = ifelse(!is_sas_miss_char(EXSTDTC), "Y", "N")
  )

# Numeric date derivations
adsl_draft <- adsl_draft %>%
  mutate(
    BRTHDT = parse_yymmdd10(BRTHDTC),
    RFICDT = parse_yymmdd10(RFICDTC),
    TRTSDT = parse_yymmdd10(EXSTDTC),
    TRTEDT = parse_yymmdd10(EXENDTC),
    EOSDT  = parse_yymmdd10(RFPENDTC)
  ) %>%
  mutate(
    AGE = calc_age_yrdif(BRTHDT, RFICDT),
    TRTDURD = ifelse(!is.na(TRTSDT) & !is.na(TRTEDT), as.numeric(TRTEDT - TRTSDT) + 1, NA_real_)
  )

# keep list
adsl_keep <- adsl_draft %>%
  select(STUDYID, USUBJID, SUBJID, SITEID, TRT01P, TRT01A, ITTFL, SAFFL,
         BRTHDT, RFICDT, TRTSDT, TRTEDT, EOSDT, AGE, TRTDURD, SEX, RACE)

# 4. FINALIZE AND SAVE : sort by USUBJID
message("Final sort by USUBJID")
adsl <- adsl_keep %>%
  mutate(.k = sas_bin_key(USUBJID)) %>%
  arrange(.k) %>%
  select(-.k)

# Ensure types: dates as Date, AGE/TRTDURD numeric
adsl$BRTHDT <- as.Date(adsl$BRTHDT, origin = "1970-01-01")
adsl$RFICDT <- as.Date(adsl$RFICDT, origin = "1970-01-01")
adsl$TRTSDT <- as.Date(adsl$TRTSDT, origin = "1970-01-01")
adsl$TRTEDT <- as.Date(adsl$TRTEDT, origin = "1970-01-01")
adsl$EOSDT  <- as.Date(adsl$EOSDT, origin = "1970-01-01")
adsl$AGE <- as.numeric(adsl$AGE)
adsl$TRTDURD <- as.numeric(adsl$TRTDURD)

# Write dataset CSV: dates YYYY-MM-DD, na=""
out_adsl <- adsl
for (cn in c("BRTHDT","RFICDT","TRTSDT","TRTEDT","EOSDT")) {
  out_adsl[[cn]] <- format(out_adsl[[cn]], "%Y-%m-%d")
  out_adsl[[cn]][is.na(out_adsl[[cn]]) | out_adsl[[cn]] == "NA"] <- NA_character_
}
message(paste0("Writing adsl.csv with ", nrow(adsl), " rows"))
write.csv(out_adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT : PROC PRINT obs=10 var USUBJID TRT01P TRT01A ITTFL SAFFL AGE
audit <- adsl %>% slice(1:10) %>% select(USUBJID, TRT01P, TRT01A, ITTFL, SAFFL, AGE)
# CSV of table body
audit_csv <- audit
audit_csv$AGE <- as.character(audit_csv$AGE)
message("Writing P006 listing CSV/TXT")
write.csv(audit_csv, file.path(out, "P006.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
fmt_cell <- function(v, w) {
  if (is.na(v) || v == "NA") v <- ""
  v <- as.character(v)
  if (nchar(v) > w) v <- substr(v, 1, w)
  sprintf(paste0("%-", w, "s"), v)
}
# Determine widths
cols <- c("USUBJID","TRT01P","TRT01A","ITTFL","SAFFL","AGE")
widths <- c(USUBJID = max(nchar(c("USUBJID", as.character(audit$USUBJID))), na.rm = TRUE),
            TRT01P = max(nchar(c("TRT01P", as.character(audit$TRT01P))), na.rm = TRUE),
            TRT01A = max(nchar(c("TRT01A", as.character(audit$TRT01A))), na.rm = TRUE),
            ITTFL = max(nchar(c("ITTFL", as.character(audit$ITTFL))), na.rm = TRUE),
            SAFFL = max(nchar(c("SAFFL", as.character(audit$SAFFL))), na.rm = TRUE),
            AGE = max(nchar(c("AGE", as.character(audit$AGE))), na.rm = TRUE))
widths <- pmax(widths, c(12, 12, 12, 5, 5, 3))
# Build lines
lines <- c(
  "ADSL Audit - Subject Level Analysis Dataset",
  "",
  paste0("Dataset ADSL: ", nrow(adsl), " subjects; showing first 10 observations sorted by USUBJID"),
  ""
)
header <- paste0("Obs  ",
  fmt_cell("USUBJID", widths[["USUBJID"]]), "  ",
  fmt_cell("TRT01P", widths[["TRT01P"]]), "  ",
  fmt_cell("TRT01A", widths[["TRT01A"]]), "  ",
  fmt_cell("ITTFL", widths[["ITTFL"]]), "  ",
  fmt_cell("SAFFL", widths[["SAFFL"]]), "  ",
  fmt_cell("AGE", widths[["AGE"]]))
sep <- paste0(rep("-", nchar(header)), collapse = "")
lines <- c(lines, header, sep)
for (i in seq_len(nrow(audit))) {
  r <- audit[i, ]
  age_s <- ifelse(is.na(r$AGE), "", as.character(as.integer(r$AGE)))
  line <- paste0(sprintf("%-4d  ", i),
    fmt_cell(r$USUBJID, widths[["USUBJID"]]), "  ",
    fmt_cell(r$TRT01P, widths[["TRT01P"]]), "  ",
    fmt_cell(r$TRT01A, widths[["TRT01A"]]), "  ",
    fmt_cell(r$ITTFL, widths[["ITTFL"]]), "  ",
    fmt_cell(r$SAFFL, widths[["SAFFL"]]), "  ",
    fmt_cell(age_s, widths[["AGE"]]))
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N = ", nrow(adsl), " total subjects in ADSL"))
writeLines(lines, file.path(out, "P006.txt"))
# Also write .lst copy for benchmark expecting P006.lst
writeLines(lines, file.path(out, "P006.lst"))

message("Done.")
