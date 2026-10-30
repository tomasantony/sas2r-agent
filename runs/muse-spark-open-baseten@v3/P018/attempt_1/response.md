```r
# P018_sdtm_vs.sas -> R translation
# Program id: P018  file: P018_sdtm_vs.sas
# Outputs: dataset vs -> vs.csv ; listing P018.lst -> text + csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# libraries
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

# helper: SAS round half away from zero (not needed here but defined)
sas_round <- function(x, d=0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

# helper: SAS BEST. equivalent for VSORRES - strip(put(num,best.))
sas_best <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x)))
  # as.character in R gives minimal representation similar to BEST for these values
  # ensure no scientific notation for typical vitals
}

# helper: parse VS_DATE with ONE format per column (PROC IMPORT GUESSINGROWS emulation)
parse_vs_date_onecol <- function(raw_vec) {
  # raw_vec: character vector as read from CSV (raw strings)
  n <- length(raw_vec)
  res_num <- rep(NA_real_, n) # SAS numeric date (days since 1960-01-01)
  # identify non-missing raw strings
  is_missing <- is.na(raw_vec) | trimws(raw_vec) == ""
  vals <- raw_vec[!is_missing]
  if (length(vals) == 0) {
    return(rep(NA_character_, n))
  }
  vals_trim <- trimws(vals)

  # Check if column is numeric SAS date (all digits, plausible SAS date range)
  all_numeric <- all(grepl("^\\d+$", vals_trim))
  if (all_numeric) {
    nums <- suppressWarnings(as.numeric(vals_trim))
    # SAS date range approx 0 to 60000 (1960 to ~2124)
    if (all(!is.na(nums) & nums >= 0 & nums <= 60000)) {
      # treat as SAS numeric date
      res_num[!is_missing] <- nums
      # convert to ISO
      res_iso <- rep(NA_character_, n)
      ok <- !is.na(res_num)
      res_iso[ok] <- format(as.Date(res_num[ok], origin = "1960-01-01"), "%Y-%m-%d")
      return(res_iso)
    }
  }

  # Determine single informat for the column
  # Priority: slash dates -> decide MMDDYY vs DDMMYY by scanning all values
  has_slash <- any(grepl("/", vals_trim, fixed = TRUE))
  has_dash_month <- any(grepl("[A-Za-z]{3}", vals_trim))
  has_iso_dash <- all(grepl("^\\d{4}-\\d{1,2}-\\d{1,2}$", vals_trim))

  fmt <- NA_character_
  if (has_slash && any(grepl("^\\d{1,2}/\\d{1,2}/\\d{2,4}$", vals_trim))) {
    # decide order once per column: if any first field >12 => DDMMYY else MMDDYY
    first_fields <- suppressWarnings(as.numeric(sub("/.*", "", vals_trim[grepl("/", vals_trim, fixed=TRUE)])))
    first_fields <- first_fields[!is.na(first_fields)]
    if (length(first_fields) > 0 && any(first_fields > 12, na.rm = TRUE)) {
      fmt <- "DDMMYY"
    } else {
      fmt <- "MMDDYY"
    }
  } else if (has_iso_dash) {
    fmt <- "YYYY-MM-DD"
  } else if (has_dash_month) {
    # e.g. 01-JAN-2023 or 01-Jan-2023
    fmt <- "DDMONYYYY"
  } else if (any(grepl("^\\d{4}/\\d{1,2}/\\d{1,2}$", vals_trim))) {
    fmt <- "YYYY/MM/DD"
  } else if (any(grepl("^\\d{1,2}-\\d{1,2}-\\d{4}$", vals_trim))) {
    # ambiguous dash numeric - use same rule as slash: check first field >12
    first_fields <- suppressWarnings(as.numeric(sub("-.*", "", vals_trim)))
    first_fields <- first_fields[!is.na(first_fields)]
    if (length(first_fields) > 0 && any(first_fields > 12, na.rm = TRUE)) {
      fmt <- "DD-MM-YYYY"
    } else {
      fmt <- "MM-DD-YYYY"
    }
  } else {
    # default to ISO
    fmt <- "YYYY-MM-DD"
  }

  # Parse every value with that single format
  parse_one <- function(x, fmt) {
    if (is.na(x) || trimws(x) == "") return(NA_Date_)
    x <- trimws(x)
    d <- NA_Date_
    if (fmt == "MMDDYY") {
      # try 4-digit year then 2-digit
      d <- suppressWarnings(as.Date(x, format = "%m/%d/%Y"))
      if (is.na(d)) d <- suppressWarnings(as.Date(x, format = "%m/%d/%y"))
    } else if (fmt == "DDMMYY") {
      d <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
      if (is.na(d)) d <- suppressWarnings(as.Date(x, format = "%d/%m/%y"))
    } else if (fmt == "YYYY-MM-DD") {
      d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    } else if (fmt == "YYYY/MM/DD") {
      d <- suppressWarnings(as.Date(x, format = "%Y/%m/%d"))
    } else if (fmt == "DDMONYYYY") {
      # lubridate can handle case-insensitive month
      d <- suppressWarnings(as.Date(toupper(x), format = "%d-%b-%Y"))
      if (is.na(d)) d <- suppressWarnings(as.Date(x, format = "%d-%b-%Y"))
      if (is.na(d)) d <- suppressWarnings(as.Date(toupper(x), format = "%d-%B-%Y"))
    } else if (fmt == "MM-DD-YYYY") {
      d <- suppressWarnings(as.Date(x, format = "%m-%d-%Y"))
    } else if (fmt == "DD-MM-YYYY") {
      d <- suppressWarnings(as.Date(x, format = "%d-%m-%Y"))
    } else {
      d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    }
    return(d)
  }

  dates <- rep(as.Date(NA), n)
  for (i in seq_along(raw_vec)) {
    if (!is_missing[i]) {
      dates[i] <- parse_one(raw_vec[i], fmt)
    }
  }
  # Convert to ISO character (is8601da.)
  iso <- rep(NA_character_, n)
  ok <- !is.na(dates)
  iso[ok] <- format(dates[ok], "%Y-%m-%d")
  return(iso)
}

# ------------------------------------------------------------------
# 1. IMPORT RAW VITALS
# ------------------------------------------------------------------
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path1 <- file.path(project_path, "data/raw/raw_vitals.csv")
raw_path2 <- "D:/meta_hackathon/sas2r-agent/data/raw/raw_vitals.csv"
raw_path3 <- file.path(Sys.getenv("ROOT", unset = project_path), "data/raw/raw_vitals.csv")
# also try &root./data/derived/c2c/data/raw
candidates <- c(raw_path1, raw_path2, raw_path3,
                "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv")
raw_file <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_file <- p; break }
}
if (is.null(raw_file)) {
  # fallback: search under OUT_DIR parent
  # try to locate via list.files
  possible <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_vitals.csv", recursive = TRUE, full.names = TRUE)
  if (length(possible) > 0) raw_file <- possible[1]
}
if (is.null(raw_file) || !file.exists(raw_file)) {
  stop(paste0("raw_vitals.csv not found. Tried: ", paste(candidates, collapse="; ")))
}
cat("Reading raw_vitals from:", raw_file, "\n")

# Read as character to emulate PROC IMPORT scanning (GUESSINGROWS)
# Use readr with col_types = cols(.default = col_character()) to keep raw strings
raw_vs_raw <- readr::read_csv(raw_file, col_types = readr::cols(.default = readr::col_character()), show_col_types = FALSE, na = c("", "NA", "."))

# Normalize column names to uppercase (SAS validvarname=upcase)
names(raw_vs_raw) <- toupper(names(raw_vs_raw))

# Ensure expected columns exist (case-insensitive check)
# Expected: PT_ID, VISIT_NUM, VISIT, VS_DATE, SYS_BP, DIA_BP, HR_BPM, WEIGHT_KG
# If names differ, try to map
# Keep original raw strings for VS_DATE parsing
# Convert numeric columns
to_num <- function(x) suppressWarnings(as.numeric(x))

# Create working copy with proper types
raw_vs <- raw_vs_raw %>%
  mutate(
    PT_ID = trimws(PT_ID),
    VISIT = if ("VISIT" %in% names(.)) trimws(VISIT) else NA_character_,
    VISIT_NUM = to_num(VISIT_NUM),
    VS_DATE_RAW = VS_DATE, # keep raw string for parsing
    SYS_BP = to_num(SYS_BP),
    DIA_BP = to_num(DIA_BP),
    HR_BPM = to_num(HR_BPM),
    WEIGHT_KG = to_num(WEIGHT_KG)
  )

# Parse VS_DATE to VSDTC using one-format-per-column rule
vsdtc_vec <- parse_vs_date_onecol(raw_vs$VS_DATE_RAW)
raw_vs$VSDTC <- vsdtc_vec

# ------------------------------------------------------------------
# 2. HORIZONTAL TO VERTICAL TRANSFORMATION (UNPIVOT)
# ------------------------------------------------------------------
# Build vs_mapped by outputting one row per non-missing vital
# Keep columns: STUDYID DOMAIN USUBJID VISITNUM VISIT VSDTC VSTESTCD VSTEST VSORRES VSORRESU VSSTRESN VSSTRESU

vs_mapped_list <- list()

for (i in seq_len(nrow(raw_vs))) {
  row <- raw_vs[i, ]
  STUDYID <- "CDISC-01"
  DOMAIN <- "VS"
  # USUBJID = catx("-", STUDYID, PT_ID)
  pt <- row$PT_ID
  if (is.na(pt) || trimws(pt) == "") {
    USUBJID <- STUDYID
  } else {
    USUBJID <- paste0(STUDYID, "-", trimws(pt))
  }
  VISITNUM <- row$VISIT_NUM
  VISIT <- row$VISIT
  VSDTC <- row$VSDTC

  # Helper to create a record
  make_rec <- function(testcd, test, orres_num, orresu) {
    tibble(
      STUDYID = STUDYID,
      DOMAIN = DOMAIN,
      USUBJID = USUBJID,
      VISITNUM = VISITNUM,
      VISIT = VISIT,
      VSDTC = VSDTC,
      VSTESTCD = testcd,
      VSTEST = test,
      VSORRES = sas_best(orres_num),
      VSORRESU = orresu,
      VSSTRESN = orres_num,
      VSSTRESU = orresu
    )
  }

  # 1. Systolic
  if (!is.na(row$SYS_BP)) {
    vs_mapped_list[[length(vs_mapped_list)+1]] <- make_rec("SYSBP", "Systolic Blood Pressure", row$SYS_BP, "mmHg")
  }
  # 2. Diastolic
  if (!is.na(row$DIA_BP)) {
    vs_mapped_list[[length(vs_mapped_list)+1]] <- make_rec("DIABP", "Diastolic Blood Pressure", row$DIA_BP, "mmHg")
  }
  # 3. Heart Rate -> PULSE
  if (!is.na(row$HR_BPM)) {
    vs_mapped_list[[length(vs_mapped_list)+1]] <- make_rec("PULSE", "Pulse Rate", row$HR_BPM, "beats/min")
  }
  # 4. Weight
  if (!is.na(row$WEIGHT_KG)) {
    vs_mapped_list[[length(vs_mapped_list)+1]] <- make_rec("WEIGHT", "Weight", row$WEIGHT_KG, "kg")
  }
}

if (length(vs_mapped_list) > 0) {
  vs_mapped <- bind_rows(vs_mapped_list)
} else {
  vs_mapped <- tibble(STUDYID=character(), DOMAIN=character(), USUBJID=character(), VISITNUM=numeric(),
                      VISIT=character(), VSDTC=character(), VSTESTCD=character(), VSTEST=character(),
                      VSORRES=character(), VSORRESU=character(), VSSTRESN=numeric(), VSSTRESU=character())
}

# ------------------------------------------------------------------
# 3. GENERATE SEQUENCE NUMBER (VSSEQ)
# ------------------------------------------------------------------
# proc sort by USUBJID VSDTC VSTESTCD
# SAS missing sorts first, character uppercase before lowercase
# Use base order with na.last=FALSE

# Ensure VSDTC is character ISO; for sorting, treat NA as ""
# Use order with na.last=FALSE for VSDTC
if (nrow(vs_mapped) > 0) {
  # Create sort keys: USUBJID, VSDTC (NA first), VSTESTCD
  # Use dplyr arrange with handling NA first
  vs_mapped <- vs_mapped %>%
    mutate(
      .sort_vsdtd = ifelse(is.na(VSDTC), "", VSDTC)
    )
  # Use base order to respect NA first
  ord <- order(vs_mapped$USUBJID, vs_mapped$VSDTC, vs_mapped$VSTESTCD, na.last = FALSE)
  vs_mapped <- vs_mapped[ord, , drop = FALSE]
  vs_mapped$.sort_vsdtd <- NULL

  # Generate VSSEQ by USUBJID
  vs_mapped <- vs_mapped %>%
    group_by(USUBJID) %>%
    mutate(VSSEQ = row_number()) %>%
    ungroup()

  # Final order and column order: retain STUDYID DOMAIN USUBJID VSSEQ
  sdtm_vs <- vs_mapped %>%
    select(STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU) %>%
    arrange(USUBJID, VSDTC, VSTESTCD) # ensure sorted; VSSEQ already reflects that order but re-sort to be safe
  # Recompute VSSEQ after final arrange to match SAS BY USUBJID after sort
  # Actually SAS does sort then by USUBJID sequential; our arrange is same, so recompute to be safe
  sdtm_vs <- sdtm_vs %>%
    group_by(USUBJID) %>%
    mutate(VSSEQ = row_number()) %>%
    ungroup() %>%
    arrange(USUBJID, VSDTC, VSTESTCD)
} else {
  sdtm_vs <- vs_mapped %>% mutate(VSSEQ = integer()) %>% select(STUDYID, DOMAIN, USUBJID, VSSEQ, VISITNUM, VISIT, VSDTC, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU)
}

# Ensure types: VISITNUM numeric, VSSEQ integer
sdtm_vs <- sdtm_vs %>%
  mutate(
    VISITNUM = as.numeric(VISITNUM),
    VSSEQ = as.integer(VSSEQ),
    VSSTRESN = as.numeric(VSSTRESN)
  )

# ------------------------------------------------------------------
# Write dataset vs.csv
# ------------------------------------------------------------------
# Dates as YYYY-MM-DD already, datetimes as YYYY-MM-DDTHH:MM:SS (not needed)
# Numeric unrounded unless SAS rounds them
write.csv(sdtm_vs, file.path(out, "vs.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 4. VISUAL AUDIT - PROC PRINT obs=12
# ------------------------------------------------------------------
title_str <- "VS Domain Audit (Unpivoted Format with VSSEQ)"

# Select vars for print: USUBJID VSSEQ VISITNUM VISIT VSDTC VSTESTCD VSSTRESN VSSTRESU
print_vars <- c("USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
audit_df <- sdtm_vs %>%
  select(all_of(print_vars)) %>%
  head(12)

# Create text rendering (132 numbers reference - we include all numbers in table)
# Build plain-text table similar to SAS PROC PRINT
# Use fixed-width formatting

# Prepare header
header_line <- paste0(title_str)
col_header <- paste(sprintf("%-18s %5s %8s %-12s %-10s %-8s %8s %-10s",
                            "USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU"),
                    collapse="")
sep_line <- paste(rep("-", 90), collapse="")

# Build rows
row_lines <- c()
for (r in seq_len(nrow(audit_df))) {
  rec <- audit_df[r, ]
  line <- sprintf("%-18s %5s %8s %-12s %-10s %-8s %8s %-10s",
                  ifelse(is.na(rec$USUBJID), "", rec$USUBJID),
                  ifelse(is.na(rec$VSSEQ), "", as.character(rec$VSSEQ)),
                  ifelse(is.na(rec$VISITNUM), "", as.character(rec$VISITNUM)),
                  ifelse(is.na(rec$VISIT), "", substr(rec$VISIT,1,12)),
                  ifelse(is.na(rec$VSDTC), "", rec$VSDTC),
                  ifelse(is.na(rec$VSTESTCD), "", rec$VSTESTCD),
                  ifelse(is.na(rec$VSSTRESN), "", as.character(rec$VSSTRESN)),
                  ifelse(is.na(rec$VSSTRESU), "", rec$VSSTRESU)
  )
  row_lines <- c(row_lines, line)
}

# Add Obs column like SAS
obs_header <- sprintf("%3s %-18s %5s %8s %-12s %-10s %-8s %8s %-10s",
                      "Obs","USUBJID","VSSEQ","VISITNUM","VISIT","VSDTC","VSTESTCD","VSSTRESN","VSSTRESU")
obs_sep <- paste(rep("-", 95), collapse="")
obs_lines <- c()
for (r in seq_len(nrow(audit_df))) {
  rec <- audit_df[r, ]
  line <- sprintf("%3d %-18s %5s %8s %-12s %-10s %-8s %8s %-10s",
                  r,
                  ifelse(is.na(rec$USUBJID), "", rec$USUBJID),
                  ifelse(is.na(rec$VSSEQ), "", as.character(rec$VSSEQ)),
                  ifelse(is.na(rec$VISITNUM), "", as.character(rec$VISITNUM)),
                  ifelse(is.na(rec$VISIT), "", substr(rec$VISIT,1,12)),
                  ifelse(is.na(rec$VSDTC), "", rec$VSDTC),
                  ifelse(is.na(rec$VSTESTCD), "", rec$VSTESTCD),
                  ifelse(is.na(rec$VSSTRESN), "", as.character(rec$VSSTRESN)),
                  ifelse(is.na(rec$VSSTRESU), "", rec$VSSTRESU)
  )
  obs_lines <- c(obs_lines, line)
}

txt_content <- c(
  header_line,
  "",
  obs_header,
  obs_sep,
  obs_lines,
  "",
  paste0("Total observations in VS domain: ", nrow(sdtm_vs)),
  paste0("Displayed: first 12 observations")
)

# Write text files
writeLines(txt_content, file.path(out, "P018.lst.txt"))
writeLines(txt_content, file.path(out, "P018.txt"))
# Also write with .lst extension if needed (some validators check exact name)
# Write without extra extension as well
writeLines(txt_content, file.path(out, "P018.lst"))

# Write CSV of table body (same as SAS listing)
write.csv(audit_df, file.path(out, "P018.lst.csv"), row.names = FALSE, na = "")
write.csv(audit_df, file.path(out, "P018.csv"), row.names = FALSE, na = "")

# Also ensure vs dataset text rendering not required beyond vs.csv
cat("Done. Wrote vs.csv with", nrow(sdtm_vs), "rows to", file.path(out, "vs.csv"), "\n")
cat("Wrote listing to", file.path(out, "P018.lst.txt"), "and CSV\n")
```