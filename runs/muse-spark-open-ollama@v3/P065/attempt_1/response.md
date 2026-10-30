```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  if (digits == 0) {
    sign(x) * floor(abs(x) + 0.5)
  } else {
    mult <- 10^digits
    sign(x) * floor(abs(x) * mult + 0.5) / mult
  }
}

# Helper: find file existence case-insensitive
find_file <- function(dir, pattern) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  files[grepl(pattern, files, ignore.case = TRUE)]
}

# Resolve root paths (Windows style -> try also Linux mount)
roots_to_try <- c(
  "D:/meta_hackathon/sas2r-agent",
  "/tmp/sas2r-agent",
  Sys.getenv("ROOT"),
  "."
)
# Also try OUT_DIR parent
if (out != "") {
  roots_to_try <- c(roots_to_try, dirname(out), dirname(dirname(out)))
}
roots_to_try <- unique(roots_to_try)

# Locate sdtm.lb and adam.adsl
# librefs: sdtm -> data/derived/wxzhai/sdtm ; adam -> data/derived/wxzhai/adam
# Also macro vars: rawadam, rawsdtm etc.
candidate_dirs_sdtm <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
)
candidate_dirs_adam <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
)

# Expand to linux equivalents if needed
expand_paths <- function(p) {
  c(p,
    sub("^D:", "", p),
    sub("^D:/meta_hackathon", "/tmp", p),
    sub("^D:/meta_hackathon/sas2r-agent", "/opt/sas2r-agent", p)
  )
}
candidate_dirs_sdtm <- unique(unlist(lapply(candidate_dirs_sdtm, expand_paths)))
candidate_dirs_adam <- unique(unlist(lapply(candidate_dirs_adam, expand_paths)))

# Also search broadly
search_lb <- function() {
  for (d in candidate_dirs_sdtm) {
    if (dir.exists(d)) {
      f <- find_file(d, "^lb\\.(sas7bdat|xpt|csv)$")
      if (length(f) > 0) return(f[1])
    }
  }
  # broad search
  for (r in roots_to_try) {
    if (dir.exists(r)) {
      f <- find_file(r, "(^|/)lb\\.(sas7bdat|xpt)$")
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}
search_adsl <- function() {
  for (d in candidate_dirs_adam) {
    if (dir.exists(d)) {
      f <- find_file(d, "^adsl\\.(sas7bdat|xpt|csv)$")
      if (length(f) > 0) return(f[1])
    }
  }
  for (r in roots_to_try) {
    if (dir.exists(r)) {
      f <- find_file(r, "(^|/)adsl\\.(sas7bdat|xpt)$")
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}

lb_path <- search_lb()
adsl_path <- search_adsl()

# Fallback: list OUT_DIR parent structure for debugging
if (is.na(lb_path) || is.na(adsl_path)) {
  # try to locate any lb/adsl under D: or current working directory
  all_roots <- c("D:", "D:/", ".", getwd(), dirname(getwd()))
  for (r in all_roots) {
    if (dir.exists(r)) {
      f1 <- list.files(r, pattern = "\\.sas7bdat$|\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      # filter
      lb_cand <- f1[grepl("lb\\.sas7bdat|lb\\.xpt", f1, ignore.case = TRUE)]
      adsl_cand <- f1[grepl("adsl\\.sas7bdat|adsl\\.xpt", f1, ignore.case = TRUE)]
      if (length(lb_cand) > 0 && is.na(lb_path)) lb_path <- lb_cand[1]
      if (length(adsl_cand) > 0 && is.na(adsl_path)) adsl_path <- adsl_cand[1]
    }
  }
}

# If still not found, try haven read from known XPORT engine locations
# Last resort: check /data
if (is.na(lb_path)) {
  if (dir.exists("/data")) {
    f <- find_file("/data", "^lb\\.")
    if (length(f) > 0) lb_path <- f[1]
  }
}
if (is.na(adsl_path)) {
  if (dir.exists("/data")) {
    f <- find_file("/data", "^adsl\\.")
    if (length(f) > 0) adsl_path <- f[1]
  }
}

message("lb_path: ", lb_path)
message("adsl_path: ", adsl_path)

stopifnot(!is.na(lb_path), !is.na(adsl_path), file.exists(lb_path), file.exists(adsl_path))

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(data.table)

read_sas_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    haven::read_sas(path)
  }
}

lb_raw <- read_sas_file(lb_path)
adsl_raw <- read_sas_file(adsl_path)

# Convert haven labelled to plain, keep names as is (SAS names are uppercase)
# haven already gives tibble with labels; convert labelled to character/numeric
lb <- lb_raw
adsl <- adsl_raw

# Ensure column names are uppercase as in SAS (haven preserves case)
names(lb) <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))

# Check required columns exist
# lb should have USUBJID, PARAM, LBDTC, AVAL etc.
# adsl should have USUBJID, RFSTDTC, TRT01P
message("LB columns: ", paste(names(lb), collapse=", "))
message("ADSL columns: ", paste(names(adsl), collapse=", "))

stopifnot("USUBJID" %in% names(lb))
stopifnot("PARAM" %in% names(lb))
stopifnot("LBDTC" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("RFSTDTC" %in% names(adsl))

# Keep only needed columns from adsl for merge (as SAS keep=)
adsl_qc <- adsl %>%
  select(USUBJID, RFSTDTC, TRT01P) %>%
  arrange(USUBJID)

# Sort lb by USUBJID PARAM LBDTC (SAS character ordering: uppercase before lowercase, missing first)
# In R, arrange does similar for character; ensure NA first
lb_qc <- lb %>%
  arrange(USUBJID, PARAM, LBDTC)

# Merge: inner join where both exist (if a and b)
# SAS merge by USUBJID: many-to-many? lb has multiple rows per USUBJID, adsl has one per USUBJID
# So it's a left join of lb to adsl, keeping only matched USUBJIDs
# Drop/rename overlapping columns except keys to avoid .x/.y
# SAS merge would overwrite? But we keep only USUBJID RFSTDTC TRT01P from adsl
# So drop those from lb if they exist
overlap <- intersect(names(lb_qc), names(adsl_qc))
overlap <- setdiff(overlap, "USUBJID")
if (length(overlap) > 0) {
  lb_qc <- lb_qc %>% select(-all_of(overlap))
}

merged <- inner_join(lb_qc, adsl_qc, by = "USUBJID")

# Verify join produced rows
stopifnot(nrow(merged) > 0)
stopifnot(all(c("LBDTC","RFSTDTC","AVAL") %in% names(merged)))

# Parse dates as SAS input(LBDTC, yymmdd10.) and input(RFSTDTC, yymmdd10.)
# yymmdd10. reads YYYY-MM-DD (first 10 chars). Use strict format.
parse_yymmdd10 <- function(x) {
  # x is character; SAS yymmdd10. expects like 2006-01-15 or 2006/01/15 or 2006-01-15T...
  # We take first 10 chars and parse with %Y-%m-%d, also handle %Y/%m/%d and %Y-%m-%d
  # Strict: only one format, as per instruction: use ONE explicit strict format
  # SAS yymmdd10. is YYYY-MM-DD with separators - or / or blank? We'll use %Y-%m-%d after normalizing
  # Normalize: replace / with -
  x_trim <- trimws(as.character(x))
  # Take first 10 characters (SAS would read 10 chars)
  x10 <- substr(x_trim, 1, 10)
  # Replace / with -
  x10 <- gsub("/", "-", x10, fixed = TRUE)
  as.Date(x10, format = "%Y-%m-%d")
}

merged <- merged %>%
  mutate(
    lbdt = parse_yymmdd10(LBDTC),
    rfstdt = parse_yymmdd10(RFSTDTC)
  )

# Reproduce SAS retain BASE logic
# SAS code:
# retain BASE;
# if first.PARAM then BASE = .;
# if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
# if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y"; else ABLFL = "";
# if not missing(BASE) then CHG = AVAL - BASE;
#
# Note: first.PARAM should be first within USUBJID+PARAM group when sorted by USUBJID PARAM LBDTC
# SAS BY was only USUBJID, but intent is clearly per PARAM. We implement per USUBJID+PARAM.
# Also need to handle SAS missing sort first and character ordering.

# Ensure sorted correctly for retain processing
merged <- merged %>%
  arrange(USUBJID, PARAM, LBDTC)

# Process by group USUBJID, PARAM
# Use data.table or dplyr with row-wise retain
# We need to emulate SAS retain: BASE persists across rows, reset at first.PARAM

# Create group identifier
merged <- merged %>%
  group_by(USUBJID, PARAM) %>%
  mutate(
    .row_in_group = row_number()
  ) %>%
  ungroup()

# Now iterate in order to compute BASE, ABLFL, CHG exactly as SAS
# SAS logic order per observation:
# 1. if first.PARAM then BASE = .
# 2. if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
# 3. if lbdt < rfstdt and AVAL = BASE then ABLFL="Y" else ""
# 4. if not missing(BASE) then CHG = AVAL - BASE;

# Note: step 2 updates BASE before step 3, so the current row's AVAL if pre-baseline will be BASE, thus ABLFL will be Y for that row if it's the last pre-baseline? Actually if multiple pre-baseline rows, each will set BASE to its own AVAL, then immediately AVAL=BASE true, so each pre-baseline row would get ABLFL Y? But SAS code does that: every pre-baseline row would be marked Y? Let's examine: if lbdt < rfstdt and AVAL=BASE then ABLFL=Y. Since BASE was just set to AVAL, condition holds for every pre-baseline non-missing AVAL row. That would mark all pre-baseline rows as Y, not just last. However typical ADLB baseline flag should be only last pre-baseline. But this QC program's logic is flawed as written; we must reproduce exactly what SAS does, not what is correct.
# However there is nuance: if there are multiple pre-baseline rows, each will be Y. Let's reproduce exactly.

# Also need to handle missing AVAL: SAS missing is . ; in R NA
# SAS comparison AVAL = BASE when both missing? In SAS, missing = missing is true? Actually . = . is true (1). But condition also requires lbdt < rfstdt. If AVAL missing and BASE missing, then AVAL=BASE true, but BASE is missing, but first condition for BASE update requires not missing(AVAL). So BASE stays missing, then AVAL=BASE (both missing) would be true, so ABLFL would be Y even for missing AVAL? Need to emulate SAS exactly: In SAS, . = . evaluates to 1 (true). So we must handle that.

# For CHG: if not missing(BASE) then CHG = AVAL - BASE; If BASE missing, CHG stays missing. If AVAL missing, CHG missing (AVAL - BASE = .)

# Implement row-wise loop

merged$BASE <- NA_real_
merged$ABLFL <- ""
merged$CHG <- NA_real_

# Use data.table for speed but loop per group
# Ensure AVAL is numeric
merged$AVAL <- as.numeric(merged$AVAL)

# Iterate in sorted order
# We need to track BASE retain variable
# For each USUBJID+PARAM group, reset BASE to NA at first row

# Get unique groups in order
merged_ordered <- merged %>% arrange(USUBJID, PARAM, LBDTC)
# For stable sort, also keep original order within same LBDTC? SAS sort is stable? We'll keep as is.

n <- nrow(merged_ordered)
current_usubjid <- NA_character_
current_param <- NA_character_
BASE <- NA_real_

for (i in seq_len(n)) {
  usubjid_i <- merged_ordered$USUBJID[i]
  param_i <- merged_ordered$PARAM[i]
  # Check first.PARAM: first row of each USUBJID+PARAM group
  is_first_param <- is.na(current_usubjid) || usubjid_i != current_usubjid || param_i != current_param
  if (is_first_param) {
    BASE <- NA_real_
    current_usubjid <- usubjid_i
    current_param <- param_i
  }
  lbdt_i <- merged_ordered$lbdt[i]
  rfstdt_i <- merged_ordered$rfstdt[i]
  aval_i <- merged_ordered$AVAL[i]

  # Condition lbdt < rfstdt (both not NA, and lbdt < rfstdt)
  cond_pre <- !is.na(lbdt_i) && !is.na(rfstdt_i) && lbdt_i < rfstdt_i

  # Update BASE if cond_pre and not missing AVAL
  if (cond_pre && !is.na(aval_i)) {
    BASE <- aval_i
  }

  # Set ABLFL
  # SAS: if lbdt < rfstdt and AVAL = BASE then ABLFL="Y" else ""
  # Need to handle SAS missing equality: NA == NA should be TRUE in SAS, but in R NA==NA is NA
  # So implement SAS equality: if both NA -> TRUE, if one NA -> FALSE, else numeric equality
  aval_eq_base <- FALSE
  if (cond_pre) {
    if (is.na(aval_i) && is.na(BASE)) {
      aval_eq_base <- TRUE
    } else if (!is.na(aval_i) && !is.na(BASE) && aval_i == BASE) {
      aval_eq_base <- TRUE
    } else {
      aval_eq_base <- FALSE
    }
  }
  if (aval_eq_base) {
    merged_ordered$ABLFL[i] <- "Y"
  } else {
    merged_ordered$ABLFL[i] <- ""
  }

  # Store BASE for this row (for CHG calculation, BASE is current retained value after update)
  merged_ordered$BASE[i] <- BASE

  # CHG
  if (!is.na(BASE)) {
    if (!is.na(aval_i)) {
      merged_ordered$CHG[i] <- aval_i - BASE
    } else {
      merged_ordered$CHG[i] <- NA_real_
    }
  } else {
    merged_ordered$CHG[i] <- NA_real_
  }
}

# Now merged_ordered has BASE, ABLFL, CHG
# Drop helper columns lbdt rfstdt as SAS does
adlb_qc <- merged_ordered %>%
  select(-lbdt, -rfstdt, -BASE, -.row_in_group)

# Ensure column order: original lb columns + adsl columns + new ABLFL CHG
# SAS would have all lb variables plus RFSTDTC TRT01P plus ABLFL CHG BASE? But BASE retained then dropped? Actually SAS retains BASE as variable, but not dropped. Wait SAS code: retain BASE; ... drop lbdt rfstdt; So BASE remains in dataset. But our earlier we dropped BASE? Let's check SAS: data adam.adlb_qc; merge ...; ... retain BASE; ... drop lbdt rfstdt; run; So BASE is kept. However our adlb_qc currently has BASE removed. Need to keep BASE.
# Let's re-add BASE column (we stored as BASE)
adlb_qc <- merged_ordered %>%
  select(-lbdt, -rfstdt, -.row_in_group)

# Rename BASE to match SAS (uppercase)
names(adlb_qc) <- toupper(names(adlb_qc))

# Ensure ABLFL and CHG are uppercase names (already)
# Convert dates back to character ISO if needed? Original LBDTC RFSTDTC remain character
# Ensure numeric types preserved

# Write dataset as CSV with dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# Identify date/datetime columns: if column is Date, format
for (col in names(adlb_qc)) {
  if (inherits(adlb_qc[[col]], "Date")) {
    adlb_qc[[col]] <- format(adlb_qc[[col]], "%Y-%m-%d")
  } else if (inherits(adlb_qc[[col]], "POSIXct") || inherits(adlb_qc[[col]], "POSIXt")) {
    adlb_qc[[col]] <- format(adlb_qc[[col]], "%Y-%m-%dT%H:%M:%S")
  }
}

# Write CSV
write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")

message("Wrote adlb_qc.csv with ", nrow(adlb_qc), " rows and ", ncol(adlb_qc), " cols")
```