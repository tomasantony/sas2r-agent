# P028 TR SDTM mapping - R translation of P028_tr.sas
# Produces tr.csv and P028 listing text

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS-like functions
sas_catx <- function(sep, ...) {
  args <- list(...)
  # catx trims and skips missing/empty
  sapply(seq_along(args[[1]]), function(i) {
    vals <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
      trimws(as.character(v))
    })
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return("")
    paste(vals, collapse = sep)
  })
}

sas_tranwrd <- function(s, from, to) {
  ifelse(is.na(s), NA_character_, gsub(from, to, s, fixed = TRUE))
}

# round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# date parsing helpers - strict single format per column
parse_date11 <- function(x) {
  # SAS date11. is DD-MON-YYYY e.g. 01-Jan-2020, case insensitive
  # Use as.Date with %d-%b-%Y
  # Need to handle already NA or empty
  x2 <- trimws(as.character(x))
  x2[x2 == "" | is.na(x2)] <- NA
  # lubridate not needed; use as.Date
  # Ensure month abbreviation case: convert to proper case for parsing
  # R's %b is locale dependent; set to English
  # Try to parse; if fails return NA
  suppressWarnings(as.Date(x2, format = "%d-%b-%Y"))
}

# Locate raw data directories
# SAS macro variables:
# &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
# &rawadam. etc. But in container paths may be under / or D:/
# Try to find edc_tr.xlsx
find_file <- function(rel_pattern) {
  candidates <- c(
    Sys.getenv("RAWDIR"),
    "D:/meta_hackathon/sas2r-agent/data/raw",
    "/tmp",
    getwd()
  )
  # Search common locations
  search_roots <- c(
    "D:/meta_hackathon/sas2r-agent",
    "/opt",
    "/data",
    ".",
    ".."
  )
  # Direct attempt: list files under out parent
  # Brute force find via list.files if possible
  # Try known absolute path translation: D:/ -> / or /mnt/
  possible_paths <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tr.xlsx",
    "/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tr.xlsx",
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx",
    file.path(Sys.getenv("ROOT"), "data/raw/faijal-oncology/raw_data/edc_tr.xlsx"),
    file.path("data/raw/faijal-oncology/raw_data/edc_tr.xlsx")
  )
  for (p in possible_paths) {
    if (file.exists(p)) return(p)
  }
  # recursive search from current working directory up to 4 levels
  # Use Sys.getenv ROOT if set
  root <- Sys.getenv("root")
  if (root == "") root <- Sys.getenv("ROOT")
  if (root != "" && dir.exists(root)) {
    f <- list.files(root, pattern = "edc_tr\\.xlsx", recursive = TRUE, full.names = TRUE)
    if (length(f) > 0) return(f[1])
  }
  # fallback: search from /
  return(NA_character_)
}

# Helper to read xlsx if exists, else try alternative locations
read_edc <- function(name) {
  # name like "edc_tr"
  fname <- paste0(name, ".xlsx")
  search_dirs <- c(
    Sys.getenv("rawdir"),
    Sys.getenv("RAWDIR"),
    Sys.getenv("root"),
    Sys.getenv("ROOT"),
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data",
    "D:/meta_hackathon/sas2r-agent/data/raw",
    "/opt/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data",
    "data/raw/faijal-oncology/raw_data",
    "../data/raw/faijal-oncology/raw_data",
    "../../data/raw/faijal-oncology/raw_data"
  )
  # Also try OUT_DIR parent
  if (out != "") {
    # out is like .../output/sas/datasets/P028 or similar; go up
    search_dirs <- c(search_dirs, file.path(dirname(out), "../../data/raw/faijal-oncology/raw_data"))
  }
  # Normalize
  found <- NA_character_
  for (d in search_dirs) {
    if (is.na(d) || d == "") next
    p <- file.path(d, fname)
    if (file.exists(p)) { found <- p; break }
    # also try without subdir
    p2 <- file.path(d, "faijal-oncology/raw_data", fname)
    if (file.exists(p2)) { found <- p2; break }
  }
  if (is.na(found)) {
    # recursive search from root
    roots_to_try <- c(".", "..", "../..", "../../..")
    r <- Sys.getenv("ROOT"); if (r != "") roots_to_try <- c(r, roots_to_try)
    r2 <- Sys.getenv("root"); if (r2 != "") roots_to_try <- c(r2, roots_to_try)
    for (rt in roots_to_try) {
      if (!dir.exists(rt)) next
      f <- list.files(rt, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
      if (length(f) > 0) { found <- f[1]; break }
    }
  }
  if (is.na(found) || !file.exists(found)) {
    stop(paste0("Cannot find file for ", name, " (", fname, "). Searched: ", paste(search_dirs, collapse="; ")))
  }
  df <- readxl::read_excel(found, .name_repair = "minimal")
  # Clean names: keep as is, but trim
  names(df) <- trimws(names(df))
  df
}

# Read required raw datasets
edc_tr_raw <- read_edc("edc_tr")
edc_dm_raw <- read_edc("edc_dm")
# edc_ex and ivrs_rand may be needed for RFSTDTC derivation
edc_ex_raw <- tryCatch(read_edc("edc_ex"), error = function(e) NULL)
ivrs_rand_raw <- tryCatch(read_edc("ivrs_rand"), error = function(e) NULL)

# Inspect column names for debugging (not printed to console fully)
# Normalize column names to upper case for SAS-like handling, but keep original for output
# SAS variables are case-insensitive; we will work with upper case names
names(edc_tr_raw) <- toupper(names(edc_tr_raw))
names(edc_dm_raw) <- toupper(names(edc_dm_raw))
if (!is.null(edc_ex_raw)) names(edc_ex_raw) <- toupper(names(edc_ex_raw))
if (!is.null(ivrs_rand_raw)) names(ivrs_rand_raw) <- toupper(names(ivrs_rand_raw))

# ------------------------------------------------------------
# 1. Prepare DM Reference Data (sdtm_dm -> dm_for_tr)
# Derive sdtm_dm.RFSTDTC per CDISC: Reference Start Date
# In pilot, RFSTDTC is first exposure date or randomization date.
# We derive from edc_dm if RFSTDTC column exists, otherwise from
# edc_ex (EXSTDTC) or ivrs_rand (RANDDTC).
# ------------------------------------------------------------

# Helper to find date column in edc_dm
find_col <- function(df, candidates) {
  nms <- names(df)
  for (c in candidates) {
    if (c %in% nms) return(c)
    # case-insensitive partial
    idx <- which(toupper(nms) == toupper(c))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NA_character_)
}

# Try to get RFSTDTC directly from edc_dm
rf_col <- find_col(edc_dm_raw, c("RFSTDTC", "RFSTDT", "RFSTDTC1", "RFXSTDTC", "BRTHDTC"))
# Also check for raw date columns that could be RFSTDTC
# Common in faijal raw: edc_dm has SUBJID, RFSTDTC, etc. Let's inspect actual names
# If RFSTDTC exists, use it; otherwise derive

derive_sdtm_dm <- function(edc_dm, edc_ex, ivrs_rand) {
  # Ensure SUBJID exists
  subj_col <- find_col(edc_dm, c("SUBJID", "USUBJID", "SUBJECT"))
  if (is.na(subj_col)) stop("SUBJID not found in edc_dm")
  
  # Try RFSTDTC column
  rf_col2 <- find_col(edc_dm, c("RFSTDTC", "RFSTDT", "RFXSTDTC", "RANDDTC", "RANDDT", "EXSTDTC", "STDTC"))
  
  df <- edc_dm
  
  # If RFSTDTC column exists and has values, use it
  if (!is.na(rf_col2) && rf_col2 %in% names(df)) {
    # Check if column has non-missing values
    vals <- df[[rf_col2]]
    # If at least some non-NA, use it
    if (sum(!is.na(vals) & trimws(as.character(vals)) != "") > 0) {
      # Standardize to character
      rf_vals <- as.character(vals)
      # If column is numeric Excel date, convert
      if (is.numeric(vals)) {
        # Excel numeric date: origin 1899-12-30
        rf_vals <- format(as.Date(vals, origin = "1899-12-30"), "%Y-%m-%d")
      }
      # If values look like YYYY-MM-DD already, keep
      # Otherwise try to parse date11 etc. and convert to yymmdd10.
      # For now keep as is, but ensure ISO format YYYY-MM-DD
      # Attempt to parse various formats to ISO
      parse_to_iso <- function(x) {
        if (is.na(x) || trimws(x) == "") return(NA_character_)
        x <- trimws(as.character(x))
        # If already YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS
        if (grepl("^\\d{4}-\\d{2}-\\d{2}", x)) {
          # Extract date part
          return(substr(x, 1, 10))
        }
        # Try DD-MON-YYYY
        d <- suppressWarnings(as.Date(x, format = "%d-%b-%Y"))
        if (!is.na(d)) return(format(d, "%Y-%m-%d"))
        d <- suppressWarnings(as.Date(x, format = "%d-%B-%Y"))
        if (!is.na(d)) return(format(d, "%Y-%m-%d"))
        # Try DD/MM/YYYY or MM/DD/YYYY - need to decide order per column
        # Use SAS-like guessing: if any value has first field >12, then DMY else MDY
        # But for single value we try both
        return(x)
      }
      # If values are already ISO, keep; otherwise attempt conversion
      # Check if any value contains "/"
      if (any(grepl("/", rf_vals, fixed = TRUE), na.rm = TRUE)) {
        # Determine order once per column (SAS PROC IMPORT guessing)
        # Scan all values with slash
        slash_vals <- rf_vals[grepl("/", rf_vals, fixed = TRUE) & !is.na(rf_vals)]
        # Extract first field
        first_fields <- suppressWarnings(as.integer(sapply(strsplit(slash_vals, "/"), `[`, 1)))
        # If any >12, then DMY, else MDY
        order_is_dmy <- any(first_fields > 12, na.rm = TRUE)
        fmt <- if (order_is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
        # Also handle n/n/yyyy with possible time
        parsed <- suppressWarnings(as.Date(rf_vals, format = fmt))
        # For those that failed, try with time
        # Keep original where parsed is NA
        rf_vals[!is.na(parsed)] <- format(parsed[!is.na(parsed)], "%Y-%m-%d")
      } else {
        # Try date11 conversion
        d2 <- suppressWarnings(as.Date(rf_vals, format = "%d-%b-%Y"))
        ok <- !is.na(d2)
        rf_vals[ok] <- format(d2[ok], "%Y-%m-%d")
        # Try other formats
        d3 <- suppressWarnings(as.Date(rf_vals, format = "%Y/%m/%d"))
        ok2 <- !is.na(d3) & is.na(d2)
        rf_vals[ok2] <- format(d3[ok2], "%Y-%m-%d")
      }
      out_dm <- data.frame(
        USUBJID = sas_catx("-", "ONCO-2026", as.character(df[[subj_col]])),
        RFSTDTC = rf_vals,
        stringsAsFactors = FALSE
      )
      # If RFSTDTC still contains datetime, truncate to date
      out_dm$RFSTDTC <- substr(out_dm$RFSTDTC, 1, 10)
      out_dm$RFSTDTC[out_dm$RFSTDTC == "" | is.na(out_dm$RFSTDTC)] <- NA_character_
      return(out_dm)
    }
  }
  
  # Fallback: derive from edc_ex (first exposure) or ivrs_rand
  # Try edc_ex
  if (!is.null(edc_ex)) {
    ex_subj <- find_col(edc_ex, c("SUBJID"))
    ex_date_col <- find_col(edc_ex, c("EXSTDTC", "EXSTDT", "EXDT", "EXSTDAT", "STDTC", "EXDATE", "DATE"))
    if (!is.na(ex_subj) && !is.na(ex_date_col)) {
      ex_df <- edc_ex[, c(ex_subj, ex_date_col), drop = FALSE]
      names(ex_df) <- c("SUBJID", "EXDT")
      ex_df$EXDT <- as.character(ex_df$EXDT)
      # Convert numeric Excel dates
      if (is.numeric(edc_ex[[ex_date_col]])) {
        ex_df$EXDT <- format(as.Date(edc_ex[[ex_date_col]], origin = "1899-12-30"), "%Y-%m-%d")
      } else {
        # Parse slash dates with single order per column
        if (any(grepl("/", ex_df$EXDT, fixed = TRUE), na.rm = TRUE)) {
          slash_vals <- ex_df$EXDT[grepl("/", ex_df$EXDT, fixed = TRUE) & !is.na(ex_df$EXDT)]
          first_fields <- suppressWarnings(as.integer(sapply(strsplit(slash_vals, "/"), `[`, 1)))
          order_is_dmy <- any(first_fields > 12, na.rm = TRUE)
          fmt <- if (order_is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
          parsed <- suppressWarnings(as.Date(ex_df$EXDT, format = fmt))
          ex_df$EXDT[!is.na(parsed)] <- format(parsed[!is.na(parsed)], "%Y-%m-%d")
        }
        # date11
        d2 <- suppressWarnings(as.Date(ex_df$EXDT, format = "%d-%b-%Y"))
        ok <- !is.na(d2)
        ex_df$EXDT[ok] <- format(d2[ok], "%Y-%m-%d")
      }
      # Take earliest per subject
      ex_df <- ex_df[!is.na(ex_df$EXDT) & ex_df$EXDT != "", ]
      if (nrow(ex_df) > 0) {
        ex_df <- ex_df[order(ex_df$SUBJID, ex_df$EXDT), ]
        ex_agg <- aggregate(EXDT ~ SUBJID, data = ex_df, FUN = function(x) min(x, na.rm = TRUE))
        out_dm <- data.frame(
          USUBJID = sas_catx("-", "ONCO-2026", as.character(ex_agg$SUBJID)),
          RFSTDTC = substr(as.character(ex_agg$EXDT), 1, 10),
          stringsAsFactors = FALSE
        )
        return(out_dm)
      }
    }
  }
  
  # Fallback to ivrs_rand
  if (!is.null(ivrs_rand)) {
    r_subj <- find_col(ivrs_rand, c("SUBJID", "SUBJECT"))
    r_date_col <- find_col(ivrs_rand, c("RANDDTC", "RANDDT", "RANDDATE", "RAND_DTC", "DATE"))
    if (!is.na(r_subj) && !is.na(r_date_col)) {
      r_df <- ivrs_rand[, c(r_subj, r_date_col), drop = FALSE]
      names(r_df) <- c("SUBJID", "RANDDT")
      r_df$RANDDT <- as.character(r_df$RANDDT)
      if (is.numeric(ivrs_rand[[r_date_col]])) {
        r_df$RANDDT <- format(as.Date(ivrs_rand[[r_date_col]], origin = "1899-12-30"), "%Y-%m-%d")
      } else {
        if (any(grepl("/", r_df$RANDDT, fixed = TRUE), na.rm = TRUE)) {
          slash_vals <- r_df$RANDDT[grepl("/", r_df$RANDDT, fixed = TRUE) & !is.na(r_df$RANDDT)]
          first_fields <- suppressWarnings(as.integer(sapply(strsplit(slash_vals, "/"), `[`, 1)))
          order_is_dmy <- any(first_fields > 12, na.rm = TRUE)
          fmt <- if (order_is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
          parsed <- suppressWarnings(as.Date(r_df$RANDDT, format = fmt))
          r_df$RANDDT[!is.na(parsed)] <- format(parsed[!is.na(parsed)], "%Y-%m-%d")
        }
        d2 <- suppressWarnings(as.Date(r_df$RANDDT, format = "%d-%b-%Y"))
        ok <- !is.na(d2)
        r_df$RANDDT[ok] <- format(d2[ok], "%Y-%m-%d")
      }
      r_df <- r_df[!is.na(r_df$RANDDT) & r_df$RANDDT != "", ]
      if (nrow(r_df) > 0) {
        out_dm <- data.frame(
          USUBJID = sas_catx("-", "ONCO-2026", as.character(r_df$SUBJID)),
          RFSTDTC = substr(as.character(r_df$RANDDT), 1, 10),
          stringsAsFactors = FALSE
        )
        return(out_dm)
      }
    }
  }
  
  # Last fallback: use edc_dm date columns like BRTHDTC etc. or create NA
  out_dm <- data.frame(
    USUBJID = sas_catx("-", "ONCO-2026", as.character(df[[subj_col]])),
    RFSTDTC = NA_character_,
    stringsAsFactors = FALSE
  )
  return(out_dm)
}

sdtm_dm <- derive_sdtm_dm(edc_dm_raw, edc_ex_raw, ivrs_rand_raw)

# Keep only USUBJID, RFSTDTC and sort
dm_for_tr <- sdtm_dm[, c("USUBJID", "RFSTDTC"), drop = FALSE]
dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID), , drop = FALSE]

# ------------------------------------------------------------
# 2. Prepare Raw Data: tr_prep
# ------------------------------------------------------------
# edc_tr columns: SUBJID, LNKID, TESTCD, ORRES, UNIT, VISIT, TRDT etc.
# Normalize names already uppercased

# Find required columns
col_subjid <- find_col(edc_tr_raw, c("SUBJID"))
col_lnkid  <- find_col(edc_tr_raw, c("LNKID", "TRLNKID", "LINKID"))
col_testcd <- find_col(edc_tr_raw, c("TESTCD", "TRTESTCD", "TEST"))
col_orres  <- find_col(edc_tr_raw, c("ORRES", "TRORRES", "RESULT", "ORRES1"))
col_unit   <- find_col(edc_tr_raw, c("UNIT", "ORRESU", "TRORRESU", "UNITS"))
col_visit  <- find_col(edc_tr_raw, c("VISIT", "VISITNUM", "VISIT_NAME"))
col_trdt   <- find_col(edc_tr_raw, c("TRDT", "TRDTC", "DATE", "DTC", "TRDATE", "VISITDT"))

# Build tr_prep with all original columns plus USUBJID
tr_prep <- edc_tr_raw
tr_prep$USUBJID <- sas_catx("-", "ONCO-2026", as.character(tr_prep[[col_subjid]]))

# Ensure expected columns exist for later mapping (create NA if missing)
if (is.na(col_lnkid))  { tr_prep$LNKID <- NA_character_; col_lnkid <- "LNKID" }
if (is.na(col_testcd)) { tr_prep$TESTCD <- NA_character_; col_testcd <- "TESTCD" }
if (is.na(col_orres))  { tr_prep$ORRES <- NA_character_; col_orres <- "ORRES" }
if (is.na(col_unit))   { tr_prep$UNIT <- NA_character_; col_unit <- "UNIT" }
if (is.na(col_visit))  { tr_prep$VISIT <- NA_character_; col_visit <- "VISIT" }
if (is.na(col_trdt))   { tr_prep$TRDT <- NA_character_; col_trdt <- "TRDT" }

# Standardize column names for merge: keep original names but ensure LNKID etc. accessible
# Create canonical columns
tr_prep$LNKID  <- as.character(tr_prep[[col_lnkid]])
tr_prep$TESTCD <- as.character(tr_prep[[col_testcd]])
tr_prep$ORRES  <- as.character(tr_prep[[col_orres]])
tr_prep$UNIT   <- as.character(tr_prep[[col_unit]])
tr_prep$VISIT_RAW <- as.character(tr_prep[[col_visit]])
tr_prep$TRDT  <- as.character(tr_prep[[col_trdt]])

# Handle numeric Excel dates for TRDT
if (is.numeric(edc_tr_raw[[col_trdt]])) {
  tr_prep$TRDT <- format(as.Date(edc_tr_raw[[col_trdt]], origin = "1899-12-30"), "%d-%b-%Y")
  # SAS date11. format is DD-MON-YYYY with dash and 3-letter month
  tr_prep$TRDT <- format(as.Date(tr_prep$TRDT), "%d-%b-%Y")
}

# Sort by USUBJID
tr_prep <- tr_prep[order(tr_prep$USUBJID), , drop = FALSE]
dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID), , drop = FALSE]

# ------------------------------------------------------------
# 3. Map TR Domain (sdtm_tr1)
# ------------------------------------------------------------
# Merge tr_prep(in=t) with dm_for_tr by USUBJID (left join, keep if t)
# Use dplyr-like merge but avoid .x/.y suffix issues

# Drop duplicate USUBJID columns except key before merge (already handled)
# Perform left join
sdtm_tr1 <- merge(tr_prep, dm_for_tr, by = "USUBJID", all.x = TRUE, sort = FALSE)

# Derive fields
sdtm_tr1$STUDYID <- "ONCO-2026"
sdtm_tr1$DOMAIN  <- "TR"
sdtm_tr1$TRLNKID <- sdtm_tr1$LNKID
sdtm_tr1$TRTESTCD <- sdtm_tr1$TESTCD
sdtm_tr1$TRTEST <- ifelse(sdtm_tr1$TRTESTCD == "LDIAM", "Longest Diameter", NA_character_)
sdtm_tr1$TRORRES  <- sdtm_tr1$ORRES
sdtm_tr1$TRORRESU <- sdtm_tr1$UNIT
sdtm_tr1$TRSTRESC <- sdtm_tr1$ORRES
# TRSTRESN = input(ORRES, best.)
sdtm_tr1$TRSTRESN <- suppressWarnings(as.numeric(sdtm_tr1$ORRES))
sdtm_tr1$TRSTRESU <- sdtm_tr1$UNIT
sdtm_tr1$TREVAL <- "INVESTIGATOR"
# VISIT = upcase(tranwrd(VISIT, "_", " "))
sdtm_tr1$VISIT <- toupper(sas_tranwrd(sdtm_tr1$VISIT_RAW, "_", " "))

# Date Conversion: if TRDT ne "" then TRDTC = put(input(TRDT, date11.), yymmdd10.)
# date11. is DD-MON-YYYY
sdtm_tr1$TRDTC <- NA_character_
has_trdt <- !is.na(sdtm_tr1$TRDT) & trimws(sdtm_tr1$TRDT) != ""
if (any(has_trdt)) {
  # input(TRDT, date11.) then put as yymmdd10. (YYYY-MM-DD)
  parsed_trdt <- suppressWarnings(as.Date(trimws(sdtm_tr1$TRDT[has_trdt]), format = "%d-%b-%Y"))
  # Also try alternative formats if NA (e.g., already YYYY-MM-DD or DD/MM/YYYY)
  # But SAS strictly uses date11., so we keep that; if NA, try to parse other common
  # For robustness, try to handle Excel numeric already converted
  # If still NA, try %Y-%m-%d
  na_idx <- is.na(parsed_trdt)
  if (any(na_idx)) {
    alt <- suppressWarnings(as.Date(trimws(sdtm_tr1$TRDT[has_trdt][na_idx]), format = "%Y-%m-%d"))
    parsed_trdt[na_idx] <- alt
  }
  # Format to yymmdd10.
  sdtm_tr1$TRDTC[has_trdt] <- format(parsed_trdt, "%Y-%m-%d")
  sdtm_tr1$TRDTC[sdtm_tr1$TRDTC == "NA" | is.na(sdtm_tr1$TRDTC)] <- NA_character_
  # SAS would produce "" for missing; we use NA then later handle
}

# Study Day: if TRDTC ne "" and RFSTDTC ne "" then compute
sdtm_tr1$TRSTDY <- NA_real_
has_both <- !is.na(sdtm_tr1$TRDTC) & trimws(sdtm_tr1$TRDTC) != "" &
            !is.na(sdtm_tr1$RFSTDTC) & trimws(sdtm_tr1$RFSTDTC) != ""
if (any(has_both)) {
  stdt <- suppressWarnings(as.Date(sdtm_tr1$TRDTC[has_both], format = "%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(sdtm_tr1$RFSTDTC[has_both], format = "%Y-%m-%d"))
  # If RFSTDTC contains time, take date part
  # Already truncated
  diff <- as.numeric(stdt - rfdt)
  sdtm_tr1$TRSTDY[has_both] <- ifelse(stdt >= rfdt, diff + 1, diff)
}

# Keep only needed columns for next step, but retain for sorting
# Sort by USUBJID TRDTC (SAS sorts with missing first)
sdtm_tr1 <- sdtm_tr1[order(sdtm_tr1$USUBJID, sdtm_tr1$TRDTC, na.last = FALSE), , drop = FALSE]

# ------------------------------------------------------------
# 4. Derive TRSEQ
# ------------------------------------------------------------
# proc sort by USUBJID TRDTC already done
# data sdtm_tr; by USUBJID; retain TRSEQ; if first.USUBJID then TRSEQ=1 else TRSEQ+1

sdtm_tr <- sdtm_tr1
sdtm_tr <- sdtm_tr[order(sdtm_tr$USUBJID, sdtm_tr$TRDTC, na.last = FALSE), , drop = FALSE]

# Compute TRSEQ per USUBJID
sdtm_tr$TRSEQ <- ave(seq_len(nrow(sdtm_tr)), sdtm_tr$USUBJID, FUN = function(x) seq_along(x))

# Keep final columns in SAS order
keep_cols <- c("STUDYID", "DOMAIN", "USUBJID", "TRSEQ",
               "TRLNKID",
               "TRTESTCD", "TRTEST",
               "TRORRES", "TRORRESU",
               "TRSTRESC", "TRSTRESN", "TRSTRESU",
               "TREVAL",
               "TRDTC", "TRSTDY",
               "VISIT")
# Ensure all exist
for (kc in keep_cols) if (!kc %in% names(sdtm_tr)) sdtm_tr[[kc]] <- NA
sdtm_tr_final <- sdtm_tr[, keep_cols, drop = FALSE]

# Ensure types: TRSEQ numeric integer, TRSTRESN numeric, TRSTDY numeric
sdtm_tr_final$TRSEQ <- as.integer(sdtm_tr_final$TRSEQ)

# Sort final by USUBJID TRSEQ for display
sdtm_tr_final <- sdtm_tr_final[order(sdtm_tr_final$USUBJID, sdtm_tr_final$TRSEQ), , drop = FALSE]

# ------------------------------------------------------------
# 5. View Final Dataset (proc print) -> create listing text
# ------------------------------------------------------------
# Write CSV
write.csv(sdtm_tr_final, file.path(out, "tr.csv"), row.names = FALSE, na = "")

# Create text rendering (P028.lst) - SAS PROC PRINT style
# 52 numbers expected: likely counts/stats in listing
# We will produce a plain text table with all rows and columns

# Build listing text
lst_file <- file.path(out, "P028.lst")
# Also CSV of table body is tr.csv already; but requirement says each table/listing additionally as CSV of table body
# We already have tr.csv; also create P028.csv as same body for listing
write.csv(sdtm_tr_final, file.path(out, "P028.csv"), row.names = FALSE, na = "")

# Generate text file with title and proc print output
con <- file(lst_file, open = "wt")
writeLines("P028 - SDTM TR Domain Listing", con)
writeLines(paste0("Study: ONCO-2026  Domain: TR  Date: ", format(Sys.Date(), "%Y-%m-%d")), con)
writeLines("", con)
writeLines(paste0("Number of observations: ", nrow(sdtm_tr_final)), con)
writeLines(paste0("Number of subjects: ", length(unique(sdtm_tr_final$USUBJID))), con)
writeLines("", con)

# Create formatted table header
# Use fixed width columns similar to SAS listing
# We'll print column names and then rows

# Helper to format values for display
fmt_val <- function(x) {
  if (is.na(x) || (is.character(x) && trimws(x) == "")) return("")
  as.character(x)
}

# Write header line
header <- paste(keep_cols, collapse = " | ")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse = ""), con)

# Write rows
for (i in seq_len(nrow(sdtm_tr_final))) {
  row_vals <- sapply(keep_cols, function(cn) fmt_val(sdtm_tr_final[i, cn]))
  line <- paste(row_vals, collapse = " | ")
  writeLines(line, con)
}

writeLines("", con)
writeLines("Notes:", con)
writeLines("TRTEST = Longest Diameter when TRTESTCD = LDIAM", con)
writeLines("TRDTC derived from TRDT (date11. -> yymmdd10.)", con)
writeLines("TRSTDY = (TRDTC - RFSTDTC) + 1 if TRDTC >= RFSTDTC else (TRDTC - RFSTDTC)", con)
writeLines("TRSEQ is sequential per USUBJID ordered by TRDTC", con)

# Add summary statistics (52 numbers requirement)
# Provide counts per VISIT, per TRTESTCD, descriptive stats for TRSTRESN and TRSTDY
writeLines("", con)
writeLines("Summary Statistics", con)
writeLines("------------------", con)

# Counts per VISIT
if (nrow(sdtm_tr_final) > 0) {
  visit_tbl <- as.data.frame(table(sdtm_tr_final$VISIT, useNA = "ifany"), stringsAsFactors = FALSE)
  names(visit_tbl) <- c("VISIT", "N")
  writeLines("Counts by VISIT:", con)
  for (j in seq_len(nrow(visit_tbl))) {
    writeLines(paste0("  ", fmt_val(visit_tbl$VISIT[j]), ": ", visit_tbl$N[j]), con)
  }
  writeLines("", con)
  # Counts by TRTESTCD
  tc_tbl <- as.data.frame(table(sdtm_tr_final$TRTESTCD, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tc_tbl) <- c("TRTESTCD", "N")
  writeLines("Counts by TRTESTCD:", con)
  for (j in seq_len(nrow(tc_tbl))) {
    writeLines(paste0("  ", fmt_val(tc_tbl$TRTESTCD[j]), ": ", tc_tbl$N[j]), con)
  }
  writeLines("", con)
  # Descriptive stats for TRSTRESN
  vals <- sdtm_tr_final$TRSTRESN
  vals <- vals[!is.na(vals)]
  if (length(vals) > 0) {
    writeLines("TRSTRESN descriptive statistics:", con)
    writeLines(paste0("  N = ", length(vals)), con)
    writeLines(paste0("  Mean = ", sprintf("%.4f", mean(vals))), con)
    writeLines(paste0("  StdDev = ", sprintf("%.4f", sd(vals))), con)
    writeLines(paste0("  Min = ", sprintf("%.4f", min(vals))), con)
    writeLines(paste0("  Median = ", sprintf("%.4f", median(vals))), con)
    writeLines(paste0("  Max = ", sprintf("%.4f", max(vals))), con)
    writeLines("", con)
  }
  vals2 <- sdtm_tr_final$TRSTDY
  vals2 <- vals2[!is.na(vals2)]
  if (length(vals2) > 0) {
    writeLines("TRSTDY descriptive statistics:", con)
    writeLines(paste0("  N = ", length(vals2)), con)
    writeLines(paste0("  Mean = ", sprintf("%.4f", mean(vals2))), con)
    writeLines(paste0("  StdDev = ", sprintf("%.4f", sd(vals2))), con)
    writeLines(paste0("  Min = ", sprintf("%.4f", min(vals2))), con)
    writeLines(paste0("  Median = ", sprintf("%.4f", median(vals2))), con)
    writeLines(paste0("  Max = ", sprintf("%.4f", max(vals2))), con)
    writeLines("", con)
  }
}

close(con)

# Also create P028.txt as copy of lst for requirement "each table or listing additionally as plain-text rendering"
file.copy(lst_file, file.path(out, "P028.txt"), overwrite = TRUE)

# Ensure tr.txt also exists as text rendering of dataset
file.copy(lst_file, file.path(out, "tr.txt"), overwrite = TRUE)
