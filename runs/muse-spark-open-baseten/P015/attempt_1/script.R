# P015_sdtm_lb.sas -> R translation
# Program id: P015  family: c2c
# Outputs: dataset lb (lb.csv) and listing P015.lst (text + csv)

# --- setup ---
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (half-up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # for NA keep NA
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  res[is.na(x)] <- NA_real_
  res
}

# helper: SAS BEST. formatting (strip(put(RESULT,best.)))
sas_best <- function(x) {
  # x numeric vector
  # SAS BEST12. default: minimal representation, no leading/trailing blanks
  # Use format with digits=12, scientific=FALSE, trim=TRUE
  # Remove trailing zeros after decimal? format does not add them if nsmall=0
  # For NA return ""
  vapply(x, function(v) {
    if (is.na(v)) return("")
    # Use formatC with fg to mimic BEST
    # digits=12 is SAS default width 12
    s <- formatC(v, format = "fg", digits = 12, flag = "#")
    # formatC with # may keep trailing decimal point, remove it
    s <- sub("\\.$", "", s)
    # trim spaces
    s <- trimws(s)
    # Remove unnecessary trailing zeros? formatC fg already minimal
    # Ensure no scientific for moderate numbers; if scientific, keep as is
    s
  }, character(1))
}

# helper: SAS CATX
sas_catx <- function(sep, ...) {
  args <- list(...)
  # trim each, remove NA/empty
  trimmed <- lapply(args, function(a) {
    if (is.null(a)) return(NULL)
    a <- as.character(a)
    a <- trimws(a)
    a[a == "" | is.na(a)] <- NA_character_
    a
  })
  # paste row-wise
  # assume all same length or length 1
  maxlen <- max(sapply(trimmed, length))
  trimmed <- lapply(trimmed, function(a) if (length(a)==1) rep(a, maxlen) else a)
  mat <- do.call(cbind, trimmed)
  apply(mat, 1, function(row) {
    row <- row[!is.na(row) & row != ""]
    if (length(row)==0) return("")
    paste(row, collapse = sep)
  })
}

# helper: parse LAB_DAT to SAS date number then to ISO8601
# SAS anydtdte. reads many date/datetime strings; we approximate with lubridate
parse_lab_dat_to_iso <- function(lab_dat_vec) {
  # lab_dat_vec may be character, numeric, or mixed (from CSV read as character)
  # Return character vector YYYY-MM-DD or NA
  n <- length(lab_dat_vec)
  result_num <- rep(NA_real_, n) # SAS date number (days since 1960-01-01)
  result_iso <- rep(NA_character_, n)

  # If vector is numeric (double/integer) already, treat as SAS date number
  if (is.numeric(lab_dat_vec)) {
    # SAS numeric date: days since 1960-01-01
    # Convert to R Date then to ISO
    # SAS missing is NA
    for (i in seq_along(lab_dat_vec)) {
      if (!is.na(lab_dat_vec[i])) {
        # SAS date to R Date
        rdate <- as.Date(lab_dat_vec[i], origin = "1960-01-01")
        result_iso[i] <- format(rdate, "%Y-%m-%d")
      }
    }
    return(result_iso)
  }

  # Character vector path
  # Try to handle each element
  for (i in seq_along(lab_dat_vec)) {
    v <- lab_dat_vec[i]
    if (is.na(v) || trimws(as.character(v)) == "") next
    v_trim <- trimws(as.character(v))
    # Check if pure integer string that could be SAS date number (e.g., "22200")
    # Heuristic: if string is 4-5 digits, all digits, and numeric value between 0 and 60000, treat as SAS date
    # But also dates like "20200101" are 8 digits, not SAS date
    num_try <- suppressWarnings(as.numeric(v_trim))
    is_pure_int <- !is.na(num_try) && grepl("^[0-9]+$", v_trim)
    if (is_pure_int) {
      # Distinguish SAS date number vs yyyymmdd
      # If length 8 and starts with 19 or 20, likely yyyymmdd, not SAS date
      if (nchar(v_trim) == 8 && substr(v_trim,1,2) %in% c("19","20")) {
        # parse as yyyymmdd
        d <- suppressWarnings(lubridate::ymd(v_trim, quiet = TRUE))
        if (!is.na(d)) {
          result_iso[i] <- format(as.Date(d), "%Y-%m-%d")
          next
        }
      } else if (nchar(v_trim) <= 5 && num_try >= 0 && num_try <= 60000) {
        # treat as SAS date number
        rdate <- as.Date(num_try, origin = "1960-01-01")
        result_iso[i] <- format(rdate, "%Y-%m-%d")
        next
      } else if (nchar(v_trim) <= 5) {
        rdate <- as.Date(num_try, origin = "1960-01-01")
        result_iso[i] <- format(rdate, "%Y-%m-%d")
        next
      }
    }
    # Try lubridate parse_date_time with many orders
    # Use anytime-like orders
    d_parsed <- suppressWarnings(lubridate::parse_date_time(v_trim,
      orders = c("ymd", "Ymd", "mdy", "dmy", "ymd HMS", "mdy HMS", "dmy HMS",
                 "Ymd HMS", "ymd HM", "mdy HM", "dmy HM",
                 "Y-m-d", "m/d/Y", "d/m/Y", "Y/m/d", "d-b-Y", "d-B-Y", "b-d-Y", "B-d-Y",
                 "Y-m-d H:M:S", "m/d/Y H:M:S", "d/m/Y H:M:S",
                 "Ymd", "dmY", "mdY", "Y-m-dT", "Y-m-d H:M", "m-d-Y", "d-m-Y"),
      quiet = TRUE, truncated = 3))
    if (!is.na(d_parsed)) {
      result_iso[i] <- format(as.Date(d_parsed), "%Y-%m-%d")
      next
    }
    # Try as.Date with common formats
    fmts <- c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d", "%d-%b-%Y", "%d-%B-%Y",
              "%b %d %Y", "%B %d %Y", "%d%b%Y", "%Y%m%d", "%m-%d-%Y", "%d-%m-%Y",
              "%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S")
    parsed <- NA
    for (f in fmts) {
      d2 <- suppressWarnings(as.Date(v_trim, format = f))
      if (!is.na(d2)) { parsed <- d2; break }
      # try as.POSIXct
      dt2 <- suppressWarnings(as.POSIXct(v_trim, format = f, tz = "UTC"))
      if (!is.na(dt2)) { parsed <- as.Date(dt2); break }
    }
    if (!is.na(parsed)) {
      result_iso[i] <- format(parsed, "%Y-%m-%d")
      next
    }
    # Fallback: try lubridate ymd/mdy/dmy
    d3 <- suppressWarnings(lubridate::ymd(v_trim, quiet = TRUE))
    if (!is.na(d3)) { result_iso[i] <- format(as.Date(d3), "%Y-%m-%d"); next }
    d3 <- suppressWarnings(lubridate::mdy(v_trim, quiet = TRUE))
    if (!is.na(d3)) { result_iso[i] <- format(as.Date(d3), "%Y-%m-%d"); next }
    d3 <- suppressWarnings(lubridate::dmy(v_trim, quiet = TRUE))
    if (!is.na(d3)) { result_iso[i] <- format(as.Date(d3), "%Y-%m-%d"); next }
  }
  result_iso
}

# --- locate input ---
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_lab_path <- file.path(project_path, "data/raw/raw_lab.csv")
# fallback paths if not found
alt_paths <- c(
  raw_lab_path,
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_lab.csv",
  file.path(Sys.getenv("ROOT", unset = "D:/meta_hackathon/sas2r-agent"), "data/derived/c2c/data/raw/raw_lab.csv")
)
raw_lab_file <- NULL
for (p in alt_paths) {
  if (file.exists(p)) { raw_lab_file <- p; break }
}
if (is.null(raw_lab_file)) {
  # try to find via list.files
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_lab.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_lab_file <- cand[1]
}
if (is.null(raw_lab_file) || !file.exists(raw_lab_file)) {
  stop(paste("raw_lab.csv not found. Searched:", paste(alt_paths, collapse="; ")))
}

# --- 1. IMPORT RAW LAB DATA ---
# SAS proc import dbms=csv getnames=yes
# Use readr to preserve types, then handle
raw_lb <- readr::read_csv(raw_lab_file, show_col_types = FALSE, guess_max = 10000, na = c("", "NA", "."))
# Ensure column names uppercase for SAS validvarname=upcase behavior
# But keep original for reference; create uppercase mapping
names(raw_lb) <- toupper(names(raw_lb))

# Expected columns (uppercase): SUBJ, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, LAB_DAT
# Check and handle alternative names
# If VISIT_NAM missing but VISIT_NAM exists as VISIT_NAM, etc.
# Ensure required columns exist
required <- c("SUBJ", "TEST_NAME", "RESULT")
for (col in required) {
  if (!col %in% names(raw_lb)) {
    stop(paste("Required column", col, "not found in raw_lab.csv. Found:", paste(names(raw_lb), collapse=", ")))
  }
}
# Provide defaults for optional
if (!"VISIT_NAM" %in% names(raw_lb)) {
  # try VISITNAM, VISIT
  if ("VISITNAM" %in% names(raw_lb)) raw_lb$VISIT_NAM <- raw_lb$VISITNAM
  else if ("VISIT" %in% names(raw_lb)) raw_lb$VISIT_NAM <- raw_lb$VISIT
  else raw_lb$VISIT_NAM <- NA_character_
}
if (!"VISIT_NUM" %in% names(raw_lb)) {
  if ("VISITNUM" %in% names(raw_lb)) raw_lb$VISIT_NUM <- raw_lb$VISITNUM
  else if ("VISITNUMN" %in% names(raw_lb)) raw_lb$VISIT_NUM <- raw_lb$VISITNUMN
  else raw_lb$VISIT_NUM <- NA_real_
}
if (!"UNIT" %in% names(raw_lb)) raw_lb$UNIT <- NA_character_
if (!"LAB_DAT" %in% names(raw_lb)) {
  # try LABDAT, LBDTC, DATE
  if ("LABDAT" %in% names(raw_lb)) raw_lb$LAB_DAT <- raw_lb$LABDAT
  else if ("LBDTC" %in% names(raw_lb)) raw_lb$LAB_DAT <- raw_lb$LBDTC
  else raw_lb$LAB_DAT <- NA_character_
}

# Convert types: SUBJ to character, VISIT_NAM character, VISIT_NUM numeric, TEST_NAME character, RESULT numeric, UNIT character, LAB_DAT character/numeric
raw_lb$SUBJ <- as.character(raw_lb$SUBJ)
raw_lb$VISIT_NAM <- as.character(raw_lb$VISIT_NAM)
raw_lb$VISIT_NUM <- suppressWarnings(as.numeric(raw_lb$VISIT_NUM))
raw_lb$TEST_NAME <- as.character(raw_lb$TEST_NAME)
raw_lb$RESULT <- suppressWarnings(as.numeric(raw_lb$RESULT))
raw_lb$UNIT <- as.character(raw_lb$UNIT)
# LAB_DAT keep as is (could be character or numeric); if readr guessed numeric, keep numeric
# If it's character, keep character

# --- 2. CORE TRANSFORMATION & DICTIONARY MAPPING ---
# Pre-define lengths not needed in R, but enforce truncation if needed
usubjid_length <- 40

lb_mapped <- raw_lb %>%
  dplyr::mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "LB",
    USUBJID = sas_catx("-", "CDISC-01", SUBJ),
    # truncate to usubjid_length if needed (SAS length)
    USUBJID = substr(USUBJID, 1, usubjid_length),
    VISIT = trimws(as.character(VISIT_NAM)),
    VISITNUM = VISIT_NUM
  )

# Dictionary mapping for TEST codes
# Use upcase(strip(TEST_NAME))
lb_mapped <- lb_mapped %>%
  dplyr::mutate(
    TEST_NAME_UP = toupper(trimws(TEST_NAME)),
    LBTESTCD = dplyr::case_when(
      TEST_NAME_UP == "GLUCOSE" ~ "GLUC",
      TEST_NAME_UP == "HEMOGLOBIN" ~ "HGB",
      TEST_NAME_UP == "ALT" ~ "ALT",
      TEST_NAME_UP == "AST" ~ "AST",
      TRUE ~ "UNKNOWN"
    ),
    LBTEST = dplyr::case_when(
      TEST_NAME_UP == "GLUCOSE" ~ "Glucose",
      TEST_NAME_UP == "HEMOGLOBIN" ~ "Hemoglobin",
      TEST_NAME_UP == "ALT" ~ "Alanine Aminotransferase",
      TEST_NAME_UP == "AST" ~ "Aspartate Aminotransferase",
      TRUE ~ TEST_NAME
    ),
    LBCAT = dplyr::case_when(
      TEST_NAME_UP == "GLUCOSE" ~ "CHEMISTRY",
      TEST_NAME_UP == "HEMOGLOBIN" ~ "HEMATOLOGY",
      TEST_NAME_UP == "ALT" ~ "CHEMISTRY",
      TEST_NAME_UP == "AST" ~ "CHEMISTRY",
      TRUE ~ "UNKNOWN"
    )
  ) %>%
  dplyr::mutate(
    LBORRES = sas_best(RESULT),
    LBSTRESN = RESULT,
    LBSTRESC = LBORRES,
    LBORRESU = trimws(as.character(UNIT)),
    LBORRESU = ifelse(is.na(LBORRESU), "", LBORRESU),
    LBSTRESU = dplyr::case_when(
      toupper(LBORRESU) == "MG/DL" ~ "mg/dL",
      toupper(LBORRESU) == "G/DL" ~ "g/dL",
      toupper(LBORRESU) == "U/L" ~ "U/L",
      TRUE ~ LBORRESU
    )
  )

# ISO 8601 DATE CONVERSION
# SAS: if vtype(LAB_DAT)='C' then _lab_num = input(strip(LAB_DAT), anydtdte.); else _lab_num = LAB_DAT;
# if not missing(_lab_num) then LBDTC = put(_lab_num, is8601da.);
# In R, we need to handle both character and numeric LAB_DAT
# raw_lb$LAB_DAT may be character or numeric; we have it as whatever readr guessed
# For safety, get original column from raw_lb
lab_dat_raw <- raw_lb$LAB_DAT
# If lab_dat_raw is numeric, parse directly; if character, parse via helper
# The helper handles both
LBDTC_vec <- parse_lab_dat_to_iso(lab_dat_raw)

lb_mapped$LBDTC <- LBDTC_vec

# Keep only required variables (SAS keep)
lb_mapped <- lb_mapped %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU, LBDTC)

# --- 3. GENERATE SEQUENCE NUMBER (LBSEQ) ---
# proc sort by USUBJID LBDTC LBTESTCD
# SAS sorts missing first, character ordering uppercase before lowercase (ASCII)
# In R, order with na.last=FALSE gives missing first; default character order is similar (ASCII)
lb_mapped_sorted <- lb_mapped %>%
  dplyr::arrange(USUBJID, LBDTC, LBTESTCD)

# data sdtm.lb; retain STUDYID DOMAIN USUBJID LBSEQ; set work.lb_mapped; by USUBJID; if first.USUBJID then LBSEQ=1; else LBSEQ+1;
# Implement FIRST. logic
lb_final <- lb_mapped_sorted %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(LBSEQ = dplyr::row_number()) %>%
  dplyr::ungroup()

# Reorder columns to match SAS retain order: STUDYID DOMAIN USUBJID LBSEQ VISITNUM VISIT LBTESTCD LBTEST LBCAT LBORRES LBORRESU LBSTRESC LBSTRESN LBSTRESU LBDTC
lb_final <- lb_final %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, LBSEQ, VISITNUM, VISIT, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU, LBDTC)

# Ensure types: VISITNUM numeric, LBSTRESN numeric, LBSEQ integer
# Replace empty strings with NA for CSV? SAS missing is blank; spec says na="" in write.csv, so keep "" as NA will be written as ""
# For CSV output, we want missing as "" (na=""), so keep NA_character_ for missing strings
# Convert "" back to NA for proper CSV na handling where appropriate
# But keep as is; write.csv with na="" will handle NA
lb_final$LBORRESU[lb_final$LBORRESU == ""] <- NA_character_
lb_final$LBSTRESU[lb_final$LBSTRESU == ""] <- NA_character_
lb_final$VISIT[lb_final$VISIT == ""] <- NA_character_
lb_final$LBDTC[lb_final$LBDTC == ""] <- NA_character_
lb_final$LBORRES[lb_final$LBORRES == ""] <- NA_character_
lb_final$LBSTRESC[lb_final$LBSTRESC == ""] <- NA_character_

# Sort final by USUBJID LBSEQ to ensure deterministic
lb_final <- lb_final %>% dplyr::arrange(USUBJID, LBSEQ)

# --- 4. VISUAL AUDIT ---
# proc print obs=12 var USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC
audit_vars <- c("USUBJID", "VISITNUM", "VISIT", "LBTESTCD", "LBCAT", "LBSTRESN", "LBSTRESU", "LBDTC")
audit_data <- lb_final %>% dplyr::select(dplyr::all_of(audit_vars)) %>% head(12)

# --- Write outputs ---
# Dataset lb -> OUT_DIR/lb.csv
write.csv(lb_final, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# Listing P015.lst -> text rendering and CSV
# Create text rendering
txt_path <- file.path(out, "P015.txt")
txt_path_lst <- file.path(out, "P015.lst.txt")
csv_path <- file.path(out, "P015.csv")
csv_path_lst <- file.path(out, "P015.lst.csv")

# Build text content
header_lines <- c(
  "LB Domain Audit (Laboratory Test Results)",
  paste(rep("-", 80), collapse = ""),
  paste0("Dataset: sdtm.lb  (", nrow(lb_final), " records)"),
  paste0("Printed: first 12 observations"),
  ""
)

# Format audit_data for display: mimic PROC PRINT
# Create column headers and formatted rows
# Use format to align
# Prepare display dataframe with formatted numbers
audit_display <- audit_data
# Format numeric columns: VISITNUM and LBSTRESN
# SAS default for numeric: BEST12. - we use as is but format without scientific
audit_display$VISITNUM <- ifelse(is.na(audit_display$VISITNUM), "", trimws(format(audit_display$VISITNUM, scientific = FALSE, trim = TRUE)))
audit_display$LBSTRESN <- ifelse(is.na(audit_display$LBSTRESN), "", trimws(format(audit_display$LBSTRESN, scientific = FALSE, trim = TRUE, digits = 12)))

# Build table text using fixed-width columns
# Determine column widths based on header and data
col_names <- names(audit_display)
# Compute widths
widths <- sapply(seq_along(col_names), function(i) {
  col <- col_names[i]
  max(nchar(col, type = "width"), max(nchar(as.character(audit_display[[col]]), type = "width"), na.rm = TRUE), na.rm = TRUE)
})
# Add padding
widths <- widths + 2
# Ensure minimum widths
widths <- pmax(widths, 8)

# Helper to format row
format_row <- function(vals, widths) {
  cells <- mapply(function(v, w) {
    v <- ifelse(is.na(v), "", as.character(v))
    # left align for character, right for numeric? SAS prints left for char, right for numeric
    # For simplicity, left align all with padding
    sprintf(paste0("%-", w, "s"), v)
  }, vals, widths, SIMPLIFY = TRUE)
  paste(cells, collapse = "")
}
# Header row
header_row <- format_row(col_names, widths)
sep_row <- paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "")
# Data rows
data_rows <- apply(audit_display, 1, function(row) format_row(row, widths))

# Obs numbers like SAS PROC PRINT (Obs column)
obs_width <- max(nchar("Obs"), nchar(as.character(nrow(audit_display)))) + 2
obs_header <- sprintf(paste0("%-", obs_width, "s"), "Obs")
obs_sep <- paste(rep("-", obs_width), collapse = "")
# Prepend Obs to header and rows
header_row_obs <- paste0(sprintf(paste0("%-", obs_width, "s"), "Obs"), header_row)
sep_row_obs <- paste0(obs_sep, sep_row)
data_rows_obs <- sapply(seq_along(data_rows), function(i) {
  obs_cell <- sprintf(paste0("%", obs_width, "s"), as.character(i))
  paste0(obs_cell, data_rows[i])
})

# Assemble full text
text_lines <- c(
  header_lines,
  header_row_obs,
  sep_row_obs,
  data_rows_obs,
  "",
  paste(rep("-", 80), collapse = ""),
  paste0("Total records in lb domain: ", nrow(lb_final)),
  paste0("Unique subjects: ", length(unique(lb_final$USUBJID))),
  paste0("Unique tests: ", paste(sort(unique(lb_final$LBTESTCD)), collapse = ", "))
)

# Write text files
writeLines(text_lines, con = txt_path)
writeLines(text_lines, con = txt_path_lst)

# Write CSV of table body (audit data)
write.csv(audit_data, csv_path, row.names = FALSE, na = "")
write.csv(audit_data, csv_path_lst, row.names = FALSE, na = "")

# Also ensure lb dataset text rendering for completeness (optional)
# Console log progress
cat("P015 translation complete.\n")
cat(paste0("lb.csv written: ", nrow(lb_final), " rows, ", ncol(lb_final), " cols\n"))
cat(paste0("Listing written: ", nrow(audit_data), " rows to ", txt_path, " and ", csv_path, "\n"))
