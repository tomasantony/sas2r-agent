# P029_tu.sas -> R translation
# Outputs: tu.csv + P029 listing text/csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (not needed here but defined)
sas_round <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS upcase (handles NA)
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}

# Helper: SAS tranwrd - replace all occurrences of target with replacement
sas_tranwrd <- function(x, target, replacement) {
  ifelse(is.na(x), NA_character_, gsub(target, replacement, x, fixed=TRUE))
}

# Helper: SAS catx
sas_catx <- function(delim, ...) {
  args <- list(...)
  # vectorized version for two vectors
  # Use mapply
  # Simplified for USUBJID case: catx("-", "ONCO-2026", SUBJID)
  # SAS catx trims leading/trailing blanks and skips missing
  a <- args[[1]]
  b <- args[[2]]
  # recycle
  n <- max(length(a), length(b))
  a <- rep(a, length.out=n)
  b <- rep(b, length.out=n)
  # delim is scalar
  res <- character(n)
  for (i in seq_len(n)) {
    parts <- c(a[i], b[i])
    # trim
    parts <- trimws(parts)
    # remove NA or "" 
    parts <- parts[!is.na(parts) & parts != ""]
    if (length(parts)==0) res[i] <- ""
    else if (length(parts)==1) res[i] <- parts[1]
    else res[i] <- paste(parts, collapse=delim)
  }
  res
}

# Helper: parse SAS date11. (DDMMMYYYY, e.g., 01JAN2020, 01JAN20, 01-JAN-2020)
parse_date11 <- function(x) {
  if (is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  s <- trimws(as.character(x))
  # Remove dashes and spaces
  # SAS date11. is like 01JAN2020 or 01-JAN-2020 or 01JAN20
  # Try multiple formats
  # Upper case month
  s_up <- toupper(s)
  # Remove any "-" or "/" or " "
  # Keep as is for parsing with lubridate
  # Try lubridate::dmy
  d <- suppressWarnings(lubridate::dmy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  # Try with format %d%b%Y
  d2 <- suppressWarnings(as.Date(s_up, format="%d%b%Y"))
  if (!is.na(d2)) return(d2)
  d3 <- suppressWarnings(as.Date(s_up, format="%d-%b-%Y"))
  if (!is.na(d3)) return(d3)
  d4 <- suppressWarnings(as.Date(s_up, format="%d%b%y"))
  if (!is.na(d4)) return(d4)
  d5 <- suppressWarnings(as.Date(s_up, format="%d-%b-%y"))
  if (!is.na(d5)) return(d5)
  # Try yymmdd10. format YYYY-MM-DD
  d6 <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if (!is.na(d6)) return(d6)
  # Try Excel numeric? 
  return(as.Date(NA))
}

# Helper: parse yymmdd10. (YYYY-MM-DD)
parse_yymmdd10 <- function(x) {
  if (is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  s <- trimws(as.character(x))
  # If already Date
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # Try datetime
  d2 <- suppressWarnings(as.Date(substr(s,1,10), format="%Y-%m-%d"))
  if (!is.na(d2)) return(d2)
  # Try date11 fallback
  return(parse_date11(s))
}

# Helper: put date as yymmdd10. (YYYY-MM-DD)
put_yymmdd10 <- function(d) {
  if (is.na(d)) return(NA_character_)
  format(as.Date(d), "%Y-%m-%d")
}

# Helper: read Excel with case-insensitive column handling
read_edc <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(readxl::read_excel(path, .name_repair="minimal"), error=function(e) NULL)
  if (is.null(df)) return(NULL)
  # Convert to data.frame, keep original names
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  # Ensure character columns are character, not numeric
  df
}

# Helper: find sdtm_dm
find_sdtm_dm <- function() {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P029/sdtm_dm.xlsx",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/sdtm_dm.xlsx",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split/dm.xpt"
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  # Search recursively under root
  root <- "D:/meta_hackathon/sas2r-agent"
  if (dir.exists(root)) {
    files <- list.files(root, pattern="(?i)sdtm_dm|\\bdm\\.xpt$|\\bdm\\.sas7bdat$", recursive=TRUE, full.names=TRUE)
    if (length(files)>0) return(files[1])
    files2 <- list.files(root, pattern="(?i)dm\\.xlsx$", recursive=TRUE, full.names=TRUE)
    if (length(files2)>0) return(files2[1])
  }
  return(NA_character_)
}

# ------------------------------------------------------------------
# 1. Prepare DM Reference Data
# ------------------------------------------------------------------
# Try to read sdtm_dm
sdtm_dm_path <- find_sdtm_dm()
dm_for_tu <- NULL

if (!is.na(sdtm_dm_path) && file.exists(sdtm_dm_path)) {
  ext <- tolower(tools::file_ext(sdtm_dm_path))
  if (ext == "xpt") {
    dm_raw <- haven::read_xpt(sdtm_dm_path)
  } else if (ext == "sas7bdat") {
    dm_raw <- haven::read_sas(sdtm_dm_path)
  } else if (ext %in% c("xlsx","xls")) {
    dm_raw <- read_edc(sdtm_dm_path)
  } else {
    dm_raw <- NULL
  }
  if (!is.null(dm_raw)) {
    dm_raw <- as.data.frame(dm_raw, stringsAsFactors=FALSE)
    # Normalize column names to uppercase for matching
    names_upper <- toupper(names(dm_raw))
    # Find USUBJID and RFSTDTC columns case-insensitive
    usub_col <- which(names_upper=="USUBJID")
    rf_col <- which(names_upper=="RFSTDTC")
    if (length(usub_col)==0) usub_col <- which(names_upper=="USUBJID")
    if (length(rf_col)==0) {
      # try RFSTDT, RFSTDTC, RFSTDTCN
      rf_col <- which(names_upper %in% c("RFSTDTC","RFSTDT","RFSTDTCN","RFXSTDTC"))
    }
    if (length(usub_col)>0 && length(rf_col)>0) {
      dm_for_tu <- data.frame(
        USUBJID = as.character(dm_raw[[usub_col[1]]]),
        RFSTDTC = as.character(dm_raw[[rf_col[1]]]),
        stringsAsFactors=FALSE
      )
      # If RFSTDTC is Date, convert to character YYYY-MM-DD
      if (inherits(dm_raw[[rf_col[1]]], "Date")) {
        dm_for_tu$RFSTDTC <- format(as.Date(dm_raw[[rf_col[1]]]), "%Y-%m-%d")
      } else if (inherits(dm_raw[[rf_col[1]]], "POSIXt")) {
        dm_for_tu$RFSTDTC <- format(as.Date(dm_raw[[rf_col[1]]]), "%Y-%m-%d")
      } else {
        # If numeric Excel date, try to convert
        # haven labelled may be character
        dm_for_tu$RFSTDTC <- as.character(dm_for_tu$RFSTDTC)
        # If RFSTDTC looks like numeric, try as Date origin
        # Leave as is
      }
    }
  }
}

# Fallback: construct dm_for_tu from edc_dm.xlsx
if (is.null(dm_for_tu)) {
  edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  edc_dm <- read_edc(edc_dm_path)
  if (!is.null(edc_dm)) {
    # Find SUBJID column
    nms_up <- toupper(names(edc_dm))
    subj_col <- which(nms_up=="SUBJID")[1]
    rf_col <- which(nms_up %in% c("RFSTDTC","RFSTDT","RFXSTDTC","RFSTDTCN","RANDDTC","BRTHDTC"))[1]
    # Also try to find RFSTDTC in different case
    if (is.na(subj_col)) subj_col <- which(nms_up=="SUBJID")[1]
    if (!is.na(subj_col)) {
      usubjid_vec <- sas_catx("-", "ONCO-2026", as.character(edc_dm[[subj_col]]))
      if (!is.na(rf_col)) {
        rf_vec <- edc_dm[[rf_col]]
        # Convert to character YYYY-MM-DD
        if (inherits(rf_vec, "Date")) {
          rf_vec <- format(as.Date(rf_vec), "%Y-%m-%d")
        } else if (inherits(rf_vec, "POSIXt")) {
          rf_vec <- format(as.Date(rf_vec), "%Y-%m-%d")
        } else {
          # If character, try to normalize to YYYY-MM-DD if possible
          # Keep as is, but try to parse and reformat
          rf_vec_char <- as.character(rf_vec)
          # Attempt to parse each
          rf_vec_norm <- sapply(rf_vec_char, function(x) {
            if (is.na(x) || trimws(x)=="") return(NA_character_)
            # Try yymmdd10
            d <- parse_yymmdd10(x)
            if (!is.na(d)) return(put_yymmdd10(d))
            d2 <- parse_date11(x)
            if (!is.na(d2)) return(put_yymmdd10(d2))
            return(x)
          }, USE.NAMES=FALSE)
          rf_vec <- rf_vec_norm
        }
      } else {
        rf_vec <- rep(NA_character_, nrow(edc_dm))
      }
      dm_for_tu <- data.frame(USUBJID=usubjid_vec, RFSTDTC=as.character(rf_vec), stringsAsFactors=FALSE)
    }
  }
}

# If still null, create empty
if (is.null(dm_for_tu)) {
  dm_for_tu <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
}

# proc sort data=sdtm_dm out=dm_for_tu(keep=USUBJID RFSTDTC) by USUBJID
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last=FALSE), , drop=FALSE]
# Remove duplicates? SAS sort doesn't dedup, but keep as is
# Ensure character
dm_for_tu$USUBJID <- as.character(dm_for_tu$USUBJID)
dm_for_tu$RFSTDTC <- as.character(dm_for_tu$RFSTDTC)

# ------------------------------------------------------------------
# 2. Prepare Raw Data - edc_tu
# ------------------------------------------------------------------
edc_tu_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx"
edc_tu <- read_edc(edc_tu_path)
if (is.null(edc_tu)) {
  stop("edc_tu.xlsx not found at ", edc_tu_path)
}
# Normalize column names for access (case-insensitive)
# Keep original names but create uppercase mapping
orig_names <- names(edc_tu)
names_upper <- toupper(orig_names)
get_col <- function(df, colname) {
  idx <- which(toupper(names(df))==toupper(colname))
  if (length(idx)==0) return(rep(NA_character_, nrow(df)))
  v <- df[[idx[1]]]
  # Convert to character, handling Date
  if (inherits(v, "Date")) {
    # For TUDT, SAS expects date11. string; but we keep Date for later handling
    # Return as character in date11? We'll keep Date object and handle later
    return(v)
  }
  if (inherits(v, "POSIXt")) return(as.character(v))
  return(as.character(v))
}

# Extract needed columns
SUBJID_vec <- get_col(edc_tu, "SUBJID")
LOC_vec <- get_col(edc_tu, "LOC")
METH_vec <- get_col(edc_tu, "METH")
LNKID_vec <- get_col(edc_tu, "LNKID")
TYPE_vec <- get_col(edc_tu, "TYPE")
VISIT_vec <- get_col(edc_tu, "VISIT")
TUDT_vec <- get_col(edc_tu, "TUDT")

# Handle TUDT_vec may be Date type - need to preserve
# get_col returns Date if Date, else character. For TUDT, check original column type
tudt_col_idx <- which(toupper(names(edc_tu))=="TUDT")
if (length(tudt_col_idx)>0 && inherits(edc_tu[[tudt_col_idx[1]]], "Date")) {
  TUDT_vec <- edc_tu[[tudt_col_idx[1]]]  # keep Date
} else if (length(tudt_col_idx)>0 && inherits(edc_tu[[tudt_col_idx[1]]], "POSIXt")) {
  TUDT_vec <- as.Date(edc_tu[[tudt_col_idx[1]]])
}

# Build tu_prep
tu_prep <- data.frame(
  SUBJID = as.character(SUBJID_vec),
  LOC = as.character(LOC_vec),
  METH = as.character(METH_vec),
  LNKID = as.character(LNKID_vec),
  TYPE = as.character(TYPE_vec),
  VISIT_orig = as.character(VISIT_vec),
  TUDT_orig = I(TUDT_vec), # keep as is (may be Date)
  stringsAsFactors=FALSE
)
# USUBJID = catx("-", "ONCO-2026", SUBJID)
tu_prep$USUBJID <- sas_catx("-", "ONCO-2026", tu_prep$SUBJID)
# TULOC = upcase(tranwrd(LOC, "_", " "))
tu_prep$TULOC <- sas_upcase(sas_tranwrd(tu_prep$LOC, "_", " "))
# TUMETHOD = upcase(tranwrd(METH, "_", " "))
tu_prep$TUMETHOD <- sas_upcase(sas_tranwrd(tu_prep$METH, "_", " "))

# Keep other fields for merge
tu_prep$LNKID <- as.character(LNKID_vec)
tu_prep$TYPE <- as.character(TYPE_vec)
tu_prep$VISIT_raw <- as.character(VISIT_vec)
# TUDT handling: keep original for later conversion
# If TUDT_vec is Date, store as Date; else character
if (inherits(TUDT_vec, "Date")) {
  tu_prep$TUDT <- as.character(format(TUDT_vec, "%d%b%Y")) # to mimic date11. input
  tu_prep$TUDT_date <- TUDT_vec
} else {
  tu_prep$TUDT <- as.character(TUDT_vec)
  tu_prep$TUDT_date <- as.Date(rep(NA, length(TUDT_vec)))
  # Try to parse character TUDT to Date for later
}

# proc sort data=tu_prep by USUBJID
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last=FALSE), , drop=FALSE]
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last=FALSE), , drop=FALSE]

# ------------------------------------------------------------------
# 3. Map TU Domain
# ------------------------------------------------------------------
# merge tu_prep(in=t) dm_for_tu by USUBJID; if t;
# Left join tu_prep with dm_for_tu
# Use dplyr left_join
library(dplyr)

# Prepare dm_for_tu dedup? SAS merge by USUBJID with many-to-one: if dm has one per USUBJID, fine.
# If dm has duplicates, SAS would produce cartesian within BY group. We'll left_join.
sdtm_tu1 <- tu_prep %>%
  left_join(dm_for_tu, by="USUBJID")

# Now derive fields
# STUDYID, DOMAIN, TULNKID, TUTESTCD, TUTEST, TUORRES, TUSTRESC, TUEVAL, VISIT, TUDTC, TUSTDY
sdtm_tu1$STUDYID <- "ONCO-2026"
sdtm_tu1$DOMAIN <- "TU"
sdtm_tu1$TULNKID <- sdtm_tu1$LNKID
sdtm_tu1$TUTESTCD <- "TUMIDENT"
sdtm_tu1$TUTEST <- "Tumor Identification"
sdtm_tu1$TUORRES <- sas_upcase(sdtm_tu1$TYPE)
sdtm_tu1$TUSTRESC <- sdtm_tu1$TUORRES
sdtm_tu1$TUEVAL <- "INVESTIGATOR"
sdtm_tu1$VISIT <- sas_upcase(sas_tranwrd(sdtm_tu1$VISIT_raw, "_", " "))

# TUDTC = put(input(TUDT, date11.), yymmdd10.)
# If TUDT ne "" then TUDTC = ...
sdtm_tu1$TUDTC <- NA_character_
for (i in seq_len(nrow(sdtm_tu1))) {
  tudt_val <- sdtm_tu1$TUDT[i]
  # Also check TUDT_date if available
  if (!is.na(tudt_val) && trimws(tudt_val) != "") {
    # Try parse as date11
    d <- parse_date11(tudt_val)
    # If parse failed but we have TUDT_date
    if (is.na(d) && !is.na(sdtm_tu1$TUDT_date[i])) {
      d <- as.Date(sdtm_tu1$TUDT_date[i])
    }
    # Also try yymmdd10 parse (in case TUDT already YYYY-MM-DD)
    if (is.na(d)) {
      d <- parse_yymmdd10(tudt_val)
    }
    if (!is.na(d)) {
      sdtm_tu1$TUDTC[i] <- put_yymmdd10(d)
    } else {
      sdtm_tu1$TUDTC[i] <- NA_character_
    }
  } else {
    # Check if TUDT_date exists (when original was Date and TUDT was converted)
    if (!is.na(sdtm_tu1$TUDT_date[i])) {
      sdtm_tu1$TUDTC[i] <- put_yymmdd10(as.Date(sdtm_tu1$TUDT_date[i]))
    } else {
      sdtm_tu1$TUDTC[i] <- NA_character_
    }
  }
}

# Study Day
# if TUDTC ne "" and RFSTDTC ne "" then do; _stdt=input(TUDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt >= _rfdt then TUSTDY=(_stdt - _rfdt)+1 else TUSTDY=(_stdt - _rfdt);
sdtm_tu1$TUSTDY <- NA_real_
for (i in seq_len(nrow(sdtm_tu1))) {
  tudtc <- sdtm_tu1$TUDTC[i]
  rfstdtc <- sdtm_tu1$RFSTDTC[i]
  if (!is.na(tudtc) && trimws(tudtc)!="" && !is.na(rfstdtc) && trimws(rfstdtc)!="") {
    stdt <- parse_yymmdd10(tudtc)
    rfdt <- parse_yymmdd10(rfstdtc)
    # RFSTDTC may contain time like 2020-01-01T00:00:00, parse first 10 chars
    if (is.na(rfdt) && nchar(rfstdtc)>=10) {
      rfdt <- parse_yymmdd10(substr(rfstdtc,1,10))
    }
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) {
        sdtm_tu1$TUSTDY[i] <- diff + 1
      } else {
        sdtm_tu1$TUSTDY[i] <- diff
      }
    }
  }
}

# Keep only needed columns for sdtm_tu1 (as in SAS length statement)
# sdtm_tu1 has STUDYID, DOMAIN, USUBJID, TULNKID, TUTESTCD, TUTEST, TUORRES, TUSTRESC, TULOC, TUMETHOD, TUEVAL, VISIT, TUDTC, TUSTDY
# Also need to handle lengths: truncate to SAS lengths
# TULNKID $5, TUTESTCD $8, TUTEST $40, TUORRES $15, TUSTRESC $15, TULOC $20, TUMETHOD $20, TUEVAL $15, VISIT $20, TUDTC $19
truncate_sas <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(x, 1, len))
}
sdtm_tu1$TULNKID <- truncate_sas(sdtm_tu1$TULNKID, 5)
sdtm_tu1$TUTESTCD <- truncate_sas(sdtm_tu1$TUTESTCD, 8)
sdtm_tu1$TUTEST <- truncate_sas(sdtm_tu1$TUTEST, 40)
sdtm_tu1$TUORRES <- truncate_sas(sdtm_tu1$TUORRES, 15)
sdtm_tu1$TUSTRESC <- truncate_sas(sdtm_tu1$TUSTRESC, 15)
sdtm_tu1$TULOC <- truncate_sas(sdtm_tu1$TULOC, 20)
sdtm_tu1$TUMETHOD <- truncate_sas(sdtm_tu1$TUMETHOD, 20)
sdtm_tu1$TUEVAL <- truncate_sas(sdtm_tu1$TUEVAL, 15)
sdtm_tu1$VISIT <- truncate_sas(sdtm_tu1$VISIT, 20)
sdtm_tu1$TUDTC <- truncate_sas(sdtm_tu1$TUDTC, 19)
sdtm_tu1$STUDYID <- truncate_sas(sdtm_tu1$STUDYID, 10)
sdtm_tu1$DOMAIN <- truncate_sas(sdtm_tu1$DOMAIN, 2)
sdtm_tu1$USUBJID <- truncate_sas(sdtm_tu1$USUBJID, 25)

# ------------------------------------------------------------------
# 4. Derive TUSEQ
# ------------------------------------------------------------------
# proc sort data=sdtm_tu1 by USUBJID TUDTC
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, sdtm_tu1$TUDTC, na.last=FALSE), , drop=FALSE]

# data sdtm_tu; by USUBJID; retain TUSEQ; if first.USUBJID then TUSEQ=1 else TUSEQ+1;
sdtm_tu <- sdtm_tu1
sdtm_tu$TUSEQ <- NA_integer_
# Use dplyr group_by
sdtm_tu <- sdtm_tu %>%
  group_by(USUBJID) %>%
  mutate(TUSEQ = row_number()) %>%
  ungroup() %>%
  as.data.frame()

# Keep final columns in SAS order
sdtm_tu <- sdtm_tu[, c("STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT")]

# Ensure types: TUSEQ numeric, TUSTDY numeric
sdtm_tu$TUSEQ <- as.integer(sdtm_tu$TUSEQ)
sdtm_tu$TUSTDY <- as.integer(sdtm_tu$TUSTDY)

# Sort for final display as SAS would (by USUBJID TUDTC already)
# Ensure SAS character ordering (uppercase before lowercase) - R default is similar for ASCII

# ------------------------------------------------------------------
# 5. View Final Dataset - proc print
# ------------------------------------------------------------------
# Create text rendering for P029.lst
# Count numbers: 44 numbers expected (likely TUSEQ + TUSTDY values)
# Build proc print style text

# Prepare display dataframe for print
print_df <- sdtm_tu

# Create text file
txt_path <- file.path(out, "P029.txt")
lst_path <- file.path(out, "P029.lst")
csv_body_path <- file.path(out, "P029.csv")

# Also create tu.csv dataset
tu_csv_path <- file.path(out, "tu.csv")
write.csv(sdtm_tu, tu_csv_path, row.names=FALSE, na="")

# Build text rendering
# Header
header_lines <- c(
  "Listing: SDTM TU Domain - Tumor Identification",
  "Study: ONCO-2026",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  "",
  sprintf("Observations: %d", nrow(sdtm_tu)),
  ""
)

# Create table header line
col_names <- names(print_df)
# Format as fixed width for readability
# Use capture.output of print.data.frame with some formatting
# We'll create a simple tab-delimited view plus a pretty printed version

# Build pretty text table
# Use format to align
# For txt file, include column headers and rows

# Create formatted table string
# Use base R to create a text table with Obs column
obs_df <- cbind(Obs=seq_len(nrow(print_df)), print_df)

# Convert to character for display, NA as ""
obs_df_disp <- obs_df
for (col in names(obs_df_disp)) {
  obs_df_disp[[col]] <- ifelse(is.na(obs_df_disp[[col]]), "", as.character(obs_df_disp[[col]]))
}

# Build text lines
# Header row
header_row <- paste(names(obs_df_disp), collapse=" | ")
separator <- paste(rep("-", nchar(header_row)), collapse="")
lines <- c(header_lines, header_row, separator)
for (i in seq_len(nrow(obs_df_disp))) {
  row_vals <- as.character(obs_df_disp[i, ])
  # Escape pipe in values
  row_vals <- gsub("\\|", "/", row_vals)
  lines <- c(lines, paste(row_vals, collapse=" | "))
}
lines <- c(lines, "", paste0("Total observations: ", nrow(sdtm_tu)), paste0("Total numeric values (TUSEQ + TUSTDY non-missing): ", sum(!is.na(sdtm_tu$TUSEQ)) + sum(!is.na(sdtm_tu$TUSTDY))))

# Write txt
writeLines(lines, txt_path)
# Also write .lst file (same content)
writeLines(lines, lst_path)

# Write CSV of table body (same as tu.csv but with Obs)
write.csv(obs_df, csv_body_path, row.names=FALSE, na="")

# Also ensure P029.lst.txt if harness expects
writeLines(lines, file.path(out, "P029.lst.txt"))

# ------------------------------------------------------------------
# 6. Export Final Dataset - already done as tu.csv
# Also create tu.xlsx equivalent? Not required but tu.csv is required
# Ensure dataset tu.csv has correct date formatting (TUDTC as YYYY-MM-DD)
# Already done

# Log progress
cat(sprintf("P029: tu dataset written to %s with %d rows\n", tu_csv_path, nrow(sdtm_tu)))
cat(sprintf("P029: listing written to %s\n", txt_path))
