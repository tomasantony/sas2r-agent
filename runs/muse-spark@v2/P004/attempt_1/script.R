# P004 adam_adae.sas -> R translation
# Output: adae.csv + listing P004 (PROC PRINT obs=15)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")
sdtm_dir <- file.path(project_path, "data/sdtm")
raw_dir  <- file.path(project_path, "data/raw")
# fallback raw pilot paths
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tibble)
})

# ---- helpers ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_file <- function(dirs, name) {
  # case-insensitive search for name with extensions
  exts <- c(".sas7bdat", ".xpt", ".XPT", ".sas7bdat")
  for (d in dirs) {
    if (is.na(d) || d == "") next
    if (!dir.exists(d)) next
    # list files
    fl <- list.files(d, full.names = TRUE)
    # exact case-insensitive match on basename without ext
    for (f in fl) {
      bn <- basename(f)
      # strip extension
      noext <- sub("\\.[^.]*$", "", bn)
      if (tolower(noext) == tolower(name)) return(f)
    }
    # try direct paths
    for (e in c(".sas7bdat", ".xpt", ".XPT", ".sas7bdat", ".csv")) {
      for (nm in c(name, tolower(name), toupper(name))) {
        p <- file.path(d, paste0(nm, e))
        if (file.exists(p)) return(p)
      }
    }
  }
  return(NA_character_)
}

read_sas_any <- function(path) {
  if (is.na(path)) return(NULL)
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    df <- tibble::as_tibble(df)
  } else {
    df <- haven::read_sas(path)
  }
  # convert to plain data.frame/tibble, keep labels but uppercase names
  df <- tibble::as_tibble(df)
  names(df) <- toupper(names(df))
  df
}

to_R_Date <- function(x) {
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    # try ISO
    suppressWarnings({
      d <- as.Date(x, format = "%Y-%m-%d")
    })
    return(d)
  }
  suppressWarnings(as.Date(as.character(x)))
}

parse_yymmdd10_strict <- function(v) {
  # SAS: if length(AESTDTC)>=10 then input(substr(1,10), yymmdd10.)
  # LENGTH strips trailing blanks
  n <- length(v)
  out <- rep(as.Date(NA), n)
  for (i in seq_len(n)) {
    s <- v[i]
    if (is.na(s)) next
    s <- as.character(s)
    # SAS LENGTH: length excluding trailing blanks
    s_notrail <- sub(" +$", "", s)
    if (nchar(s_notrail) < 10) next
    sub10 <- substr(s, 1, 10)
    # strict YYYY-MM-DD
    d <- suppressWarnings(as.Date(sub10, format = "%Y-%m-%d"))
    # haven/SAS yymmdd10. requires dashes at 5 and 8? enforce
    if (!is.na(d)) {
      # verify pattern \d{4}-\d{2}-\d{2}
      if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", sub10)) {
        d <- as.Date(NA)
      }
    }
    out[i] <- d
  }
  out
}

format_date9 <- function(d) {
  # SAS DATE9.: DDMMMYYYY, e.g. 01JAN2006, missing -> "."
  sapply(d, function(x) {
    if (is.na(x)) return(".")
    x <- as.Date(x, origin = "1970-01-01")
    dd <- format(x, "%d")
    mmm <- toupper(format(x, "%b"))
    yyyy <- format(x, "%Y")
    paste0(dd, mmm, yyyy)
  }, USE.NAMES = FALSE)
}

sas_order <- function(df, cols) {
  # SAS ordering: missing first, character ASCII (uppercase before lowercase)
  # Use C collate for ASCII
  old <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  on.exit(suppressWarnings(try(Sys.setlocale("LC_COLLATE", old), silent = TRUE)), add = TRUE)
  # build order args
  ord_list <- list()
  for (cl in cols) {
    v <- df[[cl]]
    if (is.character(v)) {
      # "" sorts first naturally in C locale; NA needs na.last=FALSE
      ord_list[[length(ord_list) + 1]] <- v
    } else {
      ord_list[[length(ord_list) + 1]] <- v
    }
  }
  # order with na.last=FALSE
  o <- do.call(order, c(ord_list, list(na.last = FALSE, method = "radix")))
  # radix method may not respect C locale for characters? fallback to default if needed
  # If radix ignores locale, use non-radix for character cols
  # Check: if any character col, redo without radix to respect locale
  has_char <- any(sapply(cols, function(cl) is.character(df[[cl]])))
  if (has_char) {
    o <- do.call(order, c(ord_list, list(na.last = FALSE)))
  }
  o
}

message("Reading inputs...")

adsl_path <- find_file(c(adam_dir, rawadam_dir, raw_dir), "adsl")
ae_path   <- find_file(c(sdtm_dir, rawsdtm_dir, raw_dir), "ae")

if (is.na(adsl_path)) stop("ADSL dataset not found")
if (is.na(ae_path)) stop("AE dataset not found")
message(paste0("ADSL: ", adsl_path))
message(paste0("AE: ", ae_path))

adsl <- read_sas_any(adsl_path)
ae   <- read_sas_any(ae_path)

# Ensure required cols exist
# ADSL needs: USUBJID STUDYID SUBJID TRT01A TRTSDT TRTEDT AGE SEX SAFFL
# AE needs: USUBJID AESEQ AETERM AEDECOD AESEV AEREL AEOUT AESER AESTDTC AEENDTC
# stopifnot checks after join per rules; check now
message("ADSL cols: "); message(paste(names(adsl), collapse = ","))
message("AE cols: "); message(paste(names(ae), collapse = ","))

# 1. SORT INPUTS BY UNIQUE SUBJECT ID (SAS sorts)
# sort adsl by USUBJID
stopifnot("USUBJID" %in% names(adsl))
stopifnot("USUBJID" %in% names(ae))
stopifnot("AESEQ" %in% names(ae))

adsl_sorted <- adsl[sas_order(adsl, c("USUBJID")), , drop = FALSE]
ae_sorted   <- ae[sas_order(ae, c("USUBJID", "AESEQ")), , drop = FALSE]

# 2. MERGE ADSL AND AE TO CREATE ADAE
# Before merging, drop/rename overlapping cols except keys to avoid .x/.y
keys <- c("USUBJID")
overlap <- intersect(names(adsl_sorted), names(ae_sorted))
overlap <- setdiff(overlap, keys)
message(paste0("Overlapping cols (except keys): ", paste(overlap, collapse = ",")))

# Rename overlapping in each to allow coalesce (AE takes precedence like SAS merge with b)
adsl_r <- adsl_sorted
ae_r <- ae_sorted
for (cl in overlap) {
  names(adsl_r)[names(adsl_r) == cl] <- paste0(cl, "__ADSL")
  names(ae_r)[names(ae_r) == cl] <- paste0(cl, "__AE")
}

merged <- dplyr::left_join(ae_r, adsl_r, by = keys)
# coalesce overlapping: AE first, then ADSL (SAS: rightmost contributing? AE present so AE wins)
for (cl in overlap) {
  aec <- paste0(cl, "__AE")
  adc <- paste0(cl, "__ADSL")
  av <- merged[[aec]]
  bv <- merged[[adc]]
  # coalesce: if av not missing (for char: not NA and not ""? SAS blank is missing)
  # For simplicity: if av is NA (or for Date NA) use bv; for char, "" in AE? treat "" as missing -> use ADSL if ADSL non-blank?
  # Implement generic coalesce with blank handling for character
  if (is.character(av) || is.character(bv)) {
    av_c <- as.character(av); bv_c <- as.character(bv)
    # consider "" as missing for coalesce? SAS blank missing
    res <- ifelse(!is.na(av_c) & av_c != "", av_c,
            ifelse(!is.na(bv_c), bv_c, av_c))
    merged[[cl]] <- res
  } else {
    # numeric/date: dplyr coalesce
    # need same class; convert Dates?
    # if one is Date and other numeric? convert both via to_R_Date? keep as is if both Date/numeric
    # Use manual: ifelse is.na(av), bv, av) preserving Date
    if (inherits(av, "Date") || inherits(bv, "Date")) {
      avd <- to_R_Date(av); bvd <- to_R_Date(bv)
      res <- avd
      idx <- is.na(avd)
      res[idx] <- bvd[idx]
      merged[[cl]] <- res
    } else {
      res <- av
      idx <- is.na(av)
      # handle length mismatch due to type? assume same length
      res[idx] <- bv[idx]
      merged[[cl]] <- res
    }
  }
  merged[[aec]] <- NULL
  merged[[adc]] <- NULL
}

# After join, stopifnot cols for filter exist
stopifnot("AESTDTC" %in% names(merged))
stopifnot("AEENDTC" %in% names(merged))
stopifnot("TRTSDT" %in% names(merged))
stopifnot("SAFFL" %in% names(merged))

# Keep only subjects who actually experienced AE: if b -> all rows of merged (left join from AE) are kept
# (merged is exactly AE rows with ADSL info; no filtering needed, but ensure zero-row check)
if (nrow(merged) == 0) stop("Merge resulted in zero rows - join bug")

# Ensure TRTSDT/TRTEDT as Date
merged[["TRTSDT"]] <- to_R_Date(merged[["TRTSDT"]])
if ("TRTEDT" %in% names(merged)) merged[["TRTEDT"]] <- to_R_Date(merged[["TRTEDT"]])

# NUMERIC DATE DERIVATIONS
# AESTDTC/AEENDTC are character ISO
AESTDTC_chr <- as.character(merged[["AESTDTC"]])
AEENDTC_chr <- as.character(merged[["AEENDTC"]])
merged[["AESTDT"]] <- parse_yymmdd10_strict(AESTDTC_chr)
merged[["AEENDT"]] <- parse_yymmdd10_strict(AEENDTC_chr)

# ANALYSIS RELATIVE DAYS (ASTDY)
AESTDT <- merged[["AESTDT"]]
TRTSDT <- merged[["TRTSDT"]]
ASTDY <- rep(NA_real_, nrow(merged))
idx <- !is.na(AESTDT) & !is.na(TRTSDT)
ge <- idx & (AESTDT >= TRTSDT)
lt <- idx & (AESTDT < TRTSDT)
ASTDY[ge] <- as.numeric(AESTDT[ge] - TRTSDT[ge]) + 1
ASTDY[lt] <- as.numeric(AESTDT[lt] - TRTSDT[lt])
merged[["ASTDY"]] <- ASTDY

# TREATMENT-EMERGENT FLAG
SAFFL <- as.character(merged[["SAFFL"]])
SAFFL[is.na(SAFFL)] <- ""
TRTEMFL <- rep("", nrow(merged))
idx2 <- (SAFFL == "Y") & !is.na(AESTDT) & !is.na(TRTSDT) & (AESTDT >= TRTSDT)
TRTEMFL[idx2] <- "Y"
merged[["TRTEMFL"]] <- TRTEMFL

# Keep essential vars
keep_vars <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
               "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
               "AESTDT","AEENDT","ASTDY","TRTEMFL")
# Ensure all keep vars exist; if missing, create as NA
for (cl in keep_vars) {
  if (!cl %in% names(merged)) {
    message(paste0("WARNING: keep var missing, creating NA: ", cl))
    merged[[cl]] <- NA
  }
}
adae_draft <- merged[, keep_vars, drop = FALSE]

# 3. DERIVE FIRST TREATMENT-EMERGENT OCCURRENCE BY DECODED TERM
adae_draft[["_occ_order"]] <- ifelse(adae_draft[["TRTEMFL"]] == "Y", 0, 1)
# Handle NA TRTEMFL -> 1
adae_draft[["_occ_order"]][is.na(adae_draft[["_occ_order"]])] <- 1

# sort by USUBJID AEDECOD _occ_order AESTDT AESEQ
o2 <- sas_order(adae_draft, c("USUBJID","AEDECOD","_occ_order","AESTDT","AESEQ"))
adae_occurrence_order <- adae_draft[o2, , drop = FALSE]

# by USUBJID AEDECOD _occ_order -> first._occ_order
# Need grouping with missing AEDECOD together ("" and NA together)
grp_aedecod <- as.character(adae_occurrence_order[["AEDECOD"]])
grp_aedecod[is.na(grp_aedecod) | grp_aedecod == ""] <- "<__MISSING__>"
grp_usubjid <- as.character(adae_occurrence_order[["USUBJID"]])
grp_usubjid[is.na(grp_usubjid)] <- "<__MISSING__>"
grp_occ <- adae_occurrence_order[["_occ_order"]]

# compute first flag per group
# use rle-like: first row of each group
key <- paste0(grp_usubjid, "\x1e", grp_aedecod, "\x1e", grp_occ)
first_occ <- !duplicated(key)
trt <- as.character(adae_occurrence_order[["TRTEMFL"]])
trt[is.na(trt)] <- ""
AOCCFL <- rep("", nrow(adae_occurrence_order))
AOCCFL[trt == "Y" & first_occ] <- "Y"
adae_occurrence_order[["AOCCFL"]] <- AOCCFL
adae_flagged <- adae_occurrence_order
adae_flagged[["_occ_order"]] <- NULL

# 4. FINALIZE AND SAVE TO ADAM LIBRARY: sort by USUBJID AESTDT AESEQ
o3 <- sas_order(adae_flagged, c("USUBJID","AESTDT","AESEQ"))
adae <- adae_flagged[o3, , drop = FALSE]

# Ensure types: AESTDT/AEENDT/TRTSDT/TRTEDT Date, ASTDY numeric, AESEQ numeric
# Keep column order as in keep + AOCCFL
adae <- adae[, c(keep_vars, "AOCCFL"), drop = FALSE]

message(paste0("ADAE rows: ", nrow(adae)))

# Write dataset adae.csv with ISO dates
adae_out <- adae
# Ensure Date columns are Date class for write.csv ISO formatting
for (dc in c("AESTDT","AEENDT","TRTSDT","TRTEDT")) {
  if (dc %in% names(adae_out)) {
    adae_out[[dc]] <- to_R_Date(adae_out[[dc]])
  }
}
# write.csv will format Date as YYYY-MM-DD
write.csv(adae_out, file.path(out, "adae.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT: PROC PRINT obs=15 vars USUBJID TRT01A AETERM AESEV AESTDT TRTSDT ASTDY TRTEMFL AOCCFL
print_vars <- c("USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
for (cl in print_vars) {
  if (!cl %in% names(adae)) stop(paste0("print var missing: ", cl))
}
audit_n <- min(15, nrow(adae))
audit <- adae[seq_len(audit_n), print_vars, drop = FALSE]

# Build text rendering mimicking PROC PRINT with DATE9. for dates
title_txt <- "ADAE Audit - Treatment-Emergent and First-Occurrence Flags"
# Prepare display values
disp <- audit
# Format dates as DATE9.
disp[["AESTDT"]] <- format_date9(audit[["AESTDT"]])
disp[["TRTSDT"]] <- format_date9(audit[["TRTSDT"]])
# ASTDY: missing -> ".", else integer
disp[["ASTDY"]] <- sapply(audit[["ASTDY"]], function(x) if (is.na(x)) "." else format(as.integer(x), trim = TRUE))
# Character missing ("") -> blank (keep as "")
for (cl in c("USUBJID","TRT01A","AETERM","AESEV","TRTEMFL","AOCCFL")) {
  v <- as.character(disp[[cl]])
  v[is.na(v)] <- ""
  disp[[cl]] <- v
}

# Create fixed-width-ish text lines
header <- sprintf("%-5s %-18s %-12s %-30s %-12s %-10s %-10s %-6s %-8s %-6s",
  "Obs","USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
sep_line <- paste(rep("-", nchar(header)), collapse = "")
lines <- c(title_txt, "", header, sep_line)
for (i in seq_len(nrow(disp))) {
  r <- disp[i, ]
  line <- sprintf("%-5d %-18s %-12s %-30s %-12s %-10s %-10s %-6s %-8s %-6s",
    i,
    substr(r[["USUBJID"]],1,18),
    substr(r[["TRT01A"]],1,12),
    substr(r[["AETERM"]],1,30),
    substr(r[["AESEV"]],1,12),
    r[["AESTDT"]],
    r[["TRTSDT"]],
    r[["ASTDY"]],
    r[["TRTEMFL"]],
    r[["AOCCFL"]])
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("NOTE: N=", nrow(adae), " total ADAE records; showing first ", audit_n, " observations."))

txt_path1 <- file.path(out, "P004.txt")
txt_path2 <- file.path(out, "P004.lst.txt")
txt_path3 <- file.path(out, "P004.lst")
writeLines(lines, txt_path1)
writeLines(lines, txt_path2)
writeLines(lines, txt_path3)

# CSV of table body (listing data) with ISO dates
listing_csv <- audit
# Ensure dates ISO (already Date in audit)
for (dc in c("AESTDT","TRTSDT")) listing_csv[[dc]] <- to_R_Date(listing_csv[[dc]])
# Add Obs column like SAS
listing_csv <- cbind(Obs = seq_len(nrow(listing_csv)), listing_csv)
write.csv(listing_csv, file.path(out, "P004.csv"), row.names = FALSE, na = "")
write.csv(listing_csv, file.path(out, "P004.lst.csv"), row.names = FALSE, na = "")

# Also CSV of plotted/listing data for figure requirement (plotted data)
write.csv(listing_csv, file.path(out, "P004_listing_data.csv"), row.names = FALSE, na = "")

message("Done.")
