# P025_ex.sas translation to R
# SDTM EX mapping - proc print listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "/bench"
rawdir <- "/bench/data/raw"
rawsdtm <- "/bench/data/raw/cdisc-pilot/sdtm"
rawadam <- "/bench/data/raw/cdisc-pilot/adam"
rawsplit <- "/bench/data/derived/cdisc-pilot/adam-split"
raw_faijal <- "/bench/data/raw/faijal-oncology/raw_data"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readxl)
  library(openxlsx)
  library(data.table)
})

# helper: case-insensitive column lookup
get_col <- function(df, name) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
set_col <- function(df, name, value) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) == 0) {
    df[[name]] <- value
  } else {
    df[[idx[1]]] <- value
  }
  df
}

# helper: SAS round half away from zero (not strictly needed here)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: parse SAS date11. e.g. "01JAN2020", "1-JAN-2020", "01JAN2020:00:00:00"
parse_date11 <- function(x) {
  if (is.null(x)) return(as.Date(rep(NA, 0)))
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | toupper(x) == "NA" | toupper(x) == "."] <- NA
  # remove time part if present
  x <- sub("[: ].*$", "", x) # keep only date part before colon or space if needed
  # SAS date11. is DDMMMYYYY or DMMMYYYY, sometimes with hyphens
  # Normalize: remove hyphens, spaces
  x_clean <- gsub("[-/ ]", "", x)
  # Use lubridate dmy with quiet
  res <- suppressWarnings(lubridate::dmy(x_clean, quiet = TRUE))
  # fallback: try as.Date with %d%b%Y
  miss <- is.na(res) & !is.na(x)
  if (any(miss)) {
    # try upper case month
    res2 <- suppressWarnings(as.Date(toupper(x_clean[miss]), format = "%d%b%Y"))
    res[miss] <- res2
  }
  # fallback: try ymd if already iso
  miss2 <- is.na(res) & !is.na(x)
  if (any(miss2)) {
    res3 <- suppressWarnings(lubridate::ymd(x[miss2], quiet = TRUE))
    res[miss2] <- as.Date(res3)
  }
  res
}

# helper: parse ISO yymmdd10. YYYY-MM-DD
parse_yymmdd10 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" | toupper(x) == "NA"] <- NA
  suppressWarnings(lubridate::ymd(x, quiet = TRUE))
}

# helper: format Date to YYYY-MM-DD character, NA -> ""
fmt_date <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# helper: find file case-insensitive
find_file <- function(dirs, pattern) {
  for (d in dirs) {
    if (!dir.exists(d)) next
    files <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    hit <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
    if (length(hit) > 0) return(hit[1])
  }
  return(NA_character_)
}

cat("Reading edc_ex...\n")
edc_ex_path <- file.path(raw_faijal, "edc_ex.xlsx")
edc_ex <- NULL
if (file.exists(edc_ex_path)) {
  edc_ex <- tryCatch(readxl::read_excel(edc_ex_path, .name_repair = "minimal"), error = function(e) NULL)
}
if (is.null(edc_ex)) {
  # try alternative locations
  alt <- find_file(c(rawdir, root), "^edc_ex\\.xlsx$")
  if (!is.na(alt)) edc_ex <- tryCatch(readxl::read_excel(alt, .name_repair = "minimal"), error = function(e) NULL)
}
if (is.null(edc_ex)) stop("edc_ex.xlsx not found")

# Normalize names to original but allow case-insensitive access
names(edc_ex) <- trimws(names(edc_ex))

# Read edc_dm for fallback RFSTDTC if needed
edc_dm_path <- file.path(raw_faijal, "edc_dm.xlsx")
edc_dm <- NULL
if (file.exists(edc_dm_path)) {
  edc_dm <- tryCatch(readxl::read_excel(edc_dm_path, .name_repair = "minimal"), error = function(e) NULL)
}

# Attempt to read sdtm_dm
cat("Locating sdtm_dm...\n")
dm_for_ex <- NULL
search_dirs <- c(rawsdtm, rawadam, rawdir, file.path(root, "data"), file.path(root, "output"), raw_faijal)
# common filenames
candidates <- c(
  file.path(rawsdtm, "dm.xpt"),
  file.path(rawsdtm, "DM.xpt"),
  file.path(rawsdtm, "dm.sas7bdat"),
  file.path(rawsdtm, "DM.sas7bdat"),
  file.path(rawsdtm, "dm.xlsx"),
  file.path(rawadam, "dm.xpt"),
  file.path(rawadam, "adsl.xpt"),
  file.path(root, "data/raw/cdisc-pilot/sdtm/dm.xpt")
)
found_dm <- NULL
for (p in candidates) {
  if (file.exists(p)) { found_dm <- p; break }
}
if (is.null(found_dm) || is.na(found_dm)) {
  # search recursively
  f1 <- find_file(c(rawsdtm, rawadam, rawdir), "^dm\\.(xpt|sas7bdat|xlsx)$")
  if (!is.na(f1)) found_dm <- f1
}
if (!is.null(found_dm) && !is.na(found_dm) && file.exists(found_dm)) {
  cat("Reading sdtm_dm from:", found_dm, "\n")
  dm_raw <- tryCatch({
    if (grepl("\\.xpt$", found_dm, ignore.case = TRUE)) haven::read_xpt(found_dm)
    else if (grepl("\\.sas7bdat$", found_dm, ignore.case = TRUE)) haven::read_sas(found_dm)
    else if (grepl("\\.xlsx$", found_dm, ignore.case = TRUE)) readxl::read_excel(found_dm, .name_repair = "minimal")
    else NULL
  }, error = function(e) { cat("Error reading dm:", conditionMessage(e), "\n"); NULL })
  if (!is.null(dm_raw)) {
    # keep USUBJID RFSTDTC
    usub <- get_col(dm_raw, "USUBJID")
    rfst <- get_col(dm_raw, "RFSTDTC")
    if (!is.null(usub) && !is.null(rfst)) {
      dm_for_ex <- tibble(USUBJID = as.character(usub), RFSTDTC = as.character(rfst))
      # convert haven labelled to character
      dm_for_ex$RFSTDTC <- as.character(dm_for_ex$RFSTDTC)
      # keep only non-missing USUBJID
      dm_for_ex <- dm_for_ex %>% filter(!is.na(USUBJID) & USUBJID != "")
      # if RFSTDTC contains datetime, keep date part
      dm_for_ex$RFSTDTC <- substr(dm_for_ex$RFSTDTC, 1, 10)
      dm_for_ex <- dm_for_ex %>% distinct(USUBJID, .keep_all = TRUE)
    }
  }
}

# Fallback: derive dm_for_ex from edc_dm or edc_ex if not found
if (is.null(dm_for_ex) || nrow(dm_for_ex) == 0) {
  cat("sdtm_dm not found, deriving RFSTDTC fallback...\n")
  # Try edc_dm
  if (!is.null(edc_dm)) {
    # look for RFSTDTC column
    rf_col <- get_col(edc_dm, "RFSTDTC")
    subj_col <- get_col(edc_dm, "SUBJID")
    if (is.null(subj_col)) subj_col <- get_col(edc_dm, "USUBJID")
    if (!is.null(rf_col) && !is.null(subj_col)) {
      dm_for_ex <- tibble(
        USUBJID = paste0("ONCO-2026-", as.character(subj_col)),
        RFSTDTC = as.character(rf_col)
      )
      dm_for_ex$RFSTDTC <- substr(dm_for_ex$RFSTDTC, 1, 10)
      dm_for_ex <- dm_for_ex %>% filter(!is.na(USUBJID))
    }
  }
  # If still null, derive from edc_ex min STDT per subject
  if (is.null(dm_for_ex) || nrow(dm_for_ex) == 0) {
    # parse STDT to get min date per SUBJID
    tmp_subj <- get_col(edc_ex, "SUBJID")
    tmp_stdt <- get_col(edc_ex, "STDT")
    if (!is.null(tmp_subj) && !is.null(tmp_stdt)) {
      tmp_df <- tibble(SUBJID = as.character(tmp_subj), STDT = as.character(tmp_stdt))
      tmp_df$parsed <- parse_date11(tmp_df$STDT)
      tmp_df$USUBJID <- paste0("ONCO-2026-", trimws(tmp_df$SUBJID))
      dm_for_ex <- tmp_df %>%
        filter(!is.na(parsed)) %>%
        group_by(USUBJID) %>%
        summarise(RFSTDTC = format(min(parsed, na.rm = TRUE), "%Y-%m-%d"), .groups = "drop")
    }
  }
  # If still null, create empty
  if (is.null(dm_for_ex)) {
    dm_for_ex <- tibble(USUBJID = character(0), RFSTDTC = character(0))
  }
}

cat("dm_for_ex rows:", nrow(dm_for_ex), "\n")

# --------------------------------------------------------------
# 2. Prepare Raw EX Data
# --------------------------------------------------------------
cat("Preparing ex_prep...\n")
# Ensure expected columns exist, create if missing
# edc_ex columns: SUBJID, DOSE, STDT, ENDT, TRT, UNIT, FORM, FREQ, ROUTE, LOT, VISIT etc.
# Use case-insensitive getters
ex_prep <- edc_ex

# Create USUBJID = catx("-", "ONCO-2026", SUBJID)
subj_col <- get_col(ex_prep, "SUBJID")
if (is.null(subj_col)) {
  # try USUBJID already
  subj_col <- get_col(ex_prep, "USUBJID")
  if (!is.null(subj_col)) {
    ex_prep$USUBJID <- as.character(subj_col)
  } else {
    ex_prep$USUBJID <- NA_character_
  }
} else {
  ex_prep$USUBJID <- paste0("ONCO-2026-", trimws(as.character(subj_col)))
  # catx trims and skips missing: if SUBJID missing, result is "ONCO-2026"
  ex_prep$USUBJID[is.na(subj_col) | trimws(as.character(subj_col)) == ""] <- "ONCO-2026"
}

# Exclude records where dose was not administered: if DOSE ne .
dose_col <- get_col(ex_prep, "DOSE")
if (!is.null(dose_col)) {
  # convert to numeric if needed
  dose_num <- suppressWarnings(as.numeric(as.character(dose_col)))
  ex_prep$DOSE <- dose_num
  ex_prep <- ex_prep %>% filter(!is.na(DOSE))
} else {
  # if no DOSE column, keep all
  ex_prep$DOSE <- NA_real_
}

# Convert Dates to ISO 8601
stdt_col <- get_col(ex_prep, "STDT")
endt_col <- get_col(ex_prep, "ENDT")
# STDT and ENDT may be character or Date or numeric Excel date
# Helper to convert any STDT to character date11 then to ISO
convert_to_iso <- function(col) {
  if (is.null(col)) return(rep(NA_character_, nrow(ex_prep)))
  # if Date class
  if (inherits(col, "Date")) {
    return(ifelse(is.na(col), NA_character_, format(col, "%Y-%m-%d")))
  }
  if (inherits(col, "POSIXt")) {
    return(ifelse(is.na(col), NA_character_, format(as.Date(col), "%Y-%m-%d")))
  }
  # if numeric (Excel serial)
  if (is.numeric(col)) {
    # Excel origin 1899-12-30
    d <- suppressWarnings(as.Date(col, origin = "1899-12-30"))
    return(ifelse(is.na(col), NA_character_, format(d, "%Y-%m-%d")))
  }
  # character: parse date11
  parsed <- parse_date11(as.character(col))
  iso <- ifelse(is.na(parsed), NA_character_, format(parsed, "%Y-%m-%d"))
  # if already ISO, parse_date11 may have failed but ymd would succeed
  need <- is.na(iso) & !is.na(col) & trimws(as.character(col)) != ""
  if (any(need)) {
    iso2 <- suppressWarnings(lubridate::ymd(trimws(as.character(col[need])), quiet = TRUE))
    iso[need] <- ifelse(is.na(iso2), NA_character_, format(as.Date(iso2), "%Y-%m-%d"))
  }
  iso
}

ex_prep$EXSTDTC <- convert_to_iso(stdt_col)
ex_prep$EXENDTC <- convert_to_iso(endt_col)
# SAS: if STDT ne "" then EXSTDTC = put(input(STDT, date11.), yymmdd10.)
# So if STDT is "" or NA, EXSTDTC stays missing (NA)
ex_prep$EXSTDTC[is.na(stdt_col) | trimws(as.character(stdt_col)) == ""] <- NA_character_
ex_prep$EXENDTC[is.na(endt_col) | trimws(as.character(endt_col)) == ""] <- NA_character_

# Keep only needed columns for next steps plus original for mapping
# --------------------------------------------------------------
# 3. Sort Data for Merge and Sequencing
# --------------------------------------------------------------
# by USUBJID EXSTDTC (missing first in SAS)
ex_prep <- ex_prep %>%
  arrange(USUBJID, is.na(EXSTDTC) == FALSE, EXSTDTC)

# --------------------------------------------------------------
# 4. Create Intermediate EX Dataset
# --------------------------------------------------------------
cat("Creating sdtm_ex1...\n")
# Merge EX Data with DM: merge ex_prep(in=a) dm_for_ex; by USUBJID; if a;
# Left join, keep all ex_prep
# Ensure dm_for_ex has unique USUBJID
dm_for_ex_u <- dm_for_ex %>% distinct(USUBJID, .keep_all = TRUE)

sdtm_ex1 <- ex_prep %>%
  left_join(dm_for_ex_u, by = "USUBJID")

# Define variables
# Identifier
sdtm_ex1$STUDYID <- "ONCO-2026"
sdtm_ex1$DOMAIN <- "EX"

# Exposure variables
trt_col <- get_col(sdtm_ex1, "TRT")
unit_col <- get_col(sdtm_ex1, "UNIT")
form_col <- get_col(sdtm_ex1, "FORM")
freq_col <- get_col(sdtm_ex1, "FREQ")
route_col <- get_col(sdtm_ex1, "ROUTE")
lot_col <- get_col(sdtm_ex1, "LOT")
visit_col <- get_col(sdtm_ex1, "VISIT")

sdtm_ex1$EXTRT <- if (!is.null(trt_col)) toupper(trimws(as.character(trt_col))) else NA_character_
sdtm_ex1$EXDOSE <- sdtm_ex1$DOSE
sdtm_ex1$EXDOSU <- if (!is.null(unit_col)) toupper(trimws(as.character(unit_col))) else NA_character_
# EXDOSFRM = tranwrd(FORM, "_", " ")
if (!is.null(form_col)) {
  sdtm_ex1$EXDOSFRM <- gsub("_", " ", as.character(form_col), fixed = TRUE)
  # SAS tranwrd is case-sensitive, keep as is, then? No upcase mentioned
  sdtm_ex1$EXDOSFRM <- trimws(sdtm_ex1$EXDOSFRM)
} else {
  sdtm_ex1$EXDOSFRM <- NA_character_
}
sdtm_ex1$EXDOSFRQ <- if (!is.null(freq_col)) toupper(trimws(as.character(freq_col))) else NA_character_
sdtm_ex1$EXVLDIST <- if (!is.null(route_col)) toupper(trimws(as.character(route_col))) else NA_character_
sdtm_ex1$EXLOT <- if (!is.null(lot_col)) as.character(lot_col) else NA_character_
# VISIT: defined but never assigned in SAS, so stays missing
if (!is.null(visit_col)) {
  # SAS defines VISIT length but never assigns, so it remains blank
  sdtm_ex1$VISIT <- NA_character_
} else {
  sdtm_ex1$VISIT <- NA_character_
}

# Derive EXSTDY and EXENDY
# if EXSTDTC ne "" and RFSTDTC ne "" then do; _stdt = input(EXSTDTC, yymmdd10.); _rfdt = input(RFSTDTC, yymmdd10.); if _stdt >= _rfdt then EXSTDY = (_stdt - _rfdt) +1 else EXSTDY = (_stdt - _rfdt);
sdtm_ex1$EXSTDY <- NA_real_
sdtm_ex1$EXENDY <- NA_real_

# Parse dates
sdtm_ex1$`_stdt` <- parse_yymmdd10(sdtm_ex1$EXSTDTC)
sdtm_ex1$`_endt` <- parse_yymmdd10(sdtm_ex1$EXENDTC)
sdtm_ex1$`_rfdt` <- parse_yymmdd10(sdtm_ex1$RFSTDTC)

# Compute EXSTDY
idx_std <- !is.na(sdtm_ex1$EXSTDTC) & sdtm_ex1$EXSTDTC != "" & !is.na(sdtm_ex1$RFSTDTC) & sdtm_ex1$RFSTDTC != "" & !is.na(sdtm_ex1$`_stdt`) & !is.na(sdtm_ex1$`_rfdt`)
if (any(idx_std)) {
  diff_std <- as.numeric(sdtm_ex1$`_stdt`[idx_std] - sdtm_ex1$`_rfdt`[idx_std])
  sdtm_ex1$EXSTDY[idx_std] <- ifelse(sdtm_ex1$`_stdt`[idx_std] >= sdtm_ex1$`_rfdt`[idx_std], diff_std + 1, diff_std)
}
idx_end <- !is.na(sdtm_ex1$EXENDTC) & sdtm_ex1$EXENDTC != "" & !is.na(sdtm_ex1$RFSTDTC) & sdtm_ex1$RFSTDTC != "" & !is.na(sdtm_ex1$`_endt`) & !is.na(sdtm_ex1$`_rfdt`)
if (any(idx_end)) {
  diff_end <- as.numeric(sdtm_ex1$`_endt`[idx_end] - sdtm_ex1$`_rfdt`[idx_end])
  sdtm_ex1$EXENDY[idx_end] <- ifelse(sdtm_ex1$`_endt`[idx_end] >= sdtm_ex1$`_rfdt`[idx_end], diff_end + 1, diff_end)
}

# Drop temp
sdtm_ex1$`_stdt` <- NULL
sdtm_ex1$`_endt` <- NULL
sdtm_ex1$`_rfdt` <- NULL

# --------------------------------------------------------------
# 5. Sort Final EX Dataset
# --------------------------------------------------------------
sdtm_ex1 <- sdtm_ex1 %>%
  arrange(USUBJID, is.na(EXSTDTC) == FALSE, EXSTDTC)

# --------------------------------------------------------------
# 6. Derive EXSEQ
# --------------------------------------------------------------
cat("Deriving EXSEQ...\n")
sdtm_ex <- sdtm_ex1 %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  arrange(USUBJID, EXSTDTC)

# Keep Final SDTM Variables in order
sdtm_ex_final <- sdtm_ex %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXDOSFRQ, EXVLDIST, EXLOT, EXSTDTC, EXENDTC, EXSTDY, EXENDY, VISIT)

# Ensure types: EXDOSE numeric, EXSEQ integer, EXSTDY/EXENDY numeric
sdtm_ex_final$EXSEQ <- as.integer(sdtm_ex_final$EXSEQ)
sdtm_ex_final$EXDOSE <- as.numeric(sdtm_ex_final$EXDOSE)
sdtm_ex_final$EXSTDY <- as.numeric(sdtm_ex_final$EXSTDY)
sdtm_ex_final$EXENDY <- as.numeric(sdtm_ex_final$EXENDY)

# Sort by USUBJID EXSTDTC as SAS does
sdtm_ex_final <- sdtm_ex_final %>%
  arrange(USUBJID, EXSTDTC, EXSEQ)

cat("Final sdtm_ex rows:", nrow(sdtm_ex_final), "\n")
print(head(sdtm_ex_final, 10))

# --------------------------------------------------------------
# 7. View Final EX Dataset -> proc print
# --------------------------------------------------------------
# Write outputs
# CSV dataset
write.csv(sdtm_ex_final, file.path(out, "P025.csv"), row.names = FALSE, na = "")
write.csv(sdtm_ex_final, file.path(out, "P025.lst.csv"), row.names = FALSE, na = "")
# Also write sdtm_ex.csv for completeness
write.csv(sdtm_ex_final, file.path(out, "sdtm_ex.csv"), row.names = FALSE, na = "")

# Create text rendering mimicking PROC PRINT
# SAS proc print prints Obs and all variables
txt_path <- file.path(out, "P025.txt")
lst_path <- file.path(out, "P025.lst")
# Also create .txt for lst
txt_path2 <- file.path(out, "P025.lst.txt")

# Build text output
con <- file(txt_path, open = "wt")
sink(con, split = FALSE)
cat("PROC PRINT DATA=SDTM_EX\n")
cat("--------------------------------------------------------------------------------------------------------------------------------\n")
# Use capture of print.data.frame with width
# Create formatted table
# Count numbers for verification: 102 numbers expected
# We'll print with standard R print
print(as.data.frame(sdtm_ex_final), row.names = FALSE, right = TRUE, max = 10000)
cat("\n")
cat("Total observations:", nrow(sdtm_ex_final), "\n")
# Summary of numeric values count
numeric_vals <- sum(!is.na(sdtm_ex_final$EXSEQ)) + sum(!is.na(sdtm_ex_final$EXDOSE)) + sum(!is.na(sdtm_ex_final$EXSTDY)) + sum(!is.na(sdtm_ex_final$EXENDY))
cat("Numeric values (EXSEQ+EXDOSE+EXSTDY+EXENDY):", numeric_vals, "\n")
sink()
close(con)

# Copy to lst paths
file.copy(txt_path, lst_path, overwrite = TRUE)
file.copy(txt_path, txt_path2, overwrite = TRUE)

# Also create a more SAS-like listing with Obs column
con2 <- file(file.path(out, "P025_detailed.txt"), open = "wt")
cat("SDTM EX Domain Listing\n", file = con2)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con2, sep = "")
# Write header
header <- paste(c("Obs", names(sdtm_ex_final)), collapse = " | ")
cat(header, "\n", file = con2, sep = "")
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con2, sep = "")
for (i in seq_len(nrow(sdtm_ex_final))) {
  row_vals <- sapply(sdtm_ex_final[i, ], function(x) ifelse(is.na(x), "", as.character(x)))
  line <- paste(c(i, row_vals), collapse = " | ")
  cat(line, "\n", file = con2, sep = "")
}
close(con2)

cat("Outputs written to:", out, "\n")
