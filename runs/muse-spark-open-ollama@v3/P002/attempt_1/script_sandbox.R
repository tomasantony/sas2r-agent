# P002_adae.sas -> R translation
# Produces dataset adae (ADaM ADAE)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (not needed for this program but defined)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
  )
}

# Helper to find file existence with multiple candidates
find_file <- function(candidates) {
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  return(NA_character_)
}

# Locate root
roots <- c("/bench",
           "/tmp/meta_hackathon/sas2r-agent",
           "/bench/data/raw/cdisc-pilot",
           getwd())
# Try to discover actual file locations via listing
# List possible base directories
base_candidates <- c(
  "/bench",
  "/opt/sas2r-agent",
  "/data",
  "."
)

# Search for AE xpt
search_dirs <- c(
  "/bench/data/raw/cdisc-pilot/sdtm",
  "/bench/data/derived/cdisc-pilot/sdtm",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/derived/cdisc-pilot/adam",
  "/tmp",
  "."
)

# Use Sys.glob to find xpt files
all_xpt <- Sys.glob("**/*.xpt")
# Also try absolute
all_xpt2 <- Sys.glob("/bench/**/*.xpt")
all_xpt3 <- Sys.glob("/**/*.xpt")

# For debugging, list OUT_DIR parent
cat("OUT_DIR:", out, "\n")
cat("Working dir:", getwd(), "\n")
cat("List files in current dir:\n")
print(list.files(".", recursive = FALSE))
print(list.files("/bench", recursive = FALSE))

# Define function to read xpt with haven, handling case-insensitive names
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(data.table)

# Locate AE and ADSL xpt files
# Try multiple absolute paths
ae_xpt_candidates <- c(
  "/bench/data/raw/cdisc-pilot/sdtm/ae.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/AE.xpt",
  "/bench/data/derived/cdisc-pilot/sdtm/ae.sas7bdat",
  "/bench/data/derived/cdisc-pilot/sdtm/ae.xpt",
  "./data/raw/cdisc-pilot/sdtm/ae.xpt",
  "./data/derived/cdisc-pilot/sdtm/ae.xpt",
  "/tmp/data/raw/cdisc-pilot/sdtm/ae.xpt"
)
adsl_xpt_candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADSL.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adsl.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/adsl.xpt",
  "./data/raw/cdisc-pilot/adam/adsl.xpt",
  "./data/derived/cdisc-pilot/adam/adsl.xpt"
)

# Also search via list.files if not found
find_xpt <- function(pattern) {
  # search common roots
  roots_to_search <- c(
    "/bench",
    ".",
    "/",
    Sys.getenv("HOME")
  )
  for (r in roots_to_search) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern = pattern, recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}

ae_path <- find_file(ae_xpt_candidates)
if (is.na(ae_path)) ae_path <- find_xpt("^ae\\.xpt$")
cat("AE path:", ae_path, "\n")

adsl_path <- find_file(adsl_xpt_candidates)
if (is.na(adsl_path)) adsl_path <- find_xpt("^adsl\\.xpt$")
cat("ADSL path:", adsl_path, "\n")

# Also check for sas7bdat versions
if (is.na(ae_path) || !file.exists(ae_path)) {
  ae_sas7 <- find_xpt("^ae\\.sas7bdat$")
  if (!is.na(ae_sas7)) ae_path <- ae_sas7
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  adsl_sas7 <- find_xpt("^adsl\\.sas7bdat$")
  if (!is.na(adsl_sas7)) adsl_path <- adsl_sas7
}

stopifnot(!is.na(ae_path), file.exists(ae_path))
stopifnot(!is.na(adsl_path), file.exists(adsl_path))

# Read AE
if (grepl("\\.xpt$", ae_path, ignore.case = TRUE)) {
  ae_raw <- haven::read_xpt(ae_path)
} else {
  ae_raw <- haven::read_sas(ae_path)
}
if (grepl("\\.xpt$", adsl_path, ignore.case = TRUE)) {
  adsl_raw <- haven::read_xpt(adsl_path)
} else {
  adsl_raw <- haven::read_sas(adsl_path)
}

cat("AE cols:", paste(names(ae_raw), collapse=", "), "\n")
cat("ADSL cols:", paste(names(adsl_raw), collapse=", "), "\n")
cat("AE nrow:", nrow(ae_raw), "\n")
cat("ADSL nrow:", nrow(adsl_raw), "\n")

# Normalize column names to uppercase for processing (SAS is case-insensitive)
# Keep original case for output but work with uppercase
names(ae_raw) <- toupper(names(ae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

# SAS program steps:
# proc sort data=sdtmnew.AE OUT=AE; by USUBJID AESEQ; run;
# data AE; set AE; by USUBJID AESEQ; ASTDT=input(strip(AESTDTC),?? yymmdd10.); AENDT=input(strip(AEENDTC),?? yymmdd10.); if ASTDT>.z and AENDT>.z then ADUR=AENDT-ASTDT+1; format ASTDT AENDT DATE9.; run;

# In SAS, yymmdd10. expects YYYY-MM-DD. The ?? suppresses errors, returns missing if invalid.
# So we need to parse AESTDTC and AEENDTC as YYYY-MM-DD strictly.

# Check unique formats of AESTDTC
cat("AESTDTC examples:\n")
print(head(unique(ae_raw$AESTDTC), 20))
cat("AEENDTC examples:\n")
print(head(unique(ae_raw$AEENDTC), 20))

# Helper to parse yymmdd10. strictly: format %Y-%m-%d, with optional? SAS yymmdd10. allows YYYY-MM-DD with dashes.
# Use as.Date with format "%Y-%m-%d", strict: must match exactly 10 chars? SAS ?? will return missing if not.
# We'll implement: trim, then if nchar==10 and grepl("^\\d{4}-\\d{2}-\\d{2}$", x) then parse, else NA.

parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  # SAS input with ?? yymmdd10. will try to parse; if fails returns missing
  # yymmdd10. expects 10 chars, but can handle e.g. "2012-01-15"
  # We'll use strict regex
  res <- rep(NA, length(x))
  # valid pattern: YYYY-MM-DD where YYYY 4 digits, MM 01-12, DD 01-31
  # SAS would also accept YYYY/MM/DD? But spec says yymmdd10. with dashes; we'll allow both - and /? But to be safe, only dash.
  # Check examples: likely all YYYY-MM-DD
  idx <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(idx)) {
    # Use as.Date with format
    res[idx] <- as.Date(x[idx], format = "%Y-%m-%d")
  }
  # Also handle if x is like "2012-1-5" ? SAS yymmdd10. might still parse with single digit? But examples show 10 char.
  # We'll also try to parse any that are not NA but failed, using lubridate? No, SAS would return missing.
  # So keep NA.
  res
}

# Sort AE by USUBJID AESEQ (SAS sort: character ordering, missing first)
# In R, order with same logic: sort by USUBJID, AESEQ
ae <- ae_raw %>%
  arrange(USUBJID, AESEQ)

# Create ASTDT, AENDT, ADUR
ae$ASTDT <- parse_yymmdd10(ae$AESTDTC)
ae$AENDT <- parse_yymmdd10(ae$AEENDTC)
ae$ADUR <- ifelse(!is.na(ae$ASTDT) & !is.na(ae$AENDT), as.integer(ae$AENDT - ae$ASTDT + 1L), NA_integer_)
# Note: SAS ADUR = AENDT - ASTDT +1 where dates are SAS numeric dates (days since 1960-01-01). Same as R Date diff.

# Next: proc sort data=adamnew.ADSL OUT=TEMP(keep=SITEID USUBJID TRT01A TRT01AN AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT RENAME=(TRT01A=TRTA TRT01AN=TRTAN)); by USUBJID; run;

# Check ADSL columns
cat("ADSL TRTSDT examples:\n")
print(head(adsl_raw$TRTSDT, 20))
cat("ADSL TRTSDT class:", class(adsl_raw$TRTSDT), "\n")
# ADSL TRTSDT/TRTEDT are likely numeric SAS dates or Date objects from haven
# haven reads SAS dates as Date or POSIXct? Let's inspect
str(adsl_raw$TRTSDT)
str(adsl_raw$TRTEDT)

# TEMP keep and rename
# Need to ensure columns exist
required_adsl_cols <- c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT")
missing_adsl <- setdiff(required_adsl_cols, names(adsl_raw))
if (length(missing_adsl) > 0) cat("Missing ADSL cols:", paste(missing_adsl, collapse=","), "\n")

temp <- adsl_raw %>%
  select(any_of(required_adsl_cols)) %>%
  arrange(USUBJID) %>%
  rename(TRTA = TRT01A, TRTAN = TRT01AN)

# Ensure TRTSDT/TRTEDT are Date
# haven may have imported as Date already; if numeric, convert from SAS date origin 1960-01-01
convert_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date is days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character: try yymmdd10
  return(parse_yymmdd10(x))
}
temp$TRTSDT <- convert_sas_date(temp$TRTSDT)
temp$TRTEDT <- convert_sas_date(temp$TRTEDT)

# Next: data AE; merge AE(in=inAE) TEMP; by USUBJID; if inAE; ...
# This is left join AE with TEMP on USUBJID, keep all AE rows
# Before merge, drop or rename every column present in both tables except keys, so no .x/.y suffixes appear
# Check overlapping columns
common_cols <- intersect(names(ae), names(temp))
common_cols <- setdiff(common_cols, "USUBJID")
cat("Common cols AE/TEMP:", paste(common_cols, collapse=","), "\n")
# According to SAS, if both have same variable, the second dataset's value overwrites? Actually MERGE with BY will take from rightmost dataset if duplicate.
# But we should drop from TEMP any that already in AE except keys? However TEMP has SITEID which may also be in AE? Let's check AE has SITEID? AE SDTM may have STUDYID, SITEID? Let's see.
# We'll follow instruction: drop or rename every column present in both tables except keys, so no .x/.y suffixes appear
# But we need to decide which to keep. SAS merge: if both have same variable, the value from the last dataset in merge list is retained (TEMP overwrites AE for overlapping non-BY vars).
# However instruction says drop/rename every column present in both except keys before merging. To emulate SAS, we should keep TEMP's version for overlapping? Let's check typical: AE SDTM has USUBJID, AESEQ etc, not SITEID? Actually SDTM AE may not have SITEID; ADSL has SITEID. So overlap may be minimal.
# We'll implement: for common cols, drop from ae (keep temp's) or keep ae's? Need to decide. The SAS code does: merge AE(in=inAE) TEMP; by USUBJID; if inAE;
# If both have SITEID, TEMP's SITEID would overwrite AE's SITEID. But AE SDTM may not have SITEID, so no conflict.
# To be safe, we will drop common cols from ae? But instruction says drop or rename every column present in both except keys, so no .x/.y suffixes appear. We'll drop from temp if already in ae? Let's check which makes sense: AE is primary, TEMP is supplemental. Usually we want TEMP's demographics to be added, not overwrite AE's AE-specific vars. So if overlap is e.g., SITEID, AE may not have it, so no issue. If AE has SITEID, we should keep TEMP's? But both should be same. We'll drop from ae? Let's just drop from temp if duplicate to avoid suffix, but log.
# Actually we will keep both but prioritize TEMP for overlapping non-key vars by removing from ae.
# Simpler: remove common cols from temp? Let's inspect common_cols
print(common_cols)
# If common includes STUDYID, SITEID etc, we need to handle.
# Let's see AE columns
print(names(ae)[1:50])

# For now, we will remove common cols from ae? No, we want TEMP's demographics to be authoritative. So if AE already has SITEID, we should use TEMP's SITEID (from ADSL). So drop from ae.
# But if AE has STUDYID, TEMP doesn't have STUDYID, so not common.
# Let's implement: for each common col, drop from ae (keep temp)
if (length(common_cols) > 0) {
  # Check if any common col is important to keep from AE (like AE-specific)
  # TEMP only has demographics, so overlap likely only SITEID, maybe STUDYID not in TEMP
  # We'll drop from ae to let TEMP win, but keep AE's if TEMP has NA? SAS merge would just take TEMP's value if present, else AE's? Actually SAS merge: if TEMP has missing, it will set to missing, overwriting AE's non-missing. So to emulate, we should just let TEMP overwrite.
  # So we will drop common cols from ae
  ae <- ae %>% select(-any_of(common_cols))
}

# Merge
ae_merged <- merge(ae, temp, by = "USUBJID", all.x = TRUE, sort = FALSE)
# SAS merge with BY requires sorted; result is sorted by BY. We'll sort by USUBJID AESEQ after merge as SAS does later
# But need to ensure inAE filter: keep all AE rows (all.x = TRUE already)

# Now compute derived variables as in SAS data step
# if ASTDT=. and length(AESTDTC)=7 then do; ASTDT=input(compress(AESTDTC||'-01'), yymmdd10.); ASTDTF='D'; end;
# length CQ01NAM $25;
# IF ASTDT>=TRTSDT>.z then ASTDY=ASTDT-TRTSDT+1; ELSE IF TRTSDT>ASTDT>.z then ASTDY=ASTDT-TRTSDT;
# Similarly AENDY
# IF ASTDT>.z and TRTSDT>.z then ANLSTDY=ASTDT-TRTSDT+1;
# IF (ASTDT>=TRTSDT>.z) then TRTEMFL='Y'; ELSE TRTEMFL='N';
# IF index(aedecod,'APPLICATION') or index(aedecod,'DERMATITIS') or index(aedecod,'ERYTHEMA') or index(aedecod,'BLISTER') or (aebodsys='SKIN AND SUBCUTANEOUS TISSUE DISORDERS' and aedecod not in ('COLD SWEAT','HYPERHIDROSIS','ALOPECIA')) then CQ01NAM='DERMATOLOGIC EVENTS';

# Note: SAS condition ASTDT>=TRTSDT>.z means ASTDT >= TRTSDT and TRTSDT not missing (>.z means > missing, i.e., not missing). In SAS, missing is less than any number, so >.z checks not missing. In R, we check !is.na.

# Also need to handle ASTDTF: only set when length(AESTDTC)=7 and ASTDT is missing before. In SAS, length() returns length of character variable without trailing blanks. So check nchar(trimws(AESTDTC)) ==7
# And compress(AESTDTC||'-01') removes blanks then appends '-01', then input with yymmdd10.

# Implement
ae_merged$ASTDTF <- NA_character_
ae_merged$CQ01NAM <- NA_character_

# For each row, apply logic
# Need to handle vectorized

# First, handle ASTDT imputation for length 7
# SAS: if ASTDT=. and length(AESTDTC)=7 then do; ASTDT=input(compress(AESTDTC||'-01'), yymmdd10.); ASTDTF='D'; end;
# Note: compress removes blanks, but AESTDTC is like "2012-01" ? length 7 would be "YYYY-MM" ? Let's check examples of length 7
cat("AESTDTC length 7 examples:\n")
print(unique(ae_merged$AESTDTC[nchar(trimws(as.character(ae_merged$AESTDTC)))==7]))

# Implement
idx_impute <- is.na(ae_merged$ASTDT) & nchar(trimws(as.character(ae_merged$AESTDTC))) == 7 & !is.na(ae_merged$AESTDTC)
if (any(idx_impute, na.rm=TRUE)) {
  # compress(AESTDTC||'-01') => remove blanks and append -01
  # For AESTDTC like "2012-01", compress gives "2012-01", then "2012-01-01"
  # For "2012/01" maybe? But we assume dash
  to_parse <- paste0(gsub(" ", "", trimws(as.character(ae_merged$AESTDTC[idx_impute]))), "-01")
  # parse with yymmdd10
  parsed <- parse_yymmdd10(to_parse)
  ae_merged$ASTDT[idx_impute] <- parsed
  ae_merged$ASTDTF[idx_impute] <- "D"
}

# Now compute ASTDY, AENDY, ANLSTDY, TRTEMFL, CQ01NAM
# Initialize
ae_merged$ASTDY <- NA_integer_
ae_merged$AENDY <- NA_integer_
ae_merged$ANLSTDY <- NA_integer_
ae_merged$TRTEMFL <- NA_character_

# ASTDY: IF ASTDT>=TRTSDT>.z then ASTDY=ASTDT-TRTSDT+1; ELSE IF TRTSDT>ASTDT>.z then ASTDY=ASTDT-TRTSDT;
# This is: if both not missing and ASTDT >= TRTSDT then ASTDT-TRTSDT+1 else if both not missing and TRTSDT > ASTDT then ASTDT-TRTSDT (negative)
# Note: SAS missing check: TRTSDT>.z means not missing, ASTDT>.z means not missing
# So we need both not NA
idx_both_ast <- !is.na(ae_merged$ASTDT) & !is.na(ae_merged$TRTSDT)
idx_ge <- idx_both_ast & ae_merged$ASTDT >= ae_merged$TRTSDT
idx_lt <- idx_both_ast & ae_merged$TRTSDT > ae_merged$ASTDT
ae_merged$ASTDY[idx_ge] <- as.integer(ae_merged$ASTDT[idx_ge] - ae_merged$TRTSDT[idx_ge] + 1L)
ae_merged$ASTDY[idx_lt] <- as.integer(ae_merged$ASTDT[idx_lt] - ae_merged$TRTSDT[idx_lt])

# AENDY similarly
idx_both_aend <- !is.na(ae_merged$AENDT) & !is.na(ae_merged$TRTSDT)
idx_ge2 <- idx_both_aend & ae_merged$AENDT >= ae_merged$TRTSDT
idx_lt2 <- idx_both_aend & ae_merged$TRTSDT > ae_merged$AENDT
ae_merged$AENDY[idx_ge2] <- as.integer(ae_merged$AENDT[idx_ge2] - ae_merged$TRTSDT[idx_ge2] + 1L)
ae_merged$AENDY[idx_lt2] <- as.integer(ae_merged$AENDT[idx_lt2] - ae_merged$TRTSDT[idx_lt2])

# ANLSTDY: IF ASTDT>.z and TRTSDT>.z then ANLSTDY=ASTDT-TRTSDT+1;
ae_merged$ANLSTDY[idx_both_ast] <- as.integer(ae_merged$ASTDT[idx_both_ast] - ae_merged$TRTSDT[idx_both_ast] + 1L)

# TRTEMFL: IF (ASTDT>=TRTSDT>.z) then TRTEMFL='Y'; ELSE TRTEMFL='N';
# This means if ASTDT >= TRTSDT and TRTSDT not missing (and ASTDT not missing implicitly because >= with missing would be false? In SAS, missing < any number, so ASTDT missing would be < TRTSDT, so false)
# So condition is idx_ge (ASTDT >= TRTSDT and both not missing)
ae_merged$TRTEMFL <- ifelse(idx_ge, "Y", "N")
# Note: if ASTDT is NA, idx_ge is FALSE, so N. Matches SAS else.

# CQ01NAM logic
# IF index(aedecod,'APPLICATION') or index(aedecod,'DERMATITIS') or index(aedecod,'ERYTHEMA') or index(aedecod,'BLISTER') or (aebodsys='SKIN AND SUBCUTANEOUS TISSUE DISORDERS' and aedecod not in ('COLD SWEAT','HYPERHIDROSIS','ALOPECIA')) then CQ01NAM='DERMATOLOGIC EVENTS';
# SAS index is case-sensitive, substring search. aedecod and aebodsys are character.
# Need to handle NA
# Also note: aedecod not in (...) means not equal to any of those three
ae_merged$CQ01NAM <- NA_character_
# Create logical vectors
# Ensure aedecod and aebodsys are character
ae_merged$AEDECOD <- as.character(ae_merged$AEDECOD)
ae_merged$AEBODSYS <- as.character(ae_merged$AEBODSYS)

has_app <- grepl("APPLICATION", ae_merged$AEDECOD, fixed = TRUE)
has_derm <- grepl("DERMATITIS", ae_merged$AEDECOD, fixed = TRUE)
has_ery <- grepl("ERYTHEMA", ae_merged$AEDECOD, fixed = TRUE)
has_blister <- grepl("BLISTER", ae_merged$AEDECOD, fixed = TRUE)
# Handle NA: grepl returns NA for NA input, need to replace NA with FALSE
has_app[is.na(has_app)] <- FALSE
has_derm[is.na(has_derm)] <- FALSE
has_ery[is.na(has_ery)] <- FALSE
has_blister[is.na(has_blister)] <- FALSE

is_skin <- ae_merged$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
is_skin[is.na(is_skin)] <- FALSE
not_in_list <- !(ae_merged$AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA"))
not_in_list[is.na(not_in_list)] <- FALSE # if AEDECOD is NA, then not in list is TRUE? But SAS: aedecod not in (...) when aedecod is missing, it would be true? But then condition requires aebodsys = skin and aedecod not in list. If aedecod is missing, it would be considered not in list, but then CQ01NAM would be set. However AEDECOD missing is unlikely. We'll treat NA as FALSE to be safe, but check SAS behavior: missing character is "" which is not in list, so would be true. But we have no missing AEDECOD likely.
# Actually for NA, we should treat as TRUE for not_in_list? Let's see: SAS missing character is "" (blank), which is not equal to those strings, so not in would be true. So if AEDECOD is NA/blank, not_in_list should be TRUE.
# We'll set NA to TRUE
not_in_list2 <- !(ae_merged$AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA"))
not_in_list2[is.na(ae_merged$AEDECOD)] <- TRUE

cond_cq <- has_app | has_derm | has_ery | has_blister | (is_skin & not_in_list2)
ae_merged$CQ01NAM[cond_cq] <- "DERMATOLOGIC EVENTS"

# Now implement flagfrst macro
# %macro flagfrst(inds=,sortvars=, invar=,outvar=,outds=, ser=, derm=);
# proc sort data=&inds out=ds01; by usubjid &sortvars ASTDT; where trtemfl='Y' and &invar ne '' and aeser='Y' and CQ01NAM='DERMATOLOGIC EVENTS'; run;
# data ds02(keep=usubjid &outvar aeseq); set ds01; by usubjid &sortvars ASTDT; if first.usubjid or first.&invar then do; &outvar='Y'; output; end; run;
# proc sort data=ds02 out=&outds; by usubjid aeseq; run;

# Note: SAS by processing: first.usubjid etc. For sortvars empty, it's just by usubjid ASTDT, and first.usubjid triggers.
# For sortvars=aebodsys, invar=aebodsys, then by usubjid aebodsys ASTDT, and first.aebodsys triggers (which is first within usubjid+aebodsys group)
# For sortvars=aebodsys aedecod, invar=aedecod, then by usubjid aebodsys aedecod ASTDT, first.aedecod triggers.

# We need to replicate exactly, including handling of missing ASTDT sorting first (SAS missing sorts first)
# Also need to handle where conditions

flagfrst <- function(inds, sortvars, invar, outvar, ser = NULL, derm = NULL) {
  # inds is data frame
  # sortvars is character vector or empty
  # invar is string or "" 
  # ser is "Y" or NULL
  # derm is "Y" or NULL
  df <- inds
  # where trtemfl='Y'
  df <- df %>% filter(TRTEMFL == "Y")
  if (!is.null(invar) && invar != "" && invar != " ") {
    # &invar ne ''  => invar not missing/blank
    # Need to check column exists
    col <- df[[invar]]
    # SAS: invar ne '' means not blank (after trimming? SAS character comparison: '' is blank, so any non-blank qualifies)
    # We'll treat NA or "" or all spaces as blank
    df <- df %>% filter(!is.na(.data[[invar]]) & trimws(.data[[invar]]) != "")
  }
  if (!is.null(ser) && ser == "Y") {
    df <- df %>% filter(AESER == "Y")
  }
  if (!is.null(derm) && derm == "Y") {
    df <- df %>% filter(CQ01NAM == "DERMATOLOGIC EVENTS")
  }
  # sort by usubjid &sortvars ASTDT
  # SAS sort: character ordering uppercase before lowercase, missing first
  # R arrange will handle missing first by default? Need to ensure NA first.
  # For character, R sorts with locale; but SAS sorts with ASCII (uppercase before lowercase). R's default may be similar but not guaranteed.
  # We'll use arrange with appropriate handling: for character, we can use xtfrm? But we will just use arrange which is close.
  # For ASTDT, missing first: arrange puts NA first by default? In dplyr, NA are last? Actually dplyr arrange puts NA last? Need to check. SAS puts missing first. We need to emulate.
  # We can create a helper: arrange with NA first by using is.na
  # We'll sort manually using order with na.last = FALSE
  # But we have multiple sort keys, need to handle.
  # Simpler: use dplyr arrange and then handle NA for ASTDT by sorting with is.na descending? Let's test.
  # We'll implement custom ordering using base order
  
  # Build order vectors
  # sortvars may be like c("AEBODSYS") or c("AEBODSYS","AEDECOD")
  sort_keys <- c("USUBJID", sortvars, "ASTDT")
  # Remove empty strings
  sort_keys <- sort_keys[sort_keys != "" & !is.na(sort_keys)]
  
  # For ordering, we need to handle character vs Date
  # Use base order with na.last = FALSE and method = "radix" for character? But radix may not handle Date.
  # We'll use dplyr arrange with .na_last = FALSE if available? dplyr arrange doesn't have na_last param, but we can use arrange with is.na
  # Approach: create temporary columns for sorting that put NA first
  # For each sort key, we can create a flag is.na and sort by that first, then by value
  # But SAS sorts missing first for all types, so we need to ensure NA first.
  # We'll use data.table setorder? Simpler: use base order
  
  # Prepare list of vectors for order
  order_list <- list()
  for (k in sort_keys) {
    vec <- df[[k]]
    # For character, SAS sorts uppercase before lowercase (ASCII). R's order with method="radix" does similar? Let's use base order which is locale-dependent but likely similar.
    # We'll just use vec as is, but need to handle NA
    order_list[[k]] <- vec
  }
  # Use do.call(order, c(order_list, list(na.last = FALSE)))
  # However order with multiple vectors and na.last=FALSE will put NA first for each
  ord <- do.call(order, c(order_list, list(na.last = FALSE, method = "radix")))
  ds01 <- df[ord, , drop = FALSE]
  
  # Now by processing: by usubjid &sortvars ASTDT; if first.usubjid or first.&invar then output
  # Need to identify first occurrence per group
  # For invar = "" (empty), condition is first.usubjid
  # For invar = "AEBODSYS", condition is first.AEBODSYS (which is first within USUBJID + AEBODSYS)
  # For invar = "AEDECOD", condition is first.AEDECOD (which is first within USUBJID + AEBODSYS + AEDECOD)
  # Note: SAS BY statement includes USUBJID, sortvars, ASTDT. So groups are defined by all BY variables.
  # first.usubjid is true when USUBJID changes (first row for that subject)
  # first.AEBODSYS is true when AEBODSYS changes within USUBJID (i.e., first row for that SOC within subject)
  # first.AEDECOD is true when AEDECOD changes within USUBJID+AEBODSYS (i.e., first row for that PT within SOC within subject)
  # But the code uses: if %if &invar= %then first.usubjid; %else first.&invar;
  # So for invar empty, it's first.usubjid; for invar=AEBODSYS, it's first.AEBODSYS; for invar=AEDECOD, it's first.AEDECOD
  
  # We need to compute these flags
  # For each row, determine if it's first in its group
  # We can use dplyr group_by and row_number
  
  if (is.null(invar) || invar == "" || trimws(invar) == "") {
    # first.usubjid
    # Need to group by USUBJID and take first row per USUBJID after sorting by USUBJID, ASTDT
    # But note sort includes ASTDT, so first row per USUBJID is earliest ASTDT
    ds02 <- ds01 %>%
      group_by(USUBJID) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(!!outvar := "Y") %>%
      select(USUBJID, AESEQ, all_of(outvar))
  } else {
    # first.&invar
    # Group by USUBJID and invar? But need to consider sortvars as well
    # For invar=AEBODSYS, sortvars=AEBODSYS, so BY is USUBJID AEBODSYS ASTDT, so first.AEBODSYS is first within USUBJID+AEBODSYS
    # For invar=AEDECOD, sortvars=AEBODSYS AEDECOD, BY is USUBJID AEBODSYS AEDECOD ASTDT, so first.AEDECOD is first within USUBJID+AEBODSYS+AEDECOD
    # However the code's logic for invar=AEDECOD uses first.AEDECOD, which is equivalent to grouping by USUBJID, AEBODSYS, AEDECOD
    # But what about invar=AEBODSYS with sortvars=AEBODSYS? Group by USUBJID, AEBODSYS
    # So we need to group by USUBJID + invar? But for AEDECOD case, need also AEBODSYS? Let's check: sortvars includes AEBODSYS and AEDECOD, invar is AEDECOD, so grouping by USUBJID, AEBODSYS, AEDECOD is more specific than just USUBJID+AEDECOD. However AEDECOD is unique within AEBODSYS? Not necessarily, but typically PT is within SOC, but could be same PT in different SOC? In MedDRA, PT is unique, but to be safe we should group by USUBJID + sortvars (which includes AEBODSYS for that case)
    # Actually SAS first.AEDECOD when BY is USUBJID AEBODSYS AEDECOD ASTDT means first.AEDECOD is true when AEDECOD changes, but it also implies USUBJID and AEBODSYS are same? No, first.AEDECOD is true if either USUBJID or AEBODSYS or AEDECOD changes. So it's effectively grouping by USUBJID, AEBODSYS, AEDECOD.
    # So we should group by USUBJID + sortvars (which includes invar)
    group_vars <- c("USUBJID", sortvars)
    # Ensure invar is in group_vars (it should be, as sortvars includes invar)
    # For safety, include invar if not already
    if (!(invar %in% group_vars)) group_vars <- c(group_vars, invar)
    ds02 <- ds01 %>%
      group_by(across(all_of(group_vars))) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(!!outvar := "Y") %>%
      select(USUBJID, AESEQ, all_of(outvar))
  }
  
  # proc sort data=ds02 out=&outds; by usubjid aeseq; run;
  ds02 <- ds02 %>% arrange(USUBJID, AESEQ)
  return(ds02)
}

# Create flags
# %flagfrst(inds=ae,sortvars=,invar=,outvar=AOCCFL,outds=aea);
# %flagfrst(inds=ae,sortvars=aebodsys,invar=aebodsys,outvar=AOCCSFL,outds=aeb);
# %flagfrst(inds=ae,sortvars=aebodsys aedecod,invar=aedecod,outvar=AOCCPFL,outds=aec);
# %flagfrst(inds=ae,sortvars=,invar=,outvar=AOCC02FL,outds=saea, ser=Y);
# %flagfrst(inds=ae,sortvars=aebodsys,invar=aebodsys,outvar=AOCC03FL,outds=saeb,ser=Y);
# %flagfrst(inds=ae,sortvars=aebodsys aedecod,invar=aedecod,outvar=AOCC04FL,outds=saec,ser=Y);
# %flagfrst(inds=ae,sortvars=,invar=,outvar=AOCC01FL,outds=derm, ser=,derm=Y);

aea <- flagfrst(inds = ae_merged, sortvars = c(), invar = "", outvar = "AOCCFL")
aeb <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCCSFL")
aec <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL")
saea <- flagfrst(inds = ae_merged, sortvars = c(), invar = "", outvar = "AOCC02FL", ser = "Y")
saeb <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCC03FL", ser = "Y")
saec <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = "Y")
derm <- flagfrst(inds = ae_merged, sortvars = c(), invar = "", outvar = "AOCC01FL", derm = "Y")

# proc sort data=AE; by usubjid aeseq; run;
# data AE; merge AE AEA AEB AEC SAEA SAEB SAEC DERM; by usubjid aeseq; ...

# Need to merge all flag datasets onto ae_merged
# Before merging, ensure no duplicate columns except keys
# Each flag dataset has USUBJID, AESEQ, and one flag var

ae_merged <- ae_merged %>% arrange(USUBJID, AESEQ)

# Merge sequentially, using left_join
ae_final <- ae_merged %>%
  left_join(aea, by = c("USUBJID","AESEQ")) %>%
  left_join(aeb, by = c("USUBJID","AESEQ")) %>%
  left_join(aec, by = c("USUBJID","AESEQ")) %>%
  left_join(saea, by = c("USUBJID","AESEQ")) %>%
  left_join(saeb, by = c("USUBJID","AESEQ")) %>%
  left_join(saec, by = c("USUBJID","AESEQ")) %>%
  left_join(derm, by = c("USUBJID","AESEQ"))

# Check that columns exist
stopifnot("TRTEMFL" %in% names(ae_final))
stopifnot("AOCCFL" %in% names(ae_final))

# Now retain and keep ordering as in SAS:
# retain STUDYID SITEID USUBJID TRTA TRTAN AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT 
# ASTDT ASTDTF ASTDY AENDT AENDY ADUR AETERM AELLT AELLTCD AEDECOD AEPTCD AEHLT AEHLTCD AEHLGT AEHLGTCD AEBODSYS AESOC AESOCCD AESEV AESER AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD AEREL AEACN AEOUT AESEQ TRTEMFL AOCCFL AOCCSFL AOCCPFL AOCC02FL AOCC03FL AOCC04FL CQ01NAM AOCC01FL ;
# set AE(keep=STUDYID SITEID USUBJID TRTAN TRTA AESEQ AETERM AELLT: AEDECOD AEPTCD AEHL: AEBODSYS AESOC: AESEV AESER AEACN AEREL AEOUT AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD TRTEMFL ASTDT AENDT ASTDTF CQ01NAM ASTDY AENDY AOCCFL AOCCSFL AOCCPFL AOCC01FL AOCC02FL AOCC03FL AOCC04FL AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT ADUR);
# Note: AELLT: means all vars starting with AELLT, AEHL: means AEHL*, AESOC: means AESOC*
# We need to replicate keep list exactly, but also ensure we have all those columns if they exist in source

# List all columns in ae_final
cat("ae_final cols:", paste(names(ae_final), collapse=", "), "\n")

# Define desired order as per retain statement
desired_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT",
                   "ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM",
                   "AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN",
                   "AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")

# But SAS keep list includes: STUDYID SITEID USUBJID TRTAN TRTA AESEQ AETERM AELLT: AEDECOD AEPTCD AEHL: AEBODSYS AESOC: AESEV AESER AEACN AEREL AEOUT AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD TRTEMFL ASTDT AENDT ASTDTF CQ01NAM ASTDY AENDY AOCCFL AOCCSFL AOCCPFL AOCC01FL AOCC02FL AOCC03FL AOCC04FL AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT ADUR
# The retain order is slightly different (TRTA before TRTAN, etc). We'll use retain order for final output, but ensure we only keep columns that exist.

# Check which desired columns exist in ae_final
existing_desired <- desired_order[desired_order %in% names(ae_final)]
missing_desired <- desired_order[!desired_order %in% names(ae_final)]
cat("Missing desired cols:", paste(missing_desired, collapse=","), "\n")

# Also need to handle AELLT: etc. Let's see what AE columns start with those prefixes
# List all AE columns that match AELLT*, AEHL*, AESOC*
aellt_cols <- grep("^AELLT", names(ae_final), value = TRUE)
aehl_cols <- grep("^AEHL", names(ae_final), value = TRUE)
aesoc_cols <- grep("^AESOC", names(ae_final), value = TRUE)
cat("AELLT cols:", paste(aellt_cols, collapse=","), "\n")
cat("AEHL cols:", paste(aehl_cols, collapse=","), "\n")
cat("AESOC cols:", paste(aesoc_cols, collapse=","), "\n")

# The retain list explicitly includes AELLT, AELLTCD, AEHLT, AEHLTCD, AEHLGT, AEHLGTCD, AESOC, AESOCCD
# So we will use those exact names if they exist

# Create final AE dataset with retain order, but only existing columns
# Also need to ensure we keep all columns that are in retain list, in that order, plus any extra? SAS keep list is restrictive, but retain list is the order for those kept.
# We'll follow retain order for output, but if some columns are missing in source, they will be NA

# For columns that are in desired_order but not in ae_final, create them as NA
for (col in missing_desired) {
  ae_final[[col]] <- NA
}

# Now select and order
ae_ordered <- ae_final %>% select(all_of(desired_order))

# Sort by USUBJID AESEQ as final SAS does: data adamnew.ADAE; set AE; by USUBJID AESEQ;
ae_ordered <- ae_ordered %>% arrange(USUBJID, AESEQ)

# Format TRTSDT TRTEDT ASTDT AENDT as DATE9. (SAS format DDMMMYYYY like 01JAN2012)
# For CSV output, we need to write dates as YYYY-MM-DD per instructions: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# So we will keep them as Date objects and write as ISO

# Also need to handle labels? Not needed for CSV, but we can keep.

# Check against expected ADAE from adamnew.ADAE xpt for validation
adae_path_candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adae.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adae.xpt",
  "./data/raw/cdisc-pilot/adam/adae.xpt",
  "./data/derived/cdisc-pilot/adam/adae.xpt"
)
adae_path <- find_file(adae_path_candidates)
if (is.na(adae_path)) adae_path <- find_xpt("^adae\\.xpt$")
cat("ADAE path:", adae_path, "\n")
if (!is.na(adae_path) && file.exists(adae_path)) {
  adae_expected <- haven::read_xpt(adae_path)
  names(adae_expected) <- toupper(names(adae_expected))
  cat("Expected ADAE nrow:", nrow(adae_expected), "ncol:", ncol(adae_expected), "\n")
  cat("Expected cols:", paste(names(adae_expected), collapse=","), "\n")
  # Compare row counts
  cat("Our nrow:", nrow(ae_ordered), "\n")
  # Check some key variables
  # Compare TRTEMFL counts
  print(table(ae_ordered$TRTEMFL, useNA="always"))
  print(table(adae_expected$TRTEMFL, useNA="always"))
  # Compare AOCCFL
  print(table(ae_ordered$AOCCFL, useNA="always"))
  print(table(adae_expected$AOCCFL, useNA="always"))
}

# Prepare for CSV output: convert Date columns to character YYYY-MM-DD
# Identify Date columns
date_cols <- c("TRTSDT","TRTEDT","ASTDT","AENDT")
for (dc in date_cols) {
  if (dc %in% names(ae_ordered)) {
    # Convert Date to character ISO, NA as ""
    ae_ordered[[dc]] <- as.character(ae_ordered[[dc]]) # as.character.Date gives YYYY-MM-DD
    # Ensure NA becomes NA (will be written as "" with na="")
  }
}

# Also need to ensure numeric columns are not rounded
# Write CSV
output_path <- file.path(out, "adae.csv")
write.csv(ae_ordered, output_path, row.names = FALSE, na = "")

cat("Written ADAE to", output_path, "rows:", nrow(ae_ordered), "\n")

# Also create a txt rendering of contents (proc contents varnum)
# SAS proc contents varnum prints variables in order of creation (varnum)
# We'll create a simple text file listing variables with labels
# Labels from SAS code:
labels <- c(
  TRTEMFL = "Treatment Emergent Analysis Flag",
  AOCCFL = "1st Occurrence of Any AE Flag",
  AOCCSFL = "1st Occurrence of SOC Flag",
  AOCCPFL = "1st Occurrence of Preferred Term Flag",
  ASTDT = "Analysis Start Date",
  ASTDTF = "Analysis Start Date Imputation Flag",
  ASTDY = "Analysis Start Relative Day",
  AENDT = "Analysis End Date",
  AENDY = "Analysis End Relative Day",
  TRTA = "Actual Treatment",
  TRTAN = "Actual Treatment (N)",
  CQ01NAM = "Customized Query 01 Name",
  AOCC02FL = "1st Occurrence 02 Flag for Serious",
  AOCC03FL = "1st Occurrence 03 Flag for Serious SOC",
  AOCC04FL = "1st Occurrence 04 Flag for Serious PT",
  AOCC01FL = "1st Occurrence 01 Flag for CQ01",
  ADUR = "AE Duration (days)"
)

# Create contents txt
contents_txt <- file.path(out, "adae.txt")
sink(contents_txt)
cat("CONTENTS OF ADAE (Adverse Events Analysis Dataset)\n")
cat("Variables in creation order (VARNUM):\n")
cat(sprintf("%-3s %-12s %-40s %s\n", "#", "Variable", "Label", "Type"))
for (i in seq_along(names(ae_ordered))) {
  var <- names(ae_ordered)[i]
  lab <- ifelse(var %in% names(labels), labels[var], "")
  typ <- class(ae_ordered[[var]])[1]
  # Determine type: character vs numeric vs Date (now character)
  cat(sprintf("%-3d %-12s %-40s %s\n", i, var, lab, typ))
}
cat("\n")
cat("Number of observations:", nrow(ae_ordered), "\n")
cat("Number of variables:", ncol(ae_ordered), "\n")
sink()

# Also ensure we handle any remaining SAS specifics: AESEQ is numeric, ensure integer
# Check that output matches expected column order and types
# For validation, compare our output to expected if available
if (exists("adae_expected")) {
  # Align columns
  common <- intersect(names(ae_ordered), names(adae_expected))
  # Check differences in key flags
  # Use all.equal for a few columns
  for (col in c("TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC01FL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","ASTDTF")) {
    if (col %in% common) {
      # Compare after sorting
      exp_sorted <- adae_expected %>% arrange(USUBJID, AESEQ)
      our_sorted <- ae_ordered %>% arrange(USUBJID, AESEQ)
      # Convert both to character for comparison (handle NA)
      exp_vec <- as.character(exp_sorted[[col]])
      our_vec <- as.character(our_sorted[[col]])
      # Replace NA with ""
      exp_vec[is.na(exp_vec)] <- ""
      our_vec[is.na(our_vec)] <- ""
      mism <- sum(exp_vec != our_vec, na.rm=TRUE)
      cat(sprintf("Column %s mismatches: %d / %d\n", col, mism, length(exp_vec)))
      if (mism > 0) {
        idx <- which(exp_vec != our_vec)[1:min(5, mism)]
        print(data.frame(USUBJID=exp_sorted$USUBJID[idx], AESEQ=exp_sorted$AESEQ[idx], expected=exp_vec[idx], ours=our_vec[idx]))
      }
    }
  }
  # Check ASTDT, AENDT, ASTDY, AENDY, ADUR
  for (col in c("ASTDT","AENDT","ASTDY","AENDY","ADUR")) {
    if (col %in% common) {
      exp_sorted <- adae_expected %>% arrange(USUBJID, AESEQ)
      our_sorted <- ae_ordered %>% arrange(USUBJID, AESEQ)
      # For dates, expected may be Date or numeric SAS date; convert to character ISO
      exp_vec <- exp_sorted[[col]]
      if (inherits(exp_vec, "Date") || inherits(exp_vec, "POSIXct")) {
        exp_vec <- as.character(as.Date(exp_vec))
      } else if (is.numeric(exp_vec)) {
        # SAS numeric date
        exp_vec <- as.character(as.Date(exp_vec, origin="1960-01-01"))
      } else {
        exp_vec <- as.character(exp_vec)
      }
      our_vec <- as.character(our_sorted[[col]])
      exp_vec[is.na(exp_vec) | exp_vec=="NA"] <- ""
      our_vec[is.na(our_vec) | our_vec=="NA"] <- ""
      mism <- sum(exp_vec != our_vec, na.rm=TRUE)
      cat(sprintf("Column %s mismatches: %d\n", col, mism))
      if (mism>0) {
        idx <- which(exp_vec != our_vec)[1:min(5, mism)]
        print(data.frame(USUBJID=exp_sorted$USUBJID[idx], AESEQ=exp_sorted$AESEQ[idx], expected=exp_vec[idx], ours=our_vec[idx]))
      }
    }
  }
}

cat("Done\n")
