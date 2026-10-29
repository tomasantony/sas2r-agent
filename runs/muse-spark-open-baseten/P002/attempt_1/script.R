# P002_adae.sas -> R translation
# Produces dataset adae.csv under OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(purrr)
  library(tibble)
})

# helper: SAS round half away from zero (not needed here but included)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file trying multiple roots
find_xpt <- function(rel_win_path) {
  # rel_win_path like "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/ae.xpt"
  candidates <- c(
    rel_win_path,
    sub("^D:", "", rel_win_path),
    sub("^D:/meta_hackathon/sas2r-agent", "/meta_hackathon/sas2r-agent", rel_win_path),
    sub("^D:/meta_hackathon/sas2r-agent", ".", rel_win_path),
    file.path(".", sub("^D:/meta_hackathon/sas2r-agent/", "", rel_win_path)),
    file.path(getwd(), sub("^D:/meta_hackathon/sas2r-agent/", "", rel_win_path))
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  # also search recursively for basename
  bn <- basename(rel_win_path)
  # try common locations
  search_roots <- c(".", "/meta_hackathon", "/tmp", getwd())
  for (r in search_roots) {
    if (dir.exists(r)) {
      found <- list.files(r, pattern = paste0("^", bn, "$"), recursive = TRUE, full.names = TRUE)
      if (length(found) > 0) return(found[1])
    }
  }
  return(rel_win_path)
}

read_xpt_safe <- function(path) {
  p <- find_xpt(path)
  if (!file.exists(p)) {
    stop(paste0("File not found: ", path, " (tried ", p, ")"))
  }
  df <- haven::read_xpt(p)
  # zap labels/formats but keep values
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  # convert labelled to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # make names uppercase to mimic SAS (SAS is case-insensitive)
  # keep original case but ensure we can access by uppercase
  # We'll keep as is but also create uppercase mapping
  names(df) <- toupper(names(df))
  df
}

# SAS yymmdd10. input with ?? (returns NA on failure)
sas_yymmdd10 <- function(x) {
  # x is character vector, trimmed
  # SAS yymmdd10. expects YYYY-MM-DD with dashes, also allows YYYY/MM/DD?
  # Here data uses YYYY-MM-DD
  x <- trimws(as.character(x))
  # if NA or empty -> NA
  res <- rep(as.Date(NA), length(x))
  # valid full date pattern: 4 digits - 2 digits - 2 digits
  # SAS would also accept without dashes? but pilot uses dashes
  idx <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(idx)) {
    # use as.Date, invalid dates become NA (e.g., 2006-02-30)
    res[idx] <- suppressWarnings(as.Date(x[idx], format = "%Y-%m-%d"))
  }
  res
}

# ------------------------------------------------------------------
# Load inputs
# sdtmnew.AE from raw sdtm ae.xpt
# adamnew.ADSL from raw adam adsl.xpt
ae_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/ae.xpt"
adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"

AE_raw <- read_xpt_safe(ae_path)
ADSL_raw <- read_xpt_safe(adsl_path)

# Ensure required columns exist (uppercase)
# AE expected: USUBJID, AESEQ, AESTDTC, AEENDTC, AEDECOD, AEBODSYS, etc.
# ADSL expected: SITEID, USUBJID, TRT01A, TRT01AN, AGE, AGEGR1, AGEGR1N, RACE, RACEN, SEX, SAFFL, TRTSDT, TRTEDT

# Convert SAS numeric dates in ADSL to Date if needed
# haven may already convert to Date; if numeric, convert from SAS origin 1960-01-01
convert_sas_date <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (is.numeric(col)) {
    # SAS date is days since 1960-01-01
    # haven usually converts automatically, but if not, do it
    # Heuristic: if values are > 10000 and < 30000, likely SAS date
    # We'll convert numeric to Date via as.Date with origin 1960-01-01
    # But if already Date-like numeric, this is correct
    # Check if any non-NA and not already Date
    return(as.Date(col, origin = "1960-01-01"))
  }
  # character -> try yymmdd10
  if (is.character(col)) {
    return(sas_yymmdd10(col))
  }
  col
}

# ADSL TRTSDT TRTEDT
if ("TRTSDT" %in% names(ADSL_raw)) ADSL_raw$TRTSDT <- convert_sas_date(ADSL_raw$TRTSDT)
if ("TRTEDT" %in% names(ADSL_raw)) ADSL_raw$TRTEDT <- convert_sas_date(ADSL_raw$TRTEDT)

# ------------------------------------------------------------------
# Step 1: proc sort data=sdtmnew.AE OUT=AE; by USUBJID AESEQ;
AE <- AE_raw %>%
  arrange(USUBJID, AESEQ)

# data AE; set AE; by USUBJID AESEQ; ASTDT=input(strip(AESTDTC),?? yymmdd10.); AENDT=input(strip(AEENDTC),?? yymmdd10.); if ASTDT>.z and AENDT>.z then ADUR=AENDT-ASTDT+1; format ASTDT AENDT DATE9.;
AE$ASTDT <- sas_yymmdd10(trimws(AE$AESTDTC))
AE$AENDT <- sas_yymmdd10(trimws(AE$AEENDTC))
AE$ADUR <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$AENDT),
                  as.numeric(AE$AENDT - AE$ASTDT) + 1,
                  NA_real_)

# ------------------------------------------------------------------
# proc sort data=adamnew.ADSL OUT=TEMP(keep=... RENAME=(TRT01A=TRTA TRT01AN=TRTAN)); by USUBJID;
TEMP <- ADSL_raw %>%
  select(any_of(c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT"))) %>%
  rename(TRTA = TRT01A, TRTAN = TRT01AN) %>%
  arrange(USUBJID)

# data AE; merge AE(in=inAE) TEMP; by USUBJID; if inAE;
# left join AE with TEMP by USUBJID
AE <- AE %>%
  left_join(TEMP, by = "USUBJID")

# if ASTDT=. and length(AESTDTC)=7 then do; ASTDT=input(compress(AESTDTC||'-01'), yymmdd10.); ASTDTF='D'; end;
# length CQ01NAM $25;
# compute ASTDY, AENDY, ANLSTDY, TRTEMFL, CQ01NAM

# Initialize ASTDTF and CQ01NAM
AE$ASTDTF <- NA_character_
AE$CQ01NAM <- NA_character_

# Imputation for partial date length 7 (e.g., "2006-01")
# SAS: length(AESTDTC)=7 ; compress(AESTDTC||'-01') removes blanks then appends -01
# So "2006-01" -> "2006-01-01"
idx_imp <- is.na(AE$ASTDT) & !is.na(AE$AESTDTC) & nchar(trimws(AE$AESTDTC)) == 7
if (any(idx_imp, na.rm = TRUE)) {
  to_parse <- paste0(gsub(" ", "", trimws(AE$AESTDTC[idx_imp])), "-01")
  # compress removes blanks, already done
  AE$ASTDT[idx_imp] <- sas_yymmdd10(to_parse)
  AE$ASTDTF[idx_imp] <- "D"
}

# ASTDY, AENDY, ANLSTDY, TRTEMFL
# IF ASTDT>=TRTSDT>.z then ASTDY=ASTDT-TRTSDT+1; ELSE IF TRTSDT>ASTDT>.z then ASTDY=ASTDT-TRTSDT;
AE$ASTDY <- NA_real_
AE$AENDY <- NA_real_
AE$ANLSTDY <- NA_real_

# Helper to compute relative day
# Both dates must be non-NA
valid_ast_trt <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT)
AE$ASTDY[valid_ast_trt & AE$ASTDT >= AE$TRTSDT] <- as.numeric(AE$ASTDT[valid_ast_trt & AE$ASTDT >= AE$TRTSDT] - AE$TRTSDT[valid_ast_trt & AE$ASTDT >= AE$TRTSDT]) + 1
AE$ASTDY[valid_ast_trt & AE$TRTSDT > AE$ASTDT] <- as.numeric(AE$ASTDT[valid_ast_trt & AE$TRTSDT > AE$ASTDT] - AE$TRTSDT[valid_ast_trt & AE$TRTSDT > AE$ASTDT])

valid_aen_trt <- !is.na(AE$AENDT) & !is.na(AE$TRTSDT)
AE$AENDY[valid_aen_trt & AE$AENDT >= AE$TRTSDT] <- as.numeric(AE$AENDT[valid_aen_trt & AE$AENDT >= AE$TRTSDT] - AE$TRTSDT[valid_aen_trt & AE$AENDT >= AE$TRTSDT]) + 1
AE$AENDY[valid_aen_trt & AE$TRTSDT > AE$AENDT] <- as.numeric(AE$AENDT[valid_aen_trt & AE$TRTSDT > AE$AENDT] - AE$TRTSDT[valid_aen_trt & AE$TRTSDT > AE$AENDT])

AE$ANLSTDY[valid_ast_trt] <- as.numeric(AE$ASTDT[valid_ast_trt] - AE$TRTSDT[valid_ast_trt]) + 1

# TRTEMFL
AE$TRTEMFL <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$TRTSDT) & AE$ASTDT >= AE$TRTSDT, "Y", "N")

# CQ01NAM logic
# IF index(aedecod,'APPLICATION') or index(aedecod,'DERMATITIS') or index(aedecod,'ERYTHEMA') or index(aedecod,'BLISTER') or (aebodsys='SKIN AND SUBCUTANEOUS TISSUE DISORDERS' and aedecod not in ('COLD SWEAT','HYPERHIDROSIS','ALOPECIA')) then CQ01NAM='DERMATOLOGIC EVENTS';
# index is case-sensitive substring
has_app <- grepl("APPLICATION", AE$AEDECOD, fixed = TRUE)
has_derm <- grepl("DERMATITIS", AE$AEDECOD, fixed = TRUE)
has_ery <- grepl("ERYTHEMA", AE$AEDECOD, fixed = TRUE)
has_blis <- grepl("BLISTER", AE$AEDECOD, fixed = TRUE)
# handle NA in grepl -> NA, replace with FALSE
has_app[is.na(has_app)] <- FALSE
has_derm[is.na(has_derm)] <- FALSE
has_ery[is.na(has_ery)] <- FALSE
has_blis[is.na(has_blis)] <- FALSE

cond_skin <- AE$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" & !(AE$AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA"))
cond_skin[is.na(cond_skin)] <- FALSE

idx_cq <- has_app | has_derm | has_ery | has_blis | cond_skin
AE$CQ01NAM[idx_cq] <- "DERMATOLOGIC EVENTS"

# ------------------------------------------------------------------
# Macro flagfrst implementation
flagfrst <- function(inds, sortvars, invar, outvar, ser = NULL, derm = NULL) {
  # inds: data.frame
  # sortvars: character vector (may be empty)
  # invar: character or "" / NULL
  # outvar: name of flag variable
  # ser: if "Y" then filter AESER=='Y'
  # derm: if "Y" then filter CQ01NAM=='DERMATOLOGIC EVENTS'
  df <- inds

  # where trtemfl='Y'
  df <- df %>% filter(TRTEMFL == "Y")

  if (!is.null(invar) && invar != "" && !is.na(invar)) {
    # additional filter handled outside? Actually invar filter is not a where, it's for first. logic
  }
  if (!is.null(ser) && ser == "Y") {
    df <- df %>% filter(AESER == "Y")
  }
  if (!is.null(derm) && derm == "Y") {
    df <- df %>% filter(CQ01NAM == "DERMATOLOGIC EVENTS")
  }

  # sort by usubjid &sortvars ASTDT
  # SAS missing first, character uppercase before lowercase (default SAS collation)
  # Use arrange with na handling; for character, R's default is locale-dependent but pilot data is uppercase
  # We'll arrange by USUBJID, sortvars, ASTDT
  sort_cols <- c("USUBJID", sortvars, "ASTDT")
  # Remove empty
  sort_cols <- sort_cols[sort_cols != "" & !is.na(sort_cols)]

  # For arrange, need to handle NA first: arrange puts NA last by default; we want NA first to mimic SAS
  # We can create helper: is.na column
  # Instead, we can use arrange with custom: order by is.na descending? Simpler: use dplyr arrange and then handle NA first via explicit
  # We'll use base order with na.last = FALSE for numeric/date, and for character, missing is ""? In SAS missing char is "".
  # For simplicity, use arrange with .locale? We'll implement manual ordering using base R order.

  # Build order vectors
  # Create list of vectors for order()
  ord_list <- list()
  for (col in sort_cols) {
    v <- df[[col]]
    # For character, replace NA with "" (SAS missing char sorts first, and "" < any non-empty)
    if (is.character(v)) {
      v[is.na(v)] <- ""
    }
    ord_list[[col]] <- v
  }
  # Use do.call(order, ...) with na.last = TRUE? For Date, NA should be first, so na.last=FALSE
  # order() handles NA via na.last
  if (length(ord_list) > 0) {
    ord <- do.call(order, c(ord_list, list(na.last = FALSE)))
    df <- df[ord, , drop = FALSE]
  }

  # Now first. logic: by usubjid &sortvars ASTDT; if first.usubjid or first.&invar then output
  # Determine grouping for first detection
  # If invar is empty, group is USUBJID
  # If invar is not empty, group is invar (but SAS first.invar is true when invar changes or higher by vars change)
  # However sortvars includes invar, so grouping by USUBJID + sortvars's invar? Actually for AOCCSFL, sortvars=aebodsys, invar=aebodsys => group by USUBJID + AEBODSYS
  # For AOCCPFL, sortvars=c(aebodsys,aedecod), invar=aedecod => group by USUBJID + AEBODSYS + AEDECOD? But first.aedecod will be true when aedecod changes, which includes when aebodsys changes.
  # So effectively group by USUBJID + sortvars (or USUBJID + invar if invar is subset)
  # To replicate SAS exactly, we can group by USUBJID and all sortvars up to invar inclusive.
  # Simpler: if invar == "" then group by USUBJID
  # else group by USUBJID + sortvars (which contains invar)
  if (is.null(invar) || invar == "" || is.na(invar)) {
    grp_vars <- "USUBJID"
  } else {
    # group by USUBJID + sortvars (which should include invar)
    grp_vars <- c("USUBJID", sortvars)
    # ensure invar is included
    if (!(invar %in% grp_vars)) grp_vars <- c(grp_vars, invar)
  }

  # Use dplyr to get first per group
  # Need to handle that df is already sorted by USUBJID, sortvars, ASTDT
  # So first row per group is the earliest ASTDT
  df2 <- df %>%
    group_by(across(all_of(grp_vars))) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(USUBJID, AESEQ, all_of(outvar) := NA_character_) %>%
    mutate(!!outvar := "Y") %>%
    arrange(USUBJID, AESEQ)

  # Keep only USUBJID, AESEQ, outvar
  df2 <- df2 %>% select(USUBJID, AESEQ, all_of(outvar)) %>% arrange(USUBJID, AESEQ)
  return(df2)
}

# Create flags
aea <- flagfrst(inds = AE, sortvars = character(0), invar = "", outvar = "AOCCFL", ser = NULL, derm = NULL)
aeb <- flagfrst(inds = AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCCSFL", ser = NULL, derm = NULL)
aec <- flagfrst(inds = AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL", ser = NULL, derm = NULL)
saea <- flagfrst(inds = AE, sortvars = character(0), invar = "", outvar = "AOCC02FL", ser = "Y", derm = NULL)
saeb <- flagfrst(inds = AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCC03FL", ser = "Y", derm = NULL)
saec <- flagfrst(inds = AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = "Y", derm = NULL)
derm <- flagfrst(inds = AE, sortvars = character(0), invar = "", outvar = "AOCC01FL", ser = NULL, derm = "Y")

# proc sort data=AE; by usubjid aeseq;
AE <- AE %>% arrange(USUBJID, AESEQ)

# data AE; merge AE AEA AEB AEC SAEA SAEB SAEC DERM; by usubjid aeseq;
# left joins
AE <- AE %>%
  left_join(aea, by = c("USUBJID","AESEQ")) %>%
  left_join(aeb, by = c("USUBJID","AESEQ")) %>%
  left_join(aec, by = c("USUBJID","AESEQ")) %>%
  left_join(saea, by = c("USUBJID","AESEQ")) %>%
  left_join(saeb, by = c("USUBJID","AESEQ")) %>%
  left_join(saec, by = c("USUBJID","AESEQ")) %>%
  left_join(derm, by = c("USUBJID","AESEQ"))

# ------------------------------------------------------------------
# data AE; retain STUDYID SITEID USUBJID TRTA TRTAN ... ; set AE(keep=...); by usubjid aeseq;
# Keep list with wildcard handling
# Keep: STUDYID SITEID USUBJID TRTAN TRTA AESEQ AETERM AELLT: AEDECOD AEPTCD AEHL: AEBODSYS AESOC: AESEV AESER AEACN AEREL AEOUT AESCAN AESCONG AESDISAB AESDTH AESHOSP AESLIFE AESOD TRTEMFL ASTDT AENDT ASTDTF CQ01NAM ASTDY AENDY AOCCFL AOCCSFL AOCCPFL AOCC01FL AOCC02FL AOCC03FL AOCC04FL AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT ADUR

# Identify wildcard columns present in AE
all_names <- names(AE)
keep_exact <- c("STUDYID","SITEID","USUBJID","TRTAN","TRTA","AESEQ","AETERM","AEDECOD","AEPTCD","AEBODSYS","AESEV","AESER","AEACN","AEREL","AEOUT","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","TRTEMFL","ASTDT","AENDT","ASTDTF","CQ01NAM","ASTDY","AENDY","AOCCFL","AOCCSFL","AOCCPFL","AOCC01FL","AOCC02FL","AOCC03FL","AOCC04FL","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT","ADUR")
# wildcard prefixes
wildcard_prefixes <- c("AELLT","AEHL","AESOC")
wildcard_cols <- unlist(lapply(wildcard_prefixes, function(p) all_names[startsWith(all_names, p)]))

keep_vars <- unique(c(keep_exact, wildcard_cols))
# retain order as specified in SAS retain statement
retain_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT","ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM","AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN","AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")
# But we will use keep_vars in retain order where possible, plus any remaining keep_vars not in retain_order
ordered_keep <- c()
for (v in retain_order) {
  if (v %in% keep_vars) ordered_keep <- c(ordered_keep, v)
  # handle wildcard already expanded: e.g., AELLT matches AELLT and AELLTCD, but retain_order lists both explicitly
}
# Add any keep_vars not yet in ordered_keep (to ensure no loss)
for (v in keep_vars) {
  if (!(v %in% ordered_keep)) ordered_keep <- c(ordered_keep, v)
}
# Subset AE to ordered_keep that exist
ordered_keep <- ordered_keep[ordered_keep %in% names(AE)]
AE_final <- AE %>% select(all_of(ordered_keep)) %>% arrange(USUBJID, AESEQ)

# data adamnew.ADAE(label='Adverse Events Analysis Dataset'); set AE; by USUBJID AESEQ; format TRTSDT TRTEDT ASTDT AENDT DATE9.;
ADAE <- AE_final

# Ensure date columns are Date type for output formatting
date_cols <- c("TRTSDT","TRTEDT","ASTDT","AENDT")
for (dc in date_cols) {
  if (dc %in% names(ADAE)) {
    # already Date, keep
    if (!inherits(ADAE[[dc]], "Date")) {
      ADAE[[dc]] <- convert_sas_date(ADAE[[dc]])
    }
  }
}

# Write dataset as CSV with dates as YYYY-MM-DD, na=""
# Use write.csv with na=""
# Need to format Dates as YYYY-MM-DD explicitly
ADAE_out <- ADAE
for (dc in date_cols) {
  if (dc %in% names(ADAE_out)) {
    ADAE_out[[dc]] <- format(ADAE_out[[dc]], "%Y-%m-%d")
    ADAE_out[[dc]][ADAE_out[[dc]] == "NA"] <- NA
  }
}

# Ensure output directory exists
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

write.csv(ADAE_out, file.path(out, "adae.csv"), row.names = FALSE, na = "")

# Also produce a plain-text rendering of contents (like PROC CONTENTS) for verification
# Create adae.txt with variable list
sink(file.path(out, "adae.txt"))
cat("Adverse Events Analysis Dataset (ADAE)\n")
cat("Label: Adverse Events Analysis Dataset\n")
cat(paste0("Observations: ", nrow(ADAE), "\n"))
cat(paste0("Variables: ", ncol(ADAE), "\n\n"))
cat("Variables in order:\n")
for (i in seq_along(names(ADAE))) {
  v <- names(ADAE)[i]
  cls <- class(ADAE[[v]])[1]
  cat(sprintf("%3d  %-12s  %s\n", i, v, cls))
}
cat("\nFirst 5 rows (formatted):\n")
print(head(ADAE_out, 5))
sink()

# End
