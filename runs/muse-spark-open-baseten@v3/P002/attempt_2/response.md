```r
# P002_adae.sas -> R translation
# Produces dataset adae.csv under OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helpers ---------------------------------------------------------------
sas_round <- function(x, digits = 0) {
  # SAS round half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# parse yymmdd10. : expects YYYY-MM-DD, returns Date or NA
parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | is.na(x)] <- NA_character_
  # SAS yymmdd10. with ?? : invalid -> NA, no error
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

# convert SAS numeric date (days since 1960-01-01) to Date if needed
to_Date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    # haven may already have converted; if numeric and looks like SAS date (e.g. 10000-20000)
    # convert from 1960-01-01
    return(as.Date(v, origin = "1960-01-01"))
  }
  # character: try yymmdd10 then generic
  if (is.character(v)) {
    # try ISO
    d <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
    # if all NA try other
    if (all(is.na(d) & !is.na(v))) {
      d2 <- suppressWarnings(as.Date(v))
      d[is.na(d)] <- d2[is.na(d)]
    }
    return(d)
  }
  v
}

# read xpt with haven, zap labels to plain vectors but keep values
read_xpt_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- haven::read_xpt(path)
  # convert labelled to plain
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  # zap_labels keeps numeric, but convert labelled character
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven labelled vectors -> convert
  for (nm in names(df)) {
    if (haven::is.labelled(df[[nm]])) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # tibble to data.frame
  df
}

# locate inputs ---------------------------------------------------------
root <- "D:/meta_hackathon/sas2r-agent"
rawsdtm <- file.path(root, "data/raw/cdisc-pilot/sdtm")
rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")
derived_sdtm <- file.path(root, "data/derived/cdisc-pilot/sdtm")
derived_adam <- file.path(root, "data/derived/cdisc-pilot/adam")

# AE
ae_path1 <- file.path(rawsdtm, "ae.xpt")
ae_path2 <- file.path(derived_sdtm, "ae.sas7bdat")
ae_path3 <- file.path(derived_sdtm, "ae.xpt")
AE <- read_xpt_safe(ae_path1)
if (is.null(AE)) AE <- read_xpt_safe(ae_path3)
if (is.null(AE) && file.exists(ae_path2)) AE <- haven::read_sas(ae_path2)

# ADSL
adsl_path1 <- file.path(rawadam, "adsl.xpt")
adsl_path2 <- file.path(derived_adam, "adsl.sas7bdat")
adsl_path3 <- file.path(derived_adam, "adsl.xpt")
ADSL <- read_xpt_safe(adsl_path1)
if (is.null(ADSL)) ADSL <- read_xpt_safe(adsl_path3)
if (is.null(ADSL) && file.exists(adsl_path2)) ADSL <- haven::read_sas(adsl_path2)

stopifnot(!is.null(AE), !is.null(ADSL))

# Ensure column names uppercase for matching SAS (SAS is case-insensitive but we keep original case)
# Work with uppercase names internally then restore
names(AE) <- toupper(names(AE))
names(ADSL) <- toupper(names(ADSL))

# proc sort AE by USUBJID AESEQ
AE <- AE[order(AE$USUBJID, AE$AESEQ, na.last = FALSE), , drop = FALSE]

# data AE: ASTDT, AENDT, ADUR
# AESTDTC, AEENDTC are character
if (!"AESTDTC" %in% names(AE)) AE$AESTDTC <- NA_character_
if (!"AEENDTC" %in% names(AE)) AE$AEENDTC <- NA_character_

AE$ASTDT <- parse_yymmdd10(AE$AESTDTC)
AE$AENDT <- parse_yymmdd10(AE$AEENDTC)

# ADUR = AENDT - ASTDT + 1 if both not missing
AE$ADUR <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$AENDT),
                  as.integer(AE$AENDT - AE$ASTDT + 1L), NA_integer_)

# TEMP from ADSL keep SITEID USUBJID TRT01A TRT01AN AGE AGEGR1 AGEGR1N RACE RACEN SEX SAFFL TRTSDT TRTEDT rename TRT01A=TRTA TRT01AN=TRTAN
# Ensure columns exist
keep_adsl <- c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT")
for (k in keep_adsl) if (!k %in% names(ADSL)) ADSL[[k]] <- NA
TEMP <- ADSL[, keep_adsl, drop = FALSE]
names(TEMP)[names(TEMP) == "TRT01A"] <- "TRTA"
names(TEMP)[names(TEMP) == "TRT01AN"] <- "TRTAN"
# sort by USUBJID
TEMP <- TEMP[order(TEMP$USUBJID, na.last = FALSE), , drop = FALSE]

# Convert TRTSDT/TRTEDT to Date if needed
TEMP$TRTSDT <- to_Date(TEMP$TRTSDT)
TEMP$TRTEDT <- to_Date(TEMP$TRTEDT)

# merge AE with TEMP by USUBJID (left join, keep if inAE)
# drop/rename overlapping columns except keys before merge to avoid .x/.y
# AE and TEMP share SITEID? AE may have SITEID already; keep AE's SITEID, drop TEMP's if duplicate? SAS merge overlays TEMP onto AE by USUBJID, so TEMP vars overwrite if same name? But AE doesn't have TRTA etc.
# For safety, remove from TEMP any column that already exists in AE except USUBJID
common <- intersect(names(AE), names(TEMP))
common <- setdiff(common, "USUBJID")
if (length(common) > 0) {
  # SAS merge: if both have same variable, the second dataset's value overwrites? Actually merge by USUBJID: variables from TEMP will be added; if AE already has SITEID, TEMP SITEID would overwrite? In SAS, if both have same variable, the value from the last dataset in merge statement is retained.
  # Here merge AE(in=inAE) TEMP; so TEMP is second, so its SITEID would overwrite AE's SITEID.
  # To emulate, we should let TEMP overwrite: drop AE's version
  AE <- AE[, setdiff(names(AE), common), drop = FALSE]
}
AE <- merge(AE, TEMP, by = "USUBJID", all.x = TRUE, sort = FALSE)
# restore order by USUBJID AESEQ
AE <- AE[order(AE$USUBJID, AE$AESEQ, na.last = FALSE), , drop = FALSE]

# Imputation for ASTDT when length(AESTDTC)=7
# SAS: if ASTDT=. and length(AESTDTC)=7 then ASTDT=input(compress(AESTDTC||'-01'), yymmdd10.)
# length in SAS is trimmed length? Actually length() returns length without trailing blanks. Use nchar(trimws)
AE$ASTDTF <- NA_character_
idx_imp <- which(is.na(AE$ASTDT) & !is.na(AE$AESTDTC) & nchar(trimws(AE$AESTDTC)) == 7)
if (length(idx_imp) > 0) {
  # compress removes blanks, then || '-01'
  comp <- gsub(" ", "", AE$AESTDTC[idx_imp], fixed = TRUE)
  comp <- paste0(comp, "-01")
  AE$ASTDT[idx_imp] <- parse_yymmdd10(comp)
  AE$ASTDTF[idx_imp] <- "D"
}

# CQ01NAM
AE$CQ01NAM <- NA_character_
# helper for index (case-sensitive substring)
has_sub <- function(x, pat) {
  ifelse(is.na(x), FALSE, grepl(pat, x, fixed = TRUE))
}
cond_cq <- has_sub(AE$AEDECOD, "APPLICATION") |
  has_sub(AE$AEDECOD, "DERMATITIS") |
  has_sub(AE$AEDECOD, "ERYTHEMA") |
  has_sub(AE$AEDECOD, "BLISTER") |
  (!is.na(AE$AEBODSYS) & AE$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
     !is.na(AE$AEDECOD) & !(AE$AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA")))
AE$CQ01NAM[cond_cq] <- "DERMATOLOGIC EVENTS"

# ASTDY, AENDY, ANLSTDY, TRTEMFL
# Ensure TRTSDT is Date
AE$TRTSDT <- to_Date(AE$TRTSDT)
AE$TRTEDT <- to_Date(AE$TRTEDT)

AE$ASTDY <- NA_integer_
AE$AENDY <- NA_integer_
AE$ANLSTDY <- NA_integer_

# ASTDY
idx1 <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT) & AE$ASTDT >= AE$TRTSDT
AE$ASTDY[idx1] <- as.integer(AE$ASTDT[idx1] - AE$TRTSDT[idx1] + 1L)
idx2 <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT) & AE$TRTSDT > AE$ASTDT
AE$ASTDY[idx2] <- as.integer(AE$ASTDT[idx2] - AE$TRTSDT[idx2])

# AENDY
idx1e <- !is.na(AE$AENDT) & !is.na(AE$TRTSDT) & AE$AENDT >= AE$TRTSDT
AE$AENDY[idx1e] <- as.integer(AE$AENDT[idx1e] - AE$TRTSDT[idx1e] + 1L)
idx2e <- !is.na(AE$AENDT) & !is.na(AE$TRTSDT) & AE$TRTSDT > AE$AENDT
AE$AENDY[idx2e] <- as.integer(AE$AENDT[idx2e] - AE$TRTSDT[idx2e])

# ANLSTDY
idx_anl <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT)
AE$ANLSTDY[idx_anl] <- as.integer(AE$ASTDT[idx_anl] - AE$TRTSDT[idx_anl] + 1L)

# TRTEMFL
AE$TRTEMFL <- "N"
idx_trt <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT) & AE$ASTDT >= AE$TRTSDT
AE$TRTEMFL[idx_trt] <- "Y"

# flagfrst macro emulation ------------------------------------------------
flagfrst <- function(inds, sortvars, invar, outvar, ser = NULL, derm = NULL) {
  # inds is data.frame AE
  # sortvars: character vector
  # invar: character or "" 
  # ser: "Y" or NULL, derm: "Y" or NULL
  ds01 <- inds
  # where trtemfl='Y' and optional
  ds01 <- ds01[!is.na(ds01$TRTEMFL) & ds01$TRTEMFL == "Y", , drop = FALSE]
  if (!is.null(ser) && ser == "Y") {
    ds01 <- ds01[!is.na(ds01$AESER) & ds01$AESER == "Y", , drop = FALSE]
  }
  if (!is.null(derm) && derm == "Y") {
    ds01 <- ds01[!is.na(ds01$CQ01NAM) & ds01$CQ01NAM == "DERMATOLOGIC EVENTS", , drop = FALSE]
  }
  if (!is.null(invar) && invar != "") {
    ds01 <- ds01[!is.na(ds01[[invar]]) & ds01[[invar]] != "", , drop = FALSE]
  }
  if (nrow(ds01) == 0) {
    return(data.frame(USUBJID = character(0), AESEQ = numeric(0), stringsAsFactors = FALSE))
  }
  # sort by usubjid &sortvars ASTDT
  # SAS missing first: arrange with NA first
  # Build order columns
  ord_cols <- c("USUBJID", sortvars, "ASTDT")
  # Ensure columns exist
  for (c in ord_cols) if (!c %in% names(ds01)) ds01[[c]] <- NA
  # order: use order with na.last=FALSE
  # For character, order handles NA
  # Create list for order
  ord_list <- lapply(ord_cols, function(c) ds01[[c]])
  # Use do.call(order, ...) with na.last=FALSE
  o <- do.call(order, c(ord_list, list(na.last = FALSE)))
  ds01 <- ds01[o, , drop = FALSE]

  # first. logic
  if (is.null(invar) || invar == "") {
    # first.usubjid
    # keep first row per USUBJID
    # Use duplicated
    keep <- !duplicated(ds01$USUBJID)
    ds02 <- ds01[keep, c("USUBJID", "AESEQ"), drop = FALSE]
    ds02[[outvar]] <- "Y"
  } else {
    # first.&invar within by usubjid &sortvars ASTDT
    # Since BY includes usubjid + sortvars + ASTDT, first.invar is first per invar group
    # Equivalent to first per USUBJID + invar when sortvars includes invar, but for safety group by USUBJID + invar
    # For the two cases:
    #  - invar=aebodsys, sortvars=aebodsys => group by USUBJID, AEBODSYS
    #  - invar=aedecod, sortvars=c(aebodsys,aedecod) => group by USUBJID, AEBODSYS, AEDECOD (or just USUBJID+AEDECOD)
    # SAS first.aedecod with BY usubjid aebodsys aedecod ASTDT means group by USUBJID, AEBODSYS, AEDECOD
    # So we group by USUBJID + sortvars (which includes invar)
    grp_cols <- c("USUBJID", sortvars)
    # Remove ASTDT from grp (not part of grouping)
    # duplicated on those columns
    # Create key
    key <- do.call(paste, c(lapply(grp_cols, function(c) as.character(ds01[[c]])), sep = "\r"))
    keep <- !duplicated(key)
    ds02 <- ds01[keep, c("USUBJID", "AESEQ"), drop = FALSE]
    ds02[[outvar]] <- "Y"
  }
  # sort by usubjid aeseq for merge
  ds02 <- ds02[order(ds02$USUBJID, ds02$AESEQ, na.last = FALSE), , drop = FALSE]
  names(ds02) <- toupper(names(ds02))
  # outvar already uppercase
  ds02
}

aea  <- flagfrst(AE, sortvars = character(0), invar = "", outvar = "AOCCFL")
aeb  <- flagfrst(AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCCSFL")
aec  <- flagfrst(AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL")
saea <- flagfrst(AE, sortvars = character(0), invar = "", outvar = "AOCC02FL", ser = "Y")
saeb <- flagfrst(AE, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCC03FL", ser = "Y")
saec <- flagfrst(AE, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = "Y")
derm <- flagfrst(AE, sortvars = character(0), invar = "", outvar = "AOCC01FL", derm = "Y")

# merge back: AE left join each by USUBJID AESEQ
# Ensure AE has uppercase names
AE$USUBJID <- as.character(AE$USUBJID)
# helper merge
merge_flag <- function(base, flagdf) {
  if (is.null(flagdf) || nrow(flagdf) == 0) return(base)
  # flagdf has USUBJID, AESEQ, outvar
  base <- merge(base, flagdf, by = c("USUBJID","AESEQ"), all.x = TRUE, sort = FALSE)
  base
}
AE <- AE[order(AE$USUBJID, AE$AESEQ, na.last = FALSE), , drop = FALSE]
AE <- merge_flag(AE, aea)
AE <- merge_flag(AE, aeb)
AE <- merge_flag(AE, aec)
AE <- merge_flag(AE, saea)
AE <- merge_flag(AE, saeb)
AE <- merge_flag(AE, saec)
AE <- merge_flag(AE, derm)
AE <- AE[order(AE$USUBJID, AE$AESEQ, na.last = FALSE), , drop = FALSE]

# retain ordering and keep selected variables
retain_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT",
                  "ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM",
                  "AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN",
                  "AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")

# Ensure all retain columns exist
for (c in retain_order) if (!c %in% names(AE)) AE[[c]] <- NA

AE_final <- AE[, retain_order, drop = FALSE]
AE_final <- AE_final[order(AE_final$USUBJID, AE_final$AESEQ, na.last = FALSE), , drop = FALSE]

# Format dates as Date (already) - for CSV write as YYYY-MM-DD
# Convert Date columns to character ISO for CSV
date_cols <- c("TRTSDT","TRTEDT","ASTDT","AENDT")
for (dc in date_cols) {
  if (dc %in% names(AE_final)) {
    # ensure Date
    AE_final[[dc]] <- to_Date(AE_final[[dc]])
  }
}

# Write dataset adae.csv
# Use write.csv with na=""
# Dates will be written as YYYY-MM-DD via as.character
# Ensure date columns are character ISO
out_df <- AE_final
for (dc in date_cols) {
  if (dc %in% names(out_df)) {
    out_df[[dc]] <- ifelse(is.na(out_df[[dc]]), "", format(out_df[[dc]], "%Y-%m-%d"))
  }
}
# Also ensure any other Date columns formatted
for (nm in names(out_df)) {
  if (inherits(out_df[[nm]], "Date")) {
    out_df[[nm]] <- ifelse(is.na(out_df[[nm]]), "", format(out_df[[nm]], "%Y-%m-%d"))
  }
  if (inherits(out_df[[nm]], "POSIXt")) {
    out_df[[nm]] <- ifelse(is.na(out_df[[nm]]), "", format(out_df[[nm]], "%Y-%m-%dT%H:%M:%S"))
  }
}

write.csv(out_df, file.path(out, "adae.csv"), row.names = FALSE, na = "")

```