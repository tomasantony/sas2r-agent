```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}

sas_sort_order <- function(df, by_cols) {
  # base order with missing first, radix for speed; SAS uppercase<lowercase (ASCII)
  # locale on Windows is case-insensitive for some collations, so force byte order via xtfrm on UTF-8 bytes
  # For pilot data all codes are uppercase, base order matches SAS.
  ord_args <- lapply(by_cols, function(c) df[[c]])
  # use order with na.last = FALSE (missing first)
  do.call(order, c(ord_args, list(na.last = FALSE, method = "radix")))
}

parse_yymmdd10 <- function(x) {
  xc <- as.character(x)
  xc <- trimws(xc)
  suppressWarnings(as.Date(xc, format = "%Y-%m-%d"))
}

convert_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  xc <- as.character(x)
  xc <- trimws(xc)
  xc[xc == ""] <- NA_character_
  suppressWarnings(as.Date(xc, format = "%Y-%m-%d"))
}

sas_length <- function(x) {
  xc <- as.character(x)
  out <- rep(NA_integer_, length(xc))
  idx <- !is.na(xc)
  # SAS LENGTH strips trailing blanks only
  out[idx] <- nchar(gsub(" +$", "", xc[idx]))
  out
}

raw_sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
raw_adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

ae_path1 <- file.path(raw_sdtm_dir, "ae.xpt")
ae_path2 <- file.path(raw_sdtm_dir, "AE.xpt")
ae_path <- if (file.exists(ae_path1)) ae_path1 else ae_path2
adsl_path1 <- file.path(raw_adam_dir, "adsl.xpt")
adsl_path2 <- file.path(raw_adam_dir, "ADSL.xpt")
adsl_path <- if (file.exists(adsl_path1)) adsl_path1 else adsl_path2

ae_raw <- haven::read_xpt(ae_path)
adsl_raw <- haven::read_xpt(adsl_path)

ae_raw <- haven::zap_formats(haven::zap_labels(ae_raw))
adsl_raw <- haven::zap_formats(haven::zap_labels(adsl_raw))
ae_raw <- as.data.frame(ae_raw, stringsAsFactors = FALSE)
adsl_raw <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
names(ae_raw) <- toupper(names(ae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

# proc sort data=sdtmnew.AE OUT=AE; by USUBJID AESEQ;
o <- sas_sort_order(ae_raw, c("USUBJID", "AESEQ"))
AE <- ae_raw[o, , drop = FALSE]

# data AE; ASTDT/AENDT/ADUR
AE$ASTDT <- parse_yymmdd10(trimws(as.character(AE$AESTDTC)))
AE$AENDT <- parse_yymmdd10(trimws(as.character(AE$AEENDTC)))
AE$ADUR <- NA_real_
idx_dur <- !is.na(AE$ASTDT) & !is.na(AE$AENDT)
AE$ADUR[idx_dur] <- as.integer(AE$AENDT[idx_dur] - AE$ASTDT[idx_dur]) + 1L

# proc sort ADSL OUT=TEMP keep ... rename TRT01A=TRTA TRT01AN=TRTAN; by USUBJID
need_adsl <- c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT")
have_adsl <- need_adsl[need_adsl %in% names(adsl_raw)]
TEMP <- adsl_raw[, have_adsl, drop = FALSE]
if ("TRT01A" %in% names(TEMP)) {
  names(TEMP)[names(TEMP) == "TRT01A"] <- "TRTA"
}
if ("TRT01AN" %in% names(TEMP)) {
  names(TEMP)[names(TEMP) == "TRT01AN"] <- "TRTAN"
}
# convert TRTSDT TRTEDT to Date if needed
if ("TRTSDT" %in% names(TEMP)) TEMP$TRTSDT <- convert_sas_date(TEMP$TRTSDT)
if ("TRTEDT" %in% names(TEMP)) TEMP$TRTEDT <- convert_sas_date(TEMP$TRTEDT)
o2 <- sas_sort_order(TEMP, c("USUBJID"))
TEMP <- TEMP[o2, , drop = FALSE]

# data AE; merge AE(in=inAE) TEMP; by USUBJID; if inAE;
common_cols <- intersect(names(AE), names(TEMP))
common_cols <- setdiff(common_cols, "USUBJID")
TEMP2 <- TEMP
if (length(common_cols) > 0) {
  TEMP2 <- TEMP2[, setdiff(names(TEMP2), common_cols), drop = FALSE]
}
stopifnot("USUBJID" %in% names(AE))
stopifnot("USUBJID" %in% names(TEMP2))
AE <- merge(AE, TEMP2, by = "USUBJID", all.x = TRUE, sort = FALSE)
# SAS merge by USUBJID keeps AE order grouped by USUBJID; re-sort by USUBJID AESEQ to match
o3 <- sas_sort_order(AE, c("USUBJID", "AESEQ"))
AE <- AE[o3, , drop = FALSE]

# imputation and derived vars
if (!"ASTDTF" %in% names(AE)) AE$ASTDTF <- NA_character_ else AE$ASTDTF <- as.character(AE$ASTDTF)
if (!"CQ01NAM" %in% names(AE)) AE$CQ01NAM <- NA_character_ else AE$CQ01NAM <- as.character(AE$CQ01NAM)
# ensure TRTSDT Date
if ("TRTSDT" %in% names(AE)) AE$TRTSDT <- convert_sas_date(AE$TRTSDT)
if ("TRTEDT" %in% names(AE)) AE$TRTEDT <- convert_sas_date(AE$TRTEDT)
AE$ASTDT <- convert_sas_date(AE$ASTDT)
AE$AENDT <- convert_sas_date(AE$AENDT)

# if ASTDT=. and length(AESTDTC)=7 then ASTDT=input(compress(AESTDTC||'-01'),yymmdd10.); ASTDTF='D';
len7 <- sas_length(as.character(AE$AESTDTC))
need_imp <- is.na(AE$ASTDT) & !is.na(len7) & len7 == 7L
need_imp[is.na(need_imp)] <- FALSE
if (any(need_imp)) {
  raw_imp <- paste0(as.character(AE$AESTDTC[need_imp]), "-01")
  raw_imp <- gsub(" ", "", raw_imp, fixed = TRUE)
  AE$ASTDT[need_imp] <- suppressWarnings(as.Date(raw_imp, format = "%Y-%m-%d"))
  AE$ASTDTF[need_imp] <- "D"
}
AE$ASTDTF[is.na(AE$ASTDTF) | AE$ASTDTF == ""] <- NA_character_

# ASTDY / AENDY / ANLSTDY / TRTEMFL
AE$ASTDY <- NA_real_
AE$AENDY <- NA_real_
AE$ANLSTDY <- NA_real_
# chained SAS semantics: ASTDT>=TRTSDT>.z  => ASTDT>=TRTSDT & TRTSDT nonmissing
astdt <- AE$ASTDT
aendt <- AE$AENDT
trtsdt <- AE$TRTSDT
ast_ge_trt <- !is.na(astdt) & !is.na(trtsdt) & (astdt >= trtsdt)
trt_gt_ast <- !is.na(astdt) & !is.na(trtsdt) & (trtsdt > astdt)
AE$ASTDY[ast_ge_trt] <- as.integer(astdt[ast_ge_trt] - trtsdt[ast_ge_trt]) + 1L
AE$ASTDY[trt_gt_ast] <- as.integer(astdt[trt_gt_ast] - trtsdt[trt_gt_ast])
aen_ge_trt <- !is.na(aendt) & !is.na(trtsdt) & (aendt >= trtsdt)
trt_gt_aen <- !is.na(aendt) & !is.na(trtsdt) & (trtsdt > aendt)
AE$AENDY[aen_ge_trt] <- as.integer(aendt[aen_ge_trt] - trtsdt[aen_ge_trt]) + 1L
AE$AENDY[trt_gt_aen] <- as.integer(aendt[trt_gt_aen] - trtsdt[trt_gt_aen])
both_nmiss <- !is.na(astdt) & !is.na(trtsdt)
AE$ANLSTDY[both_nmiss] <- as.integer(astdt[both_nmiss] - trtsdt[both_nmiss]) + 1L

AE$TRTEMFL <- "N"
AE$TRTEMFL[ast_ge_trt] <- "Y"

# CQ01NAM dermatologic events
# ensure character vectors
get_chr <- function(v) if (is.null(v)) rep(NA_character_, nrow(AE)) else as.character(v)
aedecod <- get_chr(AE$AEDECOD)
aebodsys <- get_chr(AE$AEBODSYS)
idx_app <- !is.na(aedecod) & (grepl("APPLICATION", aedecod, fixed = TRUE) |
  grepl("DERMATITIS", aedecod, fixed = TRUE) |
  grepl("ERYTHEMA", aedecod, fixed = TRUE) |
  grepl("BLISTER", aedecod, fixed = TRUE))
idx_skin <- !is.na(aebodsys) & aebodsys == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
  !is.na(aedecod) & !(aedecod %in% c("COLD SWEAT", "HYPERHIDROSIS", "ALOPECIA"))
idx_derm <- (idx_app | idx_skin)
idx_derm[is.na(idx_derm)] <- FALSE
AE$CQ01NAM[idx_derm] <- "DERMATOLOGIC EVENTS"

flagfrst <- function(inds, sortvars, invar, outvar, ser, derm) {
  d <- inds
  # where trtemfl='Y'
  d <- d[!is.na(d$TRTEMFL) & d$TRTEMFL == "Y", , drop = FALSE]
  if (!is.null(invar) && invar != "") {
    stopifnot(invar %in% names(d))
    v <- as.character(d[[invar]])
    keep <- !is.na(v) & v != ""
    d <- d[keep, , drop = FALSE]
  }
  if (!is.null(ser) && ser != "") {
    stopifnot("AESER" %in% names(d))
    v2 <- as.character(d$AESER)
    d <- d[!is.na(v2) & v2 == "Y", , drop = FALSE]
  }
  if (!is.null(derm) && derm != "") {
    stopifnot("CQ01NAM" %in% names(d))
    v3 <- as.character(d$CQ01NAM)
    d <- d[!is.na(v3) & v3 == "DERMATOLOGIC EVENTS", , drop = FALSE]
  }
  if (nrow(d) == 0) {
    out <- data.frame(USUBJID = character(0), AESEQ = numeric(0), stringsAsFactors = FALSE)
    out[[outvar]] <- character(0)
    names(out) <- c("USUBJID", "AESEQ", outvar)
    return(out)
  }
  by_cols <- c("USUBJID", sortvars, "ASTDT")
  # add AESEQ tie-breaker to emulate EQUALS (preserve AESEQ order for ties)
  by_cols_eq <- c(by_cols, "AESEQ")
  # keep only existing cols
  by_cols_eq <- by_cols_eq[by_cols_eq %in% names(d)]
  oo <- sas_sort_order(d, by_cols_eq)
  d <- d[oo, , drop = FALSE]
  if (is.null(invar) || invar == "") {
    grp <- list(d$USUBJID)
  } else {
    # group = USUBJID + sortvars (which contain invar)
    gvars <- c("USUBJID", sortvars)
    gvars <- gvars[gvars %in% names(d)]
    grp <- lapply(gvars, function(c) d[[c]])
  }
  # first per group: duplicated on group keys
  # build key paste with NA handling (NA sorts first, distinct)
  key_df <- as.data.frame(grp, stringsAsFactors = FALSE)
  # duplicated marks first as FALSE
  is_first <- !duplicated(key_df)
  d2 <- d[is_first, , drop = FALSE]
  d2[[outvar]] <- "Y"
  keepcols <- c("USUBJID", "AESEQ", outvar)
  d2 <- d2[, keepcols, drop = FALSE]
  o <- sas_sort_order(d2, c("USUBJID", "AESEQ"))
  d2[o, , drop = FALSE]
}

aea  <- flagfrst(AE, c(), "", "AOCCFL", "", "")
aeb  <- flagfrst(AE, c("AEBODSYS"), "AEBODSYS", "AOCCSFL", "", "")
aec  <- flagfrst(AE, c("AEBODSYS","AEDECOD"), "AEDECOD", "AOCCPFL", "", "")
saea <- flagfrst(AE, c(), "", "AOCC02FL", "Y", "")
saeb <- flagfrst(AE, c("AEBODSYS"), "AEBODSYS", "AOCC03FL", "Y", "")
saec <- flagfrst(AE, c("AEBODSYS","AEDECOD"), "AEDECOD", "AOCC04FL", "Y", "")
derm <- flagfrst(AE, c(), "", "AOCC01FL", "", "Y")

# proc sort AE by usubjid aeseq
oae <- sas_sort_order(AE, c("USUBJID", "AESEQ"))
AE <- AE[oae, , drop = FALSE]

merge_flag <- function(base, flag) {
  # drop overlapping non-key cols from flag (should be none except keys)
  cc <- intersect(names(base), names(flag))
  cc <- setdiff(cc, c("USUBJID", "AESEQ"))
  if (length(cc) > 0) flag <- flag[, setdiff(names(flag), cc), drop = FALSE]
  stopifnot(all(c("USUBJID","AESEQ") %in% names(flag)))
  merge(base, flag, by = c("USUBJID", "AESEQ"), all.x = TRUE, sort = FALSE)
}
AE <- merge_flag(AE, aea)
AE <- merge_flag(AE, aeb)
AE <- merge_flag(AE, aec)
AE <- merge_flag(AE, saea)
AE <- merge_flag(AE, saeb)
AE <- merge_flag(AE, saec)
AE <- merge_flag(AE, derm)
oae2 <- sas_sort_order(AE, c("USUBJID", "AESEQ"))
AE <- AE[oae2, , drop = FALSE]

retain_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT","ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM","AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN","AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")

# ensure all retain vars exist; create missing as NA
for (c in retain_order) {
  if (!c %in% names(AE)) AE[[c]] <- NA
}
# keep only retain_order in that order
ADAE <- AE[, retain_order, drop = FALSE]
# sort by USUBJID AESEQ
o_final <- sas_sort_order(ADAE, c("USUBJID", "AESEQ"))
ADAE <- ADAE[o_final, , drop = FALSE]

# ensure Date types for TRTSDT TRTEDT ASTDT AENDT
ADAE$TRTSDT <- convert_sas_date(ADAE$TRTSDT)
ADAE$TRTEDT <- convert_sas_date(ADAE$TRTEDT)
ADAE$ASTDT <- convert_sas_date(ADAE$ASTDT)
ADAE$AENDT <- convert_sas_date(ADAE$AENDT)

write.csv(ADAE, file.path(out, "adae.csv"), row.names = FALSE, na = "")
```