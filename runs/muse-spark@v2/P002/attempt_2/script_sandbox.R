# P002_adae.sas -> R translation (CDISC pilot ADAE)
# Reads: sdtm AE, adam ADSL (xpt); Writes: OUT_DIR/adae.csv

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

ae_xpt   <- "/bench/data/raw/cdisc-pilot/sdtm/ae.xpt"
adsl_xpt <- "/bench/data/raw/cdisc-pilot/adam/adsl.xpt"

unwrap <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
    }
    if (inherits(x, "haven_labelled")) x <- as.vector(x)
    df[[nm]] <- x
  }
  names(df) <- toupper(names(df))
  df
}

AE0 <- unwrap(haven::read_xpt(ae_xpt))
ADSL0 <- unwrap(haven::read_xpt(adsl_xpt))

# ---- helpers ----
# SAS YYMMDD10. with ?? : strict YYYY-MM-DD, invalid -> NA
parse_yymmdd10 <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

# SAS COMPRESS default removes blanks
sas_compress <- function(x) gsub(" ", "", x, fixed = TRUE)

# SAS LENGTH: length excluding trailing blanks; NA -> NA
sas_length <- function(x) {
  x <- as.character(x)
  out <- nchar(sub(" +$", "", x))
  out[is.na(x)] <- NA_integer_
  out
}

# Convert SAS date-like column to Date
to_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    return(parse_yymmdd10(x))
  }
  return(x)
}

# blank check: SAS '' means missing char (blank). Treat NA or all-blank as blank.
is_blank <- function(x) {
  x <- as.character(x)
  is.na(x) | trimws(x) == ""
}
is_not_blank <- function(x) !is_blank(x)

contains_fixed <- function(x, pat) {
  x <- as.character(x)
  !is.na(x) & grepl(pat, x, fixed = TRUE)
}

# ---- Step 1: sort AE by USUBJID AESEQ (SAS order: missing first) ----
stopifnot(all(c("USUBJID", "AESEQ") %in% names(AE0)))
AE <- AE0
ord <- order(ifelse(is.na(AE$USUBJID), "", AE$USUBJID), AE$AESEQ,
             na.last = FALSE, method = "radix")
AE <- AE[ord, , drop = FALSE]

# ---- Step 2: ASTDT / AENDT / ADUR ----
stopifnot(all(c("AESTDTC", "AEENDTC") %in% names(AE)))
AE$ASTDT <- parse_yymmdd10(trimws(as.character(AE$AESTDTC)))
AE$AENDT <- parse_yymmdd10(trimws(as.character(AE$AEENDTC)))
AE$ADUR <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$AENDT),
                  as.numeric(AE$AENDT - AE$ASTDT) + 1, NA_real_)

# ---- Step 3: TEMP from ADSL ----
need_adsl <- c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N",
               "RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT")
stopifnot(all(need_adsl %in% names(ADSL0)))
TEMP <- ADSL0[, need_adsl, drop = FALSE]
names(TEMP)[names(TEMP) == "TRT01A"] <- "TRTA"
names(TEMP)[names(TEMP) == "TRT01AN"] <- "TRTAN"
TEMP$TRTSDT <- to_sas_date(TEMP$TRTSDT)
TEMP$TRTEDT <- to_sas_date(TEMP$TRTEDT)
TEMP <- TEMP[order(ifelse(is.na(TEMP$USUBJID), "", TEMP$USUBJID),
                   na.last = FALSE, method = "radix"), , drop = FALSE]

# ---- Step 4: merge AE + TEMP by USUBJID (left join), impute, derive ----
common_cols <- intersect(names(AE), names(TEMP))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  TEMP <- TEMP[, setdiff(names(TEMP), common_cols), drop = FALSE]
}
stopifnot("USUBJID" %in% names(AE), "USUBJID" %in% names(TEMP))
AE <- merge(AE, TEMP, by = "USUBJID", all.x = TRUE, sort = FALSE)
AE <- AE[order(ifelse(is.na(AE$USUBJID), "", AE$USUBJID), AE$AESEQ,
               na.last = FALSE, method = "radix"), , drop = FALSE]

stopifnot(all(c("ASTDT","AESTDTC","TRTSDT") %in% names(AE)))
AE$ASTDTF <- NA_character_
len7 <- sas_length(AE$AESTDTC)
idx_imp <- is.na(AE$ASTDT) & !is.na(len7) & len7 == 7L
if (any(idx_imp, na.rm = TRUE)) {
  raw_imp <- paste0(as.character(AE$AESTDTC[idx_imp]), "-01")
  raw_imp <- sas_compress(raw_imp)
  AE$ASTDT[idx_imp] <- parse_yymmdd10(raw_imp)
  AE$ASTDTF[idx_imp] <- "D"
}

if (!("CQ01NAM" %in% names(AE))) AE$CQ01NAM <- NA_character_ else AE$CQ01NAM <- as.character(AE$CQ01NAM)

AE$TRTSDT <- to_sas_date(AE$TRTSDT)
AE$TRTEDT <- to_sas_date(AE$TRTEDT)

AE$ASTDY <- NA_real_
AE$AENDY <- NA_real_
AE$ANLSTDY <- NA_real_

has_ast <- !is.na(AE$ASTDT) & !is.na(AE$TRTSDT)
AE$ASTDY[has_ast & AE$ASTDT >= AE$TRTSDT] <- as.numeric(AE$ASTDT[has_ast & AE$ASTDT >= AE$TRTSDT] - AE$TRTSDT[has_ast & AE$ASTDT >= AE$TRTSDT]) + 1
AE$ASTDY[has_ast & AE$TRTSDT > AE$ASTDT] <- as.numeric(AE$ASTDT[has_ast & AE$TRTSDT > AE$ASTDT] - AE$TRTSDT[has_ast & AE$TRTSDT > AE$ASTDT])

has_aen <- !is.na(AE$AENDT) & !is.na(AE$TRTSDT)
AE$AENDY[has_aen & AE$AENDT >= AE$TRTSDT] <- as.numeric(AE$AENDT[has_aen & AE$AENDT >= AE$TRTSDT] - AE$TRTSDT[has_aen & AE$AENDT >= AE$TRTSDT]) + 1
AE$AENDY[has_aen & AE$TRTSDT > AE$AENDT] <- as.numeric(AE$AENDT[has_aen & AE$TRTSDT > AE$AENDT] - AE$TRTSDT[has_aen & AE$TRTSDT > AE$AENDT])

AE$ANLSTDY[has_ast] <- as.numeric(AE$ASTDT[has_ast] - AE$TRTSDT[has_ast]) + 1

AE$TRTEMFL <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$TRTSDT) & AE$ASTDT >= AE$TRTSDT, "Y", "N")

stopifnot(all(c("AEDECOD","AEBODSYS") %in% names(AE)))
cond_derm <- contains_fixed(AE$AEDECOD, "APPLICATION") |
  contains_fixed(AE$AEDECOD, "DERMATITIS") |
  contains_fixed(AE$AEDECOD, "ERYTHEMA") |
  contains_fixed(AE$AEDECOD, "BLISTER") |
  (!is.na(AE$AEBODSYS) & AE$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS" &
     !is.na(AE$AEDECOD) & !(AE$AEDECOD %in% c("COLD SWEAT", "HYPERHIDROSIS", "ALOPECIA")))
cond_derm[is.na(cond_derm)] <- FALSE
AE$CQ01NAM <- ifelse(cond_derm, "DERMATOLOGIC EVENTS", NA_character_)

# ---- flagfrst macro equivalent ----
flagfrst <- function(inds, sortvars, invar, outvar, ser = "", derm = "") {
  df <- inds
  stopifnot("TRTEMFL" %in% names(df))
  df <- df[!is.na(df$TRTEMFL) & df$TRTEMFL == "Y", , drop = FALSE]
  if (!is.null(invar) && invar != "") {
    stopifnot(invar %in% names(df))
    df <- df[is_not_blank(df[[invar]]), , drop = FALSE]
  }
  if (!is.null(ser) && ser != "") {
    stopifnot("AESER" %in% names(df))
    df <- df[!is.na(df$AESER) & df$AESER == "Y", , drop = FALSE]
  }
  if (!is.null(derm) && derm != "") {
    stopifnot("CQ01NAM" %in% names(df))
    df <- df[!is.na(df$CQ01NAM) & df$CQ01NAM == "DERMATOLOGIC EVENTS", , drop = FALSE]
  }
  if (nrow(df) == 0) {
    out0 <- data.frame(USUBJID = character(0), AESEQ = numeric(0), stringsAsFactors = FALSE)
    names(out0) <- c("USUBJID", "AESEQ")
    out0[[outvar]] <- character(0)
    return(out0[, c("USUBJID", outvar, "AESEQ"), drop = FALSE])
  }
  sv <- sortvars[sortvars != ""]
  o_usub <- ifelse(is.na(df$USUBJID), "", df$USUBJID)
  if (length(sv) == 0) {
    ord2 <- order(o_usub, df$ASTDT, df$AESEQ, na.last = FALSE, method = "radix")
  } else if (length(sv) == 1) {
    v1 <- df[[sv[1]]]
    if (is.character(v1)) {
      v1o <- ifelse(is.na(v1), "", v1)
    } else {
      v1o <- v1
    }
    ord2 <- order(o_usub, v1o, df$ASTDT, df$AESEQ, na.last = FALSE, method = "radix")
  } else if (length(sv) == 2) {
    v1 <- df[[sv[1]]]
    v2 <- df[[sv[2]]]
    if (is.character(v1)) {
      v1o <- ifelse(is.na(v1), "", v1)
    } else {
      v1o <- v1
    }
    if (is.character(v2)) {
      v2o <- ifelse(is.na(v2), "", v2)
    } else {
      v2o <- v2
    }
    ord2 <- order(o_usub, v1o, v2o, df$ASTDT, df$AESEQ, na.last = FALSE, method = "radix")
  } else {
    stop("too many sortvars")
  }
  df <- df[ord2, , drop = FALSE]
  gkeys <- c("USUBJID", sv)
  grp <- do.call(paste, c(lapply(gkeys, function(k) {
    v <- df[[k]]
    v2 <- ifelse(is.na(v), "<NA>", as.character(v))
    v2
  }), list(sep = "\x1e")))
  first_idx <- !duplicated(grp)
  ds02 <- df[first_idx, c("USUBJID", "AESEQ"), drop = FALSE]
  ds02[[outvar]] <- "Y"
  ds02 <- ds02[, c("USUBJID", outvar, "AESEQ"), drop = FALSE]
  ds02 <- ds02[order(ifelse(is.na(ds02$USUBJID), "", ds02$USUBJID), ds02$AESEQ,
                     na.last = FALSE, method = "radix"), , drop = FALSE]
  rownames(ds02) <- NULL
  ds02
}

AEA  <- flagfrst(AE, c(), "", "AOCCFL", ser = "", derm = "")
AEB  <- flagfrst(AE, c("AEBODSYS"), "AEBODSYS", "AOCCSFL", ser = "", derm = "")
AEC  <- flagfrst(AE, c("AEBODSYS","AEDECOD"), "AEDECOD", "AOCCPFL", ser = "", derm = "")
SAEA <- flagfrst(AE, c(), "", "AOCC02FL", ser = "Y", derm = "")
SAEB <- flagfrst(AE, c("AEBODSYS"), "AEBODSYS", "AOCC03FL", ser = "Y", derm = "")
SAEC <- flagfrst(AE, c("AEBODSYS","AEDECOD"), "AEDECOD", "AOCC04FL", ser = "Y", derm = "")
DERM <- flagfrst(AE, c(), "", "AOCC01FL", ser = "", derm = "Y")

AE <- AE[order(ifelse(is.na(AE$USUBJID), "", AE$USUBJID), AE$AESEQ,
               na.last = FALSE, method = "radix"), , drop = FALSE]

merge_flag <- function(base, flag) {
  cc <- intersect(names(base), names(flag))
  cc <- setdiff(cc, c("USUBJID", "AESEQ"))
  if (length(cc) > 0) flag <- flag[, setdiff(names(flag), cc), drop = FALSE]
  merge(base, flag, by = c("USUBJID", "AESEQ"), all.x = TRUE, sort = FALSE)
}

AE <- merge_flag(AE, AEA)
AE <- merge_flag(AE, AEB)
AE <- merge_flag(AE, AEC)
AE <- merge_flag(AE, SAEA)
AE <- merge_flag(AE, SAEB)
AE <- merge_flag(AE, SAEC)
AE <- merge_flag(AE, DERM)

AE <- AE[order(ifelse(is.na(AE$USUBJID), "", AE$USUBJID), AE$AESEQ,
               na.last = FALSE, method = "radix"), , drop = FALSE]

retain_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N",
  "RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT",
  "ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM",
  "AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD",
  "AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB",
  "AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN",
  "AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL",
  "AOCC04FL","CQ01NAM","AOCC01FL")

for (v in retain_order) {
  if (!(v %in% names(AE))) AE[[v]] <- NA
}
AE$TRTSDT <- to_sas_date(AE$TRTSDT)
AE$TRTEDT <- to_sas_date(AE$TRTEDT)
AE$ASTDT <- to_sas_date(AE$ASTDT)
AE$AENDT <- to_sas_date(AE$AENDT)

ADAE <- AE[, retain_order, drop = FALSE]
ADAE <- ADAE[order(ifelse(is.na(ADAE$USUBJID), "", ADAE$USUBJID), ADAE$AESEQ,
                   na.last = FALSE, method = "radix"), , drop = FALSE]
rownames(ADAE) <- NULL

write.csv(ADAE, file.path(out, "adae.csv"), row.names = FALSE, na = "")
