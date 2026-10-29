```r
# P002_adae.sas -> R translation
# Output: adae.csv
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

root_dir <- "D:/meta_hackathon/sas2r-agent"
sdtm_derived_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm"
adam_derived_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

cat("Loading inputs...\n")

load_ds <- function(derived_dir, raw_dir, ds) {
  cands <- c(
    file.path(derived_dir, paste0(ds, ".sas7bdat")),
    file.path(derived_dir, paste0(tolower(ds), ".sas7bdat")),
    file.path(derived_dir, paste0(toupper(ds), ".sas7bdat")),
    file.path(derived_dir, paste0(ds, ".xpt")),
    file.path(derived_dir, paste0(tolower(ds), ".xpt")),
    file.path(derived_dir, paste0(toupper(ds), ".xpt")),
    file.path(raw_dir, paste0(ds, ".xpt")),
    file.path(raw_dir, paste0(tolower(ds), ".xpt")),
    file.path(raw_dir, paste0(toupper(ds), ".xpt"))
  )
  for (f in cands) {
    if (file.exists(f)) {
      cat("Reading:", f, "\n")
      if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
        df <- haven::read_sas(f)
      } else {
        df <- haven::read_xpt(f)
      }
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      names(df) <- toupper(names(df))
      return(df)
    }
  }
  # fallback: list files case-insensitive
  for (d in c(derived_dir, raw_dir)) {
    if (dir.exists(d)) {
      fl <- list.files(d, full.names = TRUE)
      idx <- which(tolower(basename(tools::file_path_sans_ext(fl))) == tolower(ds))
      if (length(idx) > 0) {
        f <- fl[idx[1]]
        cat("Reading:", f, "\n")
        if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) df <- haven::read_sas(f)
        else df <- haven::read_xpt(f)
        df <- as.data.frame(df, stringsAsFactors = FALSE)
        names(df) <- toupper(names(df))
        return(df)
      }
    }
  }
  stop(paste0("Dataset not found: ", ds))
}

AE_in <- load_ds(sdtm_derived_dir, rawsdtm_dir, "ae")
ADSL_in <- load_ds(adam_derived_dir, rawadam_dir, "adsl")

# helper: SAS ordering (missing first, char byte order via radix)
sas_order_df <- function(df, cols) {
  ord_list <- list()
  for (cl in cols) {
    v <- df[[cl]]
    if (is.character(v)) {
      v2 <- v
      v2[is.na(v2)] <- ""
      ord_list[[cl]] <- v2
    } else {
      ord_list[[cl]] <- v
    }
  }
  args <- c(ord_list, list(na.last = FALSE, method = "radix"))
  idx <- do.call(order, args)
  idx
}

# helper: SAS yymmdd10. parse (?? suppress error -> NA)
parse_yymmdd10_vec <- function(x) {
  sapply(x, function(one) {
    if (is.na(one)) return(NA_real_)
    s <- trimws(as.character(one))
    if (is.na(s) || s == "") return(NA_real_)
    if (nchar(s) < 10) return(NA_real_)
    s10 <- substr(s, 1, 10)
    # normalize separators to -
    s10n <- gsub("/", "-", s10, fixed = FALSE)
    s10n <- gsub(" ", "-", s10n, fixed = TRUE)
    s10n <- gsub("\\.", "-", s10n)
    if (!grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s10n)) return(NA_real_)
    d <- suppressWarnings(as.Date(s10n, format = "%Y-%m-%d"))
    if (is.na(d)) return(NA_real_)
    as.numeric(d)
  }, USE.NAMES = FALSE)
}

# helper: convert SAS numeric date to Date if needed
ensure_date <- function(v) {
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    return(as.Date(v, origin = "1960-01-01"))
  }
  if (is.character(v)) {
    d <- suppressWarnings(as.Date(v))
    return(d)
  }
  v
}

cat("Step 1: sort AE and derive ASTDT/AENDT/ADUR...\n")
# proc sort by USUBJID AESEQ
idx <- sas_order_df(AE_in, c("USUBJID", "AESEQ"))
AE <- AE_in[idx, , drop = FALSE]
rownames(AE) <- NULL

# ASTDT / AENDT
AE$ASTDT <- as.Date(parse_yymmdd10_vec(AE$AESTDTC), origin = "1970-01-01")
AE$AENDT <- as.Date(parse_yymmdd10_vec(AE$AEENDTC), origin = "1970-01-01")
# ADUR if both non-missing
AE$ADUR <- ifelse(!is.na(AE$ASTDT) & !is.na(AE$AENDT),
                  as.numeric(AE$AENDT - AE$ASTDT) + 1, NA_real_)

cat("Step 2: TEMP from ADSL...\n")
keep_adsl <- c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N",
               "RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT")
keep_adsl <- keep_adsl[keep_adsl %in% names(ADSL_in)]
TEMP <- ADSL_in[, keep_adsl, drop = FALSE]
# rename TRT01A->TRTA, TRT01AN->TRTAN
if ("TRT01A" %in% names(TEMP)) names(TEMP)[names(TEMP)=="TRT01A"] <- "TRTA"
if ("TRT01AN" %in% names(TEMP)) names(TEMP)[names(TEMP)=="TRT01AN"] <- "TRTAN"
# ensure dates are Date
if ("TRTSDT" %in% names(TEMP)) TEMP$TRTSDT <- ensure_date(TEMP$TRTSDT)
if ("TRTEDT" %in% names(TEMP)) TEMP$TRTEDT <- ensure_date(TEMP$TRTEDT)
idx2 <- sas_order_df(TEMP, c("USUBJID"))
TEMP <- TEMP[idx2, , drop = FALSE]
rownames(TEMP) <- NULL

cat("Step 3: merge AE+TEMP and derive study-day vars...\n")
# left join by USUBJID (SAS merge)
AE <- dplyr::left_join(AE, TEMP, by = "USUBJID")
# SAS merge output sorted by USUBJID; re-sort by USUBJID AESEQ to mimic
idx3 <- sas_order_df(AE, c("USUBJID","AESEQ"))
AE <- AE[idx3, , drop = FALSE]
rownames(AE) <- NULL

# imputation when ASTDT missing and length(AESTDTC)=7
# SAS LENGTH strips trailing blanks
sas_len <- function(x) {
  sapply(x, function(one) {
    if (is.na(one)) return(NA_integer_)
    s <- as.character(one)
    s <- sub(" +$", "", s, perl = TRUE)
    nchar(s, type = "chars")
  }, USE.NAMES = FALSE)
}
if (!"ASTDTF" %in% names(AE)) AE$ASTDTF <- NA_character_
len7 <- sas_len(AE$AESTDTC)
need_imp <- is.na(AE$ASTDT) & !is.na(len7) & len7 == 7L
need_imp[is.na(need_imp)] <- FALSE
if (any(need_imp)) {
  raw_imp <- paste0(as.character(AE$AESTDTC[need_imp]), "-01")
  # COMPRESS removes blanks
  raw_imp <- gsub(" ", "", raw_imp, fixed = TRUE)
  imp_num <- parse_yymmdd10_vec(raw_imp)
  AE$ASTDT[need_imp] <- as.Date(imp_num, origin = "1970-01-01")
  AE$ASTDTF[need_imp] <- "D"
}
# ensure character NA for others stays NA
AE$ASTDTF[is.na(AE$ASTDTF) | AE$ASTDTF==""] <- NA_character_
# CQ01NAM length $25
if (!"CQ01NAM" %in% names(AE)) AE$CQ01NAM <- NA_character_ else AE$CQ01NAM <- as.character(AE$CQ01NAM)

# ensure date types
AE$ASTDT <- ensure_date(AE$ASTDT)
AE$AENDT <- ensure_date(AE$AENDT)
if ("TRTSDT" %in% names(AE)) AE$TRTSDT <- ensure_date(AE$TRTSDT)
if ("TRTEDT" %in% names(AE)) AE$TRTEDT <- ensure_date(AE$TRTEDT)

# ASTDY / AENDY / ANLSTDY / TRTEMFL
n <- nrow(AE)
AE$ASTDY <- NA_real_
AE$AENDY <- NA_real_
AE$ANLSTDY <- NA_real_
AE$TRTEMFL <- NA_character_
for (i in seq_len(n)) {
  astdt <- AE$ASTDT[i]; aendt <- AE$AENDT[i]; tsdt <- AE$TRTSDT[i]
  astdt_nmiss <- !is.na(astdt); aendt_nmiss <- !is.na(aendt); tsdt_nmiss <- !is.na(tsdt)
  # ASTDY
  if (astdt_nmiss && tsdt_nmiss && astdt >= tsdt) {
    AE$ASTDY[i] <- as.numeric(astdt - tsdt) + 1
  } else if (tsdt_nmiss && astdt_nmiss && tsdt > astdt) {
    AE$ASTDY[i] <- as.numeric(astdt - tsdt)
  }
  # AENDY
  if (aendt_nmiss && tsdt_nmiss && aendt >= tsdt) {
    AE$AENDY[i] <- as.numeric(aendt - tsdt) + 1
  } else if (tsdt_nmiss && aendt_nmiss && tsdt > aendt) {
    AE$AENDY[i] <- as.numeric(aendt - tsdt)
  }
  # ANLSTDY
  if (astdt_nmiss && tsdt_nmiss) {
    AE$ANLSTDY[i] <- as.numeric(astdt - tsdt) + 1
  }
  # TRTEMFL
  if (astdt_nmiss && tsdt_nmiss && astdt >= tsdt) AE$TRTEMFL[i] <- "Y"
  else AE$TRTEMFL[i] <- "N"
}
# CQ01NAM dermatologic
aedecod <- as.character(AE$AEDECOD)
aebodsys <- as.character(AE$AEBODSYS)
has_app <- !is.na(aedecod) & grepl("APPLICATION", aedecod, fixed = TRUE)
has_derm <- !is.na(aedecod) & grepl("DERMATITIS", aedecod, fixed = TRUE)
has_ery <- !is.na(aedecod) & grepl("ERYTHEMA", aedecod, fixed = TRUE)
has_bli <- !is.na(aedecod) & grepl("BLISTER", aedecod, fixed = TRUE)
skin_match <- !is.na(aebodsys) & aebodsys == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
not_in_list <- !(aedecod %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA"))
not_in_list[is.na(not_in_list)] <- FALSE
# NOTE: for NA aedecod, %in% gives FALSE so !(FALSE)=TRUE; keep TRUE to mimic SAS
# Recompute correctly: R's NA %in% vector -> FALSE, so !FALSE=TRUE already.
# The line above overwrote NA case; fix:
na_dec <- is.na(aedecod)
not_in_list[na_dec] <- TRUE
skin_cond <- skin_match & not_in_list
skin_cond[is.na(skin_cond)] <- FALSE
cq <- has_app | has_derm | has_ery | has_bli | skin_cond
cq[is.na(cq)] <- FALSE
AE$CQ01NAM <- ifelse(cq, "DERMATOLOGIC EVENTS", NA_character_)

cat("Step 4: first-occurrence flags...\n")
flagfrst <- function(inds, sortvars, invar, outvar, ser=FALSE, derm=FALSE) {
  df <- inds
  # where trtemfl='Y'
  df <- df[!is.na(df$TRTEMFL) & df$TRTEMFL == "Y", , drop = FALSE]
  if (invar != "") {
    inv <- df[[invar]]
    keep <- !is.na(inv) & inv != ""
    df <- df[keep, , drop = FALSE]
  }
  if (ser) {
    keep <- !is.na(df$AESER) & df$AESER == "Y"
    df <- df[keep, , drop = FALSE]
  }
  if (derm) {
    keep <- !is.na(df$CQ01NAM) & df$CQ01NAM == "DERMATOLOGIC EVENTS"
    df <- df[keep, , drop = FALSE]
  }
  if (nrow(df) == 0) {
    res <- data.frame(USUBJID = character(0), AESEQ = numeric(0), stringsAsFactors = FALSE)
    res[[outvar]] <- character(0)
    return(res)
  }
  by_cols <- c("USUBJID", sortvars, "ASTDT")
  by_cols <- by_cols[by_cols != ""]
  ord_idx <- sas_order_df(df, c(by_cols, "AESEQ"))
  df <- df[ord_idx, , drop = FALSE]
  if (length(sortvars) == 0 && invar == "") {
    first_idx <- !duplicated(df$USUBJID, incomparables = NA)
    # duplicated with incomparables=NA treats NA as distinct; we want first NA kept, rest dup
    # base duplicated without incomparables treats NA as equal (first FALSE rest TRUE) which matches SAS grouping
    first_idx <- !duplicated(df$USUBJID)
  } else {
    grp_cols <- c("USUBJID", sortvars)
    key_df <- df[, grp_cols, drop = FALSE]
    first_idx <- !duplicated(key_df)
  }
  ds02 <- df[first_idx, c("USUBJID","AESEQ"), drop = FALSE]
  ds02[[outvar]] <- "Y"
  ord2 <- sas_order_df(ds02, c("USUBJID","AESEQ"))
  ds02 <- ds02[ord2, , drop = FALSE]
  rownames(ds02) <- NULL
  ds02
}

aea  <- flagfrst(AE, c(), "", "AOCCFL", ser=FALSE, derm=FALSE)
aeb  <- flagfrst(AE, c("AEBODSYS"), "AEBODSYS", "AOCCSFL", ser=FALSE, derm=FALSE)
aec  <- flagfrst(AE, c("AEBODSYS","AEDECOD"), "AEDECOD", "AOCCPFL", ser=FALSE, derm=FALSE)
saea <- flagfrst(AE, c(), "", "AOCC02FL", ser=TRUE, derm=FALSE)
saeb <- flagfrst(AE, c("AEBODSYS"), "AEBODSYS", "AOCC03FL", ser=TRUE, derm=FALSE)
saec <- flagfrst(AE, c("AEBODSYS","AEDECOD"), "AEDECOD", "AOCC04FL", ser=TRUE, derm=FALSE)
derm <- flagfrst(AE, c(), "", "AOCC01FL", ser=FALSE, derm=TRUE)

cat("Step 5: merge flags...\n")
idxs <- sas_order_df(AE, c("USUBJID","AESEQ"))
AE <- AE[idxs, , drop = FALSE]
rownames(AE) <- NULL
AE <- dplyr::left_join(AE, aea, by = c("USUBJID","AESEQ"))
AE <- dplyr::left_join(AE, aeb, by = c("USUBJID","AESEQ"))
AE <- dplyr::left_join(AE, aec, by = c("USUBJID","AESEQ"))
AE <- dplyr::left_join(AE, saea, by = c("USUBJID","AESEQ"))
AE <- dplyr::left_join(AE, saeb, by = c("USUBJID","AESEQ"))
AE <- dplyr::left_join(AE, saec, by = c("USUBJID","AESEQ"))
AE <- dplyr::left_join(AE, derm, by = c("USUBJID","AESEQ"))

cat("Step 6: retain order and final ADAE...\n")
final_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N",
  "RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT","ASTDT","ASTDTF","ASTDY","AENDT",
  "AENDY","ADUR","AETERM","AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD",
  "AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG",
  "AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN","AEOUT","AESEQ",
  "TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")

# ensure all final cols exist
for (cl in final_order) {
  if (!cl %in% names(AE)) AE[[cl]] <- NA
}
# ensure date types
AE$TRTSDT <- ensure_date(AE$TRTSDT)
AE$TRTEDT <- ensure_date(AE$TRTEDT)
AE$ASTDT <- ensure_date(AE$ASTDT)
AE$AENDT <- ensure_date(AE$AENDT)

ADAE <- AE[, final_order, drop = FALSE]
idxf <- sas_order_df(ADAE, c("USUBJID","AESEQ"))
ADAE <- ADAE[idxf, , drop = FALSE]
rownames(ADAE) <- NULL

cat("Writing adae.csv...\n")
write.csv(ADAE, file.path(out, "adae.csv"), row.names = FALSE, na = "")
cat("Done. Rows:", nrow(ADAE), "Cols:", ncol(ADAE), "\n")
```