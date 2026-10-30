```r
# P065_adlb_qc.sas -> R translation
# QC Program: adlb_qc.sas Purpose: Independent QC of ADLB

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (not needed here but kept for fidelity)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: read SAS dataset from libref directory (tries .sas7bdat, .xpt, lower case)
read_sas_ds <- function(lib, ds) {
  ds_low <- tolower(ds)
  candidates <- c(
    file.path(lib, paste0(ds, ".sas7bdat")),
    file.path(lib, paste0(ds_low, ".sas7bdat")),
    file.path(lib, paste0(ds, ".xpt")),
    file.path(lib, paste0(ds_low, ".xpt")),
    file.path(lib, paste0(ds, ".sas7bdat".toLower <- tolower(""))),
    file.path(lib, ds)
  )
  # also try listing directory
  if (dir.exists(lib)) {
    fl <- list.files(lib, full.names = TRUE, ignore.case = TRUE)
    # find file that matches ds name without extension
    m <- fl[grepl(paste0("^", ds, "\\."), basename(fl), ignore.case = TRUE)]
    if (length(m) > 0) candidates <- c(m, candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        df <- haven::read_xpt(f)
      } else if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
        df <- haven::read_sas(f)
      } else {
        # try both
        try_df <- try(haven::read_sas(f), silent = TRUE)
        if (!inherits(try_df, "try-error")) df <- try_df else next
      }
      # zap labels/formats to plain vectors
      df <- haven::zap_labels(df)
      df <- haven::zap_formats(df)
      return(as.data.frame(df, stringsAsFactors = FALSE))
    }
  }
  return(NULL)
}

# librefs as in family environment
sdtm_lib <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_lib <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"
# fallbacks via env vars
if (!dir.exists(sdtm_lib)) {
  alt <- Sys.getenv("RAWSdtm")
  if (alt != "" && dir.exists(alt)) sdtm_lib <- alt
}
if (!dir.exists(adam_lib)) {
  alt <- Sys.getenv("RAWADAM")
  if (alt != "" && dir.exists(alt)) adam_lib <- alt
}
# additional fallbacks: raw derived locations
if (!dir.exists(sdtm_lib)) {
  for (p in c("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm",
              "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
              "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm")) {
    if (dir.exists(p)) { sdtm_lib <- p; break }
  }
}
if (!dir.exists(adam_lib)) {
  for (p in c("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam",
              "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
              "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam")) {
    if (dir.exists(p)) { adam_lib <- p; break }
  }
}

lb <- read_sas_ds(sdtm_lib, "lb")
adsl <- read_sas_ds(adam_lib, "adsl")

# if not found, try alternative lib names (sdtm.lb may be under raw)
if (is.null(lb)) {
  lb <- read_sas_ds("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw", "lb")
}
if (is.null(lb)) {
  # search whole derived tree
  search_roots <- c("D:/meta_hackathon/sas2r-agent/data/derived",
                    "D:/meta_hackathon/sas2r-agent/data/raw")
  for (r in search_roots) {
    if (dir.exists(r)) {
      cand <- list.files(r, pattern = "^lb\\.sas7bdat$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand) > 0) { lb <- as.data.frame(haven::zap_labels(haven::read_sas(cand[1])), stringsAsFactors=FALSE); break }
      cand2 <- list.files(r, pattern = "^lb\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand2) > 0) { lb <- as.data.frame(haven::zap_labels(haven::read_xpt(cand2[1])), stringsAsFactors=FALSE); break }
    }
  }
}
if (is.null(adsl)) {
  search_roots <- c("D:/meta_hackathon/sas2r-agent/data/derived",
                    "D:/meta_hackathon/sas2r-agent/data/raw")
  for (r in search_roots) {
    if (dir.exists(r)) {
      cand <- list.files(r, pattern = "^adsl\\.sas7bdat$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand) > 0) { adsl <- as.data.frame(haven::zap_labels(haven::read_sas(cand[1])), stringsAsFactors=FALSE); break }
      cand2 <- list.files(r, pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand2) > 0) { adsl <- as.data.frame(haven::zap_labels(haven::read_xpt(cand2[1])), stringsAsFactors=FALSE); break }
    }
  }
}

if (is.null(lb)) stop("sdtm.lb not found")
if (is.null(adsl)) stop("adam.adsl not found")

# Ensure character columns are character (haven may return labelled)
lb <- lb %>% mutate(across(where(is.labelled), haven::zap_labels)) %>% mutate(across(where(is.labelled), haven::zap_formats))
adsl <- adsl %>% mutate(across(where(is.labelled), haven::zap_labels)) %>% mutate(across(where(is.labelled), haven::zap_formats))

# proc sort data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
# SAS sort: missing first, uppercase before lowercase (ASCII), case-sensitive
# Use base order with na.last=FALSE and radix for byte order where possible
# dplyr arrange with .locale="C" and na_last=FALSE mimics SAS
lb_qc <- lb
# ensure sort keys exist
for (k in c("USUBJID","PARAM","LBDTC")) if (!k %in% names(lb_qc)) lb_qc[[k]] <- NA_character_
# arrange
lb_qc <- lb_qc %>% arrange(USUBJID, PARAM, LBDTC, .locale = "C")
# haven/dplyr arrange puts NA last by default in older versions; enforce NA first via explicit ordering if needed
# fallback check: if any NA in sort keys, move them first
# (dplyr 1.1+ supports na_last param; we already rely on default but ensure)
# Use base order as definitive SAS-like order for reproducibility
ord <- order(lb_qc$USUBJID, lb_qc$PARAM, lb_qc$LBDTC, na.last = FALSE, method = "radix")
lb_qc <- lb_qc[ord, , drop = FALSE]

# proc sort data=adam.adsl out=adsl_qc; by USUBJID;
adsl_qc <- adsl
if (!"USUBJID" %in% names(adsl_qc)) stop("USUBJID not in adsl")
ord2 <- order(adsl_qc$USUBJID, na.last = FALSE, method = "radix")
adsl_qc <- adsl_qc[ord2, , drop = FALSE]

# keep only USUBJID RFSTDTC TRT01P from adsl_qc (as in SAS keep=)
keep_adsl <- intersect(c("USUBJID","RFSTDTC","TRT01P"), names(adsl_qc))
adsl_qc_keep <- adsl_qc[, keep_adsl, drop = FALSE]

# data adam.adlb_qc; merge lb_qc (in=a) adsl_qc (in=b keep=...) by USUBJID; if a and b;
# inner join on USUBJID (many-to-one)
adlb_qc <- inner_join(lb_qc, adsl_qc_keep, by = "USUBJID")

# lbdt = input(LBDTC, yymmdd10.); rfstdt = input(RFSTDTC, yymmdd10.);
parse_yymmdd10 <- function(x) {
  # x may be character, Date, numeric SAS date, POSIX
  if (is.null(x)) return(as.Date(rep(NA, 0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    # Heuristic: if values are large (e.g., > 30000) likely SAS dates; but also could be AVAL etc.
    # For LBDTC/RFSTDTC numeric, treat as SAS date
    return(as.Date(x, origin = "1960-01-01"))
  }
  # character: take first 10 chars, parse YYYY-MM-DD
  x_chr <- as.character(x)
  # trim
  x_chr <- trimws(x_chr)
  # substr 1,10
  x10 <- substr(x_chr, 1, 10)
  # parse; invalid -> NA
  suppressWarnings(as.Date(x10, format = "%Y-%m-%d"))
}

lbdt <- parse_yymmdd10(adlb_qc$LBDTC)
rfstdt <- parse_yymmdd10(adlb_qc$RFSTDTC)

# Ensure AVAL exists and is numeric
if (!"AVAL" %in% names(adlb_qc)) adlb_qc$AVAL <- NA_real_
adlb_qc$AVAL <- suppressWarnings(as.numeric(adlb_qc$AVAL))

# SAS semantics helpers
is_sas_lt <- function(a, b) {
  # missing (NA) is smallest: NA < non-NA => TRUE, NA < NA => FALSE, non-NA < NA => FALSE
  ifelse(is.na(a) & is.na(b), FALSE,
  ifelse(is.na(a) & !is.na(b), TRUE,
  ifelse(!is.na(a) & is.na(b), FALSE,
         a < b)))
}
is_sas_eq <- function(a, b) {
  # missing == missing => TRUE in SAS
  ifelse(is.na(a) & is.na(b), TRUE,
  ifelse(is.na(a) | is.na(b), FALSE,
         a == b))
}
is_same_sas <- function(a, b) {
  # for BY group comparison: NA == NA true
  ifelse(is.na(a) & is.na(b), TRUE,
  ifelse(is.na(a) | is.na(b), FALSE,
         a == b))
}

# retain BASE; if first.PARAM then BASE = .;
# SAS BY is USUBJID only, but FIRST.PARAM implies grouping by USUBJID+PARAM
# Implement as reset at first observation of each USUBJID+PARAM group in sorted order
# Already sorted by USUBJID PARAM LBDTC, so group by USUBJID, PARAM

n <- nrow(adlb_qc)
BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep("", n)
CHG_vec <- rep(NA_real_, n)

# iterate in sorted order (adlb_qc is already sorted by USUBJID PARAM LBDTC via lb_qc order,
# but inner_join may have reordered; re-sort to guarantee SAS order)
# Re-apply SAS sort order to merged data: by USUBJID PARAM LBDTC
ord_m <- order(adlb_qc$USUBJID, adlb_qc$PARAM, adlb_qc$LBDTC, na.last = FALSE, method = "radix")
adlb_qc <- adlb_qc[ord_m, , drop = FALSE]
lbdt <- lbdt[ord_m]
rfstdt <- rfstdt[ord_m]

current_BASE <- NA_real_

for (i in seq_len(n)) {
  usubjid_i <- adlb_qc$USUBJID[i]
  param_i <- adlb_qc$PARAM[i]

  # determine first.PARAM
  is_first_param <- FALSE
  if (i == 1) {
    is_first_param <- TRUE
  } else {
    usubjid_prev <- adlb_qc$USUBJID[i-1]
    param_prev <- adlb_qc$PARAM[i-1]
    same_usubjid <- is_same_sas(usubjid_i, usubjid_prev)
    same_param <- is_same_sas(param_i, param_prev)
    if (!same_usubjid || !same_param) is_first_param <- TRUE
  }
  if (is_first_param) {
    current_BASE <- NA_real_
  }

  aval_i <- adlb_qc$AVAL[i]
  lbdt_i <- lbdt[i]
  rfstdt_i <- rfstdt[i]

  # if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL;
  cond_lt <- is_sas_lt(lbdt_i, rfstdt_i)
  # handle scalar NA in ifelse vectorized helper returns vector length 1
  # is_sas_lt returns logical vector; for single values it's length 1
  if (!is.na(cond_lt) && cond_lt && !is.na(aval_i)) {
    current_BASE <- aval_i
  }

  # if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y"; else ABLFL = "";
  cond_eq <- is_sas_eq(aval_i, current_BASE)
  if (!is.na(cond_lt) && cond_lt && !is.na(cond_eq) && cond_eq) {
    ABLFL_vec[i] <- "Y"
  } else {
    ABLFL_vec[i] <- ""
  }

  BASE_vec[i] <- current_BASE

  # if not missing(BASE) then CHG = AVAL - BASE;
  if (!is.na(current_BASE)) {
    if (!is.na(aval_i)) {
      CHG_vec[i] <- aval_i - current_BASE
    } else {
      CHG_vec[i] <- NA_real_
    }
  } else {
    CHG_vec[i] <- NA_real_
  }
}

adlb_qc$BASE <- BASE_vec
adlb_qc$ABLFL <- ABLFL_vec
adlb_qc$CHG <- CHG_vec

# drop lbdt rfstdt (they were temporary, not in dataset)
# Note: lbdt/rfstdt were not added as columns, so nothing to drop

# Ensure column order: original lb_qc columns, then RFSTDTC, TRT01P (if not already), then BASE, ABLFL, CHG
# inner_join already has that order; ensure BASE, ABLFL, CHG at end
# Move them to end if not already
other_cols <- setdiff(names(adlb_qc), c("BASE","ABLFL","CHG"))
adlb_qc <- adlb_qc[, c(other_cols, "BASE","ABLFL","CHG")]

# Write dataset as CSV: dates as YYYY-MM-DD, datetimes as YYYY-MM-DDTHH:MM:SS
# Format Date/POSIX columns
for (cn in names(adlb_qc)) {
  if (inherits(adlb_qc[[cn]], "Date")) {
    adlb_qc[[cn]] <- format(adlb_qc[[cn]], "%Y-%m-%d")
  } else if (inherits(adlb_qc[[cn]], "POSIXt")) {
    adlb_qc[[cn]] <- format(adlb_qc[[cn]], "%Y-%m-%dT%H:%M:%S")
  }
}

# write OUT_DIR/adlb_qc.csv with na = ""
write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")

cat("Wrote", nrow(adlb_qc), "rows to", file.path(out, "adlb_qc.csv"), "\n")
```