# P065_adlb_qc.sas -> R translation
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

read_sas_ds <- function(lib, ds) {
  if (!dir.exists(lib)) return(NULL)
  # list files case-insensitive
  fl <- list.files(lib, full.names = TRUE, ignore.case = FALSE)
  # try exact match ignoring case
  pat <- paste0("^", ds, "\\.(sas7bdat|xpt)$")
  idx <- grepl(pat, basename(fl), ignore.case = TRUE)
  cand <- fl[idx]
  # also try lower case name
  if (length(cand) == 0) return(NULL)
  f <- cand[1]
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  as.data.frame(df, stringsAsFactors = FALSE)
}

sdtm_lib <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_lib <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"

lb <- read_sas_ds(sdtm_lib, "lb")
adsl <- read_sas_ds(adam_lib, "adsl")

if (is.null(lb)) {
  lb <- read_sas_ds("D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw", "lb")
}
if (is.null(lb)) {
  search_roots <- c("D:/meta_hackathon/sas2r-agent/data/derived", "D:/meta_hackathon/sas2r-agent/data/raw")
  for (r in search_roots) {
    if (dir.exists(r)) {
      cand <- list.files(r, pattern = "^lb\\.sas7bdat$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand) > 0) { lb <- as.data.frame(haven::zap_labels(haven::read_sas(cand[1])), stringsAsFactors = FALSE); break }
      cand2 <- list.files(r, pattern = "^lb\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand2) > 0) { lb <- as.data.frame(haven::zap_labels(haven::read_xpt(cand2[1])), stringsAsFactors = FALSE); break }
    }
  }
}
if (is.null(adsl)) {
  search_roots <- c("D:/meta_hackathon/sas2r-agent/data/derived", "D:/meta_hackathon/sas2r-agent/data/raw")
  for (r in search_roots) {
    if (dir.exists(r)) {
      cand <- list.files(r, pattern = "^adsl\\.sas7bdat$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand) > 0) { adsl <- as.data.frame(haven::zap_labels(haven::read_sas(cand[1])), stringsAsFactors = FALSE); break }
      cand2 <- list.files(r, pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(cand2) > 0) { adsl <- as.data.frame(haven::zap_labels(haven::read_xpt(cand2[1])), stringsAsFactors = FALSE); break }
    }
  }
}

if (is.null(lb)) stop("sdtm.lb not found")
if (is.null(adsl)) stop("adam.adsl not found")

lb <- lb %>% mutate(across(where(is.labelled), haven::zap_labels)) %>% mutate(across(where(is.labelled), haven::zap_formats))
adsl <- adsl %>% mutate(across(where(is.labelled), haven::zap_labels)) %>% mutate(across(where(is.labelled), haven::zap_formats))

# proc sort data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
for (k in c("USUBJID","PARAM","LBDTC")) if (!k %in% names(lb)) lb[[k]] <- NA_character_
ord <- order(lb$USUBJID, lb$PARAM, lb$LBDTC, na.last = FALSE, method = "radix")
lb_qc <- lb[ord, , drop = FALSE]

# proc sort data=adam.adsl out=adsl_qc; by USUBJID;
if (!"USUBJID" %in% names(adsl)) stop("USUBJID not in adsl")
ord2 <- order(adsl$USUBJID, na.last = FALSE, method = "radix")
adsl_qc <- adsl[ord2, , drop = FALSE]

keep_adsl <- intersect(c("USUBJID","RFSTDTC","TRT01P"), names(adsl_qc))
adsl_qc_keep <- adsl_qc[, keep_adsl, drop = FALSE]

# merge lb_qc (in=a) adsl_qc (in=b) by USUBJID; if a and b;
adlb_qc <- inner_join(lb_qc, adsl_qc_keep, by = "USUBJID")

parse_yymmdd10 <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  x_chr <- as.character(x)
  x_chr <- trimws(x_chr)
  x10 <- substr(x_chr, 1, 10)
  suppressWarnings(as.Date(x10, format = "%Y-%m-%d"))
}

lbdt <- parse_yymmdd10(adlb_qc$LBDTC)
rfstdt <- parse_yymmdd10(adlb_qc$RFSTDTC)

if (!"AVAL" %in% names(adlb_qc)) adlb_qc$AVAL <- NA_real_
adlb_qc$AVAL <- suppressWarnings(as.numeric(adlb_qc$AVAL))

is_sas_lt <- function(a, b) {
  ifelse(is.na(a) & is.na(b), FALSE,
  ifelse(is.na(a) & !is.na(b), TRUE,
  ifelse(!is.na(a) & is.na(b), FALSE,
         a < b)))
}
is_sas_eq <- function(a, b) {
  ifelse(is.na(a) & is.na(b), TRUE,
  ifelse(is.na(a) | is.na(b), FALSE,
         a == b))
}
is_same_sas <- function(a, b) {
  ifelse(is.na(a) & is.na(b), TRUE,
  ifelse(is.na(a) | is.na(b), FALSE,
         a == b))
}

n <- nrow(adlb_qc)
BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep("", n)
CHG_vec <- rep(NA_real_, n)

# Re-apply SAS sort order to merged data: by USUBJID PARAM LBDTC
ord_m <- order(adlb_qc$USUBJID, adlb_qc$PARAM, adlb_qc$LBDTC, na.last = FALSE, method = "radix")
adlb_qc <- adlb_qc[ord_m, , drop = FALSE]
lbdt <- lbdt[ord_m]
rfstdt <- rfstdt[ord_m]

current_BASE <- NA_real_

for (i in seq_len(n)) {
  usubjid_i <- adlb_qc$USUBJID[i]
  param_i <- adlb_qc$PARAM[i]

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

  cond_lt <- is_sas_lt(lbdt_i, rfstdt_i)

  if (!is.na(cond_lt) && cond_lt && !is.na(aval_i)) {
    current_BASE <- aval_i
  }

  cond_eq <- is_sas_eq(aval_i, current_BASE)
  if (!is.na(cond_lt) && cond_lt && !is.na(cond_eq) && cond_eq) {
    ABLFL_vec[i] <- "Y"
  } else {
    ABLFL_vec[i] <- ""
  }

  BASE_vec[i] <- current_BASE

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

other_cols <- setdiff(names(adlb_qc), c("BASE","ABLFL","CHG"))
adlb_qc <- adlb_qc[, c(other_cols, "BASE","ABLFL","CHG")]

# Format Date/POSIX columns for CSV
for (cn in names(adlb_qc)) {
  if (inherits(adlb_qc[[cn]], "Date")) {
    adlb_qc[[cn]] <- format(adlb_qc[[cn]], "%Y-%m-%d")
  } else if (inherits(adlb_qc[[cn]], "POSIXt")) {
    adlb_qc[[cn]] <- format(adlb_qc[[cn]], "%Y-%m-%dT%H:%M:%S")
  }
}

write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")
cat("Wrote", nrow(adlb_qc), "rows to", file.path(out, "adlb_qc.csv"), "\n")
