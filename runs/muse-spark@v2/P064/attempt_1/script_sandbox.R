out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))

sdtm_dir <- "/bench/data/derived/wxzhai/sdtm"
adam_dir <- "/bench/data/derived/wxzhai/adam"
rawadam_dir <- "/bench/data/raw/cdisc-pilot/adam"
rawsdtm_dir <- "/bench/data/raw/cdisc-pilot/sdtm"

read_mem <- function(dirs, mem) {
  mem_low <- tolower(mem)
  for (d in dirs) {
    if (!dir.exists(d)) next
    fls <- list.files(d, full.names = TRUE, recursive = FALSE)
    if (length(fls) == 0) next
    # exact basename match case-insensitive without extension
    for (f in fls) {
      bn <- basename(f)
      # strip extension
      noext <- sub("\\.[^.]*$", "", bn)
      if (tolower(noext) == mem_low) {
        ext <- tolower(sub("^.*(\\.[^.]*)$", "\\1", bn))
        if (ext == ".sas7bdat") {
          df <- haven::read_sas(f)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        } else if (ext == ".xpt") {
          df <- haven::read_xpt(f)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        } else if (ext == ".csv") {
          df <- readr::read_csv(f, show_col_types = FALSE, guess_max = 100000)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        } else if (ext %in% c(".xls", ".xlsx")) {
          df <- readxl::read_excel(f)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        } else if (ext == ".rds") {
          df <- readRDS(f)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        }
      }
    }
    # also try direct file names
    cand1 <- file.path(d, paste0(mem_low, ".sas7bdat"))
    cand2 <- file.path(d, paste0(mem_low, ".xpt"))
    cand3 <- file.path(d, paste0(mem_low, ".csv"))
    # case variations: try upper
    candU1 <- file.path(d, paste0(toupper(mem), ".sas7bdat"))
    for (cc in c(cand1, cand2, cand3, candU1)) {
      if (file.exists(cc)) {
        ext <- tolower(sub("^.*(\\.[^.]*)$", "\\1", cc))
        if (ext == ".sas7bdat") {
          df <- haven::read_sas(cc)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        } else if (ext == ".xpt") {
          df <- haven::read_xpt(cc)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        } else if (ext == ".csv") {
          df <- readr::read_csv(cc, show_col_types = FALSE, guess_max = 100000)
          return(as.data.frame(df, stringsAsFactors = FALSE))
        }
      }
    }
  }
  # try XPORT engine single-file libref case: dir path is actually a file
  for (d in dirs) {
    if (file.exists(d) && !dir.exists(d)) {
      ext <- tolower(sub("^.*(\\.[^.]*)$", "\\1", d))
      if (ext == ".xpt") {
        df <- haven::read_xpt(d)
        return(as.data.frame(df, stringsAsFactors = FALSE))
      }
    }
  }
  return(NULL)
}

strip_haven <- function(df) {
  # remove haven labels/formats, convert labelled to plain vectors
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      # zap labels but keep values
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
      df[[nm]] <- as.vector(x)
    } else if (inherits(x, "haven_labelled")) {
      df[[nm]] <- as.vector(x)
    }
    # convert factors to character (SAS chars)
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

lb <- read_mem(c(sdtm_dir, rawsdtm_dir, "/bench/data/derived/wxzhai/raw"), "lb")
adsl <- read_mem(c(adam_dir, rawadam_dir, "/bench/data/derived/wxzhai/raw"), "adsl")

if (is.null(lb)) stop("sdtm.lb not found")
if (is.null(adsl)) stop("adam.adsl not found")

lb <- strip_haven(lb)
adsl <- strip_haven(adsl)

names(lb) <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))

message(paste0("LB rows=", nrow(lb), " cols=", paste(names(lb), collapse=",")))
message(paste0("ADSL rows=", nrow(adsl), " cols=", paste(names(adsl), collapse=",")))

stopifnot(all(c("USUBJID","PARAM","LBDTC","AVAL") %in% names(lb)))
stopifnot(all(c("USUBJID","TRT01P","RFSTDTC") %in% names(adsl)))

# Keep only needed cols from ADSL (USUBJID TRT01P RFSTDTC)
adsl_keep <- adsl[, c("USUBJID","TRT01P","RFSTDTC"), drop = FALSE]

# Before merging, drop or rename every column present in both tables except keys
keys <- c("USUBJID")
common <- intersect(names(lb), names(adsl_keep))
common <- setdiff(common, keys)
if (length(common) > 0) {
  # drop from lb side (adsl authoritative for TRT01P/RFSTDTC)
  lb <- lb[, setdiff(names(lb), common), drop = FALSE]
}

# SAS sorts with missing first, uppercase before lowercase (C locale set above)
# Sort lb by USUBJID PARAM LBDTC
ord_lb <- order(lb$USUBJID, lb$PARAM, lb$LBDTC, na.last = FALSE, method = "radix")
lb_sorted <- lb[ord_lb, , drop = FALSE]
ord_adsl <- order(adsl_keep$USUBJID, na.last = FALSE, method = "radix")
adsl_sorted <- adsl_keep[ord_adsl, , drop = FALSE]

# Merge by USUBJID, if a and b (inner join)
merged <- merge(lb_sorted, adsl_sorted, by = "USUBJID", all = FALSE, sort = FALSE)
stopifnot(all(c("USUBJID","PARAM","LBDTC","AVAL","TRT01P","RFSTDTC") %in% names(merged)))
if (nrow(merged) == 0) stop("merged result has 0 rows; join failed")

# Re-sort merged by USUBJID PARAM LBDTC to mimic DATA step input order after merge-by-USUBJID
# SAS merge by USUBJID preserves order within each USUBJID as in sorted lb (since adsl one per subject)
# and overall USUBJID order. Since lb_sorted already USUBJID PARAM LBDTC, merged should retain that
# but re-order explicitly to be safe.
ord_m <- order(merged$USUBJID, merged$PARAM, merged$LBDTC, na.last = FALSE, method = "radix")
df <- merged[ord_m, , drop = FALSE]
rownames(df) <- NULL

# Ensure character types for date strings
df$LBDTC <- as.character(df$LBDTC)
df$RFSTDTC <- as.character(df$RFSTDTC)
# AVAL numeric
df$AVAL <- suppressWarnings(as.numeric(df$AVAL))

# SAS INPUT(LBDTC, yymmdd10.) : strict YYYY-MM-DD, first 10 chars
sas_yymmdd10 <- function(x) {
  # x character vector; return Date vector
  s <- ifelse(is.na(x), NA_character_, substr(x, 1, 10))
  # ONE explicit strict format
  suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
}

lbdt <- sas_yymmdd10(df$LBDTC)
rfdt <- sas_yymmdd10(df$RFSTDTC)

# SAS numeric missing < any non-missing
sas_lt_date <- function(a, b) {
  # a,b Date vectors; elementwise SAS < : missing sorts first
  res <- rep(FALSE, length(a))
  for (i in seq_along(a)) {
    ai <- a[i]; bi <- b[i]
    na_a <- is.na(ai); na_b <- is.na(bi)
    if (na_a && na_b) res[i] <- FALSE
    else if (na_a && !na_b) res[i] <- TRUE
    else if (!na_a && na_b) res[i] <- FALSE
    else res[i] <- (ai < bi)
  }
  res
}
sas_num_eq <- function(a, b) {
  # SAS: . = . TRUE, . = x FALSE, x = . FALSE
  res <- rep(FALSE, length(a))
  for (i in seq_along(a)) {
    ai <- a[i]; bi <- b[i]
    na_a <- is.na(ai); na_b <- is.na(bi)
    if (na_a && na_b) res[i] <- TRUE
    else if (na_a || na_b) res[i] <- FALSE
    else res[i] <- (ai == bi)
  }
  res
}
is_missing_char <- function(x) {
  is.na(x) | x == ""
}
sas_char_eq <- function(a, b) {
  na_a <- is_missing_char(a)
  na_b <- is_missing_char(b)
  if (na_a && na_b) return(TRUE)
  if (na_a || na_b) return(FALSE)
  return(a == b)
}

n <- nrow(df)
BASE_vec <- rep(NA_real_, n)
ABLFL_vec <- rep("", n)
CHG_vec <- rep(NA_real_, n)

BASE_retain <- NA_real_
prev_usubjid <- NA_character_
prev_param <- NA_character_
first_row <- TRUE

aval_vec <- df$AVAL
lt_vec <- sas_lt_date(lbdt, rfdt)

for (i in seq_len(n)) {
  cur_usub <- df$USUBJID[i]
  cur_param <- df$PARAM[i]
  is_first_param <- FALSE
  if (first_row) {
    is_first_param <- TRUE
  } else {
    if (!sas_char_eq(cur_usub, prev_usubjid) || !sas_char_eq(cur_param, prev_param)) {
      is_first_param <- TRUE
    }
  }
  if (is_first_param) {
    BASE_retain <- NA_real_
  }
  # Baseline = last non-missing pre-dose value
  # if LBDTC < RFSTDTC and not missing(AVAL) then BASE = AVAL
  cond_base <- isTRUE(lt_vec[i]) && !is.na(aval_vec[i])
  if (cond_base) {
    BASE_retain <- aval_vec[i]
  }
  BASE_vec[i] <- BASE_retain
  # Baseline flag
  # if LBDTC < RFSTDTC and AVAL = BASE then ABLFL="Y" else ""
  cond_flag <- isTRUE(lt_vec[i]) && isTRUE(sas_num_eq(aval_vec[i], BASE_retain)[1])
  # sas_num_eq above called per element; simpler inline:
  # recompute correctly for scalar
  ai <- aval_vec[i]; bi <- BASE_retain
  eq <- FALSE
  if (is.na(ai) && is.na(bi)) eq <- TRUE
  else if (is.na(ai) || is.na(bi)) eq <- FALSE
  else eq <- (ai == bi)
  cond_flag <- isTRUE(lt_vec[i]) && eq
  if (cond_flag) ABLFL_vec[i] <- "Y" else ABLFL_vec[i] <- ""
  # Change from baseline
  if (is.na(ai) || is.na(BASE_retain)) CHG_vec[i] <- NA_real_
  else CHG_vec[i] <- ai - BASE_retain

  prev_usubjid <- cur_usub
  prev_param <- cur_param
  first_row <- FALSE
}

df$BASE <- BASE_vec
df$ABLFL <- ABLFL_vec
df$CHG <- CHG_vec

# drop LBDTC RFSTDTC
df$LBDTC <- NULL
df$RFSTDTC <- NULL

# Format dates/datetimes for CSV: dates YYYY-MM-DD, datetimes YYYY-MM-DDTHH:MM:SS
for (nm in names(df)) {
  x <- df[[nm]]
  if (inherits(x, "Date")) {
    df[[nm]] <- format(x, "%Y-%m-%d")
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    df[[nm]] <- format(as.POSIXct(x, tz = "UTC"), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  } else if (inherits(x, "difftime")) {
    df[[nm]] <- as.numeric(x)
  }
}

write.csv(df, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
message(paste0("Wrote adlb.csv rows=", nrow(df)))
