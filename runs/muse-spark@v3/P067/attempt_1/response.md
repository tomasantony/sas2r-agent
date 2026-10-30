```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

raw_dir  <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"

find_mem_file <- function(libdir, mem) {
  # try direct names first
  cands <- c(
    file.path(libdir, paste0(mem, ".sas7bdat")),
    file.path(libdir, paste0(tolower(mem), ".sas7bdat")),
    file.path(libdir, paste0(toupper(mem), ".sas7bdat")),
    file.path(libdir, paste0(mem, ".xpt")),
    file.path(libdir, paste0(tolower(mem), ".xpt")),
    file.path(libdir, paste0(toupper(mem), ".xpt")),
    file.path(libdir, paste0(mem, ".csv")),
    file.path(libdir, paste0(tolower(mem), ".csv"))
  )
  for (p in cands) {
    if (file.exists(p)) return(p)
  }
  # case-insensitive search in directory
  if (dir.exists(libdir)) {
    fs <- list.files(libdir, full.names = TRUE)
    # match basename without extension case-insensitive
    for (f in fs) {
      bn <- basename(f)
      # strip extension
      noext <- sub("\\.[^.]*$", "", bn)
      if (tolower(noext) == tolower(mem)) return(f)
    }
  }
  return(NA_character_)
}

read_lib_mem <- function(libdir, mem) {
  f <- find_mem_file(libdir, mem)
  if (is.na(f)) stop(paste0("Cannot find dataset ", mem, " in ", libdir))
  message(paste0("Reading ", mem, " from ", f))
  ext <- tolower(sub(".*\\.([^.]*)$", "\\1", basename(f)))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
    df <- as.data.frame(df)
  } else {
    # try sas then xpt
    tryCatch({
      df <- haven::read_sas(f)
    }, error = function(e) {
      df <<- haven::read_xpt(f)
    })
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap labels/formats but keep Date conversion already done by haven
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "labelled")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
      df[[nm]] <- x
    } else if (inherits(x, "haven_labelled")) {
      df[[nm]] <- haven::zap_labels(x)
    }
  }
  names(df) <- toupper(names(df))
  df
}

raw_ae_in <- read_lib_mem(raw_dir, "raw_ae")
dm_in     <- read_lib_mem(sdtm_dir, "dm")

stopifnot("SUBJID" %in% names(raw_ae_in))
stopifnot("SUBJID" %in% names(dm_in))

# PROC SORT data=raw.raw_ae; by SUBJID; (in place)
# PROC SORT data=sdtm.dm; by SUBJID;
sort_by_subjid <- function(df) {
  # SAS: missing first, character ASCII (uppercase before lowercase)
  # use radix ordering for byte order, NA first
  o <- order(df[["SUBJID"]], na.last = FALSE, method = "radix")
  df[o, , drop = FALSE]
}

raw_ae_sorted <- sort_by_subjid(raw_ae_in)
dm_sorted     <- sort_by_subjid(dm_in)

# DATA sdtm.ae; MERGE raw.raw_ae(in=a) sdtm.dm; BY SUBJID; IF a;
# SAS merge: overlapping non-BY vars from second overwrite first.
common_cols <- intersect(names(raw_ae_sorted), names(dm_sorted))
common_cols <- setdiff(common_cols, "SUBJID")
if (length(common_cols) > 0) {
  raw_for_join <- raw_ae_sorted[, setdiff(names(raw_ae_sorted), common_cols), drop = FALSE]
} else {
  raw_for_join <- raw_ae_sorted
}
# left join (keep only a)
merged <- dplyr::left_join(raw_for_join, dm_sorted, by = "SUBJID")

stopifnot("USUBJID" %in% names(merged))
# RFSTDTC should exist in DM; if not, create as NA to allow stop check message
if (!("RFSTDTC" %in% names(merged))) {
  stop("RFSTDTC not found after merge")
}
stopifnot("AESTDT" %in% names(merged) || "AESTDT" %in% names(raw_ae_in))
# AEENDT may be missing? check
if (!("AEENDT" %in% names(merged))) {
  merged$AEENDT <- NA_real_
}
if (!("AETERM" %in% names(merged))) merged$AETERM <- NA_character_
if (!("AESER" %in% names(merged))) merged$AESER <- NA_character_
if (!("AESEV" %in% names(merged))) merged$AESEV <- NA_character_

# helpers for SAS yymmdd10. semantics
to_sas_num <- function(x) {
  if (inherits(x, "Date")) {
    return(as.numeric(x - as.Date("1960-01-01")))
  }
  if (inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    d <- as.Date(x)
    return(as.numeric(d - as.Date("1960-01-01")))
  }
  if (is.numeric(x)) {
    return(x)
  }
  # character fallback: strict YYYY-MM-DD
  cx <- as.character(x)
  d <- as.Date(cx, format = "%Y-%m-%d")
  return(as.numeric(d - as.Date("1960-01-01")))
}

to_iso10 <- function(x) {
  if (inherits(x, "Date")) {
    return(ifelse(is.na(x), NA_character_, format(x, format = "%Y-%m-%d")))
  }
  if (inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    d <- as.Date(x)
    return(ifelse(is.na(d), NA_character_, format(d, format = "%Y-%m-%d")))
  }
  if (is.numeric(x)) {
    return(ifelse(is.na(x), NA_character_, format(as.Date(x, origin = "1960-01-01"), format = "%Y-%m-%d")))
  }
  # character: return as-is (should not happen for AESTDT)
  cx <- as.character(x)
  cx[cx == ""] <- NA_character_
  return(cx)
}

# STUDYID, AESTDTC, AEENDTC
n <- nrow(merged)
ae <- merged
ae$STUDYID <- "ABC-301"
ae$AESTDTC <- to_iso10(ae$AESTDT)
ae$AEENDTC <- to_iso10(ae$AEENDT)
# enforce LENGTH $10 (truncate, SAS would blank-pad)
ae$STUDYID <- substr(ae$STUDYID, 1, 10)
ae$AESTDTC <- ifelse(is.na(ae$AESTDTC), NA_character_, substr(ae$AESTDTC, 1, 10))
ae$AEENDTC <- ifelse(is.na(ae$AEENDTC), NA_character_, substr(ae$AEENDTC, 1, 10))

# rfstdt_num = input(RFSTDTC, yymmdd10.)
rf_char <- as.character(ae$RFSTDTC)
# SAS blank missing -> NA
rf_char[rf_char == ""] <- NA_character_
rf_date <- as.Date(rf_char, format = "%Y-%m-%d")
rfstdt_num <- as.numeric(rf_date - as.Date("1960-01-01"))

aest_num <- to_sas_num(ae$AESTDT)
aeen_num <- to_sas_num(ae$AEENDT)

# ASTDY / AENDY
ASTDY <- rep(NA_real_, n)
AENDY <- rep(NA_real_, n)
for (i in seq_len(n)) {
  a <- aest_num[i]
  r <- rfstdt_num[i]
  if (!is.na(a)) {
    if (is.na(r)) {
      ASTDY[i] <- NA_real_
    } else if (a >= r) {
      ASTDY[i] <- a - r + 1
    } else {
      ASTDY[i] <- a - r
    }
  }
  e <- aeen_num[i]
  if (!is.na(e)) {
    if (is.na(r)) {
      AENDY[i] <- NA_real_
    } else if (e >= r) {
      AENDY[i] <- e - r + 1
    } else {
      AENDY[i] <- e - r
    }
  }
}
ae$ASTDY <- ASTDY
ae$AENDY <- AENDY

# keep STUDYID USUBJID SUBJID AETERM AESER AESEV AESTDTC AEENDTC ASTDY AENDY
keep_vars <- c("STUDYID","USUBJID","SUBJID","AETERM","AESER","AESEV","AESTDTC","AEENDTC","ASTDY","AENDY")
for (v in keep_vars) {
  if (!(v %in% names(ae))) ae[[v]] <- NA_character_
}
ae_out <- ae[, keep_vars, drop = FALSE]

# ensure order by SUBJID (merge already sorted)
ae_out <- ae_out[order(ae_out[["SUBJID"]], na.last = FALSE, method = "radix"), , drop = FALSE]

# ---- write ae.csv ----
# dates already ISO character; numerics unrounded
write.csv(ae_out, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# ---- write raw_ae.csv (sorted raw input, dates ISO) ----
raw_out <- raw_ae_sorted
# convert Date/POSIXt columns to ISO character for CSV per spec
for (nm in names(raw_out)) {
  x <- raw_out[[nm]]
  if (inherits(x, "Date")) {
    raw_out[[nm]] <- ifelse(is.na(x), NA_character_, format(x, format = "%Y-%m-%d"))
  } else if (inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    # check if date-only or datetime: use datetime ISO if time present
    # spec: dates YYYY-MM-DD, datetimes YYYY-MM-DDTHH:MM:SS
    # detect non-midnight times
    lt <- as.POSIXlt(x)
    has_time <- !is.na(lt$hour) & (lt$hour != 0 | lt$min != 0 | lt$sec != 0)
    if (any(has_time, na.rm = TRUE)) {
      raw_out[[nm]] <- ifelse(is.na(x), NA_character_, format(x, format = "%Y-%m-%dT%H:%M:%S"))
    } else {
      raw_out[[nm]] <- ifelse(is.na(x), NA_character_, format(as.Date(x), format = "%Y-%m-%d"))
    }
  }
}
write.csv(raw_out, file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")
```