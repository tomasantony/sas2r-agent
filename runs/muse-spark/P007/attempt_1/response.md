```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"

# SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Try to locate dataset file case-insensitive with several extensions
find_dataset_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(files) == 0) return(NULL)
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(name))
  if (length(idx) == 0) return(NULL)
  cand <- files[idx]
  # priority: sas7bdat > xpt > sas7bdat-like > csv > txt > xlsx
  ext <- tolower(tools::file_ext(cand))
  pri <- match(ext, c("sas7bdat", "sas7bdat.gz", "xpt", "xpt.gz", "sas", "csv", "csv.gz", "txt", "xlsx", "xls"))
  pri[is.na(pri)] <- 99
  cand[order(pri)][1]
}

read_dataset_generic <- function(dir, name) {
  f <- find_dataset_file(dir, name)
  if (is.null(f) || is.na(f)) stop(paste0("Dataset not found: ", name, " in ", dir))
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext %in% c("sas7bdat")) {
    df <- haven::read_sas(f)
  } else if (ext %in% c("xpt")) {
    df <- haven::read_xpt(f)
  } else if (ext %in% c("csv", "gz")) {
    # handle csv.gz
    df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE, guess_max = 10000))
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(f)
  } else {
    # fallback try sas then xpt then csv
    tryCatch({ df <- haven::read_sas(f) },
      error = function(e) { df <<- haven::read_xpt(f) })
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap haven labels/formats to plain vectors
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled")) {
      # keep underlying values, drop labels
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      # haven may return labelled still as class; coerce
      if (inherits(v, "haven_labelled")) class(v) <- setdiff(class(v), "haven_labelled")
      df[[nm]] <- v
    } else {
      # zap formats if present
      try({ df[[nm]] <- haven::zap_formats(v) }, silent = TRUE)
    }
  }
  names(df) <- toupper(names(df))
  df
}

# Convert vector to R Date (for SAS date vars)
to_date_vec <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric dates: days since 1960-01-01 (haven already converts, so this is CSV numeric case)
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.logical(x) && all(is.na(x))) return(as.Date(rep(NA_character_, length(x))))
  # character / factor
  x <- as.character(x)
  x_trim <- trimws(x)
  res <- rep(as.Date(NA), length(x_trim))
  is_missing <- is.na(x_trim) | x_trim == "" | x_trim == "." | toupper(x_trim) == "NA"
  # try ISO YYYY-MM-DD first
  idx <- which(!is_missing)
  if (length(idx) > 0) {
    # lubridate ymd
    suppressWarnings({
      p1 <- lubridate::ymd(x_trim[idx], quiet = TRUE, truncated = 0)
    })
    # for those still NA, try ymd_hms / parse_date_time
    need <- idx[is.na(p1)]
    if (length(need) > 0) {
      suppressWarnings({
        p2 <- lubridate::parse_date_time(x_trim[need],
          orders = c("YmdHMS", "YmdHM", "Ymd", "dmy", "mdy", "dbY", "dBY", "Ymd HMS"),
          quiet = TRUE, exact = FALSE)
      })
      p2d <- as.Date(p2)
      p1[match(need, idx)] <- p2d
    }
    # for those still NA, try SAS DATE9 like 12JAN2013 or 12-JAN-2013
    still <- idx[is.na(p1)]
    if (length(still) > 0) {
      up <- toupper(trimws(x_trim[still]))
      up <- gsub("-", "", up, fixed = TRUE)
      # format %d%b%Y e.g. 12JAN2013 ; also allow 2-digit year?
      p3 <- suppressWarnings(as.Date(up, format = "%d%b%Y"))
      # try %d%b%y
      miss3 <- is.na(p3)
      if (any(miss3)) {
        p3b <- suppressWarnings(as.Date(up[miss3], format = "%d%b%y"))
        p3[miss3] <- p3b
      }
      p1[match(still, idx)] <- p3
    }
    res[idx] <- p1
  }
  res
}

# SAS character sort: uppercase before lowercase, missing first.
# Use C locale for ordering to mimic SAS binary order.
sas_order2 <- function(a, b) {
  old <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  on.exit(suppressWarnings(try(Sys.setlocale("LC_COLLATE", old), silent = TRUE)), add = TRUE)
  # a character, b Date/numeric; both NA first
  order(a, b, na.last = FALSE, method = "radix")
}

message("Reading ADSL and ADAE ...")
adsl <- read_dataset_generic(adam_dir, "adsl")
adae <- read_dataset_generic(adam_dir, "adae")

# Ensure required columns exist; create as NA if missing (to keep script robust)
ensure_col <- function(df, col, default = NA) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}
for (cc in c("USUBJID", "AESTDT", "TRTEMFL")) adae <- ensure_col(adae, cc)
for (cc in c("USUBJID", "STUDYID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT", "SAFFL")) adsl <- ensure_col(adsl, cc)

# Normalize date vectors
adae$AESTDT <- to_date_vec(adae$AESTDT)
adsl$TRTSDT <- to_date_vec(adsl$TRTSDT)
adsl$TRTEDT <- to_date_vec(adsl$TRTEDT)
adsl$EOSDT  <- to_date_vec(adsl$EOSDT)

# Ensure character vectors are character (haven may give labelled numeric? keep as character)
char_cols_adae <- c("USUBJID", "TRTEMFL")
for (cc in char_cols_adae) adae[[cc]] <- as.character(adae[[cc]])
char_cols_adsl <- c("USUBJID", "STUDYID", "TRT01P", "TRT01A", "SAFFL")
for (cc in char_cols_adsl) adsl[[cc]] <- as.character(adsl[[cc]])

# 1. GET FIRST ADVERSE EVENT PER SUBJECT
# proc sort data=adam.adae out=work.adae_first; by USUBJID AESTDT; where TRTEMFL='Y';
adae_first <- adae %>%
  filter(!is.na(TRTEMFL) & TRTEMFL == "Y")

if (nrow(adae_first) > 0) {
  o <- sas_order2(adae_first$USUBJID, adae_first$AESTDT)
  adae_first <- adae_first[o, , drop = FALSE]
} else {
  # keep empty with same cols
  adae_first <- adae_first
}

# data work.ae_target; set work.adae_first; by USUBJID; if first.USUBJID; keep USUBJID AESTDT; rename AESTDT=EVENTDT;
# FIRST.USUBJID after sort by USUBJID AESTDT
if (nrow(adae_first) > 0) {
  # mark first per USUBJID (SAS: missing USUBJID sorts first and groups together)
  # Use duplicated on USUBJID in sorted order; need to treat NA as equal group
  usub <- adae_first$USUBJID
  usub_key <- ifelse(is.na(usub), "<__SAS_MISSING__>", usub)
  is_first <- !duplicated(usub_key)
  ae_target <- adae_first[is_first, , drop = FALSE]
  ae_target <- ae_target[, c("USUBJID", "AESTDT"), drop = FALSE]
  names(ae_target)[names(ae_target) == "AESTDT"] <- "EVENTDT"
  # EVENTDT already Date
  ae_target$EVENTDT <- to_date_vec(ae_target$EVENTDT)
} else {
  ae_target <- data.frame(USUBJID = character(0), EVENTDT = as.Date(character(0)), stringsAsFactors = FALSE)
}

# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
o2 <- sas_order2(adsl$USUBJID, rep(1, nrow(adsl)))
# sas_order2 expects two args; for single key, use order with NA first and C locale
old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord_adsl <- order(adsl$USUBJID, na.last = FALSE, method = "radix")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
adsl_sorted <- adsl[ord_adsl, , drop = FALSE]

# data adam.adtte; merge work.adsl_sorted(in=a) work.ae_target(in=b); by USUBJID; if a and SAFFL='Y';
# LEFT JOIN adsl_sorted with ae_target (ae_target has one row per USUBJID)
adtte_merged <- dplyr::left_join(adsl_sorted, ae_target, by = "USUBJID")

# if a and SAFFL='Y'  (a is always true after left join; filter SAFFL)
adtte_merged <- adtte_merged %>%
  filter(!is.na(SAFFL) & SAFFL == "Y")

# Derive PARAMCD/PARAM/CNSR/AVAL
n <- nrow(adtte_merged)
adtte_merged$PARAMCD <- rep("TTFAEV", n)
adtte_merged$PARAM <- rep("Time to First Adverse Event", n)

# Ensure EVENTDT Date
if (!"EVENTDT" %in% names(adtte_merged)) adtte_merged$EVENTDT <- as.Date(rep(NA, n))
adtte_merged$EVENTDT <- to_date_vec(adtte_merged$EVENTDT)

CNSR <- rep(NA_real_, n)
AVAL <- rep(NA_real_, n)
TRTSDT <- adtte_merged$TRTSDT
EVENTDT <- adtte_merged$EVENTDT
EOSDT <- adtte_merged$EOSDT
for (i in seq_len(n)) {
  trts <- TRTSDT[i]
  ev <- EVENTDT[i]
  eos <- EOSDT[i]
  if (is.na(trts)) {
    CNSR[i] <- NA_real_
    AVAL[i] <- NA_real_
  } else if (!is.na(ev)) {
    CNSR[i] <- 0
    AVAL[i] <- as.numeric(ev - trts) + 1
  } else {
    CNSR[i] <- 1
    if (!is.na(eos)) {
      AVAL[i] <- as.numeric(eos - trts) + 1
    } else {
      AVAL[i] <- NA_real_
    }
  }
}
adtte_merged$CNSR <- CNSR
adtte_merged$AVAL <- AVAL

# keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT;
keep_vars <- c("STUDYID", "USUBJID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT",
               "PARAMCD", "PARAM", "CNSR", "AVAL", "EVENTDT")
for (cc in keep_vars) adtte_merged <- ensure_col(adtte_merged, cc)
adtte <- adtte_merged[, keep_vars, drop = FALSE]

# retain STUDYID USUBJID PARAMCD PARAM -> column order already as keep; ensure sort by USUBJID
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord_out <- order(adtte$USUBJID, na.last = FALSE, method = "radix")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
adtte <- adtte[ord_out, , drop = FALSE]

# Helper to format dates/datetimes for CSV: dates YYYY-MM-DD, datetimes YYYY-MM-DDTHH:MM:SS
# adtte date cols are Date; ensure they write as YYYY-MM-DD with na=""
adtte_csv <- adtte
# Convert any POSIXct to character ISO (not expected here, but for compliance)
for (nm in names(adtte_csv)) {
  v <- adtte_csv[[nm]]
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
    v2 <- format(as.POSIXct(v), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    v2[is.na(v)] <- NA_character_
    adtte_csv[[nm]] <- v2
  }
}
write.csv(adtte_csv, file.path(out, "adtte.csv"), row.names = FALSE, na = "")
message(paste0("Wrote adtte.csv with ", nrow(adtte_csv), " rows"))

# 4. VISUAL AUDIT: proc print data=adam.adtte(obs=15); var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT;
audit_vars <- c("USUBJID", "TRT01A", "PARAM", "CNSR", "AVAL", "EVENTDT", "EOSDT")
for (cc in audit_vars) adtte <- ensure_col(adtte, cc)
audit <- head(adtte[, audit_vars, drop = FALSE], 15)

# CSV of table body (same 15 rows)
audit_csv <- audit
for (nm in names(audit_csv)) {
  v <- audit_csv[[nm]]
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
    v2 <- format(as.POSIXct(v), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    v2[is.na(v)] <- NA_character_
    audit_csv[[nm]] <- v2
  }
  # Dates stay as Date -> write.csv writes YYYY-MM-DD
}
write.csv(audit_csv, file.path(out, "P007.csv"), row.names = FALSE, na = "")
message("Wrote P007.csv")

# Plain-text rendering mimicking PROC PRINT
fmt_date_txt <- function(x) {
  ifelse(is.na(x), ".", format(x, "%Y-%m-%d"))
}
fmt_num_txt <- function(x) {
  ifelse(is.na(x), ".", format(x, scientific = FALSE, trim = TRUE))
}
fmt_char_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

# Build fixed-width text table
# Compute display strings
disp <- data.frame(
  Obs = seq_len(nrow(audit)),
  USUBJID = fmt_char_txt(audit$USUBJID),
  TRT01A = fmt_char_txt(audit$TRT01A),
  PARAM = fmt_char_txt(audit$PARAM),
  CNSR = fmt_num_txt(audit$CNSR),
  AVAL = fmt_num_txt(audit$AVAL),
  EVENTDT = fmt_date_txt(to_date_vec(audit$EVENTDT)),
  EOSDT = fmt_date_txt(to_date_vec(audit$EOSDT)),
  stringsAsFactors = FALSE
)

# Determine column widths (at least header length)
headers <- c("Obs", "USUBJID", "TRT01A", "PARAM", "CNSR", "AVAL", "EVENTDT", "EOSDT")
widths <- sapply(headers, function(h) max(nchar(c(h, disp[[h]])), na.rm = TRUE))
# cap PARAM width for readability? keep full
# Build header and separator
pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)
pad_left_num <- function(x, w) sprintf(paste0("%", w, "s"), x)

lines <- c(
  "ADTTE Audit - Time to First Adverse Event",
  "",
  paste0("Dataset ADTTE: Time to First Adverse Event (first ", nrow(audit), " of ", nrow(adtte), " subjects)")
)

header_line <- paste(
  pad_right("Obs", widths["Obs"]),
  pad_right("USUBJID", widths["USUBJID"]),
  pad_right("TRT01A", widths["TRT01A"]),
  pad_right("PARAM", widths["PARAM"]),
  pad_right("CNSR", widths["CNSR"]),
  pad_right("AVAL", widths["AVAL"]),
  pad_right("EVENTDT", widths["EVENTDT"]),
  pad_right("EOSDT", widths["EOSDT"]),
  sep = "  "
)
sep_line <- paste(
  pad_right(paste(rep("-", widths["Obs"]), collapse = ""), widths["Obs"]),
  pad_right(paste(rep("-", widths["USUBJID"]), collapse = ""), widths["USUBJID"]),
  pad_right(paste(rep("-", widths["TRT01A"]), collapse = ""), widths["TRT01A"]),
  pad_right(paste(rep("-", widths["PARAM"]), collapse = ""), widths["PARAM"]),
  pad_right(paste(rep("-", widths["CNSR"]), collapse = ""), widths["CNSR"]),
  pad_right(paste(rep("-", widths["AVAL"]), collapse = ""), widths["AVAL"]),
  pad_right(paste(rep("-", widths["EVENTDT"]), collapse = ""), widths["EVENTDT"]),
  pad_right(paste(rep("-", widths["EOSDT"]), collapse = ""), widths["EOSDT"]),
  sep = "  "
)
lines <- c(lines, "", header_line, sep_line)
for (i in seq_len(nrow(disp))) {
  row_line <- paste(
    pad_left_num(disp$Obs[i], widths["Obs"]),
    pad_right(disp$USUBJID[i], widths["USUBJID"]),
    pad_right(disp$TRT01A[i], widths["TRT01A"]),
    pad_right(disp$PARAM[i], widths["PARAM"]),
    pad_left_num(disp$CNSR[i], widths["CNSR"]),
    pad_left_num(disp$AVAL[i], widths["AVAL"]),
    pad_right(disp$EVENTDT[i], widths["EVENTDT"]),
    pad_right(disp$EOSDT[i], widths["EOSDT"]),
    sep = "  "
  )
  lines <- c(lines, row_line)
}
lines <- c(lines, "", paste0("N = ", nrow(adtte), " subjects in ADTTE (SAFFL=Y). Showing first ", nrow(audit), "."))

writeLines(lines, file.path(out, "P007.txt"))
message("Wrote P007.txt")
```