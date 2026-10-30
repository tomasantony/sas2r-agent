out <- Sys.getenv("OUT_DIR")
if (!nzchar(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_vitals.csv"

sas_trim <- function(x) {
  # SAS STRIP: remove leading/trailing blanks; NA stays NA
  ifelse(is.na(x), NA_character_, stringr::str_trim(x, side = "both"))
}

sas_catx <- function(delim, ...) {
  args <- list(...)
  # CATX strips each item and skips missing (NA, "", ".")
  sapply(seq_along(args[[1]]), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- stringr::str_trim(as.character(v), side = "both")
      if (v == "" || v == ".") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}

# ---- Read raw CSV preserving raw strings (emulate PROC IMPORT scan) ----
# Read everything as character so we see raw values before informat assignment
df0 <- utils::read.csv(raw_path, colClasses = "character", check.names = FALSE,
                       na.strings = NULL, stringsAsFactors = FALSE,
                       blank.lines.skip = TRUE, encoding = "unknown")
# PROC IMPORT GETNAMES=YES + VALIDVARNAME=UPCASE
nm <- names(df0)
nm <- toupper(gsub("[^A-Za-z0-9_]", "_", nm))
names(df0) <- nm

# Normalize: empty strings stay as "" ; treat as missing later
# Ensure expected columns exist (case-sensitive after upcase)
stopifnot("PT_ID" %in% names(df0))
stopifnot("VISIT_NUM" %in% names(df0))
stopifnot("VISIT" %in% names(df0))
stopifnot("VS_DATE" %in% names(df0))

n <- nrow(df0)
PT_ID_raw <- sas_trim(as.character(df0$PT_ID))
VISIT_raw <- sas_trim(as.character(df0$VISIT))
VISIT_NUM_raw <- sas_trim(as.character(df0$VISIT_NUM))
VS_DATE_raw <- sas_trim(as.character(df0$VS_DATE))
# read.csv with na.strings=NULL keeps NA as "NA" string? fix: literal "NA" from file is ambiguous;
# treat R NA as missing
PT_ID_raw[is.na(PT_ID_raw)] <- NA_character_
VISIT_raw[is.na(VISIT_raw)] <- NA_character_
VISIT_NUM_raw[is.na(VISIT_NUM_raw)] <- NA_character_
VS_DATE_raw[is.na(VS_DATE_raw)] <- NA_character_
# empty or single dot are SAS missing
is_sas_miss <- function(x) is.na(x) | x == "" | x == "."
PT_ID_raw[PT_ID_raw == "NA"] <- NA_character_

# ---- VISITNUM: SAS automatic char->numeric conversion ----
VISITNUM <- suppressWarnings(as.numeric(VISIT_NUM_raw))
# SAS: non-numeric becomes missing; blank becomes missing
VISITNUM[is_sas_miss(VISIT_NUM_raw)] <- NA_real_

# ---- VISIT stripped, truncated to $40 ----
VISIT <- VISIT_raw
VISIT[is_sas_miss(VISIT_raw)] <- NA_character_
VISIT <- substr(VISIT, 1, 40)

# ---- STUDYID / DOMAIN / USUBJID ----
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("SV", n)
# CATX("-", STUDYID, PT_ID)
USUBJID <- sas_catx("-", STUDYID, PT_ID_raw)
USUBJID <- substr(USUBJID, 1, 40)
STUDYID <- substr(STUDYID, 1, 8)
DOMAIN <- substr(DOMAIN, 1, 2)

# ---- VISITDY mapping ----
VISITDY <- rep(NA_real_, n)
VISITDY[!is.na(VISITNUM) & VISITNUM == 1] <- 1
VISITDY[!is.na(VISITNUM) & VISITNUM == 2] <- 31
VISITDY[!is.na(VISITNUM) & VISITNUM == 3] <- 61

# ---- SV date: emulate PROC IMPORT GUESSINGROWS + ANYDTDTE32 ----
# Decide ONE informat/order per column from all values, then parse every value with it.
parse_sas_anydtdte_once <- function(vec) {
  v <- sas_trim(as.character(vec))
  # missing markers
  miss <- is.na(v) | v == "" | v == "."
  vals <- v[!miss]
  fmt <- NA_character_
  order_slash <- NA_character_  # "MDY" or "DMY"
  year2 <- FALSE
  if (length(vals) == 0) {
    return(rep(as.Date(NA), length(v)))
  }
  is_slash <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", vals)
  is_dash_mmm <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", vals)
  is_iso <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", vals)
  # datetime with time part: take date portion for type decision
  # strip time for detection if contains space or T
  has_time <- grepl(":", vals)
  # Decide column type by majority / all
  if (all(is_slash)) {
    # GUESSINGROWS: if any first field > 12 then DDMMYY else MMDDYY
    first_field <- suppressWarnings(as.integer(sub("/.*$", "", vals)))
    if (any(!is.na(first_field) & first_field > 12)) {
      order_slash <- "DMY"
    } else {
      order_slash <- "MDY"
    }
    # year width: all 2 or all 4? decide once
    yrpart <- sub("^[0-9]{1,2}/[0-9]{1,2}/", "", vals)
    if (all(nchar(yrpart) == 2)) {
      year2 <- TRUE
      fmt <- if (order_slash == "DMY") "%d/%m/%y" else "%m/%d/%y"
    } else {
      year2 <- FALSE
      fmt <- if (order_slash == "DMY") "%d/%m/%Y" else "%m/%d/%Y"
    }
  } else if (all(is_dash_mmm)) {
    yrpart <- sub("^[0-9]{1,2}-[A-Za-z]{3}-", "", vals)
    if (all(nchar(yrpart) == 2)) {
      fmt <- "%d-%b-%y"
    } else {
      fmt <- "%d-%b-%Y"
    }
  } else if (all(is_iso)) {
    fmt <- "%Y-%m-%d"
  } else if (any(is_slash) && sum(is_slash) >= sum(is_dash_mmm) && sum(is_slash) >= sum(is_iso)) {
    # mixed but predominantly slash -> treat as slash column (PROC IMPORT would have guessed from majority scan)
    slash_vals <- vals[is_slash]
    first_field <- suppressWarnings(as.integer(sub("/.*$", "", slash_vals)))
    if (any(!is.na(first_field) & first_field > 12)) {
      order_slash <- "DMY"
    } else {
      order_slash <- "MDY"
    }
    yrpart <- sub("^[0-9]{1,2}/[0-9]{1,2}/", "", slash_vals)
    if (all(nchar(yrpart) == 2)) {
      fmt <- if (order_slash == "DMY") "%d/%m/%y" else "%m/%d/%y"
    } else {
      fmt <- if (order_slash == "DMY") "%d/%m/%Y" else "%m/%d/%Y"
    }
  } else if (any(is_dash_mmm)) {
    fmt <- "%d-%b-%Y"
  } else if (any(is_iso)) {
    fmt <- "%Y-%m-%d"
  } else {
    # fallback: infer from delimiters
    if (any(grepl("/", vals, fixed = TRUE))) {
      first_field_all <- suppressWarnings(as.integer(sub("/.*$", "", vals[grepl("/", vals, fixed = TRUE)])))
      first_field_all <- first_field_all[!is.na(first_field_all)]
      if (length(first_field_all) > 0 && any(first_field_all > 12)) {
        fmt <- "%d/%m/%Y"
      } else {
        fmt <- "%m/%d/%Y"
      }
    } else if (any(grepl("-", vals, fixed = TRUE))) {
      fmt <- "%d-%b-%Y"
    } else {
      fmt <- "%Y-%m-%d"
    }
  }
  # Parse every value with that single format (strict, no per-value switching)
  res <- rep(as.Date(NA), length(v))
  for (i in seq_along(v)) {
    if (miss[i]) next
    s <- v[i]
    # VVALUE width 32 + STRIP already; ANYDTDTE ignores leading/trailing blanks
    # If datetime with time, ANYDTDTE would still read date part; emulate by taking date token
    # but keep strict: only parse the date token with fmt
    if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4} ", s)) {
      s <- sub(" .*$$", "", s)
    }
    if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4} ", s)) {
      s <- sub(" .*$$", "", s)
    }
    if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}[ T]", s)) {
      s <- sub("^([0-9]{4}-[0-9]{1,2}-[0-9]{1,2}).*$", "\\1", s)
    }
    # For %b month, R requires title case (Jan); normalize upper/lower to title
    ss <- s
    if (grepl("%b", fmt, fixed = TRUE)) {
      # normalize MMM part to Title case
      ss <- gsub("([0-9]{1,2}-)([A-Za-z]{3})(-[0-9]{2,4})", "\\1\\2\\3", ss)
      # convert middle token
      m <- regmatches(ss, regexpr("[A-Za-z]{3}", ss))
      if (length(m) > 0 && nzchar(m)) {
        low <- tolower(m)
        tit <- paste0(toupper(substr(low, 1, 1)), substr(low, 2, 3))
        ss <- sub("[A-Za-z]{3}", tit, ss)
      }
    }
    d <- suppressWarnings(as.Date(ss, format = fmt))
    res[i] <- d
  }
  attr(res, "sas_fmt") <- fmt
  res
}

svdt <- parse_sas_anydtdte_once(VS_DATE_raw)
SVSTDTC <- rep(NA_character_, n)
SVENDTC <- rep(NA_character_, n)
has_dt <- !is.na(svdt)
SVSTDTC[has_dt] <- format(svdt[has_dt], "%Y-%m-%d")
SVENDTC[has_dt] <- SVSTDTC[has_dt]
SVSTDTC <- substr(ifelse(is.na(SVSTDTC), NA_character_, SVSTDTC), 1, 10)
SVENDTC <- substr(ifelse(is.na(SVENDTC), NA_character_, SVENDTC), 1, 10)
# SAS PUT(IS8601DA.) gives YYYY-MM-DD; keep as character ISO dates

sv <- data.frame(
  STUDYID = STUDYID,
  DOMAIN = DOMAIN,
  USUBJID = USUBJID,
  VISITNUM = VISITNUM,
  VISIT = VISIT,
  VISITDY = VISITDY,
  SVSTDTC = SVSTDTC,
  SVENDTC = SVENDTC,
  stringsAsFactors = FALSE
)

# ---- Sort without deleting records: BY USUBJID VISITNUM (SAS: missing first, uppercase before lowercase) ----
# Use byte-order sort key to emulate SAS character ordering
to_sort_key <- function(x) {
  sapply(as.character(x), function(s) {
    if (is.na(s)) return("")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = "-")
  }, USE.NAMES = FALSE)
}
# USUBJID all uppercase/digits so plain order suffices; enforce C collation via keys
ord_usub <- order(to_sort_key(sv$USUBJID), sv$VISITNUM, na.last = FALSE)
sv <- sv[ord_usub, , drop = FALSE]
rownames(sv) <- NULL

# ---- Write dataset ----
utils::write.csv(sv, file.path(out, "sv.csv"), row.names = FALSE, na = "")

# ---- Listing: PROC PRINT obs=12 with TITLE "SV Domain Audit" ----
nprint <- min(12, nrow(sv))
lst <- sv[seq_len(nprint), , drop = FALSE]
lst_print <- data.frame(Obs = seq_len(nprint), lst, stringsAsFactors = FALSE, check.names = FALSE)

# CSV of table body
utils::write.csv(lst_print, file.path(out, "P017.csv"), row.names = FALSE, na = "")

# Text rendering mimicking SAS LISTING
fmt_cell_txt <- function(val, is_num) {
  if (is.na(val) || (is.character(val) && val == "")) {
    if (is_num) return(".")
    else return("")
  }
  as.character(val)
}
header <- sprintf("%-4s %-8s %-6s %-20s %-8s %-20s %-7s %-10s %-10s",
                  "Obs", "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT", "VISITDY", "SVSTDTC", "SVENDTC")
lines <- c("SV Domain Audit", "", header)
for (i in seq_len(nprint)) {
  r <- lst_print[i, ]
  line <- sprintf("%-4s %-8s %-6s %-20s %-8s %-20s %-7s %-10s %-10s",
    fmt_cell_txt(r$Obs, TRUE),
    fmt_cell_txt(r$STUDYID, FALSE),
    fmt_cell_txt(r$DOMAIN, FALSE),
    fmt_cell_txt(r$USUBJID, FALSE),
    fmt_cell_txt(r$VISITNUM, TRUE),
    fmt_cell_txt(r$VISIT, FALSE),
    fmt_cell_txt(r$VISITDY, TRUE),
    fmt_cell_txt(r$SVSTDTC, FALSE),
    fmt_cell_txt(r$SVENDTC, FALSE))
  # trim trailing blanks like SAS listing
  line <- sub(" +$$", "", line)
  lines <- c(lines, line)
}
writeLines(lines, file.path(out, "P017.txt"), useBytes = TRUE)
