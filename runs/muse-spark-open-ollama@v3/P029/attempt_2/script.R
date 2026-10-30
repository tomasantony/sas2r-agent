# P029_tu.sas translation to R
# SDTM TU mapping

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readxl)
})

# Helpers - SAS semantics
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(as.character(x)))
}
sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, as.character(x), fixed = TRUE))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  # convert to character, trim
  args_c <- lapply(args, function(a) {
    if (is.null(a)) return(NA_character_)
    trimws(as.character(a))
  })
  # catx: skip missing/empty
  do.call(mapply, c(function(...) {
    vals <- c(...)
    vals <- vals[!is.na(vals) & vals != ""]
    if (length(vals) == 0) return(NA_character_)
    paste(vals, collapse = sep)
  }, args_c))
}
# SAS date11. informat: DD-MMM-YYYY or DDMMMYYYY, case-insensitive
parse_date11 <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA") return(as.Date(NA))
  # if numeric string like 44927 (Excel serial) - try to detect
  if (grepl("^[0-9]{5}$", s)) {
    # Excel origin 1899-12-30
    n <- suppressWarnings(as.numeric(s))
    if (!is.na(n)) return(as.Date(n, origin = "1899-12-30"))
  }
  # Try yymmdd10 first (already ISO)
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # Try date11 variants
  fmts <- c("%d-%b-%Y", "%d-%B-%Y", "%d%b%Y", "%d-%b-%y", "%d%b%y",
            "%d-%m-%Y", "%d/%m/%Y", "%m/%d/%Y", "%Y/%m/%d", "%Y-%m-%dT%H:%M:%S")
  # normalize month to title case for %b
  for (fmt in fmts) {
    d2 <- suppressWarnings(as.Date(s, format = fmt))
    if (!is.na(d2)) return(d2)
    # try with toupper month -> title case
    s2 <- s
    # convert JAN -> Jan etc for %b
    s2 <- gsub("([0-9]{2})-([A-Za-z]{3})-([0-9]{4})", "\\1-\\L\\2-\\E-\\3", s, perl = TRUE)
    # manual title case
    d2 <- suppressWarnings(as.Date(toupper(s), format = toupper(fmt)))
    if (!is.na(d2)) return(d2)
  }
  # lubridate fallback dmy
  d3 <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d3)) return(as.Date(d3))
  d4 <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d4)) return(as.Date(d4))
  return(as.Date(NA))
}
parse_yymmdd10 <- function(x) {
  if (is.na(x) || is.null(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  s <- trimws(as.character(x))
  if (s == "" || toupper(s) == "NA") return(as.Date(NA))
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # try datetime
  d2 <- suppressWarnings(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
  if (!is.na(d2)) return(d2)
  parse_date11(s)
}
format_yymmdd10 <- function(d) {
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}
read_xlsx_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  # try reading with text to preserve dates as text, fallback to default
  df <- tryCatch(
    readxl::read_excel(path, col_types = "text", .name_repair = "minimal"),
    error = function(e) tryCatch(readxl::read_excel(path, .name_repair = "minimal"), error = function(e2) NULL)
  )
  if (is.null(df)) return(NULL)
  as.data.frame(df, stringsAsFactors = FALSE)
}

# 1. Prepare DM Reference Data - try to find sdtm_dm
rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
sdtm_candidates <- c(
  file.path(rawsdtm, "dm.xpt"),
  file.path(rawsdtm, "DM.xpt"),
  file.path(rawsdtm, "dm.sas7bdat"),
  file.path(rawsdtm, "DM.sas7bdat"),
  file.path(rawadam, "adsl.xpt"),
  file.path(rawadam, "ADSL.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
)
dm_for_tu <- NULL
for (p in sdtm_candidates) {
  if (file.exists(p)) {
    ext <- tolower(tools::file_ext(p))
    df <- tryCatch({
      if (ext == "xpt") haven::read_xpt(p)
      else if (ext == "sas7bdat") haven::read_sas(p)
      else NULL
    }, error = function(e) NULL)
    if (!is.null(df) && nrow(df) > 0) {
      names(df) <- toupper(names(df))
      # look for USUBJID and RFSTDTC
      if (all(c("USUBJID", "RFSTDTC") %in% names(df))) {
        dm_for_tu <- df[, c("USUBJID", "RFSTDTC"), drop = FALSE]
        # ensure RFSTDTC is character YYYY-MM-DD
        dm_for_tu$RFSTDTC <- sapply(dm_for_tu$RFSTDTC, function(v) {
          if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
          if (inherits(v, "Date") || inherits(v, "POSIXt")) return(format(as.Date(v), "%Y-%m-%d"))
          s <- trimws(as.character(v))
          # if already ISO date
          d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
          if (!is.na(d)) return(format(d, "%Y-%m-%d"))
          d2 <- parse_date11(s)
          if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
          s
        })
        dm_for_tu$USUBJID <- trimws(as.character(dm_for_tu$USUBJID))
        break
      }
    }
  }
}
# Fallback derive from EDC
if (is.null(dm_for_tu)) {
  edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  edc_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
  ivrs_rand_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"
  derived <- NULL
  if (file.exists(edc_dm_path)) {
    edc_dm <- read_xlsx_safe(edc_dm_path)
    if (!is.null(edc_dm)) {
      names(edc_dm) <- toupper(names(edc_dm))
      # find SUBJID column
      subj_col <- intersect(c("SUBJID", "SUBJECT", "USUBJID"), names(edc_dm))[1]
      if (!is.na(subj_col) && "RFSTDTC" %in% names(edc_dm)) {
        tmp <- edc_dm[, c(subj_col, "RFSTDTC"), drop = FALSE]
        names(tmp) <- c("SUBJID", "RFSTDTC")
        tmp$USUBJID <- sas_catx("-", "ONCO-2026", tmp$SUBJID)
        tmp$RFSTDTC <- sapply(tmp$RFSTDTC, function(v) {
          if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
          if (inherits(v, "Date") || inherits(v, "POSIXt")) return(format(as.Date(v), "%Y-%m-%d"))
          d <- parse_date11(v)
          if (!is.na(d)) return(format(d, "%Y-%m-%d"))
          trimws(as.character(v))
        })
        derived <- tmp[, c("USUBJID", "RFSTDTC"), drop = FALSE]
      }
    }
  }
  if (is.null(derived) && file.exists(edc_ex_path)) {
    edc_ex <- read_xlsx_safe(edc_ex_path)
    if (!is.null(edc_ex)) {
      names(edc_ex) <- toupper(names(edc_ex))
      subj_col <- intersect(c("SUBJID", "SUBJECT"), names(edc_ex))[1]
      date_col <- intersect(c("EXSTDTC", "EXSTDT", "EXDTC", "EXDT", "EXSTDAT", "STARTDT", "EXPOSURE_START"), names(edc_ex))[1]
      if (!is.na(subj_col) && !is.na(date_col)) {
        tmp <- edc_ex[, c(subj_col, date_col), drop = FALSE]
        names(tmp) <- c("SUBJID", "RFSTDTC_RAW")
        tmp$RFSTDTC_RAW <- sapply(tmp$RFSTDTC_RAW, function(v) {
          if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
          if (inherits(v, "Date") || inherits(v, "POSIXt")) return(format(as.Date(v), "%Y-%m-%d"))
          d <- parse_date11(v)
          if (!is.na(d)) return(format(d, "%Y-%m-%d"))
          trimws(as.character(v))
        })
        tmp <- tmp[!is.na(tmp$RFSTDTC_RAW) & tmp$RFSTDTC_RAW != "", , drop = FALSE]
        if (nrow(tmp) > 0) {
          tmp$USUBJID <- sas_catx("-", "ONCO-2026", tmp$SUBJID)
          # earliest per subject
          tmp$RFSTDTC_DATE <- as.Date(tmp$RFSTDTC_RAW, format = "%Y-%m-%d")
          agg <- aggregate(RFSTDTC_DATE ~ USUBJID, data = tmp, FUN = min, na.rm = TRUE)
          agg$RFSTDTC <- format(agg$RFSTDTC_DATE, "%Y-%m-%d")
          derived <- agg[, c("USUBJID", "RFSTDTC"), drop = FALSE]
        }
      }
    }
  }
  if (is.null(derived) && file.exists(ivrs_rand_path)) {
    ivrs <- read_xlsx_safe(ivrs_rand_path)
    if (!is.null(ivrs)) {
      names(ivrs) <- toupper(names(ivrs))
      subj_col <- intersect(c("SUBJID", "SUBJECT", "SUBJ"), names(ivrs))[1]
      date_col <- intersect(c("RANDDTC", "RANDDT", "RANDOMIZATION_DATE", "RAND_DATE", "RFSTDTC"), names(ivrs))[1]
      if (!is.na(subj_col) && !is.na(date_col)) {
        tmp <- ivrs[, c(subj_col, date_col), drop = FALSE]
        names(tmp) <- c("SUBJID", "RFSTDTC")
        tmp$USUBJID <- sas_catx("-", "ONCO-2026", tmp$SUBJID)
        tmp$RFSTDTC <- sapply(tmp$RFSTDTC, function(v) {
          if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
          if (inherits(v, "Date") || inherits(v, "POSIXt")) return(format(as.Date(v), "%Y-%m-%d"))
          d <- parse_date11(v)
          if (!is.na(d)) return(format(d, "%Y-%m-%d"))
          trimws(as.character(v))
        })
        derived <- tmp[, c("USUBJID", "RFSTDTC"), drop = FALSE]
      }
    }
  }
  if (!is.null(derived)) {
    dm_for_tu <- derived
  } else {
    # last resort: create empty dm_for_tu from edc_tu subjects with NA RFSTDTC
    edc_tu_tmp <- read_xlsx_safe("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx")
    if (!is.null(edc_tu_tmp)) {
      names(edc_tu_tmp) <- toupper(names(edc_tu_tmp))
      if ("SUBJID" %in% names(edc_tu_tmp)) {
        dm_for_tu <- data.frame(USUBJID = unique(sas_catx("-", "ONCO-2026", edc_tu_tmp$SUBJID)), RFSTDTC = NA_character_, stringsAsFactors = FALSE)
      } else {
        dm_for_tu <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
      }
    } else {
      dm_for_tu <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
    }
  }
}
# sort dm_for_tu by USUBJID (SAS missing first, but USUBJID never missing)
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last = FALSE), , drop = FALSE]
dm_for_tu <- unique(dm_for_tu)

# 2. Prepare Raw Data
edc_tu_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx"
tu_prep_raw <- read_xlsx_safe(edc_tu_path)
if (is.null(tu_prep_raw)) stop("edc_tu.xlsx not found at ", edc_tu_path)
names(tu_prep_raw) <- toupper(names(tu_prep_raw))
# Ensure expected columns exist - handle alternative names
# SUBJID
if (!"SUBJID" %in% names(tu_prep_raw)) {
  alt <- intersect(c("SUBJECT", "SUBJ", "USUBJID"), names(tu_prep_raw))
  if (length(alt) > 0) names(tu_prep_raw)[names(tu_prep_raw) == alt[1]] <- "SUBJID"
}
# LOC, METH, LNKID, TYPE, VISIT, TUDT
for (col in c("LOC", "METH", "LNKID", "TYPE", "VISIT", "TUDT")) {
  if (!col %in% names(tu_prep_raw)) {
    # try case-insensitive search
    idx <- which(toupper(names(tu_prep_raw)) == col)
    if (length(idx) == 0) {
      # create empty column
      tu_prep_raw[[col]] <- NA_character_
    }
  }
}
# Handle TUDT alternative names
if (!"TUDT" %in% names(tu_prep_raw)) {
  alt_dt <- intersect(c("TUDTC", "TU_DTC", "TUDAT", "DATE", "TUDATE"), names(tu_prep_raw))
  if (length(alt_dt) > 0) tu_prep_raw$TUDT <- tu_prep_raw[[alt_dt[1]]]
}
tu_prep <- tu_prep_raw
tu_prep$USUBJID <- sas_catx("-", "ONCO-2026", tu_prep$SUBJID)
tu_prep$TULOC <- sas_upcase(sas_tranwrd(tu_prep$LOC, "_", " "))
tu_prep$TUMETHOD <- sas_upcase(sas_tranwrd(tu_prep$METH, "_", " "))
# sort by USUBJID
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# 3. Map TU Domain
# merge tu_prep(in=t) dm_for_tu by USUBJID; if t;
sdtm_tu1 <- merge(tu_prep, dm_for_tu, by = "USUBJID", all.x = TRUE, sort = FALSE)
# SAS merge keeps order of BY groups; we will sort later explicitly
sdtm_tu1$STUDYID <- "ONCO-2026"
sdtm_tu1$DOMAIN <- "TU"
sdtm_tu1$TULNKID <- trimws(as.character(sdtm_tu1$LNKID))
sdtm_tu1$TUTESTCD <- "TUMIDENT"
sdtm_tu1$TUTEST <- "Tumor Identification"
sdtm_tu1$TUORRES <- sas_upcase(trimws(as.character(sdtm_tu1$TYPE)))
sdtm_tu1$TUSTRESC <- sdtm_tu1$TUORRES
sdtm_tu1$TUEVAL <- "INVESTIGATOR"
sdtm_tu1$VISIT <- sas_upcase(sas_tranwrd(trimws(as.character(sdtm_tu1$VISIT)), "_", " "))
# TUDTC derivation: if TUDT ne "" then TUDTC = put(input(TUDT, date11.), yymmdd10.)
sdtm_tu1$TUDTC <- sapply(sdtm_tu1$TUDT, function(v) {
  if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
  # if already Date
  if (inherits(v, "Date") || inherits(v, "POSIXt")) return(format(as.Date(v), "%Y-%m-%d"))
  d <- parse_date11(v)
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
})
# Study Day
sdtm_tu1$TUSTDY <- mapply(function(tudtc, rfstdtc) {
  if (is.na(tudtc) || is.na(rfstdtc)) return(NA_integer_)
  if (trimws(as.character(tudtc)) == "" || trimws(as.character(rfstdtc)) == "") return(NA_integer_)
  stdt <- parse_yymmdd10(tudtc)
  rfdt <- parse_yymmdd10(rfstdtc)
  if (is.na(stdt) || is.na(rfdt)) return(NA_integer_)
  diff <- as.integer(stdt - rfdt)
  if (stdt >= rfdt) diff + 1L else diff
}, sdtm_tu1$TUDTC, sdtm_tu1$RFSTDTC, USE.NAMES = FALSE)
sdtm_tu1$TUSTDY <- as.integer(sdtm_tu1$TUSTDY)

# 4. Derive TUSEQ - sort by USUBJID TUDTC
# SAS missing sorts first, so NA TUDTC first
ord <- order(sdtm_tu1$USUBJID, sdtm_tu1$TUDTC, na.last = FALSE)
sdtm_tu1 <- sdtm_tu1[ord, , drop = FALSE]
# retain TUSEQ by USUBJID
sdtm_tu1$TUSEQ <- ave(seq_len(nrow(sdtm_tu1)), sdtm_tu1$USUBJID, FUN = function(x) seq_along(x))
sdtm_tu1$TUSEQ <- as.integer(sdtm_tu1$TUSEQ)

# Keep final columns in SAS order
keep_cols <- c("STUDYID", "DOMAIN", "USUBJID", "TUSEQ", "TULNKID", "TUTESTCD", "TUTEST", "TUORRES", "TUSTRESC", "TULOC", "TUMETHOD", "TUEVAL", "TUDTC", "TUSTDY", "VISIT")
# ensure all exist
for (cc in keep_cols) if (!cc %in% names(sdtm_tu1)) sdtm_tu1[[cc]] <- NA_character_
sdtm_tu <- sdtm_tu1[, keep_cols, drop = FALSE]
# final sort by USUBJID TUDTC (which is USUBJID TUSEQ order)
sdtm_tu <- sdtm_tu[order(sdtm_tu$USUBJID, sdtm_tu$TUSEQ, na.last = FALSE), , drop = FALSE]
# ensure types
sdtm_tu$TUSEQ <- as.integer(sdtm_tu$TUSEQ)
sdtm_tu$TUSTDY <- as.integer(sdtm_tu$TUSTDY)
# replace NA character with NA for CSV na=""
# 5. View Final Dataset - write outputs
write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")

# Create listing text rendering (PROC PRINT)
# Build text file with SAS-like print
make_listing_text <- function(df, title = "SDTM TU Domain") {
  lines <- c(title, paste(rep("-", nchar(title)), collapse = ""), "")
  # header
  hdr <- paste(sprintf("%-4s", "Obs"),
               paste(sprintf("%-12s", names(df)), collapse = " "),
               sep = " | ")
  lines <- c(lines, hdr, paste(rep("-", 150), collapse = ""))
  for (i in seq_len(nrow(df))) {
    row_vals <- sapply(df[i, , drop = FALSE], function(v) {
      if (is.na(v)) "" else as.character(v)
    })
    # format row
    obs_str <- sprintf("%-4d", i)
    row_str <- paste(sprintf("%-12s", row_vals), collapse = " ")
    lines <- c(lines, paste(obs_str, row_str, sep = " | "))
  }
  lines <- c(lines, "", paste("Total observations:", nrow(df)))
  lines
}
listing_lines <- make_listing_text(sdtm_tu, "SDTM TU Domain - P029")
# write primary listing files
writeLines(listing_lines, file.path(out, "P029.lst.txt"))
writeLines(listing_lines, file.path(out, "P029.txt"))
writeLines(listing_lines, file.path(out, "tu.txt"))
# CSV of table body for listing
write.csv(sdtm_tu, file.path(out, "P029.lst.csv"), row.names = FALSE, na = "")
write.csv(sdtm_tu, file.path(out, "P029.csv"), row.names = FALSE, na = "")
# Also ensure dataset tu listing text contains numbers - already done
# Print to console for log
cat("P029 TU mapping complete. Rows:", nrow(sdtm_tu), "\n")
print(head(sdtm_tu))
