```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_lab.csv"

# helper: SAS BEST. (BEST12.) formatting via formatC g with 12 significant digits
sas_best <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    # SAS BEST12. : up to 12 significant digits, no trailing blanks
    s <- formatC(v, format = "g", digits = 12, flag = "")
    trimws(s)
  }, USE.NAMES = FALSE)
}

# helper: SAS CATX
sas_catx <- function(sep, ...) {
  args <- list(...)
  # strip each, skip missing/blank
  sapply(seq_along(args[[1]]), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(as.character(v))
      if (v == "" || v == ".") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return(NA_character_)
    paste(parts, collapse = sep)
  }, USE.NAMES = FALSE)
}

# 1. IMPORT RAW LAB DATA (PROC IMPORT CSV GETNAMES=YES)
# Read everything as character to emulate SAS scanning then apply SAS types
raw_all <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                           na = c("", "NA"), trim_ws = TRUE, progress = FALSE, show_col_types = FALSE)
names(raw_all) <- toupper(trimws(names(raw_all)))
# Expected columns (case-insensitive, validvarname=upcase)
# SUBJ, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, LAB_DAT
stopifnot(all(c("SUBJ","VISIT_NAM","VISIT_NUM","TEST_NAME","RESULT","UNIT","LAB_DAT") %in% names(raw_all)))

# Preserve raw character vectors
n <- nrow(raw_all)
SUBJ_raw <- trimws(as.character(raw_all$SUBJ))
VISIT_NAM_raw <- as.character(raw_all$VISIT_NAM)
VISIT_NUM_raw <- trimws(as.character(raw_all$VISIT_NUM))
TEST_NAME_raw <- as.character(raw_all$TEST_NAME)
RESULT_raw <- trimws(as.character(raw_all$RESULT))
UNIT_raw <- as.character(raw_all$UNIT)
LAB_DAT_raw <- trimws(as.character(raw_all$LAB_DAT))

# SAS PROC IMPORT type guessing for numeric columns: VISIT_NUM, RESULT numeric
VISIT_NUM <- suppressWarnings(as.numeric(VISIT_NUM_raw))
RESULT <- suppressWarnings(as.numeric(RESULT_raw))
# If conversion fails for non-numeric values, keep NA (SAS would have made char; but these are numeric)

# SUBJ handling for USUBJID CATX: emulate PROC IMPORT numeric vs char
# If all non-missing SUBJ values are integer-like, SAS makes it numeric -> CATX strips leading zeros via BEST.
subj_nonmiss <- SUBJ_raw[!is.na(SUBJ_raw) & SUBJ_raw != "" & SUBJ_raw != "."]
subj_all_numeric <- length(subj_nonmiss) > 0 && all(grepl("^[0-9]+(\\.[0-9]*)?$", subj_nonmiss))
if (subj_all_numeric) {
  SUBJ_for_id <- sas_best(suppressWarnings(as.numeric(SUBJ_raw)))
  SUBJ_for_id[is.na(SUBJ_raw) | SUBJ_raw == ""] <- NA_character_
} else {
  SUBJ_for_id <- SUBJ_raw
}

# 2. CORE TRANSFORMATION & DICTIONARY MAPPING
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("LB", n)
# Truncate to lengths: STUDYID $8, DOMAIN $2
STUDYID <- substr(STUDYID, 1, 8)
DOMAIN <- substr(DOMAIN, 1, 2)

USUBJID <- paste0(STUDYID, "-", ifelse(is.na(SUBJ_for_id), "", trimws(SUBJ_for_id)))
# SAS CATX skips missing second part: if SUBJ missing, USUBJID would be just STUDYID
USUBJID[is.na(SUBJ_for_id) | trimws(SUBJ_for_id) == ""] <- STUDYID[is.na(SUBJ_for_id) | trimws(SUBJ_for_id) == ""]
USUBJID <- substr(USUBJID, 1, 40)

VISIT <- trimws(VISIT_NAM_raw)
VISIT[is.na(VISIT_NAM_raw)] <- NA_character_
VISITNUM <- VISIT_NUM

# TEST mapping: select(upcase(strip(TEST_NAME)))
test_up <- toupper(trimws(TEST_NAME_raw))
LBTESTCD <- character(n)
LBTEST <- character(n)
LBCAT <- character(n)
for (i in seq_len(n)) {
  tu <- test_up[i]
  if (is.na(tu) || trimws(tu) == "") {
    LBTESTCD[i] <- "UNKNOWN"
    LBTEST[i] <- ifelse(is.na(TEST_NAME_raw[i]), NA_character_, TEST_NAME_raw[i])
    LBCAT[i] <- "UNKNOWN"
  } else if (tu == "GLUCOSE") {
    LBTESTCD[i] <- "GLUC"; LBTEST[i] <- "Glucose"; LBCAT[i] <- "CHEMISTRY"
  } else if (tu == "HEMOGLOBIN") {
    LBTESTCD[i] <- "HGB"; LBTEST[i] <- "Hemoglobin"; LBCAT[i] <- "HEMATOLOGY"
  } else if (tu == "ALT") {
    LBTESTCD[i] <- "ALT"; LBTEST[i] <- "Alanine Aminotransferase"; LBCAT[i] <- "CHEMISTRY"
  } else if (tu == "AST") {
    LBTESTCD[i] <- "AST"; LBTEST[i] <- "Aspartate Aminotransferase"; LBCAT[i] <- "CHEMISTRY"
  } else {
    LBTESTCD[i] <- "UNKNOWN"; LBTEST[i] <- TEST_NAME_raw[i]; LBCAT[i] <- "UNKNOWN"
  }
}
LBTESTCD <- substr(LBTESTCD, 1, 8)
LBTEST <- substr(LBTEST, 1, 40)
LBCAT <- substr(LBCAT, 1, 20)

# Results formatting: LBORRES = strip(put(RESULT,best.))
LBORRES <- character(n)
for (i in seq_len(n)) {
  if (is.na(RESULT[i])) {
    # SAS put(.,best.) = "."
    # But if RESULT_raw was truly missing/blank, SAS RESULT would be . -> LBORRES="."
    LBORRES[i] <- "."
  } else {
    LBORRES[i] <- sas_best(RESULT[i])
  }
}
LBORRES <- substr(LBORRES, 1, 20)
LBSTRESN <- RESULT
LBSTRESC <- substr(LBORRES, 1, 20)

# Units
LBORRESU <- trimws(UNIT_raw)
LBORRESU[is.na(UNIT_raw)] <- NA_character_
LBORRESU <- substr(LBORRESU, 1, 20)
LBSTRESU <- character(n)
for (i in seq_len(n)) {
  u <- LBORRESU[i]
  if (is.na(u) || trimws(u) == "") {
    LBSTRESU[i] <- u
  } else if (toupper(trimws(u)) == "MG/DL") {
    LBSTRESU[i] <- "mg/dL"
  } else if (toupper(trimws(u)) == "G/DL") {
    LBSTRESU[i] <- "g/dL"
  } else if (toupper(trimws(u)) == "U/L") {
    LBSTRESU[i] <- "U/L"
  } else {
    LBSTRESU[i] <- trimws(u)
  }
}
LBSTRESU <- substr(LBSTRESU, 1, 20)

# ISO 8601 DATE CONVERSION
# Emulate: if vtype(LAB_DAT)='C' then _lab_num=input(strip(LAB_DAT),anydtdte.) else _lab_num=LAB_DAT
# plus PROC IMPORT single-informat-per-column rule: decide order once per column
parse_lab <- function(vec) {
  # vec: character vector as read (trimmed), may contain NA/""
  v <- trimws(as.character(vec))
  v[v == ""] <- NA_character_
  v[v == "."] <- NA_character_
  nonmiss <- v[!is.na(v)]
  dates <- rep(as.Date(NA), length(v))
  lab_num <- rep(NA_real_, length(v))
  if (length(nonmiss) == 0) {
    return(list(dates = dates, lab_num = lab_num))
  }
  # Check if all non-missing are pure numbers (SAS date numbers)
  all_numeric <- all(grepl("^-?[0-9]+(\\.[0-9]*)?$", nonmiss))
  if (all_numeric) {
    lab_num <- suppressWarnings(as.numeric(v))
    for (i in seq_along(v)) {
      if (!is.na(lab_num[i])) dates[i] <- as.Date(lab_num[i], origin = "1960-01-01")
    }
    return(list(dates = dates, lab_num = lab_num))
  }
  # Otherwise character dates: decide ONE format for whole column
  # Detect patterns
  is_slash <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", nonmiss)
  is_dashnum <- grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", nonmiss)
  is_iso <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", nonmiss)
  is_dmon <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", nonmiss)
  is_slash4 <- grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", nonmiss)
  fmt <- NULL
  order_mdy_dmy <- NULL
  if (all(is_slash)) {
    # Decide MDY vs DMY once: DDMMYY10. when any first field >12 else MMDDYY10.
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", nonmiss)))
    if (any(!is.na(first_fields) & first_fields > 12)) {
      order_mdy_dmy <- "DMY"
    } else {
      order_mdy_dmy <- "MDY"
    }
    # year digits
    year_parts <- sub("^.*/([^/]*)$", "\\1", nonmiss)
    if (all(nchar(year_parts) == 2)) {
      fmt <- if (order_mdy_dmy == "DMY") "%d/%m/%y" else "%m/%d/%y"
    } else {
      fmt <- if (order_mdy_dmy == "DMY") "%d/%m/%Y" else "%m/%d/%Y"
    }
  } else if (all(is_dashnum)) {
    first_fields <- suppressWarnings(as.integer(sub("-.*$", "", nonmiss)))
    if (any(!is.na(first_fields) & first_fields > 12)) order_mdy_dmy <- "DMY" else order_mdy_dmy <- "MDY"
    year_parts <- sub("^.*-([^-]*)$", "\\1", nonmiss)
    if (all(nchar(year_parts) == 2)) {
      fmt <- if (order_mdy_dmy == "DMY") "%d-%m-%y" else "%m-%d-%y"
    } else {
      fmt <- if (order_mdy_dmy == "DMY") "%d-%m-%Y" else "%m-%d-%Y"
    }
  } else if (all(is_iso)) {
    fmt <- "%Y-%m-%d"
  } else if (all(is_dmon)) {
    # need year digits
    year_parts <- sub("^.*-([^-]*)$", "\\1", nonmiss)
    if (all(nchar(year_parts) == 2)) fmt <- "%d-%b-%y" else fmt <- "%d-%b-%Y"
  } else if (all(is_slash4)) {
    fmt <- "%Y/%m/%d"
  } else {
    # Mixed or other: default to slash rule if contains "/", else try to infer from first value
    # Still use ONE format: decide MDY/DMY by first-field>12 if slash present
    has_slash <- grepl("/", nonmiss)
    if (all(has_slash)) {
      first_fields <- suppressWarnings(as.integer(sub("/.*$", "", nonmiss)))
      if (any(!is.na(first_fields) & first_fields > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
    } else if (all(grepl("^[0-9]{4}-", nonmiss))) {
      fmt <- "%Y-%m-%d"
    } else {
      # fallback single format MDY with 4-digit year
      # check first field >12 to choose DMY
      tmp <- nonmiss[grepl("^[0-9]{1,2}[/-]", nonmiss)]
      if (length(tmp) > 0) {
        ff <- suppressWarnings(as.integer(sub("[/-].*$", "", tmp)))
        if (any(!is.na(ff) & ff > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
      } else {
        fmt <- "%Y-%m-%d"
      }
    }
  }
  # Parse every value with that single format (strict)
  # For %b month names, allow any case by upper/lower handling via as.Date (case depends); normalize to title case
  for (i in seq_along(v)) {
    if (is.na(v[i])) next
    val <- v[i]
    # normalize month abbrev case for %b: e.g., "15-jan-2023" -> "15-Jan-2023"
    if (grepl("%b", fmt, fixed = TRUE)) {
      # capitalize first letter of alpha sequences
      val <- gsub("([A-Za-z]+)", "\\1", val)
      # convert month part to title case
      parts <- strsplit(val, "-")[[1]]
      if (length(parts) == 3) {
        parts[2] <- paste0(toupper(substr(parts[2],1,1)), tolower(substr(parts[2],2,3)))
        val <- paste(parts, collapse = "-")
      }
    }
    d <- suppressWarnings(as.Date(val, format = fmt))
    dates[i] <- d
    if (!is.na(d)) lab_num[i] <- as.numeric(d - as.Date("1960-01-01"))
  }
  list(dates = dates, lab_num = lab_num, fmt = fmt)
}

lab_parsed <- parse_lab(LAB_DAT_raw)
LBDTC <- format(lab_parsed$dates, "%Y-%m-%d")
LBDTC[is.na(lab_parsed$dates)] <- NA_character_

lb_mapped <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  VISITNUM = VISITNUM, VISIT = VISIT,
  LBTESTCD = LBTESTCD, LBTEST = LBTEST, LBCAT = LBCAT,
  LBORRES = LBORRES, LBORRESU = LBORRESU,
  LBSTRESC = LBSTRESC, LBSTRESN = LBSTRESN, LBSTRESU = LBSTRESU,
  LBDTC = LBDTC, stringsAsFactors = FALSE
)

# 3. GENERATE SEQUENCE NUMBER (LBSEQ) : sort by USUBJID LBDTC LBTESTCD
# SAS sorts: missing first, character uppercase before lowercase (byte order)
# Use order with na.last=FALSE; for byte-order fidelity, order via xtfrm on raw bytes is same for uppercase data
ord <- order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD, na.last = FALSE, method = "radix")
lb_mapped <- lb_mapped[ord, , drop = FALSE]

# BY USUBJID retain counter
LBSEQ <- integer(nrow(lb_mapped))
if (nrow(lb_mapped) > 0) {
  cur <- ""
  cnt <- 0L
  for (i in seq_len(nrow(lb_mapped))) {
    u <- lb_mapped$USUBJID[i]
    if (is.na(u)) u <- "__NA__"
    if (u != cur) { cnt <- 1L; cur <- u } else { cnt <- cnt + 1L }
    LBSEQ[i] <- cnt
  }
}
sdtm_lb <- data.frame(
  STUDYID = lb_mapped$STUDYID, DOMAIN = lb_mapped$DOMAIN, USUBJID = lb_mapped$USUBJID,
  LBSEQ = LBSEQ,
  VISITNUM = lb_mapped$VISITNUM, VISIT = lb_mapped$VISIT,
  LBTESTCD = lb_mapped$LBTESTCD, LBTEST = lb_mapped$LBTEST, LBCAT = lb_mapped$LBCAT,
  LBORRES = lb_mapped$LBORRES, LBORRESU = lb_mapped$LBORRESU,
  LBSTRESC = lb_mapped$LBSTRESC, LBSTRESN = lb_mapped$LBSTRESN, LBSTRESU = lb_mapped$LBSTRESU,
  LBDTC = lb_mapped$LBDTC, stringsAsFactors = FALSE
)

# Write dataset lb
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT : PROC PRINT obs=12 vars USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC
print_vars <- c("USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
stopifnot(all(print_vars %in% names(sdtm_lb)))
audit <- head(sdtm_lb[, print_vars, drop = FALSE], 12)
audit_with_obs <- data.frame(Obs = seq_len(nrow(audit)), audit, stringsAsFactors = FALSE, check.names = FALSE)

# CSV of table body
write.csv(audit_with_obs, file.path(out, "P015.csv"), row.names = FALSE, na = "")

# Text rendering (120 numbers context: include full listing text)
title_line <- "LB Domain Audit (Laboratory Test Results)"
# Build fixed-width text similar to PROC PRINT
# Format numerics with SAS BEST for display
fmt_cell <- function(col, val) {
  if (is.na(val) || val == "") return("")
  as.character(val)
}
# Determine column widths
cols <- names(audit_with_obs)
# Header labels same as variable names
widths <- sapply(cols, function(cn) {
  vals <- as.character(audit_with_obs[[cn]])
  vals[is.na(vals)] <- ""
  # for numeric, format with sas_best
  if (cn %in% c("VISITNUM","LBSTRESN","Obs")) {
    numv <- suppressWarnings(as.numeric(audit_with_obs[[cn]]))
    vals <- sapply(seq_along(numv), function(i) if (is.na(numv[i])) "" else sas_best(numv[i]))
  }
  max(nchar(c(cn, vals)), na.rm = TRUE)
})
# Ensure minimum widths
widths <- pmax(widths, sapply(cols, nchar))

pad_r <- function(s, w) sprintf(paste0("%-", w, "s"), s)
pad_l <- function(s, w) sprintf(paste0("%", w, "s"), s)

# Numeric columns right-aligned like SAS, character left-aligned
num_cols <- c("Obs","VISITNUM","LBSTRESN")

header_line <- paste(mapply(function(cn, w) {
  if (cn %in% num_cols) pad_l(cn, w) else pad_r(cn, w)
}, cols, widths), collapse = "  ")
sep_line <- paste(mapply(function(w) paste(rep("-", w), collapse=""), widths), collapse = "  ")

body_lines <- sapply(seq_len(nrow(audit_with_obs)), function(r) {
  paste(mapply(function(cn, w) {
    v <- audit_with_obs[r, cn]
    if (is.na(v) || (is.character(v) && trimws(as.character(v))=="")) s <- ""
    else if (cn %in% num_cols) {
      nv <- suppressWarnings(as.numeric(v))
      if (is.na(nv)) s <- as.character(v) else s <- sas_best(nv)
    } else s <- as.character(v)
    if (cn %in% num_cols) pad_l(s, w) else pad_r(s, w)
  }, cols, widths), collapse = "  ")
})

txt_lines <- c(title_line, "", header_line, sep_line, body_lines, "")
txt_path <- file.path(out, "P015.txt")
writeLines(txt_lines, txt_path)
# Also write P015.lst copy for benchmark compatibility
writeLines(txt_lines, file.path(out, "P015.lst"))

cat("P015 complete: lb rows", nrow(sdtm_lb), "audit rows", nrow(audit), "\n")
```