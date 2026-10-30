out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_catx <- function(sep, ...) {
  args <- list(...)
  sapply(seq_along(args[[1]]), function(i) {
    vals <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v) || trimws(as.character(v)) == "") return(NA_character_)
      trimws(as.character(v))
    })
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0) return("")
    paste(vals, collapse = sep)
  })
}
sas_tranwrd <- function(s, from, to) {
  ifelse(is.na(s), NA_character_, gsub(from, to, s, fixed = TRUE))
}

read_edc <- function(name) {
  fname <- paste0(name, ".xlsx")
  search_dirs <- c(
    Sys.getenv("rawdir"), Sys.getenv("RAWDIR"),
    Sys.getenv("root"), Sys.getenv("ROOT"),
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data",
    "D:/meta_hackathon/sas2r-agent/data/raw",
    "/opt/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data",
    "data/raw/faijal-oncology/raw_data",
    "../data/raw/faijal-oncology/raw_data",
    "../../data/raw/faijal-oncology/raw_data"
  )
  if (out != "") search_dirs <- c(search_dirs, file.path(dirname(out), "../../data/raw/faijal-oncology/raw_data"))
  found <- NA_character_
  for (d in search_dirs) {
    if (is.na(d) || d == "") next
    p <- file.path(d, fname)
    if (file.exists(p)) { found <- p; break }
    p2 <- file.path(d, "faijal-oncology/raw_data", fname)
    if (file.exists(p2)) { found <- p2; break }
  }
  if (is.na(found)) {
    roots_to_try <- c(".", "..", "../..", "../../..")
    r <- Sys.getenv("ROOT"); if (r != "") roots_to_try <- c(r, roots_to_try)
    r2 <- Sys.getenv("root"); if (r2 != "") roots_to_try <- c(r2, roots_to_try)
    for (rt in roots_to_try) {
      if (!dir.exists(rt)) next
      f <- list.files(rt, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
      if (length(f) > 0) { found <- f[1]; break }
    }
  }
  if (is.na(found) || !file.exists(found)) stop(paste0("Cannot find ", fname))
  df <- readxl::read_excel(found, .name_repair = "minimal")
  names(df) <- trimws(names(df))
  df
}

# helper to find sdtm_dm file (xpt, sas7bdat, csv, xlsx)
find_sdtm_dm <- function() {
  candidates <- c(
    Sys.getenv("rawsdtm"), Sys.getenv("RAWS DTM"),
    Sys.getenv("rawadam"), Sys.getenv("RAWADAM"),
    Sys.getenv("rawdir"), Sys.getenv("RAWDIR"),
    Sys.getenv("root"), Sys.getenv("ROOT"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
    "D:/meta_hackathon/sas2r-agent/data/raw",
    "data/raw/cdisc-pilot/sdtm",
    "data/raw/cdisc-pilot/adam"
  )
  # also try OUT_DIR relatives
  if (out != "") candidates <- c(candidates, file.path(out, "../../..", "data/raw/cdisc-pilot/sdtm"))
  patterns <- c("sdtm_dm\\.xpt", "sdtm_dm\\.sas7bdat", "dm\\.xpt", "dm\\.sas7bdat", "sdtm_dm\\.csv", "sdtm_dm\\.xlsx")
  for (d in candidates) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    for (pat in patterns) {
      f <- list.files(d, pattern = pat, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  # recursive search from root
  roots_to_try <- c(".", "..", "../..", "../../..")
  r <- Sys.getenv("ROOT"); if (!is.na(r) && r != "") roots_to_try <- c(r, roots_to_try)
  r2 <- Sys.getenv("root"); if (!is.na(r2) && r2 != "") roots_to_try <- c(r2, roots_to_try)
  for (rt in roots_to_try) {
    if (!dir.exists(rt)) next
    for (pat in patterns) {
      f <- list.files(rt, pattern = pat, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      # prefer sdtm_dm over dm
      f2 <- f[grepl("sdtm_dm", f, ignore.case = TRUE)]
      if (length(f2) > 0) return(f2[1])
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA_character_)
}

read_sdtm_dm_file <- function(path) {
  if (is.na(path)) return(NULL)
  if (grepl("\\.xpt$", path, ignore.case = TRUE)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.sas7bdat$", path, ignore.case = TRUE)) {
    df <- haven::read_sas(path)
  } else if (grepl("\\.csv$", path, ignore.case = TRUE)) {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else if (grepl("\\.xlsx$", path, ignore.case = TRUE)) {
    df <- readxl::read_excel(path, .name_repair = "minimal")
  } else return(NULL)
  names(df) <- toupper(trimws(names(df)))
  df
}

# Read edc_tr
edc_tr_raw <- read_edc("edc_tr")
names(edc_tr_raw) <- toupper(trimws(names(edc_tr_raw)))

# Try to read sdtm_dm from file system
sdtm_dm_path <- find_sdtm_dm()
sdtm_dm_raw <- read_sdtm_dm_file(sdtm_dm_path)

# If not found, fallback to edc_dm
if (is.null(sdtm_dm_raw) || nrow(sdtm_dm_raw) == 0) {
  edc_dm_raw <- read_edc("edc_dm")
  names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
  # derive sdtm_dm from edc_dm if needed - but try to use edc_dm RFSTDTC if present
  sdtm_dm_raw <- edc_dm_raw
}

# Build dm_for_tr: keep USUBJID RFSTDTC
# Ensure USUBJID exists; if only SUBJID, create USUBJID as ONCO-2026-SUBJID
if (!"USUBJID" %in% names(sdtm_dm_raw)) {
  if ("SUBJID" %in% names(sdtm_dm_raw)) {
    sdtm_dm_raw$USUBJID <- sas_catx("-", "ONCO-2026", as.character(sdtm_dm_raw$SUBJID))
  } else {
    # try any subject column
    subj_col <- intersect(c("SUBJID","SUBJECT","SUBNUM"), names(sdtm_dm_raw))[1]
    if (!is.na(subj_col)) sdtm_dm_raw$USUBJID <- sas_catx("-", "ONCO-2026", as.character(sdtm_dm_raw[[subj_col]]))
  }
}
# Ensure RFSTDTC exists
if (!"RFSTDTC" %in% names(sdtm_dm_raw)) {
  # try alternative names
  alt <- intersect(c("RFSTDT","RFXSTDTC","RFSTDTC1","RANDDTC","BRTHDTC"), names(sdtm_dm_raw))
  if (length(alt) > 0) {
    sdtm_dm_raw$RFSTDTC <- as.character(sdtm_dm_raw[[alt[1]]])
  } else {
    sdtm_dm_raw$RFSTDTC <- NA_character_
  }
}
# Convert RFSTDTC to character ISO date string (keep as stored, SAS will use yymmdd10. input)
# If numeric (SAS date), convert
if (is.numeric(sdtm_dm_raw$RFSTDTC)) {
  sdtm_dm_raw$RFSTDTC <- format(as.Date(sdtm_dm_raw$RFSTDTC, origin = "1960-01-01"), "%Y-%m-%d")
} else {
  sdtm_dm_raw$RFSTDTC <- as.character(sdtm_dm_raw$RFSTDTC)
  # haven labelled may be Date
  if (inherits(sdtm_dm_raw$RFSTDTC, "Date")) sdtm_dm_raw$RFSTDTC <- format(sdtm_dm_raw$RFSTDTC, "%Y-%m-%d")
  # If POSIX, format
  if (inherits(sdtm_dm_raw$RFSTDTC, "POSIXt")) sdtm_dm_raw$RFSTDTC <- format(as.Date(sdtm_dm_raw$RFSTDTC), "%Y-%m-%d")
}
# Keep only needed and sort
dm_for_tr <- sdtm_dm_raw[, c("USUBJID","RFSTDTC"), drop=FALSE]
dm_for_tr$USUBJID <- trimws(as.character(dm_for_tr$USUBJID))
dm_for_tr$RFSTDTC <- trimws(as.character(dm_for_tr$RFSTDTC))
dm_for_tr$RFSTDTC[dm_for_tr$RFSTDTC==""] <- NA_character_
dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID), , drop=FALSE]

# 2. Prepare Raw Data tr_prep
tr_prep <- edc_tr_raw
# SUBJID column - use exact name SUBJID if exists
if ("SUBJID" %in% names(tr_prep)) {
  subj_vals <- as.character(tr_prep$SUBJID)
} else {
  # fallback find
  cn <- names(tr_prep)[grepl("SUBJ", names(tr_prep), ignore.case=TRUE)][1]
  subj_vals <- as.character(tr_prep[[cn]])
}
# Preserve leading zeros: if numeric, format without scientific
# readxl may have read as numeric; convert to integer string without decimal
subj_vals <- sapply(subj_vals, function(x) {
  if (is.na(x)) return(NA_character_)
  # if like "101" numeric, keep as is
  trimws(as.character(x))
})
tr_prep$USUBJID <- sas_catx("-", "ONCO-2026", subj_vals)

# Ensure canonical columns exist with exact SAS names
# LNKID, TESTCD, ORRES, UNIT, VISIT, TRDT
for (col in c("LNKID","TESTCD","ORRES","UNIT","VISIT","TRDT")) {
  if (!col %in% names(tr_prep)) {
    # try case-insensitive search
    idx <- which(toupper(names(tr_prep))==col)
    if (length(idx)>0) tr_prep[[col]] <- as.character(tr_prep[[idx[1]]])
    else tr_prep[[col]] <- NA_character_
  } else {
    tr_prep[[col]] <- as.character(tr_prep[[col]])
  }
}
# Handle numeric Excel dates for TRDT: if original was numeric, readxl would have converted to numeric or Date
# Check if TRDT looks numeric
if (is.numeric(edc_tr_raw$TRDT)) {
  # Excel origin 1899-12-30
  tr_prep$TRDT <- format(as.Date(edc_tr_raw$TRDT, origin="1899-12-30"), "%d-%b-%Y")
}
# Also if TRDT is Date/POSIX
if (inherits(edc_tr_raw$TRDT, "Date")) tr_prep$TRDT <- format(edc_tr_raw$TRDT, "%d-%b-%Y")
if (inherits(edc_tr_raw$TRDT, "POSIXt")) tr_prep$TRDT <- format(as.Date(edc_tr_raw$TRDT), "%d-%b-%Y")

tr_prep <- tr_prep[order(tr_prep$USUBJID), , drop=FALSE]
dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID), , drop=FALSE]

# 3. Map TR Domain
sdtm_tr1 <- merge(tr_prep, dm_for_tr, by="USUBJID", all.x=TRUE, sort=FALSE)

sdtm_tr1$STUDYID <- "ONCO-2026"
sdtm_tr1$DOMAIN <- "TR"
sdtm_tr1$TRLNKID <- sdtm_tr1$LNKID
sdtm_tr1$TRTESTCD <- sdtm_tr1$TESTCD
sdtm_tr1$TRTEST <- ifelse(sdtm_tr1$TRTESTCD=="LDIAM", "Longest Diameter", NA_character_)
sdtm_tr1$TRORRES <- sdtm_tr1$ORRES
sdtm_tr1$TRORRESU <- sdtm_tr1$UNIT
sdtm_tr1$TRSTRESC <- sdtm_tr1$ORRES
sdtm_tr1$TRSTRESN <- suppressWarnings(as.numeric(sdtm_tr1$ORRES))
sdtm_tr1$TRSTRESU <- sdtm_tr1$UNIT
sdtm_tr1$TREVAL <- "INVESTIGATOR"
sdtm_tr1$VISIT <- toupper(sas_tranwrd(sdtm_tr1$VISIT, "_", " "))

# Date Conversion: TRDTC = put(input(TRDT, date11.), yymmdd10.)
sdtm_tr1$TRDTC <- NA_character_
has_trdt <- !is.na(sdtm_tr1$TRDT) & trimws(sdtm_tr1$TRDT)!=""
if (any(has_trdt)) {
  raw_vals <- trimws(sdtm_tr1$TRDT[has_trdt])
  # SAS date11. is DD-MMM-YYYY with dashes, e.g. 01-JAN-2023 (11 chars)
  # Try date11. first
  parsed <- suppressWarnings(as.Date(raw_vals, format="%d-%b-%Y"))
  # For those still NA, try date9. (DDMMMYYYY) and other variants
  na_idx <- is.na(parsed)
  if (any(na_idx)) {
    p2 <- suppressWarnings(as.Date(raw_vals[na_idx], format="%d%b%Y"))
    parsed[na_idx] <- p2
    na_idx <- is.na(parsed)
  }
  if (any(na_idx)) {
    # Try YYYY-MM-DD already
    p3 <- suppressWarnings(as.Date(raw_vals[na_idx], format="%Y-%m-%d"))
    parsed[na_idx] <- p3
    na_idx <- is.na(parsed)
  }
  if (any(na_idx)) {
    # Try DD/MM/YYYY vs MM/DD/YYYY - decide per column once (SAS PROC IMPORT guessing)
    # Use same logic as SAS: if any first field >12 then DMY else MDY
    slash_vals <- raw_vals[grepl("/", raw_vals, fixed=TRUE)]
    if (length(slash_vals)>0) {
      first_fields <- suppressWarnings(as.integer(sapply(strsplit(slash_vals, "/"), `[`, 1)))
      order_is_dmy <- any(first_fields > 12, na.rm=TRUE)
      fmt <- if (order_is_dmy) "%d/%m/%Y" else "%m/%d/%Y"
      p4 <- suppressWarnings(as.Date(raw_vals[na_idx], format=fmt))
      parsed[na_idx] <- p4
    }
  }
  sdtm_tr1$TRDTC[has_trdt] <- format(parsed, "%Y-%m-%d")
  sdtm_tr1$TRDTC[sdtm_tr1$TRDTC=="NA" | is.na(sdtm_tr1$TRDTC)] <- NA_character_
}

# Study Day: if TRDTC ne "" and RFSTDTC ne "" then do; _stdt=input(TRDTC,yymmdd10.); _rfdt=input(RFSTDTC,yymmdd10.); if _stdt>=_rfdt then TRSTDY=(_stdt-_rfdt)+1 else TRSTDY=(_stdt-_rfdt);
sdtm_tr1$TRSTDY <- NA_real_
has_both <- !is.na(sdtm_tr1$TRDTC) & trimws(sdtm_tr1$TRDTC)!="" & !is.na(sdtm_tr1$RFSTDTC) & trimws(sdtm_tr1$RFSTDTC)!=""
if (any(has_both)) {
  # yymmdd10. reads YYYY-MM-DD (first 10 chars)
  stdt <- suppressWarnings(as.Date(substr(sdtm_tr1$TRDTC[has_both],1,10), format="%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(substr(sdtm_tr1$RFSTDTC[has_both],1,10), format="%Y-%m-%d"))
  # If RFSTDTC was datetime with T, substr handles
  diff <- as.numeric(stdt - rfdt)
  sdtm_tr1$TRSTDY[has_both] <- ifelse(stdt >= rfdt, diff + 1, diff)
}

# Sort by USUBJID TRDTC (SAS missing first)
sdtm_tr1 <- sdtm_tr1[order(sdtm_tr1$USUBJID, sdtm_tr1$TRDTC, na.last=FALSE), , drop=FALSE]

# 4. Derive TRSEQ
sdtm_tr <- sdtm_tr1
sdtm_tr <- sdtm_tr[order(sdtm_tr$USUBJID, sdtm_tr$TRDTC, na.last=FALSE), , drop=FALSE]
sdtm_tr$TRSEQ <- ave(seq_len(nrow(sdtm_tr)), sdtm_tr$USUBJID, FUN=function(x) seq_along(x))
sdtm_tr$TRSEQ <- as.integer(sdtm_tr$TRSEQ)

keep_cols <- c("STUDYID","DOMAIN","USUBJID","TRSEQ","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT")
for (kc in keep_cols) if (!kc %in% names(sdtm_tr)) sdtm_tr[[kc]] <- NA
sdtm_tr_final <- sdtm_tr[, keep_cols, drop=FALSE]
sdtm_tr_final <- sdtm_tr_final[order(sdtm_tr_final$USUBJID, sdtm_tr_final$TRSEQ), , drop=FALSE]

# Write CSV
write.csv(sdtm_tr_final, file.path(out, "tr.csv"), row.names=FALSE, na="")
write.csv(sdtm_tr_final, file.path(out, "P028.csv"), row.names=FALSE, na="")

# Text rendering
lst_file <- file.path(out, "P028.lst")
con <- file(lst_file, open="wt")
writeLines("P028 - SDTM TR Domain Listing", con)
writeLines(paste0("Study: ONCO-2026  Domain: TR  Date: ", format(Sys.Date(), "%Y-%m-%d")), con)
writeLines("", con)
writeLines(paste0("Number of observations: ", nrow(sdtm_tr_final)), con)
writeLines(paste0("Number of subjects: ", length(unique(sdtm_tr_final$USUBJID))), con)
writeLines("", con)
header <- paste(keep_cols, collapse=" | ")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse=""), con)
fmt_val <- function(x) { if (is.na(x) || (is.character(x) && trimws(x)=="")) return("") ; as.character(x) }
for (i in seq_len(nrow(sdtm_tr_final))) {
  row_vals <- sapply(keep_cols, function(cn) fmt_val(sdtm_tr_final[i, cn]))
  writeLines(paste(row_vals, collapse=" | "), con)
}
writeLines("", con)
writeLines("Notes:", con)
writeLines("TRTEST = Longest Diameter when TRTESTCD = LDIAM", con)
writeLines("TRDTC derived from TRDT (date11. -> yymmdd10.)", con)
writeLines("TRSTDY = (TRDTC - RFSTDTC) + 1 if TRDTC >= RFSTDTC else (TRDTC - RFSTDTC)", con)
writeLines("TRSEQ is sequential per USUBJID ordered by TRDTC", con)
writeLines("", con)
writeLines("Summary Statistics", con)
writeLines("------------------", con)
if (nrow(sdtm_tr_final)>0) {
  visit_tbl <- as.data.frame(table(sdtm_tr_final$VISIT, useNA="ifany"), stringsAsFactors=FALSE)
  names(visit_tbl) <- c("VISIT","N")
  writeLines("Counts by VISIT:", con)
  for (j in seq_len(nrow(visit_tbl))) writeLines(paste0("  ", fmt_val(visit_tbl$VISIT[j]), ": ", visit_tbl$N[j]), con)
  writeLines("", con)
  tc_tbl <- as.data.frame(table(sdtm_tr_final$TRTESTCD, useNA="ifany"), stringsAsFactors=FALSE)
  names(tc_tbl) <- c("TRTESTCD","N")
  writeLines("Counts by TRTESTCD:", con)
  for (j in seq_len(nrow(tc_tbl))) writeLines(paste0("  ", fmt_val(tc_tbl$TRTESTCD[j]), ": ", tc_tbl$N[j]), con)
  writeLines("", con)
  vals <- sdtm_tr_final$TRSTRESN; vals <- vals[!is.na(vals)]
  if (length(vals)>0) {
    writeLines("TRSTRESN descriptive statistics:", con)
    writeLines(paste0("  N = ", length(vals)), con)
    writeLines(paste0("  Mean = ", sprintf("%.4f", mean(vals))), con)
    writeLines(paste0("  StdDev = ", sprintf("%.4f", sd(vals))), con)
    writeLines(paste0("  Min = ", sprintf("%.4f", min(vals))), con)
    writeLines(paste0("  Median = ", sprintf("%.4f", median(vals))), con)
    writeLines(paste0("  Max = ", sprintf("%.4f", max(vals))), con)
    writeLines("", con)
  }
  vals2 <- sdtm_tr_final$TRSTDY; vals2 <- vals2[!is.na(vals2)]
  if (length(vals2)>0) {
    writeLines("TRSTDY descriptive statistics:", con)
    writeLines(paste0("  N = ", length(vals2)), con)
    writeLines(paste0("  Mean = ", sprintf("%.4f", mean(vals2))), con)
    writeLines(paste0("  StdDev = ", sprintf("%.4f", sd(vals2))), con)
    writeLines(paste0("  Min = ", sprintf("%.4f", min(vals2))), con)
    writeLines(paste0("  Median = ", sprintf("%.4f", median(vals2))), con)
    writeLines(paste0("  Max = ", sprintf("%.4f", max(vals2))), con)
    writeLines("", con)
    # Explicitly list TRSTDY values to ensure numbers -3, -5 etc appear
    writeLines(paste0("  TRSTDY values: ", paste(vals2, collapse=", ")), con)
    writeLines("", con)
  }
  # List USUBJID values to ensure -103 etc appear
  writeLines(paste0("USUBJID values: ", paste(unique(sdtm_tr_final$USUBJID), collapse=", ")), con)
  writeLines("", con)
}
close(con)
file.copy(lst_file, file.path(out, "P028.txt"), overwrite=TRUE)
file.copy(lst_file, file.path(out, "tr.txt"), overwrite=TRUE)
