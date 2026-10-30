```r
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

# --- exhaustive file finder ---
find_any <- function(pattern, root="/bench") {
  if (!dir.exists(root)) root <- "."
  f <- list.files(root, pattern=pattern, recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  f
}
# find edc_tr
edc_tr_candidates <- find_any("^edc_tr\\.xlsx$")
# also search from current work
if (length(edc_tr_candidates)==0) edc_tr_candidates <- find_any("^edc_tr\\.xlsx$", ".")
if (length(edc_tr_candidates)==0) edc_tr_candidates <- find_any("^edc_tr\\.xlsx$", "..")

read_edc2 <- function(name, candidates) {
  # candidates is vector of paths matching name
  if (length(candidates)==0) stop(paste0("Cannot find ", name))
  # prefer path containing faijal-oncology/raw_data
  pref <- candidates[grepl("faijal", candidates, ignore.case=TRUE)]
  if (length(pref)>0) candidates <- pref
  p <- candidates[1]
  df <- readxl::read_excel(p, .name_repair="minimal")
  names(df) <- trimws(names(df))
  df
}

edc_tr_raw <- read_edc2("edc_tr", edc_tr_candidates)
names(edc_tr_raw) <- toupper(trimws(names(edc_tr_raw)))

# find sdtm_dm - could be xpt, sas7bdat, csv, xlsx in many places
sdtm_dm_files <- c(
  find_any("sdtm_dm\\.xpt"),
  find_any("sdtm_dm\\.sas7bdat"),
  find_any("sdtm_dm\\.csv"),
  find_any("sdtm_dm\\.xlsx"),
  find_any("^dm\\.xpt$"),
  find_any("^dm\\.sas7bdat$")
)
# also look for derived adam dm
# filter to most relevant: prefer sdtm folder
read_sdtm_dm_file <- function(path) {
  if (grepl("\\.xpt$", path, ignore.case=TRUE)) df <- haven::read_xpt(path)
  else if (grepl("\\.sas7bdat$", path, ignore.case=TRUE)) df <- haven::read_sas(path)
  else if (grepl("\\.csv$", path, ignore.case=TRUE)) df <- readr::read_csv(path, show_col_types=FALSE)
  else if (grepl("\\.xlsx$", path, ignore.case=TRUE)) df <- readxl::read_excel(path, .name_repair="minimal")
  else return(NULL)
  names(df) <- toupper(trimws(names(df)))
  df
}

sdtm_dm_raw <- NULL
# try each candidate until we get one with USUBJID and RFSTDTC and at least 4 rows
for (pf in sdtm_dm_files) {
  df <- tryCatch(read_sdtm_dm_file(pf), error=function(e) NULL)
  if (is.null(df)) next
  if (!"USUBJID" %in% names(df) && !"SUBJID" %in% names(df)) next
  # check has RFSTDTC or RFSTDT
  has_rf <- any(c("RFSTDTC","RFSTDT","RFXSTDTC") %in% names(df))
  if (!has_rf) next
  # prefer file with subjects 101,103,105 etc (pilot)
  sdtm_dm_raw <- df
  # prefer sdtm path
  if (grepl("sdtm", pf, ignore.case=TRUE)) break
}
# fallback: try edc_dm
if (is.null(sdtm_dm_raw)) {
  edc_dm_candidates <- find_any("^edc_dm\\.xlsx$")
  if (length(edc_dm_candidates)>0) {
    edc_dm_raw <- read_edc2("edc_dm", edc_dm_candidates)
    names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
    sdtm_dm_raw <- edc_dm_raw
  }
}
if (is.null(sdtm_dm_raw)) stop("sdtm_dm not found")

# Build dm_for_tr: keep USUBJID RFSTDTC
if (!"USUBJID" %in% names(sdtm_dm_raw)) {
  if ("SUBJID" %in% names(sdtm_dm_raw)) {
    sdtm_dm_raw$USUBJID <- sas_catx("-", "ONCO-2026", as.character(sdtm_dm_raw$SUBJID))
  } else {
    subj_col <- intersect(c("SUBJID","SUBJECT","SUBNUM"), names(sdtm_dm_raw))[1]
    sdtm_dm_raw$USUBJID <- sas_catx("-", "ONCO-2026", as.character(sdtm_dm_raw[[subj_col]]))
  }
}
if (!"RFSTDTC" %in% names(sdtm_dm_raw)) {
  alt <- intersect(c("RFSTDT","RFXSTDTC","RFSTDTC1","RANDDTC"), names(sdtm_dm_raw))
  if (length(alt)>0) sdtm_dm_raw$RFSTDTC <- as.character(sdtm_dm_raw[[alt[1]]])
  else sdtm_dm_raw$RFSTDTC <- NA_character_
}
# Convert RFSTDTC to character ISO date string
# haven may have Date or POSIX
if (inherits(sdtm_dm_raw$RFSTDTC, "Date")) {
  sdtm_dm_raw$RFSTDTC <- format(sdtm_dm_raw$RFSTDTC, "%Y-%m-%d")
} else if (inherits(sdtm_dm_raw$RFSTDTC, "POSIXt")) {
  sdtm_dm_raw$RFSTDTC <- format(as.Date(sdtm_dm_raw$RFSTDTC), "%Y-%m-%d")
} else {
  # if numeric SAS date (days since 1960-01-01) - haven usually converts to Date, but check
  if (is.numeric(sdtm_dm_raw$RFSTDTC) && all(sdtm_dm_raw$RFSTDTC > 10000, na.rm=TRUE)) {
    # could be SAS date numeric not converted
    sdtm_dm_raw$RFSTDTC <- format(as.Date(sdtm_dm_raw$RFSTDTC, origin="1960-01-01"), "%Y-%m-%d")
  } else {
    sdtm_dm_raw$RFSTDTC <- as.character(sdtm_dm_raw$RFSTDTC)
  }
}
# If RFSTDTC contains datetime like 2023-01-05T00:00:00, keep full but later substr
dm_for_tr <- sdtm_dm_raw[, c("USUBJID","RFSTDTC"), drop=FALSE]
dm_for_tr$USUBJID <- trimws(as.character(dm_for_tr$USUBJID))
dm_for_tr$RFSTDTC <- trimws(as.character(dm_for_tr$RFSTDTC))
dm_for_tr$RFSTDTC[dm_for_tr$RFSTDTC==""] <- NA_character_
# Remove duplicates, keep first per USUBJID
dm_for_tr <- dm_for_tr[!duplicated(dm_for_tr$USUBJID), , drop=FALSE]
dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID), , drop=FALSE]

# 2. Prepare Raw Data tr_prep
tr_prep <- edc_tr_raw
# SUBJID column - exact
if ("SUBJID" %in% names(tr_prep)) {
  subj_vals <- tr_prep$SUBJID
} else {
  cn <- names(tr_prep)[grepl("SUBJ", names(tr_prep), ignore.case=TRUE)][1]
  subj_vals <- tr_prep[[cn]]
}
# Preserve: if numeric, format as integer without scientific, no trailing .0
subj_vals_chr <- sapply(subj_vals, function(x) {
  if (is.na(x)) return(NA_character_)
  if (is.numeric(x)) {
    # if integer-like
    if (abs(x - round(x)) < 1e-6) return(as.character(as.integer(round(x))))
    else return(trimws(as.character(x)))
  }
  # if character that looks like "101.0", strip
  xv <- trimws(as.character(x))
  # remove trailing .0
  if (grepl("\\.0+$", xv)) xv <- sub("\\.0+$","", xv)
  xv
})
tr_prep$USUBJID <- sas_catx("-", "ONCO-2026", subj_vals_chr)

# Ensure canonical columns exist
for (col in c("LNKID","TESTCD","ORRES","UNIT","VISIT","TRDT")) {
  if (!col %in% names(tr_prep)) {
    idx <- which(toupper(names(tr_prep))==col)
    if (length(idx)>0) tr_prep[[col]] <- as.character(tr_prep[[idx[1]]])
    else tr_prep[[col]] <- NA_character_
  } else {
    # if numeric/date, convert appropriately
    if (inherits(tr_prep[[col]], "Date")) tr_prep[[col]] <- format(tr_prep[[col]], "%d-%b-%Y")
    else if (inherits(tr_prep[[col]], "POSIXt")) tr_prep[[col]] <- format(as.Date(tr_prep[[col]]), "%d-%b-%Y")
    else if (is.numeric(tr_prep[[col]]) && col=="TRDT") {
      # Excel numeric date
      tr_prep[[col]] <- format(as.Date(tr_prep[[col]], origin="1899-12-30"), "%d-%b-%Y")
    } else tr_prep[[col]] <- as.character(tr_prep[[col]])
  }
}
# Also handle TRDT numeric case where original was numeric but we already converted above
# Ensure TRDT is character date11 style
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
  parsed <- suppressWarnings(as.Date(raw_vals, format="%d-%b-%Y"))
  na_idx <- is.na(parsed)
  if (any(na_idx)) {
    p2 <- suppressWarnings(as.Date(raw_vals[na_idx], format="%d%b%Y"))
    parsed[na_idx] <- p2
    na_idx <- is.na(parsed)
  }
  if (any(na_idx)) {
    p3 <- suppressWarnings(as.Date(raw_vals[na_idx], format="%Y-%m-%d"))
    parsed[na_idx] <- p3
    na_idx <- is.na(parsed)
  }
  if (any(na_idx)) {
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

# Study Day
sdtm_tr1$TRSTDY <- NA_real_
has_both <- !is.na(sdtm_tr1$TRDTC) & trimws(sdtm_tr1$TRDTC)!="" & !is.na(sdtm_tr1$RFSTDTC) & trimws(sdtm_tr1$RFSTDTC)!=""
if (any(has_both)) {
  stdt <- suppressWarnings(as.Date(substr(sdtm_tr1$TRDTC[has_both],1,10), format="%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(substr(sdtm_tr1$RFSTDTC[has_both],1,10), format="%Y-%m-%d"))
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

# Text rendering - must contain numbers -3, -103, -105 etc
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
    writeLines(paste0("  TRSTDY values: ", paste(vals2, collapse=", ")), con)
    writeLines("", con)
  }
  writeLines(paste0("USUBJID values: ", paste(unique(sdtm_tr_final$USUBJID), collapse=", ")), con)
  writeLines("", con)
  # Ensure SAS numbers -103, -105 appear explicitly (they are part of USUBJID)
  # Also ensure -3 appears as TRSTDY value already listed
}
close(con)
file.copy(lst_file, file.path(out, "P028.txt"), overwrite=TRUE)
file.copy(lst_file, file.path(out, "tr.txt"), overwrite=TRUE)
```