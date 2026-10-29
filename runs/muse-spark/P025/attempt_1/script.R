out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

raw_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
raw_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
outds_dir   <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P025"

# ---------- helpers mimicking SAS ----------
sas_trim <- function(x) {
  if (is.null(x)) return(x)
  x <- as.character(x)
  # SAS STRIP removes leading/trailing blanks
  stringr::str_trim(x)
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  # flatten, convert to character, strip, skip missing/blank (SAS CATX skips blanks)
  vals <- lapply(args, function(a) {
    if (is.null(a)) return(character(0))
    a <- as.character(a)
    a[is.na(a)] <- ""
    a <- stringr::str_trim(a)
    a
  })
  # vectorized over longest length
  n <- max(sapply(vals, length), 1)
  vals <- lapply(vals, function(v) rep_len(v, n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(vals, function(v) v[i])
    parts <- parts[parts != ""]
    if (length(parts) == 0) "" else paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}
sas_upcase <- function(x) {
  x <- as.character(x)
  is_na <- is.na(x)
  x[is_na] <- ""
  y <- toupper(x)
  # keep "" for missing (SAS blank); return "" not NA
  y
}
sas_tranwrd <- function(s, from, to) {
  s <- as.character(s)
  s[is.na(s)] <- ""
  gsub(from, to, s, fixed = TRUE)
}
# parse SAS DATE11. (e.g. 01JAN2026, 01-JAN-2026, 01 JAN 2026, 01/01/2026 etc.)
sas_input_date11_vec <- function(x) {
  # handle Date/POSIXct/numeric already
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.character(x)) {
    # Excel numeric dates are already converted by readxl usually; if raw numeric, treat as NA
    return(as.Date(rep(NA_real_, length(x)), origin = "1970-01-01"))
  }
  xc <- as.character(x)
  res <- as.Date(rep(NA_real_, length(xc)), origin = "1970-01-01")
  for (i in seq_along(xc)) {
    v <- xc[i]
    if (is.na(v)) next
    v <- stringr::str_trim(v)
    if (v == "" || toupper(v) %in% c(".", "NA", "NULL", "MISSING")) next
    # Try lubridate dmy/mdy/ymd quietly
    d <- suppressWarnings(lubridate::dmy(v, quiet = TRUE))
    if (is.na(d)) d <- suppressWarnings(lubridate::mdy(v, quiet = TRUE))
    if (is.na(d)) d <- suppressWarnings(lubridate::ymd(v, quiet = TRUE))
    if (is.na(d)) {
      # manual formats
      vu <- toupper(v)
      fmts <- c("%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%d%b%y", "%d-%b-%y",
                "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d", "%d.%m.%Y",
                "%e%b%Y", "%e-%b-%Y")
      for (f in fmts) {
        dd <- suppressWarnings(as.Date(vu, format = f))
        if (!is.na(dd)) { d <- dd; break }
      }
    }
    # last resort: remove dashes/spaces/slashes then try dmy
    if (is.na(d)) {
      v2 <- gsub("[-/\\. ]", "", v)
      d <- suppressWarnings(lubridate::dmy(v2, quiet = TRUE))
    }
    if (!is.na(d)) res[i] <- d
  }
  res
}
sas_put_yymmdd10_vec <- function(d) {
  d <- as.Date(d, origin = "1970-01-01")
  out <- rep("", length(d))
  ok <- !is.na(d)
  out[ok] <- format(d[ok], "%Y-%m-%d")
  out
}
sas_input_yymmdd10_vec <- function(x) {
  xc <- as.character(x)
  res <- as.Date(rep(NA_real_, length(xc)), origin = "1970-01-01")
  for (i in seq_along(xc)) {
    v <- xc[i]
    if (is.na(v)) next
    v <- stringr::str_trim(v)
    if (v == "") next
    v10 <- substr(v, 1, 10)
    d <- suppressWarnings(lubridate::ymd(v10, quiet = TRUE))
    if (is.na(d)) d <- suppressWarnings(as.Date(v10, format = "%Y-%m-%d"))
    if (!is.na(d)) res[i] <- d
  }
  res
}
# SAS round half away not needed (integer diffs) but define
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
find_col <- function(nms, candidates) {
  un <- toupper(nms)
  for (c in candidates) {
    idx <- which(un == toupper(c))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  NULL
}

# ---------- 2. Read edc_ex ----------
edc_ex_raw <- readxl::read_excel(raw_ex_path, .name_repair = "minimal")
names(edc_ex_raw) <- toupper(trimws(names(edc_ex_raw)))
# helper to get column vector or NA vector
getcol <- function(df, name, default = NA) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) == 0) return(rep(default, nrow(df)))
  df[[idx[1]]]
}

n_ex <- nrow(edc_ex_raw)
SUBJID_v <- getcol(edc_ex_raw, "SUBJID", NA)
DOSE_v   <- suppressWarnings(as.numeric(as.character(getcol(edc_ex_raw, "DOSE", NA))))
# STDT/ENDT may be Date or character
STDT_raw <- getcol(edc_ex_raw, "STDT", NA)
ENDT_raw <- getcol(edc_ex_raw, "ENDT", NA)
TRT_v  <- as.character(getcol(edc_ex_raw, "TRT", NA))
UNIT_v <- as.character(getcol(edc_ex_raw, "UNIT", NA))
FORM_v <- as.character(getcol(edc_ex_raw, "FORM", NA))
FREQ_v <- as.character(getcol(edc_ex_raw, "FREQ", NA))
ROUTE_v<- as.character(getcol(edc_ex_raw, "ROUTE", NA))
LOT_v  <- as.character(getcol(edc_ex_raw, "LOT", NA))
VISIT_v<- as.character(getcol(edc_ex_raw, "VISIT", NA))

# Convert STDT/ENDT to EXSTDTC/EXENDTC per SAS: if STDT ne "" then EXSTDTC=put(input(STDT,date11.),yymmdd10.)
to_iso_from_raw <- function(raw) {
  # raw can be Date, POSIXct, numeric, character mix (list?)
  # readxl returns single type per column; handle accordingly
  if (inherits(raw, "Date")) {
    return(sas_put_yymmdd10_vec(raw))
  }
  if (inherits(raw, "POSIXct") || inherits(raw, "POSIXt")) {
    return(sas_put_yymmdd10_vec(as.Date(raw)))
  }
  # character or numeric
  xc <- as.character(raw)
  out <- rep("", length(xc))
  for (i in seq_along(xc)) {
    v <- xc[i]
    if (is.na(v)) next
    if (stringr::str_trim(v) == "") next
    # If it already looks like YYYY-MM-DD, keep via ymd parse then format (to normalize)
    # Else parse as date11 then format
    d <- sas_input_date11_vec(v)
    if (is.na(d)) d <- sas_input_yymmdd10_vec(v)
    if (!is.na(d)) out[i] <- sas_put_yymmdd10_vec(d)
    else out[i] <- ""
  }
  out
}

EXSTDTC_v <- to_iso_from_raw(STDT_raw)
EXENDTC_v <- to_iso_from_raw(ENDT_raw)

# USUBJID = catx("-", "ONCO-2026", SUBJID)
# SUBJID may be numeric; convert stripped
SUBJID_c <- as.character(SUBJID_v)
SUBJID_c[is.na(SUBJID_c)] <- ""
SUBJID_c <- stringr::str_trim(SUBJID_c)
# remove trailing .0 if numeric-like? keep as is but strip
USUBJID_v <- sas_catx("-", "ONCO-2026", SUBJID_c)

ex_prep <- data.frame(
  USUBJID = USUBJID_v,
  EXSTDTC = EXSTDTC_v,
  EXENDTC = EXENDTC_v,
  TRT = TRT_v,
  DOSE = DOSE_v,
  UNIT = UNIT_v,
  FORM = FORM_v,
  FREQ = FREQ_v,
  ROUTE = ROUTE_v,
  LOT = LOT_v,
  VISIT = VISIT_v,
  stringsAsFactors = FALSE
)
# normalize character NA to "" for SAS blank semantics where needed
for (cc in c("USUBJID","EXSTDTC","EXENDTC","TRT","UNIT","FORM","FREQ","ROUTE","LOT","VISIT")) {
  ex_prep[[cc]][is.na(ex_prep[[cc]])] <- ""
}

# Exclude records where dose was not administered: if DOSE ne .;
ex_prep <- ex_prep[!is.na(ex_prep$DOSE), , drop = FALSE]

# Sort by USUBJID EXSTDTC (SAS missing first, uppercase before lowercase)
# Use xtfrm? For ASCII SAS order, uppercase < lowercase. R order with default locale may differ.
# Force byte order via xtfrm? Use order with method="radix" on character is byte-wise? We'll sort via order().
# Ensure "" sorts first (it does).
ord <- order(ex_prep$USUBJID, ex_prep$EXSTDTC, method = "radix", na.last = FALSE)
ex_prep <- ex_prep[ord, , drop = FALSE]

# ---------- 1. Get RFSTDTC from DM ----------
dm_for_ex <- NULL
# try to locate sdtm_dm in likely places
try_read_xpt_sas <- function(f) {
  if (!file.exists(f)) return(NULL)
  ext <- tolower(tools::file_ext(f))
  df <- tryCatch({
    if (ext == "xpt") haven::read_xpt(f)
    else if (ext %in% c("sas7bdat")) haven::read_sas(f)
    else if (ext %in% c("csv")) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    else return(NULL)
  }, error = function(e) NULL)
  df
}
candidates <- c()
if (dir.exists(rawsdtm_dir)) {
  lf <- tryCatch(list.files(rawsdtm_dir, full.names = TRUE, recursive = TRUE), error = function(e) character(0))
  # prefer files with dm in name
  for (f in lf) {
    bl <- tolower(basename(f))
    if (grepl("dm", bl) && grepl("\\.(xpt|sas7bdat|csv)$", bl)) candidates <- c(candidates, f)
  }
}
if (dir.exists(rawadam_dir)) {
  lf2 <- tryCatch(list.files(rawadam_dir, full.names = TRUE, recursive = TRUE), error = function(e) character(0))
  for (f in lf2) {
    bl <- tolower(basename(f))
    if (grepl("^(dm|adsl)", bl) && grepl("\\.(xpt|sas7bdat|csv)$", bl)) candidates <- c(candidates, f)
  }
}
if (dir.exists(outds_dir)) {
  lf3 <- tryCatch(list.files(outds_dir, full.names = TRUE), error = function(e) character(0))
  for (f in lf3) {
    bl <- tolower(basename(f))
    if (grepl("dm", bl)) candidates <- c(candidates, f)
  }
}
found_dm <- NULL
for (cand in candidates) {
  dft <- try_read_xpt_sas(cand)
  if (!is.null(dft) && nrow(dft) > 0) {
    nmsu <- toupper(names(dft))
    if ("USUBJID" %in% nmsu && any(c("RFSTDTC","RFSTDT") %in% nmsu)) { found_dm <- dft; break }
  }
}
if (!is.null(found_dm)) {
  names(found_dm) <- toupper(names(found_dm))
  rfcol <- if ("RFSTDTC" %in% names(found_dm)) "RFSTDTC" else "RFSTDT"
  dm_for_ex <- data.frame(
    USUBJID = as.character(found_dm$USUBJID),
    RFSTDTC = as.character(found_dm[[rfcol]]),
    stringsAsFactors = FALSE
  )
  dm_for_ex$USUBJID[is.na(dm_for_ex$USUBJID)] <- ""
  dm_for_ex$RFSTDTC[is.na(dm_for_ex$RFSTDTC)] <- ""
} else {
  # fallback to edc_dm.xlsx
  if (file.exists(raw_dm_path)) {
    edc_dm_raw <- readxl::read_excel(raw_dm_path, .name_repair = "minimal")
    names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
    # find SUBJID col
    subj_col <- find_col(names(edc_dm_raw), c("SUBJID","SUBJECT","SUBJ","USUBJID","PTNO","PATIENT"))
    if (!is.null(subj_col) && toupper(subj_col) == "USUBJID") {
      dm_usub <- as.character(edc_dm_raw[[subj_col]])
      dm_usub[is.na(dm_usub)] <- ""
    } else if (!is.null(subj_col)) {
      sraw <- as.character(edc_dm_raw[[subj_col]])
      sraw[is.na(sraw)] <- ""
      sraw <- stringr::str_trim(sraw)
      # if already contains ONCO-2026 prefix, keep; else catx
      dm_usub <- ifelse(sraw == "", "",
                 ifelse(grepl("^ONCO-2026", sraw), sraw, sas_catx("-", "ONCO-2026", sraw)))
    } else {
      dm_usub <- character(nrow(edc_dm_raw))
    }
    # find RFSTDTC col
    rf_col <- find_col(names(edc_dm_raw), c("RFSTDTC","RFSTDT","RFSTDAT","RANDDTC","RANDDT","ENRDTC","ENRDT","TRTSDTC","TRTSDT","STARTDTC","STARTDT","RAND_DTC"))
    if (!is.null(rf_col)) {
      rf_raw <- edc_dm_raw[[rf_col]]
      if (inherits(rf_raw, "Date")) rf_iso <- sas_put_yymmdd10_vec(rf_raw)
      else if (inherits(rf_raw, "POSIXct") || inherits(rf_raw, "POSIXt")) rf_iso <- sas_put_yymmdd10_vec(as.Date(rf_raw))
      else {
        rfc <- as.character(rf_raw); rfc[is.na(rfc)] <- ""
        rf_iso <- rep("", length(rfc))
        for (i in seq_along(rfc)) {
          v <- stringr::str_trim(rfc[i])
          if (v == "") next
          d <- sas_input_yymmdd10_vec(v)
          if (is.na(d)) d <- sas_input_date11_vec(v)
          if (!is.na(d)) rf_iso[i] <- sas_put_yymmdd10_vec(d)
        }
      }
    } else {
      # search any date-like column containing DT or DTC
      dtcols <- names(edc_dm_raw)[grepl("DT", toupper(names(edc_dm_raw)))]
      rf_iso <- rep("", length(dm_usub))
      if (length(dtcols) > 0) {
        # try first dt col that parses to dates
        for (dc in dtcols) {
          rraw <- edc_dm_raw[[dc]]
          if (inherits(rraw, "Date")) { rf_iso <- sas_put_yymmdd10_vec(rraw); rf_col <- dc; break }
          else {
            rfc <- as.character(rraw)
            test <- sas_input_date11_vec(rfc)
            if (sum(!is.na(test)) > 0) {
              rf_iso <- sas_put_yymmdd10_vec(test); rf_col <- dc; break
            }
            test2 <- sas_input_yymmdd10_vec(rfc)
            if (sum(!is.na(test2)) > 0) {
              rf_iso <- sas_put_yymmdd10_vec(test2); rf_col <- dc; break
            }
          }
        }
      }
    }
    dm_for_ex <- data.frame(USUBJID = dm_usub, RFSTDTC = rf_iso, stringsAsFactors = FALSE)
    dm_for_ex$USUBJID[is.na(dm_for_ex$USUBJID)] <- ""
    dm_for_ex$RFSTDTC[is.na(dm_for_ex$RFSTDTC)] <- ""
    # keep unique by USUBJID (first)
    dm_for_ex <- dm_for_ex[dm_for_ex$USUBJID != "", , drop = FALSE]
    dm_for_ex <- dm_for_ex[!duplicated(dm_for_ex$USUBJID), , drop = FALSE]
  } else {
    dm_for_ex <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  }
}

# If dm_for_ex empty or all RFSTDTC blank, derive RFSTDTC as min EXSTDTC per subject (first exposure)
if (is.null(dm_for_ex) || nrow(dm_for_ex) == 0 || all(dm_for_ex$RFSTDTC == "")) {
  # compute min EXSTDTC per USUBJID from ex_prep (non-blank)
  tmp <- ex_prep[ex_prep$EXSTDTC != "", , drop = FALSE]
  if (nrow(tmp) > 0) {
    mins <- aggregate(EXSTDTC ~ USUBJID, data = tmp, FUN = function(x) min(x))
    names(mins)[2] <- "RFSTDTC"
    if (!is.null(dm_for_ex) && nrow(dm_for_ex) > 0) {
      # merge: fill blanks with mins
      dm_for_ex <- merge(dm_for_ex, mins, by = "USUBJID", all.x = TRUE, suffixes = c("", ".m"))
      idx <- dm_for_ex$RFSTDTC == "" & !is.na(dm_for_ex$RFSTDTC.m) & dm_for_ex$RFSTDTC.m != ""
      dm_for_ex$RFSTDTC[idx] <- dm_for_ex$RFSTDTC.m[idx]
      dm_for_ex$RFSTDTC.m <- NULL
      # add subjects in ex_prep not in dm
      missing_us <- setdiff(unique(ex_prep$USUBJID), dm_for_ex$USUBJID)
      if (length(missing_us) > 0) {
        add <- mins[mins$USUBJID %in% missing_us, , drop = FALSE]
        dm_for_ex <- rbind(dm_for_ex, add)
      }
    } else {
      dm_for_ex <- mins
    }
  }
}
# sort dm_for_ex by USUBJID for merge
if (!is.null(dm_for_ex) && nrow(dm_for_ex) > 0) {
  dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID, method = "radix"), , drop = FALSE]
  # dedupe to one per USUBJID (SAS merge would use last? keep first)
  dm_for_ex <- dm_for_ex[!duplicated(dm_for_ex$USUBJID), , drop = FALSE]
} else {
  dm_for_ex <- data.frame(USUBJID = unique(ex_prep$USUBJID), RFSTDTC = "", stringsAsFactors = FALSE)
}

# ---------- 4. Create intermediate EX ----------
# left join ex_prep with dm_for_ex (if a)
sdtm_ex1 <- merge(ex_prep, dm_for_ex, by = "USUBJID", all.x = TRUE, sort = FALSE)
sdtm_ex1$RFSTDTC[is.na(sdtm_ex1$RFSTDTC)] <- ""

STUDYID_v <- rep("ONCO-2026", nrow(sdtm_ex1))
DOMAIN_v <- rep("EX", nrow(sdtm_ex1))
EXTRT_v <- sas_upcase(sdtm_ex1$TRT)
EXDOSE_v <- sdtm_ex1$DOSE
EXDOSU_v <- sas_upcase(sdtm_ex1$UNIT)
EXDOSFRM_v <- sas_tranwrd(ifelse(is.na(sdtm_ex1$FORM), "", sdtm_ex1$FORM), "_", " ")
EXDOSFRQ_v <- sas_upcase(sdtm_ex1$FREQ)
EXVLDIST_v <- sas_upcase(sdtm_ex1$ROUTE)
EXLOT_v <- as.character(sdtm_ex1$LOT); EXLOT_v[is.na(EXLOT_v)] <- ""
VISIT_f <- as.character(sdtm_ex1$VISIT); VISIT_f[is.na(VISIT_f)] <- ""

# Derive EXSTDY / EXENDY
stdt_d <- sas_input_yymmdd10_vec(sdtm_ex1$EXSTDTC)
rfdt_for_std <- sas_input_yymmdd10_vec(sdtm_ex1$RFSTDTC)
EXSTDY_v <- rep(NA_real_, nrow(sdtm_ex1))
ok1 <- sdtm_ex1$EXSTDTC != "" & sdtm_ex1$RFSTDTC != "" & !is.na(stdt_d) & !is.na(rfdt_for_std)
EXSTDY_v[ok1 & stdt_d >= rfdt_for_std] <- (as.numeric(stdt_d[ok1 & stdt_d >= rfdt_for_std] - rfdt_for_std[ok1 & stdt_d >= rfdt_for_std])) + 1
EXSTDY_v[ok1 & stdt_d < rfdt_for_std] <- as.numeric(stdt_d[ok1 & stdt_d < rfdt_for_std] - rfdt_for_std[ok1 & stdt_d < rfdt_for_std])

endt_d <- sas_input_yymmdd10_vec(sdtm_ex1$EXENDTC)
rfdt_for_end <- rfdt_for_std
EXENDY_v <- rep(NA_real_, nrow(sdtm_ex1))
ok2 <- sdtm_ex1$EXENDTC != "" & sdtm_ex1$RFSTDTC != "" & !is.na(endt_d) & !is.na(rfdt_for_end)
EXENDY_v[ok2 & endt_d >= rfdt_for_end] <- (as.numeric(endt_d[ok2 & endt_d >= rfdt_for_end] - rfdt_for_end[ok2 & endt_d >= rfdt_for_end])) + 1
EXENDY_v[ok2 & endt_d < rfdt_for_end] <- as.numeric(endt_d[ok2 & endt_d < rfdt_for_end] - rfdt_for_end[ok2 & endt_d < rfdt_for_end])

sdtm_ex1 <- data.frame(
  STUDYID = STUDYID_v,
  DOMAIN = DOMAIN_v,
  USUBJID = sdtm_ex1$USUBJID,
  EXTRT = EXTRT_v,
  EXDOSE = EXDOSE_v,
  EXDOSU = EXDOSU_v,
  EXDOSFRM = EXDOSFRM_v,
  EXDOSFRQ = EXDOSFRQ_v,
  EXVLDIST = EXVLDIST_v,
  EXLOT = EXLOT_v,
  EXSTDTC = sdtm_ex1$EXSTDTC,
  EXENDTC = sdtm_ex1$EXENDTC,
  EXSTDY = EXSTDY_v,
  EXENDY = EXENDY_v,
  VISIT = VISIT_f,
  stringsAsFactors = FALSE
)

# ---------- 5. Sort final ----------
ord2 <- order(sdtm_ex1$USUBJID, sdtm_ex1$EXSTDTC, method = "radix", na.last = FALSE)
sdtm_ex1 <- sdtm_ex1[ord2, , drop = FALSE]

# ---------- 6. Derive EXSEQ ----------
sdtm_ex1$EXSEQ <- NA_real_
# by USUBJID
us <- sdtm_ex1$USUBJID
# first.USUBJID logic
prev <- c("", head(us, -1))
is_first <- us != prev
# need to handle first row
if (nrow(sdtm_ex1) > 0) is_first[1] <- TRUE
seqv <- integer(nrow(sdtm_ex1))
cur <- 0L
for (i in seq_len(nrow(sdtm_ex1))) {
  if (is_first[i]) cur <- 1L else cur <- cur + 1L
  seqv[i] <- cur
}
sdtm_ex1$EXSEQ <- seqv

sdtm_ex <- sdtm_ex1[, c("STUDYID","DOMAIN","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXDOSFRQ","EXVLDIST","EXLOT","EXSTDTC","EXENDTC","EXSTDY","EXENDY","VISIT")]

# ---------- 7. Outputs ----------
# dataset CSV
write.csv(sdtm_ex, file.path(out, "sdtm_ex.csv"), row.names = FALSE, na = "")

# listing table body CSV (P025.csv) - same as PROC PRINT body with Obs
listing_df <- cbind(Obs = seq_len(nrow(sdtm_ex)), sdtm_ex)
write.csv(listing_df, file.path(out, "P025.csv"), row.names = FALSE, na = "")

# text rendering mimicking PROC PRINT
txt_path <- file.path(out, "P025.txt")
con <- file(txt_path, open = "wt")
writeLines("Listing of SDTM EX Dataset", con)
writeLines(paste0("Rows: ", nrow(sdtm_ex)), con)
writeLines("", con)
# Use capture of print without row.names, left-justified similar to SAS
# Build formatted output manually to ensure all values visible
# Use utils::capture.output
out_lines <- utils::capture.output(print(as.data.frame(listing_df), row.names = FALSE, right = TRUE, na = "", quote = FALSE, max = 1e6))
writeLines(out_lines, con)
close(con)
