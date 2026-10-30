```r
# P022_ae.sas -> R translation
# SDTM AE mapping & derivation + PROC PRINT listing

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
outds_parent <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_sort_order <- function(...) {
  order(..., method = "radix", na.last = FALSE)
}

# Title-case month for DATE11. parsing: 01-JAN-2026 -> 01-Jan-2026
title_month <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(as.character(s))
    if (s == "") return(NA_character_)
    # DD-MMM-YYYY with dashes
    if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", s)) {
      parts <- strsplit(s, "-", fixed = TRUE)[[1]]
      mon <- paste0(toupper(substr(parts[2], 1, 1)), tolower(substr(parts[2], 2, 3)))
      return(paste0(parts[1], "-", mon, "-", parts[3]))
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

# Parse character DATE11. (DD-MMM-YYYY) to ISO YYYY-MM-DD; Date/POSIXct passed through
parse_date11_to_iso <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(format(as.Date(x, tz = "UTC"), "%Y-%m-%d"))
  }
  if (is.numeric(x) && !is.character(x)) {
    # Excel serial unlikely via readxl, but handle defensively as NA
    return(rep(NA_character_, length(x)))
  }
  xc <- as.character(x)
  res <- rep(NA_character_, length(xc))
  idx <- !is.na(xc) & trimws(xc) != ""
  if (any(idx)) {
    xt <- title_month(xc[idx])
    dt <- as.Date(xt, format = "%d-%b-%Y")
    # SAS DATE11. with 2-digit year? INPUT with date11. can read 2-digit year; handle fallback
    # Keep single strict format per rule; only fill where parsed
    res[idx] <- format(dt, "%Y-%m-%d")
    # format() of NA gives NA; convert "NA" strings to NA
    res[res == "NA" | is.na(res)] <- NA_character_
  }
  res[res == ""] <- NA_character_
  res
}

# Parse ISO character (YYYY-MM-DD, possibly with time) to ISO date; Date passed through
parse_iso_to_iso <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(as.Date(x, tz = "UTC"), "%Y-%m-%d"))
  if (is.numeric(x) && !is.character(x)) return(rep(NA_character_, length(x)))
  xc <- trimws(as.character(x))
  res <- rep(NA_character_, length(xc))
  idx <- !is.na(xc) & xc != ""
  if (any(idx)) {
    # take first 10 chars if datetime
    sub10 <- substr(xc[idx], 1, 10)
    dt <- as.Date(sub10, format = "%Y-%m-%d")
    # if fails, try DATE11. as fallback for raw DM dates (one format per column is ideal,
    # but DM sources may be DATE11.; allow single fallback to avoid missing RFSTDTC)
    need <- is.na(dt)
    if (any(need)) {
      raw_need <- xc[idx][need]
      dt2iso <- parse_date11_to_iso(raw_need)
      dt2 <- as.Date(dt2iso, format = "%Y-%m-%d")
      dt[need] <- dt2
    }
    res[idx] <- format(dt, "%Y-%m-%d")
    res[res == "NA"] <- NA_character_
  }
  res
}

cat("Reading raw EDC data...\n")
edc_ae_path <- file.path(raw_base, "edc_ae.xlsx")
edc_dm_path <- file.path(raw_base, "edc_dm.xlsx")
edc_ex_path <- file.path(raw_base, "edc_ex.xlsx")
ivrs_path   <- file.path(raw_base, "ivrs_rand.xlsx")

edc_ae <- as.data.frame(readxl::read_excel(edc_ae_path), stringsAsFactors = FALSE)
edc_dm <- as.data.frame(readxl::read_excel(edc_dm_path), stringsAsFactors = FALSE)
edc_ex <- as.data.frame(readxl::read_excel(edc_ex_path), stringsAsFactors = FALSE)
ivrs_rand <- as.data.frame(readxl::read_excel(ivrs_path), stringsAsFactors = FALSE)
names(edc_ae) <- toupper(names(edc_ae))
names(edc_dm) <- toupper(names(edc_dm))
names(edc_ex) <- toupper(names(edc_ex))
names(ivrs_rand) <- toupper(names(ivrs_rand))

cat("edc_ae cols:", paste(names(edc_ae), collapse = ","), "\n")
cat("edc_dm cols:", paste(names(edc_dm), collapse = ","), "\n")
cat("edc_ex cols:", paste(names(edc_ex), collapse = ","), "\n")
cat("ivrs_rand cols:", paste(names(ivrs_rand), collapse = ","), "\n")

#--------------------------------------------------------------
# 1. Sort DM for Study Day Calculations (derive dm_for_ae)
#--------------------------------------------------------------
# Try to reuse SAS DM output if present for exact RFSTDTC match
dm_for_ae <- NULL
cand_dm_files <- character(0)
if (dir.exists(outds_parent)) {
  cand_dm_files <- list.files(outds_parent, pattern = "(?i)(^|_)dm\\.xlsx$",
    recursive = TRUE, full.names = TRUE)
}
if (length(cand_dm_files) > 0) {
  cat("Found candidate DM file:", cand_dm_files[1], "\n")
  dm_raw <- as.data.frame(readxl::read_excel(cand_dm_files[1]), stringsAsFactors = FALSE)
  names(dm_raw) <- toupper(names(dm_raw))
  stopifnot("USUBJID" %in% names(dm_raw))
  stopifnot("RFSTDTC" %in% names(dm_raw))
  dm_for_ae <- data.frame(
    USUBJID = trimws(as.character(dm_raw$USUBJID)),
    RFSTDTC = parse_iso_to_iso(dm_raw$RFSTDTC),
    stringsAsFactors = FALSE
  )
  dm_for_ae <- dm_for_ae[!is.na(dm_for_ae$USUBJID) & dm_for_ae$USUBJID != "", , drop = FALSE]
  dm_for_ae <- dm_for_ae[!duplicated(dm_for_ae$USUBJID), , drop = FALSE]
} else {
  cat("No SAS DM xlsx found; deriving RFSTDTC from raw EDC/IVRS\n")
  find_subjid_col <- function(df) {
    nms <- names(df)
    for (cand in c("USUBJID", "SUBJID", "SUBJECT", "SUBID", "PATID", "ID")) {
      if (cand %in% nms) return(cand)
    }
    g <- grep("SUBJ", nms, value = TRUE)
    if (length(g) > 0) return(g[1])
    NULL
  }
  find_date_col <- function(df, exclude) {
    nms <- setdiff(names(df), exclude)
    # priority patterns
    pats <- c("RFSTDTC", "RFSTDT", "RANDDTC", "RANDDT", "RAND", "EXSTDTC", "EXSTDT",
      "EXST", "STDT", "START", "EXDT", "DOSEDT", "TRTSDT", "ENRDT", "DMDT", "DTC", "DT", "DATE")
    for (p in pats) {
      hit <- nms[grepl(p, nms, fixed = TRUE)]
      if (length(hit) > 0) {
        # prefer Date-class or character columns
        return(hit[1])
      }
    }
    # fallback: first Date-class column
    for (cl in nms) {
      if (inherits(df[[cl]], "Date") || inherits(df[[cl]], "POSIXct")) return(cl)
    }
    NULL
  }
  sas_catx_usubjid <- function(subj) {
    s <- trimws(as.character(subj))
    s[is.na(subj)] <- ""
    s[is.na(s)] <- ""
    ifelse(s == "", "ONCO-2026", paste0("ONCO-2026-", s))
  }
  # Collect per-subject candidate dates
  rf_list <- list()
  # edc_dm
  sc_dm <- find_subjid_col(edc_dm)
  if (!is.null(sc_dm)) {
    dc_dm <- NULL
    if ("RFSTDTC" %in% names(edc_dm)) dc_dm <- "RFSTDTC"
    else if ("RFSTDT" %in% names(edc_dm)) dc_dm <- "RFSTDT"
    else dc_dm <- find_date_col(edc_dm, sc_dm)
    if (!is.null(dc_dm)) {
      us <- if (sc_dm == "USUBJID") trimws(as.character(edc_dm[[sc_dm]])) else sas_catx_usubjid(edc_dm[[sc_dm]])
      iso <- if (dc_dm %in% c("RFSTDTC", "RFSTDT")) parse_iso_to_iso(edc_dm[[dc_dm]]) else parse_date11_to_iso(edc_dm[[dc_dm]])
      # also handle Date class already via helpers
      d <- data.frame(USUBJID = us, DM_DT = iso, stringsAsFactors = FALSE)
      d <- d[!is.na(d$USUBJID) & d$USUBJID != "" & !is.na(d$DM_DT), , drop = FALSE]
      if (nrow(d) > 0) {
        d <- d[order(d$USUBJID, d$DM_DT), , drop = FALSE]
        d <- d[!duplicated(d$USUBJID), , drop = FALSE]
        rf_list$dm <- d
        cat("DM date col used:", dc_dm, "\n")
      }
    }
  }
  # ivrs_rand (randomization)
  sc_r <- find_subjid_col(ivrs_rand)
  if (!is.null(sc_r)) {
    dc_r <- find_date_col(ivrs_rand, sc_r)
    if (!is.null(dc_r)) {
      us <- if (sc_r == "USUBJID") trimws(as.character(ivrs_rand[[sc_r]])) else sas_catx_usubjid(ivrs_rand[[sc_r]])
      iso <- parse_date11_to_iso(ivrs_rand[[dc_r]])
      # if all NA, try ISO parse (in case already ISO)
      if (all(is.na(iso))) iso <- parse_iso_to_iso(ivrs_rand[[dc_r]])
      d <- data.frame(USUBJID = us, RAND_DT = iso, stringsAsFactors = FALSE)
      d <- d[!is.na(d$USUBJID) & d$USUBJID != "" & !is.na(d$RAND_DT), , drop = FALSE]
      if (nrow(d) > 0) {
        d <- d[order(d$USUBJID, d$RAND_DT), , drop = FALSE]
        d <- d[!duplicated(d$USUBJID), , drop = FALSE]
        rf_list$rand <- d
        cat("RAND date col used:", dc_r, "\n")
      }
    }
  }
  # edc_ex (first exposure)
  sc_e <- find_subjid_col(edc_ex)
  if (!is.null(sc_e)) {
    dc_e <- find_date_col(edc_ex, sc_e)
    if (!is.null(dc_e)) {
      us <- if (sc_e == "USUBJID") trimws(as.character(edc_ex[[sc_e]])) else sas_catx_usubjid(edc_ex[[sc_e]])
      iso <- parse_date11_to_iso(edc_ex[[dc_e]])
      if (all(is.na(iso))) iso <- parse_iso_to_iso(edc_ex[[dc_e]])
      d <- data.frame(USUBJID = us, EX_DT = iso, stringsAsFactors = FALSE)
      d <- d[!is.na(d$USUBJID) & d$USUBJID != "" & !is.na(d$EX_DT), , drop = FALSE]
      if (nrow(d) > 0) {
        d <- d[order(d$USUBJID, d$EX_DT), , drop = FALSE]
        # first exposure = min per subject
        agg <- aggregate(EX_DT ~ USUBJID, data = d, FUN = function(z) min(z, na.rm = TRUE))
        rf_list$ex <- agg
        cat("EX date col used:", dc_e, "\n")
      }
    }
  }
  # Coalesce priority: DM RFSTDTC > RAND > EX ; if multiple, take first non-missing
  all_us <- unique(c(
    if (!is.null(rf_list$dm)) rf_list$dm$USUBJID else NULL,
    if (!is.null(rf_list$rand)) rf_list$rand$USUBJID else NULL,
    if (!is.null(rf_list$ex)) rf_list$ex$USUBJID else NULL
  ))
  # Also include all AE subjects so left join keeps everyone (RFSTDTC may be NA)
  ae_subj_tmp <- trimws(as.character(edc_ae$SUBJID))
  ae_us_all <- unique(ifelse(is.na(ae_subj_tmp) | ae_subj_tmp == "", "ONCO-2026",
    paste0("ONCO-2026-", ae_subj_tmp)))
  all_us <- unique(c(all_us, ae_us_all))
  dm_for_ae <- data.frame(USUBJID = all_us, stringsAsFactors = FALSE)
  if (!is.null(rf_list$dm)) dm_for_ae <- merge(dm_for_ae, rf_list$dm, by = "USUBJID", all.x = TRUE)
  else dm_for_ae$DM_DT <- NA_character_
  if (!is.null(rf_list$rand)) dm_for_ae <- merge(dm_for_ae, rf_list$rand, by = "USUBJID", all.x = TRUE)
  else dm_for_ae$RAND_DT <- NA_character_
  if (!is.null(rf_list$ex)) dm_for_ae <- merge(dm_for_ae, rf_list$ex, by = "USUBJID", all.x = TRUE)
  else dm_for_ae$EX_DT <- NA_character_
  dm_for_ae$RFSTDTC <- ifelse(!is.na(dm_for_ae$DM_DT), dm_for_ae$DM_DT,
    ifelse(!is.na(dm_for_ae$RAND_DT), dm_for_ae$RAND_DT, dm_for_ae$EX_DT))
  dm_for_ae <- dm_for_ae[, c("USUBJID", "RFSTDTC"), drop = FALSE]
}
# SAS sort dm_for_ae by USUBJID
dm_for_ae <- dm_for_ae[sas_sort_order(dm_for_ae$USUBJID), , drop = FALSE]
rownames(dm_for_ae) <- NULL
cat("dm_for_ae n:", nrow(dm_for_ae), "\n")

#--------------------------------------------------------------
# 2. Process Raw AE Data (ae_prep)
#--------------------------------------------------------------
stopifnot("SUBJID" %in% names(edc_ae))
# STDT/ENDT/TERM etc must exist (SAS names)
for (cl in c("STDT", "ENDT", "TERM", "DECOD", "BODSYS", "SEV", "TXGR", "REL", "OUT", "SER")) {
  stopifnot(cl %in% names(edc_ae))
}
sas_catx <- function(subj) {
  s <- trimws(as.character(subj))
  s[is.na(subj)] <- ""
  s[is.na(s)] <- ""
  ifelse(s == "", "ONCO-2026", paste0("ONCO-2026-", s))
}
ae_prep <- edc_ae
ae_prep$USUBJID <- sas_catx(edc_ae$SUBJID)
# Truncate USUBJID to $25 like SAS LENGTH (safety)
ae_prep$USUBJID <- substr(ae_prep$USUBJID, 1, 25)
ae_prep$AESTDTC <- parse_date11_to_iso(edc_ae$STDT)
ae_prep$AEENDTC <- parse_date11_to_iso(edc_ae$ENDT)
# keep needed cols + originals for mapping
#--------------------------------------------------------------
# 3. Sort AE Data by USUBJID AESTDTC TERM (SAS order, missing first)
#--------------------------------------------------------------
o <- sas_sort_order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM)
ae_prep <- ae_prep[o, , drop = FALSE]
rownames(ae_prep) <- NULL

#--------------------------------------------------------------
# 4. Final Mapping and Derivation (sdtm_ae1)
#--------------------------------------------------------------
# Merge ae_prep(in=a) dm_for_ae by USUBJID, if a (left join)
# Drop/rename overlapping cols except keys (only USUBJID overlaps + derived DTC)
stopifnot("USUBJID" %in% names(ae_prep))
stopifnot("USUBJID" %in% names(dm_for_ae))
stopifnot("RFSTDTC" %in% names(dm_for_ae))
# ensure no .x/.y: dm_for_ae only has USUBJID, RFSTDTC
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)
# re-sort after merge to SAS order (merge may reorder)
o2 <- sas_sort_order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM)
sdtm_ae1 <- sdtm_ae1[o2, , drop = FALSE]

sas_up <- function(x) {
  xc <- as.character(x)
  xc[is.na(xc)] <- NA_character_
  idx <- !is.na(xc)
  xc[idx] <- toupper(xc[idx])
  xc
}
sdtm_ae1$STUDYID <- "ONCO-2026"
sdtm_ae1$DOMAIN <- "AE"
sdtm_ae1$AETERM <- substr(trimws(as.character(sdtm_ae1$TERM)), 1, 40)
sdtm_ae1$AETERM[sdtm_ae1$AETERM == ""] <- NA_character_
sdtm_ae1$AEDECOD <- substr(sas_up(sdtm_ae1$DECOD), 1, 40)
sdtm_ae1$AEBODSYS <- sapply(as.character(sdtm_ae1$BODSYS), function(z) {
  if (is.na(z)) return(NA_character_)
  substr(gsub("_", " ", z, fixed = TRUE), 1, 40)
}, USE.NAMES = FALSE)
sdtm_ae1$AESEV <- substr(sas_up(sdtm_ae1$SEV), 1, 10)
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))
sdtm_ae1$AEREL <- substr(sas_up(sdtm_ae1$REL), 1, 20)
sdtm_ae1$AEOUT <- substr(sas_up(sdtm_ae1$OUT), 1, 20)
sdtm_ae1$AESER <- substr(trimws(as.character(sdtm_ae1$SER)), 1, 1)
sdtm_ae1$AESER[sdtm_ae1$AESER == ""] <- NA_character_
# enforce lengths
sdtm_ae1$STUDYID <- substr(sdtm_ae1$STUDYID, 1, 10)
sdtm_ae1$DOMAIN <- substr(sdtm_ae1$DOMAIN, 1, 2)
sdtm_ae1$USUBJID <- substr(sdtm_ae1$USUBJID, 1, 25)

# Derive AESTDY / AEENDY
stdt_d <- as.Date(sdtm_ae1$AESTDTC, format = "%Y-%m-%d")
rfdt_d <- as.Date(substr(sdtm_ae1$RFSTDTC, 1, 10), format = "%Y-%m-%d")
endt_d <- as.Date(sdtm_ae1$AEENDTC, format = "%Y-%m-%d")
# RFSTDTC may already be NA
sdtm_ae1$AESTDY <- rep(NA_real_, nrow(sdtm_ae1))
idx_s <- !is.na(stdt_d) & !is.na(rfdt_d)
diff_s <- as.integer(stdt_d[idx_s] - rfdt_d[idx_s])
sdtm_ae1$AESTDY[idx_s] <- ifelse(diff_s >= 0, diff_s + 1, diff_s)
sdtm_ae1$AEENDY <- rep(NA_real_, nrow(sdtm_ae1))
idx_e <- !is.na(endt_d) & !is.na(rfdt_d)
diff_e <- as.integer(endt_d[idx_e] - rfdt_d[idx_e])
sdtm_ae1$AEENDY[idx_e] <- ifelse(diff_e >= 0, diff_e + 1, diff_e)

#--------------------------------------------------------------
# 5. Sort Final AE Dataset by USUBJID AESTDTC TERM
#--------------------------------------------------------------
o3 <- sas_sort_order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM)
sdtm_ae1 <- sdtm_ae1[o3, , drop = FALSE]
rownames(sdtm_ae1) <- NULL

#--------------------------------------------------------------
# 6. Derive AESEQ (retain by USUBJID)
#--------------------------------------------------------------
sdtm_ae1$AESEQ <- ave(seq_len(nrow(sdtm_ae1)), sdtm_ae1$USUBJID,
  FUN = function(z) seq_along(z))

sdtm_ae <- sdtm_ae1[, c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM",
  "AEDECOD", "AEBODSYS", "AESEV", "AETXGR", "AEREL", "AEOUT", "AESER",
  "AESTDTC", "AEENDTC", "AESTDY", "AEENDY"), drop = FALSE]
# final sort already applied
rownames(sdtm_ae) <- NULL

cat("sdtm_ae n:", nrow(sdtm_ae), " cols:", paste(names(sdtm_ae), collapse = ","), "\n")

#--------------------------------------------------------------
# 7-8. PROC PRINT + Export
#--------------------------------------------------------------
# CSV dataset
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# CSV of table body (same as listing body)
write.csv(sdtm_ae, file.path(out, "P022.csv"), row.names = FALSE, na = "")

# Text rendering of PROC PRINT (Obs + all vars)
txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst")
con_txt <- file(txt_path, open = "wt")
writeLines("PROC PRINT DATA=SDTM_AE;", con_txt)
writeLines("", con_txt)
# Use base print of data frame with Obs column to mimic SAS listing
df_print <- cbind(Obs = seq_len(nrow(sdtm_ae)), sdtm_ae)
# Format numerics without rounding (as stored); keep NA as blank
sink_out <- capture.output(print.data.frame(df_print, row.names = FALSE, right = TRUE, na.print = ""))
writeLines(sink_out, con_txt)
close(con_txt)
# duplicate as .lst for expected name
file.copy(txt_path, lst_path, overwrite = TRUE)
cat("Wrote", file.path(out, "ae.csv"), "and listing\n")
```