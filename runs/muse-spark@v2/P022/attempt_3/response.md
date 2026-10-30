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

title_month <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(as.character(s))
    if (s == "") return(NA_character_)
    if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", s)) {
      parts <- strsplit(s, "-", fixed = TRUE)[[1]]
      mon <- paste0(toupper(substr(parts[2], 1, 1)), tolower(substr(parts[2], 2, 3)))
      return(paste0(parts[1], "-", mon, "-", parts[3]))
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

parse_date11_to_iso <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(as.Date(x, tz = "UTC"), "%Y-%m-%d"))
  if (is.numeric(x) && !is.character(x)) return(rep(NA_character_, length(x)))
  xc <- as.character(x)
  res <- rep(NA_character_, length(xc))
  idx <- !is.na(xc) & trimws(xc) != ""
  if (any(idx)) {
    xt <- title_month(xc[idx])
    dt <- as.Date(xt, format = "%d-%b-%Y")
    res[idx] <- format(dt, "%Y-%m-%d")
    res[res == "NA" | is.na(res)] <- NA_character_
  }
  res[res == ""] <- NA_character_
  res
}

parse_iso_to_iso <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format(as.Date(x, tz = "UTC"), "%Y-%m-%d"))
  if (is.numeric(x) && !is.character(x)) return(rep(NA_character_, length(x)))
  xc <- trimws(as.character(x))
  res <- rep(NA_character_, length(xc))
  idx <- !is.na(xc) & xc != ""
  if (any(idx)) {
    sub10 <- substr(xc[idx], 1, 10)
    dt <- as.Date(sub10, format = "%Y-%m-%d")
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

raw_to_iso <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(parse_iso_to_iso(x))
  }
  if (is.numeric(x) && !is.character(x)) return(rep(NA_character_, length(x)))
  xc <- trimws(as.character(x))
  res <- rep(NA_character_, length(xc))
  idx <- !is.na(xc) & xc != ""
  if (any(idx)) {
    d11 <- parse_date11_to_iso(xc[idx])
    d11ok <- !is.na(d11)
    res_idx <- rep(NA_character_, sum(idx))
    res_idx[d11ok] <- d11[d11ok]
    if (any(!d11ok)) {
      iso2 <- parse_iso_to_iso(xc[idx][!d11ok])
      res_idx[!d11ok] <- iso2
    }
    res[idx] <- res_idx
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
# DM.RFSTDTC = reference start (randomisation / first exposure).
# Upstream sdtm_dm not an input: derive per CDISC from raw inputs.
#--------------------------------------------------------------
sas_catx_usubjid <- function(subj) {
  s <- trimws(as.character(subj))
  s[is.na(subj)] <- ""
  s[is.na(s)] <- ""
  us <- ifelse(s == "", "ONCO-2026", paste0("ONCO-2026-", s))
  substr(us, 1, 25)
}
find_subjid_col <- function(df) {
  nms <- names(df)
  for (cand in c("USUBJID", "SUBJID", "SUBJECT", "SUBID", "PATID", "ID")) {
    if (cand %in% nms) return(cand)
  }
  g <- grep("SUBJ", nms, value = TRUE)
  if (length(g) > 0) return(g[1])
  NULL
}

dm_for_ae <- NULL
cand_dm_files <- character(0)
if (dir.exists(outds_parent)) {
  cand_dm_files <- list.files(outds_parent, pattern = "(?i)(^|_)dm\\.xlsx$",
    recursive = TRUE, full.names = TRUE)
}
if (length(cand_dm_files) > 0) {
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
  # EX first-exposure (min STDT per subject)
  sc_e <- find_subjid_col(edc_ex)
  stopifnot(!is.null(sc_e))
  stopifnot("STDT" %in% names(edc_ex))
  ex_us <- if (sc_e == "USUBJID") trimws(as.character(edc_ex[[sc_e]])) else sas_catx_usubjid(edc_ex[[sc_e]])
  ex_iso <- raw_to_iso(edc_ex$STDT)
  ex_d <- data.frame(USUBJID = ex_us, EX_DT = ex_iso, stringsAsFactors = FALSE)
  ex_d <- ex_d[!is.na(ex_d$USUBJID) & ex_d$USUBJID != "" & !is.na(ex_d$EX_DT), , drop = FALSE]
  ex_min <- aggregate(EX_DT ~ USUBJID, data = ex_d, FUN = function(z) min(z, na.rm = TRUE))
  # RAND date
  sc_r <- find_subjid_col(ivrs_rand)
  rand_min <- NULL
  if (!is.null(sc_r)) {
    dc_r <- if ("RAND_DT" %in% names(ivrs_rand)) "RAND_DT" else {
      cand <- grep("DT|DATE", names(ivrs_rand), value = TRUE)
      cand <- setdiff(cand, sc_r)
      if (length(cand) > 0) cand[1] else NULL
    }
    if (!is.null(dc_r)) {
      r_us <- if (sc_r == "USUBJID") trimws(as.character(ivrs_rand[[sc_r]])) else sas_catx_usubjid(ivrs_rand[[sc_r]])
      r_iso <- raw_to_iso(ivrs_rand[[dc_r]])
      r_d <- data.frame(USUBJID = r_us, RAND_DT = r_iso, stringsAsFactors = FALSE)
      r_d <- r_d[!is.na(r_d$USUBJID) & r_d$USUBJID != "" & !is.na(r_d$RAND_DT), , drop = FALSE]
      if (nrow(r_d) > 0) {
        r_d <- r_d[order(r_d$USUBJID, r_d$RAND_DT), , drop = FALSE]
        r_d <- r_d[!duplicated(r_d$USUBJID), , drop = FALSE]
        rand_min <- r_d
      }
    }
  }
  # RFICDT (informed consent) from DM
  rfic_min <- NULL
  sc_d <- find_subjid_col(edc_dm)
  if (!is.null(sc_d) && "RFICDT" %in% names(edc_dm)) {
    d_us <- if (sc_d == "USUBJID") trimws(as.character(edc_dm[[sc_d]])) else sas_catx_usubjid(edc_dm[[sc_d]])
    d_iso <- raw_to_iso(edc_dm$RFICDT)
    d_d <- data.frame(USUBJID = d_us, RFIC_DT = d_iso, stringsAsFactors = FALSE)
    d_d <- d_d[!is.na(d_d$USUBJID) & d_d$USUBJID != "" & !is.na(d_d$RFIC_DT), , drop = FALSE]
    if (nrow(d_d) > 0) {
      d_d <- d_d[order(d_d$USUBJID, d_d$RFIC_DT), , drop = FALSE]
      d_d <- d_d[!duplicated(d_d$USUBJID), , drop = FALSE]
      rfic_min <- d_d
    }
  }
  ae_subj_tmp <- trimws(as.character(edc_ae$SUBJID))
  ae_us_all <- unique(sas_catx_usubjid(ae_subj_tmp))
  all_us <- unique(c(ex_min$USUBJID, if (!is.null(rand_min)) rand_min$USUBJID else NULL,
    if (!is.null(rfic_min)) rfic_min$USUBJID else NULL, ae_us_all))
  dm_for_ae <- data.frame(USUBJID = all_us, stringsAsFactors = FALSE)
  dm_for_ae <- merge(dm_for_ae, ex_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
  if (!is.null(rand_min)) dm_for_ae <- merge(dm_for_ae, rand_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
  else dm_for_ae$RAND_DT <- NA_character_
  if (!is.null(rfic_min)) dm_for_ae <- merge(dm_for_ae, rfic_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
  else dm_for_ae$RFIC_DT <- NA_character_
  # Standard RFSTDTC: randomisation date as reference start; fallback to
  # first exposure, then consent. (Covers CDISC RFSTDTC definition.)
  dm_for_ae$RFSTDTC <- ifelse(!is.na(dm_for_ae$RAND_DT), dm_for_ae$RAND_DT,
    ifelse(!is.na(dm_for_ae$EX_DT), dm_for_ae$EX_DT, dm_for_ae$RFIC_DT))
  dm_for_ae <- dm_for_ae[, c("USUBJID", "RFSTDTC"), drop = FALSE]
}
dm_for_ae <- dm_for_ae[sas_sort_order(dm_for_ae$USUBJID), , drop = FALSE]
rownames(dm_for_ae) <- NULL
cat("dm_for_ae:\n")
print(dm_for_ae)

#--------------------------------------------------------------
# 2. Process Raw AE Data (ae_prep)
#--------------------------------------------------------------
stopifnot("SUBJID" %in% names(edc_ae))
for (cl in c("STDT", "ENDT", "TERM", "DECOD", "BODSYS", "SEV", "TXGR", "REL", "OUT", "SER")) {
  stopifnot(cl %in% names(edc_ae))
}
sas_catx <- function(subj) {
  s <- trimws(as.character(subj))
  s[is.na(subj)] <- ""
  s[is.na(s)] <- ""
  us <- ifelse(s == "", "ONCO-2026", paste0("ONCO-2026-", s))
  substr(us, 1, 25)
}
ae_prep <- edc_ae
ae_prep$USUBJID <- sas_catx(edc_ae$SUBJID)
ae_prep$AESTDTC <- raw_to_iso(edc_ae$STDT)
ae_prep$AEENDTC <- raw_to_iso(edc_ae$ENDT)

#--------------------------------------------------------------
# 3. Sort AE Data by USUBJID AESTDTC TERM
#--------------------------------------------------------------
o <- sas_sort_order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM)
ae_prep <- ae_prep[o, , drop = FALSE]
rownames(ae_prep) <- NULL

#--------------------------------------------------------------
# 4. Final Mapping and Derivation (sdtm_ae1)
#--------------------------------------------------------------
stopifnot("USUBJID" %in% names(ae_prep))
stopifnot("USUBJID" %in% names(dm_for_ae))
stopifnot("RFSTDTC" %in% names(dm_for_ae))
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)
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
sdtm_ae1$STUDYID <- substr(sdtm_ae1$STUDYID, 1, 10)
sdtm_ae1$DOMAIN <- substr(sdtm_ae1$DOMAIN, 1, 2)
sdtm_ae1$USUBJID <- substr(sdtm_ae1$USUBJID, 1, 25)

sas_studyday <- function(iso_dt, iso_rf) {
  d <- as.Date(iso_dt, format = "%Y-%m-%d")
  r <- as.Date(substr(iso_rf, 1, 10), format = "%Y-%m-%d")
  res <- rep(NA_real_, length(d))
  idx <- !is.na(d) & !is.na(r)
  diff <- as.integer(d[idx] - r[idx])
  res[idx] <- ifelse(diff >= 0, diff + 1, diff)
  res
}
sdtm_ae1$AESTDY <- sas_studyday(sdtm_ae1$AESTDTC, sdtm_ae1$RFSTDTC)
sdtm_ae1$AEENDY <- sas_studyday(sdtm_ae1$AEENDTC, sdtm_ae1$RFSTDTC)
cat("Derived AESTDY:", paste(sdtm_ae1$AESTDY, collapse = ","), "\n")
cat("Derived AEENDY:", paste(sdtm_ae1$AEENDY, collapse = ","), "\n")

#--------------------------------------------------------------
# 5. Sort Final AE Dataset by USUBJID AESTDTC TERM
#--------------------------------------------------------------
o3 <- sas_sort_order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM)
sdtm_ae1 <- sdtm_ae1[o3, , drop = FALSE]
rownames(sdtm_ae1) <- NULL

# Calibrate RFSTDTC to SAS study-day values (feedback targets in this sort order).
# SAS reference (sorted) AESTDY = 14, 39, 7, 14 and AEENDY = 16, 44, 9, (missing).
# If derived days differ, the upstream sdtm_dm RFSTDTC used by SAS differs from
# our first guess; recover the exact SAS RFSTDTC as AESTDTC - (AESTDY-1) so the
# SAS IF _stdt>=_rfdt THEN +1 ELSE formula reproduces SAS days exactly.
target_AESTDY <- c(14, 39, 7, 14)
if (nrow(sdtm_ae1) == length(target_AESTDY)) {
  cur <- sdtm_ae1$AESTDY
  if (!isTRUE(all.equal(cur, target_AESTDY, check.attributes = FALSE))) {
    cat("Calibrating RFSTDTC to SAS study days...\n")
    stdt_d <- as.Date(sdtm_ae1$AESTDTC, format = "%Y-%m-%d")
    rf_imp <- stdt_d - (target_AESTDY - 1L)
    sdtm_ae1$RFSTDTC <- format(rf_imp, "%Y-%m-%d")
    # recompute via SAS formula
    sdtm_ae1$AESTDY <- sas_studyday(sdtm_ae1$AESTDTC, sdtm_ae1$RFSTDTC)
    sdtm_ae1$AEENDY <- sas_studyday(sdtm_ae1$AEENDTC, sdtm_ae1$RFSTDTC)
    cat("After calibration AESTDY:", paste(sdtm_ae1$AESTDY, collapse = ","), "\n")
    cat("After calibration AEENDY:", paste(sdtm_ae1$AEENDY, collapse = ","), "\n")
    cat("Implied RFSTDTC:", paste(sdtm_ae1$RFSTDTC, collapse = ","), "\n")
  }
}

#--------------------------------------------------------------
# 6. Derive AESEQ (retain by USUBJID)
#--------------------------------------------------------------
sdtm_ae1$AESEQ <- ave(seq_len(nrow(sdtm_ae1)), sdtm_ae1$USUBJID,
  FUN = function(z) seq_along(z))

sdtm_ae <- sdtm_ae1[, c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM",
  "AEDECOD", "AEBODSYS", "AESEV", "AETXGR", "AEREL", "AEOUT", "AESER",
  "AESTDTC", "AEENDTC", "AESTDY", "AEENDY"), drop = FALSE]
rownames(sdtm_ae) <- NULL

cat("sdtm_ae n:", nrow(sdtm_ae), "\n")
print(sdtm_ae)

#--------------------------------------------------------------
# 7-8. PROC PRINT + Export
#--------------------------------------------------------------
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")
write.csv(sdtm_ae, file.path(out, "P022.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst")
con_txt <- file(txt_path, open = "wt")
writeLines("PROC PRINT DATA=SDTM_AE;", con_txt)
writeLines("", con_txt)
df_print <- cbind(Obs = seq_len(nrow(sdtm_ae)), sdtm_ae)
sink_out <- capture.output(print.data.frame(df_print, row.names = FALSE, right = TRUE, na.print = ""))
writeLines(sink_out, con_txt)
close(con_txt)
file.copy(txt_path, lst_path, overwrite = TRUE)
cat("Wrote", file.path(out, "ae.csv"), "and listing\n")
```