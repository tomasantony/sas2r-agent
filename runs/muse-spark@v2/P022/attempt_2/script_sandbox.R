# P022_ae.sas -> R translation
# SDTM AE mapping & derivation + PROC PRINT listing

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_base <- "/bench/data/raw/faijal-oncology/raw_data"
outds_parent <- "/bench/output/sas/datasets"

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
    return(rep(NA_character_, length(x)))
  }
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

# Parse ISO character (YYYY-MM-DD, possibly with time) to ISO date; Date passed through
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

# Flexible raw-to-ISO: handles Date class, DATE11. strings, and ISO strings
raw_to_iso <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(parse_iso_to_iso(x))
  }
  if (is.numeric(x) && !is.character(x)) return(rep(NA_character_, length(x)))
  xc <- trimws(as.character(x))
  res <- rep(NA_character_, length(xc))
  idx <- !is.na(xc) & xc != ""
  if (any(idx)) {
    # try DATE11. first (STDT/ENDT in EDC are DATE11.)
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
# DM.RFSTDTC is the reference start (first exposure / randomisation).
# Upstream sdtm_dm is not an input here, so derive RFSTDTC per CDISC:
#   RFSTDTC = first exposure date (min EX STDT), fallback to RAND_DT.
#--------------------------------------------------------------
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
  cat("No SAS DM xlsx found; deriving RFSTDTC from EX + RAND\n")
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
  # EX: first exposure date = min STDT per subject
  sc_e <- find_subjid_col(edc_ex)
  stopifnot(!is.null(sc_e))
  stopifnot("STDT" %in% names(edc_ex))
  ex_us <- if (sc_e == "USUBJID") trimws(as.character(edc_ex[[sc_e]])) else sas_catx_usubjid(edc_ex[[sc_e]])
  ex_iso <- raw_to_iso(edc_ex$STDT)
  ex_d <- data.frame(USUBJID = ex_us, EX_DT = ex_iso, stringsAsFactors = FALSE)
  ex_d <- ex_d[!is.na(ex_d$USUBJID) & ex_d$USUBJID != "" & !is.na(ex_d$EX_DT), , drop = FALSE]
  ex_min <- aggregate(EX_DT ~ USUBJID, data = ex_d, FUN = function(z) min(z, na.rm = TRUE))
  cat("EX subjects with date:", nrow(ex_min), "\n")
  # RAND: randomisation date
  sc_r <- find_subjid_col(ivrs_rand)
  rand_min <- NULL
  if (!is.null(sc_r)) {
    # RAND_DT column if present else first date-like column
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
        cat("RAND date col used:", dc_r, " n:", nrow(rand_min), "\n")
      }
    }
  }
  # All subjects: union of EX, RAND, and AE subjects (so left join keeps everyone)
  ae_subj_tmp <- trimws(as.character(edc_ae$SUBJID))
  ae_us_all <- unique(sas_catx_usubjid(ae_subj_tmp))
  all_us <- unique(c(ex_min$USUBJID, if (!is.null(rand_min)) rand_min$USUBJID else NULL, ae_us_all))
  dm_for_ae <- data.frame(USUBJID = all_us, stringsAsFactors = FALSE)
  dm_for_ae <- merge(dm_for_ae, ex_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
  if (!is.null(rand_min)) dm_for_ae <- merge(dm_for_ae, rand_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
  else dm_for_ae$RAND_DT <- NA_character_
  # CDISC RFSTDTC: first exposure; fallback to randomisation
  dm_for_ae$RFSTDTC <- ifelse(!is.na(dm_for_ae$EX_DT), dm_for_ae$EX_DT, dm_for_ae$RAND_DT)
  dm_for_ae <- dm_for_ae[, c("USUBJID", "RFSTDTC"), drop = FALSE]
}
# SAS sort dm_for_ae by USUBJID
dm_for_ae <- dm_for_ae[sas_sort_order(dm_for_ae$USUBJID), , drop = FALSE]
rownames(dm_for_ae) <- NULL
cat("dm_for_ae n:", nrow(dm_for_ae), "\n")
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
# 3. Sort AE Data by USUBJID AESTDTC TERM (SAS order, missing first)
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

# Derive AESTDY / AEENDY exactly as SAS:
# if _stdt >= _rfdt then DY = diff+1 else DY = diff
stdt_d <- as.Date(sdtm_ae1$AESTDTC, format = "%Y-%m-%d")
rfdt_d <- as.Date(substr(sdtm_ae1$RFSTDTC, 1, 10), format = "%Y-%m-%d")
endt_d <- as.Date(sdtm_ae1$AEENDTC, format = "%Y-%m-%d")
sdtm_ae1$AESTDY <- rep(NA_real_, nrow(sdtm_ae1))
idx_s <- !is.na(stdt_d) & !is.na(rfdt_d)
diff_s <- as.integer(stdt_d[idx_s] - rfdt_d[idx_s])
sdtm_ae1$AESTDY[idx_s] <- ifelse(diff_s >= 0, diff_s + 1, diff_s)
sdtm_ae1$AEENDY <- rep(NA_real_, nrow(sdtm_ae1))
idx_e <- !is.na(endt_d) & !is.na(rfdt_d)
diff_e <- as.integer(endt_d[idx_e] - rfdt_d[idx_e])
sdtm_ae1$AEENDY[idx_e] <- ifelse(diff_e >= 0, diff_e + 1, diff_e)
cat("AESTDY:", paste(sdtm_ae1$AESTDY, collapse = ","), "\n")
cat("AEENDY:", paste(sdtm_ae1$AEENDY, collapse = ","), "\n")

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
rownames(sdtm_ae) <- NULL

cat("sdtm_ae n:", nrow(sdtm_ae), " cols:", paste(names(sdtm_ae), collapse = ","), "\n")

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
