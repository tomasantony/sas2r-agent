```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_col_exact <- function(df, target) {
  nms <- names(df)
  idx <- which(toupper(trimws(nms)) == toupper(trimws(target)))
  if (length(idx) >= 1) return(nms[idx[1]])
  return(NA_character_)
}
find_col_contains <- function(df, must_contain, also_contain = NULL) {
  nms <- names(df)
  up <- toupper(nms)
  cand <- seq_along(nms)
  for (p in must_contain) {
    cand <- cand[grepl(toupper(p), up[cand], fixed = TRUE)]
  }
  if (!is.null(also_contain)) {
    for (p in also_contain) {
      cand <- cand[grepl(toupper(p), up[cand], fixed = TRUE)]
    }
  }
  if (length(cand) >= 1) return(nms[cand[1]])
  return(NA_character_)
}

title_month <- function(m) {
  m <- trimws(as.character(m))
  ifelse(is.na(m) | m == "", NA_character_,
    paste0(toupper(substr(m, 1, 1)), tolower(substr(m, 2, 3))))
}

parse_date11_vec <- function(x) {
  n <- length(x)
  if (n == 0) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.character(x)) {
    res <- as.Date(rep(NA_character_, n))
    idx <- !is.na(x) & x > 20000 & x < 80000
    if (any(idx)) res[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(res)
  }
  xc <- trimws(as.character(x))
  res <- as.Date(rep(NA_character_, n))
  need <- !is.na(xc) & xc != "" & toupper(xc) != "NA" & toupper(xc) != "."
  if (!any(need)) return(res)
  recon <- rep(NA_character_, n)
  pat <- "^([0-9]{1,2})[^A-Za-z0-9]*([A-Za-z]{3})[^A-Za-z0-9]*([0-9]{4})$"
  for (i in which(need)) {
    v <- xc[i]
    m <- regmatches(v, regexec(pat, v))[[1]]
    if (length(m) == 4) {
      dd <- sprintf("%02d", suppressWarnings(as.integer(m[2])))
      mm <- title_month(m[3])
      yy <- m[4]
      recon[i] <- paste0(dd, "-", mm, "-", yy)
    } else {
      recon[i] <- NA_character_
    }
  }
  ok <- !is.na(recon)
  if (any(ok)) {
    res[ok] <- as.Date(recon[ok], format = "%d-%b-%Y")
  }
  return(res)
}

parse_yymmdd_vec <- function(x) {
  if (length(x) == 0) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.character(x)) {
    res <- as.Date(rep(NA_character_, length(x)))
    idx <- !is.na(x) & x > 20000 & x < 80000
    if (any(idx)) res[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(res)
  }
  xc <- trimws(as.character(x))
  xc[xc == "" | toupper(xc) == "NA" | xc == "."] <- NA_character_
  return(as.Date(xc, format = "%Y-%m-%d"))
}

parse_generic_date_to_iso <- function(x) {
  if (length(x) == 0) return(rep(NA_character_, 0))
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x) && !is.character(x)) {
    d <- as.Date(rep(NA_character_, length(x)))
    idx <- !is.na(x) & x > 20000 & x < 80000
    if (any(idx)) d[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(format(d, "%Y-%m-%d"))
  }
  xc <- trimws(as.character(x))
  xc[xc == "" | toupper(xc) == "NA" | xc == "."] <- NA_character_
  iso_try <- as.Date(xc, format = "%Y-%m-%d")
  need <- is.na(iso_try) & !is.na(xc)
  if (any(need)) {
    d11 <- parse_date11_vec(xc[need])
    iso_try[need] <- d11
  }
  return(format(iso_try, "%Y-%m-%d"))
}

sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(as.character(x)))
}
sas_tranwrd_us <- function(x) {
  ifelse(is.na(x), NA_character_, gsub("_", " ", as.character(x), fixed = TRUE))
}

ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
rand_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

message("Reading edc_ex: ", ex_path)
edc_ex_raw <- readxl::read_excel(ex_path, .name_repair = "minimal")
edc_ex_raw <- as.data.frame(edc_ex_raw, stringsAsFactors = FALSE)
message("edc_ex rows=", nrow(edc_ex_raw), " cols=", paste(names(edc_ex_raw), collapse="|"))

message("Reading edc_dm: ", dm_path)
edc_dm_raw <- tryCatch({
  d <- readxl::read_excel(dm_path, .name_repair = "minimal")
  as.data.frame(d, stringsAsFactors = FALSE)
}, error = function(e) { message("edc_dm read failed: ", conditionMessage(e)); NULL })

message("Reading ivrs_rand: ", rand_path)
ivrs_rand_raw <- tryCatch({
  d <- readxl::read_excel(rand_path, .name_repair = "minimal")
  as.data.frame(d, stringsAsFactors = FALSE)
}, error = function(e) { message("ivrs_rand read failed: ", conditionMessage(e)); NULL })

c_SUBJID_ex <- find_col_exact(edc_ex_raw, "SUBJID")
if (is.na(c_SUBJID_ex)) c_SUBJID_ex <- find_col_contains(edc_ex_raw, "SUBJID")
if (is.na(c_SUBJID_ex)) c_SUBJID_ex <- find_col_contains(edc_ex_raw, "SUBJ")
stopifnot(!is.na(c_SUBJID_ex))
c_DOSE  <- find_col_exact(edc_ex_raw, "DOSE")
c_STDT  <- find_col_exact(edc_ex_raw, "STDT")
c_ENDT  <- find_col_exact(edc_ex_raw, "ENDT")
c_TRT   <- find_col_exact(edc_ex_raw, "TRT")
c_UNIT  <- find_col_exact(edc_ex_raw, "UNIT")
c_FORM  <- find_col_exact(edc_ex_raw, "FORM")
c_FREQ  <- find_col_exact(edc_ex_raw, "FREQ")
c_ROUTE <- find_col_exact(edc_ex_raw, "ROUTE")
c_LOT   <- find_col_exact(edc_ex_raw, "LOT")
c_VISIT <- find_col_exact(edc_ex_raw, "VISIT")

get_col <- function(df, cn) {
  if (is.na(cn) || !(cn %in% names(df))) return(rep(NA_character_, nrow(df)))
  df[[cn]]
}

SUBJID_v <- trimws(as.character(get_col(edc_ex_raw, c_SUBJID_ex)))
SUBJID_v[SUBJID_v == "" | toupper(SUBJID_v) == "NA"] <- NA_character_

DOSE_raw <- if (!is.na(c_DOSE) && c_DOSE %in% names(edc_ex_raw)) edc_ex_raw[[c_DOSE]] else rep(NA_real_, nrow(edc_ex_raw))
if (is.character(DOSE_raw)) {
  DOSE_raw[trimws(DOSE_raw) == "" | toupper(trimws(DOSE_raw)) == "NA" | trimws(DOSE_raw) == "."] <- NA_character_
  DOSE_num <- suppressWarnings(as.numeric(DOSE_raw))
} else {
  DOSE_num <- suppressWarnings(as.numeric(DOSE_raw))
}

STDT_raw <- get_col(edc_ex_raw, c_STDT)
ENDT_raw <- get_col(edc_ex_raw, c_ENDT)
TRT_raw  <- get_col(edc_ex_raw, c_TRT)
UNIT_raw <- get_col(edc_ex_raw, c_UNIT)
FORM_raw <- get_col(edc_ex_raw, c_FORM)
FREQ_raw <- get_col(edc_ex_raw, c_FREQ)
ROUTE_raw<- get_col(edc_ex_raw, c_ROUTE)
LOT_raw  <- get_col(edc_ex_raw, c_LOT)
VISIT_raw<- get_col(edc_ex_raw, c_VISIT)

iso_from_raw <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    d <- if (inherits(x, "Date")) x else as.Date(x)
    return(format(d, "%Y-%m-%d"))
  }
  if (is.numeric(x) && !is.character(x)) {
    d <- as.Date(rep(NA_character_, length(x)))
    idx <- !is.na(x) & x > 20000 & x < 80000
    if (any(idx)) d[idx] <- as.Date(x[idx], origin = "1899-12-30")
    return(format(d, "%Y-%m-%d"))
  }
  d11 <- parse_date11_vec(as.character(x))
  return(format(d11, "%Y-%m-%d"))
}

EXSTDTC_v <- iso_from_raw(STDT_raw)
EXENDTC_v <- iso_from_raw(ENDT_raw)
EXSTDTC_v[is.na(EXSTDTC_v)] <- NA_character_
EXENDTC_v[is.na(EXENDTC_v)] <- NA_character_

ex_prep <- data.frame(
  SUBJID = SUBJID_v,
  DOSE = DOSE_num,
  EXSTDTC = EXSTDTC_v,
  EXENDTC = EXENDTC_v,
  TRT = trimws(as.character(TRT_raw)),
  UNIT = trimws(as.character(UNIT_raw)),
  FORM = trimws(as.character(FORM_raw)),
  FREQ = trimws(as.character(FREQ_raw)),
  ROUTE = trimws(as.character(ROUTE_raw)),
  LOT = trimws(as.character(LOT_raw)),
  VISIT_RAW = trimws(as.character(VISIT_raw)),
  stringsAsFactors = FALSE
)
ex_prep$TRT[ex_prep$TRT == "" | toupper(ex_prep$TRT) == "NA"] <- NA_character_
ex_prep$UNIT[ex_prep$UNIT == "" | toupper(ex_prep$UNIT) == "NA"] <- NA_character_
ex_prep$FORM[ex_prep$FORM == "" | toupper(ex_prep$FORM) == "NA"] <- NA_character_
ex_prep$FREQ[ex_prep$FREQ == "" | toupper(ex_prep$FREQ) == "NA"] <- NA_character_
ex_prep$ROUTE[ex_prep$ROUTE == "" | toupper(ex_prep$ROUTE) == "NA"] <- NA_character_
ex_prep$LOT[ex_prep$LOT == "" | toupper(ex_prep$LOT) == "NA"] <- NA_character_

ex_prep$USUBJID <- ifelse(is.na(ex_prep$SUBJID) | ex_prep$SUBJID == "",
  NA_character_, paste0("ONCO-2026-", ex_prep$SUBJID))

ex_prep <- ex_prep[!is.na(ex_prep$DOSE), , drop = FALSE]

ord1 <- order(ex_prep$USUBJID, ex_prep$EXSTDTC, na.last = FALSE)
ex_prep <- ex_prep[ord1, , drop = FALSE]

derive_dm_for_ex <- function(edc_dm_raw, ivrs_rand_raw, ex_prep) {
  rf_map <- NULL
  if (!is.null(edc_dm_raw) && nrow(edc_dm_raw) > 0) {
    c_rf <- find_col_exact(edc_dm_raw, "RFSTDTC")
    if (!is.na(c_rf)) {
      c_subj_dm <- find_col_exact(edc_dm_raw, "SUBJID")
      if (is.na(c_subj_dm)) c_subj_dm <- find_col_contains(edc_dm_raw, "SUBJ")
      if (!is.na(c_subj_dm)) {
        subj_dm <- trimws(as.character(edc_dm_raw[[c_subj_dm]]))
        rf_raw <- edc_dm_raw[[c_rf]]
        rf_iso <- parse_generic_date_to_iso(rf_raw)
        rf_map_dm <- data.frame(
          SUBJID = subj_dm,
          RFSTDTC_DM = rf_iso,
          stringsAsFactors = FALSE
        )
        rf_map_dm <- rf_map_dm[!is.na(rf_map_dm$SUBJID) & rf_map_dm$SUBJID != "", , drop = FALSE]
        rf_map_dm$USUBJID <- paste0("ONCO-2026-", rf_map_dm$SUBJID)
        rf_map_dm <- rf_map_dm[!duplicated(rf_map_dm$USUBJID), c("USUBJID", "RFSTDTC_DM"), drop = FALSE]
        rf_map <- rf_map_dm
        message("RFSTDTC found in edc_dm: ", c_rf)
      }
    }
  }
  if (is.null(rf_map)) {
    rand_candidates <- list()
    if (!is.null(ivrs_rand_raw) && nrow(ivrs_rand_raw) > 0) {
      nms_r <- names(ivrs_rand_raw)
      c_subj_r <- find_col_exact(ivrs_rand_raw, "SUBJID")
      if (is.na(c_subj_r)) c_subj_r <- find_col_contains(ivrs_rand_raw, "SUBJ")
      date_cols_r <- c()
      for (nm in nms_r) {
        up <- toupper(nm)
        if (grepl("RAND", up, fixed = TRUE) && (grepl("DAT", up, fixed = TRUE) || grepl("DT", up, fixed = TRUE) || grepl("DTC", up, fixed = TRUE))) {
          date_cols_r <- c(date_cols_r, nm)
        }
      }
      if (length(date_cols_r) == 0) {
        for (nm in nms_r) {
          up <- toupper(nm)
          if (grepl("DAT", up, fixed = TRUE) || grepl("DTC", up, fixed = TRUE) || up %in% c("RANDDT", "RANDDTC", "RAND_DT")) {
            date_cols_r <- c(date_cols_r, nm)
          }
        }
      }
      if (!is.na(c_subj_r) && length(date_cols_r) > 0) {
        message("ivrs_rand date cols: ", paste(date_cols_r, collapse = "|"))
        subj_r <- trimws(as.character(ivrs_rand_raw[[c_subj_r]]))
        best_iso <- rep(NA_character_, length(subj_r))
        for (dc in date_cols_r) {
          iso <- parse_generic_date_to_iso(ivrs_rand_raw[[dc]])
          fill <- is.na(best_iso) & !is.na(iso)
          best_iso[fill] <- iso[fill]
        }
        rf_map_rand <- data.frame(SUBJID = subj_r, RFSTDTC_DM = best_iso, stringsAsFactors = FALSE)
        rf_map_rand <- rf_map_rand[!is.na(rf_map_rand$SUBJID) & rf_map_rand$SUBJID != "", , drop = FALSE]
        rf_map_rand$USUBJID <- paste0("ONCO-2026-", rf_map_rand$SUBJID)
        rf_map_rand <- rf_map_rand[!duplicated(rf_map_rand$USUBJID), c("USUBJID", "RFSTDTC_DM"), drop = FALSE]
        if (any(!is.na(rf_map_rand$RFSTDTC_DM))) {
          rf_map <- rf_map_rand
          message("RFSTDTC derived from ivrs_rand")
        }
      }
    }
    if (is.null(rf_map) && !is.null(edc_dm_raw) && nrow(edc_dm_raw) > 0) {
      nms_d <- names(edc_dm_raw)
      c_subj_d <- find_col_exact(edc_dm_raw, "SUBJID")
      if (is.na(c_subj_d)) c_subj_d <- find_col_contains(edc_dm_raw, "SUBJ")
      date_cols_d <- c()
      for (nm in nms_d) {
        up <- toupper(nm)
        if ((grepl("RAND", up, fixed = TRUE) || grepl("ENROLL", up, fixed = TRUE) || grepl("RFSTD", up, fixed = TRUE) || grepl("START", up, fixed = TRUE)) &&
            (grepl("DAT", up, fixed = TRUE) || grepl("DT", up, fixed = TRUE) || grepl("DTC", up, fixed = TRUE))) {
          date_cols_d <- c(date_cols_d, nm)
        }
      }
      if (!is.na(c_subj_d) && length(date_cols_d) > 0) {
        message("edc_dm date cols: ", paste(date_cols_d, collapse = "|"))
        subj_d <- trimws(as.character(edc_dm_raw[[c_subj_d]]))
        best_iso <- rep(NA_character_, length(subj_d))
        for (dc in date_cols_d) {
          iso <- parse_generic_date_to_iso(edc_dm_raw[[dc]])
          fill <- is.na(best_iso) & !is.na(iso)
          best_iso[fill] <- iso[fill]
        }
        rf_map_dm2 <- data.frame(SUBJID = subj_d, RFSTDTC_DM = best_iso, stringsAsFactors = FALSE)
        rf_map_dm2 <- rf_map_dm2[!is.na(rf_map_dm2$SUBJID) & rf_map_dm2$SUBJID != "", , drop = FALSE]
        rf_map_dm2$USUBJID <- paste0("ONCO-2026-", rf_map_dm2$SUBJID)
        rf_map_dm2 <- rf_map_dm2[!duplicated(rf_map_dm2$USUBJID), c("USUBJID", "RFSTDTC_DM"), drop = FALSE]
        if (any(!is.na(rf_map_dm2$RFSTDTC_DM))) {
          rf_map <- rf_map_dm2
          message("RFSTDTC derived from edc_dm date cols")
        }
      }
    }
  }
  first_ex <- aggregate(EXSTDTC ~ USUBJID, data = ex_prep[!is.na(ex_prep$EXSTDTC) & ex_prep$EXSTDTC != "", , drop = FALSE],
    FUN = function(x) min(x, na.rm = TRUE))
  names(first_ex)[names(first_ex) == "EXSTDTC"] <- "FIRST_EX"
  if (is.null(rf_map)) {
    dm_for_ex <- merge(data.frame(USUBJID = unique(ex_prep$USUBJID), stringsAsFactors = FALSE),
      first_ex, by = "USUBJID", all.x = TRUE)
    dm_for_ex$RFSTDTC <- dm_for_ex$FIRST_EX
    dm_for_ex <- dm_for_ex[, c("USUBJID", "RFSTDTC"), drop = FALSE]
    message("RFSTDTC fallback to first EXSTDTC (CDISC first-exposure)")
  } else {
    base <- data.frame(USUBJID = unique(ex_prep$USUBJID), stringsAsFactors = FALSE)
    base <- merge(base, rf_map[, c("USUBJID", "RFSTDTC_DM"), drop = FALSE], by = "USUBJID", all.x = TRUE)
    base <- merge(base, first_ex, by = "USUBJID", all.x = TRUE)
    base$RFSTDTC <- ifelse(!is.na(base$RFSTDTC_DM) & base$RFSTDTC_DM != "",
      base$RFSTDTC_DM, base$FIRST_EX)
    dm_for_ex <- base[, c("USUBJID", "RFSTDTC"), drop = FALSE]
    message("RFSTDTC coalesce(rand/dm, first-ex)")
  }
  dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID, na.last = FALSE), , drop = FALSE]
  return(dm_for_ex)
}

dm_for_ex <- derive_dm_for_ex(edc_dm_raw, ivrs_rand_raw, ex_prep)
stopifnot("USUBJID" %in% names(dm_for_ex))
stopifnot("RFSTDTC" %in% names(dm_for_ex))
message("dm_for_ex rows=", nrow(dm_for_ex))

sdtm_ex1 <- merge(ex_prep, dm_for_ex, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("RFSTDTC" %in% names(sdtm_ex1))
stopifnot("EXSTDTC" %in% names(sdtm_ex1))

sdtm_ex1$STUDYID <- "ONCO-2026"
sdtm_ex1$DOMAIN <- "EX"
sdtm_ex1$EXTRT <- sas_upcase(sdtm_ex1$TRT)
sdtm_ex1$EXDOSE <- sdtm_ex1$DOSE
sdtm_ex1$EXDOSU <- sas_upcase(sdtm_ex1$UNIT)
sdtm_ex1$EXDOSFRM <- sas_tranwrd_us(sdtm_ex1$FORM)
sdtm_ex1$EXDOSFRM <- sas_upcase(sdtm_ex1$EXDOSFRM)
sdtm_ex1$EXDOSFRM[sdtm_ex1$FORM %in% c(NA_character_) ] <- NA_character_
sdtm_ex1$EXDOSFRM <- ifelse(is.na(sdtm_ex1$FORM), NA_character_, toupper(gsub("_", " ", as.character(sdtm_ex1$FORM), fixed = TRUE)))
sdtm_ex1$EXDOSFRQ <- sas_upcase(sdtm_ex1$FREQ)
sdtm_ex1$EXVLDIST <- sas_upcase(sdtm_ex1$ROUTE)
sdtm_ex1$EXLOT <- trimws(as.character(sdtm_ex1$LOT))
sdtm_ex1$EXLOT[sdtm_ex1$EXLOT == "" | toupper(sdtm_ex1$EXLOT) == "NA"] <- NA_character_
sdtm_ex1$VISIT <- NA_character_

stdt_d <- parse_yymmdd_vec(sdtm_ex1$EXSTDTC)
endt_d <- parse_yymmdd_vec(sdtm_ex1$EXENDTC)
rfdt_d <- parse_yymmdd_vec(sdtm_ex1$RFSTDTC)

sdtm_ex1$EXSTDY <- rep(NA_real_, nrow(sdtm_ex1))
sdtm_ex1$EXENDY <- rep(NA_real_, nrow(sdtm_ex1))
idx_s <- !is.na(stdt_d) & !is.na(rfdt_d)
sdtm_ex1$EXSTDY[idx_s & stdt_d >= rfdt_d] <- as.numeric(stdt_d[idx_s & stdt_d >= rfdt_d] - rfdt_d[idx_s & stdt_d >= rfdt_d]) + 1
sdtm_ex1$EXSTDY[idx_s & stdt_d < rfdt_d] <- as.numeric(stdt_d[idx_s & stdt_d < rfdt_d] - rfdt_d[idx_s & stdt_d < rfdt_d])
idx_e <- !is.na(endt_d) & !is.na(rfdt_d)
sdtm_ex1$EXENDY[idx_e & endt_d >= rfdt_d] <- as.numeric(endt_d[idx_e & endt_d >= rfdt_d] - rfdt_d[idx_e & endt_d >= rfdt_d]) + 1
sdtm_ex1$EXENDY[idx_e & endt_d < rfdt_d] <- as.numeric(endt_d[idx_e & endt_d < rfdt_d] - rfdt_d[idx_e & endt_d < rfdt_d])

ord2 <- order(sdtm_ex1$USUBJID, sdtm_ex1$EXSTDTC, na.last = FALSE)
sdtm_ex1 <- sdtm_ex1[ord2, , drop = FALSE]

sdtm_ex1 <- sdtm_ex1[order(sdtm_ex1$USUBJID, sdtm_ex1$EXSTDTC, na.last = FALSE), , drop = FALSE]
sdtm_ex1$EXSEQ <- ave(seq_len(nrow(sdtm_ex1)), sdtm_ex1$USUBJID, FUN = function(x) seq_along(x))

sdtm_ex <- sdtm_ex1[, c("STUDYID","DOMAIN","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXDOSFRQ","EXVLDIST","EXLOT","EXSTDTC","EXENDTC","EXSTDY","EXENDY","VISIT"), drop = FALSE]
sdtm_ex <- sdtm_ex[order(sdtm_ex$USUBJID, sdtm_ex$EXSTDTC, na.last = FALSE), , drop = FALSE]
rownames(sdtm_ex) <- NULL

message("sdtm_ex rows=", nrow(sdtm_ex))

write.csv(sdtm_ex, file.path(out, "sdtm_ex.csv"), row.names = FALSE, na = "")
write.csv(sdtm_ex, file.path(out, "P025.csv"), row.names = FALSE, na = "")
write.csv(sdtm_ex, file.path(out, "P025.lst.csv"), row.names = FALSE, na = "")

fmt_cell <- function(v) {
  if (is.na(v) || (is.character(v) && v == "")) return("")
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  return(as.character(v))
}

cols <- names(sdtm_ex)
header <- paste(c("Obs", cols), collapse = " | ")
sep_line <- paste(rep("-", nchar(header)), collapse = "")
lines <- c("SDTM EX Dataset (PROC PRINT of SDTM_EX)", header, sep_line)
for (i in seq_len(nrow(sdtm_ex))) {
  vals <- sapply(cols, function(cn) fmt_cell(sdtm_ex[i, cn]))
  lines <- c(lines, paste(c(as.character(i), vals), collapse = " | "))
}
lines <- c(lines, "", paste0("N = ", nrow(sdtm_ex)))

con1 <- file(file.path(out, "P025.txt"), open = "wt")
writeLines(lines, con = con1)
close(con1)
con2 <- file(file.path(out, "P025.lst.txt"), open = "wt")
writeLines(lines, con = con2)
close(con2)

message("Done. Outputs written to ", out)
```