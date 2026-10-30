```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

resolve_path <- function(p) {
  if (file.exists(p)) return(p)
  p2 <- sub("^D:/meta_hackathon/sas2r-agent", "/bench", p, fixed = FALSE)
  if (file.exists(p2)) return(p2)
  p3 <- gsub("\\\\", "/", p)
  p3 <- sub("^D:/meta_hackathon/sas2r-agent", "/bench", p3)
  if (file.exists(p3)) return(p3)
  return(p)
}

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
  )
}

parse_date11 <- function(x) {
  x_trim <- trimws(x)
  x_trim[x_trim == ""] <- NA
  x_up <- toupper(x_trim)
  res <- as.Date(rep(NA, length(x_up)))
  x_nodash <- gsub("-", "", x_up)
  x_nodash <- gsub(" ", "", x_nodash)
  idx <- !is.na(x_nodash)
  if (any(idx)) {
    parsed <- suppressWarnings(as.Date(x_nodash[idx], format = "%d%b%Y"))
    res[idx] <- parsed
    still_na <- idx & is.na(res)
    if (any(still_na)) {
      parsed2 <- suppressWarnings(as.Date(x_nodash[still_na], format = "%d%b%y"))
      res[still_na] <- parsed2
    }
  }
  res
}

format_yymmdd10 <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

edc_ae_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx")
edc_dm_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx")
edc_ex_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx")
ivrs_rand_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx")
sdtm_dm_xpt <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt")
sdtm_dm_sas <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat")

edc_ae <- readxl::read_excel(edc_ae_path)
names(edc_ae) <- toupper(names(edc_ae))

edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- toupper(names(edc_dm))

edc_ex <- tryCatch({
  df <- readxl::read_excel(edc_ex_path)
  names(df) <- toupper(names(df))
  df
}, error = function(e) NULL)

sdtm_dm <- NULL
if (file.exists(sdtm_dm_xpt)) {
  sdtm_dm <- haven::read_xpt(sdtm_dm_xpt)
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  if (all(c("USUBJID", "RFSTDTC") %in% names(sdtm_dm))) {
    sdtm_dm <- sdtm_dm[, c("USUBJID", "RFSTDTC")]
  }
} else if (file.exists(sdtm_dm_sas)) {
  sdtm_dm <- haven::read_sas(sdtm_dm_sas)
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  if (all(c("USUBJID", "RFSTDTC") %in% names(sdtm_dm))) {
    sdtm_dm <- sdtm_dm[, c("USUBJID", "RFSTDTC")]
  }
}

if (is.null(sdtm_dm)) {
  if ("RFSTDTC" %in% names(edc_dm)) {
    rf_col <- edc_dm$RFSTDTC
    if (inherits(rf_col, "Date")) {
      rf_iso <- format(rf_col, "%Y-%m-%d")
    } else if (inherits(rf_col, "POSIXt")) {
      rf_iso <- format(as.Date(rf_col), "%Y-%m-%d")
    } else {
      rf_char <- as.character(rf_col)
      parsed_iso <- suppressWarnings(as.Date(rf_char, format = "%Y-%m-%d"))
      na_idx <- is.na(parsed_iso) & !is.na(rf_char) & rf_char != ""
      if (any(na_idx)) {
        parsed_iso[na_idx] <- parse_date11(rf_char[na_idx])
      }
      rf_iso <- ifelse(is.na(parsed_iso), "", format(parsed_iso, "%Y-%m-%d"))
      rf_iso[is.na(rf_iso)] <- ""
    }
    sdtm_dm <- data.frame(
      USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
      RFSTDTC = rf_iso,
      stringsAsFactors = FALSE
    )
  } else {
    if (!is.null(edc_ex) && nrow(edc_ex) > 0) {
      date_candidates <- grep("STDTC|STDT|DATE|DT$", names(edc_ex), value = TRUE)
      if ("EXSTDTC" %in% names(edc_ex)) date_candidates <- "EXSTDTC"
      else if ("EXSTDT" %in% names(edc_ex)) date_candidates <- "EXSTDT"
      else if ("STDT" %in% names(edc_ex)) date_candidates <- "STDT"
      if (length(date_candidates) > 0) {
        dc <- date_candidates[1]
        ex_dates <- edc_ex[, c("SUBJID", dc)]
        names(ex_dates) <- c("SUBJID", "DT_RAW")
        if (inherits(ex_dates$DT_RAW, "Date")) {
          parsed <- ex_dates$DT_RAW
        } else if (inherits(ex_dates$DT_RAW, "POSIXt")) {
          parsed <- as.Date(ex_dates$DT_RAW)
        } else {
          dt_raw <- as.character(ex_dates$DT_RAW)
          parsed <- suppressWarnings(as.Date(dt_raw, format = "%Y-%m-%d"))
          na_idx <- is.na(parsed) & !is.na(dt_raw) & dt_raw != ""
          if (any(na_idx)) {
            parsed[na_idx] <- parse_date11(dt_raw[na_idx])
          }
          na_idx2 <- is.na(parsed) & !is.na(dt_raw) & dt_raw != ""
          if (any(na_idx2)) {
            parsed[na_idx2] <- suppressWarnings(as.Date(dt_raw[na_idx2], format = "%m/%d/%Y"))
          }
        }
        ex_dates$PARSED <- parsed
        ex_dates$USUBJID <- paste0("ONCO-2026-", as.character(ex_dates$SUBJID))
        agg <- aggregate(PARSED ~ USUBJID, data = ex_dates, FUN = function(x) min(x, na.rm = TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        agg$RFSTDTC[is.na(agg$RFSTDTC)] <- ""
        sdtm_dm <- agg[, c("USUBJID", "RFSTDTC")]
      }
    }
    if (is.null(sdtm_dm) && file.exists(ivrs_rand_path)) {
      ivrs_rand <- readxl::read_excel(ivrs_rand_path)
      names(ivrs_rand) <- toupper(names(ivrs_rand))
      date_candidates <- grep("RAND|STDTC|DATE|DT$", names(ivrs_rand), value = TRUE)
      if (length(date_candidates) > 0) {
        dc <- date_candidates[1]
        if (!"SUBJID" %in% names(ivrs_rand)) {
          subj_col <- grep("SUBJ", names(ivrs_rand), value = TRUE)[1]
          iv_dates <- ivrs_rand[, c(subj_col, dc)]
          names(iv_dates) <- c("SUBJID", "DT_RAW")
        } else {
          iv_dates <- ivrs_rand[, c("SUBJID", dc)]
          names(iv_dates) <- c("SUBJID", "DT_RAW")
        }
        dt_raw <- as.character(iv_dates$DT_RAW)
        if (inherits(iv_dates$DT_RAW, "Date")) {
          parsed <- iv_dates$DT_RAW
        } else if (inherits(iv_dates$DT_RAW, "POSIXt")) {
          parsed <- as.Date(iv_dates$DT_RAW)
        } else {
          parsed <- suppressWarnings(as.Date(dt_raw, format = "%Y-%m-%d"))
          na_idx <- is.na(parsed) & !is.na(dt_raw) & dt_raw != ""
          if (any(na_idx)) parsed[na_idx] <- parse_date11(dt_raw[na_idx])
        }
        iv_dates$PARSED <- parsed
        iv_dates$USUBJID <- paste0("ONCO-2026-", as.character(iv_dates$SUBJID))
        agg <- aggregate(PARSED ~ USUBJID, data = iv_dates, FUN = function(x) min(x, na.rm = TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        agg$RFSTDTC[is.na(agg$RFSTDTC)] <- ""
        sdtm_dm <- agg[, c("USUBJID", "RFSTDTC")]
      }
    }
    if (is.null(sdtm_dm)) {
      sdtm_dm <- data.frame(
        USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
        RFSTDTC = "",
        stringsAsFactors = FALSE
      )
    }
  }
  sdtm_dm <- sdtm_dm[!duplicated(sdtm_dm$USUBJID), ]
}

sdtm_dm$USUBJID <- as.character(sdtm_dm$USUBJID)
sdtm_dm$RFSTDTC <- as.character(sdtm_dm$RFSTDTC)
sdtm_dm$RFSTDTC[is.na(sdtm_dm$RFSTDTC)] <- ""

for (col in c("STDT", "ENDT")) {
  if (col %in% names(edc_ae)) {
    if (inherits(edc_ae[[col]], "Date")) {
      edc_ae[[col]] <- format(edc_ae[[col]], "%d%b%Y")
    } else if (inherits(edc_ae[[col]], "POSIXt")) {
      edc_ae[[col]] <- format(as.Date(edc_ae[[col]]), "%d%b%Y")
    } else {
      edc_ae[[col]] <- as.character(edc_ae[[col]])
    }
    edc_ae[[col]][is.na(edc_ae[[col]])] <- ""
  } else {
    edc_ae[[col]] <- ""
  }
}

ae_prep <- edc_ae
ae_prep$USUBJID <- paste0("ONCO-2026-", as.character(ae_prep$SUBJID))
ae_prep$AESTDTC <- ""
ae_prep$AEENDTC <- ""
has_stdt <- ae_prep$STDT != "" & !is.na(ae_prep$STDT)
if (any(has_stdt)) {
  parsed_stdt <- parse_date11(ae_prep$STDT[has_stdt])
  ae_prep$AESTDTC[has_stdt] <- format_yymmdd10(parsed_stdt)
}
has_endt <- ae_prep$ENDT != "" & !is.na(ae_prep$ENDT)
if (any(has_endt)) {
  parsed_endt <- parse_date11(ae_prep$ENDT[has_endt])
  ae_prep$AEENDTC[has_endt] <- format_yymmdd10(parsed_endt)
}
for (col in c("TERM", "DECOD", "BODSYS", "SEV", "TXGR", "REL", "OUT", "SER")) {
  if (!col %in% names(ae_prep)) ae_prep[[col]] <- ""
  ae_prep[[col]][is.na(ae_prep[[col]])] <- ""
  ae_prep[[col]] <- as.character(ae_prep[[col]])
}

ae_prep <- ae_prep[order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM, na.last = TRUE), ]
dm_for_ae <- sdtm_dm[order(sdtm_dm$USUBJID), ]
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)
if (!"RFSTDTC" %in% names(sdtm_ae1)) sdtm_ae1$RFSTDTC <- ""
sdtm_ae1$RFSTDTC[is.na(sdtm_ae1$RFSTDTC)] <- ""
sdtm_ae1$STUDYID <- "ONCO-2026"
sdtm_ae1$DOMAIN <- "AE"
sdtm_ae1$AETERM <- sdtm_ae1$TERM
sdtm_ae1$AEDECOD <- toupper(sdtm_ae1$DECOD)
sdtm_ae1$AEBODSYS <- gsub("_", " ", sdtm_ae1$BODSYS, fixed = TRUE)
sdtm_ae1$AESEV <- toupper(sdtm_ae1$SEV)
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))
sdtm_ae1$AEREL <- toupper(sdtm_ae1$REL)
sdtm_ae1$AEOUT <- toupper(sdtm_ae1$OUT)
sdtm_ae1$AESER <- as.character(sdtm_ae1$SER)
sdtm_ae1$AESER[is.na(sdtm_ae1$AESER)] <- ""
sdtm_ae1$AESTDY <- NA_real_
sdtm_ae1$AEENDY <- NA_real_
has_both_start <- sdtm_ae1$AESTDTC != "" & sdtm_ae1$RFSTDTC != "" & !is.na(sdtm_ae1$AESTDTC) & !is.na(sdtm_ae1$RFSTDTC)
if (any(has_both_start)) {
  stdt <- suppressWarnings(as.Date(sdtm_ae1$AESTDTC[has_both_start], format = "%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(sdtm_ae1$RFSTDTC[has_both_start], format = "%Y-%m-%d"))
  diff_days <- as.numeric(stdt - rfdt)
  aestdy <- ifelse(stdt >= rfdt, diff_days + 1, diff_days)
  sdtm_ae1$AESTDY[has_both_start] <- aestdy
}
has_both_end <- sdtm_ae1$AEENDTC != "" & sdtm_ae1$RFSTDTC != "" & !is.na(sdtm_ae1$AEENDTC) & !is.na(sdtm_ae1$RFSTDTC)
if (any(has_both_end)) {
  endt <- suppressWarnings(as.Date(sdtm_ae1$AEENDTC[has_both_end], format = "%Y-%m-%d"))
  rfdt2 <- suppressWarnings(as.Date(sdtm_ae1$RFSTDTC[has_both_end], format = "%Y-%m-%d"))
  diff_days2 <- as.numeric(endt - rfdt2)
  aeendy <- ifelse(endt >= rfdt2, diff_days2 + 1, diff_days2)
  sdtm_ae1$AEENDY[has_both_end] <- aeendy
}
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM, na.last = TRUE), ]
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM), ]
sdtm_ae1$AESEQ <- NA_integer_
usubjids <- unique(sdtm_ae1$USUBJID)
for (uid in usubjids) {
  idx <- which(sdtm_ae1$USUBJID == uid)
  sdtm_ae1$AESEQ[idx] <- seq_along(idx)
}
keep_vars <- c("STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD", "AEBODSYS", "AESEV", "AETXGR", "AEREL", "AEOUT", "AESER", "AESTDTC", "AEENDTC", "AESTDY", "AEENDY")
for (v in keep_vars) {
  if (!v %in% names(sdtm_ae1)) sdtm_ae1[[v]] <- NA
}
sdtm_ae <- sdtm_ae1[, keep_vars]
sdtm_ae$STUDYID <- as.character(sdtm_ae$STUDYID)
sdtm_ae$DOMAIN <- as.character(sdtm_ae$DOMAIN)
sdtm_ae$USUBJID <- as.character(sdtm_ae$USUBJID)
sdtm_ae$AESEQ <- as.integer(sdtm_ae$AESEQ)
sdtm_ae$AETERM <- as.character(sdtm_ae$AETERM)
sdtm_ae$AEDECOD <- as.character(sdtm_ae$AEDECOD)
sdtm_ae$AEBODSYS <- as.character(sdtm_ae$AEBODSYS)
sdtm_ae$AESEV <- as.character(sdtm_ae$AESEV)
sdtm_ae$AETXGR <- as.numeric(sdtm_ae$AETXGR)
sdtm_ae$AEREL <- as.character(sdtm_ae$AEREL)
sdtm_ae$AEOUT <- as.character(sdtm_ae$AEOUT)
sdtm_ae$AESER <- as.character(sdtm_ae$AESER)
sdtm_ae$AESTDTC <- as.character(sdtm_ae$AESTDTC)
sdtm_ae$AEENDTC <- as.character(sdtm_ae$AEENDTC)
sdtm_ae$AESTDY <- as.numeric(sdtm_ae$AESTDY)
sdtm_ae$AEENDY <- as.numeric(sdtm_ae$AEENDY)
sdtm_ae <- sdtm_ae[order(sdtm_ae$USUBJID, sdtm_ae$AESTDTC, sdtm_ae$AETERM), ]

write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

lst_path <- file.path(out, "P022.lst")
txt_path <- file.path(out, "P022.txt")
con <- file(lst_path, open = "wt")
cat("Listing: SDTM AE Dataset\n", file = con)
cat("Study: ONCO-2026  Domain: AE\n", file = con)
cat(paste0("Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\n"), file = con)
cat("\n", file = con)
header <- paste(keep_vars, collapse = " | ")
cat(header, "\n", file = con)
cat(paste(rep("-", nchar(header)), collapse = ""), "\n", file = con)
for (i in seq_len(nrow(sdtm_ae))) {
  row_vals <- sapply(keep_vars, function(v) {
    val <- sdtm_ae[i, v]
    if (is.na(val) || val == "") return("")
    as.character(val)
  })
  line <- paste(row_vals, collapse = " | ")
  cat(line, "\n", file = con)
}
cat("\n", file = con)
cat(paste0("Total observations: ", nrow(sdtm_ae), "\n"), file = con)
cat(paste0("Total subjects: ", length(unique(sdtm_ae$USUBJID)), "\n"), file = con)
close(con)
file.copy(lst_path, txt_path, overwrite = TRUE)
write.csv(sdtm_ae, file.path(out, "P022.csv"), row.names = FALSE, na = "")
ae_txt_path <- file.path(out, "ae.txt")
file.copy(lst_path, ae_txt_path, overwrite = TRUE)
stopifnot(file.exists(file.path(out, "ae.csv")))
stopifnot(file.exists(lst_path))
```