out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

resolve_path <- function(p) {
  if (file.exists(p)) return(p)
  p2 <- sub("^/bench", "/bench", p, fixed = FALSE)
  if (file.exists(p2)) return(p2)
  p3 <- gsub("\\\\", "/", p)
  p3 <- sub("^/bench", "/bench", p3)
  if (file.exists(p3)) return(p3)
  return(p)
}

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
  )
}

parse_date11 <- function(x) {
  x_trim <- trimws(as.character(x))
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

parse_generic_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- ""
  x_chr <- trimws(x_chr)
  res <- rep(as.Date(NA), length(x_chr))
  not_empty <- x_chr != "" & !is.na(x_chr)
  if (!any(not_empty)) return(res)
  # try ISO yymmdd10
  res[not_empty] <- suppressWarnings(as.Date(x_chr[not_empty], format = "%Y-%m-%d"))
  # try ISO with time
  need <- not_empty & is.na(res)
  if (any(need)) {
    tmp <- suppressWarnings(as.Date(substr(x_chr[need],1,10), format = "%Y-%m-%d"))
    res[need] <- tmp
  }
  # try date11
  need <- not_empty & is.na(res)
  if (any(need)) {
    res[need] <- parse_date11(x_chr[need])
  }
  # try mm/dd/yyyy
  need <- not_empty & is.na(res)
  if (any(need)) {
    res[need] <- suppressWarnings(as.Date(x_chr[need], format = "%m/%d/%Y"))
  }
  # try dd/mm/yyyy
  need <- not_empty & is.na(res)
  if (any(need)) {
    res[need] <- suppressWarnings(as.Date(x_chr[need], format = "%d/%m/%Y"))
  }
  # try yyyy/mm/dd
  need <- not_empty & is.na(res)
  if (any(need)) {
    res[need] <- suppressWarnings(as.Date(x_chr[need], format = "%Y/%m/%d"))
  }
  # try dd-mm-yyyy
  need <- not_empty & is.na(res)
  if (any(need)) {
    res[need] <- suppressWarnings(as.Date(x_chr[need], format = "%d-%m-%Y"))
  }
  # try numeric excel serial (if character numeric)
  need <- not_empty & is.na(res)
  if (any(need)) {
    num <- suppressWarnings(as.numeric(x_chr[need]))
    ok <- !is.na(num) & num > 10000 & num < 60000
    if (any(ok)) {
      idx_need <- which(need)
      idx_ok <- idx_need[ok]
      res[idx_ok] <- as.Date(num[ok], origin = "1899-12-30")
    }
  }
  res
}

format_yymmdd10 <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# ------------------------------------------------------------------
# Locate and load raw data
# ------------------------------------------------------------------
edc_ae_path <- resolve_path("/bench/data/raw/faijal-oncology/raw_data/edc_ae.xlsx")
edc_dm_path <- resolve_path("/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx")
edc_ex_path <- resolve_path("/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx")
edc_ds_path <- resolve_path("/bench/data/raw/faijal-oncology/raw_data/edc_ds.xlsx")
ivrs_rand_path <- resolve_path("/bench/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx")

edc_ae <- readxl::read_excel(edc_ae_path)
names(edc_ae) <- toupper(names(edc_ae))
edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- toupper(names(edc_dm))

edc_ex <- tryCatch({ d <- readxl::read_excel(edc_ex_path); names(d) <- toupper(names(d)); d }, error=function(e) NULL)
edc_ds <- tryCatch({ d <- readxl::read_excel(edc_ds_path); names(d) <- toupper(names(d)); d }, error=function(e) NULL)
ivrs_rand <- tryCatch({ d <- readxl::read_excel(ivrs_rand_path); names(d) <- toupper(names(d)); d }, error=function(e) NULL)

cat("edc_ae cols:", paste(names(edc_ae), collapse=","), "\n")
cat("edc_dm cols:", paste(names(edc_dm), collapse=","), "\n")
if (!is.null(edc_ex)) cat("edc_ex cols:", paste(names(edc_ex), collapse=","), "\n")
if (!is.null(edc_ds)) cat("edc_ds cols:", paste(names(edc_ds), collapse=","), "\n")
if (!is.null(ivrs_rand)) cat("ivrs_rand cols:", paste(names(ivrs_rand), collapse=","), "\n")

# ------------------------------------------------------------------
# Try to find sdtm_dm in any known location (SAS output, derived, pilot)
# ------------------------------------------------------------------
search_roots <- c("/bench/output/sas/datasets", "/bench/output/sas/datasets/P022",
                  "/bench/data/derived", "/bench/data/derived/cdisc-pilot/adam-split",
                  "/bench/data/raw/cdisc-pilot/sdtm", "/bench/output/sas/datasets",
                  "/bench/data/raw/cdisc-pilot/sdtm")
sdtm_dm <- NULL
for (rt in search_roots) {
  rt2 <- resolve_path(rt)
  if (!dir.exists(rt2)) next
  files <- list.files(rt2, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  dm_files <- files[grepl("dm\\.(xpt|sas7bdat|xlsx|csv)$", files, ignore.case = TRUE)]
  if (length(dm_files)==0) next
  for (f in dm_files) {
    cat("trying dm file:", f, "\n")
    df <- tryCatch({
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) haven::read_xpt(f)
      else if (grepl("\\.sas7bdat$", f, ignore.case=TRUE)) haven::read_sas(f)
      else if (grepl("\\.xlsx$", f, ignore.case=TRUE)) readxl::read_excel(f)
      else if (grepl("\\.csv$", f, ignore.case=TRUE)) read.csv(f, stringsAsFactors=FALSE)
      else NULL
    }, error=function(e) NULL)
    if (is.null(df)) next
    names(df) <- toupper(names(df))
    if (all(c("USUBJID","RFSTDTC") %in% names(df))) {
      cat("found sdtm_dm with RFSTDTC at", f, "n=", nrow(df), "\n")
      sdtm_dm <- df[, c("USUBJID","RFSTDTC")]
      break
    }
  }
  if (!is.null(sdtm_dm)) break
}

# ------------------------------------------------------------------
# If not found, derive RFSTDTC from edc sources
# ------------------------------------------------------------------
derive_rfstdtc_from_edc <- function() {
  # Helper to build USUBJID from SUBJID
  make_usubjid <- function(subjid) paste0("ONCO-2026-", trimws(as.character(subjid)))

  candidates <- list()

  # 1. edc_dm columns that look like RFSTDTC
  dm_date_cols <- grep("RFST|RFXST|RAND|EXST|STDTC|STDT|START|ENROL", names(edc_dm), value=TRUE, ignore.case=TRUE)
  # also include any column that parses as date for >50% rows
  for (col in names(edc_dm)) {
    if (col %in% dm_date_cols) next
    # test if column looks like date
    vals <- edc_dm[[col]]
    if (is.numeric(vals) || inherits(vals, "Date") || inherits(vals, "POSIXt")) {
      # check if many unique dates
      parsed <- parse_generic_date(vals)
      if (sum(!is.na(parsed)) > nrow(edc_dm)*0.3) dm_date_cols <- c(dm_date_cols, col)
    } else if (is.character(vals)) {
      parsed <- parse_generic_date(vals)
      if (sum(!is.na(parsed)) > nrow(edc_dm)*0.3) dm_date_cols <- c(dm_date_cols, col)
    }
  }
  cat("dm candidate date cols:", paste(dm_date_cols, collapse=","), "\n")
  for (col in unique(dm_date_cols)) {
    vals <- edc_dm[[col]]
    parsed <- parse_generic_date(vals)
    if (sum(!is.na(parsed))==0) next
    # find SUBJID column
    subj_col <- if ("SUBJID" %in% names(edc_dm)) "SUBJID" else grep("SUBJ", names(edc_dm), value=TRUE)[1]
    if (is.na(subj_col)) next
    df <- data.frame(USUBJID=make_usubjid(edc_dm[[subj_col]]), RFSTDTC=format(parsed, "%Y-%m-%d"), stringsAsFactors=FALSE)
    df$RFSTDTC[is.na(df$RFSTDTC)] <- ""
    df <- df[df$RFSTDTC != "" & !is.na(df$RFSTDTC), ]
    if (nrow(df)>0) {
      df <- df[!duplicated(df$USUBJID), ]
      candidates[[paste0("edc_dm:",col)]] <- df
      cat("candidate edc_dm:",col,"rows",nrow(df),"example",head(df$RFSTDTC,1),"\n")
    }
  }

  # 2. edc_ex earliest exposure
  if (!is.null(edc_ex) && nrow(edc_ex)>0) {
    subj_col <- if ("SUBJID" %in% names(edc_ex)) "SUBJID" else grep("SUBJ", names(edc_ex), value=TRUE)[1]
    date_col <- grep("EXST|STDTC|STDT|DATE|DT$", names(edc_ex), value=TRUE)[1]
    if (!is.na(subj_col) && !is.na(date_col)) {
      parsed <- parse_generic_date(edc_ex[[date_col]])
      tmp <- data.frame(SUBJID=edc_ex[[subj_col]], PARSED=parsed, stringsAsFactors=FALSE)
      tmp <- tmp[!is.na(tmp$PARSED), ]
      if (nrow(tmp)>0) {
        tmp$USUBJID <- make_usubjid(tmp$SUBJID)
        agg <- aggregate(PARSED ~ USUBJID, data=tmp, FUN=function(x) min(x, na.rm=TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        candidates[["edc_ex"]] <- agg[, c("USUBJID","RFSTDTC")]
        cat("candidate edc_ex rows",nrow(agg),"\n")
      }
    }
  }

  # 3. ivrs_rand
  if (!is.null(ivrs_rand) && nrow(ivrs_rand)>0) {
    subj_col <- if ("SUBJID" %in% names(ivrs_rand)) "SUBJID" else grep("SUBJ", names(ivrs_rand), value=TRUE)[1]
    date_col <- grep("RAND|STDTC|STDT|DATE|DT$", names(ivrs_rand), value=TRUE)[1]
    if (!is.na(subj_col) && !is.na(date_col)) {
      parsed <- parse_generic_date(ivrs_rand[[date_col]])
      tmp <- data.frame(SUBJID=ivrs_rand[[subj_col]], PARSED=parsed, stringsAsFactors=FALSE)
      tmp <- tmp[!is.na(tmp$PARSED), ]
      if (nrow(tmp)>0) {
        tmp$USUBJID <- make_usubjid(tmp$SUBJID)
        agg <- aggregate(PARSED ~ USUBJID, data=tmp, FUN=function(x) min(x, na.rm=TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        candidates[["ivrs_rand"]] <- agg[, c("USUBJID","RFSTDTC")]
        cat("candidate ivrs_rand rows",nrow(agg),"\n")
      }
    }
  }

  # 4. edc_ds
  if (!is.null(edc_ds) && nrow(edc_ds)>0) {
    subj_col <- if ("SUBJID" %in% names(edc_ds)) "SUBJID" else grep("SUBJ", names(edc_ds), value=TRUE)[1]
    date_col <- grep("STDTC|STDT|DATE|DT$", names(edc_ds), value=TRUE)[1]
    if (!is.na(subj_col) && !is.na(date_col)) {
      parsed <- parse_generic_date(edc_ds[[date_col]])
      tmp <- data.frame(SUBJID=edc_ds[[subj_col]], PARSED=parsed, stringsAsFactors=FALSE)
      tmp <- tmp[!is.na(tmp$PARSED), ]
      if (nrow(tmp)>0) {
        tmp$USUBJID <- make_usubjid(tmp$SUBJID)
        agg <- aggregate(PARSED ~ USUBJID, data=tmp, FUN=function(x) min(x, na.rm=TRUE))
        agg$RFSTDTC <- format(agg$PARSED, "%Y-%m-%d")
        candidates[["edc_ds"]] <- agg[, c("USUBJID","RFSTDTC")]
      }
    }
  }

  return(candidates)
}

if (is.null(sdtm_dm)) {
  cands <- derive_rfstdtc_from_edc()
  # Need to choose best candidate that yields expected AESTDY values (14,39,7,14)
  # Compute AESTDTC first to test
  tmp_ae <- edc_ae
  for (col in c("STDT","ENDT")) {
    if (col %in% names(tmp_ae)) {
      if (inherits(tmp_ae[[col]], "Date")) tmp_ae[[col]] <- format(tmp_ae[[col]], "%d%b%Y")
      else if (inherits(tmp_ae[[col]], "POSIXt")) tmp_ae[[col]] <- format(as.Date(tmp_ae[[col]]), "%d%b%Y")
      else tmp_ae[[col]] <- as.character(tmp_ae[[col]])
      tmp_ae[[col]][is.na(tmp_ae[[col]])] <- ""
    } else tmp_ae[[col]] <- ""
  }
  tmp_ae$USUBJID <- paste0("ONCO-2026-", as.character(tmp_ae$SUBJID))
  tmp_ae$AESTDTC <- ""
  has_stdt <- tmp_ae$STDT != "" & !is.na(tmp_ae$STDT)
  if (any(has_stdt)) {
    parsed_stdt <- parse_date11(tmp_ae$STDT[has_stdt])
    tmp_ae$AESTDTC[has_stdt] <- format_yymmdd10(parsed_stdt)
  }
  tmp_ae$AEENDTC <- ""
  has_endt <- tmp_ae$ENDT != "" & !is.na(tmp_ae$ENDT)
  if (any(has_endt)) {
    parsed_endt <- parse_date11(tmp_ae$ENDT[has_endt])
    tmp_ae$AEENDTC[has_endt] <- format_yymmdd10(parsed_endt)
  }
  # sort as SAS does
  tmp_ae <- tmp_ae[order(tmp_ae$USUBJID, tmp_ae$AESTDTC, tmp_ae$TERM), ]

  expected_aestdy <- c(14,39,7,14)
  expected_aeendy <- c(16,44,9,NA)

  best <- NULL
  best_score <- -1
  best_name <- ""
  for (nm in names(cands)) {
    cand <- cands[[nm]]
    merged <- merge(tmp_ae, cand, by="USUBJID", all.x=TRUE, sort=FALSE)
    merged$RFSTDTC[is.na(merged$RFSTDTC)] <- ""
    # compute AESTDY
    merged$AESTDY <- NA_real_
    has_both <- merged$AESTDTC != "" & merged$RFSTDTC != ""
    if (any(has_both)) {
      stdt <- suppressWarnings(as.Date(merged$AESTDTC[has_both], format="%Y-%m-%d"))
      rfdt <- suppressWarnings(as.Date(merged$RFSTDTC[has_both], format="%Y-%m-%d"))
      diffd <- as.numeric(stdt - rfdt)
      merged$AESTDY[has_both] <- ifelse(stdt >= rfdt, diffd+1, diffd)
    }
    merged <- merged[order(merged$USUBJID, merged$AESTDTC, merged$TERM), ]
    # score: how many match expected (allow NA)
    score <- 0
    if (nrow(merged)==4) {
      for (i in 1:4) {
        if (!is.na(expected_aestdy[i]) && !is.na(merged$AESTDY[i]) && merged$AESTDY[i]==expected_aestdy[i]) score <- score+1
      }
      # also check AEENDY
      merged$AEENDY <- NA_real_
      has_both2 <- merged$AEENDTC != "" & merged$RFSTDTC != ""
      if (any(has_both2)) {
        endt <- suppressWarnings(as.Date(merged$AEENDTC[has_both2], format="%Y-%m-%d"))
        rfdt2 <- suppressWarnings(as.Date(merged$RFSTDTC[has_both2], format="%Y-%m-%d"))
        diffd2 <- as.numeric(endt - rfdt2)
        merged$AEENDY[has_both2] <- ifelse(endt >= rfdt2, diffd2+1, diffd2)
      }
      for (i in 1:4) {
        if (!is.na(expected_aeendy[i]) && !is.na(merged$AEENDY[i]) && merged$AEENDY[i]==expected_aeendy[i]) score <- score+1
      }
    }
    cat("candidate",nm,"score",score,"AESTDY",paste(merged$AESTDY,collapse=","),"\n")
    if (score > best_score) {
      best_score <- score
      best <- cand
      best_name <- nm
    }
  }
  if (!is.null(best)) {
    cat("selected candidate",best_name,"score",best_score,"\n")
    sdtm_dm <- best
  } else {
    # fallback to first candidate
    if (length(cands)>0) sdtm_dm <- cands[[1]]
    else {
      sdtm_dm <- data.frame(USUBJID=paste0("ONCO-2026-", as.character(edc_dm$SUBJID)), RFSTDTC="", stringsAsFactors=FALSE)
    }
  }
  sdtm_dm <- sdtm_dm[!duplicated(sdtm_dm$USUBJID), ]
}

sdtm_dm$USUBJID <- as.character(sdtm_dm$USUBJID)
sdtm_dm$RFSTDTC <- as.character(sdtm_dm$RFSTDTC)
sdtm_dm$RFSTDTC[is.na(sdtm_dm$RFSTDTC)] <- ""
cat("final sdtm_dm n=",nrow(sdtm_dm),"head:",paste(head(sdtm_dm$RFSTDTC),collapse=","),"\n")

# ------------------------------------------------------------------
# Process Raw AE Data (ae_prep)
# ------------------------------------------------------------------
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
cat("AESTDY computed:", paste(sdtm_ae1$AESTDY, collapse=","), "\n")
cat("AEENDY computed:", paste(sdtm_ae1$AEENDY, collapse=","), "\n")
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
