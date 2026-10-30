```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
f_tr   <- file.path(raw_base, "edc_tr.xlsx")
f_dm   <- file.path(raw_base, "edc_dm.xlsx")
f_ex   <- file.path(raw_base, "edc_ex.xlsx")
f_rand <- file.path(raw_base, "ivrs_rand.xlsx")

# ---------- helpers ----------
pick_col <- function(df, candidates) {
  nm <- toupper(names(df))
  cand <- toupper(candidates)
  for (c in cand) {
    w <- which(nm == c)
    if (length(w) > 0) return(names(df)[w[1]])
  }
  return(NULL)
}
find_date_col <- function(df, prefer = NULL) {
  if (!is.null(prefer)) {
    c0 <- pick_col(df, prefer)
    if (!is.null(c0)) return(c0)
  }
  nm <- toupper(names(df))
  idx <- grep("DATE|DTC|_DT$|^DT$|DT$", nm)
  if (length(idx) == 0) idx <- grep("DT", nm)
  if (length(idx) == 0) return(NULL)
  # prefer containing ST (start)
  st <- idx[grepl("ST", nm[idx])]
  if (length(st) > 0) return(names(df)[st[1]])
  return(names(df)[idx[1]])
}
to_Date_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  # character / factor
  xc <- trimws(as.character(x))
  xc[xc == "" | xc == "." | toupper(xc) == "NA" | toupper(xc) == "NULL" | toupper(xc) == "MISSING"] <- NA
  outd <- rep(as.Date(NA), length(xc))
  need <- which(!is.na(xc))
  if (length(need) == 0) return(outd)
  vals <- xc[need]
  # decide ONE format for whole column
  is_ymd <- grepl("^\\d{4}-\\d{2}-\\d{2}", vals)
  is_dmy_dashmon <- grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}", vals)
  is_slash <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", vals)
  is_ymd_slash <- grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}", vals)
  if (all(is_ymd, na.rm = TRUE) && length(vals) > 0 && all(is_ymd)) {
    outd[need] <- as.Date(substr(vals, 1, 10), format = "%Y-%m-%d")
  } else if (all(is_dmy_dashmon)) {
    norm <- sapply(vals, function(s) {
      pp <- strsplit(s, "-")[[1]]
      if (length(pp) < 3) return(s)
      mon <- pp[2]
      mon_t <- paste0(toupper(substr(mon, 1, 1)), tolower(substr(mon, 2, 3)))
      paste0(pp[1], "-", mon_t, "-", pp[3])
    }, USE.NAMES = FALSE)
    outd[need] <- as.Date(norm, format = "%d-%b-%Y")
  } else if (all(is_slash)) {
    firstfield <- suppressWarnings(as.integer(sub("/.*$", "", vals)))
    if (any(firstfield > 12, na.rm = TRUE)) {
      outd[need] <- as.Date(vals, format = "%d/%m/%Y")
    } else {
      outd[need] <- as.Date(vals, format = "%m/%d/%Y")
    }
  } else if (all(is_ymd_slash)) {
    outd[need] <- as.Date(vals, format = "%Y/%m/%d")
  } else {
    # fallback: try ymd first 10 chars, then date11
    d1 <- suppressWarnings(as.Date(substr(vals, 1, 10), format = "%Y-%m-%d"))
    miss <- is.na(d1)
    if (any(miss)) {
      norm <- sapply(vals[miss], function(s) {
        pp <- strsplit(s, "-")[[1]]
        if (length(pp) < 3) return(s)
        mon <- pp[2]
        mon_t <- paste0(toupper(substr(mon, 1, 1)), tolower(substr(mon, 2, 3)))
        paste0(pp[1], "-", mon_t, "-", pp[3])
      }, USE.NAMES = FALSE)
      d2 <- suppressWarnings(as.Date(norm, format = "%d-%b-%Y"))
      d1[miss] <- d2
    }
    outd[need] <- d1
  }
  outd
}
parse_date11_strict <- function(x) {
  xc <- trimws(as.character(x))
  outd <- rep(as.Date(NA), length(xc))
  blank <- is.na(xc) | xc == ""
  idx <- which(!blank)
  if (length(idx) == 0) return(outd)
  vals <- xc[idx]
  norm <- sapply(vals, function(s) {
    pp <- strsplit(s, "-")[[1]]
    if (length(pp) != 3) return(s)
    mon <- pp[2]
    if (nchar(mon) != 3) return(s)
    mon_t <- paste0(toupper(substr(mon, 1, 1)), tolower(substr(mon, 2, 3)))
    paste0(trimws(pp[1]), "-", mon_t, "-", trimws(pp[3]))
  }, USE.NAMES = FALSE)
  outd[idx] <- suppressWarnings(as.Date(norm, format = "%d-%b-%Y"))
  outd
}
subj_to_usubjid <- function(v) {
  vc <- trimws(as.character(v))
  res <- rep(NA_character_, length(vc))
  ok <- !is.na(vc) & vc != ""
  has_prefix <- ok & grepl("^ONCO-2026-", vc)
  res[has_prefix] <- vc[has_prefix]
  need <- ok & !has_prefix
  res[need] <- paste0("ONCO-2026-", vc[need])
  res
}

# ---------- read edc_tr ----------
tr_raw <- as.data.frame(readxl::read_excel(f_tr), stringsAsFactors = FALSE, check.names = FALSE)
names(tr_raw) <- toupper(names(tr_raw))
c_subj  <- pick_col(tr_raw, c("SUBJID","SUBJECTID","SUBJECT","PTID","PATID","USUBJID"))
c_lnk   <- pick_col(tr_raw, c("LNKID","LINKID","TRLNKID","LNK_ID"))
if (is.null(c_lnk)) { w <- grep("LNK|LINK", toupper(names(tr_raw))); if (length(w)>0) c_lnk <- names(tr_raw)[w[1]] }
c_test  <- pick_col(tr_raw, c("TESTCD","TRTESTCD","TEST","TEST_CODE"))
if (is.null(c_test)) { w <- grep("TESTCD", toupper(names(tr_raw))); if (length(w)>0) c_test <- names(tr_raw)[w[1]] }
c_orres <- pick_col(tr_raw, c("ORRES","TRORRES","RESULT","OR_RES"))
if (is.null(c_orres)) { w <- grep("ORRES", toupper(names(tr_raw))); if (length(w)>0) c_orres <- names(tr_raw)[w[1]] }
c_unit  <- pick_col(tr_raw, c("UNIT","UNITS","ORRESU","TRORRESU"))
if (is.null(c_unit)) { w <- grep("UNIT", toupper(names(tr_raw))); if (length(w)>0) c_unit <- names(tr_raw)[w[1]] }
c_visit <- pick_col(tr_raw, c("VISIT","VISITNAME","VISIT_NM","AVISIT","VISIT_N"))
if (is.null(c_visit)) { w <- grep("VISIT", toupper(names(tr_raw))); if (length(w)>0) c_visit <- names(tr_raw)[w[1]] }
c_trdt  <- pick_col(tr_raw, c("TRDT","TRDTC","TRDAT","TR_DT"))
if (is.null(c_trdt)) { w <- grep("^TR.*DT|^TR.*DATE|TRDTC", toupper(names(tr_raw))); if (length(w)>0) c_trdt <- names(tr_raw)[w[1]] }
if (is.null(c_trdt)) c_trdt <- find_date_col(tr_raw)
stopifnot(!is.null(c_subj), !is.null(c_lnk), !is.null(c_test), !is.null(c_orres), !is.null(c_unit), !is.null(c_visit), !is.null(c_trdt))

# ---------- build dm_for_tr (RFSTDTC) ----------
dm_raw <- as.data.frame(readxl::read_excel(f_dm), stringsAsFactors = FALSE, check.names = FALSE)
names(dm_raw) <- toupper(names(dm_raw))
dm_subj_col <- pick_col(dm_raw, c("SUBJID","SUBJECTID","SUBJECT","PTID","USUBJID"))
dm_usub_col <- NULL
if ("USUBJID" %in% toupper(names(dm_raw))) dm_usub_col <- names(dm_raw)[which(toupper(names(dm_raw))=="USUBJID")[1]]
rf_col <- pick_col(dm_raw, c("RFSTDTC","RFSTDT","RFSTDAT","RFSTART","RFST_DTC"))

rf_map <- NULL
if (!is.null(rf_col)) {
  dm_usub <- if (!is.null(dm_usub_col)) trimws(as.character(dm_raw[[dm_usub_col]])) else subj_to_usubjid(dm_raw[[dm_subj_col]])
  rf_dates <- to_Date_vec(dm_raw[[rf_col]])
  rf_str <- ifelse(is.na(rf_dates), NA_character_, format(rf_dates, "%Y-%m-%d"))
  rf_map <- data.frame(USUBJID = dm_usub, RFSTDTC = rf_str, stringsAsFactors = FALSE)
  rf_map <- rf_map[!is.na(rf_map$USUBJID) & rf_map$USUBJID != "", , drop = FALSE]
  rf_map <- rf_map[!duplicated(rf_map$USUBJID), , drop = FALSE]
} else {
  # derive from EX and RAND
  ex_raw <- as.data.frame(readxl::read_excel(f_ex), stringsAsFactors = FALSE, check.names = FALSE)
  names(ex_raw) <- toupper(names(ex_raw))
  ex_subj_col <- pick_col(ex_raw, c("SUBJID","SUBJECTID","SUBJECT","PTID","USUBJID"))
  if (is.null(ex_subj_col)) { w <- grep("SUBJ", toupper(names(ex_raw))); if (length(w)>0) ex_subj_col <- names(ex_raw)[w[1]] }
  ex_date_col <- find_date_col(ex_raw, prefer = c("EXSTDTC","EXSTDT","EXSTDAT","EXST_D","EXSTART","EXDT","EXDATE","EXDTC","STDTC","STARTDT"))
  rand_raw <- as.data.frame(readxl::read_excel(f_rand), stringsAsFactors = FALSE, check.names = FALSE)
  names(rand_raw) <- toupper(names(rand_raw))
  rand_subj_col <- pick_col(rand_raw, c("SUBJID","SUBJECTID","SUBJECT","PTID","USUBJID"))
  if (is.null(rand_subj_col)) { w <- grep("SUBJ", toupper(names(rand_raw))); if (length(w)>0) rand_subj_col <- names(rand_raw)[w[1]] }
  rand_date_col <- find_date_col(rand_raw, prefer = c("RANDDT","RANDDTC","RANDDATE","RANDOMDT","RAND_DT","RNDDT","RANDD","RANDTC"))
  # also check dm date cols as fallback
  dm_date_col <- find_date_col(dm_raw)
  ex_min <- NULL; rand_min <- NULL
  if (!is.null(ex_subj_col) && !is.null(ex_date_col)) {
    ex_usub <- if ("USUBJID" %in% toupper(names(ex_raw))) trimws(as.character(ex_raw[["USUBJID"]])) else subj_to_usubjid(ex_raw[[ex_subj_col]])
    # USUBJID column name may differ in case; handle
    if (!("USUBJID" %in% toupper(names(ex_raw)))) { } else {
      uc <- names(ex_raw)[which(toupper(names(ex_raw))=="USUBJID")[1]]
      ex_usub <- trimws(as.character(ex_raw[[uc]]))
      # if USUBJID looks like bare subjid without prefix, add prefix
      needp <- !is.na(ex_usub) & ex_usub != "" & !grepl("^ONCO-2026-", ex_usub)
      # check if values look like ONCO? if not, treat as subjid
      # only prefix if ex_subj_col is USUBJID and values don't have prefix but look numeric-ish -> prefix
      ex_usub[needp] <- paste0("ONCO-2026-", ex_usub[needp])
    }
    ex_dt <- to_Date_vec(ex_raw[[ex_date_col]])
    edf <- data.frame(USUBJID = ex_usub, DT = ex_dt, stringsAsFactors = FALSE)
    edf <- edf[!is.na(edf$USUBJID) & edf$USUBJID != "" & !is.na(edf$DT), , drop = FALSE]
    if (nrow(edf) > 0) {
      ex_min <- aggregate(DT ~ USUBJID, data = edf, FUN = min)
      names(ex_min)[2] <- "EXDT"
    }
  }
  if (!is.null(rand_subj_col) && !is.null(rand_date_col)) {
    uc_r <- which(toupper(names(rand_raw))=="USUBJID")
    if (length(uc_r) > 0) {
      rand_usub <- trimws(as.character(rand_raw[[uc_r[1]]]))
      needp <- !is.na(rand_usub) & rand_usub != "" & !grepl("^ONCO-2026-", rand_usub)
      rand_usub[needp] <- paste0("ONCO-2026-", rand_usub[needp])
    } else {
      rand_usub <- subj_to_usubjid(rand_raw[[rand_subj_col]])
    }
    rand_dt <- to_Date_vec(rand_raw[[rand_date_col]])
    rdf <- data.frame(USUBJID = rand_usub, DT = rand_dt, stringsAsFactors = FALSE)
    rdf <- rdf[!is.na(rdf$USUBJID) & rdf$USUBJID != "" & !is.na(rdf$DT), , drop = FALSE]
    if (nrow(rdf) > 0) {
      rand_min <- aggregate(DT ~ USUBJID, data = rdf, FUN = min)
      names(rand_min)[2] <- "RANDDT"
    }
  }
  # base subjects from DM (or union)
  if (!is.null(dm_subj_col) || !is.null(dm_usub_col)) {
    if (!is.null(dm_usub_col)) base_usub <- trimws(as.character(dm_raw[[dm_usub_col]]))
    else base_usub <- subj_to_usubjid(dm_raw[[dm_subj_col]])
    base <- data.frame(USUBJID = unique(base_usub[!is.na(base_usub) & base_usub != ""]), stringsAsFactors = FALSE)
  } else {
    base <- data.frame(USUBJID = character(0), stringsAsFactors = FALSE)
  }
  # union with ex/rand/tr subjects to not lose TR subjects
  tr_subj_vals <- trimws(as.character(tr_raw[[c_subj]]))
  tr_usub_all <- unique(subj_to_usubjid(tr_subj_vals))
  all_usub <- unique(c(base$USUBJID,
                       if (!is.null(ex_min)) ex_min$USUBJID else NULL,
                       if (!is.null(rand_min)) rand_min$USUBJID else NULL,
                       tr_usub_all))
  all_usub <- all_usub[!is.na(all_usub) & all_usub != ""]
  base <- data.frame(USUBJID = sort(all_usub), stringsAsFactors = FALSE)
  if (!is.null(ex_min)) base <- merge(base, ex_min, by = "USUBJID", all.x = TRUE, sort = FALSE) else base$EXDT <- as.Date(NA)
  if (!is.null(rand_min)) base <- merge(base, rand_min, by = "USUBJID", all.x = TRUE, sort = FALSE) else base$RANDDT <- as.Date(NA)
  # dm fallback date
  if (!is.null(dm_date_col) && (!is.null(dm_subj_col) || !is.null(dm_usub_col))) {
    if (!is.null(dm_usub_col)) dm_u2 <- trimws(as.character(dm_raw[[dm_usub_col]])) else dm_u2 <- subj_to_usubjid(dm_raw[[dm_subj_col]])
    dm_d2 <- to_Date_vec(dm_raw[[dm_date_col]])
    ddf <- data.frame(USUBJID = dm_u2, DMDT = dm_d2, stringsAsFactors = FALSE)
    ddf <- ddf[!is.na(ddf$USUBJID) & ddf$USUBJID != "" & !is.na(ddf$DMDT), , drop = FALSE]
    if (nrow(ddf) > 0) {
      dm_min <- aggregate(DMDT ~ USUBJID, data = ddf, FUN = min)
      base <- merge(base, dm_min, by = "USUBJID", all.x = TRUE, sort = FALSE)
    } else base$DMDT <- as.Date(NA)
  } else base$DMDT <- as.Date(NA)
  # RFSTDTC = EXDT else RANDDT else DMDT (first exposure priority)
  base$RFDT <- base$EXDT
  miss <- is.na(base$RFDT)
  base$RFDT[miss] <- base$RANDDT[miss]
  miss2 <- is.na(base$RFDT)
  base$RFDT[miss2] <- base$DMDT[miss2]
  base$RFSTDTC <- ifelse(is.na(base$RFDT), NA_character_, format(base$RFDT, "%Y-%m-%d"))
  rf_map <- base[, c("USUBJID","RFSTDTC"), drop = FALSE]
}
# SAS sort dm_for_tr by USUBJID
rf_map <- rf_map[order(rf_map$USUBJID), , drop = FALSE]

# ---------- 2. tr_prep ----------
tr_prep <- data.frame(
  SUBJ_raw = tr_raw[[c_subj]],
  LNKID = as.character(tr_raw[[c_lnk]]),
  TESTCD = as.character(tr_raw[[c_test]]),
  ORRES = as.character(tr_raw[[c_orres]]),
  UNIT = as.character(tr_raw[[c_unit]]),
  VISIT_raw = as.character(tr_raw[[c_visit]]),
  TRDT_raw = as.character(tr_raw[[c_trdt]]),
  stringsAsFactors = FALSE
)
# USUBJID = catx("-", "ONCO-2026", SUBJID)
tr_prep$USUBJID <- subj_to_usubjid(tr_prep$SUBJ_raw)
# sort by USUBJID (SAS character order, missing first)
tr_prep <- tr_prep[order(ifelse(is.na(tr_prep$USUBJID), "", tr_prep$USUBJID)), , drop = FALSE]

# ---------- 3. Map TR domain (merge) ----------
# left join tr_prep with rf_map by USUBJID, keep all tr (if t)
m <- merge(tr_prep, rf_map, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot("USUBJID" %in% names(m), "RFSTDTC" %in% names(m))

sdtm_tr1 <- m
sdtm_tr1$STUDYID <- "ONCO-2026"
sdtm_tr1$DOMAIN <- "TR"
sdtm_tr1$TRLNKID <- substr(trimws(as.character(sdtm_tr1$LNKID)), 1, 5)
sdtm_tr1$TRLNKID[trimws(as.character(sdtm_tr1$LNKID)) == "" | is.na(sdtm_tr1$LNKID)] <- NA_character_
sdtm_tr1$TRTESTCD <- substr(trimws(as.character(sdtm_tr1$TESTCD)), 1, 8)
sdtm_tr1$TRTEST <- ifelse(!is.na(sdtm_tr1$TRTESTCD) & sdtm_tr1$TRTESTCD == "LDIAM", "Longest Diameter", NA_character_)
sdtm_tr1$TRORRES <- substr(as.character(sdtm_tr1$ORRES), 1, 10)
sdtm_tr1$TRORRES[is.na(sdtm_tr1$ORRES)] <- NA_character_
# keep blank as NA? SAS blank -> missing; preserve "" as NA for csv na=""
sdtm_tr1$TRORRES[trimws(sdtm_tr1$TRORRES) == ""] <- NA_character_
sdtm_tr1$TRORRESU <- substr(as.character(sdtm_tr1$UNIT), 1, 10)
sdtm_tr1$TRORRESU[is.na(sdtm_tr1$UNIT)] <- NA_character_
sdtm_tr1$TRORRESU[trimws(sdtm_tr1$TRORRESU) == ""] <- NA_character_
sdtm_tr1$TRSTRESC <- sdtm_tr1$TRORRES
sdtm_tr1$TRSTRESN <- suppressWarnings(as.numeric(trimws(as.character(sdtm_tr1$ORRES))))
sdtm_tr1$TRSTRESU <- sdtm_tr1$TRORRESU
sdtm_tr1$TREVAL <- "INVESTIGATOR"
vv <- as.character(sdtm_tr1$VISIT_raw)
sdtm_tr1$VISIT <- toupper(gsub("_", " ", vv, fixed = TRUE))
sdtm_tr1$VISIT[is.na(vv)] <- NA_character_
sdtm_tr1$VISIT[trimws(as.character(vv)) == "" & !is.na(vv)] <- NA_character_
sdtm_tr1$VISIT <- substr(sdtm_tr1$VISIT, 1, 20)
# Date conversion: if TRDT ne "" then TRDTC = put(input(TRDT,date11.),yymmdd10.)
trdt_char <- trimws(as.character(sdtm_tr1$TRDT_raw))
trdt_char[is.na(sdtm_tr1$TRDT_raw)] <- NA_character_
parsed_tr <- parse_date11_strict(trdt_char)
sdtm_tr1$TRDTC <- ifelse(is.na(parsed_tr), NA_character_, format(parsed_tr, "%Y-%m-%d"))
# Study Day
rf_parsed <- suppressWarnings(as.Date(sdtm_tr1$RFSTDTC, format = "%Y-%m-%d"))
tr_parsed <- suppressWarnings(as.Date(sdtm_tr1$TRDTC, format = "%Y-%m-%d"))
diffd <- as.integer(tr_parsed - rf_parsed)
sdtm_tr1$TRSTDY <- ifelse(!is.na(tr_parsed) & !is.na(rf_parsed),
                          ifelse(tr_parsed >= rf_parsed, diffd + 1L, diffd),
                          NA_integer_)
# keep needed
sdtm_tr1 <- sdtm_tr1[, c("STUDYID","DOMAIN","USUBJID","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT")]

# ---------- 4. Derive TRSEQ (sort by USUBJID TRDTC, missing first) ----------
ord <- order(ifelse(is.na(sdtm_tr1$USUBJID), "", sdtm_tr1$USUBJID),
             ifelse(is.na(sdtm_tr1$TRDTC), "", sdtm_tr1$TRDTC))
sdtm_tr1 <- sdtm_tr1[ord, , drop = FALSE]
sdtm_tr <- sdtm_tr1
sdtm_tr$TRSEQ <- NA_integer_
# by USUBJID retain
us <- sdtm_tr$USUBJID
cur <- 0L
prev <- NA_character_
for (i in seq_len(nrow(sdtm_tr))) {
  u <- us[i]
  first <- is.na(prev) || is.na(u) || is.na(prev) || u != prev
  # handle NA USUBJID: each NA group? SAS first.USUBJID with missing: all missings are one group
  if (i == 1) first <- TRUE else {
    p <- us[i-1]
    if (is.na(u) && is.na(p)) first <- FALSE else if (is.na(u) || is.na(p)) first <- TRUE else first <- (u != p)
  }
  if (first) cur <- 1L else cur <- cur + 1L
  sdtm_tr$TRSEQ[i] <- cur
}
sdtm_tr <- sdtm_tr[, c("STUDYID","DOMAIN","USUBJID","TRSEQ","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT")]

# ---------- 5/6. outputs ----------
write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

# PROC PRINT listing: Obs + all columns
print_df <- cbind(Obs = seq_len(nrow(sdtm_tr)), sdtm_tr)
write.csv(print_df, file.path(out, "P028.csv"), row.names = FALSE, na = "")

# text rendering
txt_path <- file.path(out, "P028.txt")
con <- file(txt_path, open = "wt")
writeLines("PROC PRINT DATA=SDTM_TR", con)
writeLines("", con)
capt <- capture.output(print.data.frame(print_df, row.names = FALSE, right = FALSE, na = ""))
writeLines(capt, con)
close(con)
```