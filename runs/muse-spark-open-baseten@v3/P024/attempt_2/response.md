```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(readxl)
library(dplyr)
library(stringr)
library(haven)

`%||%` <- function(a,b) if (!is.null(a) && length(a)==1 && !is.na(a) && nzchar(trimws(as.character(a)))) a else b
tranwrd <- function(str, from, to) {
  if (is.na(str)) return(NA_character_)
  gsub(from, to, str, fixed=TRUE)
}

# robust date parsers -------------------------------------------------
parse_date_flex <- function(x) {
  if (is.na(x) || trimws(as.character(x))=="") return(as.Date(NA))
  s <- trimws(as.character(x))
  # numeric excel serial as character
  num <- suppressWarnings(as.numeric(s))
  if (!is.na(num) && num > 30000 && num < 60000 && !grepl("[-/]", s)) {
    return(as.Date(num, origin="1899-12-30"))
  }
  # try ISO first
  d <- suppressWarnings(as.Date(s, format="%Y-%m-%d"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(s, format="%Y/%m/%d"))
  if (!is.na(d)) return(d)
  # handle month name case-insensitive: convert month part to Title
  s2 <- s
  if (grepl("[A-Za-z]", s2)) {
    m <- regmatches(s2, regexpr("[A-Za-z]+", s2))
    if (length(m)>0 && nchar(m[[1]])>=3) {
      m_title <- paste0(toupper(substr(m[[1]],1,1)), tolower(substr(m[[1]],2,nchar(m[[1]]))))
      s2 <- sub("[A-Za-z]+", m_title, s2)
    }
  }
  fmts <- c("%d-%b-%Y","%d-%B-%Y","%d%b%Y","%d %b %Y","%d %B %Y","%d-%b-%y","%d/%m/%Y","%m/%d/%Y","%d-%m-%Y","%d.%m.%Y","%b %d %Y","%B %d %Y","%d-%m-%y")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s2, format=f))
    if (!is.na(d)) return(d)
    d <- suppressWarnings(as.Date(s, format=f))
    if (!is.na(d)) return(d)
  }
  # generic
  d <- suppressWarnings(as.Date(s2))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}
parse_date_flex_vec <- function(v) {
  if (inherits(v, "Date")) return(as.Date(v))
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin="1899-12-30"))
  v <- as.character(v)
  res <- as.Date(rep(NA, length(v)))
  for (i in seq_along(v)) res[i] <- parse_date_flex(v[i])
  res
}
fmt_iso <- function(d) ifelse(is.na(d), "", format(d, "%Y-%m-%d"))

# locate raw files ----------------------------------------------------
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

edc_ds_path <- file.path(edc_dir, "edc_ds.xlsx")
edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
edc_death_path <- file.path(edc_dir, "edc_death.xlsx")
ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")

edc_ds <- readxl::read_excel(edc_ds_path)
edc_dm_raw <- readxl::read_excel(edc_dm_path)

# helper to find column name case-insensitive
find_col <- function(nms, pattern) {
  idx <- grep(pattern, nms, ignore.case=TRUE)
  if (length(idx)>0) nms[idx[1]] else NA_character_
}

# Build dm_for_ds from edc_dm (authoritative for ONCO-2026) ----------
nms_dm <- names(edc_dm_raw)
# SUBJID
subj_col_dm <- find_col(nms_dm, "^SUBJID$")
if (is.na(subj_col_dm)) subj_col_dm <- find_col(nms_dm, "SUBJID")
# RFSTDTC candidates
rf_col <- find_col(nms_dm, "^RFSTDTC$")
if (is.na(rf_col)) rf_col <- find_col(nms_dm, "RFSTDTC")
if (is.na(rf_col)) rf_col <- find_col(nms_dm, "^RFSTDT")
if (is.na(rf_col)) rf_col <- find_col(nms_dm, "RANDDT|RAND_DTC|RANDTC")
if (is.na(rf_col)) rf_col <- find_col(nms_dm, "EXSTDTC|EX_STTC")
# DTHDTC candidates
dth_col <- find_col(nms_dm, "^DTHDTC$")
if (is.na(dth_col)) dth_col <- find_col(nms_dm, "DTHDTC")
if (is.na(dth_col)) dth_col <- find_col(nms_dm, "DTHDT|DEATHDT|DTH_DTC|DEATH_DTC")
if (is.na(dth_col)) dth_col <- find_col(nms_dm, "DTHFL")

# Build base dm
dm_for_ds <- data.frame(USUBJID=character(), SUBJID=character(), RFSTDTC=character(), DTHDTC=character(), stringsAsFactors=FALSE)

# Extract SUBJID vector
subj_vals <- as.character(edc_dm_raw[[subj_col_dm]])

# Helper to convert any column to ISO character
col_to_iso <- function(col_vec) {
  if (is.null(col_vec)) return(rep(NA_character_, length(subj_vals)))
  if (inherits(col_vec, "Date") || inherits(col_vec, "POSIXt")) {
    return(format(as.Date(col_vec), "%Y-%m-%d"))
  }
  if (is.numeric(col_vec)) {
    # excel serial
    d <- as.Date(col_vec, origin="1899-12-30")
    return(format(d, "%Y-%m-%d"))
  }
  # character
  d <- parse_date_flex_vec(col_vec)
  out <- fmt_iso(d)
  out[out==""] <- NA_character_
  # if still NA but original had value, keep original if it looks ISO
  iso_like <- grepl("^\\d{4}-\\d{2}-\\d{2}", as.character(col_vec))
  out[is.na(out) & iso_like] <- as.character(col_vec)[is.na(out) & iso_like]
  out
}

rf_vals <- if (!is.na(rf_col)) edc_dm_raw[[rf_col]] else NULL
# if rf_col not found, try to derive from edc_ex or ivrs_rand
if (is.null(rf_vals) || all(is.na(rf_vals) | trimws(as.character(rf_vals))=="")) {
  # try edc_ex
  if (file.exists(edc_ex_path)) {
    edc_ex <- readxl::read_excel(edc_ex_path)
    nms_ex <- names(edc_ex)
    ex_subj <- find_col(nms_ex, "^SUBJID$")
    if (is.na(ex_subj)) ex_subj <- find_col(nms_ex, "SUBJID")
    ex_dt <- find_col(nms_ex, "EXSTDTC|EXSTDT|EX_DT|EXSTD|EXDATE")
    if (is.na(ex_dt)) {
      dtcands <- grep("DT", nms_ex, value=TRUE, ignore.case=TRUE)
      if (length(dtcands)>0) ex_dt <- dtcands[1]
    }
    if (!is.na(ex_subj) && !is.na(ex_dt)) {
      ex_subj_vals <- as.character(edc_ex[[ex_subj]])
      ex_dt_vals <- edc_ex[[ex_dt]]
      ex_iso <- col_to_iso(ex_dt_vals)
      ex_df <- data.frame(SUBJID=ex_subj_vals, RFSTDTC=ex_iso, stringsAsFactors=FALSE)
      ex_df <- ex_df[!is.na(ex_df$RFSTDTC) & ex_df$RFSTDTC!="",]
      if (nrow(ex_df)>0) {
        ex_agg <- ex_df %>% group_by(SUBJID) %>% summarise(RFSTDTC=min(RFSTDTC), .groups="drop")
        # map to dm
        rf_vals <- rep(NA_character_, length(subj_vals))
        for (i in seq_along(subj_vals)) {
          hit <- ex_agg$RFSTDTC[ex_agg$SUBJID==subj_vals[i]]
          if (length(hit)>0) rf_vals[i] <- hit[1]
        }
      }
    }
  }
  if ((is.null(rf_vals) || all(is.na(rf_vals))) && file.exists(ivrs_rand_path)) {
    ivrs_rand <- readxl::read_excel(ivrs_rand_path)
    nms_r <- names(ivrs_rand)
    r_subj <- find_col(nms_r, "^SUBJID$")
    if (is.na(r_subj)) r_subj <- find_col(nms_r, "SUBJID")
    r_dt <- find_col(nms_r, "RANDDT|RANDTC|RAND_D")
    if (!is.na(r_subj) && !is.na(r_dt)) {
      r_iso <- col_to_iso(ivrs_rand[[r_dt]])
      r_df <- data.frame(SUBJID=as.character(ivrs_rand[[r_subj]]), RFSTDTC=r_iso, stringsAsFactors=FALSE)
      r_df <- r_df[!is.na(r_df$RFSTDTC) & r_df$RFSTDTC!="",]
      if (nrow(r_df)>0) {
        r_agg <- r_df %>% group_by(SUBJID) %>% summarise(RFSTDTC=min(RFSTDTC), .groups="drop")
        if (is.null(rf_vals) || all(is.na(rf_vals))) rf_vals <- rep(NA_character_, length(subj_vals))
        for (i in seq_along(subj_vals)) {
          if (is.na(rf_vals[i])) {
            hit <- r_agg$RFSTDTC[r_agg$SUBJID==subj_vals[i]]
            if (length(hit)>0) rf_vals[i] <- hit[1]
          }
        }
      }
    }
  }
}

rf_iso <- col_to_iso(rf_vals)

# DTHDTC
dth_vals <- if (!is.na(dth_col)) edc_dm_raw[[dth_col]] else NULL
# if DTH column is flag Y/N, need death date from edc_death
if (!is.null(dth_vals) && all(toupper(na.omit(as.character(dth_vals))) %in% c("Y","N",""))) {
  dth_vals <- NULL
}
if (is.null(dth_vals) || all(is.na(dth_vals) | trimws(as.character(dth_vals))=="")) {
  if (file.exists(edc_death_path)) {
    edc_death <- readxl::read_excel(edc_death_path)
    nms_dd <- names(edc_death)
    dd_subj <- find_col(nms_dd, "^SUBJID$")
    if (is.na(dd_subj)) dd_subj <- find_col(nms_dd, "SUBJID")
    dd_dt <- find_col(nms_dd, "DTHDTC|DEATHDT|DTHDT|DTH_DT|DEATH_DT|DTHDTC")
    if (is.na(dd_dt)) {
      dtcands <- grep("DT", nms_dd, value=TRUE, ignore.case=TRUE)
      if (length(dtcands)>0) dd_dt <- dtcands[1]
    }
    if (!is.na(dd_subj) && !is.na(dd_dt)) {
      dd_iso <- col_to_iso(edc_death[[dd_dt]])
      dd_df <- data.frame(SUBJID=as.character(edc_death[[dd_subj]]), DTHDTC=dd_iso, stringsAsFactors=FALSE)
      dd_df <- dd_df[!is.na(dd_df$DTHDTC) & dd_df$DTHDTC!="",]
      if (nrow(dd_df)>0) {
        dd_agg <- dd_df %>% group_by(SUBJID) %>% summarise(DTHDTC=min(DTHDTC), .groups="drop")
        dth_vals <- rep(NA_character_, length(subj_vals))
        for (i in seq_along(subj_vals)) {
          hit <- dd_agg$DTHDTC[dd_agg$SUBJID==subj_vals[i]]
          if (length(hit)>0) dth_vals[i] <- hit[1]
        }
      } else {
        dth_vals <- rep(NA_character_, length(subj_vals))
      }
    } else {
      dth_vals <- rep(NA_character_, length(subj_vals))
    }
  } else {
    dth_vals <- rep(NA_character_, length(subj_vals))
  }
}
dth_iso <- col_to_iso(dth_vals)

dm_for_ds <- data.frame(
  USUBJID = paste0("ONCO-2026-", subj_vals),
  SUBJID = as.character(subj_vals),
  RFSTDTC = rf_iso,
  DTHDTC = dth_iso,
  stringsAsFactors=FALSE
)
dm_for_ds <- dm_for_ds %>% arrange(USUBJID)

# If RFSTDTC still all NA, try to read any date column from edc_dm as fallback
if (all(is.na(dm_for_ds$RFSTDTC))) {
  # find any column with date-like values
  for (cn in nms_dm) {
    if (cn == subj_col_dm) next
    vals <- edc_dm_raw[[cn]]
    dtest <- parse_date_flex_vec(vals)
    if (sum(!is.na(dtest)) >= 3) {
      dm_for_ds$RFSTDTC <- col_to_iso(vals)
      break
    }
  }
}
# If DTHDTC still all NA, check edc_dm for any DTH column again
if (all(is.na(dm_for_ds$DTHDTC))) {
  # keep NA, DTHFL will be NA except where death exists
}

# Prepare Raw DS Data -------------------------------------------------
nms_ds <- names(edc_ds)
term_col <- find_col(nms_ds, "^DS_TERM$")
if (is.na(term_col)) term_col <- find_col(nms_ds, "^DSTERM$")
if (is.na(term_col)) term_col <- find_col(nms_ds, "TERM")
cat_col <- find_col(nms_ds, "^DS_CAT$")
if (is.na(cat_col)) cat_col <- find_col(nms_ds, "^DSCAT$")
if (is.na(cat_col)) cat_col <- find_col(nms_ds, "^CAT$")
dt_col <- find_col(nms_ds, "^DS_DT$")
if (is.na(dt_col)) dt_col <- find_col(nms_ds, "^DSSTDTC$")
if (is.na(dt_col)) dt_col <- find_col(nms_ds, "DS_DT")
if (is.na(dt_col)) {
  dtcands <- grep("DT", nms_ds, value=TRUE, ignore.case=TRUE)
  if (length(dtcands)>0) dt_col <- dtcands[1]
}
subj_col_ds <- find_col(nms_ds, "^SUBJID$")
if (is.na(subj_col_ds)) subj_col_ds <- find_col(nms_ds, "SUBJID")

edc_ds_prep <- data.frame(
  SUBJID = as.character(edc_ds[[subj_col_ds]]),
  TERM = as.character(edc_ds[[term_col]]),
  DECOD = as.character(edc_ds[[term_col]]),
  CAT = if (!is.na(cat_col)) as.character(edc_ds[[cat_col]]) else NA_character_,
  STDT_raw = edc_ds[[dt_col]],
  stringsAsFactors=FALSE
)
# Convert STDT_raw to character date11-like string for SAS input logic, but keep original for parsing
# If STDT_raw is Date/numeric, convert to character in date11 format
stdt_char <- rep(NA_character_, nrow(edc_ds_prep))
for (i in seq_len(nrow(edc_ds_prep))) {
  v <- edc_ds_prep$STDT_raw[i]
  if (is.na(v)) { stdt_char[i] <- NA_character_; next }
  if (inherits(v, "Date") || inherits(v, "POSIXt")) {
    stdt_char[i] <- format(as.Date(v), "%d-%b-%Y")
    stdt_char[i] <- toupper(stdt_char[i])
  } else if (is.numeric(v)) {
    d <- as.Date(as.numeric(v), origin="1899-12-30")
    stdt_char[i] <- toupper(format(d, "%d-%b-%Y"))
  } else {
    stdt_char[i] <- as.character(v)
  }
}
edc_ds_prep$STDT <- stdt_char
edc_ds_prep$USUBJID <- paste0("ONCO-2026-", edc_ds_prep$SUBJID)
edc_ds_prep <- edc_ds_prep %>% select(USUBJID, SUBJID, TERM, DECOD, CAT, STDT) %>% arrange(USUBJID)

# Merge ---------------------------------------------------------------
sdtm_ds1 <- edc_ds_prep %>% left_join(dm_for_ds, by="USUBJID", suffix=c("",".dm"))
if ("SUBJID.dm" %in% names(sdtm_ds1)) sdtm_ds1$SUBJID.dm <- NULL

sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"
sdtm_ds1$DSTERM <- sapply(sdtm_ds1$TERM, function(x) if (is.na(x)) NA_character_ else tranwrd(x, "_", " "), USE.NAMES=FALSE)
sdtm_ds1$DSDECOD <- toupper(sdtm_ds1$DECOD)
sdtm_ds1$DSCAT <- sapply(sdtm_ds1$CAT, function(x) if (is.na(x)) NA_character_ else tranwrd(x, "_", " "), USE.NAMES=FALSE)
sdtm_ds1$DSTERM <- substr(sdtm_ds1$DSTERM, 1, 50)
sdtm_ds1$DSDECOD <- substr(sdtm_ds1$DSDECOD, 1, 30)
sdtm_ds1$DSCAT <- substr(sdtm_ds1$DSCAT, 1, 30)

# DTHFL
sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(sdtm_ds1$DTHDTC)!="", "Y", NA_character_)

# DSSTDTC = put(input(STDT, date11.), yymmdd10.)
sdtm_ds1$DSSTDTC <- NA_character_
for (i in seq_len(nrow(sdtm_ds1))) {
  stdt <- sdtm_ds1$STDT[i]
  if (!is.na(stdt) && trimws(stdt)!="") {
    d <- parse_date_flex(stdt)
    if (!is.na(d)) sdtm_ds1$DSSTDTC[i] <- format(d, "%Y-%m-%d")
  }
}

# DSSTDY
sdtm_ds1$DSSTDY <- NA_real_
for (i in seq_len(nrow(sdtm_ds1))) {
  dsstdtc <- sdtm_ds1$DSSTDTC[i]
  rfstdtc <- sdtm_ds1$RFSTDTC[i]
  if (!is.na(dsstdtc) && dsstdtc!="" && !is.na(rfstdtc) && rfstdtc!="") {
    stdt <- suppressWarnings(as.Date(dsstdtc, format="%Y-%m-%d"))
    rfdt <- suppressWarnings(as.Date(rfstdtc, format="%Y-%m-%d"))
    if (is.na(stdt)) stdt <- parse_date_flex(dsstdtc)
    if (is.na(rfdt)) rfdt <- parse_date_flex(rfstdtc)
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) sdtm_ds1$DSSTDY[i] <- diff + 1 else sdtm_ds1$DSSTDY[i] <- diff
    }
  }
}

# Warning for 103
warn_msg <- NULL
idx103 <- which(sdtm_ds1$USUBJID=="ONCO-2026-103")
if (length(idx103)>0) {
  for (i in idx103) {
    if (!is.na(sdtm_ds1$DSSTDTC[i]) && sdtm_ds1$DSSTDTC[i]!="" && !is.na(sdtm_ds1$DTHDTC[i]) && sdtm_ds1$DSSTDTC[i] != sdtm_ds1$DTHDTC[i]) {
      msg <- paste0("WARNING: Date mismatch! DS=", sdtm_ds1$DSSTDTC[i], " DM=", sdtm_ds1$DTHDTC[i])
      warning(msg); warn_msg <- c(warn_msg, msg)
    }
  }
}

sdtm_ds1 <- sdtm_ds1 %>% arrange(USUBJID, DSSTDTC)
sdtm_ds1 <- sdtm_ds1 %>% group_by(USUBJID) %>% mutate(DSSEQ=row_number()) %>% ungroup()
sdtm_ds <- sdtm_ds1 %>% select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, DSSTDTC, DSSTDY, DTHFL) %>% arrange(USUBJID, DSSTDTC)
sdtm_ds$DSSEQ <- as.integer(sdtm_ds$DSSEQ)
sdtm_ds$DSSTDY <- as.integer(sdtm_ds$DSSTDY)

# Export --------------------------------------------------------------
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names=FALSE, na="")

# Listing -------------------------------------------------------------
lst_path <- file.path(out, "P024.lst")
txt_path <- file.path(out, "P024.txt")
csv_path <- file.path(out, "P024.csv")
write.csv(sdtm_ds, csv_path, row.names=FALSE, na="")

make_listing <- function(path) {
  con <- file(path, open="wt")
  cat("SDTM DS Dataset - Disposition\n", file=con)
  cat("Study: ONCO-2026  Domain: DS\n", file=con)
  cat(paste0("Generated: ", format(Sys.Date(), "%Y-%m-%d"), "\n"), file=con)
  if (!is.null(warn_msg) && length(warn_msg)>0) {
    cat("\n", file=con)
    for (m in warn_msg) cat(paste0(m, "\n"), file=con)
  }
  cat("\n", file=con)
  header <- sprintf("%3s %-10s %-6s %-15s %5s %-20s %-20s %-20s %-10s %6s %5s",
                    "Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")
  cat(header, "\n", file=con, sep="")
  cat(paste(rep("-", nchar(header)), collapse=""), "\n", file=con, sep="")
  for (i in seq_len(nrow(sdtm_ds))) {
    row <- sdtm_ds[i,]
    line <- sprintf("%3d %-10s %-6s %-15s %5s %-20s %-20s %-20s %-10s %6s %5s",
                    i,
                    ifelse(is.na(row$STUDYID),"",row$STUDYID),
                    ifelse(is.na(row$DOMAIN),"",row$DOMAIN),
                    ifelse(is.na(row$USUBJID),"",row$USUBJID),
                    ifelse(is.na(row$DSSEQ),"",as.character(row$DSSEQ)),
                    ifelse(is.na(row$DSTERM),"",substr(row$DSTERM,1,20)),
                    ifelse(is.na(row$DSDECOD),"",substr(row$DSDECOD,1,20)),
                    ifelse(is.na(row$DSCAT),"",substr(row$DSCAT,1,20)),
                    ifelse(is.na(row$DSSTDTC),"",row$DSSTDTC),
                    ifelse(is.na(row$DSSTDY),"",as.character(row$DSSTDY)),
                    ifelse(is.na(row$DTHFL),"",row$DTHFL))
    cat(line, "\n", file=con, sep="")
  }
  cat("\n", file=con)
  cat(paste0("Total observations: ", nrow(sdtm_ds), "\n"), file=con)
  close(con)
}
make_listing(lst_path)
make_listing(txt_path)

stopifnot(file.exists(file.path(out, "ds.csv")))
stopifnot(file.exists(lst_path))
```