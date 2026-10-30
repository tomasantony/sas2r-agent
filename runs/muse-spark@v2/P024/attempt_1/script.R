out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
f_ds    <- file.path(raw_base, "edc_ds.xlsx")
f_dm    <- file.path(raw_base, "edc_dm.xlsx")
f_ex    <- file.path(raw_base, "edc_ex.xlsx")
f_death <- file.path(raw_base, "edc_death.xlsx")
f_rand  <- file.path(raw_base, "ivrs_rand.xlsx")

safe_read <- function(p) {
  if (!file.exists(p)) return(NULL)
  df <- as.data.frame(readxl::read_excel(p, .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- trimws(names(df))
  df
}

edc_ds    <- safe_read(f_ds)
edc_dm    <- safe_read(f_dm)
edc_ex    <- safe_read(f_ex)
edc_death <- safe_read(f_death)
ivrs_rand <- safe_read(f_rand)

message("edc_ds cols: ", paste(names(edc_ds), collapse="|"))
if (!is.null(edc_dm)) message("edc_dm cols: ", paste(names(edc_dm), collapse="|"))
if (!is.null(edc_ex)) message("edc_ex cols: ", paste(names(edc_ex), collapse="|"))
if (!is.null(edc_death)) message("edc_death cols: ", paste(names(edc_death), collapse="|"))
if (!is.null(ivrs_rand)) message("ivrs_rand cols: ", paste(names(ivrs_rand), collapse="|"))
message("edc_ds nrow=", nrow(edc_ds))

# ---------- helpers ----------
find_col <- function(df, candidates) {
  if (is.null(df) || is.null(candidates)) return(NA_character_)
  un <- toupper(trimws(names(df)))
  for (cand in candidates) {
    idx <- which(un == toupper(cand))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}
find_col_contains <- function(df, patterns) {
  if (is.null(df)) return(NA_character_)
  un <- toupper(trimws(names(df)))
  for (pat in patterns) {
    idx <- grep(pat, un, fixed = TRUE)
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}
sas_tranwrd <- function(x, from, to) {
  # SAS TRANWRD case-sensitive, missing stays missing
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    gsub(from, to, v, fixed = TRUE)
  }, character(1), USE.NAMES = FALSE)
}
sas_upcase <- function(x) {
  vapply(x, function(v) { if (is.na(v)) return(NA_character_); toupper(v) }, character(1), USE.NAMES = FALSE)
}
sas_substr1 <- function(x, n) {
  vapply(x, function(v) { if (is.na(v)) return(NA_character_); substr(v, 1, n) }, character(1), USE.NAMES = FALSE)
}
# normalize DD-MON-YYYY month to Title case for R %b parsing
norm_date11 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v) || trimws(v) == "") return(NA_character_)
    v <- trimws(v)
    # expect dd-mon-yyyy with - or / or space or no sep? SAS date11. is DDMMMYYYY? handle DD-MON-YYYY
    # extract month letters
    m <- regmatches(v, regexpr("[A-Za-z]+", v))
    if (length(m) == 0 || is.na(m)) return(v)
    month_title <- paste0(toupper(substr(m,1,1)), tolower(substr(m,2,nchar(m))))
    sub("[A-Za-z]+", month_title, v, fixed = FALSE)
  }, character(1), USE.NAMES = FALSE)
}
parse_date11_to_iso <- function(stdt_vec) {
  # STDT is $11 char like 15-Jan-2026 ; SAS input(STDT,date11.) then yymmdd10.
  # ONE strict format %d-%b-%Y
  norm <- norm_date11(stdt_vec)
  # R as.Date with single format
  d <- as.Date(norm, format = "%d-%b-%Y")
  iso <- format(d, "%Y-%m-%d")
  iso[is.na(d)] <- NA_character_
  # blank input stays blank
  blank_in <- is.na(stdt_vec) | trimws(as.character(stdt_vec)) == ""
  iso[blank_in] <- NA_character_
  iso
}
fmt_excel_to_iso <- function(x) {
  # convert Date/POSIXct/character-ISO/character-date11 to ISO YYYY-MM-DD
  if (is.null(x)) return(rep(NA_character_, 0))
  if (inherits(x, "Date")) {
    iso <- format(x, "%Y-%m-%d"); iso[is.na(x)] <- NA_character_; return(iso)
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    d <- as.Date(x); iso <- format(d, "%Y-%m-%d"); iso[is.na(d)] <- NA_character_; return(iso)
  }
  if (is.numeric(x)) {
    # Excel serial unlikely; treat as days since 1899-12-30 only if plausible
    iso <- rep(NA_character_, length(x))
    ok <- !is.na(x) & x > 1000 & x < 80000
    iso[ok] <- format(as.Date(x[ok], origin = "1899-12-30"), "%Y-%m-%d")
    return(iso)
  }
  xc <- trimws(as.character(x))
  xc[xc == "" | xc == "." | toupper(xc) == "NA" | toupper(xc) == "NULL"] <- NA_character_
  iso <- rep(NA_character_, length(xc))
  notna <- !is.na(xc)
  # already ISO YYYY-MM-DD (allow datetime prefix, take first 10)
  is_iso <- grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", xc)
  is_iso[is.na(is_iso)] <- FALSE
  iso[notna & is_iso] <- substr(xc[notna & is_iso], 1, 10)
  # normalize ISO to zero-padded YYYY-MM-DD via single format %Y-%m-%d
  need <- notna & is_iso
  if (any(need, na.rm = TRUE)) {
    d <- as.Date(xc[need], format = "%Y-%m-%d")
    iso[need] <- format(d, "%Y-%m-%d")
  }
  # remainder try date11 single format
  need2 <- notna & !is_iso
  if (any(need2, na.rm = TRUE)) {
    iso[need2] <- parse_date11_to_iso(xc[need2])
    # if still NA, try ISO with slashes? keep NA to mimic SAS missing
  }
  iso
}

# ---------- 1. Build sdtm_dm equivalent (dm_for_ds) ----------
# SUBJID from edc_dm
subj_dm_col <- find_col(edc_dm, c("SUBJID","SUBJ_ID","SUBJECTID","PTID","PATID","PATIENTID","SID"))
if (is.na(subj_dm_col)) {
  # fallback: first column containing SUBJ or PT
  subj_dm_col <- find_col_contains(edc_dm, c("SUBJ","PTID"))
}
stopifnot(!is.na(subj_dm_col))

subj_vals <- trimws(as.character(edc_dm[[subj_dm_col]]))
subj_vals[subj_vals=="" ] <- NA_character_
# drop missing subjects
keep_s <- !is.na(subj_vals)
edc_dm_s <- edc_dm[keep_s, , drop=FALSE]
subj_vals <- subj_vals[keep_s]

usubjid_dm <- paste0("ONCO-2026-", subj_vals)
# SAS CATX strips blanks; truncate to $25 like SAS LENGTH
usubjid_dm <- sas_substr1(usubjid_dm, 25)

# RFSTDTC
rf_col_dm <- find_col(edc_dm, c("RFSTDTC","RFSTDT","RFSTDAT","RANDDTC","RANDDT","RAND_DT","EXSTDT","FIRSTEXDT","ENROLLDT","ENRLDT","ENROLDT","STARTDT","STDT"))
rf_iso <- rep(NA_character_, length(subj_vals))
if (!is.na(rf_col_dm)) {
  rf_iso <- fmt_excel_to_iso(edc_dm_s[[rf_col_dm]])
} else {
  rf_iso <- rep(NA_character_, length(subj_vals))
}
# supplement from edc_ex minimum date per subject if missing
if (!is.null(edc_ex)) {
  ex_subj_col <- find_col(edc_ex, c("SUBJID","SUBJ_ID","PTID","PATID","SID"))
  if (is.na(ex_subj_col)) ex_subj_col <- find_col_contains(edc_ex, c("SUBJ"))
  ex_dt_col <- find_col(edc_ex, c("EXSTDTC","EXSTDT","EX_DT","EXDAT","EXDATE","EXDT","STDTC","STDT","EXSTARTDT","EXDTC","DATE","DT"))
  if (is.na(ex_dt_col)) ex_dt_col <- find_col_contains(edc_ex, c("DT"))
  if (!is.na(ex_subj_col) && !is.na(ex_dt_col)) {
    ex_subj <- trimws(as.character(edc_ex[[ex_subj_col]]))
    ex_iso_all <- fmt_excel_to_iso(edc_ex[[ex_dt_col]])
    # min per subject
    for (i in seq_along(subj_vals)) {
      if (!is.na(rf_iso[i]) && rf_iso[i] != "") next
      m <- which(ex_subj == subj_vals[i] & !is.na(ex_iso_all) & ex_iso_all != "")
      if (length(m) > 0) rf_iso[i] <- min(ex_iso_all[m])
    }
  }
}
# supplement from ivrs_rand if still missing
if (!is.null(ivrs_rand)) {
  r_subj_col <- find_col(ivrs_rand, c("SUBJID","SUBJ_ID","PTID","PATID","SID"))
  if (is.na(r_subj_col)) r_subj_col <- find_col_contains(ivrs_rand, c("SUBJ"))
  r_dt_col <- find_col(ivrs_rand, c("RANDDTC","RANDDT","RAND_DT","RANDDATE","RANDD","DT","DATE","DTC"))
  if (is.na(r_dt_col)) r_dt_col <- find_col_contains(ivrs_rand, c("DT"))
  if (!is.na(r_subj_col) && !is.na(r_dt_col)) {
    r_subj <- trimws(as.character(ivrs_rand[[r_subj_col]]))
    r_iso_all <- fmt_excel_to_iso(ivrs_rand[[r_dt_col]])
    for (i in seq_along(subj_vals)) {
      if (!is.na(rf_iso[i]) && rf_iso[i] != "") next
      m <- which(r_subj == subj_vals[i] & !is.na(r_iso_all) & r_iso_all != "")
      if (length(m) > 0) rf_iso[i] <- min(r_iso_all[m])
    }
  }
}

# DTHDTC
dth_col_dm <- find_col(edc_dm, c("DTHDTC","DTHDT","DTH_DT","DEATHDTC","DEATHDT","DEATH_DT","DEATHDATE","DTHDATE"))
dth_iso <- rep(NA_character_, length(subj_vals))
if (!is.na(dth_col_dm)) {
  dth_iso <- fmt_excel_to_iso(edc_dm_s[[dth_col_dm]])
}
if (!is.null(edc_death)) {
  dd_subj_col <- find_col(edc_death, c("SUBJID","SUBJ_ID","PTID","PATID","SID"))
  if (is.na(dd_subj_col)) dd_subj_col <- find_col_contains(edc_death, c("SUBJ"))
  dd_dt_col <- find_col(edc_death, c("DTHDTC","DTHDT","DEATHDTC","DEATHDT","DEATH_DT","DTH_DT","DEATHDATE","DTHDATE","DT","DATE","DTC"))
  if (is.na(dd_dt_col)) dd_dt_col <- find_col_contains(edc_death, c("DT"))
  if (!is.na(dd_subj_col) && !is.na(dd_dt_col)) {
    dd_subj <- trimws(as.character(edc_death[[dd_subj_col]]))
    dd_iso_all <- fmt_excel_to_iso(edc_death[[dd_dt_col]])
    for (i in seq_along(subj_vals)) {
      if (!is.na(dth_iso[i]) && dth_iso[i] != "") next
      m <- which(dd_subj == subj_vals[i] & !is.na(dd_iso_all) & dd_iso_all != "")
      if (length(m) > 0) dth_iso[i] <- dd_iso_all[m[1]]
    }
  }
}

dm_for_ds <- data.frame(
  USUBJID = usubjid_dm,
  SUBJID = subj_vals,
  RFSTDTC = rf_iso,
  DTHDTC = dth_iso,
  stringsAsFactors = FALSE
)
# SAS sort by USUBJID (missing first, ASCII). All present.
dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last = FALSE), , drop = FALSE]

# ---------- 2. Prepare Raw DS Data ----------
ds_term_col <- find_col(edc_ds, c("DS_TERM","DSTERM","TERM","DS_TERM_","DSDECOD"))
ds_cat_col  <- find_col(edc_ds, c("DS_CAT","DSCAT","CAT"))
ds_dt_col   <- find_col(edc_ds, c("DS_DT","DSSTDTC","DSSTDAT","STDT","DS_DATE","DSDate","DT","DATE"))
ds_subj_col <- find_col(edc_ds, c("SUBJID","SUBJ_ID","PTID","PATID","SID"))
if (is.na(ds_subj_col)) ds_subj_col <- find_col_contains(edc_ds, c("SUBJ"))
stopifnot(!is.na(ds_subj_col), !is.na(ds_term_col))
if (is.na(ds_cat_col)) ds_cat_col <- ds_term_col  # fallback, will be blank?
if (is.na(ds_dt_col)) ds_dt_col <- find_col_contains(edc_ds, c("DT"))

get_raw_char <- function(v) {
  if (inherits(v, "Date")) return(format(v, "%d-%b-%Y"))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(format(as.Date(v), "%d-%b-%Y"))
  if (is.numeric(v)) {
    # if Excel serial mistaken as numeric, convert plausible dates
    out <- as.character(v)
    ok <- !is.na(v) & v > 1000 & v < 80000
    out[ok] <- format(as.Date(v[ok], origin="1899-12-30"), "%d-%b-%Y")
    return(out)
  }
  trimws(as.character(v))
}

n_ds <- nrow(edc_ds)
raw_subj <- trimws(as.character(edc_ds[[ds_subj_col]]))
raw_term_c <- get_raw_char(edc_ds[[ds_term_col]])
raw_cat_c  <- if (!is.na(ds_cat_col)) get_raw_char(edc_ds[[ds_cat_col]]) else rep(NA_character_, n_ds)
# STDT handling: if DS_DT is Date type, get_raw_char already gives date11; else char
raw_stdt_c <- if (!is.na(ds_dt_col)) get_raw_char(edc_ds[[ds_dt_col]]) else rep(NA_character_, n_ds)
raw_stdt_c[raw_stdt_c == "" | toupper(raw_stdt_c) == "NA"] <- NA_character_

# SAS LENGTH TERM DECOD CAT $50 STDT $11  (truncate)
TERM  <- sas_substr1(ifelse(is.na(raw_term_c) | raw_term_c=="", NA_character_, raw_term_c), 50)
DECOD <- sas_substr1(ifelse(is.na(raw_term_c) | raw_term_c=="", NA_character_, raw_term_c), 50)
CAT   <- sas_substr1(ifelse(is.na(raw_cat_c)  | raw_cat_c=="",  NA_character_, raw_cat_c), 50)
STDT  <- sas_substr1(raw_stdt_c, 11)
STDT[is.na(STDT)] <- NA_character_

USUBJID_ds <- paste0("ONCO-2026-", trimws(raw_subj))
USUBJID_ds <- sas_substr1(USUBJID_ds, 25)

edc_ds_prep <- data.frame(USUBJID=USUBJID_ds, SUBJID=raw_subj, TERM=TERM, DECOD=DECOD, CAT=CAT, STDT=STDT, stringsAsFactors=FALSE)
# sort by USUBJID (SAS)
edc_ds_prep <- edc_ds_prep[order(edc_ds_prep$USUBJID, na.last=FALSE), , drop=FALSE]

# ---------- 4. Create SDTM DS (merge) ----------
# merge edc_ds_prep(in=a) dm_for_ds by USUBJID, keep if a (left join)
# drop/rename overlapping non-key cols: SUBJID present in both -> keep ds version, drop dm SUBJID before merge
dm_join <- dm_for_ds[, c("USUBJID","RFSTDTC","DTHDTC"), drop=FALSE]
m <- merge(edc_ds_prep, dm_join, by="USUBJID", all.x=TRUE, sort=FALSE)

STUDYID <- rep(sas_substr1("ONCO-2026",10), nrow(m))
DOMAIN  <- rep("DS", nrow(m))
DSTERM  <- sas_substr1(sas_tranwrd(m$TERM, "_", " "), 50)
DSDECOD <- sas_substr1(sas_upcase(m$DECOD), 30)
DSCAT   <- sas_substr1(sas_tranwrd(m$CAT, "_", " "), 30)

DTHFL <- ifelse(!is.na(m$DTHDTC) & trimws(m$DTHDTC) != "", "Y", NA_character_)
DTHFL <- sas_substr1(DTHFL, 1)

DSSTDTC <- parse_date11_to_iso(m$STDT)
# if STDT was NA/blank -> NA (SAS: if STDT ne "" then DSSTDTC=...)
DSSTDTC[is.na(m$STDT) | trimws(m$STDT)==""] <- NA_character_

# DSSTDY
RFSTDTC <- m$RFSTDTC
DSSTDY <- rep(NA_real_, nrow(m))
for (i in seq_len(nrow(m))) {
  ds <- DSSTDTC[i]; rf <- RFSTDTC[i]
  if (!is.na(ds) && ds != "" && !is.na(rf) && rf != "") {
    stdt <- as.Date(ds, format="%Y-%m-%d")
    rfdt <- as.Date(rf, format="%Y-%m-%d")
    if (!is.na(stdt) && !is.na(rfdt)) {
      if (stdt >= rfdt) DSSTDY[i] <- as.numeric(stdt - rfdt) + 1
      else DSSTDY[i] <- as.numeric(stdt - rfdt)
    }
  }
}

sdtm_ds1 <- data.frame(
  STUDYID=STUDYID, DOMAIN=DOMAIN, USUBJID=m$USUBJID,
  DSTERM=ifelse(is.na(DSTERM),"",DSTERM),
  DSDECOD=ifelse(is.na(DSDECOD),"",DSDECOD),
  DSCAT=ifelse(is.na(DSCAT),"",DSCAT),
  DSSTDTC=DSSTDTC, RFSTDTC=RFSTDTC, DTHDTC=m$DTHDTC, DTHFL=DTHFL, DSSTDY=DSSTDY,
  stringsAsFactors=FALSE
)
# SAS missing char "" vs NA: convert "" in DSTERM etc? SAS blank is "". Keep "" for display but NA handling for CSV na="" either works.
# For sorting DSSTDTC missing first: use "" key
sdtm_ds1$DSTERM[sdtm_ds1$DSTERM==""] <- NA_character_
sdtm_ds1$DSDECOD[sdtm_ds1$DSDECOD==""] <- NA_character_
sdtm_ds1$DSCAT[sdtm_ds1$DSCAT==""] <- NA_character_

# Consistency check subject 103
chk <- which(sdtm_ds1$USUBJID=="ONCO-2026-103")
for (i in chk) {
  a <- sdtm_ds1$DSSTDTC[i]; b <- sdtm_ds1$DTHDTC[i]
  ai <- ifelse(is.na(a),"",a); bi <- ifelse(is.na(b),"",b)
  if (ai != bi) message("WARNING: Date mismatch! DS=", ai, " DM=", bi)
}

# ---------- 5. Sort final ----------
ord_key_std <- ifelse(is.na(sdtm_ds1$DSSTDTC), "", sdtm_ds1$DSSTDTC)
sdtm_ds1 <- sdtm_ds1[order(sdtm_ds1$USUBJID, ord_key_std, na.last=FALSE), , drop=FALSE]

# ---------- 6. Derive DSSEQ ----------
sdtm_ds1$DSSEQ <- NA_real_
# by USUBJID retain
us <- sdtm_ds1$USUBJID
seqv <- rep(NA_real_, nrow(sdtm_ds1))
prev <- NA_character_
cnt <- 0
for (i in seq_len(nrow(sdtm_ds1))) {
  cur <- us[i]
  if (is.na(cur) && is.na(prev)) { cnt <- cnt + 1 }
  else if (!is.na(cur) && !is.na(prev) && cur==prev) { cnt <- cnt + 1 }
  else { cnt <- 1 }
  seqv[i] <- cnt
  prev <- cur
}
sdtm_ds1$DSSEQ <- seqv

sdtm_ds <- sdtm_ds1[, c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")]
# ensure lengths
sdtm_ds$STUDYID <- sas_substr1(sdtm_ds$STUDYID,10)
sdtm_ds$DOMAIN <- sas_substr1(sdtm_ds$DOMAIN,2)
sdtm_ds$USUBJID <- sas_substr1(sdtm_ds$USUBJID,25)
sdtm_ds$DSTERM <- sas_substr1(sdtm_ds$DSTERM,50)
sdtm_ds$DSDECOD <- sas_substr1(sdtm_ds$DSDECOD,30)
sdtm_ds$DSCAT <- sas_substr1(sdtm_ds$DSCAT,30)

# ---------- 7/8. Outputs ----------
# dataset ds
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names=FALSE, na="")

# PROC PRINT rendering
# Build text like SAS PROC PRINT with Obs column
prt <- sdtm_ds
prt$Obs <- seq_len(nrow(prt))
prt <- prt[, c("Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")]
# format numerics as SAS BEST (no decimals for DSSEQ/DSSTDY integers, blank for missing)
fmt_cell <- function(v) { if (is.na(v)) return(""); as.character(v) }
lines <- c(
  "PROC PRINT of SDTM DS",
  paste0("Obs STUDYID DOMAIN USUBJID DSSEQ DSTERM DSDECOD DSCAT DSSTDTC DSSTDY DTHFL"),
  "--------------------------------------------------------------------------------"
)
for (i in seq_len(nrow(prt))) {
  r <- prt[i,]
  lines <- c(lines, paste(
    fmt_cell(r$Obs),
    ifelse(is.na(r$STUDYID),"",r$STUDYID),
    ifelse(is.na(r$DOMAIN),"",r$DOMAIN),
    ifelse(is.na(r$USUBJID),"",r$USUBJID),
    fmt_cell(r$DSSEQ),
    ifelse(is.na(r$DSTERM),"",r$DSTERM),
    ifelse(is.na(r$DSDECOD),"",r$DSDECOD),
    ifelse(is.na(r$DSCAT),"",r$DSCAT),
    ifelse(is.na(r$DSSTDTC),"",r$DSSTDTC),
    fmt_cell(r$DSSTDY),
    ifelse(is.na(r$DTHFL),"",r$DTHFL),
    sep=" | "))
}
writeLines(lines, file.path(out, "P024.txt"))
writeLines(lines, file.path(out, "P024.lst"))
write.csv(prt, file.path(out, "P024.csv"), row.names=FALSE, na="")
message("done n=", nrow(sdtm_ds))
