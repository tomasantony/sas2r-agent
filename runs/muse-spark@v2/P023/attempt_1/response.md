```r
# P023_dm.sas -> R translation
# SDTM DM creation + PROC PRINT listing

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# round half away from zero (SAS) helper
round_sas <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
p_edc_dm    <- file.path(raw_base, "edc_dm.xlsx")
p_ivrs_rand <- file.path(raw_base, "ivrs_rand.xlsx")
p_edc_ex    <- file.path(raw_base, "edc_ex.xlsx")
p_edc_ds    <- file.path(raw_base, "edc_ds.xlsx")
p_edc_death <- file.path(raw_base, "edc_death.xlsx")

read_text_xlsx <- function(path) {
  df <- as.data.frame(readxl::read_excel(path, col_types = "text", .name_repair = "minimal"),
                      stringsAsFactors = FALSE, check.names = FALSE)
  names(df) <- toupper(trimws(names(df)))
  # trim all character values
  for (nm in names(df)) {
    v <- df[[nm]]
    # read with col_types=text gives character; be safe
    v <- as.character(v)
    # readxl returns NA for blanks already
    v[trimws(v) == "" & !is.na(v)] <- NA_character_
    df[[nm]] <- trimws(v)
    df[[nm]][df[[nm]] == "" ] <- NA_character_
  }
  df
}

edc_dm    <- read_text_xlsx(p_edc_dm)
ivrs_rand <- read_text_xlsx(p_ivrs_rand)
edc_ex    <- read_text_xlsx(p_edc_ex)
edc_ds    <- read_text_xlsx(p_edc_ds)
edc_death <- read_text_xlsx(p_edc_death)

# Ensure required columns exist (case-insensitive already uppercased)
# SUBJID key normalization to character
norm_key <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == "" ] <- NA_character_
  x
}
edc_dm$SUBJID    <- norm_key(edc_dm$SUBJID)
ivrs_rand$SUBJID <- norm_key(ivrs_rand$SUBJID)
edc_ex$SUBJID    <- norm_key(edc_ex$SUBJID)
edc_ds$SUBJID    <- norm_key(edc_ds$SUBJID)
edc_death$SUBJID <- norm_key(edc_death$SUBJID)

stopifnot("SUBJID" %in% names(edc_dm))
stopifnot("SUBJID" %in% names(ivrs_rand))
stopifnot("SUBJID" %in% names(edc_ex))
stopifnot("SUBJID" %in% names(edc_ds))
stopifnot("SUBJID" %in% names(edc_death))

# Strict DATE11. parser: DD-MMM-YYYY (e.g. 01-JAN-2020), SAS date11. input
# Handles Date/POSIX/numeric (excel) plus character
parse_date11_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # Excel serial dates
    return(as.Date(x, origin = "1899-12-30"))
  }
  x <- as.character(x)
  res <- rep(as.Date(NA), length(x))
  idx <- which(!is.na(x) & trimws(x) != "" & toupper(trimws(x)) != "NA")
  if (length(idx) == 0) return(res)
  mon_map <- c(JAN=1L,FEB=2L,MAR=3L,APR=4L,MAY=5L,JUN=6L,
               JUL=7L,AUG=8L,SEP=9L,OCT=10L,NOV=11L,DEC=12L)
  for (k in idx) {
    s <- trimws(x[k])
    # strict DD-MMM-YYYY with 1-2 digit day
    m <- regmatches(s, regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{4})$", s))[[1]]
    if (length(m) == 4) {
      d <- suppressWarnings(as.integer(m[2]))
      mon <- mon_map[[toupper(m[3])]]
      y <- suppressWarnings(as.integer(m[4]))
      if (!is.na(d) && !is.na(mon) && !is.na(y)) {
        dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mon, d), format = "%Y-%m-%d"))
        # validate components (reject overflow like 31-FEB)
        if (!is.na(dt)) {
          lt <- as.POSIXlt(dt)
          if (lt$mday == d && (lt$mon + 1L) == mon && (lt$year + 1900L) == y) {
            res[k] <- dt
          }
        }
      }
    } else {
      # fallback strict single format attempt via as.Date %d-%b-%Y after title-casing month
      # convert MON to Title case e.g. JAN -> Jan
      s2 <- gsub("(?i)-([a-z]{3})-", function(mm) paste0("-", toupper(substr(mm,2,2)), tolower(substr(mm,3,3)), "-"), s, perl = TRUE)
      dt2 <- suppressWarnings(as.Date(s2, format = "%d-%b-%Y"))
      if (!is.na(dt2)) res[k] <- dt2
    }
  }
  res
}

fmt_yymmdd10 <- function(d) {
  # SAS PUT(date, yymmdd10.) -> YYYY-MM-DD, blank if missing
  if (inherits(d, "Date") || inherits(d, "POSIXt")) {
    d <- as.Date(d)
    out <- format(d, "%Y-%m-%d")
    out[is.na(d)] <- NA_character_
    return(out)
  }
  # if already Date vector
  d <- as.Date(d, origin = "1970-01-01")
  out <- format(d, "%Y-%m-%d")
  out[is.na(d)] <- NA_character_
  out
}

# STEP 1a: ds_enrol from ivrs_rand RAND_DT
stopifnot("RAND_DT" %in% names(ivrs_rand))
ds_enrol <- data.frame(
  SUBJID = ivrs_rand$SUBJID,
  enr_dt = parse_date11_vec(ivrs_rand$RAND_DT),
  stringsAsFactors = FALSE
)
# one row per SUBJID (keep first if duplicates, SAS sort/merge expects sorted)
ds_enrol <- ds_enrol[order(ds_enrol$SUBJID), , drop = FALSE]
ds_enrol <- ds_enrol[!duplicated(ds_enrol$SUBJID), , drop = FALSE]

# STEP 1b: ex_dates from edc_ex STDT/ENDT
stopifnot("STDT" %in% names(edc_ex))
stopifnot("ENDT" %in% names(edc_ex))
edc_ex$`.stdt_d` <- parse_date11_vec(edc_ex$STDT)
edc_ex$`.endt_d` <- parse_date11_vec(edc_ex$ENDT)
# min/max per SUBJID ignoring NA (SQL MIN/MAX ignore nulls)
subs_ex <- sort(unique(edc_ex$SUBJID[!is.na(edc_ex$SUBJID)]))
ex_dates <- data.frame(SUBJID = subs_ex, f_dose = as.Date(NA), l_dose = as.Date(NA),
                       stringsAsFactors = FALSE)
for (i in seq_along(subs_ex)) {
  s <- subs_ex[i]
  sd <- edc_ex$`.stdt_d`[edc_ex$SUBJID == s]
  ed <- edc_ex$`.endt_d`[edc_ex$SUBJID == s]
  sd <- sd[!is.na(sd)]; ed <- ed[!is.na(ed)]
  ex_dates$f_dose[i] <- if (length(sd)) min(sd) else as.Date(NA)
  ex_dates$l_dose[i] <- if (length(ed)) max(ed) else as.Date(NA)
}
ex_dates <- ex_dates[order(ex_dates$SUBJID), , drop = FALSE]

# STEP 1c: ds_final from edc_ds DS_DT / DS_TERM
stopifnot("DS_DT" %in% names(edc_ds))
stopifnot("DS_TERM" %in% names(edc_ds))
ds_final <- data.frame(
  SUBJID = edc_ds$SUBJID,
  end_dt = parse_date11_vec(edc_ds$DS_DT),
  dth_fl = ifelse(!is.na(edc_ds$DS_TERM) & edc_ds$DS_TERM == "DEATH", "Y", "N"),
  stringsAsFactors = FALSE
)
ds_final <- ds_final[order(ds_final$SUBJID), , drop = FALSE]
# keep first per SUBJID if duplicates (protect DM uniqueness)
ds_final <- ds_final[!duplicated(ds_final$SUBJID), , drop = FALSE]

# STEP 2: sorts (SAS requires sorting before BY merge)
edc_dm_s <- edc_dm[order(edc_dm$SUBJID), , drop = FALSE]
# ivrs TRT_CODE keep
stopifnot("TRT_CODE" %in% names(ivrs_rand))
ivrs_keep <- ivrs_rand[, c("SUBJID", "TRT_CODE"), drop = FALSE]
ivrs_keep <- ivrs_keep[order(ivrs_keep$SUBJID), , drop = FALSE]
ivrs_keep <- ivrs_keep[!duplicated(ivrs_keep$SUBJID), , drop = FALSE]

ds_enrol <- ds_enrol[order(ds_enrol$SUBJID), , drop = FALSE]
ex_dates <- ex_dates[order(ex_dates$SUBJID), , drop = FALSE]
ds_final <- ds_final[order(ds_final$SUBJID), , drop = FALSE]

# edc_death rename DTH_DT -> death_date_raw, keep only needed to avoid .x/.y
stopifnot("DTH_DT" %in% names(edc_death))
death_keep <- data.frame(SUBJID = edc_death$SUBJID,
                         death_date_raw = edc_death$DTH_DT,
                         stringsAsFactors = FALSE, check.names = FALSE)
death_keep <- death_keep[order(death_keep$SUBJID), , drop = FALSE]
death_keep <- death_keep[!duplicated(death_keep$SUBJID), , drop = FALSE]

# STEP 3: merge (left join, keep only subjects in DM: if a)
# drop/rename overlapping cols except keys before merging (already selected)
m <- merge(edc_dm_s, ivrs_keep, by = "SUBJID", all.x = TRUE, sort = TRUE)
m <- merge(m, ds_enrol, by = "SUBJID", all.x = TRUE, sort = TRUE)
m <- merge(m, ex_dates, by = "SUBJID", all.x = TRUE, sort = TRUE)
m <- merge(m, ds_final, by = "SUBJID", all.x = TRUE, sort = TRUE)
m <- merge(m, death_keep, by = "SUBJID", all.x = TRUE, sort = TRUE)
stopifnot("SUBJID" %in% names(m))

# Helper to get column or NA vector if missing
getcol <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA_character_, nrow(df))
}

SITE_v   <- getcol(m, "SITE")
BRTHDT_v <- getcol(m, "BRTHDT")
RFICDT_v <- getcol(m, "RFICDT")
GENDER_v <- getcol(m, "GENDER")
RACE_v   <- getcol(m, "RACE")
ETHNIC_v <- getcol(m, "ETHNIC")

BRTHDTC_v <- fmt_yymmdd10(parse_date11_vec(BRTHDT_v))
RFICDTC_v <- fmt_yymmdd10(parse_date11_vec(RFICDT_v))

SEX_v <- substr(ifelse(is.na(GENDER_v), "", GENDER_v), 1, 1)
SEX_v[is.na(GENDER_v)] <- NA_character_
SEX_v[SEX_v == ""] <- NA_character_

# AGE = intck('year', BRTHDTC, RFICDTC, 'c') = completed years
brth_d <- parse_date11_vec(BRTHDT_v)
rfic_d <- parse_date11_vec(RFICDT_v)
AGE_v <- rep(NA_real_, nrow(m))
for (i in seq_len(nrow(m))) {
  b <- brth_d[i]; r <- rfic_d[i]
  if (!is.na(b) && !is.na(r)) {
    bl <- as.POSIXlt(b); rl <- as.POSIXlt(r)
    a <- (rl$year - bl$year)
    if ((rl$mon < bl$mon) || (rl$mon == bl$mon && rl$mday < bl$mday)) a <- a - 1L
    AGE_v[i] <- a
  }
}

RFSTDTC_v  <- fmt_yymmdd10(m$enr_dt)
RFXSTDTC_v <- fmt_yymmdd10(m$f_dose)
RFXENDTC_v <- fmt_yymmdd10(m$l_dose)
RFENDTC_v  <- fmt_yymmdd10(m$end_dt)
RFPENDTC_v <- RFENDTC_v

ARMCD_v <- if ("TRT_CODE" %in% names(m)) m$TRT_CODE else rep(NA_character_, nrow(m))
ARM_v <- rep(NA_character_, nrow(m))
ARM_v[!is.na(ARMCD_v) & ARMCD_v == "DRUGA"] <- "Experimental Drug A"
ARM_v[!is.na(ARMCD_v) & ARMCD_v == "PBO"] <- "Placebo"

ACTARMCD_v <- ARMCD_v
ACTARM_v <- ARM_v

DTHFL_v <- if ("dth_fl" %in% names(m)) m$dth_fl else rep(NA_character_, nrow(m))
death_raw <- if ("death_date_raw" %in% names(m)) m$death_date_raw else rep(NA_character_, nrow(m))
DTHDTC_v <- rep(NA_character_, nrow(m))
has_death <- !is.na(death_raw) & trimws(as.character(death_raw)) != ""
DTHDTC_v[has_death] <- fmt_yymmdd10(parse_date11_vec(death_raw[has_death]))
# if death_date_raw blank/missing -> missing (SAS leaves missing)

sdtm_dm <- data.frame(
  STUDYID  = rep("ONCO-2026", nrow(m)),
  DOMAIN   = rep("DM", nrow(m)),
  USUBJID  = paste0("ONCO-2026-", m$SUBJID),
  SUBJID   = m$SUBJID,
  SITEID   = SITE_v,
  RFSTDTC  = RFSTDTC_v,
  RFENDTC  = RFENDTC_v,
  RFXSTDTC = RFXSTDTC_v,
  RFXENDTC = RFXENDTC_v,
  RFICDTC  = RFICDTC_v,
  RFPENDTC = RFPENDTC_v,
  BRTHDTC  = BRTHDTC_v,
  AGE      = AGE_v,
  AGEU     = rep("YEARS", nrow(m)),
  SEX      = SEX_v,
  RACE     = RACE_v,
  ETHNIC   = ETHNIC_v,
  ARMCD    = ARMCD_v,
  ARM      = ARM_v,
  ACTARMCD = ACTARMCD_v,
  ACTARM   = ACTARM_v,
  COUNTRY  = rep("IND", nrow(m)),
  DTHFL    = DTHFL_v,
  DTHDTC   = DTHDTC_v,
  stringsAsFactors = FALSE, check.names = FALSE
)

# SAS sorts with uppercase before lowercase; sort by SUBJID for deterministic PRINT
sdtm_dm <- sdtm_dm[order(sdtm_dm$SUBJID), , drop = FALSE]
rownames(sdtm_dm) <- NULL

# STEP 4: PROC PRINT -> listing outputs
# CSV of table body (dataset)
write.csv(sdtm_dm, file.path(out, "P023.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
df_print <- data.frame(Obs = seq_len(nrow(sdtm_dm)), sdtm_dm,
                       stringsAsFactors = FALSE, check.names = FALSE)
# Format AGE without decimals, blank if missing
txt_lines <- c(
  "PROC PRINT DATA=SDTM_DM;",
  paste0("Observations: ", nrow(sdtm_dm)),
  ""
)
prt <- capture.output(print(df_print, right = TRUE, na.print = "", row.names = FALSE))
txt_lines <- c(txt_lines, prt)
txt_out <- paste(txt_lines, collapse = "\n")
writeLines(txt_out, file.path(out, "P023.txt"))
# also write .lst counterpart
writeLines(txt_out, file.path(out, "P023.lst"))
```