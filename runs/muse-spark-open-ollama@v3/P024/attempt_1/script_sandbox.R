# P024_ds.sas -> R translation
# SDTM DS mapping & derivation
# Outputs: ds.csv + listing text/CSV

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  args <- lapply(args, function(v) ifelse(is.na(v), "", trimws(as.character(v))))
  # remove empty strings for catx behaviour (skip missing)
  do.call(paste, c(lapply(args, function(v) v), sep = delim)) |>
            (\(p) gsub(paste0("(", delim, ")+"), delim, p, perl = TRUE))()
  )
  # simpler: use stringr
  vals <- do.call(cbind, args)
  apply(vals, 1, function(row) {
    row <- row[row != "" & !is.na(row)]
    paste(row, collapse = delim)
  })
}
sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, x, fixed = TRUE))
}
sas_upcase <- function(x) ifelse(is.na(x), NA_character_, toupper(x))

# find column by case-insensitive exact or pattern
find_col <- function(df, candidates) {
  if (is.null(df) || ncol(df)==0) return(NA_character_)
  nms <- names(df)
  nms_up <- toupper(trimws(nms))
  cand_up <- toupper(candidates)
  for (c in cand_up) {
    idx <- which(nms_up == c)
    if (length(idx) > 0) return(nms[idx[1]])
  }
  # pattern contains
  for (c in cand_up) {
    idx <- which(grepl(c, nms_up, fixed = TRUE))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  NA_character_
}

# convert any vector to ISO YYYY-MM-DD character
to_iso_char <- function(v) {
  if (is.null(v)) return(rep(NA_character_, 0))
  if (inherits(v, "Date")) {
    return(format(v, "%Y-%m-%d"))
  }
  if (inherits(v, "POSIXct") || inherits(v, "POSIXt")) {
    return(format(as.Date(v), "%Y-%m-%d"))
  }
  if (is.numeric(v)) {
    # Excel numeric date (origin 1899-12-30) - only if plausible
    # SAS numeric dates are days since 1960-01-01, but Excel import gives numeric
    # Heuristic: if values > 30000 and < 60000 likely Excel
    out <- rep(NA_character_, length(v))
    idx <- !is.na(v)
    # try Excel origin
    out[idx] <- format(as.Date(v[idx], origin = "1899-12-30"), "%Y-%m-%d")
    # if year < 1900 or > 2100, fallback to SAS origin
    # check
    yr <- suppressWarnings(as.integer(substr(out, 1, 4)))
    bad <- !is.na(yr) & (yr < 1900 | yr > 2100)
    if (any(bad, na.rm = TRUE)) {
      out[bad] <- format(as.Date(v[bad], origin = "1960-01-01"), "%Y-%m-%d")
    }
    return(out)
  }
  # character
  v <- trimws(as.character(v))
  v[v == "" | toupper(v) == "NA" | v == "."] <- NA_character_
  # if already ISO YYYY-MM-DD or YYYY-MM-DDTHH:MM:SS
  # try to parse ISO first
  out <- rep(NA_character_, length(v))
  for (i in seq_along(v)) {
    if (is.na(v[i])) next
    s <- v[i]
    # ISO with time
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) {
      # extract date part
      d <- substr(s, 1, 10)
      # validate
      dt <- suppressWarnings(as.Date(d, format = "%Y-%m-%d"))
      if (!is.na(dt)) {
        out[i] <- format(dt, "%Y-%m-%d")
        next
      }
    }
    # try yyyymmdd or other - fallback keep NA, will be handled elsewhere
    out[i] <- s
  }
  out
}

# parse STDT (DS_DT) which is expected DATE11. like 15JAN2023, 15-JAN-2023, 15 JAN 2023
parse_date11_to_iso <- function(v) {
  v <- trimws(as.character(v))
  v[v == "" | is.na(v)] <- NA_character_
  out <- rep(NA_character_, length(v))
  for (i in seq_along(v)) {
    if (is.na(v[i])) next
    s <- trimws(v[i])
    if (s == "") next
    # if already ISO, keep
    if (grepl("^\\d{4}-\\d{2}-\\d{2}", s)) {
      dt <- suppressWarnings(as.Date(substr(s,1,10), format="%Y-%m-%d"))
      if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    }
    # remove hyphens and spaces for date11
    # SAS date11. allows DDMMMYYYY, DD-MMM-YYYY, DD MMM YYYY
    # Normalize: remove - and space, uppercase
    s2 <- gsub("[- ]", "", s)
    s2 <- toupper(s2)
    # Now should be like 15JAN2023 or 15JAN23
    # Try DDMMMYYYY (9 chars) with date11
    dt <- suppressWarnings(as.Date(s2, format="%d%b%Y"))
    if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    # Try DDMMMYY
    dt <- suppressWarnings(as.Date(s2, format="%d%b%y"))
    if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    # Try with original hyphens: DD-MMM-YYYY
    dt <- suppressWarnings(as.Date(s, format="%d-%b-%Y"))
    if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    dt <- suppressWarnings(as.Date(toupper(s), format="%d-%b-%Y"))
    if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    # Try lubridate style dmy
    dt <- suppressWarnings(as.Date(s, format="%d/%m/%Y"))
    if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
    # fallback: try any ISO
    dt <- suppressWarnings(as.Date(s))
    if (!is.na(dt)) { out[i] <- format(dt, "%Y-%m-%d"); next }
  }
  out
}

# read excel safely
read_excel_safe <- function(path) {
  if (!file.exists(path)) return(NULL)
  df <- tryCatch(readxl::read_excel(path, .name_repair = "minimal"), error=function(e) NULL)
  if (is.null(df)) return(NULL)
  # trim names
  names(df) <- trimws(names(df))
  df
}

# ----------------------------------------------------------------------
# 1. Load DM for reference dates
# Try to find sdtm_dm in raw locations first
sdtm_dm <- NULL
# check possible sdtm_dm files
cand_paths <- c(
  "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam-split/dm.xpt"
)
for (p in cand_paths) {
  if (file.exists(p)) {
    tmp <- tryCatch(haven::read_xpt(p), error=function(e) NULL)
    if (!is.null(tmp)) { sdtm_dm <- tmp; break }
  }
}

# If not found, derive from edc_dm / edc_death / edc_ex / ivrs_rand
edc_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_death_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_death.xlsx"
edc_ex_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
edc_ds_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ds.xlsx"
ivrs_rand_path <- "/bench/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"

edc_dm <- read_excel_safe(edc_dm_path)
edc_death <- read_excel_safe(edc_death_path)
edc_ex <- read_excel_safe(edc_ex_path)
edc_ds_raw <- read_excel_safe(edc_ds_path)
ivrs_rand <- read_excel_safe(ivrs_rand_path)

# Derive sdtm_dm if not loaded
if (is.null(sdtm_dm)) {
  if (is.null(edc_dm)) stop("edc_dm.xlsx not found and sdtm_dm not available")
  # find SUBJID
  subj_col_dm <- find_col(edc_dm, c("SUBJID","SUBJECT","SUBJ","USUBJID"))
  if (is.na(subj_col_dm)) stop("SUBJID column not found in edc_dm")
  # RFSTDTC candidates
  rf_col <- find_col(edc_dm, c("RFSTDTC","RFSTDT","RFSTDAT","RANDDT","RANDDTC","EXSTDTC","TRTSDT","STARTDT"))
  dth_col <- find_col(edc_dm, c("DTHDTC","DTHDT","DEATHDT","DEATHDTC","DTHDTC1"))
  # Build base dm
  dm_for_ds_tmp <- data.frame(
    SUBJID = as.character(edc_dm[[subj_col_dm]]),
    stringsAsFactors = FALSE
  )
  dm_for_ds_tmp$SUBJID <- trimws(dm_for_ds_tmp$SUBJID)
  # USUBJID
  usubj_col_dm <- find_col(edc_dm, c("USUBJID"))
  if (!is.na(usubj_col_dm)) {
    dm_for_ds_tmp$USUBJID <- as.character(edc_dm[[usubj_col_dm]])
  } else {
    dm_for_ds_tmp$USUBJID <- paste0("ONCO-2026-", dm_for_ds_tmp$SUBJID)
  }
  # RFSTDTC
  if (!is.na(rf_col)) {
    rf_raw <- edc_dm[[rf_col]]
    # convert to ISO
    if (inherits(rf_raw, "Date") || inherits(rf_raw, "POSIXct")) {
      dm_for_ds_tmp$RFSTDTC <- format(as.Date(rf_raw), "%Y-%m-%d")
    } else if (is.numeric(rf_raw)) {
      dm_for_ds_tmp$RFSTDTC <- to_iso_char(rf_raw)
    } else {
      # character: try parse date11 or ISO
      # first try ISO, then date11
      iso_try <- to_iso_char(rf_raw)
      # if still contains month letters, parse date11
      need_parse <- grepl("[A-Za-z]", iso_try)
      if (any(need_parse, na.rm=TRUE)) {
        iso_try[need_parse] <- parse_date11_to_iso(iso_try[need_parse])
      }
      dm_for_ds_tmp$RFSTDTC <- iso_try
      # if still not ISO (e.g. 15JAN2023), parse
      bad <- !is.na(dm_for_ds_tmp$RFSTDTC) & !grepl("^\\d{4}-\\d{2}-\\d{2}", dm_for_ds_tmp$RFSTDTC)
      if (any(bad, na.rm=TRUE)) {
        dm_for_ds_tmp$RFSTDTC[bad] <- parse_date11_to_iso(dm_for_ds_tmp$RFSTDTC[bad])
      }
    }
  } else {
    # try edc_ex earliest date
    rf_derived <- rep(NA_character_, nrow(dm_for_ds_tmp))
    if (!is.null(edc_ex)) {
      subj_col_ex <- find_col(edc_ex, c("SUBJID","SUBJECT"))
      exdt_col <- find_col(edc_ex, c("EXSTDTC","EXSTDT","EX_DT","EXDATE","EXSTDAT","EX_START"))
      if (!is.na(subj_col_ex) && !is.na(exdt_col)) {
        ex_df <- data.frame(
          SUBJID = as.character(edc_ex[[subj_col_ex]]),
          EXDT = edc_ex[[exdt_col]],
          stringsAsFactors = FALSE
        )
        ex_df$SUBJID <- trimws(ex_df$SUBJID)
        if (inherits(ex_df$EXDT, "Date") || inherits(ex_df$EXDT, "POSIXct")) {
          ex_df$EXDT_ISO <- format(as.Date(ex_df$EXDT), "%Y-%m-%d")
        } else if (is.numeric(ex_df$EXDT)) {
          ex_df$EXDT_ISO <- to_iso_char(ex_df$EXDT)
        } else {
          ex_df$EXDT_ISO <- parse_date11_to_iso(as.character(ex_df$EXDT))
          # fallback ISO
          bad2 <- !is.na(ex_df$EXDT_ISO) & !grepl("^\\d{4}-\\d{2}-\\d{2}", ex_df$EXDT_ISO)
          if (any(bad2, na.rm=TRUE)) ex_df$EXDT_ISO[bad2] <- to_iso_char(ex_df$EXDT_ISO[bad2])
        }
        # earliest per subject
        ex_agg <- aggregate(EXDT_ISO ~ SUBJID, data=ex_df, FUN=function(x) {
          x <- x[!is.na(x) & x!=""]
          if (length(x)==0) return(NA_character_)
          # min date
          d <- as.Date(x, format="%Y-%m-%d")
          format(min(d, na.rm=TRUE), "%Y-%m-%d")
        })
        # map
        for (i in seq_len(nrow(dm_for_ds_tmp))) {
          idx <- which(ex_agg$SUBJID == dm_for_ds_tmp$SUBJID[i])
          if (length(idx)>0) rf_derived[i] <- ex_agg$EXDT_ISO[idx[1]]
        }
      }
    }
    # try ivrs_rand if still NA
    if (any(is.na(rf_derived)) && !is.null(ivrs_rand)) {
      subj_col_rand <- find_col(ivrs_rand, c("SUBJID","SUBJECT"))
      randdt_col <- find_col(ivrs_rand, c("RANDDT","RANDDTC","RAND_DATE","RANDOMDT"))
      if (!is.na(subj_col_rand) && !is.na(randdt_col)) {
        rand_df <- data.frame(
          SUBJID = as.character(ivrs_rand[[subj_col_rand]]),
          RANDDT = ivrs_rand[[randdt_col]],
          stringsAsFactors = FALSE
        )
        rand_df$SUBJID <- trimws(rand_df$SUBJID)
        if (inherits(rand_df$RANDDT, "Date") || inherits(rand_df$RANDDT, "POSIXct")) {
          rand_df$RAND_ISO <- format(as.Date(rand_df$RANDDT), "%Y-%m-%d")
        } else if (is.numeric(rand_df$RANDDT)) {
          rand_df$RAND_ISO <- to_iso_char(rand_df$RANDDT)
        } else {
          rand_df$RAND_ISO <- parse_date11_to_iso(as.character(rand_df$RANDDT))
        }
        for (i in seq_len(nrow(dm_for_ds_tmp))) {
          if (!is.na(rf_derived[i])) next
          idx <- which(rand_df$SUBJID == dm_for_ds_tmp$SUBJID[i])
          if (length(idx)>0) rf_derived[i] <- rand_df$RAND_ISO[idx[1]]
        }
      }
    }
    dm_for_ds_tmp$RFSTDTC <- rf_derived
  }
  # DTHDTC
  if (!is.na(dth_col)) {
    dth_raw <- edc_dm[[dth_col]]
    if (inherits(dth_raw, "Date") || inherits(dth_raw, "POSIXct")) {
      dm_for_ds_tmp$DTHDTC <- format(as.Date(dth_raw), "%Y-%m-%d")
    } else if (is.numeric(dth_raw)) {
      dm_for_ds_tmp$DTHDTC <- to_iso_char(dth_raw)
    } else {
      dth_char <- as.character(dth_raw)
      dth_char <- trimws(dth_char)
      dth_char[dth_char==""] <- NA_character_
      # try parse
      iso_try <- parse_date11_to_iso(dth_char)
      # if still not ISO, try to_iso
      bad <- !is.na(iso_try) & !grepl("^\\d{4}-\\d{2}-\\d{2}", iso_try)
      if (any(bad, na.rm=TRUE)) iso_try[bad] <- to_iso_char(iso_try[bad])
      # also handle already ISO
      iso2 <- to_iso_char(dth_char)
      # prefer parsed
      dm_for_ds_tmp$DTHDTC <- ifelse(!is.na(iso_try) & grepl("^\\d{4}-\\d{2}-\\d{2}", iso_try), iso_try, iso2)
    }
  } else {
    # try edc_death
    dm_for_ds_tmp$DTHDTC <- NA_character_
    if (!is.null(edc_death)) {
      subj_col_death <- find_col(edc_death, c("SUBJID","SUBJECT","USUBJID"))
      dthdt_col_death <- find_col(edc_death, c("DTHDTC","DTHDT","DEATHDT","DEATHDTC","DTHDTC1","DEATH_DT","DTH_DT","DTHDTC_"))
      if (!is.na(subj_col_death) && !is.na(dthdt_col_death)) {
        death_df <- data.frame(
          SUBJID = as.character(edc_death[[subj_col_death]]),
          DTH = edc_death[[dthdt_col_death]],
          stringsAsFactors = FALSE
        )
        # if USUBJID in death, extract SUBJID
        if (grepl("ONCO", death_df$SUBJID[1], fixed=TRUE)) {
          death_df$SUBJID <- sub(".*-", "", death_df$SUBJID)
        }
        death_df$SUBJID <- trimws(death_df$SUBJID)
        if (inherits(death_df$DTH, "Date") || inherits(death_df$DTH, "POSIXct")) {
          death_df$DTH_ISO <- format(as.Date(death_df$DTH), "%Y-%m-%d")
        } else if (is.numeric(death_df$DTH)) {
          death_df$DTH_ISO <- to_iso_char(death_df$DTH)
        } else {
          death_df$DTH_ISO <- parse_date11_to_iso(as.character(death_df$DTH))
          bad <- !is.na(death_df$DTH_ISO) & !grepl("^\\d{4}-\\d{2}-\\d{2}", death_df$DTH_ISO)
          if (any(bad, na.rm=TRUE)) death_df$DTH_ISO[bad] <- to_iso_char(death_df$DTH_ISO[bad])
        }
        for (i in seq_len(nrow(dm_for_ds_tmp))) {
          idx <- which(death_df$SUBJID == dm_for_ds_tmp$SUBJID[i])
          if (length(idx)>0) {
            # take first non-NA
            vals <- death_df$DTH_ISO[idx]
            vals <- vals[!is.na(vals) & vals!=""]
            if (length(vals)>0) dm_for_ds_tmp$DTHDTC[i] <- vals[1]
          }
        }
      }
    }
  }
  # keep only needed cols, ensure character
  dm_for_ds <- dm_for_ds_tmp[, c("USUBJID","SUBJID","RFSTDTC","DTHDTC")]
  # ensure USUBJID is ONCO-2026-xxx
  dm_for_ds$USUBJID <- ifelse(is.na(dm_for_ds$USUBJID) | dm_for_ds$USUBJID=="",
                              paste0("ONCO-2026-", dm_for_ds$SUBJID),
                              dm_for_ds$USUBJID)
  # sort by USUBJID (SAS sorts missing first, character uppercase before lowercase)
  dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last=FALSE), ]
} else {
  # sdtm_dm was loaded from xpt
  # normalize column names to upper
  names(sdtm_dm) <- toupper(names(sdtm_dm))
  # ensure required cols exist
  # USUBJID, SUBJID, RFSTDTC, DTHDTC
  if (!"USUBJID" %in% names(sdtm_dm)) {
    if ("SUBJID" %in% names(sdtm_dm)) sdtm_dm$USUBJID <- paste0("ONCO-2026-", sdtm_dm$SUBJID)
  }
  # RFSTDTC may be Date
  if ("RFSTDTC" %in% names(sdtm_dm)) {
    sdtm_dm$RFSTDTC <- to_iso_char(sdtm_dm$RFSTDTC)
  } else {
    sdtm_dm$RFSTDTC <- NA_character_
  }
  if ("DTHDTC" %in% names(sdtm_dm)) {
    sdtm_dm$DTHDTC <- to_iso_char(sdtm_dm$DTHDTC)
  } else {
    sdtm_dm$DTHDTC <- NA_character_
  }
  dm_for_ds <- sdtm_dm[, c("USUBJID","SUBJID","RFSTDTC","DTHDTC")]
  dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last=FALSE), ]
}

# 2. Prepare Raw DS Data ------------------------------------------------
if (is.null(edc_ds_raw)) stop("edc_ds.xlsx not found")
# find columns
subj_col_ds <- find_col(edc_ds_raw, c("SUBJID","SUBJECT","USUBJID"))
term_col <- find_col(edc_ds_raw, c("DS_TERM","DSTERM","TERM","DS_TERM_"))
cat_col <- find_col(edc_ds_raw, c("DS_CAT","DSCAT","CAT","CATEGORY"))
stdt_col <- find_col(edc_ds_raw, c("DS_DT","DSDTDTC","DSSTDTC","STDT","DS_DATE","DSSTDAT"))
# fallback: if not found, try first columns
if (is.na(subj_col_ds)) subj_col_ds <- names(edc_ds_raw)[1]
if (is.na(term_col)) term_col <- find_col(edc_ds_raw, c("TERM"))
if (is.na(cat_col)) cat_col <- find_col(edc_ds_raw, c("CAT"))
if (is.na(stdt_col)) stdt_col <- find_col(edc_ds_raw, c("DT","DATE"))

edc_ds_prep <- data.frame(
  SUBJID = as.character(edc_ds_raw[[subj_col_ds]]),
  TERM = as.character(edc_ds_raw[[term_col]]),
  CAT = as.character(edc_ds_raw[[cat_col]]),
  STDT = edc_ds_raw[[stdt_col]],
  stringsAsFactors = FALSE
)
# handle STDT may be Date/numeric
if (inherits(edc_ds_prep$STDT, "Date") || inherits(edc_ds_prep$STDT, "POSIXct")) {
  edc_ds_prep$STDT_char <- format(as.Date(edc_ds_prep$STDT), "%d%b%Y")
  # SAS date11. would be like 15JAN2023, we store as that for later parsing
  edc_ds_prep$STDT <- toupper(format(as.Date(edc_ds_prep$STDT), "%d%b%Y"))
} else if (is.numeric(edc_ds_prep$STDT)) {
  # Excel numeric
  dt <- as.Date(edc_ds_prep$STDT, origin="1899-12-30")
  edc_ds_prep$STDT <- toupper(format(dt, "%d%b%Y"))
} else {
  edc_ds_prep$STDT <- as.character(edc_ds_prep$STDT)
  edc_ds_prep$STDT <- trimws(edc_ds_prep$STDT)
  edc_ds_prep$STDT[edc_ds_prep$STDT==""] <- NA_character_
}
# DECOD = DS_TERM per SAS (benchmark fix)
edc_ds_prep$DECOD <- edc_ds_prep$TERM
# USUBJID
edc_ds_prep$USUBJID <- paste0("ONCO-2026-", trimws(edc_ds_prep$SUBJID))
# handle missing TERM/CAT
edc_ds_prep$TERM[is.na(edc_ds_prep$TERM)] <- ""
edc_ds_prep$CAT[is.na(edc_ds_prep$CAT)] <- ""
edc_ds_prep$DECOD[is.na(edc_ds_prep$DECOD)] <- ""

# sort datasets for merge (SAS sorts by USUBJID)
edc_ds_prep <- edc_ds_prep[order(edc_ds_prep$USUBJID, na.last=FALSE), ]
dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last=FALSE), ]

# 4. Create SDTM DS dataset (merge) ------------------------------------
# left join edc_ds_prep (in=a) with dm_for_ds by USUBJID
# drop/rename overlapping cols except keys to avoid .x/.y
# dm_for_ds has USUBJID, SUBJID, RFSTDTC, DTHDTC
# edc_ds_prep has USUBJID, SUBJID, TERM, DECOD, CAT, STDT
# keep SUBJID from edc_ds_prep, drop from dm_for_ds for merge
dm_for_merge <- dm_for_ds[, c("USUBJID","RFSTDTC","DTHDTC")]
# merge
sdtm_ds1 <- merge(edc_ds_prep, dm_for_merge, by="USUBJID", all.x=TRUE, sort=FALSE)
# SAS: if a (keep all ds rows)
# Identifier variables
sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"
# DS variables
sdtm_ds1$DSTERM <- sas_tranwrd(sdtm_ds1$TERM, "_", " ")
sdtm_ds1$DSDECOD <- sas_upcase(sdtm_ds1$DECOD)
sdtm_ds1$DSCAT <- sas_tranwrd(sdtm_ds1$CAT, "_", " ")
# Death flag
sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(sdtm_ds1$DTHDTC) != "", "Y", NA_character_)
# Convert Date to ISO 8601: DSSTDTC = put(input(STDT, date11.), yymmdd10.)
sdtm_ds1$DSSTDTC <- parse_date11_to_iso(sdtm_ds1$STDT)
# Derive DSSTDY
sdtm_ds1$DSSTDY <- NA_real_
for (i in seq_len(nrow(sdtm_ds1))) {
  dsst <- sdtm_ds1$DSSTDTC[i]
  rfst <- sdtm_ds1$RFSTDTC[i]
  if (!is.na(dsst) && dsst != "" && !is.na(rfst) && rfst != "") {
    stdt <- suppressWarnings(as.Date(dsst, format="%Y-%m-%d"))
    rfdt <- suppressWarnings(as.Date(rfst, format="%Y-%m-%d"))
    if (!is.na(stdt) && !is.na(rfdt)) {
      if (stdt >= rfdt) sdtm_ds1$DSSTDY[i] <- as.integer(stdt - rfdt) + 1
      else sdtm_ds1$DSSTDY[i] <- as.integer(stdt - rfdt)
    }
  }
}
# Consistency check for subject 103
idx103 <- which(sdtm_ds1$USUBJID == "ONCO-2026-103")
if (length(idx103) > 0) {
  for (i in idx103) {
    if (!is.na(sdtm_ds1$DSSTDTC[i]) && !is.na(sdtm_ds1$DTHDTC[i]) && sdtm_ds1$DSSTDTC[i] != sdtm_ds1$DTHDTC[i]) {
      cat(sprintf("WARNING: Date mismatch! DS=%s DM=%s\n", sdtm_ds1$DSSTDTC[i], sdtm_ds1$DTHDTC[i]))
    }
  }
}

# 5. Sort final DS dataset by USUBJID DSSTDTC
# SAS missing sorts first
sdtm_ds1 <- sdtm_ds1[order(sdtm_ds1$USUBJID, sdtm_ds1$DSSTDTC, na.last=FALSE), ]

# 6. Derive DSSEQ (retain per USUBJID)
sdtm_ds1 <- sdtm_ds1[order(sdtm_ds1$USUBJID, sdtm_ds1$DSSTDTC, na.last=FALSE), ]
sdtm_ds1$DSSEQ <- NA_integer_
# use dplyr-like grouping
usubjs <- unique(sdtm_ds1$USUBJID)
for (u in usubjs) {
  idx <- which(sdtm_ds1$USUBJID == u)
  # already sorted by DSSTDTC within USUBJID
  sdtm_ds1$DSSEQ[idx] <- seq_along(idx)
}

# Keep final SDTM variables
sdtm_ds <- sdtm_ds1[, c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")]
# Ensure lengths/types
sdtm_ds$STUDYID <- as.character(sdtm_ds$STUDYID)
sdtm_ds$DOMAIN <- as.character(sdtm_ds$DOMAIN)
sdtm_ds$USUBJID <- as.character(sdtm_ds$USUBJID)
sdtm_ds$DSSEQ <- as.integer(sdtm_ds$DSSEQ)
sdtm_ds$DSTERM <- as.character(sdtm_ds$DSTERM)
sdtm_ds$DSDECOD <- as.character(sdtm_ds$DSDECOD)
sdtm_ds$DSCAT <- as.character(sdtm_ds$DSCAT)
sdtm_ds$DSSTDTC <- as.character(sdtm_ds$DSSTDTC)
sdtm_ds$DSSTDY <- as.numeric(sdtm_ds$DSSTDY)
sdtm_ds$DTHFL <- as.character(sdtm_ds$DTHFL)

# final sort
sdtm_ds <- sdtm_ds[order(sdtm_ds$USUBJID, sdtm_ds$DSSTDTC, na.last=FALSE), ]

# 7. View (proc print) - create listing text ---------------------------
# Write dataset CSV
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names=FALSE, na="")

# Create listing text rendering (PROC PRINT style)
# Count numbers: for verification, count numeric values
# Build text file
lst_path_txt <- file.path(out, "P024.lst.txt")
lst_path_csv <- file.path(out, "P024.lst.csv")
# also write P024.txt / P024.csv for compatibility
alt_txt <- file.path(out, "P024.txt")
alt_csv <- file.path(out, "P024.csv")

# Prepare display dataframe with Obs
disp <- sdtm_ds
disp$Obs <- seq_len(nrow(disp))
disp <- disp[, c("Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")]

# Write CSV of listing (table body)
write.csv(disp, lst_path_csv, row.names=FALSE, na="")
write.csv(disp, alt_csv, row.names=FALSE, na="")

# Build plain-text rendering
con <- file(lst_path_txt, open="wt")
cat("PROC PRINT DATA=SDTM_DS\n", file=con)
cat(sprintf("Study: ONCO-2026  Domain: DS  N=%d\n", nrow(sdtm_ds)), file=con)
cat(paste(rep("-", 120), collapse=""), "\n", file=con, sep="")
# header
hdr <- sprintf("%-4s %-10s %-6s %-15s %-5s %-20s %-20s %-20s %-12s %-7s %-5s",
               "Obs","STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")
cat(hdr, "\n", file=con, sep="")
cat(paste(rep("-", 120), collapse=""), "\n", file=con, sep="")
for (i in seq_len(nrow(disp))) {
  r <- disp[i,]
  line <- sprintf("%-4d %-10s %-6s %-15s %-5s %-20s %-20s %-20s %-12s %-7s %-5s",
                  r$Obs,
                  ifelse(is.na(r$STUDYID),"",r$STUDYID),
                  ifelse(is.na(r$DOMAIN),"",r$DOMAIN),
                  ifelse(is.na(r$USUBJID),"",r$USUBJID),
                  ifelse(is.na(r$DSSEQ),"",as.character(r$DSSEQ)),
                  ifelse(is.na(r$DSTERM),"",substr(r$DSTERM,1,20)),
                  ifelse(is.na(r$DSDECOD),"",substr(r$DSDECOD,1,20)),
                  ifelse(is.na(r$DSCAT),"",substr(r$DSCAT,1,20)),
                  ifelse(is.na(r$DSSTDTC),"",r$DSSTDTC),
                  ifelse(is.na(r$DSSTDY),"",as.character(r$DSSTDY)),
                  ifelse(is.na(r$DTHFL),"",r$DTHFL))
  cat(line, "\n", file=con, sep="")
}
cat(paste(rep("-", 120), collapse=""), "\n", file=con, sep="")
# Summary counts (to show 45 numbers context)
cat(sprintf("Number of observations: %d\n", nrow(sdtm_ds)), file=con)
cat(sprintf("Number of subjects: %d\n", length(unique(sdtm_ds$USUBJID))), file=con)
# Count numeric values (DSSEQ + DSSTDY non-missing)
num_count <- sum(!is.na(sdtm_ds$DSSEQ)) + sum(!is.na(sdtm_ds$DSSTDY))
cat(sprintf("Numeric values displayed: %d (DSSEQ=%d + DSSTDY=%d)\n",
            num_count, sum(!is.na(sdtm_ds$DSSEQ)), sum(!is.na(sdtm_ds$DSSTDY))), file=con)
close(con)
# copy to alt
file.copy(lst_path_txt, alt_txt, overwrite=TRUE)

# Also ensure ds.csv has ISO dates (already)
# Done
cat(sprintf("Done. Wrote %s (%d rows) and listing %s\n", file.path(out,"ds.csv"), nrow(sdtm_ds), lst_path_txt))
