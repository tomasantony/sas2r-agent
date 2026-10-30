out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_col <- function(df, target) {
  cn <- names(df)
  idx <- which(toupper(cn) == toupper(target))
  if (length(idx) >= 1) return(cn[idx[1]])
  idx2 <- which(toupper(cn) == toupper(target))
  if (length(idx2) >= 1) return(cn[idx2[1]])
  return(NA_character_)
}
find_col_contains <- function(df, patterns) {
  cn <- names(df)
  up <- toupper(cn)
  for (p in patterns) {
    idx <- grep(p, up)
    if (length(idx) >= 1) return(cn[idx[1]])
  }
  return(NA_character_)
}
get_col <- function(df, target) {
  cname <- find_col(df, target)
  if (is.na(cname)) return(rep(NA_character_, nrow(df)))
  df[[cname]]
}
get_col_raw <- function(df, target) {
  cname <- find_col(df, target)
  if (is.na(cname)) return(NULL)
  df[[cname]]
}

month_map <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)

parse_one_to_iso <- function(v) {
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(format(as.Date(v), "%Y-%m-%d"))
  s <- trimws(as.character(v))
  if (is.na(s) || s == "" || s == "." || toupper(s) == "NA") return(NA_character_)
  # already ISO YYYY-MM-DD (allow datetime prefix)
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)) {
    d <- suppressWarnings(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  # Excel serial as string of digits?
  # DMY with month name: single fixed order DMY
  up <- toupper(s)
  # extract day / mon / year, separators can be -,/,space,.
  m <- regmatches(up, regexec("^([0-9]{1,2})[^A-Z0-9]*([A-Z]{3,9})[^A-Z0-9]*([0-9]{4})$", up))[[1]]
  if (length(m) == 4) {
    dd <- suppressWarnings(as.integer(m[2]))
    mon3 <- substr(m[3], 1, 3)
    yy <- suppressWarnings(as.integer(m[4]))
    if (!is.na(dd) && !is.na(yy) && mon3 %in% names(month_map)) {
      mm <- month_map[[mon3]]
      d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
      if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    }
  }
  # numeric yyyymmdd?
  if (grepl("^[0-9]{8}$", s)) {
    d <- suppressWarnings(as.Date(s, format = "%Y%m%d"))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  return(NA_character_)
}

parse_vec_to_iso <- function(v) {
  if (is.null(v)) return(character(0))
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(format(as.Date(v), "%Y-%m-%d"))
  if (is.numeric(v)) {
    # Excel serial dates (typically 30000-60000) -> convert; otherwise NA
    res <- rep(NA_character_, length(v))
    for (i in seq_along(v)) {
      if (is.na(v[i])) { res[i] <- NA_character_ }
      else if (v[i] > 20000 && v[i] < 60000) {
        d <- suppressWarnings(as.Date(v[i], origin = "1899-12-30"))
        res[i] <- ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
      } else {
        res[i] <- parse_one_to_iso(v[i])
      }
    }
    return(res)
  }
  # character / logical / integer etc
  vapply(v, function(x) {
    if (is.na(x)) return(NA_character_)
    parse_one_to_iso(x)
  }, character(1), USE.NAMES = FALSE)
}

trunc_s <- function(x, n) {
  # SAS LENGTH truncation; NA stays NA
  vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    s <- as.character(v)
    if (is.na(s)) return(NA_character_)
    if (nchar(s, type = "chars") > n) substr(s, 1, n) else s
  }, character(1), USE.NAMES = FALSE)
}

base_raw <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
f_ex   <- file.path(base_raw, "edc_ex.xlsx")
f_dm   <- file.path(base_raw, "edc_dm.xlsx")
f_rand <- file.path(base_raw, "ivrs_rand.xlsx")

edc_ex <- readxl::read_excel(f_ex, guess_max = 100000)
edc_dm <- tryCatch(readxl::read_excel(f_dm, guess_max = 100000), error = function(e) NULL)
ivrs_rand <- tryCatch(readxl::read_excel(f_rand, guess_max = 100000), error = function(e) NULL)

# ------------------------------------------------------------
# 2. Prepare Raw EX Data  (ex_prep)
# ------------------------------------------------------------
# locate columns case-insensitive
c_SUBJID <- find_col(edc_ex, "SUBJID")
if (is.na(c_SUBJID)) {
  # fallback: any column containing SUBJ
  c_SUBJID <- find_col_contains(edc_ex, "SUBJ")
}
c_DOSE <- find_col(edc_ex, "DOSE")
c_STDT <- find_col(edc_ex, "STDT")
c_ENDT <- find_col(edc_ex, "ENDT")
c_TRT  <- find_col(edc_ex, "TRT")
c_UNIT <- find_col(edc_ex, "UNIT")
c_FORM <- find_col(edc_ex, "FORM")
c_FREQ <- find_col(edc_ex, "FREQ")
c_ROUTE <- find_col(edc_ex, "ROUTE")
c_LOT  <- find_col(edc_ex, "LOT")
c_VISIT <- find_col(edc_ex, "VISIT")

stopifnot(!is.na(c_SUBJID))
stopifnot(!is.na(c_DOSE))

raw_SUBJID <- edc_ex[[c_SUBJID]]
raw_DOSE   <- edc_ex[[c_DOSE]]
raw_STDT   <- if (!is.na(c_STDT)) edc_ex[[c_STDT]] else rep(NA, nrow(edc_ex))
raw_ENDT   <- if (!is.na(c_ENDT)) edc_ex[[c_ENDT]] else rep(NA, nrow(edc_ex))
raw_TRT    <- if (!is.na(c_TRT)) edc_ex[[c_TRT]] else rep(NA_character_, nrow(edc_ex))
raw_UNIT   <- if (!is.na(c_UNIT)) edc_ex[[c_UNIT]] else rep(NA_character_, nrow(edc_ex))
raw_FORM   <- if (!is.na(c_FORM)) edc_ex[[c_FORM]] else rep(NA_character_, nrow(edc_ex))
raw_FREQ   <- if (!is.na(c_FREQ)) edc_ex[[c_FREQ]] else rep(NA_character_, nrow(edc_ex))
raw_ROUTE  <- if (!is.na(c_ROUTE)) edc_ex[[c_ROUTE]] else rep(NA_character_, nrow(edc_ex))
raw_LOT    <- if (!is.na(c_LOT)) edc_ex[[c_LOT]] else rep(NA_character_, nrow(edc_ex))
raw_VISIT  <- if (!is.na(c_VISIT)) edc_ex[[c_VISIT]] else rep(NA_character_, nrow(edc_ex))

# USUBJID = catx("-", "ONCO-2026", SUBJID)
subj_chr <- trimws(as.character(raw_SUBJID))
subj_chr[is.na(raw_SUBJID)] <- NA_character_
subj_chr[subj_chr == ""] <- NA_character_
# SAS catx with numeric uses BEST12. -> as.character numeric is fine; strip spaces
USUBJID_all <- ifelse(is.na(subj_chr), "ONCO-2026", paste0("ONCO-2026-", subj_chr))

# DOSE numeric; exclude records where dose was not administered (DOSE ne .)
if (is.numeric(raw_DOSE)) {
  dose_num_all <- raw_DOSE
} else {
  dose_chr <- trimws(as.character(raw_DOSE))
  dose_num_all <- suppressWarnings(as.numeric(dose_chr))
  # values like "" / "." -> NA already
}
keep <- !is.na(dose_num_all)

# Convert Dates to ISO 8601 (STDT/ENDT are date11.)
iso_STDT_all <- parse_vec_to_iso(raw_STDT)
iso_ENDT_all <- parse_vec_to_iso(raw_ENDT)

ex_prep <- data.frame(
  USUBJID = USUBJID_all,
  DOSE = dose_num_all,
  STDT_raw = as.character(raw_STDT),
  ENDT_raw = as.character(raw_ENDT),
  EXSTDTC = iso_STDT_all,
  EXENDTC = iso_ENDT_all,
  TRT = as.character(raw_TRT),
  UNIT = as.character(raw_UNIT),
  FORM = as.character(raw_FORM),
  FREQ = as.character(raw_FREQ),
  ROUTE = as.character(raw_ROUTE),
  LOT = as.character(raw_LOT),
  VISIT_raw = as.character(raw_VISIT),
  stringsAsFactors = FALSE
)
# if STDT ne "" then EXSTDTC=... ; else missing
ex_prep$EXSTDTC[is.na(ex_prep$EXSTDTC)] <- NA_character_
ex_prep$EXENDTC[is.na(ex_prep$EXENDTC)] <- NA_character_
# Exclude records where dose was not administered
ex_prep <- ex_prep[keep, , drop = FALSE]

# sort by USUBJID EXSTDTC (missing first)
ex_prep$EXSTDTC[ex_prep$EXSTDTC == ""] <- NA_character_
ex_prep$EXENDTC[ex_prep$EXENDTC == ""] <- NA_character_
o <- order(ex_prep$USUBJID, ex_prep$EXSTDTC, na.last = FALSE)
ex_prep <- ex_prep[o, , drop = FALSE]

# ------------------------------------------------------------
# 1. Get RFSTDTC from DM  (dm_for_ex) - derive per CDISC
# ------------------------------------------------------------
dm_for_ex <- NULL
# Try to use edc_dm RFSTDTC if present
rf_from_dm <- NULL
if (!is.null(edc_dm) && nrow(edc_dm) > 0) {
  c_dm_subj <- find_col(edc_dm, "SUBJID")
  if (is.na(c_dm_subj)) c_dm_subj <- find_col_contains(edc_dm, "SUBJ")
  c_rf <- find_col(edc_dm, "RFSTDTC")
  if (!is.na(c_rf) && !is.na(c_dm_subj)) {
    dm_subj_chr <- trimws(as.character(edc_dm[[c_dm_subj]]))
    dm_usub <- ifelse(is.na(dm_subj_chr) | dm_subj_chr == "", "ONCO-2026",
                      paste0("ONCO-2026-", dm_subj_chr))
    dm_rf_raw <- edc_dm[[c_rf]]
    dm_rf_iso <- parse_vec_to_iso(dm_rf_raw)
    dm_for_ex <- data.frame(USUBJID = dm_usub, RFSTDTC = dm_rf_iso,
                            stringsAsFactors = FALSE)
    # one per USUBJID (first)
    dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID), , drop = FALSE]
    dm_for_ex <- dm_for_ex[!duplicated(dm_for_ex$USUBJID), , drop = FALSE]
  }
}
if (is.null(dm_for_ex)) {
  # Derive RFSTDTC = first exposure date (min EXSTDTC); fallback to randomisation date
  # min EXSTDTC per USUBJID
  tmp <- ex_prep[, c("USUBJID", "EXSTDTC"), drop = FALSE]
  tmp <- tmp[!is.na(tmp$EXSTDTC), , drop = FALSE]
  if (nrow(tmp) > 0) {
    agg <- aggregate(EXSTDTC ~ USUBJID, data = tmp, FUN = function(x) min(x, na.rm = TRUE))
    names(agg)[2] <- "RFSTDTC"
  } else {
    agg <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  }
  # randomisation date from ivrs_rand
  rand_df <- NULL
  if (!is.null(ivrs_rand) && nrow(ivrs_rand) > 0) {
    c_r_subj <- find_col(ivrs_rand, "SUBJID")
    if (is.na(c_r_subj)) c_r_subj <- find_col_contains(ivrs_rand, "SUBJ")
    # find date column: prefer containing RAND
    cn_up <- toupper(names(ivrs_rand))
    cand <- names(ivrs_rand)
    # prefer RAND+DAT/DT
    pref <- cand[grepl("RAND", cn_up) & (grepl("DAT", cn_up) | grepl("DT", cn_up) | grepl("DTC", cn_up))]
    if (length(pref) == 0) {
      # any date column
      pref <- cand[grepl("DAT", cn_up) | grepl("DTC", cn_up) | grepl("^.*DT$", cn_up)]
    }
    if (length(pref) >= 1 && !is.na(c_r_subj)) {
      r_subj_chr <- trimws(as.character(ivrs_rand[[c_r_subj]]))
      r_usub <- ifelse(is.na(r_subj_chr) | r_subj_chr == "", "ONCO-2026",
                       paste0("ONCO-2026-", r_subj_chr))
      r_date_raw <- ivrs_rand[[pref[1]]]
      r_date_iso <- parse_vec_to_iso(r_date_raw)
      rand_df <- data.frame(USUBJID = r_usub, RANDTC = r_date_iso, stringsAsFactors = FALSE)
      rand_df <- rand_df[!is.na(rand_df$RANDTC) & rand_df$RANDTC != "", , drop = FALSE]
      if (nrow(rand_df) > 0) {
        rand_df <- rand_df[order(rand_df$USUBJID, rand_df$RANDTC), , drop = FALSE]
        rand_df <- rand_df[!duplicated(rand_df$USUBJID), , drop = FALSE]
      }
    }
  }
  if (!is.null(rand_df) && nrow(rand_df) > 0) {
    full_usub <- unique(c(agg$USUBJID, rand_df$USUBJID, unique(ex_prep$USUBJID)))
    dm_for_ex <- data.frame(USUBJID = full_usub, stringsAsFactors = FALSE)
    dm_for_ex <- merge(dm_for_ex, agg, by = "USUBJID", all.x = TRUE, sort = FALSE)
    dm_for_ex <- merge(dm_for_ex, rand_df, by = "USUBJID", all.x = TRUE, sort = FALSE)
    dm_for_ex$RFSTDTC <- ifelse(!is.na(dm_for_ex$RFSTDTC) & dm_for_ex$RFSTDTC != "",
                                dm_for_ex$RFSTDTC, dm_for_ex$RANDTC)
    dm_for_ex$RANDTC <- NULL
  } else {
    dm_for_ex <- agg
    # ensure all ex subjects present (even if RF missing)
    full_usub <- unique(ex_prep$USUBJID)
    miss <- setdiff(full_usub, dm_for_ex$USUBJID)
    if (length(miss) > 0) {
      dm_for_ex <- rbind(dm_for_ex,
                         data.frame(USUBJID = miss, RFSTDTC = NA_character_,
                                    stringsAsFactors = FALSE))
    }
  }
  dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID), , drop = FALSE]
}
# SAS: proc sort out=dm_for_ex by USUBJID (missing first, uppercase before lowercase)
dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID, na.last = FALSE), , drop = FALSE]

# ------------------------------------------------------------
# 4. Create Intermediate EX Dataset (sdtm_ex1) : merge ex_prep(in=a) dm_for_ex
# ------------------------------------------------------------
# Drop/rename overlapping non-key columns before merge to avoid .x/.y
# dm_for_ex has USUBJID, RFSTDTC only; ex_prep has USUBJID + others -> no overlap except key
stopifnot("USUBJID" %in% names(ex_prep))
stopifnot("USUBJID" %in% names(dm_for_ex))
stopifnot("RFSTDTC" %in% names(dm_for_ex))

sdtm_ex1 <- merge(ex_prep, dm_for_ex, by = "USUBJID", all.x = TRUE, sort = FALSE)

# Identifier Variables
sdtm_ex1$STUDYID <- "ONCO-2026"
sdtm_ex1$DOMAIN <- "EX"

# Exposure Variables with LENGTH truncation
sdtm_ex1$EXTRT <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$TRT), NA_character_, sdtm_ex1$TRT)), 40)
sdtm_ex1$EXTRT[sdtm_ex1$TRT == "" | is.na(sdtm_ex1$TRT)] <- NA_character_
# EXDOSE numeric
sdtm_ex1$EXDOSE <- sdtm_ex1$DOSE
sdtm_ex1$EXDOSU <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$UNIT), NA_character_, sdtm_ex1$UNIT)), 10)
sdtm_ex1$EXDOSFRM <- trunc_s(gsub("_", " ", ifelse(is.na(sdtm_ex1$FORM), NA_character_, sdtm_ex1$FORM), fixed = TRUE), 30)
sdtm_ex1$EXDOSFRQ <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$FREQ), NA_character_, sdtm_ex1$FREQ)), 10)
sdtm_ex1$EXVLDIST <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$ROUTE), NA_character_, sdtm_ex1$ROUTE)), 20)
sdtm_ex1$EXLOT <- trunc_s(ifelse(is.na(sdtm_ex1$LOT), NA_character_, as.character(sdtm_ex1$LOT)), 15)
sdtm_ex1$EXSTDTC <- trunc_s(sdtm_ex1$EXSTDTC, 19)
sdtm_ex1$EXENDTC <- trunc_s(sdtm_ex1$EXENDTC, 19)
sdtm_ex1$VISIT <- trunc_s(ifelse(is.na(sdtm_ex1$VISIT_raw), NA_character_, as.character(sdtm_ex1$VISIT_raw)), 40)
sdtm_ex1$STUDYID <- trunc_s(sdtm_ex1$STUDYID, 10)
sdtm_ex1$DOMAIN <- trunc_s(sdtm_ex1$DOMAIN, 2)
sdtm_ex1$USUBJID <- trunc_s(sdtm_ex1$USUBJID, 25)

# Derive EXSTDY / EXENDY
sdtm_ex1$EXSTDY <- NA_real_
sdtm_ex1$EXENDY <- NA_real_
for (i in seq_len(nrow(sdtm_ex1))) {
  a <- sdtm_ex1$EXSTDTC[i]; b <- sdtm_ex1$RFSTDTC[i]
  if (!is.na(a) && a != "" && !is.na(b) && b != "") {
    stdt <- suppressWarnings(as.Date(a, format = "%Y-%m-%d"))
    rfdt <- suppressWarnings(as.Date(b, format = "%Y-%m-%d"))
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      sdtm_ex1$EXSTDY[i] <- ifelse(stdt >= rfdt, diff + 1, diff)
    }
  }
  c1 <- sdtm_ex1$EXENDTC[i]; b2 <- sdtm_ex1$RFSTDTC[i]
  if (!is.na(c1) && c1 != "" && !is.na(b2) && b2 != "") {
    endt <- suppressWarnings(as.Date(c1, format = "%Y-%m-%d"))
    rfdt2 <- suppressWarnings(as.Date(b2, format = "%Y-%m-%d"))
    if (!is.na(endt) && !is.na(rfdt2)) {
      diff2 <- as.integer(endt - rfdt2)
      sdtm_ex1$EXENDY[i] <- ifelse(endt >= rfdt2, diff2 + 1, diff2)
    }
  }
}

# ------------------------------------------------------------
# 5. Sort Final EX Dataset by USUBJID EXSTDTC
# ------------------------------------------------------------
sdtm_ex1 <- sdtm_ex1[order(sdtm_ex1$USUBJID, sdtm_ex1$EXSTDTC, na.last = FALSE), , drop = FALSE]

# ------------------------------------------------------------
# 6. Derive EXSEQ (by USUBJID, retain)
# ------------------------------------------------------------
sdtm_ex1$EXSEQ <- NA_integer_
if (nrow(sdtm_ex1) > 0) {
  cur <- NA_character_
  seqn <- 0L
  for (i in seq_len(nrow(sdtm_ex1))) {
    if (is.na(cur) || is.na(sdtm_ex1$USUBJID[i]) || sdtm_ex1$USUBJID[i] != cur) {
      cur <- sdtm_ex1$USUBJID[i]
      seqn <- 1L
    } else {
      seqn <- seqn + 1L
    }
    sdtm_ex1$EXSEQ[i] <- seqn
  }
}

sdtm_ex <- sdtm_ex1[, c("STUDYID","DOMAIN","USUBJID","EXSEQ","EXTRT","EXDOSE",
                        "EXDOSU","EXDOSFRM","EXDOSFRQ","EXVLDIST","EXLOT",
                        "EXSTDTC","EXENDTC","EXSTDY","EXENDY","VISIT"), drop = FALSE]

# ------------------------------------------------------------
# 7. Outputs: CSV + text listing (PROC PRINT)
# ------------------------------------------------------------
fmt_val <- function(x) {
  if (is.na(x) || (is.character(x) && x == "")) return("")
  as.character(x)
}

write.csv(sdtm_ex, file.path(out, "P025.csv"), row.names = FALSE, na = "")

# Build PROC PRINT style text
hdr <- c("Obs","STUDYID","DOMAIN","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU",
         "EXDOSFRM","EXDOSFRQ","EXVLDIST","EXLOT","EXSTDTC","EXENDTC","EXSTDY","EXENDY","VISIT")
lines <- c("PROC PRINT DATA=SDTM_EX", "", paste(hdr, collapse = " | "))
if (nrow(sdtm_ex) > 0) {
  for (i in seq_len(nrow(sdtm_ex))) {
    r <- sdtm_ex[i, , drop = FALSE]
    vals <- c(as.character(i),
              fmt_val(r$STUDYID), fmt_val(r$DOMAIN), fmt_val(r$USUBJID),
              fmt_val(r$EXSEQ), fmt_val(r$EXTRT), fmt_val(r$EXDOSE),
              fmt_val(r$EXDOSU), fmt_val(r$EXDOSFRM), fmt_val(r$EXDOSFRQ),
              fmt_val(r$EXVLDIST), fmt_val(r$EXLOT), fmt_val(r$EXSTDTC),
              fmt_val(r$EXENDTC), fmt_val(r$EXSTDY), fmt_val(r$EXENDY),
              fmt_val(r$VISIT))
    lines <- c(lines, paste(vals, collapse = " | "))
  }
} else {
  lines <- c(lines, "(No observations)")
}
writeLines(lines, file.path(out, "P025.txt"))
# exact .lst counterpart
writeLines(lines, file.path(out, "P025.lst"))
# CSV of table body (same as dataset, with Obs)
tbl <- cbind(Obs = seq_len(nrow(sdtm_ex)), sdtm_ex)
write.csv(tbl, file.path(out, "P025_table.csv"), row.names = FALSE, na = "")
file.copy(file.path(out, "P025.csv"), file.path(out, "P025.lst.csv"), overwrite = TRUE)
