```r
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

month_map <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)

parse_one_to_iso <- function(v) {
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(format(as.Date(v), "%Y-%m-%d"))
  s <- trimws(as.character(v))
  if (is.na(s) || s == "" || s == "." || toupper(s) == "NA") return(NA_character_)
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s)) {
    d <- suppressWarnings(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
  }
  up <- toupper(s)
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
  vapply(v, function(x) {
    if (is.na(x)) return(NA_character_)
    parse_one_to_iso(x)
  }, character(1), USE.NAMES = FALSE)
}

trunc_s <- function(x, n) {
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
c_SUBJID <- find_col(edc_ex, "SUBJID")
if (is.na(c_SUBJID)) c_SUBJID <- find_col_contains(edc_ex, "SUBJ")
c_DOSE <- find_col(edc_ex, "DOSE")
c_STDT <- find_col(edc_ex, "STDT")
c_ENDT <- find_col(edc_ex, "ENDT")
c_TRT  <- find_col(edc_ex, "TRT")
c_UNIT <- find_col(edc_ex, "UNIT")
c_FORM <- find_col(edc_ex, "FORM")
c_FREQ <- find_col(edc_ex, "FREQ")
c_ROUTE <- find_col(edc_ex, "ROUTE")
c_LOT  <- find_col(edc_ex, "LOT")

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

subj_chr <- trimws(as.character(raw_SUBJID))
subj_chr[is.na(raw_SUBJID)] <- NA_character_
subj_chr[subj_chr == ""] <- NA_character_
USUBJID_all <- ifelse(is.na(subj_chr), "ONCO-2026", paste0("ONCO-2026-", subj_chr))

if (is.numeric(raw_DOSE)) {
  dose_num_all <- raw_DOSE
} else {
  dose_chr <- trimws(as.character(raw_DOSE))
  dose_num_all <- suppressWarnings(as.numeric(dose_chr))
}
keep <- !is.na(dose_num_all)

iso_STDT_all <- parse_vec_to_iso(raw_STDT)
iso_ENDT_all <- parse_vec_to_iso(raw_ENDT)

ex_prep <- data.frame(
  USUBJID = USUBJID_all,
  DOSE = dose_num_all,
  EXSTDTC = iso_STDT_all,
  EXENDTC = iso_ENDT_all,
  TRT = as.character(raw_TRT),
  UNIT = as.character(raw_UNIT),
  FORM = as.character(raw_FORM),
  FREQ = as.character(raw_FREQ),
  ROUTE = as.character(raw_ROUTE),
  LOT = as.character(raw_LOT),
  stringsAsFactors = FALSE
)
ex_prep$EXSTDTC[ex_prep$EXSTDTC == ""] <- NA_character_
ex_prep$EXENDTC[ex_prep$EXENDTC == ""] <- NA_character_
ex_prep <- ex_prep[keep, , drop = FALSE]

o <- order(ex_prep$USUBJID, ex_prep$EXSTDTC, na.last = FALSE)
ex_prep <- ex_prep[o, , drop = FALSE]

# ------------------------------------------------------------
# 1. Get RFSTDTC from DM for Study Day Calculation (CDISC RFSTDTC
#    = randomisation date, else first-exposure date)
# ------------------------------------------------------------
# helper: find randomisation date column in a data frame
find_rand_date_col <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NA_character_)
  cn <- names(df); up <- toupper(cn)
  # exact RFSTDTC first
  idx <- which(up == "RFSTDTC")
  if (length(idx) >= 1) return(cn[idx[1]])
  idx <- which(up == "RFSTDTC" | up == "RFSTDT" | up == "RFSTART")
  if (length(idx) >= 1) return(cn[idx[1]])
  # columns containing both RAND and (DAT/DT/DTC)
  idx <- which(grepl("RAND", up) & (grepl("DAT", up) | grepl("DTC", up) | grepl("DT", up)))
  if (length(idx) >= 1) return(cn[idx[1]])
  idx <- which(grepl("RAND", up))
  if (length(idx) >= 1) return(cn[idx[1]])
  return(NA_character_)
}
find_subj_col <- function(df) {
  c1 <- find_col(df, "SUBJID")
  if (!is.na(c1)) return(c1)
  c2 <- find_col_contains(df, "SUBJ")
  return(c2)
}

rand_maps <- list()

# ivrs_rand -> randomisation date
if (!is.null(ivrs_rand) && nrow(ivrs_rand) > 0) {
  c_r_subj <- find_subj_col(ivrs_rand)
  c_r_dt <- find_rand_date_col(ivrs_rand)
  # fallback: any DATE/DTC column if no RAND-named column
  if (is.na(c_r_dt)) {
    cn <- names(ivrs_rand); up <- toupper(cn)
    idx <- which(grepl("DAT", up) | grepl("DTC", up))
    if (length(idx) >= 1) c_r_dt <- cn[idx[1]]
  }
  if (!is.na(c_r_subj) && !is.na(c_r_dt)) {
    r_subj_chr <- trimws(as.character(ivrs_rand[[c_r_subj]]))
    r_usub <- ifelse(is.na(r_subj_chr) | r_subj_chr == "", "ONCO-2026",
                     paste0("ONCO-2026-", r_subj_chr))
    r_iso <- parse_vec_to_iso(ivrs_rand[[c_r_dt]])
    rdf <- data.frame(USUBJID = r_usub, RANDTC = r_iso, stringsAsFactors = FALSE)
    rdf <- rdf[!is.na(rdf$RANDTC) & rdf$RANDTC != "", , drop = FALSE]
    if (nrow(rdf) > 0) {
      rdf <- rdf[order(rdf$USUBJID, rdf$RANDTC), , drop = FALSE]
      rdf <- rdf[!duplicated(rdf$USUBJID), , drop = FALSE]
      rand_maps[[length(rand_maps) + 1]] <- rdf
    }
  }
}

# edc_dm -> only RF/RAND dates (never DOB etc.)
if (!is.null(edc_dm) && nrow(edc_dm) > 0) {
  c_d_subj <- find_subj_col(edc_dm)
  c_d_dt <- find_rand_date_col(edc_dm)
  if (!is.na(c_d_subj) && !is.na(c_d_dt)) {
    d_subj_chr <- trimws(as.character(edc_dm[[c_d_subj]]))
    d_usub <- ifelse(is.na(d_subj_chr) | d_subj_chr == "", "ONCO-2026",
                     paste0("ONCO-2026-", d_subj_chr))
    d_iso <- parse_vec_to_iso(edc_dm[[c_d_dt]])
    ddf <- data.frame(USUBJID = d_usub, RANDTC = d_iso, stringsAsFactors = FALSE)
    ddf <- ddf[!is.na(ddf$RANDTC) & ddf$RANDTC != "", , drop = FALSE]
    if (nrow(ddf) > 0) {
      ddf <- ddf[order(ddf$USUBJID, ddf$RANDTC), , drop = FALSE]
      ddf <- ddf[!duplicated(ddf$USUBJID), , drop = FALSE]
      rand_maps[[length(rand_maps) + 1]] <- ddf
    }
  }
}

if (length(rand_maps) > 0) {
  rand_all <- do.call(rbind, rand_maps)
  rand_all <- rand_all[order(rand_all$USUBJID, rand_all$RANDTC), , drop = FALSE]
  rand_all <- rand_all[!duplicated(rand_all$USUBJID), , drop = FALSE]
} else {
  rand_all <- data.frame(USUBJID = character(0), RANDTC = character(0), stringsAsFactors = FALSE)
}

# first-exposure fallback per subject
tmp_exp <- ex_prep[, c("USUBJID", "EXSTDTC"), drop = FALSE]
tmp_exp <- tmp_exp[!is.na(tmp_exp$EXSTDTC) & tmp_exp$EXSTDTC != "", , drop = FALSE]
if (nrow(tmp_exp) > 0) {
  first_exp <- aggregate(EXSTDTC ~ USUBJID, data = tmp_exp, FUN = function(x) min(x, na.rm = TRUE))
  names(first_exp)[2] <- "FIRSTEX"
} else {
  first_exp <- data.frame(USUBJID = character(0), FIRSTEX = character(0), stringsAsFactors = FALSE)
}

all_usub <- unique(c(ex_prep$USUBJID, rand_all$USUBJID))
dm_for_ex <- data.frame(USUBJID = all_usub, stringsAsFactors = FALSE)
dm_for_ex <- merge(dm_for_ex, rand_all, by = "USUBJID", all.x = TRUE, sort = FALSE)
dm_for_ex <- merge(dm_for_ex, first_exp, by = "USUBJID", all.x = TRUE, sort = FALSE)
# RFSTDTC = randomisation date when available, else first exposure (CDISC reference start)
dm_for_ex$RFSTDTC <- ifelse(!is.na(dm_for_ex$RANDTC) & dm_for_ex$RANDTC != "",
                            dm_for_ex$RANDTC, dm_for_ex$FIRSTEX)
dm_for_ex$RANDTC <- NULL
dm_for_ex$FIRSTEX <- NULL
dm_for_ex <- dm_for_ex[order(dm_for_ex$USUBJID, na.last = FALSE), , drop = FALSE]

# ------------------------------------------------------------
# 4. Create Intermediate EX Dataset (sdtm_ex1)
# ------------------------------------------------------------
stopifnot("USUBJID" %in% names(ex_prep))
stopifnot("USUBJID" %in% names(dm_for_ex))
stopifnot("RFSTDTC" %in% names(dm_for_ex))

sdtm_ex1 <- merge(ex_prep, dm_for_ex, by = "USUBJID", all.x = TRUE, sort = FALSE)

sdtm_ex1$STUDYID <- "ONCO-2026"
sdtm_ex1$DOMAIN <- "EX"

sdtm_ex1$EXTRT <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$TRT), NA_character_, sdtm_ex1$TRT)), 40)
sdtm_ex1$EXTRT[sdtm_ex1$TRT == "" | is.na(sdtm_ex1$TRT)] <- NA_character_
sdtm_ex1$EXDOSE <- sdtm_ex1$DOSE
sdtm_ex1$EXDOSU <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$UNIT), NA_character_, sdtm_ex1$UNIT)), 10)
sdtm_ex1$EXDOSFRM <- trunc_s(gsub("_", " ", ifelse(is.na(sdtm_ex1$FORM), NA_character_, sdtm_ex1$FORM), fixed = TRUE), 30)
sdtm_ex1$EXDOSFRQ <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$FREQ), NA_character_, sdtm_ex1$FREQ)), 10)
sdtm_ex1$EXVLDIST <- trunc_s(toupper(ifelse(is.na(sdtm_ex1$ROUTE), NA_character_, sdtm_ex1$ROUTE)), 20)
sdtm_ex1$EXLOT <- trunc_s(ifelse(is.na(sdtm_ex1$LOT), NA_character_, as.character(sdtm_ex1$LOT)), 15)
sdtm_ex1$EXSTDTC <- trunc_s(sdtm_ex1$EXSTDTC, 19)
sdtm_ex1$EXENDTC <- trunc_s(sdtm_ex1$EXENDTC, 19)
# SAS program declares LENGTH VISIT $40 but never assigns it -> always missing
sdtm_ex1$VISIT <- NA_character_
sdtm_ex1$STUDYID <- trunc_s(sdtm_ex1$STUDYID, 10)
sdtm_ex1$DOMAIN <- trunc_s(sdtm_ex1$DOMAIN, 2)
sdtm_ex1$USUBJID <- trunc_s(sdtm_ex1$USUBJID, 25)

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

sdtm_ex1 <- sdtm_ex1[order(sdtm_ex1$USUBJID, sdtm_ex1$EXSTDTC, na.last = FALSE), , drop = FALSE]

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
writeLines(lines, file.path(out, "P025.lst"))
tbl <- cbind(Obs = seq_len(nrow(sdtm_ex)), sdtm_ex)
write.csv(tbl, file.path(out, "P025_table.csv"), row.names = FALSE, na = "")
file.copy(file.path(out, "P025.csv"), file.path(out, "P025.lst.csv"), overwrite = TRUE)
```