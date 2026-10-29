```r
# P026_lb.sas -> R translation
# SDTM LB mapping & derivation

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}
sas_trunc <- function(x, len) {
  sapply(as.character(x), function(v) {
    if (is.na(v)) return(NA_character_)
    if (nchar(v, type = "chars") > len) substr(v, 1, len) else v
  }, USE.NAMES = FALSE)
}
sas_catx <- function(delim, a, b) {
  n <- max(length(a), length(b))
  a <- rep(a, length.out = n); b <- rep(b, length.out = n)
  mapply(function(x, y) {
    xs <- if (is.na(x)) "" else trimws(as.character(x))
    ys <- if (is.na(y)) "" else trimws(as.character(y))
    if (xs == "" && ys == "") return("")
    if (xs == "") return(ys)
    if (ys == "") return(xs)
    paste0(xs, delim, ys)
  }, a, b, USE.NAMES = FALSE)
}
sas_tranwrd <- function(s, from, to) {
  sapply(s, function(v) {
    if (is.na(v)) return(NA_character_)
    gsub(from, to, as.character(v), fixed = TRUE)
  }, USE.NAMES = FALSE)
}
sas_best <- function(x) {
  suppressWarnings(as.numeric(trimws(as.character(x))))
}
parse_date11 <- function(x) {
  if (length(x) != 1) {
    if (length(x) == 0) return(as.Date(character(0)))
    return(suppressWarnings(as.Date(sapply(x, function(v) {
      d <- parse_date11(v); if (is.na(d)) return(NA_real_) else return(as.numeric(d))
    }), origin = "1970-01-01")))
  }
  if (is.na(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    if (!is.na(x) && x > 20000 && x < 60000) return(as.Date(x, origin = "1899-12-30"))
    else return(as.Date(NA))
  }
  s <- trimws(as.character(x))
  if (is.na(s) || s == "" || toupper(s) == "NA") return(as.Date(NA))
  d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  fmts <- c("%d-%b-%Y", "%d%b%Y", "%d-%B-%Y", "%d %b %Y", "%d/%m/%Y",
            "%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d-%m-%Y", "%Y%m%d")
  for (f in fmts) {
    d2 <- suppressWarnings(as.Date(s, format = f))
    if (!is.na(d2)) return(d2)
    d2 <- suppressWarnings(as.Date(stringr::str_to_title(s), format = f))
    if (!is.na(d2)) return(d2)
  }
  return(as.Date(NA))
}
parse_yymmdd <- function(x) {
  if (is.na(x)) return(as.Date(NA))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    if (!is.na(x) && x > 20000 && x < 60000) return(as.Date(x, origin = "1899-12-30"))
    else return(as.Date(NA))
  }
  s <- trimws(as.character(x))
  if (is.na(s) || s == "" || toupper(s) == "NA") return(as.Date(NA))
  d <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}
fmt_yymmdd <- function(d) {
  ifelse(is.na(d), NA_character_, format(as.Date(d, origin = "1970-01-01"), "%Y-%m-%d"))
}
find_col <- function(nms, candidates) {
  up <- toupper(nms)
  for (c in candidates) {
    idx <- which(up == toupper(c))
    if (length(idx) > 0) return(nms[idx[1]])
  }
  return(NA_character_)
}
get_char_col <- function(df, candidates) {
  if (is.null(df) || ncol(df) == 0) return(character(0))
  cn <- find_col(names(df), candidates)
  if (is.na(cn)) return(rep(NA_character_, nrow(df)))
  v <- df[[cn]]
  if (inherits(v, "Date")) return(format(v, "%d-%b-%Y"))
  if (inherits(v, "POSIXt")) return(format(as.Date(v), "%d-%b-%Y"))
  as.character(v)
}

cat("Reading inputs...\n")

lb_path <- file.path(edc_dir, "edc_lb.xlsx")
edc_lb_raw <- readxl::read_excel(lb_path, .name_repair = "minimal")
names(edc_lb_raw) <- trimws(as.character(names(edc_lb_raw)))
cat(paste0("edc_lb rows=", nrow(edc_lb_raw), " cols=", paste(names(edc_lb_raw), collapse = ","), "\n"))

SUBJID_v <- get_char_col(edc_lb_raw, c("SUBJID", "SUBJECT", "SUBJ", "USUBJID", "PTID"))
TESTCD_v <- get_char_col(edc_lb_raw, c("TESTCD", "LBTESTCD", "TEST"))
CAT_v    <- get_char_col(edc_lb_raw, c("CAT", "LBCAT", "CATEGORY"))
ORRES_v  <- get_char_col(edc_lb_raw, c("ORRES", "LBORRES", "RESULT", "VALUE"))
UNIT_v   <- get_char_col(edc_lb_raw, c("UNIT", "LBORRESU", "ORRESU", "UNITS", "LBSTRESU"))
LOW_v    <- get_char_col(edc_lb_raw, c("LOW", "LBORNRLO", "ORNRLO", "LOWER", "NRLO"))
HIGH_v   <- get_char_col(edc_lb_raw, c("HIGH", "LBORNRHI", "ORNRHI", "UPPER", "NRHI"))
VISIT_v  <- get_char_col(edc_lb_raw, c("VISIT", "VISITNUM", "VISIT_NAME", "FOLDER"))

cn_lbdt <- find_col(names(edc_lb_raw), c("LBDT", "LBDTC", "LBDAT", "DATE", "LBDY", "COLLDAT", "LABDT"))
LBDT_raw <- edc_lb_raw[[cn_lbdt]] if (!is.na(cn_lbdt)) else rep(NA_character_, nrow(edc_lb_raw))
if (!is.na(cn_lbdt)) {
  LBDT_raw <- edc_lb_raw[[cn_lbdt]]
}
LBDT_v <- sapply(seq_len(nrow(edc_lb_raw)), function(i) {
  v <- LBDT_raw[i]
  if (inherits(LBDT_raw, "Date")) {
    vv <- LBDT_raw[i]
    if (is.na(vv)) return(NA_character_) else return(format(as.Date(vv, origin = "1970-01-01"), "%d-%b-%Y"))
  }
  if (inherits(LBDT_raw, "POSIXct") || inherits(LBDT_raw, "POSIXt")) {
    vv <- LBDT_raw[i]
    if (is.na(vv)) return(NA_character_) else return(format(as.Date(vv), "%d-%b-%Y"))
  }
  if (is.numeric(LBDT_raw) && !is.character(LBDT_raw)) {
    if (is.na(v)) return(NA_character_)
    if (v > 20000 && v < 60000) return(format(as.Date(v, origin = "1899-12-30"), "%d-%b-%Y"))
    else return(as.character(v))
  }
  if (is.na(v)) return(NA_character_) else return(as.character(v))
}, USE.NAMES = FALSE)

clean_c <- function(v) {
  v <- sas_trim(as.character(v))
  v[v == "NA"] <- NA_character_
  v[is.na(v) | trimws(v) == ""] <- NA_character_
  v
}

lb_prep <- tibble::tibble(
  SUBJID = clean_c(SUBJID_v),
  TESTCD = clean_c(TESTCD_v),
  CAT = clean_c(CAT_v),
  ORRES = clean_c(ORRES_v),
  UNIT = clean_c(UNIT_v),
  LOW = clean_c(LOW_v),
  HIGH = clean_c(HIGH_v),
  VISIT = clean_c(VISIT_v),
  LBDT = clean_c(LBDT_v)
)
lb_prep$USUBJID <- sas_trunc(sas_catx("-", "ONCO-2026", lb_prep$SUBJID), 25)
lb_prep <- lb_prep[order(lb_prep$USUBJID, na.last = FALSE), , drop = FALSE]

cat("Preparing DM reference...\n")
edc_dm_raw <- NULL
try({ edc_dm_raw <- readxl::read_excel(file.path(edc_dir, "edc_dm.xlsx"), .name_repair = "minimal") }, silent = TRUE)
if (!is.null(edc_dm_raw)) names(edc_dm_raw) <- trimws(as.character(names(edc_dm_raw)))

dm_subjid <- NULL
rfstdtc_vec <- NULL
if (!is.null(edc_dm_raw)) {
  dm_subjid <- clean_c(get_char_col(edc_dm_raw, c("SUBJID", "SUBJECT", "SUBJ", "USUBJID", "PTID")))
  rf_cn <- find_col(names(edc_dm_raw), c("RFSTDTC", "RFSTDT", "RFSTDAT", "RANDDT", "RANDDTC", "EXSTDTC", "STDTC", "RFSTDC"))
  if (!is.na(rf_cn)) {
    v <- edc_dm_raw[[rf_cn]]
    if (inherits(v, "Date")) {
      rfstdtc_vec <- format(v, "%Y-%m-%d")
    } else if (inherits(v, "POSIXt")) {
      rfstdtc_vec <- format(as.Date(v), "%Y-%m-%d")
    } else {
      rfstdtc_vec <- sapply(as.character(v), function(x) {
        if (is.na(x) || trimws(x) == "" || toupper(trimws(x)) == "NA") return(NA_character_)
        d1 <- parse_yymmdd(x)
        if (!is.na(d1)) return(format(d1, "%Y-%m-%d"))
        d2 <- parse_date11(x)
        if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
        trimws(x)
      }, USE.NAMES = FALSE)
    }
  }
}

# derive exposure min date per subject from edc_ex (STDT)
ex_min <- NULL
try({
  ex_path <- file.path(edc_dir, "edc_ex.xlsx")
  if (file.exists(ex_path)) {
    edc_ex_raw <- readxl::read_excel(ex_path, .name_repair = "minimal")
    names(edc_ex_raw) <- trimws(as.character(names(edc_ex_raw)))
    cat(paste0("edc_ex cols=", paste(names(edc_ex_raw), collapse = ","), "\n"))
    ex_subjid <- clean_c(get_char_col(edc_ex_raw, c("SUBJID", "SUBJECT", "SUBJ", "USUBJID", "PTID")))
    ex_cn <- find_col(names(edc_ex_raw), c("EXSTDTC", "EXSTDT", "EXDTC", "EXDT", "STDT", "EXDATE", "EXSTDAT", "DATE", "STDTC", "DTC", "STARTDT"))
    if (!is.na(ex_cn)) {
      exv <- edc_ex_raw[[ex_cn]]
      if (inherits(exv, "Date")) {
        exd <- as.Date(exv)
      } else if (inherits(exv, "POSIXt")) {
        exd <- as.Date(exv)
      } else {
        exd <- sapply(as.character(exv), function(x) {
          if (is.na(x) || trimws(x) == "" || toupper(trimws(x)) == "NA") return(as.Date(NA))
          d1 <- parse_yymmdd(x)
          if (!is.na(d1)) return(d1)
          d2 <- parse_date11(x)
          return(d2)
        }, USE.NAMES = FALSE)
        exd <- as.Date(exd, origin = "1970-01-01")
      }
      exd <- as.Date(exd, origin = "1970-01-01")
      ex_df <- data.frame(SUBJID = ex_subjid, EXD = exd, stringsAsFactors = FALSE)
      ex_df <- ex_df[!is.na(ex_df$SUBJID) & !is.na(ex_df$EXD), , drop = FALSE]
      if (nrow(ex_df) > 0) {
        agg <- aggregate(EXD ~ SUBJID, data = ex_df, FUN = min)
        agg$EXD <- as.Date(agg$EXD, origin = "1970-01-01")
        ex_min <- agg
      }
    }
  }
}, silent = TRUE)

# derive rand date from ivrs_rand as fallback
rand_min <- NULL
try({
  r_path <- file.path(edc_dir, "ivrs_rand.xlsx")
  if (file.exists(r_path)) {
    r_raw <- readxl::read_excel(r_path, .name_repair = "minimal")
    names(r_raw) <- trimws(as.character(names(r_raw)))
    cat(paste0("ivrs_rand cols=", paste(names(r_raw), collapse = ","), "\n"))
    r_subjid <- clean_c(get_char_col(r_raw, c("SUBJID", "SUBJECT", "SUBJ", "USUBJID", "PTID")))
    # pick first date-like column
    date_cands <- names(r_raw)[grepl("DAT|DT|DTC", toupper(names(r_raw)))]
    if (length(date_cands) > 0) {
      rcn <- date_cands[1]
      rv <- r_raw[[rcn]]
      if (inherits(rv, "Date")) rd <- as.Date(rv)
      else if (inherits(rv, "POSIXt")) rd <- as.Date(rv)
      else {
        rd <- sapply(as.character(rv), function(x) {
          if (is.na(x) || trimws(x) == "" || toupper(trimws(x)) == "NA") return(as.Date(NA))
          d1 <- parse_yymmdd(x)
          if (!is.na(d1)) return(d1)
          d2 <- parse_date11(x)
          return(d2)
        }, USE.NAMES = FALSE)
        rd <- as.Date(rd, origin = "1970-01-01")
      }
      rd <- as.Date(rd, origin = "1970-01-01")
      r_df <- data.frame(SUBJID = r_subjid, RD = rd, stringsAsFactors = FALSE)
      r_df <- r_df[!is.na(r_df$SUBJID) & !is.na(r_df$RD), , drop = FALSE]
      if (nrow(r_df) > 0) {
        agg2 <- aggregate(RD ~ SUBJID, data = r_df, FUN = min)
        agg2$RD <- as.Date(agg2$RD, origin = "1970-01-01")
        rand_min <- agg2
      }
    }
  }
}, silent = TRUE)

if (!is.null(edc_dm_raw) && !is.null(dm_subjid)) {
  if (is.null(rfstdtc_vec)) rfstdtc_vec <- rep(NA_character_, length(dm_subjid))
  dm_for_lb0 <- data.frame(SUBJID = dm_subjid, RFSTDTC_raw = rfstdtc_vec, stringsAsFactors = FALSE)
  # fill missing RFSTDTC from ex_min / rand_min
  dm_for_lb0$RFSTDTC_raw <- sapply(seq_len(nrow(dm_for_lb0)), function(i) {
    v <- dm_for_lb0$RFSTDTC_raw[i]
    if (!is.na(v) && trimws(v) != "") return(v)
    s <- dm_for_lb0$SUBJID[i]
    if (!is.null(ex_min)) {
      m <- ex_min$EXD[ex_min$SUBJID == s]
      if (length(m) > 0 && !all(is.na(m))) return(format(min(m, na.rm = TRUE), "%Y-%m-%d"))
    }
    if (!is.null(rand_min)) {
      m2 <- rand_min$RD[rand_min$SUBJID == s]
      if (length(m2) > 0 && !all(is.na(m2))) return(format(min(m2, na.rm = TRUE), "%Y-%m-%d"))
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)
  dm_for_lb0$USUBJID <- sas_trunc(sas_catx("-", "ONCO-2026", dm_for_lb0$SUBJID), 25)
  dm_for_lb0$RFSTDTC <- sapply(dm_for_lb0$RFSTDTC_raw, function(x) {
    if (is.na(x) || trimws(as.character(x)) == "") return(NA_character_)
    d <- parse_yymmdd(x)
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    d2 <- parse_date11(x)
    if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    trimws(as.character(x))
  }, USE.NAMES = FALSE)
  dm_for_lb <- dm_for_lb0[!is.na(dm_for_lb0$USUBJID) & dm_for_lb0$USUBJID != "", c("USUBJID", "RFSTDTC"), drop = FALSE]
  dm_for_lb <- dm_for_lb[order(dm_for_lb$USUBJID, na.last = FALSE), , drop = FALSE]
  dm_for_lb <- dm_for_lb[!duplicated(dm_for_lb$USUBJID), , drop = FALSE]
} else {
  dm_for_lb <- data.frame(USUBJID = unique(lb_prep$USUBJID), RFSTDTC = NA_character_, stringsAsFactors = FALSE)
}
# ensure every lb subject has a row (merge keeps all lb anyway); add missing subjects with ex-derived date
miss_u <- setdiff(unique(lb_prep$USUBJID), dm_for_lb$USUBJID)
if (length(miss_u) > 0) {
  add <- data.frame(USUBJID = miss_u, RFSTDTC = NA_character_, stringsAsFactors = FALSE)
  # try to fill from ex_min via SUBJID part
  add$RFSTDTC <- sapply(add$USUBJID, function(u) {
    s <- sub("^ONCO-2026-", "", u)
    if (!is.null(ex_min)) {
      m <- ex_min$EXD[ex_min$SUBJID == s]
      if (length(m) > 0 && !all(is.na(m))) return(format(min(m, na.rm = TRUE), "%Y-%m-%d"))
    }
    if (!is.null(rand_min)) {
      m2 <- rand_min$RD[rand_min$SUBJID == s]
      if (length(m2) > 0 && !all(is.na(m2))) return(format(min(m2, na.rm = TRUE), "%Y-%m-%d"))
    }
    return(NA_character_)
  }, USE.NAMES = FALSE)
  dm_for_lb <- rbind(dm_for_lb, add)
}
cat(paste0("dm_for_lb rows=", nrow(dm_for_lb), "\n"))

# Map LB domain (merge lb_prep + dm_for_lb, keep all lb_prep)
sdtm_lb1m <- dplyr::left_join(lb_prep, dm_for_lb, by = "USUBJID")
n1 <- nrow(sdtm_lb1m)
STUDYID <- rep("ONCO-2026", n1)
DOMAIN <- rep("LB", n1)
LBTESTCD <- sas_trunc(ifelse(is.na(sdtm_lb1m$TESTCD), NA_character_, sdtm_lb1m$TESTCD), 8)
LBCAT <- sas_trunc(ifelse(is.na(sdtm_lb1m$CAT), NA_character_, sdtm_lb1m$CAT), 15)
LBTEST <- rep(NA_character_, n1)
idx_h <- !is.na(LBTESTCD) & LBTESTCD == "HGB"
idx_a <- !is.na(LBTESTCD) & LBTESTCD == "ALT"
LBTEST[idx_h] <- "Hemoglobin"
LBTEST[idx_a] <- "Alanine Aminotransferase"
LBTEST <- sas_trunc(LBTEST, 40)

LBORRES <- sas_trunc(sdtm_lb1m$ORRES, 10)
LBORRESU <- sas_trunc(sdtm_lb1m$UNIT, 20)
LBSTRESC <- sas_trunc(sdtm_lb1m$ORRES, 10)
LBSTRESN <- sas_best(sdtm_lb1m$ORRES)
LBSTRESU <- sas_trunc(sdtm_lb1m$UNIT, 20)
LBORNRLO <- sas_trunc(sdtm_lb1m$LOW, 10)
LBORNRHI <- sas_trunc(sdtm_lb1m$HIGH, 10)
VISIT <- sas_trunc(sas_tranwrd(sdtm_lb1m$VISIT, "_", " "), 20)

LOWN <- sas_best(sdtm_lb1m$LOW)
HIGHN <- sas_best(sdtm_lb1m$HIGH)
LBNRIND <- rep(NA_character_, n1)
for (i in seq_len(n1)) {
  v <- LBSTRESN[i]
  if (!is.na(v)) {
    lo <- LOWN[i]
    hi <- HIGHN[i]
    cond_low <- FALSE
    if (!is.na(lo)) cond_low <- (v < lo)
    if (cond_low) {
      LBNRIND[i] <- "LOW"
    } else {
      cond_high <- FALSE
      if (!is.na(hi)) cond_high <- (v > hi)
      else cond_high <- TRUE
      if (cond_high) LBNRIND[i] <- "HIGH"
      else LBNRIND[i] <- "NORMAL"
    }
  }
}
LBNRIND <- sas_trunc(LBNRIND, 8)

LBDTC <- rep(NA_character_, n1)
for (i in seq_len(n1)) {
  lbdt <- sdtm_lb1m$LBDT[i]
  if (!is.na(lbdt) && trimws(as.character(lbdt)) != "") {
    d <- parse_date11(lbdt)
    if (!is.na(d)) LBDTC[i] <- format(d, "%Y-%m-%d")
    else LBDTC[i] <- NA_character_
  }
}
LBDTC <- sas_trunc(LBDTC, 19)

LBSTDY <- rep(NA_real_, n1)
for (i in seq_len(n1)) {
  a <- LBDTC[i]
  b <- sdtm_lb1m$RFSTDTC[i]
  if (!is.na(a) && trimws(a) != "" && !is.na(b) && trimws(as.character(b)) != "") {
    stdt <- parse_yymmdd(a)
    rfdt <- parse_yymmdd(b)
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) LBSTDY[i] <- diff + 1
      else LBSTDY[i] <- diff
    }
  }
}

sdtm_lb1 <- data.frame(
  STUDYID = sas_trunc(STUDYID, 10), DOMAIN = sas_trunc(DOMAIN, 2),
  USUBJID = sas_trunc(sdtm_lb1m$USUBJID, 25),
  LBTESTCD = LBTESTCD, LBTEST = LBTEST, LBCAT = LBCAT,
  LBORRES = LBORRES, LBORRESU = LBORRESU,
  LBSTRESC = LBSTRESC, LBSTRESN = LBSTRESN, LBSTRESU = LBSTRESU,
  LBORNRLO = LBORNRLO, LBORNRHI = LBORNRHI, LBNRIND = LBNRIND,
  LBDTC = LBDTC, LBSTDY = LBSTDY, VISIT = VISIT,
  stringsAsFactors = FALSE
)

# Derive LBSEQ: sort by USUBJID LBDTC then retain
ord <- order(sdtm_lb1$USUBJID, sdtm_lb1$LBDTC, na.last = FALSE)
sdtm_lb1 <- sdtm_lb1[ord, , drop = FALSE]
LBSEQ <- rep(NA_real_, nrow(sdtm_lb1))
seqn <- 0
for (i in seq_len(nrow(sdtm_lb1))) {
  if (i == 1) {
    is_first <- TRUE
  } else {
    u <- sdtm_lb1$USUBJID[i]
    p <- sdtm_lb1$USUBJID[i - 1]
    if (is.na(u) && is.na(p)) is_first <- FALSE
    else if (!is.na(u) && !is.na(p) && u == p) is_first <- FALSE
    else is_first <- TRUE
  }
  if (is_first) seqn <- 1
  else seqn <- seqn + 1
  LBSEQ[i] <- seqn
}
sdtm_lb <- data.frame(
  STUDYID = sdtm_lb1$STUDYID, DOMAIN = sdtm_lb1$DOMAIN, USUBJID = sdtm_lb1$USUBJID,
  LBSEQ = LBSEQ, LBTESTCD = sdtm_lb1$LBTESTCD, LBTEST = sdtm_lb1$LBTEST,
  LBCAT = sdtm_lb1$LBCAT, LBORRES = sdtm_lb1$LBORRES, LBORRESU = sdtm_lb1$LBORRESU,
  LBSTRESC = sdtm_lb1$LBSTRESC, LBSTRESN = sdtm_lb1$LBSTRESN, LBSTRESU = sdtm_lb1$LBSTRESU,
  LBORNRLO = sdtm_lb1$LBORNRLO, LBORNRHI = sdtm_lb1$LBORNRHI,
  LBNRIND = sdtm_lb1$LBNRIND, LBDTC = sdtm_lb1$LBDTC, LBSTDY = sdtm_lb1$LBSTDY,
  VISIT = sdtm_lb1$VISIT, stringsAsFactors = FALSE
)
sdtm_lb <- sdtm_lb[, c("STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST", "LBCAT",
  "LBORRES", "LBORRESU", "LBSTRESC", "LBSTRESN", "LBSTRESU", "LBORNRLO", "LBORNRHI",
  "LBNRIND", "LBDTC", "LBSTDY", "VISIT")]

cat(paste0("sdtm_lb rows=", nrow(sdtm_lb), "\n"))

# Outputs
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names = FALSE, na = "")

fmt_cell <- function(v) ifelse(is.na(v), "", as.character(v))
disp <- as.data.frame(lapply(sdtm_lb, fmt_cell), stringsAsFactors = FALSE)
cols <- names(sdtm_lb)
hdr <- paste0("Listing P026: SDTM LB (N=", nrow(sdtm_lb), ")")
header_line <- paste(c("Obs", cols), collapse = " | ")
lines <- c(hdr, "Study: ONCO-2026  Domain: LB", "")
lines <- c(lines, header_line, paste(rep("-", nchar(header_line)), collapse = ""))
for (i in seq_len(nrow(disp))) {
  rowvals <- c(as.character(i), as.character(unlist(disp[i, ])))
  lines <- c(lines, paste(rowvals, collapse = " | "))
}
lines <- c(lines, "", paste0("End of listing. N=", nrow(sdtm_lb)))
writeLines(lines, file.path(out, "P026.txt"))
writeLines(lines, file.path(out, "P026.lst.txt"))
write.csv(sdtm_lb, file.path(out, "P026.csv"), row.names = FALSE, na = "")
write.csv(sdtm_lb, file.path(out, "P026.lst.csv"), row.names = FALSE, na = "")
writeLines(lines, file.path(out, "lb.txt"))

cat("Done.\n")
```