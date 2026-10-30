# P029 TU mapping - R translation of P029_tu.sas
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

cat("P029 TU mapping started\n")

sas_tranwrd <- function(x, from, to) {
  sapply(x, function(v) {
    if (is.na(v) || v == "") return("")
    gsub(from, to, v, fixed = TRUE)
  }, USE.NAMES = FALSE)
}
sas_upcase <- function(x) {
  sapply(x, function(v) {
    if (is.na(v) || v == "") return("")
    toupper(v)
  }, USE.NAMES = FALSE)
}
sas_catx <- function(delim, ...) {
  args <- list(...)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return("")
      trimws(v)
    })
    parts <- parts[parts != ""]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}
sas_truncate <- function(x, len) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- as.character(v)
    if (nchar(v, type = "chars") > len) substr(v, 1, len) else v
  }, USE.NAMES = FALSE)
}
sas_input_date11_one <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  if (inherits(s, "Date")) return(s)
  if (inherits(s, "POSIXt")) return(as.Date(s))
  s <- trimws(as.character(s))
  if (s == "" || toupper(s) == "NA" || s == ".") return(as.Date(NA))
  m <- regmatches(s, regexec("^\\s*(\\d{1,2})-([A-Za-z]{3})-(\\d{4})\\s*$", s))[[1]]
  if (length(m) != 4) return(as.Date(NA))
  dd <- suppressWarnings(as.integer(m[2]))
  mon <- toupper(m[3])
  yy <- suppressWarnings(as.integer(m[4]))
  months <- c(JAN=1L,FEB=2L,MAR=3L,APR=4L,MAY=5L,JUN=6L,
              JUL=7L,AUG=8L,SEP=9L,OCT=10L,NOV=11L,DEC=12L)
  if (!mon %in% names(months) || is.na(dd) || is.na(yy)) return(as.Date(NA))
  mm <- months[[mon]]
  d <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd),
                        format = "%Y-%m-%d"),
                error = function(e) as.Date(NA))
  if (is.na(d)) return(as.Date(NA))
  lt <- as.POSIXlt(d)
  if (lt$mday != dd || (lt$mon + 1L) != mm || (lt$year + 1900L) != yy) return(as.Date(NA))
  d
}
sas_input_date11 <- function(x) {
  res <- as.Date(rep(NA_integer_, length(x)))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) { res[i] <- as.Date(NA); next }
    if (inherits(v, "Date")) { res[i] <- v; next }
    if (inherits(v, "POSIXt")) { res[i] <- as.Date(v); next }
    res[i] <- sas_input_date11_one(as.character(v))
  }
  res
}
sas_input_yymmdd10_one <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  if (inherits(s, "Date")) return(s)
  if (inherits(s, "POSIXt")) return(as.Date(s))
  s <- trimws(as.character(s))
  if (s == "" || s == ".") return(as.Date(NA))
  d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
  if (is.na(d)) return(as.Date(NA))
  if (format(d, "%Y-%m-%d") != s) return(as.Date(NA))
  d
}
sas_input_yymmdd10 <- function(x) {
  res <- as.Date(rep(NA_integer_, length(x)))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) { res[i] <- as.Date(NA); next }
    if (inherits(v, "Date")) { res[i] <- v; next }
    if (inherits(v, "POSIXt")) { res[i] <- as.Date(v); next }
    res[i] <- sas_input_yymmdd10_one(as.character(v))
  }
  res
}
fmt_yymmdd10 <- function(d) {
  d <- as.Date(d, origin = "1970-01-01")
  sapply(seq_along(d), function(i) {
    v <- d[i]
    if (is.na(v)) return("")
    format(v, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}
get_colname <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}
as_char_clean <- function(v) {
  sapply(seq_along(v), function(i) {
    x <- v[i]
    if (is.na(x)) return("")
    if (inherits(x, "Date")) return(format(as.Date(x), "%Y-%m-%d"))
    if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
    s <- trimws(as.character(x))
    if (is.na(s)) return("")
    s
  }, USE.NAMES = FALSE)
}

raw_base <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
f_tu   <- file.path(raw_base, "edc_tu.xlsx")
f_dm   <- file.path(raw_base, "edc_dm.xlsx")
f_ex   <- file.path(raw_base, "edc_ex.xlsx")
f_rand <- file.path(raw_base, "ivrs_rand.xlsx")

stopifnot(file.exists(f_tu))
edc_tu <- readxl::read_excel(f_tu)
cat("edc_tu rows:", nrow(edc_tu), " cols:", paste(names(edc_tu), collapse=","), "\n")

c_SUBJID <- get_colname(edc_tu, "SUBJID")
c_LOC    <- get_colname(edc_tu, "LOC")
c_METH   <- get_colname(edc_tu, "METH")
c_LNKID  <- get_colname(edc_tu, "LNKID")
c_TYPE   <- get_colname(edc_tu, "TYPE")
c_VISIT  <- get_colname(edc_tu, "VISIT")
c_TUDT   <- get_colname(edc_tu, "TUDT")
stopifnot(!is.null(c_SUBJID))
if (is.null(c_LOC))   { edc_tu$LOC <- "";   c_LOC <- "LOC" }
if (is.null(c_METH))  { edc_tu$METH <- "";  c_METH <- "METH" }
if (is.null(c_LNKID)) { edc_tu$LNKID <- ""; c_LNKID <- "LNKID" }
if (is.null(c_TYPE))  { edc_tu$TYPE <- "";  c_TYPE <- "TYPE" }
if (is.null(c_VISIT)) { edc_tu$VISIT <- ""; c_VISIT <- "VISIT" }
if (is.null(c_TUDT))  { edc_tu$TUDT <- "";  c_TUDT <- "TUDT" }

tu_prep <- edc_tu
tu_prep$SUBJID_c <- as_char_clean(tu_prep[[c_SUBJID]])
tu_prep$LOC_c    <- as_char_clean(tu_prep[[c_LOC]])
tu_prep$METH_c   <- as_char_clean(tu_prep[[c_METH]])
tu_prep$LNKID_c  <- as_char_clean(tu_prep[[c_LNKID]])
tu_prep$TYPE_c   <- as_char_clean(tu_prep[[c_TYPE]])
tu_prep$VISIT_c  <- as_char_clean(tu_prep[[c_VISIT]])
TUDT_raw <- tu_prep[[c_TUDT]]
TUDT_char_for_check <- sapply(seq_along(TUDT_raw), function(i) {
  v <- TUDT_raw[i]
  if (length(v) == 0 || is.na(v)) return("")
  if (inherits(v, "Date") || inherits(v, "POSIXt")) {
    if (is.na(as.Date(v))) return("")
    return(format(as.Date(v), "%d-%b-%Y"))
  }
  s <- trimws(as.character(v))
  if (is.na(s)) return("")
  s
}, USE.NAMES = FALSE)

tu_prep$USUBJID  <- sas_catx("-", "ONCO-2026", tu_prep$SUBJID_c)
tu_prep$TULOC    <- sas_upcase(sas_tranwrd(tu_prep$LOC_c, "_", " "))
tu_prep$TUMETHOD <- sas_upcase(sas_tranwrd(tu_prep$METH_c, "_", " "))
tu_prep$TUDT_raw_list <- TUDT_raw
tu_prep$TUDT_chk <- TUDT_char_for_check
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# DM.RFSTDTC derivation: standard CDISC reference start = randomisation date,
# fallback to first exposure. SAS sdtm_dm RFSTDTC for this study equals RAND_DT.
parse_generic_date <- function(v) {
  if (inherits(v, "Date")) return(as.Date(v, origin = "1970-01-01"))
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    return(suppressWarnings(as.Date(v, origin = "1899-12-30")))
  }
  ch <- as_char_clean(v)
  nonblank <- ch[ch != ""]
  if (length(nonblank) == 0) return(as.Date(rep(NA_integer_, length(ch)), origin = "1970-01-01"))
  sample <- trimws(nonblank[1])
  if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", sample)) {
    return(sas_input_date11(ch))
  } else if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", sample)) {
    res <- as.Date(rep(NA_integer_, length(ch)), origin = "1970-01-01")
    for (i in seq_along(ch)) {
      s <- trimws(ch[i])
      if (s == "") next
      p <- strsplit(s, "-", fixed = TRUE)[[1]]
      if (length(p) != 3) next
      dd <- suppressWarnings(as.integer(p[3])); mm <- suppressWarnings(as.integer(p[2])); yy <- suppressWarnings(as.integer(p[1]))
      if (any(is.na(c(dd, mm, yy)))) next
      d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
      if (is.na(d)) next
      res[i] <- d
    }
    return(res)
  } else if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", sample)) {
    res <- as.Date(rep(NA_integer_, length(ch)), origin = "1970-01-01")
    for (i in seq_along(ch)) {
      s <- trimws(ch[i])
      if (s == "") next
      p <- strsplit(s, "/", fixed = TRUE)[[1]]
      if (length(p) != 3) next
      dd <- suppressWarnings(as.integer(p[1])); mm <- suppressWarnings(as.integer(p[2])); yy <- suppressWarnings(as.integer(p[3]))
      if (any(is.na(c(dd, mm, yy)))) next
      if (yy < 100) yy <- ifelse(yy > 30, 1900L + yy, 2000L + yy)
      d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
      if (is.na(d)) next
      res[i] <- d
    }
    return(res)
  } else if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", sample)) {
    res <- as.Date(rep(NA_integer_, length(ch)), origin = "1970-01-01")
    for (i in seq_along(ch)) {
      s <- trimws(ch[i])
      if (s == "") next
      p <- strsplit(s, "-", fixed = TRUE)[[1]]
      if (length(p) != 3) next
      dd <- suppressWarnings(as.integer(p[1])); mm <- suppressWarnings(as.integer(p[2])); yy <- suppressWarnings(as.integer(p[3]))
      if (any(is.na(c(dd, mm, yy)))) next
      if (yy < 100) yy <- ifelse(yy > 30, 1900L + yy, 2000L + yy)
      d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
      if (is.na(d)) next
      res[i] <- d
    }
    return(res)
  } else {
    d1 <- sas_input_date11(ch)
    need <- is.na(d1) & ch != ""
    if (any(need)) {
      for (i in which(need)) {
        d2 <- suppressWarnings(as.Date(trimws(ch[i]), format = "%Y-%m-%d"))
        if (!is.na(d2)) d1[i] <- d2
      }
    }
    return(d1)
  }
}

df_rand_min <- NULL
df_ex_min <- NULL
df_dm_direct <- NULL

if (file.exists(f_rand)) {
  ivrs <- readxl::read_excel(f_rand)
  cat("ivrs_rand rows:", nrow(ivrs), " cols:", paste(names(ivrs), collapse=","), "\n")
  sc <- get_colname(ivrs, "SUBJID")
  if (is.null(sc)) sc <- get_colname(ivrs, "USUBJID")
  if (!is.null(sc)) {
    r_sub_raw <- as_char_clean(ivrs[[sc]])
    r_usub <- sapply(r_sub_raw, function(s) {
      if (s == "") return("")
      if (grepl("^ONCO-2026-", s)) return(s)
      paste0("ONCO-2026-", s)
    }, USE.NAMES = FALSE)
    cand_order2 <- c("RANDDTC","RANDDT","RANDOMDTC","RANDOMDT","ENROLLDTC","ENROLLDT","RANDDATE","RAND_DT","DATE")
    found2 <- NULL
    for (cn in cand_order2) {
      cc <- get_colname(ivrs, cn)
      if (!is.null(cc)) { found2 <- cc; break }
    }
    if (is.null(found2)) {
      cands <- names(ivrs)[grepl("DTC$|DT$|DATE", names(ivrs), ignore.case = TRUE)]
      if (length(cands) > 0) found2 <- cands[1]
    }
    if (!is.null(found2)) {
      r_dt <- parse_generic_date(ivrs[[found2]])
      df_r <- data.frame(USUBJID = r_usub, RF_DATE = as.Date(r_dt, origin="1970-01-01"), stringsAsFactors = FALSE)
      df_r <- df_r[df_r$USUBJID != "" & !is.na(df_r$RF_DATE), , drop = FALSE]
      if (nrow(df_r) > 0) {
        df_rand_min <- df_r %>% group_by(USUBJID) %>% summarise(RF_DATE = min(RF_DATE), .groups = "drop") %>% as.data.frame()
      }
    }
  }
}
if (file.exists(f_ex)) {
  edc_ex <- readxl::read_excel(f_ex)
  cat("edc_ex rows:", nrow(edc_ex), " cols:", paste(names(edc_ex), collapse=","), "\n")
  sc <- get_colname(edc_ex, "SUBJID")
  if (is.null(sc)) sc <- get_colname(edc_ex, "USUBJID")
  if (!is.null(sc)) {
    ex_sub_raw <- as_char_clean(edc_ex[[sc]])
    ex_usub <- sapply(ex_sub_raw, function(s) {
      if (s == "") return("")
      if (grepl("^ONCO-2026-", s)) return(s)
      paste0("ONCO-2026-", s)
    }, USE.NAMES = FALSE)
    cand_order <- c("EXSTDTC","EXSTDT","EXDT","EXDATE","STARTDTC","STARTDT","TRTSDTC","TRTSDT","RFSTDTC","STDT")
    found <- NULL
    for (cn in cand_order) {
      cc <- get_colname(edc_ex, cn)
      if (!is.null(cc)) { found <- cc; break }
    }
    if (is.null(found)) {
      cands <- names(edc_ex)[grepl("DTC$|DT$|DATE", names(edc_ex), ignore.case = TRUE)]
      cands_start <- cands[!grepl("END|STOP|EN$", cands, ignore.case = TRUE)]
      if (length(cands_start) > 0) found <- cands_start[1]
      else if (length(cands) > 0) found <- cands[1]
    }
    if (!is.null(found)) {
      ex_dt <- parse_generic_date(edc_ex[[found]])
      df_ex <- data.frame(USUBJID = ex_usub, RF_DATE = as.Date(ex_dt, origin="1970-01-01"), stringsAsFactors = FALSE)
      df_ex <- df_ex[df_ex$USUBJID != "" & !is.na(df_ex$RF_DATE), , drop = FALSE]
      if (nrow(df_ex) > 0) {
        df_ex_min <- df_ex %>% group_by(USUBJID) %>% summarise(RF_DATE = min(RF_DATE), .groups = "drop") %>% as.data.frame()
      }
    }
  }
}
if (file.exists(f_dm)) {
  edc_dm <- readxl::read_excel(f_dm)
  cat("edc_dm rows:", nrow(edc_dm), " cols:", paste(names(edc_dm), collapse=","), "\n")
  sc <- get_colname(edc_dm, "SUBJID")
  dm_subjid <- NULL
  if (!is.null(sc)) {
    dm_subjid_raw <- as_char_clean(edc_dm[[sc]])
    dm_subjid <- sas_catx("-", "ONCO-2026", dm_subjid_raw)
  } else {
    sc2 <- get_colname(edc_dm, "USUBJID")
    if (!is.null(sc2)) {
      dm_subjid <- as_char_clean(edc_dm[[sc2]])
      dm_subjid <- sapply(dm_subjid, function(s) {
        if (s == "") return("")
        if (grepl("^ONCO-2026-", s)) return(s)
        paste0("ONCO-2026-", s)
      }, USE.NAMES = FALSE)
    }
  }
  rfc <- get_colname(edc_dm, "RFSTDTC")
  if (is.null(rfc)) rfc <- get_colname(edc_dm, "RFSTDT")
  if (!is.null(rfc) && !is.null(dm_subjid)) {
    dm_rf <- parse_generic_date(edc_dm[[rfc]])
    tmp <- data.frame(USUBJID = dm_subjid, RF_DATE = as.Date(dm_rf, origin="1970-01-01"), stringsAsFactors = FALSE)
    tmp <- tmp[tmp$USUBJID != "" & !is.na(tmp$RF_DATE), , drop = FALSE]
    if (nrow(tmp) > 0) df_dm_direct <- tmp
  }
}

# Priority: randomisation first, then first exposure, then DM direct
rf_list <- list()
if (!is.null(df_rand_min) && nrow(df_rand_min) > 0) rf_list[[length(rf_list)+1]] <- df_rand_min
if (!is.null(df_ex_min) && nrow(df_ex_min) > 0) rf_list[[length(rf_list)+1]] <- df_ex_min
if (!is.null(df_dm_direct) && nrow(df_dm_direct) > 0) rf_list[[length(rf_list)+1]] <- df_dm_direct

dm_for_tu <- NULL
if (length(rf_list) > 0) {
  combined <- bind_rows(rf_list)
  combined$RF_DATE <- as.Date(combined$RF_DATE, origin = "1970-01-01")
  combined <- combined[!is.na(combined$RF_DATE), , drop = FALSE]
  combined <- combined[!duplicated(combined$USUBJID), , drop = FALSE]
  dm_for_tu <- data.frame(USUBJID = combined$USUBJID,
                          RFSTDTC = fmt_yymmdd10(combined$RF_DATE),
                          stringsAsFactors = FALSE)
} else {
  dm_for_tu <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
}
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last = FALSE), , drop = FALSE]
cat("dm_for_tu rows:", nrow(dm_for_tu), "\n")
print(head(dm_for_tu))

tu_prep$RFSTDTC <- NULL
if (nrow(dm_for_tu) > 0) {
  dm_ded <- dm_for_tu[!duplicated(dm_for_tu$USUBJID), , drop = FALSE]
  sdtm_tu1 <- merge(tu_prep, dm_ded, by = "USUBJID", all.x = TRUE, sort = FALSE)
} else {
  sdtm_tu1 <- tu_prep
  sdtm_tu1$RFSTDTC <- ""
}
stopifnot("USUBJID" %in% names(sdtm_tu1))
stopifnot("RFSTDTC" %in% names(sdtm_tu1))
sdtm_tu1$RFSTDTC[is.na(sdtm_tu1$RFSTDTC)] <- ""

n1 <- nrow(sdtm_tu1)
STUDYID  <- rep("ONCO-2026", n1)
DOMAIN   <- rep("TU", n1)
TULNKID  <- sas_truncate(sdtm_tu1$LNKID_c, 5)
TUTESTCD <- rep("TUMIDENT", n1)
TUTEST   <- rep("Tumor Identification", n1)
TUORRES  <- sas_upcase(sdtm_tu1$TYPE_c)
TUSTRESC <- TUORRES
TUEVAL   <- rep("INVESTIGATOR", n1)
VISIT    <- sas_upcase(sas_tranwrd(sdtm_tu1$VISIT_c, "_", " "))
TULOC    <- sdtm_tu1$TULOC
TUMETHOD <- sdtm_tu1$TUMETHOD
USUBJID  <- sdtm_tu1$USUBJID

TUDTC <- character(n1)
for (i in seq_len(n1)) {
  chk <- sdtm_tu1$TUDT_chk[i]
  if (is.na(chk) || chk == "") { TUDTC[i] <- ""; next }
  raw <- sdtm_tu1$TUDT_raw_list[i]
  if (inherits(raw, "Date")) {
    if (is.na(raw)) TUDTC[i] <- "" else TUDTC[i] <- format(as.Date(raw, origin="1970-01-01"), "%Y-%m-%d")
    next
  }
  if (inherits(raw, "POSIXt")) {
    if (is.na(raw)) TUDTC[i] <- "" else TUDTC[i] <- format(as.Date(raw), "%Y-%m-%d")
    next
  }
  d <- sas_input_date11_one(as.character(raw))
  if (is.na(d)) TUDTC[i] <- "" else TUDTC[i] <- format(d, "%Y-%m-%d")
}

RFSTDTC <- sapply(sdtm_tu1$RFSTDTC, function(v) {
  if (is.na(v)) return("")
  trimws(as.character(v))
}, USE.NAMES = FALSE)

TUSTDY <- rep(NA_real_, n1)
for (i in seq_len(n1)) {
  if (!is.na(TUDTC[i]) && TUDTC[i] != "" && !is.na(RFSTDTC[i]) && RFSTDTC[i] != "") {
    stdt <- sas_input_yymmdd10_one(TUDTC[i])
    rfdt <- sas_input_yymmdd10_one(RFSTDTC[i])
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) TUSTDY[i] <- diff + 1
      else TUSTDY[i] <- diff
    }
  }
}

sdtm_tu1_out <- data.frame(
  STUDYID = sas_truncate(STUDYID, 10),
  DOMAIN  = sas_truncate(DOMAIN, 2),
  USUBJID = sas_truncate(USUBJID, 25),
  TULNKID = sas_truncate(TULNKID, 5),
  TUTESTCD = sas_truncate(TUTESTCD, 8),
  TUTEST  = sas_truncate(TUTEST, 40),
  TUORRES = sas_truncate(TUORRES, 15),
  TUSTRESC = sas_truncate(TUSTRESC, 15),
  TULOC   = sas_truncate(TULOC, 20),
  TUMETHOD = sas_truncate(TUMETHOD, 20),
  TUEVAL  = sas_truncate(TUEVAL, 15),
  VISIT   = sas_truncate(VISIT, 20),
  TUDTC   = TUDTC,
  TUSTDY  = TUSTDY,
  stringsAsFactors = FALSE
)

ord <- order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last = FALSE)
sdtm_tu1_out <- sdtm_tu1_out[ord, , drop = FALSE]

sdtm_tu1_out <- sdtm_tu1_out[order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last = FALSE), , drop = FALSE]
TUSEQ <- rep(NA_real_, nrow(sdtm_tu1_out))
prev_usub <- NA_character_
seqn <- 0L
for (i in seq_len(nrow(sdtm_tu1_out))) {
  u <- sdtm_tu1_out$USUBJID[i]
  if (is.na(u)) u <- ""
  if (is.na(prev_usub) || u != prev_usub) {
    seqn <- 1L
  } else {
    seqn <- seqn + 1L
  }
  TUSEQ[i] <- seqn
  prev_usub <- u
}
sdtm_tu <- data.frame(
  STUDYID = sdtm_tu1_out$STUDYID,
  DOMAIN  = sdtm_tu1_out$DOMAIN,
  USUBJID = sdtm_tu1_out$USUBJID,
  TUSEQ   = TUSEQ,
  TULNKID = sdtm_tu1_out$TULNKID,
  TUTESTCD = sdtm_tu1_out$TUTESTCD,
  TUTEST  = sdtm_tu1_out$TUTEST,
  TUORRES = sdtm_tu1_out$TUORRES,
  TUSTRESC = sdtm_tu1_out$TUSTRESC,
  TULOC   = sdtm_tu1_out$TULOC,
  TUMETHOD = sdtm_tu1_out$TUMETHOD,
  TUEVAL  = sdtm_tu1_out$TUEVAL,
  TUDTC   = sdtm_tu1_out$TUDTC,
  TUSTDY  = sdtm_tu1_out$TUSTDY,
  VISIT   = sdtm_tu1_out$VISIT,
  stringsAsFactors = FALSE
)

cat("sdtm_tu rows:", nrow(sdtm_tu), "\n")
print(head(as.data.frame(sdtm_tu)))

write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")

fmt_num <- function(x) {
  vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return("")
    as.character(as.integer(v))
  }, character(1), USE.NAMES = FALSE)
}
pt <- sdtm_tu
pt_disp <- data.frame(
  Obs = seq_len(nrow(pt)),
  STUDYID = pt$STUDYID,
  DOMAIN = pt$DOMAIN,
  USUBJID = pt$USUBJID,
  TUSEQ = fmt_num(pt$TUSEQ),
  TULNKID = pt$TULNKID,
  TUTESTCD = pt$TUTESTCD,
  TUTEST = pt$TUTEST,
  TUORRES = pt$TUORRES,
  TUSTRESC = pt$TUSTRESC,
  TULOC = pt$TULOC,
  TUMETHOD = pt$TUMETHOD,
  TUEVAL = pt$TUEVAL,
  TUDTC = pt$TUDTC,
  TUSTDY = fmt_num(pt$TUSTDY),
  VISIT = pt$VISIT,
  stringsAsFactors = FALSE
)

write.csv(pt_disp, file.path(out, "P029.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P029.txt")
con <- file(txt_path, open = "wt")
cat("PROC PRINT DATA=SDTM_TU\n", file = con)
cat("\n", file = con)
cols <- names(pt_disp)
widths <- sapply(cols, function(cn) max(nchar(c(cn, pt_disp[[cn]]), type = "chars"), na.rm = TRUE))
header <- paste(mapply(function(cn, w) sprintf(paste0("%-", w, "s"), cn), cols, widths), collapse = "  ")
cat(paste0(header, "\n"), file = con)
sep <- paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "  ")
cat(paste0(sep, "\n"), file = con)
for (i in seq_len(nrow(pt_disp))) {
  parts <- sapply(seq_along(cols), function(j) {
    cn <- cols[j]; v <- as.character(pt_disp[i, cn]); w <- widths[j]
    if (cn %in% c("Obs", "TUSEQ", "TUSTDY")) sprintf(paste0("%", w, "s"), v)
    else sprintf(paste0("%-", w, "s"), v)
  })
  cat(paste0(paste(parts, collapse = "  "), "\n"), file = con)
}
cat("\n", file = con)
cat(paste0("N = ", nrow(pt_disp), "\n"), file = con)
close(con)

invisible(try(file.copy(txt_path, file.path(out, "P029.lst"), overwrite = TRUE), silent = TRUE))

cat("Done. Wrote tu.csv, P029.csv, P029.txt\n")
