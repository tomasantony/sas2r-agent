# P029 TU mapping - R translation of P029_tu.sas
# Reads edc_tu + DM reference (derived), maps SDTM TU, derives TUSEQ/TUSTDY

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

cat("P029 TU mapping started\n")

# ---------- helpers (SAS semantics) ----------
sas_round <- function(x, d = 0) {
  # round half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}
sas_tranwrd <- function(x, from, to) {
  # SAS TRANWRD: replace all occurrences; missing (""/NA) -> ""
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
# SAS DATE11. input: DD-MMM-YYYY (e.g. 02-JAN-2026), case-insensitive month
sas_input_date11 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    s <- trimws(as.character(v))
    if (s == "" || toupper(s) == "NA" || s == ".") return(as.Date(NA))
    # strict DATE11: 1-2 digit day, -, 3-letter month, -, 4-digit year
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
    # validate day/month (e.g. 30-FEB) : check components round-trip
    if (is.na(d)) return(as.Date(NA))
    lt <- as.POSIXlt(d)
    if (lt$mday != dd || (lt$mon + 1L) != mm || (lt$year + 1900L) != yy) return(as.Date(NA))
    d
  }, USE.NAMES = FALSE)
}
# SAS YYMMDD10. input/output: YYYY-MM-DD
sas_input_yymmdd10 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    s <- trimws(as.character(v))
    if (s == "" || s == ".") return(as.Date(NA))
    # strict YYYY-MM-DD, allow single digit m/d? SAS yymmdd10 requires 10 chars; be strict
    d <- suppressWarnings(as.Date(s, format = "%Y-%m-%d"))
    # verify round-trip formatting to catch invalid like 2026-02-30
    if (is.na(d)) return(as.Date(NA))
    if (format(d, "%Y-%m-%d") != s) {
      # allow SAS to still accept? require exact; if not exact return NA
      # but tolerate e.g. 2026-1-5? SAS would read with yymmdd10.? Keep strict: return NA
      return(as.Date(NA))
    }
    d
  }, USE.NAMES = FALSE)
}
fmt_yymmdd10 <- function(d) {
  sapply(d, function(v) {
    if (is.na(v)) return("")
    # v may be numeric days since epoch from sapply simplification
    if (is.numeric(v)) v <- as.Date(v, origin = "1970-01-01")
    format(v, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}
get_col <- function(df, target) {
  # case-insensitive column lookup; returns vector or NULL
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}
get_colname <- function(df, target) {
  idx <- which(toupper(names(df)) == toupper(target))
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}
as_char_clean <- function(v) {
  sapply(v, function(x) {
    if (is.na(x)) return("")
    if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
    if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
    s <- trimws(as.character(x))
    if (is.na(s)) return("")
    s
  }, USE.NAMES = FALSE)
}

raw_base <- "/bench/data/raw/faijal-oncology/raw_data"
f_tu   <- file.path(raw_base, "edc_tu.xlsx")
f_dm   <- file.path(raw_base, "edc_dm.xlsx")
f_ex   <- file.path(raw_base, "edc_ex.xlsx")
f_rand <- file.path(raw_base, "ivrs_rand.xlsx")

# ---------- 2. Prepare Raw Data: read edc_tu ----------
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
# required columns per SAS program must exist (except allow missing optional -> create blank)
if (is.null(c_LOC))   { edc_tu$LOC <- "";   c_LOC <- "LOC" }
if (is.null(c_METH))  { edc_tu$METH <- "";  c_METH <- "METH" }
if (is.null(c_LNKID)) { edc_tu$LNKID <- ""; c_LNKID <- "LNKID" }
if (is.null(c_TYPE))  { edc_tu$TYPE <- "";  c_TYPE <- "TYPE" }
if (is.null(c_VISIT)) { edc_tu$VISIT <- ""; c_VISIT <- "VISIT" }
if (is.null(c_TUDT))  { edc_tu$TUDT <- "";  c_TUDT <- "TUDT" }

tu_prep <- edc_tu
# keep original raw values for later mapping; create SAS variables
tu_prep$SUBJID_c <- as_char_clean(tu_prep[[c_SUBJID]])
tu_prep$LOC_c    <- as_char_clean(tu_prep[[c_LOC]])
tu_prep$METH_c   <- as_char_clean(tu_prep[[c_METH]])
tu_prep$LNKID_c  <- as_char_clean(tu_prep[[c_LNKID]])
tu_prep$TYPE_c   <- as_char_clean(tu_prep[[c_TYPE]])
tu_prep$VISIT_c  <- as_char_clean(tu_prep[[c_VISIT]])
# TUDT raw: may be Date object from Excel or character DATE11
TUDT_raw <- tu_prep[[c_TUDT]]
# normalize TUDT to character for ne "" check, but keep Date handling for parsing
TUDT_char_for_check <- sapply(seq_along(TUDT_raw), function(i) {
  v <- TUDT_raw[i]
  if (is.na(v)) return("")
  if (inherits(v, "Date") || inherits(v, "POSIXt")) {
    # Excel date: non-missing -> treat as non-blank
    return(format(as.Date(v), "%d-%b-%Y"))
  }
  s <- trimws(as.character(v))
  if (is.na(s)) return("")
  s
}, USE.NAMES = FALSE)

tu_prep$USUBJID  <- sas_catx("-", "ONCO-2026", tu_prep$SUBJID_c)
tu_prep$TULOC    <- sas_upcase(sas_tranwrd(tu_prep$LOC_c, "_", " "))
tu_prep$TUMETHOD <- sas_upcase(sas_tranwrd(tu_prep$METH_c, "_", " "))
# store raw TUDT character (for TUDTC derivation)
tu_prep$TUDT_raw_list <- TUDT_raw
tu_prep$TUDT_chk <- TUDT_char_for_check

# sort by USUBJID (SAS character ordering: uppercase before lowercase; R default similar for ASCII)
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last = FALSE), , drop = FALSE]

# ---------- 1. Prepare DM Reference Data (sdtm_dm not in WORK -> derive per CDISC) ----------
# Standard: DM.RFSTDTC = first exposure date if available else randomization date
# Try to derive from edc_ex / ivrs_rand / edc_dm

derive_rfstdtc <- function() {
  rf_list <- list()  # list of data.frames USUBJID, RFSTDTC_date
  # helper to parse a generic date column strictly with ONE format chosen by inspection
  parse_generic_date <- function(v) {
    # v: vector (Date/POSIXt/numeric/character)
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    if (is.numeric(v)) {
      # Excel numeric dates? readxl usually converts; if numeric, assume Excel origin 1899-12-30
      return(suppressWarnings(as.Date(v, origin = "1899-12-30")))
    }
    ch <- as_char_clean(v)
    # inspect first non-blank to choose ONE format
    nonblank <- ch[ch != ""]
    if (length(nonblank) == 0) return(rep(as.Date(NA), length(ch)))
    sample <- nonblank[1]
    fmt <- NULL
    if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{4}$", trimws(sample))) {
      # DATE11
      return(sas_input_date11(ch))
    } else if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", trimws(sample))) {
      # YYYY-MM-DD (allow 1-2 digit m/d for source robustness, then strict-ish)
      return(sapply(ch, function(s) {
        if (s == "") return(as.Date(NA))
        s2 <- trimws(s)
        # normalize to zero-padded for parsing with single format %Y-%m-%d after padding
        parts <- strsplit(s2, "-", fixed = TRUE)[[1]]
        if (length(parts) != 3) return(as.Date(NA))
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d",
          suppressWarnings(as.integer(parts[1])),
          suppressWarnings(as.integer(parts[2])),
          suppressWarnings(as.integer(parts[3]))), format = "%Y-%m-%d"))
        if (is.na(d)) return(as.Date(NA))
        d
      }, USE.NAMES = FALSE))
    } else if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", trimws(sample))) {
      fmt <- "%d/%m/%Y"
      # handle 2-digit year? assume SAS MDY/DMY? Use DMY per instruction default? Use %d/%m/%Y with 4-digit; if 2-digit, expand
      return(sapply(ch, function(s) {
        if (s == "") return(as.Date(NA))
        s2 <- trimws(s)
        p <- strsplit(s2, "/", fixed = TRUE)[[1]]
        if (length(p) != 3) return(as.Date(NA))
        dd <- suppressWarnings(as.integer(p[1])); mm <- suppressWarnings(as.integer(p[2])); yy <- suppressWarnings(as.integer(p[3]))
        if (any(is.na(c(dd, mm, yy)))) return(as.Date(NA))
        if (yy < 100) yy <- ifelse(yy > 30, 1900L + yy, 2000L + yy)
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
        if (is.na(d)) return(as.Date(NA))
        d
      }, USE.NAMES = FALSE))
    } else if (grepl("^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$", trimws(sample))) {
      return(sapply(ch, function(s) {
        if (s == "") return(as.Date(NA))
        s2 <- trimws(s)
        p <- strsplit(s2, "-", fixed = TRUE)[[1]]
        if (length(p) != 3) return(as.Date(NA))
        dd <- suppressWarnings(as.integer(p[1])); mm <- suppressWarnings(as.integer(p[2])); yy <- suppressWarnings(as.integer(p[3]))
        if (any(is.na(c(dd, mm, yy)))) return(as.Date(NA))
        if (yy < 100) yy <- ifelse(yy > 30, 1900L + yy, 2000L + yy)
        d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mm, dd), format = "%Y-%m-%d"))
        if (is.na(d)) return(as.Date(NA))
        d
      }, USE.NAMES = FALSE))
    } else {
      # fallback: try DATE11 then YYYY-MM-DD (single strict each) - choose DATE11 parse
      d1 <- sas_input_date11(ch)
      need <- is.na(d1) & ch != ""
      if (any(need)) {
        d2 <- suppressWarnings(as.Date(ch[need], format = "%Y-%m-%d"))
        d1[need] <- d2
      }
      return(d1)
    }
  }

  # --- edc_dm ---
  dm_subjid <- NULL; dm_rf <- NULL
  if (file.exists(f_dm)) {
    edc_dm <- readxl::read_excel(f_dm)
    cat("edc_dm rows:", nrow(edc_dm), " cols:", paste(names(edc_dm), collapse=","), "\n")
    # SUBJID
    sc <- get_colname(edc_dm, "SUBJID")
    if (is.null(sc)) {
      # try USUBJID
      sc2 <- get_colname(edc_dm, "USUBJID")
      if (!is.null(sc2)) {
        dm_subjid <- as_char_clean(edc_dm[[sc2]])
        # if already ONCO-2026-*, keep; else prefix
        dm_subjid <- sapply(dm_subjid, function(s) {
          if (s == "") return("")
          if (grepl("^ONCO-2026-", s)) return(s)
          paste0("ONCO-2026-", s)
        }, USE.NAMES = FALSE)
      }
    } else {
      dm_subjid_raw <- as_char_clean(edc_dm[[sc]])
      dm_subjid <- sas_catx("-", "ONCO-2026", dm_subjid_raw)
    }
    # RFSTDTC direct?
    rfc <- get_colname(edc_dm, "RFSTDTC")
    if (is.null(rfc)) rfc <- get_colname(edc_dm, "RFSTDT")
    if (!is.null(rfc) && !is.null(dm_subjid)) {
      dm_rf <- parse_generic_date(edc_dm[[rfc]])
      df_dm_direct <- data.frame(USUBJID = dm_subjid, RF_DATE = dm_rf, stringsAsFactors = FALSE)
      df_dm_direct <- df_dm_direct[df_dm_direct$USUBJID != "" & !is.na(df_dm_direct$RF_DATE), , drop = FALSE]
      if (nrow(df_dm_direct) > 0) rf_list[[length(rf_list) + 1]] <- df_dm_direct
    }
    # store dm_subjid universe for later (all subjects)
    assign("dm_universe", data.frame(USUBJID = unique(dm_subjid[dm_subjid != ""]), stringsAsFactors = FALSE), envir = .GlobalEnv)
  }

  # --- edc_ex: earliest exposure per subject ---
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
      # find start date column: prefer EXSTDTC/EXSTDT/EXDT/STARTDT/RFSTDTC
      cand_order <- c("EXSTDTC","EXSTDT","EXDT","EXDATE","STARTDTC","STARTDT","TRTSDTC","TRTSDT","RFSTDTC")
      found <- NULL
      for (cn in cand_order) {
        cc <- get_colname(edc_ex, cn)
        if (!is.null(cc)) { found <- cc; break }
      }
      if (is.null(found)) {
        # any column with DTC/DT/DATE in name
        cands <- names(edc_ex)[grepl("DTC$|DT$|DATE", names(edc_ex), ignore.case = TRUE)]
        # exclude end dates
        cands_start <- cands[!grepl("END|STOP|EN$", cands, ignore.case = TRUE)]
        if (length(cands_start) > 0) found <- cands_start[1]
        else if (length(cands) > 0) found <- cands[1]
      }
      if (!is.null(found)) {
        ex_dt <- parse_generic_date(edc_ex[[found]])
        df_ex <- data.frame(USUBJID = ex_usub, RF_DATE = ex_dt, stringsAsFactors = FALSE)
        df_ex <- df_ex[df_ex$USUBJID != "" & !is.na(df_ex$RF_DATE), , drop = FALSE]
        if (nrow(df_ex) > 0) {
          df_ex_min <- df_ex %>% group_by(USUBJID) %>% summarise(RF_DATE = min(RF_DATE), .groups = "drop") %>% as.data.frame()
          # exposure has highest priority -> put first
          rf_list <- c(list(df_ex_min), rf_list)
        }
      }
    }
  }

  # --- ivrs_rand: randomization date ---
  if (file.exists(f_rand)) {
    ivrs <- readxl::read_excel(f_rand)
    cat("ivrs_rand rows:", nrow(ivrs), " cols:", paste(names(ivrs), collapse=","), "\n")
    sc <- get_colname(ivrs, "SUBJID")
    if (is.null(sc)) sc <- get_colname(ivrs, "USUBJID")
    if (is.null(sc)) {
      # try SUBJECT / SUBJ
      for (alt in c("SUBJECT","SUBJ","PATIENT")) {
        sc <- get_colname(ivrs, alt)
        if (!is.null(sc)) break
      }
    }
    if (!is.null(sc)) {
      r_sub_raw <- as_char_clean(ivrs[[sc]])
      r_usub <- sapply(r_sub_raw, function(s) {
        if (s == "") return("")
        if (grepl("^ONCO-2026-", s)) return(s)
        paste0("ONCO-2026-", s)
      }, USE.NAMES = FALSE)
      cand_order2 <- c("RANDDTC","RANDDT","RANDOMDTC","RANDOMDT","ENROLLDTC","ENROLLDT","RANDDATE","DATE")
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
        df_r <- data.frame(USUBJID = r_usub, RF_DATE = r_dt, stringsAsFactors = FALSE)
        df_r <- df_r[df_r$USUBJID != "" & !is.na(df_r$RF_DATE), , drop = FALSE]
        if (nrow(df_r) > 0) {
          df_r_min <- df_r %>% group_by(USUBJID) %>% summarise(RF_DATE = min(RF_DATE), .groups = "drop") %>% as.data.frame()
          rf_list[[length(rf_list) + 1]] <- df_r_min
        }
      }
    }
  }
  rf_list
}

rf_list <- derive_rfstdtc()
# combine by priority: first list wins per USUBJID
dm_for_tu <- NULL
if (length(rf_list) > 0) {
  combined <- bind_rows(rf_list)
  # keep first occurrence per USUBJID respecting priority order (bind_rows preserves order)
  combined <- combined[!is.na(combined$RF_DATE), , drop = FALSE]
  combined <- combined[order(match(combined$USUBJID, unique(combined$USUBJID))), , drop = FALSE]
  # deduplicate: first row per USUBJID
  combined <- combined[!duplicated(combined$USUBJID), , drop = FALSE]
  dm_for_tu <- data.frame(USUBJID = combined$USUBJID,
                          RFSTDTC = fmt_yymmdd10(combined$RF_DATE),
                          stringsAsFactors = FALSE)
} else {
  dm_for_tu <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
}
# ensure all TU subjects present? merge is left join (if t) so missing RFSTDTC -> blank
# sort by USUBJID
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, na.last = FALSE), , drop = FALSE]
cat("dm_for_tu rows:", nrow(dm_for_tu), "\n")
print(head(dm_for_tu))

# ---------- 3. Map TU Domain (merge tu_prep + dm_for_tu) ----------
# SAS merge by USUBJID, keep if t; drop/rename overlapping cols except keys (RFSTDTC only in dm)
# tu_prep has USUBJID; dm_for_tu has USUBJID, RFSTDTC
# left join
tu_prep$RFSTDTC <- NULL
if (nrow(dm_for_tu) > 0) {
  # dedupe dm to one per USUBJID (first)
  dm_ded <- dm_for_tu[!duplicated(dm_for_tu$USUBJID), , drop = FALSE]
  sdtm_tu1 <- merge(tu_prep, dm_ded, by = "USUBJID", all.x = TRUE, sort = FALSE)
} else {
  sdtm_tu1 <- tu_prep
  sdtm_tu1$RFSTDTC <- ""
}
stopifnot("USUBJID" %in% names(sdtm_tu1))
stopifnot("RFSTDTC" %in% names(sdtm_tu1))
sdtm_tu1$RFSTDTC[is.na(sdtm_tu1$RFSTDTC)] <- ""

# derive SDTM vars
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

# TUDTC = put(input(TUDT,date11.),yymmdd10.)
# TUDT_raw may be Date or character
TUDTC <- sapply(seq_len(n1), function(i) {
  chk <- sdtm_tu1$TUDT_chk[i]
  if (chk == "") return("")
  raw <- sdtm_tu1$TUDT_raw_list[i]
  # if raw is Date/POSIXt, format directly
  if (inherits(raw, "Date")) {
    if (is.na(raw)) return("")
    return(format(raw, "%Y-%m-%d"))
  }
  if (inherits(raw, "POSIXt")) {
    if (is.na(raw)) return("")
    return(format(as.Date(raw), "%Y-%m-%d"))
  }
  d <- sas_input_date11(as.character(raw))
  if (is.na(d)) return("")
  format(d, "%Y-%m-%d")
}, USE.NAMES = FALSE)

# Study Day
RFSTDTC <- sapply(sdtm_tu1$RFSTDTC, function(v) {
  if (is.na(v)) return("")
  trimws(as.character(v))
}, USE.NAMES = FALSE)

TUSTDY <- rep(NA_real_, n1)
for (i in seq_len(n1)) {
  if (TUDTC[i] != "" && RFSTDTC[i] != "") {
    stdt <- sas_input_yymmdd10(TUDTC[i])
    rfdt <- sas_input_yymmdd10(RFSTDTC[i])
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

# sort by USUBJID TUDTC (missing first: "" sorts first naturally)
ord <- order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last = FALSE)
sdtm_tu1_out <- sdtm_tu1_out[ord, , drop = FALSE]

# ---------- 4. Derive TUSEQ (by USUBJID) ----------
sdtm_tu1_out <- sdtm_tu1_out[order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last = FALSE), , drop = FALSE]
TUSEQ <- rep(NA_real_, nrow(sdtm_tu1_out))
# retain logic
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
print(head(sdtm_tu))

# ---------- 5/6. Outputs ----------
# dataset tu -> tu.csv with ISO dates, na=""
write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")

# Listing: PROC PRINT of sdtm_tu -> text rendering + CSV of table body
# Build text similar to SAS PROC PRINT (Obs + all vars)
pt <- sdtm_tu
# format numerics as SAS would (BEST / integer)
fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    # TUSEQ/TUSTDY integers
    formatC(v, format = "f", digits = 0, flag = "")
  }, USE.NAMES = FALSE)
}
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

# CSV of table body (same numbers)
write.csv(pt_disp, file.path(out, "P029.csv"), row.names = FALSE, na = "")

# Plain-text rendering
txt_path <- file.path(out, "P029.txt")
sink(txt_path)
cat("PROC PRINT DATA=SDTM_TU\n")
cat("\n")
# print as fixed-width table
# compute widths
cols <- names(pt_disp)
widths <- sapply(cols, function(cn) max(nchar(c(cn, pt_disp[[cn]]), type = "chars"), na.rm = TRUE))
# header
header <- paste(sprintf(paste0("%-", widths, "s"), cols), collapse = "  ")
cat(header, "\n")
sep <- paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "  ")
cat(sep, "\n")
for (i in seq_len(nrow(pt_disp))) {
  rowv <- sapply(cols, function(cn) pt_disp[i, cn])
  # right-align Obs, TUSEQ, TUSTDY like SAS numbers; left-align others
  parts <- sapply(seq_along(cols), function(j) {
    cn <- cols[j]; v <- as.character(rowv[j])
    w <- widths[j]
    if (cn %in% c("Obs", "TUSEQ", "TUSTDY")) sprintf(paste0("%", w, "s"), v)
    else sprintf(paste0("%-", w, "s"), v)
  })
  cat(paste(parts, collapse = "  "), "\n")
}
cat("\n")
cat(paste0("N = ", nrow(pt_disp)), "\n")
sink()

# also copy to P029.lst.txt / P029.lst for benchmark compatibility
try({
  file.copy(txt_path, file.path(out, "P029.lst"), overwrite = TRUE)
}, silent = TRUE)

cat("Done. Wrote tu.csv, P029.csv, P029.txt\n")
