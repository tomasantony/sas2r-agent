Fixing the missing study-day values — rebuilding the reference-date join to match SAS.```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_toupper <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}
sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, x, fixed = TRUE))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  n <- max(lengths(args))
  args <- lapply(args, function(a) {
    if (length(a) == 1L) rep(a, n) else a
  })
  vapply(seq_len(n), function(i) {
    parts <- vapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return("")
      v <- trimws(as.character(v))
      if (v == "") return("")
      v
    }, character(1))
    parts <- parts[parts != ""]
    if (length(parts) == 0) return("")
    paste(parts, collapse = sep)
  }, character(1))
}
sas_substr_trunc <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(x, 1, len))
}
get_col <- function(df, target) {
  nm <- names(df)
  idx <- which(tolower(trimws(nm)) == tolower(target))
  if (length(idx) == 0) return(NULL)
  nm[idx[1]]
}
get_vec_char <- function(df, target) {
  cn <- get_col(df, target)
  if (is.null(cn)) return(rep(NA_character_, nrow(df)))
  v <- df[[cn]]
  if (inherits(v, "Date")) return(format(v, "%Y-%m-%d"))
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) return(format(as.Date(v), "%Y-%m-%d"))
  v <- as.character(v)
  v[v == "NA"] <- NA_character_
  v
}

parse_date11_to_ymd <- function(x) {
  sapply(x, function(one) {
    if (is.na(one)) return(NA_character_)
    s <- trimws(as.character(one))
    if (s == "" || tolower(s) == "na" || s == ".") return(NA_character_)
    d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    d2 <- suppressWarnings(lubridate::ymd(substr(s, 1, 10), quiet = TRUE))
    if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    d3 <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
    if (!is.na(d3)) return(format(d3, "%Y-%m-%d"))
    d4 <- suppressWarnings(lubridate::ymd_hms(s, quiet = TRUE))
    if (!is.na(d4)) return(format(as.Date(d4), "%Y-%m-%d"))
    return(NA_character_)
  }, USE.NAMES = FALSE)
}

parse_any_to_Date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  s <- trimws(as.character(x))
  n <- length(s)
  if (n == 0) return(as.Date(character(0)))
  is_na <- is.na(s) | s == "" | tolower(s) == "na" | s == "."
  y1 <- suppressWarnings(lubridate::ymd(substr(s, 1, 10), quiet = TRUE))
  need <- is.na(y1) & !is_na
  if (any(need, na.rm = TRUE)) {
    y2 <- suppressWarnings(lubridate::dmy(s[need], quiet = TRUE))
    y1[need] <- y2
  }
  need2 <- is.na(y1) & !is_na
  if (any(need2, na.rm = TRUE)) {
    y3 <- suppressWarnings(lubridate::mdy(s[need2], quiet = TRUE))
    y1[need2] <- y3
  }
  need3 <- is.na(y1) & !is_na
  if (any(need3, na.rm = TRUE)) {
    y4 <- suppressWarnings(lubridate::ymd_hms(s[need3], quiet = TRUE))
    y1[need3] <- as.Date(y4)
  }
  need4 <- is.na(y1) & !is_na & grepl("^[0-9]{4,6}$", s)
  if (any(need4, na.rm = TRUE)) {
    nums <- suppressWarnings(as.numeric(s[need4]))
    conv <- as.Date(nums, origin = "1960-01-01")
    ok <- !is.na(conv) & conv >= as.Date("1990-01-01") & conv <= as.Date("2035-12-31")
    tmp <- y1[need4]
    tmp[ok] <- conv[ok]
    y1[need4] <- tmp
  }
  if (is.numeric(y1)) y1 <- as.Date(y1, origin = "1970-01-01")
  return(y1)
}

norm_rf_to_ymd <- function(raw) {
  # raw can be Date/POSIX/numeric/character
  try({ raw <- haven::zap_labels(raw) }, silent = TRUE)
  if (inherits(raw, "Date")) return(format(raw, "%Y-%m-%d"))
  if (inherits(raw, "POSIXct") || inherits(raw, "POSIXlt")) return(format(as.Date(raw), "%Y-%m-%d"))
  if (is.numeric(raw)) {
    # try SAS origin first, then excel origin; prefer reasonable 2020-2030
    d_sas <- as.Date(raw, origin = "1960-01-01")
    d_xl  <- as.Date(raw, origin = "1899-12-30")
    pick_xl <- !is.na(d_xl) & d_xl >= as.Date("2020-01-01") & d_xl <= as.Date("2030-12-31")
    pick_sas <- !is.na(d_sas) & d_sas >= as.Date("2020-01-01") & d_sas <= as.Date("2030-12-31")
    # prefer SAS if both reasonable? SAS dates ~23000, excel ~45000
    res <- ifelse(pick_sas, format(d_sas, "%Y-%m-%d"),
           ifelse(pick_xl, format(d_xl, "%Y-%m-%d"), format(d_sas, "%Y-%m-%d")))
    res[res == "NA"] <- NA_character_
    return(res)
  }
  s <- trimws(as.character(raw))
  s[s == "" | tolower(s) == "na" | s == "."] <- NA_character_
  # fast path: already YYYY-MM-DD at start
  out <- rep(NA_character_, length(s))
  has <- !is.na(s)
  # extract first 10 if matches date pattern
  m <- regmatches(s[has], regexpr("^[0-9]{4}-[0-9]{2}-[0-9]{2}", s[has]))
  # regexpr returns "" if no match? handle
  idx_has <- which(has)
  for (k in seq_along(idx_has)) {
    i <- idx_has[k]
    mm <- m[k]
    if (!is.na(mm) && nchar(mm) == 10) {
      out[i] <- mm
    }
  }
  need <- has & is.na(out)
  if (any(need)) {
    d <- parse_any_to_Date(s[need])
    out[need] <- format(d, "%Y-%m-%d")
  }
  out[out == "NA"] <- NA_character_
  out
}

message("P029 TU mapping: reading inputs")

edc_tu_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx"
edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"

edc_tu <- as.data.frame(readxl::read_excel(edc_tu_path, .name_repair = "minimal"), stringsAsFactors = FALSE)

SUBJID_v <- get_vec_char(edc_tu, "SUBJID")
LOC_v <- get_vec_char(edc_tu, "LOC")
METH_v <- get_vec_char(edc_tu, "METH")
LNKID_v <- get_vec_char(edc_tu, "LNKID")
TYPE_v <- get_vec_char(edc_tu, "TYPE")
VISIT_raw <- get_vec_char(edc_tu, "VISIT")
cn_tudt_raw <- get_col(edc_tu, "TUDT")
if (!is.null(cn_tudt_raw)) {
  raw_tudt0 <- edc_tu[[cn_tudt_raw]]
  try({ raw_tudt0 <- haven::zap_labels(raw_tudt0) }, silent = TRUE)
  if (inherits(raw_tudt0, "Date")) TUDT_raw <- format(raw_tudt0, "%d%b%Y")
  else if (inherits(raw_tudt0, "POSIXct") || inherits(raw_tudt0, "POSIXlt")) TUDT_raw <- format(as.Date(raw_tudt0), "%d%b%Y")
  else if (is.numeric(raw_tudt0)) {
    d_sas <- as.Date(raw_tudt0, origin = "1960-01-01")
    d_xl <- as.Date(raw_tudt0, origin = "1899-12-30")
    pick_xl <- !is.na(d_xl) & d_xl >= as.Date("2020-01-01") & d_xl <= as.Date("2030-12-31")
    d_pick <- ifelse(pick_xl, d_xl, d_sas)
    d_pick <- as.Date(d_pick, origin = "1970-01-01")
    TUDT_raw <- format(d_pick, "%d%b%Y")
  } else TUDT_raw <- get_vec_char(edc_tu, "TUDT")
} else TUDT_raw <- rep(NA_character_, nrow(edc_tu))

n_tu <- nrow(edc_tu)
USUBJID_v <- sas_catx("-", "ONCO-2026", SUBJID_v)
if (length(USUBJID_v) != n_tu) {
  USUBJID_v <- paste0("ONCO-2026-", ifelse(is.na(SUBJID_v), "", trimws(SUBJID_v)))
  USUBJID_v[is.na(SUBJID_v) | trimws(SUBJID_v) == ""] <- "ONCO-2026"
}
USUBJID_v <- trimws(USUBJID_v)
USUBJID_v <- sas_substr_trunc(USUBJID_v, 25)

TULOC_v <- sas_substr_trunc(sas_toupper(sas_tranwrd(LOC_v, "_", " ")), 20)
TUMETHOD_v <- sas_substr_trunc(sas_toupper(sas_tranwrd(METH_v, "_", " ")), 20)

tu_prep <- data.frame(
  USUBJID = USUBJID_v, SUBJID = SUBJID_v, LOC = LOC_v, METH = METH_v,
  LNKID = LNKID_v, TYPE = TYPE_v, VISIT_raw = VISIT_raw, TUDT = TUDT_raw,
  TULOC = TULOC_v, TUMETHOD = TUMETHOD_v, stringsAsFactors = FALSE
)
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]
message(paste0("TU USUBJIDs: ", paste(unique(tu_prep$USUBJID), collapse=",")))
message(paste0("TU SUBJIDs: ", paste(unique(tu_prep$SUBJID), collapse=",")))
message(paste0("TU TUDTs: ", paste(unique(tu_prep$TUDT), collapse=",")))

# ---- Collect ALL DM candidates ----
search_roots <- c("D:/meta_hackathon/sas2r-agent/output/sas/datasets",
                  "D:/meta_hackathon/sas2r-agent/data/derived",
                  "D:/meta_hackathon/sas2r-agent/data/raw",
                  "D:/meta_hackathon/sas2r-agent")
cand_files <- character(0)
for (rt in search_roots) {
  if (!dir.exists(rt)) next
  fl <- list.files(rt, pattern = "(?i)dm\\.(xlsx|csv|xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE)
  cand_files <- c(cand_files, fl)
}
cand_files <- unique(cand_files)
message(paste0("DM candidate files: ", length(cand_files)))
for (f in head(cand_files, 20)) message(paste0("  cand: ", f))

master_lookup <- data.frame(USUBJID=character(0), RFSTDTC=character(0), src=character(0), stringsAsFactors=FALSE)
for (f in cand_files) {
  try({
    if (grepl("\\.xlsx$", f, ignore.case = TRUE)) tmp <- as.data.frame(readxl::read_excel(f, .name_repair="minimal"), stringsAsFactors=FALSE)
    else if (grepl("\\.csv$", f, ignore.case = TRUE)) tmp <- as.data.frame(readr::read_csv(f, show_col_types=FALSE, name_repair="minimal"), stringsAsFactors=FALSE)
    else if (grepl("\\.xpt$", f, ignore.case = TRUE)) tmp <- as.data.frame(haven::read_xpt(f), stringsAsFactors=FALSE)
    else if (grepl("sas7bdat$", f, ignore.case = TRUE)) tmp <- as.data.frame(haven::read_sas(f), stringsAsFactors=FALSE)
    else next
    cn_u <- get_col(tmp, "USUBJID"); cn_r <- get_col(tmp, "RFSTDTC")
    if (is.null(cn_u) || is.null(cn_r)) next
    uu <- trimws(as.character(haven::zap_labels(tmp[[cn_u]])))
    # keep even if uu has different case? preserve
    rr <- norm_rf_to_ymd(tmp[[cn_r]])
    keep <- data.frame(USUBJID=uu, RFSTDTC=rr, src=f, stringsAsFactors=FALSE)
    keep <- keep[!is.na(keep$USUBJID) & keep$USUBJID != "", , drop=FALSE]
    # score
    exact <- sum(unique(tu_prep$USUBJID) %in% keep$USUBJID)
    nonmiss <- sum(!is.na(keep$RFSTDTC))
    message(paste0("file=", f, " n=", nrow(keep), " exact_overlap=", exact, " nonmiss_rf=", nonmiss,
                   " sample_usub=", paste(head(unique(keep$USUBJID),3), collapse="|"),
                   " sample_rf=", paste(head(keep$RFSTDTC[!is.na(keep$RFSTDTC)],3), collapse="|")))
    master_lookup <- rbind(master_lookup, keep)
  }, silent=TRUE)
}

# Also add edc_dm-derived lookup (USUBJID constructed same as TU)
try({
  if (file.exists(edc_dm_path)) {
    edc_dm <- as.data.frame(readxl::read_excel(edc_dm_path, .name_repair="minimal"), stringsAsFactors=FALSE)
    cn_s <- get_col(edc_dm, "SUBJID")
    if (!is.null(cn_s)) {
      s_raw <- get_vec_char(edc_dm, cn_s)
      u2 <- sas_catx("-", "ONCO-2026", s_raw)
      # find best RFSTDTC-like column
      cand_names <- c("RFSTDTC","RFSTDT","RFSTDAT","RANDDT","ENRDT","RFICDT")
      cn_r2 <- NULL
      for (cc in cand_names) { t2 <- get_col(edc_dm, cc); if (!is.null(t2)) { cn_r2 <- t2; break } }
      if (is.null(cn_r2)) {
        # pick first Date-typed column
        for (nm in names(edc_dm)) {
          if (inherits(edc_dm[[nm]], "Date") || inherits(edc_dm[[nm]], "POSIXct")) { cn_r2 <- nm; break }
        }
      }
      if (!is.null(cn_r2)) {
        r2 <- norm_rf_to_ymd(edc_dm[[cn_r2]])
        add <- data.frame(USUBJID=trimws(as.character(u2)), RFSTDTC=r2, src="edc_dm", stringsAsFactors=FALSE)
        message(paste0("edc_dm lookup n=", nrow(add), " col=", cn_r2, " nonmiss=", sum(!is.na(r2)),
                       " sample_usub=", paste(head(unique(add$USUBJID),5), collapse="|")))
        master_lookup <- rbind(master_lookup, add)
      } else {
        message("edc_dm: no RFSTDTC-like column found")
        message(paste0("edc_dm cols: ", paste(names(edc_dm), collapse="|")))
      }
    }
  }
}, silent=FALSE)

# EX-derived lookup (first EX date per subject as RFSTDTC)
try({
  if (file.exists(edc_ex_path)) {
    edc_ex <- as.data.frame(readxl::read_excel(edc_ex_path, .name_repair="minimal"), stringsAsFactors=FALSE)
    cn_es <- get_col(edc_ex, "SUBJID")
    if (!is.null(cn_es)) {
      es_raw <- get_vec_char(edc_ex, cn_es)
      eu <- sas_catx("-", "ONCO-2026", es_raw)
      cand2 <- c("EXSTDTC","EXDTC","EXSTDT","EXDT","EXDATE","STDTC","DTC","EXSTDAT","EXDAT")
      cn_ed <- NULL
      for (cc in cand2) { t2 <- get_col(edc_ex, cc); if (!is.null(t2)) { cn_ed <- t2; break } }
      if (is.null(cn_ed)) {
        for (nm in names(edc_ex)) {
          if (inherits(edc_ex[[nm]], "Date") || inherits(edc_ex[[nm]], "POSIXct")) { cn_ed <- nm; break }
        }
      }
      message(paste0("edc_ex cols: ", paste(names(edc_ex), collapse="|")))
      if (!is.null(cn_ed)) {
        eraw <- edc_ex[[cn_ed]]
        try({ eraw <- haven::zap_labels(eraw) }, silent=TRUE)
        if (inherits(eraw, "Date")) ed <- format(eraw, "%Y-%m-%d")
        else if (inherits(eraw, "POSIXct")) ed <- format(as.Date(eraw), "%Y-%m-%d")
        else if (is.numeric(eraw)) {
          d_sas <- as.Date(eraw, origin="1960-01-01"); d_xl <- as.Date(eraw, origin="1899-12-30")
          pick_xl <- !is.na(d_xl) & d_xl >= as.Date("2020-01-01") & d_xl <= as.Date("2030-12-31")
          ed <- format(ifelse(pick_xl, d_xl, d_sas), "%Y-%m-%d")
          ed <- as.character(as.Date(as.numeric(ifelse(pick_xl, d_xl, d_sas)), origin="1970-01-01"))
          ed <- format(parse_any_to_Date(ed), "%Y-%m-%d")
        } else ed <- norm_rf_to_ymd(eraw)
        exdf <- data.frame(USUBJID=trimws(as.character(eu)), EXD=ed, stringsAsFactors=FALSE)
        exdf <- exdf[!is.na(exdf$USUBJID) & exdf$USUBJID!="" & !is.na(exdf$EXD), , drop=FALSE]
        if (nrow(exdf)>0) {
          exdf <- exdf[order(exdf$USUBJID, exdf$EXD, na.last=FALSE, method="radix"), , drop=FALSE]
          agg <- aggregate(EXD ~ USUBJID, data=exdf, FUN=function(x) sort(x)[1])
          add2 <- data.frame(USUBJID=agg$USUBJID, RFSTDTC=agg$EXD, src="edc_ex_first", stringsAsFactors=FALSE)
          message(paste0("edc_ex lookup n=", nrow(add2), " sample=", paste(head(paste0(add2$USUBJID,"=",add2$RFSTDTC),5), collapse="|")))
          master_lookup <- rbind(master_lookup, add2)
        }
      } else message("edc_ex: no date column found")
    }
  }
}, silent=FALSE)

# Dedupe master: prefer non-missing RFSTDTC, prefer sdtm sources over edc
if (nrow(master_lookup)>0) {
  master_lookup$hasrf <- ifelse(!is.na(master_lookup$RFSTDTC) & master_lookup$RFSTDTC!="", 1, 0)
  master_lookup$prio <- ifelse(grepl("edc", master_lookup$src, ignore.case=TRUE), 2, 1)
  master_lookup <- master_lookup[order(master_lookup$USUBJID, -master_lookup$hasrf, master_lookup$prio, master_lookup$RFSTDTC, na.last=FALSE, method="radix"), , drop=FALSE]
  master_lookup <- master_lookup[!duplicated(master_lookup$USUBJID), , drop=FALSE]
  message(paste0("master lookup n=", nrow(master_lookup)))
}

# Helper to resolve RFSTDTC for each TU USUBJID with multi-key fallback
resolve_rf <- function(usub_vec, subj_vec, lookup) {
  n <- length(usub_vec)
  res <- rep(NA_character_, n)
  if (nrow(lookup)==0) return(res)
  # exact
  m <- match(usub_vec, lookup$USUBJID)
  res <- lookup$RFSTDTC[m]
  # suffix exact for still-missing
  need <- which(is.na(res))
  if (length(need)>0) {
    lu_suf <- sub(".*-", "", trimws(as.character(lookup$USUBJID)))
    tu_suf <- sub(".*-", "", trimws(as.character(usub_vec)))
    # build map suffix -> first non-missing RFSTDTC
    dfm <- data.frame(suf=lu_suf, rf=lookup$RFSTDTC, stringsAsFactors=FALSE)
    dfm <- dfm[!is.na(dfm$suf) & dfm$suf!="" & !is.na(dfm$rf) & dfm$rf!="", , drop=FALSE]
    dfm <- dfm[order(dfm$suf, dfm$rf, method="radix"), , drop=FALSE]
    dfm <- dfm[!duplicated(dfm$suf), , drop=FALSE]
    mm <- match(tu_suf[need], dfm$suf)
    fill <- !is.na(mm)
    res[need[fill]] <- dfm$rf[mm[fill]]
  }
  # numeric suffix for still-missing
  need2 <- which(is.na(res))
  if (length(need2)>0) {
    lu_suf2 <- sub(".*-", "", trimws(as.character(lookup$USUBJID)))
    tu_suf2 <- sub(".*-", "", trimws(as.character(usub_vec[need2])))
    lu_num <- suppressWarnings(as.numeric(lu_suf2))
    tu_num <- suppressWarnings(as.numeric(tu_suf2))
    # also try raw SUBJID numeric
    tu_num2 <- suppressWarnings(as.numeric(trimws(as.character(subj_vec[need2]))))
    tu_num_comb <- ifelse(!is.na(tu_num), tu_num, tu_num2)
    dfn <- data.frame(num=lu_num, rf=lookup$RFSTDTC, stringsAsFactors=FALSE)
    dfn <- dfn[!is.na(dfn$num) & !is.na(dfn$rf) & dfn$rf!="", , drop=FALSE]
    dfn <- dfn[order(dfn$num, dfn$rf, method="radix"), , drop=FALSE]
    dfn <- dfn[!duplicated(dfn$num), , drop=FALSE]
    mm2 <- match(tu_num_comb, dfn$num)
    fill2 <- !is.na(mm2)
    res[need2[fill2]] <- dfn$rf[mm2[fill2]]
  }
  # direct SUBJID string match against lookup suffix (handles leading-zero diff via numeric already)
  res
}

dm_for_tu_exact <- data.frame(USUBJID=character(0), RFSTDTC=character(0), stringsAsFactors=FALSE)
# Build dm_for_tu as exact-match-first lookup (to mimic SAS merge), but resolved robustly
rf_resolved <- resolve_rf(tu_prep$USUBJID, tu_prep$SUBJID, master_lookup)
message(paste0("resolved RFSTDTC: ", paste(rf_resolved, collapse="|")))
dm_for_tu <- data.frame(USUBJID=tu_prep$USUBJID, RFSTDTC=rf_resolved, stringsAsFactors=FALSE)
# For merge emulation keep one row per USUBJID
dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, dm_for_tu$RFSTDTC, na.last=FALSE, method="radix"), , drop=FALSE]
dm_for_tu <- dm_for_tu[!duplicated(dm_for_tu$USUBJID), , drop=FALSE]

# ---- Merge (left join, SAS: merge tu_prep(in=t) dm_for_tu by USUBJID; if t) ----
sdtm_tu1 <- merge(tu_prep, dm_for_tu, by="USUBJID", all.x=TRUE, sort=FALSE)
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, na.last=FALSE, method="radix"), , drop=FALSE]

STUDYID_v <- sas_substr_trunc(rep("ONCO-2026", nrow(sdtm_tu1)), 10)
DOMAIN_v <- sas_substr_trunc(rep("TU", nrow(sdtm_tu1)), 2)
TULNKID_v <- sas_substr_trunc(as.character(sdtm_tu1$LNKID), 5)
TULNKID_v[is.na(sdtm_tu1$LNKID) | trimws(as.character(sdtm_tu1$LNKID))==""] <- NA_character_
TUTESTCD_v <- sas_substr_trunc(rep("TUMIDENT", nrow(sdtm_tu1)), 8)
TUTEST_v <- sas_substr_trunc(rep("Tumor Identification", nrow(sdtm_tu1)), 40)
TUORRES_v <- sas_toupper(as.character(sdtm_tu1$TYPE))
TUORRES_v[is.na(sdtm_tu1$TYPE) | trimws(as.character(sdtm_tu1$TYPE))==""] <- NA_character_
TUORRES_v <- sas_substr_trunc(TUORRES_v, 15)
TUSTRESC_v <- TUORRES_v
TUEVAL_v <- sas_substr_trunc(rep("INVESTIGATOR", nrow(sdtm_tu1)), 15)
VISIT_v <- sas_toupper(sas_tranwrd(as.character(sdtm_tu1$VISIT_raw), "_", " "))
VISIT_v[is.na(sdtm_tu1$VISIT_raw) | trimws(as.character(sdtm_tu1$VISIT_raw))==""] <- NA_character_
VISIT_v <- sas_substr_trunc(VISIT_v, 20)

TUDTC_v <- rep(NA_character_, nrow(sdtm_tu1))
need_idx <- !is.na(sdtm_tu1$TUDT) & trimws(as.character(sdtm_tu1$TUDT)) != ""
if (any(need_idx)) {
  parsed <- parse_date11_to_ymd(as.character(sdtm_tu1$TUDT[need_idx]))
  parsed <- sas_substr_trunc(parsed, 19)
  TUDTC_v[need_idx] <- parsed
}
TUDTC_v[TUDTC_v==""] <- NA_character_
message(paste0("TUDTC: ", paste(TUDTC_v, collapse="|")))
message(paste0("RFSTDTC: ", paste(as.character(sdtm_tu1$RFSTDTC), collapse="|")))

TUSTDY_v <- rep(NA_real_, nrow(sdtm_tu1))
stdt_all <- parse_any_to_Date(TUDTC_v)
rfdt_all <- parse_any_to_Date(as.character(sdtm_tu1$RFSTDTC))
if (is.numeric(stdt_all)) stdt_all <- as.Date(stdt_all, origin="1970-01-01")
if (is.numeric(rfdt_all)) rfdt_all <- as.Date(rfdt_all, origin="1970-01-01")
has_both <- !is.na(stdt_all) & !is.na(rfdt_all)
if (any(has_both)) {
  diffd <- as.numeric(stdt_all[has_both] - rfdt_all[has_both])
  adj <- ifelse(stdt_all[has_both] >= rfdt_all[has_both], diffd + 1, diffd)
  TUSTDY_v[has_both] <- adj
}
message(paste0("TUSTDY: ", paste(TUSTDY_v, collapse="|")))

sdtm_tu1_out <- data.frame(
  STUDYID=STUDYID_v, DOMAIN=DOMAIN_v, USUBJID=as.character(sdtm_tu1$USUBJID),
  TULNKID=TULNKID_v, TUTESTCD=TUTESTCD_v, TUTEST=TUTEST_v,
  TUORRES=TUORRES_v, TUSTRESC=TUSTRESC_v,
  TULOC=as.character(sdtm_tu1$TULOC), TUMETHOD=as.character(sdtm_tu1$TUMETHOD),
  TUEVAL=TUEVAL_v, VISIT=VISIT_v, TUDTC=TUDTC_v, TUSTDY=TUSTDY_v,
  stringsAsFactors=FALSE
)
ord <- order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last=FALSE, method="radix")
sdtm_tu1_out <- sdtm_tu1_out[ord, , drop=FALSE]
TUSEQ_v <- rep(NA_real_, nrow(sdtm_tu1_out))
if (nrow(sdtm_tu1_out)>0) {
  prev <- c(NA_character_, head(sdtm_tu1_out$USUBJID, -1))
  cur <- sdtm_tu1_out$USUBJID
  is_first <- is.na(cur) != is.na(prev) | (!is.na(cur) & !is.na(prev) & cur != prev)
  is_first[1] <- TRUE
  seqn <- 0
  for (i in seq_len(nrow(sdtm_tu1_out))) {
    if (is_first[i]) seqn <- 1 else seqn <- seqn + 1
    TUSEQ_v[i] <- seqn
  }
}
sdtm_tu1_out$TUSEQ <- TUSEQ_v
sdtm_tu <- sdtm_tu1_out[, c("STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT"), drop=FALSE]
message(paste0("sdtm_tu rows: ", nrow(sdtm_tu)))
write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names=FALSE, na="")

print_df <- sdtm_tu
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT"), drop=FALSE]
txt_path <- file.path(out, "P029.txt")
lst_path <- file.path(out, "P029.lst")
csv_path <- file.path(out, "P029.csv")
write.csv(print_df, csv_path, row.names=FALSE, na="")
con_txt <- file(txt_path, open="wt")
writeLines("Listing P029: SDTM TU Mapping & Derivation (PROC PRINT DATA=SDTM_TU)", con_txt)
writeLines(paste0("Rows: ", nrow(sdtm_tu)), con_txt)
writeLines("", con_txt)
hdr <- paste(names(print_df), collapse=" | ")
writeLines(hdr, con_txt)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con_txt)
for (i in seq_len(nrow(print_df))) {
  row <- print_df[i, , drop=FALSE]
  vals <- sapply(names(print_df), function(nm) {
    v <- row[[nm]]
    if (nm %in% c("Obs","TUSEQ","TUSTDY")) {
      if (is.na(v)) return("")
      return(formatC(as.integer(v), format="d"))
    } else {
      if (is.na(v)) return("")
      return(as.character(v))
    }
  }, USE.NAMES=FALSE)
  writeLines(paste(vals, collapse=" | "), con_txt)
}
close(con_txt)
file.copy(txt_path, lst_path, overwrite=TRUE)
message("Outputs written to OUT_DIR")
```