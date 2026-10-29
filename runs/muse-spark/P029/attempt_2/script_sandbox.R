out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x) * mult
  r <- floor(ax + 0.5 + 1e-10) / mult
  s * r
}

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
  # haven labelled numeric?
  if (is.integer(x)) {
    return(as.Date(as.numeric(x), origin = "1960-01-01"))
  }
  s <- trimws(as.character(x))
  n <- length(s)
  res <- rep(as.Date(NA), n)
  if (n == 0) return(res)
  is_na <- is.na(s) | s == "" | tolower(s) == "na" | s == "."
  # ymd on first 10
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
    # sanity: only keep if reasonable (1990-2035)
    ok <- !is.na(conv) & conv >= as.Date("1990-01-01") & conv <= as.Date("2035-12-31")
    tmp <- y1[need4]
    tmp[ok] <- conv[ok]
    y1[need4] <- tmp
  }
  # class may be numeric after lubridate vector ops; ensure Date
  if (is.numeric(y1)) y1 <- as.Date(y1, origin = "1970-01-01")
  return(y1)
}

message("P029 TU mapping: reading inputs")

edc_tu_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_tu.xlsx"
edc_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_ex_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"

edc_tu <- readxl::read_excel(edc_tu_path, .name_repair = "minimal")
edc_tu <- as.data.frame(edc_tu, stringsAsFactors = FALSE)

# ---- 1. Prepare DM Reference Data ----
dm_for_tu <- NULL
found_dm <- FALSE

search_roots <- c("/bench/output/sas/datasets",
                  "/bench/data/derived",
                  "/bench/data/raw/cdisc-pilot/sdtm")
for (rt in search_roots) {
  if (found_dm) break
  if (!dir.exists(rt)) next
  fl <- list.files(rt, pattern = "(?i)dm\\.(xlsx|csv|xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE)
  for (f in fl) {
    try({
      if (grepl("\\.xlsx$", f, ignore.case = TRUE)) {
        tmp <- as.data.frame(readxl::read_excel(f, .name_repair = "minimal"), stringsAsFactors = FALSE)
      } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        tmp <- as.data.frame(readr::read_csv(f, show_col_types = FALSE, name_repair = "minimal"), stringsAsFactors = FALSE)
      } else if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        tmp <- as.data.frame(haven::read_xpt(f), stringsAsFactors = FALSE)
      } else if (grepl("sas7bdat$", f, ignore.case = TRUE)) {
        tmp <- as.data.frame(haven::read_sas(f), stringsAsFactors = FALSE)
      } else next
      cn_u <- get_col(tmp, "USUBJID")
      cn_r <- get_col(tmp, "RFSTDTC")
      if (!is.null(cn_u) && !is.null(cn_r)) {
        raw_u <- tmp[[cn_u]]
        raw_r <- tmp[[cn_r]]
        # zap labels if haven labelled
        try({ raw_u <- haven::zap_labels(raw_u) }, silent = TRUE)
        try({ raw_r <- haven::zap_labels(raw_r) }, silent = TRUE)
        usub_c <- trimws(as.character(raw_u))
        usub_c[usub_c == ""] <- NA_character_
        # robust RFSTDTC -> Date -> YYYY-MM-DD string
        if (inherits(raw_r, "Date")) {
          rf_c <- format(raw_r, "%Y-%m-%d")
        } else if (inherits(raw_r, "POSIXct") || inherits(raw_r, "POSIXlt")) {
          rf_c <- format(as.Date(raw_r), "%Y-%m-%d")
        } else if (is.numeric(raw_r)) {
          rf_d <- as.Date(raw_r, origin = "1960-01-01")
          rf_c <- format(rf_d, "%Y-%m-%d")
        } else {
          rf_d <- parse_any_to_Date(as.character(raw_r))
          rf_c <- format(rf_d, "%Y-%m-%d")
        }
        rf_c[rf_c == "NA" | rf_c == ""] <- NA_character_
        keep <- data.frame(USUBJID = as.character(usub_c), RFSTDTC = as.character(rf_c), stringsAsFactors = FALSE)
        # drop rows with missing USUBJID
        keep <- keep[!is.na(keep$USUBJID) & keep$USUBJID != "", , drop = FALSE]
        dm_for_tu <- keep
        found_dm <- TRUE
        message(paste0("Found sdtm DM reference: ", f, " rows=", nrow(keep)))
        break
      }
    }, silent = TRUE)
  }
}

if (!found_dm || is.null(dm_for_tu)) {
  message("sdtm_dm not found; deriving fallback from edc_dm/edc_ex")
  dm_for_tu <- NULL
  try({
    if (file.exists(edc_dm_path)) {
      edc_dm <- readxl::read_excel(edc_dm_path, .name_repair = "minimal")
      edc_dm <- as.data.frame(edc_dm, stringsAsFactors = FALSE)
      cn_sub <- get_col(edc_dm, "SUBJID")
      cn_rf <- get_col(edc_dm, "RFSTDTC")
      if (!is.null(cn_sub)) {
        subj <- get_vec_char(edc_dm, "SUBJID")
        usub <- sas_catx("-", "ONCO-2026", subj)
        usub[is.na(subj) | trimws(subj) == ""] <- "ONCO-2026"
        if (!is.null(cn_rf)) {
          rf_raw2 <- edc_dm[[cn_rf]]
          try({ rf_raw2 <- haven::zap_labels(rf_raw2) }, silent = TRUE)
          if (inherits(rf_raw2, "Date")) rf <- format(rf_raw2, "%Y-%m-%d")
          else if (inherits(rf_raw2, "POSIXct")) rf <- format(as.Date(rf_raw2), "%Y-%m-%d")
          else if (is.numeric(rf_raw2)) rf <- format(as.Date(rf_raw2, origin = "1960-01-01"), "%Y-%m-%d")
          else rf <- format(parse_any_to_Date(as.character(rf_raw2)), "%Y-%m-%d")
          dm_for_tu <- data.frame(USUBJID = as.character(usub), RFSTDTC = as.character(rf), stringsAsFactors = FALSE)
          found_dm <- TRUE
        } else {
          dm_for_tu <- data.frame(USUBJID = as.character(usub), RFSTDTC = NA_character_, stringsAsFactors = FALSE)
        }
      }
    }
  }, silent = TRUE)
  if (is.null(dm_for_tu)) {
    dm_for_tu <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  }
}

dm_for_tu$USUBJID <- trimws(as.character(dm_for_tu$USUBJID))
dm_for_tu$RFSTDTC <- trimws(as.character(dm_for_tu$RFSTDTC))
dm_for_tu$RFSTDTC[dm_for_tu$RFSTDTC == "" | dm_for_tu$RFSTDTC == "NA"] <- NA_character_
if (nrow(dm_for_tu) > 0) {
  dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, dm_for_tu$RFSTDTC, na.last = FALSE, method = "radix"), , drop = FALSE]
  dm_for_tu <- dm_for_tu[!duplicated(dm_for_tu$USUBJID), , drop = FALSE]
}

# ---- 2. Prepare Raw Data : tu_prep ----
SUBJID_v <- get_vec_char(edc_tu, "SUBJID")
LOC_v <- get_vec_char(edc_tu, "LOC")
METH_v <- get_vec_char(edc_tu, "METH")
LNKID_v <- get_vec_char(edc_tu, "LNKID")
TYPE_v <- get_vec_char(edc_tu, "TYPE")
VISIT_raw <- get_vec_char(edc_tu, "VISIT")
# TUDT may be Date-typed in Excel: handle raw directly for robustness
cn_tudt_raw <- get_col(edc_tu, "TUDT")
TUDT_raw <- NULL
TUDT_date_flex <- NULL
if (!is.null(cn_tudt_raw)) {
  raw_tudt <- edc_tu[[cn_tudt_raw]]
  try({ raw_tudt <- haven::zap_labels(raw_tudt) }, silent = TRUE)
  if (inherits(raw_tudt, "Date")) {
    TUDT_raw <- format(raw_tudt, "%d%b%Y")
    TUDT_date_flex <- raw_tudt
  } else if (inherits(raw_tudt, "POSIXct") || inherits(raw_tudt, "POSIXlt")) {
    TUDT_raw <- format(as.Date(raw_tudt), "%d%b%Y")
    TUDT_date_flex <- as.Date(raw_tudt)
  } else if (is.numeric(raw_tudt)) {
    # Excel numeric date? try excel origin 1899-12-30 and SAS origin
    # Heuristic: if values look like Excel (>30000) use excel, else SAS
    # Prefer parse via both; choose reasonable
    d_excel <- as.Date(raw_tudt, origin = "1899-12-30")
    d_sas <- as.Date(raw_tudt, origin = "1960-01-01")
    # pick which falls in 2020-2030 if one does
    pick <- ifelse(!is.na(d_excel) & d_excel >= as.Date("2020-01-01") & d_excel <= as.Date("2030-12-31"), TRUE, FALSE)
    TUDT_date_flex <- ifelse(pick, d_excel, d_sas)
    TUDT_date_flex <- as.Date(TUDT_date_flex, origin = "1970-01-01")
    TUDT_raw <- format(TUDT_date_flex, "%d%b%Y")
  } else {
    TUDT_raw <- get_vec_char(edc_tu, "TUDT")
  }
} else {
  TUDT_raw <- rep(NA_character_, nrow(edc_tu))
}

n_tu <- nrow(edc_tu)
USUBJID_v <- sas_catx("-", "ONCO-2026", SUBJID_v)
if (length(USUBJID_v) != n_tu) {
  USUBJID_v <- paste0("ONCO-2026-", ifelse(is.na(SUBJID_v), "", trimws(SUBJID_v)))
  USUBJID_v[is.na(SUBJID_v) | trimws(SUBJID_v) == ""] <- "ONCO-2026"
}
USUBJID_v <- trimws(USUBJID_v)
USUBJID_v <- sas_substr_trunc(USUBJID_v, 25)

TULOC_v <- sas_toupper(sas_tranwrd(LOC_v, "_", " "))
TULOC_v <- sas_substr_trunc(TULOC_v, 20)
TUMETHOD_v <- sas_toupper(sas_tranwrd(METH_v, "_", " "))
TUMETHOD_v <- sas_substr_trunc(TUMETHOD_v, 20)

tu_prep <- data.frame(
  USUBJID = USUBJID_v,
  SUBJID = SUBJID_v,
  LOC = LOC_v,
  METH = METH_v,
  LNKID = LNKID_v,
  TYPE = TYPE_v,
  VISIT_raw = VISIT_raw,
  TUDT = TUDT_raw,
  stringsAsFactors = FALSE
)
tu_prep$TULOC <- TULOC_v
tu_prep$TUMETHOD <- TUMETHOD_v
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

# ---- 3. Map TU Domain ----
sdtm_tu1 <- merge(tu_prep, dm_for_tu, by = "USUBJID", all.x = TRUE, sort = FALSE)
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

# Fallback join on numeric SUBJID suffix if all RFSTDTC missing but dm has values
if (nrow(sdtm_tu1) > 0 && nrow(dm_for_tu) > 0 && all(is.na(sdtm_tu1$RFSTDTC))) {
  message("Primary USUBJID join yielded all missing RFSTDTC; trying suffix/numeric fallback")
  tu_suf <- sub(".*-", "", trimws(as.character(sdtm_tu1$USUBJID)))
  dm_suf <- sub(".*-", "", trimws(as.character(dm_for_tu$USUBJID)))
  # exact suffix match
  m1 <- match(tu_suf, dm_suf)
  if (any(!is.na(m1))) {
    sdtm_tu1$RFSTDTC <- dm_for_tu$RFSTDTC[m1]
    message("Fallback suffix join matched")
  } else {
    # numeric match stripping leading zeros
    tu_num <- suppressWarnings(as.numeric(tu_suf))
    dm_num <- suppressWarnings(as.numeric(dm_suf))
    m2 <- match(tu_num, dm_num)
    if (any(!is.na(m2))) {
      sdtm_tu1$RFSTDTC <- dm_for_tu$RFSTDTC[m2]
      message("Fallback numeric join matched")
    }
  }
}
# If still all missing, try edc_dm/ex derived RFSTDTC as last resort
if (nrow(sdtm_tu1) > 0 && all(is.na(sdtm_tu1$RFSTDTC))) {
  try({
    if (file.exists(edc_dm_path)) {
      edc_dm2 <- as.data.frame(readxl::read_excel(edc_dm_path, .name_repair = "minimal"), stringsAsFactors = FALSE)
      cn_s2 <- get_col(edc_dm2, "SUBJID")
      cn_r2 <- get_col(edc_dm2, "RFSTDTC")
      if (!is.null(cn_s2) && !is.null(cn_r2)) {
        s2 <- trimws(as.character(edc_dm2[[cn_s2]]))
        r2raw <- edc_dm2[[cn_r2]]
        if (inherits(r2raw, "Date")) r2 <- format(r2raw, "%Y-%m-%d")
        else if (inherits(r2raw, "POSIXct")) r2 <- format(as.Date(r2raw), "%Y-%m-%d")
        else if (is.numeric(r2raw)) r2 <- format(as.Date(r2raw, origin = "1960-01-01"), "%Y-%m-%d")
        else r2 <- format(parse_any_to_Date(as.character(r2raw)), "%Y-%m-%d")
        tu_suf2 <- sub(".*-", "", trimws(as.character(sdtm_tu1$USUBJID)))
        # match by raw SUBJID string or numeric
        idx <- match(tu_suf2, trimws(as.character(s2)))
        if (any(!is.na(idx))) {
          cand <- r2[idx]
          fill <- is.na(sdtm_tu1$RFSTDTC) & !is.na(cand)
          sdtm_tu1$RFSTDTC[fill] <- cand[fill]
        }
        if (all(is.na(sdtm_tu1$RFSTDTC))) {
          tu_num2 <- suppressWarnings(as.numeric(tu_suf2))
          dm_num2 <- suppressWarnings(as.numeric(trimws(as.character(s2))))
          idx2 <- match(tu_num2, dm_num2)
          if (any(!is.na(idx2))) {
            cand2 <- r2[idx2]
            fill2 <- is.na(sdtm_tu1$RFSTDTC) & !is.na(cand2)
            sdtm_tu1$RFSTDTC[fill2] <- cand2[fill2]
          }
        }
      }
    }
  }, silent = TRUE)
}

STUDYID_v <- sas_substr_trunc(rep("ONCO-2026", nrow(sdtm_tu1)), 10)
DOMAIN_v <- sas_substr_trunc(rep("TU", nrow(sdtm_tu1)), 2)
TULNKID_v <- sas_substr_trunc(as.character(sdtm_tu1$LNKID), 5)
TULNKID_v[is.na(sdtm_tu1$LNKID) | trimws(as.character(sdtm_tu1$LNKID)) == ""] <- NA_character_
TUTESTCD_v <- sas_substr_trunc(rep("TUMIDENT", nrow(sdtm_tu1)), 8)
TUTEST_v <- sas_substr_trunc(rep("Tumor Identification", nrow(sdtm_tu1)), 40)
TUORRES_v <- sas_toupper(as.character(sdtm_tu1$TYPE))
TUORRES_v[is.na(sdtm_tu1$TYPE) | trimws(as.character(sdtm_tu1$TYPE)) == ""] <- NA_character_
TUORRES_v <- sas_substr_trunc(TUORRES_v, 15)
TUSTRESC_v <- TUORRES_v
TUEVAL_v <- sas_substr_trunc(rep("INVESTIGATOR", nrow(sdtm_tu1)), 15)
VISIT_v <- sas_toupper(sas_tranwrd(as.character(sdtm_tu1$VISIT_raw), "_", " "))
VISIT_v[is.na(sdtm_tu1$VISIT_raw) | trimws(as.character(sdtm_tu1$VISIT_raw)) == ""] <- NA_character_
VISIT_v <- sas_substr_trunc(VISIT_v, 20)

TUDTC_v <- rep(NA_character_, nrow(sdtm_tu1))
need_idx <- !is.na(sdtm_tu1$TUDT) & trimws(as.character(sdtm_tu1$TUDT)) != ""
if (any(need_idx)) {
  parsed <- parse_date11_to_ymd(as.character(sdtm_tu1$TUDT[need_idx]))
  parsed <- sas_substr_trunc(parsed, 19)
  TUDTC_v[need_idx] <- parsed
}
# If TUDT was Date-typed and parse failed, use flexible date directly
if (exists("TUDT_date_flex") && !is.null(TUDT_date_flex)) {
  # align TUDT_date_flex to tu_prep order then to sdtm_tu1 order is same as tu_prep order (merge preserves)
  # tu_prep was sorted; sdtm_tu1 sorted same; but merge may have reordered - recompute by USUBJID match is complex.
  # Instead, for any still-missing TUDTC where flexible date available, fill via direct parse of TUDT string already done.
  # No-op: parse_date11 already covers.
  NULL
}
TUDTC_v[TUDTC_v == ""] <- NA_character_

# Study Day - robust parsing of both dates
TUSTDY_v <- rep(NA_real_, nrow(sdtm_tu1))
stdt_all <- parse_any_to_Date(TUDTC_v)
rfdt_all <- parse_any_to_Date(as.character(sdtm_tu1$RFSTDTC))
# parse_any returns Date; ensure Date class
if (is.numeric(stdt_all)) stdt_all <- as.Date(stdt_all, origin = "1970-01-01")
if (is.numeric(rfdt_all)) rfdt_all <- as.Date(rfdt_all, origin = "1970-01-01")
has_both <- !is.na(stdt_all) & !is.na(rfdt_all)
if (any(has_both)) {
  diffd <- as.numeric(stdt_all[has_both] - rfdt_all[has_both])
  adj <- ifelse(stdt_all[has_both] >= rfdt_all[has_both], diffd + 1, diffd)
  TUSTDY_v[has_both] <- adj
}

sdtm_tu1_out <- data.frame(
  STUDYID = STUDYID_v,
  DOMAIN = DOMAIN_v,
  USUBJID = as.character(sdtm_tu1$USUBJID),
  TULNKID = TULNKID_v,
  TUTESTCD = TUTESTCD_v,
  TUTEST = TUTEST_v,
  TUORRES = TUORRES_v,
  TUSTRESC = TUSTRESC_v,
  TULOC = as.character(sdtm_tu1$TULOC),
  TUMETHOD = as.character(sdtm_tu1$TUMETHOD),
  TUEVAL = TUEVAL_v,
  VISIT = VISIT_v,
  TUDTC = TUDTC_v,
  TUSTDY = TUSTDY_v,
  RFSTDTC = as.character(sdtm_tu1$RFSTDTC),
  stringsAsFactors = FALSE
)

ord <- order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last = FALSE, method = "radix")
sdtm_tu1_out <- sdtm_tu1_out[ord, , drop = FALSE]

TUSEQ_v <- rep(NA_real_, nrow(sdtm_tu1_out))
if (nrow(sdtm_tu1_out) > 0) {
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

sdtm_tu <- sdtm_tu1_out[, c("STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT"), drop = FALSE]

message(paste0("sdtm_tu rows: ", nrow(sdtm_tu)))

write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")

print_df <- sdtm_tu
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT"), drop = FALSE]

txt_path <- file.path(out, "P029.txt")
lst_path <- file.path(out, "P029.lst")
csv_path <- file.path(out, "P029.csv")

write.csv(print_df, csv_path, row.names = FALSE, na = "")

con_txt <- file(txt_path, open = "wt")
writeLines("Listing P029: SDTM TU Mapping & Derivation (PROC PRINT DATA=SDTM_TU)", con_txt)
writeLines(paste0("Rows: ", nrow(sdtm_tu)), con_txt)
writeLines("", con_txt)
hdr <- paste(names(print_df), collapse = " | ")
writeLines(hdr, con_txt)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con_txt)
for (i in seq_len(nrow(print_df))) {
  row <- print_df[i, , drop = FALSE]
  vals <- sapply(names(print_df), function(nm) {
    v <- row[[nm]]
    if (nm %in% c("Obs","TUSEQ","TUSTDY")) {
      if (is.na(v)) return("")
      return(formatC(as.integer(v), format = "d"))
    } else {
      if (is.na(v)) return("")
      return(as.character(v))
    }
  }, USE.NAMES = FALSE)
  writeLines(paste(vals, collapse = " | "), con_txt)
}
close(con_txt)
file.copy(txt_path, lst_path, overwrite = TRUE)

message("Outputs written to OUT_DIR")
