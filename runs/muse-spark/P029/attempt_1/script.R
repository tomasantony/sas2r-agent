out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x) * mult
  # SAS rounds half away from zero
  r <- floor(ax + 0.5 + 1e-10) / mult
  s * r
}

sas_toupper <- function(x) {
  # preserve NA, empty stays empty
  ifelse(is.na(x), NA_character_, toupper(x))
}
sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, x, fixed = TRUE))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  # catx strips leading/trailing blanks and ignores missing/blank
  # simplify: trim each, drop NA/""
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
  # numeric excel dates? leave as character; if all NA keep NA
  v <- as.character(v)
  v[v == "NA"] <- NA_character_
  # trim? SAS would keep blanks; treat "" as missing later
  v
}

parse_date11_to_ymd <- function(x) {
  # x character vector (TUDT raw). Return YYYY-MM-DD or NA
  sapply(x, function(one) {
    if (is.na(one)) return(NA_character_)
    s <- trimws(as.character(one))
    if (s == "" || tolower(s) == "na" || s == ".") return(NA_character_)
    # if already YYYY-MM-DD
    # try Date object already handled; here character
    # Try lubridate dmy first (handles 01JAN2026, 01-JAN-2026, 01/JAN/2026, 01 JAN 2026)
    d <- suppressWarnings(lubridate::dmy(s, quiet = TRUE))
    if (!is.na(d)) return(format(d, "%Y-%m-%d"))
    # try dmy with dashes removed? already covered
    # try ymd (in case Excel exported ISO)
    d2 <- suppressWarnings(lubridate::ymd(substr(s, 1, 10), quiet = TRUE))
    if (!is.na(d2)) return(format(d2, "%Y-%m-%d"))
    # try mdy
    d3 <- suppressWarnings(lubridate::mdy(s, quiet = TRUE))
    if (!is.na(d3)) return(format(d3, "%Y-%m-%d"))
    # try any ymd_hms
    d4 <- suppressWarnings(lubridate::ymd_hms(s, quiet = TRUE))
    if (!is.na(d4)) return(format(as.Date(d4), "%Y-%m-%d"))
    return(NA_character_)
  }, USE.NAMES = FALSE)
}
parse_yymmdd10 <- function(x) {
  sapply(x, function(one) {
    if (is.na(one)) return(as.Date(NA))
    s <- trimws(as.character(one))
    if (s == "" ) return(as.Date(NA))
    # SAS yymmdd10 reads first 10 chars YYYY-MM-DD
    s10 <- substr(s, 1, 10)
    d <- suppressWarnings(lubridate::ymd(s10, quiet = TRUE))
    if (!is.na(d)) return(d)
    # fallback: try full parse
    d2 <- suppressWarnings(lubridate::ymd(s, quiet = TRUE))
    if (!is.na(d2)) return(d2)
    return(as.Date(NA))
  }, USE.NAMES = FALSE)
}
# need numeric Date origin handling for sapply returning numeric
parse_yymmdd10_date <- function(x) {
  res <- parse_yymmdd10(x)
  # sapply may return numeric; convert
  if (is.numeric(res)) res <- as.Date(res, origin = "1970-01-01")
  if (inherits(res, "Date")) return(res)
  # list case
  as.Date(unlist(res), origin = "1970-01-01")
}

message("P029 TU mapping: reading inputs")

edc_tu_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx"
edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
edc_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"

edc_tu <- readxl::read_excel(edc_tu_path, .name_repair = "minimal")
edc_tu <- as.data.frame(edc_tu, stringsAsFactors = FALSE)

# ---- 1. Prepare DM Reference Data : try to locate sdtm_dm ----
dm_for_tu <- NULL
found_dm <- FALSE

# candidate search locations
search_roots <- c("D:/meta_hackathon/sas2r-agent/output/sas/datasets",
                  "D:/meta_hackathon/sas2r-agent/data/derived",
                  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm")
for (rt in search_roots) {
  if (found_dm) break
  if (!dir.exists(rt)) next
  # look for dm files
  fl <- list.files(rt, pattern = "(?i)(^|/|\\\\)dm\\.(xlsx|csv|xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE)
  # also dm in subfolders with prefix? e.g., sdtm_dm
  if (length(fl) == 0) {
    fl2 <- list.files(rt, pattern = "(?i)dm\\.(xlsx|csv|xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE)
    fl <- fl2
  }
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
        keep <- tmp[, c(cn_u, cn_r), drop = FALSE]
        names(keep) <- c("USUBJID", "RFSTDTC")
        # normalize RFSTDTC to character YYYY-MM-DD... keep original string but ensure format
        # if Date/POSIX convert
        if (inherits(keep$RFSTDTC, "Date")) keep$RFSTDTC <- format(keep$RFSTDTC, "%Y-%m-%d")
        else if (inherits(keep$RFSTDTC, "POSIXct")) keep$RFSTDTC <- format(as.Date(keep$RFSTDTC), "%Y-%m-%d")
        else {
          keep$RFSTDTC <- as.character(keep$RFSTDTC)
          keep$RFSTDTC[keep$RFSTDTC == "" | keep$RFSTDTC == "NA"] <- NA_character_
        }
        keep$USUBJID <- as.character(keep$USUBJID)
        dm_for_tu <- keep
        found_dm <- TRUE
        message(paste0("Found sdtm DM reference: ", f))
        break
      }
    }, silent = TRUE)
  }
}

if (!found_dm || is.null(dm_for_tu)) {
  message("sdtm_dm not found in search paths; deriving fallback from edc_dm/edc_ex")
  dm_for_tu <- NULL
  # try edc_dm RFSTDTC
  try({
    if (file.exists(edc_dm_path)) {
      edc_dm <- readxl::read_excel(edc_dm_path, .name_repair = "minimal")
      edc_dm <- as.data.frame(edc_dm, stringsAsFactors = FALSE)
      cn_sub <- get_col(edc_dm, "SUBJID")
      cn_rf <- get_col(edc_dm, "RFSTDTC")
      if (!is.null(cn_sub)) {
        subj <- get_vec_char(edc_dm, "SUBJID")
        usub <- sas_catx("-", "ONCO-2026", subj)
        # blank SUBJID -> "ONCO-2026" per catx; keep
        usub[is.na(subj) | trimws(subj) == ""] <- "ONCO-2026"
        if (!is.null(cn_rf)) {
          rf <- edc_dm[[cn_rf]]
          if (inherits(rf, "Date")) rf <- format(rf, "%Y-%m-%d")
          else if (inherits(rf, "POSIXct")) rf <- format(as.Date(rf), "%Y-%m-%d")
          else rf <- as.character(rf)
          rf[trimws(rf) == "" ] <- NA_character_
          dm_for_tu <- data.frame(USUBJID = as.character(usub), RFSTDTC = as.character(rf), stringsAsFactors = FALSE)
          found_dm <- TRUE
        } else {
          # no RFSTDTC in edc_dm: try to derive from ex dates later, create placeholder
          dm_for_tu <- data.frame(USUBJID = as.character(usub), RFSTDTC = NA_character_, stringsAsFactors = FALSE)
        }
      }
    }
  }, silent = TRUE)
  # try to refine RFSTDTC from edc_ex first dose date if RFSTDTC all missing
  try({
    if (!is.null(dm_for_tu) && all(is.na(dm_for_tu$RFSTDTC)) && file.exists(edc_ex_path)) {
      edc_ex <- readxl::read_excel(edc_ex_path, .name_repair = "minimal")
      edc_ex <- as.data.frame(edc_ex, stringsAsFactors = FALSE)
      cn_esub <- get_col(edc_ex, "SUBJID")
      # find date column: prefer EXSTDTC, EXDTC, EXDT, EXSTDAT, STDTC, DTC, DATE
      cand <- c("EXSTDTC","EXDTC","EXSTDT","EXDT","EXDATE","STDTC","DTC","EXSTDAT")
      cn_edt <- NULL
      for (cc in cand) {
        tmpc <- get_col(edc_ex, cc)
        if (!is.null(tmpc)) { cn_edt <- tmpc; break }
      }
      if (is.null(cn_edt)) {
        # fallback: first column with date-like values or Date class
        for (nm in names(edc_ex)) {
          v <- edc_ex[[nm]]
          if (inherits(v, "Date") || inherits(v, "POSIXct")) { cn_edt <- nm; break }
        }
      }
      if (!is.null(cn_esub) && !is.null(cn_edt)) {
        esub <- get_vec_char(edc_ex, cn_esub)
        eusub <- sas_catx("-", "ONCO-2026", esub)
        edt_raw <- edc_ex[[cn_edt]]
        if (inherits(edt_raw, "Date")) edt <- format(edt_raw, "%Y-%m-%d")
        else if (inherits(edt_raw, "POSIXct")) edt <- format(as.Date(edt_raw), "%Y-%m-%d")
        else edt <- parse_date11_to_ymd(as.character(edt_raw))
        # also try ymd parse if still NA
        need <- is.na(edt)
        if (any(need)) {
          alt <- suppressWarnings(lubridate::ymd(as.character(edt_raw[need]), quiet = TRUE))
          edt[need] <- format(alt, "%Y-%m-%d")
        }
        ex_df <- data.frame(USUBJID = eusub, EXD = edt, stringsAsFactors = FALSE)
        ex_df <- ex_df[!is.na(ex_df$USUBJID) & ex_df$USUBJID != "" & !is.na(ex_df$EXD), , drop = FALSE]
        if (nrow(ex_df) > 0) {
          # SAS sort ordering: uppercase before lowercase; use radix
          ex_df <- ex_df[order(ex_df$USUBJID, ex_df$EXD, na.last = FALSE, method = "radix"), , drop = FALSE]
          # first per subject
          first_ex <- aggregate(EXD ~ USUBJID, data = ex_df, FUN = function(x) sort(x)[1])
          names(first_ex)[2] <- "RFSTDTC"
          # merge into dm_for_tu
          dm_for_tu$RFSTDTC <- NULL
          dm_for_tu <- merge(dm_for_tu, first_ex, by = "USUBJID", all.x = TRUE, sort = FALSE)
        }
      }
    }
  }, silent = TRUE)
  if (is.null(dm_for_tu)) {
    dm_for_tu <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  }
}

# ensure character and sort by USUBJID (SAS missing first, ASCII)
dm_for_tu$USUBJID <- as.character(dm_for_tu$USUBJID)
dm_for_tu$RFSTDTC <- as.character(dm_for_tu$RFSTDTC)
dm_for_tu$RFSTDTC[dm_for_tu$RFSTDTC == ""] <- NA_character_
# dedupe: keep first per USUBJID after sort
if (nrow(dm_for_tu) > 0) {
  dm_for_tu <- dm_for_tu[order(dm_for_tu$USUBJID, dm_for_tu$RFSTDTC, na.last = FALSE, method = "radix"), , drop = FALSE]
  dm_for_tu <- dm_for_tu[!duplicated(dm_for_tu$USUBJID), , drop = FALSE]
}

# ---- 2. Prepare Raw Data : tu_prep ----
# edc_tu columns case-insensitive: SUBJID, LOC, METH, LNKID, TYPE, VISIT, TUDT
SUBJID_v <- get_vec_char(edc_tu, "SUBJID")
LOC_v <- get_vec_char(edc_tu, "LOC")
METH_v <- get_vec_char(edc_tu, "METH")
LNKID_v <- get_vec_char(edc_tu, "LNKID")
TYPE_v <- get_vec_char(edc_tu, "TYPE")
VISIT_raw <- get_vec_char(edc_tu, "VISIT")
TUDT_raw <- get_vec_char(edc_tu, "TUDT")
# TUDT may have been read as Date if Excel typed; get_vec_char already formatted Dates.
# But if original column was Date class, get_vec_char handled; if numeric, handle:
cn_tudt <- get_col(edc_tu, "TUDT")
if (!is.null(cn_tudt) && (inherits(edc_tu[[cn_tudt]], "Date") || inherits(edc_tu[[cn_tudt]], "POSIXct"))) {
  # already formatted above
} else if (!is.null(cn_tudt) && is.numeric(edc_tu[[cn_tudt]])) {
  # unlikely; try excel origin? leave as NA
}

n_tu <- nrow(edc_tu)
USUBJID_v <- sas_catx("-", "ONCO-2026", SUBJID_v)
# catx already handles NA -> "ONCO-2026"; ensure vector length
# sas_catx with vector returns vector; check
if (length(USUBJID_v) != n_tu) {
  USUBJID_v <- paste0("ONCO-2026-", ifelse(is.na(SUBJID_v), "", trimws(SUBJID_v)))
  USUBJID_v[is.na(SUBJID_v) | trimws(SUBJID_v) == ""] <- "ONCO-2026"
}
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
  TULOC = TULOC_v,
  TUMETHOD = TUMETHOD_v,
  stringsAsFactors = FALSE
)
# SAS sorts: by USUBJID
tu_prep <- tu_prep[order(tu_prep$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

# ---- 3. Map TU Domain : sdtm_tu1 ----
# merge tu_prep(in=t) dm_for_tu; by USUBJID; if t; => left join
sdtm_tu1 <- merge(tu_prep, dm_for_tu, by = "USUBJID", all.x = TRUE, sort = FALSE)
# restore sort by USUBJID (merge may reorder); SAS merge preserves sorted order
sdtm_tu1 <- sdtm_tu1[order(sdtm_tu1$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

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

# TUDTC = put(input(TUDT,date11.),yymmdd10.) if TUDT ne ""
TUDTC_v <- rep(NA_character_, nrow(sdtm_tu1))
need_idx <- !is.na(sdtm_tu1$TUDT) & trimws(as.character(sdtm_tu1$TUDT)) != ""
if (any(need_idx)) {
  parsed <- parse_date11_to_ymd(as.character(sdtm_tu1$TUDT[need_idx]))
  # SAS put yymmdd10 gives 10-char; truncate to 19 length (fits)
  parsed <- sas_substr_trunc(parsed, 19)
  TUDTC_v[need_idx] <- parsed
}
TUDTC_v[TUDTC_v == ""] <- NA_character_

# Study Day
TUSTDY_v <- rep(NA_real_, nrow(sdtm_tu1))
has_both <- !is.na(TUDTC_v) & TUDTC_v != "" & !is.na(sdtm_tu1$RFSTDTC) & trimws(as.character(sdtm_tu1$RFSTDTC)) != ""
if (any(has_both)) {
  stdt <- parse_yymmdd10_date(TUDTC_v[has_both])
  rfdt <- parse_yymmdd10_date(as.character(sdtm_tu1$RFSTDTC[has_both]))
  # compute diff in days
  diffd <- as.integer(stdt - rfdt)
  TUSTDY_v[has_both] <- ifelse(!is.na(diffd) & !is.na(stdt) & !is.na(rfdt),
    ifelse(stdt >= rfdt, diffd + 1L, diffd), NA_real_)
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

# ---- 4. Derive TUSEQ : sort by USUBJID TUDTC ----
# SAS missing sorts first for TUDTC
ord <- order(sdtm_tu1_out$USUBJID, sdtm_tu1_out$TUDTC, na.last = FALSE, method = "radix")
sdtm_tu1_out <- sdtm_tu1_out[ord, , drop = FALSE]

# retain TUSEQ by USUBJID
TUSEQ_v <- rep(NA_real_, nrow(sdtm_tu1_out))
if (nrow(sdtm_tu1_out) > 0) {
  # identify first.USUBJID: compare to previous (missing==missing as same group)
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

# ---- 5/6. View + Export ----
# Write dataset tu.csv
write.csv(sdtm_tu, file.path(out, "tu.csv"), row.names = FALSE, na = "")

# Listing rendering: mimic PROC PRINT
# Create text file P029.txt and P029.lst, and CSV P029.csv
print_df <- sdtm_tu
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","TUSEQ","TULNKID","TUTESTCD","TUTEST","TUORRES","TUSTRESC","TULOC","TUMETHOD","TUEVAL","TUDTC","TUSTDY","VISIT"), drop = FALSE]

# Format numbers as integers without decimals for display
fmt_cell <- function(v) {
  ifelse(is.na(v) | v == "", "", as.character(v))
}

txt_path <- file.path(out, "P029.txt")
lst_path <- file.path(out, "P029.lst")
csv_path <- file.path(out, "P029.csv")

# CSV of table body (same as tu but with Obs? use print_df without extra formatting)
write.csv(print_df, csv_path, row.names = FALSE, na = "")

# Build plain-text rendering
con_txt <- file(txt_path, open = "wt")
writeLines("Listing P029: SDTM TU Mapping & Derivation (PROC PRINT DATA=SDTM_TU)", con_txt)
writeLines(paste0("Rows: ", nrow(sdtm_tu)), con_txt)
writeLines("", con_txt)
# header
hdr <- paste(names(print_df), collapse = " | ")
writeLines(hdr, con_txt)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con_txt)
for (i in seq_len(nrow(print_df))) {
  row <- print_df[i, , drop = FALSE]
  # ensure TUSEQ/TUSTDY/Obs shown as integers
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
# copy to .lst
file.copy(txt_path, lst_path, overwrite = TRUE)

message("Outputs written to OUT_DIR")
