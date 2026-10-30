```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(readxl))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))

root_raw_faijal <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
edc_ae_path   <- file.path(root_raw_faijal, "edc_ae.xlsx")
edc_dm_path   <- file.path(root_raw_faijal, "edc_dm.xlsx")
edc_ex_path   <- file.path(root_raw_faijal, "edc_ex.xlsx")
ivrs_path     <- file.path(root_raw_faijal, "ivrs_rand.xlsx")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, x, fixed = TRUE))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}
sas_substr_trunc <- function(x, n) {
  ifelse(is.na(x), NA_character_, substr(x, 1, n))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(a) {
    a <- as.character(a)
    a[is.na(a)] <- ""
    trimws(a)
  })
  # CATX strips and skips missing/blank? SAS CATX strips and ignores empty? It still joins with sep skipping empties?
  # Here first arg constant non-blank, second SUBJID non-blank, so simple paste
  do.call(paste, c(args, sep = sep))
}

# SAS DATE11. informat: DD-MMM-YYYY e.g. 01-JAN-2020, 1-Jan-2020
parse_date11_to_date <- function(x) {
  # x character vector; return Date vector
  res <- as.Date(rep(NA_character_, length(x)))
  for (i in seq_along(x)) {
    v <- x[i]
    if (is.na(v)) next
    v <- trimws(as.character(v))
    if (v == "" || toupper(v) == "NA" || v == ".") next
    # already ISO? try ISO first if matches YYYY-MM-DD
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", v)) {
      d <- as.Date(v, format = "%Y-%m-%d")
      res[i] <- d
      next
    }
    m <- regmatches(v, regexec("^\\s*([0-9]{1,2})-([A-Za-z]{3})-([0-9]{4})\\s*$", v))[[1]]
    if (length(m) == 4) {
      dd <- suppressWarnings(as.integer(m[2]))
      mon <- toupper(m[3])
      yy <- suppressWarnings(as.integer(m[4]))
      monnum <- match(mon, c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"))
      if (!is.na(dd) && !is.na(monnum) && !is.na(yy)) {
        dstr <- sprintf("%04d-%02d-%02d", yy, monnum, dd)
        d <- as.Date(dstr, format = "%Y-%m-%d")
        res[i] <- d
        next
      }
    }
    # invalid -> NA (SAS missing)
    res[i] <- as.Date(NA)
  }
  res
}
format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}
# Convert raw STDT/ENDT value (could be Date, POSIXct, character, numeric) to ISO via DATE11. semantics
raw_to_iso_via_date11 <- function(x) {
  if (inherits(x, "Date")) {
    return(format_yymmdd10(x))
  }
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(format_yymmdd10(as.Date(x)))
  }
  # character / numeric / factor
  xc <- as.character(x)
  # readxl may give NA as NA_character_
  out <- rep(NA_character_, length(xc))
  is_na <- is.na(xc) | trimws(xc) == ""
  out[is_na] <- NA_character_
  idx <- which(!is_na)
  if (length(idx) > 0) {
    d <- parse_date11_to_date(xc[idx])
    out[idx] <- format_yymmdd10(d)
  }
  out
}
# Generic raw date column to ISO Date (handles Date class or character date11 or ISO)
raw_col_to_date <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) return(as.Date(col))
  xc <- as.character(col)
  # try date11 parse
  d1 <- parse_date11_to_date(xc)
  # for those still NA but non-blank, try ISO
  need <- is.na(d1) & !is.na(xc) & trimws(xc) != ""
  if (any(need)) {
    d2 <- suppressWarnings(as.Date(trimws(xc[need]), format = "%Y-%m-%d"))
    # also try YYYY/MM/DD? SAS ANYDTDTE not needed; keep ISO only + date11
    # try DD/MM/YYYY vs MM/DD/YYYY single-order decision not needed here
    d1[need] <- d2
  }
  # try datetime prefix YYYY-MM-DDTHH:MM:SS -> take date part
  need2 <- is.na(d1) & !is.na(xc) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}T", trimws(xc))
  if (any(need2)) {
    d1[need2] <- suppressWarnings(as.Date(substr(trimws(xc[need2]), 1, 10), format = "%Y-%m-%d"))
  }
  d1
}

sas_sort_order <- function(df, cols) {
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(suppressWarnings(Sys.setlocale("LC_COLLATE", old)), add = TRUE)
  suppressWarnings(Sys.setlocale("LC_COLLATE", "C"))
  args <- lapply(cols, function(cn) {
    v <- df[[cn]]
    if (is.character(v)) {
      v[is.na(v)] <- ""
      v
    } else {
      v
    }
  })
  do.call(order, c(args, list(na.last = FALSE, method = "radix")))
}

read_excel_upper <- function(path) {
  df <- as.data.frame(readxl::read_excel(path, .name_repair = "minimal"), stringsAsFactors = FALSE)
  names(df) <- toupper(trimws(names(df)))
  # trim character values? keep as is but trim trailing blanks like SAS? SAS character comparison trims? Keep original trimmed for CATX etc.
  df
}

message("Reading edc_ae")
edc_ae <- read_excel_upper(edc_ae_path)
stopifnot(all(c("SUBJID") %in% names(edc_ae)))
# Ensure expected columns exist (create as NA if missing to avoid crash, but stopifnot for filter cols)
for (cn in c("STDT","ENDT","TERM","DECOD","BODSYS","SEV","TXGR","REL","OUT","SER")) {
  if (!cn %in% names(edc_ae)) edc_ae[[cn]] <- NA
}

# ------------------------------------------------------------
# Derive dm_for_ae (USUBJID, RFSTDTC)
# ------------------------------------------------------------
find_existing_dm <- function() {
  base <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets"
  files <- character(0)
  if (dir.exists(base)) {
    files <- list.files(base, pattern = "^dm\\.(xlsx|csv|sashdat)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  }
  if (length(files) == 0) return(NULL)
  for (f in files) {
    try({
      if (grepl("\\.xlsx$", f, ignore.case = TRUE)) {
        d <- as.data.frame(readxl::read_excel(f, .name_repair = "minimal"), stringsAsFactors = FALSE)
      } else if (grepl("\\.csv$", f, ignore.case = TRUE)) {
        d <- as.data.frame(read.csv(f, stringsAsFactors = FALSE, check.names = FALSE), stringsAsFactors = FALSE)
      } else next
      names(d) <- toupper(trimws(names(d)))
      if (all(c("USUBJID","RFSTDTC") %in% names(d))) return(d[, c("USUBJID","RFSTDTC")])
      if ("SUBJID" %in% names(d)) {
        # build USUBJID, find RFSTDTC-like col
        cand <- names(d)[grepl("RFSTD", names(d))]
        if (length(cand) >= 1) {
          d$USUBJID <- sas_catx("-", "ONCO-2026", d[["SUBJID"]])
          d$RFSTDTC <- as.character(d[[cand[1]]])
          # normalize to ISO date (take first 10 chars if datetime)
          d$RFSTDTC <- substr(trimws(d$RFSTDTC), 1, 10)
          return(d[, c("USUBJID","RFSTDTC")])
        }
      }
    }, silent = TRUE)
  }
  NULL
}

derive_dm_from_raw <- function(ae_subjids) {
  # ae_subjids: character vector of SUBJID (trimmed)
  subj_all <- data.frame(SUBJID_C = character(0), DT = as.Date(character(0)))
  add_source <- function(df, subj_col, date_cols, src) {
    if (is.null(df) || is.null(subj_col) || length(date_cols) == 0) return(NULL)
    # convert each date col to Date, take min per row then per subject? For exposure multiple rows, min per subject across rows and cols
    dates_list <- lapply(date_cols, function(dc) raw_col_to_date(df[[dc]]))
    # row-wise min? Actually per observation earliest across cols, then per subject min across rows
    m <- do.call(pmin, c(dates_list, list(na.rm = TRUE)))
    # pmin with all NA gives Inf warning; fix
    # do manual: if all NA per row -> NA
    tmp <- data.frame(SUBJID_C = trimws(as.character(df[[subj_col]])), DT = m, stringsAsFactors = FALSE)
    tmp <- tmp[!is.na(tmp$SUBJID_C) & trimws(tmp$SUBJID_C) != "", , drop = FALSE]
    # remove NA dates
    tmp <- tmp[!is.na(tmp$DT), , drop = FALSE]
    tmp
  }
  find_subj_col <- function(df) {
    nms <- names(df)
    if ("SUBJID" %in% nms) return("SUBJID")
    hit <- nms[grepl("SUBJ", nms)]
    if (length(hit) > 0) return(hit[1])
    NULL
  }
  find_date_cols_filtered <- function(df, include_pat, exclude_pat) {
    nms <- names(df)
    # candidate date-like: Date class or character with date values
    cand <- c()
    for (nm in nms) {
      if (grepl(exclude_pat, nm, ignore.case = TRUE)) next
      if (!grepl(include_pat, nm, ignore.case = TRUE)) next
      col <- df[[nm]]
      if (inherits(col, "Date") || inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
        cand <- c(cand, nm)
      } else {
        # check if any value parses as date
        d <- suppressWarnings(raw_col_to_date(col))
        if (any(!is.na(d))) cand <- c(cand, nm)
      }
    }
    cand
  }

  edc_dm <- tryCatch(read_excel_upper(edc_dm_path), error = function(e) NULL)
  edc_ex <- tryCatch(read_excel_upper(edc_ex_path), error = function(e) NULL)
  ivrs  <- tryCatch(read_excel_upper(ivrs_path), error = function(e) NULL)

  parts <- list()

  if (!is.null(ivrs)) {
    sc <- find_subj_col(ivrs)
    # prefer RAND/RF columns, exclude DOB etc.
    dc <- find_date_cols_filtered(ivrs, "RAND|RFSTD|RF_|REFERENCE|RANDDT|RANDDTC", "DOB|BIRTH|DEATH|END|STOP")
    if (length(dc) == 0) {
      # fallback: any date col except DOB/DEATH/END
      dc <- find_date_cols_filtered(ivrs, "DT|DTC|DATE|DAY", "DOB|BIRTH|DEATH")
    }
    p <- add_source(ivrs, sc, dc, "ivrs")
    if (!is.null(p) && nrow(p) > 0) parts[[length(parts)+1]] <- p
  }
  if (!is.null(edc_ex)) {
    sc <- find_subj_col(edc_ex)
    dc <- find_date_cols_filtered(edc_ex, "EX|STD|START|DOSE|FIRST|RF|DT|DTC|DATE", "DOB|BIRTH|DEATH|END|STOP")
    # for EX, exclude END/STOP dates (exposure end) to get start only
    # keep only START/STD/FIRST/RAND/RF if present
    pref <- dc[grepl("STD|START|FIRST|RAND|RF|DOSE", dc, ignore.case = TRUE)]
    if (length(pref) > 0) dc <- pref
    p <- add_source(edc_ex, sc, dc, "ex")
    if (!is.null(p) && nrow(p) > 0) parts[[length(parts)+1]] <- p
  }
  if (!is.null(edc_dm)) {
    sc <- find_subj_col(edc_dm)
    # if RFSTDTC exists use directly
    if ("RFSTDTC" %in% names(edc_dm)) {
      dcol <- raw_col_to_date(edc_dm[["RFSTDTC"]])
      tmp <- data.frame(SUBJID_C = trimws(as.character(edc_dm[[sc]])), DT = dcol, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$SUBJID_C) & tmp$SUBJID_C != "" & !is.na(tmp$DT), , drop = FALSE]
      if (nrow(tmp) > 0) parts[[length(parts)+1]] <- tmp
    } else {
      dc <- find_date_cols_filtered(edc_dm, "RAND|RFSTD|RF_|ENROLL|RANDOM|FIRST|START|STD|DOSE", "DOB|BIRTH|DEATH|END|STOP|DISC")
      if (length(dc) == 0) {
        dc <- find_date_cols_filtered(edc_dm, "DT|DTC|DATE", "DOB|BIRTH|DEATH|END|STOP|DISC")
      }
      p <- add_source(edc_dm, sc, dc, "dm")
      if (!is.null(p) && nrow(p) > 0) parts[[length(parts)+1]] <- p
    }
  }

  if (length(parts) == 0) {
    # no source: return NA RFSTDTC for ae subjects
    usub <- sas_catx("-", "ONCO-2026", ae_subjids)
    return(data.frame(USUBJID = usub, RFSTDTC = NA_character_, stringsAsFactors = FALSE))
  }
  allp <- do.call(rbind, parts)
  # per SUBJID_C take min DT (earliest = randomisation or first exposure)
  allp <- allp[!is.na(allp$DT), , drop = FALSE]
  if (nrow(allp) == 0) {
    usub <- sas_catx("-", "ONCO-2026", ae_subjids)
    return(data.frame(USUBJID = usub, RFSTDTC = NA_character_, stringsAsFactors = FALSE))
  }
  agg <- aggregate(DT ~ SUBJID_C, data = allp, FUN = function(x) min(x, na.rm = TRUE))
  agg$USUBJID <- sas_catx("-", "ONCO-2026", agg$SUBJID_C)
  agg$RFSTDTC <- format_yymmdd10(agg$DT)
  # ensure all ae subjects present
  usub_all <- data.frame(SUBJID_C = trimws(as.character(ae_subjids)), stringsAsFactors = FALSE)
  usub_all$USUBJID <- sas_catx("-", "ONCO-2026", usub_all$SUBJID_C)
  outdm <- merge(usub_all[, "USUBJID", drop = FALSE], agg[, c("USUBJID","RFSTDTC")], by = "USUBJID", all.x = TRUE)
  outdm
}

# Build ae_prep first to get subjids for dm derivation
message("Preparing ae_prep")
# SUBJID as character trimmed
edc_ae$SUBJID_C <- trimws(as.character(edc_ae$SUBJID))
edc_ae$SUBJID_C[is.na(edc_ae$SUBJID_C)] <- ""

ae_prep <- edc_ae
ae_prep$USUBJID <- sas_substr_trunc(sas_catx("-", "ONCO-2026", ae_prep$SUBJID_C), 25)
# STDT/ENDT to ISO via date11.
ae_prep$AESTDTC <- raw_to_iso_via_date11(ae_prep$STDT)
ae_prep$AEENDTC <- raw_to_iso_via_date11(ae_prep$ENDT)
ae_prep$AESTDTC <- sas_substr_trunc(ae_prep$AESTDTC, 19)
ae_prep$AEENDTC <- sas_substr_trunc(ae_prep$AEENDTC, 19)

# Sort ae_prep by USUBJID AESTDTC TERM (SAS missing first, ASCII order)
ae_prep$TERM_C <- as.character(ae_prep$TERM)
ord <- sas_sort_order(ae_prep, c("USUBJID","AESTDTC","TERM_C"))
ae_prep <- ae_prep[ord, , drop = FALSE]

# dm_for_ae
dm_existing <- find_existing_dm()
if (!is.null(dm_existing) && nrow(dm_existing) > 0) {
  message("Using existing DM")
  names(dm_existing) <- toupper(names(dm_existing))
  dm_for_ae <- dm_existing
  dm_for_ae$USUBJID <- trimws(as.character(dm_for_ae$USUBJID))
  dm_for_ae$RFSTDTC <- trimws(as.character(dm_for_ae$RFSTDTC))
  dm_for_ae$RFSTDTC[dm_for_ae$RFSTDTC %in% c("", "NA", ".")] <- NA_character_
  # normalize to YYYY-MM-DD (take 10 chars, parse)
  dm_for_ae$RFSTDTC <- sapply(dm_for_ae$RFSTDTC, function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    v <- trimws(v)
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) return(substr(v, 1, 10))
    d <- raw_col_to_date(v)
    if (is.na(d)) return(NA_character_)
    format(d, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
  dm_for_ae <- dm_for_ae[, c("USUBJID","RFSTDTC"), drop = FALSE]
} else {
  message("Deriving DM RFSTDTC from raw")
  ae_subjids <- unique(ae_prep$SUBJID_C)
  dm_for_ae <- derive_dm_from_raw(ae_subjids)
}
# sort dm_for_ae by USUBJID
dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID, na.last = FALSE), , drop = FALSE]
# dedupe to one per USUBJID (take first / min RFSTDTC)
dm_for_ae <- dm_for_ae[!duplicated(dm_for_ae$USUBJID), , drop = FALSE]
message(paste0("dm_for_ae rows: ", nrow(dm_for_ae)))

# ------------------------------------------------------------
# 4. Final Mapping and Derivation (merge ae_prep + dm_for_ae)
# ------------------------------------------------------------
# Drop overlapping non-key cols before merge to avoid .x/.y
common_cols <- intersect(names(ae_prep), names(dm_for_ae))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  # dm_for_ae keeps RFSTDTC; ae_prep should not have RFSTDTC, but drop if exists
  ae_prep <- ae_prep[, setdiff(names(ae_prep), common_cols), drop = FALSE]
}
stopifnot("USUBJID" %in% names(ae_prep), "USUBJID" %in% names(dm_for_ae))

merged <- merge(ae_prep, dm_for_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)

# Identifier + AE variables with lengths
sdtm_ae1 <- merged
sdtm_ae1$STUDYID <- sas_substr_trunc("ONCO-2026", 10)
sdtm_ae1$DOMAIN  <- sas_substr_trunc("AE", 2)
sdtm_ae1$USUBJID <- sas_substr_trunc(trimws(as.character(sdtm_ae1$USUBJID)), 25)
sdtm_ae1$AETERM  <- sas_substr_trunc(as.character(sdtm_ae1$TERM), 40)
sdtm_ae1$AEDECOD <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$DECOD)), 40)
sdtm_ae1$AEBODSYS <- sas_substr_trunc(sas_tranwrd(as.character(sdtm_ae1$BODSYS), "_", " "), 40)
sdtm_ae1$AESEV   <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$SEV)), 10)
# AETXGR numeric
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))
sdtm_ae1$AEREL   <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$REL)), 20)
sdtm_ae1$AEOUT   <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$OUT)), 20)
sdtm_ae1$AESER   <- sas_substr_trunc(as.character(sdtm_ae1$SER), 1)
# normalize empty strings to NA for character? Keep "" as NA for ISO dates; SAS blank = missing
# AESTDTC/AEENDTC already ISO or NA
# RFSTDTC already ISO or NA

# Derive AESTDY / AEENDY
stdt_d <- suppressWarnings(as.Date(sdtm_ae1$AESTDTC, format = "%Y-%m-%d"))
rfdt_d <- suppressWarnings(as.Date(sdtm_ae1$RFSTDTC, format = "%Y-%m-%d"))
endt_d <- suppressWarnings(as.Date(sdtm_ae1$AEENDTC, format = "%Y-%m-%d"))
# invalid parses -> NA (already)
sdtm_ae1$AESTDY <- ifelse(!is.na(stdt_d) & !is.na(rfdt_d),
  ifelse(stdt_d >= rfdt_d, as.integer(stdt_d - rfdt_d) + 1L, as.integer(stdt_d - rfdt_d)),
  NA_real_)
sdtm_ae1$AEENDY <- ifelse(!is.na(endt_d) & !is.na(rfdt_d),
  ifelse(endt_d >= rfdt_d, as.integer(endt_d - rfdt_d) + 1L, as.integer(endt_d - rfdt_d)),
  NA_real_)

# Ensure AESTDTC/AEENDTC blank for NA (for sorting SAS missing first)
# Keep as character with NA; sorting handles NA first via sas_sort_order

# ------------------------------------------------------------
# 5. Sort Final AE Dataset by USUBJID AESTDTC TERM
# ------------------------------------------------------------
sdtm_ae1$TERM_C <- as.character(sdtm_ae1$TERM)
ord2 <- sas_sort_order(sdtm_ae1, c("USUBJID","AESTDTC","TERM_C"))
sdtm_ae1 <- sdtm_ae1[ord2, , drop = FALSE]

# ------------------------------------------------------------
# 6. Derive AESEQ by USUBJID
# ------------------------------------------------------------
sdtm_ae1 <- sdtm_ae1 %>% arrange(match(seq_len(nrow(sdtm_ae1)), seq_len(nrow(sdtm_ae1)))) # keep current order
# Use base rle for FIRST./LAST.
sdtm_ae1$AESEQ <- NA_real_
# retain logic
prev <- NA_character_
seqn <- 0L
for (i in seq_len(nrow(sdtm_ae1))) {
  cur <- sdtm_ae1$USUBJID[i]
  if (is.na(cur)) cur <- ""
  if (is.na(prev) || cur != prev) {
    seqn <- 1L
  } else {
    seqn <- seqn + 1L
  }
  sdtm_ae1$AESEQ[i] <- seqn
  prev <- cur
}

sdtm_ae <- sdtm_ae1[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY"), drop = FALSE]

# Ensure types: dates as character ISO, numerics as numeric
# Replace empty strings with NA for character cols
for (cn in c("STUDYID","DOMAIN","USUBJID","AETERM","AEDECOD","AEBODSYS","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")) {
  v <- sdtm_ae[[cn]]
  v[v == ""] <- NA_character_
  sdtm_ae[[cn]] <- v
}

message(paste0("sdtm_ae rows: ", nrow(sdtm_ae)))

# ------------------------------------------------------------
# 7/8. PROC PRINT rendering + exports
# ------------------------------------------------------------
# Write dataset ae.csv
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# Build PROC PRINT text
# SAS PROC PRINT prints Obs + all variables in keep order
print_df <- sdtm_ae
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY"), drop = FALSE]

fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    # SAS BEST12. prints integers without decimals
    if (abs(v - round(v)) < 1e-9) formatC(as.integer(round(v)), format = "d") else as.character(v)
  }, USE.NAMES = FALSE)
}
fmt_char <- function(x) {
  sapply(x, function(v) if (is.na(v)) "" else as.character(v), USE.NAMES = FALSE)
}

txt_path <- file.path(out, "P022.txt")
lst_path <- file.path(out, "P022.lst")
csv_path <- file.path(out, "P022.csv")

con <- file(txt_path, open = "wt")
writeLines("PROC PRINT data=sdtm_ae;", con)
writeLines("", con)
# header
hdr <- sprintf("%-4s %-10s %-6s %-15s %-5s %-20s %-20s %-20s %-10s %-5s %-20s %-20s %-4s %-10s %-10s %-6s %-6s",
  "Obs","STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse = ""), con)
for (i in seq_len(nrow(print_df))) {
  r <- print_df[i, ]
  line <- sprintf("%-4s %-10s %-6s %-15s %-5s %-20s %-20s %-20s %-10s %-5s %-20s %-20s %-4s %-10s %-10s %-6s %-6s",
    fmt_char(r$Obs),
    fmt_char(r$STUDYID),
    fmt_char(r$DOMAIN),
    fmt_char(r$USUBJID),
    fmt_num(r$AESEQ),
    fmt_char(r$AETERM),
    fmt_char(r$AEDECOD),
    fmt_char(r$AEBODSYS),
    fmt_char(r$AESEV),
    fmt_num(r$AETXGR),
    fmt_char(r$AEREL),
    fmt_char(r$AEOUT),
    fmt_char(r$AESER),
    fmt_char(r$AESTDTC),
    fmt_char(r$AEENDTC),
    fmt_num(r$AESTDY),
    fmt_num(r$AEENDY))
  writeLines(line, con)
}
close(con)
# copy to .lst
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))
# CSV of table body (same as print without formatting dots? use raw values with na="")
write.csv(print_df, csv_path, row.names = FALSE, na = "")

message("Done")
```