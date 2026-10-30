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
  do.call(paste, c(args, sep = sep))
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
  df
}

format_yymmdd10 <- function(d) {
  if (inherits(d, "Date")) {
    ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
  } else {
    d <- as.Date(d, origin = "1970-01-01")
    ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
  }
}

# ---- strict single-format date column parser (PROC IMPORT GUESSINGROWS logic) ----
parse_date_col <- function(col) {
  if (inherits(col, "Date")) return(col)
  if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) return(as.Date(col))
  if (is.numeric(col)) {
    # Excel serial dates (only called for date columns)
    return(suppressWarnings(as.Date(col, origin = "1899-12-30")))
  }
  xc <- trimws(as.character(col))
  n <- length(xc)
  res <- as.Date(rep(NA_character_, n))
  is_blank <- is.na(xc) | xc == "" | toupper(xc) == "NA" | xc == "."
  idx <- which(!is_blank)
  if (length(idx) == 0) return(res)
  vals <- xc[idx]
  # ISO YYYY-MM-DD (possibly with time suffix)
  if (all(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}(T.*| .*)?$", vals))) {
    for (k in seq_along(idx)) {
      v <- vals[k]
      dpart <- substr(v, 1, 10)
      res[idx[k]] <- suppressWarnings(as.Date(dpart, format = "%Y-%m-%d"))
    }
    return(res)
  }
  # DD-MMM-YYYY or DD-MMM-YY (DATE11./DATE9.)
  if (all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", vals))) {
    monmap <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
    for (k in seq_along(idx)) {
      v <- vals[k]
      m <- regmatches(v, regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{2,4})$", v))[[1]]
      if (length(m) == 4) {
        dd <- suppressWarnings(as.integer(m[2])); mon <- monmap[[toupper(m[3])]]
        yy <- suppressWarnings(as.integer(m[4]))
        if (!is.na(yy) && yy < 100) yy <- ifelse(yy <= 69, 2000 + yy, 1900 + yy)
        if (!is.na(dd) && !is.na(mon) && !is.na(yy)) {
          res[idx[k]] <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mon, dd), format = "%Y-%m-%d"))
        }
      }
    }
    return(res)
  }
  # slash or dash numeric n/n/yyyy
  if (all(grepl("^[0-9]{1,2}[/-][0-9]{1,2}[/-][0-9]{2,4}$", vals))) {
    sep <- ifelse(any(grepl("/", vals)), "/", "-")
    parts <- strsplit(vals, sep, fixed = TRUE)
    firsts <- suppressWarnings(as.integer(sapply(parts, `[`, 1)))
    order <- if (any(firsts > 12, na.rm = TRUE)) "DMY" else "MDY"
    fmt <- if (order == "DMY") "%d/%m/%Y" else "%m/%d/%Y"
    if (sep == "-") fmt <- gsub("/", "-", fmt, fixed = TRUE)
    # handle 2-digit years
    for (k in seq_along(idx)) {
      v <- vals[k]
      # normalize 2-digit year to 4-digit for strptime
      pp <- strsplit(v, sep, fixed = TRUE)[[1]]
      if (nchar(pp[3]) == 2) {
        yy2 <- suppressWarnings(as.integer(pp[3]))
        pp[3] <- as.character(ifelse(yy2 <= 69, 2000 + yy2, 1900 + yy2))
        v <- paste(pp, collapse = sep)
      }
      res[idx[k]] <- suppressWarnings(as.Date(v, format = fmt))
    }
    return(res)
  }
  # fallback: try ISO prefix then DATE11 per value but with single decision:
  # decide by majority pattern
  for (k in seq_along(idx)) {
    v <- vals[k]
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) {
      res[idx[k]] <- suppressWarnings(as.Date(substr(v, 1, 10), format = "%Y-%m-%d"))
    } else if (grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", v)) {
      m <- regmatches(v, regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{2,4})$", v))[[1]]
      if (length(m) == 4) {
        monmap <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
        dd <- suppressWarnings(as.integer(m[2])); mon <- monmap[[toupper(m[3])]]
        yy <- suppressWarnings(as.integer(m[4]))
        if (!is.na(yy) && yy < 100) yy <- ifelse(yy <= 69, 2000 + yy, 1900 + yy)
        if (!is.na(dd) && !is.na(mon) && !is.na(yy))
          res[idx[k]] <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", yy, mon, dd), format = "%Y-%m-%d"))
      }
    } else {
      res[idx[k]] <- suppressWarnings(as.Date(v, format = "%Y-%m-%d"))
    }
  }
  res
}

raw_to_iso_via_date11 <- function(x) {
  if (inherits(x, "Date")) return(format_yymmdd10(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(format_yymmdd10(as.Date(x)))
  if (is.numeric(x)) return(format_yymmdd10(suppressWarnings(as.Date(x, origin = "1899-12-30"))))
  d <- parse_date_col(as.character(x))
  format_yymmdd10(d)
}

message("Reading edc_ae")
edc_ae <- read_excel_upper(edc_ae_path)
stopifnot(all(c("SUBJID") %in% names(edc_ae)))
for (cn in c("STDT","ENDT","TERM","DECOD","BODSYS","SEV","TXGR","REL","OUT","SER")) {
  if (!cn %in% names(edc_ae)) edc_ae[[cn]] <- NA
}

find_subj_col <- function(df) {
  nms <- names(df)
  if ("SUBJID" %in% nms) return("SUBJID")
  hit <- nms[grepl("SUBJ", nms)]
  if (length(hit) > 0) return(hit[1])
  NULL
}
is_birth_col <- function(nm) grepl("BIRTH|BRTH|BORN|DOB", nm, ignore.case = TRUE)
is_death_col <- function(nm) grepl("DEATH|DTH|DOD", nm, ignore.case = TRUE)

# Derive dm_for_ae strictly from RFSTDTC definition: first exposure else randomization
derive_dm_from_raw <- function(ae_subjids) {
  edc_dm <- tryCatch(read_excel_upper(edc_dm_path), error = function(e) NULL)
  edc_ex <- tryCatch(read_excel_upper(edc_ex_path), error = function(e) NULL)
  ivrs  <- tryCatch(read_excel_upper(ivrs_path), error = function(e) NULL)

  subj_df <- data.frame(SUBJID_C = trimws(as.character(ae_subjids)), stringsAsFactors = FALSE)
  subj_df$USUBJID <- sas_catx("-", "ONCO-2026", subj_df$SUBJID_C)
  subj_df$RF_EX <- as.Date(rep(NA_character_, nrow(subj_df)))
  subj_df$RF_RAND <- as.Date(rep(NA_character_, nrow(subj_df)))

  agg_min_per_subj <- function(df, subj_col, date_cols) {
    if (is.null(df) || is.null(subj_col) || length(date_cols) == 0) return(NULL)
    dl <- lapply(date_cols, function(dc) parse_date_col(df[[dc]]))
    # row-wise min across date cols
    n <- nrow(df)
    rowmin <- as.Date(rep(NA_character_, n))
    for (i in seq_len(n)) {
      vals <- sapply(dl, function(v) v[i])
      vals <- vals[!is.na(vals)]
      if (length(vals) > 0) rowmin[i] <- as.Date(min(vals), origin = "1970-01-01")
    }
    tmp <- data.frame(SUBJID_C = trimws(as.character(df[[subj_col]])), DT = rowmin, stringsAsFactors = FALSE)
    tmp <- tmp[!is.na(tmp$SUBJID_C) & tmp$SUBJID_C != "" & !is.na(tmp$DT), , drop = FALSE]
    if (nrow(tmp) == 0) return(NULL)
    agg <- aggregate(DT ~ SUBJID_C, data = tmp, FUN = function(x) as.Date(min(x), origin = "1970-01-01"))
    agg
  }

  # IVRS randomization date columns: contain RAND, not birth/death
  if (!is.null(ivrs)) {
    sc <- find_subj_col(ivrs)
    cand <- names(ivrs)[grepl("RAND", names(ivrs), ignore.case = TRUE) & !sapply(names(ivrs), is_birth_col) & !sapply(names(ivrs), is_death_col)]
    # also RFSTD* if present
    cand2 <- names(ivrs)[grepl("RFSTD", names(ivrs), ignore.case = TRUE)]
    cand <- unique(c(cand, cand2))
    # drop subj-like cols
    cand <- cand[!grepl("SUBJ|ID$", cand, ignore.case = TRUE) | grepl("DT|DTC|DATE", cand, ignore.case = TRUE)]
    if (length(cand) > 0 && !is.null(sc)) {
      a <- agg_min_per_subj(ivrs, sc, cand)
      if (!is.null(a)) {
        m <- match(subj_df$SUBJID_C, a$SUBJID_C)
        subj_df$RF_RAND <- as.Date(ifelse(is.na(m), NA, a$DT[m]), origin = "1970-01-01")
      }
    }
  }
  # EX first exposure: columns with STD or START, excluding END/STOP, birth/death
  if (!is.null(edc_ex)) {
    sc <- find_subj_col(edc_ex)
    cand <- names(edc_ex)[grepl("STD|START", names(edc_ex), ignore.case = TRUE)]
    cand <- cand[!grepl("END|STOP", cand, ignore.case = TRUE)]
    cand <- cand[!sapply(cand, is_birth_col) & !sapply(cand, is_death_col)]
    if (length(cand) > 0 && !is.null(sc)) {
      a <- agg_min_per_subj(edc_ex, sc, cand)
      if (!is.null(a)) {
        m <- match(subj_df$SUBJID_C, a$SUBJID_C)
        subj_df$RF_EX <- as.Date(ifelse(is.na(m), NA, a$DT[m]), origin = "1970-01-01")
      }
    }
  }
  # DM randomization/enrollment/RFSTD (not birth)
  dm_rand <- as.Date(rep(NA_character_, nrow(subj_df)))
  if (!is.null(edc_dm)) {
    sc <- find_subj_col(edc_dm)
    if ("RFSTDTC" %in% names(edc_dm)) {
      dcol <- parse_date_col(edc_dm[["RFSTDTC"]])
      tmp <- data.frame(SUBJID_C = trimws(as.character(edc_dm[[sc]])), DT = dcol, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$SUBJID_C) & tmp$SUBJID_C != "" & !is.na(tmp$DT), , drop = FALSE]
      if (nrow(tmp) > 0) {
        agg <- aggregate(DT ~ SUBJID_C, data = tmp, FUN = function(x) as.Date(min(x), origin = "1970-01-01"))
        m <- match(subj_df$SUBJID_C, agg$SUBJID_C)
        dm_rand <- as.Date(ifelse(is.na(m), NA, agg$DT[m]), origin = "1970-01-01")
      }
    } else {
      cand <- names(edc_dm)[grepl("RAND|RFSTD|ENROLL", names(edc_dm), ignore.case = TRUE)]
      cand <- cand[!sapply(cand, is_birth_col) & !sapply(cand, is_death_col)]
      if (length(cand) > 0 && !is.null(sc)) {
        a <- agg_min_per_subj(edc_dm, sc, cand)
        if (!is.null(a)) {
          m <- match(subj_df$SUBJID_C, a$SUBJID_C)
          dm_rand <- as.Date(ifelse(is.na(m), NA, a$DT[m]), origin = "1970-01-01")
        }
      }
    }
  }
  # RFSTDTC = first exposure if available else randomization (IVRS or DM)
  rf <- subj_df$RF_EX
  need <- is.na(rf)
  rf[need] <- subj_df$RF_RAND[need]
  need2 <- is.na(rf)
  rf[need2] <- dm_rand[need2]
  subj_df$RFSTDTC <- format_yymmdd10(rf)
  subj_df[, c("USUBJID", "RFSTDTC"), drop = FALSE]
}

message("Preparing ae_prep")
edc_ae$SUBJID_C <- trimws(as.character(edc_ae$SUBJID))
edc_ae$SUBJID_C[is.na(edc_ae$SUBJID_C)] <- ""

ae_prep <- edc_ae
ae_prep$USUBJID <- sas_substr_trunc(sas_catx("-", "ONCO-2026", ae_prep$SUBJID_C), 25)
ae_prep$AESTDTC <- raw_to_iso_via_date11(ae_prep$STDT)
ae_prep$AEENDTC <- raw_to_iso_via_date11(ae_prep$ENDT)
ae_prep$AESTDTC <- sas_substr_trunc(ae_prep$AESTDTC, 19)
ae_prep$AEENDTC <- sas_substr_trunc(ae_prep$AEENDTC, 19)

ae_prep$TERM_C <- as.character(ae_prep$TERM)
ord <- sas_sort_order(ae_prep, c("USUBJID","AESTDTC","TERM_C"))
ae_prep <- ae_prep[ord, , drop = FALSE]

# dm_for_ae: check for existing derived DM first
find_existing_dm <- function() {
  base <- "D:/meta_hackathon/sas2r-agent/output/sas/datasets"
  files <- character(0)
  if (dir.exists(base)) {
    files <- list.files(base, pattern = "^dm\\.(xlsx|csv)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
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
    }, silent = TRUE)
  }
  NULL
}
dm_existing <- find_existing_dm()
if (!is.null(dm_existing) && nrow(dm_existing) > 0) {
  message("Using existing DM")
  names(dm_existing) <- toupper(names(dm_existing))
  dm_for_ae <- dm_existing
  dm_for_ae$USUBJID <- trimws(as.character(dm_for_ae$USUBJID))
  dm_for_ae$RFSTDTC <- trimws(as.character(dm_for_ae$RFSTDTC))
  dm_for_ae$RFSTDTC[dm_for_ae$RFSTDTC %in% c("", "NA", ".")] <- NA_character_
  dm_for_ae$RFSTDTC <- sapply(dm_for_ae$RFSTDTC, function(v) {
    if (is.na(v) || v == "") return(NA_character_)
    v <- trimws(v)
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) return(substr(v, 1, 10))
    d <- parse_date_col(v)
    if (is.na(d)) return(NA_character_)
    format(d, "%Y-%m-%d")
  }, USE.NAMES = FALSE)
  dm_for_ae <- dm_for_ae[, c("USUBJID","RFSTDTC"), drop = FALSE]
} else {
  message("Deriving DM RFSTDTC from raw")
  ae_subjids <- unique(ae_prep$SUBJID_C)
  dm_for_ae <- derive_dm_from_raw(ae_subjids)
}
dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID, na.last = FALSE), , drop = FALSE]
dm_for_ae <- dm_for_ae[!duplicated(dm_for_ae$USUBJID), , drop = FALSE]
message(paste0("dm_for_ae rows: ", nrow(dm_for_ae)))

common_cols <- intersect(names(ae_prep), names(dm_for_ae))
common_cols <- setdiff(common_cols, "USUBJID")
if (length(common_cols) > 0) {
  ae_prep <- ae_prep[, setdiff(names(ae_prep), common_cols), drop = FALSE]
}
stopifnot("USUBJID" %in% names(ae_prep), "USUBJID" %in% names(dm_for_ae))

merged <- merge(ae_prep, dm_for_ae, by = "USUBJID", all.x = TRUE, sort = FALSE)

sdtm_ae1 <- merged
sdtm_ae1$STUDYID <- sas_substr_trunc("ONCO-2026", 10)
sdtm_ae1$DOMAIN  <- sas_substr_trunc("AE", 2)
sdtm_ae1$USUBJID <- sas_substr_trunc(trimws(as.character(sdtm_ae1$USUBJID)), 25)
sdtm_ae1$AETERM  <- sas_substr_trunc(as.character(sdtm_ae1$TERM), 40)
sdtm_ae1$AEDECOD <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$DECOD)), 40)
sdtm_ae1$AEBODSYS <- sas_substr_trunc(sas_tranwrd(as.character(sdtm_ae1$BODSYS), "_", " "), 40)
sdtm_ae1$AESEV   <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$SEV)), 10)
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(sdtm_ae1$TXGR))
sdtm_ae1$AEREL   <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$REL)), 20)
sdtm_ae1$AEOUT   <- sas_substr_trunc(sas_upcase(as.character(sdtm_ae1$OUT)), 20)
sdtm_ae1$AESER   <- sas_substr_trunc(as.character(sdtm_ae1$SER), 1)

# Derive AESTDY / AEENDY with SAS semantics using numeric day differences
stdt_d <- suppressWarnings(as.Date(sdtm_ae1$AESTDTC, format = "%Y-%m-%d"))
rfdt_d <- suppressWarnings(as.Date(sdtm_ae1$RFSTDTC, format = "%Y-%m-%d"))
endt_d <- suppressWarnings(as.Date(sdtm_ae1$AEENDTC, format = "%Y-%m-%d"))
diff_s <- as.numeric(stdt_d - rfdt_d)
diff_e <- as.numeric(endt_d - rfdt_d)
sdtm_ae1$AESTDY <- ifelse(!is.na(diff_s), ifelse(diff_s >= 0, diff_s + 1, diff_s), NA_real_)
sdtm_ae1$AEENDY <- ifelse(!is.na(diff_e), ifelse(diff_e >= 0, diff_e + 1, diff_e), NA_real_)

sdtm_ae1$TERM_C <- as.character(sdtm_ae1$TERM)
ord2 <- sas_sort_order(sdtm_ae1, c("USUBJID","AESTDTC","TERM_C"))
sdtm_ae1 <- sdtm_ae1[ord2, , drop = FALSE]

sdtm_ae1$AESEQ <- NA_real_
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

for (cn in c("STUDYID","DOMAIN","USUBJID","AETERM","AEDECOD","AEBODSYS","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")) {
  v <- sdtm_ae[[cn]]
  v[v == ""] <- NA_character_
  sdtm_ae[[cn]] <- v
}

message(paste0("sdtm_ae rows: ", nrow(sdtm_ae)))

write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

print_df <- sdtm_ae
print_df$Obs <- seq_len(nrow(print_df))
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY"), drop = FALSE]

fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
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
invisible(file.copy(txt_path, lst_path, overwrite = TRUE))
write.csv(print_df, csv_path, row.names = FALSE, na = "")

message("Done")
