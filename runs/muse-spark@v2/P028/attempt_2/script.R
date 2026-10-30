out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)
suppressMessages(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
})

raw_tr_path   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tr.xlsx"
raw_dm_path   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
raw_ex_path   <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
raw_rand_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"
rawsdtm_dir   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_dir   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

sas_trunc <- function(x, n) {
  x <- as.character(x)
  isna <- is.na(x)
  x[!isna] <- substr(x[!isna], 1, n)
  x
}

find_col <- function(df, candidates) {
  un <- toupper(names(df))
  for (cand in candidates) {
    idx <- which(un == toupper(cand))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  NULL
}
find_col_contains <- function(df, patterns) {
  un <- toupper(names(df))
  for (p in patterns) {
    idx <- grep(p, un, fixed = FALSE)
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  NULL
}

parse_date11_to_iso <- function(x) {
  x <- as.character(x)
  res <- rep(NA_character_, length(x))
  for (i in seq_along(x)) {
    v0 <- x[i]
    if (is.na(v0)) next
    v <- trimws(v0)
    if (v == "") next
    m <- regmatches(v, regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{4})", v))[[1]]
    if (length(m) == 4) {
      dd <- sprintf("%02d", suppressWarnings(as.integer(m[2])))
      monraw <- m[3]
      mon <- paste0(toupper(substr(monraw, 1, 1)), tolower(substr(monraw, 2, 3)))
      yy <- m[4]
      v2 <- paste0(dd, "-", mon, "-", yy)
      d <- as.Date(v2, format = "%d-%b-%Y")
      if (!is.na(d)) res[i] <- format(d, "%Y-%m-%d")
      next
    }
    if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", v)) {
      d2 <- as.Date(substr(v, 1, 10), format = "%Y-%m-%d")
      if (!is.na(d2)) res[i] <- format(d2, "%Y-%m-%d")
      next
    }
  }
  res
}

normalize_to_iso <- function(x) {
  x <- as.character(x)
  res <- rep(NA_character_, length(x))
  for (i in seq_along(x)) {
    v0 <- x[i]
    if (is.na(v0)) next
    v <- trimws(v0)
    if (v == "") next
    if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}[T ]", v)) {
      d <- as.Date(substr(v, 1, 10), format = "%Y-%m-%d")
      if (!is.na(d)) { res[i] <- format(d, "%Y-%m-%d"); next }
    }
    if (grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}", v)) {
      d <- as.Date(substr(v, 1, 10), format = "%Y-%m-%d")
      if (!is.na(d)) { res[i] <- format(d, "%Y-%m-%d"); next }
    }
    m2 <- regmatches(v, regexec("^([0-9]{1,2})-([A-Za-z]{3})-([0-9]{4})", v))[[1]]
    if (length(m2) == 4) {
      dd <- sprintf("%02d", suppressWarnings(as.integer(m2[2])))
      mon <- paste0(toupper(substr(m2[3], 1, 1)), tolower(substr(m2[3], 2, 3)))
      v2 <- paste0(dd, "-", mon, "-", m2[4])
      d <- as.Date(v2, format = "%d-%b-%Y")
      if (!is.na(d)) { res[i] <- format(d, "%Y-%m-%d"); next }
    }
    if (grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", v)) {
      d <- as.Date(v, format = "%d/%m/%Y")
      if (!is.na(d)) { res[i] <- format(d, "%Y-%m-%d"); next }
    }
    if (grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}", v)) {
      d <- as.Date(v, format = "%Y/%m/%d")
      if (!is.na(d)) { res[i] <- format(d, "%Y-%m-%d"); next }
    }
  }
  res
}

read_all_text <- function(path) {
  df <- as.data.frame(readxl::read_excel(path, col_types = "text", .name_repair = "minimal"), stringsAsFactors = FALSE, check.names = FALSE)
  for (j in seq_along(df)) {
    df[[j]] <- as.character(df[[j]])
  }
  df
}

edc_tr <- read_all_text(raw_tr_path)
cn_tr <- toupper(names(edc_tr))
get_tr_col <- function(want) {
  idx <- which(cn_tr == want)
  if (length(idx) == 0) return(NULL)
  names(edc_tr)[idx[1]]
}
c_SUBJID <- get_tr_col("SUBJID"); if (is.null(c_SUBJID)) c_SUBJID <- find_col_contains(edc_tr, "SUBJ")
c_LNKID  <- get_tr_col("LNKID");  if (is.null(c_LNKID))  c_LNKID  <- find_col_contains(edc_tr, "LNK")
c_TESTCD <- get_tr_col("TESTCD"); if (is.null(c_TESTCD)) c_TESTCD <- find_col_contains(edc_tr, "TESTCD")
c_ORRES  <- get_tr_col("ORRES");  if (is.null(c_ORRES))  c_ORRES  <- find_col_contains(edc_tr, "ORRES")
c_UNIT   <- get_tr_col("UNIT");   if (is.null(c_UNIT))   c_UNIT   <- find_col_contains(edc_tr, "UNIT")
c_VISIT  <- get_tr_col("VISIT");  if (is.null(c_VISIT))  c_VISIT  <- find_col_contains(edc_tr, "VISIT")
c_TRDT   <- get_tr_col("TRDT");   if (is.null(c_TRDT))   c_TRDT   <- find_col_contains(edc_tr, "TRDT")
stopifnot(!is.null(c_SUBJID), !is.null(c_LNKID), !is.null(c_TESTCD), !is.null(c_ORRES), !is.null(c_UNIT), !is.null(c_VISIT), !is.null(c_TRDT))

dm_for_tr <- NULL
try({
  cand_files <- c()
  if (dir.exists(rawsdtm_dir)) {
    f1 <- list.files(rawsdtm_dir, full.names = TRUE, recursive = TRUE)
    cand_files <- c(cand_files, f1)
  }
  if (dir.exists(rawadam_dir)) {
    f2 <- list.files(rawadam_dir, full.names = TRUE, recursive = TRUE)
    cand_files <- c(cand_files, f2)
  }
  dm_cands <- cand_files[grepl("(?i)(^|/|\\\\)dm\\.(xpt|sas7bdat)$", cand_files)]
  if (length(dm_cands) > 0) {
    for (cf in dm_cands) {
      dmtmp <- NULL
      if (grepl("\\.xpt$", cf, ignore.case = TRUE)) dmtmp <- try(haven::read_xpt(cf), silent = TRUE)
      if (grepl("\\.sas7bdat$", cf, ignore.case = TRUE)) dmtmp <- try(haven::read_sas(cf), silent = TRUE)
      if (inherits(dmtmp, "try-error") || is.null(dmtmp)) next
      nn <- toupper(names(dmtmp))
      if (all(c("USUBJID", "RFSTDTC") %in% nn)) {
        usub_c <- names(dmtmp)[which(nn == "USUBJID")[1]]
        rf_c <- names(dmtmp)[which(nn == "RFSTDTC")[1]]
        dm_for_tr <- data.frame(USUBJID = as.character(dmtmp[[usub_c]]), RFSTDTC = as.character(dmtmp[[rf_c]]), stringsAsFactors = FALSE)
        if (sum(grepl("^ONCO-2026-", dm_for_tr$USUBJID), na.rm = TRUE) == 0) {
          dm_for_tr <- NULL
          next
        } else {
          break
        }
      }
    }
  }
}, silent = TRUE)

if (is.null(dm_for_tr)) {
  edc_dm <- try(read_all_text(raw_dm_path), silent = TRUE)
  edc_ex <- try(read_all_text(raw_ex_path), silent = TRUE)
  ivrs_rand <- try(read_all_text(raw_rand_path), silent = TRUE)
  if (inherits(edc_dm, "try-error")) edc_dm <- NULL
  if (inherits(edc_ex, "try-error")) edc_ex <- NULL
  if (inherits(ivrs_rand, "try-error")) ivrs_rand <- NULL

  subj_to_usub <- function(subj) {
    subj <- as.character(subj)
    s <- trimws(subj)
    s[is.na(subj)] <- NA
    us <- ifelse(is.na(s) | s == "", "ONCO-2026", paste0("ONCO-2026-", s))
    us
  }

  rf_parts <- list()

  if (!is.null(edc_dm)) {
    c_dm_subj <- find_col(edc_dm, c("SUBJID", "SUBJ", "USUBJID"))
    if (is.null(c_dm_subj)) c_dm_subj <- find_col_contains(edc_dm, "SUBJ")
    # Only exact RFSTDTC / RFSTDT / RFSTDAT counts as DM reference start (CDISC DM.RFSTDTC)
    c_dm_rf <- find_col(edc_dm, c("RFSTDTC", "RFSTDT", "RFSTDAT"))
    if (!is.null(c_dm_subj) && !is.null(c_dm_rf)) {
      dm_subj <- trimws(as.character(edc_dm[[c_dm_subj]]))
      if (toupper(c_dm_subj) == "USUBJID") {
        dm_usub <- trimws(as.character(edc_dm[[c_dm_subj]]))
      } else {
        dm_usub <- subj_to_usub(dm_subj)
      }
      dm_rf_iso <- normalize_to_iso(edc_dm[[c_dm_rf]])
      rf_parts$dm <- data.frame(USUBJID = dm_usub, RF_DM = dm_rf_iso, stringsAsFactors = FALSE)
    } else if (!is.null(c_dm_subj)) {
      dm_subj <- trimws(as.character(edc_dm[[c_dm_subj]]))
      dm_usub <- subj_to_usub(dm_subj)
      rf_parts$dm_subj <- data.frame(USUBJID = dm_usub, stringsAsFactors = FALSE)
    }
  }

  if (!is.null(edc_ex)) {
    c_ex_subj <- find_col(edc_ex, c("SUBJID", "SUBJ"))
    if (is.null(c_ex_subj)) c_ex_subj <- find_col_contains(edc_ex, "SUBJ")
    c_ex_dt <- find_col(edc_ex, c("EXSTDTC", "EXSTDT", "EXSTDAT", "EXDTC", "EXDT"))
    if (is.null(c_ex_dt)) c_ex_dt <- find_col_contains(edc_ex, "EXSTD|EXSTDT")
    if (!is.null(c_ex_subj) && !is.null(c_ex_dt)) {
      ex_subj <- trimws(as.character(edc_ex[[c_ex_subj]]))
      ex_usub <- subj_to_usub(ex_subj)
      ex_iso <- normalize_to_iso(edc_ex[[c_ex_dt]])
      ex_df <- data.frame(USUBJID = ex_usub, EX_ISO = ex_iso, stringsAsFactors = FALSE)
      ex_df <- ex_df[!is.na(ex_df$EX_ISO) & ex_df$EX_ISO != "", , drop = FALSE]
      if (nrow(ex_df) > 0) {
        ex_min <- aggregate(EX_ISO ~ USUBJID, data = ex_df, FUN = function(z) min(z, na.rm = TRUE))
        names(ex_min)[2] <- "RF_EX"
        rf_parts$ex <- ex_min
      }
    }
  }

  if (!is.null(ivrs_rand)) {
    c_r_subj <- find_col(ivrs_rand, c("SUBJID", "SUBJ"))
    if (is.null(c_r_subj)) c_r_subj <- find_col_contains(ivrs_rand, "SUBJ")
    c_r_dt <- find_col(ivrs_rand, c("RANDDTC", "RANDDT", "RANDAT", "RANDATE", "RANDDTC", "RANDSTDTC"))
    if (is.null(c_r_dt)) {
      # any RAND-named column, else generic date column
      c_r_dt <- find_col_contains(ivrs_rand, "RAND")
      if (is.null(c_r_dt)) c_r_dt <- find_col(ivrs_rand, c("DTC", "DT", "DATE", "DAT"))
    }
    if (!is.null(c_r_subj) && !is.null(c_r_dt)) {
      r_subj <- trimws(as.character(ivrs_rand[[c_r_subj]]))
      r_usub <- subj_to_usub(r_subj)
      r_iso <- normalize_to_iso(ivrs_rand[[c_r_dt]])
      r_df <- data.frame(USUBJID = r_usub, R_ISO = r_iso, stringsAsFactors = FALSE)
      r_df <- r_df[!is.na(r_df$R_ISO) & r_df$R_ISO != "", , drop = FALSE]
      if (nrow(r_df) > 0) {
        r_min <- aggregate(R_ISO ~ USUBJID, data = r_df, FUN = function(z) min(z, na.rm = TRUE))
        names(r_min)[2] <- "RF_RAND"
        rf_parts$rand <- r_min
      }
    }
  }

  all_usub <- unique(c(
    if (!is.null(rf_parts$dm)) rf_parts$dm$USUBJID else NULL,
    if (!is.null(rf_parts$dm_subj)) rf_parts$dm_subj$USUBJID else NULL,
    if (!is.null(rf_parts$ex)) rf_parts$ex$USUBJID else NULL,
    if (!is.null(rf_parts$rand)) rf_parts$rand$USUBJID else NULL
  ))
  tr_subj_raw <- trimws(as.character(edc_tr[[c_SUBJID]]))
  tr_usub_all <- subj_to_usub(tr_subj_raw)
  all_usub <- unique(c(all_usub, tr_usub_all))
  dm_for_tr <- data.frame(USUBJID = all_usub, stringsAsFactors = FALSE)
  if (!is.null(rf_parts$ex)) dm_for_tr <- merge(dm_for_tr, rf_parts$ex, by = "USUBJID", all.x = TRUE, sort = FALSE)
  if (!is.null(rf_parts$rand)) dm_for_tr <- merge(dm_for_tr, rf_parts$rand, by = "USUBJID", all.x = TRUE, sort = FALSE)
  if (!is.null(rf_parts$dm)) dm_for_tr <- merge(dm_for_tr, rf_parts$dm, by = "USUBJID", all.x = TRUE, sort = FALSE)
  coalesce2 <- function(...) {
    args <- list(...)
    res <- args[[1]]
    for (k in 2:length(args)) {
      v <- args[[k]]
      if (is.null(v)) next
      idx <- is.na(res) | res == ""
      res[idx] <- v[idx]
    }
    res
  }
  # SAS DM.RFSTDTC is the reference start (randomisation). Prioritise DM RFSTDTC, then RAND, then EX.
  e_dm   <- if ("RF_DM" %in% names(dm_for_tr)) dm_for_tr$RF_DM else rep(NA_character_, nrow(dm_for_tr))
  e_rand <- if ("RF_RAND" %in% names(dm_for_tr)) dm_for_tr$RF_RAND else rep(NA_character_, nrow(dm_for_tr))
  e_ex   <- if ("RF_EX" %in% names(dm_for_tr)) dm_for_tr$RF_EX else rep(NA_character_, nrow(dm_for_tr))
  dm_for_tr$RFSTDTC <- coalesce2(e_dm, e_rand, e_ex)
  dm_for_tr <- dm_for_tr[, c("USUBJID", "RFSTDTC"), drop = FALSE]
  dm_for_tr$USUBJID <- as.character(dm_for_tr$USUBJID)
  dm_for_tr$RFSTDTC <- as.character(dm_for_tr$RFSTDTC)
  dm_for_tr$RFSTDTC[is.na(dm_for_tr$RFSTDTC)] <- NA
  o <- order(dm_for_tr$USUBJID, na.last = FALSE, method = "radix")
  dm_for_tr <- dm_for_tr[o, , drop = FALSE]
}

stopifnot("USUBJID" %in% names(dm_for_tr))
stopifnot("RFSTDTC" %in% names(dm_for_tr))

tr_prep <- data.frame(
  SUBJID_raw = as.character(edc_tr[[c_SUBJID]]),
  LNKID_raw  = as.character(edc_tr[[c_LNKID]]),
  TESTCD_raw = as.character(edc_tr[[c_TESTCD]]),
  ORRES_raw  = as.character(edc_tr[[c_ORRES]]),
  UNIT_raw   = as.character(edc_tr[[c_UNIT]]),
  VISIT_raw  = as.character(edc_tr[[c_VISIT]]),
  TRDT_raw   = as.character(edc_tr[[c_TRDT]]),
  stringsAsFactors = FALSE
)
tr_prep$USUBJID <- sapply(tr_prep$SUBJID_raw, function(s) {
  if (is.na(s)) return("ONCO-2026")
  s2 <- trimws(s)
  if (s2 == "" || toupper(s2) == "NA") return("ONCO-2026")
  paste0("ONCO-2026-", s2)
}, USE.NAMES = FALSE)
tr_prep$USUBJID <- sas_trunc(tr_prep$USUBJID, 25)

tr_prep <- tr_prep[order(tr_prep$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]
dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]

mrg <- merge(tr_prep, dm_for_tr, by = "USUBJID", all.x = TRUE, sort = FALSE)
stopifnot(nrow(mrg) == nrow(tr_prep))
stopifnot("RFSTDTC" %in% names(mrg))

n <- nrow(mrg)
STUDYID <- sas_trunc(rep("ONCO-2026", n), 10)
DOMAIN <- sas_trunc(rep("TR", n), 2)
TRLNKID <- sas_trunc(trimws(ifelse(is.na(mrg$LNKID_raw), "", mrg$LNKID_raw)), 5)
TRLNKID[TRLNKID == ""] <- NA_character_
TRTESTCD <- sas_trunc(trimws(ifelse(is.na(mrg$TESTCD_raw), "", mrg$TESTCD_raw)), 8)
TRTESTCD[TRTESTCD == ""] <- NA_character_
TRTEST <- rep(NA_character_, n)
idx_ld <- !is.na(TRTESTCD) & trimws(TRTESTCD) == "LDIAM"
TRTEST[idx_ld] <- "Longest Diameter"
TRTEST <- sas_trunc(TRTEST, 40)
TRORRES <- sas_trunc(ifelse(is.na(mrg$ORRES_raw), NA, trimws(mrg$ORRES_raw)), 10)
TRORRES[TRORRES == ""] <- NA_character_
TRORRESU <- sas_trunc(ifelse(is.na(mrg$UNIT_raw), NA, trimws(mrg$UNIT_raw)), 10)
TRORRESU[TRORRESU == ""] <- NA_character_
TRSTRESC <- sas_trunc(ifelse(is.na(mrg$ORRES_raw), NA, trimws(mrg$ORRES_raw)), 10)
TRSTRESC[TRSTRESC == ""] <- NA_character_
TRSTRESN <- suppressWarnings(as.numeric(trimws(mrg$ORRES_raw)))
TRSTRESU <- sas_trunc(ifelse(is.na(mrg$UNIT_raw), NA, trimws(mrg$UNIT_raw)), 10)
TRSTRESU[TRSTRESU == ""] <- NA_character_
TREVAL <- sas_trunc(rep("INVESTIGATOR", n), 15)
VISIT <- toupper(gsub("_", " ", ifelse(is.na(mrg$VISIT_raw), "", mrg$VISIT_raw), fixed = TRUE))
VISIT <- sas_trunc(VISIT, 20)
VISIT[VISIT == ""] <- NA_character_
TRDTC <- parse_date11_to_iso(mrg$TRDT_raw)
TRDTC <- sas_trunc(TRDTC, 19)
TRSTDY <- rep(NA_real_, n)
for (i in seq_len(n)) {
  tc <- TRDTC[i]
  rf <- mrg$RFSTDTC[i]
  if (is.na(tc) || tc == "" || is.na(rf) || trimws(as.character(rf)) == "") next
  stdt <- as.Date(substr(tc, 1, 10), format = "%Y-%m-%d")
  rfdt <- as.Date(substr(trimws(as.character(rf)), 1, 10), format = "%Y-%m-%d")
  if (is.na(stdt) || is.na(rfdt)) next
  diff <- as.integer(stdt - rfdt)
  if (stdt >= rfdt) TRSTDY[i] <- diff + 1 else TRSTDY[i] <- diff
}

sdtm_tr1 <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = mrg$USUBJID,
  TRLNKID = TRLNKID, TRTESTCD = TRTESTCD, TRTEST = TRTEST,
  TRORRES = TRORRES, TRORRESU = TRORRESU,
  TRSTRESC = TRSTRESC, TRSTRESN = TRSTRESN, TRSTRESU = TRSTRESU,
  TREVAL = TREVAL, VISIT = VISIT, TRDTC = TRDTC, TRSTDY = TRSTDY,
  RFSTDTC_tmp = mrg$RFSTDTC,
  stringsAsFactors = FALSE
)

sdtm_tr1 <- sdtm_tr1[order(sdtm_tr1$USUBJID, sdtm_tr1$TRDTC, na.last = FALSE, method = "radix"), , drop = FALSE]

sdtm_tr1$TRSEQ <- NA_real_
seqv <- 0L
for (i in seq_len(nrow(sdtm_tr1))) {
  if (i == 1) first <- TRUE
  else {
    u <- sdtm_tr1$USUBJID[i]
    prev <- sdtm_tr1$USUBJID[i - 1]
    if (is.na(u) && is.na(prev)) first <- FALSE
    else if (is.na(u) || is.na(prev)) first <- TRUE
    else first <- (u != prev)
  }
  if (first) seqv <- 1L else seqv <- seqv + 1L
  sdtm_tr1$TRSEQ[i] <- seqv
}

sdtm_tr <- sdtm_tr1[, c("STUDYID", "DOMAIN", "USUBJID", "TRSEQ", "TRLNKID", "TRTESTCD", "TRTEST", "TRORRES", "TRORRESU", "TRSTRESC", "TRSTRESN", "TRSTRESU", "TREVAL", "TRDTC", "TRSTDY", "VISIT"), drop = FALSE]

write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

fmt_cell <- function(val, is_num) {
  if (is.na(val)) {
    if (is_num) return(".")
    else return("")
  } else {
    if (is_num) {
      return(as.character(val))
    } else return(as.character(val))
  }
}
num_cols <- c("TRSEQ", "TRSTRESN", "TRSTDY")
disp <- sdtm_tr
header <- c("Obs", names(disp))
mat <- matrix("", nrow = nrow(disp), ncol = length(header))
colnames(mat) <- header
for (j in seq_along(header)) {
  cn <- header[j]
  if (cn == "Obs") {
    mat[, j] <- as.character(seq_len(nrow(disp)))
  } else {
    is_num <- cn %in% num_cols
    mat[, j] <- sapply(disp[[cn]], function(v) fmt_cell(v, is_num), USE.NAMES = FALSE)
  }
}
widths <- sapply(seq_along(header), function(j) max(nchar(header[j], type = "chars"), if (nrow(mat) > 0) max(nchar(mat[, j], type = "chars")) else 0))
pad_r <- function(s, w) sprintf(paste0("%-", w, "s"), s)
line_header <- paste(mapply(pad_r, header, widths), collapse = "  ")
line_sep <- paste(mapply(function(w) paste(rep("-", w), collapse = ""), widths), collapse = "  ")
lines <- c("PROC PRINT data=sdtm_tr;", "", line_header, line_sep)
if (nrow(mat) > 0) {
  for (i in seq_len(nrow(mat))) {
    lines <- c(lines, paste(mapply(pad_r, mat[i, ], widths), collapse = "  "))
  }
}
lines <- c(lines, "", paste0("N = ", nrow(disp)))
writeLines(lines, file.path(out, "P028.txt"))
writeLines(lines, file.path(out, "P028.lst"))
csv_body <- data.frame(Obs = seq_len(nrow(disp)), disp, stringsAsFactors = FALSE, check.names = FALSE)
write.csv(csv_body, file.path(out, "P028.csv"), row.names = FALSE, na = "")
