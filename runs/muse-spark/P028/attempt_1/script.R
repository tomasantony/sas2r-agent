out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
edc_dir <- file.path(rawdir, "faijal-oncology/raw_data")

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(stringr)
  library(lubridate)
})

cat("P028 TR mapping started\n")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_trim <- function(x) {
  ifelse(is.na(x), NA_character_, trimws(as.character(x), which = "both"))
}

sas_catx <- function(delim, ...) {
  args <- list(...)
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(as.character(a), n))
  sapply(seq_len(n), function(i) {
    parts <- sapply(args, function(a) {
      v <- a[i]
      if (is.na(v)) return(NA_character_)
      v <- trimws(v, which = "both")
      if (v == "") return(NA_character_)
      v
    })
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) return("")
    paste(parts, collapse = delim)
  }, USE.NAMES = FALSE)
}

sas_tranwrd <- function(source, from, to) {
  sapply(source, function(s) {
    if (is.na(s)) return(NA_character_)
    stringr::str_replace_all(s, stringr::fixed(from), to)
  }, USE.NAMES = FALSE)
}

sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}

sas_substr <- function(x, len) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    v <- as.character(v)
    substr(v, 1, len)
  }, USE.NAMES = FALSE)
}

parse_date11 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    vc <- trimws(as.character(v), which = "both")
    if (vc == "" || toupper(vc) == "NA") return(as.Date(NA))
    # remove dashes and slashes for date11 style
    clean <- gsub("[-/ ]", "", vc)
    # clean should be like 01JAN2024
    d <- suppressWarnings(as.Date(clean, format = "%d%b%Y"))
    if (!is.na(d)) return(d)
    d2 <- suppressWarnings(as.Date(toupper(clean), format = "%d%b%Y"))
    if (!is.na(d2)) return(d2)
    # try with dashes
    d3 <- suppressWarnings(as.Date(vc, format = "%d-%b-%Y"))
    if (!is.na(d3)) return(d3)
    d4 <- suppressWarnings(as.Date(toupper(vc), format = "%d-%b-%Y"))
    if (!is.na(d4)) return(d4)
    # try lubridate dmy
    d5 <- suppressWarnings(lubridate::dmy(vc, quiet = TRUE))
    if (!is.na(d5)) return(as.Date(d5))
    return(as.Date(NA))
  }, USE.NAMES = FALSE)
}

# need to preserve Date class after sapply: sapply strips class, so re-convert
parse_date11_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && length(x) > 0) {
    # numeric excel dates unlikely via readxl (already Date), return NA
    return(as.Date(rep(NA, length(x))))
  }
  res_num <- parse_date11(as.character(x))
  # parse_date11 returns numeric days via sapply; convert
  as.Date(res_num, origin = "1970-01-01")
}

format_yymmdd10 <- function(d) {
  sapply(d, function(v) {
    if (is.na(v)) return(NA_character_)
    if (inherits(v, "character")) {
      # already? try parse
      dd <- suppressWarnings(as.Date(substr(v, 1, 10), format = "%Y-%m-%d"))
      if (is.na(dd)) return(NA_character_)
      return(format(dd, "%Y-%m-%d"))
    }
    format(as.Date(v, origin = "1970-01-01"), "%Y-%m-%d")
  }, USE.NAMES = FALSE)
}

input_yymmdd10 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    vc <- trimws(as.character(v), which = "both")
    if (vc == "") return(as.Date(NA))
    dd <- suppressWarnings(as.Date(substr(vc, 1, 10), format = "%Y-%m-%d"))
    dd
  }, USE.NAMES = FALSE)
}
input_yymmdd10_vec <- function(x) {
  n <- suppressWarnings(as.Date(substr(trimws(as.character(x)), 1, 10), format = "%Y-%m-%d"))
  # as.Date vectorised already handles NA/"" -> NA
  n[trimws(as.character(x)) == "" | is.na(x)] <- as.Date(NA)
  n
}

read_edc <- function(name) {
  p <- file.path(edc_dir, paste0(name, ".xlsx"))
  if (!file.exists(p)) {
    cat(paste0("Missing file: ", p, "\n"))
    return(tibble::tibble())
  }
  df <- readxl::read_excel(p, .name_repair = "minimal")
  # normalise names to upper for SAS case-insensitivity, keep values
  names(df) <- toupper(trimws(names(df)))
  # convert haven labelled to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  tibble::as_tibble(df)
}

get_col <- function(df, col) {
  if (nrow(df) == 0) return(rep(NA_character_, 0))
  idx <- which(toupper(names(df)) == toupper(col))
  if (length(idx) == 0) return(rep(NA_character_, nrow(df)))
  v <- df[[idx[1]]]
  if (inherits(v, "Date")) return(as.character(format(v, "%d%b%Y")))
  if (inherits(v, "POSIXt")) return(as.character(format(as.Date(v), "%d%b%Y")))
  as.character(v)
}
get_col_raw <- function(df, col) {
  if (nrow(df) == 0) return(vector())
  idx <- which(toupper(names(df)) == toupper(col))
  if (length(idx) == 0) return(rep(NA, nrow(df)))
  df[[idx[1]]]
}

# ---- 2. Prepare Raw Data ----
edc_tr <- read_edc("edc_tr")
cat(paste0("edc_tr rows: ", nrow(edc_tr), " cols: ", paste(names(edc_tr), collapse=","), "\n"))

# Build tr_prep : USUBJID = catx("-", "ONCO-2026", SUBJID)
n_tr <- nrow(edc_tr)
subj_raw <- if (n_tr > 0) get_col(edc_tr, "SUBJID") else character(0)
# SAS CATX strips blanks
usubjid_prep <- if (n_tr > 0) sas_catx("-", rep("ONCO-2026", n_tr), ifelse(is.na(subj_raw), "", subj_raw)) else character(0)
usubjid_prep <- sas_substr(usubjid_prep, 25)
# keep original columns needed
lnk_raw   <- if (n_tr > 0) get_col(edc_tr, "LNKID") else character(0)
testcd_raw<- if (n_tr > 0) get_col(edc_tr, "TESTCD") else character(0)
orres_raw <- if (n_tr > 0) get_col(edc_tr, "ORRES") else character(0)
unit_raw  <- if (n_tr > 0) get_col(edc_tr, "UNIT") else character(0)
visit_raw <- if (n_tr > 0) get_col(edc_tr, "VISIT") else character(0)
trdt_raw_vec <- if (n_tr > 0) get_col_raw(edc_tr, "TRDT") else vector()

tr_prep <- tibble::tibble(
  USUBJID = usubjid_prep,
  LNKID = as.character(lnk_raw),
  TESTCD = as.character(testcd_raw),
  ORRES = as.character(orres_raw),
  UNIT = as.character(unit_raw),
  VISIT_RAW = as.character(visit_raw)
)
# preserve TRDT raw (could be Date)
tr_prep$TRDT_RAW <- trdt_raw_vec
# SAS sort by USUBJID (missing first, uppercase before lowercase)
# Convert NA to "" for ordering
if (nrow(tr_prep) > 0) {
  tr_prep$USUBJID[is.na(tr_prep$USUBJID)] <- ""
  ord <- order(tr_prep$USUBJID, na.last = FALSE, method = "radix")
  tr_prep <- tr_prep[ord, , drop = FALSE]
}

# ---- 1. Prepare DM Reference Data ----
# Try to locate sdtm_dm from prior SAS outputs
dm_for_tr <- NULL
# search in root output/sas/datasets for dm.csv / dm.xlsx / dm.xpt
search_dirs <- c(file.path(root, "output/sas/datasets"),
                 file.path(root, "output/sas"),
                 "D:/meta_hackathon/sas2r-agent/output/sas/datasets")
found_dm <- NULL
for (sd in search_dirs) {
  if (dir.exists(sd)) {
    cand <- list.files(sd, pattern = "(?i)(^|/|\\\\)dm\\.(csv|xlsx|xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE)
    if (length(cand) > 0) {
      # prefer shortest path / most recent? take first
      found_dm <- cand[1]
      break
    }
  }
}
# also check rawsdtm DM
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
if (is.null(found_dm) && dir.exists(rawsdtm_dir)) {
  cand2 <- list.files(rawsdtm_dir, pattern = "(?i)^dm\\.", full.names = TRUE)
  if (length(cand2) > 0) found_dm <- cand2[1]
}

if (!is.null(found_dm) && file.exists(found_dm)) {
  cat(paste0("Found DM file: ", found_dm, "\n"))
  ext <- tolower(tools::file_ext(found_dm))
  dm_raw <- NULL
  try({
    if (ext == "csv") dm_raw <- readr::read_csv(found_dm, show_col_types = FALSE, name_repair = "minimal")
    else if (ext == "xlsx") dm_raw <- readxl::read_excel(found_dm, .name_repair = "minimal")
    else if (ext == "xpt") dm_raw <- haven::read_xpt(found_dm)
    else if (ext == "sas7bdat") dm_raw <- haven::read_sas(found_dm)
  }, silent = TRUE)
  if (!is.null(dm_raw)) {
    names(dm_raw) <- toupper(trimws(names(dm_raw)))
    df2 <- as.data.frame(dm_raw, stringsAsFactors = FALSE)
    # extract USUBJID, RFSTDTC
    ucol <- which(names(df2) == "USUBJID")
    rcol <- which(names(df2) == "RFSTDTC")
    if (length(ucol) > 0 && length(rcol) > 0) {
      uu <- as.character(df2[[ucol[1]]])
      rr <- as.character(df2[[rcol[1]]])
      # RFSTDTC may be datetime; keep as character, substr later
      # If Date object, format
      if (inherits(df2[[rcol[1]]], "Date")) rr <- format(df2[[rcol[1]]], "%Y-%m-%d")
      if (inherits(df2[[rcol[1]]], "POSIXt")) rr <- format(as.Date(df2[[rcol[1]]]), "%Y-%m-%d")
      dm_for_tr <- tibble::tibble(USUBJID = sas_substr(trimws(uu), 25),
                                  RFSTDTC = trimws(rr))
      dm_for_tr$RFSTDTC[is.na(dm_for_tr$RFSTDTC)] <- ""
      dm_for_tr$USUBJID[is.na(dm_for_tr$USUBJID)] <- ""
    }
  }
}

if (is.null(dm_for_tr)) {
  cat("DM file not found or unusable; deriving dm_for_tr from edc_dm\n")
  edc_dm <- read_edc("edc_dm")
  cat(paste0("edc_dm rows: ", nrow(edc_dm), " cols: ", paste(names(edc_dm), collapse=","), "\n"))
  if (nrow(edc_dm) > 0) {
    dm_subj <- get_col(edc_dm, "SUBJID")
    dm_usub <- sas_catx("-", rep("ONCO-2026", length(dm_subj)), ifelse(is.na(dm_subj), "", dm_subj))
    dm_usub <- sas_substr(dm_usub, 25)
    # find RFSTDTC column case-insensitive
    rfcol_idx <- which(toupper(names(edc_dm)) == "RFSTDTC")
    rf_vals <- NULL
    if (length(rfcol_idx) > 0) {
      rv <- edc_dm[[rfcol_idx[1]]]
      if (inherits(rv, "Date")) rf_vals <- format(rv, "%Y-%m-%d")
      else if (inherits(rv, "POSIXt")) rf_vals <- format(as.Date(rv), "%Y-%m-%d")
      else {
        rf_vals <- trimws(as.character(rv))
        # if values look like date11, convert to yymmdd10
        # detect if contains month letters
        need_conv <- grepl("[A-Za-z]", rf_vals)
        if (any(need_conv, na.rm = TRUE)) {
          conv <- format_yymmdd10(parse_date11_vec(rf_vals))
          rf_vals[need_conv] <- ifelse(is.na(conv[need_conv]), rf_vals[need_conv], conv[need_conv])
        }
        # if Date objects stored as numeric? leave
      }
    } else {
      # try alternative names: TRTSDT, STDTC, EXSTDTC, RFSTDT
      alt_names <- c("TRTSDT", "TRTSTDTC", "RFSTDT", "STDTC", "EXSTDTC", "STARTDT", "STDT")
      found <- FALSE
      for (an in alt_names) {
        ai <- which(toupper(names(edc_dm)) == an)
        if (length(ai) > 0) {
          rv <- edc_dm[[ai[1]]]
          if (inherits(rv, "Date")) rf_vals <- format(rv, "%Y-%m-%d")
          else if (inherits(rv, "POSIXt")) rf_vals <- format(as.Date(rv), "%Y-%m-%d")
          else {
            rf_vals <- trimws(as.character(rv))
            need_conv <- grepl("[A-Za-z]", rf_vals)
            if (any(need_conv, na.rm = TRUE)) {
              conv <- format_yymmdd10(parse_date11_vec(rf_vals))
              rf_vals[need_conv] <- ifelse(is.na(conv[need_conv]), rf_vals[need_conv], conv[need_conv])
            }
          }
          found <- TRUE
          break
        }
      }
      if (!found) rf_vals <- rep("", length(dm_usub))
    }
    rf_vals[is.na(rf_vals)] <- ""
    dm_for_tr <- tibble::tibble(USUBJID = dm_usub, RFSTDTC = rf_vals)
  } else {
    dm_for_tr <- tibble::tibble(USUBJID = character(0), RFSTDTC = character(0))
  }
}

# sort dm_for_tr by USUBJID
if (nrow(dm_for_tr) > 0) {
  dm_for_tr$USUBJID[is.na(dm_for_tr$USUBJID)] <- ""
  dm_for_tr$RFSTDTC[is.na(dm_for_tr$RFSTDTC)] <- ""
  # keep only USUBJID, RFSTDTC; if duplicates, keep first per USUBJID sorted (SAS merge would need handling)
  # For faithful SAS merge with duplicates, keep all sorted; left join will duplicate.
  # Deduplicate to first to avoid cartesian explosion if dm has dupes, but preserve SAS-like: take first RFSTDTC per USUBJID
  dm_for_tr <- dm_for_tr[order(dm_for_tr$USUBJID, na.last = FALSE, method = "radix"), , drop = FALSE]
  # deduplicate: keep first occurrence per USUBJID (common DM uniqueness)
  dm_for_tr <- dm_for_tr[!duplicated(dm_for_tr$USUBJID), , drop = FALSE]
}

cat(paste0("dm_for_tr rows: ", nrow(dm_for_tr), "\n"))

# ---- 3. Map TR Domain (merge + derivations) ----
# merge tr_prep(in=t) dm_for_tr by USUBJID; if t;
if (nrow(tr_prep) == 0) {
  sdtm_tr1 <- tibble::tibble()
} else {
  # left join
  sdtm_tr1_base <- dplyr::left_join(tr_prep, dm_for_tr, by = "USUBJID")
  # RFSTDTC missing if no match -> ""
  sdtm_tr1_base$RFSTDTC[is.na(sdtm_tr1_base$RFSTDTC)] <- ""
  n1 <- nrow(sdtm_tr1_base)
  STUDYID <- rep("ONCO-2026", n1)
  DOMAIN <- rep("TR", n1)
  TRLNKID <- sas_substr(sas_trim(sdtm_tr1_base$LNKID), 5)
  TRTESTCD <- sas_substr(sas_trim(sdtm_tr1_base$TESTCD), 8)
  TRTEST <- ifelse(!is.na(TRTESTCD) & TRTESTCD == "LDIAM", "Longest Diameter", NA_character_)
  TRTEST <- sas_substr(TRTEST, 40)
  TRORRES <- sas_substr(sas_trim(sdtm_tr1_base$ORRES), 10)
  TRORRESU <- sas_substr(sas_trim(sdtm_tr1_base$UNIT), 10)
  TRSTRESC <- sas_substr(sas_trim(sdtm_tr1_base$ORRES), 10)
  # TRSTRESN = input(ORRES, best.)
  TRSTRESN <- suppressWarnings(as.numeric(trimws(as.character(sdtm_tr1_base$ORRES))))
  TRSTRESU <- sas_substr(sas_trim(sdtm_tr1_base$UNIT), 10)
  TREVAL <- rep("INVESTIGATOR", n1)
  TREVAL <- sas_substr(TREVAL, 15)
  # VISIT = upcase(tranwrd(VISIT,"_"," "))
  vis_tmp <- sas_tranwrd(sdtm_tr1_base$VISIT_RAW, "_", " ")
  vis_tmp <- sas_upcase(vis_tmp)
  VISIT <- sas_substr(vis_tmp, 20)
  VISIT[VISIT == ""] <- NA_character_
  # TRDTC conversion
  TRDTC <- rep(NA_character_, n1)
  trdt_raw <- sdtm_tr1_base$TRDT_RAW
  for (i in seq_len(n1)) {
    v <- trdt_raw[[i]]
    if (is.null(v) || length(v) == 0) { TRDTC[i] <- NA_character_; next }
    if (is.na(v)) { TRDTC[i] <- NA_character_; next }
    if (inherits(v, "Date")) {
      TRDTC[i] <- format(v, "%Y-%m-%d")
    } else if (inherits(v, "POSIXt")) {
      TRDTC[i] <- format(as.Date(v), "%Y-%m-%d")
    } else {
      vc <- trimws(as.character(v))
      if (vc == "" || toupper(vc) == "NA") {
        TRDTC[i] <- NA_character_
      } else {
        # if already YYYY-MM-DD, keep if valid
        if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", vc)) {
          dd <- suppressWarnings(as.Date(substr(vc, 1, 10), format = "%Y-%m-%d"))
          if (!is.na(dd)) TRDTC[i] <- format(dd, "%Y-%m-%d")
          else TRDTC[i] <- NA_character_
        } else {
          dd <- parse_date11_vec(vc)
          if (!is.na(dd)) TRDTC[i] <- format(dd, "%Y-%m-%d")
          else TRDTC[i] <- NA_character_
        }
      }
    }
  }
  TRDTC <- sas_substr(TRDTC, 19)
  # Study Day
  TRSTDY <- rep(NA_real_, n1)
  rf_vec <- sdtm_tr1_base$RFSTDTC
  for (i in seq_len(n1)) {
    a <- TRDTC[i]; b <- rf_vec[i]
    if (is.na(a) || a == "") next
    if (is.na(b) || trimws(as.character(b)) == "") next
    stdt <- suppressWarnings(as.Date(substr(a, 1, 10), format = "%Y-%m-%d"))
    rfdt <- suppressWarnings(as.Date(substr(trimws(as.character(b)), 1, 10), format = "%Y-%m-%d"))
    if (is.na(stdt) || is.na(rfdt)) next
    diff <- as.integer(stdt - rfdt)
    if (stdt >= rfdt) TRSTDY[i] <- diff + 1
    else TRSTDY[i] <- diff
  }
  sdtm_tr1 <- tibble::tibble(
    STUDYID = sas_substr(STUDYID, 10),
    DOMAIN = sas_substr(DOMAIN, 2),
    USUBJID = sas_substr(sdtm_tr1_base$USUBJID, 25),
    TRLNKID = TRLNKID,
    TRTESTCD = TRTESTCD,
    TRTEST = TRTEST,
    TRORRES = TRORRES,
    TRORRESU = TRORRESU,
    TRSTRESC = TRSTRESC,
    TRSTRESN = TRSTRESN,
    TRSTRESU = TRSTRESU,
    TREVAL = TREVAL,
    TRDTC = TRDTC,
    TRSTDY = TRSTDY,
    VISIT = VISIT
  )
  # SAS missing character is "" ; convert "" to NA for R, will write as "" in csv
  # keep NA as NA
  sdtm_tr1$TRTEST[sdtm_tr1$TRTEST == ""] <- NA_character_
}

# ---- 4. Derive TRSEQ : sort by USUBJID TRDTC, retain ----
if (nrow(sdtm_tr1) > 0) {
  # SAS sort by USUBJID TRDTC (missing first)
  sdtm_tr1$._u <- ifelse(is.na(sdtm_tr1$USUBJID), "", sdtm_tr1$USUBJID)
  sdtm_tr1$._t <- ifelse(is.na(sdtm_tr1$TRDTC), "", sdtm_tr1$TRDTC)
  o <- order(sdtm_tr1$._u, sdtm_tr1$._t, na.last = FALSE, method = "radix")
  sdtm_tr1 <- sdtm_tr1[o, , drop = FALSE]
  sdtm_tr1$._u <- NULL
  sdtm_tr1$._t <- NULL
  # retain TRSEQ by USUBJID
  # need FIRST.USUBJID with missing handling: treat NA as group
  grp <- ifelse(is.na(sdtm_tr1$USUBJID), "<NA>", sdtm_tr1$USUBJID)
  sdtm_tr1$TRSEQ <- ave(seq_len(nrow(sdtm_tr1)), grp, FUN = seq_along)
  sdtm_tr1$TRSEQ <- as.numeric(sdtm_tr1$TRSEQ)
  # keep order and columns
  sdtm_tr <- sdtm_tr1[, c("STUDYID","DOMAIN","USUBJID","TRSEQ","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT")]
} else {
  sdtm_tr <- tibble::tibble(STUDYID=character(0),DOMAIN=character(0),USUBJID=character(0),TRSEQ=numeric(0),TRLNKID=character(0),TRTESTCD=character(0),TRTEST=character(0),TRORRES=character(0),TRORRESU=character(0),TRSTRESC=character(0),TRSTRESN=numeric(0),TRSTRESU=character(0),TREVAL=character(0),TRDTC=character(0),TRSTDY=numeric(0),VISIT=character(0))
}

cat(paste0("sdtm_tr rows: ", nrow(sdtm_tr), "\n"))

# ---- 5/6. Outputs ----
# dataset tr -> OUT_DIR/tr.csv
write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

# Listing rendering: PROC PRINT of sdtm_tr
# Build text similar to SAS PROC PRINT
fmt_num <- function(x) {
  ifelse(is.na(x), ".", as.character(x))
}
fmt_char <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

print_df <- sdtm_tr
# Create display data with Obs
obs <- seq_len(nrow(print_df))
disp <- data.frame(
  Obs = obs,
  STUDYID = fmt_char(print_df$STUDYID),
  DOMAIN = fmt_char(print_df$DOMAIN),
  USUBJID = fmt_char(print_df$USUBJID),
  TRSEQ = fmt_num(print_df$TRSEQ),
  TRLNKID = fmt_char(print_df$TRLNKID),
  TRTESTCD = fmt_char(print_df$TRTESTCD),
  TRTEST = fmt_char(print_df$TRTEST),
  TRORRES = fmt_char(print_df$TRORRES),
  TRORRESU = fmt_char(print_df$TRORRESU),
  TRSTRESC = fmt_char(print_df$TRSTRESC),
  TRSTRESN = fmt_num(print_df$TRSTRESN),
  TRSTRESU = fmt_char(print_df$TRSTRESU),
  TREVAL = fmt_char(print_df$TREVAL),
  TRDTC = fmt_char(print_df$TRDTC),
  TRSTDY = fmt_num(print_df$TRSTDY),
  VISIT = fmt_char(print_df$VISIT),
  stringsAsFactors = FALSE, check.names = FALSE
)

# Write CSV of table body (P028.csv) - same as listing body
write.csv(disp, file.path(out, "P028.csv"), row.names = FALSE, na = "")

# Build plain-text rendering P028.txt (and P028.lst copy)
txt_path <- file.path(out, "P028.txt")
lst_path <- file.path(out, "P028.lst")
con_lines <- c(
  "P028 TR Listing",
  "PROC PRINT of SDTM TR",
  "",
  paste0("Observations: ", nrow(disp))
)
# header line
hdr <- paste(names(disp), collapse = " | ")
con_lines <- c(con_lines, "", hdr, paste(rep("-", nchar(hdr)), collapse = ""))
if (nrow(disp) > 0) {
  for (i in seq_len(nrow(disp))) {
    rowvals <- sapply(disp[i, ], as.character, USE.NAMES = FALSE)
    # SAS missing numeric already "." ; missing char "" -> show blank
    con_lines <- c(con_lines, paste(rowvals, collapse = " | "))
  }
} else {
  con_lines <- c(con_lines, "(no observations)")
}
con_lines <- c(con_lines, "", paste0("End of listing. N=", nrow(disp)))
writeLines(con_lines, txt_path)
# copy to .lst for expected name
writeLines(con_lines, lst_path)

cat("Outputs written to OUT_DIR\n")
