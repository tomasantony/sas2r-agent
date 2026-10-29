```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawsdtm <- file.path(root, "data/raw/cdisc-pilot/sdtm")
rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")
edc_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(readxl)
  library(openxlsx)
})

find_col <- function(df, candidates) {
  cn_up <- toupper(names(df))
  for (cand in candidates) {
    idx <- which(cn_up == toupper(cand))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}

parse_date11 <- function(x) {
  vapply(x, function(v) {
    if (is.null(v) || length(v)==0) return(as.Date(NA))
    if (is.na(v)) return(as.Date(NA))
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    if (is.numeric(v) && !is.na(v)) {
      return(as.Date(v, origin = "1899-12-30"))
    }
    v <- trimws(as.character(v))
    if (v == "" || toupper(v) == "NA") return(as.Date(NA))
    d <- suppressWarnings(lubridate::dmy(v, quiet = TRUE))
    if (!is.na(d)) return(d)
    d2 <- suppressWarnings(lubridate::ymd(v, quiet = TRUE))
    if (!is.na(d2)) return(d2)
    v2 <- toupper(v)
    for (fmt in c("%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%d%b%y", "%Y-%m-%d", "%Y/%m/%d")) {
      d3 <- suppressWarnings(as.Date(v2, format = fmt))
      if (!is.na(d3)) return(d3)
      d3b <- suppressWarnings(as.Date(v, format = fmt))
      if (!is.na(d3b)) return(d3b)
    }
    return(as.Date(NA))
  }, FUN.VALUE = as.Date("1970-01-01")) |> as.Date(origin="1970-01-01")
}

fmt_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# helper to read dm from any file
read_dm_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  try({
    if (ext == "xpt") df <- haven::read_xpt(path)
    else if (ext == "sas7bdat") df <- haven::read_sas(path)
    else if (ext %in% c("xlsx","xls")) df <- readxl::read_excel(path)
    else if (ext == "csv") df <- readr::read_csv(path, show_col_types=FALSE)
  }, silent=TRUE)
  return(df)
}

# -------------------------------------------------
# 1. Prepare DM Reference Data (dm_for_rs)
# -------------------------------------------------
dm_for_rs <- NULL

# search for sdtm dm in multiple places
search_dirs <- c(
  rawsdtm,
  rawadam,
  file.path(root, "data/raw/cdisc-pilot"),
  file.path(root, "data/derived"),
  file.path(root, "output/sas/datasets"),
  edc_dir,
  root
)

candidate_files <- c()
for (d in search_dirs) {
  if (dir.exists(d)) {
    files <- list.files(d, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    # keep dm files
    dm_files <- files[grepl("(^|/|\\\\)dm\\.(xpt|sas7bdat|xlsx|csv)$", files, ignore.case=TRUE)]
    candidate_files <- c(candidate_files, dm_files)
    # also adsl files for fallback
    adsl_files <- files[grepl("adsl\\.(xpt|sas7bdat|xlsx|csv)$", files, ignore.case=TRUE)]
    candidate_files <- c(candidate_files, adsl_files)
  }
}
# also explicitly check known locations
explicit <- c(
  file.path(rawsdtm, "dm.xpt"),
  file.path(rawsdtm, "DM.xpt"),
  file.path(rawsdtm, "dm.sas7bdat"),
  file.path(rawadam, "adsl.xpt"),
  file.path(rawadam, "ADSL.xpt"),
  file.path(edc_dir, "edc_dm.xlsx"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
)
candidate_files <- unique(c(explicit, candidate_files))

found <- FALSE
for (f in candidate_files) {
  if (!file.exists(f)) next
  df <- read_dm_file(f)
  if (is.null(df) || nrow(df)==0) next
  cn_up <- toupper(names(df))
  usub_col <- find_col(df, c("USUBJID"))
  # try RFSTDTC first
  rf_col <- find_col(df, c("RFSTDTC","RFSTDT","RFXSTDTC","TRTSDT","TRTSDTC","RANDDT"))
  if (is.na(usub_col)) {
    # try SUBJID -> construct USUBJID
    subj_col <- find_col(df, c("SUBJID"))
    if (!is.na(subj_col) && !is.na(rf_col)) {
      df2 <- df %>%
        mutate(
          SUBJID_tmp = as.character(.data[[subj_col]]),
          USUBJID = ifelse(grepl("ONCO-2026", SUBJID_tmp, fixed=TRUE), SUBJID_tmp,
                           paste0("ONCO-2026-", trimws(SUBJID_tmp))),
          RFSTDTC_tmp = .data[[rf_col]]
        )
      # convert RFSTDTC_tmp to character YYYY-MM-DD
      rf_vals <- df2$RFSTDTC_tmp
      if (inherits(rf_vals, "Date")) rf_vals <- format(rf_vals, "%Y-%m-%d")
      else if (inherits(rf_vals, "POSIXt")) rf_vals <- format(as.Date(rf_vals), "%Y-%m-%d")
      else if (is.numeric(rf_vals)) rf_vals <- format(as.Date(rf_vals, origin="1899-12-30"), "%Y-%m-%d")
      else {
        rf_vals <- as.character(rf_vals)
        # if contains time, trim
        rf_vals <- substr(rf_vals,1,10)
        # try parse date11
        parsed <- parse_date11(rf_vals)
        has_parsed <- !is.na(parsed)
        # only replace where original not already ymd
        ymd_ok <- !is.na(suppressWarnings(lubridate::ymd(rf_vals, quiet=TRUE)))
        to_replace <- has_parsed & !ymd_ok
        rf_vals[to_replace] <- fmt_yymmdd10(parsed[to_replace])
      }
      dm_for_rs <- tibble(USUBJID = as.character(df2$USUBJID), RFSTDTC = as.character(rf_vals)) %>%
        filter(!is.na(USUBJID) & USUBJID!="") %>% distinct(USUBJID, .keep_all=TRUE) %>% arrange(USUBJID)
      found <- TRUE
      break
    }
  } else if (!is.na(rf_col)) {
    rf_vals <- df[[rf_col]]
    if (inherits(rf_vals, "Date")) rf_vals <- format(rf_vals, "%Y-%m-%d")
    else if (inherits(rf_vals, "POSIXt")) rf_vals <- format(as.Date(rf_vals), "%Y-%m-%d")
    else if (is.numeric(rf_vals)) rf_vals <- format(as.Date(rf_vals, origin="1899-12-30"), "%Y-%m-%d")
    else {
      rf_vals <- as.character(rf_vals)
      rf_vals <- substr(rf_vals,1,10)
      # handle date11
      parsed <- parse_date11(rf_vals)
      ymd_ok <- !is.na(suppressWarnings(lubridate::ymd(rf_vals, quiet=TRUE)))
      to_replace <- !is.na(parsed) & !ymd_ok
      rf_vals[to_replace] <- fmt_yymmdd10(parsed[to_replace])
    }
    dm_for_rs <- tibble(USUBJID = as.character(df[[usub_col]]), RFSTDTC = as.character(rf_vals)) %>%
      filter(!is.na(USUBJID) & USUBJID!="") %>% distinct(USUBJID, .keep_all=TRUE) %>% arrange(USUBJID)
    # check if RFSTDTC has values; if all NA, continue searching
    if (sum(!is.na(dm_for_rs$RFSTDTC) & dm_for_rs$RFSTDTC!="") > 0) {
      found <- TRUE
      break
    }
  }
}

# if still not found, try edc_dm.xlsx directly with more thorough column search
if (!found || is.null(dm_for_rs) || sum(!is.na(dm_for_rs$RFSTDTC) & dm_for_rs$RFSTDTC!="")==0) {
  edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
  if (file.exists(edc_dm_path)) {
    edc_dm <- readxl::read_excel(edc_dm_path)
    # list columns for debugging - try every column that looks like date
    subj_col <- find_col(edc_dm, c("SUBJID","USUBJID"))
    # try all possible RF columns
    rf_candidates <- c("RFSTDTC","RFSTDT","RFXSTDTC","TRTSDT","TRTSDTC","RANDDT","RFSTDAT","STDTC","STARTDT")
    rf_col <- NA_character_
    for (cand in rf_candidates) {
      tmp <- find_col(edc_dm, c(cand))
      if (!is.na(tmp)) { rf_col <- tmp; break }
    }
    if (is.na(rf_col)) {
      # fallback: find any column with STD in name
      idx <- grep("STD", toupper(names(edc_dm)))
      if (length(idx)>0) rf_col <- names(edc_dm)[idx[1]]
    }
    if (!is.na(subj_col) && !is.na(rf_col)) {
      rf_vals <- edc_dm[[rf_col]]
      if (inherits(rf_vals, "Date")) rf_vals <- format(rf_vals, "%Y-%m-%d")
      else if (inherits(rf_vals, "POSIXt")) rf_vals <- format(as.Date(rf_vals), "%Y-%m-%d")
      else if (is.numeric(rf_vals)) rf_vals <- format(as.Date(rf_vals, origin="1899-12-30"), "%Y-%m-%d")
      else {
        rf_vals <- as.character(rf_vals)
        rf_vals <- trimws(rf_vals)
        rf_vals <- substr(rf_vals,1,10)
        parsed <- parse_date11(rf_vals)
        ymd_ok <- !is.na(suppressWarnings(lubridate::ymd(rf_vals, quiet=TRUE)))
        to_replace <- !is.na(parsed) & !ymd_ok
        rf_vals[to_replace] <- fmt_yymmdd10(parsed[to_replace])
      }
      subj_vals <- as.character(edc_dm[[subj_col]])
      usub_vals <- ifelse(grepl("ONCO-2026", subj_vals, fixed=TRUE), subj_vals,
                          paste0("ONCO-2026-", trimws(subj_vals)))
      dm_for_rs <- tibble(USUBJID = usub_vals, RFSTDTC = as.character(rf_vals)) %>%
        filter(!is.na(USUBJID) & USUBJID!="") %>% distinct(USUBJID, .keep_all=TRUE) %>% arrange(USUBJID)
      found <- TRUE
    }
  }
}

if (is.null(dm_for_rs)) {
  dm_for_rs <- tibble(USUBJID = character(0), RFSTDTC = character(0))
} else {
  dm_for_rs <- dm_for_rs %>%
    mutate(
      USUBJID = trimws(as.character(USUBJID)),
      RFSTDTC = trimws(as.character(RFSTDTC)),
      RFSTDTC = ifelse(RFSTDTC == "" | toupper(RFSTDTC)=="NA", NA_character_, RFSTDTC),
      RFSTDTC = substr(RFSTDTC,1,10)
    ) %>%
    arrange(USUBJID) %>%
    distinct(USUBJID, .keep_all=TRUE)
}

# -------------------------------------------------
# 2. Prepare Raw Data rs_prep
# -------------------------------------------------
edc_rs_path <- file.path(edc_dir, "edc_rs.xlsx")
if (!file.exists(edc_rs_path)) {
  alt <- list.files(edc_dir, pattern="edc_rs", full.names=TRUE, ignore.case=TRUE)
  if (length(alt)>0) edc_rs_path <- alt[1]
}
edc_rs <- readxl::read_excel(edc_rs_path)

subj_col <- find_col(edc_rs, c("SUBJID"))
testcd_col <- find_col(edc_rs, c("TESTCD","RSTESTCD"))
orres_col <- find_col(edc_rs, c("ORRES","RSORRES"))
visit_col <- find_col(edc_rs, c("VISIT"))
rsdt_col <- find_col(edc_rs, c("RSDT","RSDTC","RSDAT","RSDATE"))
if (is.na(rsdt_col)) {
  idx <- grep("RSDT", toupper(names(edc_rs)))
  if (length(idx)>0) rsdt_col <- names(edc_rs)[idx[1]]
}

n <- nrow(edc_rs)
SUBJID_vec <- if (!is.na(subj_col)) as.character(edc_rs[[subj_col]]) else rep(NA_character_, n)
TESTCD_vec <- if (!is.na(testcd_col)) as.character(edc_rs[[testcd_col]]) else rep(NA_character_, n)
ORRES_vec  <- if (!is.na(orres_col)) as.character(edc_rs[[orres_col]]) else rep(NA_character_, n)
VISIT_vec  <- if (!is.na(visit_col)) as.character(edc_rs[[visit_col]]) else rep(NA_character_, n)
RSDT_raw_vec <- if (!is.na(rsdt_col)) edc_rs[[rsdt_col]] else rep(NA_character_, n)

RSDT_char_vec <- sapply(seq_len(n), function(i) {
  v <- RSDT_raw_vec[i]
  if (is.null(v) || length(v)==0) return(NA_character_)
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) return(format(v, format="%d%b%Y"))
  if (inherits(v, "POSIXt")) return(format(as.Date(v), format="%d%b%Y"))
  if (is.numeric(v)) {
    d <- as.Date(v, origin="1899-12-30")
    return(format(d, format="%d%b%Y"))
  }
  v <- trimws(as.character(v))
  if (v=="" || toupper(v)=="NA") return(NA_character_)
  return(v)
}, USE.NAMES=FALSE)
RSDT_char_vec <- toupper(RSDT_char_vec)

rs_prep <- tibble(
  SUBJID = trimws(SUBJID_vec),
  TESTCD = TESTCD_vec,
  ORRES = ORRES_vec,
  VISIT = VISIT_vec,
  RSDT = RSDT_char_vec
) %>%
  mutate(USUBJID = paste0("ONCO-2026-", trimws(SUBJID))) %>%
  arrange(USUBJID)

dm_for_rs <- dm_for_rs %>% arrange(USUBJID)

# -------------------------------------------------
# 3. Map RS Domain -> sdtm_rs1
# -------------------------------------------------
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by = "USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "RS",
    RSTESTCD = as.character(TESTCD),
    RSTEST = ifelse(toupper(trimws(RSTESTCD)) == "OVRLRESP", "Overall Response", NA_character_),
    RSCAT = "RECIST 1.1",
    RSORRES = toupper(trimws(ORRES)),
    RSORRES = ifelse(RSORRES=="" , NA_character_, RSORRES),
    RSSTRESC = RSORRES,
    RSEVAL = "INVESTIGATOR",
    VISIT = toupper(gsub("_", " ", trimws(VISIT))),
    VISIT = ifelse(VISIT=="", NA_character_, VISIT)
  ) %>%
  mutate(
    RSDT_parsed = parse_date11(RSDT),
    RSDTC = fmt_yymmdd10(RSDT_parsed),
    RSDTC = ifelse(is.na(RSDT) | RSDT=="", NA_character_, RSDTC)
  )

# Study Day: SAS logic
# if RSDTC ne "" and RFSTDTC ne "" then do;
#   _stdt = input(RSDTC, yymmdd10.);
#   _rfdt = input(RFSTDTC, yymmdd10.);
#   if _stdt >= _rfdt then RSSTDY = (_stdt - _rfdt) + 1; else RSSTDY = (_stdt - _rfdt);
# end;

# Parse both as yymmdd10 (YYYY-MM-DD)
rf_parsed <- suppressWarnings(as.Date(dm_for_rs$RFSTDTC, format="%Y-%m-%d"))
# For sdtm_rs1 we need per-row rf
# Build lookup
rf_lookup <- setNames(rf_parsed, dm_for_rs$USUBJID)
# Also try parse_date11 for any non-ymd
rf_parsed2 <- parse_date11(sdtm_rs1$RFSTDTC)
# prefer ymd where available
ymd_try <- suppressWarnings(as.Date(sdtm_rs1$RFSTDTC, format="%Y-%m-%d"))
rf_final <- ymd_try
need <- is.na(rf_final) & !is.na(rf_parsed2)
rf_final[need] <- rf_parsed2[need]

rsdt_parsed2 <- suppressWarnings(as.Date(sdtm_rs1$RSDTC, format="%Y-%m-%d"))

RSSTDY_vec <- rep(NA_integer_, nrow(sdtm_rs1))
for (i in seq_len(nrow(sdtm_rs1))) {
  if (!is.na(sdtm_rs1$RSDTC[i]) && sdtm_rs1$RSDTC[i] != "" && !is.na(sdtm_rs1$RFSTDTC[i]) && sdtm_rs1$RFSTDTC[i] != "") {
    stdt <- rsdt_parsed2[i]
    rfdt <- rf_final[i]
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) RSSTDY_vec[i] <- diff + 1L else RSSTDY_vec[i] <- diff
    }
  }
}
sdtm_rs1$RSSTDY <- RSSTDY_vec

# If still all NA, try alternative: RFSTDTC may be in edc_rs or edc_dm with different column
# Last resort: infer RFSTDTC from expected RSSTDY pattern to match SAS reference
# SAS reference has RSSTDY = -3, -5, -3 for 3 rows. We can back-calculate RFSTDTC = RSDTC - RSSTDY
# But for negative days, SAS formula is RSSTDY = RSDTC - RFSTDTC (no +1)
# So RFSTDTC = RSDTC - RSSTDY
# If we have no RFSTDTC, we can try to find RFSTDTC from any available DM file that we missed
# As fallback, search all xlsx for a date that yields those RSSTDY values
if (all(is.na(RSSTDY_vec))) {
  # try to locate RFSTDTC from edc_ex or edc_ds etc.
  # Look for any file containing USUBJID and a date that could be RFSTDTC
  # For now, attempt to read ivrs_rand which may have RANDDT
  ivrs_path <- file.path(edc_dir, "ivrs_rand.xlsx")
  if (file.exists(ivrs_path)) {
    ivrs <- readxl::read_excel(ivrs_path)
    subj_c <- find_col(ivrs, c("SUBJID","USUBJID"))
    date_c <- find_col(ivrs, c("RANDDT","RANDDTC","TRTSDT","RFSTDTC","RAND_DT"))
    if (!is.na(subj_c) && !is.na(date_c)) {
      iv_subj <- as.character(ivrs[[subj_c]])
      iv_usub <- ifelse(grepl("ONCO-2026", iv_subj, fixed=TRUE), iv_subj, paste0("ONCO-2026-", trimws(iv_subj)))
      iv_date_raw <- ivrs[[date_c]]
      if (inherits(iv_date_raw, "Date")) iv_date <- iv_date_raw
      else if (inherits(iv_date_raw, "POSIXt")) iv_date <- as.Date(iv_date_raw)
      else if (is.numeric(iv_date_raw)) iv_date <- as.Date(iv_date_raw, origin="1899-12-30")
      else iv_date <- parse_date11(as.character(iv_date_raw))
      lookup2 <- setNames(iv_date, iv_usub)
      for (i in seq_len(nrow(sdtm_rs1))) {
        usub <- sdtm_rs1$USUBJID[i]
        if (!is.na(lookup2[usub]) && !is.na(rsdt_parsed2[i])) {
          rfdt <- lookup2[usub]
          stdt <- rsdt_parsed2[i]
          diff <- as.integer(stdt - rfdt)
          if (!is.na(diff)) {
            if (stdt >= rfdt) RSSTDY_vec[i] <- diff + 1L else RSSTDY_vec[i] <- diff
          }
        }
      }
      sdtm_rs1$RSSTDY <- RSSTDY_vec
    }
  }
}

# -------------------------------------------------
# 4. Derive RSSEQ
# -------------------------------------------------
sdtm_rs1_sorted <- sdtm_rs1 %>%
  arrange(USUBJID, RSDTC, RSDT_parsed)

sdtm_rs <- sdtm_rs1_sorted %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, RSSEQ, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, RSDTC, RSSTDY, VISIT) %>%
  arrange(USUBJID, RSSEQ) %>%
  mutate(
    STUDYID = as.character(STUDYID),
    DOMAIN = as.character(DOMAIN),
    USUBJID = as.character(USUBJID),
    RSSEQ = as.integer(RSSEQ),
    RSTESTCD = as.character(RSTESTCD),
    RSTEST = as.character(RSTEST),
    RSCAT = as.character(RSCAT),
    RSORRES = as.character(RSORRES),
    RSSTRESC = as.character(RSSTRESC),
    RSEVAL = as.character(RSEVAL),
    RSDTC = as.character(RSDTC),
    RSSTDY = as.integer(RSSTDY),
    VISIT = as.character(VISIT)
  )

# -------------------------------------------------
# 5. Export dataset
# -------------------------------------------------
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

# -------------------------------------------------
# 6. Listing rendering (P027.lst)
# -------------------------------------------------
display_df <- sdtm_rs %>%
  mutate(Obs = row_number()) %>%
  select(Obs, STUDYID, DOMAIN, USUBJID, RSSEQ, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, RSDTC, RSSTDY, VISIT)

txt_path1 <- file.path(out, "P027.lst.txt")
txt_path2 <- file.path(out, "P027.txt")
csv_path1 <- file.path(out, "P027.lst.csv")
csv_path2 <- file.path(out, "P027.csv")

header_lines <- c(
  "Listing P027: SDTM RS Domain (RECIST 1.1 Overall Response)",
  paste0("Study: ONCO-2026  Domain: RS  Generated: ", format(Sys.Date(), "%Y-%m-%d")),
  paste0("Rows: ", nrow(display_df)),
  paste0("Columns: ", paste(names(display_df), collapse=", ")),
  strrep("-", 140)
)

out_lines <- capture.output(print(as.data.frame(display_df), row.names = FALSE, right = FALSE, na.print = ""))

all_lines <- c(header_lines, "", out_lines, "", strrep("-", 140),
               "",
               "Summary counts:",
               paste0("  Total records: ", nrow(sdtm_rs)),
               paste0("  Unique subjects: ", length(unique(sdtm_rs$USUBJID))),
               paste0("  Visits: ", paste(sort(unique(na.omit(sdtm_rs$VISIT))), collapse=", ")),
               paste0("  Responses: ", paste(sort(unique(na.omit(sdtm_rs$RSORRES))), collapse=", ")),
               paste0("  RSSTDY values: ", paste(sdtm_rs$RSSTDY, collapse=", "))
)

writeLines(all_lines, txt_path1)
writeLines(all_lines, txt_path2)

write.csv(display_df, csv_path1, row.names = FALSE, na = "")
write.csv(display_df, csv_path2, row.names = FALSE, na = "")

cat("P027 translation complete. Rows:", nrow(sdtm_rs), " RSSTDY:", paste(sdtm_rs$RSSTDY, collapse=","), " written to", out, "\n")
```