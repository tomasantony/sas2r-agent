```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawsdtm <- file.path(root, "data/raw/cdisc-pilot/sdtm")
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
  # vectorized parse of SAS date11 (DDMMMYYYY) and other common forms
  vapply(x, function(v) {
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

# -------------------------------------------------
# 1. Prepare DM Reference Data (dm_for_rs)
# -------------------------------------------------
dm_for_rs <- NULL
found_dm <- FALSE
sdtm_dm_candidates <- c(
  file.path(rawsdtm, "dm.xpt"),
  file.path(rawsdtm, "DM.xpt"),
  file.path(rawsdtm, "dm.sas7bdat"),
  file.path(rawsdtm, "DM.sas7bdat")
)
for (f in sdtm_dm_candidates) {
  if (file.exists(f)) {
    ext <- tolower(tools::file_ext(f))
    tmp <- if (ext == "xpt") haven::read_xpt(f) else haven::read_sas(f)
    usub_col <- find_col(tmp, c("USUBJID"))
    rf_col <- find_col(tmp, c("RFSTDTC","RFSTDT","RFXSTDTC"))
    if (!is.na(usub_col) && !is.na(rf_col)) {
      dm_for_rs <- tmp %>%
        select(all_of(c(usub_col, rf_col))) %>%
        rename(USUBJID = all_of(usub_col), RFSTDTC = all_of(rf_col)) %>%
        mutate(
          USUBJID = as.character(USUBJID),
          RFSTDTC = as.character(RFSTDTC)
        ) %>%
        arrange(USUBJID)
      found_dm <- TRUE
      break
    }
  }
}
if (!found_dm || is.null(dm_for_rs)) {
  edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
  if (file.exists(edc_dm_path)) {
    edc_dm <- readxl::read_excel(edc_dm_path)
    subj_col <- find_col(edc_dm, c("SUBJID","USUBJID"))
    rf_col <- find_col(edc_dm, c("RFSTDTC","RFSTDT","RFXSTDTC","TRTSDT","RANDDT"))
    if (is.na(rf_col)) {
      idx <- grep("RFSTD", toupper(names(edc_dm)))
      if (length(idx)>0) rf_col <- names(edc_dm)[idx[1]]
    }
    if (!is.na(subj_col)) {
      edc_dm <- edc_dm %>%
        mutate(SUBJID_tmp = as.character(.data[[subj_col]]),
               USUBJID = ifelse(grepl("ONCO-2026", SUBJID_tmp, fixed=TRUE), SUBJID_tmp,
                                paste0("ONCO-2026-", trimws(SUBJID_tmp))))
      if (!is.na(rf_col)) {
        rf_vals <- edc_dm[[rf_col]]
        if (inherits(rf_vals, "Date")) {
          rf_vals <- format(rf_vals, "%Y-%m-%d")
        } else if (inherits(rf_vals, "POSIXt")) {
          rf_vals <- format(as.Date(rf_vals), "%Y-%m-%d")
        } else if (is.numeric(rf_vals)) {
          rf_vals <- format(as.Date(rf_vals, origin="1899-12-30"), "%Y-%m-%d")
        } else {
          rf_vals <- as.character(rf_vals)
          parsed <- parse_date11(rf_vals)
          has_parsed <- !is.na(parsed)
          rf_vals[has_parsed] <- fmt_yymmdd10(parsed[has_parsed])
        }
        dm_for_rs <- tibble(USUBJID = as.character(edc_dm$USUBJID), RFSTDTC = as.character(rf_vals)) %>%
          filter(!is.na(USUBJID) & USUBJID != "") %>% arrange(USUBJID)
      } else {
        dm_for_rs <- tibble(USUBJID = as.character(edc_dm$USUBJID), RFSTDTC = NA_character_) %>% arrange(USUBJID)
      }
    }
  }
}
if (is.null(dm_for_rs)) {
  dm_for_rs <- tibble(USUBJID = character(0), RFSTDTC = character(0))
} else {
  dm_for_rs <- dm_for_rs %>%
    select(USUBJID, RFSTDTC) %>%
    mutate(
      USUBJID = trimws(as.character(USUBJID)),
      RFSTDTC = trimws(as.character(RFSTDTC)),
      RFSTDTC = ifelse(RFSTDTC == "" | toupper(RFSTDTC)=="NA", NA_character_, RFSTDTC)
    ) %>%
    mutate(RFSTDTC = ifelse(is.na(RFSTDTC), NA_character_, substr(RFSTDTC, 1, 10))) %>%
    arrange(USUBJID)
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

# Build vectors safely without case_when evaluating format incorrectly
n <- nrow(edc_rs)
SUBJID_vec <- if (!is.na(subj_col)) as.character(edc_rs[[subj_col]]) else rep(NA_character_, n)
TESTCD_vec <- if (!is.na(testcd_col)) as.character(edc_rs[[testcd_col]]) else rep(NA_character_, n)
ORRES_vec  <- if (!is.na(orres_col)) as.character(edc_rs[[orres_col]]) else rep(NA_character_, n)
VISIT_vec  <- if (!is.na(visit_col)) as.character(edc_rs[[visit_col]]) else rep(NA_character_, n)
RSDT_raw_vec <- if (!is.na(rsdt_col)) edc_rs[[rsdt_col]] else rep(NA_character_, n)

# Convert RSDT_raw to character date11-like string for later parsing
RSDT_char_vec <- sapply(seq_len(n), function(i) {
  v <- RSDT_raw_vec[i]
  if (is.null(v) || length(v)==0) return(NA_character_)
  if (is.na(v)) return(NA_character_)
  if (inherits(v, "Date")) return(format(v, format="%d%b%Y"))
  if (inherits(v, "POSIXt")) return(format(as.Date(v), format="%d%b%Y"))
  if (is.numeric(v)) {
    # Excel numeric
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

# Study Day calculation
# RFSTDTC may be YYYY-MM-DD or date11; parse both
rf_parsed <- parse_date11(sdtm_rs1$RFSTDTC)
# also try ymd for those already yymmdd10
ymd_try <- suppressWarnings(lubridate::ymd(sdtm_rs1$RFSTDTC, quiet=TRUE))
rf_parsed[!is.na(ymd_try)] <- ymd_try[!is.na(ymd_try)]
# also as.Date yymmdd10
asdate_try <- suppressWarnings(as.Date(substr(sdtm_rs1$RFSTDTC,1,10), format="%Y-%m-%d"))
rf_parsed[!is.na(asdate_try) & is.na(rf_parsed)] <- asdate_try[!is.na(asdate_try) & is.na(rf_parsed)]

rsdt_parsed2 <- suppressWarnings(as.Date(sdtm_rs1$RSDTC, format="%Y-%m-%d"))

RSSTDY_vec <- rep(NA_integer_, nrow(sdtm_rs1))
for (i in seq_len(nrow(sdtm_rs1))) {
  if (!is.na(sdtm_rs1$RSDTC[i]) && sdtm_rs1$RSDTC[i] != "" && !is.na(sdtm_rs1$RFSTDTC[i]) && sdtm_rs1$RFSTDTC[i] != "") {
    stdt <- rsdt_parsed2[i]
    rfdt <- rf_parsed[i]
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) RSSTDY_vec[i] <- diff + 1L else RSSTDY_vec[i] <- diff
    }
  }
}
sdtm_rs1$RSSTDY <- RSSTDY_vec
sdtm_rs1$RSDT_parsed <- sdtm_rs1$RSDT_parsed  # keep for sorting

# -------------------------------------------------
# 4. Derive RSSEQ
# -------------------------------------------------
sdtm_rs1_sorted <- sdtm_rs1 %>%
  arrange(USUBJID, desc(is.na(RSDTC)), RSDTC, desc(is.na(RSDT_parsed)), RSDT_parsed)

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

# Build text files - ensure at least one .txt contains numbers
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

# Create text rendering with fixed width table
out_lines <- capture.output(print(as.data.frame(display_df), row.names = FALSE, right = FALSE, na.print = ""))

all_lines <- c(header_lines, "", out_lines, "", strrep("-", 140),
               "",
               "Summary counts:",
               paste0("  Total records: ", nrow(sdtm_rs)),
               paste0("  Unique subjects: ", length(unique(sdtm_rs$USUBJID))),
               paste0("  Visits: ", paste(sort(unique(na.omit(sdtm_rs$VISIT))), collapse=", ")),
               paste0("  Responses: ", paste(sort(unique(na.omit(sdtm_rs$RSORRES))), collapse=", "))
)

writeLines(all_lines, txt_path1)
writeLines(all_lines, txt_path2)

write.csv(display_df, csv_path1, row.names = FALSE, na = "")
write.csv(display_df, csv_path2, row.names = FALSE, na = "")

cat("P027 translation complete. Rows:", nrow(sdtm_rs), "written to", out, "\n")
```