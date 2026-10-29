```r
# P027_rs.sas translation to R
# Produces rs.csv and P027 listing (txt + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawsdtm <- file.path(root, "data/raw/cdisc-pilot/sdtm")
rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")
rawdir  <- file.path(root, "data/raw")
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
  library(data.table)
})

# helper: SAS catx
sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(x) trimws(as.character(x)))
  # remove NA/empty? SAS catx skips empty
  # but we keep simple paste
  do.call(paste, c(args, sep = sep))
}

# helper: parse date11 (DDMMMYYYY) -> Date
parse_date11 <- function(x) {
  if (is.null(x)) return(as.Date(NA))
  if (length(x)==0) return(as.Date(NA))
  # vectorized
  sapply(x, function(v) {
    if (is.na(v)) return(as.Date(NA))
    if (inherits(v, "Date")) return(v)
    if (inherits(v, "POSIXt")) return(as.Date(v))
    if (is.numeric(v) && !is.na(v)) {
      # Excel numeric date
      return(as.Date(v, origin = "1899-12-30"))
    }
    v <- trimws(as.character(v))
    if (v == "" || toupper(v) == "NA") return(as.Date(NA))
    # try lubridate dmy
    d <- suppressWarnings(lubridate::dmy(v, quiet = TRUE))
    if (!is.na(d)) return(d)
    # try ymd
    d2 <- suppressWarnings(lubridate::ymd(v, quiet = TRUE))
    if (!is.na(d2)) return(d2)
    # try as.Date with date11 format
    # ensure month abbreviation case
    # convert to upper then title
    v2 <- toupper(v)
    # remove dashes/spaces
    for (fmt in c("%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%d%b%y")) {
      d3 <- suppressWarnings(as.Date(v2, format = fmt))
      if (!is.na(d3)) return(d3)
    }
    return(as.Date(NA))
  }, USE.NAMES = FALSE) |> as.Date(origin = "1970-01-01")
}

# helper: format Date to yymmdd10 (YYYY-MM-DD)
fmt_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# helper: find column by upper name candidates
find_col <- function(df, candidates) {
  cn_up <- toupper(names(df))
  for (cand in candidates) {
    idx <- which(cn_up == toupper(cand))
    if (length(idx) > 0) return(names(df)[idx[1]])
  }
  return(NA_character_)
}

# -------------------------------------------------
# 1. Prepare DM Reference Data (dm_for_rs)
# -------------------------------------------------
dm_for_rs <- NULL

# try to locate sdtm_dm file
sdtm_dm_candidates <- c(
  file.path(rawsdtm, "dm.xpt"),
  file.path(rawsdtm, "DM.xpt"),
  file.path(rawsdtm, "dm.sas7bdat"),
  file.path(rawsdtm, "DM.sas7bdat"),
  file.path(root, "data/raw/cdisc-pilot/sdtm/dm.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt"
)

found_dm <- FALSE
for (f in sdtm_dm_candidates) {
  if (file.exists(f)) {
    ext <- tolower(tools::file_ext(f))
    if (ext == "xpt") {
      tmp <- haven::read_xpt(f)
    } else if (ext == "sas7bdat") {
      tmp <- haven::read_sas(f)
    } else {
      next
    }
    # keep USUBJID RFSTDTC
    cn_up <- toupper(names(tmp))
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
        # if RFSTDTC is Date/POSIX, format
        mutate(RFSTDTC = ifelse(is.na(RFSTDTC), NA_character_, as.character(RFSTDTC))) %>%
        arrange(USUBJID)
      found_dm <- TRUE
      break
    }
  }
}

if (!found_dm || is.null(dm_for_rs)) {
  # fallback: build from edc_dm.xlsx
  edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
  if (file.exists(edc_dm_path)) {
    edc_dm <- readxl::read_excel(edc_dm_path)
    # normalize
    cn_up <- toupper(names(edc_dm))
    subj_col <- find_col(edc_dm, c("SUBJID","USUBJID"))
    rf_col <- find_col(edc_dm, c("RFSTDTC","RFSTDT","RFXSTDTC","TRTSDT","RANDDT","RFSTDT","BRTHDTC"))
    # if no RFSTDTC, try to find any date column
    if (is.na(rf_col)) {
      # look for column containing RFSTD
      idx <- grep("RFSTD", cn_up)
      if (length(idx)>0) rf_col <- names(edc_dm)[idx[1]]
    }
    if (!is.na(subj_col)) {
      edc_dm <- edc_dm %>%
        mutate(
          SUBJID_tmp = as.character(.data[[subj_col]]),
          USUBJID = ifelse(grepl("ONCO-2026", SUBJID_tmp, fixed=TRUE), SUBJID_tmp,
                           paste0("ONCO-2026-", trimws(SUBJID_tmp)))
        )
      if (!is.na(rf_col)) {
        # handle Date type
        rf_vals <- edc_dm[[rf_col]]
        if (inherits(rf_vals, "Date")) {
          rf_vals <- format(rf_vals, "%Y-%m-%d")
        } else if (inherits(rf_vals, "POSIXt")) {
          rf_vals <- format(as.Date(rf_vals), "%Y-%m-%d")
        } else if (is.numeric(rf_vals)) {
          rf_vals <- format(as.Date(rf_vals, origin="1899-12-30"), "%Y-%m-%d")
        } else {
          rf_vals <- as.character(rf_vals)
          # try to parse and reformat to yymmdd10 if needed
          # if values like 01JAN2020, convert
          parsed <- parse_date11(rf_vals)
          # if parsed not NA, format
          has_parsed <- !is.na(parsed)
          rf_vals[has_parsed] <- fmt_yymmdd10(parsed[has_parsed])
        }
        dm_for_rs <- tibble(
          USUBJID = as.character(edc_dm$USUBJID),
          RFSTDTC = as.character(rf_vals)
        ) %>%
          filter(!is.na(USUBJID) & USUBJID != "") %>%
          arrange(USUBJID)
      } else {
        dm_for_rs <- tibble(
          USUBJID = as.character(edc_dm$USUBJID),
          RFSTDTC = NA_character_
        ) %>%
          arrange(USUBJID)
      }
    }
  }
}

if (is.null(dm_for_rs)) {
  dm_for_rs <- tibble(USUBJID = character(0), RFSTDTC = character(0))
} else {
  # keep only USUBJID RFSTDTC, sort by USUBJID (SAS sort)
  dm_for_rs <- dm_for_rs %>%
    select(USUBJID, RFSTDTC) %>%
    mutate(
      USUBJID = trimws(as.character(USUBJID)),
      RFSTDTC = trimws(as.character(RFSTDTC)),
      RFSTDTC = ifelse(RFSTDTC == "" | toupper(RFSTDTC)=="NA", NA_character_, RFSTDTC)
    ) %>%
    arrange(USUBJID)
  # if RFSTDTC contains datetime, keep date part
  dm_for_rs$RFSTDTC <- ifelse(is.na(dm_for_rs$RFSTDTC), NA_character_,
                              substr(dm_for_rs$RFSTDTC, 1, 10))
}

# -------------------------------------------------
# 2. Prepare Raw Data rs_prep
# -------------------------------------------------
edc_rs_path <- file.path(edc_dir, "edc_rs.xlsx")
if (!file.exists(edc_rs_path)) {
  # alternative search
  alt <- list.files(edc_dir, pattern="edc_rs", full.names=TRUE, ignore.case=TRUE)
  if (length(alt)>0) edc_rs_path <- alt[1]
}
edc_rs <- readxl::read_excel(edc_rs_path)

# Normalize column names to upper for lookup but keep original
# Find required columns
subj_col <- find_col(edc_rs, c("SUBJID"))
testcd_col <- find_col(edc_rs, c("TESTCD","RSTESTCD"))
orres_col <- find_col(edc_rs, c("ORRES","RSORRES"))
visit_col <- find_col(edc_rs, c("VISIT"))
rsdt_col <- find_col(edc_rs, c("RSDT","RSDTC","RSDAT","RSDATE","RSDTC1","DATE"))

# If not found, try to guess: column containing RSDT
if (is.na(rsdt_col)) {
  cn_up <- toupper(names(edc_rs))
  idx <- grep("RSDT", cn_up)
  if (length(idx)>0) rsdt_col <- names(edc_rs)[idx[1]]
}

# Build rs_prep
rs_prep <- edc_rs %>%
  mutate(
    SUBJID = if (!is.na(subj_col)) as.character(.data[[subj_col]]) else NA_character_,
    TESTCD = if (!is.na(testcd_col)) as.character(.data[[testcd_col]]) else NA_character_,
    ORRES = if (!is.na(orres_col)) as.character(.data[[orres_col]]) else NA_character_,
    VISIT = if (!is.na(visit_col)) as.character(.data[[visit_col]]) else NA_character_,
    RSDT_raw = if (!is.na(rsdt_col)) .data[[rsdt_col]] else NA
  ) %>%
  mutate(
    SUBJID = trimws(SUBJID),
    USUBJID = paste0("ONCO-2026-", trimws(SUBJID))
  ) %>%
  # keep original RSDT as character for conversion logic
  mutate(
    RSDT_char = case_when(
      inherits(RSDT_raw, "Date") ~ format(RSDT_raw, "%d%b%Y") %>% toupper(),
      inherits(RSDT_raw, "POSIXt") ~ format(as.Date(RSDT_raw), "%d%b%Y") %>% toupper(),
      is.numeric(RSDT_raw) ~ format(as.Date(RSDT_raw, origin="1899-12-30"), "%d%b%Y") %>% toupper(),
      TRUE ~ as.character(RSDT_raw)
    ),
    RSDT_char = trimws(RSDT_char),
    RSDT_char = ifelse(RSDT_char=="" | toupper(RSDT_char)=="NA", NA_character_, RSDT_char)
  ) %>%
  select(USUBJID, SUBJID, TESTCD, ORRES, VISIT, RSDT = RSDT_char, RSDT_raw)

# sort by USUBJID (SAS)
rs_prep <- rs_prep %>% arrange(USUBJID)
dm_for_rs <- dm_for_rs %>% arrange(USUBJID)

# -------------------------------------------------
# 3. Map RS Domain -> sdtm_rs1
# -------------------------------------------------
# merge rs_prep(in=r) dm_for_rs by USUBJID (left join keep all rs_prep)
sdtm_rs1 <- rs_prep %>%
  left_join(dm_for_rs, by = "USUBJID")

# Now derive fields
sdtm_rs1 <- sdtm_rs1 %>%
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
  # Date Conversion: if RSDT ne "" then RSDTC = put(input(RSDT, date11.), yymmdd10.)
  mutate(
    RSDT_parsed = parse_date11(RSDT),
    RSDTC = fmt_yymmdd10(RSDT_parsed),
    RSDTC = ifelse(is.na(RSDT) | RSDT=="", NA_character_, RSDTC)
  ) %>%
  # Study Day
  mutate(
    RFSTDTC_parsed = suppressWarnings(as.Date(substr(RFSTDTC,1,10), format="%Y-%m-%d")),
    # if RFSTDTC not in yymmdd10, try parse_date11
    RFSTDTC_parsed = ifelse(is.na(RFSTDTC_parsed) & !is.na(RFSTDTC),
                            as.character(parse_date11(RFSTDTC)), as.character(RFSTDTC_parsed)) %>% as.Date(),
    RSDTC_parsed2 = suppressWarnings(as.Date(RSDTC, format="%Y-%m-%d")),
    RSSTDY = case_when(
      is.na(RSDTC) | RSDTC=="" ~ NA_real_,
      is.na(RFSTDTC) | RFSTDTC=="" ~ NA_real_,
      TRUE ~ {
        stdt <- RSDTC_parsed2
        rfdt <- RFSTDTC_parsed
        # compute diff
        diff <- as.integer(stdt - rfdt)
        ifelse(stdt >= rfdt, diff + 1, diff)
      }
    )
  ) %>%
  # keep needed columns for next step
  select(STUDYID, DOMAIN, USUBJID, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, VISIT, RSDTC, RSSTDY, RSDT_parsed)

# -------------------------------------------------
# 4. Derive RSSEQ
# -------------------------------------------------
# proc sort data=sdtm_rs1 by USUBJID RSDTC
sdtm_rs1_sorted <- sdtm_rs1 %>%
  arrange(USUBJID, desc(is.na(RSDTC)), RSDTC, desc(is.na(RSDT_parsed)), RSDT_parsed)

# retain RSSEQ per USUBJID
sdtm_rs <- sdtm_rs1_sorted %>%
  group_by(USUBJID) %>%
  mutate(RSSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, RSSEQ, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, RSDTC, RSSTDY, VISIT) %>%
  arrange(USUBJID, RSSEQ)

# Ensure types and lengths (SAS lengths not needed but keep)
sdtm_rs <- sdtm_rs %>%
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
# 5. View Final Dataset (proc print) -> create listing text
# -------------------------------------------------
# Write dataset rs.csv
write.csv(sdtm_rs, file.path(out, "rs.csv"), row.names = FALSE, na = "")

# Create listing text rendering (P027.lst)
# SAS proc print prints all observations with Obs column
# Reproduce as plain text
listing_df <- sdtm_rs

# Build text file
txt_path <- file.path(out, "P027.txt")
lst_txt_path <- file.path(out, "P027.lst.txt")
csv_body_path <- file.path(out, "P027.csv")
csv_lst_path <- file.path(out, "P027.lst.csv")

# Create formatted text
header_lines <- c(
  "Listing P027: SDTM RS Domain (RECIST 1.1 Overall Response)",
  paste0("Study: ONCO-2026  Domain: RS  Generated: ", format(Sys.Date(), "%Y-%m-%d")),
  strrep("-", 120)
)

# Use capture.output for print
# Prepare display dataframe with Obs
display_df <- listing_df %>%
  mutate(Obs = row_number()) %>%
  select(Obs, STUDYID, DOMAIN, USUBJID, RSSEQ, RSTESTCD, RSTEST, RSCAT, RSORRES, RSSTRESC, RSEVAL, RSDTC, RSSTDY, VISIT)

# Format for text: use print with width
txt_con <- file(txt_path, open = "wt")
writeLines(header_lines, txt_con)
writeLines(paste0("Rows: ", nrow(display_df)), txt_con)
writeLines("", txt_con)
# capture print
out_lines <- capture.output(print(as.data.frame(display_df), row.names = FALSE, right = FALSE, na.print = ""))
writeLines(out_lines, txt_con)
writeLines("", txt_con)
writeLines(strrep("-", 120), txt_con)
close(txt_con)

# also write lst variant
file.copy(txt_path, lst_txt_path, overwrite = TRUE)

# CSV of table body (same as rs but with Obs)
write.csv(display_df, csv_body_path, row.names = FALSE, na = "")
file.copy(csv_body_path, csv_lst_path, overwrite = TRUE)

# Also ensure rs.csv already written; also write sdtm_rs.csv variant if needed
# Write additional copy for verification
write.csv(sdtm_rs, file.path(out, "P027_rs.csv"), row.names = FALSE, na = "")

# Log progress
cat("P027 translation complete. Rows:", nrow(sdtm_rs), "written to", out, "\n")
```