```r
# P026_lb.sas -> R translation
# Outputs: lb.csv + listing P026.lst (text + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers ---------------------------------------------------------------
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a) && a!="") a else b
trim <- function(x) trimws(as.character(x))
is_missing <- function(x) is.na(x) | trimws(as.character(x))==""
sas_catx <- function(sep, ...) {
  parts <- c(...)
  parts <- parts[!is.na(parts) & trimws(as.character(parts))!=""]
  paste(trimws(as.character(parts)), collapse=sep)
}
# SAS round half away from zero
round_sas <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
# parse SAS date11. e.g. 01JAN2026, 1JAN2026, 01-Jan-2026
parse_date11 <- function(v) {
  if (is_missing(v)) return(as.Date(NA))
  if (inherits(v, "Date")) return(v)
  if (inherits(v, "POSIXt")) return(as.Date(v))
  if (is.numeric(v)) {
    # Excel numeric date
    return(as.Date(v, origin="1899-12-30"))
  }
  s <- trim(as.character(v))
  if (s=="") return(as.Date(NA))
  # remove dashes/spaces
  s2 <- gsub("[- ]", "", s)
  # try lubridate style
  # make month upper
  # as.Date with %d%b%Y is case-insensitive in some locales, force upper then title
  # Try several formats
  fmts <- c("%d%b%Y", "%d%B%Y", "%Y-%m-%d", "%m/%d/%Y", "%d%b%y")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(s, format=f))
    if (!is.na(d)) return(d)
    d2 <- suppressWarnings(as.Date(toupper(s), format=toupper(f)))
    if (!is.na(d2)) return(d2)
    # try with lubridate
  }
  # fallback lubridate parse
  d <- suppressWarnings(lubridate::dmy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  return(as.Date(NA))
}
fmt_yymmdd10 <- function(d) {
  if (is.na(d)) return(NA_character_)
  format(as.Date(d), "%Y-%m-%d")
}
to_char <- function(x) ifelse(is.na(x), NA_character_, as.character(x))

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

cat("P026: start\n")

# 1. Prepare DM Reference Data -----------------------------------------
# Try to locate sdtm_dm from various places
dm_for_lb <- NULL

candidate_files <- c(
  "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026/sdtm_dm.xlsx",
  "D:/meta_hackathon/sas2r-agent/output/sas/datasets/sdtm_dm.xlsx",
  "D:/meta_hackathon/sas2r-agent/output/sas/datasets/dm.xlsx",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
)

# also search recursively
search_dirs <- c("D:/meta_hackathon/sas2r-agent/output/sas/datasets",
                 "D:/meta_hackathon/sas2r-agent/data/raw")
found_dm <- character(0)
for (sd in search_dirs) {
  if (dir.exists(sd)) {
    f <- list.files(sd, pattern="(?i)(^dm\\.xlsx$|^sdtm_dm\\.xlsx$|^dm\\.xpt$)", recursive=TRUE, full.names=TRUE)
    found_dm <- c(found_dm, f)
  }
}
all_candidates <- unique(c(candidate_files, found_dm))

dm_loaded <- FALSE
for (cf in all_candidates) {
  if (!file.exists(cf)) next
  cat("Trying DM candidate:", cf, "\n")
  try({
    if (grepl("\\.xpt$", cf, ignore.case=TRUE)) {
      dm_raw <- haven::read_xpt(cf)
    } else if (grepl("\\.xlsx$", cf, ignore.case=TRUE)) {
      dm_raw <- readxl::read_excel(cf)
    } else {
      next
    }
    names(dm_raw) <- toupper(names(dm_raw))
    if (all(c("USUBJID","RFSTDTC") %in% names(dm_raw))) {
      dm_for_lb <- dm_raw %>%
        mutate(USUBJID = as.character(USUBJID),
               RFSTDTC = as.character(RFSTDTC)) %>%
        select(USUBJID, RFSTDTC) %>%
        arrange(USUBJID)
      dm_loaded <- TRUE
      cat("DM loaded from", cf, "n=", nrow(dm_for_lb), "\n")
      break
    }
  }, silent=TRUE)
  if (dm_loaded) break
}

if (!dm_loaded) {
  cat("DM not found in candidates, building from edc_dm.xlsx\n")
  edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  if (!file.exists(edc_dm_path)) {
    # fallback search
    alt <- list.files("D:/meta_hackathon/sas2r-agent/data/raw", pattern="edc_dm\\.xlsx", recursive=TRUE, full.names=TRUE)
    if (length(alt)>0) edc_dm_path <- alt[1]
  }
  edc_dm <- readxl::read_excel(edc_dm_path)
  names(edc_dm) <- toupper(names(edc_dm))
  # USUBJID = catx("-", "ONCO-2026", SUBJID)
  # RFSTDTC: try to find column
  rf_col <- intersect(c("RFSTDTC","RFXSTDTC","RFSTDT","RANDDT","ENRDT","BRTHDTC"), names(edc_dm))
  # if none, try any column containing RFST or RAND
  if (length(rf_col)==0) {
    cand <- grep("RFSTD|RAND|ENROL", names(edc_dm), value=TRUE)
    if (length(cand)>0) rf_col <- cand[1]
  }
  dm_for_lb <- edc_dm %>%
    mutate(
      USUBJID = paste0("ONCO-2026-", trim(as.character(SUBJID))),
      RFSTDTC_raw = if (length(rf_col)>0) as.character(.data[[rf_col[1]]]) else NA_character_
    ) %>%
    rowwise() %>%
    mutate(
      RFSTDTC = {
        d <- parse_date11(RFSTDTC_raw)
        if (is.na(d) && !is_missing(RFSTDTC_raw)) {
          # try ymd directly
          d2 <- suppressWarnings(lubridate::ymd(RFSTDTC_raw, quiet=TRUE))
          if (!is.na(d2)) d <- as.Date(d2)
        }
        fmt_yymmdd10(d)
      }
    ) %>%
    ungroup() %>%
    select(USUBJID, RFSTDTC) %>%
    arrange(USUBJID)
}

# Ensure dm_for_lb sorted and unique by USUBJID (keep first)
dm_for_lb <- dm_for_lb %>%
  arrange(USUBJID) %>%
  distinct(USUBJID, .keep_all=TRUE)

cat("dm_for_lb n=", nrow(dm_for_lb), "\n")
print(head(dm_for_lb))

# 2. Prepare Raw Data with USUBJID -------------------------------------
edc_lb_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_lb.xlsx"
if (!file.exists(edc_lb_path)) {
  alt <- list.files("D:/meta_hackathon/sas2r-agent/data/raw", pattern="edc_lb\\.xlsx", recursive=TRUE, full.names=TRUE)
  if (length(alt)>0) edc_lb_path <- alt[1]
}
edc_lb_raw <- readxl::read_excel(edc_lb_path)
names(edc_lb_raw) <- toupper(names(edc_lb_raw))
cat("edc_lb columns:", paste(names(edc_lb_raw), collapse=", "), " n=", nrow(edc_lb_raw), "\n")

lb_prep <- edc_lb_raw %>%
  mutate(
    USUBJID = paste0("ONCO-2026-", trim(as.character(SUBJID)))
  ) %>%
  arrange(USUBJID)

# 3. Map LB Domain ------------------------------------------------------
# merge lb_prep(in=l) dm_for_lb by USUBJID ; if l
# left join
sdtm_lb1 <- lb_prep %>%
  left_join(dm_for_lb, by="USUBJID") %>%
  # if l is implicit (all lb_prep kept)
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "LB",
    LBTESTCD = trim(as.character(TESTCD)),
    LBCAT = trim(as.character(CAT)),
    LBTEST = case_when(
      LBTESTCD == "HGB" ~ "Hemoglobin",
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase",
      TRUE ~ NA_character_
    ),
    LBORRES = trim(as.character(ORRES)),
    LBORRESU = trim(as.character(UNIT)),
    LBSTRESC = trim(as.character(ORRES)),
    LBSTRESN = suppressWarnings(as.numeric(ORRES)),
    LBSTRESU = trim(as.character(UNIT)),
    LBORNRLO = trim(as.character(LOW)),
    LBORNRHI = trim(as.character(HIGH)),
    VISIT = str_replace_all(trim(as.character(VISIT)), "_", " "),
    # Date Conversion: if LBDT ne "" then LBDTC = put(input(LBDT, date11.), yymmdd10.)
    LBDTC = sapply(as.character(LBDT), function(x) {
      if (is_missing(x)) return(NA_character_)
      d <- parse_date11(x)
      fmt_yymmdd10(d)
    }),
    # Study Day
    LBSTDY = {
      # vectorized
      sapply(seq_len(n()), function(i) {
        lbdtc <- LBDTC[i]
        rf <- RFSTDTC[i]
        if (is_missing(lbdtc) || is_missing(rf)) return(NA_real_)
        stdt <- suppressWarnings(as.Date(lbdtc, format="%Y-%m-%d"))
        rfdt <- suppressWarnings(as.Date(rf, format="%Y-%m-%d"))
        if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
        diff <- as.integer(stdt - rfdt)
        if (stdt >= rfdt) diff + 1 else diff
      })
    },
    LBNRIND = {
      sapply(seq_len(n()), function(i) {
        sn <- LBSTRESN[i]
        low <- suppressWarnings(as.numeric(LOW[i]))
        high <- suppressWarnings(as.numeric(HIGH[i]))
        if (is.na(sn)) return(NA_character_)
        if (!is.na(low) && sn < low) return("LOW")
        if (!is.na(high) && sn > high) return("HIGH")
        return("NORMAL")
      })
    }
  )

# Keep needed columns for next step (including LBDTC for sorting)
# SAS keeps LBSTDY numeric, LBDTC char

# 4. Derive LBSEQ -------------------------------------------------------
# proc sort by USUBJID LBDTC
# SAS missing sorts first; R order with na.last=FALSE achieves that
sdtm_lb1_sorted <- sdtm_lb1 %>%
  arrange(USUBJID, LBDTC)  # NAs first by default? In dplyr, NAs last; need to force
# Force NA first for LBDTC
sdtm_lb1_sorted <- sdtm_lb1 %>%
  mutate(.lbdtc_sort = LBDTC) %>%
  arrange(USUBJID, factor(is.na(LBDTC), levels=c(FALSE, TRUE)), LBDTC) %>%
  select(-.lbdtc_sort)

# Actually use base order for SAS-like
ord <- order(sdtm_lb1$USUBJID, sdtm_lb1$LBDTC, na.last=FALSE)
sdtm_lb1_sorted <- sdtm_lb1[ord, ]

sdtm_lb <- sdtm_lb1_sorted %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  transmute(
    STUDYID,
    DOMAIN,
    USUBJID,
    LBSEQ = as.numeric(LBSEQ),
    LBTESTCD,
    LBTEST,
    LBCAT,
    LBORRES,
    LBORRESU,
    LBSTRESC,
    LBSTRESN,
    LBSTRESU,
    LBORNRLO,
    LBORNRHI,
    LBNRIND,
    LBDTC,
    LBSTDY = as.numeric(LBSTDY),
    VISIT
  ) %>%
  # final sort by USUBJID LBDTC already, but ensure
  arrange(USUBJID, LBDTC, LBSEQ)

cat("sdtm_lb n=", nrow(sdtm_lb), " cols=", ncol(sdtm_lb), "\n")
print(head(sdtm_lb, 10))

# 5. View Final Dataset (proc print) -> create listing text ------------
# 6. Export Final Dataset -----------------------------------------------
# Write dataset lb.csv
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")

# Also write xlsx for reference (optional)
try({
  openxlsx::write.xlsx(sdtm_lb, file.path(out, "lb.xlsx"), overwrite=TRUE)
}, silent=TRUE)

# Create listing text rendering (P026.lst)
# SAS proc print prints all rows with columns
# Build text file with header and rows

lst_path_txt <- file.path(out, "P026.lst.txt")
lst_path_txt2 <- file.path(out, "P026.txt")
lst_path_csv <- file.path(out, "P026.lst.csv")
lst_path_csv2 <- file.path(out, "P026.csv")

# Prepare display dataframe for listing (same as sdtm_lb)
# Format numeric without extra decimals, dates as YYYY-MM-DD
display_df <- sdtm_lb

# Create text rendering
con <- file(lst_path_txt, open="wt")
writeLines("Listing P026: SDTM LB Domain (ONCO-2026)", con)
writeLines(paste(rep("-", 120), collapse=""), con)
writeLines(paste0("Study: ONCO-2026  Domain: LB  N=", nrow(display_df)), con)
writeLines("", con)
# header line
hdr <- paste(names(display_df), collapse=" | ")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
for (i in seq_len(nrow(display_df))) {
  row <- display_df[i, ]
  # format each value: NA -> "" ; numeric -> as.character without scientific
  vals <- sapply(row, function(v) {
    if (is.na(v)) return("")
    if (is.numeric(v)) {
      # remove trailing .0 if integer-like
      if (v == floor(v)) format(v, scientific=FALSE, trim=TRUE, nsmall=0)
      else format(v, scientific=FALSE, trim=TRUE)
    } else as.character(v)
  })
  line <- paste(vals, collapse=" | ")
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Total records: ", nrow(display_df)), con)
close(con)

# copy to P026.txt as well
file.copy(lst_path_txt, lst_path_txt2, overwrite=TRUE)

# CSV of table body (same as lb.csv but named for listing)
write.csv(display_df, lst_path_csv, row.names=FALSE, na="")
file.copy(lst_path_csv, lst_path_csv2, overwrite=TRUE)

cat("Outputs written to", out, "\n")
cat(" - lb.csv\n")
cat(" - P026.lst.txt / P026.txt\n")
cat(" - P026.lst.csv / P026.csv\n")
```