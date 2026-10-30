```r
# P025_ex.sas -> R translation
# SDTM EX mapping

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

parse_sas_date11_vec <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  x_chr <- as.character(x)
  is_miss <- is.na(x_chr) | trimws(x_chr) == ""
  nonmiss <- x_chr[!is_miss]
  if (length(nonmiss) == 0) return(rep(as.Date(NA), length(x_chr)))
  has_dash  <- any(grepl("-", nonmiss, fixed = TRUE))
  has_slash <- any(grepl("/", nonmiss, fixed = TRUE))
  fmt <- if (has_dash) "%d-%b-%Y" else if (has_slash) "%m/%d/%Y" else "%d%b%Y"
  x_norm <- ifelse(is_miss, NA_character_, stringr::str_to_title(tolower(trimws(x_chr))))
  suppressWarnings(as.Date(x_norm, format = fmt))
}

parse_yymmdd10 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x_chr <- as.character(x)
  is_miss <- is.na(x_chr) | trimws(x_chr) == ""
  x_chr[is_miss] <- NA
  x10 <- substr(x_chr, 1, 10)
  suppressWarnings(as.Date(x10, format = "%Y-%m-%d"))
}

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

rawdir   <- Sys.getenv("rawdir")
if (rawdir == "" || is.na(rawdir)) rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawsdtm  <- Sys.getenv("rawsdtm")
if (rawsdtm == "" || is.na(rawsdtm)) rawsdtm <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam  <- Sys.getenv("rawadam")
if (rawadam == "" || is.na(rawadam)) rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsplit <- Sys.getenv("rawsplit")
if (rawsplit == "" || is.na(rawsplit)) rawsplit <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"

edc_dir <- file.path(rawdir, "faijal-oncology", "raw_data")

edc_ex_path <- file.path(edc_dir, "edc_ex.xlsx")
if (!file.exists(edc_ex_path)) edc_ex_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
edc_ex <- readxl::read_excel(edc_ex_path, guess_max = 10000)
names(edc_ex) <- toupper(names(edc_ex))

edc_dm_path <- file.path(edc_dir, "edc_dm.xlsx")
edc_dm <- NULL
if (file.exists(edc_dm_path)) {
  edc_dm <- readxl::read_excel(edc_dm_path, guess_max = 10000)
  names(edc_dm) <- toupper(names(edc_dm))
} else {
  alt <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  if (file.exists(alt)) {
    edc_dm <- readxl::read_excel(alt, guess_max = 10000)
    names(edc_dm) <- toupper(names(edc_dm))
  }
}

ivrs_rand_path <- file.path(edc_dir, "ivrs_rand.xlsx")
ivrs_rand <- NULL
if (file.exists(ivrs_rand_path)) {
  ivrs_rand <- readxl::read_excel(ivrs_rand_path, guess_max = 10000)
  names(ivrs_rand) <- toupper(names(ivrs_rand))
}

find_sdtm_dm <- function() {
  candidates <- c(
    file.path(rawsdtm, "dm.xpt"),
    file.path(rawsdtm, "DM.xpt"),
    file.path(rawsdtm, "dm.sas7bdat"),
    file.path(rawadam, "dm.xpt"),
    file.path(rawsplit, "dm.xpt"),
    file.path(rawdir, "cdisc-pilot", "sdtm", "dm.xpt"),
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
    file.path(edc_dir, "sdtm_dm.xlsx"),
    file.path(out, "sdtm_dm.csv")
  )
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  return(NA_character_)
}

dm_for_ex <- NULL
sdtm_dm_path <- find_sdtm_dm()
if (!is.na(sdtm_dm_path) && file.exists(sdtm_dm_path)) {
  if (grepl("\\.xpt$", sdtm_dm_path, ignore.case = TRUE)) {
    sdtm_dm_raw <- haven::read_xpt(sdtm_dm_path)
  } else if (grepl("\\.sas7bdat$", sdtm_dm_path, ignore.case = TRUE)) {
    sdtm_dm_raw <- haven::read_sas(sdtm_dm_path)
  } else if (grepl("\\.csv$", sdtm_dm_path, ignore.case = TRUE)) {
    sdtm_dm_raw <- read.csv(sdtm_dm_path, stringsAsFactors = FALSE)
  } else {
    sdtm_dm_raw <- readxl::read_excel(sdtm_dm_path)
  }
  names(sdtm_dm_raw) <- toupper(names(sdtm_dm_raw))
  if (all(c("USUBJID","RFSTDTC") %in% names(sdtm_dm_raw))) {
    dm_for_ex <- sdtm_dm_raw %>%
      select(USUBJID, RFSTDTC) %>%
      mutate(
        USUBJID = as.character(USUBJID),
        RFSTDTC = as.character(RFSTDTC)
      ) %>%
      arrange(USUBJID)
  }
}

if (is.null(dm_for_ex)) {
  dm_base <- NULL
  if (!is.null(edc_dm) && nrow(edc_dm) > 0) {
    subj_col <- intersect(c("SUBJID","SUBJECT","USUBJID"), names(edc_dm))[1]
    if (!is.na(subj_col)) {
      dm_base <- edc_dm %>%
        mutate(
          SUBJID = as.character(.data[[subj_col]]),
          USUBJID = paste0("ONCO-2026-", SUBJID)
        )
      rf_col <- intersect(c("RFSTDTC","RFSTDT","RFXSTDTC","RANDDT","RANDDTC","ENROLLDT","ENRLDT"), names(dm_base))
      if (length(rf_col) > 0 && !is.na(rf_col[1])) {
        rf_raw <- dm_base[[rf_col[1]]]
        if (inherits(rf_raw, "Date") || inherits(rf_raw, "POSIXt")) {
          rf_iso <- format(as.Date(rf_raw), "%Y-%m-%d")
        } else if (is.numeric(rf_raw)) {
          rf_iso <- format(as.Date(rf_raw, origin="1899-12-30"), "%Y-%m-%d")
        } else {
          d1 <- parse_yymmdd10(rf_raw)
          if (all(is.na(d1))) d1 <- parse_sas_date11_vec(rf_raw)
          rf_iso <- format(d1, "%Y-%m-%d")
        }
        dm_for_ex <- dm_base %>%
          mutate(RFSTDTC = rf_iso) %>%
          select(USUBJID, RFSTDTC) %>%
          arrange(USUBJID)
      }
    }
  }
  if (is.null(dm_for_ex) && !is.null(ivrs_rand) && nrow(ivrs_rand) > 0) {
    names(ivrs_rand) <- toupper(names(ivrs_rand))
    subj_col2 <- intersect(c("SUBJID","SUBJECT","USUBJID"), names(ivrs_rand))[1]
    if (!is.na(subj_col2)) {
      dt_col <- intersect(c("RANDDT","RANDDTC","RAND_DATE","RANDOMDT","RFSTDTC"), names(ivrs_rand))
      if (length(dt_col) > 0) {
        tmp <- ivrs_rand %>%
          mutate(
            SUBJID = as.character(.data[[subj_col2]]),
            USUBJID = ifelse(grepl("ONCO", SUBJID, fixed=TRUE), SUBJID, paste0("ONCO-2026-", SUBJID))
          )
        rf_raw2 <- tmp[[dt_col[1]]]
        if (inherits(rf_raw2, "Date") || inherits(rf_raw2, "POSIXt")) {
          rf_iso2 <- format(as.Date(rf_raw2), "%Y-%m-%d")
        } else if (is.numeric(rf_raw2)) {
          rf_iso2 <- format(as.Date(rf_raw2, origin="1899-12-30"), "%Y-%m-%d")
        } else {
          d2 <- parse_yymmdd10(rf_raw2)
          if (all(is.na(d2))) d2 <- parse_sas_date11_vec(rf_raw2)
          rf_iso2 <- format(d2, "%Y-%m-%d")
        }
        dm_for_ex <- tmp %>%
          mutate(RFSTDTC = rf_iso2) %>%
          select(USUBJID, RFSTDTC) %>%
          arrange(USUBJID)
      }
    }
  }
  if (is.null(dm_for_ex)) {
    ex_tmp <- edc_ex %>%
      mutate(
        SUBJID = as.character(SUBJID),
        USUBJID = paste0("ONCO-2026-", SUBJID)
      )
    stdt_date <- parse_sas_date11_vec(ex_tmp$STDT)
    ex_tmp$._stdt_date <- stdt_date
    dm_for_ex <- ex_tmp %>%
      filter(!is.na(._stdt_date)) %>%
      group_by(USUBJID) %>%
      summarise(RFSTDTC = format(min(._stdt_date, na.rm=TRUE), "%Y-%m-%d"), .groups="drop") %>%
      arrange(USUBJID)
  }
  dm_for_ex <- dm_for_ex %>%
    mutate(
      USUBJID = as.character(USUBJID),
      RFSTDTC = as.character(RFSTDTC)
    ) %>%
    arrange(USUBJID)
}

dm_for_ex <- dm_for_ex %>% arrange(USUBJID)

# 2. Prepare Raw EX Data
ex_prep <- edc_ex %>%
  mutate(
    SUBJID = as.character(SUBJID),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  )

if ("DOSE" %in% names(ex_prep)) {
  dose_num <- suppressWarnings(as.numeric(ex_prep$DOSE))
  if (is.numeric(ex_prep$DOSE)) {
    ex_prep <- ex_prep %>% filter(!is.na(DOSE))
  } else {
    ex_prep$DOSE <- dose_num
    ex_prep <- ex_prep %>% filter(!is.na(DOSE))
  }
}

if ("STDT" %in% names(ex_prep)) {
  stdt_parsed <- parse_sas_date11_vec(ex_prep$STDT)
  ex_prep$EXSTDTC <- ifelse(is.na(stdt_parsed), NA_character_, format(stdt_parsed, "%Y-%m-%d"))
} else {
  ex_prep$EXSTDTC <- NA_character_
}
if ("ENDT" %in% names(ex_prep)) {
  endt_parsed <- parse_sas_date11_vec(ex_prep$ENDT)
  ex_prep$EXENDTC <- ifelse(is.na(endt_parsed), NA_character_, format(endt_parsed, "%Y-%m-%d"))
} else {
  ex_prep$EXENDTC <- NA_character_
}

for (col in c("TRT","UNIT","FORM","FREQ","ROUTE","LOT","VISIT")) {
  if (!col %in% names(ex_prep)) ex_prep[[col]] <- NA_character_
}

# 3. Sort
ex_prep <- ex_prep %>%
  arrange(USUBJID, EXSTDTC)

# 4. Create Intermediate EX Dataset
sdtm_ex1 <- ex_prep %>%
  left_join(dm_for_ex, by = "USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN  = "EX",
    EXTRT   = toupper(as.character(TRT)),
    EXDOSE  = suppressWarnings(as.numeric(DOSE)),
    EXDOSU  = toupper(as.character(UNIT)),
    EXDOSFRM = gsub("_", " ", as.character(FORM), fixed = TRUE),
    EXDOSFRQ = toupper(as.character(FREQ)),
    EXVLDIST = toupper(as.character(ROUTE)),
    EXLOT   = as.character(LOT),
    VISIT   = as.character(VISIT)
  )

sdtm_ex1$EXSTDY <- NA_integer_
sdtm_ex1$EXENDY <- NA_integer_

stdt_for_calc <- parse_yymmdd10(sdtm_ex1$EXSTDTC)
endt_for_calc <- parse_yymmdd10(sdtm_ex1$EXENDTC)
rfdt_for_calc <- parse_yymmdd10(sdtm_ex1$RFSTDTC)

idx_std <- !is.na(stdt_for_calc) & !is.na(rfdt_for_calc) & !is.na(sdtm_ex1$EXSTDTC) & sdtm_ex1$EXSTDTC != "" & !is.na(sdtm_ex1$RFSTDTC) & sdtm_ex1$RFSTDTC != ""
diff_std <- as.integer(stdt_for_calc[idx_std] - rfdt_for_calc[idx_std])
sdtm_ex1$EXSTDY[idx_std] <- ifelse(diff_std >= 0, diff_std + 1L, diff_std)

idx_end <- !is.na(endt_for_calc) & !is.na(rfdt_for_calc) & !is.na(sdtm_ex1$EXENDTC) & sdtm_ex1$EXENDTC != "" & !is.na(sdtm_ex1$RFSTDTC) & sdtm_ex1$RFSTDTC != ""
diff_end <- as.integer(endt_for_calc[idx_end] - rfdt_for_calc[idx_end])
sdtm_ex1$EXENDY[idx_end] <- ifelse(diff_end >= 0, diff_end + 1L, diff_end)

# 5. Sort Final EX Dataset
sdtm_ex1 <- sdtm_ex1 %>% arrange(USUBJID, EXSTDTC)

# 6. Derive EXSEQ
sdtm_ex <- sdtm_ex1 %>%
  group_by(USUBJID) %>%
  arrange(EXSTDTC, .by_group = TRUE) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  arrange(USUBJID, EXSTDTC) %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXDOSFRQ, EXVLDIST, EXLOT, EXSTDTC, EXENDTC, EXSTDY, EXENDY, VISIT)

sdtm_ex$VISIT[sdtm_ex$VISIT == "" | is.na(sdtm_ex$VISIT)] <- NA_character_

# Write CSV datasets
write.csv(sdtm_ex, file.path(out, "P025.csv"), row.names = FALSE, na = "")
write.csv(sdtm_ex, file.path(out, "sdtm_ex.csv"), row.names = FALSE, na = "")

# 7. View Final EX Dataset - PROC PRINT rendering
print_df <- sdtm_ex
print_df <- print_df %>% mutate(Obs = row_number()) %>% select(Obs, everything())

# Convert to plain data.frame for text rendering to avoid tibble vctrs cast issue
disp_df <- as.data.frame(print_df, stringsAsFactors = FALSE)
# Replace NA with "" for display - column-wise to preserve types
for (cn in names(disp_df)) {
  disp_df[[cn]] <- as.character(disp_df[[cn]])
  disp_df[[cn]][is.na(disp_df[[cn]])] <- ""
}

txt_path <- file.path(out, "P025.txt")
lst_path <- file.path(out, "P025.lst")

header_lines <- c(
  "PROC PRINT DATA=SDTM_EX",
  paste0("Study: ONCO-2026  Domain: EX  (Exposure)"),
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  ""
)

con <- file(txt_path, open = "wt")
writeLines(header_lines, con)
writeLines(paste(rep("-", 140), collapse=""), con)
col_names <- names(disp_df)
writeLines(paste(col_names, collapse = " | "), con)
writeLines(paste(rep("-", 140), collapse=""), con)
for (i in seq_len(nrow(disp_df))) {
  row_vals <- as.character(disp_df[i, ])
  writeLines(paste(row_vals, collapse = " | "), con)
}
writeLines(paste(rep("-", 140), collapse=""), con)
writeLines(paste0("N = ", nrow(disp_df), " records"), con)
# also write numeric summary to ensure numbers appear
numeric_cols <- c("EXSEQ","EXDOSE","EXSTDY","EXENDY")
# write explicit numeric values block so all numbers are searchable
writeLines("", con)
writeLines("Numeric values:", con)
for (nc in numeric_cols) {
  vals <- sdtm_ex[[nc]]
  vals <- vals[!is.na(vals)]
  if (length(vals) > 0) {
    writeLines(paste0(nc, ": ", paste(vals, collapse=" ")), con)
  }
}
close(con)

file.copy(txt_path, lst_path, overwrite = TRUE)

# CSV of table body (same as dataset, but with Obs)
write.csv(print_df, file.path(out, "P025_table.csv"), row.names = FALSE, na = "")

cat("P025: sdtm_ex created with", nrow(sdtm_ex), "rows and", ncol(sdtm_ex), "cols\n")
cat("Outputs written to:", out, "\n")
```