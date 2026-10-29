out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_tranwrd <- function(x, from, to) {
  ifelse(is.na(x), NA_character_, gsub(from, to, x, fixed = TRUE))
}
sas_upcase <- function(x) {
  ifelse(is.na(x), NA_character_, toupper(x))
}
catx_dash <- function(a, b) {
  a <- as.character(a); b <- as.character(b)
  a[is.na(a)] <- ""; b[is.na(b)] <- ""
  a <- trimws(a); b <- trimws(b)
  ifelse(a == "" & b == "", "",
  ifelse(a == "", b,
  ifelse(b == "", a, paste(a, b, sep = "-"))))
}
truncate_sas <- function(x, len) {
  ifelse(is.na(x), NA_character_, substr(x, 1, len))
}
parse_date11 <- function(x) {
  x <- as.character(x)
  x_trim <- trimws(x)
  is_empty <- is.na(x_trim) | x_trim == ""
  result <- rep(as.Date(NA), length(x_trim))
  idx <- which(!is_empty)
  if (length(idx) > 0) {
    parsed <- suppressWarnings(lubridate::dmy(x_trim[idx], quiet = TRUE))
    na_idx <- is.na(parsed)
    if (any(na_idx)) {
      x_norm <- gsub("[-/. ]", "", x_trim[idx[na_idx]])
      x_norm_up <- toupper(x_norm)
      parsed2 <- suppressWarnings(lubridate::dmy(x_norm_up, quiet = TRUE))
      parsed[na_idx] <- parsed2
    }
    result[idx] <- parsed
  }
  result
}
format_yymmdd10 <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

raw_dir <- "/bench/data/raw"
rawsdtm_dir <- "/bench/data/raw/cdisc-pilot/sdtm"
faijal_raw <- "/bench/data/raw/faijal-oncology/raw_data"

find_sdtm_dm <- function() {
  candidates <- c(
    file.path(rawsdtm_dir, "dm.xpt"),
    file.path(rawsdtm_dir, "DM.xpt"),
    file.path(rawsdtm_dir, "dm.sas7bdat"),
    file.path(rawsdtm_dir, "dm.xlsx")
  )
  for (f in candidates) if (file.exists(f)) return(f)
  if (dir.exists(rawsdtm_dir)) {
    fl <- list.files(rawsdtm_dir, full.names = TRUE, ignore.case = TRUE)
    dmf <- fl[grepl("dm", basename(fl), ignore.case = TRUE)]
    if (length(dmf) > 0) return(dmf[1])
  }
  return(NA_character_)
}
sdtm_dm_path <- find_sdtm_dm()

edc_ds_path <- file.path(faijal_raw, "edc_ds.xlsx")
if (!file.exists(edc_ds_path)) edc_ds_path <- file.path(raw_dir, "faijal-oncology", "raw_data", "edc_ds.xlsx")
edc_ds <- readxl::read_excel(edc_ds_path, .name_repair = "minimal")
names(edc_ds) <- trimws(names(edc_ds))

edc_dm_path <- file.path(faijal_raw, "edc_dm.xlsx")
if (!file.exists(edc_dm_path)) edc_dm_path <- file.path(raw_dir, "faijal-oncology", "raw_data", "edc_dm.xlsx")
edc_dm <- NULL
if (file.exists(edc_dm_path)) {
  edc_dm <- readxl::read_excel(edc_dm_path, .name_repair = "minimal")
  names(edc_dm) <- trimws(names(edc_dm))
}

sdtm_dm <- NULL
if (!is.na(sdtm_dm_path) && file.exists(sdtm_dm_path)) {
  ext <- tolower(tools::file_ext(sdtm_dm_path))
  if (ext == "xpt") {
    sdtm_dm <- haven::read_xpt(sdtm_dm_path)
  } else if (ext == "sas7bdat") {
    sdtm_dm <- haven::read_sas(sdtm_dm_path)
  } else if (ext %in% c("xlsx", "xls")) {
    sdtm_dm <- readxl::read_excel(sdtm_dm_path, .name_repair = "minimal")
  }
  names(sdtm_dm) <- toupper(names(sdtm_dm))
} else {
  edc_death_path <- file.path(faijal_raw, "edc_death.xlsx")
  if (!file.exists(edc_death_path)) edc_death_path <- file.path(raw_dir, "faijal-oncology", "raw_data", "edc_death.xlsx")
  edc_death <- NULL
  if (file.exists(edc_death_path)) {
    edc_death <- readxl::read_excel(edc_death_path, .name_repair = "minimal")
    names(edc_death) <- trimws(names(edc_death))
  }
  if (!is.null(edc_dm)) {
    names(edc_dm) <- toupper(names(edc_dm))
    if (!"USUBJID" %in% names(edc_dm)) {
      if ("SUBJID" %in% names(edc_dm)) {
        edc_dm$USUBJID <- catx_dash("ONCO-2026", as.character(edc_dm$SUBJID))
      }
    }
    if (!"RFSTDTC" %in% names(edc_dm)) {
      rf_col <- grep("RFST", names(edc_dm), value = TRUE)
      if (length(rf_col) > 0) {
        edc_dm$RFSTDTC <- edc_dm[[rf_col[1]]]
      } else {
        edc_dm$RFSTDTC <- NA_character_
      }
    }
    if (!"DTHDTC" %in% names(edc_dm)) {
      if (!is.null(edc_death)) {
        names(edc_death) <- toupper(names(edc_death))
        dth_col <- grep("DTH|DEATH|DTHDTC", names(edc_death), value = TRUE)
        if (length(dth_col) == 0) {
          dth_col <- grep("DT|DATE", names(edc_death), value = TRUE)
        }
        if (length(dth_col) > 0 && "SUBJID" %in% names(edc_death)) {
          death_map <- edc_death[, c("SUBJID", dth_col[1]), drop = FALSE]
          names(death_map)[2] <- "DTHDTC_TMP"
          if (inherits(death_map$DTHDTC_TMP, "Date") || inherits(death_map$DTHDTC_TMP, "POSIXt")) {
            death_map$DTHDTC_TMP <- format(as.Date(death_map$DTHDTC_TMP), "%Y-%m-%d")
          } else {
            death_map$DTHDTC_TMP <- as.character(death_map$DTHDTC_TMP)
          }
          edc_dm <- merge(edc_dm, death_map, by = "SUBJID", all.x = TRUE)
          edc_dm$DTHDTC <- edc_dm$DTHDTC_TMP
          edc_dm$DTHDTC_TMP <- NULL
        } else {
          edc_dm$DTHDTC <- NA_character_
        }
      } else {
        edc_dm$DTHDTC <- NA_character_
      }
    }
    for (col in c("RFSTDTC", "DTHDTC")) {
      if (col %in% names(edc_dm)) {
        if (inherits(edc_dm[[col]], "Date") || inherits(edc_dm[[col]], "POSIXt")) {
          edc_dm[[col]] <- format(as.Date(edc_dm[[col]]), "%Y-%m-%d")
        } else if (is.numeric(edc_dm[[col]])) {
          edc_dm[[col]] <- format(as.Date(edc_dm[[col]], origin = "1899-12-30"), "%Y-%m-%d")
        } else {
          edc_dm[[col]] <- as.character(edc_dm[[col]])
        }
        edc_dm[[col]][edc_dm[[col]] == ""] <- NA_character_
      }
    }
    sdtm_dm <- edc_dm
  }
  if (is.null(sdtm_dm)) {
    sdtm_dm <- data.frame(USUBJID = character(0), SUBJID = character(0),
                          RFSTDTC = character(0), DTHDTC = character(0),
                          stringsAsFactors = FALSE)
  } else {
    for (c in c("USUBJID", "SUBJID", "RFSTDTC", "DTHDTC")) {
      if (!c %in% names(sdtm_dm)) sdtm_dm[[c]] <- NA_character_
    }
    sdtm_dm <- sdtm_dm[, c("USUBJID", "SUBJID", "RFSTDTC", "DTHDTC"), drop = FALSE]
  }
}

for (c in c("USUBJID", "SUBJID", "RFSTDTC", "DTHDTC")) {
  if (c %in% names(sdtm_dm)) sdtm_dm[[c]] <- as.character(sdtm_dm[[c]])
}

dm_for_ds <- sdtm_dm[, c("USUBJID", "SUBJID", "RFSTDTC", "DTHDTC"), drop = FALSE]
dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last = FALSE), , drop = FALSE]

names(edc_ds) <- toupper(names(edc_ds))
for (col in c("SUBJID", "DS_TERM", "DS_CAT", "DS_DT")) {
  if (!col %in% names(edc_ds)) edc_ds[[col]] <- NA_character_
}
edc_ds$SUBJID <- as.character(edc_ds$SUBJID)

convert_to_stdt <- function(x) {
  if (inherits(x, "Date")) {
    return(toupper(format(x, "%d%b%Y")))
  } else if (inherits(x, "POSIXt")) {
    return(toupper(format(as.Date(x), "%d%b%Y")))
  } else if (is.numeric(x)) {
    d <- as.Date(x, origin = "1899-12-30")
    return(toupper(format(d, "%d%b%Y")))
  } else {
    x <- as.character(x)
    x[is.na(x) | trimws(x) == ""] <- NA_character_
    return(x)
  }
}
edc_ds$STDT_raw <- convert_to_stdt(edc_ds$DS_DT)

edc_ds_prep <- data.frame(
  SUBJID = edc_ds$SUBJID,
  TERM = as.character(edc_ds$DS_TERM),
  DECOD = as.character(edc_ds$DS_TERM),
  CAT = as.character(edc_ds$DS_CAT),
  STDT = edc_ds$STDT_raw,
  stringsAsFactors = FALSE
)
edc_ds_prep$USUBJID <- catx_dash("ONCO-2026", edc_ds_prep$SUBJID)
edc_ds_prep$TERM <- truncate_sas(edc_ds_prep$TERM, 50)
edc_ds_prep$DECOD <- truncate_sas(edc_ds_prep$DECOD, 50)
edc_ds_prep$CAT <- truncate_sas(edc_ds_prep$CAT, 50)
edc_ds_prep$STDT <- truncate_sas(edc_ds_prep$STDT, 11)
edc_ds_prep$USUBJID <- truncate_sas(edc_ds_prep$USUBJID, 25)

edc_ds_prep <- edc_ds_prep[order(edc_ds_prep$USUBJID, na.last = FALSE), , drop = FALSE]
dm_for_ds <- dm_for_ds[order(dm_for_ds$USUBJID, na.last = FALSE), , drop = FALSE]

dm_for_ds_unique <- dm_for_ds[!duplicated(dm_for_ds$USUBJID), , drop = FALSE]
sdtm_ds1 <- merge(edc_ds_prep, dm_for_ds_unique, by = "USUBJID", all.x = TRUE, sort = FALSE)

sdtm_ds1$STUDYID <- "ONCO-2026"
sdtm_ds1$DOMAIN <- "DS"
sdtm_ds1$DSTERM <- truncate_sas(sas_tranwrd(sdtm_ds1$TERM, "_", " "), 50)
sdtm_ds1$DSDECOD <- truncate_sas(sas_upcase(sdtm_ds1$DECOD), 30)
sdtm_ds1$DSCAT <- truncate_sas(sas_tranwrd(sdtm_ds1$CAT, "_", " "), 30)
sdtm_ds1$DTHFL <- ifelse(!is.na(sdtm_ds1$DTHDTC) & trimws(sdtm_ds1$DTHDTC) != "", "Y", NA_character_)
sdtm_ds1$DTHFL <- truncate_sas(sdtm_ds1$DTHFL, 1)

sdtm_ds1$DSSTDTC <- rep(NA_character_, nrow(sdtm_ds1))
nonblank_stdt <- !is.na(sdtm_ds1$STDT) & trimws(sdtm_ds1$STDT) != ""
if (any(nonblank_stdt)) {
  parsed_stdt <- parse_date11(sdtm_ds1$STDT[nonblank_stdt])
  na_parsed <- is.na(parsed_stdt)
  if (any(na_parsed)) {
    ymd_try <- suppressWarnings(lubridate::ymd(sdtm_ds1$STDT[nonblank_stdt][na_parsed], quiet = TRUE))
    parsed_stdt[na_parsed] <- ymd_try
  }
  na_parsed2 <- is.na(parsed_stdt)
  if (any(na_parsed2)) {
    mdy_try <- suppressWarnings(lubridate::mdy(sdtm_ds1$STDT[nonblank_stdt][na_parsed2], quiet = TRUE))
    parsed_stdt[na_parsed2] <- mdy_try
  }
  sdtm_ds1$DSSTDTC[nonblank_stdt] <- format_yymmdd10(parsed_stdt)
  sdtm_ds1$DSSTDTC <- truncate_sas(sdtm_ds1$DSSTDTC, 19)
}

sdtm_ds1$DSSTDY <- rep(NA_real_, nrow(sdtm_ds1))
has_both <- !is.na(sdtm_ds1$DSSTDTC) & trimws(sdtm_ds1$DSSTDTC) != "" &
            !is.na(sdtm_ds1$RFSTDTC) & trimws(sdtm_ds1$RFSTDTC) != ""
if (any(has_both)) {
  stdt_date <- suppressWarnings(lubridate::ymd(sdtm_ds1$DSSTDTC[has_both], quiet = TRUE))
  rfdt_date <- suppressWarnings(lubridate::ymd(sdtm_ds1$RFSTDTC[has_both], quiet = TRUE))
  na_rfdt <- is.na(rfdt_date)
  if (any(na_rfdt)) {
    rf_raw <- sdtm_ds1$RFSTDTC[has_both][na_rfdt]
    rf_raw_date <- substr(rf_raw, 1, 10)
    rfdt_date[na_rfdt] <- suppressWarnings(lubridate::ymd(rf_raw_date, quiet = TRUE))
  }
  diff_days <- as.numeric(stdt_date - rfdt_date)
  dsstdy_calc <- ifelse(stdt_date >= rfdt_date, diff_days + 1, diff_days)
  sdtm_ds1$DSSTDY[has_both] <- dsstdy_calc
}

warn_idx <- which(sdtm_ds1$USUBJID == "ONCO-2026-103" &
                  !is.na(sdtm_ds1$DSSTDTC) & !is.na(sdtm_ds1$DTHDTC) &
                  sdtm_ds1$DSSTDTC != sdtm_ds1$DTHDTC)
if (length(warn_idx) > 0) {
  for (i in warn_idx) {
    message(paste0("WARNING: Date mismatch! DS=", sdtm_ds1$DSSTDTC[i], " DM=", sdtm_ds1$DTHDTC[i]))
  }
}

sdtm_ds1 <- sdtm_ds1[order(sdtm_ds1$USUBJID, sdtm_ds1$DSSTDTC, na.last = FALSE), , drop = FALSE]

sdtm_ds1$DSSEQ <- NA_real_
unique_usubjid <- unique(sdtm_ds1$USUBJID)
for (uid in unique_usubjid) {
  idx <- which(sdtm_ds1$USUBJID == uid)
  sdtm_ds1$DSSEQ[idx] <- seq_along(idx)
}

sdtm_ds <- sdtm_ds1[, c("STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "DSSTDTC", "DSSTDY", "DTHFL"), drop = FALSE]
sdtm_ds$STUDYID <- truncate_sas(sdtm_ds$STUDYID, 10)
sdtm_ds$DOMAIN <- truncate_sas(sdtm_ds$DOMAIN, 2)
sdtm_ds$USUBJID <- truncate_sas(sdtm_ds$USUBJID, 25)
sdtm_ds$DSTERM <- truncate_sas(sdtm_ds$DSTERM, 50)
sdtm_ds$DSDECOD <- truncate_sas(sdtm_ds$DSDECOD, 30)
sdtm_ds$DSCAT <- truncate_sas(sdtm_ds$DSCAT, 30)
sdtm_ds$DSSTDTC <- truncate_sas(sdtm_ds$DSSTDTC, 19)
sdtm_ds$DTHFL <- truncate_sas(sdtm_ds$DTHFL, 1)
sdtm_ds$DSSEQ <- as.numeric(sdtm_ds$DSSEQ)
sdtm_ds$DSSTDY <- as.numeric(sdtm_ds$DSSTDY)
sdtm_ds <- sdtm_ds[order(sdtm_ds$USUBJID, sdtm_ds$DSSTDTC, na.last = FALSE), , drop = FALSE]

write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

lst_path <- file.path(out, "P024.lst")
csv_path <- file.path(out, "P024.csv")

display_ds <- sdtm_ds
display_ds_txt <- display_ds
for (col in names(display_ds_txt)) {
  display_ds_txt[[col]][is.na(display_ds_txt[[col]])] <- ""
}

con <- file(lst_path, open = "wt")
writeLines("SDTM DS Domain - Disposition", con)
writeLines(paste0("Study: ONCO-2026  Domain: DS  Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")), con)
writeLines("", con)
writeLines(paste0("Number of observations: ", nrow(sdtm_ds)), con)
writeLines(paste0("Number of subjects: ", length(unique(sdtm_ds$USUBJID))), con)
writeLines("", con)
header <- sprintf("%-4s %-10s %-4s %-15s %-6s %-30s %-30s %-20s %-12s %-7s %-5s",
                  "Obs", "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD", "DSCAT", "DSSTDTC", "DSSTDY", "DTHFL")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse = ""), con)
for (i in seq_len(nrow(display_ds_txt))) {
  row <- display_ds_txt[i, ]
  line <- sprintf("%-4d %-10s %-4s %-15s %-6s %-30s %-30s %-20s %-12s %-7s %-5s",
                  i,
                  substr(as.character(row$STUDYID), 1, 10),
                  substr(as.character(row$DOMAIN), 1, 4),
                  substr(as.character(row$USUBJID), 1, 15),
                  as.character(row$DSSEQ),
                  substr(as.character(row$DSTERM), 1, 30),
                  substr(as.character(row$DSDECOD), 1, 30),
                  substr(as.character(row$DSCAT), 1, 20),
                  substr(as.character(row$DSSTDTC), 1, 12),
                  as.character(row$DSSTDY),
                  substr(as.character(row$DTHFL), 1, 5))
  writeLines(line, con)
}
writeLines("", con)
writeLines("Summary by DSCAT:", con)
if (nrow(sdtm_ds) > 0) {
  tab_cat <- as.data.frame(table(sdtm_ds$DSCAT, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tab_cat) <- c("DSCAT", "Count")
  for (j in seq_len(nrow(tab_cat))) {
    writeLines(sprintf("  %s: %d (%.1f%%)", tab_cat$DSCAT[j], tab_cat$Count[j], 100*tab_cat$Count[j]/nrow(sdtm_ds)), con)
  }
  writeLines("", con)
  writeLines("Summary by DSTERM:", con)
  tab_term <- as.data.frame(table(sdtm_ds$DSTERM, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tab_term) <- c("DSTERM", "Count")
  for (j in seq_len(nrow(tab_term))) {
    writeLines(sprintf("  %s: %d (%.1f%%)", tab_term$DSTERM[j], tab_term$Count[j], 100*tab_term$Count[j]/nrow(sdtm_ds)), con)
  }
  writeLines("", con)
  writeLines("Summary by DTHFL:", con)
  tab_dth <- as.data.frame(table(sdtm_ds$DTHFL, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tab_dth) <- c("DTHFL", "Count")
  for (j in seq_len(nrow(tab_dth))) {
    writeLines(sprintf("  %s: %d (%.1f%%)", ifelse(is.na(tab_dth$DTHFL[j]), "Missing", tab_dth$DTHFL[j]), tab_dth$Count[j], 100*tab_dth$Count[j]/nrow(sdtm_ds)), con)
  }
  writeLines("", con)
  writeLines("DSSTDY Descriptive Statistics:", con)
  dsstdy_vals <- sdtm_ds$DSSTDY[!is.na(sdtm_ds$DSSTDY)]
  if (length(dsstdy_vals) > 0) {
    writeLines(sprintf("  N: %d", length(dsstdy_vals)), con)
    writeLines(sprintf("  Mean: %.2f", mean(dsstdy_vals)), con)
    writeLines(sprintf("  SD: %.2f", sd(dsstdy_vals)), con)
    writeLines(sprintf("  Min: %.0f", min(dsstdy_vals)), con)
    writeLines(sprintf("  Median: %.1f", median(dsstdy_vals)), con)
    writeLines(sprintf("  Max: %.0f", max(dsstdy_vals)), con)
    writeLines(sprintf("  Missing: %d", sum(is.na(sdtm_ds$DSSTDY))), con)
  } else {
    writeLines("  N: 0", con)
  }
}
close(con)

write.csv(display_ds, csv_path, row.names = FALSE, na = "")
file.copy(lst_path, file.path(out, "P024.txt"), overwrite = TRUE)

message(paste0("P024: ds.csv written with ", nrow(sdtm_ds), " rows to ", file.path(out, "ds.csv")))
message(paste0("P024: listing written to ", lst_path))
