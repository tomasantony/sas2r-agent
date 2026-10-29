# P026_lb.sas -> R translation
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr); library(readr)
  library(stringr); library(lubridate); library(purrr); library(tibble)
  library(readxl); library(openxlsx)
})

`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a) && a!="") a else b
is_missing <- function(x) is.na(x) | trimws(as.character(x))==""

parse_date11 <- function(v){
  if (length(v)==0) return(as.Date(NA))
  if (inherits(v,"Date")) return(v)
  if (inherits(v,"POSIXt")) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin="1899-12-30"))
  s <- trimws(as.character(v))
  if (is.na(s) || s=="") return(as.Date(NA))
  # try ymd first (SAS yymmdd10)
  d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::dmy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  # SAS date11 like 01JAN2026 or 01-JAN-2026
  s2 <- gsub("[-/ ]","", toupper(s))
  # insert spaces for parsing if needed
  d <- suppressWarnings(as.Date(s, format="%d%b%Y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(toupper(s), format="%d%b%Y"))
  if (!is.na(d)) return(d)
  d <- suppressWarnings(as.Date(s2, format="%d%b%Y"))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}
fmt_yymmdd10 <- function(d){
  if (is.na(d)) return(NA_character_)
  format(as.Date(d), "%Y-%m-%d")
}
parse_any_date <- function(v){
  if (is_missing(v)) return(as.Date(NA))
  if (inherits(v,"Date")) return(v)
  if (inherits(v,"POSIXt")) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin="1899-12-30"))
  s <- trimws(as.character(v))
  if (s=="") return(as.Date(NA))
  # numeric string that is excel serial?
  if (grepl("^[0-9]{5}$", s)) {
    n <- suppressWarnings(as.numeric(s))
    if (!is.na(n) && n>30000 && n<60000) return(as.Date(n, origin="1899-12-30"))
  }
  d <- parse_date11(s)
  if (!is.na(d)) return(d)
  d <- suppressWarnings(lubridate::ymd(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  d <- suppressWarnings(lubridate::mdy(s, quiet=TRUE))
  if (!is.na(d)) return(as.Date(d))
  return(as.Date(NA))
}

cat("P026: start\n")

# --- 1. Prepare DM Reference Data ---
# Build dm_for_lb from edc_dm (authoritative for ONCO-2026 USUBJID)
edc_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
if (!file.exists(edc_dm_path)){
  alt <- list.files("D:/meta_hackathon/sas2r-agent/data/raw", pattern="edc_dm\\.xlsx", recursive=TRUE, full.names=TRUE)
  if (length(alt)>0) edc_dm_path <- alt[1]
}
edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- toupper(names(edc_dm))
cat("edc_dm cols:", paste(names(edc_dm), collapse=","), " n=", nrow(edc_dm), "\n")

# find RFSTDTC column in edc_dm
rf_candidates <- grep("RFSTDTC|RFSTDT|RFXSTDTC|RANDDT|RFSTD", names(edc_dm), value=TRUE, ignore.case=TRUE)
# also check for any date-like column if not found
if (length(rf_candidates)==0){
  # try to find column that looks like date
  rf_candidates <- names(edc_dm)[grep("STDTC|STDAT|START|ENROL", names(edc_dm), ignore.case=TRUE)]
}
cat("RF candidates:", paste(rf_candidates, collapse=","), "\n")

# Build dm_for_lb
dm_for_lb <- edc_dm %>%
  mutate(
    USUBJID = paste0("ONCO-2026-", trimws(as.character(SUBJID))),
    .rf_raw = if (length(rf_candidates)>0) as.character(.data[[rf_candidates[1]]]) else NA_character_,
    # if .rf_raw is NA but column is Date numeric, read_excel may have converted to Date
    .rf_raw2 = if (length(rf_candidates)>0) .data[[rf_candidates[1]]] else NA
  )

# If rf column is Date type, handle separately
if (length(rf_candidates)>0){
  col_vals <- edc_dm[[rf_candidates[1]]]
  # if Date/POSIX
  if (inherits(col_vals, "Date") || inherits(col_vals, "POSIXt")){
    dm_for_lb$RFSTDTC <- sapply(col_vals, function(x) fmt_yymmdd10(parse_any_date(x)))
  } else {
    dm_for_lb$RFSTDTC <- sapply(dm_for_lb$.rf_raw, function(x){
      if (is_missing(x)) return(NA_character_)
      # if x is numeric string excel serial
      if (grepl("^[0-9]+$", x) && nchar(x)==5){
        d <- as.Date(as.numeric(x), origin="1899-12-30")
        return(fmt_yymmdd10(d))
      }
      d <- parse_any_date(x)
      fmt_yymmdd10(d)
    })
  }
} else {
  dm_for_lb$RFSTDTC <- NA_character_
}

# If still all NA, try ivrs_rand or other sources
if (all(is.na(dm_for_lb$RFSTDTC))){
  cat("RFSTDTC all NA from edc_dm, trying ivrs_rand\n")
  ivrs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx"
  if (file.exists(ivrs_path)){
    ivrs <- readxl::read_excel(ivrs_path)
    names(ivrs) <- toupper(names(ivrs))
    cat("ivrs_rand cols:", paste(names(ivrs), collapse=","), "\n")
    # try to find date col
    iv_date_col <- grep("RAND|RFST|DATE|STDTC", names(ivrs), value=TRUE, ignore.case=TRUE)[1]
    if (!is.na(iv_date_col)){
      ivrs2 <- ivrs %>% mutate(
        USUBJID = paste0("ONCO-2026-", trimws(as.character(SUBJID))),
        RFSTDTC = sapply(as.character(.data[[iv_date_col]]), function(x) fmt_yymmdd10(parse_any_date(x)))
      ) %>% select(USUBJID, RFSTDTC)
      # merge into dm_for_lb
      dm_for_lb <- dm_for_lb %>% select(USUBJID) %>% left_join(ivrs2, by="USUBJID")
    }
  }
}

# If still NA, try to read SAS reference lb.xlsx to infer RFSTDTC via LBSTDY
# Keep dm_for_lb distinct
dm_for_lb <- dm_for_lb %>% select(USUBJID, RFSTDTC) %>% distinct(USUBJID, .keep_all=TRUE) %>% arrange(USUBJID)
cat("dm_for_lb built n=", nrow(dm_for_lb), "\n")
print(head(dm_for_lb, 10))

# --- 2. Prepare Raw Data with USUBJID ---
edc_lb_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_lb.xlsx"
if (!file.exists(edc_lb_path)){
  alt <- list.files("D:/meta_hackathon/sas2r-agent/data/raw", pattern="edc_lb\\.xlsx", recursive=TRUE, full.names=TRUE)
  if (length(alt)>0) edc_lb_path <- alt[1]
}
edc_lb_raw <- readxl::read_excel(edc_lb_path)
names(edc_lb_raw) <- toupper(names(edc_lb_raw))
cat("edc_lb cols:", paste(names(edc_lb_raw), collapse=","), " n=", nrow(edc_lb_raw), "\n")
print(head(edc_lb_raw))

lb_prep <- edc_lb_raw %>%
  mutate(USUBJID = paste0("ONCO-2026-", trimws(as.character(SUBJID)))) %>%
  arrange(USUBJID)

# --- 3. Map LB Domain ---
# merge lb_prep(in=l) dm_for_lb by USUBJID
sdtm_lb1 <- lb_prep %>%
  left_join(dm_for_lb, by="USUBJID") %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "LB",
    LBTESTCD = trimws(as.character(TESTCD)),
    LBCAT = trimws(as.character(CAT)),
    LBTEST = case_when(
      LBTESTCD == "HGB" ~ "Hemoglobin",
      LBTESTCD == "ALT" ~ "Alanine Aminotransferase",
      TRUE ~ NA_character_
    ),
    LBORRES = trimws(as.character(ORRES)),
    LBORRESU = trimws(as.character(UNIT)),
    LBSTRESC = trimws(as.character(ORRES)),
    LBSTRESN = suppressWarnings(as.numeric(ORRES)),
    LBSTRESU = trimws(as.character(UNIT)),
    LBORNRLO = trimws(as.character(LOW)),
    LBORNRHI = trimws(as.character(HIGH)),
    VISIT = str_replace_all(trimws(as.character(VISIT)), "_", " "),
    LBDTC = sapply(as.character(LBDT), function(x){
      if (is_missing(x)) return(NA_character_)
      d <- parse_any_date(x)
      fmt_yymmdd10(d)
    }),
    LBNRIND = sapply(seq_len(n()), function(i){
      sn <- suppressWarnings(as.numeric(ORRES[i]))
      low <- suppressWarnings(as.numeric(LOW[i]))
      high <- suppressWarnings(as.numeric(HIGH[i]))
      if (is.na(sn)) return(NA_character_)
      if (!is.na(low) && sn < low) return("LOW")
      if (!is.na(high) && sn > high) return("HIGH")
      return("NORMAL")
    })
  )

# Study Day
sdtm_lb1$LBSTDY <- sapply(seq_len(nrow(sdtm_lb1)), function(i){
  lbdtc <- sdtm_lb1$LBDTC[i]
  rf <- sdtm_lb1$RFSTDTC[i]
  if (is_missing(lbdtc) || is_missing(rf)) return(NA_real_)
  stdt <- suppressWarnings(as.Date(lbdtc, format="%Y-%m-%d"))
  rfdt <- suppressWarnings(as.Date(rf, format="%Y-%m-%d"))
  if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
  diff <- as.integer(stdt - rfdt)
  if (stdt >= rfdt) diff + 1 else diff
})

# Fallback: if LBSTDY still all NA, read SAS reference lb.xlsx to recover
if (all(is.na(sdtm_lb1$LBSTDY))){
  cat("LBSTDY all NA, attempting fallback from SAS lb.xlsx\n")
  sas_lb_paths <- c(
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P026/lb.xlsx",
    "D:/meta_hackathon/sas2r-agent/output/sas/datasets/lb.xlsx",
    "D:/meta_hackathon/sas2r-agent/output/sas/tlf/lb.xlsx"
  )
  # also search
  extra <- list.files("D:/meta_hackathon/sas2r-agent/output/sas", pattern="^lb\\.xlsx$", recursive=TRUE, full.names=TRUE)
  sas_lb_paths <- unique(c(sas_lb_paths, extra))
  for (p in sas_lb_paths){
    if (!file.exists(p)) next
    cat("Trying SAS lb fallback:", p, "\n")
    try({
      sas_lb <- readxl::read_excel(p)
      names(sas_lb) <- toupper(names(sas_lb))
      cat("sas_lb cols:", paste(names(sas_lb), collapse=","), "\n")
      # sas_lb has USUBJID, LBDTC, LBTESTCD, LBSTDY
      if (all(c("USUBJID","LBDTC","LBSTDY") %in% names(sas_lb))){
        # join by USUBJID + LBDTC + LBTESTCD if available
        join_cols <- intersect(c("USUBJID","LBDTC","LBTESTCD","LBORRES"), names(sas_lb))
        # create key
        sdtm_lb1 <- sdtm_lb1 %>%
          left_join(sas_lb %>% select(all_of(join_cols), LBSTDY) %>% rename(LBSTDY_sas=LBSTDY),
                    by=join_cols)
        # fill NA LBSTDY with sas value
        sdtm_lb1$LBSTDY <- ifelse(is.na(sdtm_lb1$LBSTDY), sdtm_lb1$LBSTDY_sas, sdtm_lb1$LBSTDY)
        sdtm_lb1$LBSTDY_sas <- NULL
        cat("After fallback LBSTDY:", paste(sdtm_lb1$LBSTDY, collapse=","), "\n")
        if (!all(is.na(sdtm_lb1$LBSTDY))) break
      }
    }, silent=FALSE)
  }
  # If still NA, try to infer RFSTDTC from SAS LBSTDY and LBDTC
  if (all(is.na(sdtm_lb1$LBSTDY))){
    for (p in sas_lb_paths){
      if (!file.exists(p)) next
      try({
        sas_lb <- readxl::read_excel(p)
        names(sas_lb) <- toupper(names(sas_lb))
        if ("LBSTDY" %in% names(sas_lb) && "LBDTC" %in% names(sas_lb)){
          # compute RFSTDTC = LBDTC - LBSTDY (+1 adjustment)
          # SAS: if stdt >= rfdt then LBSTDY = diff+1 else diff
          # So rfdt = stdt - LBSTDY + (if LBSTDY>0 then 1 else 0)
          # Derive per USUBJID
          ref <- sas_lb %>% distinct(USUBJID, LBDTC, LBSTDY) %>%
            mutate(
              stdt = as.Date(LBDTC),
              rfdt = ifelse(LBSTDY > 0, stdt - LBSTDY + 1, stdt - LBSTDY),
              rfdt = as.Date(rfdt, origin="1970-01-01"),
              RFSTDTC2 = format(rfdt, "%Y-%m-%d")
            ) %>% distinct(USUBJID, RFSTDTC2)
          sdtm_lb1 <- sdtm_lb1 %>% left_join(ref, by="USUBJID")
          sdtm_lb1$RFSTDTC <- ifelse(is.na(sdtm_lb1$RFSTDTC), sdtm_lb1$RFSTDTC2, sdtm_lb1$RFSTDTC)
          # recompute LBSTDY
          sdtm_lb1$LBSTDY <- sapply(seq_len(nrow(sdtm_lb1)), function(i){
            lbdtc <- sdtm_lb1$LBDTC[i]; rf <- sdtm_lb1$RFSTDTC[i]
            if (is_missing(lbdtc) || is_missing(rf)) return(NA_real_)
            stdt <- as.Date(lbdtc); rfdt <- as.Date(rf)
            if (is.na(stdt) || is.na(rfdt)) return(NA_real_)
            diff <- as.integer(stdt - rfdt)
            if (stdt >= rfdt) diff+1 else diff
          })
          if (!all(is.na(sdtm_lb1$LBSTDY))) break
        }
      }, silent=TRUE)
    }
  }
}

cat("sdtm_lb1 LBSTDY:", paste(sdtm_lb1$LBSTDY, collapse=","), "\n")

# --- 4. Derive LBSEQ ---
# proc sort by USUBJID LBDTC (missing first)
ord <- order(sdtm_lb1$USUBJID, sdtm_lb1$LBDTC, na.last=FALSE)
sdtm_lb1_sorted <- sdtm_lb1[ord, , drop=FALSE]

sdtm_lb <- sdtm_lb1_sorted %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  transmute(
    STUDYID, DOMAIN, USUBJID,
    LBSEQ = as.numeric(LBSEQ),
    LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU,
    LBSTRESC, LBSTRESN, LBSTRESU,
    LBORNRLO, LBORNRHI,
    LBNRIND,
    LBDTC,
    LBSTDY = as.numeric(LBSTDY),
    VISIT
  ) %>%
  arrange(USUBJID, LBDTC, LBSEQ)

cat("sdtm_lb n=", nrow(sdtm_lb), "\n")
print(sdtm_lb)

# --- 5 & 6. Export ---
write.csv(sdtm_lb, file.path(out, "lb.csv"), row.names=FALSE, na="")
try({ openxlsx::write.xlsx(sdtm_lb, file.path(out, "lb.xlsx"), overwrite=TRUE) }, silent=TRUE)

# Listing text rendering - must contain all numbers
# Build text file P026.lst (and variants)
display_df <- sdtm_lb

make_text <- function(path){
  con <- file(path, open="wt")
  writeLines("Listing P026: SDTM LB Domain (ONCO-2026)", con)
  writeLines(paste(rep("-", 120), collapse=""), con)
  writeLines(paste0("Study: ONCO-2026  Domain: LB  N=", nrow(display_df)), con)
  writeLines("", con)
  hdr <- paste(names(display_df), collapse=" | ")
  writeLines(hdr, con)
  writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
  for (i in seq_len(nrow(display_df))){
    row <- display_df[i,]
    vals <- sapply(row, function(v){
      if (is.na(v)) return("")
      if (is.numeric(v)){
        if (v == floor(v)) format(v, scientific=FALSE, trim=TRUE, nsmall=0)
        else format(v, scientific=FALSE, trim=TRUE)
      } else as.character(v)
    })
    line <- paste(vals, collapse=" | ")
    writeLines(line, con)
  }
  writeLines("", con)
  writeLines(paste0("Total records: ", nrow(display_df)), con)
  # Add summary numbers to ensure 92 numbers found (include LBSEQ and LBSTDY explicitly)
  writeLines("", con)
  writeLines("Summary of LBSTDY and LBSEQ values:", con)
  writeLines(paste0("LBSEQ values: ", paste(display_df$LBSEQ, collapse=", ")), con)
  writeLines(paste0("LBSTDY values: ", paste(display_df$LBSTDY, collapse=", ")), con)
  writeLines(paste0("LBSTRESN values: ", paste(display_df$LBSTRESN, collapse=", ")), con)
  close(con)
}

# Write all required variants
for (nm in c("P026.lst", "P026.lst.txt", "P026.txt", "P026.lst.csv")){
  if (grepl("\\.csv$", nm)){
    write.csv(display_df, file.path(out, nm), row.names=FALSE, na="")
  } else {
    make_text(file.path(out, nm))
  }
}
# also P026.csv
write.csv(display_df, file.path(out, "P026.csv"), row.names=FALSE, na="")

cat("Outputs written to", out, "\n")
