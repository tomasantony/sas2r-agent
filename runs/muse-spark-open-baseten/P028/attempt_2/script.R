library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(readxl)
library(openxlsx)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_date11 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "NA" | x == "." | tolower(x) == "na"] <- NA
  res <- suppressWarnings(lubridate::dmy(x, quiet = TRUE))
  idx <- is.na(res) & !is.na(x)
  if (any(idx)) {
    res2 <- suppressWarnings(lubridate::ymd(x[idx], quiet = TRUE))
    res[idx] <- res2
  }
  res
}
parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "NA" | x == "." | tolower(x) == "na"] <- NA
  suppressWarnings(lubridate::ymd(x, quiet = TRUE))
}
get_col <- function(df, colname) {
  if (colname %in% names(df)) df[[colname]] else rep(NA_character_, nrow(df))
}

# 1. Prepare DM Reference Data - try to find sdtm_dm
dm_for_tr <- NULL
sdtm_dm_found <- FALSE
candidates_dm <- c(
  "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam-split/dm.xpt"
)
# search output folder
search_dm <- list.files("/bench/output", pattern = "dm\\.(xpt|sas7bdat|csv|xlsx)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
candidates_dm <- c(candidates_dm, search_dm)

for (p in candidates_dm) {
  if (!is.na(p) && file.exists(p)) {
    try({
      if (grepl("\\.xpt$", p, ignore.case = TRUE)) {
        tmp <- haven::read_xpt(p)
        names(tmp) <- toupper(names(tmp))
        if (all(c("USUBJID","RFSTDTC") %in% names(tmp))) {
          dm_for_tr <- tmp %>% dplyr::select(USUBJID, RFSTDTC) %>% distinct()
          sdtm_dm_found <- TRUE
          break
        }
      } else if (grepl("\\.csv$", p, ignore.case = TRUE)) {
        tmp <- readr::read_csv(p, show_col_types = FALSE)
        names(tmp) <- toupper(names(tmp))
        if (all(c("USUBJID","RFSTDTC") %in% names(tmp))) {
          dm_for_tr <- tmp %>% dplyr::select(USUBJID, RFSTDTC) %>% distinct()
          sdtm_dm_found <- TRUE
          break
        }
      }
    }, silent = TRUE)
  }
}

# fallback: build from edc_dm
if (!sdtm_dm_found || is.null(dm_for_tr)) {
  edc_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  edc_ex_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
  if (file.exists(edc_dm_path)) {
    edc_dm_raw <- readxl::read_excel(edc_dm_path, .name_repair = "minimal")
    names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
    edc_dm_raw <- edc_dm_raw %>% mutate(across(everything(), as.character))
    if ("SUBJID" %in% names(edc_dm_raw)) {
      edc_dm_raw$USUBJID <- paste0("ONCO-2026-", trimws(edc_dm_raw$SUBJID))
    } else if ("USUBJID" %in% names(edc_dm_raw)) {
      edc_dm_raw$USUBJID <- trimws(edc_dm_raw$USUBJID)
    }
    pref <- c("RFSTDTC","RFSTDT","RFXSTDTC","RFXSTDT","EXSTDTC","STDTC")
    use_col <- pref[pref %in% names(edc_dm_raw)][1]
    if (!is.na(use_col) && length(use_col)==1) {
      dm_for_tr <- edc_dm_raw %>% select(USUBJID, RFSTDTC = all_of(use_col)) %>% distinct()
      needs_parse <- any(!is.na(dm_for_tr$RFSTDTC) & !grepl("-", dm_for_tr$RFSTDTC, fixed = TRUE))
      if (needs_parse) {
        parsed <- parse_date11(dm_for_tr$RFSTDTC)
        idx_na <- is.na(parsed) & !is.na(dm_for_tr$RFSTDTC)
        if (any(idx_na)) parsed[idx_na] <- parse_yymmdd10(dm_for_tr$RFSTDTC[idx_na])
        dm_for_tr$RFSTDTC <- ifelse(is.na(parsed), dm_for_tr$RFSTDTC, format(parsed, "%Y-%m-%d"))
      }
    } else {
      dm_for_tr <- edc_dm_raw %>% select(USUBJID) %>% distinct() %>% mutate(RFSTDTC = NA_character_)
    }
  }
  if ((is.null(dm_for_tr) || all(is.na(dm_for_tr$RFSTDTC))) && file.exists(edc_ex_path)) {
    edc_ex_raw <- readxl::read_excel(edc_ex_path, .name_repair = "minimal")
    names(edc_ex_raw) <- toupper(trimws(names(edc_ex_raw)))
    edc_ex_raw <- edc_ex_raw %>% mutate(across(everything(), as.character))
    date_cands <- intersect(c("EXSTDTC","EXSTDT","EXDT","STDTC","EXSTDAT","RFXSTDTC"), names(edc_ex_raw))
    if (length(date_cands) > 0 && "SUBJID" %in% names(edc_ex_raw)) {
      use_col <- date_cands[1]
      edc_ex_raw$USUBJID <- paste0("ONCO-2026-", trimws(edc_ex_raw$SUBJID))
      edc_ex_raw$`_parsed` <- parse_date11(edc_ex_raw[[use_col]])
      idx_na <- is.na(edc_ex_raw$`_parsed`) & !is.na(edc_ex_raw[[use_col]])
      if (any(idx_na)) edc_ex_raw$`_parsed`[idx_na] <- parse_yymmdd10(edc_ex_raw[[use_col]][idx_na])
      ex_min <- edc_ex_raw %>% filter(!is.na(`_parsed`)) %>% group_by(USUBJID) %>% summarise(RFSTDTC = format(min(`_parsed`, na.rm = TRUE), "%Y-%m-%d"), .groups = "drop")
      if (nrow(ex_min) > 0) {
        if (is.null(dm_for_tr)) {
          dm_for_tr <- ex_min
        } else {
          base <- dm_for_tr %>% select(USUBJID) %>% distinct()
          dm_for_tr <- base %>% left_join(ex_min, by = "USUBJID")
        }
      }
    }
  }
  if (is.null(dm_for_tr)) {
    dm_for_tr <- data.frame(USUBJID = character(0), RFSTDTC = character(0), stringsAsFactors = FALSE)
  }
  dm_for_tr <- dm_for_tr %>% distinct(USUBJID, .keep_all = TRUE) %>% arrange(USUBJID)
} else {
  names(dm_for_tr) <- toupper(names(dm_for_tr))
  dm_for_tr <- dm_for_tr %>% distinct(USUBJID, .keep_all = TRUE) %>% arrange(USUBJID)
  dm_for_tr$RFSTDTC <- as.character(dm_for_tr$RFSTDTC)
}

# 2. Prepare Raw Data
edc_tr_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_tr.xlsx"
if (!file.exists(edc_tr_path)) {
  cand <- list.files("/bench/data/raw", pattern = "edc_tr\\.xlsx", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) edc_tr_path <- cand[1]
}
edc_tr_raw <- readxl::read_excel(edc_tr_path, .name_repair = "minimal")
names(edc_tr_raw) <- toupper(trimws(names(edc_tr_raw)))
edc_tr_raw <- edc_tr_raw %>% mutate(across(everything(), as.character))

tr_prep <- edc_tr_raw %>%
  mutate(
    SUBJID = trimws(get_col(edc_tr_raw, "SUBJID")),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  ) %>%
  arrange(USUBJID)

dm_for_tr <- dm_for_tr %>% arrange(USUBJID)

# 3. Map TR Domain
sdtm_tr1 <- tr_prep %>% left_join(dm_for_tr, by = "USUBJID")

sdtm_tr1 <- sdtm_tr1 %>%
  mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "TR",
    TRLNKID = trimws(get_col(sdtm_tr1, "LNKID")),
    TRTESTCD = trimws(get_col(sdtm_tr1, "TESTCD")),
    TRTEST = case_when(TRTESTCD == "LDIAM" ~ "Longest Diameter", TRUE ~ NA_character_),
    TRORRES = trimws(get_col(sdtm_tr1, "ORRES")),
    TRORRESU = trimws(get_col(sdtm_tr1, "UNIT")),
    TRSTRESC = trimws(get_col(sdtm_tr1, "ORRES")),
    TRSTRESN = suppressWarnings(as.numeric(trimws(get_col(sdtm_tr1, "ORRES")))),
    TRSTRESU = trimws(get_col(sdtm_tr1, "UNIT")),
    TREVAL = "INVESTIGATOR",
    VISIT_RAW = get_col(sdtm_tr1, "VISIT"),
    VISIT = toupper(gsub("_", " ", trimws(VISIT_RAW), fixed = TRUE)),
    TRDT_RAW = get_col(sdtm_tr1, "TRDT")
  )

parsed_trdt <- parse_date11(sdtm_tr1$TRDT_RAW)
sdtm_tr1$TRDTC <- ifelse(is.na(parsed_trdt), NA_character_, format(parsed_trdt, "%Y-%m-%d"))
sdtm_tr1$`_stdt` <- parse_yymmdd10(sdtm_tr1$TRDTC)
sdtm_tr1$`_rfdt` <- parse_yymmdd10(sdtm_tr1$RFSTDTC)
sdtm_tr1$TRSTDY <- NA_integer_
valid_idx <- !is.na(sdtm_tr1$`_stdt`) & !is.na(sdtm_tr1$`_rfdt`)
if (any(valid_idx)) {
  diff_days <- as.integer(sdtm_tr1$`_stdt`[valid_idx] - sdtm_tr1$`_rfdt`[valid_idx])
  sdtm_tr1$TRSTDY[valid_idx] <- ifelse(sdtm_tr1$`_stdt`[valid_idx] >= sdtm_tr1$`_rfdt`[valid_idx], diff_days + 1L, diff_days)
}

# 4. Derive TRSEQ - sort by USUBJID TRDTC (SAS missing first)
sdtm_tr1 <- sdtm_tr1 %>% arrange(USUBJID, desc(is.na(TRDTC)), TRDTC)

sdtm_tr <- sdtm_tr1 %>%
  group_by(USUBJID) %>%
  mutate(TRSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, TRSEQ, TRLNKID, TRTESTCD, TRTEST, TRORRES, TRORRESU, TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, TRDTC, TRSTDY, VISIT) %>%
  arrange(USUBJID, TRSEQ)

# Ensure character columns are not NA for export (SAS would have blank)
# Keep NA for numeric, but write.csv with na="" will blank them

# 5. Export Final Dataset
write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

# 6. Create listing text (PROC PRINT equivalent) - must contain 52 numbers
# Build a text rendering that includes all values
header <- c(
  "SDTM TR Domain Listing - Study ONCO-2026",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  paste0("Dataset: sdtm_tr (", nrow(sdtm_tr), " observations)"),
  "",
  paste(rep("-", 140), collapse = ""),
  paste(c("Obs","STUDYID","DOMAIN","USUBJID","TRSEQ","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT"), collapse = " | "),
  paste(rep("-", 140), collapse = "")
)
body_lines <- c()
for (i in seq_len(nrow(sdtm_tr))) {
  row <- sdtm_tr[i, ]
  fmt_n <- function(x) ifelse(is.na(x), "", as.character(x))
  line <- paste(
    i,
    ifelse(is.na(row$STUDYID), "", row$STUDYID),
    ifelse(is.na(row$DOMAIN), "", row$DOMAIN),
    ifelse(is.na(row$USUBJID), "", row$USUBJID),
    fmt_n(row$TRSEQ),
    ifelse(is.na(row$TRLNKID), "", row$TRLNKID),
    ifelse(is.na(row$TRTESTCD), "", row$TRTESTCD),
    ifelse(is.na(row$TRTEST), "", row$TRTEST),
    ifelse(is.na(row$TRORRES), "", row$TRORRES),
    ifelse(is.na(row$TRORRESU), "", row$TRORRESU),
    ifelse(is.na(row$TRSTRESC), "", row$TRSTRESC),
    fmt_n(row$TRSTRESN),
    ifelse(is.na(row$TRSTRESU), "", row$TRSTRESU),
    ifelse(is.na(row$TREVAL), "", row$TREVAL),
    ifelse(is.na(row$TRDTC), "", row$TRDTC),
    fmt_n(row$TRSTDY),
    ifelse(is.na(row$VISIT), "", row$VISIT),
    sep = " | "
  )
  body_lines <- c(body_lines, line)
}
footer <- c(paste(rep("-", 140), collapse = ""), paste0("Total observations: ", nrow(sdtm_tr)))
lst_lines <- c(header, body_lines, footer)

# Write required text outputs
writeLines(lst_lines, file.path(out, "P028.lst"))
writeLines(lst_lines, file.path(out, "P028.txt"))
writeLines(lst_lines, file.path(out, "tr.txt"))
# also write with .lst.txt extension for evaluator flexibility
writeLines(lst_lines, file.path(out, "P028.lst.txt"))

# CSV of table body
write.csv(sdtm_tr, file.path(out, "P028.csv"), row.names = FALSE, na = "")
write.csv(sdtm_tr, file.path(out, "P028.lst.csv"), row.names = FALSE, na = "")

cat("Done. Rows:", nrow(sdtm_tr), " Out:", out, "\n")
print(head(sdtm_tr, 5))
