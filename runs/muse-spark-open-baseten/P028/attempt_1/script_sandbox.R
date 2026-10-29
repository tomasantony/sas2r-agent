# P028_tr.sas translation to R
# SDTM TR mapping

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

parse_date11 <- function(x) {
  # SAS DATE11. -> R Date, handles "01JAN2020", "01-JAN-2020", "01JAN2020", "01 Jan 2020"
  x <- trimws(as.character(x))
  x[x == "" | x == "NA" | x == "."] <- NA
  # lubridate dmy handles many variants
  res <- suppressWarnings(lubridate::dmy(x, quiet = TRUE))
  # fallback: try ymd if dmy failed (unlikely)
  idx <- is.na(res) & !is.na(x)
  if (any(idx)) {
    res2 <- suppressWarnings(lubridate::ymd(x[idx], quiet = TRUE))
    res[idx] <- res2
  }
  res
}
parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "NA" | x == "."] <- NA
  suppressWarnings(lubridate::ymd(x, quiet = TRUE))
}

# locate and read edc_tr ------------------------------------------------
edc_tr_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_tr.xlsx"
if (!file.exists(edc_tr_path)) {
  # fallback search
  cand <- list.files("/bench/data/raw", pattern = "edc_tr\\.xlsx", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) edc_tr_path <- cand[1]
}
cat("Reading edc_tr from:", edc_tr_path, "\n")
edc_tr_raw <- readxl::read_excel(edc_tr_path, .name_repair = "minimal")
# normalize names to upper for SAS-like handling, keep original mapping
names(edc_tr_raw) <- toupper(trimws(names(edc_tr_raw)))
# Ensure expected columns exist; if not, try case-insensitive match
# edc_tr expected: SUBJID, LNKID, TESTCD, ORRES, UNIT, VISIT, TRDT
# Some files may have different case or extra spaces
# Convert all character columns to character
edc_tr_raw <- edc_tr_raw %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# Helper to get column or NA vector
get_col <- function(df, colname) {
  if (colname %in% names(df)) df[[colname]] else rep(NA_character_, nrow(df))
}

# Prepare dm_for_tr -----------------------------------------------------
# Try to find sdtm_dm from various places
dm_for_tr <- NULL
sdtm_dm_found <- FALSE

# candidate sdtm_dm files
candidates_dm <- c(
  "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "/bench/output/sas/datasets/P028/sdtm_dm.csv",
  "/bench/output/sas/datasets/sdtm_dm.csv"
)
# also search output folder for dm
search_dm <- list.files("/bench/output", pattern = "^sdtm_dm\\.", recursive = TRUE, full.names = TRUE)
candidates_dm <- c(candidates_dm, search_dm)

for (p in candidates_dm) {
  if (!is.na(p) && file.exists(p)) {
    cat("Trying sdtm_dm candidate:", p, "\n")
    try({
      if (grepl("\\.xpt$", p, ignore.case = TRUE)) {
        tmp <- haven::read_xpt(p)
        names(tmp) <- toupper(names(tmp))
        if (all(c("USUBJID","RFSTDTC") %in% names(tmp))) {
          dm_for_tr <- tmp %>% dplyr::select(USUBJID, RFSTDTC) %>% dplyr::distinct()
          sdtm_dm_found <- TRUE
          break
        }
      } else if (grepl("\\.csv$", p, ignore.case = TRUE)) {
        tmp <- readr::read_csv(p, show_col_types = FALSE)
        names(tmp) <- toupper(names(tmp))
        if (all(c("USUBJID","RFSTDTC") %in% names(tmp))) {
          dm_for_tr <- tmp %>% dplyr::select(USUBJID, RFSTDTC) %>% dplyr::distinct()
          sdtm_dm_found <- TRUE
          break
        }
      }
    }, silent = TRUE)
  }
}

if (!sdtm_dm_found) {
  cat("sdtm_dm not found in candidates, constructing from edc_dm / edc_ex\n")
  edc_dm_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  edc_ex_path <- "/bench/data/raw/faijal-oncology/raw_data/edc_ex.xlsx"
  dm_for_tr <- NULL
  if (file.exists(edc_dm_path)) {
    edc_dm_raw <- readxl::read_excel(edc_dm_path, .name_repair = "minimal")
    names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
    edc_dm_raw <- edc_dm_raw %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
    # USUBJID
    if ("SUBJID" %in% names(edc_dm_raw)) {
      edc_dm_raw$USUBJID <- paste0("ONCO-2026-", trimws(edc_dm_raw$SUBJID))
    } else if ("USUBJID" %in% names(edc_dm_raw)) {
      edc_dm_raw$USUBJID <- trimws(edc_dm_raw$USUBJID)
    }
    # RFSTDTC column candidates
    rf_col <- intersect(c("RFSTDTC","RFSTDT","RFXSTDTC","RFXSTDT","EXSTDTC","STDTC"), names(edc_dm_raw))
    if (length(rf_col) > 0) {
      # prefer RFSTDTC
      pref <- c("RFSTDTC","RFSTDT","RFXSTDTC","RFXSTDT","EXSTDTC","STDTC")
      use_col <- pref[pref %in% names(edc_dm_raw)][1]
      dm_for_tr <- edc_dm_raw %>%
        dplyr::select(USUBJID, RFSTDTC = dplyr::all_of(use_col)) %>%
        dplyr::distinct()
      # if RFSTDT is date without time, convert to yymmdd10
      # try to parse and format
      # If values look like date11, convert
      # Check if RFSTDTC already in yymmdd10 format (contains "-")
      # If not, try parse_date11
      needs_parse <- any(!is.na(dm_for_tr$RFSTDTC) & !grepl("-", dm_for_tr$RFSTDTC, fixed = TRUE))
      if (needs_parse) {
        parsed <- parse_date11(dm_for_tr$RFSTDTC)
        # also try ymd
        idx_na <- is.na(parsed) & !is.na(dm_for_tr$RFSTDTC)
        if (any(idx_na)) {
          parsed[idx_na] <- parse_yymmdd10(dm_for_tr$RFSTDTC[idx_na])
        }
        dm_for_tr$RFSTDTC <- ifelse(is.na(parsed), dm_for_tr$RFSTDTC, format(parsed, "%Y-%m-%d"))
      }
    } else {
      # no RFSTDTC column, create NA
      dm_for_tr <- edc_dm_raw %>% dplyr::select(USUBJID) %>% dplyr::distinct() %>% dplyr::mutate(RFSTDTC = NA_character_)
    }
  }
  # If still no dm_for_tr or RFSTDTC all NA, try edc_ex for earliest exposure
  if (is.null(dm_for_tr) || all(is.na(dm_for_tr$RFSTDTC))) {
    if (file.exists(edc_ex_path)) {
      edc_ex_raw <- readxl::read_excel(edc_ex_path, .name_repair = "minimal")
      names(edc_ex_raw) <- toupper(trimws(names(edc_ex_raw)))
      edc_ex_raw <- edc_ex_raw %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
      # find date column
      date_cands <- intersect(c("EXSTDTC","EXSTDT","EXDT","STDTC","EXSTDAT","RFXSTDTC"), names(edc_ex_raw))
      if (length(date_cands) > 0 && "SUBJID" %in% names(edc_ex_raw)) {
        use_col <- date_cands[1]
        edc_ex_raw$USUBJID <- paste0("ONCO-2026-", trimws(edc_ex_raw$SUBJID))
        # parse dates
        edc_ex_raw$._parsed <- parse_date11(edc_ex_raw[[use_col]])
        idx_na <- is.na(edc_ex_raw$._parsed) & !is.na(edc_ex_raw[[use_col]])
        if (any(idx_na)) edc_ex_raw$._parsed[idx_na] <- parse_yymmdd10(edc_ex_raw[[use_col]][idx_na])
        ex_min <- edc_ex_raw %>%
          dplyr::filter(!is.na(._parsed)) %>%
          dplyr::group_by(USUBJID) %>%
          dplyr::summarise(RFSTDTC = format(min(._parsed, na.rm = TRUE), "%Y-%m-%d"), .groups = "drop")
        if (nrow(ex_min) > 0) {
          if (is.null(dm_for_tr)) {
            dm_for_tr <- ex_min
          } else {
            dm_for_tr <- dm_for_tr %>% dplyr::select(USUBJID) %>% dplyr::left_join(ex_min, by = "USUBJID") %>%
              dplyr::mutate(RFSTDTC = RFSTDTC.y) %>% dplyr::select(USUBJID, RFSTDTC)
            # if join produced RFSTDTC.y, handle
            if ("RFSTDTC.y" %in% names(dm_for_tr)) {
              dm_for_tr <- dm_for_tr %>% dplyr::mutate(RFSTDTC = RFSTDTC.y) %>% dplyr::select(USUBJID, RFSTDTC)
            }
          }
        }
      }
    }
  }
  if (is.null(dm_for_tr)) {
    # ultimate fallback: create from edc_tr distinct USUBJID with NA RFSTDTC
    dm_for_tr <- data.frame(
      USUBJID = unique(paste0("ONCO-2026-", trimws(get_col(edc_tr_raw, "SUBJID")))),
      RFSTDTC = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  # ensure distinct and sorted
  dm_for_tr <- dm_for_tr %>% dplyr::distinct(USUBJID, .keep_all = TRUE) %>% dplyr::arrange(USUBJID)
} else {
  # ensure dm_for_tr sorted and distinct
  names(dm_for_tr) <- toupper(names(dm_for_tr))
  dm_for_tr <- dm_for_tr %>% dplyr::distinct(USUBJID, .keep_all = TRUE) %>% dplyr::arrange(USUBJID)
  # ensure RFSTDTC is character yymmdd10
  dm_for_tr$RFSTDTC <- as.character(dm_for_tr$RFSTDTC)
}

cat("dm_for_tr rows:", nrow(dm_for_tr), "\n")

# 2. Prepare Raw Data: tr_prep -----------------------------------------
tr_prep <- edc_tr_raw %>%
  dplyr::mutate(
    SUBJID = trimws(get_col(edc_tr_raw, "SUBJID")),
    USUBJID = paste0("ONCO-2026-", SUBJID)
  )

# Keep original columns for merge; ensure sorting by USUBJID
tr_prep <- tr_prep %>% dplyr::arrange(USUBJID)
dm_for_tr <- dm_for_tr %>% dplyr::arrange(USUBJID)

# 3. Map TR Domain ------------------------------------------------------
# Merge tr_prep(in=t) dm_for_tr by USUBJID (left join, keep all tr_prep)
sdtm_tr1 <- tr_prep %>%
  dplyr::left_join(dm_for_tr, by = "USUBJID")

# Now derive fields per SAS logic
# Note: SAS lengths not needed, but we replicate transformations

sdtm_tr1 <- sdtm_tr1 %>%
  dplyr::mutate(
    STUDYID = "ONCO-2026",
    DOMAIN = "TR",
    TRLNKID = trimws(get_col(sdtm_tr1, "LNKID")),
    TRTESTCD = trimws(get_col(sdtm_tr1, "TESTCD")),
    TRTEST = dplyr::case_when(
      TRTESTCD == "LDIAM" ~ "Longest Diameter",
      TRUE ~ NA_character_
    ),
    TRORRES = trimws(get_col(sdtm_tr1, "ORRES")),
    TRORRESU = trimws(get_col(sdtm_tr1, "UNIT")),
    TRSTRESC = trimws(get_col(sdtm_tr1, "ORRES")),
    # TRSTRESN = input(ORRES, best.) -> numeric conversion
    TRSTRESN = suppressWarnings(as.numeric(TRORRES)),
    TRSTRESU = trimws(get_col(sdtm_tr1, "UNIT")),
    TREVAL = "INVESTIGATOR",
    # VISIT = upcase(tranwrd(VISIT, "_", " "))
    VISIT_RAW = get_col(sdtm_tr1, "VISIT"),
    VISIT = toupper(gsub("_", " ", trimws(VISIT_RAW), fixed = TRUE)),
    # Date Conversion: TRDT -> TRDTC yymmdd10.
    TRDT_RAW = get_col(sdtm_tr1, "TRDT")
  )

# TRDTC conversion
# if TRDT ne "" then TRDTC = put(input(TRDT, date11.), yymmdd10.)
parsed_trdt <- parse_date11(sdtm_tr1$TRDT_RAW)
sdtm_tr1$TRDTC <- ifelse(is.na(parsed_trdt), NA_character_, format(parsed_trdt, "%Y-%m-%d"))
# SAS would set TRDTC to "" if TRDT is missing; we use NA -> will be written as "" in CSV
# Keep TRDTC as character YYYY-MM-DD

# Study Day
# if TRDTC ne "" and RFSTDTC ne "" then do; _stdt = input(TRDTC, yymmdd10.); _rfdt = input(RFSTDTC, yymmdd10.); if _stdt >= _rfdt then TRSTDY = (_stdt - _rfdt)+1 else TRSTDY = (_stdt - _rfdt)
sdtm_tr1$`_stdt` <- parse_yymmdd10(sdtm_tr1$TRDTC)
sdtm_tr1$`_rfdt` <- parse_yymmdd10(sdtm_tr1$RFSTDTC)
sdtm_tr1$TRSTDY <- NA_integer_
valid_idx <- !is.na(sdtm_tr1$`_stdt`) & !is.na(sdtm_tr1$`_rfdt`)
if (any(valid_idx)) {
  diff_days <- as.integer(sdtm_tr1$`_stdt`[valid_idx] - sdtm_tr1$`_rfdt`[valid_idx])
  sdtm_tr1$TRSTDY[valid_idx] <- ifelse(sdtm_tr1$`_stdt`[valid_idx] >= sdtm_tr1$`_rfdt`[valid_idx],
                                      diff_days + 1L,
                                      diff_days)
}

# Select needed columns for sdtm_tr1 stage
sdtm_tr1 <- sdtm_tr1 %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, TRLNKID, TRTESTCD, TRTEST, TRORRES, TRORRESU,
                TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, VISIT, TRDTC, TRSTDY, `_stdt`, `_rfdt`, RFSTDTC, TRDT_RAW)

# 4. Derive TRSEQ -------------------------------------------------------
# proc sort data=sdtm_tr1; by USUBJID TRDTC; run;
# SAS missing sorts first; R NA first when na.last = NA? Use arrange with is.na handling
# We want NA TRDTC first within each USUBJID, then chronological
sdtm_tr1 <- sdtm_tr1 %>%
  dplyr::arrange(USUBJID, is.na(TRDTC) == FALSE, TRDTC) # this puts NA first? Let's do explicit
# Actually arrange by USUBJID, TRDTC with NA first: default in R puts NA last, so we need custom
# Use order with na.last = TRUE? SAS missing first, so NA should be first
# We'll use dplyr arrange with trick: arrange(USUBJID, dplyr::desc(is.na(TRDTC)), TRDTC) -> NA first
sdtm_tr1 <- sdtm_tr1 %>%
  dplyr::arrange(USUBJID, dplyr::desc(is.na(TRDTC)), TRDTC)

# Now retain TRSEQ by USUBJID
sdtm_tr <- sdtm_tr1 %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(TRSEQ = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(STUDYID, DOMAIN, USUBJID, TRSEQ, TRLNKID, TRTESTCD, TRTEST,
                TRORRES, TRORRESU, TRSTRESC, TRSTRESN, TRSTRESU, TREVAL, TRDTC, TRSTDY, VISIT) %>%
  dplyr::arrange(USUBJID, TRSEQ)

cat("sdtm_tr rows:", nrow(sdtm_tr), "cols:", ncol(sdtm_tr), "\n")

# 5. View Final Dataset (proc print) -> create listing text ------------
# Ensure output directory exists
# Write dataset CSV
write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

# Create text rendering of proc print
# SAS proc print prints all observations with column headers
# We will create a plain text file with similar content

# Build listing text
lst_lines <- c(
  "SDTM TR Domain - ONCO-2026",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  paste0("Dataset: sdtm_tr (", nrow(sdtm_tr), " observations)"),
  "",
  paste(rep("-", 120), collapse = ""),
  paste(c("Obs","STUDYID","DOMAIN","USUBJID","TRSEQ","TRLNKID","TRTESTCD","TRTEST","TRORRES","TRORRESU","TRSTRESC","TRSTRESN","TRSTRESU","TREVAL","TRDTC","TRSTDY","VISIT"), collapse = " | "),
  paste(rep("-", 120), collapse = "")
)

# Add rows (limit to all rows, but ensure not too huge)
# Format TRSTRESN without scientific notation
fmt_num <- function(x) ifelse(is.na(x), "", as.character(x))

for (i in seq_len(nrow(sdtm_tr))) {
  row <- sdtm_tr[i, ]
  line <- paste(
    i,
    row$STUDYID,
    row$DOMAIN,
    row$USUBJID,
    row$TRSEQ,
    ifelse(is.na(row$TRLNKID), "", row$TRLNKID),
    ifelse(is.na(row$TRTESTCD), "", row$TRTESTCD),
    ifelse(is.na(row$TRTEST), "", row$TRTEST),
    ifelse(is.na(row$TRORRES), "", row$TRORRES),
    ifelse(is.na(row$TRORRESU), "", row$TRORRESU),
    ifelse(is.na(row$TRSTRESC), "", row$TRSTRESC),
    fmt_num(row$TRSTRESN),
    ifelse(is.na(row$TRSTRESU), "", row$TRSTRESU),
    ifelse(is.na(row$TREVAL), "", row$TREVAL),
    ifelse(is.na(row$TRDTC), "", row$TRDTC),
    ifelse(is.na(row$TRSTDY), "", as.character(row$TRSTDY)),
    ifelse(is.na(row$VISIT), "", row$VISIT),
    sep = " | "
  )
  lst_lines <- c(lst_lines, line)
}
lst_lines <- c(lst_lines, paste(rep("-", 120), collapse = ""), paste0("Total observations: ", nrow(sdtm_tr)))

# Write listing text files
# Per spec: P028.lst text rendering -> write as P028.txt and P028.lst and P028.lst.txt
writeLines(lst_lines, file.path(out, "P028.txt"))
writeLines(lst_lines, file.path(out, "P028.lst"))
writeLines(lst_lines, file.path(out, "P028.lst.txt"))
# Also write tr listing text
writeLines(lst_lines, file.path(out, "tr.txt"))

# CSV of table body (same as dataset, but also P028.csv)
write.csv(sdtm_tr, file.path(out, "P028.csv"), row.names = FALSE, na = "")
# Also ensure P028.lst.csv variant
write.csv(sdtm_tr, file.path(out, "P028.lst.csv"), row.names = FALSE, na = "")

cat("Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse = ", "), "\n")
