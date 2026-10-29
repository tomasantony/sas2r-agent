# P005 adam_adlb.sas -> R translation
# Outputs: adlb.csv + listing P005 (txt + csv)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

project_path <- "/bench/data/derived/c2c"
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

read_any <- function(path) {
  lp <- tolower(path)
  if (grepl("\\.sas7bdat$", lp)) {
    df <- haven::read_sas(path)
  } else if (grepl("\\.xpt$", lp)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.csv$", lp)) {
    df <- readr::read_csv(path, show_col_types = FALSE, guess_max = 100000) |> as.data.frame()
  } else {
    stop(paste("Unknown file type:", path))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  # zap haven labels/formats to plain vectors
  for (nm in names(df)) {
    v <- df[[nm]]
    if (haven::is.labelled(v)) {
      # keep underlying values, drop labels
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      # haven Date labels remain Date; otherwise coerce labelled to vector
      if (haven::is.labelled(v)) v <- as.vector(v)
      df[[nm]] <- v
    }
    # haven POSIX? keep
  }
  df
}

find_file <- function(dir, base) {
  # base like "adsl" or "lb" (case-insensitive exact name)
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE)
  if (length(fl) == 0) return(NULL)
  # exact match first
  pat <- paste0("^", base, "\\.(sas7bdat|xpt|csv)$")
  idx <- grep(pat, basename(fl), ignore.case = TRUE)
  if (length(idx) > 0) return(fl[idx[1]])
  # contains
  pat2 <- paste0(base, ".*\\.(sas7bdat|xpt|csv)$")
  idx2 <- grep(pat2, basename(fl), ignore.case = TRUE)
  if (length(idx2) > 0) return(fl[idx2[1]])
  return(NULL)
}

adsl_path <- find_file(adam_dir, "adsl")
lb_path   <- find_file(sdtm_dir, "lb")

# fallback to raw split dirs if not found (benchmark paths)
if (is.null(adsl_path)) {
  alt1 <- "/bench/data/raw/cdisc-pilot/adam"
  alt2 <- "/bench/data/derived/cdisc-pilot/adam-split"
  for (d in c(alt1, alt2)) {
    p <- find_file(d, "adsl")
    if (!is.null(p)) { adsl_path <- p; break }
  }
}
if (is.null(lb_path)) {
  alt3 <- "/bench/data/raw/cdisc-pilot/sdtm"
  p <- find_file(alt3, "lb")
  if (!is.null(p)) lb_path <- p
}
if (is.null(adsl_path)) stop("ADSL file not found")
if (is.null(lb_path)) stop("LB file not found")

message("Reading ADSL: ", adsl_path)
message("Reading LB: ", lb_path)
adsl <- read_any(adsl_path)
lb   <- read_any(lb_path)

message(paste("ADSL rows:", nrow(adsl), "cols:", paste(names(adsl), collapse=",")))
message(paste("LB rows:", nrow(lb), "cols:", paste(names(lb), collapse=",")))

# Ensure expected columns exist (uppercase)
# ADSL: STUDYID USUBJID TRT01A TRTSDT TRTEDT
# LB: USUBJID LBDTC LBTESTCD LBTEST LBSTRESN VISIT VISITNUM LBSTRESU LBSEQ

# Helper: convert SAS/character dates to Date
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS date: days since 1960-01-01 ; haven already converts, but csv numeric may be SAS date
    # Heuristic: if values look like SAS dates (e.g., 10000-20000) convert; otherwise NA handling
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    suppressWarnings({
      d <- as.Date(x, format = "%Y-%m-%d")
    })
    # try datetime prefix
    need <- is.na(d) & !is.na(x) & nchar(x) >= 10
    d[need] <- suppressWarnings(as.Date(substr(x[need], 1, 10), format = "%Y-%m-%d"))
    return(d)
  }
  suppressWarnings(as.Date(x))
}

# Coerce types for merge keys to character
adsl$USUBJID <- as.character(adsl$USUBJID)
lb$USUBJID   <- as.character(lb$USUBJID)

if (!"STUDYID" %in% names(adsl)) adsl$STUDYID <- NA_character_ else adsl$STUDYID <- as.character(adsl$STUDYID)
if (!"TRT01A" %in% names(adsl)) adsl$TRT01A <- NA_character_ else adsl$TRT01A <- as.character(adsl$TRT01A)
adsl$TRTSDT <- to_date(adsl$TRTSDT)
adsl$TRTEDT <- to_date(adsl$TRTEDT)

lb$LBDTC    <- as.character(lb$LBDTC)
lb$LBTESTCD <- as.character(lb$LBTESTCD)
lb$LBTEST   <- as.character(lb$LBTEST)
lb$LBSTRESN <- suppressWarnings(as.numeric(lb$LBSTRESN))
lb$VISIT    <- as.character(lb$VISIT)
lb$VISITNUM <- suppressWarnings(as.numeric(lb$VISITNUM))
lb$LBSTRESU <- as.character(lb$LBSTRESU)
lb$LBSEQ    <- suppressWarnings(as.numeric(lb$LBSEQ))

# Keep only needed ADSL cols for merge (retain all? merge brings all, but draft keeps subset)
adsl_keep <- adsl %>% select(any_of(c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT")))

# 1. MERGE ADSL AND LB : keep only records that have lab tests (if b)
# SAS: proc sort adsl by USUBJID; lb by USUBJID LBDTC LBTESTCD
# merge by USUBJID -> left join lb to adsl
adsl_sorted <- adsl_keep %>% arrange(USUBJID)
# sort lb for merge (not strictly needed for join, but replicate)
lb_sorted <- lb %>% arrange(USUBJID, LBDTC, LBTESTCD)

adlb_draft <- lb_sorted %>%
  left_join(adsl_sorted, by = "USUBJID", suffix = c("", ".adsl")) %>%
  filter(!is.na(LBTESTCD) | !is.na(LBSTRESN) | !is.na(LBSEQ) | !is.na(LBDTC) | TRUE) %>%
  # SAS if b keeps all lb rows (lb_sorted is b). left_join already keeps all lb rows.
  mutate(
    PARAMCD = LBTESTCD,
    PARAM   = LBTEST,
    AVAL    = LBSTRESN,
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    SRCDOM  = "LB",
    SRCVAR  = "LBSEQ",
    SRCSEQ  = LBSEQ
  )

# Convert ISO Date to Numeric Analysis Date: if length(LBDTC)>=10 then ASTDT=input(substr(LBDTC,1,10),yymmdd10.)
# SAS LENGTH strips trailing blanks
sas_length <- function(x) {
  ifelse(is.na(x), NA_integer_, nchar(gsub(" +$", "", x)))
}
adlb_draft <- adlb_draft %>%
  mutate(
    ASTDT = suppressWarnings({
      len <- sas_length(LBDTC)
      sub10 <- ifelse(!is.na(LBDTC) & len >= 10, substr(LBDTC, 1, 10), NA_character_)
      as.Date(sub10, format = "%Y-%m-%d")
    })
  )
# Ensure Date class (as.Date numeric above may lose class if vectorized via ifelse)
adlb_draft$ASTDT <- as.Date(adlb_draft$ASTDT, origin = "1970-01-01")

# Analysis Relative Day (ASTDY)
adlb_draft <- adlb_draft %>%
  mutate(
    ASTDY = ifelse(!is.na(ASTDT) & !is.na(TRTSDT),
             ifelse(ASTDT >= TRTSDT,
                    as.numeric(ASTDT - TRTSDT) + 1,
                    as.numeric(ASTDT - TRTSDT)),
             NA_real_)
  )

# keep STUDYID USUBJID TRT01A TRTSDT TRTEDT PARAMCD PARAM AVAL AVISIT AVISITN ASTDT ASTDY LBSTRESU SRCDOM SRCVAR SRCSEQ
adlb_draft <- adlb_draft %>%
  select(any_of(c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL",
                  "AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ")))

# 2. IDENTIFY BASELINE RECORD
# where ASTDT <= TRTSDT and not missing(AVAL); SAS missing = -Inf
adlb_draft_num <- adlb_draft %>%
  mutate(
    .ast_n = ifelse(is.na(ASTDT), -Inf, as.numeric(ASTDT)),
    .trt_n = ifelse(is.na(TRTSDT), -Inf, as.numeric(TRTSDT))
  )
base_candidates <- adlb_draft_num %>%
  filter(.ast_n <= .trt_n & !is.na(AVAL)) %>%
  select(-.ast_n, -.trt_n)

# sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ (missing first)
# Use base order with na.last=FALSE, character ordering: use xtfrm? base order is fine (uppercase codes)
ord <- order(base_candidates$USUBJID, base_candidates$PARAMCD,
             base_candidates$ASTDT, base_candidates$AVISITN, base_candidates$SRCSEQ,
             na.last = FALSE, method = "radix")
base_candidates <- base_candidates[ord, , drop = FALSE]

# last.PARAMCD per USUBJID PARAMCD
# Need grouping with SAS semantics: missing PARAMCD groups together; USUBJID never missing
# Use dplyr: arrange already, then group and take last
base_flags <- base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  arrange(ASTDT, AVISITN, SRCSEQ, .by_group = TRUE) %>%
  # Ensure NA first within group (arrange puts NA last by default in dplyr? force)
  # Re-order explicitly: NA first -> use order per group? Simpler: already ordered globally with NA first,
  # dplyr arrange above would reorder with NA last, so avoid. Use slice with global order:
  slice(n()) %>%
  ungroup() %>%
  # Actually slice(n()) after arrange with NA last would pick wrong record if NA handling differs.
  # Recompute correctly: within each group, pick row with max rank per SAS sort (last in SAS order).
  # Since we already have global SAS order, just take last row per group in that order:
  { invisible(NULL); base_candidates %>% group_by(USUBJID, PARAMCD) %>% filter(row_number() == n()) %>% ungroup() } %>%
  mutate(BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ) %>%
  select(USUBJID, PARAMCD, BASE, BASE_DT, BASE_SEQ)

# The above double-computation: simplify - base_candidates is already SAS-sorted, take last per group
# Re-derive cleanly to avoid arrange NA-last issue:
base_flags <- base_candidates %>%
  group_by(USUBJID, PARAMCD) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(BASE = AVAL, BASE_DT = ASTDT, BASE_SEQ = SRCSEQ) %>%
  select(USUBJID, PARAMCD, BASE, BASE_DT, BASE_SEQ)

# 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE
# proc sort adlb_draft by USUBJID PARAMCD
od <- order(adlb_draft$USUBJID, adlb_draft$PARAMCD, na.last = FALSE, method = "radix")
adlb_draft <- adlb_draft[od, , drop = FALSE]

adlb <- adlb_draft %>%
  left_join(base_flags, by = c("USUBJID", "PARAMCD"))

# Flag exact source record; SAS: if SRCSEQ = BASE_SEQ then ABLFL='Y' else ''
adlb <- adlb %>%
  mutate(
    ABLFL = ifelse(
      (!is.na(SRCSEQ) & !is.na(BASE_SEQ) & SRCSEQ == BASE_SEQ) |
        (is.na(SRCSEQ) & is.na(BASE_SEQ) & !is.na(BASE) | (is.na(SRCSEQ) & is.na(BASE_SEQ) & is.na(BASE) & FALSE)),
      "Y", ""),
    # Above second term handles both-missing case (SAS true). Simplify:
    ABLFL2 = ifelse(is.na(SRCSEQ) & is.na(BASE_SEQ), "Y", ABLFL)
  )
# Correct ABLFL: if both NA -> Y (SAS . = . true), else if one NA -> ""
adlb$ABLFL <- ifelse(is.na(adlb$SRCSEQ) & is.na(adlb$BASE_SEQ), "Y",
               ifelse(!is.na(adlb$SRCSEQ) & !is.na(adlb$BASE_SEQ) & adlb$SRCSEQ == adlb$BASE_SEQ, "Y", ""))
adlb$ABLFL2 <- NULL
# When no baseline (BASE is NA and BASE_SEQ NA) and SRCSEQ not NA -> "" (handled)

# Calculate Change from Baseline
adlb <- adlb %>%
  mutate(CHG = ifelse(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_))

# 5. DERIVE ABNORMALITY INDICATOR (LBNRIND)
adlb <- adlb %>%
  mutate(
    LBNRIND = NA_character_
  )
# vectorized logic
calc_val <- adlb$AVAL
# GLUC conversion
is_gluc <- !is.na(adlb$AVAL) & !is.na(adlb$PARAMCD) & adlb$PARAMCD == "GLUC"
conv_gluc <- is_gluc & !is.na(adlb$LBSTRESU) & toupper(adlb$LBSTRESU) == "MG/DL"
calc_gluc <- adlb$AVAL
calc_gluc[conv_gluc] <- adlb$AVAL[conv_gluc] * 0.0555
lbn_gluc <- rep(NA_character_, nrow(adlb))
lbn_gluc[is_gluc & calc_gluc < 3.9] <- "LOW"
lbn_gluc[is_gluc & calc_gluc > 5.6] <- "HIGH"
lbn_gluc[is_gluc & calc_gluc >= 3.9 & calc_gluc <= 5.6] <- "NORMAL"

is_hgb <- !is.na(adlb$AVAL) & !is.na(adlb$PARAMCD) & adlb$PARAMCD == "HGB"
conv_hgb <- is_hgb & !is.na(adlb$LBSTRESU) & toupper(adlb$LBSTRESU) == "G/DL"
calc_hgb <- adlb$AVAL
calc_hgb[conv_hgb] <- adlb$AVAL[conv_hgb] * 10
lbn_hgb <- rep(NA_character_, nrow(adlb))
lbn_hgb[is_hgb & calc_hgb < 120] <- "LOW"
lbn_hgb[is_hgb & calc_hgb > 175] <- "HIGH"
lbn_hgb[is_hgb & calc_hgb >= 120 & calc_hgb <= 175] <- "NORMAL"

is_other <- !is.na(adlb$AVAL) & !is.na(adlb$PARAMCD) & !(adlb$PARAMCD %in% c("GLUC","HGB"))
# PARAMCD missing but AVAL non-missing? SAS select otherwise -> UNKNOWN (includes missing PARAMCD? select missing goes to otherwise)
is_other2 <- !is.na(adlb$AVAL) & is.na(adlb$PARAMCD)
lbn_other <- rep(NA_character_, nrow(adlb))
lbn_other[is_other | is_other2] <- "UNKNOWN"

adlb$LBNRIND[is_gluc] <- lbn_gluc[is_gluc]
adlb$LBNRIND[is_hgb] <- lbn_hgb[is_hgb]
adlb$LBNRIND[is_other | is_other2] <- "UNKNOWN"
# AVAL missing -> remains NA (SAS missing char '')

# drop _calc_val BASE_DT BASE_SEQ (BASE_DT/BASE_SEQ)
adlb$BASE_DT <- NULL
adlb$BASE_SEQ <- NULL

# Retain order: STUDYID USUBJID PARAMCD PARAM first
final_order <- c("STUDYID","USUBJID","PARAMCD","PARAM","TRT01A","TRTSDT","TRTEDT",
                 "AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ",
                 "BASE","CHG","ABLFL","LBNRIND")
# keep only existing, append any extras
extras <- setdiff(names(adlb), final_order)
final_order <- c(intersect(final_order, names(adlb)), extras)
adlb <- adlb[, final_order, drop = FALSE]

# Re-sort for final presentation by USUBJID PARAMCD ASTDT (missing first)
ord2 <- order(adlb$USUBJID, adlb$PARAMCD, adlb$ASTDT, na.last = FALSE, method = "radix")
adlb <- adlb[ord2, , drop = FALSE]

# Write dataset adlb.csv with dates YYYY-MM-DD, na=""
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: proc print obs=15 vars USUBJID PARAMCD AVISIT ASTDT AVAL LBSTRESU BASE CHG ABLFL LBNRIND
audit_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
audit <- head(adlb, 15)
# ensure all vars exist
for (v in audit_vars) if (!v %in% names(audit)) audit[[v]] <- NA
audit_print <- audit[, audit_vars, drop = FALSE]

# CSV of table body (dates YYYY-MM-DD)
write.csv(audit_print, file.path(out, "P005.csv"), row.names = FALSE, na = "")

# Text rendering with DATE9. for ASTDT
fmt_date9 <- function(x) {
  ifelse(is.na(x), "",
    paste0(sprintf("%02d", as.integer(format(x, "%d"))),
           toupper(format(x, "%b")),
           format(x, "%Y")))
}
fmt_num <- function(x) {
  ifelse(is.na(x), "",
    format(x, scientific = FALSE, trim = TRUE, digits = 15, nsmall = 0))
}
fmt_char <- function(x) ifelse(is.na(x), "", as.character(x))

lines <- c(
  "ADLB Audit - Laboratory Analysis with Clinical Indicators",
  "",
  paste(sprintf("%-12s", audit_vars), collapse = " "),
  paste(sapply(audit_vars, function(v) paste0(strrep("-", min(12, max(nchar(v), 8))))), collapse = " ")
)
for (i in seq_len(nrow(audit_print))) {
  r <- audit_print[i, , drop = FALSE]
  vals <- c(
    sprintf("%-12s", fmt_char(r$USUBJID)),
    sprintf("%-12s", fmt_char(r$PARAMCD)),
    sprintf("%-12s", fmt_char(r$AVISIT)),
    sprintf("%-12s", fmt_date9(r$ASTDT)),
    sprintf("%-12s", fmt_num(r$AVAL)),
    sprintf("%-12s", fmt_char(r$LBSTRESU)),
    sprintf("%-12s", fmt_num(r$BASE)),
    sprintf("%-12s", fmt_num(r$CHG)),
    sprintf("%-12s", fmt_char(r$ABLFL)),
    sprintf("%-12s", fmt_char(r$LBNRIND))
  )
  lines <- c(lines, paste(vals, collapse = " "))
}
lines <- c(lines, "", paste0("N = ", nrow(audit_print), " (first 15 records of ADLB, sorted by USUBJID PARAMCD ASTDT)"))

writeLines(lines, file.path(out, "P005.txt"))
# also write .lst counterpart
writeLines(lines, file.path(out, "P005.lst"))

message("Done. ADLB rows: ", nrow(adlb))
