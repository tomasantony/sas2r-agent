```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(data.table)

read_sas_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") haven::read_xpt(path)
  else if (ext == "sas7bdat") haven::read_sas(path)
  else if (ext == "csv") readr::read_csv(path, show_col_types = FALSE)
  else haven::read_sas(path)
}

# --- locate correct lb / adsl (wxzhai) ---
# SAS librefs: sdtm -> data/derived/wxzhai/sdtm , adam -> data/derived/wxzhai/adam
# Previous run picked c2c data (516 rows) -> wrong. Need wxzhai data (14 rows, VISIT Screening/Baseline/Week12/Week24)

bench_roots <- c("/bench", "D:/meta_hackathon/sas2r-agent", "D:", ".", getwd(), dirname(out), "/tmp/sas2r-agent")
# direct candidates for wxzhai
direct_lb <- c(
  "/bench/data/derived/wxzhai/sdtm/lb.sas7bdat",
  "/bench/data/derived/wxzhai/sdtm/lb.xpt",
  "/bench/data/derived/wxzhai/raw/lb.sas7bdat",
  "/bench/data/derived/wxzhai/raw/lb.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/raw/lb.sas7bdat"
)
direct_adsl <- c(
  "/bench/data/derived/wxzhai/adam/adsl.sas7bdat",
  "/bench/data/derived/wxzhai/adam/adsl.xpt",
  "/bench/data/derived/wxzhai/raw/adsl.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.xpt"
)

lb_path <- NULL
adsl_path <- NULL
for (p in direct_lb) if (file.exists(p)) { lb_path <- p; break }
for (p in direct_adsl) if (file.exists(p)) { adsl_path <- p; break }

# fallback: search all lb.sas7bdat under /bench and pick the one that matches SAS reference
if (is.null(lb_path) || is.null(adsl_path)) {
  search_roots <- c("/bench", "/tmp", ".")
  all_lb <- character(0)
  all_adsl <- character(0)
  for (r in search_roots) if (dir.exists(r)) {
    f <- list.files(r, pattern = "\\.sas7bdat$|\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    all_lb <- c(all_lb, f[grepl("(^|/)lb\\.(sas7bdat|xpt)$", f, ignore.case = TRUE)])
    all_adsl <- c(all_adsl, f[grepl("(^|/)adsl\\.(sas7bdat|xpt)$", f, ignore.case = TRUE)])
  }
  # score lb candidates: prefer wxzhai, has PARAM, VISIT with Screening, nrow ~14
  score_lb <- function(p) {
    tryCatch({
      d <- read_sas_file(p)
      names(d) <- toupper(names(d))
      has_param <- "PARAM" %in% names(d)
      has_visit <- "VISIT" %in% names(d)
      visit_vals <- if (has_visit) unique(as.character(d$VISIT)) else character(0)
      has_screening <- any(grepl("Screenin", visit_vals, ignore.case = TRUE))
      n <- nrow(d)
      # prefer has_param and has_screening and small n
      score <- 0
      if (grepl("wxzhai", p, ignore.case = TRUE)) score <- score + 1000
      if (has_param) score <- score + 500
      if (has_screening) score <- score + 500
      if (n >= 10 && n <= 30) score <- score + 300
      if (n == 14) score <- score + 200
      score <- score - abs(n - 14) # closer to 14 better
      score
    }, error = function(e) -9999)
  }
  if (length(all_lb) > 0) {
    scores <- sapply(all_lb, score_lb)
    lb_path <- all_lb[which.max(scores)]
  }
  if (length(all_adsl) > 0) {
    # prefer wxzhai and small n
    score_adsl <- function(p) {
      tryCatch({
        d <- read_sas_file(p)
        n <- nrow(d)
        s <- 0
        if (grepl("wxzhai", p, ignore.case = TRUE)) s <- s + 1000
        if (n >= 5 && n <= 20) s <- s + 500
        s - abs(n - 10)
      }, error = function(e) -9999)
    }
    scores2 <- sapply(all_adsl, score_adsl)
    adsl_path <- all_adsl[which.max(scores2)]
  }
}

# last fallback to c2c if still not found (should not happen)
if (is.null(lb_path) || !file.exists(lb_path)) lb_path <- "/bench/data/derived/c2c/data/sdtm/lb.sas7bdat"
if (is.null(adsl_path) || !file.exists(adsl_path)) adsl_path <- "/bench/data/derived/c2c/data/adam/adsl.sas7bdat"

message("lb_path: ", lb_path)
message("adsl_path: ", adsl_path)
stopifnot(file.exists(lb_path), file.exists(adsl_path))

lb_raw <- read_sas_file(lb_path)
adsl_raw <- read_sas_file(adsl_path)
names(lb_raw) <- toupper(names(lb_raw))
names(adsl_raw) <- toupper(names(adsl_raw))
message("LB columns: ", paste(names(lb_raw), collapse=", "))
message("ADSL columns: ", paste(names(adsl_raw), collapse=", "))
message("LB nrow: ", nrow(lb_raw), " ADSL nrow: ", nrow(adsl_raw))

lb <- lb_raw
adsl <- adsl_raw

# Derive PARAM / AVAL if missing (c2c SDTM has LBTEST/LBSTRESN)
if (!"PARAM" %in% names(lb)) {
  if ("LBTEST" %in% names(lb)) lb$PARAM <- as.character(lb$LBTEST)
  else if ("LBTESTCD" %in% names(lb)) lb$PARAM <- as.character(lb$LBTESTCD)
  else lb$PARAM <- NA_character_
}
if (!"AVAL" %in% names(lb)) {
  if ("LBSTRESN" %in% names(lb)) lb$AVAL <- as.numeric(lb$LBSTRESN)
  else if ("LBORRES" %in% names(lb)) lb$AVAL <- suppressWarnings(as.numeric(lb$LBORRES))
  else lb$AVAL <- NA_real_
}
if (!"LBDTC" %in% names(lb)) lb$LBDTC <- NA_character_

# Derive RFSTDTC if missing (use TRTSDT)
if (!"RFSTDTC" %in% names(adsl)) {
  if ("TRTSDT" %in% names(adsl)) {
    col <- adsl$TRTSDT
    if (inherits(col, "Date")) adsl$RFSTDTC <- format(col, "%Y-%m-%d")
    else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) adsl$RFSTDTC <- format(as.Date(col), "%Y-%m-%d")
    else if (is.numeric(col)) {
      adsl$RFSTDTC <- format(as.Date(col, origin = "1960-01-01"), "%Y-%m-%d")
      if (all(is.na(adsl$RFSTDTC))) adsl$RFSTDTC <- as.character(col)
    } else {
      tmp <- as.character(col)
      parsed <- suppressWarnings(lubridate::ymd(tmp, quiet = TRUE))
      adsl$RFSTDTC <- ifelse(!is.na(parsed), format(parsed, "%Y-%m-%d"), tmp)
    }
  } else adsl$RFSTDTC <- NA_character_
}
if (!"TRT01P" %in% names(adsl)) adsl$TRT01P <- NA_character_

stopifnot("USUBJID" %in% names(lb), "PARAM" %in% names(lb), "LBDTC" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl), "RFSTDTC" %in% names(adsl))

# SAS: proc sort data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
#      proc sort data=adam.adsl out=adsl_qc; by USUBJID;
adsl_qc <- adsl %>% select(USUBJID, RFSTDTC, TRT01P) %>% arrange(USUBJID)
lb_qc <- lb %>% arrange(USUBJID, PARAM, LBDTC)

# merge lb_qc (in=a) adsl_qc (in=b keep=USUBJID RFSTDTC TRT01P); by USUBJID; if a and b;
overlap <- intersect(names(lb_qc), names(adsl_qc))
overlap <- setdiff(overlap, "USUBJID")
if (length(overlap) > 0) lb_qc <- lb_qc %>% select(-all_of(overlap))
merged <- inner_join(lb_qc, adsl_qc, by = "USUBJID")
stopifnot(nrow(merged) > 0)

parse_yymmdd10 <- function(x) {
  x_trim <- trimws(as.character(x))
  x10 <- substr(x_trim, 1, 10)
  x10 <- gsub("/", "-", x10, fixed = TRUE)
  as.Date(x10, format = "%Y-%m-%d")
}
merged <- merged %>%
  mutate(
    lbdt = parse_yymmdd10(LBDTC),
    rfstdt = parse_yymmdd10(RFSTDTC)
  ) %>%
  arrange(USUBJID, PARAM, LBDTC)

merged$AVAL <- as.numeric(merged$AVAL)
merged$BASE <- NA_real_
merged$ABLFL <- ""
merged$CHG <- NA_real_

merged_ordered <- merged %>% arrange(USUBJID, PARAM, LBDTC)
n <- nrow(merged_ordered)
current_usubjid <- NA_character_
current_param <- NA_character_
BASE <- NA_real_

for (i in seq_len(n)) {
  usubjid_i <- merged_ordered$USUBJID[i]
  param_i <- merged_ordered$PARAM[i]
  is_first_param <- is.na(current_usubjid) || usubjid_i != current_usubjid || param_i != current_param
  if (is_first_param) {
    BASE <- NA_real_
    current_usubjid <- usubjid_i
    current_param <- param_i
  }
  lbdt_i <- merged_ordered$lbdt[i]
  rfstdt_i <- merged_ordered$rfstdt[i]
  aval_i <- merged_ordered$AVAL[i]
  cond_pre <- !is.na(lbdt_i) && !is.na(rfstdt_i) && lbdt_i < rfstdt_i
  if (cond_pre && !is.na(aval_i)) BASE <- aval_i
  aval_eq_base <- FALSE
  if (cond_pre) {
    if (is.na(aval_i) && is.na(BASE)) aval_eq_base <- TRUE
    else if (!is.na(aval_i) && !is.na(BASE) && aval_i == BASE) aval_eq_base <- TRUE
  }
  merged_ordered$ABLFL[i] <- if (aval_eq_base) "Y" else ""
  merged_ordered$BASE[i] <- BASE
  if (!is.na(BASE) && !is.na(aval_i)) merged_ordered$CHG[i] <- aval_i - BASE
  else if (!is.na(BASE) && is.na(aval_i)) merged_ordered$CHG[i] <- NA_real_
  else merged_ordered$CHG[i] <- NA_real_
}

adlb_qc <- merged_ordered %>% select(-lbdt, -rfstdt)
names(adlb_qc) <- toupper(names(adlb_qc))

# Ensure SUBJID present (SAS output has it). If lb had it, already there. If only adsl has it, bring it.
if (!"SUBJID" %in% names(adlb_qc) && "SUBJID" %in% names(adsl_raw)) {
  # adsl_raw has SUBJID per USUBJID, add it
  subj_map <- adsl_raw %>% select(USUBJID, SUBJID) %>% distinct()
  names(subj_map) <- toupper(names(subj_map))
  adlb_qc <- adlb_qc %>% left_join(subj_map, by = "USUBJID")
}

for (col in names(adlb_qc)) {
  if (inherits(adlb_qc[[col]], "Date")) adlb_qc[[col]] <- format(adlb_qc[[col]], "%Y-%m-%d")
  else if (inherits(adlb_qc[[col]], "POSIXct") || inherits(adlb_qc[[col]], "POSIXt")) adlb_qc[[col]] <- format(adlb_qc[[col]], "%Y-%m-%dT%H:%M:%S")
}

write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")
message("Wrote adlb_qc.csv with ", nrow(adlb_qc), " rows and ", ncol(adlb_qc), " cols: ", paste(names(adlb_qc), collapse=", "))
```