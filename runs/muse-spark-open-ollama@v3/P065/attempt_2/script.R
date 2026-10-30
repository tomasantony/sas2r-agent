out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_sas <- function(x, digits = 0) {
  if (digits == 0) {
    sign(x) * floor(abs(x) + 0.5)
  } else {
    mult <- 10^digits
    sign(x) * floor(abs(x) * mult + 0.5) / mult
  }
}

find_file <- function(dir, pattern) {
  if (!dir.exists(dir)) return(character(0))
  files <- list.files(dir, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  files[grepl(pattern, files, ignore.case = TRUE)]
}

candidate_dirs_sdtm <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
)
candidate_dirs_adam <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
)
expand_paths <- function(p) {
  c(p,
    sub("^D:", "", p),
    sub("^D:/meta_hackathon", "/tmp", p),
    sub("^D:/meta_hackathon/sas2r-agent", "/opt/sas2r-agent", p)
  )
}
candidate_dirs_sdtm <- unique(unlist(lapply(candidate_dirs_sdtm, expand_paths)))
candidate_dirs_adam <- unique(unlist(lapply(candidate_dirs_adam, expand_paths)))

roots_to_try <- c(
  "D:/meta_hackathon/sas2r-agent",
  "/tmp/sas2r-agent",
  Sys.getenv("ROOT"),
  "."
)
if (out != "") {
  roots_to_try <- c(roots_to_try, dirname(out), dirname(dirname(out)))
}
roots_to_try <- unique(roots_to_try)

search_lb <- function() {
  for (d in candidate_dirs_sdtm) {
    if (dir.exists(d)) {
      f <- find_file(d, "^lb\\.(sas7bdat|xpt|csv)$")
      if (length(f) > 0) return(f[1])
    }
  }
  for (r in roots_to_try) {
    if (dir.exists(r)) {
      f <- find_file(r, "(^|/)lb\\.(sas7bdat|xpt)$")
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}
search_adsl <- function() {
  for (d in candidate_dirs_adam) {
    if (dir.exists(d)) {
      f <- find_file(d, "^adsl\\.(sas7bdat|xpt|csv)$")
      if (length(f) > 0) return(f[1])
    }
  }
  for (r in roots_to_try) {
    if (dir.exists(r)) {
      f <- find_file(r, "(^|/)adsl\\.(sas7bdat|xpt)$")
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}

lb_path <- search_lb()
adsl_path <- search_adsl()

if (is.na(lb_path) || is.na(adsl_path)) {
  all_roots <- c("D:", "D:/", ".", getwd(), dirname(getwd()))
  for (r in all_roots) {
    if (dir.exists(r)) {
      f1 <- list.files(r, pattern = "\\.sas7bdat$|\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      lb_cand <- f1[grepl("lb\\.sas7bdat|lb\\.xpt", f1, ignore.case = TRUE)]
      adsl_cand <- f1[grepl("adsl\\.sas7bdat|adsl\\.xpt", f1, ignore.case = TRUE)]
      if (length(lb_cand) > 0 && is.na(lb_path)) lb_path <- lb_cand[1]
      if (length(adsl_cand) > 0 && is.na(adsl_path)) adsl_path <- adsl_cand[1]
    }
  }
}
if (is.na(lb_path)) {
  if (dir.exists("/data")) {
    f <- find_file("/data", "^lb\\.")
    if (length(f) > 0) lb_path <- f[1]
  }
}
if (is.na(adsl_path)) {
  if (dir.exists("/data")) {
    f <- find_file("/data", "^adsl\\.")
    if (length(f) > 0) adsl_path <- f[1]
  }
}

# Fallback to bench paths seen in log
if (is.na(lb_path) || !file.exists(lb_path)) {
  if (file.exists("/bench/data/derived/c2c/data/sdtm/lb.sas7bdat")) lb_path <- "/bench/data/derived/c2c/data/sdtm/lb.sas7bdat"
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  if (file.exists("/bench/data/derived/c2c/data/adam/adsl.sas7bdat")) adsl_path <- "/bench/data/derived/c2c/data/adam/adsl.sas7bdat"
}

message("lb_path: ", lb_path)
message("adsl_path: ", adsl_path)

stopifnot(!is.na(lb_path), !is.na(adsl_path), file.exists(lb_path), file.exists(adsl_path))

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
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    haven::read_sas(path)
  }
}

lb_raw <- read_sas_file(lb_path)
adsl_raw <- read_sas_file(adsl_path)

lb <- lb_raw
adsl <- adsl_raw

names(lb) <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))

message("LB columns: ", paste(names(lb), collapse=", "))
message("ADSL columns: ", paste(names(adsl), collapse=", "))

# Derive PARAM and AVAL for LB if missing (SDTM LB has LBTEST/LBTESTCD and LBSTRESN)
if (!"PARAM" %in% names(lb)) {
  if ("LBTEST" %in% names(lb)) {
    lb$PARAM <- as.character(lb$LBTEST)
  } else if ("LBTESTCD" %in% names(lb)) {
    lb$PARAM <- as.character(lb$LBTESTCD)
  } else {
    lb$PARAM <- NA_character_
  }
}
if (!"AVAL" %in% names(lb)) {
  if ("LBSTRESN" %in% names(lb)) {
    lb$AVAL <- as.numeric(lb$LBSTRESN)
  } else if ("LBSTRESN" %in% names(lb_raw)) {
    lb$AVAL <- as.numeric(lb$LBSTRESN)
  } else if ("LBORRES" %in% names(lb)) {
    lb$AVAL <- suppressWarnings(as.numeric(lb$LBORRES))
  } else {
    lb$AVAL <- NA_real_
  }
}
# Ensure LBDTC exists
if (!"LBDTC" %in% names(lb)) {
  # try to construct from LBDT or LBDTC variants
  if ("LBDTC" %in% toupper(names(lb_raw))) {
    # already handled
  } else {
    lb$LBDTC <- NA_character_
  }
}

# Derive RFSTDTC for ADSL if missing (use TRTSDT)
if (!"RFSTDTC" %in% names(adsl)) {
  if ("TRTSDT" %in% names(adsl)) {
    # TRTSDT may be Date, numeric SAS date, or character
    trtsdt_col <- adsl$TRTSDT
    if (inherits(trtsdt_col, "Date")) {
      adsl$RFSTDTC <- format(trtsdt_col, "%Y-%m-%d")
    } else if (inherits(trtsdt_col, "POSIXct") || inherits(trtsdt_col, "POSIXt")) {
      adsl$RFSTDTC <- format(as.Date(trtsdt_col), "%Y-%m-%d")
    } else if (is.numeric(trtsdt_col)) {
      # haven already converts SAS dates to Date, but if numeric, treat as SAS date (days since 1960-01-01)
      # Check if values look like SAS dates (e.g., 10000-20000)
      # Convert via as.Date with origin 1960-01-01
      adsl$RFSTDTC <- format(as.Date(trtsdt_col, origin = "1960-01-01"), "%Y-%m-%d")
      # If conversion yields NA or implausible, fallback to character
      # If many NAs, try as character
      if (all(is.na(adsl$RFSTDTC))) {
        adsl$RFSTDTC <- as.character(trtsdt_col)
      }
    } else {
      # character
      # Try to parse and reformat to YYYY-MM-DD
      tmp <- as.character(trtsdt_col)
      # If already YYYY-MM-DD, keep
      # Otherwise try to parse with ymd
      parsed <- suppressWarnings(lubridate::ymd(tmp, quiet = TRUE))
      # lubridate ymd handles YYYY-MM-DD
      adsl$RFSTDTC <- ifelse(!is.na(parsed), format(parsed, "%Y-%m-%d"), tmp)
      adsl$RFSTDTC[is.na(tmp) | tmp == ""] <- NA_character_
    }
  } else if ("RFSTDTC" %in% names(adsl)) {
    # nothing
  } else {
    adsl$RFSTDTC <- NA_character_
  }
}

# Ensure TRT01P exists (it does per log)
if (!"TRT01P" %in% names(adsl)) {
  adsl$TRT01P <- NA_character_
}

stopifnot("USUBJID" %in% names(lb))
stopifnot("PARAM" %in% names(lb))
stopifnot("LBDTC" %in% names(lb))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("RFSTDTC" %in% names(adsl))

adsl_qc <- adsl %>%
  select(USUBJID, RFSTDTC, TRT01P) %>%
  arrange(USUBJID)

lb_qc <- lb %>%
  arrange(USUBJID, PARAM, LBDTC)

overlap <- intersect(names(lb_qc), names(adsl_qc))
overlap <- setdiff(overlap, "USUBJID")
if (length(overlap) > 0) {
  lb_qc <- lb_qc %>% select(-all_of(overlap))
}

merged <- inner_join(lb_qc, adsl_qc, by = "USUBJID")

stopifnot(nrow(merged) > 0)
stopifnot(all(c("LBDTC","RFSTDTC","AVAL") %in% names(merged)))

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
  )

merged <- merged %>%
  arrange(USUBJID, PARAM, LBDTC)

merged$BASE <- NA_real_
merged$ABLFL <- ""
merged$CHG <- NA_real_
merged$AVAL <- as.numeric(merged$AVAL)

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

  if (cond_pre && !is.na(aval_i)) {
    BASE <- aval_i
  }

  aval_eq_base <- FALSE
  if (cond_pre) {
    if (is.na(aval_i) && is.na(BASE)) {
      aval_eq_base <- TRUE
    } else if (!is.na(aval_i) && !is.na(BASE) && aval_i == BASE) {
      aval_eq_base <- TRUE
    } else {
      aval_eq_base <- FALSE
    }
  }
  if (aval_eq_base) {
    merged_ordered$ABLFL[i] <- "Y"
  } else {
    merged_ordered$ABLFL[i] <- ""
  }

  merged_ordered$BASE[i] <- BASE

  if (!is.na(BASE)) {
    if (!is.na(aval_i)) {
      merged_ordered$CHG[i] <- aval_i - BASE
    } else {
      merged_ordered$CHG[i] <- NA_real_
    }
  } else {
    merged_ordered$CHG[i] <- NA_real_
  }
}

adlb_qc <- merged_ordered %>%
  select(-lbdt, -rfstdt)

names(adlb_qc) <- toupper(names(adlb_qc))

for (col in names(adlb_qc)) {
  if (inherits(adlb_qc[[col]], "Date")) {
    adlb_qc[[col]] <- format(adlb_qc[[col]], "%Y-%m-%d")
  } else if (inherits(adlb_qc[[col]], "POSIXct") || inherits(adlb_qc[[col]], "POSIXt")) {
    adlb_qc[[col]] <- format(adlb_qc[[col]], "%Y-%m-%dT%H:%M:%S")
  }
}

write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")

message("Wrote adlb_qc.csv with ", nrow(adlb_qc), " rows and ", ncol(adlb_qc), " cols")
