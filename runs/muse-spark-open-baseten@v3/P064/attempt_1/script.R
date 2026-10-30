# P064_adlb.sas -> R translation
# Purpose: Create ADaM ADLB dataset
# Note: Baseline = last non-missing pre-dose value per USUBJID/PARAM

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: parse yymmdd10. strictly (first 10 chars YYYY-MM-DD)
parse_yymmdd10 <- function(x) {
  # x is character; SAS yymmdd10. reads YYYY-MM-DD
  # Use substr 1,10 then as.Date with %Y-%m-%d
  # haven labelled -> convert to character
  if (is.null(x)) return(as.Date(character()))
  # convert labelled to character if needed
  if (inherits(x, "haven_labelled")) x <- as.character(x)
  x <- as.character(x)
  # trim
  x <- trimws(x)
  # substr 1,10
  x10 <- substr(x, 1, 10)
  # strict parse
  suppressWarnings(as.Date(x10, format = "%Y-%m-%d"))
}

# helper: SAS missing comparison for numeric dates
# SAS: . < . is false, . < non-missing is true, non-missing < . is false
sas_lt <- function(a, b) {
  # a,b are Date or numeric (with NA as missing)
  # return logical vector, FALSE where SAS would be false/missing
  # In SAS data step IF condition, missing is false (0)
  # So we return TRUE only when SAS condition true
  res <- rep(FALSE, length(a))
  for (i in seq_along(a)) {
    ai <- a[i]; bi <- b[i]
    if (is.na(ai) && is.na(bi)) res[i] <- FALSE
    else if (is.na(ai) && !is.na(bi)) res[i] <- TRUE
    else if (!is.na(ai) && is.na(bi)) res[i] <- FALSE
    else res[i] <- ai < bi
  }
  res
}

# helper: find column name case-insensitive
find_col <- function(df, target) {
  nms <- names(df)
  idx <- which(toupper(nms) == toupper(target))
  if (length(idx) == 0) return(NA_character_)
  nms[idx[1]]
}

# helper: locate file
locate_file <- function(dir, name) {
  # name without extension, lower
  if (!dir.exists(dir)) return(NA_character_)
  files <- list.files(dir, recursive = FALSE, full.names = TRUE)
  # case-insensitive match
  pat <- paste0("^", name, "\\.(sas7bdat|xpt|sas7bdat\\.gz|xpt\\.gz|csv)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) > 0) return(files[idx[1]])
  # also try without extension exact
  idx2 <- which(toupper(basename(files)) == toupper(name))
  if (length(idx2) > 0) return(files[idx2[1]])
  # try listing subdirs one level
  files2 <- list.files(dir, recursive = TRUE, full.names = TRUE)
  idx3 <- grep(pat, basename(files2), ignore.case = TRUE)
  if (length(idx3) > 0) return(files2[idx3[1]])
  NA_character_
}

read_sas_dataset <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  # handle .gz double ext
  if (grepl("\\.xpt\\.gz$", tolower(path))) ext <- "xpt"
  if (grepl("\\.sas7bdat\\.gz$", tolower(path))) ext <- "sas7bdat"
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else {
    # try xpt then sas
    tryCatch(haven::read_xpt(path), error = function(e) haven::read_sas(path))
  }
}

# ------------------------------------------------------------------
# Locate inputs
# librefs in SAS:
#   sdtm -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm
#   adam -> D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam
# Also macro vars: &rawadam., &rawsdtm., &rawsplit., &rawdir.

candidate_sdtm_dirs <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm",
  Sys.getenv("rawsdtm"),
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm"
)
candidate_adam_dirs <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  Sys.getenv("rawadam"),
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split",
  Sys.getenv("rawsplit")
)

lb_path <- NA_character_
for (d in candidate_sdtm_dirs) {
  if (is.na(d) || d == "") next
  p <- locate_file(d, "lb")
  if (!is.na(p)) { lb_path <- p; break }
}
# fallback: search raw dir
if (is.na(lb_path)) {
  rawdir <- Sys.getenv("rawdir")
  if (!is.na(rawdir) && rawdir != "" && dir.exists(rawdir)) {
    p <- locate_file(rawdir, "lb")
    if (!is.na(p)) lb_path <- p
  }
}
# last fallback: any lb in derived wxzhai
if (is.na(lb_path)) {
  all_candidates <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^lb\\.(xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(all_candidates) > 0) lb_path <- all_candidates[1]
}

adsl_path <- NA_character_
for (d in candidate_adam_dirs) {
  if (is.na(d) || d == "") next
  p <- locate_file(d, "adsl")
  if (!is.na(p)) { adsl_path <- p; break }
}
if (is.na(adsl_path)) {
  rawdir <- Sys.getenv("rawdir")
  if (!is.na(rawdir) && rawdir != "" && dir.exists(rawdir)) {
    p <- locate_file(rawdir, "adsl")
    if (!is.na(p)) adsl_path <- p
  }
}
if (is.na(adsl_path)) {
  all_candidates <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^adsl\\.(xpt|sas7bdat)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(all_candidates) > 0) adsl_path <- all_candidates[1]
}

# If still not found, try explicit absolute paths with forward slashes
if (is.na(lb_path) || !file.exists(lb_path)) {
  try_paths <- c(
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.sas7bdat.gz",
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.xpt.gz"
  )
  for (p in try_paths) if (file.exists(p)) { lb_path <- p; break }
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  try_paths <- c(
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.sas7bdat.gz",
    "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.xpt.gz"
  )
  for (p in try_paths) if (file.exists(p)) { adsl_path <- p; break }
}

if (is.na(lb_path) || !file.exists(lb_path)) stop(paste0("LB dataset not found. Searched: ", paste(candidate_sdtm_dirs, collapse="; ")))
if (is.na(adsl_path) || !file.exists(adsl_path)) stop(paste0("ADSL dataset not found. Searched: ", paste(candidate_adam_dirs, collapse="; ")))

lb <- read_sas_dataset(lb_path)
adsl <- read_sas_dataset(adsl_path)

# Convert haven_labelled to appropriate types but keep names
# haven::read_xpt already gives tibble; convert labelled to numeric/character via as_factor? Keep numeric.
# For character labelled, convert to character
for (nm in names(lb)) {
  if (inherits(lb[[nm]], "haven_labelled")) {
    # if numeric labelled, keep numeric values
    # haven_labelled is numeric with labels; as.numeric preserves
    # Use zap_labels
    lb[[nm]] <- haven::zap_labels(lb[[nm]])
    # zap_formats as well
    lb[[nm]] <- haven::zap_formats(lb[[nm]])
  }
}
for (nm in names(adsl)) {
  if (inherits(adsl[[nm]], "haven_labelled")) {
    adsl[[nm]] <- haven::zap_labels(adsl[[nm]])
    adsl[[nm]] <- haven::zap_formats(adsl[[nm]])
  }
}

# Ensure column names are as in SAS (preserve case, but find needed cols case-insensitive)
col_USUBJID_lb <- find_col(lb, "USUBJID")
col_PARAM_lb   <- find_col(lb, "PARAM")
col_LBDTC_lb   <- find_col(lb, "LBDTC")
col_AVAL_lb    <- find_col(lb, "AVAL")

col_USUBJID_adsl <- find_col(adsl, "USUBJID")
col_TRT01P_adsl  <- find_col(adsl, "TRT01P")
col_RFSTDTC_adsl <- find_col(adsl, "RFSTDTC")

stopifnot(!is.na(col_USUBJID_lb), !is.na(col_PARAM_lb), !is.na(col_LBDTC_lb), !is.na(col_AVAL_lb))
stopifnot(!is.na(col_USUBJID_adsl), !is.na(col_TRT01P_adsl), !is.na(col_RFSTDTC_adsl))

# Standardize key column names to uppercase for join (but keep original names for output)
# Rename to canonical uppercase for processing, then restore
names(lb)[names(lb) == col_USUBJID_lb] <- "USUBJID"
names(lb)[names(lb) == col_PARAM_lb]   <- "PARAM"
names(lb)[names(lb) == col_LBDTC_lb]   <- "LBDTC"
names(lb)[names(lb) == col_AVAL_lb]    <- "AVAL"

names(adsl)[names(adsl) == col_USUBJID_adsl] <- "USUBJID"
names(adsl)[names(adsl) == col_TRT01P_adsl]  <- "TRT01P"
names(adsl)[names(adsl) == col_RFSTDTC_adsl] <- "RFSTDTC"

# Keep only needed columns from adsl (as SAS keep=)
adsl_keep <- adsl %>% select(USUBJID, TRT01P, RFSTDTC)

# Drop/rename overlapping columns except keys before merge (avoid .x/.y)
# SAS merge keeps all from lb plus TRT01P,RFSTDTC from adsl; if lb also has TRT01P, SAS would overwrite with adsl's? In SAS merge by USUBJID, overlapping vars from second dataset overwrite first.
# To emulate, drop overlapping non-key cols from lb that are in adsl_keep except USUBJID
overlap <- intersect(names(lb), names(adsl_keep))
overlap <- setdiff(overlap, "USUBJID")
if (length(overlap) > 0) {
  lb <- lb %>% select(-all_of(overlap))
}

# Inner join (if a and b)
merged <- inner_join(lb, adsl_keep, by = "USUBJID")

stopifnot(nrow(merged) > 0)
stopifnot(all(c("USUBJID","PARAM","LBDTC","AVAL","RFSTDTC") %in% names(merged)))

# Sort by USUBJID PARAM LBDTC (SAS character ordering, missing first)
# Use order with na.last=FALSE to put NA first (SAS missing first)
# For character, order is byte-wise; R's order is similar
merged <- merged %>%
  arrange(USUBJID, PARAM, LBDTC)

# Parse dates strictly yymmdd10.
merged <- merged %>%
  mutate(
    LBDTC_dt = parse_yymmdd10(LBDTC),
    RFSTDTC_dt = parse_yymmdd10(RFSTDTC)
  )

# Implement retain BASE logic per USUBJID+PARAM
# SAS: retain BASE; if first.PARAM then BASE=.; if LBDTC_dt < RFSTDTC_dt and not missing AVAL then BASE=AVAL
# Then ABLFL and CHG

# Ensure AVAL is numeric
merged$AVAL <- as.numeric(merged$AVAL)

# Process by groups
# Use data.table or dplyr group_modify with loop to retain correctly
merged <- merged %>%
  group_by(USUBJID, PARAM) %>%
  arrange(LBDTC, .by_group = TRUE) %>%
  mutate(
    # placeholder, will fill via loop
    BASE = NA_real_,
    ABLFL = "",
    CHG = NA_real_
  ) %>%
  ungroup()

# Loop per group to emulate retain
# Need to respect original sorted order within group (already sorted)
# We'll iterate row-wise
# Create index order as current row order
merged$.__rowid <- seq_len(nrow(merged))

# Split by USUBJID+PARAM
grp_keys <- merged %>% distinct(USUBJID, PARAM)
# For efficiency, use data.table approach: iterate groups
# Convert to data.frame for loop
merged_df <- as.data.frame(merged, stringsAsFactors = FALSE)

# Ensure sorted by USUBJID, PARAM, LBDTC, __rowid
merged_df <- merged_df[order(merged_df$USUBJID, merged_df$PARAM, merged_df$LBDTC, merged_df$.__rowid, na.last = FALSE), ]

# Initialize BASE retain variable
# We'll process sequentially, resetting at first.PARAM
# first.PARAM is first row of each USUBJID+PARAM group
merged_df$BASE <- NA_real_
merged_df$ABLFL <- ""
merged_df$CHG <- NA_real_

# Get group boundaries
# Use rle on paste USUBJID PARAM
grp_id <- paste0(merged_df$USUBJID, "||", merged_df$PARAM)
r <- rle(grp_id)
starts <- cumsum(c(1, head(r$lengths, -1)))
ends <- cumsum(r$lengths)

for (g in seq_along(r$lengths)) {
  s <- starts[g]; e <- ends[g]
  BASE <- NA_real_
  for (i in s:e) {
    # if first.PARAM then BASE = .
    if (i == s) BASE <- NA_real_
    # condition: LBDTC_dt < RFSTDTC_dt and not missing AVAL
    cond_pre <- sas_lt(merged_df$LBDTC_dt[i], merged_df$RFSTDTC_dt[i])
    # not missing AVAL
    if (!is.na(cond_pre) && cond_pre && !is.na(merged_df$AVAL[i])) {
      BASE <- merged_df$AVAL[i]
    }
    merged_df$BASE[i] <- BASE
    # Baseline flag: if LBDTC_dt < RFSTDTC_dt and AVAL == BASE then Y else ""
    # SAS equality: missing == missing is true
    cond_flag_pre <- sas_lt(merged_df$LBDTC_dt[i], merged_df$RFSTDTC_dt[i])
    if (!is.na(cond_flag_pre) && cond_flag_pre) {
      aval <- merged_df$AVAL[i]
      # SAS equality
      eq <- FALSE
      if (is.na(aval) && is.na(BASE)) eq <- TRUE
      else if (!is.na(aval) && !is.na(BASE) && aval == BASE) eq <- TRUE
      else eq <- FALSE
      if (eq) merged_df$ABLFL[i] <- "Y" else merged_df$ABLFL[i] <- ""
    } else {
      merged_df$ABLFL[i] <- ""
    }
    # Change from baseline
    if (!is.na(merged_df$AVAL[i]) && !is.na(BASE)) {
      merged_df$CHG[i] <- merged_df$AVAL[i] - BASE
    } else if (is.na(merged_df$AVAL[i]) || is.na(BASE)) {
      # SAS: . - . = ., n - . = ., . - n = .
      merged_df$CHG[i] <- NA_real_
      # If both missing? Still NA
      # If AVAL missing and BASE missing, CHG missing
    }
  }
}

# Drop helper columns LBDTC RFSTDTC (original character) and temp dates and rowid
# SAS drops LBDTC RFSTDTC (the original character vars)
# Keep BASE, ABLFL, CHG plus all other vars from lb and TRT01P
# Remove LBDTC, RFSTDTC, LBDTC_dt, RFSTDTC_dt, __rowid
adlb <- merged_df %>%
  select(-LBDTC, -RFSTDTC, -LBDTC_dt, -RFSTDTC_dt, -.__rowid)

# Ensure column order: original lb columns (minus dropped) + TRT01P + BASE, ABLFL, CHG at end (as SAS would append)
# SAS retains order of lb then adsl vars, then new vars in order created: BASE, ABLFL, CHG
# Our adlb already has that order except BASE,ABLFL,CHG are at end

# Format dates in remaining columns as YYYY-MM-DD if Date, or YYYY-MM-DDTHH:MM:SS if POSIXt
# Identify Date/POSIXt columns
for (nm in names(adlb)) {
  if (inherits(adlb[[nm]], "Date")) {
    adlb[[nm]] <- format(adlb[[nm]], "%Y-%m-%d")
  } else if (inherits(adlb[[nm]], "POSIXt")) {
    adlb[[nm]] <- format(adlb[[nm]], "%Y-%m-%dT%H:%M:%S")
  } else if (inherits(adlb[[nm]], "haven_labelled")) {
    adlb[[nm]] <- haven::zap_labels(adlb[[nm]])
  }
}

# Ensure character NAs are empty strings on write (write.csv na="")
# Write dataset
write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# Also ensure no leftover temp files
invisible(NULL)
