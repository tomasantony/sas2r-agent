```r
# P006 adam_adsl.sas -> R translation
# ADSL derivation from SDTM DM + EX, audit listing
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"

cat("P006 start\n")

# ---- helpers to mimic SAS ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x), which = "both") == ""
}
sas_length <- function(x) {
  # LENGTH strips trailing blanks; NA -> 0
  s <- as.character(x)
  outv <- rep(0L, length(s))
  idx <- !is.na(s)
  # remove trailing blanks only
  outv[idx] <- nchar(sub(" +$", "", s[idx], perl = TRUE))
  outv
}
sas_substr <- function(x, start, len) {
  substr(as.character(x), start, start + len - 1L)
}
sas_input_yymmdd10 <- function(x) {
  # input(substr(x,1,10), yymmdd10.) : strict %Y-%m-%d
  s <- as.character(x)
  res <- as.Date(rep(NA_character_, length(s)))
  idx <- !is.na(s) & sas_length(s) >= 10L
  if (any(idx)) {
    sub10 <- sas_substr(s[idx], 1L, 10L)
    res[idx] <- as.Date(sub10, format = "%Y-%m-%d")
  }
  res
}
sas_age_completed <- function(brthdt, rficdt) {
  # floor(yrdif(brthdt, rficdt, 'AGE')) = completed years
  n <- length(brthdt)
  res <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    b <- brthdt[i]; r <- rficdt[i]
    if (is.na(b) || is.na(r)) { res[i] <- NA_real_; next }
    blt <- as.POSIXlt(b); rlt <- as.POSIXlt(r)
    by <- blt$year + 1900L; ry <- rlt$year + 1900L
    bm <- blt$mon + 1L; rm <- rlt$mon + 1L
    bd <- blt$mday; rd <- rlt$mday
    adj <- ifelse(rm < bm | (rm == bm & rd < bd), 1L, 0L)
    res[i] <- (ry - by) - adj
  }
  res
}
zap_haven <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) {
      # keep underlying values, drop labels
      x <- haven::as_vector(x)
    }
    if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
      x <- as.character(x)
    }
    if (is.factor(x)) x <- as.character(x)
    df[[nm]] <- x
  }
  names(df) <- toupper(names(df))
  df
}
find_mem_file <- function(libdir, mem) {
  # search case-insensitive for mem with known extensions
  if (!dir.exists(libdir)) return(NA_character_)
  files <- list.files(libdir, full.names = TRUE, recursive = FALSE)
  if (length(files) == 0) return(NA_character_)
  low_mem <- tolower(mem)
  # map lower basename without ext
  for (f in files) {
    bn <- basename(f)
    # strip extension
    base_noext <- tolower(sub("\\.[^.]*$", "", bn))
    ext <- tolower(sub("^.*\\.([^.]*)$", "\\1", bn))
    if (base_noext == low_mem && ext %in% c("sas7bdat", "xpt", "xptv5", "csv", "sas7bcat")) {
      return(f)
    }
  }
  # fallback: any file whose basename starts with mem (case-insensitive)
  for (f in files) {
    if (grepl(paste0("^", low_mem, "\\."), tolower(basename(f)))) return(f)
  }
  return(NA_character_)
}
read_libmem <- function(libdir, mem) {
  f <- find_mem_file(libdir, mem)
  cat(paste0("Reading ", mem, " from ", libdir, " -> ", ifelse(is.na(f), "NOT FOUND", f), "\n"))
  if (is.na(f)) stop(paste0("Dataset not found: ", libdir, "/", mem))
  ext <- tolower(sub("^.*\\.([^.]*)$", "\\1", f))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext %in% c("xpt", "xptv5")) {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- read.csv(f, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  } else {
    # try sas then xpt
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) { df <<- haven::read_xpt(f) })
  }
  df <- zap_haven(df)
  df
}

# ---- 1. read SDTM DM and EX ----
dm <- read_libmem(sdtm_dir, "dm")
ex <- read_libmem(sdtm_dir, "ex")

cat(paste0("DM rows=", nrow(dm), " cols=", paste(names(dm), collapse=","), "\n"))
cat(paste0("EX rows=", nrow(ex), " cols=", paste(names(ex), collapse=","), "\n"))

stopifnot(all(c("USUBJID") %in% names(dm)))
stopifnot(all(c("USUBJID") %in% names(ex)))
# required DM vars (check existence, warn if missing but continue)
need_dm <- c("STUDYID","USUBJID","SUBJID","SITEID","ARMCD","ARM","SEX","RACE","BRTHDTC","RFICDTC","RFPENDTC")
for (v in need_dm) if (!(v %in% names(dm))) cat(paste0("WARNING: DM missing ", v, "\n"))
need_ex <- c("USUBJID","EXTRT","EXSTDTC","EXENDTC")
for (v in need_ex) if (!(v %in% names(ex))) cat(paste0("WARNING: EX missing ", v, "\n"))
stopifnot(c("EXSTDTC") %in% names(ex))

# Ensure character columns are character (haven may give numeric for some)
# Keep as is; helpers handle NA/""

# ---- 1. GET FIRST EXPOSURE RECORD PER SUBJECT ----
# proc sort data=sdtm.ex out=work.ex_first; by USUBJID EXSTDTC;
# SAS: missing sorts first; uppercase before lowercase (binary). Use base order radix.
ex_first <- ex
# create sort keys: USUBJID (NA first), EXSTDTC (NA/"" first)
# For character ordering, method="radix" is binary/C-like
ord_ex <- order(is.na(ex_first$USUBJID), ex_first$USUBJID,
                is.na(ex_first$EXSTDTC), ex_first$EXSTDTC,
                na.last = FALSE, method = "radix")
ex_first <- ex_first[ord_ex, , drop = FALSE]

# data work.ex_trt; set work.ex_first; by USUBJID; if first.USUBJID; keep USUBJID EXTRT EXSTDTC EXENDTC;
# keep first per USUBJID in sorted order
# use base: duplicated
dup <- duplicated(ex_first$USUBJID)
# Note: duplicated treats NA as duplicated after first; matches FIRST. logic (missing groups together)
ex_trt <- ex_first[!dup, , drop = FALSE]
# keep only required cols (drop/rename overlapping cols except keys to avoid .x/.y)
keep_ex <- intersect(c("USUBJID","EXTRT","EXSTDTC","EXENDTC"), names(ex_trt))
ex_trt <- ex_trt[, keep_ex, drop = FALSE]

cat(paste0("EX_TRt rows=", nrow(ex_trt), "\n"))

# ---- 2. SORT DM BEFORE MERGING ----
ord_dm <- order(is.na(dm$USUBJID), dm$USUBJID, na.last = FALSE, method = "radix")
dm_sorted <- dm[ord_dm, , drop = FALSE]

# ---- 3. MERGE DEMOGRAPHICS AND EXPOSURE ----
# merge dm_sorted(in=a) ex_trt(in=b); by USUBJID; if a;
# left join, with b flag
ex_trt_keys <- ex_trt$USUBJID
dm_sorted$`__A__` <- TRUE
ex_trt$`__B__` <- TRUE
# Before merging, drop every column present in both except keys (ex_trt already minimal)
common <- intersect(names(dm_sorted), names(ex_trt))
common <- setdiff(common, "USUBJID")
if (length(common) > 0) {
  # drop from ex_trt (keep dm version) - but ex_trt only has 4 cols, so shouldn't happen
  ex_trt <- ex_trt[, setdiff(names(ex_trt), common), drop = FALSE]
}
merged <- merge(dm_sorted, ex_trt, by = "USUBJID", all.x = TRUE, all.y = FALSE, sort = FALSE)
# merge() sorts by default unless sort=FALSE; restore DM sort order by USUBJID radix
ord_m <- order(is.na(merged$USUBJID), merged$USUBJID, na.last = FALSE, method = "radix")
merged <- merged[ord_m, , drop = FALSE]
# b flag: TRUE if USUBJID was in ex_trt
merged$`__B__`[is.na(merged$`__B__`)] <- FALSE
# a is always TRUE (all.x, no extra)
stopifnot("ARMCD" %in% names(merged))
stopifnot("ARM" %in% names(merged))
stopifnot("EXTRT" %in% names(merged) || "EXTRT" %in% names(ex_trt))
# Ensure EX cols exist after join (if no match, they are NA)
for (v in c("EXTRT","EXSTDTC","EXENDTC")) if (!(v %in% names(merged))) merged[[v]] <- NA_character_

b_flag <- merged$`__B__`

# Core + treatment/population flags with SAS missing semantics
# ARMCD comparison: missing (""/NA) ne 'SCRNFAIL' => TRUE
armcd_for_cmp <- ifelse(is.na(merged$ARMCD), "", as.character(merged$ARMCD))
is_rand <- armcd_for_cmp != "SCRNFAIL"

TRT01P <- ifelse(is_rand,
                 as.character(merged$ARM),
                 "UNPLANNED")
# Actual treatment
extrt_missing <- sas_is_missing_c(merged$EXTRT)
TRT01A <- ifelse(b_flag & !extrt_missing,
                 as.character(merged$EXTRT),
                 "NOT TREATED")
ITTFL <- ifelse(is_rand, "Y", "N")
exstdtc_missing <- sas_is_missing_c(merged$EXSTDTC)
SAFFL <- ifelse(!exstdtc_missing, "Y", "N")

# Numeric date derivations
# if length(var)>=10 then date = input(substr(var,1,10), yymmdd10.);
get_dt <- function(charvec) {
  res <- rep(as.Date(NA), length(charvec))
  idx <- !is.na(charvec) & sas_length(charvec) >= 10L
  if (any(idx)) res[idx] <- sas_input_yymmdd10(charvec[idx])
  res
}
BRTHDT <- get_dt(if ("BRTHDTC" %in% names(merged)) merged$BRTHDTC else rep(NA_character_, nrow(merged)))
RFICDT <- get_dt(if ("RFICDTC" %in% names(merged)) merged$RFICDTC else rep(NA_character_, nrow(merged)))
TRTSDT <- get_dt(merged$EXSTDTC)
TRTEDT <- get_dt(merged$EXENDTC)
EOSDT  <- get_dt(if ("RFPENDTC" %in% names(merged)) merged$RFPENDTC else rep(NA_character_, nrow(merged)))

AGE <- sas_age_completed(BRTHDT, RFICDT)
# Treatment duration inclusive
TRTDURD <- rep(NA_real_, nrow(merged))
idx_dur <- !is.na(TRTSDT) & !is.na(TRTEDT)
TRTDURD[idx_dur] <- as.integer(TRTEDT[idx_dur] - TRTSDT[idx_dur] + 1L)

# Build draft, retain order STUDYID USUBJID SUBJID SITEID ...
get_col <- function(nm) if (nm %in% names(merged)) merged[[nm]] else rep(NA_character_, nrow(merged))
adsl_draft <- data.frame(
  STUDYID = get_col("STUDYID"),
  USUBJID = get_col("USUBJID"),
  SUBJID  = get_col("SUBJID"),
  SITEID  = get_col("SITEID"),
  TRT01P  = TRT01P,
  TRT01A  = TRT01A,
  ITTFL   = ITTFL,
  SAFFL   = SAFFL,
  BRTHDT  = BRTHDT,
  RFICDT  = RFICDT,
  TRTSDT  = TRTSDT,
  TRTEDT  = TRTEDT,
  EOSDT   = EOSDT,
  AGE     = AGE,
  TRTDURD = TRTDURD,
  SEX     = get_col("SEX"),
  RACE    = get_col("RACE"),
  stringsAsFactors = FALSE
)
# Ensure Date class
for (v in c("BRTHDT","RFICDT","TRTSDT","TRTEDT","EOSDT")) adsl_draft[[v]] <- as.Date(adsl_draft[[v]], origin = "1970-01-01")

# ---- 4. FINALIZE AND SAVE ----
ord_adsl <- order(is.na(adsl_draft$USUBJID), adsl_draft$USUBJID, na.last = FALSE, method = "radix")
adsl <- adsl_draft[ord_adsl, , drop = FALSE]
rownames(adsl) <- NULL

cat(paste0("ADSL rows=", nrow(adsl), "\n"))

# Write dataset
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ---- 5. VISUAL AUDIT: PROC PRINT obs=10 vars USUBJID TRT01P TRT01A ITTFL SAFFL AGE ----
nprt <- min(10L, nrow(adsl))
print_df <- data.frame(
  Obs = seq_len(nprt),
  USUBJID = adsl$USUBJID[seq_len(nprt)],
  TRT01P = adsl$TRT01P[seq_len(nprt)],
  TRT01A = adsl$TRT01A[seq_len(nprt)],
  ITTFL = adsl$ITTFL[seq_len(nprt)],
  SAFFL = adsl$SAFFL[seq_len(nprt)],
  AGE = adsl$AGE[seq_len(nprt)],
  stringsAsFactors = FALSE
)
# CSV of table body
write.csv(print_df, file.path(out, "P006.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
title_txt <- "ADSL Audit - Subject Level Analysis Dataset"
# Build fixed-width text
hdr <- sprintf("%-5s %-18s %-22s %-22s %-6s %-6s %-5s", "Obs", "USUBJID", "TRT01P", "TRT01A", "ITTFL", "SAFFL", "AGE")
sep <- paste(rep("-", nchar(hdr)), collapse = "")
lines <- c(title_txt, "", hdr, sep)
for (i in seq_len(nprt)) {
  age_s <- ifelse(is.na(print_df$AGE[i]), "", as.character(as.integer(print_df$AGE[i])))
  lines <- c(lines, sprintf("%-5d %-18s %-22s %-22s %-6s %-6s %-5s",
    print_df$Obs[i],
    ifelse(is.na(print_df$USUBJID[i]), "", print_df$USUBJID[i]),
    ifelse(is.na(print_df$TRT01P[i]), "", print_df$TRT01P[i]),
    ifelse(is.na(print_df$TRT01A[i]), "", print_df$TRT01A[i]),
    ifelse(is.na(print_df$ITTFL[i]), "", print_df$ITTFL[i]),
    ifelse(is.na(print_df$SAFFL[i]), "", print_df$SAFFL[i]),
    age_s))
}
lines <- c(lines, "", paste0("N = ", nrow(adsl), " subjects in ADSL; showing first ", nprt, " ordered by USUBJID."))
txt_path <- file.path(out, "P006.txt")
writeLines(lines, txt_path)
# Also write .lst copy for expected name
lst_path <- file.path(out, "P006.lst")
writeLines(lines, lst_path)

cat("P006 complete\n")
```