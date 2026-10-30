out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(dplyr))

sdtm_dir_primary <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_dir_primary <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"
rawsdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawadam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsplit_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"

round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, floor(x * 10^d + 0.5 + 1e-10) / 10^d)
}

find_file_for_mem <- function(dirs, mem) {
  mem_l <- tolower(mem)
  for (d in dirs) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    files <- list.files(d, full.names = TRUE, recursive = FALSE)
    if (length(files) == 0) next
    bn <- basename(files)
    noext <- tolower(sub("\\.[^.]*$", "", bn))
    idx <- which(noext == mem_l)
    if (length(idx) == 0) next
    ext <- tolower(sub("^.*\\.", "", bn[idx]))
    pri <- ifelse(ext == "sas7bdat", 1L,
           ifelse(ext == "xpt", 2L,
           ifelse(ext == "csv", 3L,
           ifelse(ext == "xpt.gz", 4L, 5L))))
    o <- order(pri)
    idx <- idx[o]
    return(files[idx[1]])
  }
  return(NULL)
}

read_mem_dataset <- function(dirs, mem) {
  f <- find_file_for_mem(dirs, mem)
  if (is.null(f)) stop(paste0("Dataset ", mem, " not found in: ", paste(dirs, collapse=" | ")))
  cat("Reading", mem, "from", f, "\n")
  e <- tolower(sub("^.*\\.", "", basename(f)))
  if (e == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (e == "xpt") {
    df <- haven::read_xpt(f)
  } else if (e == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try sas then xpt
    df <- tryCatch(haven::read_sas(f), error = function(e2) haven::read_xpt(f))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # remove haven labels/formats but keep values
  df <- haven::zap_formats(haven::zap_labels(df))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  return(df)
}

lb_raw <- read_mem_dataset(c(sdtm_dir_primary, rawsdtm_dir), "lb")
adsl_raw <- read_mem_dataset(c(adam_dir_primary, rawadam_dir, rawsplit_dir), "adsl")

stopifnot("USUBJID" %in% names(lb_raw))
stopifnot("PARAM" %in% names(lb_raw))
stopifnot("LBDTC" %in% names(lb_raw))
stopifnot("AVAL" %in% names(adsl_raw) | "AVAL" %in% names(lb_raw))
stopifnot("USUBJID" %in% names(adsl_raw))
stopifnot("RFSTDTC" %in% names(adsl_raw))
# TRT01P should exist in ADSL; if not, keep whatever exists
# proc sort data=sdtm.lb out=lb_qc; by USUBJID PARAM LBDTC;
sas_key <- function(x) {
  xc <- as.character(x)
  xc[is.na(xc)] <- ""
  xc
}
ord_lb <- order(sas_key(lb_raw$USUBJID), sas_key(lb_raw$PARAM), sas_key(lb_raw$LBDTC),
                method = "radix")
lb_qc <- lb_raw[ord_lb, , drop = FALSE]
rownames(lb_qc) <- NULL

# proc sort data=adam.adsl out=adsl_qc; by USUBJID;
ord_adsl <- order(sas_key(adsl_raw$USUBJID), method = "radix")
adsl_qc <- adsl_raw[ord_adsl, , drop = FALSE]
rownames(adsl_qc) <- NULL

# data adam.adlb_qc; merge lb_qc (in=a) adsl_qc (in=b keep=USUBJID RFSTDTC TRT01P); by USUBJID; if a and b;
keep_adsl <- intersect(c("USUBJID", "RFSTDTC", "TRT01P"), names(adsl_qc))
stopifnot("USUBJID" %in% keep_adsl)
stopifnot("RFSTDTC" %in% keep_adsl)
# TRT01P expected; do not fail if absent, but record
adsl_sub <- adsl_qc[, keep_adsl, drop = FALSE]

# before merging, drop overlapping non-key cols from lb_qc so no .x/.y
keys <- "USUBJID"
overlap <- intersect(names(lb_qc), names(adsl_sub))
overlap_nonkey <- setdiff(overlap, keys)
if (length(overlap_nonkey) > 0) {
  lb_qc <- lb_qc[, setdiff(names(lb_qc), overlap_nonkey), drop = FALSE]
}

merged <- dplyr::inner_join(lb_qc, adsl_sub, by = "USUBJID")
stopifnot("USUBJID" %in% names(merged))
stopifnot("PARAM" %in% names(merged))
stopifnot("LBDTC" %in% names(merged))
stopifnot("AVAL" %in% names(merged))
stopifnot("RFSTDTC" %in% names(merged))
if ("TRT01P" %in% keep_adsl) stopifnot("TRT01P" %in% names(merged))

# re-sort merged by USUBJID PARAM LBDTC to preserve DATA step order (SAS merge order)
ord_m <- order(sas_key(merged$USUBJID), sas_key(merged$PARAM), sas_key(merged$LBDTC),
               method = "radix")
merged <- merged[ord_m, , drop = FALSE]
rownames(merged) <- NULL

# lbdt = input(LBDTC, yymmdd10.); rfstdt = input(RFSTDTC, yymmdd10.);
parse_yymmdd10 <- function(x) {
  xc <- as.character(x)
  # trim blanks; NA stays NA
  xt <- trimws(xc)
  s10 <- substr(xt, 1, 10)
  # empty string -> NA
  s10[s10 == ""] <- NA_character_
  suppressWarnings(as.Date(s10, format = "%Y-%m-%d"))
}
lbdt <- parse_yymmdd10(merged$LBDTC)
rfstdt <- parse_yymmdd10(merged$RFSTDTC)
aval <- suppressWarnings(as.numeric(as.character(merged$AVAL)))
# AVAL may already be numeric; as.numeric preserves; non-numeric -> NA (SAS INPUT would be missing)

n <- nrow(merged)
BASE <- rep(NA_real_, n)
ABLFL <- rep("", n)
CHG <- rep(NA_real_, n)

# FIRST.PARAM logic: reset BASE at first row of each USUBJID+PARAM group
# (BY USUBJID in SAS is insufficient; intended QC is per subject-param in LBDTC order)
us_key <- sas_key(merged$USUBJID)
pa_key <- sas_key(merged$PARAM)
is_first_param <- logical(n)
if (n > 0) {
  is_first_param[1] <- TRUE
  if (n > 1) {
    for (i in 2:n) {
      if (us_key[i] != us_key[i-1] || pa_key[i] != pa_key[i-1]) {
        is_first_param[i] <- TRUE
      } else {
        is_first_param[i] <- FALSE
      }
    }
  }
}

sas_num_equal <- function(a, b) {
  # SAS: . = . is TRUE; . = non-missing is FALSE
  res <- logical(length(a))
  for (k in seq_along(a)) {
    ai <- a[k]; bi <- b[k]
    if (is.na(ai) && is.na(bi)) res[k] <- TRUE
    else if (is.na(ai) || is.na(bi)) res[k] <- FALSE
    else res[k] <- (ai == bi)
  }
  res
}

cur_base <- NA_real_
if (n > 0) {
  for (i in seq_len(n)) {
    if (is_first_param[i]) cur_base <- NA_real_
    lbdti <- lbdt[i]
    rfdti <- rfstdt[i]
    avali <- aval[i]
    # SAS: missing numeric sorts first, so missing lbdt < non-missing rfstdt is TRUE
    # cond1 = lbdt < rfstdt under SAS missing semantics
    cond1 <- FALSE
    if (!is.na(rfdti)) {
      if (is.na(lbdti)) cond1 <- TRUE
      else if (lbdti < rfdti) cond1 <- TRUE
      else cond1 <- FALSE
    } else {
      cond1 <- FALSE
    }
    if (cond1 && !is.na(avali)) {
      cur_base <- avali
    }
    # ABLFL check uses updated BASE
    eq <- FALSE
    if (is.na(avali) && is.na(cur_base)) eq <- TRUE
    else if (is.na(avali) || is.na(cur_base)) eq <- FALSE
    else eq <- (avali == cur_base)
    cond_abl <- cond1 && eq
    if (is.na(cond_abl)) cond_abl <- FALSE
    ABLFL[i] <- if (cond_abl) "Y" else ""
    BASE[i] <- cur_base
    if (!is.na(cur_base)) {
      if (!is.na(avali)) CHG[i] <- avali - cur_base
      else CHG[i] <- NA_real_
    } else {
      CHG[i] <- NA_real_
    }
  }
}

# assemble output: lb vars + RFSTDTC/TRT01P + BASE ABLFL CHG; drop lbdt rfstdt
adlb_qc <- merged
adlb_qc$BASE <- BASE
adlb_qc$ABLFL <- ABLFL
adlb_qc$CHG <- CHG

# order columns: original lb_qc order, then new adsl cols in keep order, then BASE ABLFL CHG
lb_names <- names(lb_qc)
adsl_new <- setdiff(keep_adsl, c(names(lb_qc), keys))
# keys USUBJID already in lb_names; RFSTDTC/TRT01P are new
desired <- c(lb_names, adsl_new, c("BASE", "ABLFL", "CHG"))
# keep only existing, in desired order, plus any extra merged cols not yet listed (should be none)
extra <- setdiff(names(adlb_qc), desired)
final_order <- c(desired[desired %in% names(adlb_qc)], extra)
adlb_qc <- adlb_qc[, final_order, drop = FALSE]

# format Date/POSIX columns to ISO strings for CSV (LBDTC/RFSTDTC stay character)
for (cn in names(adlb_qc)) {
  col <- adlb_qc[[cn]]
  if (inherits(col, "Date")) {
    adlb_qc[[cn]] <- format(col, "%Y-%m-%d")
  } else if (inherits(col, "POSIXct") || inherits(col, "POSIXt")) {
    adlb_qc[[cn]] <- format(col, "%Y-%m-%dT%H:%M:%S")
  }
}

cat("Writing adlb_qc with", nrow(adlb_qc), "rows and", ncol(adlb_qc), "cols\n")
write.csv(adlb_qc, file.path(out, "adlb_qc.csv"), row.names = FALSE, na = "")
