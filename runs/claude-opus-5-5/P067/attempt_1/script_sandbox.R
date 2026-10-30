# P067 - AE SDTM derivation (translation of P067_ae.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_dir  <- "/bench/data/derived/wxzhai/raw"
sdtm_dir <- "/bench/data/derived/wxzhai/sdtm"

read_ds <- function(dir, name) {
  f <- file.path(dir, paste0(name, ".sas7bdat"))
  if (!file.exists(f)) {
    cand <- list.files(dir, pattern = paste0("^", name, "\\.sas7bdat$"),
                       ignore.case = TRUE, full.names = TRUE)
    if (length(cand) > 0) f <- cand[1]
  }
  df <- haven::read_sas(f)
  df <- haven::zap_labels(df)
  df
}

cat("Reading inputs...\n")
raw_ae <- read_ds(raw_dir, "raw_ae")
dm     <- read_ds(sdtm_dir, "dm")

# case-insensitive locate of SUBJID
fixname <- function(df, nm) {
  i <- which(toupper(names(df)) == toupper(nm))
  if (length(i)) names(df)[i[1]] <- nm
  df
}
for (v in c("SUBJID", "AESTDT", "AEENDT", "AETERM", "AESER", "AESEV")) raw_ae <- fixname(raw_ae, v)
for (v in c("SUBJID", "RFSTDTC", "USUBJID", "STUDYID")) dm <- fixname(dm, v)
# harmonise other common names to raw_ae casing
for (nm in names(dm)) {
  j <- which(toupper(names(raw_ae)) == toupper(nm))
  if (length(j)) names(dm)[names(dm) == nm] <- names(raw_ae)[j[1]]
}

# SAS-like sort key: missing first, character in C (byte) order; stable
sas_order <- function(x) {
  if (is.character(x)) {
    miss <- is.na(x) | trimws(x) == ""
    xx <- ifelse(miss, "", x)
    r <- rank(xx, ties.method = "min")  # placeholder; use C-locale ordering below
    old <- Sys.getlocale("LC_COLLATE")
    on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
    Sys.setlocale("LC_COLLATE", "C")
    order(!miss, xx, seq_along(xx), method = "radix")
  } else {
    xx <- as.numeric(x)
    order(!is.na(xx), xx, seq_along(xx), method = "radix")
  }
}

# PROC SORT
raw_ae <- raw_ae[sas_order(raw_ae$SUBJID), , drop = FALSE]
dm     <- dm[sas_order(dm$SUBJID), , drop = FALSE]

# write sorted raw_ae
fmt_out <- function(df) {
  for (v in names(df)) {
    if (inherits(df[[v]], "POSIXt")) df[[v]] <- format(df[[v]], "%Y-%m-%dT%H:%M:%S")
    else if (inherits(df[[v]], "Date")) df[[v]] <- format(df[[v]], "%Y-%m-%d")
  }
  df
}
write.csv(fmt_out(raw_ae), file.path(out, "raw_ae.csv"), row.names = FALSE, na = "")

# ---- DATA step MERGE (many-to-one, if a) ----
key_a <- if (is.character(raw_ae$SUBJID)) trimws(raw_ae$SUBJID) else as.numeric(raw_ae$SUBJID)
key_b <- if (is.character(dm$SUBJID)) trimws(dm$SUBJID) else as.numeric(dm$SUBJID)
dm1 <- dm[!duplicated(key_b), , drop = FALSE]
key_b1 <- key_b[!duplicated(key_b)]

idx <- match(key_a, key_b1)
common <- setdiff(intersect(names(raw_ae), names(dm1)), "SUBJID")
dm_only <- setdiff(names(dm1), names(raw_ae))

ae <- raw_ae
first_in_grp <- !duplicated(key_a)
for (v in dm_only) {
  ae[[v]] <- dm1[[v]][idx]
}
# common vars: dm value overwrites only on first record of BY group (dm read once)
for (v in common) {
  use <- first_in_grp & !is.na(idx)
  vals <- ae[[v]]
  dmv <- dm1[[v]][idx]
  if (is.character(vals) || is.character(dmv)) {
    vals <- as.character(vals); dmv <- as.character(dmv)
  }
  vals[use] <- dmv[use]
  ae[[v]] <- vals
}

# derivations
ae$STUDYID <- "ABC-301"
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  as.Date(substr(as.character(x), 1, 10), format = "%Y-%m-%d")
}
aestdt <- to_date(ae$AESTDT)
aeendt <- to_date(ae$AEENDT)
ae$AESTDTC <- ifelse(is.na(aestdt), ".", format(aestdt, "%Y-%m-%d"))
ae$AEENDTC <- ifelse(is.na(aeendt), ".", format(aeendt, "%Y-%m-%d"))

rf <- ae$RFSTDTC
rf <- ifelse(is.na(rf), NA_character_, substr(as.character(rf), 1, 10))
rfstdt_num <- suppressWarnings(as.Date(rf, format = "%Y-%m-%d"))

sn <- as.numeric(aestdt); en <- as.numeric(aeendt); rn <- as.numeric(rfstdt_num)
ae$ASTDY <- ifelse(is.na(sn) | is.na(rn), NA_real_,
                   ifelse(sn >= rn, sn - rn + 1, sn - rn))
ae$AENDY <- ifelse(is.na(en) | is.na(rn), NA_real_,
                   ifelse(en >= rn, en - rn + 1, en - rn))

keep <- c("STUDYID", "USUBJID", "SUBJID", "AETERM", "AESER", "AESEV",
          "AESTDTC", "AEENDTC", "ASTDY", "AENDY")
# PDV order: variables in order of first appearance
pdv <- unique(c(names(raw_ae), dm_only, "STUDYID", "AESTDTC", "AEENDTC", "ASTDY", "AENDY"))
keep_ord <- pdv[pdv %in% keep & pdv %in% names(ae)]
ae_out <- as.data.frame(ae[, keep_ord, drop = FALSE])

write.csv(fmt_out(ae_out), file.path(out, "ae.csv"), row.names = FALSE, na = "")
cat("Done: ae rows =", nrow(ae_out), "\n")
