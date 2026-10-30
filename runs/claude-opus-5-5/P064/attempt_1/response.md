```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm"
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam"

read_ds <- function(dir, name) {
  f <- file.path(dir, paste0(name, ".sas7bdat"))
  if (!file.exists(f)) f <- file.path(dir, paste0(name, ".xpt"))
  if (grepl("\\.xpt$", f)) read_xpt(f) else read_sas(f)
}

lb   <- read_ds(sdtm_dir, "lb")
adsl <- read_ds(adam_dir, "adsl")
names(lb)   <- toupper(names(lb))
names(adsl) <- toupper(names(adsl))
cat("Read lb:", nrow(lb), "rows; adsl:", nrow(adsl), "rows\n")

# SAS-like character sort key: missing ("" or NA) first, byte ordering
skey <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

# proc sort data=sdtm.lb; by USUBJID PARAM LBDTC  (stable)
old_loc <- Sys.getlocale("LC_COLLATE")
invisible(Sys.setlocale("LC_COLLATE", "C"))
lb <- lb[order(skey(lb$USUBJID), skey(lb$PARAM), skey(lb$LBDTC), method = "radix"), , drop = FALSE]
adsl <- adsl[order(skey(adsl$USUBJID), method = "radix"), , drop = FALSE]
invisible(Sys.setlocale("LC_COLLATE", old_loc))

# merge ... by USUBJID; if a and b
adsl_k <- adsl[, c("USUBJID", "TRT01P", "RFSTDTC")]
adsl_k <- adsl_k[!duplicated(adsl_k$USUBJID), ]
idx <- match(lb$USUBJID, adsl_k$USUBJID)
keep <- !is.na(idx)
dat <- lb[keep, , drop = FALSE]
idx <- idx[keep]
dat$TRT01P  <- adsl_k$TRT01P[idx]    # adsl values overwrite if present in lb
dat$RFSTDTC <- adsl_k$RFSTDTC[idx]

# input(x, yymmdd10.)
parse_ymd10 <- function(x) {
  x <- substr(as.character(x), 1, 10)
  x <- gsub("[/.]", "-", x)
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  d[!grepl("^\\d{4}-\\d{2}-\\d{2}$", x)] <- NA
  as.numeric(d)
}
# SAS comparisons with missing as smallest value
sas_lt <- function(a, b) {
  ifelse(is.na(a) & is.na(b), FALSE,
         ifelse(is.na(a), TRUE, ifelse(is.na(b), FALSE, a < b)))
}
sas_eq <- function(a, b) {
  ifelse(is.na(a) & is.na(b), TRUE,
         ifelse(is.na(a) | is.na(b), FALSE, a == b))
}

ldt <- parse_ymd10(dat$LBDTC)
rdt <- parse_ymd10(dat$RFSTDTC)
aval <- as.numeric(dat$AVAL)
n <- nrow(dat)

# NOTE: PARAM is not a BY variable, so first.PARAM is an uninitialized
# (missing) variable in SAS -> the reset never happens; BASE is retained.
BASE  <- rep(NA_real_, n)
ABLFL <- rep("", n)
base_r <- NA_real_
for (i in seq_len(n)) {
  pre <- sas_lt(ldt[i], rdt[i])
  if (pre && !is.na(aval[i])) base_r <- aval[i]
  BASE[i] <- base_r
  ABLFL[i] <- if (pre && sas_eq(aval[i], base_r)) "Y" else ""
}
CHG <- aval - BASE

dat$BASE  <- BASE
dat$ABLFL <- ABLFL
dat$CHG   <- CHG
dat$LBDTC   <- NULL
dat$RFSTDTC <- NULL

# format dates / datetimes for output
adlb <- as.data.frame(dat, stringsAsFactors = FALSE)
for (v in names(adlb)) {
  x <- adlb[[v]]
  if (inherits(x, "Date")) adlb[[v]] <- format(x, "%Y-%m-%d")
  else if (inherits(x, "POSIXt")) adlb[[v]] <- format(x, "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  else adlb[[v]] <- as.vector(zap_labels(x))
}

write.csv(adlb, file.path(out, "adlb.csv"), row.names = FALSE, na = "")
cat("Wrote adlb:", nrow(adlb), "rows,", ncol(adlb), "cols\n")
```