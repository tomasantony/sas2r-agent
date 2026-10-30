# P008 - ADVS derivation (translation of P008_adam_advs.sas)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"

# Read a SAS dataset from a libref directory (sas7bdat preferred, xpt fallback)
read_lib <- function(dir, name) {
  f1 <- file.path(dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(dir, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else if (file.exists(f2)) {
    d <- haven::read_xpt(f2)
  } else {
    stop("Cannot find dataset ", name, " in ", dir)
  }
  names(d) <- toupper(names(d))
  d <- haven::zap_labels(d)
  as.data.frame(d, stringsAsFactors = FALSE)
}

# Convert a variable to Date (handles numeric SAS dates as well)
to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01") - 3653)
  as.Date(x)
}

# SAS-style ordering key: missing sorts first
sas_key <- function(x) {
  if (is.character(x)) {
    x[is.na(x)] <- ""
    x
  } else {
    as.numeric(x)
  }
}

sas_order <- function(df, vars) {
  keys <- lapply(vars, function(v) sas_key(df[[v]]))
  # na.last = FALSE puts missing numerics first; radix method gives C-locale character ordering
  do.call(order, c(keys, list(na.last = FALSE, method = "radix")))
}

cat("Reading inputs...\n")
adsl <- read_lib(file.path(project_path, "data/adam"), "adsl")
vs   <- read_lib(file.path(project_path, "data/sdtm"), "vs")

# Make sure date variables are Date class
for (v in c("TRTSDT", "TRTEDT")) {
  if (v %in% names(adsl)) adsl[[v]] <- to_date(adsl[[v]])
}

# ---- 1. MERGE ADSL AND VS ----
adsl_sorted <- adsl[sas_order(adsl, "USUBJID"), , drop = FALSE]
vs_sorted   <- vs[sas_order(vs, c("USUBJID", "VSDTC", "VSTESTCD")), , drop = FALSE]

# Common variables: VS values overwrite ADSL values (except BY variable)
common <- setdiff(intersect(names(adsl_sorted), names(vs_sorted)), "USUBJID")
adsl_m <- adsl_sorted[, setdiff(names(adsl_sorted), common), drop = FALSE]
# keep only first record per USUBJID in ADSL (one-to-many merge)
adsl_m <- adsl_m[!duplicated(adsl_m$USUBJID), , drop = FALSE]

vs_sorted$.ord <- seq_len(nrow(vs_sorted))
draft <- left_join(vs_sorted, adsl_m, by = "USUBJID")
draft <- draft[order(draft$.ord), , drop = FALSE]

for (v in c("TRT01A", "TRTSDT", "TRTEDT")) {
  if (!v %in% names(draft)) draft[[v]] <- if (v == "TRT01A") NA_character_ else as.Date(NA)
}

draft$PARAMCD <- draft$VSTESTCD
draft$PARAM   <- draft$VSTEST
draft$AVAL    <- as.numeric(draft$VSSTRESN)
draft$AVISIT  <- draft$VISIT
draft$AVISITN <- as.numeric(draft$VISITNUM)
draft$SRCDOM  <- "VS"
draft$SRCVAR  <- "VSSEQ"
draft$SRCSEQ  <- as.numeric(draft$VSSEQ)

vsdtc <- ifelse(is.na(draft$VSDTC), "", as.character(draft$VSDTC))
len_ok <- nchar(sub("\\s+$", "", vsdtc)) >= 10
d10 <- substr(vsdtc, 1, 10)
astdt <- rep(as.Date(NA), nrow(draft))
valid <- len_ok & grepl("^\\d{4}-\\d{2}-\\d{2}$", d10)
astdt[valid] <- suppressWarnings(as.Date(d10[valid], format = "%Y-%m-%d"))
draft$ASTDT <- astdt

trtsdt <- draft$TRTSDT
draft$ASTDY <- NA_real_
ok <- !is.na(draft$ASTDT) & !is.na(trtsdt)
diffd <- as.numeric(draft$ASTDT - trtsdt)
draft$ASTDY[ok] <- ifelse(diffd[ok] >= 0, diffd[ok] + 1, diffd[ok])

keep_vars <- c("STUDYID", "USUBJID", "TRT01A", "TRTSDT", "TRTEDT",
               "PARAMCD", "PARAM", "AVAL", "AVISIT", "AVISITN",
               "SRCDOM", "SRCVAR", "SRCSEQ", "ASTDT", "ASTDY")
advs_draft <- draft[, keep_vars, drop = FALSE]
rownames(advs_draft) <- NULL

# ---- 2. IDENTIFY BASELINE RECORD ----
# SAS WHERE: ASTDT <= TRTSDT with missing as smallest value
a_n <- as.numeric(advs_draft$ASTDT)
t_n <- as.numeric(advs_draft$TRTSDT)
le <- ifelse(is.na(a_n), TRUE,
             ifelse(is.na(t_n), FALSE, a_n <= t_n))
cand <- advs_draft[le & !is.na(advs_draft$AVAL), , drop = FALSE]
cand$ASTDT_N <- as.numeric(cand$ASTDT)
cand <- cand[sas_order(cand, c("USUBJID", "PARAMCD", "ASTDT_N", "AVISITN", "SRCSEQ")), , drop = FALSE]

grp <- paste(cand$USUBJID, cand$PARAMCD, sep = "\r")
is_last <- !duplicated(grp, fromLast = TRUE)
base_flags <- data.frame(
  USUBJID  = cand$USUBJID[is_last],
  PARAMCD  = cand$PARAMCD[is_last],
  BASE     = cand$AVAL[is_last],
  BASE_DT  = cand$ASTDT[is_last],
  BASE_SEQ = cand$SRCSEQ[is_last],
  stringsAsFactors = FALSE
)

# ---- 3. MERGE BASELINE BACK AND CALCULATE FLAGS/CHANGE ----
advs_draft <- advs_draft[sas_order(advs_draft, c("USUBJID", "PARAMCD")), , drop = FALSE]
advs_draft$.ord <- seq_len(nrow(advs_draft))
advs <- left_join(advs_draft, base_flags, by = c("USUBJID", "PARAMCD"))
advs <- advs[order(advs$.ord), , drop = FALSE]
advs$.ord <- NULL

eq <- (is.na(advs$SRCSEQ) & is.na(advs$BASE_SEQ)) |
      (!is.na(advs$SRCSEQ) & !is.na(advs$BASE_SEQ) & advs$SRCSEQ == advs$BASE_SEQ)
advs$ABLFL <- ifelse(eq, "Y", "")

advs$CHG <- ifelse(!is.na(advs$AVAL) & !is.na(advs$BASE), advs$AVAL - advs$BASE, NA_real_)

advs$BASE_DT <- NULL
advs$BASE_SEQ <- NULL

# Re-sort for final presentation
advs$ASTDT_N <- as.numeric(advs$ASTDT)
advs <- advs[sas_order(advs, c("USUBJID", "PARAMCD", "ASTDT_N")), , drop = FALSE]
advs$ASTDT_N <- NULL
rownames(advs) <- NULL

# ---- Write dataset ----
advs_out <- advs
for (v in c("TRTSDT", "TRTEDT", "ASTDT")) {
  advs_out[[v]] <- ifelse(is.na(advs_out[[v]]), "", format(advs_out[[v]], "%Y-%m-%d"))
}
write.csv(advs_out, file.path(out, "advs.csv"), row.names = FALSE, na = "")
cat("Wrote advs.csv with", nrow(advs_out), "rows\n")

# ---- 4. VISUAL AUDIT (PROC PRINT obs=15) ----
date9 <- function(d) ifelse(is.na(d), ".", toupper(format(d, "%d%b%Y")))
num_best <- function(x) ifelse(is.na(x), ".", sub("\\.?0+$", "",
                         formatC(x, digits = 8, format = "f")))
num_best <- function(x) {
  sapply(x, function(v) if (is.na(v)) "." else format(signif(v, 12), scientific = FALSE, trim = TRUE))
}

p <- head(advs, 15)
lst <- data.frame(
  Obs     = as.character(seq_len(nrow(p))),
  USUBJID = as.character(p$USUBJID),
  PARAMCD = as.character(p$PARAMCD),
  AVISIT  = as.character(p$AVISIT),
  ASTDT   = date9(p$ASTDT),
  TRTSDT  = date9(p$TRTSDT),
  AVAL    = num_best(p$AVAL),
  BASE    = num_best(p$BASE),
  CHG     = num_best(p$CHG),
  ABLFL   = ifelse(is.na(p$ABLFL), "", p$ABLFL),
  stringsAsFactors = FALSE
)
write.csv(lst, file.path(out, "P008.csv"), row.names = FALSE, na = "")

numcols <- c("Obs", "AVAL", "BASE", "CHG")
widths <- sapply(names(lst), function(n) max(nchar(n), nchar(lst[[n]]), na.rm = TRUE))
fmt_cell <- function(val, n) {
  w <- widths[[n]]
  if (n %in% numcols) formatC(val, width = w) else formatC(val, width = w, flag = "-")
}
header <- paste(sapply(names(lst), function(n) fmt_cell(n, n)), collapse = "  ")
lines <- c("ADVS Audit - Vital Signs Baseline and Change Analysis", "", header)
for (i in seq_len(nrow(lst))) {
  lines <- c(lines, paste(sapply(names(lst), function(n) fmt_cell(lst[[n]][i], n)), collapse = "  "))
}
writeLines(lines, file.path(out, "P008.txt"))
cat("Wrote P008.txt and P008.csv\n")
