suppressPackageStartupMessages({
  library(readxl); library(dplyr)
})
out <- Sys.getenv("OUT_DIR")
rawdir <- "/bench/data/raw/faijal-oncology/raw_data"
rd <- function(n) {
  f <- file.path(rawdir, paste0(n, ".xlsx"))
  if (file.exists(f)) as.data.frame(read_excel(f)) else NULL
}

# ---- helpers -------------------------------------------------------------
num2chr <- function(x) {             # SAS numeric->char (BEST12.) for catx
  if (is.numeric(x)) { r <- format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE); r[is.na(x)] <- ""; r }
  else { r <- as.character(x); r[is.na(r)] <- ""; trimws(r) }
}
catx_dash <- function(a, b) ifelse(b == "", a, paste(a, b, sep = "-"))
mons <- c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
parse_date11 <- function(s) {        # SAS DATE11. informat (dd-MON-yyyy / ddMONyyyy)
  s <- toupper(trimws(s)); s[is.na(s)] <- ""
  m <- regmatches(s, regexec("^([0-9]{1,2})[- ]?([A-Z]{3})[- ]?([0-9]{2,4})$", s))
  vapply(m, function(v) {
    if (length(v) < 4) return(NA_real_)
    mo <- match(v[3], mons); if (is.na(mo)) return(NA_real_)
    y <- as.integer(v[4]); if (nchar(v[4]) == 2) y <- ifelse(y < 26, 2000 + y, 1900 + y)
    d <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mo, as.integer(v[2]))))
    as.numeric(d)
  }, numeric(1))
}
any2date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  s <- trimws(as.character(x))
  d <- suppressWarnings(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
  d2 <- as.Date(parse_date11(s), origin = "1970-01-01")
  d[is.na(d)] <- d2[is.na(d)]
  d3 <- suppressWarnings(as.Date(s, format = "%d/%m/%Y"))
  d[is.na(d)] <- d3[is.na(d)]
  d
}
iso <- function(d) { r <- format(d, "%Y-%m-%d"); r[is.na(d)] <- ""; r }

date_cols <- function(df, exclude = "BRTH|BIRTH|DOB|EN_?DT|END|STOP") {
  res <- character(0)
  for (nm in names(df)) {
    if (toupper(nm) == "SUBJID") next
    if (grepl(exclude, toupper(nm))) next
    x <- df[[nm]]
    isdt <- grepl("DT|DATE|DTC", toupper(nm))
    if (inherits(x, c("Date", "POSIXt"))) { res <- c(res, nm); next }
    if (is.numeric(x)) { if (isdt) res <- c(res, nm); next }
    xs <- trimws(as.character(x)); nn <- xs[!is.na(xs) & xs != ""]
    if (!length(nn)) next
    rate <- mean(!is.na(any2date(nn)))
    if (rate > 0.5 || (isdt && rate > 0)) res <- c(res, nm)
  }
  res
}
min_date_by_subj <- function(df, cols) {
  if (is.null(df) || !length(cols) || is.null(df$SUBJID)) return(NULL)
  l <- lapply(cols, function(cn) data.frame(SUBJID = num2chr(df$SUBJID), d = any2date(df[[cn]])))
  do.call(rbind, l) %>% filter(!is.na(d), SUBJID != "") %>%
    group_by(SUBJID) %>% summarise(d = min(d), .groups = "drop")
}
fill_from <- function(cur, subj, src) {
  if (is.null(src)) return(cur)
  v <- iso(src$d[match(subj, src$SUBJID)]); v[is.na(v)] <- ""
  cur[cur == ""] <- v[cur == ""]
  cur
}

# ---- 1. DM reference dates (sdtm_dm reconstructed from raw EDC) ----------
edc_dm <- rd("edc_dm"); edc_ex <- rd("edc_ex"); edc_death <- rd("edc_death")
ivrs <- rd("ivrs_rand")
dm_for_ds <- data.frame(USUBJID = character(0), RFSTDTC = character(0), DTHDTC = character(0))
if (!is.null(edc_dm)) {
  dm <- data.frame(SUBJID = num2chr(edc_dm$SUBJID), stringsAsFactors = FALSE)
  dm$USUBJID <- catx_dash("ONCO-2026", dm$SUBJID)
  dm$RFSTDTC <- ""
  rfc <- names(edc_dm)[grepl("RFST", toupper(names(edc_dm)))]
  if (length(rfc)) { dm$RFSTDTC <- iso(any2date(edc_dm[[rfc[1]]])); dm$RFSTDTC[is.na(dm$RFSTDTC)] <- "" }
  # reference start = randomisation date (IVRS), then first exposure as fallback
  if (!is.null(ivrs)) dm$RFSTDTC <- fill_from(dm$RFSTDTC, dm$SUBJID, min_date_by_subj(ivrs, date_cols(ivrs)))
  if (!is.null(edc_ex)) dm$RFSTDTC <- fill_from(dm$RFSTDTC, dm$SUBJID, min_date_by_subj(edc_ex, date_cols(edc_ex)))
  dm$RFSTDTC[is.na(dm$RFSTDTC)] <- ""
  dm$DTHDTC <- ""
  if (!is.null(edc_death)) {
    de <- min_date_by_subj(edc_death, date_cols(edc_death))
    if (!is.null(de)) dm$DTHDTC <- iso(de$d[match(dm$SUBJID, de$SUBJID)])
  }
  dm$DTHDTC[is.na(dm$DTHDTC)] <- ""
  dm_for_ds <- dm %>% select(USUBJID, RFSTDTC, DTHDTC) %>% distinct(USUBJID, .keep_all = TRUE) %>% arrange(USUBJID)
}

# ---- 2. Prepare raw DS ----------------------------------------------------
edc_ds <- rd("edc_ds")
chr <- function(x) { if (inherits(x, c("Date","POSIXt"))) x <- as.numeric(as.Date(x)) - as.numeric(as.Date("1960-01-01"))
  num2chr(x) }
edc_ds_prep <- data.frame(
  TERM = substr(chr(edc_ds$DS_TERM), 1, 50),
  DECOD = substr(chr(edc_ds$DS_TERM), 1, 50),
  CAT = substr(chr(edc_ds$DS_CAT), 1, 50),
  STDT = substr(chr(edc_ds$DS_DT), 1, 11),
  stringsAsFactors = FALSE)
edc_ds_prep$USUBJID <- substr(catx_dash("ONCO-2026", num2chr(edc_ds$SUBJID)), 1, 25)
edc_ds_prep <- edc_ds_prep[order(edc_ds_prep$USUBJID, method = "radix"), ]

# ---- 4. Merge & derive ----------------------------------------------------
d <- left_join(edc_ds_prep, dm_for_ds, by = "USUBJID")
d$RFSTDTC[is.na(d$RFSTDTC)] <- ""; d$DTHDTC[is.na(d$DTHDTC)] <- ""
d$STUDYID <- "ONCO-2026"; d$DOMAIN <- "DS"
d$DSTERM <- substr(gsub("_", " ", d$TERM, fixed = TRUE), 1, 50)
d$DSDECOD <- substr(toupper(d$DECOD), 1, 30)
d$DSCAT <- substr(gsub("_", " ", d$CAT, fixed = TRUE), 1, 30)
d$DTHFL <- ifelse(d$DTHDTC != "", "Y", "")
dn <- parse_date11(d$STDT)
d$DSSTDTC <- ifelse(is.na(dn), "", format(as.Date(dn, origin = "1970-01-01"), "%Y-%m-%d"))
st <- as.numeric(suppressWarnings(as.Date(d$DSSTDTC, "%Y-%m-%d")))
rf <- as.numeric(suppressWarnings(as.Date(substr(d$RFSTDTC, 1, 10), "%Y-%m-%d")))
d$DSSTDY <- ifelse(d$DSSTDTC != "" & d$RFSTDTC != "" & !is.na(st) & !is.na(rf),
                   ifelse(st >= rf, st - rf + 1, st - rf), NA)
w <- d$USUBJID == "ONCO-2026-103" & d$DSSTDTC != d$DTHDTC
for (i in which(w)) message("WARNING: Date mismatch! DS=", d$DSSTDTC[i], " DM=", d$DTHDTC[i])

# ---- 5/6. Sort, DSSEQ -----------------------------------------------------
d <- d[order(d$USUBJID, d$DSSTDTC, method = "radix"), ]
d$DSSEQ <- ave(seq_len(nrow(d)), d$USUBJID, FUN = seq_along)
sdtm_ds <- d[, c("STUDYID","DOMAIN","USUBJID","DSSEQ","DSTERM","DSDECOD","DSCAT","DSSTDTC","DSSTDY","DTHFL")]
rownames(sdtm_ds) <- NULL
write.csv(sdtm_ds, file.path(out, "ds.csv"), row.names = FALSE, na = "")

# ---- 7. PROC PRINT listing ------------------------------------------------
pr <- cbind(Obs = seq_len(nrow(sdtm_ds)), sdtm_ds)
pr$DSSTDY <- ifelse(is.na(pr$DSSTDY), ".", as.character(pr$DSSTDY))
write.csv(pr, file.path(out, "P024.csv"), row.names = FALSE, na = "")
pc <- as.data.frame(lapply(pr, function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }), stringsAsFactors = FALSE)
w <- pmax(nchar(names(pc)), sapply(pc, function(x) max(c(0, nchar(x)))))
num <- names(pc) %in% c("Obs","DSSEQ","DSSTDY")
fmt <- function(v, i) if (num[i]) formatC(v, width = w[i]) else formatC(v, width = -w[i])
lines <- c("The SAS System", "",
  paste(sapply(seq_along(pc), function(i) fmt(names(pc)[i], i)), collapse = "  "))
for (r in seq_len(nrow(pc))) lines <- c(lines, paste(sapply(seq_along(pc), function(i) fmt(pc[r, i], i)), collapse = "  "))
writeLines(lines, file.path(out, "P024.txt"))
message("P024 done: ", nrow(sdtm_ds), " records")
