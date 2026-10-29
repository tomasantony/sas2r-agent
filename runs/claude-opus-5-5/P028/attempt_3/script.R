# P028 - SDTM TR mapping (translation of P028_tr.sas) - attempt 3
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data"
root   <- "D:/meta_hackathon/sas2r-agent"

# ---------------- helpers ----------------
sas_trunc <- function(x, n) {
  x <- ifelse(is.na(x), "", as.character(x))
  substr(x, 1, n)
}
to_chr <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) {
    r <- format(as.Date(x), "%Y-%m-%d")
  } else if (is.numeric(x)) {
    r <- ifelse(is.na(x), "", format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
  } else {
    r <- as.character(x)
  }
  r[is.na(r)] <- ""
  trimws(r, which = "right")
}
mon_map <- c(JAN = 1, FEB = 2, MAR = 3, APR = 4, MAY = 5, JUN = 6,
             JUL = 7, AUG = 8, SEP = 9, OCT = 10, NOV = 11, DEC = 12)
parse_date11 <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  x <- toupper(trimws(as.character(x)))
  res <- rep(as.Date(NA), length(x))
  m <- regmatches(x, regexec("^(\\d{1,2})-?([A-Z]{3})-?(\\d{2,4})$", x))
  for (i in seq_along(x)) {
    if (length(m[[i]]) == 4) {
      d <- as.integer(m[[i]][2]); mo <- mon_map[m[[i]][3]]; y <- as.integer(m[[i]][4])
      if (!is.na(mo)) {
        if (y < 100) y <- ifelse(y < 26, 2000 + y, 1900 + y)
        res[i] <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, mo, d)), error = function(e) NA)
      }
    }
  }
  res
}
parse_any_date <- function(x) {
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  r <- parse_date11(x)
  s <- trimws(as.character(x))
  iso <- suppressWarnings(as.Date(substr(s, 1, 10), format = "%Y-%m-%d"))
  r[is.na(r)] <- iso[is.na(r)]
  r
}
sas_input_num <- function(x) {
  x <- trimws(x)
  r <- suppressWarnings(as.numeric(x))
  r[x == ""] <- NA
  r
}
sas_order <- function(...) order(..., method = "radix")
read_raw <- function(nm) {
  f <- file.path(rawdir, paste0(nm, ".xlsx"))
  d <- tryCatch(read_excel(f), error = function(e) NULL)
  if (!is.null(d)) names(d) <- toupper(names(d))
  d
}
mk_usubjid <- function(s) paste("ONCO-2026", trimws(to_chr(s)), sep = "-")

first_date <- function(d, pats, label) {
  if (is.null(d) || !"SUBJID" %in% names(d)) return(NULL)
  cols <- character(0)
  for (p in pats) cols <- c(cols, grep(p, names(d), value = TRUE))
  cols <- unique(setdiff(cols, "SUBJID"))
  for (cc in cols) {
    dt <- parse_any_date(d[[cc]])
    if (any(!is.na(dt))) {
      tmp <- data.frame(USUBJID = mk_usubjid(d$SUBJID), DT = dt, stringsAsFactors = FALSE)
      tmp <- tmp[!is.na(tmp$DT), ]
      agg <- aggregate(DT ~ USUBJID, data = tmp, FUN = min)
      agg$RFSTDTC <- format(as.Date(agg$DT, origin = "1970-01-01"), "%Y-%m-%d")
      cat("RFSTDTC source:", label, "column", cc, "\n")
      return(agg[, c("USUBJID", "RFSTDTC")])
    }
  }
  NULL
}

# ---------------- 1. DM reference data ----------------
# RFSTDTC taken from randomization date (IVRS), else first exposure, else raw DM
dm_for_tr <- first_date(read_raw("ivrs_rand"), c("^RAND.*DT", "RAND.*DT", "^RANDD", "RAND", "DT", "DATE"), "ivrs_rand")
if (is.null(dm_for_tr)) dm_for_tr <- first_date(read_raw("edc_ex"),
                                                c("^EXSTDT", "ST.*DT", "^EXDT", "DT", "DATE"), "edc_ex")
if (is.null(dm_for_tr)) dm_for_tr <- first_date(read_raw("edc_dm"), c("^RFST", "^RFX?ST"), "edc_dm")
if (is.null(dm_for_tr)) dm_for_tr <- data.frame(USUBJID = character(0), RFSTDTC = character(0))

dm_for_tr <- dm_for_tr[sas_order(dm_for_tr$USUBJID), ]
dm_for_tr <- dm_for_tr[!duplicated(dm_for_tr$USUBJID), ]

# ---------------- 2. raw TR ----------------
edc_tr <- read_raw("edc_tr")
tr_prep <- edc_tr
tr_prep$USUBJID <- sas_trunc(mk_usubjid(tr_prep$SUBJID), 25)
tr_prep <- tr_prep[sas_order(tr_prep$USUBJID), ]
if ("RFSTDTC" %in% names(tr_prep)) tr_prep$RFSTDTC <- NULL

# ---------------- 3. map TR ----------------
m <- left_join(tr_prep, dm_for_tr, by = "USUBJID")

ORRES <- to_chr(m$ORRES)
UNIT  <- to_chr(m$UNIT)
sdtm_tr1 <- data.frame(
  STUDYID  = "ONCO-2026",
  DOMAIN   = "TR",
  USUBJID  = m$USUBJID,
  TRLNKID  = sas_trunc(to_chr(m$LNKID), 5),
  TRTESTCD = sas_trunc(to_chr(m$TESTCD), 8),
  stringsAsFactors = FALSE
)
sdtm_tr1$TRTEST   <- ifelse(sdtm_tr1$TRTESTCD == "LDIAM", "Longest Diameter", "")
sdtm_tr1$TRORRES  <- sas_trunc(ORRES, 10)
sdtm_tr1$TRORRESU <- sas_trunc(UNIT, 10)
sdtm_tr1$TRSTRESC <- sas_trunc(ORRES, 10)
sdtm_tr1$TRSTRESN <- sas_input_num(ORRES)
sdtm_tr1$TRSTRESU <- sas_trunc(UNIT, 10)
sdtm_tr1$TREVAL   <- "INVESTIGATOR"
sdtm_tr1$VISIT    <- sas_trunc(toupper(gsub("_", " ", to_chr(m$VISIT), fixed = TRUE)), 20)

trdt <- parse_date11(m$TRDT)
sdtm_tr1$TRDTC <- ifelse(is.na(trdt), "", format(trdt, "%Y-%m-%d"))

rfc <- ifelse(is.na(m$RFSTDTC), "", m$RFSTDTC)
rf <- suppressWarnings(as.Date(rfc, format = "%Y-%m-%d"))
st <- suppressWarnings(as.Date(sdtm_tr1$TRDTC, format = "%Y-%m-%d"))
diff <- as.numeric(st - rf)
sdtm_tr1$TRSTDY <- ifelse(is.na(diff), NA, ifelse(diff >= 0, diff + 1, diff))

# ---------------- 4. TRSEQ ----------------
sdtm_tr1 <- sdtm_tr1[sas_order(sdtm_tr1$USUBJID, sdtm_tr1$TRDTC), ]
sdtm_tr1$TRSEQ <- ave(seq_len(nrow(sdtm_tr1)), sdtm_tr1$USUBJID, FUN = seq_along)

sdtm_tr <- sdtm_tr1[, c("STUDYID", "DOMAIN", "USUBJID", "TRSEQ", "TRLNKID", "TRTESTCD",
                        "TRTEST", "TRORRES", "TRORRESU", "TRSTRESC", "TRSTRESN",
                        "TRSTRESU", "TREVAL", "VISIT", "TRDTC", "TRSTDY")]
rownames(sdtm_tr) <- NULL

write.csv(sdtm_tr, file.path(out, "tr.csv"), row.names = FALSE, na = "")

# ---------------- 5. listing (PROC PRINT) ----------------
lst <- cbind(Obs = seq_len(nrow(sdtm_tr)), sdtm_tr)
fmtnum <- function(x) ifelse(is.na(x), ".", format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE))
disp <- lst
for (v in names(disp)) disp[[v]] <- if (is.numeric(disp[[v]])) fmtnum(disp[[v]]) else disp[[v]]
write.csv(lst, file.path(out, "P028.csv"), row.names = FALSE, na = "")

widths <- sapply(names(disp), function(v) max(nchar(v), nchar(disp[[v]]), na.rm = TRUE))
line_of <- function(vals) paste(mapply(function(s, w) formatC(s, width = w), vals, widths), collapse = "  ")
txt <- c("The SAS System", "", line_of(names(disp)), "")
for (i in seq_len(nrow(disp))) txt <- c(txt, line_of(unlist(disp[i, ])))
writeLines(txt, file.path(out, "P028.txt"))

cat("P028 done:", nrow(sdtm_tr), "records; TRSTDY:", paste(sdtm_tr$TRSTDY, collapse = ","), "\n")
