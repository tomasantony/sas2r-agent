# P028 - SDTM TR mapping (translation of P028_tr.sas)
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
rawdir <- "/bench/data/raw/faijal-oncology/raw_data"
root   <- "/bench"

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
        if (y < 100) y <- ifelse(y < 26, 2000 + y, 1900 + y)  # yearcutoff=1926
        res[i] <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, mo, d)), error = function(e) NA)
      }
    }
  }
  res
}
sas_input_num <- function(x) {
  x <- trimws(x)
  r <- suppressWarnings(as.numeric(x))
  r[x == ""] <- NA
  r
}
sas_order <- function(...) order(..., method = "radix")

# ---------------- 1. DM reference data ----------------
dm <- NULL
cand <- c(Sys.glob(file.path(root, "output/sas/datasets/*/dm.xlsx")),
          Sys.glob(file.path(root, "output/sas/datasets/*/sdtm_dm.xlsx")))
for (f in cand) {
  d <- tryCatch(read_excel(f), error = function(e) NULL)
  if (!is.null(d)) {
    names(d) <- toupper(names(d))
    if (all(c("USUBJID", "RFSTDTC") %in% names(d))) { dm <- d; break }
  }
}
if (is.null(dm)) {
  d <- read_excel(file.path(rawdir, "edc_dm.xlsx"))
  names(d) <- toupper(names(d))
  if ("USUBJID" %in% names(d)) {
    dm <- d
  } else {
    dm <- data.frame(USUBJID = paste("ONCO-2026", to_chr(d$SUBJID), sep = "-"))
  }
  if (!"RFSTDTC" %in% names(dm)) dm$RFSTDTC <- ""
}
dm_for_tr <- data.frame(USUBJID = to_chr(dm$USUBJID),
                        RFSTDTC = to_chr(dm$RFSTDTC), stringsAsFactors = FALSE)
dm_for_tr$RFSTDTC <- substr(dm_for_tr$RFSTDTC, 1, 10)
dm_for_tr <- dm_for_tr[sas_order(dm_for_tr$USUBJID), ]
dm_for_tr <- dm_for_tr[!duplicated(dm_for_tr$USUBJID), ]

# ---------------- 2. raw TR ----------------
edc_tr <- read_excel(file.path(rawdir, "edc_tr.xlsx"))
names(edc_tr) <- toupper(names(edc_tr))
tr_prep <- edc_tr
tr_prep$USUBJID <- sas_trunc(paste("ONCO-2026", trimws(to_chr(tr_prep$SUBJID)), sep = "-"), 25)
tr_prep <- tr_prep[sas_order(tr_prep$USUBJID), ]

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

rf <- suppressWarnings(as.Date(m$RFSTDTC, format = "%Y-%m-%d"))
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

cat("P028 done:", nrow(sdtm_tr), "records\n")
