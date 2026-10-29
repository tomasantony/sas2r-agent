suppressPackageStartupMessages({
  library(readxl); library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
root   <- "D:/meta_hackathon/sas2r-agent"
rawdir <- file.path(root, "data/raw/faijal-oncology/raw_data")
if (!dir.exists(rawdir) && dir.exists("/bench/data/raw/faijal-oncology/raw_data"))
  rawdir <- "/bench/data/raw/faijal-oncology/raw_data"

rd <- function(n) {
  f <- file.path(rawdir, paste0(n, ".xlsx"))
  if (file.exists(f)) as.data.frame(read_excel(f)) else NULL
}

## ---------- helpers ----------
mon <- c(JAN=1,FEB=2,MAR=3,APR=4,MAY=5,JUN=6,JUL=7,AUG=8,SEP=9,OCT=10,NOV=11,DEC=12)
parse_date11 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  x <- toupper(trimws(as.character(x)))
  res <- rep(as.Date(NA), length(x))
  m <- regmatches(x, regexec("^([0-9]{1,2})-?([A-Z]{3})-?([0-9]{2,4})$", x))
  for (i in seq_along(x)) {
    if (length(m[[i]]) == 4) {
      d <- as.integer(m[[i]][2]); mo <- mon[m[[i]][3]]; y <- as.integer(m[[i]][4])
      if (nchar(m[[i]][4]) == 2) y <- ifelse(y < 26, y + 2000, y + 1900)
      if (!is.na(mo)) res[i] <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mo, d)))
    } else if (!is.na(x[i]) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x[i])) {
      res[i] <- as.Date(substr(x[i], 1, 10))
    }
  }
  res
}
num_chr <- function(x) {
  if (is.numeric(x)) { r <- ifelse(is.na(x), "", format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)); return(trimws(r)) }
  x <- as.character(x); x[is.na(x)] <- ""; trimws(x)
}
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
trunc_len <- function(x, n) substr(chr(x), 1, n)
mk_usubjid <- function(s) { s <- num_chr(s); ifelse(grepl("^ONCO-2026", s), s, paste0("ONCO-2026-", s)) }

## ---------- 1. DM for study day (RFSTDTC = randomization date) ----------
rf_tab <- NULL
iv <- rd("ivrs_rand")
if (!is.null(iv)) {
  names(iv) <- toupper(names(iv))
  scol <- intersect(c("USUBJID", "SUBJID", "SUBJECT", "SUBJ"), names(iv))
  dcols <- names(iv)[grepl("DT|DATE|DTC", names(iv))]
  pref <- dcols[grepl("RAND", dcols)]
  dcol <- if (length(pref)) pref[1] else if (length(dcols)) dcols[1] else NA
  if (length(scol) && !is.na(dcol)) {
    rf_tab <- data.frame(USUBJID = mk_usubjid(iv[[scol[1]]]), rf = parse_date11(iv[[dcol]]),
                         stringsAsFactors = FALSE)
  }
}
if (is.null(rf_tab)) {
  edm <- rd("edc_dm")
  if (!is.null(edm)) {
    names(edm) <- toupper(names(edm))
    dcols <- names(edm)[grepl("DT|DATE|DTC", names(edm)) & !grepl("BRTH|BIRTH|DOB", names(edm))]
    rf <- if (length(dcols)) parse_date11(edm[[dcols[1]]]) else rep(as.Date(NA), nrow(edm))
    rf_tab <- data.frame(USUBJID = mk_usubjid(edm$SUBJID), rf = rf, stringsAsFactors = FALSE)
  } else rf_tab <- data.frame(USUBJID = character(0), rf = as.Date(character(0)))
}
dm_for_ae <- data.frame(USUBJID = rf_tab$USUBJID,
                        RFSTDTC = ifelse(is.na(rf_tab$rf), "", format(rf_tab$rf, "%Y-%m-%d")),
                        stringsAsFactors = FALSE)
dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID, method = "radix"), , drop = FALSE]
dm_for_ae <- dm_for_ae[!duplicated(dm_for_ae$USUBJID), , drop = FALSE]

## ---------- 2. Raw AE ----------
ae <- rd("edc_ae")
names(ae) <- toupper(names(ae))
sid <- num_chr(ae$SUBJID)
ae$USUBJID <- substr(ifelse(sid == "", "ONCO-2026", paste0("ONCO-2026-", sid)), 1, 25)
sd <- parse_date11(ae$STDT); ed <- parse_date11(ae$ENDT)
ae$AESTDTC <- ifelse(is.na(sd), "", format(sd, "%Y-%m-%d"))
ae$AEENDTC <- ifelse(is.na(ed), "", format(ed, "%Y-%m-%d"))

## ---------- 3. sort ----------
ae <- ae[order(ae$USUBJID, ae$AESTDTC, chr(ae$TERM), method = "radix"), , drop = FALSE]

## ---------- 4. merge + derive ----------
ae <- left_join(ae, dm_for_ae, by = "USUBJID")
ae$RFSTDTC[is.na(ae$RFSTDTC)] <- ""
ae$STUDYID  <- "ONCO-2026"
ae$DOMAIN   <- "AE"
ae$AETERM   <- trunc_len(ae$TERM, 40)
ae$AEDECOD  <- trunc_len(toupper(chr(ae$DECOD)), 40)
ae$AEBODSYS <- trunc_len(gsub("_", " ", chr(ae$BODSYS), fixed = TRUE), 40)
ae$AESEV    <- trunc_len(toupper(chr(ae$SEV)), 10)
ae$AETXGR   <- suppressWarnings(as.numeric(ae$TXGR))
ae$AEREL    <- trunc_len(toupper(chr(ae$REL)), 20)
ae$AEOUT    <- trunc_len(toupper(chr(ae$OUT)), 20)
ae$AESER    <- trunc_len(ae$SER, 1)

rfd <- suppressWarnings(as.Date(substr(ae$RFSTDTC, 1, 10), format = "%Y-%m-%d"))
stdd <- suppressWarnings(as.Date(ae$AESTDTC, format = "%Y-%m-%d"))
endd <- suppressWarnings(as.Date(ae$AEENDTC, format = "%Y-%m-%d"))
dy <- function(d, r, cs) {
  v <- as.numeric(d - r)
  o <- ifelse(is.na(v), NA, ifelse(v >= 0, v + 1, v))
  o[!cs] <- NA
  o
}
ae$AESTDY <- dy(stdd, rfd, ae$AESTDTC != "" & ae$RFSTDTC != "")
ae$AEENDY <- dy(endd, rfd, ae$AEENDTC != "" & ae$RFSTDTC != "")

## ---------- 5. sort ----------
ae <- ae[order(ae$USUBJID, ae$AESTDTC, chr(ae$TERM), method = "radix"), , drop = FALSE]

## ---------- 6. AESEQ ----------
ae$AESEQ <- ave(seq_len(nrow(ae)), ae$USUBJID, FUN = seq_along)
sdtm_ae <- ae[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV",
                  "AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY")]
rownames(sdtm_ae) <- NULL

## ---------- outputs ----------
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

lst <- cbind(Obs = seq_len(nrow(sdtm_ae)), sdtm_ae)
lst_chr <- as.data.frame(lapply(lst, function(v) {
  if (is.numeric(v)) ifelse(is.na(v), ".", format(v, trim = TRUE, drop0trailing = TRUE)) else chr(v)
}), stringsAsFactors = FALSE)
write.csv(lst, file.path(out, "P022.csv"), row.names = FALSE, na = "")
txt <- c("The SAS System", "", capture.output(print(lst_chr, row.names = FALSE, right = TRUE)))
writeLines(txt, file.path(out, "P022.txt"))
cat("P022 done:", nrow(sdtm_ae), "AE records\n")
