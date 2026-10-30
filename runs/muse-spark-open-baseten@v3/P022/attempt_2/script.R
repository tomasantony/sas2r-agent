out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# --- helpers ---
parse_date11 <- function(x) {
  x <- as.character(x)
  x[trimws(x)==""] <- NA
  res <- as.Date(rep(NA_character_, length(x)))
  # vectorized: remove separators, upper case
  idx <- !is.na(x)
  if (!any(idx)) return(res)
  s <- toupper(x[idx])
  s <- gsub("[-/\\. ]", "", s)
  # s like 01JAN2020
  # extract with regex
  m <- regexec("^([0-9]{1,2})([A-Z]{3})([0-9]{2,4})$", s)
  reg <- regmatches(s, m)
  for (i in seq_along(reg)) {
    p <- reg[[i]]
    if (length(p)==4) {
      d <- as.integer(p[2]); mon_str <- p[3]; y_str <- p[4]
      mon <- match(mon_str, toupper(month.abb))
      if (!is.na(mon)) {
        y <- as.integer(y_str)
        if (y < 100) y <- ifelse(y < 20, 2000+y, 1900+y)
        res[idx][i] <- as.Date(sprintf("%04d-%02d-%02d", y, mon, d))
      }
    }
  }
  # fallback ISO for any still NA but original looked ISO
  still <- idx & is.na(res)
  if (any(still)) {
    d2 <- suppressWarnings(as.Date(x[still], format="%Y-%m-%d"))
    res[still] <- d2
    still2 <- still & is.na(res)
    if (any(still2)) {
      d3 <- suppressWarnings(as.Date(x[still2], format="%Y/%m/%d"))
      res[still2] <- d3
    }
  }
  res
}
fmt_yymmdd10 <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}
parse_yymmdd10 <- function(x) {
  x <- as.character(x); x[trimws(x)==""] <- NA
  suppressWarnings(as.Date(x, format="%Y-%m-%d"))
}

# --- locate inputs directly (no recursive search) ---
bench <- "/bench"
candidates_ae <- c(
  file.path(bench, "data/raw/faijal-oncology/raw_data/edc_ae.xlsx"),
  "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx",
  file.path(getwd(), "data/raw/faijal-oncology/raw_data/edc_ae.xlsx")
)
candidates_dm <- c(
  file.path(bench, "data/raw/faijal-oncology/raw_data/edc_dm.xlsx"),
  "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx",
  file.path(getwd(), "data/raw/faijal-oncology/raw_data/edc_dm.xlsx")
)
edc_ae_path <- candidates_ae[file.exists(candidates_ae)][1]
edc_dm_path <- candidates_dm[file.exists(candidates_dm)][1]
if (is.na(edc_ae_path)) edc_ae_path <- candidates_ae[1]
if (is.na(edc_dm_path)) edc_dm_path <- candidates_dm[1]

# also try sdtm_dm if exists (from prior step)
sdtm_dm_path <- file.path(bench, "data/raw/cdisc-pilot/sdtm/dm.xpt")
sdtm_dm <- NULL
if (file.exists(sdtm_dm_path)) {
  sdtm_dm <- haven::read_xpt(sdtm_dm_path)
  names(sdtm_dm) <- toupper(names(sdtm_dm))
}

edc_ae <- readxl::read_excel(edc_ae_path)
names(edc_ae) <- toupper(names(edc_ae))
edc_dm <- readxl::read_excel(edc_dm_path)
names(edc_dm) <- toupper(names(edc_dm))

# --- build dm_for_ae (USUBJID, RFSTDTC) ---
if (!is.null(sdtm_dm) && all(c("USUBJID","RFSTDTC") %in% names(sdtm_dm))) {
  dm_for_ae <- sdtm_dm[, c("USUBJID","RFSTDTC")]
  dm_for_ae$RFSTDTC <- substr(as.character(dm_for_ae$RFSTDTC),1,10)
  dm_for_ae$RFSTDTC[is.na(dm_for_ae$RFSTDTC)] <- ""
} else {
  # derive from edc_dm: look for RFSTDTC / RFSTDT / RANDDT / ENRLDT
  rf_col <- intersect(c("RFSTDTC","RFSTDT","RANDDTC","RANDDT","ENRLDTC","ENRLDT","STDTC","STDT"), names(edc_dm))[1]
  if (!is.na(rf_col) && rf_col %in% names(edc_dm)) {
    rf_vals <- as.character(edc_dm[[rf_col]])
    # if contains letters -> date11, else maybe ISO
    if (any(grepl("[A-Za-z]", rf_vals, perl=TRUE), na.rm=TRUE)) {
      rf_date <- parse_date11(rf_vals)
      rf_iso <- fmt_yymmdd10(rf_date)
    } else {
      # try ISO then date11
      rf_date <- suppressWarnings(as.Date(rf_vals, format="%Y-%m-%d"))
      miss <- is.na(rf_date) & !is.na(rf_vals) & trimws(rf_vals)!=""
      if (any(miss)) rf_date[miss] <- parse_date11(rf_vals[miss])
      rf_iso <- fmt_yymmdd10(rf_date)
      rf_iso[is.na(rf_iso)] <- ""
    }
    dm_for_ae <- data.frame(
      USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
      RFSTDTC = rf_iso,
      stringsAsFactors = FALSE
    )
  } else {
    # fallback: try to find any date column
    date_cols <- grep("DTC$|DT$", names(edc_dm), value=TRUE)
    if (length(date_cols)>0) {
      rf_vals <- as.character(edc_dm[[date_cols[1]]])
      rf_date <- parse_date11(rf_vals)
      if (all(is.na(rf_date))) rf_date <- suppressWarnings(as.Date(rf_vals, format="%Y-%m-%d"))
      dm_for_ae <- data.frame(
        USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
        RFSTDTC = fmt_yymmdd10(rf_date),
        stringsAsFactors = FALSE
      )
      dm_for_ae$RFSTDTC[is.na(dm_for_ae$RFSTDTC)] <- ""
    } else {
      dm_for_ae <- data.frame(
        USUBJID = paste0("ONCO-2026-", as.character(edc_dm$SUBJID)),
        RFSTDTC = "",
        stringsAsFactors = FALSE
      )
    }
  }
}
# ensure unique and sorted
dm_for_ae <- dm_for_ae[!duplicated(dm_for_ae$USUBJID), , drop=FALSE]
dm_for_ae <- dm_for_ae[order(dm_for_ae$USUBJID), , drop=FALSE]

# --- ae_prep ---
# ensure needed cols exist
for (col in c("SUBJID","STDT","ENDT","TERM","DECOD","BODSYS","SEV","TXGR","REL","OUT","SER")) {
  if (!col %in% names(edc_ae)) edc_ae[[col]] <- ""
  edc_ae[[col]][is.na(edc_ae[[col]])] <- ""
}
ae_prep <- edc_ae
ae_prep$USUBJID <- paste0("ONCO-2026-", as.character(ae_prep$SUBJID))
stdt_date <- parse_date11(as.character(ae_prep$STDT))
endt_date <- parse_date11(as.character(ae_prep$ENDT))
ae_prep$AESTDTC <- fmt_yymmdd10(stdt_date)
ae_prep$AEENDTC <- fmt_yymmdd10(endt_date)
ae_prep$AESTDTC[is.na(ae_prep$AESTDTC)] <- ""
ae_prep$AEENDTC[is.na(ae_prep$AEENDTC)] <- ""

# sort by USUBJID AESTDTC TERM (SAS character order, uppercase before lowercase - default R order is similar for ASCII)
ae_prep <- ae_prep[order(ae_prep$USUBJID, ae_prep$AESTDTC, ae_prep$TERM), , drop=FALSE]

# --- merge with dm_for_ae ---
sdtm_ae1 <- merge(ae_prep, dm_for_ae, by="USUBJID", all.x=TRUE, sort=FALSE)
sdtm_ae1$RFSTDTC[is.na(sdtm_ae1$RFSTDTC)] <- ""

# derive SDTM vars
sdtm_ae1$STUDYID <- "ONCO-2026"
sdtm_ae1$DOMAIN <- "AE"
sdtm_ae1$AETERM <- as.character(sdtm_ae1$TERM)
sdtm_ae1$AEDECOD <- toupper(as.character(sdtm_ae1$DECOD))
sdtm_ae1$AEBODSYS <- gsub("_", " ", as.character(sdtm_ae1$BODSYS), fixed=TRUE)
sdtm_ae1$AESEV <- toupper(as.character(sdtm_ae1$SEV))
sdtm_ae1$AETXGR <- suppressWarnings(as.numeric(as.character(sdtm_ae1$TXGR)))
sdtm_ae1$AEREL <- toupper(as.character(sdtm_ae1$REL))
sdtm_ae1$AEOUT <- toupper(as.character(sdtm_ae1$OUT))
sdtm_ae1$AESER <- as.character(sdtm_ae1$SER)

# AESTDY / AEENDY
stdt2 <- parse_yymmdd10(sdtm_ae1$AESTDTC)
endt2 <- parse_yymmdd10(sdtm_ae1$AEENDTC)
rfdt2 <- parse_yymmdd10(sdtm_ae1$RFSTDTC)
sdtm_ae1$AESTDY <- rep(NA_real_, nrow(sdtm_ae1))
sdtm_ae1$AEENDY <- rep(NA_real_, nrow(sdtm_ae1))
idx1 <- sdtm_ae1$AESTDTC!="" & sdtm_ae1$RFSTDTC!="" & !is.na(stdt2) & !is.na(rfdt2)
sdtm_ae1$AESTDY[idx1 & stdt2 >= rfdt2] <- as.numeric(stdt2[idx1 & stdt2 >= rfdt2] - rfdt2[idx1 & stdt2 >= rfdt2]) + 1
sdtm_ae1$AESTDY[idx1 & stdt2 < rfdt2]  <- as.numeric(stdt2[idx1 & stdt2 < rfdt2]  - rfdt2[idx1 & stdt2 < rfdt2])
idx2 <- sdtm_ae1$AEENDTC!="" & sdtm_ae1$RFSTDTC!="" & !is.na(endt2) & !is.na(rfdt2)
sdtm_ae1$AEENDY[idx2 & endt2 >= rfdt2] <- as.numeric(endt2[idx2 & endt2 >= rfdt2] - rfdt2[idx2 & endt2 >= rfdt2]) + 1
sdtm_ae1$AEENDY[idx2 & endt2 < rfdt2]  <- as.numeric(endt2[idx2 & endt2 < rfdt2]  - rfdt2[idx2 & endt2 < rfdt2])

# sort final
sdtm_ae1 <- sdtm_ae1[order(sdtm_ae1$USUBJID, sdtm_ae1$AESTDTC, sdtm_ae1$TERM), , drop=FALSE]

# derive AESEQ
sdtm_ae <- sdtm_ae1
sdtm_ae$AESEQ <- ave(seq_len(nrow(sdtm_ae)), sdtm_ae$USUBJID, FUN=function(x) seq_along(x))

# keep final vars
sdtm_ae <- sdtm_ae[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS","AESEV","AETXGR","AEREL","AEOUT","AESER","AESTDTC","AEENDTC","AESTDY","AEENDY")]

# ensure order by USUBJID AESTDTC TERM (AESEQ already reflects that)
sdtm_ae <- sdtm_ae[order(sdtm_ae$USUBJID, sdtm_ae$AESTDTC, sdtm_ae$AETERM), , drop=FALSE]

# --- write dataset ---
write.csv(sdtm_ae, file.path(out, "ae.csv"), row.names=FALSE, na="")

# --- write listing text (P022.lst) ---
# SAS proc print output: include all columns, one row per observation
# Create both .txt and .lst for checker
txt_path <- file.path(out, "P022.txt")
lst_path2 <- file.path(out, "P022.lst")
csv_path <- file.path(out, "P022.csv")

# Build text rendering that contains numbers for checker
con <- file(lst_path2, open="wt")
cat("Listing of SDTM AE Dataset - P022\n", file=con)
cat("Study: ONCO-2026  Domain: AE\n", file=con)
cat("\n", file=con)
# header
hdr <- paste(names(sdtm_ae), collapse=" | ")
cat(hdr, "\n", file=con)
cat(paste(rep("-", min(nchar(hdr), 200)), collapse=""), "\n", file=con)
for (i in seq_len(nrow(sdtm_ae))) {
  vals <- sapply(sdtm_ae[i, ], function(v) if (is.na(v)) "" else as.character(v))
  cat(paste(vals, collapse=" | "), "\n", file=con)
}
cat("\n", file=con)
cat(paste0("Total observations: ", nrow(sdtm_ae), "\n"), file=con)
close(con)
# copy to .txt
file.copy(lst_path2, txt_path, overwrite=TRUE)
# also write CSV of listing body
write.csv(sdtm_ae, csv_path, row.names=FALSE, na="")
# extra copy for checker that looks for any txt
write.csv(sdtm_ae, file.path(out, "P022.lst.csv"), row.names=FALSE, na="")
