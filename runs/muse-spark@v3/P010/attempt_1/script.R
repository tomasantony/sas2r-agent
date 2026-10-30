out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_conmeds.csv"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_strip <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x, which = "both")
}
sas_upcase <- function(x) {
  x <- as.character(x)
  toupper(x)
}
sas_scan <- function(x, n, delim = " ") {
  # SAS SCAN: ignore leading delimiters, consecutive delimiters count as one
  if (is.na(x) || trimws(as.character(x), which = "left") == "") {
    return("")
  }
  s <- as.character(x)
  # trim leading/trailing delimiters (spaces)
  # general: split on one-or-more of delim chars
  # delim is single space here
  s_trim <- sub("^[ ]+", "", s)
  s_trim <- sub("[ ]+$", "", s_trim)
  if (s_trim == "") return("")
  parts <- strsplit(s_trim, "[ ]+", perl = TRUE)[[1]]
  if (n >= 1 && n <= length(parts)) parts[n] else ""
}
parse_ddmmyy <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  v <- trimws(x)
  res <- rep(NA_character_, length(v))
  idx <- v != ""
  if (any(idx)) {
    d <- suppressWarnings(as.Date(v[idx], format = "%d/%m/%Y"))
    ok <- !is.na(d)
    res[idx][ok] <- format(d[ok], "%Y-%m-%d")
  }
  res[is.na(res)] <- ""
  res
}

# 2. IMPORT RAW DATA
# infile delimiter=',' dsd missover firstobs=2 ; lengths SUBJECT $10 etc.
raw_df <- utils::read.csv(raw_path, header = FALSE, skip = 1,
  colClasses = "character", na.strings = "__MISSING__",
  strip.white = FALSE, stringsAsFactors = FALSE,
  fill = TRUE, quote = "\"", comment.char = "", blank.lines.skip = TRUE)
# ensure 6 columns
if (ncol(raw_df) < 6) {
  for (j in (ncol(raw_df) + 1):6) raw_df[[j]] <- ""
}
if (ncol(raw_df) > 6) raw_df <- raw_df[, 1:6, drop = FALSE]
colnames(raw_df) <- c("SUBJECT","MEDICATION","DOSE","START_DT","END_DT","ONGOING")
for (nm in colnames(raw_df)) {
  v <- raw_df[[nm]]
  v[is.na(v)] <- ""
  v[v == "__MISSING__"] <- ""
  raw_df[[nm]] <- v
}
# LENGTH truncation
raw_df$SUBJECT <- substr(raw_df$SUBJECT, 1, 10)
raw_df$MEDICATION <- substr(raw_df$MEDICATION, 1, 50)
raw_df$DOSE <- substr(raw_df$DOSE, 1, 20)
raw_df$START_DT <- substr(raw_df$START_DT, 1, 10)
raw_df$END_DT <- substr(raw_df$END_DT, 1, 10)
raw_df$ONGOING <- substr(raw_df$ONGOING, 1, 1)

# 3. PROCESS AND MAP TO SDTM
n <- nrow(raw_df)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("CM", n)
USUBJID <- paste0(sas_strip(STUDYID), "-", sas_strip(raw_df$SUBJECT))
CMTRT <- sas_strip(raw_df$MEDICATION)
CMDECOD <- ifelse(sas_upcase(CMTRT) == "PARACETAMOL", "ACETAMINOPHEN", sas_upcase(CMTRT))
CMDECOD[is.na(CMDECOD)] <- ""
# Dose parsing
tok1 <- vapply(raw_df$DOSE, function(z) sas_scan(z, 1, " "), character(1), USE.NAMES = FALSE)
tok2 <- vapply(raw_df$DOSE, function(z) sas_scan(z, 2, " "), character(1), USE.NAMES = FALSE)
CMDOSE <- suppressWarnings(as.numeric(tok1))
CMDOSE[tok1 == ""] <- NA_real_
CMDOSU <- sas_strip(tok2)
CMSTDTC <- parse_ddmmyy(raw_df$START_DT)
CMENDTC <- parse_ddmmyy(raw_df$END_DT)
ONGOING_TRIM <- sas_strip(raw_df$ONGOING)
CMENRTPT <- ifelse(ONGOING_TRIM == "Y", "ONGOING", "")
CMENRTPT[is.na(CMENRTPT)] <- ""

cm_draft <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  CMTRT = CMTRT, CMDECOD = CMDECOD, CMDOSE = CMDOSE, CMDOSU = CMDOSU,
  CMSTDTC = CMSTDTC, CMENDTC = CMENDTC, CMENRTPT = CMENRTPT,
  stringsAsFactors = FALSE
)
# truncate to lengths: CMTRT/CMDECOD $50, CMDOSU $20, CMSTDTC/CMENDTC $10, CMENRTPT $8
cm_draft$CMTRT <- substr(cm_draft$CMTRT, 1, 50)
cm_draft$CMDECOD <- substr(cm_draft$CMDECOD, 1, 50)
cm_draft$CMDOSU <- substr(cm_draft$CMDOSU, 1, 20)
cm_draft$CMSTDTC[cm_draft$CMSTDTC == ""] <- ""
cm_draft$CMENDTC[cm_draft$CMENDTC == ""] <- ""
cm_draft$CMENRTPT <- substr(cm_draft$CMENRTPT, 1, 8)

# 4. SORT AND ASSIGN SEQUENCE NUMBER
# SAS sort by USUBJID CMSTDTC CMTRT (missing first, uppercase before lowercase)
ord <- order(cm_draft$USUBJID, cm_draft$CMSTDTC, cm_draft$CMTRT, method = "radix", na.last = FALSE)
cm_draft <- cm_draft[ord, , drop = FALSE]

# Read sdtm.dm keep USUBJID RFENDTC
read_dm_anchor <- function() {
  cand <- c(
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/dm.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/DM.sas7bdat",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/DM.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/dm.csv",
    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm/DM.csv",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt"
  )
  # also discover via list.files
  extra <- character(0)
  if (dir.exists(sdtm_dir)) {
    fl <- list.files(sdtm_dir, full.names = TRUE, ignore.case = FALSE)
    # case-insensitive match dm.*
    m <- fl[toupper(basename(fl)) == toupper(basename(fl)) & grepl("^dm\\.", basename(fl), ignore.case = TRUE)]
    extra <- m
  }
  allcand <- unique(c(cand, extra))
  for (f in allcand) {
    if (!is.na(f) && f != "" && file.exists(f)) {
      lf <- tolower(f)
      if (grepl("\\.sas7bdat$", lf)) {
        dm <- haven::read_sas(f)
        return(as.data.frame(dm, stringsAsFactors = FALSE))
      } else if (grepl("\\.xpt$", lf)) {
        dm <- haven::read_xpt(f)
        return(as.data.frame(dm, stringsAsFactors = FALSE))
      } else if (grepl("\\.csv$", lf)) {
        dm <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE)
        return(as.data.frame(dm, stringsAsFactors = FALSE))
      }
    }
  }
  return(NULL)
}
dm_raw <- read_dm_anchor()
if (is.null(dm_raw)) {
  dm_anchor <- data.frame(USUBJID = unique(cm_draft$USUBJID), stringsAsFactors = FALSE)
  dm_anchor$`_CMENTPT_ANCHOR` <- ""
} else {
  cn <- colnames(dm_raw)
  up <- toupper(cn)
  usub_col <- cn[match("USUBJID", up)]
  rfend_col <- cn[match("RFENDTC", up)]
  stopifnot(!is.na(usub_col), !is.na(rfend_col))
  usub_v <- as.character(dm_raw[[usub_col]])
  rf_v <- dm_raw[[rfend_col]]
  if (inherits(rf_v, "Date")) {
    rf_c <- format(rf_v, "%Y-%m-%d")
  } else if (inherits(rf_v, "POSIXct") || inherits(rf_v, "POSIXt")) {
    rf_c <- format(rf_v, "%Y-%m-%dT%H:%M:%S")
  } else {
    rf_c <- as.character(rf_v)
  }
  rf_c[is.na(rf_c)] <- ""
  # SAS length $10 truncation
  anchor_c <- substr(rf_c, 1, 10)
  # SAS missing sorts first; sort by USUBJID
  dm_anchor <- data.frame(USUBJID = usub_v, `_CMENTPT_ANCHOR` = anchor_c, stringsAsFactors = FALSE, check.names = FALSE)
  colnames(dm_anchor) <- c("USUBJID", "_CMENTPT_ANCHOR")
  dm_anchor$USUBJID[is.na(dm_anchor$USUBJID)] <- ""
  dm_anchor$`_CMENTPT_ANCHOR`[is.na(dm_anchor$`_CMENTPT_ANCHOR`)] <- ""
  o2 <- order(dm_anchor$USUBJID, method = "radix", na.last = FALSE)
  dm_anchor <- dm_anchor[o2, , drop = FALSE]
  # deduplicate to one per USUBJID (keep first) to emulate single anchor
  dm_anchor <- dm_anchor[!duplicated(dm_anchor$USUBJID), , drop = FALSE]
}
# drop/rename overlapping columns except keys before merging
# cm_draft has no _CMENTPT_ANCHOR; ensure dm_anchor only has keys + anchor
stopifnot("USUBJID" %in% colnames(cm_draft))
stopifnot("USUBJID" %in% colnames(dm_anchor))
stopifnot("_CMENTPT_ANCHOR" %in% colnames(dm_anchor))

# merge work.cm_draft(in=in_cm) work.dm_cm_anchor; by USUBJID; if in_cm (left join)
cm_merged <- merge(cm_draft, dm_anchor, by = "USUBJID", all.x = TRUE, sort = FALSE)
# missing anchor -> ""
cm_merged$`_CMENTPT_ANCHOR`[is.na(cm_merged$`_CMENTPT_ANCHOR`)] <- ""
# re-sort to BY order (USUBJID CMSTDTC CMTRT) to ensure FIRST. logic
o3 <- order(cm_merged$USUBJID, cm_merged$CMSTDTC, cm_merged$CMTRT, method = "radix", na.last = FALSE)
cm_merged <- cm_merged[o3, , drop = FALSE]

# CMENTPT logic
CMENTPT <- ifelse(cm_merged$CMENRTPT == "ONGOING", cm_merged$`_CMENTPT_ANCHOR`, "")
CMENTPT[is.na(CMENTPT)] <- ""
# putlog error check (write to console, not fail)
ongoing_missing <- which(cm_merged$CMENRTPT == "ONGOING" & (is.na(CMENTPT) | CMENTPT == ""))
if (length(ongoing_missing) > 0) {
  for (i in ongoing_missing) {
    message(paste0("ERROR: Ongoing CM record has no DM reference end: USUBJID=", cm_merged$USUBJID[i]))
  }
}
cm_merged$CMENTPT <- CMENTPT
# CMSEQ by first.USUBJID
cm_merged$CMSEQ <- ave(seq_len(nrow(cm_merged)), cm_merged$USUBJID, FUN = function(z) seq_along(z))
cm_merged$CMSEQ <- as.numeric(cm_merged$CMSEQ)

# retain order
cm <- cm_merged[, c("STUDYID","DOMAIN","USUBJID","CMSEQ","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT","CMENTPT"), drop = FALSE]

# Write dataset cm
write.csv(cm, file.path(out, "cm.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT: PROC PRINT obs=15 title SDTM CM Domain - Concomitant Medications
ttl <- "SDTM CM Domain - Concomitant Medications"
nprint <- min(15, nrow(cm))
prt <- cm[seq_len(nprint), , drop = FALSE]
# text rendering
txt_path <- file.path(out, "P010.txt")
lst_path <- file.path(out, "P010.lst")
csv_path <- file.path(out, "P010.csv")
# Build plain-text table similar to PROC PRINT (Obs + all vars)
df_txt <- data.frame(Obs = seq_len(nprint), prt, stringsAsFactors = FALSE, check.names = FALSE)
# format numerics like SAS BEST (no rounding)
fmt_cell <- function(v) {
  if (is.numeric(v)) {
    s <- ifelse(is.na(v), "", as.character(v))
    s
  } else {
    v[is.na(v)] <- ""
    as.character(v)
  }
}
df_c <- as.data.frame(lapply(df_txt, fmt_cell), stringsAsFactors = FALSE, check.names = FALSE)
# compute column widths
cnames <- colnames(df_c)
widths <- sapply(seq_along(cnames), function(j) max(nchar(cnames[j]), max(nchar(df_c[[j]]), na.rm = TRUE), na.rm = TRUE))
# header lines
header_line <- paste(sprintf(paste0("%-", widths, "s"), cnames), collapse = "  ")
sep_line <- paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "  ")
body_lines <- apply(df_c, 1, function(r) paste(sprintf(paste0("%-", widths, "s"), r), collapse = "  "))
con <- file(txt_path, open = "wt")
writeLines(ttl, con)
writeLines("", con)
writeLines(header_line, con)
writeLines(sep_line, con)
for (ln in body_lines) writeLines(ln, con)
writeLines("", con)
close(con)
# copy to .lst expected name
file.copy(txt_path, lst_path, overwrite = TRUE)
# CSV of table body (same 15 rows with Obs)
write.csv(df_txt, csv_path, row.names = FALSE, na = "")
