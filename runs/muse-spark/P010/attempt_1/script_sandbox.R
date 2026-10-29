out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
raw_csv <- paste0(project_path, "/data/raw/raw_conmeds.csv")
sdtm_dir <- paste0(project_path, "/data/sdtm")

cat("Reading raw:", raw_csv, "\n")

sas_strip <- function(x) {
  ifelse(is.na(x), "", trimws(as.character(x), which = "both"))
}
sas_upcase <- function(x) {
  ifelse(is.na(x) | x == "", "", toupper(x))
}
sas_scan <- function(x, n, delim = " ") {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return("")
    xi <- as.character(xi)
    if (trimws(xi) == "") return("")
    # SAS SCAN with ' ' : consecutive delimiters as one, leading ignored
    # split on one or more blanks
    parts <- strsplit(trimws(xi), "[ ]+", perl = TRUE)[[1]]
    if (length(parts) >= n) trimws(parts[n]) else ""
  }, character(1), USE.NAMES = FALSE)
}
sas_best <- function(x) {
  suppressWarnings(as.numeric(x))
}
sas_dmy_to_iso <- function(x) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return("")
    xi <- trimws(as.character(xi))
    if (xi == "") return("")
    # try lubridate dmy then base formats
    d <- suppressWarnings(lubridate::dmy(xi, quiet = TRUE))
    if (is.na(d)) {
      # fallback base try ddmmyy10 variants
      d2 <- suppressWarnings(as.Date(xi, format = "%d/%m/%Y"))
      if (!is.na(d2)) d <- d2 else {
        d3 <- suppressWarnings(as.Date(xi, format = "%d/%m/%y"))
        if (!is.na(d3)) d <- d3
      }
    }
    if (is.na(d)) return("")
    format(d, "%Y-%m-%d")
  }, character(1), USE.NAMES = FALSE)
}

# 2. IMPORT RAW DATA mimicking infile firstobs=2 dsd missover
if (!file.exists(raw_csv)) stop(paste0("Raw file not found: ", raw_csv))
raw_df <- utils::read.csv(raw_csv, skip = 1, header = FALSE, colClasses = "character",
  sep = ",", quote = "\"", na.strings = NULL, strip.white = FALSE,
  fill = TRUE, comment.char = "", blank.lines.skip = TRUE, stringsAsFactors = FALSE)
# ensure 6 cols
while (ncol(raw_df) < 6) raw_df[[ncol(raw_df)+1]] <- ""
if (ncol(raw_df) > 6) raw_df <- raw_df[, 1:6]
names(raw_df) <- c("SUBJECT","MEDICATION","DOSE","START_DT","END_DT","ONGOING")
# replace NA with ""
for (nm in names(raw_df)) {
  raw_df[[nm]][is.na(raw_df[[nm]])] <- ""
  raw_df[[nm]] <- as.character(raw_df[[nm]])
}
# LENGTH truncation: SUBJECT $10 MEDICATION $50 DOSE $20 START_DT $10 END_DT $10 ONGOING $1
raw_df$SUBJECT <- substr(raw_df$SUBJECT, 1, 10)
raw_df$MEDICATION <- substr(raw_df$MEDICATION, 1, 50)
raw_df$DOSE <- substr(raw_df$DOSE, 1, 20)
raw_df$START_DT <- substr(raw_df$START_DT, 1, 10)
raw_df$END_DT <- substr(raw_df$END_DT, 1, 10)
raw_df$ONGOING <- substr(trimws(raw_df$ONGOING), 1, 1)
# drop fully blank rows that may come from trailing newline
keep_row <- !(trimws(raw_df$SUBJECT)=="" & trimws(raw_df$MEDICATION)=="" & trimws(raw_df$DOSE)=="" & trimws(raw_df$START_DT)=="" & trimws(raw_df$END_DT)=="" & trimws(raw_df$ONGOING)=="")
raw_df <- raw_df[keep_row, , drop = FALSE]

cat("Raw rows:", nrow(raw_df), "\n")

# 3. PROCESS AND MAP TO SDTM
n <- nrow(raw_df)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("CM", n)
USUBJID <- paste0(sas_strip(STUDYID), "-", sas_strip(raw_df$SUBJECT))
# truncate USUBJID to 40 like $40.
USUBJID <- substr(USUBJID, 1, 40)
CMTRT <- sas_strip(raw_df$MEDICATION)
CMTRT <- substr(CMTRT, 1, 50)
up <- toupper(CMTRT)
CMDECOD <- ifelse(up == "PARACETAMOL", "ACETAMINOPHEN", up)
CMDECOD <- substr(CMDECOD, 1, 50)
w1 <- sas_scan(raw_df$DOSE, 1, " ")
w2 <- sas_scan(raw_df$DOSE, 2, " ")
CMDOSE <- sas_best(w1)
CMDOSU <- sas_strip(w2)
CMDOSU <- substr(CMDOSU, 1, 20)
CMSTDTC <- sas_dmy_to_iso(raw_df$START_DT)
CMENDTC <- sas_dmy_to_iso(raw_df$END_DT)
CMSTDTC <- substr(CMSTDTC, 1, 10)
CMENDTC <- substr(CMENDTC, 1, 10)
CMENRTPT <- ifelse(trimws(raw_df$ONGOING) == "Y", "ONGOING", "")
CMENRTPT <- substr(CMENRTPT, 1, 8)

cm_draft <- data.frame(
  STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  CMTRT = CMTRT, CMDECOD = CMDECOD, CMDOSE = CMDOSE, CMDOSU = CMDOSU,
  CMSTDTC = CMSTDTC, CMENDTC = CMENDTC, CMENRTPT = CMENRTPT,
  stringsAsFactors = FALSE
)
# SAS missing char = "" ; ensure NA -> ""
for (cc in c("STUDYID","DOMAIN","USUBJID","CMTRT","CMDECOD","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT")) {
  cm_draft[[cc]][is.na(cm_draft[[cc]])] <- ""
}

# 4. SORT AND ASSIGN SEQUENCE NUMBER
# Force C collation to mimic SAS (uppercase before lowercase), missing first
old_coll <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
sort_key_std <- ifelse(is.na(cm_draft$CMSTDTC), "", cm_draft$CMSTDTC)
sort_key_trt <- ifelse(is.na(cm_draft$CMTRT), "", cm_draft$CMTRT)
sort_key_sub <- ifelse(is.na(cm_draft$USUBJID), "", cm_draft$USUBJID)
ord_draft <- order(sort_key_sub, sort_key_std, sort_key_trt, na.last = FALSE, method = "radix")
cm_draft <- cm_draft[ord_draft, , drop = FALSE]

# Read sdtm.dm keep USUBJID RFENDTC
read_dm <- function() {
  cands <- c(
    paste0(sdtm_dir, "/dm.sas7bdat"),
    paste0(sdtm_dir, "/DM.sas7bdat"),
    paste0(sdtm_dir, "/dm.xpt"),
    paste0(sdtm_dir, "/DM.xpt"),
    paste0(sdtm_dir, "/dm.csv"),
    paste0(sdtm_dir, "/DM.csv"),
    "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt",
    "/bench/data/raw/cdisc-pilot/sdtm/DM.xpt",
    "/bench/data/raw/cdisc-pilot/sdtm/dm.sas7bdat"
  )
  # also discover via list.files
  if (dir.exists(sdtm_dir)) {
    fl <- list.files(sdtm_dir, full.names = TRUE)
    # prefer dm.* case-insensitive
    dmfl <- fl[tolower(basename(fl)) %in% c("dm.sas7bdat","dm.xpt","dm.csv")]
    if (length(dmfl) > 0) cands <- c(dmfl, cands)
  }
  for (f in cands) {
    if (!is.na(f) && file.exists(f)) {
      cat("Reading DM:", f, "\n")
      lf <- tolower(f)
      if (grepl("\\.sas7bdat$", lf)) {
        d <- haven::read_sas(f)
        d <- as.data.frame(d, stringsAsFactors = FALSE)
        return(d)
      } else if (grepl("\\.xpt$", lf)) {
        d <- haven::read_xpt(f)
        d <- as.data.frame(d, stringsAsFactors = FALSE)
        return(d)
      } else if (grepl("\\.csv$", lf)) {
        d <- utils::read.csv(f, stringsAsFactors = FALSE, colClasses = "character", na.strings = NULL)
        return(as.data.frame(d, stringsAsFactors = FALSE))
      }
    }
  }
  cat("WARNING: DM file not found, using empty anchor\n")
  return(NULL)
}
dm_raw <- read_dm()
rf_to_char <- function(x) {
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  if (is.numeric(x) && length(x)>0) {
    # could be SAS date numeric if haven didn't convert; try as Date origin 1960-01-01
    # but RFENDTC is character normally; keep as character to avoid mis-conversion
    # if all NA keep NA
    return(as.character(x))
  }
  return(as.character(x))
}
if (is.null(dm_raw)) {
  dm_anchor <- data.frame(USUBJID = character(0), `_CMENTPT_ANCHOR` = character(0), stringsAsFactors = FALSE, check.names = FALSE)
  names(dm_anchor) <- c("USUBJID","_CMENTPT_ANCHOR")
} else {
  names(dm_raw) <- toupper(names(dm_raw))
  if (!("USUBJID" %in% names(dm_raw))) stop("DM missing USUBJID")
  if (!("RFENDTC" %in% names(dm_raw))) {
    dm_raw$RFENDTC <- ""
  }
  us <- as.character(dm_raw$USUBJID)
  rf <- rf_to_char(dm_raw$RFENDTC)
  rf[is.na(rf)] <- ""
  rf <- trimws(rf)
  # LENGTH $10 truncation like SAS
  rf <- substr(rf, 1, 10)
  # SAS: missing RFENDTC that is datetime with T -> truncated to date part; substr handles
  dm_anchor <- data.frame(USUBJID = ifelse(is.na(us),"",us), `_CMENTPT_ANCHOR` = rf, stringsAsFactors = FALSE, check.names = FALSE)
  names(dm_anchor) <- c("USUBJID","_CMENTPT_ANCHOR")
  # sort by USUBJID C collation
  dm_anchor <- dm_anchor[order(ifelse(is.na(dm_anchor$USUBJID),"",dm_anchor$USUBJID), na.last=FALSE, method="radix"), , drop=FALSE]
  # dedup to first per USUBJID (DM should be unique)
  dm_anchor <- dm_anchor[!duplicated(dm_anchor$USUBJID), , drop=FALSE]
}

# merge work.cm_draft(in=in_cm) work.dm_cm_anchor; by USUBJID; if in_cm;
# left join
cm_merged <- merge(cm_draft, dm_anchor, by = "USUBJID", all.x = TRUE, sort = FALSE)
cm_merged$`_CMENTPT_ANCHOR`[is.na(cm_merged$`_CMENTPT_ANCHOR`)] <- ""
# re-sort to SAS merge order: by USUBJID CMSTDTC CMTRT
sk1 <- ifelse(is.na(cm_merged$USUBJID),"",cm_merged$USUBJID)
sk2 <- ifelse(is.na(cm_merged$CMSTDTC),"",cm_merged$CMSTDTC)
sk3 <- ifelse(is.na(cm_merged$CMTRT),"",cm_merged$CMTRT)
ord2 <- order(sk1, sk2, sk3, na.last = FALSE, method = "radix")
cm_merged <- cm_merged[ord2, , drop = FALSE]

# CMENTPT logic
CMENTPT <- character(nrow(cm_merged))
for (i in seq_len(nrow(cm_merged))) {
  enrt <- ifelse(is.na(cm_merged$CMENRTPT[i]), "", cm_merged$CMENRTPT[i])
  anch <- ifelse(is.na(cm_merged$`_CMENTPT_ANCHOR`[i]), "", cm_merged$`_CMENTPT_ANCHOR`[i])
  if (enrt == "ONGOING") {
    CMENTPT[i] <- anch
    if (is.na(CMENTPT[i]) || trimws(CMENTPT[i]) == "") {
      cat(paste0("ERROR: Ongoing CM record has no DM reference end: USUBJID=", cm_merged$USUBJID[i], "\n"))
      CMENTPT[i] <- ""
    }
  } else {
    CMENTPT[i] <- ""
  }
}
cm_merged$CMENTPT <- substr(CMENTPT, 1, 10)
# FIRST.USUBJID CMSEQ
CMSEQ <- integer(nrow(cm_merged))
cur <- NA_character_
seqn <- 0L
for (i in seq_len(nrow(cm_merged))) {
  u <- cm_merged$USUBJID[i]
  if (is.na(u)) u <- ""
  if (is.na(cur) || u != cur) {
    seqn <- 1L
    cur <- u
  } else {
    seqn <- seqn + 1L
  }
  CMSEQ[i] <- seqn
}
cm_merged$CMSEQ <- CMSEQ
# drop anchor, retain order
cm_final <- cm_merged[, c("STUDYID","DOMAIN","USUBJID","CMSEQ","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT","CMENTPT"), drop = FALSE]
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_coll), silent = TRUE))

cat("Final CM rows:", nrow(cm_final), "\n")

# Write dataset cm.csv
write.csv(cm_final, file.path(out, "cm.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT proc print obs=15
nprint <- min(15, nrow(cm_final))
print_df <- cm_final[seq_len(nprint), , drop = FALSE]
print_df$Obs <- seq_len(nprint)
print_df <- print_df[, c("Obs","STUDYID","DOMAIN","USUBJID","CMSEQ","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT","CMENTPT"), drop = FALSE]

# Build text rendering similar to SAS PROC PRINT
title_line <- "SDTM CM Domain - Concomitant Medications"
# format numeric missing as "." like SAS, char missing as blank
fmt_cell <- function(col, val) {
  if (col %in% c("Obs","CMSEQ","CMDOSE")) {
    if (is.na(val)) return(".")
    # SAS BEST: no trailing decimals
    if (val == floor(val) && abs(val) < 1e12) return(formatC(val, format = "f", digits = 0))
    return(as.character(val))
  } else {
    if (is.na(val)) return("")
    return(as.character(val))
  }
}
header <- c("Obs","STUDYID","DOMAIN","USUBJID","CMSEQ","CMTRT","CMDECOD","CMDOSE","CMDOSU","CMSTDTC","CMENDTC","CMENRTPT","CMENTPT")
# compute widths
widths <- nchar(header)
for (j in seq_along(header)) {
  col <- header[j]
  vals <- vapply(print_df[[col]], function(v) fmt_cell(col, v), character(1))
  widths[j] <- max(widths[j], max(nchar(vals), 0))
}
pad <- function(s, w) sprintf(paste0("%-", w, "s"), s)
lines <- c(title_line, "", paste(sapply(seq_along(header), function(j) pad(header[j], widths[j])), collapse = "  "))
lines <- c(lines, paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "  "))
for (i in seq_len(nrow(print_df))) {
  rowvals <- sapply(seq_along(header), function(j) {
    col <- header[j]
    pad(fmt_cell(col, print_df[i, col]), widths[j])
  })
  lines <- c(lines, paste(rowvals, collapse = "  "))
}
lines <- c(lines, "", paste0("NOTE: Table shows first ", nprint, " of ", nrow(cm_final), " records from SDTM CM domain."))
txt_path <- file.path(out, "P010.txt")
writeLines(lines, txt_path)
# also write expected .lst name copy
writeLines(lines, file.path(out, "P010.lst"))
# CSV of table body (same 15 rows)
write.csv(print_df, file.path(out, "P010.csv"), row.names = FALSE, na = "")
# extra copy with .lst.csv for validators expecting that pattern
write.csv(print_df, file.path(out, "P010.lst.csv"), row.names = FALSE, na = "")
cat("Wrote cm.csv, P010.txt/lst/csv\n")
