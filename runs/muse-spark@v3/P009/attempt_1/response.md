```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10 ^ digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # SAS rounds half away from zero
  rounded <- floor(abs_x + 0.5 + 1e-9) / mult
  rounded * sign_x
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ae.csv"

# Read everything as character to emulate PROC IMPORT scanning (GUESSINGROWS=MAX)
raw0 <- readr::read_csv(raw_path, col_types = readr::cols(.default = readr::col_character()),
                        na = c("", "NA"), trim_ws = FALSE, guess_max = 1000000,
                        show_col_types = FALSE)
# Normalize names to uppercase (OPTIONS VALIDVARNAME=UPCASE, case-insensitive refs)
names(raw0) <- toupper(trimws(names(raw0)))

# Helper to get column case-insensitively (already upper)
need <- c("ID","AE_TERM","SEV","SERIOUS","RELATED","START","END")
missing_need <- setdiff(need, names(raw0))
if (length(missing_need) > 0) stop(paste("missing columns:", paste(missing_need, collapse=",")))
stopifnot(all(c("START","END") %in% names(raw0)))

# Trim helper
trim_chr <- function(x) {
  x <- as.character(x)
  # readr already NA for ""; trim remaining
  idx <- !is.na(x)
  x[idx] <- trimws(x[idx], which = "both")
  # empty after trim -> NA to represent SAS missing character
  x[idx & x == ""] <- NA_character_
  x
}

# Single-format date parser: decide ONE format per column from all values, then parse all with it
parse_single_format_dates <- function(vec_chr) {
  v <- trim_chr(vec_chr)
  n <- length(v)
  res <- rep(as.Date(NA), n)
  nonmiss <- v[!is.na(v)]
  if (length(nonmiss) == 0) return(res)
  # decide format once
  fmt <- NA_character_
  is_slash_n_n_yyyy <- all(grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$", nonmiss))
  is_yyyy_mm_dd_dash <- all(grepl("^[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}$", nonmiss))
  is_yyyy_mm_dd_slash <- all(grepl("^[0-9]{4}/[0-9]{1,2}/[0-9]{1,2}$", nonmiss))
  is_ddmonyyyy <- all(grepl("^[0-9]{1,2}-[A-Za-z]{3}-[0-9]{2,4}$", nonmiss))
  is_mon_dd_yyyy <- all(grepl("^[A-Za-z]{3} +[0-9]{1,2}, *[0-9]{4}$", nonmiss))
  is_yyyymmdd <- all(grepl("^[0-9]{8}$", nonmiss))
  if (is_slash_n_n_yyyy) {
    first_fields <- suppressWarnings(as.integer(sub("/.*$", "", nonmiss)))
    # PROC IMPORT: any first field >12 => DDMMYY10. else MMDDYY10.
    if (any(!is.na(first_fields) & first_fields > 12)) fmt <- "%d/%m/%Y" else fmt <- "%m/%d/%Y"
  } else if (is_yyyy_mm_dd_dash) {
    fmt <- "%Y-%m-%d"
  } else if (is_yyyy_mm_dd_slash) {
    fmt <- "%Y/%m/%d"
  } else if (is_ddmonyyyy) {
    fmt <- "%d-%b-%Y"
  } else if (is_mon_dd_yyyy) {
    fmt <- "%b %d, %Y"
  } else if (is_yyyymmdd) {
    fmt <- "%Y%m%d"
  } else {
    # Fallback single ANYDTDTE MDY assumption: choose by dominant pattern, one format only
    # Prefer ISO if contains "-", else MDY slash
    if (all(grepl("-", nonmiss, fixed = TRUE))) fmt <- "%Y-%m-%d"
    else if (all(grepl("/", nonmiss, fixed = TRUE))) fmt <- "%m/%d/%Y"
    else fmt <- "%Y-%m-%d"
  }
  # Normalize month-abbrev case for %b (e.g., 15-JAN-2023 -> 15-Jan-2023)
  v_norm <- v
  if (fmt == "%d-%b-%Y") {
    # convert MON part to Title case
    v_norm[!is.na(v_norm)] <- gsub("([0-9]{1,2}-)([A-Za-z]{3})(-[0-9]{2,4})",
      function(m) {
        # vectorized via regmatches? do per element below
        m
      }, v_norm[!is.na(v_norm)])
    # per-element title-case month
    for (i in which(!is.na(v_norm))) {
      parts <- strsplit(v_norm[i], "-", fixed = TRUE)[[1]]
      if (length(parts) == 3 && nchar(parts[2]) == 3) {
        parts[2] <- paste0(toupper(substr(parts[2],1,1)), tolower(substr(parts[2],2,3)))
        v_norm[i] <- paste(parts, collapse = "-")
      }
    }
  }
  if (fmt == "%b %d, %Y") {
    for (i in which(!is.na(v_norm))) {
      # normalize first 3 letters to Title
      s <- trimws(v_norm[i])
      if (nchar(s) >= 3) {
        mon <- substr(s,1,3)
        mon <- paste0(toupper(substr(mon,1,1)), tolower(substr(mon,2,3)))
        v_norm[i] <- paste0(mon, substr(s,4,nchar(s)))
      }
    }
  }
  # Strict single-format parse for every value
  res <- suppressWarnings(as.Date(v_norm, format = fmt))
  attr(res, "sas_fmt") <- fmt
  res
}

# Emulate VTYPE + INPUT(anydtdte.) vs numeric branch, but both use single-format decision above
start_dates <- parse_single_format_dates(raw0$START)
end_dates <- parse_single_format_dates(raw0$END)
start_fmt <- attr(start_dates, "sas_fmt")
end_fmt <- attr(end_dates, "sas_fmt")
cat("START fmt:", start_fmt, " END fmt:", end_fmt, "\n")

# Core transformation
ID_chr <- trim_chr(raw0$ID)
AE_TERM_chr <- trim_chr(raw0$AE_TERM)
SEV_chr <- trim_chr(raw0$SEV)
SERIOUS_chr <- trim_chr(raw0$SERIOUS)
RELATED_chr <- trim_chr(raw0$RELATED)

n <- nrow(raw0)
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("AE", n)
# CATX("-", STUDYID, ID): strip ID, ignore missing
USUBJID <- ifelse(is.na(ID_chr), "CDISC-01", paste0("CDISC-01-", ID_chr))
USUBJID <- substr(USUBJID, 1, 40)

AETERM <- AE_TERM_chr
# truncate to $60 (LENGTH)
AETERM[!is.na(AETERM)] <- substr(AETERM[!is.na(AETERM)], 1, 60)
AEDECOD <- toupper(AETERM)
AEDECOD[!is.na(AEDECOD)] <- substr(AEDECOD[!is.na(AEDECOD)], 1, 60)

AESEV <- toupper(SEV_chr)
AESEV[!is.na(AESEV)] <- substr(AESEV[!is.na(AESEV)], 1, 10)

SER_up <- toupper(ifelse(is.na(SERIOUS_chr), "", SERIOUS_chr))
AESER <- ifelse(SER_up == "Y", "Y", "N")
AESER <- substr(AESER, 1, 1)

REL_up <- toupper(ifelse(is.na(RELATED_chr), "", RELATED_chr))
AEREL <- ifelse(REL_up == "Y", "RELATED",
         ifelse(REL_up == "N", "NOT RELATED", "UNKNOWN"))
AEREL <- substr(AEREL, 1, 15)

# ISO 8601 DA from SAS date numbers: AESTDTC = put(_start_num, is8601da.)
AESTDTC <- ifelse(is.na(start_dates), NA_character_, format(start_dates, "%Y-%m-%d"))
AEENDTC <- ifelse(is.na(end_dates), NA_character_, format(end_dates, "%Y-%m-%d"))
AEOUT <- ifelse(!is.na(end_dates), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")
AEOUT <- substr(AEOUT, 1, 30)

ae_mapped <- data.frame(STUDYID=STUDYID, DOMAIN=DOMAIN, USUBJID=USUBJID,
  AETERM=AETERM, AEDECOD=AEDECOD, AESEV=AESEV, AESER=AESER, AEREL=AEREL,
  AESTDTC=AESTDTC, AEENDTC=AEENDTC, AEOUT=AEOUT, stringsAsFactors=FALSE)

# Sort by USUBJID AESTDTC (missing first, SAS binary uppercase-before-lowercase)
# Force C collation for SAS character ordering
old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE","C"), silent=TRUE))
aest_sort <- ifelse(is.na(ae_mapped$AESTDTC), "", ae_mapped$AESTDTC)
ord <- order(ae_mapped$USUBJID, aest_sort, na.last = FALSE, method = "radix")
# radix with C locale approximates SAS; fallback to default order if needed
ae_sorted <- ae_mapped[ord, , drop=FALSE]
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent=TRUE))

# BY-group AESEQ
ae_sorted$AESEQ <- ave(seq_len(nrow(ae_sorted)), ae_sorted$USUBJID, FUN=function(x) seq_along(x))
ae_sorted$AESEQ <- as.integer(ae_sorted$AESEQ)

# RETAIN order: STUDYID DOMAIN USUBJID AESEQ ...
ae <- ae_sorted[, c("STUDYID","DOMAIN","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")]
rownames(ae) <- NULL

# Write dataset
write.csv(ae, file.path(out, "ae.csv"), row.names=FALSE, na="")

# Visual audit: PROC PRINT obs=12 vars USUBJID AESEQ AETERM AESEV AESER AEREL AESTDTC AEENDTC AEOUT
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
stopifnot(all(audit_vars %in% names(ae)))
audit <- head(ae[, audit_vars, drop=FALSE], 12)
audit$Obs <- seq_len(nrow(audit))
audit <- audit[, c("Obs", audit_vars), drop=FALSE]

# CSV of table body
write.csv(audit, file.path(out, "P009.csv"), row.names=FALSE, na="")

# Text rendering mimicking PROC PRINT
title_line <- "AE Domain Audit (Adverse Events with Ongoing Logic)"
# Build fixed-width text
con_txt <- file.path(out, "P009.txt")
sink(con_txt)
cat(title_line, "\n\n", sep="")
# Header
hdr <- sprintf("%-4s %-20s %-5s %-30s %-10s %-5s %-15s %-10s %-10s %-25s",
  "Obs","USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
cat(hdr, "\n", sep="")
cat(paste(rep("-", nchar(hdr)), collapse=""), "\n", sep="")
for (i in seq_len(nrow(audit))) {
  r <- audit[i,]
  line <- sprintf("%-4d %-20s %-5d %-30s %-10s %-5s %-15s %-10s %-10s %-25s",
    ifelse(is.na(r$Obs),"",r$Obs),
    ifelse(is.na(r$USUBJID),"",r$USUBJID),
    ifelse(is.na(r$AESEQ),0,r$AESEQ),
    ifelse(is.na(r$AETERM),"",substr(r$AETERM,1,30)),
    ifelse(is.na(r$AESEV),"",r$AESEV),
    ifelse(is.na(r$AESER),"",r$AESER),
    ifelse(is.na(r$AEREL),"",r$AEREL),
    ifelse(is.na(r$AESTDTC),"",r$AESTDTC),
    ifelse(is.na(r$AEENDTC),"",r$AEENDTC),
    ifelse(is.na(r$AEOUT),"",r$AEOUT))
  cat(line, "\n", sep="")
}
sink()
# Also write .lst copy for SAS name parity
file.copy(file.path(out, "P009.txt"), file.path(out, "P009.lst"), overwrite=TRUE)
cat("Done: ae rows", nrow(ae), " audit rows", nrow(audit), "\n")
```