out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_lab.csv"

cat("Reading raw lab:", raw_path, "\n")
raw_lb0 <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
names(raw_lb0) <- toupper(trimws(names(raw_lb0)))
cat("Columns:", paste(names(raw_lb0), collapse=","), " n=", nrow(raw_lb0), "\n")

# helper: SAS STRIP (trim, keep NA as NA)
sas_strip_vec <- function(x) {
  x <- as.character(x)
  isna <- is.na(x)
  x[!isna] <- trimws(x[!isna], which = "both")
  x
}
# helper: SAS BEST12. via formatC g digits 12; missing -> "."
sas_best_put <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (!is.finite(v)) return(as.character(v))
    formatC(v, format = "g", digits = 12, flag = "")
  }, USE.NAMES = FALSE)
}
sas_trunc <- function(x, n) {
  x <- as.character(x)
  idx <- !is.na(x) & nchar(x, type="chars") > n
  x[idx] <- substr(x[idx], 1, n)
  x
}

# locate required columns (exact uppercase expected)
need <- c("SUBJ","VISIT_NAM","VISIT_NUM","TEST_NAME","RESULT","UNIT","LAB_DAT")
miss <- setdiff(need, names(raw_lb0))
if (length(miss) > 0) {
  stop(paste0("Missing columns in raw_lab.csv: ", paste(miss, collapse=","), " | have: ", paste(names(raw_lb0), collapse=",")))
}

n <- nrow(raw_lb0)
SUBJ_c <- as.character(raw_lb0[["SUBJ"]])
VISIT_NAM_c <- as.character(raw_lb0[["VISIT_NAM"]])
VISIT_NUM_o <- raw_lb0[["VISIT_NUM"]]
TEST_NAME_c <- as.character(raw_lb0[["TEST_NAME"]])
UNIT_c <- as.character(raw_lb0[["UNIT"]])
LAB_col <- raw_lb0[["LAB_DAT"]]
RESULT_o <- raw_lb0[["RESULT"]]

# RESULT numeric (PROC IMPORT would make numeric)
RESULT_num <- suppressWarnings(as.numeric(as.character(RESULT_o)))
# if RESULT_o already Date? not expected; keep NA then
# LBORRES via PUT BEST.
LBORRES <- sas_best_put(RESULT_num)
LBSTRESC <- LBORRES
LBSTRESN <- RESULT_num

# Core identifiers
STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("LB", n)
# CATX("-", STUDYID, SUBJ): SUBJ numeric -> BEST stripped; char -> stripped; missing skipped
subj_part <- sapply(seq_len(n), function(i) {
  sv <- SUBJ_c[i]
  if (is.na(sv)) return(NA_character_)
  svt <- trimws(sv)
  if (svt == "" || toupper(svt) == "NA") return(NA_character_)
  # if SUBJ looks numeric, normalize via BEST (remove leading zeros? SAS CATX with numeric would not keep leading zeros)
  # Check original SUBJ column type: if numeric, format via BEST
  # Here SUBJ_c came from character conversion; try to detect if original was numeric
  # Use BEST formatting if string is numeric representation
  num <- suppressWarnings(as.numeric(svt))
  if (!is.na(num) && grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", svt)) {
    # mimic numeric CATX: BEST12. stripped
    if (is.na(num)) return(svt)
    return(formatC(num, format="g", digits=12, flag=""))
  } else {
    return(svt)
  }
}, USE.NAMES=FALSE)
USUBJID <- ifelse(is.na(subj_part), "CDISC-01", paste0("CDISC-01-", subj_part))

VISIT <- sas_strip_vec(VISIT_NAM_c)
# SAS: missing char -> blank ""; convert NA from strip to ""? keep NA for CSV (writes as "")
# keep as NA (will write as "")
VISITNUM <- VISIT_NUM_o

# Test mapping: SELECT UPCASE(STRIP(TEST_NAME))
test_up <- toupper(trimws(as.character(TEST_NAME_c)))
# NA -> NA
LBTESTCD <- character(n)
LBTEST <- character(n)
LBCAT <- character(n)
for (i in seq_len(n)) {
  tu <- test_up[i]
  if (!is.na(tu) && tu == "GLUCOSE") {
    LBTESTCD[i] <- "GLUC"; LBTEST[i] <- "Glucose"; LBCAT[i] <- "CHEMISTRY"
  } else if (!is.na(tu) && tu == "HEMOGLOBIN") {
    LBTESTCD[i] <- "HGB"; LBTEST[i] <- "Hemoglobin"; LBCAT[i] <- "HEMATOLOGY"
  } else if (!is.na(tu) && tu == "ALT") {
    LBTESTCD[i] <- "ALT"; LBTEST[i] <- "Alanine Aminotransferase"; LBCAT[i] <- "CHEMISTRY"
  } else if (!is.na(tu) && tu == "AST") {
    LBTESTCD[i] <- "AST"; LBTEST[i] <- "Aspartate Aminotransferase"; LBCAT[i] <- "CHEMISTRY"
  } else {
    LBTESTCD[i] <- "UNKNOWN"
    # SAS: LBTEST = TEST_NAME (original, not stripped)
    orig <- TEST_NAME_c[i]
    if (is.na(orig)) LBTEST[i] <- NA_character_ else LBTEST[i] <- orig
    LBCAT[i] <- "UNKNOWN"
  }
}

# Units
LBORRESU <- sas_strip_vec(UNIT_c)
LBSTRESU <- sapply(seq_len(n), function(i) {
  u <- LBORRESU[i]
  if (is.na(u)) return(NA_character_)
  uu <- toupper(u)
  if (uu == "MG/DL") return("mg/dL")
  else if (uu == "G/DL") return("g/dL")
  else if (uu == "U/L") return("U/L")
  else return(u)
}, USE.NAMES=FALSE)

# ISO 8601 date conversion
# _lab_num is SAS date (days since 1960-01-01); LBDTC = PUT(_lab_num, IS8601DA.)
sas_origin <- as.Date("1960-01-01")
parse_one_char_date <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  t <- trimws(as.character(s))
  if (t == "" || toupper(t) == "NA" || t == ".") return(as.Date(NA))
  # try lubridate parse_date_time
  dt <- suppressWarnings(lubridate::parse_date_time(t,
    orders = c("Ymd HMS","Ymd HM","Ymd","ymd HMS","ymd HM","ymd",
               "mdy HMS","mdy HM","mdy","dmy HMS","dmy HM","dmy",
               "dby HMS","dby HM","dby","YmdT HMS","ymdT HMS",
               "YmdTHMS","ymdTHMS","Ymd HM","ymd HM"),
    quiet = TRUE, tz = "UTC", truncated = 0))
  if (length(dt) == 1 && !is.na(dt)) return(as.Date(dt))
  # try as.Date with common formats
  d2 <- suppressWarnings(as.Date(t, tryFormats = c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%m-%d-%Y",
    "%d/%m/%Y","%d-%m-%Y","%d-%b-%Y","%d-%B-%Y","%d%b%Y","%d %b %Y","%Y%m%d","%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d %H:%M:%S","%m/%d/%Y %H:%M","%Y/%m/%d %H:%M")))
  if (!is.na(d2)) return(d2)
  # try numeric SAS date number stored as character
  if (grepl("^\\s*-?\\d+(\\.\\d+)?\\s*$", t)) {
    num <- suppressWarnings(as.numeric(t))
    if (!is.na(num)) {
      # SAS date: integer days; use floor for decimals to mimic date part
      return(sas_origin + floor(num))
    }
  }
  return(as.Date(NA))
}

lab_dates <- rep(as.Date(NA), n)
if (inherits(LAB_col, "Date")) {
  lab_dates <- LAB_col
} else if (inherits(LAB_col, "POSIXct") || inherits(LAB_col, "POSIXlt")) {
  lab_dates <- as.Date(LAB_col)
} else if (is.numeric(LAB_col)) {
  lab_dates <- sas_origin + floor(LAB_col)
  lab_dates[is.na(LAB_col)] <- as.Date(NA)
} else {
  # character / factor / logical etc.
  lab_char <- as.character(LAB_col)
  lab_dates <- sapply(lab_char, parse_one_char_date, USE.NAMES=FALSE)
  # sapply may return numeric (days since 1970); convert
  if (is.numeric(lab_dates)) lab_dates <- as.Date(lab_dates, origin="1970-01-01")
  else lab_dates <- as.Date(lab_dates, origin="1970-01-01")
}
LBDTC <- format(lab_dates, "%Y-%m-%d")
LBDTC[is.na(lab_dates)] <- NA_character_

# Apply SAS LENGTH truncation
STUDYID <- sas_trunc(STUDYID, 8)
DOMAIN <- sas_trunc(DOMAIN, 2)
USUBJID <- sas_trunc(USUBJID, 40)
LBTESTCD <- sas_trunc(LBTESTCD, 8)
LBTEST <- sas_trunc(LBTEST, 40)
LBCAT <- sas_trunc(LBCAT, 20)
LBORRES <- sas_trunc(LBORRES, 20)
LBORRESU <- sas_trunc(LBORRESU, 20)
LBSTRESC <- sas_trunc(LBSTRESC, 20)
LBSTRESU <- sas_trunc(LBSTRESU, 20)

lb_mapped <- data.frame(
  STUDYID=STUDYID, DOMAIN=DOMAIN, USUBJID=USUBJID,
  VISITNUM=VISITNUM, VISIT=VISIT,
  LBTESTCD=LBTESTCD, LBTEST=LBTEST, LBCAT=LBCAT,
  LBORRES=LBORRES, LBORRESU=LBORRESU,
  LBSTRESC=LBSTRESC, LBSTRESN=LBSTRESN, LBSTRESU=LBSTRESU,
  LBDTC=LBDTC, stringsAsFactors=FALSE, check.names=FALSE
)

# 3. SORT by USUBJID LBDTC LBTESTCD (SAS: missing first, uppercase before lowercase)
ord <- order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD, method="radix", na.last=FALSE)
lb_sorted <- lb_mapped[ord, , drop=FALSE]

# LBSEQ by USUBJID (FIRST.USUBJID)
usub_for_rle <- ifelse(is.na(lb_sorted$USUBJID), "<NA_MISSING>", lb_sorted$USUBJID)
r <- rle(usub_for_rle)
lb_sorted$LBSEQ <- sequence(r$lengths)

# RETAIN order: STUDYID DOMAIN USUBJID LBSEQ then rest
lb_final <- lb_sorted[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]

cat("Final LB n=", nrow(lb_final), "\n")

# Write dataset
write.csv(lb_final, file.path(out, "lb.csv"), row.names=FALSE, na="")

# 4. VISUAL AUDIT: PROC PRINT obs=12 vars USUBJID VISITNUM VISIT LBTESTCD LBCAT LBSTRESN LBSTRESU LBDTC
listing_vars <- c("USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
listing_df <- head(lb_final[, listing_vars, drop=FALSE], 12)

# CSV of table body
write.csv(listing_df, file.path(out, "P015.csv"), row.names=FALSE, na="")

# Text rendering
fmt_num_disp <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    formatC(v, format="g", digits=12, flag="")
  }, USE.NAMES=FALSE)
}
fmt_char_disp <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}
disp_USUBJID <- fmt_char_disp(listing_df$USUBJID)
disp_VISITNUM <- if (is.numeric(listing_df$VISITNUM)) fmt_num_disp(listing_df$VISITNUM) else fmt_char_disp(listing_df$VISITNUM)
disp_VISIT <- fmt_char_disp(listing_df$VISIT)
disp_LBTESTCD <- fmt_char_disp(listing_df$LBTESTCD)
disp_LBCAT <- fmt_char_disp(listing_df$LBCAT)
disp_LBSTRESN <- fmt_num_disp(listing_df$LBSTRESN)
disp_LBSTRESU <- fmt_char_disp(listing_df$LBSTRESU)
disp_LBDTC <- fmt_char_disp(listing_df$LBDTC)

# column widths (at least header length)
hds <- c("Obs","USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
cols <- list(sprintf("%d", seq_len(nrow(listing_df))), disp_USUBJID, disp_VISITNUM, disp_VISIT, disp_LBTESTCD, disp_LBCAT, disp_LBSTRESN, disp_LBSTRESU, disp_LBDTC)
widths <- sapply(seq_along(hds), function(j) max(nchar(hds[j]), if (length(cols[[j]])>0) max(nchar(cols[[j]])) else 0))
# cap widths reasonable
widths <- pmin(widths, 40)
# build lines
padR <- function(s, w) sprintf(paste0("%-", w, "s"), s)
padL <- function(s, w) sprintf(paste0("%", w, "s"), s)
# Obs, VISITNUM, LBSTRESN right-aligned; rest left
align_right <- c(TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
header_line <- paste(mapply(function(h,w,ar) if (ar) padL(h,w) else padR(h,w), hds, widths, align_right), collapse="  ")
sep_line <- paste(sapply(widths, function(w) paste(rep("-", w), collapse="")), collapse="  ")
lines <- c("LB Domain Audit (Laboratory Test Results)", "", header_line, sep_line)
for (i in seq_len(nrow(listing_df))) {
  vals <- c(cols[[1]][i], cols[[2]][i], cols[[3]][i], cols[[4]][i], cols[[5]][i], cols[[6]][i], cols[[7]][i], cols[[8]][i], cols[[9]][i])
  line <- paste(mapply(function(v,w,ar) if (ar) padL(v,w) else padR(v,w), vals, widths, align_right), collapse="  ")
  lines <- c(lines, line)
}
lines <- c(lines, "", paste0("N = ", nrow(listing_df), " (first 12 observations of LB, total ", nrow(lb_final), ")"))
txt_path <- file.path(out, "P015.txt")
writeLines(lines, txt_path)
# duplicate for discoverability
writeLines(lines, file.path(out, "lb.txt"))
cat("Wrote", file.path(out, "lb.csv"), "and", txt_path, "\n")
