out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

raw_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_lab.csv"
# fallback for linux mount used in sandbox
if (!file.exists(raw_path)) {
  alt <- "/bench/data/derived/c2c/data/raw/raw_lab.csv"
  if (file.exists(alt)) raw_path <- alt
}
cat("Reading raw lab:", raw_path, "\n")
raw_lb0 <- read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
names(raw_lb0) <- toupper(trimws(names(raw_lb0)))
cat("Columns:", paste(names(raw_lb0), collapse=","), " n=", nrow(raw_lb0), "\n")

sas_strip_vec <- function(x) {
  x <- as.character(x)
  isna <- is.na(x)
  x[!isna] <- trimws(x[!isna], which = "both")
  x
}
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

need <- c("SUBJ","VISIT_NAM","VISIT_NUM","TEST_NAME","RESULT","UNIT","LAB_DAT")
miss <- setdiff(need, names(raw_lb0))
if (length(miss) > 0) {
  stop(paste0("Missing columns in raw_lab.csv: ", paste(miss, collapse=","), " | have: ", paste(names(raw_lb0), collapse=",")))
}

n <- nrow(raw_lb0)
SUBJ_col <- raw_lb0[["SUBJ"]]
VISIT_NAM_c <- as.character(raw_lb0[["VISIT_NAM"]])
VISIT_NUM_o <- raw_lb0[["VISIT_NUM"]]
TEST_NAME_c <- as.character(raw_lb0[["TEST_NAME"]])
UNIT_c <- as.character(raw_lb0[["UNIT"]])
LAB_col <- raw_lb0[["LAB_DAT"]]
RESULT_o <- raw_lb0[["RESULT"]]

RESULT_num <- suppressWarnings(as.numeric(as.character(RESULT_o)))
LBORRES <- sas_best_put(RESULT_num)
LBSTRESC <- LBORRES
LBSTRESN <- RESULT_num

STUDYID <- rep("CDISC-01", n)
DOMAIN <- rep("LB", n)

# CATX("-", STUDYID, SUBJ): numeric via BEST12 stripped; char stripped; missing skipped
subj_part <- sapply(seq_len(n), function(i) {
  v <- SUBJ_col[i]
  if (is.na(v)) return(NA_character_)
  if (is.numeric(v)) {
    return(formatC(v, format="g", digits=12, flag=""))
  } else {
    sv <- trimws(as.character(v))
    if (sv == "" || toupper(sv) == "NA") return(NA_character_)
    num <- suppressWarnings(as.numeric(sv))
    if (!is.na(num) && grepl("^\\s*-?[0-9]+(\\.[0-9]+)?\\s*$", sv)) {
      return(formatC(num, format="g", digits=12, flag=""))
    } else {
      return(sv)
    }
  }
}, USE.NAMES=FALSE)
USUBJID <- ifelse(is.na(subj_part), "CDISC-01", paste0("CDISC-01-", subj_part))

VISIT <- sas_strip_vec(VISIT_NAM_c)
VISITNUM <- VISIT_NUM_o

test_up <- toupper(trimws(as.character(TEST_NAME_c)))
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
    orig <- TEST_NAME_c[i]
    if (is.na(orig)) LBTEST[i] <- NA_character_ else LBTEST[i] <- orig
    LBCAT[i] <- "UNKNOWN"
  }
}

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

# ---- ISO 8601 date conversion without lubridate ----
sas_origin <- as.Date("1960-01-01")
month_abbr <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
month_full <- c("January","February","March","April","May","June","July","August","September","October","November","December")

normalize_month_case <- function(s) {
  # title-case month names/abbrs case-insensitively
  for (j in seq_along(month_abbr)) {
    s <- gsub(paste0("(?i)", month_full[j]), month_full[j], s, perl=TRUE)
  }
  for (j in seq_along(month_abbr)) {
    # replace standalone abbr (3 letters) case-insensitively; avoid touching full names already fixed
    # do after full names; full names start with abbr, so protect by temporary placeholder
    s <- gsub(paste0("(?i)\\b", month_abbr[j], "\\b"), month_abbr[j], s, perl=TRUE)
  }
  s
}

parse_one_char_date <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  t <- trimws(as.character(s))
  if (t == "" || t == "." || toupper(t) == "NA") return(as.Date(NA))
  # pure numeric string: decide YYYYMMDD vs SAS date number
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", t)) {
    # 8-digit YYYYMMDD starting 18-21
    if (grepl("^(18|19|20|21)[0-9]{6}$", t)) {
      d <- suppressWarnings(as.Date(t, format="%Y%m%d"))
      if (!is.na(d)) return(d)
    }
    num <- suppressWarnings(as.numeric(t))
    if (!is.na(num)) {
      return(sas_origin + floor(num))
    } else {
      return(as.Date(NA))
    }
  }
  # strip trailing time if present but keep for parsing attempts
  t2 <- normalize_month_case(t)
  # remove comma for some formats? keep, try both
  fmts <- c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
            "%m/%d/%Y", "%m-%d-%Y", "%m/%d/%y", "%m-%d-%y",
            "%d/%m/%Y", "%d-%m-%Y",
            "%d-%b-%Y", "%d-%B-%Y", "%d %b %Y", "%d %B %Y",
            "%d%b%Y", "%d%b%y", "%d-%b-%y",
            "%b %d %Y", "%B %d %Y", "%b %d, %Y", "%B %d, %Y",
            "%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S",
            "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
            "%Y-%m-%dT%H:%M:%S", "%Y-%m-%dT%H:%M",
            "%d-%b-%Y %H:%M:%S", "%d %b %Y %H:%M:%S")
  for (f in fmts) {
    d <- suppressWarnings(as.Date(t2, format=f))
    if (!is.na(d)) return(d)
  }
  # try ISO datetime via POSIX then Date
  for (f in c("%Y-%m-%dT%H:%M:%S", "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M")) {
    dt <- suppressWarnings(as.POSIXct(t2, format=f, tz="UTC"))
    if (!is.na(dt)) return(as.Date(dt, tz="UTC"))
  }
  # last resort: as.Date with tryFormats
  d <- suppressWarnings(as.Date(t2, tryFormats=c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d-%b-%Y","%d %b %Y","%Y%m%d")))
  if (!is.na(d)) return(d)
  return(as.Date(NA))
}

lab_dates <- as.Date(rep(NA, n))
if (inherits(LAB_col, "Date")) {
  lab_dates <- LAB_col
} else if (inherits(LAB_col, "POSIXct") || inherits(LAB_col, "POSIXlt")) {
  lab_dates <- as.Date(LAB_col)
} else if (is.numeric(LAB_col)) {
  lab_dates <- sas_origin + floor(LAB_col)
  lab_dates[is.na(LAB_col)] <- as.Date(NA)
} else {
  lab_char <- as.character(LAB_col)
  for (i in seq_len(n)) {
    lab_dates[i] <- parse_one_char_date(lab_char[i])
  }
}
LBDTC <- format(lab_dates, "%Y-%m-%d")
LBDTC[is.na(lab_dates)] <- NA_character_

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

ord <- order(lb_mapped$USUBJID, lb_mapped$LBDTC, lb_mapped$LBTESTCD, method="radix", na.last=FALSE)
lb_sorted <- lb_mapped[ord, , drop=FALSE]

usub_for_rle <- ifelse(is.na(lb_sorted$USUBJID), "<NA_MISSING>", lb_sorted$USUBJID)
r <- rle(usub_for_rle)
lb_sorted$LBSEQ <- sequence(r$lengths)

lb_final <- lb_sorted[, c("STUDYID","DOMAIN","USUBJID","LBSEQ","VISITNUM","VISIT","LBTESTCD","LBTEST","LBCAT","LBORRES","LBORRESU","LBSTRESC","LBSTRESN","LBSTRESU","LBDTC")]

cat("Final LB n=", nrow(lb_final), "\n")

write.csv(lb_final, file.path(out, "lb.csv"), row.names=FALSE, na="")

listing_vars <- c("USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
listing_df <- head(lb_final[, listing_vars, drop=FALSE], 12)

write.csv(listing_df, file.path(out, "P015.csv"), row.names=FALSE, na="")

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

hds <- c("Obs","USUBJID","VISITNUM","VISIT","LBTESTCD","LBCAT","LBSTRESN","LBSTRESU","LBDTC")
cols <- list(sprintf("%d", seq_len(nrow(listing_df))), disp_USUBJID, disp_VISITNUM, disp_VISIT, disp_LBTESTCD, disp_LBCAT, disp_LBSTRESN, disp_LBSTRESU, disp_LBDTC)
widths <- sapply(seq_along(hds), function(j) max(nchar(hds[j]), if (length(cols[[j]])>0) max(nchar(cols[[j]])) else 0))
widths <- pmin(widths, 40)
padR <- function(s, w) sprintf(paste0("%-", w, "s"), s)
padL <- function(s, w) sprintf(paste0("%", w, "s"), s)
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
writeLines(lines, file.path(out, "lb.txt"))
cat("Wrote", file.path(out, "lb.csv"), "and", txt_path, "\n")
