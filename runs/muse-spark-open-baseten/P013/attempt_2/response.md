```r
# P013_sdtm_eg.sas -> R translation
# Produces dataset eg.csv in OUT_DIR

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_strip <- function(x) {
  ifelse(is.na(x), "", trimws(as.character(x), which = "both"))
}

sas_catx <- function(delim, ...) {
  args <- list(...)
  stripped <- lapply(args, function(a) {
    if (is.null(a)) return(NA_character_)
    s <- sas_strip(a)
    s[s == ""] <- NA_character_
    s
  })
  n <- max(sapply(stripped, length))
  stripped <- lapply(stripped, function(v) rep_len(v, n))
  res <- character(n)
  for (i in seq_len(n)) {
    vals <- sapply(stripped, function(v) v[i])
    vals <- vals[!is.na(vals)]
    res[i] <- paste(vals, collapse = delim)
  }
  res
}

sas_best32_clean <- function(x) {
  v <- ifelse(is.na(x), ".",
              trimws(format(x, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0), which = "both"))
  need_sci <- !is.na(x) & (abs(x) >= 1e12 | (abs(x) < 1e-4 & abs(x) != 0))
  v[need_sci] <- trimws(formatC(x[need_sci], format = "g", digits = 12), which = "both")
  v <- sub("\\.$", "", v)
  v
}

parse_anydtdte <- function(x) {
  res <- rep(as.Date(NA), length(x))
  xc <- trimws(as.character(x))
  xc[xc == "" | is.na(xc)] <- NA_character_
  for (i in seq_along(xc)) {
    if (is.na(xc[i])) next
    d <- suppressWarnings(as.Date(xc[i], format = "%Y-%m-%d"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%Y/%m/%d"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%m/%d/%Y"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%d%b%Y"))
    if (!is.na(d)) { res[i] <- d; next }
    d <- suppressWarnings(as.Date(xc[i], format = "%d-%b-%Y"))
    if (!is.na(d)) { res[i] <- d; next }
    d2 <- suppressWarnings(lubridate::parse_date_time(xc[i],
              orders = c("ymd","mdy","dmy","Ymd","ymd HMS","mdy HMS","dmy HMS",
                         "Y-m-d","m/d/Y","d-b-Y","Y/m/d"), exact = FALSE, quiet = TRUE))
    if (!is.na(d2)) { res[i] <- as.Date(d2); next }
    num <- suppressWarnings(as.numeric(xc[i]))
    if (!is.na(num) && num > 1000 && num < 30000) {
      res[i] <- as.Date(num, origin = "1960-01-01")
      next
    }
  }
  res
}

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_ecg.csv",
  Sys.getenv("project_path"),
  Sys.getenv("PROJECT_PATH"),
  file.path(Sys.getenv("root"), "data/derived/c2c/data/raw/raw_ecg.csv"),
  file.path(Sys.getenv("root"), "data/raw/raw_ecg.csv"),
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_ecg.csv",
  file.path(getwd(), "raw_ecg.csv"),
  "/tmp/raw_ecg.csv"
)
candidates <- unique(candidates)
candidates <- candidates[candidates != "" & !is.na(candidates)]

raw_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { raw_path <- p; break }
  if (file.exists(normalizePath(p, mustWork = FALSE))) {
    raw_path <- normalizePath(p, mustWork = FALSE)
    if (file.exists(raw_path)) break
  }
}
if (is.null(raw_path)) {
  search_roots <- c(getwd(), tempdir(), Sys.getenv("HOME"), "/tmp", ".", "/bench")
  for (r in search_roots) {
    if (!dir.exists(r)) next
    found <- list.files(r, pattern = "^raw_ecg\\.csv$", recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) { raw_path <- found[1]; break }
  }
}
if (is.null(raw_path)) {
  stop("raw_ecg.csv not found. Searched: ", paste(candidates, collapse = "; "))
}
cat("Reading raw_ecg from:", raw_path, "\n")

raw_eg <- tryCatch({
  df <- readr::read_csv(raw_path, show_col_types = FALSE, guess_max = 10000)
  as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) {
  read.csv(raw_path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA", "."))
})

names(raw_eg) <- toupper(names(raw_eg))

get_col <- function(df, name) {
  if (name %in% names(df)) return(df[[name]])
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx) > 0) return(df[[idx[1]]])
  return(rep(NA, nrow(df)))
}

SUBJECT   <- get_col(raw_eg, "SUBJECT")
VISIT_NAM <- get_col(raw_eg, "VISIT_NAM")
VISIT_NUM <- get_col(raw_eg, "VISIT_NUM")
TEST_NAME <- get_col(raw_eg, "TEST_NAME")
RESULT    <- get_col(raw_eg, "RESULT")
UNIT      <- get_col(raw_eg, "UNIT")
DATE      <- get_col(raw_eg, "DATE")

RESULT_num <- suppressWarnings(as.numeric(RESULT))
VISIT_NUM_num <- suppressWarnings(as.numeric(VISIT_NUM))

n <- nrow(raw_eg)
sdtm_eg <- data.frame(
  STUDYID = rep("CDISC-01", n),
  DOMAIN  = rep("EG", n),
  stringsAsFactors = FALSE
)

sdtm_eg$USUBJID <- sas_catx("-", sdtm_eg$STUDYID, SUBJECT)
sdtm_eg$VISIT <- sas_strip(VISIT_NAM)
sdtm_eg$VISITNUM <- VISIT_NUM_num
sdtm_eg$EGTEST <- sas_strip(TEST_NAME)

sdtm_eg$EGTESTCD <- ifelse(sdtm_eg$EGTEST == "Heart Rate", "HR",
                    ifelse(sdtm_eg$EGTEST == "QT Duration", "QT",
                    ifelse(sdtm_eg$EGTEST == "RR Duration", "RR", "UNKNOWN")))

sdtm_eg$EGORRES <- sas_best32_clean(RESULT_num)
sdtm_eg$EGORRESU <- sas_strip(UNIT)
sdtm_eg$EGORRESU[sdtm_eg$EGORRESU == ""] <- NA_character_
sdtm_eg$EGSTRESN <- RESULT_num
sdtm_eg$EGSTRESC <- sas_best32_clean(sdtm_eg$EGSTRESN)
sdtm_eg$EGSTRESU <- sas_strip(UNIT)
sdtm_eg$EGSTRESU[sdtm_eg$EGSTRESU == ""] <- NA_character_

DATE_raw <- DATE
is_DATE_numeric <- is.numeric(DATE_raw)
is_DATE_Date <- inherits(DATE_raw, "Date") || inherits(DATE_raw, "POSIXt")

if (is_DATE_numeric) {
  egdt_tmp <- as.Date(DATE_raw, origin = "1960-01-01")
} else if (is_DATE_Date) {
  egdt_tmp <- as.Date(DATE_raw)
} else {
  DATE_char <- sas_strip(DATE_raw)
  DATE_char[DATE_char == ""] <- NA_character_
  egdt_tmp <- parse_anydtdte(DATE_char)
  idx_na <- is.na(egdt_tmp) & !is.na(DATE_char)
  if (any(idx_na)) {
    num_try <- suppressWarnings(as.numeric(DATE_char[idx_na]))
    valid_num <- !is.na(num_try) & num_try >= -1000 & num_try <= 50000
    egdt_tmp[idx_na][valid_num] <- as.Date(num_try[valid_num], origin = "1960-01-01")
  }
}

sdtm_eg$EGDTC <- ifelse(is.na(egdt_tmp), NA_character_, format(egdt_tmp, "%Y-%m-%d"))

sdtm_eg <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC")]

ord <- order(sdtm_eg$USUBJID, sdtm_eg$EGDTC, sdtm_eg$EGTESTCD, na.last = FALSE)
sdtm_eg <- sdtm_eg[ord, , drop = FALSE]

sdtm_eg$EGSEQ <- NA_integer_
usubjids <- sdtm_eg$USUBJID
rle_usub <- rle(usubjids)
idx <- 1
for (i in seq_along(rle_usub$lengths)) {
  len <- rle_usub$lengths[i]
  sdtm_eg$EGSEQ[idx:(idx+len-1)] <- seq_len(len)
  idx <- idx + len
}

sdtm_eg <- sdtm_eg[, c("STUDYID","DOMAIN","USUBJID","EGSEQ","VISITNUM","VISIT","EGTESTCD","EGTEST","EGORRES","EGORRESU","EGSTRESC","EGSTRESN","EGSTRESU","EGDTC")]

out_file <- file.path(out, "eg.csv")
write.csv(sdtm_eg, out_file, row.names = FALSE, na = "")

cat("Wrote", nrow(sdtm_eg), "rows to", out_file, "\n")
txt_file <- file.path(out, "eg.txt")
sink(txt_file)
cat("Dataset EG - SDTM EG domain\n")
cat("Rows:", nrow(sdtm_eg), "Cols:", ncol(sdtm_eg), "\n")
print(utils::head(sdtm_eg, 20))
sink()
```