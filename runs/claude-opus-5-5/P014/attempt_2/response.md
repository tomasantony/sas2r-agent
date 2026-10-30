```r
# P014 - SDTM EX mapping (translation of P014_sdtm_ex.sas)
suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40

# ---------------------------------------------------------------------------
# 1. IMPORT RAW EXPOSURE DATA (mimic PROC IMPORT type guessing)
# ---------------------------------------------------------------------------
raw <- read.csv(file.path(project_path, "data/raw/raw_exposure.csv"),
                colClasses = "character", na.strings = character(0),
                check.names = FALSE, stringsAsFactors = FALSE)
names(raw) <- toupper(trimws(names(raw)))
raw[] <- lapply(raw, function(x) { x <- trimws(x); x[x == ""] <- NA; x })

is_num_col <- function(x) {
  v <- x[!is.na(x)]
  length(v) > 0 && all(!is.na(suppressWarnings(as.numeric(v))))
}
num_to_char <- function(x) {
  ifelse(is.na(x), NA_character_,
         ifelse(x == round(x), format(x, scientific = FALSE, trim = TRUE),
                formatC(x, digits = 11, format = "g")))
}

# Per-value parser mimicking ANYDTDTE. (DATESTYLE=MDY)
parse_any_date <- function(x) {
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  for (f in c("Ymd", "mdY", "dbY", "bdY", "dmY", "Ymd HMS", "mdY HMS", "dbY HMS")) {
    idx <- is.na(res) & !is.na(x)
    if (any(idx)) {
      p <- suppressWarnings(as.Date(parse_date_time(x[idx], orders = f, quiet = TRUE)))
      res[idx] <- p
    }
  }
  res
}

# Column-level date import mimicking PROC IMPORT guessing (DDMMYY vs MMDDYY)
import_date_col <- function(x) {
  x <- trimws(as.character(x))
  v <- x[!is.na(x)]
  if (length(v) == 0) return(as.Date(rep(NA_real_, length(x)), origin = "1970-01-01"))
  pat <- "^(\\d{1,2})[/.-](\\d{1,2})[/.-](\\d{2,4})$"
  if (all(grepl(pat, v))) {
    a <- as.integer(sub(pat, "\\1", v))
    b <- as.integer(sub(pat, "\\2", v))
    if (any(a > 12) && !any(b > 12)) {
      ord <- "dmY"
    } else if (any(b > 12) && !any(a > 12)) {
      ord <- "mdY"
    } else {
      ord <- "mdY"
    }
    return(as.Date(parse_date_time(x, orders = c(ord, sub("Y", "y", ord)), quiet = TRUE)))
  }
  parse_any_date(x)
}

# ---------------------------------------------------------------------------
# 2. CORE TRANSFORMATION
# ---------------------------------------------------------------------------
catx_dash <- function(a, b) {
  mapply(function(p, q) {
    parts <- c(p, q)
    parts <- trimws(parts[!is.na(parts) & trimws(parts) != ""])
    paste(parts, collapse = "-")
  }, a, b, USE.NAMES = FALSE)
}

patient <- raw$PATIENT
if (is_num_col(patient)) patient <- num_to_char(as.numeric(patient))

dose <- raw$DOSE
dose <- if (is.null(dose)) NA_real_ else suppressWarnings(as.numeric(dose))

ex_mapped <- tibble(
  STUDYID = "CDISC-01",
  DOMAIN  = "EX",
  USUBJID = substr(catx_dash(rep("CDISC-01", nrow(raw)), patient), 1, usubjid_length),
  EXTRT   = substr(ifelse(is.na(raw$TREATMENT), "", trimws(raw$TREATMENT)), 1, 50),
  EXDOSU  = substr(ifelse(is.na(raw$UNIT), "", trimws(raw$UNIT)), 1, 20)
)
st <- import_date_col(raw$START_DATE)
en <- import_date_col(raw$END_DATE)
ex_mapped$EXSTDTC <- ifelse(is.na(st), "", format(st, "%Y-%m-%d"))
ex_mapped$EXENDTC <- ifelse(is.na(en), "", format(en, "%Y-%m-%d"))
ex_mapped$EXDOSE  <- dose

# ---------------------------------------------------------------------------
# 3. SORT + EXSEQ
# ---------------------------------------------------------------------------
ord <- order(ex_mapped$USUBJID, ex_mapped$EXSTDTC, method = "radix")
ex_mapped <- ex_mapped[ord, ]

ex <- ex_mapped %>%
  group_by(USUBJID) %>%
  mutate(EXSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, EXSEQ, EXTRT, EXDOSU, EXSTDTC, EXENDTC, EXDOSE)
ex <- as.data.frame(ex)

write.csv(ex, file.path(out, "ex.csv"), row.names = FALSE, na = "")

# ---------------------------------------------------------------------------
# 4. VISUAL AUDIT (PROC PRINT obs=10)
# ---------------------------------------------------------------------------
lst <- head(ex, 10)
lst <- data.frame(Obs = seq_len(nrow(lst)),
                  USUBJID = lst$USUBJID, EXSEQ = lst$EXSEQ, EXTRT = lst$EXTRT,
                  EXDOSE = ifelse(is.na(lst$EXDOSE), ".", num_to_char(lst$EXDOSE)),
                  EXDOSU = lst$EXDOSU, EXSTDTC = lst$EXSTDTC, EXENDTC = lst$EXENDTC,
                  stringsAsFactors = FALSE)
write.csv(lst, file.path(out, "P014.csv"), row.names = FALSE, na = "")

txt_tab <- lst
txt_tab[] <- lapply(txt_tab, as.character)
widths <- sapply(names(txt_tab), function(n) max(nchar(n), nchar(txt_tab[[n]]), na.rm = TRUE))
fmt_row <- function(v) paste(mapply(function(s, w) formatC(s, width = w), v, widths),
                             collapse = "  ")
lines <- c("EX Domain Audit (Study Drug Exposure)", "",
           fmt_row(names(txt_tab)), "",
           apply(txt_tab, 1, fmt_row))
writeLines(lines, file.path(out, "P014.txt"))

message("P014 done: ", nrow(ex), " EX records written.")
```