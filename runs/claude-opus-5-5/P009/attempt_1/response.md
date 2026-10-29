```r
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lubridate); library(stringr)
})

out <- Sys.getenv("OUT_DIR")
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
usubjid_length <- 40

# 1. IMPORT RAW ADVERSE EVENTS (read everything as character, then mimic PROC IMPORT typing)
raw_ae <- read_csv(file.path(project_path, "data/raw/raw_ae.csv"),
                   col_types = cols(.default = col_character()),
                   na = c("", "NA"), trim_ws = TRUE)
names(raw_ae) <- toupper(names(raw_ae))

nz <- function(x) { x[is.na(x)] <- ""; x }
sas_strip <- function(x) trimws(nz(x))
trunc_len <- function(x, n) substr(x, 1, n)

# PROC IMPORT would read an all-numeric column as numeric -> catx uses BEST12. (no leading zeros)
id_chr <- raw_ae$ID
id_nonmiss <- id_chr[!is.na(id_chr) & trimws(id_chr) != ""]
if (length(id_nonmiss) > 0 && all(suppressWarnings(!is.na(as.numeric(id_nonmiss))))) {
  idn <- suppressWarnings(as.numeric(id_chr))
  id_chr <- ifelse(is.na(idn), "", trimws(format(idn, scientific = FALSE, drop0trailing = TRUE, trim = TRUE)))
  id_chr <- sapply(idn, function(v) if (is.na(v)) "" else as.character(v))
}
id_chr <- sas_strip(id_chr)

# Date parser approximating ANYDTDTE.
parse_any_date <- function(x) {
  x <- sas_strip(x)
  res <- as.Date(rep(NA_character_, length(x)))
  ok <- x != ""
  if (any(ok)) {
    p <- suppressWarnings(parse_date_time(x[ok],
            orders = c("Ymd", "Ymd HMS", "Ymd HM", "mdY", "dbY", "dmY", "bdY", "Ym"),
            quiet = TRUE))
    res[ok] <- as.Date(p)
  }
  res
}

# 2. CORE TRANSFORMATION & DERIVATION
STUDYID <- "CDISC-01"
ae_mapped <- tibble(
  STUDYID = STUDYID,
  DOMAIN  = "AE",
  USUBJID = trunc_len(ifelse(id_chr == "", STUDYID, paste(STUDYID, id_chr, sep = "-")), usubjid_length),
  AETERM  = trunc_len(sas_strip(raw_ae$AE_TERM), 60)
)
ae_mapped$AEDECOD <- trunc_len(toupper(ae_mapped$AETERM), 60)
rel <- toupper(sas_strip(raw_ae$RELATED))
ae_mapped$AEREL <- trunc_len(ifelse(rel == "Y", "RELATED", ifelse(rel == "N", "NOT RELATED", "UNKNOWN")), 15)
ae_mapped$AESEV <- trunc_len(toupper(sas_strip(raw_ae$SEV)), 10)
st <- parse_any_date(raw_ae$START)
en <- parse_any_date(raw_ae$END)
ae_mapped$AEOUT <- ifelse(!is.na(en), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED")
ae_mapped$AESER <- ifelse(toupper(sas_strip(raw_ae$SERIOUS)) == "Y", "Y", "N")
ae_mapped$AESTDTC <- ifelse(is.na(st), "", format(st, "%Y-%m-%d"))
ae_mapped$AEENDTC <- ifelse(is.na(en), "", format(en, "%Y-%m-%d"))

# 3. SORT and AESEQ
ord <- order(ae_mapped$USUBJID, ae_mapped$AESTDTC, method = "radix")
ae_mapped <- ae_mapped[ord, ]
ae <- ae_mapped %>%
  group_by(USUBJID) %>% mutate(AESEQ = row_number()) %>% ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AEREL, AESEV, AEOUT, AESER, AESTDTC, AEENDTC)

write.csv(ae, file.path(out, "ae.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT
lst <- head(ae, 12) %>%
  mutate(Obs = row_number()) %>%
  select(Obs, USUBJID, AESEQ, AETERM, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)
write.csv(lst, file.path(out, "P009.csv"), row.names = FALSE, na = "")

lst_chr <- as.data.frame(lapply(lst, as.character), stringsAsFactors = FALSE)
widths <- sapply(names(lst_chr), function(n) max(nchar(n), nchar(lst_chr[[n]]), na.rm = TRUE))
fmt_row <- function(v) paste(mapply(function(s, w, n) {
  if (n %in% c("Obs", "AESEQ")) formatC(s, width = w) else formatC(s, width = w, flag = "-")
}, v, widths, names(lst_chr)), collapse = "  ")
lines <- c("AE Domain Audit (Adverse Events with Ongoing Logic)", "",
           fmt_row(names(lst_chr)), "",
           apply(lst_chr, 1, fmt_row))
writeLines(lines, file.path(out, "P009.txt"))
cat("P009 done\n")
```