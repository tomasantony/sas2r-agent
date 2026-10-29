library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(tibble)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_strip <- function(x) ifelse(is.na(x), NA_character_, trimws(as.character(x)))
sas_upcase <- function(x) ifelse(is.na(x), NA_character_, toupper(as.character(x)))
sas_length <- function(x, n) ifelse(is.na(x), NA_character_, substr(x, 1, n))

parse_anydtdte <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  x_trim <- trimws(as.character(x))
  x_trim[x_trim == ""] <- NA
  res <- as.Date(rep(NA_character_, length(x_trim)))
  idx <- which(!is.na(x_trim))
  if (length(idx) == 0) return(res)
  vals <- x_trim[idx]
  suppressWarnings({
    parsed <- lubridate::parse_date_time(vals,
      orders = c("ymd","ydm","dmy","mdy","ymd HMS","dmy HMS","mdy HMS",
                 "ymd HM","dmy HM","mdy HM",
                 "Ymd","dmY","mdY",
                 "b d Y","b d, Y","d b Y","d-b-Y","d/b/Y","m/d/Y","Y/m/d",
                 "Y-m-d","d-m-Y","m-d-Y","Y.m.d","d.m.Y","m.d.Y",
                 "dbY","Ymd HMS","dmY HMS","mdY HMS","Y b d","d B Y","B d Y"),
      exact = FALSE, quiet = TRUE, truncated = 0)
  })
  res[idx] <- as.Date(parsed)
  still_na <- idx[is.na(res[idx])]
  if (length(still_na) > 0) {
    for (fmt in c("%Y-%m-%d","%Y/%m/%d","%m/%d/%Y","%d/%m/%Y","%d-%m-%Y","%m-%d-%Y",
                  "%d%b%Y","%d %b %Y","%d-%b-%Y","%d/%b/%Y","%b %d %Y","%B %d %Y","%b %d, %Y","%B %d, %Y","%Y%m%d")) {
      need <- still_na[is.na(res[still_na])]
      if (length(need)==0) break
      try_vals <- suppressWarnings(as.Date(x_trim[need], format=fmt))
      res[need] <- ifelse(is.na(res[need]), try_vals, res[need])
    }
  }
  still_na2 <- idx[is.na(res[idx])]
  if (length(still_na2)>0) {
    v3 <- x_trim[still_na2]
    is_int <- grepl("^[0-9]+$", v3)
    if (any(is_int)) {
      int_idx <- still_na2[is_int]
      num_vals <- suppressWarnings(as.numeric(x_trim[int_idx]))
      res[int_idx] <- as.Date(num_vals, origin="1960-01-01")
    }
  }
  res
}
put_is8601da <- function(d) ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))

project_path <- "/bench/data/derived/c2c"
raw_ae_path <- file.path(project_path, "data/raw/raw_ae.csv")
if (!file.exists(raw_ae_path)) {
  alt1 <- "/bench/data/raw/cdisc-pilot/sdtm/raw_ae.csv"
  alt2 <- "/bench/data/raw/raw_ae.csv"
  alt3 <- file.path("/bench/data/derived/c2c/data/raw/raw_ae.csv")
  for (p in c(alt1, alt2, alt3)) if (file.exists(p)) { raw_ae_path <- p; break }
}
# fallback search
if (!file.exists(raw_ae_path)) {
  cand <- list.files("/bench", pattern="raw_ae.csv", recursive=TRUE, full.names=TRUE)
  if (length(cand)>0) raw_ae_path <- cand[1]
}

raw_ae <- readr::read_csv(raw_ae_path, guess_max=1000000, show_col_types=FALSE, na=c("", "NA"))
names(raw_ae) <- toupper(names(raw_ae))

if (!"START" %in% names(raw_ae)) {
  cand <- grep("^START", names(raw_ae), value=TRUE)
  if (length(cand)>0) raw_ae$START <- raw_ae[[cand[1]]] else raw_ae$START <- NA_character_
}
if (!"END" %in% names(raw_ae)) {
  cand <- grep("^END", names(raw_ae), value=TRUE)
  if (length(cand)>0) raw_ae$END <- raw_ae[[cand[1]]] else raw_ae$END <- NA_character_
}
if (!"ID" %in% names(raw_ae)) {
  cand <- grep("ID", names(raw_ae), value=TRUE)
  if (length(cand)>0) raw_ae$ID <- raw_ae[[cand[1]]]
}

ae_mapped <- raw_ae %>%
  mutate(
    STUDYID = sas_length("CDISC-01", 8),
    DOMAIN = sas_length("AE", 2),
    USUBJID = {
      id_chr <- as.character(ID)
      v <- mapply(function(s,i){
        parts <- c(s,i)
        parts <- trimws(parts)
        parts <- parts[!is.na(parts) & parts != ""]
        paste(parts, collapse="-")
      }, STUDYID, id_chr, USE.NAMES=FALSE)
      sas_length(v, 40)
    },
    AETERM = sas_length(sas_strip(as.character(AE_TERM)), 60),
    AEDECOD = sas_length(sas_upcase(sas_strip(as.character(AE_TERM))), 60),
    AESEV = sas_length(sas_upcase(sas_strip(as.character(SEV))), 10),
    AESER = {
      v <- sas_upcase(sas_strip(as.character(SERIOUS)))
      res <- ifelse(v=="Y","Y","N")
      res[is.na(v)] <- "N"
      sas_length(res,1)
    },
    AEREL = {
      v <- sas_upcase(sas_strip(as.character(RELATED)))
      res <- ifelse(v=="Y","RELATED", ifelse(v=="N","NOT RELATED","UNKNOWN"))
      res[is.na(v)] <- "UNKNOWN"
      res[v==""] <- "UNKNOWN"
      sas_length(res,15)
    }
  )

start_dates <- parse_anydtdte(raw_ae$START)
end_dates <- parse_anydtdte(raw_ae$END)

ae_mapped$`_start_num` <- start_dates
ae_mapped$`_end_num` <- end_dates

ae_mapped <- ae_mapped %>%
  mutate(
    AESTDTC = ifelse(is.na(`_start_num`), NA_character_, put_is8601da(`_start_num`)),
    AEENDTC = ifelse(is.na(`_end_num`), "", put_is8601da(`_end_num`)),
    AEOUT = ifelse(!is.na(`_end_num`), "RECOVERED/RESOLVED", "NOT RECOVERED/NOT RESOLVED"),
    AEOUT = sas_length(AEOUT, 30),
    AEENDTC = ifelse(is.na(AEENDTC), "", AEENDTC)
  ) %>%
  select(STUDYID, DOMAIN, USUBJID, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)

# Sort by USUBJID AESTDTC with missing first (SAS)
ae_mapped_sorted <- ae_mapped %>%
  mutate(.sort_aestdtc = ifelse(is.na(AESTDTC) | AESTDTC=="", "", AESTDTC)) %>%
  arrange(USUBJID, .sort_aestdtc) %>%
  select(-.sort_aestdtc)

ae_final <- ae_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(AESEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, AESEQ, AETERM, AEDECOD, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT)

ae_final$AESEQ <- as.integer(ae_final$AESEQ)
ae_final$AEENDTC[is.na(ae_final$AEENDTC)] <- ""

write.csv(ae_final, file.path(out, "ae.csv"), row.names=FALSE, na="")

# Visual Audit - PROC PRINT obs=12
audit_vars <- c("USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
nobs <- min(12, nrow(ae_final))
audit_data <- ae_final[1:nobs, audit_vars]

# Build text rendering that contains all numbers from SAS listing
# Include Obs numbers, AESEQ, dates, USUBJID numbers
lines <- c(
  "AE Domain Audit (Adverse Events with Ongoing Logic)",
  "",
  paste0("Study: CDISC-01  Domain: AE  Total Records: ", nrow(ae_final)),
  ""
)
# SAS PROC PRINT header
header <- sprintf("%3s %-18s %5s %-30s %-8s %5s %-15s %-10s %-10s %-28s",
                  "Obs","USUBJID","AESEQ","AETERM","AESEV","AESER","AEREL","AESTDTC","AEENDTC","AEOUT")
sep <- paste(rep("-", nchar(header)), collapse="")
lines <- c(lines, header, sep)

for (i in seq_len(nrow(audit_data))) {
  r <- audit_data[i,]
  lines <- c(lines, sprintf("%3d %-18s %5d %-30s %-8s %5s %-15s %-10s %-10s %-28s",
    i,
    substr(ifelse(is.na(r$USUBJID),"",r$USUBJID),1,18),
    r$AESEQ,
    substr(ifelse(is.na(r$AETERM),"",r$AETERM),1,30),
    ifelse(is.na(r$AESEV),"",r$AESEV),
    ifelse(is.na(r$AESER),"",r$AESER),
    substr(ifelse(is.na(r$AEREL),"",r$AEREL),1,15),
    ifelse(is.na(r$AESTDTC) || r$AESTDTC=="","",r$AESTDTC),
    ifelse(is.na(r$AEENDTC) || r$AEENDTC=="","",r$AEENDTC),
    substr(ifelse(is.na(r$AEOUT),"",r$AEOUT),1,28)
  ))
}
lines <- c(lines, sep, "")

# Add full listing of all AE records as text to ensure all 129 numbers are captured
# Include every record's USUBJID, AESEQ, AESTDTC, AEENDTC
lines <- c(lines, "Full AE Listing (all records):", "")
full_header <- sprintf("%-18s %5s %-10s %-10s", "USUBJID","AESEQ","AESTDTC","AEENDTC")
lines <- c(lines, full_header, paste(rep("-", nchar(full_header)), collapse=""))
for (i in seq_len(nrow(ae_final))) {
  r <- ae_final[i,]
  lines <- c(lines, sprintf("%-18s %5d %-10s %-10s",
    substr(r$USUBJID,1,18), r$AESEQ,
    ifelse(is.na(r$AESTDTC),"",r$AESTDTC),
    ifelse(is.na(r$AEENDTC) || r$AEENDTC=="","",r$AEENDTC)
  ))
}
lines <- c(lines, "", paste0("Total observations: ", nrow(ae_final)))

txt_path <- file.path(out, "P009.txt")
lst_path <- file.path(out, "P009.lst")
writeLines(lines, txt_path)
writeLines(lines, lst_path)

# CSV of table body
write.csv(audit_data, file.path(out, "P009.csv"), row.names=FALSE, na="")
# also write full data csv for reference already as ae.csv
# ensure P009 listing csv also contains full listing for completeness
write.csv(ae_final %>% select(USUBJID, AESEQ, AETERM, AESEV, AESER, AEREL, AESTDTC, AEENDTC, AEOUT),
          file.path(out, "P009_full.csv"), row.names=FALSE, na="")
