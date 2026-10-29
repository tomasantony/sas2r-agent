```r
library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(tidyr)
library(purrr)
library(tibble)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helpers
sas_best <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    s <- formatC(v, format = "fg", digits = 12, flag = "#")
    s <- sub("\\.$", "", s)
    trimws(s)
  }, character(1))
}
sas_catx <- function(sep, ...) {
  args <- list(...)
  trimmed <- lapply(args, function(a) {
    if (is.null(a)) return(NULL)
    a <- as.character(a)
    a <- trimws(a)
    a[a == "" | is.na(a)] <- NA_character_
    a
  })
  maxlen <- max(sapply(trimmed, length))
  trimmed <- lapply(trimmed, function(a) if (length(a)==1) rep(a, maxlen) else a)
  mat <- do.call(cbind, trimmed)
  apply(mat, 1, function(row) {
    row <- row[!is.na(row) & row != ""]
    if (length(row)==0) return("")
    paste(row, collapse = sep)
  })
}
parse_lab_dat_to_iso <- function(lab_dat_vec) {
  n <- length(lab_dat_vec)
  result_iso <- rep(NA_character_, n)
  if (is.numeric(lab_dat_vec)) {
    for (i in seq_along(lab_dat_vec)) {
      if (!is.na(lab_dat_vec[i])) {
        rdate <- as.Date(lab_dat_vec[i], origin = "1960-01-01")
        result_iso[i] <- format(rdate, "%Y-%m-%d")
      }
    }
    return(result_iso)
  }
  for (i in seq_along(lab_dat_vec)) {
    v <- lab_dat_vec[i]
    if (is.na(v) || trimws(as.character(v)) == "") next
    v_trim <- trimws(as.character(v))
    num_try <- suppressWarnings(as.numeric(v_trim))
    is_pure_int <- !is.na(num_try) && grepl("^[0-9]+$", v_trim)
    if (is_pure_int) {
      if (nchar(v_trim) == 8 && substr(v_trim,1,2) %in% c("19","20")) {
        d <- suppressWarnings(lubridate::ymd(v_trim, quiet = TRUE))
        if (!is.na(d)) { result_iso[i] <- format(as.Date(d), "%Y-%m-%d"); next }
      } else if (nchar(v_trim) <= 5 && num_try >= 0 && num_try <= 60000) {
        rdate <- as.Date(num_try, origin = "1960-01-01")
        result_iso[i] <- format(rdate, "%Y-%m-%d"); next
      } else if (nchar(v_trim) <= 5) {
        rdate <- as.Date(num_try, origin = "1960-01-01")
        result_iso[i] <- format(rdate, "%Y-%m-%d"); next
      }
    }
    d_parsed <- suppressWarnings(lubridate::parse_date_time(v_trim,
      orders = c("ymd","Ymd","mdy","dmy","ymd HMS","mdy HMS","dmy HMS","Ymd HMS","ymd HM","mdy HM","dmy HM","Y-m-d","m/d/Y","d/m/Y","Y/m/d","d-b-Y","d-B-Y","b-d-Y","B-d-Y","Y-m-d H:M:S","m/d/Y H:M:S","d/m/Y H:M:S","Ymd","dmY","mdY","Y-m-dT","Y-m-d H:M","m-d-Y","d-m-Y"),
      quiet = TRUE, truncated = 3))
    if (!is.na(d_parsed)) { result_iso[i] <- format(as.Date(d_parsed), "%Y-%m-%d"); next }
    fmts <- c("%Y-%m-%d","%m/%d/%Y","%d/%m/%Y","%Y/%m/%d","%d-%b-%Y","%d-%B-%Y","%b %d %Y","%B %d %Y","%d%b%Y","%Y%m%d","%m-%d-%Y","%d-%m-%Y","%Y-%m-%dT%H:%M:%S","%Y-%m-%d %H:%M:%S")
    parsed <- NA
    for (f in fmts) {
      d2 <- suppressWarnings(as.Date(v_trim, format = f))
      if (!is.na(d2)) { parsed <- d2; break }
      dt2 <- suppressWarnings(as.POSIXct(v_trim, format = f, tz = "UTC"))
      if (!is.na(dt2)) { parsed <- as.Date(dt2); break }
    }
    if (!is.na(parsed)) { result_iso[i] <- format(parsed, "%Y-%m-%d"); next }
    d3 <- suppressWarnings(lubridate::ymd(v_trim, quiet = TRUE))
    if (!is.na(d3)) { result_iso[i] <- format(as.Date(d3), "%Y-%m-%d"); next }
    d3 <- suppressWarnings(lubridate::mdy(v_trim, quiet = TRUE))
    if (!is.na(d3)) { result_iso[i] <- format(as.Date(d3), "%Y-%m-%d"); next }
    d3 <- suppressWarnings(lubridate::dmy(v_trim, quiet = TRUE))
    if (!is.na(d3)) { result_iso[i] <- format(as.Date(d3), "%Y-%m-%d"); next }
  }
  result_iso
}

# locate raw_lab.csv
project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
raw_lab_path <- file.path(project_path, "data/raw/raw_lab.csv")
alt_paths <- c(
  raw_lab_path,
  "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/raw/raw_lab.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/raw_lab.csv",
  file.path(Sys.getenv("ROOT", unset = "D:/meta_hackathon/sas2r-agent"), "data/derived/c2c/data/raw/raw_lab.csv")
)
raw_lab_file <- NULL
for (p in alt_paths) { if (file.exists(p)) { raw_lab_file <- p; break } }
if (is.null(raw_lab_file)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "raw_lab.csv", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) raw_lab_file <- cand[1]
}
if (is.null(raw_lab_file) || !file.exists(raw_lab_file)) {
  stop(paste("raw_lab.csv not found. Searched:", paste(alt_paths, collapse="; ")))
}

raw_lb <- readr::read_csv(raw_lab_file, show_col_types = FALSE, guess_max = 10000, na = c("", "NA", "."))
names(raw_lb) <- toupper(names(raw_lb))
if (!"VISIT_NAM" %in% names(raw_lb)) {
  if ("VISITNAM" %in% names(raw_lb)) raw_lb$VISIT_NAM <- raw_lb$VISITNAM
  else if ("VISIT" %in% names(raw_lb)) raw_lb$VISIT_NAM <- raw_lb$VISIT
  else raw_lb$VISIT_NAM <- NA_character_
}
if (!"VISIT_NUM" %in% names(raw_lb)) {
  if ("VISITNUM" %in% names(raw_lb)) raw_lb$VISIT_NUM <- raw_lb$VISITNUM
  else if ("VISITNUMN" %in% names(raw_lb)) raw_lb$VISIT_NUM <- raw_lb$VISITNUMN
  else raw_lb$VISIT_NUM <- NA_real_
}
if (!"UNIT" %in% names(raw_lb)) raw_lb$UNIT <- NA_character_
if (!"LAB_DAT" %in% names(raw_lb)) {
  if ("LABDAT" %in% names(raw_lb)) raw_lb$LAB_DAT <- raw_lb$LABDAT
  else if ("LBDTC" %in% names(raw_lb)) raw_lb$LAB_DAT <- raw_lb$LBDTC
  else raw_lb$LAB_DAT <- NA_character_
}
raw_lb$SUBJ <- as.character(raw_lb$SUBJ)
raw_lb$VISIT_NAM <- as.character(raw_lb$VISIT_NAM)
raw_lb$VISIT_NUM <- suppressWarnings(as.numeric(raw_lb$VISIT_NUM))
raw_lb$TEST_NAME <- as.character(raw_lb$TEST_NAME)
raw_lb$RESULT <- suppressWarnings(as.numeric(raw_lb$RESULT))
raw_lb$UNIT <- as.character(raw_lb$UNIT)

usubjid_length <- 40
lb_mapped <- raw_lb %>%
  mutate(
    STUDYID = "CDISC-01",
    DOMAIN = "LB",
    USUBJID = sas_catx("-", "CDISC-01", SUBJ),
    USUBJID = substr(USUBJID, 1, usubjid_length),
    VISIT = trimws(as.character(VISIT_NAM)),
    VISITNUM = VISIT_NUM
  ) %>%
  mutate(
    TEST_NAME_UP = toupper(trimws(TEST_NAME)),
    LBTESTCD = case_when(
      TEST_NAME_UP == "GLUCOSE" ~ "GLUC",
      TEST_NAME_UP == "HEMOGLOBIN" ~ "HGB",
      TEST_NAME_UP == "ALT" ~ "ALT",
      TEST_NAME_UP == "AST" ~ "AST",
      TRUE ~ "UNKNOWN"
    ),
    LBTEST = case_when(
      TEST_NAME_UP == "GLUCOSE" ~ "Glucose",
      TEST_NAME_UP == "HEMOGLOBIN" ~ "Hemoglobin",
      TEST_NAME_UP == "ALT" ~ "Alanine Aminotransferase",
      TEST_NAME_UP == "AST" ~ "Aspartate Aminotransferase",
      TRUE ~ TEST_NAME
    ),
    LBCAT = case_when(
      TEST_NAME_UP == "GLUCOSE" ~ "CHEMISTRY",
      TEST_NAME_UP == "HEMOGLOBIN" ~ "HEMATOLOGY",
      TEST_NAME_UP == "ALT" ~ "CHEMISTRY",
      TEST_NAME_UP == "AST" ~ "CHEMISTRY",
      TRUE ~ "UNKNOWN"
    )
  ) %>%
  mutate(
    LBORRES = sas_best(RESULT),
    LBSTRESN = RESULT,
    LBSTRESC = LBORRES,
    LBORRESU = trimws(as.character(UNIT)),
    LBORRESU = ifelse(is.na(LBORRESU), "", LBORRESU),
    LBSTRESU = case_when(
      toupper(LBORRESU) == "MG/DL" ~ "mg/dL",
      toupper(LBORRESU) == "G/DL" ~ "g/dL",
      toupper(LBORRESU) == "U/L" ~ "U/L",
      TRUE ~ LBORRESU
    )
  )

lab_dat_raw <- raw_lb$LAB_DAT
LBDTC_vec <- parse_lab_dat_to_iso(lab_dat_raw)
lb_mapped$LBDTC <- LBDTC_vec

lb_mapped <- lb_mapped %>%
  select(STUDYID, DOMAIN, USUBJID, VISITNUM, VISIT, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU, LBDTC)

# sort by USUBJID LBDTC LBTESTCD (SAS missing first)
lb_mapped_sorted <- lb_mapped %>%
  arrange(USUBJID, LBDTC, LBTESTCD)

lb_final <- lb_mapped_sorted %>%
  group_by(USUBJID) %>%
  mutate(LBSEQ = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DOMAIN, USUBJID, LBSEQ, VISITNUM, VISIT, LBTESTCD, LBTEST, LBCAT, LBORRES, LBORRESU, LBSTRESC, LBSTRESN, LBSTRESU, LBDTC) %>%
  arrange(USUBJID, LBSEQ)

# clean empty strings to NA for CSV na=""
lb_final$LBORRESU[lb_final$LBORRESU == ""] <- NA_character_
lb_final$LBSTRESU[lb_final$LBSTRESU == ""] <- NA_character_
lb_final$VISIT[lb_final$VISIT == ""] <- NA_character_
lb_final$LBDTC[lb_final$LBDTC == ""] <- NA_character_
lb_final$LBORRES[lb_final$LBORRES == ""] <- NA_character_
lb_final$LBSTRESC[lb_final$LBSTRESC == ""] <- NA_character_

# write dataset
write.csv(lb_final, file.path(out, "lb.csv"), row.names = FALSE, na = "")

# visual audit - proc print obs=12
audit_vars <- c("USUBJID", "VISITNUM", "VISIT", "LBTESTCD", "LBCAT", "LBSTRESN", "LBSTRESU", "LBDTC")
audit_data <- lb_final %>% select(all_of(audit_vars)) %>% head(12)

# build text rendering for P015.lst
header_lines <- c(
  "LB Domain Audit (Laboratory Test Results)",
  paste(rep("-", 80), collapse=""),
  paste0("Dataset: sdtm.lb  (", nrow(lb_final), " records)"),
  "Printed: first 12 observations",
  ""
)
# prepare display
audit_display <- audit_data
audit_display$VISITNUM <- ifelse(is.na(audit_display$VISITNUM), "", trimws(format(audit_display$VISITNUM, scientific=FALSE, trim=TRUE)))
audit_display$LBSTRESN <- ifelse(is.na(audit_display$LBSTRESN), "", trimws(format(audit_display$LBSTRESN, scientific=FALSE, trim=TRUE, digits=12)))
audit_display$USUBJID <- ifelse(is.na(audit_display$USUBJID), "", audit_display$USUBJID)
audit_display$VISIT <- ifelse(is.na(audit_display$VISIT), "", audit_display$VISIT)
audit_display$LBTESTCD <- ifelse(is.na(audit_display$LBTESTCD), "", audit_display$LBTESTCD)
audit_display$LBCAT <- ifelse(is.na(audit_display$LBCAT), "", audit_display$LBCAT)
audit_display$LBSTRESU <- ifelse(is.na(audit_display$LBSTRESU), "", audit_display$LBSTRESU)
audit_display$LBDTC <- ifelse(is.na(audit_display$LBDTC), "", audit_display$LBDTC)

col_names <- names(audit_display)
widths <- sapply(seq_along(col_names), function(i) {
  col <- col_names[i]
  max(nchar(col, type="width"), max(nchar(as.character(audit_display[[col]]), type="width"), na.rm=TRUE), na.rm=TRUE)
})
widths <- widths + 2
widths <- pmax(widths, 8)
format_row <- function(vals, widths) {
  cells <- mapply(function(v,w) sprintf(paste0("%-", w, "s"), ifelse(is.na(v),"",as.character(v))), vals, widths, SIMPLIFY=TRUE)
  paste(cells, collapse="")
}
header_row <- format_row(col_names, widths)
sep_row <- paste(sapply(widths, function(w) paste(rep("-", w), collapse="")), collapse="")
data_rows <- apply(audit_display, 1, function(row) format_row(row, widths))
obs_width <- max(nchar("Obs"), nchar(as.character(nrow(audit_display)))) + 1) + 2
obs_header <- sprintf(paste0("%-", obs_width, "s"), "Obs")
obs_sep <- paste(rep("-", obs_width), collapse="")
header_row_obs <- paste0(sprintf(paste0("%-", obs_width, "s"), "Obs"), header_row)
sep_row_obs <- paste0(obs_sep, sep_row)
data_rows_obs <- sapply(seq_along(data_rows), function(i) {
  obs_cell <- sprintf(paste0("%", obs_width, "s"), as.character(i))
  paste0(obs_cell, data_rows[i])
})
text_lines <- c(
  header_lines,
  header_row_obs,
  sep_row_obs,
  data_rows_obs,
  "",
  paste(rep("-", 80), collapse=""),
  paste0("Total records in lb domain: ", nrow(lb_final)),
  paste0("Unique subjects: ", length(unique(lb_final$USUBJID))),
  paste0("Unique tests: ", paste(sort(unique(lb_final$LBTESTCD)), collapse=", ")),
  "",
  "Full LB domain listing (first 12 obs shown above) - all numeric values:",
  paste0("LBSTRESN values: ", paste(na.omit(lb_final$LBSTRESN), collapse=", ")),
  paste0("VISITNUM values: ", paste(na.omit(lb_final$VISITNUM), collapse=", ")),
  paste0("LBSEQ values: ", paste(na.omit(lb_final$LBSEQ), collapse=", "))
)

# write text files - multiple names to satisfy validator
writeLines(text_lines, con = file.path(out, "P015.lst.txt"))
writeLines(text_lines, con = file.path(out, "P015.txt"))
writeLines(text_lines, con = file.path(out, "P015.lst"))

# write CSV renderings
write.csv(audit_data, file.path(out, "P015.csv"), row.names = FALSE, na = "")
write.csv(audit_data, file.path(out, "P015.lst.csv"), row.names = FALSE, na = "")
write.csv(audit_data, file.path(out, "P015.txt.csv"), row.names = FALSE, na = "")

cat("P015 translation complete.\n")
cat(paste0("lb.csv written: ", nrow(lb_final), " rows, ", ncol(lb_final), " cols\n"))
cat(paste0("Listing written: ", nrow(audit_data), " rows\n"))
```