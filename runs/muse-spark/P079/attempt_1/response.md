```r
# P079_l_sae.sas -> R translation
# Listing 16.2.7.2 - Listing of Serious Adverse Events
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(haven))
suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(lubridate))
suppressMessages(library(readr))

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, base) {
  files <- list.files(dir, full.names = TRUE)
  pat <- paste0("^", base, "\\.(sas7bdat|xpt|csv)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) == 0) stop(paste0("File not found: ", base, " in ", dir))
  files[idx[1]]
}
read_auto <- function(p) {
  ext <- tolower(tools::file_ext(p))
  if (ext == "sas7bdat") {
    haven::read_sas(p)
  } else if (ext == "xpt") {
    haven::read_xpt(p)
  } else if (ext == "csv") {
    readr::read_csv(p, show_col_types = FALSE)
  } else stop(paste0("Unknown ext: ", p))
}
zap_all <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) x <- haven::zap_labels(x)
    if (inherits(x, "haven_labelled_spss")) x <- haven::zap_labels(x)
    # zap formats
    attr(x, "format.sas") <- NULL
    attr(x, "format") <- NULL
    attr(x, "label") <- NULL
    # convert factors from haven to character
    if (is.factor(x)) x <- as.character(x)
    df[[nm]] <- x
  }
  df
}

cat("Reading ADAE / ADSL ...\n")
adae_file <- find_file(adam_path, "adae")
adsl_file <- find_file(adam_path, "adsl")
adae <- zap_all(read_auto(adae_file))
adsl <- zap_all(read_auto(adsl_file))
names(adae) <- toupper(names(adae))
names(adsl) <- toupper(names(adsl))

# SAS round half away from zero helper (for completeness)
round_half_up <- function(x, digits = 0) {
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5) / m
}
sas_catx <- function(delim, ...) {
  items <- c(...)
  items <- items[!is.na(items)]
  items <- trimws(as.character(items))
  items <- items[items != ""]
  if (length(items) == 0) return("")
  paste(items, collapse = delim)
}
sas_best <- function(x) {
  if (is.na(x)) return(".")
  trimws(format(x, scientific = FALSE, trim = TRUE, digits = 12))
}
sas_substr1 <- function(x, start = 1, len = 1) {
  if (is.na(x)) return("")
  substr(as.character(x), start, start + len - 1)
}
# SAS yymmdd10. input with ?? : parse first 10 chars, require YYYY-MM-DD or YYYY/MM/DD
sas_input_yymmdd10 <- function(s) {
  if (is.na(s)) return(as.Date(NA))
  s <- as.character(s)
  s <- trimws(s)
  if (s == "") return(as.Date(NA))
  s10 <- substr(s, 1, 10)
  if (!grepl("^[0-9]{4}[-/][0-9]{2}[-/][0-9]{2}$", s10)) return(as.Date(NA))
  s10n <- gsub("/", "-", s10, fixed = FALSE)
  d <- suppressWarnings(as.Date(s10n, format = "%Y-%m-%d"))
  d
}

# --- Step 1: Prepare SAE listing data (PROC SQL) ---
# select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, b.TRTSDT as TRT_START,
# a.AEBODSYS as SOC, a.AEDECOD as PT, a.AETERM as VERBATIM,
# a.AESTDTC as START_DATE, a.AEENDTC as END_DATE,
# a.AETOXGR as GRADE, a.AEREL as RELATIONSHIP, a.AEACN as ACTION_TAKEN,
# a.AEOUT as OUTCOME, AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE
# from adae inner join adsl on USUBJID where SAFFL='Y' and AESER='Y'
# order by TRTA, USUBJID, AESTDTC

# Ensure needed columns exist (create as NA if missing)
need_adae <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC","AETOXGR","AEREL","AEACN","AEOUT","AESER","AESDTH","AESLIFE","AESHOSP","AESDISAB","AESCONG","AESMIE")
need_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","SAFFL")
for (cc in need_adae) if (!cc %in% names(adae)) adae[[cc]] <- NA_character_
for (cc in need_adsl) if (!cc %in% names(adsl)) adsl[[cc]] <- NA

# Normalize TRTSDT to Date (haven usually gives Date; if numeric SAS date, convert)
norm_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  # character?
  d <- suppressWarnings(as.Date(as.character(x)))
  d
}
adsl$TRTSDT_D <- norm_sas_date(adsl$TRTSDT)

a <- adae
b <- adsl[, c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT_D","SAFFL")]
names(b)[names(b)=="TRTSDT_D"] <- "TRT_START_D"

sae_list <- dplyr::inner_join(a, b, by = "USUBJID")

# where b.SAFFL='Y' and a.AESER='Y' (case-sensitive, missing fails)
sae_list <- sae_list[!is.na(sae_list$SAFFL) & sae_list$SAFFL == "Y" &
                     !is.na(sae_list$AESER) & sae_list$AESER == "Y", , drop = FALSE]

sae_list <- data.frame(
  USUBJID = as.character(sae_list$USUBJID),
  TRTA = as.character(sae_list$TRTA),
  AGE = suppressWarnings(as.numeric(sae_list$AGE)),
  SEX = as.character(sae_list$SEX),
  RACE = as.character(sae_list$RACE),
  TRT_START_D = sae_list$TRT_START_D,
  SOC = as.character(sae_list$AEBODSYS),
  PT = as.character(sae_list$AEDECOD),
  VERBATIM = as.character(sae_list$AETERM),
  START_DATE = as.character(sae_list$AESTDTC),
  END_DATE = as.character(sae_list$AEENDTC),
  GRADE = as.character(sae_list$AETOXGR),
  RELATIONSHIP = as.character(sae_list$AEREL),
  ACTION_TAKEN = as.character(sae_list$AEACN),
  OUTCOME = as.character(sae_list$AEOUT),
  AESDTH = as.character(sae_list$AESDTH),
  AESLIFE = as.character(sae_list$AESLIFE),
  AESHOSP = as.character(sae_list$AESHOSP),
  AESDISAB = as.character(sae_list$AESDISAB),
  AESCONG = as.character(sae_list$AESCONG),
  AESMIE = as.character(sae_list$AESMIE),
  stringsAsFactors = FALSE
)
# SAS missing character is "" ; convert NA to "" for ordering keys but keep NA handling later
# For ordering, SAS missing sorts first; implement SAS binary ordering via byte codes
sas_key <- function(v) {
  sapply(v, function(s) {
    if (is.na(s)) return("\001")
    s <- as.character(s)
    if (trimws(s) == "") return("\001")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = " ")
  }, USE.NAMES = FALSE)
}
# order by TRTA, USUBJID, AESTDTC(START_DATE)
k1 <- sas_key(sae_list$TRTA)
k2 <- sas_key(sae_list$USUBJID)
k3 <- sas_key(sae_list$START_DATE)
ord <- order(k1, k2, k3, na.last = FALSE, method = "radix")
sae_list <- sae_list[ord, , drop = FALSE]

# --- Step 2: Derive SAE criteria display ---
n <- nrow(sae_list)
sae_criteria <- character(n)
onset_day <- character(n)
demog <- character(n)
TRT_START <- sae_list$TRT_START_D  # Date

for (i in seq_len(n)) {
  sc <- ""
  if (!is.na(sae_list$AESDTH[i]) && sae_list$AESDTH[i] == "Y") sc <- sas_catx(", ", sc, "Death")
  if (!is.na(sae_list$AESLIFE[i]) && sae_list$AESLIFE[i] == "Y") sc <- sas_catx(", ", sc, "Life-threatening")
  if (!is.na(sae_list$AESHOSP[i]) && sae_list$AESHOSP[i] == "Y") sc <- sas_catx(", ", sc, "Hospitalization")
  if (!is.na(sae_list$AESDISAB[i]) && sae_list$AESDISAB[i] == "Y") sc <- sas_catx(", ", sc, "Disability")
  if (!is.na(sae_list$AESCONG[i]) && sae_list$AESCONG[i] == "Y") sc <- sas_catx(", ", sc, "Congenital anomaly")
  if (!is.na(sae_list$AESMIE[i]) && sae_list$AESMIE[i] == "Y") sc <- sas_catx(", ", sc, "Medically important")
  sae_criteria[i] <- sc

  # Days from first dose to onset
  od <- ""
  trt_d <- TRT_START[i]
  sd <- sae_list$START_DATE[i]
  if (!is.na(trt_d) && !is.na(sd) && trimws(as.character(sd)) != "") {
    p <- sas_input_yymmdd10(sd)
    if (!is.na(p)) {
      onset <- as.numeric(p - trt_d + 1)
      # SAS put with 8. then strip
      od <- trimws(as.character(as.integer(round_half_up(onset, 0))))
    }
  }
  onset_day[i] <- od

  # Demographics string: catx('/', strip(put(AGE,best.)), strip(SEX), strip(substr(RACE,1,1)))
  age_s <- sas_best(sae_list$AGE[i])
  # SAS: strip(put) -> trim; if AGE missing, put gives "." -> keep "."
  # sas_best already returns "." for NA
  sex_s <- if (is.na(sae_list$SEX[i])) "" else trimws(as.character(sae_list$SEX[i]))
  race1 <- sas_substr1(sae_list$RACE[i], 1, 1)
  race1 <- trimws(race1)
  # catx ignores empty but keeps "."
  parts <- c(age_s, sex_s, race1)
  # sas_catx with '/' : need to handle "." as non-empty (keep)
  # Convert NA to ""
  parts[is.na(parts)] <- ""
  parts <- trimws(parts)
  # keep "." ; drop "" only
  keep <- parts != ""
  if (all(!keep)) demog[i] <- ""
  else demog[i] <- paste(parts[keep], collapse = "/")
}

sae_report <- data.frame(
  TRTA = sae_list$TRTA,
  USUBJID = sae_list$USUBJID,
  demog = demog,
  SOC = sae_list$SOC,
  PT = sae_list$PT,
  START_DATE = sae_list$START_DATE,
  END_DATE = sae_list$END_DATE,
  onset_day = onset_day,
  GRADE = sae_list$GRADE,
  RELATIONSHIP = sae_list$RELATIONSHIP,
  ACTION_TAKEN = sae_list$ACTION_TAKEN,
  OUTCOME = sae_list$OUTCOME,
  sae_criteria = sae_criteria,
  stringsAsFactors = FALSE
)
# Replace NA character with "" for display (SAS blank)
for (nm in names(sae_report)) {
  sae_report[[nm]][is.na(sae_report[[nm]])] <- ""
}

cat(paste0("SAE rows: ", nrow(sae_report), "\n"))

# --- Step 3: Generate listing outputs ---
# CSV of table body (report columns in order)
csv_path <- file.path(out, "l_sae.csv")
write.csv(sae_report, csv_path, row.names = FALSE, na = "")

# Text rendering (mimics PROC REPORT + titles, break after TRTA / page)
txt_path <- file.path(out, "l_sae.txt")
# Titles from u_titles.sas for L_SAE
t_trt <- unique(sae_report$TRTA)
# Ensure TRTA order follows SAS binary order
if (length(t_trt) > 0) {
  t_trt <- t_trt[order(sas_key(t_trt), method = "radix")]
}
npages <- max(1, length(t_trt))
if (nrow(sae_report) == 0) npages <- 1

col_labels <- c("Treatment","Subject ID","Age/Sex/Race","SOC","Preferred Term","Start Date","End Date","Study Day","Grade","Relationship","Action Taken","Outcome","SAE Criteria")

fmt_cell <- function(x) { x[is.na(x)] <- ""; trimws(as.character(x)) }

con <- file(txt_path, open = "wt")
writeLines("[Sponsor Name]", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Listing 16.2.7.2", con)
writeLines("Listing of Serious Adverse Events", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)

if (nrow(sae_report) == 0) {
  writeLines("Page 1 of 1", con)
  writeLines("", con)
  writeLines(paste(col_labels, collapse = " | "), con)
  writeLines(paste(rep("----", length(col_labels)), collapse = " | "), con)
  writeLines("No serious adverse events reported.", con)
  writeLines("", con)
  writeLines("Data cutoff: [DDMONYYYY].", con)
  writeLines("Source: ADaM datasets", con)
} else {
  pg <- 0
  for (ti in seq_along(t_trt)) {
    pg <- pg + 1
    trt <- t_trt[ti]
    sub <- sae_report[sae_report$TRTA == trt, , drop = FALSE]
    # within treatment already ordered by USUBJID, START_DATE
    writeLines(paste0("Page ", pg, " of ", npages, "   Treatment: ", trt), con)
    writeLines("", con)
    writeLines(paste(col_labels, collapse = " | "), con)
    writeLines(paste(rep("----------", length(col_labels)), collapse = " | "), con)
    for (r in seq_len(nrow(sub))) {
      vals <- c(fmt_cell(sub$TRTA[r]), fmt_cell(sub$USUBJID[r]), fmt_cell(sub$demog[r]),
                fmt_cell(sub$SOC[r]), fmt_cell(sub$PT[r]),
                fmt_cell(sub$START_DATE[r]), fmt_cell(sub$END_DATE[r]),
                fmt_cell(sub$onset_day[r]), fmt_cell(sub$GRADE[r]),
                fmt_cell(sub$RELATIONSHIP[r]), fmt_cell(sub$ACTION_TAKEN[r]),
                fmt_cell(sub$OUTCOME[r]), fmt_cell(sub$sae_criteria[r]))
      writeLines(paste(vals, collapse = " | "), con)
    }
    writeLines("", con)
    if (ti < length(t_trt)) writeLines("\f", con)
  }
  writeLines("", con)
  writeLines("Data cutoff: [DDMONYYYY].", con)
  writeLines("Source: ADaM datasets", con)
}
close(con)

cat("Done. Outputs written to OUT_DIR\n")
```