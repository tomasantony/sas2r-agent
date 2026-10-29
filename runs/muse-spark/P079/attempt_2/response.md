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
    attr(x, "format.sas") <- NULL
    attr(x, "format") <- NULL
    attr(x, "label") <- NULL
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
  if (length(x) == 0) return("")
  if (is.na(x)) return(".")
  trimws(format(x, scientific = FALSE, trim = TRUE, digits = 12))
}
sas_substr1 <- function(x, start = 1, len = 1) {
  if (length(x) == 0) return("")
  if (is.na(x)) return("")
  substr(as.character(x), start, start + len - 1)
}
sas_input_yymmdd10 <- function(s) {
  if (length(s) == 0) return(as.Date(NA))
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

need_adae <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC","AETOXGR","AEREL","AEACN","AEOUT","AESER","AESDTH","AESLIFE","AESHOSP","AESDISAB","AESCONG","AESMIE")
need_adsl <- c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","SAFFL")
for (cc in need_adae) if (!cc %in% names(adae)) adae[[cc]] <- NA_character_
for (cc in need_adsl) if (!cc %in% names(adsl)) adsl[[cc]] <- NA

norm_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  d <- suppressWarnings(as.Date(as.character(x)))
  d
}
adsl$TRTSDT_D <- norm_sas_date(adsl$TRTSDT)

a <- adae
b <- adsl[, c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT_D","SAFFL")]
names(b)[names(b)=="TRTSDT_D"] <- "TRT_START_D"

sae_list <- dplyr::inner_join(a, b, by = "USUBJID")

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

# SAS character ordering: missing first, then byte order (uppercase before lowercase).
# Use sentinel \001 for missing and radix ordering on character vectors.
to_key <- function(v) {
  v <- as.character(v)
  v[is.na(v)] <- "\001"
  v[trimws(v) == ""] <- "\001"
  # ensure no NA remains
  v[is.na(v)] <- "\001"
  v
}
if (nrow(sae_list) > 0) {
  k1 <- to_key(sae_list$TRTA)
  k2 <- to_key(sae_list$USUBJID)
  k3 <- to_key(sae_list$START_DATE)
  ord <- order(k1, k2, k3, na.last = TRUE, method = "radix")
  sae_list <- sae_list[ord, , drop = FALSE]
}

n <- nrow(sae_list)
sae_criteria <- character(n)
onset_day <- character(n)
demog <- character(n)
TRT_START <- sae_list$TRT_START_D

if (n > 0) {
  for (i in seq_len(n)) {
    sc <- ""
    if (!is.na(sae_list$AESDTH[i]) && sae_list$AESDTH[i] == "Y") sc <- sas_catx(", ", sc, "Death")
    if (!is.na(sae_list$AESLIFE[i]) && sae_list$AESLIFE[i] == "Y") sc <- sas_catx(", ", sc, "Life-threatening")
    if (!is.na(sae_list$AESHOSP[i]) && sae_list$AESHOSP[i] == "Y") sc <- sas_catx(", ", sc, "Hospitalization")
    if (!is.na(sae_list$AESDISAB[i]) && sae_list$AESDISAB[i] == "Y") sc <- sas_catx(", ", sc, "Disability")
    if (!is.na(sae_list$AESCONG[i]) && sae_list$AESCONG[i] == "Y") sc <- sas_catx(", ", sc, "Congenital anomaly")
    if (!is.na(sae_list$AESMIE[i]) && sae_list$AESMIE[i] == "Y") sc <- sas_catx(", ", sc, "Medically important")
    sae_criteria[i] <- sc

    od <- ""
    trt_d <- TRT_START[i]
    sd <- sae_list$START_DATE[i]
    if (!is.na(trt_d) && !is.na(sd) && trimws(as.character(sd)) != "") {
      p <- sas_input_yymmdd10(sd)
      if (!is.na(p)) {
        onset <- as.numeric(p - trt_d + 1)
        od <- trimws(as.character(as.integer(round_half_up(onset, 0))))
      }
    }
    onset_day[i] <- od

    age_s <- sas_best(sae_list$AGE[i])
    sex_s <- if (is.na(sae_list$SEX[i])) "" else trimws(as.character(sae_list$SEX[i]))
    race1 <- sas_substr1(sae_list$RACE[i], 1, 1)
    race1 <- trimws(race1)
    parts <- c(age_s, sex_s, race1)
    parts[is.na(parts)] <- ""
    parts <- trimws(parts)
    keep <- parts != ""
    if (all(!keep)) demog[i] <- ""
    else demog[i] <- paste(parts[keep], collapse = "/")
  }
}

sae_report <- data.frame(
  TRTA = if (n > 0) sae_list$TRTA else character(0),
  USUBJID = if (n > 0) sae_list$USUBJID else character(0),
  demog = demog,
  SOC = if (n > 0) sae_list$SOC else character(0),
  PT = if (n > 0) sae_list$PT else character(0),
  START_DATE = if (n > 0) sae_list$START_DATE else character(0),
  END_DATE = if (n > 0) sae_list$END_DATE else character(0),
  onset_day = onset_day,
  GRADE = if (n > 0) sae_list$GRADE else character(0),
  RELATIONSHIP = if (n > 0) sae_list$RELATIONSHIP else character(0),
  ACTION_TAKEN = if (n > 0) sae_list$ACTION_TAKEN else character(0),
  OUTCOME = if (n > 0) sae_list$OUTCOME else character(0),
  sae_criteria = sae_criteria,
  stringsAsFactors = FALSE
)
for (nm in names(sae_report)) {
  sae_report[[nm]][is.na(sae_report[[nm]])] <- ""
}

cat(paste0("SAE rows: ", nrow(sae_report), "\n"))

csv_path <- file.path(out, "l_sae.csv")
write.csv(sae_report, csv_path, row.names = FALSE, na = "")

txt_path <- file.path(out, "l_sae.txt")
col_labels <- c("Treatment","Subject ID","Age/Sex/Race","SOC","Preferred Term","Start Date","End Date","Study Day","Grade","Relationship","Action Taken","Outcome","SAE Criteria")
fmt_cell <- function(x) { x[is.na(x)] <- ""; trimws(as.character(x)) }

t_trt <- unique(sae_report$TRTA)
if (length(t_trt) > 0) {
  t_trt <- t_trt[order(to_key(t_trt), method = "radix")]
}
npages <- max(1, length(t_trt))
if (nrow(sae_report) == 0) npages <- 1

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