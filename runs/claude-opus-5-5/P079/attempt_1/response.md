```r
# P079 - Listing 16.2.7.2 - Listing of Serious Adverse Events (R translation)
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# Read a dataset from the ADaM library (.sas7bdat preferred, .xpt as fallback)
read_lib <- function(name) {
  f1 <- file.path(adampath, paste0(name, ".sas7bdat"))
  f2 <- file.path(adampath, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- read_sas(f1)
  } else if (file.exists(f2)) {
    d <- read_xpt(f2)
  } else {
    stop("Dataset not found: ", name)
  }
  d <- zap_labels(zap_formats(d))
  names(d) <- toupper(names(d))
  as.data.frame(d, stringsAsFactors = FALSE)
}

adae <- read_lib("adae")
adsl <- read_lib("adsl")
cat("Read adae:", nrow(adae), "rows; adsl:", nrow(adsl), "rows\n")

# Return a column, or an all-NA column if the variable does not exist
getcol <- function(d, v) if (v %in% names(d)) d[[v]] else rep(NA, nrow(d))

# SAS-like character value: missing becomes ""
chr <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

# SAS sort key: missing sorts first, then byte ordering (C locale)
sas_order <- function(...) {
  args <- lapply(list(...), function(x) chr(x))
  do.call(order, c(args, list(method = "radix")))
}

#--- Step 1: PROC SQL inner join ---
a <- adae
b <- adsl

b_sel <- data.frame(
  USUBJID = chr(b$USUBJID),
  TRTA    = chr(getcol(b, "TRTA")),
  AGE     = getcol(b, "AGE"),
  SEX     = chr(getcol(b, "SEX")),
  RACE    = chr(getcol(b, "RACE")),
  TRT_START = getcol(b, "TRTSDT"),
  SAFFL   = chr(getcol(b, "SAFFL")),
  stringsAsFactors = FALSE
)

a_sel <- data.frame(
  USUBJID      = chr(a$USUBJID),
  SOC          = chr(getcol(a, "AEBODSYS")),
  PT           = chr(getcol(a, "AEDECOD")),
  VERBATIM     = chr(getcol(a, "AETERM")),
  START_DATE   = chr(getcol(a, "AESTDTC")),
  END_DATE     = chr(getcol(a, "AEENDTC")),
  GRADE        = getcol(a, "AETOXGR"),
  RELATIONSHIP = chr(getcol(a, "AEREL")),
  ACTION_TAKEN = chr(getcol(a, "AEACN")),
  OUTCOME      = chr(getcol(a, "AEOUT")),
  AESDTH       = chr(getcol(a, "AESDTH")),
  AESLIFE      = chr(getcol(a, "AESLIFE")),
  AESHOSP      = chr(getcol(a, "AESHOSP")),
  AESDISAB     = chr(getcol(a, "AESDISAB")),
  AESCONG      = chr(getcol(a, "AESCONG")),
  AESMIE       = chr(getcol(a, "AESMIE")),
  AESER        = chr(getcol(a, "AESER")),
  .rowa        = seq_len(nrow(a)),
  stringsAsFactors = FALSE
)

sae_list <- inner_join(a_sel, b_sel, by = "USUBJID", relationship = "many-to-many") %>%
  filter(SAFFL == "Y", AESER == "Y")
sae_list <- sae_list[sas_order(sae_list$TRTA, sae_list$USUBJID, sae_list$START_DATE), ]

# Convert TRTSDT to Date if it is numeric (SAS date = days since 1960-01-01)
if (!inherits(sae_list$TRT_START, "Date")) {
  sae_list$TRT_START <- as.Date(as.numeric(sae_list$TRT_START), origin = "1960-01-01")
}

#--- Step 2: derive SAE criteria / onset day / demographics ---
catx_add <- function(cur, cond, txt) {
  ifelse(cond, ifelse(cur == "", txt, paste0(cur, ", ", txt)), cur)
}

sae_report <- sae_list
sc <- rep("", nrow(sae_report))
sc <- catx_add(sc, sae_report$AESDTH   == "Y", "Death")
sc <- catx_add(sc, sae_report$AESLIFE  == "Y", "Life-threatening")
sc <- catx_add(sc, sae_report$AESHOSP  == "Y", "Hospitalization")
sc <- catx_add(sc, sae_report$AESDISAB == "Y", "Disability")
sc <- catx_add(sc, sae_report$AESCONG  == "Y", "Congenital anomaly")
sc <- catx_add(sc, sae_report$AESMIE   == "Y", "Medically important")
sae_report$sae_criteria <- sc

# input(START_DATE, yymmdd10.) reads the first 10 characters as a full date
parse_yymmdd10 <- function(x) {
  s <- substr(x, 1, 10)
  ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", s)
  d <- rep(as.Date(NA), length(x))
  d[ok] <- as.Date(s[ok], format = "%Y-%m-%d")
  d
}
stdt <- parse_yymmdd10(sae_report$START_DATE)
onset <- as.numeric(stdt - sae_report$TRT_START) + 1
sae_report$onset_day <- ifelse(is.na(onset), "", as.character(onset))

# put(AGE, best.) then strip
best_fmt <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), ".", formatC(x, digits = 12, format = "g"))
}
sae_report$demog <- mapply(function(...) {
  v <- c(...)
  v <- v[v != ""]
  paste(v, collapse = "/")
}, trimws(best_fmt(sae_report$AGE)), trimws(sae_report$SEX),
   trimws(substr(sae_report$RACE, 1, 1)), USE.NAMES = FALSE)

# GRADE displayed as character
if (is.numeric(sae_report$GRADE)) {
  sae_report$GRADE <- ifelse(is.na(sae_report$GRADE), "", formatC(sae_report$GRADE, digits = 12, format = "g"))
} else {
  sae_report$GRADE <- chr(sae_report$GRADE)
}

# Output dataset (full values)
ds_out <- sae_report %>%
  select(USUBJID, TRTA, AGE, SEX, RACE, TRT_START, SOC, PT, VERBATIM,
         START_DATE, END_DATE, GRADE, RELATIONSHIP, ACTION_TAKEN, OUTCOME,
         AESDTH, AESLIFE, AESHOSP, AESDISAB, AESCONG, AESMIE,
         sae_criteria, onset_day, demog)
ds_out$TRT_START <- format(ds_out$TRT_START, "%Y-%m-%d")
write.csv(ds_out, file.path(out, "sae_report.csv"), row.names = FALSE, na = "")

#--- Step 3: PROC REPORT listing ---
# order variables: PROC REPORT sorts by TRTA, USUBJID (stable for the rest)
rep_df <- sae_report[sas_order(sae_report$TRTA, sae_report$USUBJID), ]
cols <- c("TRTA", "USUBJID", "demog", "SOC", "PT", "START_DATE", "END_DATE",
          "onset_day", "GRADE", "RELATIONSHIP", "ACTION_TAKEN", "OUTCOME", "sae_criteria")
hdrs <- c("Treatment", "Subject ID", "Age/Sex/Race", "SOC", "Preferred Term",
          "Start Date", "End Date", "Study Day", "Grade", "Relationship",
          "Action Taken", "Outcome", "SAE Criteria")
body <- rep_df[, cols, drop = FALSE]
for (cc in cols) body[[cc]] <- chr(body[[cc]])

# Blank repeated values of order variables (as displayed by PROC REPORT)
disp <- body
if (nrow(disp) > 1) {
  trt_prev <- c(NA, body$TRTA[-nrow(body)])
  usu_prev <- c(NA, body$USUBJID[-nrow(body)])
  same_trt <- !is.na(trt_prev) & body$TRTA == trt_prev
  same_usu <- same_trt & !is.na(usu_prev) & body$USUBJID == usu_prev
  disp$TRTA[same_trt] <- ""
  disp$USUBJID[same_usu] <- ""
}

write.csv(setNames(disp, cols), file.path(out, "l_sae.csv"), row.names = FALSE, na = "")

# Plain-text rendering, one page per treatment
titles <- c("[Sponsor Name]", "Protocol: [Protocol Number]", "",
            "Listing 16.2.7.2", "Listing of Serious Adverse Events",
            "Safety Analysis Set", "")
foot <- c("", "Data cutoff: [DDMONYYYY]", "Source: ADaM datasets",
          "Program: P079_l_sae.sas")

all_txt <- rbind(hdrs, as.matrix(disp))
widths <- apply(all_txt, 2, function(z) max(nchar(z), na.rm = TRUE))
fmt_row <- function(v) paste(formatC(v, width = -1 * 1, flag = "-") |>
                               mapply(FUN = function(s, w) formatC(s, width = w, flag = "-"), widths),
                             collapse = "  ")
lines <- character(0)
trts <- unique(body$TRTA)
if (length(trts) == 0) trts <- character(0)
for (t in trts) {
  idx <- which(body$TRTA == t)
  lines <- c(lines, titles, fmt_row(hdrs),
             paste(rep("-", sum(widths) + 2 * (length(widths) - 1)), collapse = ""))
  for (i in idx) lines <- c(lines, fmt_row(unlist(disp[i, ])))
  lines <- c(lines, foot, "\f")
}
if (length(lines) == 0) lines <- c(titles, fmt_row(hdrs), "No serious adverse events.", foot)
writeLines(lines, file.path(out, "l_sae.txt"))

cat("NOTE: l_sae completed successfully. Rows:", nrow(body), "\n")
```