```r
# P079 l_sae.sas -> R translation
# Listing 16.2.7.2 Listing of Serious Adverse Events
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

catx <- function(delim, ...) {
  items <- list(...)
  # allow single vector
  if (length(items) == 1 && length(items[[1]]) > 1 && is.character(items[[1]])) {
    items <- as.list(items[[1]])
  }
  # flatten
  vals <- unlist(items, use.names = FALSE)
  vals <- as.character(vals)
  vals <- trimws(vals)
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return("")
  paste(vals, collapse = delim)
}

find_file <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  # match basename without extension case-insensitive
  base <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base == tolower(name))
  if (length(idx) == 0) {
    # try pattern
    idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  }
  if (length(idx) == 0) stop(paste0("dataset not found: ", name, " in ", dir))
  files[idx[1]]
}

read_any <- function(path) {
  lp <- tolower(path)
  if (grepl("\\.xpt$", lp)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.sas7bdat$", lp)) {
    df <- haven::read_sas(path)
  } else {
    # try both readers
    try1 <- try(haven::read_sas(path), silent = TRUE)
    if (!inherits(try1, "try-error")) df <- try1
    else df <- haven::read_xpt(path)
  }
  as.data.frame(df, stringsAsFactors = FALSE)
}

zap_df <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      x <- haven::zap_labels(x)
    }
    if (inherits(x, "haven_labelled") || !is.null(attr(x, "format.sas"))) {
      x <- haven::zap_formats(x)
    }
    # haven Date/POSIX stays; labelled numeric -> numeric
    if (inherits(x, "haven_labelled")) x <- as.vector(x)
    # convert factors to character (SAS chars)
    if (is.factor(x)) x <- as.character(x)
    df[[nm]] <- x
  }
  df
}

adae_path <- find_file(adam_dir, "adae")
adsl_path <- find_file(adam_dir, "adsl")

adae0 <- zap_df(read_any(adae_path))
adsl0 <- zap_df(read_any(adsl_path))

names(adae0) <- toupper(names(adae0))
names(adsl0) <- toupper(names(adsl0))

# stopifnot columns exist
stopifnot("USUBJID" %in% names(adae0))
stopifnot("USUBJID" %in% names(adsl0))
stopifnot("AESER" %in% names(adae0))
stopifnot("SAFFL" %in% names(adsl0))
stopifnot("TRTA" %in% names(adsl0))
stopifnot("TRTSDT" %in% names(adsl0))
stopifnot("AEBODSYS" %in% names(adae0))
stopifnot("AEDECOD" %in% names(adae0))

# Select needed columns, drop/rename overlapping except keys to avoid .x/.y
# Keys: USUBJID
keep_ae <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC",
             "AETOXGR","AEREL","AEACN","AEOUT",
             "AESDTH","AESLIFE","AESHOSP","AESDISAB","AESCONG","AESMIE","AESER")
keep_ae <- keep_ae[keep_ae %in% names(adae0)]
keep_sl <- c("USUBJID","TRTA","AGE","SEX","RACE","TRTSDT","SAFFL")
keep_sl <- keep_sl[keep_sl %in% names(adsl0)]

a_sub <- adae0[, keep_ae, drop = FALSE]
b_sub <- adsl0[, keep_sl, drop = FALSE]

# Ensure character columns are character, NA stays NA
char_cols_a <- c("USUBJID","AEBODSYS","AEDECOD","AETERM","AESTDTC","AEENDTC",
                 "AETOXGR","AEREL","AEACN","AEOUT",
                 "AESDTH","AESLIFE","AESHOSP","AESDISAB","AESCONG","AESMIE","AESER")
for (cn in intersect(char_cols_a, names(a_sub))) {
  v <- a_sub[[cn]]
  if (inherits(v, "Date")) v <- format(v, "%Y-%m-%d")
  else if (inherits(v, "POSIXt")) v <- format(v, "%Y-%m-%d")
  else if (is.numeric(v)) v <- as.character(v)
  else v <- as.character(v)
  a_sub[[cn]] <- v
}
char_cols_b <- c("USUBJID","TRTA","SEX","RACE","SAFFL")
for (cn in intersect(char_cols_b, names(b_sub))) {
  v <- b_sub[[cn]]
  if (inherits(v, "Date")) v <- format(v, "%Y-%m-%d")
  else if (inherits(v, "POSIXt")) v <- format(v, "%Y-%m-%d")
  else if (is.numeric(v) && cn != "AGE") v <- as.character(v)
  else v <- as.character(v)
  b_sub[[cn]] <- v
}
# SEX/RACE/AGE handling: AGE numeric keep numeric
if ("AGE" %in% names(b_sub)) {
  if (is.character(b_sub$AGE)) b_sub$AGE <- suppressWarnings(as.numeric(b_sub$AGE))
}
# TRTSDT: SAS numeric date or Date or character ISO -> convert to Date TRT_START
trt_raw <- b_sub$TRTSDT
if (inherits(trt_raw, "Date")) {
  TRT_START_vec <- trt_raw
} else if (inherits(trt_raw, "POSIXt")) {
  TRT_START_vec <- as.Date(trt_raw)
} else if (is.numeric(trt_raw)) {
  TRT_START_vec <- as.Date(trt_raw, origin = "1960-01-01")
} else {
  # character: ONE strict format YYYY-MM-DD
  trt_char <- as.character(trt_raw)
  TRT_START_vec <- as.Date(substr(trimws(trt_char), 1, 10), format = "%Y-%m-%d")
}
b_sub$TRT_START <- TRT_START_vec

# Join inner on USUBJID
merged <- dplyr::inner_join(a_sub, b_sub, by = "USUBJID")

stopifnot("SAFFL" %in% names(merged))
stopifnot("AESER" %in% names(merged))

# Filter SAFFL='Y' and AESER='Y' case-sensitive; NA fails
merged <- merged[!is.na(merged$SAFFL) & merged$SAFFL == "Y" &
                 !is.na(merged$AESER) & merged$AESER == "Y", , drop = FALSE]

# Build sae_list equivalent
# Rename to report names
# a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, TRT_START, SOC=AEBODSYS, PT=AEDECOD,
# VERBATIM=AETERM, START_DATE=AESTDTC, END_DATE=AEENDTC, GRADE=AETOXGR,
# RELATIONSHIP=AEREL, ACTION_TAKEN=AEACN, OUTCOME=AEOUT, AESDTH etc.
sae_list <- data.frame(
  USUBJID = as.character(merged$USUBJID),
  TRTA = as.character(merged$TRTA),
  AGE = suppressWarnings(as.numeric(merged$AGE)),
  SEX = as.character(merged$SEX),
  RACE = as.character(merged$RACE),
  TRT_START = merged$TRT_START,
  SOC = as.character(merged$AEBODSYS),
  PT = as.character(merged$AEDECOD),
  VERBATIM = as.character(merged$AETERM),
  START_DATE = as.character(merged$AESTDTC),
  END_DATE = as.character(merged$AEENDTC),
  GRADE = as.character(merged$AETOXGR),
  RELATIONSHIP = as.character(merged$AEREL),
  ACTION_TAKEN = as.character(merged$AEACN),
  OUTCOME = as.character(merged$AEOUT),
  AESDTH = as.character(merged$AESDTH),
  AESLIFE = as.character(merged$AESLIFE),
  AESHOSP = as.character(merged$AESHOSP),
  AESDISAB = as.character(merged$AESDISAB),
  AESCONG = as.character(merged$AESCONG),
  AESMIE = as.character(merged$AESMIE),
  stringsAsFactors = FALSE
)
# Normalize NA strings: haven may give NA; keep NA; also "" stays ""
# For character missing SAS is blank; convert NA to "" for display later but keep NA for logic?
# Keep as NA for now; catx handles both.

# SAS order by TRTA, USUBJID, AESTDTC with SAS collation (uppercase before lowercase, missing first)
sas_key <- function(v) {
  sapply(v, function(s) {
    if (is.na(s) || s == "") return("")
    # byte-wise code
    codes <- utf8ToInt(s)
    paste(sprintf("%03d", codes), collapse = " ")
  }, USE.NAMES = FALSE)
}
# Replace NA with "" for ordering keys
o_TRTA <- sas_key(ifelse(is.na(sae_list$TRTA), "", sae_list$TRTA))
o_USUB <- sas_key(ifelse(is.na(sae_list$USUBJID), "", sae_list$USUBJID))
o_DT <- sas_key(ifelse(is.na(sae_list$START_DATE), "", sae_list$START_DATE))
ord <- order(o_TRTA, o_USUB, o_DT, na.last = FALSE)
sae_list <- sae_list[ord, , drop = FALSE]

# Step 2: Derive sae_criteria, onset_day, demog
n <- nrow(sae_list)
sae_criteria <- character(n)
onset_day <- character(n)
demog <- character(n)

# helper best format for AGE (SAS BEST.)
best_fmt <- function(x) {
  if (is.na(x)) return("")
  if (!is.finite(x)) return("")
  # SAS BEST. for typical ages: integers without decimal
  if (abs(x - round(x)) < 1e-9) {
    sprintf("%d", as.integer(round(x)))
  } else {
    # use default with stripping, up to 12 significant digits like BEST12.
    s <- format(x, scientific = FALSE, trim = TRUE, digits = 12, nsmall = 0)
    trimws(s)
  }
}

for (i in seq_len(n)) {
  # sae_criteria via CATX
  parts <- c()
  if (!is.na(sae_list$AESDTH[i]) && sae_list$AESDTH[i] == "Y") parts <- c(parts, "Death")
  if (!is.na(sae_list$AESLIFE[i]) && sae_list$AESLIFE[i] == "Y") parts <- c(parts, "Life-threatening")
  if (!is.na(sae_list$AESHOSP[i]) && sae_list$AESHOSP[i] == "Y") parts <- c(parts, "Hospitalization")
  if (!is.na(sae_list$AESDISAB[i]) && sae_list$AESDISAB[i] == "Y") parts <- c(parts, "Disability")
  if (!is.na(sae_list$AESCONG[i]) && sae_list$AESCONG[i] == "Y") parts <- c(parts, "Congenital anomaly")
  if (!is.na(sae_list$AESMIE[i]) && sae_list$AESMIE[i] == "Y") parts <- c(parts, "Medically important")
  if (length(parts) == 0) sae_criteria[i] <- ""
  else sae_criteria[i] <- paste(parts, collapse = ", ")

  # onset_day: if TRT_START ne . and input(START_DATE, ??yymmdd10.) ne .
  trt <- sae_list$TRT_START[i]
  sdt_raw <- sae_list$START_DATE[i]
  sdt10 <- if (is.na(sdt_raw) || trimws(sdt_raw) == "") "" else substr(trimws(sdt_raw), 1, 10)
  sdt <- suppressWarnings(as.Date(sdt10, format = "%Y-%m-%d"))
  # check TRT_START not missing and sdt not missing (NA)
  if (!is.na(trt) && !is.na(sdt)) {
    onset <- as.integer(sdt - trt + 1L)
    # SAS put 8. then strip
    onset_day[i] <- trimws(as.character(onset))
  } else {
    onset_day[i] <- ""
  }

  # demog = catx('/', strip(put(AGE,best.)), strip(SEX), strip(substr(RACE,1,1)))
  age_s <- best_fmt(sae_list$AGE[i])
  sex_s <- if (is.na(sae_list$SEX[i])) "" else trimws(as.character(sae_list$SEX[i]))
  race_raw <- if (is.na(sae_list$RACE[i])) "" else as.character(sae_list$RACE[i])
  race_s <- if (race_raw == "") "" else substr(race_raw, 1, 1)
  race_s <- trimws(race_s)
  demog[i] <- catx("/", age_s, sex_s, race_s)
}

sae_report <- cbind(sae_list, sae_criteria = sae_criteria, onset_day = onset_day, demog = demog, stringsAsFactors = FALSE)

# Re-order after derive? SAS data step preserves order from SQL; keep order.

# Columns for PROC REPORT
rep_cols <- c("TRTA","USUBJID","demog","SOC","PT","START_DATE","END_DATE",
              "onset_day","GRADE","RELATIONSHIP","ACTION_TAKEN","OUTCOME","sae_criteria")
# Ensure all exist
for (cc in rep_cols) if (!cc %in% names(sae_report)) sae_report[[cc]] <- ""
report_df <- sae_report[, rep_cols, drop = FALSE]

# Normalize display values: NA -> ""
for (cc in rep_cols) {
  v <- report_df[[cc]]
  v[is.na(v)] <- ""
  v <- trimws(as.character(v))
  # keep "" for missing
  v[is.na(v)] <- ""
  report_df[[cc]] <- v
}
# START_DATE/END_DATE: keep as stored (character ISO). Ensure ISO dates YYYY-MM-DD where possible?
# They are already AESTDTC/AEENDTC strings; leave as is (SAS displays raw).
# For CSV, dates as YYYY-MM-DD: if datetime with T, keep full? SAS displays full string.
# Keep as is.

# Write CSV of table body (full values, no ORDER blanking)
csv_df <- report_df
# Ensure TRT_START not included; dates already ISO
write.csv(csv_df, file.path(out, "l_sae.csv"), row.names = FALSE, na = "")

# Build text rendering with ORDER suppression and break after TRTA / page
# Titles/footnotes verbatim
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
run_time <- format(Sys.time(), "%H:%M")

title4 <- "Listing 16.2.7.2"
title5 <- "Listing of Serious Adverse Events"
title6 <- "Safety Analysis Set"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: l_sae.sas | Run: ", run_date, " ", run_time)

# Column headers with split '~' -> replace ~ with space for single-line text
headers <- c(
  TRTA = "Treatment",
  USUBJID = "Subject ID",
  demog = "Age/Sex/ Race",
  SOC = "SOC",
  PT = "Preferred Term",
  START_DATE = "Start Date",
  END_DATE = "End Date",
  onset_day = "Study Day",
  GRADE = "Grade",
  RELATIONSHIP = "Relation- ship",
  ACTION_TAKEN = "Action Taken",
  OUTCOME = "Outcome",
  sae_criteria = "SAE Criteria"
)

# Display dataframe with ORDER blanking for TRTA and USUBJID
disp_df <- report_df
# ORDER variables: TRTA, USUBJID
# Blank repeats: TRTA only first row per TRTA group; USUBJID only first row per USUBJID within TRTA
prev_trta <- NA_character_
prev_usub <- NA_character_
prev_trta_grp <- NA_character_
for (i in seq_len(nrow(disp_df))) {
  cur_trta <- report_df$TRTA[i]
  cur_usub <- report_df$USUBJID[i]
  # TRTA: show only if different from previous row overall (since ordered by TRTA)
  if (i == 1 || cur_trta != prev_trta) {
    # show
  } else {
    disp_df$TRTA[i] <- ""
  }
  # USUBJID: show only if different from previous row OR TRTA changed
  if (i == 1 || cur_usub != prev_usub || cur_trta != prev_trta_grp) {
    # show
  } else {
    disp_df$USUBJID[i] <- ""
  }
  prev_trta <- cur_trta
  prev_usub <- cur_usub
  prev_trta_grp <- cur_trta
}

# Pagination: break after TRTA / page -> each TRTA new page
trta_vals <- unique(report_df$TRTA)  # already in SAS order
if (length(trta_vals) == 0 || (length(trta_vals) == 1 && trta_vals[1] == "" && nrow(report_df) == 0)) {
  npages <- 1L
  pages <- list(integer(0))
} else {
  npages <- length(trta_vals)
  pages <- lapply(trta_vals, function(t) which(report_df$TRTA == t))
}

txt_path <- file.path(out, "l_sae.txt")
con <- file(txt_path, open = "wt")

# Helper to write padded table: use " | " separator
write_page <- function(pageno, total_pages, idx) {
  pageof <- paste0("Page ", pageno, " of ", total_pages)
  # Title1: sponsor left, page right
  # Fixed width 132 for landscape
  total_w <- 132
  t1_left <- sponsor
  t1_right <- pageof
  spaces <- total_w - nchar(t1_left) - nchar(t1_right)
  if (spaces < 2) spaces <- 2
  line1 <- paste0(t1_left, paste(rep(" ", spaces), collapse = ""), t1_right)
  cat(line1, "\n", sep = "", file = con)
  cat(paste0("Protocol: ", protocol), "\n", sep = "", file = con)
  cat(" \n", sep = "", file = con)
  cat(title4, "\n", sep = "", file = con)
  cat(title5, "\n", sep = "", file = con)
  cat(title6, "\n", sep = "", file = con)
  cat("\n", file = con)
  # Header row
  hdr <- paste(headers[rep_cols], collapse = " | ")
  cat(hdr, "\n", sep = "", file = con)
  sep_line <- paste(rep("-", min(nchar(hdr), total_w)), collapse = "")
  cat(sep_line, "\n", sep = "", file = con)
  if (length(idx) > 0) {
    for (r in idx) {
      row_vals <- as.character(disp_df[r, rep_cols])
      row_vals[is.na(row_vals)] <- ""
      # Replace any "|" in data to avoid confusion
      row_vals <- gsub("\\|", "/", row_vals, fixed = FALSE)
      # Trim newlines
      row_vals <- gsub("\r|\n", " ", row_vals)
      line <- paste(row_vals, collapse = " | ")
      cat(line, "\n", sep = "", file = con)
    }
  } else {
    cat("No serious adverse events reported.\n", file = con)
  }
  cat("\n", file = con)
  cat(paste0(foot1, "\n"), file = con)
  cat(paste0(foot2, "\n"), file = con)
  cat(paste0(foot3, "\n"), file = con)
  if (pageno < total_pages) cat("\f\n", file = con)
}

if (nrow(report_df) == 0) {
  write_page(1, 1, integer(0))
} else {
  for (p in seq_along(pages)) {
    write_page(p, length(pages), pages[[p]])
  }
}
close(con)
```