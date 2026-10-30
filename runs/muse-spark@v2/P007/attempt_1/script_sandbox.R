# P007 adam_adtte.sas -> R translation
# ADTTE: Time to First Adverse Event
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "/bench/data/derived/c2c"
adam_dir <- file.path(project_path, "data/adam")

# ---- helpers ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

unwrap_haven <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    col <- df[[nm]]
    if (inherits(col, "haven_labelled") || inherits(col, "haven_labelled_spss")) {
      col <- haven::zap_formats(haven::zap_labels(col))
      # drop haven classes
      class(col) <- setdiff(class(col), c("haven_labelled", "haven_labelled_spss", "haven_labelled_defined"))
      df[[nm]] <- col
    }
    # haven Date/POSIXct keep; labelled character -> character
    if (is.list(df[[nm]])) df[[nm]] <- unlist(df[[nm]])
  }
  names(df) <- toupper(names(df))
  df
}

find_member_file <- function(libdir, mem) {
  # case-insensitive search for mem with known extensions
  if (!dir.exists(libdir)) return(NULL)
  files <- list.files(libdir, full.names = TRUE)
  # normalize
  mem_low <- tolower(mem)
  for (f in files) {
    base <- tolower(basename(f))
    # strip extension
    noext <- sub("\\.[^.]*$", "", base)
    if (noext == mem_low) return(f)
  }
  # also try exact match without extension handling
  return(NULL)
}

read_sas_member <- function(libdir, mem) {
  f <- find_member_file(libdir, mem)
  if (is.null(f)) stop(paste0("Member not found: ", libdir, "/", mem))
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat" || ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
    df <- as.data.frame(df)
  } else if (ext == "sas7bcat") {
    stop("catalog not readable")
  } else {
    # try sas then xpt
    tryCatch({ df <- haven::read_sas(f) },
      error = function(e) { df <<- haven::read_xpt(f) })
  }
  df <- unwrap_haven(df)
  df
}

# SAS date conversion: haven already gives Date for formatted vars.
# For remaining numeric SAS dates -> origin 1960-01-01; character -> single strict ISO format
to_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt") || inherits(x, "POSIXt")) {
    return(as.Date(x))
  }
  if (is.numeric(x)) {
    # numeric SAS date values: days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    x2 <- trimws(x)
    x2[x2 == "" | x2 == "."] <- NA_character_
    # ONE explicit strict format: ISO YYYY-MM-DD (ADaM standard)
    return(as.Date(x2, format = "%Y-%m-%d"))
  }
  # factor etc
  if (is.factor(x)) {
    return(as.Date(trimws(as.character(x)), format = "%Y-%m-%d"))
  }
  return(as.Date(NA))
}

cat("Reading ADAE and ADSL...\n")
adae <- read_sas_member(adam_dir, "adae")
adsl <- read_sas_member(adam_dir, "adsl")

# verify expected columns exist
stopifnot("USUBJID" %in% names(adae))
stopifnot("AESTDT" %in% names(adae))
stopifnot("TRTEMFL" %in% names(adae))
stopifnot("USUBJID" %in% names(adsl))
stopifnot("SAFFL" %in% names(adsl))

# Ensure character vars are character (strip haven)
for (cn in c("USUBJID", "TRTEMFL") ) if (cn %in% names(adae)) adae[[cn]] <- as.character(adae[[cn]])
for (cn in c("USUBJID","STUDYID","TRT01P","TRT01A","SAFFL") ) if (cn %in% names(adsl)) adsl[[cn]] <- as.character(adsl[[cn]])

# Convert date vars with single strict rule
adae$AESTDT <- to_sas_date(adae$AESTDT)
for (cn in c("TRTSDT","TRTEDT","EOSDT")) {
  if (cn %in% names(adsl)) adsl[[cn]] <- to_sas_date(adsl[[cn]])
  else adsl[[cn]] <- as.Date(NA)
}

# 1. GET FIRST ADVERSE EVENT PER SUBJECT
# proc sort data=adam.adae out=work.adae_first; by USUBJID AESTDT; where TRTEMFL='Y';
adae_first <- adae[!is.na(adae$TRTEMFL) & adae$TRTEMFL == "Y", , drop = FALSE]
stopifnot("TRTEMFL" %in% names(adae_first))
# SAS sort: character ordering, missing dates first
# order with na.last=FALSE to put missing first
ord <- order(adae_first$USUBJID, adae_first$AESTDT, na.last = FALSE)
adae_first <- adae_first[ord, , drop = FALSE]

# data work.ae_target; set work.adae_first; by USUBJID; if first.USUBJID; keep USUBJID AESTDT rename EVENTDT
# first per USUBJID in sorted order
ae_target <- adae_first[!duplicated(adae_first$USUBJID), c("USUBJID","AESTDT"), drop = FALSE]
names(ae_target)[names(ae_target) == "AESTDT"] <- "EVENTDT"

# proc sort data=adam.adsl out=work.adsl_sorted; by USUBJID;
adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]

# data adam.adtte; merge work.adsl_sorted(in=a) work.ae_target(in=b); by USUBJID; if a and SAFFL='Y';
# drop/rename overlapping cols except keys before merge (only USUBJID is key)
# ae_target has USUBJID, EVENTDT; adsl has no EVENTDT normally, but guard:
if ("EVENTDT" %in% names(adsl_sorted)) adsl_sorted$EVENTDT <- NULL
# left join adsl_sorted + ae_target
adtte_m <- merge(adsl_sorted, ae_target, by = "USUBJID", all.x = TRUE, sort = TRUE)
stopifnot("SAFFL" %in% names(adtte_m))
# if a and SAFFL='Y'  (merge is left join so all rows are a=1)
adtte_m <- adtte_m[!is.na(adtte_m$SAFFL) & adtte_m$SAFFL == "Y", , drop = FALSE]

# Ensure needed columns exist (create as NA if absent per CDISC)
for (cn in c("STUDYID","TRT01P","TRT01A","TRTSDT","TRTEDT","EOSDT")) {
  if (!cn %in% names(adtte_m)) {
    if (cn %in% c("TRTSDT","TRTEDT","EOSDT","EVENTDT")) adtte_m[[cn]] <- as.Date(NA)
    else adtte_m[[cn]] <- NA_character_
  }
}
stopifnot(all(c("STUDYID","USUBJID","TRT01P","TRT01A","TRTSDT","TRTEDT","EOSDT","EVENTDT") %in% names(adtte_m)))

# Re-ensure Date types after merge (merge can strip Date class)
adtte_m$TRTSDT <- to_sas_date(adtte_m$TRTSDT)
adtte_m$TRTEDT <- to_sas_date(adtte_m$TRTEDT)
adtte_m$EOSDT <- to_sas_date(adtte_m$EOSDT)
adtte_m$EVENTDT <- to_sas_date(adtte_m$EVENTDT)

# Derive CNSR and AVAL
n <- nrow(adtte_m)
CNSR <- rep(NA_real_, n)
AVAL <- rep(NA_real_, n)
for (i in seq_len(n)) {
  trtsdt <- adtte_m$TRTSDT[i]
  eventdt <- adtte_m$EVENTDT[i]
  eosdt <- adtte_m$EOSDT[i]
  if (is.na(trtsdt)) {
    CNSR[i] <- NA_real_
    AVAL[i] <- NA_real_
  } else if (!is.na(eventdt)) {
    CNSR[i] <- 0
    AVAL[i] <- as.numeric(eventdt - trtsdt) + 1
  } else {
    CNSR[i] <- 1
    if (!is.na(eosdt)) AVAL[i] <- as.numeric(eosdt - trtsdt) + 1
    else AVAL[i] <- NA_real_
  }
}
adtte_m$CNSR <- CNSR
adtte_m$AVAL <- AVAL
adtte_m$PARAMCD <- "TTFAEV"
adtte_m$PARAM <- "Time to First Adverse Event"

# keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT
# retain STUDYID USUBJID PARAMCD PARAM -> column order as in keep
adtte <- adtte_m[, c("STUDYID","USUBJID","TRT01P","TRT01A","TRTSDT","TRTEDT","EOSDT","PARAMCD","PARAM","CNSR","AVAL","EVENTDT")]
# already sorted by USUBJID via merge sort=TRUE
adtte <- adtte[order(adtte$USUBJID, na.last = FALSE), , drop = FALSE]
rownames(adtte) <- NULL

cat(paste0("ADTTE rows: ", nrow(adtte), "\n"))

# Write dataset adtte.csv : dates as YYYY-MM-DD, na=""
# write.csv handles Date as YYYY-MM-DD automatically
write.csv(adtte, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT : proc print obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT
# title "ADTTE Audit - Time to First Adverse Event";
print_vars <- c("USUBJID","TRT01A","PARAM","CNSR","AVAL","EVENTDT","EOSDT")
# ensure all exist
for (v in print_vars) stopifnot(v %in% names(adtte))
audit_n <- min(15L, nrow(adtte))
audit_df <- adtte[seq_len(audit_n), print_vars, drop = FALSE]
# add Obs
audit_df <- cbind(OBS = seq_len(audit_n), audit_df)

# CSV of table body (with Obs to match listing)
# Format dates ISO for CSV; numeric unrounded
audit_csv <- audit_df
# ensure Date columns stay Date for write
write.csv(audit_csv, file.path(out, "P007.csv"), row.names = FALSE, na = "")

# Plain-text rendering like PROC PRINT
fmt_date_txt <- function(d) ifelse(is.na(d), ".", format(d, "%Y-%m-%d"))
fmt_num_txt <- function(x) ifelse(is.na(x), ".", as.character(x))
fmt_char_txt <- function(x) ifelse(is.na(x), "", as.character(x))

disp <- data.frame(
  Obs = as.character(audit_df$OBS),
  USUBJID = fmt_char_txt(audit_df$USUBJID),
  TRT01A = fmt_char_txt(audit_df$TRT01A),
  PARAM = fmt_char_txt(audit_df$PARAM),
  CNSR = fmt_num_txt(audit_df$CNSR),
  AVAL = fmt_num_txt(audit_df$AVAL),
  EVENTDT = fmt_date_txt(audit_df$EVENTDT),
  EOSDT = fmt_date_txt(audit_df$EOSDT),
  stringsAsFactors = FALSE
)

# Build fixed-width text
title_txt <- "ADTTE Audit - Time to First Adverse Event"
# compute widths
cols <- names(disp)
widths <- sapply(cols, function(cn) max(nchar(c(cn, disp[[cn]])), na.rm = TRUE))
# header line
pad <- function(x, w) sprintf(paste0("%-", w, "s"), x)
padR <- function(x, w) sprintf(paste0("%", w, "s"), x)
header_line <- paste(
  padR("Obs", widths["Obs"]),
  pad("USUBJID", widths["USUBJID"]),
  pad("TRT01A", widths["TRT01A"]),
  pad("PARAM", widths["PARAM"]),
  padR("CNSR", widths["CNSR"]),
  padR("AVAL", widths["AVAL"]),
  pad("EVENTDT", widths["EVENTDT"]),
  pad("EOSDT", widths["EOSDT"]),
  sep = "  "
)
sep_line <- paste(sapply(widths, function(w) paste(rep("-", w), collapse = "")), collapse = "  ")
# data lines
data_lines <- sapply(seq_len(nrow(disp)), function(i) {
  paste(
    padR(disp$Obs[i], widths["Obs"]),
    pad(disp$USUBJID[i], widths["USUBJID"]),
    pad(disp$TRT01A[i], widths["TRT01A"]),
    pad(disp$PARAM[i], widths["PARAM"]),
    padR(disp$CNSR[i], widths["CNSR"]),
    padR(disp$AVAL[i], widths["AVAL"]),
    pad(disp$EVENTDT[i], widths["EVENTDT"]),
    pad(disp$EOSDT[i], widths["EOSDT"]),
    sep = "  "
  )
})

txt_lines <- c(
  title_txt,
  "",
  header_line,
  sep_line,
  data_lines,
  ""
)
writeLines(txt_lines, file.path(out, "P007.txt"))

cat("Done.\n")
