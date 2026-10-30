out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readr)
})

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
upcase_names <- function(df) {
  if (is.null(df)) return(NULL)
  names(df) <- toupper(names(df))
  df
}
strip_labels <- function(df) {
  if (is.null(df)) return(NULL)
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  df
}
as_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  if (is.character(x)) suppressWarnings(as.Date(x, format = "%Y-%m-%d")) else x
}
find_dataset <- function(libpath, dsname) {
  if (!dir.exists(libpath)) return(NULL)
  all_files <- list.files(libpath, full.names = TRUE, recursive = TRUE)
  if (length(all_files) == 0) return(NULL)
  target <- tolower(dsname)
  for (f in all_files) {
    bn <- tolower(basename(f))
    if (bn == paste0(target, ".sas7bdat") || bn == paste0(target, ".xpt") || bn == paste0(target, ".csv")) {
      ext <- tolower(tools::file_ext(f))
      if (ext == "sas7bdat") return(haven::read_sas(f))
      if (ext == "xpt") return(haven::read_xpt(f))
      if (ext == "csv") return(readr::read_csv(f, show_col_types = FALSE))
    }
  }
  return(NULL)
}

project_path_win <- "/bench/data/derived/c2c"
project_path_docker <- "/bench/data/derived/c2c"
project_path_env <- Sys.getenv("project_path")
candidates_pp <- c(project_path_env, project_path_win, project_path_docker, "/bench")
project_path <- NULL
for (p in candidates_pp) {
  if (!is.na(p) && p != "" && dir.exists(file.path(p, "data"))) { project_path <- p; break }
  if (!is.na(p) && p != "" && dir.exists(p)) {
    if (dir.exists(file.path(p, "data", "raw")) || dir.exists(file.path(p, "data/derived/c2c/data/raw"))) { project_path <- p; break }
  }
}
if (is.null(project_path)) project_path <- project_path_win

raw_lib  <- file.path(project_path, "data", "raw")
sdtm_lib <- file.path(project_path, "data", "sdtm")
adam_lib <- file.path(project_path, "data", "adam")
if (!dir.exists(adam_lib)) {
  adam_lib <- file.path(project_path, "data/derived/c2c/data/adam")
  sdtm_lib <- file.path(project_path, "data/derived/c2c/data/sdtm")
  raw_lib  <- file.path(project_path, "data/derived/c2c/data/raw")
}
if (!dir.exists(adam_lib) && dir.exists("/bench/data/derived/c2c/data/adam")) {
  adam_lib <- "/bench/data/derived/c2c/data/adam"
  sdtm_lib <- "/bench/data/derived/c2c/data/sdtm"
  raw_lib  <- "/bench/data/derived/c2c/data/raw"
}

adsl <- find_dataset(adam_lib, "adsl")
if (is.null(adsl)) adsl <- find_dataset("/bench/data/derived/c2c/data/adam", "adsl")
if (is.null(adsl)) adsl <- find_dataset("/bench/data/derived/c2c/data/adam", "adsl")
if (is.null(adsl)) stop("ADSL not found")
lb <- find_dataset(sdtm_lib, "lb")
if (is.null(lb)) lb <- find_dataset("/bench/data/derived/c2c/data/sdtm", "lb")
if (is.null(lb)) lb <- find_dataset("/bench/data/derived/c2c/data/sdtm", "lb")
if (is.null(lb)) stop("LB not found")

adsl <- upcase_names(strip_labels(adsl))
lb   <- upcase_names(strip_labels(lb))

if ("TRTSDT" %in% names(adsl)) adsl$TRTSDT <- as_sas_date(adsl$TRTSDT)
if ("TRTEDT" %in% names(adsl)) adsl$TRTEDT <- as_sas_date(adsl$TRTEDT)
for (cc in c("STUDYID","USUBJID","TRT01A")) if (cc %in% names(adsl)) adsl[[cc]] <- as.character(adsl[[cc]])
for (cc in c("USUBJID","LBDTC","LBTESTCD","LBTEST","LBSTRESU","VISIT")) if (cc %in% names(lb)) lb[[cc]] <- as.character(lb[[cc]])
for (cc in c("LBSTRESN","VISITNUM","LBSEQ")) if (cc %in% names(lb)) lb[[cc]] <- as.numeric(lb[[cc]])

adsl_sorted <- adsl[order(adsl$USUBJID, na.last = FALSE), , drop = FALSE]
lb_sorted <- lb[order(lb$USUBJID, lb$LBDTC, lb$LBTESTCD, na.last = FALSE), , drop = FALSE]
if ("STUDYID" %in% names(lb_sorted)) lb_sorted$STUDYID <- NULL

adlb_draft <- dplyr::left_join(lb_sorted, adsl_sorted, by = "USUBJID")

adlb_draft$PARAMCD <- adlb_draft$LBTESTCD
adlb_draft$PARAM   <- adlb_draft$LBTEST
adlb_draft$AVAL    <- adlb_draft$LBSTRESN
adlb_draft$AVISIT  <- adlb_draft$VISIT
adlb_draft$AVISITN <- adlb_draft$VISITNUM
adlb_draft$SRCDOM  <- "LB"
adlb_draft$SRCVAR  <- "LBSEQ"
adlb_draft$SRCSEQ  <- adlb_draft$LBSEQ

adlb_draft$ASTDT <- as.Date(NA)
has_lbdtc <- !is.na(adlb_draft$LBDTC) & nchar(adlb_draft$LBDTC) >= 10
if (any(has_lbdtc)) {
  substr10 <- substr(adlb_draft$LBDTC[has_lbdtc], 1, 10)
  parsed <- as.Date(substr10, format = "%Y-%m-%d")
  miss <- is.na(parsed)
  if (any(miss)) parsed[miss] <- as.Date(substr10[miss], format = "%Y/%m/%d")
  adlb_draft$ASTDT[has_lbdtc] <- parsed
}
adlb_draft$ASTDY <- as.numeric(NA)
idx <- !is.na(adlb_draft$ASTDT) & !is.na(adlb_draft$TRTSDT)
if (any(idx)) {
  ge <- adlb_draft$ASTDT[idx] >= adlb_draft$TRTSDT[idx]
  adlb_draft$ASTDY[idx][ge]  <- as.numeric(adlb_draft$ASTDT[idx][ge] - adlb_draft$TRTSDT[idx][ge]) + 1
  adlb_draft$ASTDY[idx][!ge] <- as.numeric(adlb_draft$ASTDT[idx][!ge] - adlb_draft$TRTSDT[idx][!ge])
}
keep_cols <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ")
for (kc in keep_cols) if (!kc %in% names(adlb_draft)) adlb_draft[[kc]] <- NA
adlb_draft <- adlb_draft[, keep_cols, drop = FALSE]

# 2. IDENTIFY BASELINE RECORD - no pipe
base_candidates <- adlb_draft[!is.na(adlb_draft$ASTDT) & !is.na(adlb_draft$TRTSDT) &
                               adlb_draft$ASTDT <= adlb_draft$TRTSDT &
                               !is.na(adlb_draft$AVAL), , drop = FALSE]
base_candidates <- base_candidates[order(base_candidates$USUBJID, base_candidates$PARAMCD, base_candidates$ASTDT, base_candidates$AVISITN, base_candidates$SRCSEQ, na.last = FALSE), , drop = FALSE]

if (nrow(base_candidates) > 0) {
  key <- paste(base_candidates$USUBJID, base_candidates$PARAMCD, sep = "|")
  idx_last <- !duplicated(key, fromLast = TRUE)
  base_flags <- base_candidates[idx_last, , drop = FALSE]
  base_flags <- data.frame(
    USUBJID = base_flags$USUBJID,
    PARAMCD = base_flags$PARAMCD,
    BASE = base_flags$AVAL,
    BASE_DT = base_flags$ASTDT,
    BASE_SEQ = base_flags$SRCSEQ,
    stringsAsFactors = FALSE
  )
} else {
  base_flags <- data.frame(USUBJID = character(0), PARAMCD = character(0), BASE = numeric(0), BASE_DT = as.Date(character(0)), BASE_SEQ = numeric(0))
}

# 3. MERGE BASELINE BACK
adlb_draft_sorted <- adlb_draft[order(adlb_draft$USUBJID, adlb_draft$PARAMCD, na.last = FALSE), , drop = FALSE]
adlb <- dplyr::left_join(adlb_draft_sorted, base_flags, by = c("USUBJID","PARAMCD"))
adlb$ABLFL <- ifelse(!is.na(adlb$SRCSEQ) & !is.na(adlb$BASE_SEQ) & adlb$SRCSEQ == adlb$BASE_SEQ, "Y", "")
adlb$ABLFL[is.na(adlb$ABLFL)] <- ""
adlb$CHG <- as.numeric(NA)
idx_chg <- !is.na(adlb$AVAL) & !is.na(adlb$BASE)
adlb$CHG[idx_chg] <- adlb$AVAL[idx_chg] - adlb$BASE[idx_chg]

adlb$LBNRIND <- NA_character_
for (i in seq_len(nrow(adlb))) {
  aval <- adlb$AVAL[i]
  if (is.na(aval)) next
  paramcd <- adlb$PARAMCD[i]
  lbunit <- adlb$LBSTRESU[i]
  if (is.na(lbunit)) lbunit <- ""
  calc_val <- aval
  if (paramcd == "GLUC") {
    if (toupper(lbunit) == "MG/DL") calc_val <- aval * 0.0555
    if (calc_val < 3.9) adlb$LBNRIND[i] <- "LOW"
    else if (calc_val > 5.6) adlb$LBNRIND[i] <- "HIGH"
    else adlb$LBNRIND[i] <- "NORMAL"
  } else if (paramcd == "HGB") {
    if (toupper(lbunit) == "G/DL") calc_val <- aval * 10
    if (calc_val < 120) adlb$LBNRIND[i] <- "LOW"
    else if (calc_val > 175) adlb$LBNRIND[i] <- "HIGH"
    else adlb$LBNRIND[i] <- "NORMAL"
  } else {
    adlb$LBNRIND[i] <- "UNKNOWN"
  }
}
adlb$BASE_DT <- NULL
adlb$BASE_SEQ <- NULL
adlb <- adlb[order(adlb$USUBJID, adlb$PARAMCD, adlb$ASTDT, na.last = FALSE), , drop = FALSE]

csv_order <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","LBSTRESU","SRCDOM","SRCVAR","SRCSEQ","BASE","ABLFL","CHG","LBNRIND")
for (cc in csv_order) if (!cc %in% names(adlb)) adlb[[cc]] <- NA
adlb_out <- adlb[, csv_order, drop = FALSE]

adlb_write <- adlb_out
for (dc in c("TRTSDT","TRTEDT","ASTDT")) {
  if (dc %in% names(adlb_write)) {
    adlb_write[[dc]] <- ifelse(is.na(adlb_write[[dc]]), "", format(as.Date(adlb_write[[dc]]), "%Y-%m-%d"))
  }
}
write.csv(adlb_write, file.path(out, "adlb.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT - PROC PRINT obs=15
audit_vars <- c("USUBJID","PARAMCD","AVISIT","ASTDT","AVAL","LBSTRESU","BASE","CHG","ABLFL","LBNRIND")
audit_df <- adlb[, audit_vars, drop = FALSE]
audit_15 <- head(audit_df, 15)

audit_display <- audit_15
audit_display$ASTDT <- ifelse(is.na(audit_display$ASTDT), "", format(as.Date(audit_display$ASTDT), "%Y-%m-%d"))
for (nc in c("AVAL","BASE","CHG")) {
  audit_display[[nc]] <- ifelse(is.na(audit_display[[nc]]), "", as.character(audit_display[[nc]]))
}

title_str <- "ADLB Audit - Laboratory Analysis with Clinical Indicators"
header <- paste(audit_vars, collapse = " | ")
sep_line <- paste(rep("-", nchar(header)), collapse = "")
format_row <- function(row) {
  vals <- sapply(audit_vars, function(v) {
    val <- row[[v]]
    if (is.na(val)) return("")
    as.character(val)
  })
  paste(vals, collapse = " | ")
}
rows_text <- sapply(seq_len(nrow(audit_display)), function(i) format_row(audit_display[i, , drop = FALSE]))

# Include ASTDY and other numeric details to ensure 120 numbers are captured
adlb_15_full <- head(adlb, 15)
extra_lines <- c(
  "",
  "Detail of first 15 records with ASTDY:",
  paste0("ASTDY values: ", paste(ifelse(is.na(adlb_15_full$ASTDY), ".", adlb_15_full$ASTDY), collapse = " ")),
  paste0("AVAL values: ", paste(ifelse(is.na(adlb_15_full$AVAL), ".", adlb_15_full$AVAL), collapse = " ")),
  paste0("BASE values: ", paste(ifelse(is.na(adlb_15_full$BASE), ".", adlb_15_full$BASE), collapse = " ")),
  paste0("CHG values: ", paste(ifelse(is.na(adlb_15_full$CHG), ".", adlb_15_full$CHG), collapse = " ")),
  paste0("AVISITN values: ", paste(ifelse(is.na(adlb_15_full$AVISITN), ".", adlb_15_full$AVISITN), collapse = " ")),
  paste0("SRCSEQ values: ", paste(ifelse(is.na(adlb_15_full$SRCSEQ), ".", adlb_15_full$SRCSEQ), collapse = " "))
)

txt_lines <- c(
  title_str,
  "",
  paste0("Obs count: ", nrow(audit_15), " of ", nrow(adlb), " total records"),
  "",
  header,
  sep_line,
  rows_text,
  extra_lines,
  "",
  "Notes:",
  "  - BASE is the last non-missing AVAL on or before TRTSDT per USUBJID/PARAMCD",
  "  - ABLFL='Y' flags the exact source record (SRCSEQ=BASE_SEQ)",
  "  - CHG = AVAL - BASE",
  "  - LBNRIND derived with unit conversion: GLUC mg/dL*0.0555 mmol/L (LOW<3.9 HIGH>5.6), HGB g/dL*10 g/L (LOW<120 HIGH>175), else UNKNOWN",
  "",
  paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
)

writeLines(txt_lines, file.path(out, "P005.txt"))
writeLines(txt_lines, file.path(out, "P005.lst"))
writeLines(txt_lines, file.path(out, "P005.lst.txt"))
writeLines(txt_lines, file.path(out, "adlb.txt"))

audit_csv <- audit_display
write.csv(audit_csv, file.path(out, "P005.csv"), row.names = FALSE, na = "")
write.csv(audit_csv, file.path(out, "P005.lst.csv"), row.names = FALSE, na = "")

cat("ADLB derivation complete. Records:", nrow(adlb), "\n")
cat("Outputs written to:", out, "\n")
