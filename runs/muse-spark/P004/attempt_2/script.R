out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
adam_dir  <- paste0(project_path, "/data/adam")
sdtm_dir  <- paste0(project_path, "/data/sdtm")
raw_dir   <- paste0(project_path, "/data/raw")
rawadam_dir  <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawsdtm_dir  <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm"
rawsplit_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

sas_date9 <- function(d) {
  d <- as.Date(d, origin = "1970-01-01")
  vapply(seq_along(d), function(i) {
    if (is.na(d[i])) "." else paste0(format(d[i], "%d"), toupper(format(d[i], "%b")), format(d[i], "%Y"))
  }, character(1))
}

to_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1960-01-01"))
  if (is.character(x)) {
    suppressWarnings({
      d <- as.Date(x, format = "%Y-%m-%d")
    })
    return(d)
  }
  return(as.Date(as.character(x)))
}

find_dataset <- function(name, dirs) {
  for (dd in dirs) {
    if (is.na(dd) || dd == "") next
    if (!dir.exists(dd)) next
    f <- list.files(dd, pattern = paste0("^", name, "\\."), full.names = TRUE, ignore.case = TRUE)
    if (length(f) > 0) {
      ord <- order(match(tolower(tools::file_ext(f)), c("sas7bdat","xpt","sas7bdat","xpt","csv","xlsx","xls")))
      f <- f[ord]
      return(f[1])
    }
  }
  return(NULL)
}

read_any <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE, guess_max = 100000)
  } else if (ext %in% c("xls","xlsx")) {
    df <- readxl::read_excel(path)
  } else {
    df <- haven::read_sas(path)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    cl <- class(df[[nm]])
    if (any(cl %in% c("haven_labelled","haven_labelled_spss"))) {
      df[[nm]] <- as.vector(df[[nm]])
    }
  }
  df
}

sas_order_idx <- function(df, cols) {
  lst <- lapply(cols, function(cn) df[[cn]])
  do.call(order, c(lst, list(na.last = FALSE, method = "radix")))
}

sas_length <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) 1L else nchar(sub(" +$", "", as.character(v)), type = "chars")
  }, integer(1))
}

parse_yymmdd10 <- function(x) {
  vapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return(NA_real_)
    s <- as.character(v)
    s <- sub(" +$", "", s)
    if (nchar(s) < 10) return(NA_real_)
    sub10 <- substr(s, 1, 10)
    d <- suppressWarnings(as.Date(sub10, format = "%Y-%m-%d"))
    if (is.na(d)) return(NA_real_) else return(as.numeric(d))
  }, numeric(1))
}

cat("Reading inputs...\n")
adsl_path <- find_dataset("adsl", c(adam_dir, rawadam_dir, rawsplit_dir, raw_dir))
ae_path   <- find_dataset("ae",   c(sdtm_dir, rawsdtm_dir, raw_dir))
cat(paste0("ADSL: ", adsl_path, "\n"))
cat(paste0("AE: ", ae_path, "\n"))
if (is.null(adsl_path)) stop("ADSL dataset not found")
if (is.null(ae_path)) stop("AE dataset not found")

adsl <- read_any(adsl_path)
ae   <- read_any(ae_path)
names(adsl) <- toupper(names(adsl))
names(ae)   <- toupper(names(ae))

# 1. SORT INPUT DATASETS BY UNIQUE SUBJECT ID
adsl_sorted <- adsl[sas_order_idx(adsl, c("USUBJID")), , drop = FALSE]
ae_sorted   <- ae[sas_order_idx(ae, c("USUBJID","AESEQ")), , drop = FALSE]

# Ensure date types for ADSL treatment dates
for (cn in c("TRTSDT","TRTEDT")) {
  if (cn %in% names(adsl_sorted)) {
    if (!inherits(adsl_sorted[[cn]], "Date")) {
      if (is.numeric(adsl_sorted[[cn]])) adsl_sorted[[cn]] <- as.Date(adsl_sorted[[cn]], origin = "1960-01-01")
      else if (inherits(adsl_sorted[[cn]], "POSIXct")) adsl_sorted[[cn]] <- as.Date(adsl_sorted[[cn]])
    }
  }
}

# 2. MERGE ADSL AND AE TO CREATE ADAE (keep only if b)
adsl_keep <- intersect(c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX","SAFFL"), names(adsl_sorted))
ae_keep   <- intersect(c("STUDYID","USUBJID","AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC"), names(ae_sorted))
adsl_sub <- adsl_sorted[, adsl_keep, drop = FALSE]
ae_sub   <- ae_sorted[, ae_keep, drop = FALSE]

merged <- dplyr::left_join(ae_sub, adsl_sub, by = "USUBJID", suffix = c("", "_ADSL"))

# Resolve STUDYID when present in both (AE overwrites ADSL per SAS merge order)
if ("STUDYID_ADSL" %in% names(merged)) {
  if ("STUDYID" %in% names(merged)) {
    s1 <- as.character(merged[["STUDYID"]])
    s2 <- as.character(merged[["STUDYID_ADSL"]])
    s1[is.na(s1)] <- ""
    s2[is.na(s2)] <- ""
    merged[["STUDYID"]] <- ifelse(s1 != "", s1, s2)
    merged[["STUDYID"]][merged[["STUDYID"]] == ""] <- NA_character_
  } else {
    merged[["STUDYID"]] <- merged[["STUDYID_ADSL"]]
  }
  merged[["STUDYID_ADSL"]] <- NULL
}
# if ADSL STUDYID only via suffix? already handled. If STUDYID only in ADSL and not AE, left_join brings STUDYID_ADSL? Actually if AE has no STUDYID, then join creates STUDYID_ADSL? No, suffix only for common cols. If AE has no STUDYID, then ADSL STUDYID comes as STUDYID. Good.
# Normalize: ensure all expected columns exist
expect_all <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX","SAFFL",
                "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER","AESTDTC","AEENDTC")
for (cn in expect_all) if (!cn %in% names(merged)) merged[[cn]] <- NA

# NUMERIC DATE DERIVATIONS
aestdt_num <- parse_yymmdd10(as.character(merged[["AESTDTC"]]))
aeendt_num <- parse_yymmdd10(as.character(merged[["AEENDTC"]]))
merged[["AESTDT"]] <- as.Date(aestdt_num, origin = "1970-01-01")
merged[["AEENDT"]] <- as.Date(aeendt_num, origin = "1970-01-01")
# Ensure TRTSDT/TRTEDT are Date
merged[["TRTSDT"]] <- to_sas_date(merged[["TRTSDT"]])
merged[["TRTEDT"]] <- to_sas_date(merged[["TRTEDT"]])

# ANALYSIS RELATIVE DAYS (ASTDY)
n <- nrow(merged)
astdy <- rep(NA_real_, n)
for (i in seq_len(n)) {
  a <- merged[["AESTDT"]][i]
  t <- merged[["TRTSDT"]][i]
  if (!is.na(a) && !is.na(t)) {
    diff <- as.integer(a - t)
    if (!is.na(diff)) {
      if (a >= t) astdy[i] <- diff + 1 else astdy[i] <- diff
    }
  }
}
merged[["ASTDY"]] <- astdy

# TREATMENT-EMERGENT FLAG (TRTEMFL)
saffl <- as.character(merged[["SAFFL"]])
trtemfl <- rep("", n)
for (i in seq_len(n)) {
  s <- saffl[i]
  a <- merged[["AESTDT"]][i]
  t <- merged[["TRTSDT"]][i]
  cond <- !is.na(s) && s == "Y" && !is.na(a) && !is.na(t)
  if (isTRUE(cond)) {
    if (a >= t) trtemfl[i] <- "Y" else trtemfl[i] <- ""
  } else {
    trtemfl[i] <- ""
  }
}
merged[["TRTEMFL"]] <- trtemfl

# Keep essential clinical and analysis variables
keep_vars <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
               "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
               "AESTDT","AEENDT","ASTDY","TRTEMFL")
for (cn in keep_vars) if (!cn %in% names(merged)) merged[[cn]] <- NA
adae_draft <- merged[, keep_vars, drop = FALSE]
# retain STUDYID USUBJID order already first

# 3. DERIVE FIRST TREATMENT-EMERGENT OCCURRENCE BY DECODED TERM
adae_draft[["_occ_order"]] <- ifelse(!is.na(adae_draft[["TRTEMFL"]]) & adae_draft[["TRTEMFL"]] == "Y", 0, 1)
adae_occurrence_order <- adae_draft[sas_order_idx(adae_draft, c("USUBJID","AEDECOD","_occ_order","AESTDT","AESEQ")), , drop = FALSE]

# BY USUBJID AEDECOD _occ_order -> FIRST._occ_order
usub_key <- ifelse(is.na(adae_occurrence_order[["USUBJID"]]), "", as.character(adae_occurrence_order[["USUBJID"]]))
pt_key   <- ifelse(is.na(adae_occurrence_order[["AEDECOD"]]), "", as.character(adae_occurrence_order[["AEDECOD"]]))
occ_key  <- adae_occurrence_order[["_occ_order"]]
first_flag <- !duplicated(data.frame(usub_key, pt_key, occ_key, stringsAsFactors = FALSE))
aoccfl <- rep("", nrow(adae_occurrence_order))
is_y <- !is.na(adae_occurrence_order[["TRTEMFL"]]) & adae_occurrence_order[["TRTEMFL"]] == "Y"
aoccfl[is_y & first_flag] <- "Y"
adae_occurrence_order[["AOCCFL"]] <- aoccfl
adae_flagged <- adae_occurrence_order
adae_flagged[["_occ_order"]] <- NULL

# 4. FINALIZE AND SAVE TO ADAM LIBRARY (sort by USUBJID AESTDT AESEQ)
adae <- adae_flagged[sas_order_idx(adae_flagged, c("USUBJID","AESTDT","AESEQ")), , drop = FALSE]
final_order <- c("STUDYID","USUBJID","SUBJID","TRT01A","TRTSDT","TRTEDT","AGE","SEX",
                 "AESEQ","AETERM","AEDECOD","AESEV","AEREL","AEOUT","AESER",
                 "AESTDT","AEENDT","ASTDY","TRTEMFL","AOCCFL")
for (cn in final_order) if (!cn %in% names(adae)) adae[[cn]] <- NA
adae <- adae[, final_order, drop = FALSE]

# Prepare for CSV: format datetimes if any POSIXct
for (cn in names(adae)) {
  if (inherits(adae[[cn]], "POSIXct") || inherits(adae[[cn]], "POSIXlt")) {
    adae[[cn]] <- format(adae[[cn]], "%Y-%m-%dT%H:%M:%S")
  }
}

cat(paste0("Writing adae.csv with ", nrow(adae), " rows\n"))
write.csv(adae, file.path(out, "adae.csv"), row.names = FALSE, na = "")

# 5. VISUAL AUDIT - PROC PRINT obs=15
audit_vars <- c("USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
audit_n <- min(15, nrow(adae))
audit <- adae[seq_len(audit_n), audit_vars, drop = FALSE]
audit_with_obs <- cbind(Obs = seq_len(audit_n), audit)

# CSV of table body (ISO dates)
write.csv(audit_with_obs, file.path(out, "P004.csv"), row.names = FALSE, na = "")

# Text rendering with SAS date9. formatting
hdr <- sprintf("%-4s %-20s %-12s %-30s %-10s %-10s %-10s %-6s %-8s %-8s",
               "Obs","USUBJID","TRT01A","AETERM","AESEV","AESTDT","TRTSDT","ASTDY","TRTEMFL","AOCCFL")
lines <- c("ADAE Audit - Treatment-Emergent and First-Occurrence Flags", "", hdr)
for (i in seq_len(audit_n)) {
  r <- audit[i, , drop = FALSE]
  usubjid <- ifelse(is.na(r[["USUBJID"]]), "", as.character(r[["USUBJID"]]))
  trt01a  <- ifelse(is.na(r[["TRT01A"]]), "", as.character(r[["TRT01A"]]))
  aeterm  <- ifelse(is.na(r[["AETERM"]]), "", as.character(r[["AETERM"]]))
  aesev   <- ifelse(is.na(r[["AESEV"]]), "", as.character(r[["AESEV"]]))
  aestdt  <- sas_date9(r[["AESTDT"]])
  trtsdt  <- sas_date9(r[["TRTSDT"]])
  astdy_v <- r[["ASTDY"]]
  astdy_s <- ifelse(is.na(astdy_v), ".", as.character(as.integer(astdy_v)))
  trtemfl <- ifelse(is.na(r[["TRTEMFL"]]), "", as.character(r[["TRTEMFL"]]))
  aoccfl  <- ifelse(is.na(r[["AOCCFL"]]), "", as.character(r[["AOCCFL"]]))
  # truncate for display width like SAS (avoid overflow)
  usubjid_d <- substr(usubjid, 1, 20)
  trt01a_d  <- substr(trt01a, 1, 12)
  aeterm_d  <- substr(aeterm, 1, 30)
  aesev_d   <- substr(aesev, 1, 10)
  lines <- c(lines, sprintf("%-4d %-20s %-12s %-30s %-10s %-10s %-10s %-6s %-8s %-8s",
                            i, usubjid_d, trt01a_d, aeterm_d, aesev_d, aestdt, trtsdt, astdy_s, trtemfl, aoccfl))
}
lines <- c(lines, "", paste0("N = ", audit_n, " (first ", audit_n, " observations of ADAE, N_total = ", nrow(adae), ")"))
writeLines(lines, file.path(out, "P004.txt"))
writeLines(lines, file.path(out, "P004.lst"))
cat("Done.\n")
