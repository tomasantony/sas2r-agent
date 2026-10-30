# P007 adam_adtte.sas -> R translation
# ADTTE Time to First Adverse Event
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir_primary <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam"
adam_dir_fallback <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, floor(x * 10^digits + 0.5 + 1e-9) / 10^digits)
}

read_adam <- function(dirs, name) {
  for (d in dirs) {
    if (!dir.exists(d)) next
    files <- list.files(d, full.names = TRUE)
    if (length(files) == 0) next
    pat <- paste0("^", name, "\\.(sas7bdat|xpt)$")
    idx <- grep(pat, basename(files), ignore.case = TRUE)
    if (length(idx) == 0) next
    f <- files[idx[1]]
    cat("Reading", name, "from", f, "\n")
    if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
      df <- haven::read_xpt(f)
    } else {
      df <- haven::read_sas(f)
    }
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    names(df) <- toupper(names(df))
    for (nm in names(df)) {
      col <- df[[nm]]
      if (inherits(col, "haven_labelled")) {
        # drop labels, keep values
        v <- as.vector(col)
        # preserve Date/POSIXct if applicable
        if (inherits(col, "Date")) v <- as.Date(v, origin = "1970-01-01")
        df[[nm]] <- v
      } else if (is.factor(col)) {
        df[[nm]] <- as.character(col)
      } else if (inherits(col, "hms") || inherits(col, "difftime")) {
        df[[nm]] <- as.character(col)
      }
    }
    return(df)
  }
  stop(paste0("Dataset ", name, " not found in ", paste(dirs, collapse = "; ")))
}

to_sas_date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (is.numeric(x)) {
    # SAS numeric date: days since 1960-01-01
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.character(x)) {
    xc <- trimws(as.character(x))
    xc[xc == "" | xc == "."] <- NA_character_
    # ONE strict format: ISO YYYY-MM-DD, allow datetime prefix YYYY-MM-DDTHH:MM:SS
    # take first 10 chars for datetime values, then parse YYYY-MM-DD
    x10 <- ifelse(is.na(xc), NA_character_, substr(xc, 1, 10))
    return(as.Date(x10, format = "%Y-%m-%d"))
  }
  # fallback
  return(as.Date(as.character(x), format = "%Y-%m-%d"))
}

is_char_missing <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
is_char_Y <- function(x) {
  !is.na(x) & trimws(as.character(x)) == "Y"
}

dirs <- c(adam_dir_primary, adam_dir_fallback)

adae <- read_adam(dirs, "ADAE")
adsl <- read_adam(dirs, "ADSL")

stopifnot(all(c("USUBJID", "AESTDT", "TRTEMFL") %in% names(adae)))
stopifnot(all(c("USUBJID", "SAFFL") %in% names(adsl)))

# Ensure date columns are Date with single strict conversion
# ADAE AESTDT, ADSL TRTSDT TRTEDT EOSDT
for (cn in c("AESTDT") ) {
  if (cn %in% names(adae)) adae[[cn]] <- to_sas_date(adae[[cn]])
}
for (cn in c("TRTSDT", "TRTEDT", "EOSDT")) {
  if (cn %in% names(adsl)) adsl[[cn]] <- to_sas_date(adsl[[cn]])
}

# 1. GET FIRST ADVERSE EVENT PER SUBJECT
# proc sort by USUBJID AESTDT where TRTEMFL='Y'; missing sorts first; uppercase before lowercase
adae_first_all <- adae[is_char_Y(adae$TRTEMFL), , drop = FALSE]
cat("ADAE TRTEMFL=Y rows:", nrow(adae_first_all), "\n")

# SAS sort: character ordering binary, missing first
# USUBJID missing (NA/"") first, AESTDT NA first
# Use order with na.last=FALSE; "" naturally sorts first
# To emulate SAS binary collation, order by raw bytes via radix where possible
usub_for_ord <- ifelse(is.na(adae_first_all$USUBJID), NA_character_, as.character(adae_first_all$USUBJID))
# base order
ord <- order(usub_for_ord, adae_first_all$AESTDT, na.last = FALSE, method = "radix")
adae_first <- adae_first_all[ord, , drop = FALSE]

# by USUBJID; if first.USUBJID; keep USUBJID AESTDT rename EVENTDT
# first per USUBJID in sorted order (NA USUBJID grouped first)
dup <- duplicated(adae_first$USUBJID)
# duplicated treats NA == NA? In R duplicated( c(NA,NA) ) marks second TRUE, matching SAS BY grouping of missings
ae_target <- adae_first[!dup, c("USUBJID", "AESTDT"), drop = FALSE]
names(ae_target)[names(ae_target) == "AESTDT"] <- "EVENTDT"
cat("AE target subjects:", nrow(ae_target), "\n")

# proc sort adsl by USUBJID
usub_adsl <- ifelse(is.na(adsl$USUBJID), NA_character_, as.character(adsl$USUBJID))
ord2 <- order(usub_adsl, na.last = FALSE, method = "radix")
adsl_sorted <- adsl[ord2, , drop = FALSE]

# merge adsl_sorted(in=a) ae_target(in=b) by USUBJID; drop overlapping non-keys to avoid .x/.y
# ae_target has only USUBJID + EVENTDT, no overlap except key
common <- intersect(names(adsl_sorted), names(ae_target))
common <- setdiff(common, "USUBJID")
if (length(common) > 0) {
  ae_target <- ae_target[, setdiff(names(ae_target), common), drop = FALSE]
}
# left join (keep all adsl)
merged <- merge(adsl_sorted, ae_target, by = "USUBJID", all.x = TRUE, sort = FALSE)
# merge with sort=FALSE preserves adsl_sorted order? merge reorders unless sort=FALSE keeps x order
# Re-sort by USUBJID to guarantee SAS merge order (BY USUBJID sorted)
musub <- ifelse(is.na(merged$USUBJID), NA_character_, as.character(merged$USUBJID))
merged <- merged[order(musub, na.last = FALSE, method = "radix"), , drop = FALSE]

stopifnot("SAFFL" %in% names(merged))
stopifnot("TRTSDT" %in% names(merged))

# if a and SAFFL='Y'  (a = in adsl, all rows here are a)
adtte_f <- merged[is_char_Y(merged$SAFFL), , drop = FALSE]
cat("ADSL SAFFL=Y rows:", nrow(adtte_f), "\n")

# Ensure needed columns exist; create as NA if absent (to keep KEEP list)
need_cols <- c("STUDYID", "USUBJID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT", "EVENTDT")
for (cn in need_cols) {
  if (!cn %in% names(adtte_f)) {
    if (cn %in% c("TRTSDT", "TRTEDT", "EOSDT", "EVENTDT")) adtte_f[[cn]] <- as.Date(rep(NA_character_))
    else adtte_f[[cn]] <- NA_character_
  }
}
# Ensure date types
adtte_f$TRTSDT <- to_sas_date(adtte_f$TRTSDT)
adtte_f$TRTEDT <- to_sas_date(adtte_f$TRTEDT)
adtte_f$EOSDT <- to_sas_date(adtte_f$EOSDT)
adtte_f$EVENTDT <- to_sas_date(adtte_f$EVENTDT)

n <- nrow(adtte_f)
PARAMCD <- rep("TTFAEV", n)
PARAM <- rep("Time to First Adverse Event", n)
CNSR <- rep(NA_real_, n)
AVAL <- rep(NA_real_, n)

miss_trts <- is.na(adtte_f$TRTSDT)
miss_evt <- is.na(adtte_f$EVENTDT)
miss_eos <- is.na(adtte_f$EOSDT)

idx_evt <- !miss_trts & !miss_evt
CNSR[idx_evt] <- 0
AVAL[idx_evt] <- as.numeric(adtte_f$EVENTDT[idx_evt] - adtte_f$TRTSDT[idx_evt]) + 1

idx_cens <- !miss_trts & miss_evt
CNSR[idx_cens] <- 1
idx_cens_eos <- idx_cens & !miss_eos
AVAL[idx_cens_eos] <- as.numeric(adtte_f$EOSDT[idx_cens_eos] - adtte_f$TRTSDT[idx_cens_eos]) + 1
# cens without EOS stays NA; miss_trts stays NA (already)

adtte_f$PARAMCD <- PARAMCD
adtte_f$PARAM <- PARAM
adtte_f$CNSR <- CNSR
adtte_f$AVAL <- AVAL

# keep STUDYID USUBJID TRT01P TRT01A TRTSDT TRTEDT EOSDT PARAMCD PARAM CNSR AVAL EVENTDT
keep_order <- c("STUDYID", "USUBJID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT",
                "PARAMCD", "PARAM", "CNSR", "AVAL", "EVENTDT")
# retain STUDYID USUBJID PARAMCD PARAM -> final order is keep order
adtte <- adtte_f[, keep_order, drop = FALSE]

# Ensure sorted by USUBJID (SAS merge order)
adtte <- adtte[order(ifelse(is.na(adtte$USUBJID), NA_character_, as.character(adtte$USUBJID)), na.last = FALSE, method = "radix"), , drop = FALSE]

cat("ADTTE rows:", nrow(adtte), "\n")

# Write dataset adtte.csv with ISO dates, na=""
out_ds <- adtte
# Format dates for CSV: Date -> YYYY-MM-DD, POSIXct -> YYYY-MM-DDTHH:MM:SS
for (nm in names(out_ds)) {
  col <- out_ds[[nm]]
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) {
    out_ds[[nm]] <- format(col, format = "%Y-%m-%dT%H:%M:%S")
    out_ds[[nm]][is.na(col)] <- NA_character_
  } else if (inherits(col, "Date")) {
    out_ds[[nm]] <- format(col, format = "%Y-%m-%d")
  }
}
write.csv(out_ds, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# 4. VISUAL AUDIT: proc print obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT
audit_vars <- c("USUBJID", "TRT01A", "PARAM", "CNSR", "AVAL", "EVENTDT", "EOSDT")
for (v in audit_vars) {
  if (!v %in% names(adtte)) stop(paste("audit var missing", v))
}
audit_n <- min(15, nrow(adtte))
audit_df <- adtte[seq_len(audit_n), audit_vars, drop = FALSE]
audit_df <- cbind(Obs = seq_len(audit_n), audit_df)

# CSV of table body (ISO dates)
audit_csv <- audit_df
for (nm in names(audit_csv)) {
  col <- audit_csv[[nm]]
  if (inherits(col, "Date")) audit_csv[[nm]] <- format(col, format = "%Y-%m-%d")
  if (inherits(col, "POSIXct") || inherits(col, "POSIXlt")) audit_csv[[nm]] <- format(col, format = "%Y-%m-%dT%H:%M:%S")
}
# audit_csv currently has Date already? adtte has Date, audit_df has Date
# Re-format: ensure character ISO
for (nm in c("EVENTDT", "EOSDT")) {
  if (nm %in% names(audit_csv)) {
    col0 <- audit_df[[nm]]
    if (inherits(col0, "Date")) audit_csv[[nm]] <- format(col0, format = "%Y-%m-%d")
  }
}
write.csv(audit_csv, file.path(out, "P007.csv"), row.names = FALSE, na = "")

# Text rendering like PROC PRINT
fmt_cell_char <- function(x) ifelse(is.na(x) | trimws(as.character(x)) == "", "", as.character(x))
fmt_cell_num <- function(x) ifelse(is.na(x), ".", as.character(x))
fmt_cell_date <- function(x) {
  if (inherits(x, "Date")) {
    ifelse(is.na(x), ".", format(x, format = "%Y-%m-%d"))
  } else if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    ifelse(is.na(x), ".", format(x, format = "%Y-%m-%dT%H:%M:%S"))
  } else {
    # character or other
    s <- as.character(x)
    ifelse(is.na(x), ".", s)
  }
}

lines <- c()
lines <- c(lines, "ADTTE Audit - Time to First Adverse Event")
lines <- c(lines, "")
# header
hdr <- sprintf("%-4s %-14s %-12s %-30s %-4s %-6s %-12s %-12s",
               "Obs", "USUBJID", "TRT01A", "PARAM", "CNSR", "AVAL", "EVENTDT", "EOSDT")
lines <- c(lines, hdr)
for (i in seq_len(audit_n)) {
  r <- audit_df[i, , drop = FALSE]
  usub <- fmt_cell_char(r$USUBJID)
  trt <- fmt_cell_char(r$TRT01A)
  par <- fmt_cell_char(r$PARAM)
  cnsr <- fmt_cell_num(r$CNSR)
  aval <- fmt_cell_num(r$AVAL)
  evdt <- fmt_cell_date(adtte$EVENTDT[i])
  eos <- fmt_cell_date(adtte$EOSDT[i])
  # truncate to widths for display (SAS truncates long values)
  if (nchar(par) > 30) par <- substr(par, 1, 30)
  if (nchar(usub) > 14) usub <- substr(usub, 1, 14)
  if (nchar(trt) > 12) trt <- substr(trt, 1, 12)
  line <- sprintf("%-4d %-14s %-12s %-30s %-4s %-6s %-12s %-12s",
                  r$Obs, usub, trt, par, cnsr, aval, evdt, eos)
  lines <- c(lines, line)
}
lines <- c(lines, "")
lines <- c(lines, paste0("N = ", audit_n, " (first ", audit_n, " observations of ADTTE, N=", nrow(adtte), ")"))

writeLines(lines, file.path(out, "P007.txt"))

cat("Done. ADTTE:", nrow(adtte), " audit:", audit_n, "\n")
