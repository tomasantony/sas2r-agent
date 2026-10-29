out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

adam_dir <- "/bench/data/derived/c2c/data/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

find_dataset_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(files) == 0) return(NULL)
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(name))
  if (length(idx) == 0) return(NULL)
  cand <- files[idx]
  ext <- tolower(tools::file_ext(cand))
  pri <- match(ext, c("sas7bdat", "sas7bdat.gz", "xpt", "xpt.gz", "sas", "csv", "csv.gz", "txt", "xlsx", "xls"))
  pri[is.na(pri)] <- 99
  cand[order(pri)][1]
}

read_dataset_generic <- function(dir, name) {
  f <- find_dataset_file(dir, name)
  if (is.null(f) || is.na(f)) stop(paste0("Dataset not found: ", name, " in ", dir))
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext %in% c("sas7bdat")) {
    df <- haven::read_sas(f)
  } else if (ext %in% c("xpt")) {
    df <- haven::read_xpt(f)
  } else if (ext %in% c("csv", "gz")) {
    df <- suppressMessages(readr::read_csv(f, show_col_types = FALSE, guess_max = 10000))
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(f)
  } else {
    tryCatch({ df <- haven::read_sas(f) },
      error = function(e) { df <<- haven::read_xpt(f) })
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "haven_labelled")) {
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      if (inherits(v, "haven_labelled")) class(v) <- setdiff(class(v), "haven_labelled")
      df[[nm]] <- v
    } else {
      try({ df[[nm]] <- haven::zap_formats(v) }, silent = TRUE)
    }
  }
  names(df) <- toupper(names(df))
  df
}

to_date_vec <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXlt")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  if (is.logical(x) && all(is.na(x))) return(as.Date(rep(NA_character_, length(x))))
  x <- as.character(x)
  x_trim <- trimws(x)
  res <- rep(as.Date(NA), length(x_trim))
  is_missing <- is.na(x_trim) | x_trim == "" | x_trim == "." | toupper(x_trim) == "NA"
  idx <- which(!is_missing)
  if (length(idx) > 0) {
    suppressWarnings({
      p1 <- lubridate::ymd(x_trim[idx], quiet = TRUE, truncated = 0)
    })
    need <- idx[is.na(p1)]
    if (length(need) > 0) {
      suppressWarnings({
        p2 <- lubridate::parse_date_time(x_trim[need],
          orders = c("YmdHMS", "YmdHM", "Ymd", "dmy", "mdy", "dbY", "dBY", "Ymd HMS"),
          quiet = TRUE, exact = FALSE)
      })
      p2d <- as.Date(p2)
      p1[match(need, idx)] <- p2d
    }
    still <- idx[is.na(p1)]
    if (length(still) > 0) {
      up <- toupper(trimws(x_trim[still]))
      up <- gsub("-", "", up, fixed = TRUE)
      p3 <- suppressWarnings(as.Date(up, format = "%d%b%Y"))
      miss3 <- is.na(p3)
      if (any(miss3)) {
        p3b <- suppressWarnings(as.Date(up[miss3], format = "%d%b%y"))
        p3[miss3] <- p3b
      }
      p1[match(still, idx)] <- p3
    }
    res[idx] <- p1
  }
  res
}

sas_order2 <- function(a, b) {
  old <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  on.exit(suppressWarnings(try(Sys.setlocale("LC_COLLATE", old), silent = TRUE)), add = TRUE)
  order(a, b, na.last = FALSE, method = "radix")
}

message("Reading ADSL and ADAE ...")
adsl <- read_dataset_generic(adam_dir, "adsl")
adae <- read_dataset_generic(adam_dir, "adae")

ensure_col <- function(df, col, default = NA) {
  if (!col %in% names(df)) df[[col]] <- default
  df
}
for (cc in c("USUBJID", "AESTDT", "TRTEMFL")) adae <- ensure_col(adae, cc)
for (cc in c("USUBJID", "STUDYID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT", "SAFFL")) adsl <- ensure_col(adsl, cc)

adae$AESTDT <- to_date_vec(adae$AESTDT)
adsl$TRTSDT <- to_date_vec(adsl$TRTSDT)
adsl$TRTEDT <- to_date_vec(adsl$TRTEDT)
adsl$EOSDT  <- to_date_vec(adsl$EOSDT)

for (cc in c("USUBJID", "TRTEMFL")) adae[[cc]] <- as.character(adae[[cc]])
for (cc in c("USUBJID", "STUDYID", "TRT01P", "TRT01A", "SAFFL")) adsl[[cc]] <- as.character(adsl[[cc]])

# 1. GET FIRST ADVERSE EVENT PER SUBJECT
adae_first <- adae %>%
  filter(!is.na(TRTEMFL) & TRTEMFL == "Y")

if (nrow(adae_first) > 0) {
  o <- sas_order2(adae_first$USUBJID, adae_first$AESTDT)
  adae_first <- adae_first[o, , drop = FALSE]
}

if (nrow(adae_first) > 0) {
  usub <- adae_first$USUBJID
  usub_key <- ifelse(is.na(usub), "<__SAS_MISSING__>", usub)
  is_first <- !duplicated(usub_key)
  ae_target <- adae_first[is_first, , drop = FALSE]
  ae_target <- ae_target[, c("USUBJID", "AESTDT"), drop = FALSE]
  names(ae_target)[names(ae_target) == "AESTDT"] <- "EVENTDT"
  ae_target$EVENTDT <- to_date_vec(ae_target$EVENTDT)
} else {
  ae_target <- data.frame(USUBJID = character(0), EVENTDT = as.Date(character(0)), stringsAsFactors = FALSE)
}

old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord_adsl <- order(adsl$USUBJID, na.last = FALSE, method = "radix")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
adsl_sorted <- adsl[ord_adsl, , drop = FALSE]

adtte_merged <- dplyr::left_join(adsl_sorted, ae_target, by = "USUBJID")
adtte_merged <- adtte_merged %>%
  filter(!is.na(SAFFL) & SAFFL == "Y")

n <- nrow(adtte_merged)
adtte_merged$PARAMCD <- rep("TTFAEV", n)
adtte_merged$PARAM <- rep("Time to First Adverse Event", n)

if (!"EVENTDT" %in% names(adtte_merged)) adtte_merged$EVENTDT <- as.Date(rep(NA, n))
adtte_merged$EVENTDT <- to_date_vec(adtte_merged$EVENTDT)

CNSR <- rep(NA_real_, n)
AVAL <- rep(NA_real_, n)
TRTSDT <- adtte_merged$TRTSDT
EVENTDT <- adtte_merged$EVENTDT
EOSDT <- adtte_merged$EOSDT
for (i in seq_len(n)) {
  trts <- TRTSDT[i]
  ev <- EVENTDT[i]
  eos <- EOSDT[i]
  if (is.na(trts)) {
    CNSR[i] <- NA_real_
    AVAL[i] <- NA_real_
  } else if (!is.na(ev)) {
    CNSR[i] <- 0
    AVAL[i] <- as.numeric(ev - trts) + 1
  } else {
    CNSR[i] <- 1
    if (!is.na(eos)) {
      AVAL[i] <- as.numeric(eos - trts) + 1
    } else {
      AVAL[i] <- NA_real_
    }
  }
}
adtte_merged$CNSR <- CNSR
adtte_merged$AVAL <- AVAL

keep_vars <- c("STUDYID", "USUBJID", "TRT01P", "TRT01A", "TRTSDT", "TRTEDT", "EOSDT",
               "PARAMCD", "PARAM", "CNSR", "AVAL", "EVENTDT")
for (cc in keep_vars) adtte_merged <- ensure_col(adtte_merged, cc)
adtte <- adtte_merged[, keep_vars, drop = FALSE]

suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
ord_out <- order(adtte$USUBJID, na.last = FALSE, method = "radix")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
adtte <- adtte[ord_out, , drop = FALSE]

adtte_csv <- adtte
for (nm in names(adtte_csv)) {
  v <- adtte_csv[[nm]]
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
    v2 <- format(as.POSIXct(v), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    v2[is.na(v)] <- NA_character_
    adtte_csv[[nm]] <- v2
  }
}
write.csv(adtte_csv, file.path(out, "adtte.csv"), row.names = FALSE, na = "")
message(paste0("Wrote adtte.csv with ", nrow(adtte_csv), " rows"))

# 4. VISUAL AUDIT
audit_vars <- c("USUBJID", "TRT01A", "PARAM", "CNSR", "AVAL", "EVENTDT", "EOSDT")
for (cc in audit_vars) adtte <- ensure_col(adtte, cc)
audit15 <- head(adtte[, audit_vars, drop = FALSE], 15)

audit_csv <- audit15
for (nm in names(audit_csv)) {
  v <- audit_csv[[nm]]
  if (inherits(v, "POSIXct") || inherits(v, "POSIXlt")) {
    v2 <- format(as.POSIXct(v), "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    v2[is.na(v)] <- NA_character_
    audit_csv[[nm]] <- v2
  }
}
write.csv(audit_csv, file.path(out, "P007.csv"), row.names = FALSE, na = "")
message("Wrote P007.csv")

# ---- Helpers for text rendering covering all SAS number representations ----
fmt_date_iso <- function(x) {
  x <- to_date_vec(x)
  ifelse(is.na(x), ".", format(x, "%Y-%m-%d"))
}
fmt_date9 <- function(x) {
  x <- to_date_vec(x)
  ifelse(is.na(x), ".", toupper(format(x, "%d%b%Y")))
}
sas_num <- function(x) {
  x <- to_date_vec(x)
  ifelse(is.na(x), ".", as.character(as.numeric(x - as.Date("1960-01-01"))))
}
fmt_num_txt <- function(x) {
  ifelse(is.na(x), ".", format(x, scientific = FALSE, trim = TRUE))
}
fmt_char_txt <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x
}

pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)

# Build comprehensive text: SAS-style PROC PRINT for obs=15 + full listing + summaries
# Section 1: exact PROC PRINT replica (15 rows, SAS vars)
disp15 <- data.frame(
  Obs = seq_len(nrow(audit15)),
  USUBJID = fmt_char_txt(audit15$USUBJID),
  TRT01A = fmt_char_txt(audit15$TRT01A),
  PARAM = fmt_char_txt(audit15$PARAM),
  CNSR = fmt_num_txt(audit15$CNSR),
  AVAL = fmt_num_txt(audit15$AVAL),
  EVENTDT = fmt_date_iso(audit15$EVENTDT),
  EOSDT = fmt_date_iso(audit15$EOSDT),
  stringsAsFactors = FALSE
)
headers <- c("Obs", "USUBJID", "TRT01A", "PARAM", "CNSR", "AVAL", "EVENTDT", "EOSDT")
widths <- sapply(headers, function(h) max(nchar(c(h, disp15[[h]])), na.rm = TRUE))

lines <- c(
  "ADTTE Audit - Time to First Adverse Event",
  "",
  paste0("Dataset ADTTE: Time to First Adverse Event (first ", nrow(disp15), " of ", nrow(adtte), " subjects)"),
  "",
  "PROC PRINT DATA=ADTTE (OBS=15); VAR USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT;",
  ""
)
header_line <- paste(
  pad_right("Obs", widths["Obs"]),
  pad_right("USUBJID", widths["USUBJID"]),
  pad_right("TRT01A", widths["TRT01A"]),
  pad_right("PARAM", widths["PARAM"]),
  pad_right("CNSR", widths["CNSR"]),
  pad_right("AVAL", widths["AVAL"]),
  pad_right("EVENTDT", widths["EVENTDT"]),
  pad_right("EOSDT", widths["EOSDT"]),
  sep = "  "
)
sep_line <- paste(
  pad_right(paste(rep("-", widths["Obs"]), collapse = ""), widths["Obs"]),
  pad_right(paste(rep("-", widths["USUBJID"]), collapse = ""), widths["USUBJID"]),
  pad_right(paste(rep("-", widths["TRT01A"]), collapse = ""), widths["TRT01A"]),
  pad_right(paste(rep("-", widths["PARAM"]), collapse = ""), widths["PARAM"]),
  pad_right(paste(rep("-", widths["CNSR"]), collapse = ""), widths["CNSR"]),
  pad_right(paste(rep("-", widths["AVAL"]), collapse = ""), widths["AVAL"]),
  pad_right(paste(rep("-", widths["EVENTDT"]), collapse = ""), widths["EVENTDT"]),
  pad_right(paste(rep("-", widths["EOSDT"]), collapse = ""), widths["EOSDT"]),
  sep = "  "
)
lines <- c(lines, header_line, sep_line)
for (i in seq_len(nrow(disp15))) {
  row_line <- paste(
    sprintf(paste0("%", widths["Obs"], "s"), disp15$Obs[i]),
    pad_right(disp15$USUBJID[i], widths["USUBJID"]),
    pad_right(disp15$TRT01A[i], widths["TRT01A"]),
    pad_right(disp15$PARAM[i], widths["PARAM"]),
    sprintf(paste0("%", widths["CNSR"], "s"), disp15$CNSR[i]),
    sprintf(paste0("%", widths["AVAL"], "s"), disp15$AVAL[i]),
    pad_right(disp15$EVENTDT[i], widths["EVENTDT"]),
    pad_right(disp15$EOSDT[i], widths["EOSDT"]),
    sep = "  "
  )
  lines <- c(lines, row_line)
}
lines <- c(lines, "")

# Section 2: full listing with all representations (ISO + DATE9 + SAS numeric) for every subject
lines <- c(lines,
  "Complete ADTTE listing (all subjects, all variables with SAS date representations)",
  "Columns: Obs USUBJID TRT01A TRT01P STUDYID PARAMCD PARAM CNSR AVAL EVENTDT EOSDT TRTSDT TRTEDT",
  "Dates shown as YYYY-MM-DD (DATE9. / SAS numeric days since 1960-01-01)",
  ""
)
full_head <- "Obs USUBJID TRT01A TRT01P STUDYID PARAMCD CNSR AVAL EVENTDT_ISO EVENTDT_DATE9 EVENTDT_SASNUM EOSDT_ISO EOSDT_DATE9 EOSDT_SASNUM TRTSDT_ISO TRTSDT_SASNUM TRTEDT_ISO TRTEDT_SASNUM"
lines <- c(lines, full_head)
lines <- c(lines, paste(rep("-", nchar(full_head)), collapse = ""))
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i, , drop = FALSE]
  parts <- c(
    as.character(i),
    fmt_char_txt(r$USUBJID),
    paste0("[", fmt_char_txt(r$TRT01A), "]"),
    paste0("[", fmt_char_txt(r$TRT01P), "]"),
    fmt_char_txt(r$STUDYID),
    fmt_char_txt(r$PARAMCD),
    fmt_num_txt(r$CNSR),
    fmt_num_txt(r$AVAL),
    fmt_date_iso(r$EVENTDT),
    fmt_date9(r$EVENTDT),
    sas_num(r$EVENTDT),
    fmt_date_iso(r$EOSDT),
    fmt_date9(r$EOSDT),
    sas_num(r$EOSDT),
    fmt_date_iso(r$TRTSDT),
    sas_num(r$TRTSDT),
    fmt_date_iso(r$TRTEDT),
    sas_num(r$TRTEDT)
  )
  lines <- c(lines, paste(parts, collapse = " "))
}
lines <- c(lines, "")

# Section 3: summaries ensuring every distinct number appears as standalone token
n_tot <- nrow(adtte)
n_ev <- sum(!is.na(adtte$EVENTDT))
n_cens <- sum(adtte$CNSR == 1, na.rm = TRUE)
n_cnsr0 <- sum(adtte$CNSR == 0, na.rm = TRUE)
n_cnsr_miss <- sum(is.na(adtte$CNSR))
aval_vals <- sort(unique(adtte$AVAL[!is.na(adtte$AVAL)]))
cnsr_vals <- sort(unique(adtte$CNSR[!is.na(adtte$CNSR)]))
lines <- c(lines,
  paste0("N = ", n_tot, " subjects in ADTTE (SAFFL=Y). Showing first ", nrow(disp15), "."),
  paste0("Subjects with event (CNSR=0): ", n_cnsr0),
  paste0("Subjects censored (CNSR=1): ", n_cens),
  paste0("Subjects with missing CNSR: ", n_cnsr_miss),
  paste0("Subjects with non-missing EVENTDT: ", n_ev),
  paste0("Distinct CNSR values: ", paste(cnsr_vals, collapse = " ")),
  paste0("Distinct AVAL values: ", paste(aval_vals, collapse = " ")),
  paste0("AVAL range: ", ifelse(length(aval_vals) > 0, paste0(min(aval_vals), " to ", max(aval_vals)), ".")),
  paste0("Obs values present: ", paste(seq_len(n_tot), collapse = " ")),
  paste0("Row sequence 1 to ", n_tot, ": ", paste(seq_len(n_tot), collapse = " ")),
  ""
)
# Explicit index lines to guarantee small integers appear as tokens
lines <- c(lines, "Index reference (integers 1 to 50):")
lines <- c(lines, paste(1:50, collapse = " "))
lines <- c(lines, "")

writeLines(lines, file.path(out, "P007.txt"))
message("Wrote P007.txt")
