```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## helpers ---------------------------------------------------------------
sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(x) trimws(as.character(x)))
  args <- args[!sapply(args, function(x) is.na(x) || x == "")]
  if (length(args) == 0) return(NA_character_)
  paste(args, collapse = sep)
}

parse_sas_date11 <- function(x) {
  # x may be character like "01JAN2020" / "01-JAN-2020" / "01JAN20" or Date/POSIXt or numeric Excel date
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    # Excel numeric date: origin 1899-12-30
    # readxl already converts to Date, but if numeric remains, try convert
    # only if plausible Excel date number (e.g., 30000-50000)
    if (!is.na(x) && x > 10000 && x < 80000) {
      return(as.Date(x, origin = "1899-12-30"))
    } else {
      return(as.Date(NA))
    }
  }
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(as.Date(NA))
  # remove extra spaces
  # Try lubridate dmy first (handles 01JAN2020, 01-JAN-2020, 01 JAN 2020)
  d <- suppressWarnings(lubridate::dmy(x, quiet = TRUE))
  if (!is.na(d)) return(d)
  # Try explicit formats
  fmts <- c("%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%d%b%y", "%d-%b-%y", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d")
  for (f in fmts) {
    d2 <- suppressWarnings(as.Date(x, format = f))
    if (!is.na(d2)) return(d2)
    # try upper/lower case month
    d2 <- suppressWarnings(as.Date(toupper(x), format = toupper(f)))
    if (!is.na(d2)) return(d2)
  }
  return(as.Date(NA))
}

format_yymmdd10 <- function(d) {
  if (is.na(d)) return(NA_character_)
  format(d, "%Y-%m-%d")
}

parse_yymmdd10 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x) && !is.na(x) && x > 10000 && x < 80000) {
    return(as.Date(x, origin = "1899-12-30"))
  }
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(as.Date(NA))
  d <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  if (!is.na(d)) return(d)
  # fallback to date11
  parse_sas_date11(x)
}

to_char <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
  if (inherits(x, "POSIXt")) return(format(as.Date(x), "%Y-%m-%d"))
  x <- as.character(x)
  x[is.na(x)] <- NA_character_
  trimws(x)
}

## locate and read inputs ------------------------------------------------
raw_vs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_vs.xlsx"
raw_dm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"

# Try to find sdtm_dm in various possible locations
sdtm_dm_candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/output/sas/datasets/P030/sdtm_dm.csv"
)

dm_for_vs <- NULL
found_sdtm_dm <- FALSE
for (p in sdtm_dm_candidates) {
  if (file.exists(p)) {
    cat("Found sdtm_dm candidate:", p, "\n")
    if (grepl("\\.xpt$", p, ignore.case = TRUE)) {
      tmp <- haven::read_xpt(p)
    } else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) {
      tmp <- haven::read_sas(p)
    } else if (grepl("\\.csv$", p, ignore.case = TRUE)) {
      tmp <- readr::read_csv(p, show_col_types = FALSE)
    } else {
      next
    }
    names(tmp) <- toupper(names(tmp))
    # keep only needed cols if exist
    keep <- intersect(c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), names(tmp))
    if (length(keep) > 0) {
      dm_for_vs <- tmp[, keep, drop = FALSE]
      # ensure character
      for (cn in c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC")) {
        if (cn %in% names(dm_for_vs)) dm_for_vs[[cn]] <- to_char(dm_for_vs[[cn]])
      }
      found_sdtm_dm <- TRUE
      break
    }
  }
}

# Read edc_dm to build dm_for_vs if not found
edc_dm <- NULL
if (file.exists(raw_dm_path)) {
  edc_dm <- readxl::read_excel(raw_dm_path, .name_repair = "minimal")
  names(edc_dm) <- toupper(trimws(names(edc_dm)))
  cat("Read edc_dm:", nrow(edc_dm), "rows,", ncol(edc_dm), "cols\n")
  print(names(edc_dm))
} else {
  cat("WARNING: edc_dm not found at", raw_dm_path, "\n")
}

if (!found_sdtm_dm) {
  cat("Deriving dm_for_vs from edc_dm\n")
  if (!is.null(edc_dm)) {
    # Ensure SUBJID exists
    if (!"SUBJID" %in% names(edc_dm)) {
      # try alternative names
      alt <- intersect(c("SUBJECT","SUBJ","USUBJID"), names(edc_dm))
      if (length(alt) > 0) {
        edc_dm$SUBJID <- edc_dm[[alt[1]]]
      } else {
        edc_dm$SUBJID <- NA_character_
      }
    }
    # RFSTDTC / RFXSTDTC may be Date or character
    for (cn in c("RFSTDTC","RFXSTDTC")) {
      if (!cn %in% names(edc_dm)) {
        # try to find similar
        cand <- grep("RFST", names(edc_dm), value = TRUE)
        if (length(cand) > 0 && cn == "RFSTDTC") edc_dm[[cn]] <- edc_dm[[cand[1]]]
        else edc_dm[[cn]] <- NA_character_
      }
      # convert to character YYYY-MM-DD if Date
      if (cn %in% names(edc_dm)) {
        col <- edc_dm[[cn]]
        if (inherits(col, "Date") || inherits(col, "POSIXt")) {
          edc_dm[[cn]] <- format(as.Date(col), "%Y-%m-%d")
        } else if (is.numeric(col)) {
          # Excel numeric
          edc_dm[[cn]] <- format(as.Date(col, origin = "1899-12-30"), "%Y-%m-%d")
        } else {
          edc_dm[[cn]] <- to_char(col)
          # if it's date11 like 01JAN2020, convert to YYYY-MM-DD
          # try to parse and reformat if it looks like date11
          parsed <- sapply(edc_dm[[cn]], function(v) {
            if (is.na(v) || v == "") return(NA_character_)
            # if already YYYY-MM-DD keep
            if (grepl("^\\d{4}-\\d{2}-\\d{2}", v)) return(v)
            d <- parse_sas_date11(v)
            if (!is.na(d)) format_yymmdd10(d) else v
          })
          edc_dm[[cn]] <- parsed
        }
      }
    }
    edc_dm$SUBJID <- to_char(edc_dm$SUBJID)
    edc_dm$USUBJID <- ifelse(is.na(edc_dm$SUBJID) | edc_dm$SUBJID == "",
                             NA_character_,
                             paste0("ONCO-2026-", trimws(edc_dm$SUBJID)))
    dm_for_vs <- edc_dm[, intersect(c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), names(edc_dm)), drop = FALSE]
    # ensure all 4 cols exist
    for (cn in c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC")) {
      if (!cn %in% names(dm_for_vs)) dm_for_vs[[cn]] <- NA_character_
    }
  } else {
    dm_for_vs <- data.frame(USUBJID=character(0), SUBJID=character(0), RFSTDTC=character(0), RFXSTDTC=character(0), stringsAsFactors=FALSE)
  }
}

# sort dm_for_vs by USUBJID (SAS missing first, character ordering)
if (!is.null(dm_for_vs) && nrow(dm_for_vs) > 0) {
  dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID, na.last = TRUE), , drop = FALSE]
}

## Read edc_vs -----------------------------------------------------------
edc_vs <- NULL
if (file.exists(raw_vs_path)) {
  edc_vs <- readxl::read_excel(raw_vs_path, .name_repair = "minimal")
  names(edc_vs) <- toupper(trimws(names(edc_vs)))
  cat("Read edc_vs:", nrow(edc_vs), "rows,", ncol(edc_vs), "cols\n")
  print(names(edc_vs))
} else {
  stop(paste("edc_vs file not found at", raw_vs_path))
}

# Prepare vs_prep: USUBJID = catx("-", "ONCO-2026", SUBJID)
if (!"SUBJID" %in% names(edc_vs)) {
  stop("SUBJID not found in edc_vs")
}
edc_vs$SUBJID <- to_char(edc_vs$SUBJID)
vs_prep <- edc_vs
vs_prep$USUBJID <- ifelse(is.na(vs_prep$SUBJID) | vs_prep$SUBJID == "",
                          NA_character_,
                          paste0("ONCO-2026-", trimws(vs_prep$SUBJID)))

# Ensure expected columns exist, create NA if missing
for (cn in c("TESTCD","ORRES","UNIT","VISIT","POS","VSDT")) {
  if (!cn %in% names(vs_prep)) {
    vs_prep[[cn]] <- NA_character_
    cat("WARNING: column", cn, "not in edc_vs, filling with NA\n")
  }
}
# Convert relevant cols to character (handle Date/numeric)
for (cn in c("TESTCD","ORRES","UNIT","VISIT","POS","VSDT")) {
  col <- vs_prep[[cn]]
  if (inherits(col, "Date") || inherits(col, "POSIXt")) {
    # VSDT as Date should be kept as character for later parsing, but we need original string
    # For VSDT, if it's Date, we will later format to YYYY-MM-DD via date11 logic
    # Keep as character date11? Actually SAS input(VSDT,date11.) expects date11 string
    # If it's already Date, we can treat as Date
    vs_prep[[cn]] <- as.character(col)
  } else if (is.numeric(col) && cn %in% c("ORRES")) {
    vs_prep[[cn]] <- as.character(col)
  } else {
    vs_prep[[cn]] <- to_char(col)
  }
  # For VSDT numeric Excel date, convert to Date then to character date11? Keep as is
  if (cn == "VSDT" && is.numeric(edc_vs[[cn]])) {
    # edc_vs original numeric
    orig <- edc_vs[[cn]]
    vs_prep[[cn]] <- sapply(orig, function(v) {
      if (is.na(v)) return(NA_character_)
      # if numeric Excel date, convert to Date then format as 01JAN2020 style for SAS parsing
      d <- as.Date(v, origin = "1899-12-30")
      format(d, "%d%b%Y")
    })
  }
}

# Sort vs_prep by USUBJID
vs_prep <- vs_prep[order(vs_prep$USUBJID, na.last = TRUE), , drop = FALSE]

# Sort dm_for_vs by USUBJID (already)
dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID, na.last = TRUE), , drop = FALSE]

## 2. Map VS Domain -> sdtm_vs1 -----------------------------------------
# Merge vs_prep(in=v) dm_for_vs(keep=USUBJID RFSTDTC RFXSTDTC) by USUBJID; if v;
# This is left join vs_prep with dm_for_vs
dm_keep <- dm_for_vs[, intersect(c("USUBJID","RFSTDTC","RFXSTDTC"), names(dm_for_vs)), drop = FALSE]
# Ensure RFSTDTC/RFXSTDTC are character YYYY-MM-DD
for (cn in c("RFSTDTC","RFXSTDTC")) {
  if (cn %in% names(dm_keep)) dm_keep[[cn]] <- to_char(dm_keep[[cn]])
}

# Left join
sdtm_vs1 <- dplyr::left_join(vs_prep, dm_keep, by = "USUBJID")

# Now derive variables per SAS
n <- nrow(sdtm_vs1)
sdtm_vs1$STUDYID <- "ONCO-2026"
sdtm_vs1$DOMAIN <- "VS"

# VSTESTCD = TESTCD
sdtm_vs1$VSTESTCD <- to_char(sdtm_vs1$TESTCD)

# VSTEST mapping
sdtm_vs1$VSTEST <- NA_character_
sdtm_vs1$VSTEST[sdtm_vs1$VSTESTCD == "SYSBP"] <- "Systolic Blood Pressure"
sdtm_vs1$VSTEST[sdtm_vs1$VSTESTCD == "DIABP"] <- "Diastolic Blood Pressure"
# For other codes, leave NA (SAS would leave missing)

sdtm_vs1$VSORRES <- to_char(sdtm_vs1$ORRES)
sdtm_vs1$VSORRESU <- to_char(sdtm_vs1$UNIT)

sdtm_vs1$VSSTRESC <- to_char(sdtm_vs1$ORRES)
# VSSTRESN = input(ORRES, best.) -> numeric
sdtm_vs1$VSSTRESN <- suppressWarnings(as.numeric(sdtm_vs1$ORRES))
# SAS best. would parse numbers, R as.numeric does similar; handle empty
sdtm_vs1$VSSTRESN[is.na(sdtm_vs1$VSORRES) | trimws(sdtm_vs1$VSORRES) == ""] <- NA_real_

sdtm_vs1$VSSTRESU <- to_char(sdtm_vs1$UNIT)

# VISIT = tranwrd(VISIT, "_", " ")
sdtm_vs1$VISIT <- gsub("_", " ", to_char(sdtm_vs1$VISIT), fixed = TRUE)

# VSPOS = upcase(POS)
sdtm_vs1$VSPOS <- toupper(to_char(sdtm_vs1$POS))

# VSDTC = put(input(VSDT, date11.), yymmdd10.) if VSDT ne ""
sdtm_vs1$VSDTC <- NA_character_
for (i in seq_len(n)) {
  vsdt_raw <- sdtm_vs1$VSDT[i]
  if (!is.na(vsdt_raw) && trimws(vsdt_raw) != "") {
    d <- parse_sas_date11(vsdt_raw)
    if (!is.na(d)) {
      sdtm_vs1$VSDTC[i] <- format_yymmdd10(d)
    } else {
      # try yymmdd10 already
      d2 <- parse_yymmdd10(vsdt_raw)
      if (!is.na(d2)) sdtm_vs1$VSDTC[i] <- format_yymmdd10(d2)
      else sdtm_vs1$VSDTC[i] <- NA_character_
    }
  } else {
    sdtm_vs1$VSDTC[i] <- NA_character_
  }
}

# Study Day
sdtm_vs1$VSTDY <- NA_real_
for (i in seq_len(n)) {
  vsdtc <- sdtm_vs1$VSDTC[i]
  rfstdtc <- if ("RFSTDTC" %in% names(sdtm_vs1)) sdtm_vs1$RFSTDTC[i] else NA_character_
  if (!is.na(vsdtc) && trimws(vsdtc) != "" && !is.na(rfstdtc) && trimws(rfstdtc) != "") {
    stdt <- parse_yymmdd10(vsdtc)
    rfdt <- parse_yymmdd10(rfstdtc)
    if (!is.na(stdt) && !is.na(rfdt)) {
      diff <- as.integer(stdt - rfdt)
      if (stdt >= rfdt) sdtm_vs1$VSTDY[i] <- diff + 1
      else sdtm_vs1$VSTDY[i] <- diff
    }
  }
}

## 3. Derive VSSEQ -------------------------------------------------------
# proc sort data=sdtm_vs1; by USUBJID VSDTC; run;
# SAS sorts missing first for VSDTC
# Implement order with NA first
ord <- order(sdtm_vs1$USUBJID, ifelse(is.na(sdtm_vs1$VSDTC), "", sdtm_vs1$VSDTC), na.last = TRUE)
sdtm_vs1 <- sdtm_vs1[ord, , drop = FALSE]

# data sdtm_vs; by USUBJID; retain VSSEQ; if first.USUBJID then VSSEQ=1 else VSSEQ+1;
sdtm_vs1$VSSEQ <- NA_integer_
# Use dplyr group_by
sdtm_vs1 <- sdtm_vs1 %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(VSSEQ = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  as.data.frame()

# Keep final columns
keep_cols <- c("STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")
# Ensure all exist
for (cn in keep_cols) {
  if (!cn %in% names(sdtm_vs1)) sdtm_vs1[[cn]] <- NA
}
sdtm_vs <- sdtm_vs1[, keep_cols, drop = FALSE]

# Ensure types: VSSEQ integer, VSSTRESN numeric, VSTDY numeric
sdtm_vs$VSSEQ <- as.integer(sdtm_vs$VSSEQ)
sdtm_vs$VSSTRESN <- as.numeric(sdtm_vs$VSSTRESN)
sdtm_vs$VSTDY <- as.numeric(sdtm_vs$VSTDY)

# Sort for final output by USUBJID VSDTC VSSEQ (already)
sdtm_vs <- sdtm_vs[order(sdtm_vs$USUBJID, ifelse(is.na(sdtm_vs$VSDTC), "", sdtm_vs$VSDTC), sdtm_vs$VSSEQ, na.last = TRUE), , drop = FALSE]

cat("Final sdtm_vs rows:", nrow(sdtm_vs), "\n")
print(head(sdtm_vs))

## Write outputs ---------------------------------------------------------
# Dataset CSV
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")

# Also write P030.csv as table body (same as sdtm_vs)
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")

# Text rendering (P030.lst equivalent)
txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst")
# Create a plain-text listing similar to PROC PRINT
# Count numbers for verification: count numeric values in table
num_count <- sum(!is.na(sdtm_vs$VSSEQ)) + sum(!is.na(sdtm_vs$VSSTRESN)) + sum(!is.na(sdtm_vs$VSTDY))
cat("Numeric count in listing:", num_count, "\n")

# Build text file
con <- file(txt_path, open = "wt")
writeLines("Listing P030: SDTM VS Domain (Vital Signs)", con)
writeLines(paste0("Study: ONCO-2026  Domain: VS  Generated: ", format(Sys.Date(), "%Y-%m-%d")), con)
writeLines(paste0("Rows: ", nrow(sdtm_vs)), con)
writeLines("", con)
# Header
header <- sprintf("%-15s %5s %-8s %-28s %8s %-8s %10s %6s %-12s %-8s",
                  "USUBJID","SEQ","TESTCD","TEST","ORRES","ORRESU","VSDTC","VSTDY","VISIT","VSPOS")
writeLines(header, con)
writeLines(paste(rep("-", nchar(header)), collapse=""), con)
for (i in seq_len(nrow(sdtm_vs))) {
  r <- sdtm_vs[i,]
  line <- sprintf("%-15s %5s %-8s %-28s %8s %-8s %10s %6s %-12s %-8s",
                  ifelse(is.na(r$USUBJID), "", substr(r$USUBJID,1,15)),
                  ifelse(is.na(r$VSSEQ), "", as.character(r$VSSEQ)),
                  ifelse(is.na(r$VSTESTCD), "", r$VSTESTCD),
                  ifelse(is.na(r$VSTEST), "", substr(r$VSTEST,1,28)),
                  ifelse(is.na(r$VSORRES), "", r$VSORRES),
                  ifelse(is.na(r$VSORRESU), "", r$VSORRESU),
                  ifelse(is.na(r$VSDTC), "", r$VSDTC),
                  ifelse(is.na(r$VSTDY), "", as.character(r$VSTDY)),
                  ifelse(is.na(r$VISIT), "", substr(r$VISIT,1,12)),
                  ifelse(is.na(r$VSPOS), "", r$VSPOS))
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Total observations: ", nrow(sdtm_vs)), con)
writeLines(paste0("Numeric values displayed: ", num_count), con)
close(con)

# Also copy to .lst extension for expected name
if (file.exists(txt_path)) {
  file.copy(txt_path, lst_path, overwrite = TRUE)
}

# Also write a CSV of plotted data placeholder (no figure in this program, but create empty)
# For completeness, write sdtm_vs plotted data CSV already done

cat("Outputs written to", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
```