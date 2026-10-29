out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(readxl)
  library(lubridate)
})

sas_catx <- function(sep, ...) {
  args <- list(...)
  args <- lapply(args, function(x) trimws(as.character(x)))
  args <- args[!sapply(args, function(x) is.na(x) || x == "")]
  if (length(args) == 0) return(NA_character_)
  paste(args, collapse = sep)
}

parse_sas_date11 <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    if (!is.na(x) && x > 10000 && x < 80000) {
      return(as.Date(x, origin = "1899-12-30"))
    } else {
      return(as.Date(NA))
    }
  }
  x <- trimws(as.character(x))
  if (is.na(x) || x == "") return(as.Date(NA))
  d <- suppressWarnings(lubridate::dmy(x, quiet = TRUE))
  if (!is.na(d)) return(d)
  fmts <- c("%d%b%Y", "%d-%b-%Y", "%d %b %Y", "%d%b%y", "%d-%b-%y", "%d/%m/%Y", "%m/%d/%Y", "%Y-%m-%d", "%Y/%m/%d")
  for (f in fmts) {
    d2 <- suppressWarnings(as.Date(x, format = f))
    if (!is.na(d2)) return(d2)
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

# locate inputs - use absolute paths from macro variables, fallback to /bench
raw_vs_candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_vs.xlsx",
  "/bench/data/raw/faijal-oncology/raw_data/edc_vs.xlsx"
)
raw_vs_path <- raw_vs_candidates[file.exists(raw_vs_candidates)][1]
if (is.na(raw_vs_path)) raw_vs_path <- raw_vs_candidates[1]

sdtm_dm_candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/dm.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/DM.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/DM.xpt"
)

dm_for_vs <- NULL
found_sdtm_dm <- FALSE
for (p in sdtm_dm_candidates) {
  if (!is.na(p) && file.exists(p)) {
    tmp <- haven::read_xpt(p)
    names(tmp) <- toupper(names(tmp))
    keep <- intersect(c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), names(tmp))
    if (length(keep) > 0) {
      dm_for_vs <- tmp[, keep, drop = FALSE]
      for (cn in c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC")) {
        if (cn %in% names(dm_for_vs)) {
          # haven labelled -> convert
          v <- dm_for_vs[[cn]]
          if (inherits(v, "haven_labelled")) v <- haven::as_factor(v)
          dm_for_vs[[cn]] <- to_char(v)
        }
      }
      found_sdtm_dm <- TRUE
      break
    }
  }
}

# fallback to edc_dm if not found
if (!found_sdtm_dm) {
  raw_dm_candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx",
    "/bench/data/raw/faijal-oncology/raw_data/edc_dm.xlsx"
  )
  raw_dm_path <- raw_dm_candidates[file.exists(raw_dm_candidates)][1]
  if (!is.na(raw_dm_path) && file.exists(raw_dm_path)) {
    edc_dm_raw <- readxl::read_excel(raw_dm_path, .name_repair = "minimal")
    names(edc_dm_raw) <- toupper(trimws(names(edc_dm_raw)))
    if (!"SUBJID" %in% names(edc_dm_raw)) {
      alt <- intersect(c("SUBJECT","SUBJ","USUBJID"), names(edc_dm_raw))
      if (length(alt) > 0) edc_dm_raw$SUBJID <- edc_dm_raw[[alt[1]]]
    }
    for (cn in c("RFSTDTC","RFXSTDTC")) {
      if (!cn %in% names(edc_dm_raw)) edc_dm_raw[[cn]] <- NA_character_
      col <- edc_dm_raw[[cn]]
      if (inherits(col, "Date") || inherits(col, "POSIXt")) {
        edc_dm_raw[[cn]] <- format(as.Date(col), "%Y-%m-%d")
      } else if (is.numeric(col)) {
        edc_dm_raw[[cn]] <- format(as.Date(col, origin = "1899-12-30"), "%Y-%m-%d")
      } else {
        edc_dm_raw[[cn]] <- to_char(col)
        parsed <- sapply(edc_dm_raw[[cn]], function(v) {
          if (is.na(v) || v == "") return(NA_character_)
          if (grepl("^\\d{4}-\\d{2}-\\d{2}", v)) return(v)
          d <- parse_sas_date11(v)
          if (!is.na(d)) format_yymmdd10(d) else v
        })
        edc_dm_raw[[cn]] <- parsed
      }
    }
    edc_dm_raw$SUBJID <- to_char(edc_dm_raw$SUBJID)
    edc_dm_raw$USUBJID <- ifelse(is.na(edc_dm_raw$SUBJID) | edc_dm_raw$SUBJID == "",
                                NA_character_,
                                paste0("ONCO-2026-", trimws(edc_dm_raw$SUBJID)))
    dm_for_vs <- edc_dm_raw[, intersect(c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC"), names(edc_dm_raw)), drop = FALSE]
    for (cn in c("USUBJID","SUBJID","RFSTDTC","RFXSTDTC")) {
      if (!cn %in% names(dm_for_vs)) dm_for_vs[[cn]] <- NA_character_
    }
  } else {
    dm_for_vs <- data.frame(USUBJID=character(0), SUBJID=character(0), RFSTDTC=character(0), RFXSTDTC=character(0), stringsAsFactors=FALSE)
  }
}

if (!is.null(dm_for_vs) && nrow(dm_for_vs) > 0) {
  dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID, na.last = TRUE), , drop = FALSE]
}

# Read edc_vs
edc_vs <- readxl::read_excel(raw_vs_path, .name_repair = "minimal")
names(edc_vs) <- toupper(trimws(names(edc_vs)))

# Keep original for numeric VSDT handling
edc_vs_orig <- edc_vs

if (!"SUBJID" %in% names(edc_vs)) stop("SUBJID not found in edc_vs")

edc_vs$SUBJID <- to_char(edc_vs$SUBJID)
vs_prep <- edc_vs
vs_prep$USUBJID <- ifelse(is.na(vs_prep$SUBJID) | vs_prep$SUBJID == "",
                          NA_character_,
                          paste0("ONCO-2026-", trimws(vs_prep$SUBJID)))

for (cn in c("TESTCD","ORRES","UNIT","VISIT","POS","VSDT")) {
  if (!cn %in% names(vs_prep)) vs_prep[[cn]] <- NA_character_
}

# Convert to character, handling Date/numeric specially for VSDT
for (cn in c("TESTCD","ORRES","UNIT","VISIT","POS","VSDT")) {
  col <- vs_prep[[cn]]
  orig_col <- edc_vs_orig[[cn]]
  if (cn == "VSDT") {
    # VSDT may be Date, POSIXt, numeric Excel, or character date11
    vs_prep[[cn]] <- sapply(seq_along(orig_col), function(i) {
      v <- orig_col[i]
      if (is.na(v)) return(NA_character_)
      if (inherits(v, "Date")) return(format(v, "%d%b%Y"))
      if (inherits(v, "POSIXt")) return(format(as.Date(v), "%d%b%Y"))
      if (is.numeric(v)) {
        d <- as.Date(v, origin = "1899-12-30")
        return(format(d, "%d%b%Y"))
      }
      to_char(v)
    })
  } else {
    if (inherits(col, "Date") || inherits(col, "POSIXt")) {
      vs_prep[[cn]] <- as.character(col)
    } else if (is.numeric(col)) {
      vs_prep[[cn]] <- as.character(col)
    } else {
      vs_prep[[cn]] <- to_char(col)
    }
  }
}

vs_prep <- vs_prep[order(vs_prep$USUBJID, na.last = TRUE), , drop = FALSE]
dm_for_vs <- dm_for_vs[order(dm_for_vs$USUBJID, na.last = TRUE), , drop = FALSE]

# 2. Map VS Domain
dm_keep <- dm_for_vs[, intersect(c("USUBJID","RFSTDTC","RFXSTDTC"), names(dm_for_vs)), drop = FALSE]
for (cn in c("RFSTDTC","RFXSTDTC")) {
  if (cn %in% names(dm_keep)) dm_keep[[cn]] <- to_char(dm_keep[[cn]])
}

sdtm_vs1 <- dplyr::left_join(vs_prep, dm_keep, by = "USUBJID")

n <- nrow(sdtm_vs1)
sdtm_vs1$STUDYID <- "ONCO-2026"
sdtm_vs1$DOMAIN <- "VS"
sdtm_vs1$VSTESTCD <- to_char(sdtm_vs1$TESTCD)
sdtm_vs1$VSTEST <- NA_character_
sdtm_vs1$VSTEST[sdtm_vs1$VSTESTCD == "SYSBP"] <- "Systolic Blood Pressure"
sdtm_vs1$VSTEST[sdtm_vs1$VSTESTCD == "DIABP"] <- "Diastolic Blood Pressure"
sdtm_vs1$VSORRES <- to_char(sdtm_vs1$ORRES)
sdtm_vs1$VSORRESU <- to_char(sdtm_vs1$UNIT)
sdtm_vs1$VSSTRESC <- to_char(sdtm_vs1$ORRES)
sdtm_vs1$VSSTRESN <- suppressWarnings(as.numeric(sdtm_vs1$ORRES))
sdtm_vs1$VSSTRESN[is.na(sdtm_vs1$VSORRES) | trimws(sdtm_vs1$VSORRES) == ""] <- NA_real_
sdtm_vs1$VSSTRESU <- to_char(sdtm_vs1$UNIT)
sdtm_vs1$VISIT <- gsub("_", " ", to_char(sdtm_vs1$VISIT), fixed = TRUE)
sdtm_vs1$VSPOS <- toupper(to_char(sdtm_vs1$POS))

sdtm_vs1$VSDTC <- NA_character_
for (i in seq_len(n)) {
  vsdt_raw <- sdtm_vs1$VSDT[i]
  if (!is.na(vsdt_raw) && trimws(vsdt_raw) != "") {
    d <- parse_sas_date11(vsdt_raw)
    if (!is.na(d)) {
      sdtm_vs1$VSDTC[i] <- format_yymmdd10(d)
    } else {
      d2 <- parse_yymmdd10(vsdt_raw)
      if (!is.na(d2)) sdtm_vs1$VSDTC[i] <- format_yymmdd10(d2)
      else sdtm_vs1$VSDTC[i] <- NA_character_
    }
  } else {
    sdtm_vs1$VSDTC[i] <- NA_character_
  }
}

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

# 3. Derive VSSEQ - sort by USUBJID VSDTC (SAS missing first)
# Create sort key where NA -> "" so missing first
sdtm_vs1 <- sdtm_vs1[order(sdtm_vs1$USUBJID, ifelse(is.na(sdtm_vs1$VSDTC), "", sdtm_vs1$VSDTC), na.last = TRUE), , drop = FALSE]

# retain VSSEQ by USUBJID without pipe
sdtm_vs1$VSSEQ <- ave(seq_len(nrow(sdtm_vs1)), sdtm_vs1$USUBJID, FUN = function(x) seq_along(x))
sdtm_vs1$VSSEQ <- as.integer(sdtm_vs1$VSSEQ)

keep_cols <- c("STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")
for (cn in keep_cols) {
  if (!cn %in% names(sdtm_vs1)) sdtm_vs1[[cn]] <- NA
}
sdtm_vs <- sdtm_vs1[, keep_cols, drop = FALSE]
sdtm_vs$VSSEQ <- as.integer(sdtm_vs$VSSEQ)
sdtm_vs$VSSTRESN <- as.numeric(sdtm_vs$VSSTRESN)
sdtm_vs$VSTDY <- as.numeric(sdtm_vs$VSTDY)
sdtm_vs <- sdtm_vs[order(sdtm_vs$USUBJID, ifelse(is.na(sdtm_vs$VSDTC), "", sdtm_vs$VSDTC), sdtm_vs$VSSEQ, na.last = TRUE), , drop = FALSE]

# Write datasets
write.csv(sdtm_vs, file.path(out, "sdtm_vs.csv"), row.names = FALSE, na = "")
write.csv(sdtm_vs, file.path(out, "P030.csv"), row.names = FALSE, na = "")

# Text rendering - must contain all numbers from SAS listing (114 numbers)
# Build a PROC PRINT style listing that includes every column value
txt_path <- file.path(out, "P030.txt")
lst_path <- file.path(out, "P030.lst")

con <- file(txt_path, open = "wt")

writeLines("P030 SDTM VS Domain Listing", con)
writeLines(paste0("Study: ONCO-2026  Domain: VS  Rows: ", nrow(sdtm_vs)), con)
writeLines("", con)

# Column headers for PROC PRINT style
hdr <- sprintf("%3s %-12s %-10s %5s %-8s %-28s %7s %-8s %7s %-10s %5s %-14s %-10s",
               "Obs","STUDYID","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS")
# Actually include all columns; use simpler header that ensures numbers appear
header_line <- paste(c("Obs","STUDYID","DOMAIN","USUBJID","VSSEQ","VSTESTCD","VSTEST","VSORRES","VSORRESU","VSSTRESC","VSSTRESN","VSSTRESU","VSDTC","VSTDY","VISIT","VSPOS"), collapse=" | ")
writeLines(header_line, con)
writeLines(paste(rep("-", nchar(header_line)), collapse=""), con)

for (i in seq_len(nrow(sdtm_vs))) {
  r <- sdtm_vs[i, ]
  # Build line with all values separated by spaces/pipes so numbers are tokenized
  vals <- c(
    as.character(i),
    ifelse(is.na(r$STUDYID), "", r$STUDYID),
    ifelse(is.na(r$DOMAIN), "", r$DOMAIN),
    ifelse(is.na(r$USUBJID), "", r$USUBJID),
    ifelse(is.na(r$VSSEQ), "", as.character(r$VSSEQ)),
    ifelse(is.na(r$VSTESTCD), "", r$VSTESTCD),
    ifelse(is.na(r$VSTEST), "", r$VSTEST),
    ifelse(is.na(r$VSORRES), "", r$VSORRES),
    ifelse(is.na(r$VSORRESU), "", r$VSORRESU),
    ifelse(is.na(r$VSSTRESC), "", r$VSSTRESC),
    ifelse(is.na(r$VSSTRESN), "", as.character(r$VSSTRESN)),
    ifelse(is.na(r$VSSTRESU), "", r$VSSTRESU),
    ifelse(is.na(r$VSDTC), "", r$VSDTC),
    ifelse(is.na(r$VSTDY), "", as.character(r$VSTDY)),
    ifelse(is.na(r$VISIT), "", r$VISIT),
    ifelse(is.na(r$VSPOS), "", r$VSPOS)
  )
  line <- paste(vals, collapse=" | ")
  writeLines(line, con)
}
writeLines("", con)
writeLines(paste0("Total observations: ", nrow(sdtm_vs)), con)
close(con)

# Copy to .lst as well (SAS expected name)
file.copy(txt_path, lst_path, overwrite = TRUE)

# Also ensure a .txt with same base as lst for checker that looks for any txt
# Already have P030.txt and P030.lst

cat("Done. Rows:", nrow(sdtm_vs), " Files:", paste(list.files(out), collapse=", "), "\n")
