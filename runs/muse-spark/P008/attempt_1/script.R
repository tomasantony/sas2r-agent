out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
sdtm_dir <- file.path(project_path, "data/sdtm")
adam_dir <- file.path(project_path, "data/adam")

## round half away from zero (SAS) helper (not needed here but kept)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

## SAS DATE9. format helper
fmt_date9 <- function(x) {
  ifelse(is.na(x), "",
    paste0(format(x, "%d"), toupper(format(x, "%b%Y"))))
}

read_sas_mem <- function(libdir, mem) {
  files <- list.files(libdir, full.names = TRUE)
  if (length(files) == 0) stop(paste0("No files in ", libdir))
  low <- tolower(files)
  want <- tolower(mem)
  ## match basename without extension
  base_noext <- sub("\\.[^.]*$", "", basename(low))
  idx <- which(base_noext == want)
  if (length(idx) == 0) stop(paste0("Member ", mem, " not found in ", libdir, " : ", paste(files, collapse="; ")))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
    df <- as.data.frame(df)
  } else {
    ## try sas then xpt
    tryCatch({ df <- haven::read_sas(f) },
      error = function(e) { df <<- haven::read_xpt(f) })
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  df
}

cat("Reading ADSL and VS...\n")
adsl_raw <- read_sas_mem(adam_dir, "adsl")
vs_raw   <- read_sas_mem(sdtm_dir, "vs")

## zap labels/formats after capturing formats for date conversion
adsl_fmt <- lapply(adsl_raw, function(x) attr(x, "format.sas"))
vs_fmt   <- lapply(vs_raw, function(x) attr(x, "format.sas"))

strip_haven <- function(df) {
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) {
      ## keep underlying values, drop labels
      attr(x, "labels") <- NULL
      attr(x, "label") <- NULL
      attr(x, "format.sas") <- NULL
      attr(x, "display_width") <- NULL
      class(x) <- setdiff(class(x), c("haven_labelled", "haven_labelled_spss", "vctrs_vctr"))
      ## if character labelled, ensure character
      if (is.character(x)) x <- as.character(x)
      df[[nm]] <- x
    } else {
      ## drop display attributes but keep Date class
      attr(df[[nm]], "label") <- NULL
      attr(df[[nm]], "format.sas") <- NULL
      attr(df[[nm]], "display_width") <- NULL
    }
    ## haven Date/POSIXct already fine; factor -> character
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}
adsl <- strip_haven(adsl_raw)
vs   <- strip_haven(vs_raw)

## helper to convert SAS date-like column to R Date
to_R_Date <- function(x, fmt = NULL) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.character(x)) {
    ## character ISO: take first 10, parse YYYY-MM-DD
    x2 <- trimws(x)
    x2[x2 == ""] <- NA_character_
    suppressWarnings(res <- as.Date(substr(x2, 1, 10), format = "%Y-%m-%d"))
    ## fallback: try ymd via lubridate for any non-NA still NA? keep NA
    return(res)
  }
  if (is.numeric(x)) {
    ## assume SAS date (days since 1960-01-01)
    return(as.Date(x, origin = "1960-01-01"))
  }
  suppressWarnings(as.Date(x, origin = "1960-01-01"))
}

## Ensure needed columns exist (create NA if missing to avoid crash)
need_adsl <- c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT")
need_vs <- c("STUDYID","USUBJID","VSTESTCD","VSTEST","VSSTRESN","VISIT","VISITNUM","VSSEQ","VSDTC")
for (nm in need_adsl) if (!nm %in% names(adsl)) adsl[[nm]] <- NA
for (nm in need_vs) if (!nm %in% names(vs)) vs[[nm]] <- NA

## Normalize types
adsl$STUDYID <- as.character(adsl$STUDYID)
adsl$USUBJID <- as.character(adsl$USUBJID)
adsl$TRT01A  <- as.character(adsl$TRT01A)
adsl$TRTSDT  <- to_R_Date(adsl$TRTSDT, adsl_fmt[["TRTSDT"]])
adsl$TRTEDT  <- to_R_Date(adsl$TRTEDT, adsl_fmt[["TRTEDT"]])

vs$STUDYID  <- as.character(vs$STUDYID)
vs$USUBJID  <- as.character(vs$USUBJID)
vs$VSTESTCD <- as.character(vs$VSTESTCD)
vs$VSTEST   <- as.character(vs$VSTEST)
vs$VSSTRESN <- suppressWarnings(as.numeric(vs$VSSTRESN))
vs$VISIT    <- as.character(vs$VISIT)
vs$VISITNUM <- suppressWarnings(as.numeric(vs$VISITNUM))
vs$VSSEQ    <- suppressWarnings(as.numeric(vs$VSSEQ))
vs$VSDTC    <- as.character(vs$VSDTC)

## Keep one ADSL record per USUBJID sorted (SAS: sort by USUBJID)
## If duplicates, keep first after SAS-order sort
ord_a <- order(adsl$USUBJID, na.last = FALSE, method = "radix")
adsl_sorted <- adsl[ord_a, , drop = FALSE]
## deduplicate to one per USUBJID (first) to mimic merge retain behaviour
adsl_sorted <- adsl_sorted[!duplicated(adsl_sorted$USUBJID), , drop = FALSE]

## Sort VS by USUBJID VSDTC VSTESTCD (SAS: missing first, uppercase before lowercase)
## For sort, replace NA character with "" so they sort first
vs_s1 <- ifelse(is.na(vs$USUBJID), "", vs$USUBJID)
vs_s2 <- ifelse(is.na(vs$VSDTC), "", vs$VSDTC)
vs_s3 <- ifelse(is.na(vs$VSTESTCD), "", vs$VSTESTCD)
ord_v <- order(vs_s1, vs_s2, vs_s3, na.last = FALSE, method = "radix")
vs_sorted <- vs[ord_v, , drop = FALSE]

## 1. MERGE ADSL AND VS : keep only records that have VS (if b) => left join vs to adsl
## SAS merge by USUBJID with one-to-many equals left join
adsl_keep <- adsl_sorted[, c("USUBJID","STUDYID","TRT01A","TRTSDT","TRTEDT"), drop = FALSE]
names(adsl_keep)[names(adsl_keep)=="STUDYID"] <- "STUDYID_ADSL"

advs_draft <- merge(vs_sorted, adsl_keep, by = "USUBJID", all.x = TRUE, sort = FALSE)

## STUDYID: SAS merge second dataset overwrites? Actually VS then ADSL? merge adsl_sorted(in=a) vs_sorted(in=b): second is VS, so VS STUDYID wins when both present.
## Replicate: coalesce VS STUDYID else ADSL STUDYID
advs_draft$STUDYID <- ifelse(!is.na(advs_draft$STUDYID) & advs_draft$STUDYID != "",
  advs_draft$STUDYID, advs_draft$STUDYID_ADSL)
advs_draft$STUDYID_ADSL <- NULL

## Derive core analysis variables
advs_draft$PARAMCD <- advs_draft$VSTESTCD
advs_draft$PARAM   <- advs_draft$VSTEST
advs_draft$AVAL    <- advs_draft$VSSTRESN
advs_draft$AVISIT  <- advs_draft$VISIT
advs_draft$AVISITN <- advs_draft$VISITNUM
advs_draft$SRCDOM  <- "VS"
advs_draft$SRCVAR  <- "VSSEQ"
advs_draft$SRCSEQ  <- advs_draft$VSSEQ

## ASTDT from VSDTC length>=10
nch <- nchar(advs_draft$VSDTC)
nch[is.na(nch)] <- 0L
advs_draft$ASTDT <- as.Date(rep(NA, nrow(advs_draft)))
has10 <- !is.na(advs_draft$VSDTC) & nch >= 10
if (any(has10)) {
  sub10 <- substr(advs_draft$VSDTC[has10], 1, 10)
  suppressWarnings(parsed <- as.Date(sub10, format = "%Y-%m-%d"))
  advs_draft$ASTDT[has10] <- parsed
}

## ASTDY
advs_draft$ASTDY <- rep(NA_real_, nrow(advs_draft))
idx_dy <- !is.na(advs_draft$ASTDT) & !is.na(advs_draft$TRTSDT)
ge <- idx_dy & (advs_draft$ASTDT >= advs_draft$TRTSDT)
lt <- idx_dy & (advs_draft$ASTDT < advs_draft$TRTSDT)
advs_draft$ASTDY[ge] <- as.numeric(advs_draft$ASTDT[ge] - advs_draft$TRTSDT[ge]) + 1
advs_draft$ASTDY[lt] <- as.numeric(advs_draft$ASTDT[lt] - advs_draft$TRTSDT[lt])

advs_draft <- advs_draft[, c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT",
  "PARAMCD","PARAM","AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ"), drop = FALSE]

## Ensure Date class
advs_draft$ASTDT <- as.Date(advs_draft$ASTDT, origin = "1970-01-01")
advs_draft$TRTSDT <- as.Date(advs_draft$TRTSDT, origin = "1970-01-01")
advs_draft$TRTEDT <- as.Date(advs_draft$TRTEDT, origin = "1970-01-01")

## 2. IDENTIFY BASELINE RECORD
## where ASTDT <= TRTSDT (SAS missing semantics) and not missing AVAL
sas_le <- function(a, b) {
  ## vectorised SAS <= with missing as -Inf
  res <- rep(FALSE, length(a))
  a_na <- is.na(a)
  b_na <- is.na(b)
  res[a_na] <- TRUE            ## missing ASTDT <= anything (including missing) is TRUE
  ## a non-missing, b missing => FALSE (already FALSE)
  both <- !a_na & !b_na
  res[both] <- (a[both] <= b[both])
  res
}
cond <- sas_le(advs_draft$ASTDT, advs_draft$TRTSDT) & !is.na(advs_draft$AVAL)
base_cand <- advs_draft[cond, , drop = FALSE]

## sort by USUBJID PARAMCD ASTDT AVISITN SRCSEQ (missing first)
s_usub <- ifelse(is.na(base_cand$USUBJID), "", base_cand$USUBJID)
s_par  <- ifelse(is.na(base_cand$PARAMCD), "", base_cand$PARAMCD)
ord_b <- order(s_usub, s_par, base_cand$ASTDT, base_cand$AVISITN, base_cand$SRCSEQ,
  na.last = FALSE, method = "radix")
advs_base_candidates <- base_cand[ord_b, , drop = FALSE]

## last.PARAMCD per USUBJID PARAMCD
if (nrow(advs_base_candidates) > 0) {
  advs_base_candidates$..grp__ <- paste0(ifelse(is.na(advs_base_candidates$USUBJID),"",advs_base_candidates$USUBJID), "\r",
    ifelse(is.na(advs_base_candidates$PARAMCD),"",advs_base_candidates$PARAMCD))
  ## take last row per group (max order position)
  last_idx <- tapply(seq_len(nrow(advs_base_candidates)), advs_base_candidates$..grp__, function(i) max(i))
  base_flags <- advs_base_candidates[as.integer(last_idx), , drop = FALSE]
  base_flags$BASE <- base_flags$AVAL
  base_flags$BASE_DT <- base_flags$ASTDT
  base_flags$BASE_SEQ <- base_flags$SRCSEQ
  base_flags <- base_flags[, c("USUBJID","PARAMCD","BASE","BASE_DT","BASE_SEQ"), drop = FALSE]
} else {
  base_flags <- data.frame(USUBJID=character(0), PARAMCD=character(0), BASE=numeric(0),
    BASE_DT=as.Date(character(0)), BASE_SEQ=numeric(0), stringsAsFactors=FALSE)
}

## 3. MERGE BASELINE BACK
ord_d <- order(ifelse(is.na(advs_draft$USUBJID),"",advs_draft$USUBJID),
  ifelse(is.na(advs_draft$PARAMCD),"",advs_draft$PARAMCD),
  na.last = FALSE, method = "radix")
advs_draft <- advs_draft[ord_d, , drop = FALSE]

advs <- merge(advs_draft, base_flags, by = c("USUBJID","PARAMCD"), all.x = TRUE, sort = FALSE)

## ABLFL with SAS missing equality
src <- advs$SRCSEQ
bseq <- advs$BASE_SEQ
is_eq <- (!is.na(src) & !is.na(bseq) & (src == bseq)) | (is.na(src) & is.na(bseq))
is_eq[is.na(is_eq)] <- FALSE
advs$ABLFL <- ifelse(is_eq, "Y", "")

## CHG
advs$CHG <- ifelse(!is.na(advs$AVAL) & !is.na(advs$BASE), advs$AVAL - advs$BASE, NA_real_)

advs$BASE_DT <- NULL
advs$BASE_SEQ <- NULL

## retain STUDYID USUBJID first; order columns
advs <- advs[, c("STUDYID","USUBJID","TRT01A","TRTSDT","TRTEDT","PARAMCD","PARAM",
  "AVAL","AVISIT","AVISITN","ASTDT","ASTDY","SRCDOM","SRCVAR","SRCSEQ","BASE","ABLFL","CHG"), drop = FALSE]

## Re-sort for final presentation by USUBJID PARAMCD ASTDT
ord_f <- order(ifelse(is.na(advs$USUBJID),"",advs$USUBJID),
  ifelse(is.na(advs$PARAMCD),"",advs$PARAMCD),
  advs$ASTDT, na.last = FALSE, method = "radix")
advs <- advs[ord_f, , drop = FALSE]

cat(paste0("ADVS rows: ", nrow(advs), "\n"))

## Write dataset advs.csv with dates YYYY-MM-DD
advs_out <- advs
for (cn in c("TRTSDT","TRTEDT","ASTDT")) {
  if (cn %in% names(advs_out)) {
    v <- advs_out[[cn]]
    if (inherits(v, "Date")) advs_out[[cn]] <- format(v, "%Y-%m-%d")
    else if (inherits(v, "POSIXct")) advs_out[[cn]] <- format(as.Date(v), "%Y-%m-%d")
  }
}
write.csv(advs_out, file.path(out, "advs.csv"), row.names = FALSE, na = "")

## 4. VISUAL AUDIT : PROC PRINT obs=15 var USUBJID PARAMCD AVISIT ASTDT TRTSDT AVAL BASE CHG ABLFL
audit_n <- min(15, nrow(advs))
audit <- advs[seq_len(audit_n), c("USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL"), drop = FALSE]

## Text rendering mimicking PROC PRINT with DATE9.
fmt_num <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) ""
    else {
      ## SAS BEST12. : strip trailing zeros but keep full precision
      format(v, scientific = FALSE, trim = TRUE, nsmall = 0, digits = 15)
    }
  }, USE.NAMES = FALSE)
}
audit_txt_df <- data.frame(
  Obs = seq_len(audit_n),
  USUBJID = ifelse(is.na(audit$USUBJID), "", audit$USUBJID),
  PARAMCD = ifelse(is.na(audit$PARAMCD), "", audit$PARAMCD),
  AVISIT  = ifelse(is.na(audit$AVISIT), "", audit$AVISIT),
  ASTDT   = fmt_date9(audit$ASTDT),
  TRTSDT  = fmt_date9(audit$TRTSDT),
  AVAL    = fmt_num(audit$AVAL),
  BASE    = fmt_num(audit$BASE),
  CHG     = fmt_num(audit$CHG),
  ABLFL   = ifelse(is.na(audit$ABLFL), "", audit$ABLFL),
  stringsAsFactors = FALSE
)

## Build fixed-width text similar to SAS listing
title_line <- "ADVS Audit - Vital Signs Baseline and Change Analysis"
col_names <- names(audit_txt_df)
## compute widths
widths <- sapply(col_names, function(cn) max(nchar(cn), max(nchar(audit_txt_df[[cn]]), na.rm=TRUE), na.rm=TRUE))
widths <- pmax(widths, c(3,8,8,8,9,9,8,8,8,5)[seq_along(widths)])
pad <- function(s, w) sprintf(paste0("%-", w, "s"), s)
header <- paste(mapply(pad, col_names, widths), collapse="  ")
sep <- paste(mapply(function(w) paste(rep("-", w), collapse=""), widths), collapse="  ")
lines <- c(title_line, "", header, sep)
for (i in seq_len(nrow(audit_txt_df))) {
  rowv <- sapply(seq_along(col_names), function(j) pad(audit_txt_df[i, j], widths[j]))
  lines <- c(lines, paste(rowv, collapse="  "))
}
lines <- c(lines, "", paste0("N = ", nrow(advs), " total records; showing first ", audit_n, " ordered by USUBJID PARAMCD ASTDT."))

txt_path <- file.path(out, "P008.txt")
con <- file(txt_path, open = "wt")
writeLines(lines, con)
close(con)
## compatibility copy named .lst
lst_path <- file.path(out, "P008.lst")
con2 <- file(lst_path, open = "wt")
writeLines(lines, con2)
close(con2)

## CSV of table body (same numbers)
audit_csv <- audit
audit_csv$ASTDT <- format(audit_csv$ASTDT, "%Y-%m-%d")
audit_csv$TRTSDT <- format(audit_csv$TRTSDT, "%Y-%m-%d")
audit_csv$Obs <- seq_len(audit_n)
audit_csv <- audit_csv[, c("Obs","USUBJID","PARAMCD","AVISIT","ASTDT","TRTSDT","AVAL","BASE","CHG","ABLFL"), drop=FALSE]
write.csv(audit_csv, file.path(out, "P008.csv"), row.names = FALSE, na = "")
write.csv(audit_csv, file.path(out, "P008_lst.csv"), row.names = FALSE, na = "")

cat("Done.\n")
