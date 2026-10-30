suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/sdtm"

# Read a SAS dataset from the sdtm library (sas7bdat preferred, xpt fallback)
read_lib <- function(name) {
  f1 <- file.path(sdtm_dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(sdtm_dir, paste0(name, ".xpt"))
  if (file.exists(f1)) {
    d <- haven::read_sas(f1)
  } else if (file.exists(f2)) {
    d <- haven::read_xpt(f2)
  } else {
    stop("Cannot find dataset ", name)
  }
  names(d) <- toupper(names(d))
  d
}

# SAS missing() for character values: NA or all blanks
is_blank <- function(x) is.na(x) | trimws(x) == ""

# SAS LENGTH(): trailing blanks trimmed, blank string gives 1
sas_length <- function(x) {
  x <- ifelse(is.na(x), "", x)
  n <- nchar(sub("\\s+$", "", x))
  ifelse(n == 0, 1L, n)
}

# INPUT(SUBSTR(x,1,10), yymmdd10.) when LENGTH(x) >= 10
to_date <- function(x) {
  res <- as.Date(rep(NA_character_, length(x)))
  ok <- sas_length(x) >= 10
  s <- substr(ifelse(is.na(x), "", x), 1, 10)
  res[ok] <- suppressWarnings(as.Date(s[ok], format = "%Y-%m-%d"))
  res
}

# Width of a character column, if known
char_width <- function(x) {
  w <- attr(x, "width")
  if (is.null(w)) NA_integer_ else as.integer(w)
}

# Completed years (YRDIF AGE basis, floored)
age_years <- function(b, e) {
  bl <- as.POSIXlt(b)
  el <- as.POSIXlt(e)
  yrs <- (el$year - bl$year)
  before <- (el$mon < bl$mon) | (el$mon == bl$mon & el$mday < bl$mday)
  as.numeric(yrs - ifelse(before, 1, 0))
}

## 1. First exposure record per subject
ex <- read_lib("ex")
for (v in c("EXSTDTC", "EXENDTC", "EXTRT")) if (!v %in% names(ex)) ex[[v]] <- NA_character_
extrt_width <- char_width(ex$EXTRT)
ex_first <- ex
ord <- order(ex_first$USUBJID, ifelse(is.na(ex_first$EXSTDTC), "", ex_first$EXSTDTC),
             method = "radix")
ex_first <- ex_first[ord, ]
ex_trt <- ex_first[!duplicated(ex_first$USUBJID), c("USUBJID", "EXTRT", "EXSTDTC", "EXENDTC")]
ex_trt$IN_B <- TRUE

## 2. Sort DM
dm <- read_lib("dm")
arm_width <- char_width(dm$ARM)
dm_sorted <- dm[order(dm$USUBJID, method = "radix"), ]
for (v in c("RFICDTC", "RFPENDTC", "BRTHDTC", "AGE", "SEX", "RACE", "ARMCD", "ARM", "SITEID", "SUBJID"))
  if (!v %in% names(dm_sorted)) dm_sorted[[v]] <- if (v == "AGE") NA_real_ else NA_character_

## 3. Merge
adsl <- dm_sorted %>% left_join(ex_trt, by = "USUBJID")
adsl$IN_B[is.na(adsl$IN_B)] <- FALSE

armcd <- ifelse(is.na(adsl$ARMCD), "", adsl$ARMCD)
trt01p <- ifelse(armcd != "SCRNFAIL", ifelse(is.na(adsl$ARM), "", adsl$ARM), "UNPLANNED")
if (!is.na(arm_width)) trt01p <- substr(trt01p, 1, arm_width)
adsl$TRT01P <- trt01p

trt01a <- ifelse(adsl$IN_B & !is_blank(adsl$EXTRT), adsl$EXTRT, "NOT TREATED")
if (!is.na(extrt_width)) trt01a <- substr(trt01a, 1, extrt_width)
adsl$TRT01A <- trt01a

adsl$ITTFL <- ifelse(armcd != "SCRNFAIL", "Y", "N")
adsl$SAFFL <- ifelse(!is_blank(adsl$EXSTDTC), "Y", "N")

adsl$BRTHDT <- to_date(adsl$BRTHDTC)
adsl$RFICDT <- to_date(adsl$RFICDTC)
adsl$TRTSDT <- to_date(adsl$EXSTDTC)
adsl$TRTEDT <- to_date(adsl$EXENDTC)
adsl$EOSDT  <- to_date(adsl$RFPENDTC)

adsl$AGE <- as.numeric(adsl$AGE)
okage <- !is.na(adsl$BRTHDT) & !is.na(adsl$RFICDT)
adsl$AGE[okage] <- age_years(adsl$BRTHDT[okage], adsl$RFICDT[okage])

adsl$TRTDURD <- ifelse(!is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT),
                       as.numeric(adsl$TRTEDT - adsl$TRTSDT) + 1, NA_real_)

keep <- c("STUDYID", "USUBJID", "SUBJID", "SITEID", "TRT01P", "TRT01A", "ITTFL", "SAFFL",
          "BRTHDT", "RFICDT", "TRTSDT", "TRTEDT", "EOSDT", "AGE", "TRTDURD", "SEX", "RACE")
# PDV order: retained vars, then DM vars, then new vars
pdv <- unique(c("STUDYID", "USUBJID", "SUBJID", "SITEID", names(dm_sorted),
                "TRT01P", "TRT01A", "ITTFL", "SAFFL", "BRTHDT", "RFICDT", "TRTSDT",
                "TRTEDT", "EOSDT", "TRTDURD"))
cols <- pdv[pdv %in% keep]
adsl_draft <- as.data.frame(adsl)[, cols]

## 4. Sort and save
adsl_final <- adsl_draft[order(adsl_draft$USUBJID, method = "radix"), ]
adsl_out <- adsl_final
for (v in c("BRTHDT", "RFICDT", "TRTSDT", "TRTEDT", "EOSDT"))
  adsl_out[[v]] <- ifelse(is.na(adsl_out[[v]]), "", format(adsl_out[[v]], "%Y-%m-%d"))
adsl_out[] <- lapply(adsl_out, function(x) { attributes(x) <- NULL; x })
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

## 5. Visual audit listing (first 10 obs)
lst <- head(adsl_final, 10)
lst <- data.frame(Obs = seq_len(nrow(lst)),
                  USUBJID = as.character(lst$USUBJID),
                  TRT01P = as.character(lst$TRT01P),
                  TRT01A = as.character(lst$TRT01A),
                  ITTFL = lst$ITTFL, SAFFL = lst$SAFFL,
                  AGE = ifelse(is.na(lst$AGE), ".", format(lst$AGE, trim = TRUE)),
                  stringsAsFactors = FALSE)
write.csv(lst, file.path(out, "P006.csv"), row.names = FALSE, na = "")

fmt_tab <- function(df) {
  w <- sapply(names(df), function(n) max(nchar(n), nchar(as.character(df[[n]])), na.rm = TRUE))
  hdr <- paste(mapply(function(n, wd) formatC(n, width = wd), names(df), w), collapse = "  ")
  rows <- apply(df, 1, function(r) paste(mapply(function(v, wd) formatC(v, width = wd), r, w),
                                         collapse = "  "))
  c(hdr, "", rows)
}
txt <- c("ADSL Audit - Subject Level Analysis Dataset", "", fmt_tab(lst))
writeLines(txt, file.path(out, "P006.txt"))

cat("P006 done:", nrow(adsl_final), "subjects\n")
