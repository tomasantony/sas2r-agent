out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
  )
}

find_file <- function(candidates) {
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  return(NA_character_)
}

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(data.table)

find_xpt <- function(pattern) {
  roots_to_search <- c(
    "D:/meta_hackathon/sas2r-agent",
    "/bench",
    "/tmp/meta_hackathon/sas2r-agent",
    ".",
    "/",
    Sys.getenv("HOME")
  )
  for (r in roots_to_search) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern = pattern, recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
      if (length(f) > 0) {
        return(f[1])
      }
    }
  }
  return(NA)
}

cat("OUT_DIR:", out, "\n")
cat("Working dir:", getwd(), "\n")

ae_xpt_candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/ae.xpt",
  "/bench/data/raw/cdisc-pilot/sdtm/ae.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/sdtm/ae.sas7bdat",
  "/bench/data/derived/cdisc-pilot/sdtm/ae.sas7bdat",
  "./data/raw/cdisc-pilot/sdtm/ae.xpt",
  "./data/derived/cdisc-pilot/sdtm/ae.xpt"
)
adsl_xpt_candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adsl.xpt",
  "./data/raw/cdisc-pilot/adam/adsl.xpt",
  "./data/derived/cdisc-pilot/adam/adsl.xpt"
)

ae_path <- find_file(ae_xpt_candidates)
if (is.na(ae_path)) ae_path <- find_xpt("^ae\\.xpt$")
cat("AE path:", ae_path, "\n")

adsl_path <- find_file(adsl_xpt_candidates)
if (is.na(adsl_path)) adsl_path <- find_xpt("^adsl\\.xpt$")
cat("ADSL path:", adsl_path, "\n")

if (is.na(ae_path) || !file.exists(ae_path)) {
  ae_sas7 <- find_xpt("^ae\\.sas7bdat$")
  if (!is.na(ae_sas7)) ae_path <- ae_sas7
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  adsl_sas7 <- find_xpt("^adsl\\.sas7bdat$")
  if (!is.na(adsl_sas7)) adsl_path <- adsl_sas7
}

stopifnot(!is.na(ae_path), file.exists(ae_path))
stopifnot(!is.na(adsl_path), file.exists(adsl_path))

if (grepl("\\.xpt$", ae_path, ignore.case = TRUE)) {
  ae_raw <- haven::read_xpt(ae_path)
} else {
  ae_raw <- haven::read_sas(ae_path)
}
if (grepl("\\.xpt$", adsl_path, ignore.case = TRUE)) {
  adsl_raw <- haven::read_xpt(adsl_path)
} else {
  adsl_raw <- haven::read_sas(adsl_path)
}

names(ae_raw) <- toupper(names(ae_raw))
names(adsl_raw) <- toupper(names(adsl_raw))

cat("AE nrow:", nrow(ae_raw), "\n")
cat("ADSL nrow:", nrow(adsl_raw), "\n")

parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA_character_, length(x)))
  idx <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(idx)) {
    res[idx] <- as.Date(x[idx], format = "%Y-%m-%d")
  }
  res
}

ae <- ae_raw %>%
  arrange(USUBJID, AESEQ)

ae$ASTDT <- parse_yymmdd10(ae$AESTDTC)
ae$AENDT <- parse_yymmdd10(ae$AEENDTC)
ae$ADUR <- ifelse(!is.na(ae$ASTDT) & !is.na(ae$AENDT), as.integer(as.numeric(ae$AENDT - ae$ASTDT) + 1L), NA_integer_)

required_adsl_cols <- c("SITEID","USUBJID","TRT01A","TRT01AN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT")
temp <- adsl_raw %>%
  select(any_of(required_adsl_cols)) %>%
  arrange(USUBJID) %>%
  rename(TRTA = TRT01A, TRTAN = TRT01AN)

convert_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) {
    return(as.Date(x, origin = "1960-01-01"))
  }
  return(parse_yymmdd10(x))
}
temp$TRTSDT <- convert_sas_date(temp$TRTSDT)
temp$TRTEDT <- convert_sas_date(temp$TRTEDT)

common_cols <- intersect(names(ae), names(temp))
common_cols <- setdiff(common_cols, "USUBJID")
cat("Common cols AE/TEMP:", paste(common_cols, collapse=","), "\n")
if (length(common_cols) > 0) {
  ae <- ae %>% select(-any_of(common_cols))
}

ae_merged <- merge(ae, temp, by = "USUBJID", all.x = TRUE, sort = FALSE)

ae_merged$ASTDTF <- NA_character_
ae_merged$CQ01NAM <- NA_character_

idx_impute <- is.na(ae_merged$ASTDT) & nchar(trimws(as.character(ae_merged$AESTDTC))) == 7 & !is.na(ae_merged$AESTDTC)
if (any(idx_impute, na.rm=TRUE)) {
  to_parse <- paste0(gsub(" ", "", trimws(as.character(ae_merged$AESTDTC[idx_impute]))), "-01")
  parsed <- parse_yymmdd10(to_parse)
  ae_merged$ASTDT[idx_impute] <- parsed
  ae_merged$ASTDTF[idx_impute] <- "D"
}

ae_merged$ASTDY <- NA_integer_
ae_merged$AENDY <- NA_integer_
ae_merged$ANLSTDY <- NA_integer_
ae_merged$TRTEMFL <- NA_character_

idx_both_ast <- !is.na(ae_merged$ASTDT) & !is.na(ae_merged$TRTSDT)
idx_ge <- idx_both_ast & ae_merged$ASTDT >= ae_merged$TRTSDT
idx_lt <- idx_both_ast & ae_merged$TRTSDT > ae_merged$ASTDT
ae_merged$ASTDY[idx_ge] <- as.integer(as.numeric(ae_merged$ASTDT[idx_ge] - ae_merged$TRTSDT[idx_ge]) + 1L)
ae_merged$ASTDY[idx_lt] <- as.integer(as.numeric(ae_merged$ASTDT[idx_lt] - ae_merged$TRTSDT[idx_lt]))

idx_both_aend <- !is.na(ae_merged$AENDT) & !is.na(ae_merged$TRTSDT)
idx_ge2 <- idx_both_aend & ae_merged$AENDT >= ae_merged$TRTSDT
idx_lt2 <- idx_both_aend & ae_merged$TRTSDT > ae_merged$AENDT
ae_merged$AENDY[idx_ge2] <- as.integer(as.numeric(ae_merged$AENDT[idx_ge2] - ae_merged$TRTSDT[idx_ge2]) + 1L)
ae_merged$AENDY[idx_lt2] <- as.integer(as.numeric(ae_merged$AENDT[idx_lt2] - ae_merged$TRTSDT[idx_lt2]))

ae_merged$ANLSTDY[idx_both_ast] <- as.integer(as.numeric(ae_merged$ASTDT[idx_both_ast] - ae_merged$TRTSDT[idx_both_ast]) + 1L)

ae_merged$TRTEMFL <- ifelse(idx_ge, "Y", "N")

ae_merged$AEDECOD <- as.character(ae_merged$AEDECOD)
ae_merged$AEBODSYS <- as.character(ae_merged$AEBODSYS)

has_app <- grepl("APPLICATION", ae_merged$AEDECOD, fixed = TRUE)
has_derm <- grepl("DERMATITIS", ae_merged$AEDECOD, fixed = TRUE)
has_ery <- grepl("ERYTHEMA", ae_merged$AEDECOD, fixed = TRUE)
has_blister <- grepl("BLISTER", ae_merged$AEDECOD, fixed = TRUE)
has_app[is.na(has_app)] <- FALSE
has_derm[is.na(has_derm)] <- FALSE
has_ery[is.na(has_ery)] <- FALSE
has_blister[is.na(has_blister)] <- FALSE

is_skin <- ae_merged$AEBODSYS == "SKIN AND SUBCUTANEOUS TISSUE DISORDERS"
is_skin[is.na(is_skin)] <- FALSE
not_in_list2 <- !(ae_merged$AEDECOD %in% c("COLD SWEAT","HYPERHIDROSIS","ALOPECIA"))
not_in_list2[is.na(ae_merged$AEDECOD)] <- TRUE

cond_cq <- has_app | has_derm | has_ery | has_blister | (is_skin & not_in_list2)
ae_merged$CQ01NAM[cond_cq] <- "DERMATOLOGIC EVENTS"

flagfrst <- function(inds, sortvars, invar, outvar, ser = NULL, derm = NULL) {
  df <- inds
  df <- df %>% filter(TRTEMFL == "Y")
  if (!is.null(invar) && invar != "" && trimws(invar) != "") {
    df <- df %>% filter(!is.na(.data[[invar]]) & trimws(.data[[invar]]) != "")
  }
  if (!is.null(ser) && ser == "Y") {
    df <- df %>% filter(AESER == "Y")
  }
  if (!is.null(derm) && derm == "Y") {
    df <- df %>% filter(CQ01NAM == "DERMATOLOGIC EVENTS")
  }
  sort_keys <- c("USUBJID", sortvars, "ASTDT")
  sort_keys <- sort_keys[sort_keys != "" & !is.na(sort_keys)]
  order_list <- list()
  for (k in sort_keys) {
    order_list[[k]] <- df[[k]]
  }
  ord <- do.call(order, c(order_list, list(na.last = FALSE, method = "radix")))
  ds01 <- df[ord, , drop = FALSE]

  if (is.null(invar) || invar == "" || trimws(invar) == "") {
    ds02 <- ds01 %>%
      group_by(USUBJID) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(!!outvar := "Y") %>%
      select(USUBJID, AESEQ, all_of(outvar))
  } else {
    group_vars <- c("USUBJID", sortvars)
    if (!(invar %in% group_vars)) group_vars <- c(group_vars, invar)
    ds02 <- ds01 %>%
      group_by(across(all_of(group_vars))) %>%
      slice(1) %>%
      ungroup() %>%
      mutate(!!outvar := "Y") %>%
      select(USUBJID, AESEQ, all_of(outvar))
  }
  ds02 <- ds02 %>% arrange(USUBJID, AESEQ)
  return(ds02)
}

aea <- flagfrst(inds = ae_merged, sortvars = c(), invar = "", outvar = "AOCCFL")
aeb <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCCSFL")
aec <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCCPFL")
saea <- flagfrst(inds = ae_merged, sortvars = c(), invar = "", outvar = "AOCC02FL", ser = "Y")
saeb <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS"), invar = "AEBODSYS", outvar = "AOCC03FL", ser = "Y")
saec <- flagfrst(inds = ae_merged, sortvars = c("AEBODSYS","AEDECOD"), invar = "AEDECOD", outvar = "AOCC04FL", ser = "Y")
derm <- flagfrst(inds = ae_merged, sortvars = c(), invar = "", outvar = "AOCC01FL", derm = "Y")

ae_merged <- ae_merged %>% arrange(USUBJID, AESEQ)

ae_final <- ae_merged %>%
  left_join(aea, by = c("USUBJID","AESEQ")) %>%
  left_join(aeb, by = c("USUBJID","AESEQ")) %>%
  left_join(aec, by = c("USUBJID","AESEQ")) %>%
  left_join(saea, by = c("USUBJID","AESEQ")) %>%
  left_join(saeb, by = c("USUBJID","AESEQ")) %>%
  left_join(saec, by = c("USUBJID","AESEQ")) %>%
  left_join(derm, by = c("USUBJID","AESEQ"))

stopifnot("TRTEMFL" %in% names(ae_final))
stopifnot("AOCCFL" %in% names(ae_final))

desired_order <- c("STUDYID","SITEID","USUBJID","TRTA","TRTAN","AGE","AGEGR1","AGEGR1N","RACE","RACEN","SEX","SAFFL","TRTSDT","TRTEDT",
                   "ASTDT","ASTDTF","ASTDY","AENDT","AENDY","ADUR","AETERM",
                   "AELLT","AELLTCD","AEDECOD","AEPTCD","AEHLT","AEHLTCD","AEHLGT","AEHLGTCD","AEBODSYS","AESOC","AESOCCD","AESEV","AESER","AESCAN","AESCONG","AESDISAB","AESDTH","AESHOSP","AESLIFE","AESOD","AEREL","AEACN",
                   "AEOUT","AESEQ","TRTEMFL","AOCCFL","AOCCSFL","AOCCPFL","AOCC02FL","AOCC03FL","AOCC04FL","CQ01NAM","AOCC01FL")

existing_desired <- desired_order[desired_order %in% names(ae_final)]
missing_desired <- desired_order[!desired_order %in% names(ae_final)]
cat("Missing desired cols:", paste(missing_desired, collapse=","), "\n")

for (col in missing_desired) {
  ae_final[[col]] <- NA
}

ae_ordered <- ae_final %>% select(all_of(desired_order))
ae_ordered <- ae_ordered %>% arrange(USUBJID, AESEQ)

date_cols <- c("TRTSDT","TRTEDT","ASTDT","AENDT")
for (dc in date_cols) {
  if (dc %in% names(ae_ordered)) {
    ae_ordered[[dc]] <- as.character(ae_ordered[[dc]])
  }
}

output_path <- file.path(out, "adae.csv")
write.csv(ae_ordered, output_path, row.names = FALSE, na = "")

cat("Written ADAE to", output_path, "rows:", nrow(ae_ordered), "\n")

labels <- c(
  TRTEMFL = "Treatment Emergent Analysis Flag",
  AOCCFL = "1st Occurrence of Any AE Flag",
  AOCCSFL = "1st Occurrence of SOC Flag",
  AOCCPFL = "1st Occurrence of Preferred Term Flag",
  ASTDT = "Analysis Start Date",
  ASTDTF = "Analysis Start Date Imputation Flag",
  ASTDY = "Analysis Start Relative Day",
  AENDT = "Analysis End Date",
  AENDY = "Analysis End Relative Day",
  TRTA = "Actual Treatment",
  TRTAN = "Actual Treatment (N)",
  CQ01NAM = "Customized Query 01 Name",
  AOCC02FL = "1st Occurrence 02 Flag for Serious",
  AOCC03FL = "1st Occurrence 03 Flag for Serious SOC",
  AOCC04FL = "1st Occurrence 04 Flag for Serious PT",
  AOCC01FL = "1st Occurrence 01 Flag for CQ01",
  ADUR = "AE Duration (days)"
)

contents_txt <- file.path(out, "adae.txt")
sink(contents_txt)
cat("CONTENTS OF ADAE (Adverse Events Analysis Dataset)\n")
cat("Variables in creation order (VARNUM):\n")
cat(sprintf("%-3s %-12s %-40s %s\n", "#", "Variable", "Label", "Type"))
for (i in seq_along(names(ae_ordered))) {
  var <- names(ae_ordered)[i]
  lab <- ifelse(var %in% names(labels), labels[var], "")
  typ <- class(ae_ordered[[var]])[1]
  cat(sprintf("%-3d %-12s %-40s %s\n", i, var, lab, typ))
}
cat("\n")
cat("Number of observations:", nrow(ae_ordered), "\n")
cat("Number of variables:", ncol(ae_ordered), "\n")
sink()

cat("Done\n")
