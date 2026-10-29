out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
})

# SAS round half away from zero helper (not strictly needed here)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_date9 <- function(x) {
  # format Date as SAS DATE9. e.g. 15JAN2024
  ifelse(is.na(x), "",
    paste0(format(x, "%d"), toupper(format(x, "%b")), format(x, "%Y")))
}

# =====================================================================
# STEP 1: mock SDTM source data (DM, DS, EX) - inline datalines
# =====================================================================
dm <- tibble::tibble(
  STUDYID  = c("STUDY01","STUDY01","STUDY01","STUDY01","STUDY01","STUDY01"),
  USUBJID  = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SUBJID   = c("1001","1002","1003","1004","1005","1006"),
  SITEID   = c("101","101","102","102","103","103"),
  SEX      = c("M","F","M","F","M","F"),
  RACE     = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO","NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO"),
  ARMCD    = c("TRT","PBO","TRT","TRT","PBO","TRT"),
  ARM      = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  COUNTRY  = c("USA","USA","USA","USA","USA","USA"),
  BRTHDTC  = c("1965-03-15","1978-08-22","1980-11-01","1972-06-30","1988-02-14","1955-12-05"),
  RFICDTC  = c("2024-01-10","2024-01-12","2024-01-15","2024-01-18","2024-01-20","2024-01-22"),
  RFSTDTC  = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFENDTC  = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  RFXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  AGE      = c(59,45,43,51,36,68)
)

ds <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  DSTERM  = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSDECOD = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSCAT   = rep("DISPOSITION EVENT", 6),
  DSSTDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)

ex <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  EXTRT   = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  EXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  EXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)

# =====================================================================
# STEP 2: Derive ADSL variables (base = DM)
# =====================================================================
adsl <- dm %>%
  mutate(
    TRTP = ARM,
    TRTPN = case_when(ARMCD == "TRT" ~ 1, ARMCD == "PBO" ~ 2, TRUE ~ NA_real_),
    TRTA = TRTP,
    TRTAN = TRTPN,
    TRTSDT = suppressWarnings(ymd(RFXSTDTC)),
    TRTEDT = case_when(RFXENDTC != "" & RFXENDTC != "." ~ suppressWarnings(ymd(RFXENDTC)),
                       TRUE ~ as.Date(NA)),
    RANDDT = case_when(RFSTDTC != "" ~ suppressWarnings(ymd(RFSTDTC)),
                       TRUE ~ as.Date(NA)),
    TRTDURD = case_when(!is.na(TRTSDT) & !is.na(TRTEDT) ~ as.numeric(TRTEDT - TRTSDT) + 1,
                        TRUE ~ NA_real_),
    AGEGR1 = ifelse(AGE < 65, "<65", ">=65"),
    AGEGR1N = ifelse(AGE < 65, 1, 2),
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ NA_real_),
    RACEN = case_when(RACE == "WHITE" ~ 1,
                      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
                      RACE == "ASIAN" ~ 3,
                      TRUE ~ 4),
    SAFFL = "Y",
    ITTFL = "Y",
    EFFFL = "Y"
  )

# SAS sorts (character ordering, missing first) then merge
adsl <- adsl %>% arrange(STUDYID, USUBJID)
ds_s <- ds %>% arrange(STUDYID, USUBJID)

adsl <- adsl %>%
  left_join(ds_s %>% select(STUDYID, USUBJID, DSTERM, DSDECOD),
            by = c("STUDYID", "USUBJID")) %>%
  mutate(
    EOSSTT = case_when(DSDECOD == "COMPLETED" ~ "COMPLETED",
                       DSDECOD == "ONGOING" ~ "ONGOING",
                       TRUE ~ "DISCONTINUED"),
    COMPLFL = case_when(DSDECOD == "COMPLETED" ~ "Y",
                        DSDECOD == "ONGOING" ~ "N",
                        TRUE ~ "N"),
    DCSREAS = case_when(DSDECOD == "COMPLETED" ~ "",
                        DSDECOD == "ONGOING" ~ "",
                        TRUE ~ DSDECOD)
  ) %>%
  select(-DSTERM, -DSDECOD)

# =====================================================================
# STEP 3: labels and finalize, sort by STUDYID USUBJID
# =====================================================================
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# SAS variable order: DM order + derived order
var_order <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC",
  "ARMCD","ARM","COUNTRY","BRTHDTC","RFICDTC","RFSTDTC","RFENDTC",
  "RFXSTDTC","RFXENDTC","AGE",
  "TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN",
  "AGEGR1","AGEGR1N","RACEN","SEXN","SAFFL","ITTFL","EFFFL","COMPLFL",
  "DCSREAS","EOSSTT","TRTDURD")
var_order <- var_order[var_order %in% names(adsl)]
adsl <- adsl %>% select(all_of(var_order))

labels <- c(
  STUDYID="Study Identifier", USUBJID="Unique Subject Identifier",
  SUBJID="Subject Identifier for the Study", SITEID="Study Site Identifier",
  SEX="Sex", SEXN="Sex (N)", RACE="Race", RACEN="Race (N)",
  ETHNIC="Ethnicity", AGE="Age", AGEGR1="Age Group 1", AGEGR1N="Age Group 1 (N)",
  ARM="Description of Planned Arm", ARMCD="Planned Arm Code",
  TRTP="Planned Treatment", TRTPN="Planned Treatment (N)",
  TRTA="Actual Treatment", TRTAN="Actual Treatment (N)",
  TRTSDT="Date of First Exposure to Treatment",
  TRTEDT="Date of Last Exposure to Treatment",
  TRTDURD="Total Treatment Duration (Days)",
  RANDDT="Date of Randomization",
  SAFFL="Safety Population Flag", ITTFL="Intent-to-Treat Population Flag",
  EFFFL="Efficacy Population Flag", COMPLFL="Completers Population Flag",
  EOSSTT="End of Study Status", DCSREAS="Reason for Discontinuation",
  COUNTRY="Country"
)
for (v in names(labels)) {
  if (v %in% names(adsl)) attr(adsl[[v]], "label") <- labels[[v]]
}

# =====================================================================
# Write dataset adsl.csv (dates YYYY-MM-DD, na="")
# =====================================================================
adsl_out <- adsl
for (cn in names(adsl_out)) {
  if (inherits(adsl_out[[cn]], "Date")) {
    adsl_out[[cn]] <- format(adsl_out[[cn]], "%Y-%m-%d")
  }
}
# ensure numeric unrounded; keep as is
write.csv(adsl_out, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# =====================================================================
# P083.lst rendering: PROC CONTENTS VARNUM + PROC PRINT
# =====================================================================
# Build contents-like metadata in VARNUM order (current column order)
vartype <- sapply(adsl, function(x) if (is.numeric(x) || inherits(x, "Date")) "Num" else "Char")
varlen <- sapply(adsl, function(x) {
  if (inherits(x, "Date")) return(8)
  if (is.numeric(x)) return(8)
  m <- max(nchar(as.character(x), keepNA = FALSE), na.rm = TRUE)
  if (!is.finite(m)) m <- 1
  max(m, 1)
})
varlabel <- sapply(names(adsl), function(v) {
  lb <- attr(adsl[[v]], "label")
  if (is.null(lb)) "" else lb
})
varformat <- sapply(names(adsl), function(v) {
  if (v %in% c("TRTSDT","TRTEDT","RANDDT")) "DATE9." else ""
})
contents_df <- data.frame(
  Num = seq_along(names(adsl)),
  Variable = names(adsl),
  Type = as.character(vartype),
  Len = as.integer(varlen),
  Format = as.character(varformat),
  Label = as.character(varlabel),
  stringsAsFactors = FALSE
)

# Display version with DATE9. formatted dates (as SAS PRINT would show)
adsl_disp <- adsl
adsl_disp$TRTSDT <- sas_date9(adsl$TRTSDT)
adsl_disp$TRTEDT <- sas_date9(adsl$TRTEDT)
adsl_disp$RANDDT <- sas_date9(adsl$RANDDT)
# character missing already ""; numeric missing -> "" for display
disp_mat <- as.data.frame(lapply(adsl_disp, function(x) {
  if (is.numeric(x)) { y <- as.character(x); y[is.na(x)] <- ""; y }
  else { y <- as.character(x); y[is.na(y)] <- ""; y }
}), stringsAsFactors = FALSE, check.names = FALSE)

# Build text file
txt_path <- file.path(out, "P083.txt")
lst_path <- file.path(out, "P083.lst")
con <- file(txt_path, open = "wt")
writeLines("Program: P083_adsl.sas  ADSL (Subject-Level Analysis Dataset)", con)
writeLines("Standard: CDISC ADaM IG 1.3", con)
writeLines(paste0("Date: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")), con)
writeLines("", con)
writeLines("PROC CONTENTS DATA=ADSL VARNUM;", con)
writeLines(paste0("Observations: ", nrow(adsl), "  Variables: ", ncol(adsl)), con)
writeLines("", con)
writeLines(sprintf("%-4s %-10s %-5s %-4s %-9s %s", "Num", "Variable", "Type", "Len", "Format", "Label"), con)
writeLines(paste0(strrep("-", 90)), con)
for (i in seq_len(nrow(contents_df))) {
  writeLines(sprintf("%-4d %-10s %-5s %-4d %-9s %s",
    contents_df$Num[i], contents_df$Variable[i], contents_df$Type[i],
    contents_df$Len[i], contents_df$Format[i], contents_df$Label[i]), con)
}
writeLines("", con)
writeLines("PROC PRINT DATA=ADSL (OBS=10) NOOBS;", con)
writeLines(paste0("Rows shown: ", nrow(adsl_disp)), con)
writeLines("", con)
# header
hdr <- paste(names(disp_mat), collapse = " | ")
writeLines(hdr, con)
writeLines(paste0(strrep("-", nchar(hdr))), con)
for (r in seq_len(nrow(disp_mat))) {
  writeLines(paste(as.character(unlist(disp_mat[r, ])), collapse = " | "), con)
}
writeLines("", con)
writeLines("Treatment duration (TRTDURD = TRTEDT - TRTSDT + 1) derived where both dates non-missing.", con)
writeLines("Population flags SAFFL/ITTFL/EFFFL = Y for all 6 subjects.", con)
writeLines(paste0("Completers: ", sum(adsl$COMPLFL == "Y"), " of ", nrow(adsl),
  " ; Discontinued: ", sum(adsl$EOSSTT == "DISCONTINUED"),
  " ; Ongoing: ", sum(adsl$EOSSTT == "ONGOING")), con)
writeLines(paste0("Age mean=", sprintf("%.1f", mean(adsl$AGE)),
  " min=", min(adsl$AGE), " max=", max(adsl$AGE)), con)
close(con)
# SAS .lst counterpart: copy of txt
file.copy(txt_path, lst_path, overwrite = TRUE)

# CSV of the table body (printed table) -> P083.csv
# Use YYYY-MM-DD dates for CSV body (same as adsl.csv)
write.csv(adsl_out, file.path(out, "P083.csv"), row.names = FALSE, na = "")
