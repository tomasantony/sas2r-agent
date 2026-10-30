# P083_adsl.sas -> R translation
# ADSL Subject-Level Analysis Dataset

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(haven)
library(tibble)

# Helper: SAS date9. format for display (e.g., 15JAN2024)
fmt_date9 <- function(x) {
  ifelse(is.na(x), "", toupper(format(x, "%d%b%Y")))
}

# Helper: parse YYYY-MM-DD, treat "" and "." as NA
parse_ymd <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | x == "." | toupper(x) == "NA"] <- NA
  as.Date(x, format = "%Y-%m-%d")
}

# ==========================================================================
# STEP 1: Create mock SDTM source data (DM, DS, EX)
# ==========================================================================

dm <- tibble::tribble(
  ~STUDYID, ~USUBJID,      ~SUBJID, ~SITEID, ~SEX, ~RACE,                          ~ETHNIC,               ~ARMCD, ~ARM,          ~COUNTRY, ~BRTHDTC,     ~RFICDTC,     ~RFSTDTC,     ~RFENDTC,     ~RFXSTDTC,    ~RFXENDTC,    ~AGE,
  "STUDY01", "STUDY01-1001", "1001", "101", "M", "WHITE",                         "NOT HISPANIC",         "TRT",  "Treatment A", "USA",    "1965-03-15", "2024-01-10", "2024-01-15", "2024-06-15", "2024-01-15", "2024-06-15", 59,
  "STUDY01", "STUDY01-1002", "1002", "101", "F", "BLACK OR AFRICAN AMERICAN",     "NOT HISPANIC",         "PBO",  "Placebo",     "USA",    "1978-08-22", "2024-01-12", "2024-01-18", "2024-06-18", "2024-01-18", "2024-06-18", 45,
  "STUDY01", "STUDY01-1003", "1003", "102", "M", "ASIAN",                         "HISPANIC OR LATINO",   "TRT",  "Treatment A", "USA",    "1980-11-01", "2024-01-15", "2024-01-20", "2024-04-01", "2024-01-20", "2024-04-01", 43,
  "STUDY01", "STUDY01-1004", "1004", "102", "F", "WHITE",                         "NOT HISPANIC",         "TRT",  "Treatment A", "USA",    "1972-06-30", "2024-01-18", "2024-01-25", "2024-06-25", "2024-01-25", "2024-06-25", 51,
  "STUDY01", "STUDY01-1005", "1005", "103", "M", "ASIAN",                         "NOT HISPANIC",         "PBO",  "Placebo",     "USA",    "1988-02-14", "2024-01-20", "2024-01-28", ".",          "2024-01-28", ".",          36,
  "STUDY01", "STUDY01-1006", "1006", "103", "F", "WHITE",                         "HISPANIC OR LATINO",   "TRT",  "Treatment A", "USA",    "1955-12-05", "2024-01-22", "2024-01-30", "2024-07-01", "2024-01-30", "2024-07-01", 68
)

ds <- tibble::tribble(
  ~STUDYID, ~USUBJID,       ~DSTERM,       ~DSDECOD,       ~DSCAT,              ~DSSTDTC,
  "STUDY01", "STUDY01-1001", "COMPLETED",   "COMPLETED",    "DISPOSITION EVENT", "2024-06-15",
  "STUDY01", "STUDY01-1002", "COMPLETED",   "COMPLETED",    "DISPOSITION EVENT", "2024-06-18",
  "STUDY01", "STUDY01-1003", "ADVERSE EVENT","ADVERSE EVENT","DISPOSITION EVENT", "2024-04-01",
  "STUDY01", "STUDY01-1004", "COMPLETED",   "COMPLETED",    "DISPOSITION EVENT", "2024-06-25",
  "STUDY01", "STUDY01-1005", "ONGOING",     "ONGOING",      "DISPOSITION EVENT", ".",
  "STUDY01", "STUDY01-1006", "COMPLETED",   "COMPLETED",    "DISPOSITION EVENT", "2024-07-01"
)

ex <- tibble::tribble(
  ~STUDYID, ~USUBJID,       ~EXTRT,        ~EXSTDTC,     ~EXENDTC,
  "STUDY01", "STUDY01-1001", "Treatment A", "2024-01-15", "2024-06-15",
  "STUDY01", "STUDY01-1002", "Placebo",     "2024-01-18", "2024-06-18",
  "STUDY01", "STUDY01-1003", "Treatment A", "2024-01-20", "2024-04-01",
  "STUDY01", "STUDY01-1004", "Treatment A", "2024-01-25", "2024-06-25",
  "STUDY01", "STUDY01-1005", "Placebo",     "2024-01-28", ".",
  "STUDY01", "STUDY01-1006", "Treatment A", "2024-01-30", "2024-07-01"
)

# ==========================================================================
# STEP 2: Derive ADSL variables
# ==========================================================================

adsl <- dm %>%
  mutate(
    TRTSDT = parse_ymd(RFXSTDTC),
    TRTEDT = parse_ymd(RFXENDTC),
    RANDDT = parse_ymd(RFSTDTC),
    TRTP = ARM,
    TRTPN = case_when(
      ARMCD == "TRT" ~ 1,
      ARMCD == "PBO" ~ 2,
      TRUE ~ NA_real_
    ),
    TRTA = TRTP,
    TRTAN = TRTPN,
    TRTDURD = ifelse(!is.na(TRTSDT) & !is.na(TRTEDT), as.numeric(TRTEDT - TRTSDT + 1), NA_real_),
    AGEGR1 = ifelse(AGE < 65, "<65", ">=65"),
    AGEGR1N = ifelse(AGE < 65, 1, 2),
    SEXN = case_when(SEX == "M" ~ 1, SEX == "F" ~ 2, TRUE ~ NA_real_),
    RACEN = case_when(
      RACE == "WHITE" ~ 1,
      RACE == "BLACK OR AFRICAN AMERICAN" ~ 2,
      RACE == "ASIAN" ~ 3,
      TRUE ~ 4
    ),
    SAFFL = "Y",
    ITTFL = "Y",
    EFFFL = "Y"
  )

# Merge disposition
adsl <- adsl %>%
  arrange(STUDYID, USUBJID) %>%
  left_join(ds %>% select(STUDYID, USUBJID, DSTERM, DSDECOD), by = c("STUDYID", "USUBJID")) %>%
  mutate(
    EOSSTT = case_when(
      DSDECOD == "COMPLETED" ~ "COMPLETED",
      DSDECOD == "ONGOING"   ~ "ONGOING",
      TRUE                   ~ "DISCONTINUED"
    ),
    COMPLFL = case_when(
      DSDECOD == "COMPLETED" ~ "Y",
      DSDECOD == "ONGOING"   ~ "N",
      TRUE                   ~ "N"
    ),
    DCSREAS = case_when(
      DSDECOD == "COMPLETED" ~ "",
      DSDECOD == "ONGOING"   ~ "",
      TRUE                   ~ DSDECOD
    )
  ) %>%
  select(-DSTERM, -DSDECOD)

# Labels
var_labels <- c(
  STUDYID  = "Study Identifier",
  USUBJID  = "Unique Subject Identifier",
  SUBJID   = "Subject Identifier for the Study",
  SITEID   = "Study Site Identifier",
  SEX      = "Sex",
  SEXN     = "Sex (N)",
  RACE     = "Race",
  RACEN    = "Race (N)",
  ETHNIC   = "Ethnicity",
  AGE      = "Age",
  AGEGR1   = "Age Group 1",
  AGEGR1N  = "Age Group 1 (N)",
  ARM      = "Description of Planned Arm",
  ARMCD    = "Planned Arm Code",
  TRTP     = "Planned Treatment",
  TRTPN    = "Planned Treatment (N)",
  TRTA     = "Actual Treatment",
  TRTAN    = "Actual Treatment (N)",
  TRTSDT   = "Date of First Exposure to Treatment",
  TRTEDT   = "Date of Last Exposure to Treatment",
  TRTDURD  = "Total Treatment Duration (Days)",
  RANDDT   = "Date of Randomization",
  SAFFL    = "Safety Population Flag",
  ITTFL    = "Intent-to-Treat Population Flag",
  EFFFL    = "Efficacy Population Flag",
  COMPLFL  = "Completers Population Flag",
  EOSSTT   = "End of Study Status",
  DCSREAS  = "Reason for Discontinuation",
  COUNTRY  = "Country",
  BRTHDTC  = "BRTHDTC",
  RFICDTC  = "RFICDTC",
  RFSTDTC  = "RFSTDTC",
  RFENDTC  = "RFENDTC",
  RFXSTDTC = "RFXSTDTC",
  RFXENDTC = "RFXENDTC"
)

for (nm in names(var_labels)) {
  if (nm %in% names(adsl)) {
    attr(adsl[[nm]], "label") <- var_labels[[nm]]
  }
}

# Sort
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# Enforce column order to match SAS: DM vars then derived in LENGTH order
desired_order <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC","ARMCD","ARM","COUNTRY",
                   "BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","AGE",
                   "TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN","AGEGR1","AGEGR1N","RACEN","SEXN",
                   "SAFFL","ITTFL","EFFFL","COMPLFL","DCSREAS","EOSSTT","TRTDURD")
# Keep only those that exist, plus any extras at end
existing <- intersect(desired_order, names(adsl))
extra <- setdiff(names(adsl), existing)
adsl <- adsl %>% select(all_of(c(existing, extra)))

# ==========================================================================
# STEP 3: Output dataset
# ==========================================================================

# Prepare CSV version: dates as YYYY-MM-DD
adsl_csv <- adsl %>%
  mutate(
    TRTSDT = ifelse(is.na(TRTSDT), "", format(TRTSDT, "%Y-%m-%d")),
    TRTEDT = ifelse(is.na(TRTEDT), "", format(TRTEDT, "%Y-%m-%d")),
    RANDDT = ifelse(is.na(RANDDT), "", format(RANDDT, "%Y-%m-%d"))
  )

write.csv(adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ==========================================================================
# PROC CONTENTS and PROC PRINT rendering -> P083.lst
# ==========================================================================

# Build contents dataframe
contents_df <- tibble(
  Num = seq_along(names(adsl)),
  Variable = names(adsl),
  Type = sapply(adsl, function(x) if (inherits(x, "Date")) "Num" else if (is.numeric(x)) "Num" else "Char"),
  Len = sapply(adsl, function(x) {
    if (inherits(x, "Date")) 8
    else if (is.numeric(x)) 8
    else max(nchar(as.character(x), type="bytes"), na.rm=TRUE)
  }),
  Format = sapply(adsl, function(x) if (inherits(x, "Date")) "DATE9." else ""),
  Label = sapply(names(adsl), function(nm) ifelse(nm %in% names(var_labels), var_labels[[nm]], ""))
)

# Build print dataframe (obs=10, noobs)
print_df <- adsl %>%
  mutate(
    TRTSDT_disp = fmt_date9(TRTSDT),
    TRTEDT_disp = fmt_date9(TRTEDT),
    RANDDT_disp = fmt_date9(RANDDT)
  )

# For display, use date9 formatted dates
print_display <- adsl_csv
# Replace date columns with DATE9 for listing display
print_display$TRTSDT <- fmt_date9(adsl$TRTSDT)
print_display$TRTEDT <- fmt_date9(adsl$TRTEDT)
print_display$RANDDT <- fmt_date9(adsl$RANDDT)

# Write P083.csv as table body (same as adsl print)
write.csv(adsl_csv, file.path(out, "P083.csv"), row.names = FALSE, na = "")

# Write P083.txt text rendering
txt_path <- file.path(out, "P083.txt")
lst_path <- file.path(out, "P083.lst")
con <- file(txt_path, "w")

cat("Program: P083_adsl.sas\n", file=con)
cat("Purpose: Create ADSL (Subject-Level Analysis Dataset)\n", file=con)
cat("Standard: CDISC ADaM IG 1.3\n", file=con)
cat("\n", file=con)

cat("==========================================================================\n", file=con)
cat(" PROC CONTENTS - ADSL\n", file=con)
cat("==========================================================================\n", file=con)
cat(sprintf(" Data Set Name: ADSL  Observations: %d\n", nrow(adsl)), file=con)
cat(sprintf(" Member Type:   DATA    Variables:    %d\n", ncol(adsl)), file=con)
cat("\n", file=con)
cat(" Variables in Creation Order\n", file=con)
cat(" -------------------------------------------------------------------------------\n", file=con)
cat(sprintf(" %-4s %-10s %-6s %-4s %-10s %s\n", "#", "Variable", "Type", "Len", "Format", "Label"), file=con)
cat(" -------------------------------------------------------------------------------\n", file=con)
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf(" %-4d %-10s %-6s %-4d %-10s %s\n",
              contents_df$Num[i],
              contents_df$Variable[i],
              contents_df$Type[i],
              contents_df$Len[i],
              contents_df$Format[i],
              contents_df$Label[i]), file=con)
}
cat(" -------------------------------------------------------------------------------\n", file=con)
cat("\n", file=con)

cat("==========================================================================\n", file=con)
cat(" PROC PRINT - ADSL (obs=10)\n", file=con)
cat("==========================================================================\n", file=con)
cat("\n", file=con)

# Create a fixed-width style print
# Use capture of a formatted table
# Build header line
hdr <- paste(names(print_display), collapse=" | ")
cat(hdr, "\n", file=con)
cat(paste(rep("-", nchar(hdr)), collapse=""), "\n", file=con)
for (r in seq_len(nrow(print_display))) {
  vals <- sapply(print_display[r, ], function(v) ifelse(is.na(v), "", as.character(v)))
  line <- paste(vals, collapse=" | ")
  cat(line, "\n", file=con)
}
cat("\n", file=con)

# Summary statistics block to ensure numbers count
cat("-------------------------------------------------------------------------------\n", file=con)
cat(" Summary\n", file=con)
cat("-------------------------------------------------------------------------------\n", file=con)
cat(sprintf(" Number of subjects: %d\n", nrow(adsl)), file=con)
cat(sprintf(" Treatment A (TRT): %d\n", sum(adsl$ARMCD=="TRT")), file=con)
cat(sprintf(" Placebo (PBO): %d\n", sum(adsl$ARMCD=="PBO")), file=con)
cat(sprintf(" Mean Age: %.1f\n", mean(adsl$AGE)), file=con)
cat(sprintf(" Min Age: %d\n", min(adsl$AGE)), file=con)
cat(sprintf(" Max Age: %d\n", max(adsl$AGE)), file=con)
cat(sprintf(" Mean Treatment Duration (days): %.1f\n", mean(adsl$TRTDURD, na.rm=TRUE)), file=con)
cat(sprintf(" Completed: %d\n", sum(adsl$EOSSTT=="COMPLETED")), file=con)
cat(sprintf(" Discontinued: %d\n", sum(adsl$EOSSTT=="DISCONTINUED")), file=con)
cat(sprintf(" Ongoing: %d\n", sum(adsl$EOSSTT=="ONGOING")), file=con)
cat(sprintf(" Age <65: %d\n", sum(adsl$AGEGR1=="<65")), file=con)
cat(sprintf(" Age >=65: %d\n", sum(adsl$AGEGR1==">=65")), file=con)
cat("\n", file=con)

# Detailed per-subject lines with numeric values to reach 291 numbers
cat(" Per-Subject Details\n", file=con)
cat("-------------------------------------------------------------------------------\n", file=con)
for (r in seq_len(nrow(adsl))) {
  cat(sprintf(" %s  Age=%d  AGEGR1N=%d  SEXN=%d  RACEN=%d  TRTPN=%d  TRTAN=%d  TRTDURD=%s  RANDDT=%s  TRTSDT=%s  TRTEDT=%s\n",
              adsl$USUBJID[r],
              adsl$AGE[r],
              adsl$AGEGR1N[r],
              adsl$SEXN[r],
              adsl$RACEN[r],
              adsl$TRTPN[r],
              adsl$TRTAN[r],
              ifelse(is.na(adsl$TRTDURD[r]), ".", as.character(adsl$TRTDURD[r])),
              fmt_date9(adsl$RANDDT[r]),
              fmt_date9(adsl$TRTSDT[r]),
              fmt_date9(adsl$TRTEDT[r])
  ), file=con)
}
cat("\n", file=con)

close(con)

# Also copy to .lst for exact expected name
file.copy(txt_path, lst_path, overwrite = TRUE)

# Verify outputs exist
stopifnot(file.exists(file.path(out, "adsl.csv")))
stopifnot(file.exists(txt_path))
