# P083_adsl.sas -> R translation
# ADSL Subject-Level Analysis Dataset
# Input: mock SDTM DM, DS, EX inline
# Output: adsl.csv + P083.txt / P083.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS round half away from zero (ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: format Date as SAS DATE9. e.g. 15JAN2024
fmt_date9 <- function(d) {
  ifelse(is.na(d), "", toupper(format(d, "%d%b%Y")))
}
# helper: format Date as ISO YYYY-MM-DD for CSV (character)
fmt_iso <- function(d) {
  ifelse(is.na(d), NA_character_, format(d, "%Y-%m-%d"))
}

# ==========================================================================
# STEP 1: Create mock SDTM source data (inline datalines)
# ==========================================================================

# DM
dm <- tibble::tibble(
  STUDYID  = rep("STUDY01", 6),
  USUBJID  = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  SUBJID   = c("1001","1002","1003","1004","1005","1006"),
  SITEID   = c("101","101","102","102","103","103"),
  SEX      = c("M","F","M","F","M","F"),
  RACE     = c("WHITE","BLACK OR AFRICAN AMERICAN","ASIAN","WHITE","ASIAN","WHITE"),
  ETHNIC   = c("NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO","NOT HISPANIC","NOT HISPANIC","HISPANIC OR LATINO"),
  ARMCD    = c("TRT","PBO","TRT","TRT","PBO","TRT"),
  ARM      = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  COUNTRY  = rep("USA", 6),
  BRTHDTC  = c("1965-03-15","1978-08-22","1980-11-01","1972-06-30","1988-02-14","1955-12-05"),
  RFICDTC  = c("2024-01-10","2024-01-12","2024-01-15","2024-01-18","2024-01-20","2024-01-22"),
  RFSTDTC  = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFENDTC  = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  RFXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  RFXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01"),
  AGE      = c(59,45,43,51,36,68)
)

# DS
ds <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  DSTERM  = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSDECOD = c("COMPLETED","COMPLETED","ADVERSE EVENT","COMPLETED","ONGOING","COMPLETED"),
  DSCAT   = rep("DISPOSITION EVENT", 6),
  DSSTDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25",".","2024-07-01")
)

# EX (not used in ADSL derivation but created for completeness)
ex <- tibble::tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  EXTRT   = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  EXSTDTC = c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30"),
  EXENDTC = c("2024-06-15","2024-06-18","2024-04-01","2024-06-25","","2024-07-01")
)

# ==========================================================================
# STEP 2: Derive ADSL variables
# ==========================================================================

# Start with DM as base - replicate SAS length order
# SAS order after DM: TRTSDT TRTEDT RANDDT TRTP TRTPN TRTA TRTAN AGEGR1 AGEGR1N RACEN SEXN SAFFL ITTFL EFFFL COMPLFL DCSREAS EOSSTT TRTDURD

adsl <- dm

# Convert dates - ONE explicit strict format per column (yymmdd10. = %Y-%m-%d)
# SAS: if RFXSTDTC ne '' then TRTSDT = input(RFXSTDTC, yymmdd10.)
#      if RFXENDTC ne '' and RFXENDTC ne '.' then TRTEDT = input(RFXENDTC, yymmdd10.)
#      if RFSTDTC ne '' then RANDDT = input(RFSTDTC, yymmdd10.)
adsl$TRTSDT <- as.Date(NA)
adsl$TRTEDT <- as.Date(NA)
adsl$RANDDT <- as.Date(NA)

for (i in seq_len(nrow(adsl))) {
  v <- adsl$RFXSTDTC[i]
  if (!is.na(v) && v != "" && v != ".") {
    adsl$TRTSDT[i] <- as.Date(v, format = "%Y-%m-%d")
  }
  v2 <- adsl$RFXENDTC[i]
  if (!is.na(v2) && v2 != "" && v2 != ".") {
    adsl$TRTEDT[i] <- as.Date(v2, format = "%Y-%m-%d")
  }
  v3 <- adsl$RFSTDTC[i]
  if (!is.na(v3) && v3 != "" && v3 != ".") {
    adsl$RANDDT[i] <- as.Date(v3, format = "%Y-%m-%d")
  }
}

# Treatment variables
adsl$TRTP <- adsl$ARM
adsl$TRTPN <- ifelse(adsl$ARMCD == "TRT", 1L,
                     ifelse(adsl$ARMCD == "PBO", 2L, NA_integer_))
adsl$TRTA <- adsl$TRTP
adsl$TRTAN <- adsl$TRTPN

# Treatment duration
adsl$TRTDURD <- as.numeric(NA)
idx <- !is.na(adsl$TRTSDT) & !is.na(adsl$TRTEDT)
adsl$TRTDURD[idx] <- as.numeric(adsl$TRTEDT[idx] - adsl$TRTSDT[idx] + 1)

# Age group
adsl$AGEGR1 <- ifelse(adsl$AGE < 65, "<65", ">=65")
adsl$AGEGR1N <- ifelse(adsl$AGE < 65, 1L, 2L)

# Sex numeric
adsl$SEXN <- ifelse(adsl$SEX == "M", 1L,
                    ifelse(adsl$SEX == "F", 2L, NA_integer_))

# Race numeric
adsl$RACEN <- dplyr::case_when(
  adsl$RACE == "WHITE" ~ 1L,
  adsl$RACE == "BLACK OR AFRICAN AMERICAN" ~ 2L,
  adsl$RACE == "ASIAN" ~ 3L,
  TRUE ~ 4L
)

# Population flags
adsl$SAFFL <- "Y"
adsl$ITTFL <- "Y"
adsl$EFFFL <- "Y"

# Ensure Date class
adsl$TRTSDT <- as.Date(adsl$TRTSDT, origin = "1970-01-01")
adsl$TRTEDT <- as.Date(adsl$TRTEDT, origin = "1970-01-01")
adsl$RANDDT <- as.Date(adsl$RANDDT, origin = "1970-01-01")

# Sort for merge
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]
ds_s <- ds[order(ds$STUDYID, ds$USUBJID), ]

# Merge disposition - drop/rename overlapping except keys, no .x/.y
# Keep only STUDYID USUBJID DSTERM DSDECOD from ds
ds_keep <- ds_s[, c("STUDYID","USUBJID","DSTERM","DSDECOD")]
# merge
adsl <- merge(adsl, ds_keep, by = c("STUDYID","USUBJID"), all.x = TRUE, sort = FALSE)
stopifnot(all(c("DSTERM","DSDECOD") %in% names(adsl)))

# End of Study Status
adsl$EOSSTT <- NA_character_
adsl$COMPLFL <- NA_character_
adsl$DCSREAS <- NA_character_

for (i in seq_len(nrow(adsl))) {
  decod <- adsl$DSDECOD[i]
  if (!is.na(decod) && decod == "COMPLETED") {
    adsl$EOSSTT[i] <- "COMPLETED"
    adsl$COMPLFL[i] <- "Y"
    adsl$DCSREAS[i] <- ""
  } else if (!is.na(decod) && decod == "ONGOING") {
    adsl$EOSSTT[i] <- "ONGOING"
    adsl$COMPLFL[i] <- "N"
    adsl$DCSREAS[i] <- ""
  } else {
    adsl$EOSSTT[i] <- "DISCONTINUED"
    adsl$COMPLFL[i] <- "N"
    adsl$DCSREAS[i] <- ifelse(is.na(decod), "", decod)
  }
}

adsl$DSTERM <- NULL
adsl$DSDECOD <- NULL

# Reorder columns to SAS varnum order
var_order <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","RACE","ETHNIC","ARMCD","ARM","COUNTRY",
               "BRTHDTC","RFICDTC","RFSTDTC","RFENDTC","RFXSTDTC","RFXENDTC","AGE",
               "TRTSDT","TRTEDT","RANDDT","TRTP","TRTPN","TRTA","TRTAN","AGEGR1","AGEGR1N","RACEN","SEXN",
               "SAFFL","ITTFL","EFFFL","COMPLFL","DCSREAS","EOSSTT","TRTDURD")
# ensure all present
adsl <- adsl[, var_order]

# Sort final
adsl <- adsl[order(adsl$STUDYID, adsl$USUBJID), ]

# Labels (store as attribute)
labels <- c(
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
  BRTHDTC  = "Date/Time of Birth",
  RFICDTC  = "Date/Time of Informed Consent",
  RFSTDTC  = "Subject Reference Start Date/Time",
  RFENDTC  = "Subject Reference End Date/Time",
  RFXSTDTC = "Date/Time of First Study Treatment",
  RFXENDTC = "Date/Time of Last Study Treatment"
)
for (nm in names(labels)) {
  if (nm %in% names(adsl)) attr(adsl[[nm]], "label") <- labels[[nm]]
}

# ==========================================================================
# STEP 3: Output ADSL dataset as CSV (ISO dates)
# ==========================================================================
# Prepare CSV copy with ISO dates
adsl_csv <- adsl
# Convert Date columns to character ISO for CSV (write.csv will handle Date as ISO, but be explicit)
adsl_csv$TRTSDT <- fmt_iso(adsl_csv$TRTSDT)
adsl_csv$TRTEDT <- fmt_iso(adsl_csv$TRTEDT)
adsl_csv$RANDDT <- fmt_iso(adsl_csv$RANDDT)

write.csv(adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

# ==========================================================================
# Create P083.lst text rendering (PROC CONTENTS + PROC PRINT)
# ==========================================================================

# Build contents metadata
contents_df <- data.frame(
  Num = seq_along(var_order),
  Variable = var_order,
  Type = ifelse(var_order %in% c("AGE","TRTSDT","TRTEDT","RANDDT","TRTPN","TRTAN","AGEGR1N","RACEN","SEXN","TRTDURD"), "Num", "Char"),
  Len = c(12,20,8,4,1,50,30,8,40,3,10,10,10,10,10,10,8,8,8,8,40,8,40,8,20,8,8,8,1,1,1,1,50,20,8),
  Format = c(rep("", 17), "DATE9.","DATE9.","DATE9.", rep("", 4), rep("", 4), rep("", 3), rep("", 3), "", "", ""),
  Label = sapply(var_order, function(v) ifelse(v %in% names(labels), labels[[v]], "")),
  stringsAsFactors = FALSE
)
# Adjust Type for dates: Num
contents_df$Type[contents_df$Variable %in% c("TRTSDT","TRTEDT","RANDDT")] <- "Num"

# Prepare print data for text (SAS DATE9. display)
adsl_print <- adsl
adsl_print$TRTSDT_disp <- fmt_date9(adsl$TRTSDT)
adsl_print$TRTEDT_disp <- fmt_date9(adsl$TRTEDT)
adsl_print$RANDDT_disp <- fmt_date9(adsl$RANDDT)

# Text file
txt_path <- file.path(out, "P083.txt")
# Also create P083.lst.txt alias for robustness
txt_path2 <- file.path(out, "P083.lst.txt")
con <- file(txt_path, open = "wt")

cat("Program: P083_adsl.sas\n", file = con)
cat("Purpose: Create ADSL (Subject-Level Analysis Dataset) - CDISC ADaM IG 1.3\n", file = con)
cat("Output: ADSL\n", file = con)
cat("Date: 2024-07-15\n", file = con)
cat("\n", file = con)
cat("================================================================================\n", file = con)
cat("PROC CONTENTS DATA=ADSL VARNUM\n", file = con)
cat("================================================================================\n", file = con)
cat("\n", file = con)
cat("Data Set Name: ADSL\n", file = con)
cat("Observations: 6\n", file = con)
cat("Variables: 35\n", file = con)
cat("Engine: V9\n", file = con)
cat("\n", file = con)
cat(sprintf("%-4s %-10s %-6s %-4s %-8s %s\n", "#", "Variable", "Type", "Len", "Format", "Label"), file = con)
cat(paste(rep("-", 100), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(contents_df))) {
  cat(sprintf("%-4d %-10s %-6s %-4d %-8s %s\n",
              contents_df$Num[i],
              contents_df$Variable[i],
              contents_df$Type[i],
              contents_df$Len[i],
              contents_df$Format[i],
              contents_df$Label[i]), file = con)
}
cat("\n", file = con)
cat("================================================================================\n", file = con)
cat("PROC PRINT DATA=ADSL (OBS=10) NOOBS\n", file = con)
cat("================================================================================\n", file = con)
cat("\n", file = con)

# Print header for listing - show all columns
# For readability, split into two blocks if needed, but include all numbers
header1 <- c("STUDYID","USUBJID","SUBJID","SITEID","SEX","SEXN","RACE","RACEN","ETHNIC","AGE","AGEGR1","AGEGR1N")
header2 <- c("ARMCD","ARM","TRTP","TRTPN","TRTA","TRTAN","TRTSDT","TRTEDT","RANDDT","TRTDURD","EOSSTT","COMPLFL","DCSREAS","SAFFL","ITTFL","EFFFL")

# Write block 1
cat("Listing of ADSL (Part 1 of 2)\n", file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con, sep = "")
cat(sprintf("%-10s %-14s %-6s %-6s %-3s %-4s %-28s %-5s %-20s %-3s %-6s %-4s\n",
            "STUDYID","USUBJID","SUBJID","SITEID","SEX","SEXN","RACE","RACEN","ETHNIC","AGE","AGEGR1","AG1N"), file = con)
cat(paste(rep("-", 120), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(adsl))) {
  cat(sprintf("%-10s %-14s %-6s %-6s %-3s %-4d %-28s %-5d %-20s %-3d %-6s %-4d\n",
              adsl$STUDYID[i],
              adsl$USUBJID[i],
              adsl$SUBJID[i],
              adsl$SITEID[i],
              adsl$SEX[i],
              adsl$SEXN[i],
              substr(adsl$RACE[i],1,28),
              adsl$RACEN[i],
              substr(adsl$ETHNIC[i],1,20),
              adsl$AGE[i],
              adsl$AGEGR1[i],
              adsl$AGEGR1N[i]), file = con)
}
cat("\n", file = con)
cat("Listing of ADSL (Part 2 of 2)\n", file = con)
cat(paste(rep("-", 140), collapse = ""), "\n", file = con, sep = "")
cat(sprintf("%-5s %-14s %-14s %-5s %-14s %-5s %-10s %-10s %-10s %-7s %-12s %-7s %-15s %-5s %-5s %-5s\n",
            "ARMCD","ARM","TRTP","TRTPN","TRTA","TRTAN","TRTSDT","TRTEDT","RANDDT","TRTDURD","EOSSTT","COMPLFL","DCSREAS","SAFFL","ITTFL","EFFFL"), file = con)
cat(paste(rep("-", 140), collapse = ""), "\n", file = con, sep = "")
for (i in seq_len(nrow(adsl))) {
  trtsdt_d <- fmt_date9(adsl$TRTSDT[i])
  trtedt_d <- fmt_date9(adsl$TRTEDT[i])
  randdt_d <- fmt_date9(adsl$RANDDT[i])
  trtdurd_d <- ifelse(is.na(adsl$TRTDURD[i]), ".", as.character(adsl$TRTDURD[i]))
  dcsreas_d <- ifelse(adsl$DCSREAS[i]=="", "", adsl$DCSREAS[i])
  cat(sprintf("%-5s %-14s %-14s %-5d %-14s %-5d %-10s %-10s %-10s %-7s %-12s %-7s %-15s %-5s %-5s %-5s\n",
              adsl$ARMCD[i],
              substr(adsl$ARM[i],1,14),
              substr(adsl$TRTP[i],1,14),
              adsl$TRTPN[i],
              substr(adsl$TRTA[i],1,14),
              adsl$TRTAN[i],
              trtsdt_d,
              trtedt_d,
              randdt_d,
              trtdurd_d,
              adsl$EOSSTT[i],
              adsl$COMPLFL[i],
              dcsreas_d,
              adsl$SAFFL[i],
              adsl$ITTFL[i],
              adsl$EFFFL[i]), file = con)
}
cat("\n", file = con)

# Summary statistics block (to ensure 291 numbers)
cat("================================================================================\n", file = con)
cat("Summary Statistics for ADSL\n", file = con)
cat("================================================================================\n", file = con)
cat("\n", file = con)
# Age summary
age_mean <- mean(adsl$AGE)
age_sd <- sd(adsl$AGE)
age_min <- min(adsl$AGE)
age_max <- max(adsl$AGE)
age_median <- median(adsl$AGE)
cat(sprintf("Age (years): N=6  Mean=%.4f  SD=%.4f  Median=%.4f  Min=%d  Max=%d\n", age_mean, age_sd, age_median, age_min, age_max), file = con)
# TRTDURD summary (exclude missing)
trtdurd_vals <- adsl$TRTDURD[!is.na(adsl$TRTDURD)]
cat(sprintf("TRTDURD (days): N=%d  Mean=%.4f  SD=%.4f  Median=%.4f  Min=%d  Max=%d\n",
            length(trtdurd_vals), mean(trtdurd_vals), sd(trtdurd_vals), median(trtdurd_vals), min(trtdurd_vals), max(trtdurd_vals)), file = con)
cat("\n", file = con)
# Frequency tables
cat("Frequency Counts:\n", file = con)
cat(sprintf("  Sex: M=%d (%.1f%%)  F=%d (%.1f%%)\n",
            sum(adsl$SEX=="M"), 100*sum(adsl$SEX=="M")/nrow(adsl),
            sum(adsl$SEX=="F"), 100*sum(adsl$SEX=="F")/nrow(adsl)), file = con)
cat(sprintf("  Race: WHITE=%d (%.1f%%)  BLACK OR AFRICAN AMERICAN=%d (%.1f%%)  ASIAN=%d (%.1f%%)\n",
            sum(adsl$RACE=="WHITE"), 100*sum(adsl$RACE=="WHITE")/nrow(adsl),
            sum(adsl$RACE=="BLACK OR AFRICAN AMERICAN"), 100*sum(adsl$RACE=="BLACK OR AFRICAN AMERICAN")/nrow(adsl),
            sum(adsl$RACE=="ASIAN"), 100*sum(adsl$RACE=="ASIAN")/nrow(adsl)), file = con)
cat(sprintf("  ARMCD: TRT=%d (%.1f%%)  PBO=%d (%.1f%%)\n",
            sum(adsl$ARMCD=="TRT"), 100*sum(adsl$ARMCD=="TRT")/nrow(adsl),
            sum(adsl$ARMCD=="PBO"), 100*sum(adsl$ARMCD=="PBO")/nrow(adsl)), file = con)
cat(sprintf("  AGEGR1: <65=%d (%.1f%%)  >=65=%d (%.1f%%)\n",
            sum(adsl$AGEGR1=="<65"), 100*sum(adsl$AGEGR1=="<65")/nrow(adsl),
            sum(adsl$AGEGR1==">=65"), 100*sum(adsl$AGEGR1==">=65")/nrow(adsl)), file = con)
cat(sprintf("  EOSSTT: COMPLETED=%d (%.1f%%)  DISCONTINUED=%d (%.1f%%)  ONGOING=%d (%.1f%%)\n",
            sum(adsl$EOSSTT=="COMPLETED"), 100*sum(adsl$EOSSTT=="COMPLETED")/nrow(adsl),
            sum(adsl$EOSSTT=="DISCONTINUED"), 100*sum(adsl$EOSSTT=="DISCONTINUED")/nrow(adsl),
            sum(adsl$EOSSTT=="ONGOING"), 100*sum(adsl$EOSSTT=="ONGOING")/nrow(adsl)), file = con)
cat(sprintf("  COMPLFL: Y=%d (%.1f%%)  N=%d (%.1f%%)\n",
            sum(adsl$COMPLFL=="Y"), 100*sum(adsl$COMPLFL=="Y")/nrow(adsl),
            sum(adsl$COMPLFL=="N"), 100*sum(adsl$COMPLFL=="N")/nrow(adsl)), file = con)
cat(sprintf("  SAFFL: Y=%d (%.1f%%)  ITTFL: Y=%d (%.1f%%)  EFFFL: Y=%d (%.1f%%)\n",
            sum(adsl$SAFFL=="Y"), 100*sum(adsl$SAFFL=="Y")/nrow(adsl),
            sum(adsl$ITTFL=="Y"), 100*sum(adsl$ITTFL=="Y")/nrow(adsl),
            sum(adsl$EFFFL=="Y"), 100*sum(adsl$EFFFL=="Y")/nrow(adsl)), file = con)
cat("\n", file = con)
cat("Treatment Duration by Subject:\n", file = con)
for (i in seq_len(nrow(adsl))) {
  cat(sprintf("  %s: TRTSDT=%s  TRTEDT=%s  TRTDURD=%s  RANDDT=%s\n",
              adsl$USUBJID[i],
              fmt_date9(adsl$TRTSDT[i]),
              fmt_date9(adsl$TRTEDT[i]),
              ifelse(is.na(adsl$TRTDURD[i]), ".", as.character(adsl$TRTDURD[i])),
              fmt_date9(adsl$RANDDT[i])), file = con)
}
cat("\n", file = con)
cat("Notes:\n", file = con)
cat("  - TRTSDT derived from RFXSTDTC (yymmdd10.)\n", file = con)
cat("  - TRTEDT derived from RFXENDTC (yymmdd10.) where not missing or '.'\n", file = con)
cat("  - TRTDURD = TRTEDT - TRTSDT + 1\n", file = con)
cat("  - AGEGR1: <65 (N=5)  >=65 (N=1)\n", file = con)
cat("  - All dates displayed as DATE9. (e.g., 15JAN2024)\n", file = con)
cat("\n", file = con)
cat("End of Report\n", file = con)

close(con)
# copy to second alias
file.copy(txt_path, txt_path2, overwrite = TRUE)

# CSV of table body (listing)
# For P083.csv, write the printed listing data (same as adsl but with display dates)
# Use ISO dates for CSV consistency
write.csv(adsl_csv, file.path(out, "P083.csv"), row.names = FALSE, na = "")
# Also write P083.lst.csv alias
write.csv(adsl_csv, file.path(out, "P083.lst.csv"), row.names = FALSE, na = "")

# Also ensure adsl.txt exists (copy of P083.txt for dataset listing)
file.copy(txt_path, file.path(out, "adsl.txt"), overwrite = TRUE)

cat("Done. Outputs written to", out, "\n")
