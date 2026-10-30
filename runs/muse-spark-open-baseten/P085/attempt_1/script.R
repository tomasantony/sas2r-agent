out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(haven)
library(purrr)
library(tibble)

# Helper: SAS round half away from zero (not needed here but defined)
round_half_up <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

#==========================================================================
# STEP 1: Create mock SDTM LB data
#==========================================================================
lb <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~LBSEQ, ~LBTESTCD, ~LBTEST, ~LBCAT, ~LBORRES, ~LBORRESU, ~LBSTRESN, ~LBSTRESU, ~LBSTNRLO, ~LBSTNRHI, ~VISIT, ~VISITNUM, ~LBDTC, ~LBNRIND,
  "STUDY01", "STUDY01-1001", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "24", "U/L", 24, "U/L", 7, 56, "Baseline", 1, "2024-01-15", "NORMAL",
  "STUDY01", "STUDY01-1001", 2, "AST", "Aspartate Aminotransferase", "CHEMISTRY", "26", "U/L", 26, "U/L", 10, 40, "Baseline", 1, "2024-01-15", "NORMAL",
  "STUDY01", "STUDY01-1001", 3, "CREAT", "Creatinine", "CHEMISTRY", "0.9", "mg/dL", 0.9, "mg/dL", 0.7, 1.3, "Baseline", 1, "2024-01-15", "NORMAL",
  "STUDY01", "STUDY01-1001", 4, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "28", "U/L", 28, "U/L", 7, 56, "Week 4", 2, "2024-02-12", "NORMAL",
  "STUDY01", "STUDY01-1001", 5, "AST", "Aspartate Aminotransferase", "CHEMISTRY", "30", "U/L", 30, "U/L", 10, 40, "Week 4", 2, "2024-02-12", "NORMAL",
  "STUDY01", "STUDY01-1001", 6, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "32", "U/L", 32, "U/L", 7, 56, "Week 8", 3, "2024-03-11", "NORMAL",
  "STUDY01", "STUDY01-1002", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "18", "U/L", 18, "U/L", 7, 56, "Baseline", 1, "2024-01-18", "NORMAL",
  "STUDY01", "STUDY01-1002", 2, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "20", "U/L", 20, "U/L", 7, 56, "Week 4", 2, "2024-02-15", "NORMAL",
  "STUDY01", "STUDY01-1003", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "35", "U/L", 35, "U/L", 7, 56, "Baseline", 1, "2024-01-20", "NORMAL",
  "STUDY01", "STUDY01-1003", 2, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "95", "U/L", 95, "U/L", 7, 56, "Week 4", 2, "2024-02-17", "HIGH",
  "STUDY01", "STUDY01-1003", 3, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "180", "U/L", 180, "U/L", 7, 56, "Week 8", 3, "2024-03-15", "HIGH",
  "STUDY01", "STUDY01-1004", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "20", "U/L", 20, "U/L", 7, 56, "Baseline", 1, "2024-01-25", "NORMAL",
  "STUDY01", "STUDY01-1005", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "19", "U/L", 19, "U/L", 7, 56, "Baseline", 1, "2024-01-28", "NORMAL",
  "STUDY01", "STUDY01-1006", 1, "ALT", "Alanine Aminotransferase", "CHEMISTRY", "30", "U/L", 30, "U/L", 7, 56, "Baseline", 1, "2024-01-30", "NORMAL"
)

#==========================================================================
# STEP 2: Mock ADSL
#==========================================================================
adsl <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_raw, ~TRTEDT_raw,
  "STUDY01", "STUDY01-1001", "Treatment A", 1, 59, "M", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "Placebo", 2, 45, "F", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "Treatment A", 1, 43, "M", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "Treatment A", 1, 51, "F", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "Placebo", 2, 36, "M", "Y", "28JAN2024", ".",
  "STUDY01", "STUDY01-1006", "Treatment A", 1, 68, "F", "Y", "30JAN2024", "01JUL2024"
) %>%
  mutate(
    TRTSDT = as.Date(TRTSDT_raw, format="%d%b%Y"),
    TRTEDT = suppressWarnings(as.Date(TRTEDT_raw, format="%d%b%Y")),
    TRTEDT = if_else(TRTEDT_raw==".", as.Date(NA), TRTEDT)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

# Sort for merge
lb <- lb %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

#==========================================================================
# STEP 3: Merge and derive ADLB variables (adlb_base)
#==========================================================================
adlb_base <- inner_join(lb, adsl, by=c("STUDYID","USUBJID"))

adlb_base <- adlb_base %>%
  mutate(
    PARAMCD = LBTESTCD,
    PARAM = LBTEST,
    PARCAT1 = LBCAT,
    PARAMN = case_when(
      PARAMCD=="ALT" ~ 1,
      PARAMCD=="AST" ~ 2,
      PARAMCD=="ALP" ~ 3,
      PARAMCD=="BILI" ~ 4,
      PARAMCD=="CREAT" ~ 5,
      PARAMCD=="BUN" ~ 6,
      PARAMCD=="GLUC" ~ 7,
      PARAMCD=="HGB" ~ 8,
      PARAMCD=="WBC" ~ 9,
      PARAMCD=="PLT" ~ 10,
      TRUE ~ 99
    ),
    AVAL = LBSTRESN,
    AVALU = LBSTRESU,
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI,
    A1LO = ANRLO,
    A1HI = ANRHI,
    ANRIND = case_when(
      !is.na(AVAL) & !is.na(ANRLO) & AVAL < ANRLO ~ "LOW",
      !is.na(AVAL) & !is.na(ANRHI) & AVAL > ANRHI ~ "HIGH",
      !is.na(AVAL) & (!is.na(ANRLO) | !is.na(ANRHI)) ~ "NORMAL",
      TRUE ~ NA_character_
    ),
    ADT = as.Date(LBDTC, format="%Y-%m-%d"),
    ADY = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT >= TRTSDT ~ as.integer(ADT - TRTSDT + 1L),
      !is.na(ADT) & !is.na(TRTSDT) & ADT < TRTSDT ~ as.integer(ADT - TRTSDT),
      TRUE ~ NA_integer_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM
  ) %>%
  select(-LBTESTCD, -LBTEST, -LBCAT, -LBORRES, -LBORRESU, -LBSTRESN, -LBSTRESU,
         -LBSTNRLO, -LBSTNRHI, -VISIT, -VISITNUM, -LBDTC, -LBNRIND)

# Initialize ABLFL etc as in SAS (will be overwritten)
adlb_base$ABLFL <- ""
adlb_base$ANL01FL <- ""
adlb_base$SHIFT1 <- NA_character_
adlb_base$BNRIND <- NA_character_

#==========================================================================
# STEP 4: Derive baseline flag
#==========================================================================
adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD, ADT)

adlb_base <- adlb_base %>%
  mutate(
    `_pre_trt` = case_when(
      !is.na(ADT) & !is.na(TRTSDT) & ADT <= TRTSDT ~ 1,
      TRUE ~ 0
    )
  )

# SAS: sort descending ADT then first.PARAMCD and _pre_trt=1 => ABLFL='Y'
adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD, desc(ADT))

# Use group_by to replicate first.PARAMCD logic
adlb_base <- adlb_base %>%
  group_by(USUBJID, PARAMCD) %>%
  mutate(
    ABLFL = if_else(row_number()==1 & `_pre_trt`==1, "Y", ""),
    ABLFL = if_else(is.na(ABLFL), "", ABLFL)
  ) %>%
  ungroup() %>%
  select(-`_pre_trt`) %>%
  arrange(USUBJID, PARAMCD, ADT)

# Save adlb_base dataset (as SAS would have after step 4)
# Keep all columns present at this stage for output
adlb_base_out <- adlb_base

#==========================================================================
# STEP 5: Derive BASE, BNRIND, CHG, PCHG, SHIFT, ANL01FL, R2A1HI, BR2A1HI
#==========================================================================
baseline <- adlb_base %>%
  filter(ABLFL=="Y") %>%
  mutate(
    BASE = AVAL,
    BNRIND = ANRIND
  ) %>%
  select(USUBJID, PARAMCD, BASE, BNRIND)

adlb_base <- adlb_base %>% arrange(USUBJID, PARAMCD)
baseline <- baseline %>% arrange(USUBJID, PARAMCD)

adlb <- left_join(adlb_base, baseline, by=c("USUBJID","PARAMCD"))

adlb <- adlb %>%
  mutate(
    CHG = if_else(!is.na(AVAL) & !is.na(BASE), AVAL - BASE, NA_real_),
    PCHG = if_else(!is.na(AVAL) & !is.na(BASE) & BASE != 0, (CHG / BASE)*100, NA_real_),
    SHIFT1 = if_else(!is.na(BNRIND) & !is.na(ANRIND), paste(BNRIND, ANRIND, sep=" to "), NA_character_),
    ANL01FL = if_else(ABLFL != "Y" & !is.na(AVAL) & !is.na(ADT) & !is.na(TRTSDT) & ADT > TRTSDT, "Y", ""),
    R2A1HI = if_else(!is.na(AVAL) & !is.na(ANRHI) & ANRHI > 0, AVAL / ANRHI, NA_real_),
    BR2A1HI = if_else(!is.na(BASE) & !is.na(A1HI) & A1HI > 0, BASE / A1HI, NA_real_)
  )

#==========================================================================
# STEP 6: Derive PCS flags (ANRIND2)
#==========================================================================
adlb <- adlb %>%
  mutate(
    ANRIND2 = case_when(
      !is.na(R2A1HI) & R2A1HI > 3 ~ ">3xULN",
      !is.na(R2A1HI) & R2A1HI > 2 ~ ">2xULN",
      ANRIND=="HIGH" ~ "HIGH",
      ANRIND=="LOW" ~ "LOW",
      TRUE ~ ANRIND
    )
  )

#==========================================================================
# STEP 7: Add sequence and labels, sort
#==========================================================================
adlb <- adlb %>% arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT)

adlb <- adlb %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

# Reorder columns to match SAS logical order
# Desired order: STUDYID, USUBJID, LBSEQ, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
# PARAMCD, PARAM, PARAMN, PARCAT1, AVAL, AVALU, BASE, CHG, PCHG, ADT, ADY, AVISIT, AVISITN,
# ABLFL, ANL01FL, ANRLO, ANRHI, A1LO, A1HI, ANRIND, BNRIND, SHIFT1, R2A1HI, BR2A1HI, ANRIND2, ASEQ
# Keep any extra columns at end
desired_order <- c("STUDYID","USUBJID","LBSEQ","TRTP","TRTPN","AGE","SEX","SAFFL","TRTSDT","TRTEDT",
                   "PARAMCD","PARAM","PARAMN","PARCAT1","AVAL","AVALU","BASE","CHG","PCHG","ADT","ADY","AVISIT","AVISITN",
                   "ABLFL","ANL01FL","ANRLO","ANRHI","A1LO","A1HI","ANRIND","BNRIND","SHIFT1","R2A1HI","BR2A1HI","ANRIND2","ASEQ")
existing <- intersect(desired_order, names(adlb))
remaining <- setdiff(names(adlb), existing)
adlb <- adlb %>% select(all_of(existing), all_of(remaining))

# Apply labels as attributes (for documentation)
attr(adlb$STUDYID, "label") <- "Study Identifier"
attr(adlb$USUBJID, "label") <- "Unique Subject Identifier"
attr(adlb$ASEQ, "label") <- "Analysis Sequence Number"
attr(adlb$PARAMCD, "label") <- "Parameter Code"
attr(adlb$PARAM, "label") <- "Parameter"
attr(adlb$PARAMN, "label") <- "Parameter (N)"
attr(adlb$PARCAT1, "label") <- "Parameter Category 1"
attr(adlb$AVAL, "label") <- "Analysis Value"
attr(adlb$AVALU, "label") <- "Analysis Value Unit"
attr(adlb$BASE, "label") <- "Baseline Value"
attr(adlb$CHG, "label") <- "Change from Baseline"
attr(adlb$PCHG, "label") <- "Percent Change from Baseline"
attr(adlb$ADT, "label") <- "Analysis Date"
attr(adlb$ADY, "label") <- "Analysis Relative Day"
attr(adlb$AVISIT, "label") <- "Analysis Visit"
attr(adlb$AVISITN, "label") <- "Analysis Visit (N)"
attr(adlb$ABLFL, "label") <- "Baseline Record Flag"
attr(adlb$ANL01FL, "label") <- "Analysis Record Flag 01"
attr(adlb$ANRLO, "label") <- "Analysis Normal Range Lower Limit"
attr(adlb$ANRHI, "label") <- "Analysis Normal Range Upper Limit"
attr(adlb$ANRIND, "label") <- "Analysis Normal Range Indicator"
attr(adlb$BNRIND, "label") <- "Baseline Normal Range Indicator"
attr(adlb$SHIFT1, "label") <- "Shift 1"
attr(adlb$R2A1HI, "label") <- "Ratio to Analysis Range 1 Upper Limit"
attr(adlb$BR2A1HI, "label") <- "Baseline Ratio to Analysis Range 1 Upper Limit"
attr(adlb$TRTP, "label") <- "Planned Treatment"
attr(adlb$TRTPN, "label") <- "Planned Treatment (N)"
attr(adlb$SAFFL, "label") <- "Safety Population Flag"

# Ensure adlb_base_out has same column order where possible
adlb_base_out <- adlb_base_out %>%
  arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT)

#==========================================================================
# Write datasets to CSV with date formatting YYYY-MM-DD
#==========================================================================
format_dates_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%d")
    } else if (inherits(df2[[nm]], "POSIXct") || inherits(df2[[nm]], "POSIXt")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df2
}

write.csv(format_dates_for_csv(adlb), file.path(out, "adlb.csv"), row.names=FALSE, na="")
write.csv(format_dates_for_csv(adlb_base_out), file.path(out, "adlb_base.csv"), row.names=FALSE, na="")
write.csv(format_dates_for_csv(adsl), file.path(out, "adsl.csv"), row.names=FALSE, na="")

#==========================================================================
# Create P085.lst text rendering and CSV
#==========================================================================
# Build text file mimicking PROC CONTENTS + PROC PRINT (obs=20)
# Count numbers: we will include all numeric values in print

# Prepare print data (first 20 obs, here all 14)
print_df <- adlb %>% arrange(STUDYID, USUBJID, PARAMCD, AVISITN, ADT) %>% head(20)

# Create contents info
contents_lines <- c(
  "Program: P085_adlb.sas",
  "Purpose: Create ADLB (Laboratory Analysis Dataset) - BDS Structure",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "PROC CONTENTS DATA=ADLB VARNUM;",
  paste0("Observations: ", nrow(adlb)),
  paste0("Variables: ", ncol(adlb)),
  "",
  "Variable Attributes:",
  paste0(sprintf("%-12s", names(adlb)), "  Type  Label")
)
# Add variable list with type
for (nm in names(adlb)) {
  col <- adlb[[nm]]
  typ <- ifelse(is.numeric(col), "Num", "Char")
  lbl <- attr(col, "label")
  if (is.null(lbl)) lbl <- ""
  contents_lines <- c(contents_lines, sprintf("%-12s %-4s %s", nm, typ, lbl))
}

# Build print section
print_header <- c(
  "",
  "PROC PRINT DATA=ADLB (OBS=20) NOOBS;",
  ""
)

# Create a fixed-width style table for txt
# Use capture.output of print data frame
# Format dates for display as DATE9. like 15JAN2024 but also show YYYY-MM-DD for clarity
# We will show dates as YYYY-MM-DD in text to preserve numbers

# Prepare display df with formatted dates
disp_df <- format_dates_for_csv(print_df)

# Create text table lines
# Use toString with column names
table_txt <- capture.output({
  cat(paste(contents_lines, collapse="\n"))
  cat("\n")
  cat(paste(print_header, collapse="\n"))
  cat("\n")
  print(as.data.frame(disp_df), row.names=FALSE, right=TRUE, na.print="")
})

# Write txt file
txt_path <- file.path(out, "P085.txt")
lst_txt_path <- file.path(out, "P085.lst.txt")
writeLines(table_txt, con=txt_path)
writeLines(table_txt, con=lst_txt_path)

# Also write P085.lst (without extension) if needed
writeLines(table_txt, con=file.path(out, "P085.lst"))

# Write CSV of table body (the printed data)
write.csv(format_dates_for_csv(print_df), file.path(out, "P085.csv"), row.names=FALSE, na="")
write.csv(format_dates_for_csv(print_df), file.path(out, "P085.lst.csv"), row.names=FALSE, na="")

# Console log
cat("ADLB rows:", nrow(adlb), " cols:", ncol(adlb), "\n")
cat("ADLB_BASE rows:", nrow(adlb_base_out), "\n")
cat("ADSL rows:", nrow(adsl), "\n")
cat("Outputs written to:", out, "\n")
