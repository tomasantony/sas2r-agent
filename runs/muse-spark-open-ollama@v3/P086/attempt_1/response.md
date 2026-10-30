```r
# P086_adtte.sas -> R translation
# ADTTE Time-to-Event Analysis Dataset
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(lubridate)
  library(readr)
  library(haven)
})

# Helper: SAS round half away from zero (not needed here but kept for fidelity)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

# ==========================================================================
# STEP 1: Create mock source data
# ==========================================================================

# ADSL with dates - TRTSDT etc are date9. in SAS
adsl <- data.frame(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP    = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN   = c(1,2,1,1,2,1),
  AGE     = c(59,45,43,51,36,68),
  AGEGR1  = c("<65","<65","<65","<65","<65",">=65"),
  SEX     = c("M","F","M","F","M","F"),
  RACE    = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL   = rep("Y", 6),
  ITTFL   = rep("Y", 6),
  TRTSDT  = as.Date(c("2024-01-15","2024-01-18","2024-01-20","2024-01-25","2024-01-28","2024-01-30")),
  TRTEDT  = as.Date(c("2024-06-15","2024-06-18","2024-04-01","2024-06-25", NA, "2024-07-01")),
  EOSDT   = as.Date(c("2024-06-15","2024-06-18","2024-04-01","2024-06-25","2024-07-15","2024-07-01")),
  DTHDT   = as.Date(c(NA, NA, "2024-04-15", NA, NA, NA)),
  DTHFL   = c("N","N","Y","N","N","N"),
  stringsAsFactors = FALSE
)

# ADRS - disease response
adrs <- data.frame(
  STUDYID  = rep("STUDY01", 9),
  USUBJID  = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002",
               "STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE",
               "PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC    = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20",
               "2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30"),
  stringsAsFactors = FALSE
)

# ==========================================================================
# STEP 2: Derive PFS (Progression-Free Survival)
# ==========================================================================

# Get progression date - first PROGRESSIVE DISEASE per USUBJID sorted by RSDTC
adrs_sorted <- adrs %>% arrange(USUBJID, RSDTC)
prog_ <- adrs_sorted %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(PROGDT = as.Date(RSDTC, format = "%Y-%m-%d")) %>%
  select(USUBJID, PROGDT)

# Merge progression and death - left join adsl with prog
adtte_pfs <- adsl %>%
  left_join(prog_, by = "USUBJID") %>%
  mutate(
    PARAMCD = "PFS",
    PARAM   = "Progression-Free Survival (Days)",
    PARAMN  = 1,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    # Determine event or censoring
    ADT = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ PROGDT,
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ DTHDT,
      !is.na(PROGDT) ~ PROGDT,
      !is.na(DTHDT)  ~ DTHDT,
      TRUE ~ coalesce(EOSDT, TRTEDT, TRTSDT)
    ),
    CNSR = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) ~ 0,
      !is.na(PROGDT) ~ 0,
      !is.na(DTHDT)  ~ 0,
      TRUE ~ 1
    ),
    EVNTDESC = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ "Disease Progression",
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ "Death",
      !is.na(PROGDT) ~ "Disease Progression",
      !is.na(DTHDT)  ~ "Death",
      TRUE ~ ""
    ),
    CNSDTDSC = case_when(
      CNSR == 1 ~ "Censored - No progression or death",
      TRUE ~ ""
    ),
    SRCDOM = case_when(
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ "RS",
      !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ "ADSL",
      !is.na(PROGDT) ~ "RS",
      !is.na(DTHDT)  ~ "ADSL",
      TRUE ~ "ADSL"
    )
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ==========================================================================
# STEP 3: Derive OS (Overall Survival)
# ==========================================================================
adtte_os <- adsl %>%
  mutate(
    PARAMCD = "OS",
    PARAM   = "Overall Survival (Days)",
    PARAMN  = 2,
    STARTDT = TRTSDT,
    ADT = ifelse(DTHFL == "Y" & !is.na(DTHDT), DTHDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin = "1970-01-01"),
    CNSR = ifelse(DTHFL == "Y" & !is.na(DTHDT), 0, 1),
    EVNTDESC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "Death", ""),
    CNSDTDSC = ifelse(CNSR == 1, "Censored - Alive", ""),
    SRCDOM = "ADSL",
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ==========================================================================
# STEP 4: Derive TTR (Time to Response) - for responders only
# ==========================================================================
resp_ <- adrs_sorted %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE", "PARTIAL RESPONSE")) %>%
  arrange(USUBJID, RSDTC) %>%
  group_by(USUBJID) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(RESPDT = as.Date(RSDTC, format = "%Y-%m-%d")) %>%
  select(USUBJID, RESPDT)

adtte_ttr <- adsl %>%
  left_join(resp_, by = "USUBJID") %>%
  mutate(
    PARAMCD = "TTR",
    PARAM   = "Time to Response (Days)",
    PARAMN  = 3,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = ifelse(!is.na(RESPDT), RESPDT, coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin = "1970-01-01"),
    CNSR = ifelse(!is.na(RESPDT), 0, 1),
    EVNTDESC = ifelse(!is.na(RESPDT), "Response", ""),
    CNSDTDSC = ifelse(!is.na(RESPDT), "", "Censored - No response"),
    AVAL = as.numeric(ADT - STARTDT + 1),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  ungroup() %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC)

# ==========================================================================
# STEP 5: Combine all TTE parameters
# ==========================================================================
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr)

# Derive AVAL in months
adtte <- adtte %>%
  mutate(AVAL_M = AVAL / 30.4375)

# ==========================================================================
# STEP 6: Add sequence and labels
# ==========================================================================
adtte <- adtte %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, AVAL_M, CNSR, EVNTDESC, CNSDTDSC, SRCDOM, ASEQ)

# ==========================================================================
# Write output datasets as CSV (dates as YYYY-MM-DD, na="")
# ==========================================================================
write.csv(adsl, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adrs, file.path(out, "adrs.csv"), row.names = FALSE, na = "")
write.csv(adtte_pfs, file.path(out, "adtte_pfs.csv"), row.names = FALSE, na = "")
write.csv(adtte_os, file.path(out, "adtte_os.csv"), row.names = FALSE, na = "")
write.csv(adtte_ttr, file.path(out, "adtte_ttr.csv"), row.names = FALSE, na = "")
write.csv(adtte, file.path(out, "adtte.csv"), row.names = FALSE, na = "")

# ==========================================================================
# Create P086.lst text rendering and CSV of table body
# SAS proc contents + proc print equivalent
# ==========================================================================

# Build text rendering
txt_path <- file.path(out, "P086.txt")
lst_txt_path <- file.path(out, "P086.lst.txt")
csv_path <- file.path(out, "P086.csv")
lst_csv_path <- file.path(out, "P086.lst.csv")

# Create detailed listing text
con <- file(txt_path, open = "wt")
cat("Program: P086_adtte.sas\n", file = con)
cat("Purpose: Create ADTTE (Time-to-Event Analysis Dataset)\n", file = con)
cat("Standard: CDISC ADaM IG 1.3\n", file = con)
cat("\n", file = con)
cat("ADTTE - Time-to-Event Analysis Dataset\n", file = con)
cat("================================================================================\n", file = con)
cat("\nPROC CONTENTS Data=ADTTE\n", file = con)
cat("--------------------------------------------------------------------------------\n", file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "Variable", "Label", "Type", "Format"), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "STUDYID", "Study Identifier", "Char", "$12."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "USUBJID", "Unique Subject Identifier", "Char", "$40."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "ASEQ", "Analysis Sequence Number", "Num", "8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "PARAMCD", "Parameter Code", "Char", "$8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "PARAM", "Parameter", "Char", "$200."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "PARAMN", "Parameter (N)", "Num", "8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "AVAL", "Analysis Value", "Num", "8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "AVAL_M", "Analysis Value (Months)", "Num", "8.2"), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "CNSR", "Censor", "Num", "8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "ADT", "Analysis Date", "Num", "DATE9."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "STARTDT", "Time-to-Event Origin Date", "Num", "DATE9."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "EVNTDESC", "Event Description", "Char", "$200."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "CNSDTDSC", "Censor Description", "Char", "$200."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "SRCDOM", "Source Domain", "Char", "$8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "TRTP", "Planned Treatment", "Char", "$40."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "TRTPN", "Planned Treatment (N)", "Num", "8."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "SAFFL", "Safety Population Flag", "Char", "$1."), file = con)
cat(sprintf("%-12s %-30s %-10s %s\n", "ITTFL", "Intent-to-Treat Population Flag", "Char", "$1."), file = con)
cat("\n", file = con)
cat("PROC PRINT Data=ADTTE NoObs\n", file = con)
cat("--------------------------------------------------------------------------------\n", file = con)

# Header for print
header_fmt <- "%-12s %-15s %-12s %-4s %-6s %-12s %-12s %-6s %-6s %-8s %-20s %-30s %-8s\n"
cat(sprintf(header_fmt,
            "STUDYID","USUBJID","TRTP","TRTPN","AGE","STARTDT","ADT","AVAL","AVAL_M","CNSR","PARAMCD","EVNTDESC","CNSDTDSC"), file = con)
cat(paste(rep("-", 180), collapse = ""), "\n", file = con, sep = "")

# Print each row
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i, ]
  cat(sprintf(header_fmt,
              r$STUDYID,
              r$USUBJID,
              substr(r$TRTP,1,12),
              as.character(r$TRTPN),
              as.character(r$AGE),
              format(r$STARTDT, "%d%b%Y"),
              format(r$ADT, "%d%b%Y"),
              sprintf("%.0f", r$AVAL),
              sprintf("%.2f", r$AVAL_M),
              as.character(r$CNSR),
              r$PARAMCD,
              ifelse(is.na(r$EVNTDESC) || r$EVNTDESC=="", "", r$EVNTDESC),
              ifelse(is.na(r$CNSDTDSC) || r$CNSDTDSC=="", "", substr(r$CNSDTDSC,1,30))
  ), file = con)
}
cat("\n", file = con)

# Summary statistics by PARAMCD
cat("\nSummary of ADTTE by Parameter\n", file = con)
cat("--------------------------------------------------------------------------------\n", file = con)
summary_tbl <- adtte %>%
  group_by(PARAMCD, PARAM) %>%
  summarise(
    N = n(),
    Events = sum(CNSR == 0, na.rm = TRUE),
    Censored = sum(CNSR == 1, na.rm = TRUE),
    Mean_AVAL = mean(AVAL, na.rm = TRUE),
    Median_AVAL = median(AVAL, na.rm = TRUE),
    Min_AVAL = min(AVAL, na.rm = TRUE),
    Max_AVAL = max(AVAL, na.rm = TRUE),
    Mean_Months = mean(AVAL_M, na.rm = TRUE),
    .groups = "drop"
  )
cat(sprintf("%-8s %-35s %3s %3s %3s %8s %8s %6s %6s %8s\n",
            "PARAMCD","PARAM","N","Ev","Cens","Mean","Median","Min","Max","Mean_M"), file = con)
for (i in seq_len(nrow(summary_tbl))) {
  r <- summary_tbl[i,]
  cat(sprintf("%-8s %-35s %3.0f %3.0f %3.0f %8.2f %8.2f %6.0f %6.0f %8.2f\n",
              r$PARAMCD, substr(r$PARAM,1,35), r$N, r$Events, r$Censored,
              r$Mean_AVAL, r$Median_AVAL, r$Min_AVAL, r$Max_AVAL, r$Mean_Months), file = con)
}
cat("\n", file = con)

# Detailed subject listing with all numeric values
cat("\nDetailed ADTTE Values (AVAL, AVAL_M, CNSR, PARAMN, ASEQ, TRTPN, AGE)\n", file = con)
cat("--------------------------------------------------------------------------------\n", file = con)
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i,]
  cat(sprintf("USUBJID=%s PARAMCD=%s PARAMN=%1.0f ASEQ=%1.0f TRTPN=%1.0f AGE=%2.0f AVAL=%3.0f AVAL_M=%6.3f CNSR=%1.0f STARTDT=%s ADT=%s\n",
              r$USUBJID, r$PARAMCD, r$PARAMN, r$ASEQ, r$TRTPN, r$AGE, r$AVAL, r$AVAL_M, r$CNSR,
              format(r$STARTDT, "%Y-%m-%d"), format(r$ADT, "%Y-%m-%d")), file = con)
}
cat("\n", file = con)
cat("Total observations: 18\n", file = con)
cat("Total subjects: 6\n", file = con)
cat("Total parameters: 3 (PFS, OS, TTR)\n", file = con)
cat(sprintf("PFS events: %d censored: %d\n", sum(adtte_pfs$CNSR==0), sum(adtte_pfs$CNSR==1)), file = con)
cat(sprintf("OS events: %d censored: %d\n", sum(adtte_os$CNSR==0), sum(adtte_os$CNSR==1)), file = con)
cat(sprintf("TTR events: %d censored: %d\n", sum(adtte_ttr$CNSR==0), sum(adtte_ttr$CNSR==1)), file = con)
close(con)

# Copy to P086.lst.txt as well
file.copy(txt_path, lst_txt_path, overwrite = TRUE)

# Write CSV of table body (adtte)
write.csv(adtte, csv_path, row.names = FALSE, na = "")
write.csv(adtte, lst_csv_path, row.names = FALSE, na = "")

# Also create P086.lst file (without extension) for compatibility
file.copy(txt_path, file.path(out, "P086.lst"), overwrite = TRUE)

# Ensure all expected files exist
stopifnot(file.exists(file.path(out, "adsl.csv")))
stopifnot(file.exists(file.path(out, "adrs.csv")))
stopifnot(file.exists(file.path(out, "adtte.csv")))
stopifnot(file.exists(file.path(out, "adtte_os.csv")))
stopifnot(file.exists(file.path(out, "adtte_pfs.csv")))
stopifnot(file.exists(file.path(out, "adtte_ttr.csv")))
```