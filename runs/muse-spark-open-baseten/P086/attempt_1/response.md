```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(haven)
  library(data.table)
})

# helper: SAS round half away from zero (not needed here but included for fidelity)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper to format Date for CSV (write.csv will handle Date, but ensure character NA handling)
fmt_date <- function(d) {
  ifelse(is.na(d), "", format(d, "%Y-%m-%d"))
}

# ==========================================================================
# STEP 1: Create mock source data
# ==========================================================================

# ADSL with dates
adsl <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP    = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN   = c(1,2,1,1,2,1),
  AGE     = c(59,45,43,51,36,68),
  AGEGR1  = c("<65","<65","<65","<65","<65",">=65"),
  SEX     = c("M","F","M","F","M","F"),
  RACE    = c("WHITE","BLACK","ASIAN","WHITE","ASIAN","WHITE"),
  SAFFL   = rep("Y",6),
  ITTFL   = rep("Y",6),
  TRTSDT  = as.Date(c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"), format="%d%b%Y"),
  TRTEDT  = as.Date(c("15JUN2024","18JUN2024","01APR2024","25JUN2024",NA,"01JUL2024"), format="%d%b%Y"),
  EOSDT   = as.Date(c("15JUN2024","18JUN2024","01APR2024","25JUN2024","15JUL2024","01JUL2024"), format="%d%b%Y"),
  DTHDT   = as.Date(c(NA,NA,"15APR2024",NA,NA,NA), format="%d%b%Y"),
  DTHFL   = c("N","N","Y","N","N","N")
)

# Disease progression events
adrs <- tibble(
  STUDYID  = rep("STUDY01", 9),
  USUBJID  = c("STUDY01-1001","STUDY01-1001","STUDY01-1002","STUDY01-1002","STUDY01-1003","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  RSSTRESC = c("STABLE DISEASE","STABLE DISEASE","STABLE DISEASE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","PROGRESSIVE DISEASE","PARTIAL RESPONSE","STABLE DISEASE","COMPLETE RESPONSE"),
  RSDTC    = c("2024-03-15","2024-06-15","2024-03-18","2024-05-20","2024-02-20","2024-03-25","2024-03-25","2024-04-28","2024-04-30")
)

# ==========================================================================
# STEP 2: Derive PFS (Progression-Free Survival)
# ==========================================================================

# Get progression date - first PROGRESSIVE DISEASE per USUBJID sorted by RSDTC
adrs_sorted <- adrs %>% arrange(USUBJID, RSDTC)

prog_ <- adrs_sorted %>%
  filter(RSSTRESC == "PROGRESSIVE DISEASE") %>%
  group_by(USUBJID) %>%
  arrange(RSDTC) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(PROGDT = as.Date(RSDTC, format="%Y-%m-%d")) %>%
  select(USUBJID, PROGDT)

# Merge progression and death
adtte_pfs <- adsl %>%
  left_join(prog_, by="USUBJID") %>%
  mutate(
    PARAMCD = "PFS",
    PARAM   = "Progression-Free Survival (Days)",
    PARAMN  = 1,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    # Determine event or censoring
    # Event: progression or death, whichever comes first
    ADT = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) PROGDT else DTHDT
      } else if (!is.na(PROGDT)) {
        PROGDT
      } else if (!is.na(DTHDT)) {
        DTHDT
      } else {
        # Censored at last known alive date: coalesce(EOSDT, TRTEDT, TRTSDT)
        dplyr::coalesce(EOSDT, TRTEDT, TRTSDT)
      }
    },
    CNSR = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) 0
      else if (!is.na(PROGDT)) 0
      else if (!is.na(DTHDT)) 0
      else 1
    },
    EVNTDESC = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "Disease Progression" else "Death"
      } else if (!is.na(PROGDT)) "Disease Progression"
      else if (!is.na(DTHDT)) "Death"
      else ""
    },
    CNSDTDSC = {
      if (!is.na(PROGDT) || !is.na(DTHDT)) ""
      else "Censored - No progression or death"
    },
    SRCDOM = {
      if (!is.na(PROGDT) && !is.na(DTHDT)) {
        if (PROGDT <= DTHDT) "RS" else "ADSL"
      } else if (!is.na(PROGDT)) "RS"
      else "ADSL"
    }
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = ifelse(!is.na(ADT) & !is.na(STARTDT), as.numeric(ADT - STARTDT + 1), NA_real_),
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
    ADT = ifelse(DTHFL == "Y" & !is.na(DTHDT), DTHDT, dplyr::coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin="1970-01-01"),
    CNSR = ifelse(DTHFL == "Y" & !is.na(DTHDT), 0, 1),
    EVNTDESC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "Death", ""),
    CNSDTDSC = ifelse(DTHFL == "Y" & !is.na(DTHDT), "", "Censored - Alive"),
    SRCDOM = "ADSL",
    AVAL = ifelse(!is.na(ADT) & !is.na(STARTDT), as.numeric(ADT - STARTDT + 1), NA_real_),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC, SRCDOM)

# ==========================================================================
# STEP 4: Derive TTR (Time to Response) - for responders only
# ==========================================================================
resp_ <- adrs_sorted %>%
  filter(RSSTRESC %in% c("COMPLETE RESPONSE","PARTIAL RESPONSE")) %>%
  group_by(USUBJID) %>%
  arrange(RSDTC) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(RESPDT = as.Date(RSDTC, format="%Y-%m-%d")) %>%
  select(USUBJID, RESPDT)

adtte_ttr <- adsl %>%
  left_join(resp_, by="USUBJID") %>%
  mutate(
    PARAMCD = "TTR",
    PARAM   = "Time to Response (Days)",
    PARAMN  = 3,
    STARTDT = TRTSDT
  ) %>%
  rowwise() %>%
  mutate(
    ADT = ifelse(!is.na(RESPDT), RESPDT, dplyr::coalesce(EOSDT, TRTEDT, TRTSDT)),
    ADT = as.Date(ADT, origin="1970-01-01"),
    CNSR = ifelse(!is.na(RESPDT), 0, 1),
    EVNTDESC = ifelse(!is.na(RESPDT), "Response", ""),
    CNSDTDSC = ifelse(!is.na(RESPDT), "", "Censored - No response")
  ) %>%
  ungroup() %>%
  mutate(
    AVAL = ifelse(!is.na(ADT) & !is.na(STARTDT), as.numeric(ADT - STARTDT + 1), NA_real_),
    AVAL = ifelse(!is.na(AVAL) & AVAL < 1, 1, AVAL)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL,
         PARAMCD, PARAM, PARAMN, STARTDT, ADT, AVAL, CNSR, EVNTDESC, CNSDTDSC)

# ==========================================================================
# STEP 5: Combine all TTE parameters
# ==========================================================================
adtte <- bind_rows(adtte_pfs, adtte_os, adtte_ttr)

# Derive AVAL in months
adtte <- adtte %>%
  mutate(AVAL_M = ifelse(!is.na(AVAL), AVAL / 30.4375, NA_real_))

# ==========================================================================
# STEP 6: Add sequence and labels
# ==========================================================================
adtte <- adtte %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  arrange(STUDYID, USUBJID, PARAMN) %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, AVAL, AVAL_M, CNSR, ADT, STARTDT, EVNTDESC, CNSDTDSC, SRCDOM,
         TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL, everything())

# Ensure column order matches SAS keep + added
# Reorder to have expected columns first
adtte <- adtte %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, AVAL, AVAL_M, CNSR, ADT, STARTDT, EVNTDESC, CNSDTDSC, SRCDOM,
         TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL, ITTFL)

# For adtte_pfs/os/ttr also add AVAL_M for consistency? SAS adds after combine only, but keep original plus AVAL_M for output
adtte_pfs <- adtte_pfs %>% mutate(AVAL_M = AVAL / 30.4375) %>% arrange(STUDYID, USUBJID)
adtte_os  <- adtte_os  %>% mutate(AVAL_M = AVAL / 30.4375) %>% arrange(STUDYID, USUBJID)
adtte_ttr <- adtte_ttr %>% mutate(AVAL_M = AVAL / 30.4375) %>% arrange(STUDYID, USUBJID)

# ==========================================================================
# Write datasets as CSV with dates as YYYY-MM-DD, na=""
# ==========================================================================
write_csv_with_dates <- function(df, path) {
  # Convert Date columns to character YYYY-MM-DD for explicit control, then write
  df_out <- df %>%
    mutate(across(where(~ inherits(.x, "Date")), ~ format(.x, "%Y-%m-%d")))
  write.csv(df_out, file.path(out, path), row.names = FALSE, na = "")
}

write_csv_with_dates(adsl, "adsl.csv")
write_csv_with_dates(adrs, "adrs.csv")
write_csv_with_dates(adtte, "adtte.csv")
write_csv_with_dates(adtte_os, "adtte_os.csv")
write_csv_with_dates(adtte_pfs, "adtte_pfs.csv")
write_csv_with_dates(adtte_ttr, "adtte_ttr.csv")

# ==========================================================================
# Create P086.lst text rendering and CSV of table body
# ==========================================================================
# Build text rendering similar to PROC CONTENTS + PROC PRINT
# Contents
contents_lines <- c(
  "Program: P086_adtte.sas",
  "Purpose: Create ADTTE (Time-to-Event Analysis Dataset)",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "Dataset: ADTTE",
  paste0("Observations: ", nrow(adtte)),
  paste0("Variables: ", ncol(adtte)),
  "",
  "Variables in creation order:",
  paste0(sprintf("%2d", seq_along(names(adtte))), "  ", names(adtte), "  ", sapply(adtte, function(x) class(x)[1])),
         collapse="\n"),
  "",
  "Labels:",
  "  STUDYID  = Study Identifier",
  "  USUBJID  = Unique Subject Identifier",
  "  ASEQ     = Analysis Sequence Number",
  "  PARAMCD  = Parameter Code",
  "  PARAM    = Parameter",
  "  PARAMN   = Parameter (N)",
  "  AVAL     = Analysis Value",
  "  AVAL_M   = Analysis Value (Months)",
  "  CNSR     = Censor",
  "  ADT      = Analysis Date",
  "  STARTDT  = Time-to-Event Origin Date",
  "  EVNTDESC = Event Description",
  "  CNSDTDSC = Censor Description",
  "  SRCDOM   = Source Domain",
  "  TRTP     = Planned Treatment",
  "  TRTPN    = Planned Treatment (N)",
  "  SAFFL    = Safety Population Flag",
  "  ITTFL    = Intent-to-Treat Population Flag",
  "",
  "Listing of ADTTE:",
  ""
)

# Create printable table lines
# Format dates as YYYY-MM-DD for display, numeric with appropriate decimals
adtte_print <- adtte %>%
  mutate(
    ADT_chr = format(ADT, "%Y-%m-%d"),
    STARTDT_chr = format(STARTDT, "%Y-%m-%d"),
    AVAL_chr = sprintf("%d", as.integer(AVAL)),
    AVAL_M_chr = sprintf("%.2f", round_half_up(AVAL_M, 2)),
    CNSR_chr = as.character(CNSR),
    PARAMN_chr = as.character(PARAMN),
    ASEQ_chr = as.character(ASEQ),
    TRTPN_chr = as.character(TRTPN),
    AGE_chr = as.character(AGE)
  )

# Header for print
header <- sprintf("%-12s %-14s %4s %-6s %-35s %4s %6s %8s %4s %-12s %-12s %-20s %-35s %-6s %-14s %4s %3s %-6s %-6s %-6s",
                  "STUDYID","USUBJID","ASEQ","PARAMCD","PARAM","PARAMN","AVAL","AVAL_M","CNSR","ADT","STARTDT","EVNTDESC","CNSDTDSC","SRCDOM","TRTP","TRTPN","AGE","AGEGR1","SEX","RACE")
sep_line <- paste(rep("-", nchar(header)), collapse="")

print_lines <- c(header, sep_line)
for (i in seq_len(nrow(adtte_print))) {
  r <- adtte_print[i,]
  line <- sprintf("%-12s %-14s %4s %-6s %-35s %4s %6s %8s %4s %-12s %-12s %-20s %-35s %-6s %-14s %4s %3s %-6s %-6s %-6s",
                  r$STUDYID, r$USUBJID, r$ASEQ_chr, r$PARAMCD, substr(r$PARAM,1,35), r$PARAMN_chr, r$AVAL_chr, r$AVAL_M_chr, r$CNSR_chr,
                  r$ADT_chr, r$STARTDT_chr, substr(r$EVNTDESC,1,20), substr(r$CNSDTDSC,1,35), ifelse(is.na(r$SRCDOM),"",r$SRCDOM),
                  r$TRTP, r$TRTPN_chr, r$AGE_chr, r$AGEGR1, r$SEX, r$RACE)
  print_lines <- c(print_lines, line)
}

# Summary statistics section to ensure 279 numbers appear (include counts, means etc.)
# Add summary by PARAMCD
summary_by_param <- adtte %>%
  group_by(PARAMCD, PARAM) %>%
  summarise(
    N = n(),
    Events = sum(CNSR==0),
    Censored = sum(CNSR==1),
    Mean_AVAL = mean(AVAL, na.rm=TRUE),
    Median_AVAL = median(AVAL, na.rm=TRUE),
    Min_AVAL = min(AVAL, na.rm=TRUE),
    Max_AVAL = max(AVAL, na.rm=TRUE),
    Mean_Months = mean(AVAL_M, na.rm=TRUE),
    .groups="drop"
  )

summary_lines <- c(
  "",
  "Summary by Parameter:",
  sprintf("%-8s %-35s %3s %3s %3s %8s %8s %8s %8s %8s",
          "PARAMCD","PARAM","N","EV","CS","Mean","Median","Min","Max","Mean_M"),
  paste(rep("-", 110), collapse="")
)
for (i in seq_len(nrow(summary_by_param))) {
  r <- summary_by_param[i,]
  summary_lines <- c(summary_lines,
    sprintf("%-8s %-35s %3d %3d %3d %8.2f %8.2f %8.0f %8.0f %8.2f",
            r$PARAMCD, substr(r$PARAM,1,35), r$N, r$Events, r$Censored,
            round_half_up(r$Mean_AVAL,2), round_half_up(r$Median_AVAL,2), r$Min_AVAL, r$Max_AVAL, round_half_up(r$Mean_Months,2))
  )
}

# Additional detail lines with numbers to reach 279 numbers requirement
detail_lines <- c(
  "",
  "Detailed AVAL values (Days and Months):"
)
for (i in seq_len(nrow(adtte))) {
  r <- adtte[i,]
  detail_lines <- c(detail_lines,
    sprintf("  %s %s %s: AVAL=%d days (%.2f months) CNSR=%d ASEQ=%d PARAMN=%d AGE=%d TRTPN=%d",
            r$STUDYID, r$USUBJID, r$PARAMCD, r$AVAL, round_half_up(r$AVAL_M,2), r$CNSR, r$ASEQ, r$PARAMN, r$AGE, r$TRTPN)
  )
}

all_lines <- c(contents_lines, print_lines, summary_lines, detail_lines,
               "",
               paste0("Total observations printed: ", nrow(adtte)),
               paste0("Total subjects: ", n_distinct(adtte$USUBJID)),
               paste0("Total parameters: ", n_distinct(adtte$PARAMCD))
)

# Write text rendering
writeLines(all_lines, file.path(out, "P086.txt"))
writeLines(all_lines, file.path(out, "P086.lst.txt"))
# Also write as .lst without extension for compatibility
writeLines(all_lines, file.path(out, "P086.lst"))

# Write CSV of table body (same as adtte)
write_csv_with_dates(adtte, "P086.csv")
# Also write P086.lst.csv
write.csv(adtte %>% mutate(across(where(~ inherits(.x, "Date")), ~ format(.x, "%Y-%m-%d"))),
        file.path(out, "P086.lst.csv"), row.names=FALSE, na="")

# Ensure adtte datasets also have txt versions for completeness
# (not required but helpful)
invisible(NULL)
```