```r
# P089_adex.R
# Reproduces SAS program P089_adex.sas
# -------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(lubridate)
library(readr)
library(stringr)
library(data.table)

# -------------------------------------------------
# Output directory from environment
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Helper: convert SAS missing '.' to NA for character columns
na_if_dot <- function(df) {
  df %>% mutate(across(where(is.character), ~na_if(., ".")))
}

# -------------------------------------------------
# STEP 1: Create mock SDTM EX data
ex_txt <- "
STUDY01|STUDY01-1001|1|Treatment A|100|mg|TABLET|ORAL|2024-01-15|2024-01-28|1|Week 1-2
STUDY01|STUDY01-1001|2|Treatment A|100|mg|TABLET|ORAL|2024-01-29|2024-02-11|2|Week 3-4
STUDY01|STUDY01-1001|3|Treatment A|150|mg|TABLET|ORAL|2024-02-12|2024-03-10|3|Week 5-8
STUDY01|STUDY01-1001|4|Treatment A|150|mg|TABLET|ORAL|2024-03-11|2024-06-15|4|Week 9+
STUDY01|STUDY01-1002|1|Placebo|0|mg|TABLET|ORAL|2024-01-18|2024-02-14|1|Week 1-4
STUDY01|STUDY01-1002|2|Placebo|0|mg|TABLET|ORAL|2024-02-15|2024-06-18|2|Week 5+
STUDY01|STUDY01-1003|1|Treatment A|100|mg|TABLET|ORAL|2024-01-20|2024-02-16|1|Week 1-4
STUDY01|STUDY01-1003|2|Treatment A|100|mg|TABLET|ORAL|2024-02-17|2024-03-15|2|Week 5-8
STUDY01|STUDY01-1003|3|Treatment A|50|mg|TABLET|ORAL|2024-03-16|2024-04-01|3|Week 9+
STUDY01|STUDY01-1004|1|Treatment A|100|mg|TABLET|ORAL|2024-01-25|2024-06-25|1|All
STUDY01|STUDY01-1005|1|Placebo|0|mg|TABLET|ORAL|2024-01-28|.|1|All
STUDY01|STUDY01-1006|1|Treatment A|100|mg|TABLET|ORAL|2024-01-30|2024-03-30|1|Week 1-8
STUDY01|STUDY01-1006|2|Treatment A|150|mg|TABLET|ORAL|2024-03-31|2024-07-01|2|Week 9+
"
ex <- read_delim(
  I(str_trim(ex_txt)),
  delim = "|",
  col_names = c("STUDYID","USUBJID","EXSEQ","EXTRT","EXDOSE","EXDOSU",
                "EXDOSFRM","EXROUTE","EXSTDTC","EXENDTC","VISITNUM","VISIT"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>% na_if_dot()

# -------------------------------------------------
# STEP 2: Create mock ADSL
adsl_txt <- "
STUDY01|STUDY01-1001|Treatment A|1|59|M|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|Placebo|2|45|F|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|Treatment A|1|43|M|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|Treatment A|1|51|F|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|Placebo|2|36|M|Y|28JAN2024|.
STUDY01|STUDY01-1006|Treatment A|1|68|F|Y|30JAN2024|01JUL2024
"
adsl <- read_delim(
  I(str_trim(adsl_txt)),
  delim = "|",
  col_names = c("STUDYID","USUBJID","TRTP","TRTPN","AGE","SEX","SAFFL",
                "TRTSDT_raw","TRTEDT_raw"),
  trim_ws = TRUE,
  show_col_types = FALSE
) %>%
  mutate(
    TRTSDT = dmy(TRTSDT_raw),
    TRTEDT = dmy(TRTEDT_raw)
  ) %>%
  select(-TRTSDT_raw, -TRTEDT_raw)

# -------------------------------------------------
# STEP 3: Merge and create base exposure variables
adex_base <- ex %>%
  left_join(adsl, by = c("STUDYID","USUBJID")) %>%
  filter(!is.na(TRTP)) %>%                     # keep only records present in both
  mutate(
    PARAMCD = "DOSE",
    PARAM   = "Dose",
    PARAMN  = 1L,
    AVAL    = EXDOSE,
    AVALU   = EXDOSU,
    # convert dates (EXSTDTC/EXENDTC are YYYY-MM-DD)
    ASTDT = if_else(!is.na(EXSTDTC) & EXSTDTC != "", ymd(EXSTDTC), as.Date(NA)),
    AENDT = if_else(!is.na(EXENDTC) & EXENDTC != "", ymd(EXENDTC), as.Date(NA)),
    # analysis days
    ASTDY = if_else(!is.na(ASTDT) & !is.na(TRTSDT),
                   if_else(ASTDT >= TRTSDT,
                           as.numeric(ASTDT - TRTSDT) + 1,
                           as.numeric(ASTDT - TRTSDT)),
                   as.numeric(NA)),
    AENDY = if_else(!is.na(AENDT) & !is.na(TRTSDT),
                   if_else(AENDT >= TRTSDT,
                           as.numeric(AENDT - TRTSDT) + 1,
                           as.numeric(AENDT - TRTSDT)),
                   as.numeric(NA)),
    AVISIT  = VISIT,
    AVISITN = VISITNUM,
    EXDURN  = if_else(!is.na(ASTDT) & !is.na(AENDT),
                     as.numeric(AENDT - ASTDT) + 1,
                     as.numeric(NA)),
    DOSINTFL = "N",
    DOSEADFL = "N"
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT,
         AGE, SEX, PARAMCD, PARAM, PARAMN, AVAL, AVALU,
         ASTDT, AENDT, ASTDY, AENDY, EXDURN, DOSINTFL, DOSEADFL,
         EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ,
         AVISIT, AVISITN)

# -------------------------------------------------
# STEP 4: Identify dose adjustments
dt <- as.data.table(adex_base)
setorder(dt, USUBJID, ASTDT)

dt[, c("_prev_dose","_prev_end") := .(
  shift(AVAL, type = "lag"),
  shift(AENDT, type = "lag")
), by = USUBJID]

dt[!is.na(_prev_dose) & AVAL != _prev_dose, DOSEADFL := "Y"]
dt[!is.na(_prev_end) & !is.na(ASTDT) &
      (as.numeric(ASTDT - _prev_end) > 1), DOSINTFL := "Y"]

dt[is.na(DOSEADFL), DOSEADFL := "N"]
dt[is.na(DOSINTFL), DOSINTFL := "N"]

dt[, c("_prev_dose","_prev_end") := NULL]
adex_base <- as_tibble(dt)

# -------------------------------------------------
# STEP 5: Create summary exposure parameters
common_cols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL",
                 "TRTSDT","TRTEDT","AGE","SEX")

_totdur <- adex_base %>%
  group_by(across(all_of(common_cols))) %>%
  summarise(
    AVAL = sum(EXDURN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = "TOTDUR",
    PARAM   = "Total Exposure Duration (Days)",
    PARAMN  = 2L,
    AVALU   = "days"
  )

_avgdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(across(all_of(common_cols))) %>%
  summarise(
    AVAL = sum(AVAL * EXDURN, na.rm = TRUE) / sum(EXDURN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = "AVGDOSE",
    PARAM   = "Average Daily Dose",
    PARAMN  = 3L,
    AVALU   = "mg"
  )

_maxdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(across(all_of(common_cols))) %>%
  summarise(
    AVAL = max(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = "MAXDOSE",
    PARAM   = "Maximum Dose",
    PARAMN  = 4L,
    AVALU   = "mg"
  )

_cumdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(across(all_of(common_cols))) %>%
  summarise(
    AVAL = sum(AVAL * EXDURN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = "CUMDOSE",
    PARAM   = "Cumulative Dose",
    PARAMN  = 5L,
    AVALU   = "mg"
  )

_ndosadj <- adex_base %>%
  group_by(across(all_of(common_cols))) %>%
  summarise(
    AVAL = sum(DOSEADFL == "Y", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = "NDOSADJ",
    PARAM   = "Number of Dose Adjustments",
    PARAMN  = 6L,
    AVALU   = ""
  )

# -------------------------------------------------
# STEP 6: Combine all parameters
all_vars <- c(common_cols,
              "PARAMCD","PARAM","PARAMN","AVAL","AVALU",
              "ASTDT","AENDT","ASTDY","AENDY",
              "EXDURN","DOSINTFL","DOSEADFL",
              "EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ",
              "AVISIT","AVISITN")

fill_missing <- function(df) {
  missing_vars <- setdiff(all_vars, names(df))
  df %>% mutate(across(all_of(missing_vars), ~as.character(NA)))
}

adex <- bind_rows(
  fill_missing(adex_base),
  fill_missing(_totdur),
  fill_missing(_avgdose),
  fill_missing(_maxdose),
  fill_missing(_cumdose),
  fill_missing(_ndosadj)
) %>%
  select(all_of(all_vars))

# -------------------------------------------------
# STEP 7: Add sequence and labels
adex <- adex %>%
  arrange(STUDYID, USUBJID, PARAMN, ASTDT) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(
    ASEQ = row_number() - 1L,
    DOSECAT = case_when(
      PARAMCD == "DOSE" & AVAL == 0               ~ "Placebo",
      PARAMCD == "DOSE" & AVAL > 0 & AVAL <= 50   ~ "Low",
      PARAMCD == "DOSE" & AVAL > 50 & AVAL <= 100 ~ "Medium",
      PARAMCD == "DOSE" & AVAL > 100             ~ "High",
      TRUE                                        ~ NA_character_
    )
  ) %>%
  ungroup()

# Attach SAS-like labels as attributes (optional)
label_list <- list(
  STUDYID   = "Study Identifier",
  USUBJID   = "Unique Subject Identifier",
  ASEQ      = "Analysis Sequence Number",
  PARAMCD   = "Parameter Code",
  PARAM     = "Parameter",
  PARAMN    = "Parameter (N)",
  AVAL      = "Analysis Value",
  AVALU     = "Analysis Value Unit",
  ASTDT     = "Analysis Start Date",
  AENDT     = "Analysis End Date",
  ASTDY     = "Analysis Start Relative Day",
  AENDY     = "Analysis End Relative Day",
  AVISIT    = "Analysis Visit",
  AVISITN   = "Analysis Visit (N)",
  EXDURN    = "Exposure Duration (Days)",
  EXTRT     = "Name of Treatment",
  EXDOSE    = "Dose per Administration",
  EXDOSU    = "Dose Units",
  EXDOSFRM  = "Dose Form",
  EXROUTE   = "Route of Administration",
  DOSINTFL  = "Dose Interruption Flag",
  DOSEADFL  = "Dose Adjustment Flag",
  DOSECAT   = "Dose Category",
  TRTP      = "Planned Treatment",
  TRTPN     = "Planned Treatment (N)",
  SAFFL     = "Safety Population Flag"
)
for (nm in names(label_list)) {
  attr(adex[[nm]], "label") <- label_list[[nm]]
}

# -------------------------------------------------
# Write output datasets
write.csv(adex, file.path(out_dir, "adex.csv"), row.names = FALSE, na = "")
write.csv(adex_base, file.path(out_dir, "adex_base.csv"), row.names = FALSE, na = "")
write.csv(adsl, file.path(out_dir, "adsl.csv"), row.names = FALSE, na = "")

# -------------------------------------------------
# Create PROC CONTENTS‑like table
var_info <- data.frame(
  VarNum = seq_along(names(adex)),
  Name   = names(adex),
  Type   = sapply(adex, function(col) if (is.numeric(col)) "Num" else "Char"),
  Length = sapply(adex, function(col) {
    if (is.numeric(col)) 8 else max(nchar(as.character(col)), na.rm = TRUE)
  }),
  Label  = sapply(names(adex), function(n) attr(adex[[n]], "label") %||% "")
)

# PROC PRINT first 20 rows (no row numbers)
print_rows <- head(adex, 20)

# Write listing text file
listing_txt_path <- file.path(out_dir, "P089.txt")
con <- file(listing_txt_path, open = "wt")
writeLines("PROC CONTENTS output (variables in order):", con)
write.table(var_info, con, sep = "\t", row.names = FALSE, quote = FALSE)
writeLines("\nPROC PRINT output (first 20 observations):", con)
write.table(print_rows, con, sep = "\t", row.names = FALSE, quote = FALSE)
close(con)

# Also write CSV of the printed rows (table body)
write.csv(print_rows, file.path(out_dir, "P089.csv"), row.names = FALSE, na = "")

# -------------------------------------------------
# End of script
```