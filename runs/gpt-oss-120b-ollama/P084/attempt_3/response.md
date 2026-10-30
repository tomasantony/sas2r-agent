```r
# P084_adae.R
# Translation of SAS program to R
# Produces: adae.csv, adsl.csv, P084.lst (text listing) and accompanying CSVs

suppressMessages({
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  library(tidyr)
})

out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------------
# Step 1: Mock AE data (inline)
# ------------------------------------------------------------------
ae_raw <- read_delim(
  file = I(
    "STUDY01|STUDY01-1001|1|Headache|Headache|Nervous system disorders|2024-02-01|2024-02-03|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1001|2|Nausea|Nausea|Gastrointestinal disorders|2024-02-15|2024-02-18|MODERATE|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|2
STUDY01|STUDY01-1002|1|Fatigue|Fatigue|General disorders|2024-02-10|2024-03-01|MILD|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1003|1|Dizziness|Dizziness|Nervous system disorders|2024-02-20|2024-02-22|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1003|2|Rash|Rash|Skin disorders|2024-03-05|2024-03-15|MODERATE|N|PROBABLY RELATED|DOSE REDUCED|RECOVERED|2
STUDY01|STUDY01-1003|3|Hepatotoxicity|Hepatotoxicity|Hepatobiliary disorders|2024-03-25|.|SEVERE|Y|RELATED|DRUG WITHDRAWN|NOT RECOVERED|3
STUDY01|STUDY01-1004|1|Insomnia|Insomnia|Psychiatric disorders|2024-02-25|2024-03-10|MILD|N|UNLIKELY|DOSE NOT CHANGED|RECOVERED|1
STUDY01|STUDY01-1005|1|Vomiting|Vomiting|Gastrointestinal disorders|2024-03-01|2024-03-02|MODERATE|N|POSSIBLY RELATED|DOSE NOT CHANGED|RECOVERED|2
STUDY01|STUDY01-1006|1|Back pain|Back pain|Musculoskeletal disorders|2024-02-28|2024-03-05|MILD|N|NOT RELATED|DOSE NOT CHANGED|RECOVERED|1"
  ),
  delim = "|",
  col_names = c("STUDYID","USUBJID","AESEQ","AETERM","AEDECOD","AEBODSYS",
                "AESTDTC","AEENDTC","AESEV","AESER","AEREL","AEACN","AEOUT","AETOXGR"),
  trim_ws = TRUE,
  na = c(".", "")
)

# ------------------------------------------------------------------
# Step 2: Mock ADSL data (inline)
# ------------------------------------------------------------------
adsl_raw <- read_delim(
  file = I(
    "STUDY01|STUDY01-1001|101|Treatment A|1|59|<65|M|WHITE|Y|15JAN2024|15JUN2024
STUDY01|STUDY01-1002|101|Placebo|2|45|<65|F|BLACK OR AFRICAN AMERICAN|Y|18JAN2024|18JUN2024
STUDY01|STUDY01-1003|102|Treatment A|1|43|<65|M|ASIAN|Y|20JAN2024|01APR2024
STUDY01|STUDY01-1004|102|Treatment A|1|51|<65|F|WHITE|Y|25JAN2024|25JUN2024
STUDY01|STUDY01-1005|103|Placebo|2|36|<65|M|ASIAN|Y|28JAN2024|.
STUDY01|STUDY01-1006|103|Treatment A|1|68|>=65|F|WHITE|Y|30JAN2024|01JUL2024"
  ),
  delim = "|",
  col_names = c("STUDYID","USUBJID","SITEID","TRTP","TRTPN","AGE","AGEGR1",
                "SEX","RACE","SAFFL","TRTSDT_raw","TRTEDT_raw"),
  trim_ws = TRUE,
  na = c(".", "")
) %>%
  mutate(
    TRTSDT = dmy(TRTSDT_raw),
    TRTEDT = dmy(TRTEDT_raw)
  ) %>%
  select(STUDYID, USUBJID, SITEID, TRTP, TRTPN,
         AGE, AGEGR1, SEX, RACE, SAFFL,
         TRTSDT, TRTEDT)

# ------------------------------------------------------------------
# Step 3: Merge AE with ADSL and derive ADAE variables
# ------------------------------------------------------------------
adae <- ae_raw %>%
  inner_join(
    adsl_raw,
    by = c("STUDYID","USUBJID")
  ) %>%
  mutate(
    # Convert character dates to Date objects
    ASTDT = if_else(AESTDTC != "" & !is.na(AESTDTC), ymd(AESTDTC), as.Date(NA)),
    AENDT = if_else(AEENDTC != "" & !is.na(AEENDTC), ymd(AEENDTC), as.Date(NA))
  ) %>%
  # Analysis days
  mutate(
    ASTDY = if_else(!is.na(ASTDT) & !is.na(TRTSDT),
                   if_else(ASTDT >= TRTSDT,
                           as.numeric(ASTDT - TRTSDT) + 1,
                           as.numeric(ASTDT - TRTSDT)),
                   as.numeric(NA)),
    AENDY = if_else(!is.na(AENDT) & !is.na(TRTSDT),
                   if_else(AENDT >= TRTSDT,
                           as.numeric(AENDT - TRTSDT) + 1,
                           as.numeric(AENDT - TRTSDT)),
                   as.numeric(NA))
  ) %>%
  # Treatment‑emergent flag
  mutate(
    TRTEMFL = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT &
        (is.na(TRTEDT) | ASTDT <= TRTEDT + days(30)) ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  # Severity numeric code
  mutate(
    ASEV = AESEV,
    ASEVN = case_when(
      toupper(AESEV) == "MILD"    ~ 1L,
      toupper(AESEV) == "MODERATE" ~ 2L,
      toupper(AESEV) == "SEVERE"   ~ 3L,
      TRUE                         ~ NA_integer_
    )
  ) %>%
  # Relationship numeric code
  mutate(
    AREL = AEREL,
    ARELN = case_when(
      toupper(AEREL) == "NOT RELATED"      ~ 1L,
      toupper(AEREL) == "UNLIKELY"        ~ 2L,
      toupper(AEREL) == "POSSIBLY RELATED" ~ 3L,
      toupper(AEREL) == "PROBABLY RELATED"  ~ 4L,
      toupper(AEREL) == "RELATED"           ~ 5L,
      TRUE                                 ~ NA_integer_
    )
  ) %>%
  # Serious flag copy
  mutate(AESER_FL = AESER) %>%
  # SOC and upper case PT
  mutate(
    AESOC = AEBODSYS,
    AEDECOD_UP = toupper(AEDECOD)
  ) %>%
  # Duration
  mutate(
    AEDUR = if_else(!is.na(ASTDT) & !is.na(AENDT),
                   as.numeric(AENDT - ASTDT) + 1,
                   NA_integer_)
  ) %>%
  # Order columns like SAS
  select(
    STUDYID, USUBJID, AESEQ, AETERM, AEDECOD, AEBODSYS,
    AESOC, AEDECOD_UP, ASTDT, AENDT, ASTDY, AENDY,
    AESEV, ASEV, ASEVN, AEREL, AREL, ARELN,
    AESER, AESER_FL, TRTEMFL,
    AOCCFL = NA_character_, AOCCSFL = NA_character_, AOCCPFL = NA_character_,
    AEDUR,
    TRTP, TRTPN, AGE, AGEGR1, SEX, RACE, SAFFL,
    SITEID, TRTSDT, TRTEDT
  )

# ------------------------------------------------------------------
# Step 4: Occurrence flags
# ------------------------------------------------------------------
adae <- adae %>%
  arrange(USUBJID, AEBODSYS, AEDECOD, ASTDT) %>%
  group_by(USUBJID) %>%
  mutate(AOCCFL = if_else(row_number() == 1, "Y", "")) %>%
  ungroup() %>%
  group_by(AEBODSYS) %>%
  mutate(AOCCSFL = if_else(row_number() == 1, "Y", "")) %>%
  ungroup() %>%
  group_by(AEDECOD) %>%
  mutate(AOCCPFL = if_else(row_number() == 1, "Y", "")) %>%
  ungroup()

# ------------------------------------------------------------------
# Step 5: Analysis sequence (ASEQ)
# ------------------------------------------------------------------
adae <- adae %>%
  arrange(STUDYID, USUBJID, ASTDT, AESEQ) %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

# ------------------------------------------------------------------
# Step 6: Write datasets
# ------------------------------------------------------------------
write.csv(
  adae,
  file = file.path(out_dir, "adae.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  adsl_raw,
  file = file.path(out_dir, "adsl.csv"),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------
# Step 7: Produce listing (PROC CONTENTS + PROC PRINT)
# ------------------------------------------------------------------
# PROC CONTENTS‑like table (variable metadata)
contents_tbl <- adae %>%
  summarise(across(everything(), ~ .x[1])) %>%          # keep one row for type inspection
  pivot_longer(everything(), names_to = "name", values_to = "example") %>%
  mutate(
    varnum = row_number(),
    type = if_else(is.numeric(example) | inherits(example, "Date"), "NUM", "CHAR"),
    length = if_else(type == "NUM", 8,
                     pmax(nchar(as.character(example)), na.rm = TRUE)),
    format = if_else(inherits(example, "Date"), "DATE9.", "")
  ) %>%
  select(varnum, name, type, length, format)

# PROC PRINT first 10 rows (no obs numbers)
print_tbl <- adae %>% slice_head(n = 10)

# Write the combined listing as .lst (text)
lst_path <- file.path(out_dir, "P084.lst")
con <- file(lst_path, open = "wt")
writeLines("PROC CONTENTS OUTPUT", con)
write.table(
  contents_tbl,
  con = con,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)
writeLines("\nPROC PRINT (first 10 rows) OUTPUT", con)
write.table(
  print_tbl,
  con = con,
  sep = "\t",
  row.names = FALSE,
  col.names = TRUE,
  quote = FALSE
)
close(con)

# CSV versions of the two tables
write.csv(
  contents_tbl,
  file = file.path(out_dir, "P084_contents.csv"),
  row.names = FALSE,
  na = ""
)

write.csv(
  print_tbl,
  file = file.path(out_dir, "P084_print.csv"),
  row.names = FALSE,
  na = ""
)
```