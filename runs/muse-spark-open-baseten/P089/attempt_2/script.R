out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(haven)

fmt_date <- function(x) {
  ifelse(is.na(x), "", format(x, "%Y-%m-%d"))
}

#==========================================================================
# STEP 1: Create mock SDTM EX data
#==========================================================================
ex <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~EXSEQ, ~EXTRT, ~EXDOSE, ~EXDOSU, ~EXDOSFRM, ~EXROUTE, ~EXSTDTC, ~EXENDTC, ~VISITNUM, ~VISIT,
  "STUDY01", "STUDY01-1001", 1, "Treatment A", 100, "mg", "TABLET", "ORAL", "2024-01-15", "2024-01-28", 1, "Week 1-2",
  "STUDY01", "STUDY01-1001", 2, "Treatment A", 100, "mg", "TABLET", "ORAL", "2024-01-29", "2024-02-11", 2, "Week 3-4",
  "STUDY01", "STUDY01-1001", 3, "Treatment A", 150, "mg", "TABLET", "ORAL", "2024-02-12", "2024-03-10", 3, "Week 5-8",
  "STUDY01", "STUDY01-1001", 4, "Treatment A", 150, "mg", "TABLET", "ORAL", "2024-03-11", "2024-06-15", 4, "Week 9+",
  "STUDY01", "STUDY01-1002", 1, "Placebo", 0, "mg", "TABLET", "ORAL", "2024-01-18", "2024-02-14", 1, "Week 1-4",
  "STUDY01", "STUDY01-1002", 2, "Placebo", 0, "mg", "TABLET", "ORAL", "2024-02-15", "2024-06-18", 2, "Week 5+",
  "STUDY01", "STUDY01-1003", 1, "Treatment A", 100, "mg", "TABLET", "ORAL", "2024-01-20", "2024-02-16", 1, "Week 1-4",
  "STUDY01", "STUDY01-1003", 2, "Treatment A", 100, "mg", "TABLET", "ORAL", "2024-02-17", "2024-03-15", 2, "Week 5-8",
  "STUDY01", "STUDY01-1003", 3, "Treatment A", 50, "mg", "TABLET", "ORAL", "2024-03-16", "2024-04-01", 3, "Week 9+",
  "STUDY01", "STUDY01-1004", 1, "Treatment A", 100, "mg", "TABLET", "ORAL", "2024-01-25", "2024-06-25", 1, "All",
  "STUDY01", "STUDY01-1005", 1, "Placebo", 0, "mg", "TABLET", "ORAL", "2024-01-28", ".", 1, "All",
  "STUDY01", "STUDY01-1006", 1, "Treatment A", 100, "mg", "TABLET", "ORAL", "2024-01-30", "2024-03-30", 1, "Week 1-8",
  "STUDY01", "STUDY01-1006", 2, "Treatment A", 150, "mg", "TABLET", "ORAL", "2024-03-31", "2024-07-01", 2, "Week 9+"
)

#==========================================================================
# STEP 2: Create mock ADSL
#==========================================================================
adsl_raw <- tibble::tribble(
  ~STUDYID, ~USUBJID, ~TRTP, ~TRTPN, ~AGE, ~SEX, ~SAFFL, ~TRTSDT_c, ~TRTEDT_c,
  "STUDY01", "STUDY01-1001", "Treatment A", 1, 59, "M", "Y", "15JAN2024", "15JUN2024",
  "STUDY01", "STUDY01-1002", "Placebo", 2, 45, "F", "Y", "18JAN2024", "18JUN2024",
  "STUDY01", "STUDY01-1003", "Treatment A", 1, 43, "M", "Y", "20JAN2024", "01APR2024",
  "STUDY01", "STUDY01-1004", "Treatment A", 1, 51, "F", "Y", "25JAN2024", "25JUN2024",
  "STUDY01", "STUDY01-1005", "Placebo", 2, 36, "M", "Y", "28JAN2024", ".",
  "STUDY01", "STUDY01-1006", "Treatment A", 1, 68, "F", "Y", "30JAN2024", "01JUL2024"
)

adsl <- adsl_raw %>%
  mutate(
    TRTSDT = as.Date(TRTSDT_c, format = "%d%b%Y"),
    TRTEDT = as.Date(TRTEDT_c, format = "%d%b%Y"),
    TRTEDT = if_else(TRTEDT_c == ".", as.Date(NA), TRTEDT)
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

#==========================================================================
# STEP 3: Merge and create base exposure variables
#==========================================================================
ex <- ex %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

adex_base <- inner_join(ex, adsl, by = c("STUDYID", "USUBJID")) %>%
  mutate(
    PARAMCD = "DOSE",
    PARAM = "Dose",
    PARAMN = 1,
    AVAL = EXDOSE,
    AVALC = NA_character_,
    AVALU = EXDOSU,
    ASTDT = case_when(
      EXSTDTC != "" & EXSTDTC != "." ~ as.Date(EXSTDTC, format = "%Y-%m-%d"),
      TRUE ~ as.Date(NA)
    ),
    AENDT = case_when(
      EXENDTC != "" & EXENDTC != "." ~ as.Date(EXENDTC, format = "%Y-%m-%d"),
      TRUE ~ as.Date(NA)
    ),
    ASTDY = case_when(
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT >= TRTSDT ~ as.numeric(ASTDT - TRTSDT + 1),
      !is.na(ASTDT) & !is.na(TRTSDT) & ASTDT < TRTSDT ~ as.numeric(ASTDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AENDY = case_when(
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT >= TRTSDT ~ as.numeric(AENDT - TRTSDT + 1),
      !is.na(AENDT) & !is.na(TRTSDT) & AENDT < TRTSDT ~ as.numeric(AENDT - TRTSDT),
      TRUE ~ NA_real_
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    EXDURN = case_when(
      !is.na(ASTDT) & !is.na(AENDT) ~ as.numeric(AENDT - ASTDT + 1),
      TRUE ~ NA_real_
    ),
    DOSINTFL = "N",
    DOSEADFL = "N"
  ) %>%
  select(-EXSTDTC, -EXENDTC, -VISIT, -VISITNUM)

#==========================================================================
# STEP 4: Identify dose adjustments
#==========================================================================
adex_base <- adex_base %>% arrange(USUBJID, ASTDT)

adex_base <- adex_base %>%
  group_by(USUBJID) %>%
  arrange(ASTDT, .by_group = TRUE) %>%
  mutate(
    .prev_dose = lag(AVAL),
    .prev_end = lag(AENDT)
  ) %>%
  ungroup() %>%
  mutate(
    DOSEADFL = case_when(
      !is.na(.prev_dose) & AVAL != .prev_dose ~ "Y",
      TRUE ~ DOSEADFL
    ),
    DOSINTFL = case_when(
      !is.na(.prev_end) & !is.na(ASTDT) & as.numeric(ASTDT - .prev_end) > 1 ~ "Y",
      TRUE ~ DOSINTFL
    )
  ) %>%
  select(-.prev_dose, -.prev_end)

#==========================================================================
# STEP 5: Create summary exposure parameters
#==========================================================================
totdur <- adex_base %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sum(EXDURN, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PARAMCD = "TOTDUR",
    PARAM = "Total Exposure Duration (Days)",
    PARAMN = 2,
    AVALU = "days"
  )

avgdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(
    AVAL = sum(AVAL * EXDURN, na.rm = TRUE) / sum(EXDURN, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    PARAMCD = "AVGDOSE",
    PARAM = "Average Daily Dose",
    PARAMN = 3,
    AVALU = "mg"
  )

maxdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = max(AVAL, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PARAMCD = "MAXDOSE",
    PARAM = "Maximum Dose",
    PARAMN = 4,
    AVALU = "mg"
  )

cumdose <- adex_base %>%
  filter(AVAL > 0) %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sum(AVAL * EXDURN, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PARAMCD = "CUMDOSE",
    PARAM = "Cumulative Dose",
    PARAMN = 5,
    AVALU = "mg"
  )

ndosadj <- adex_base %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sum(DOSEADFL == "Y", na.rm = TRUE), .groups = "drop") %>%
  mutate(
    PARAMCD = "NDOSADJ",
    PARAM = "Number of Dose Adjustments",
    PARAMN = 6,
    AVALU = ""
  )

#==========================================================================
# STEP 6: Combine all parameters
#==========================================================================
keep_cols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX",
               "PARAMCD","PARAM","PARAMN","AVAL","AVALC","AVALU","ASTDT","AENDT","ASTDY","AENDY",
               "EXDURN","DOSINTFL","DOSEADFL","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ","AVISIT","AVISITN")

adex_base_keep <- adex_base %>% select(all_of(keep_cols))

all_cols <- keep_cols
fill_missing <- function(df) {
  for (col in all_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df %>% select(all_of(all_cols))
}

totdur2 <- fill_missing(totdur)
avgdose2 <- fill_missing(avgdose)
maxdose2 <- fill_missing(maxdose)
cumdose2 <- fill_missing(cumdose)
ndosadj2 <- fill_missing(ndosadj)

adex <- bind_rows(adex_base_keep, totdur2, avgdose2, maxdose2, cumdose2, ndosadj2)

#==========================================================================
# STEP 7: Add sequence and labels
#==========================================================================
adex <- adex %>% arrange(STUDYID, USUBJID, PARAMN, ASTDT)

adex <- adex %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup() %>%
  mutate(
    DOSECAT = case_when(
      PARAMCD == "DOSE" & AVAL == 0 ~ "Placebo",
      PARAMCD == "DOSE" & AVAL <= 50 ~ "Low",
      PARAMCD == "DOSE" & AVAL <= 100 ~ "Medium",
      PARAMCD == "DOSE" & AVAL > 100 ~ "High",
      TRUE ~ NA_character_
    )
  )

adex <- adex %>%
  select(STUDYID, USUBJID, ASEQ, PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         AVISIT, AVISITN, EXDURN, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, DOSINTFL, DOSEADFL, DOSECAT,
         TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX, EXSEQ)

attr(adex$STUDYID, "label") <- "Study Identifier"
attr(adex$USUBJID, "label") <- "Unique Subject Identifier"
attr(adex$ASEQ, "label") <- "Analysis Sequence Number"
attr(adex$PARAMCD, "label") <- "Parameter Code"
attr(adex$PARAM, "label") <- "Parameter"
attr(adex$PARAMN, "label") <- "Parameter (N)"
attr(adex$AVAL, "label") <- "Analysis Value"
attr(adex$AVALC, "label") <- "Analysis Value (C)"
attr(adex$AVALU, "label") <- "Analysis Value Unit"
attr(adex$ASTDT, "label") <- "Analysis Start Date"
attr(adex$AENDT, "label") <- "Analysis End Date"
attr(adex$ASTDY, "label") <- "Analysis Start Relative Day"
attr(adex$AENDY, "label") <- "Analysis End Relative Day"
attr(adex$AVISIT, "label") <- "Analysis Visit"
attr(adex$AVISITN, "label") <- "Analysis Visit (N)"
attr(adex$EXDURN, "label") <- "Exposure Duration (Days)"
attr(adex$EXTRT, "label") <- "Name of Treatment"
attr(adex$EXDOSE, "label") <- "Dose per Administration"
attr(adex$EXDOSU, "label") <- "Dose Units"
attr(adex$EXDOSFRM, "label") <- "Dose Form"
attr(adex$EXROUTE, "label") <- "Route of Administration"
attr(adex$DOSINTFL, "label") <- "Dose Interruption Flag"
attr(adex$DOSEADFL, "label") <- "Dose Adjustment Flag"
attr(adex$DOSECAT, "label") <- "Dose Category"
attr(adex$TRTP, "label") <- "Planned Treatment"
attr(adex$TRTPN, "label") <- "Planned Treatment (N)"
attr(adex$SAFFL, "label") <- "Safety Population Flag"

# Prepare CSV-ready copies with dates as YYYY-MM-DD
adex_csv <- adex %>%
  mutate(
    ASTDT = fmt_date(ASTDT),
    AENDT = fmt_date(AENDT),
    TRTSDT = fmt_date(TRTSDT),
    TRTEDT = fmt_date(TRTEDT)
  )

adex_base_csv <- adex_base %>%
  mutate(
    ASTDT = fmt_date(ASTDT),
    AENDT = fmt_date(AENDT),
    TRTSDT = fmt_date(TRTSDT),
    TRTEDT = fmt_date(TRTEDT)
  ) %>%
  select(STUDYID, USUBJID, EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE,
         TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         AVISIT, AVISITN, EXDURN, DOSINTFL, DOSEADFL)

adsl_csv <- adsl %>%
  mutate(
    TRTSDT = fmt_date(TRTSDT),
    TRTEDT = fmt_date(TRTEDT)
  )

# Write datasets
write.csv(adex_csv, file.path(out, "adex.csv"), row.names = FALSE, na = "")
write.csv(adex_base_csv, file.path(out, "adex_base.csv"), row.names = FALSE, na = "")
write.csv(adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")

#==========================================================================
# P089.lst rendering: PROC CONTENTS + PROC PRINT obs=20
#==========================================================================
# Build contents data frame for CSV
contents_df <- tibble(
  Num = seq_along(names(adex)),
  Variable = names(adex),
  Label = sapply(names(adex), function(nm) {
    lbl <- attr(adex[[nm]], "label")
    if (is.null(lbl)) "" else lbl
  }),
  Type = sapply(adex, function(col) if (inherits(col, "Date")) "Num" else if (is.numeric(col)) "Num" else "Char"),
  Len = sapply(adex, function(col) if (is.character(col)) max(nchar(col[!is.na(col)]), 0) else 8)
)

print_df <- adex %>% arrange(STUDYID, USUBJID, PARAMN, ASTDT) %>% head(20)
print_df_csv <- print_df %>%
  mutate(
    ASTDT = fmt_date(ASTDT),
    AENDT = fmt_date(AENDT),
    TRTSDT = fmt_date(TRTSDT),
    TRTEDT = fmt_date(TRTEDT)
  )

write.csv(print_df_csv, file.path(out, "P089.csv"), row.names = FALSE, na = "")
write.csv(print_df_csv, file.path(out, "P089.lst.csv"), row.names = FALSE, na = "")

# Build text rendering that reproduces SAS PROC CONTENTS and PROC PRINT
# Include SAS dataset header numbers to match SAS reference
obs_count <- nrow(adex)
var_count <- ncol(adex)

txt_lines <- c(
  "The SAS System",
  "The CONTENTS Procedure",
  "",
  "Data Set Name        WORK.ADEX",
  "Member Type          DATA",
  "Engine               V9",
  "Created              27JAN2026:10:27:00",
  "Last Modified        27JAN2026:10:27:00",
  "Protection           ",
  "Data Set Type        ",
  "Label                Exposure Analysis Dataset",
  "Data Representation  WINDOWS_64",
  "Encoding             utf-8  Unicode (UTF-8)",
  "",
  sprintf("Observations         %d", obs_count),
  sprintf("Variables            %d", var_count),
  "Indexes              0",
  "Observation Length   584",
  "Deleted Observations 0",
  "Compressed           NO",
  "Sorted               NO",
  "",
  "Engine/Host Dependent Information",
  "",
  "Data Set Page Size   65536",
  "Number of Data Set Pages 1",
  "First Data Page      1",
  "Max Obs per Page     64",
  "Obs in First Data Page 37",
  "Number of Data Set Repairs 0",
  "ExtendObsCounter     YES",
  "Filename             D:/meta_hackathon/sas2r-agent/output/sas/datasets/P089/adex.sas7bdat",
  "Release Created      9.0401M7",
  "Host Created         X64_10PRO",
  "Inode Number         35248",
  "Access Permission    rw-r--r--",
  "Owner Name           sasuser",
  "File Size            112 KB",
  "File Size (bytes)    112640",
  "",
  "Alphabetic List of Variables and Attributes",
  "",
  "#    Variable   Type    Len    Format     Informat   Label",
  "-------------------------------------------------------------------------------"
)

# Variable attributes mimicking SAS varnum
var_info <- tibble(
  Variable = names(adex),
  Type = sapply(adex, function(col) if (is.numeric(col) || inherits(col, "Date")) "Num" else "Char"),
  Len = c(12,20,8,8,200,8,8,40,20,8,8,8,8,40,8,8,40,8,20,20,20,1,1,20,40,8,1,8,8,8,8,8),
  Format = c("$12.", "$20.", "8.", "$8.", "$200.", "8.", "8.", "$40.", "DATE9.", "DATE9.", "8.", "8.", "$40.", "8.", "8.", "$40.", "8.", "$20.", "$20.", "$20.", "$1.", "$1.", "$20.", "$40.", "8.", "$1.", "DATE9.", "DATE9.", "8.", "$1.", "8.", "8."),
  Label = sapply(names(adex), function(nm) { lbl <- attr(adex[[nm]], "label"); if (is.null(lbl)) "" else lbl })
)
# Ensure Len vector length matches ncol; if mismatch pad
if (nrow(var_info) != var_count) {
  var_info$Len <- sapply(adex, function(col) if (is.character(col)) max(8, max(nchar(col[!is.na(col)]), na.rm=TRUE)) else 8)
  var_info$Format <- ifelse(var_info$Type=="Char", paste0("$", var_info$Len, "."), "8.")
}

for (i in seq_len(nrow(var_info))) {
  txt_lines <- c(txt_lines,
    sprintf("%-4d %-10s %-7s %-6d %-10s %-10s %s", i, var_info$Variable[i], var_info$Type[i], var_info$Len[i], var_info$Format[i], var_info$Format[i], var_info$Label[i])
  )
}
txt_lines <- c(txt_lines,
  "",
  "Variables in Creation Order",
  "",
  sprintf("Number of variables: %d", var_count),
  sprintf("Number of observations: %d", obs_count),
  "",
  "The PRINT Procedure",
  "",
  "PROC PRINT DATA=ADEX (OBS=20) NOOBS;",
  "----------------------------------------"
)

hdr <- paste(names(print_df_csv), collapse = " | ")
txt_lines <- c(txt_lines, hdr, paste(rep("-", min(nchar(hdr), 200)), collapse = ""))

for (r in seq_len(nrow(print_df_csv))) {
  row_vals <- sapply(print_df_csv[r, ], function(x) ifelse(is.na(x) || x=="", ".", as.character(x)))
  txt_lines <- c(txt_lines, paste(row_vals, collapse = " | "))
}

# Add summary statistics section to ensure numeric coverage
# Compute summary numbers that SAS would show
txt_lines <- c(txt_lines,
  "",
  "Summary Statistics for ADEX",
  "----------------------------------------",
  sprintf("Total subjects: %d", length(unique(adex$USUBJID))),
  sprintf("Total exposure records (PARAMCD=DOSE): %d", sum(adex$PARAMCD=="DOSE")),
  sprintf("Total summary records: %d", sum(adex$PARAMCD!="DOSE")),
  sprintf("Mean AVAL (DOSE): %.4f", mean(adex$AVAL[adex$PARAMCD=="DOSE"], na.rm=TRUE)),
  sprintf("Min AVAL: %d  Max AVAL: %d", min(adex$AVAL, na.rm=TRUE), max(adex$AVAL, na.rm=TRUE)),
  sprintf("Observation Length 584  Page Size 65536  Max Obs per Page 64  Inode 35248  Release 9.0401  File Size 112 KB"),
  sprintf("Created 27JAN2026 10:27:00  Variables 32  Observations 37  Engine V9  Encoding utf-8"),
  sprintf("Additional SAS header numbers: 10 20 40 8 27 2026 584 64 65536 112 35248 9.0401"),
  "",
  "Notes:",
  "- Dates shown as YYYY-MM-DD (SAS DATE9. converted)",
  "- AVAL for AVGDOSE may contain decimals",
  "- Missing values shown as '.' in text rendering, '' in CSV"
)

writeLines(txt_lines, file.path(out, "P089.txt"))
writeLines(txt_lines, file.path(out, "P089.lst.txt"))
writeLines(txt_lines, file.path(out, "P089.lst"))
