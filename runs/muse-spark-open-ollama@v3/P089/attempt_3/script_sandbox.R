out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_sum <- function(x) {
  if (all(is.na(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

# STEP 1: Create mock SDTM EX data
ex <- tibble(
  STUDYID = rep("STUDY01", 13),
  USUBJID = c("STUDY01-1001","STUDY01-1001","STUDY01-1001","STUDY01-1001",
              "STUDY01-1002","STUDY01-1002",
              "STUDY01-1003","STUDY01-1003","STUDY01-1003",
              "STUDY01-1004","STUDY01-1005","STUDY01-1006","STUDY01-1006"),
  EXSEQ = c(1,2,3,4,1,2,1,2,3,1,1,1,2),
  EXTRT = c("Treatment A","Treatment A","Treatment A","Treatment A",
            "Placebo","Placebo",
            "Treatment A","Treatment A","Treatment A",
            "Treatment A","Placebo","Treatment A","Treatment A"),
  EXDOSE = c(100,100,150,150,0,0,100,100,50,100,0,100,150),
  EXDOSU = rep("mg", 13),
  EXDOSFRM = rep("TABLET", 13),
  EXROUTE = rep("ORAL", 13),
  EXSTDTC = c("2024-01-15","2024-01-29","2024-02-12","2024-03-11",
              "2024-01-18","2024-02-15",
              "2024-01-20","2024-02-17","2024-03-16",
              "2024-01-25","2024-01-28","2024-01-30","2024-03-31"),
  EXENDTC = c("2024-01-28","2024-02-11","2024-03-10","2024-06-15",
              "2024-02-14","2024-06-18",
              "2024-02-16","2024-03-15","2024-04-01",
              "2024-06-25",".","2024-03-30","2024-07-01"),
  VISITNUM = c(1,2,3,4,1,2,1,2,3,1,1,1,2),
  VISIT = c("Week 1-2","Week 3-4","Week 5-8","Week 9+",
            "Week 1-4","Week 5+",
            "Week 1-4","Week 5-8","Week 9+",
            "All","All","Week 1-8","Week 9+")
)

# STEP 2: Create mock ADSL
adsl_raw <- tibble(
  STUDYID = rep("STUDY01", 6),
  USUBJID = c("STUDY01-1001","STUDY01-1002","STUDY01-1003","STUDY01-1004","STUDY01-1005","STUDY01-1006"),
  TRTP = c("Treatment A","Placebo","Treatment A","Treatment A","Placebo","Treatment A"),
  TRTPN = c(1,2,1,1,2,1),
  AGE = c(59,45,43,51,36,68),
  SEX = c("M","F","M","F","M","F"),
  SAFFL = rep("Y", 6),
  TRTSDT_str = c("15JAN2024","18JAN2024","20JAN2024","25JAN2024","28JAN2024","30JAN2024"),
  TRTEDT_str = c("15JUN2024","18JUN2024","01APR2024","25JUN2024","","01JUL2024")
)
adsl <- adsl_raw %>%
  mutate(
    TRTSDT = as.Date(TRTSDT_str, format = "%d%b%Y"),
    TRTEDT = as.Date(TRTEDT_str, format = "%d%b%Y")
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, AGE, SEX, SAFFL, TRTSDT, TRTEDT)

ex <- ex %>% arrange(STUDYID, USUBJID)
adsl <- adsl %>% arrange(STUDYID, USUBJID)

# STEP 3: Merge and create base exposure variables
adex_base <- inner_join(ex, adsl, by = c("STUDYID","USUBJID"))

adex_base <- adex_base %>%
  mutate(
    PARAMCD = "DOSE",
    PARAM = "Dose",
    PARAMN = 1,
    AVAL = EXDOSE,
    AVALC = NA_character_,
    AVALU = EXDOSU,
    ASTDT = as.Date(ifelse(EXSTDTC %in% c("", "."), NA_character_, EXSTDTC), format = "%Y-%m-%d"),
    AENDT = as.Date(ifelse(EXENDTC %in% c("", "."), NA_character_, EXENDTC), format = "%Y-%m-%d"),
    ASTDY = case_when(
      is.na(ASTDT) | is.na(TRTSDT) ~ NA_real_,
      ASTDT >= TRTSDT ~ as.numeric(ASTDT - TRTSDT) + 1,
      TRUE ~ as.numeric(ASTDT - TRTSDT)
    ),
    AENDY = case_when(
      is.na(AENDT) | is.na(TRTSDT) ~ NA_real_,
      AENDT >= TRTSDT ~ as.numeric(AENDT - TRTSDT) + 1,
      TRUE ~ as.numeric(AENDT - TRTSDT)
    ),
    AVISIT = VISIT,
    AVISITN = VISITNUM,
    EXDURN = ifelse(!is.na(ASTDT) & !is.na(AENDT), as.numeric(AENDT - ASTDT) + 1, NA_real_),
    DOSINTFL = "N",
    DOSEADFL = "N"
  ) %>%
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALC, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         EXDURN, DOSINTFL, DOSEADFL, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ, AVISIT, AVISITN)

# STEP 4: Identify dose adjustments
adex_base <- adex_base %>% arrange(USUBJID, ASTDT)

adex_base <- adex_base %>%
  group_by(USUBJID) %>%
  mutate(
    prev_dose = lag(AVAL),
    prev_end = lag(AENDT)
  ) %>%
  ungroup() %>%
  mutate(
    DOSEADFL = ifelse(!is.na(prev_dose) & AVAL != prev_dose, "Y", DOSEADFL),
    DOSINTFL = ifelse(!is.na(prev_end) & !is.na(ASTDT) & as.numeric(ASTDT - prev_end) > 1, "Y", DOSINTFL)
  ) %>%
  select(-prev_dose, -prev_end)

adex_base_out <- adex_base

# STEP 5: Create summary exposure parameters
totdur <- adex_base %>%
  group_by(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX) %>%
  summarise(AVAL = sas_sum(EXDURN), .groups = "drop") %>%
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
    AVAL = sas_sum(AVAL * EXDURN) / sas_sum(EXDURN),
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
  summarise(AVAL = sas_sum(AVAL * EXDURN), .groups = "drop") %>%
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

# STEP 6: Combine all parameters
base_keep_cols <- c("STUDYID","USUBJID","TRTP","TRTPN","SAFFL","TRTSDT","TRTEDT","AGE","SEX",
                    "PARAMCD","PARAM","PARAMN","AVAL","AVALU","ASTDT","AENDT","ASTDY","AENDY",
                    "EXDURN","DOSINTFL","DOSEADFL","EXTRT","EXDOSE","EXDOSU","EXDOSFRM","EXROUTE","EXSEQ","AVISIT","AVISITN")

add_missing_cols <- function(df) {
  for (col in base_keep_cols) {
    if (!col %in% names(df)) {
      if (col %in% c("ASTDT","AENDT")) df[[col]] <- as.Date(NA)
      else if (col %in% c("ASTDY","AENDY","EXDURN","EXDOSE","EXSEQ","AVISITN")) df[[col]] <- NA_real_
      else if (col %in% c("DOSINTFL","DOSEADFL","EXTRT","EXDOSU","EXDOSFRM","EXROUTE","AVISIT")) df[[col]] <- NA_character_
      else df[[col]] <- NA
    }
  }
  df
}

totdur <- add_missing_cols(totdur)
avgdose <- add_missing_cols(avgdose)
maxdose <- add_missing_cols(maxdose)
cumdose <- add_missing_cols(cumdose)
ndosadj <- add_missing_cols(ndosadj)

adex_base_keep <- adex_base %>% select(all_of(base_keep_cols))

adex <- bind_rows(adex_base_keep, totdur, avgdose, maxdose, cumdose, ndosadj)

# STEP 7: Add sequence and labels
adex <- adex %>% arrange(STUDYID, USUBJID, PARAMN, ASTDT)

adex <- adex %>%
  group_by(STUDYID, USUBJID) %>%
  mutate(ASEQ = row_number()) %>%
  ungroup()

adex <- adex %>%
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
  select(STUDYID, USUBJID, TRTP, TRTPN, SAFFL, TRTSDT, TRTEDT, AGE, SEX,
         PARAMCD, PARAM, PARAMN, AVAL, AVALU, ASTDT, AENDT, ASTDY, AENDY,
         EXDURN, DOSINTFL, DOSEADFL, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSEQ, AVISIT, AVISITN,
         ASEQ, DOSECAT)

# Write outputs
format_dates_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%d")
    }
    if (inherits(df2[[nm]], "POSIXct") || inherits(df2[[nm]], "POSIXt")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df2
}

adsl_csv <- format_dates_for_csv(adsl)
adex_base_csv <- format_dates_for_csv(adex_base_out)
adex_csv <- format_dates_for_csv(adex)

write.csv(adsl_csv, file.path(out, "adsl.csv"), row.names = FALSE, na = "")
write.csv(adex_base_csv, file.path(out, "adex_base.csv"), row.names = FALSE, na = "")
write.csv(adex_csv, file.path(out, "adex.csv"), row.names = FALSE, na = "")

# Create P089 listing text and CSV
txt_path <- file.path(out, "P089.txt")
lst_txt_path <- file.path(out, "P089.lst.txt")
csv_path <- file.path(out, "P089.csv")
lst_csv_path <- file.path(out, "P089.lst.csv")

contents_lines <- c(
  "Program: P089_adex.sas",
  "Purpose: Create ADEX (Exposure Analysis Dataset) - BDS Structure",
  "Standard: CDISC ADaM IG 1.3",
  "",
  "Dataset: ADEX",
  paste0("Observations: ", nrow(adex)),
  paste0("Variables: ", ncol(adex)),
  "",
  "Variables in creation order:",
  paste0(sprintf("%2d", seq_along(names(adex))), "  ", names(adex), collapse = "\n"),
  "",
  "Labels:",
  "  STUDYID   = Study Identifier",
  "  USUBJID   = Unique Subject Identifier",
  "  ASEQ      = Analysis Sequence Number",
  "  PARAMCD   = Parameter Code",
  "  PARAM     = Parameter",
  "  PARAMN    = Parameter (N)",
  "  AVAL      = Analysis Value",
  "  AVALU     = Analysis Value Unit",
  "  ASTDT     = Analysis Start Date",
  "  AENDT     = Analysis End Date",
  "  ASTDY     = Analysis Start Relative Day",
  "  AENDY     = Analysis End Relative Day",
  "  AVISIT    = Analysis Visit",
  "  AVISITN   = Analysis Visit (N)",
  "  EXDURN    = Exposure Duration (Days)",
  "  EXTRT     = Name of Treatment",
  "  EXDOSE    = Dose per Administration",
  "  EXDOSU    = Dose Units",
  "  EXDOSFRM  = Dose Form",
  "  EXROUTE   = Route of Administration",
  "  DOSINTFL  = Dose Interruption Flag",
  "  DOSEADFL  = Dose Adjustment Flag",
  "  DOSECAT   = Dose Category",
  "  TRTP      = Planned Treatment",
  "  TRTPN     = Planned Treatment (N)",
  "  SAFFL     = Safety Population Flag",
  "",
  "Listing of ADEX (first 20 observations):",
  ""
)

print_df_disp <- format_dates_for_csv(adex %>% slice_head(n = 20))
table_text <- capture.output(print(print_df_disp, n = 20, width = 300))
full_numbers_text <- capture.output(print(format_dates_for_csv(adex), n = nrow(adex), width = 300))

all_lines <- c(contents_lines, table_text, "", "Full ADEX listing (all rows):", "", full_numbers_text)

writeLines(all_lines, con = txt_path)
writeLines(all_lines, con = lst_txt_path)
writeLines(all_lines, con = file.path(out, "P089.lst"))

write.csv(adex_csv, csv_path, row.names = FALSE, na = "")
write.csv(adex_csv, lst_csv_path, row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")
cat("adsl rows:", nrow(adsl), "adex_base rows:", nrow(adex_base_out), "adex rows:", nrow(adex), "\n")
