```r
# P041 translation to R
# ------------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# ------------------------------------------------------------
# Helper functions for SAS formats
fmt_age <- function(x) {
  # SAS format AGE.
  # 1='  18-19', 2='  20-29', 3='  30-39', 4='  40-49', 5='  50-59', 6='  >=60 '
  vec <- c("  18-19", "  20-29", "  30-39", "  40-49", "  50-59", "  >=60 ")
  ifelse(is.na(x), "", vec[x])
}
fmt_sex <- function(x) {
  # SAS format SEX.
  # 1='Male', 2='Female'
  vec <- c("Male", "Female")
  ifelse(is.na(x), "", vec[x])
}

# ------------------------------------------------------------
# Environment
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------
# Read source data
mcsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/phuse-mcsl/mcsl.csv"
mcsl <- read_csv(mcsl_path,
                 col_types = cols(
                   studyid = col_character(),
                   usubjid = col_character(),
                   trt01a  = col_character(),
                   trt01an = col_double(),
                   age     = col_double(),
                   ageu    = col_character(),
                   race    = col_character(),
                   racen   = col_double(),
                   sex     = col_character(),
                   saffl   = col_character(),
                   efffl   = col_character(),
                   bmibl   = col_double(),
                   heightbl= col_double(),
                   weightbl= col_double(),
                   dcdecod = col_character(),
                   dcreascd= col_character()
                 ))

# Create numeric sex variable (sexn)
mcsl <- mcsl %>%
  mutate(sexn = case_when(
    sex == "M" ~ 1L,
    sex == "F" ~ 2L,
    TRUE       ~ NA_integer_
  ))

# ------------------------------------------------------------
# Age categorisation
age1 <- mcsl %>%
  mutate(agecat = case_when(
    age >= 18 & age <= 19 ~ 1L,
    age >= 20 & age <= 29 ~ 2L,
    age >= 30 & age <= 39 ~ 3L,
    age >= 40 & age <= 49 ~ 4L,
    age >= 50 & age <= 59 ~ 5L,
    age >= 60            ~ 6L,
    TRUE                 ~ NA_integer_
  )) %>%
  filter(!is.na(agecat)) %>%
  select(trt01an, usubjid, age, agecat)

# ------------------------------------------------------------
# Age counts per treatment (cat = 1)
agenums <- age1 %>%
  group_by(cat = 1L, subcat = agecat, trt01an, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

agetots <- age1 %>%
  group_by(cat = 1L, subcat = agecat, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(trt01an = 99L)   # total row marker

agecat_df <- bind_rows(agenums, agetots) %>%
  mutate(text = paste0("  ", fmt_age(agecat))) %>%
  arrange(cat, subcat, desc(agecat))

# ------------------------------------------------------------
# Transpose age data (wide)
age_wide <- agecat_df %>%
  select(cat, subcat, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an,
              values_from = num,
              names_prefix = "trt") %>%
  # Ensure all expected treatment columns exist
  mutate(trt0  = ifelse(!"trt0"  %in% names(.), NA_integer_, trt0),
         trt54 = ifelse(!"trt54" %in% names(.), NA_integer_, trt54),
         trt81 = ifelse(!"trt81" %in% names(.), NA_integer_, trt81),
         trt99 = ifelse(!"trt99" %in% names(.), NA_integer_, trt99))

# ------------------------------------------------------------
# Gender counts (cat = 2)
sexg <- mcsl %>%
  filter(sex != " ") %>%
  group_by(cat = 2L, subcat = sexn, trt01an, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

sext <- mcsl %>%
  filter(!is.na(sexn)) %>%
  group_by(cat = 2L, subcat = sexn, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(trt01an = 99L)

sexcat_df <- bind_rows(sexg, sext) %>%
  mutate(text = paste0("  ", fmt_sex(sexn))) %>%
  arrange(cat, subcat, desc(sexn))

# ------------------------------------------------------------
# Transpose gender data (wide)
sex_wide <- sexcat_df %>%
  select(cat, subcat, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an,
              values_from = num,
              names_prefix = "trt") %>%
  mutate(trt0  = ifelse(!"trt0"  %in% names(.), NA_integer_, trt0),
         trt54 = ifelse(!"trt54" %in% names(.), NA_integer_, trt54),
         trt81 = ifelse(!"trt81" %in% names(.), NA_integer_, trt81),
         trt99 = ifelse(!"trt99" %in% names(.), NA_integer_, trt99))

# ------------------------------------------------------------
# Combine age and sex data
combined <- bind_rows(age_wide, sex_wide) %>%
  arrange(cat, subcat)

# ------------------------------------------------------------
# Insert total rows (subcat = 0) before each category block
total_age <- tibble(
  cat = 1L,
  subcat = 0L,
  text = "Age",
  trt0 = NA_integer_, trt54 = NA_integer_, trt81 = NA_integer_, trt99 = NA_integer_
)

total_gender <- tibble(
  cat = 2L,
  subcat = 0L,
  text = "Gender",
  trt0 = NA_integer_, trt54 = NA_integer_, trt81 = NA_integer_, trt99 = NA_integer_
)

final_df <- bind_rows(total_age, total_gender, combined) %>%
  arrange(cat, subcat) %>%
  mutate(
    col1 = ifelse(is.na(trt0),  "", as.character(trt0)),
    col2 = ifelse(is.na(trt54), "", as.character(trt54)),
    col3 = ifelse(is.na(trt81), "", as.character(trt81)),
    col4 = ifelse(is.na(trt99), "", as.character(trt99))
  ) %>%
  select(cat, subcat, text, col1, col2, col3, col4)

# ------------------------------------------------------------
# Write CSV version of the table
write.csv(final_df,
          file = file.path(out_dir, "P041.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------
# Write plain‑text rendering (listing)
txt_file <- file.path(out_dir, "P041.txt")
con <- file(txt_file, open = "wt", encoding = "UTF-8")

# Title and footnote
writeLines("Demography", con)
writeLines("", con)

# Header line (matching PROC REPORT column titles)
header <- sprintf("%-30s%12s%12s%12s%12s",
                  " ", "Group 1", "Group 2", "Group 3", "Total")
writeLines(header, con)
writeLines(strrep("-", nchar(header)), con)

# Data rows
for (i in seq_len(nrow(final_df))) {
  line <- sprintf("%-30s%12s%12s%12s%12s",
                  final_df$text[i],
                  final_df$col1[i],
                  final_df$col2[i],
                  final_df$col3[i],
                  final_df$col4[i])
  writeLines(line, con)
}
writeLines("", con)
writeLines("NOTE: Numbers in parentheses are percentages", con)

close(con)

# ------------------------------------------------------------
# End of script
```