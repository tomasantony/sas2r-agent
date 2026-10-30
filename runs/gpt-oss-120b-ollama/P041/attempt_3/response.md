```r
# ------------------------------------------------------------
# P041_t-demog.sas  -->  R translation (attempt 3)
# ------------------------------------------------------------
# Load required packages (pre‑installed in the execution environment)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

# ------------------------------------------------------------
# Helper functions to mimic SAS formats
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
# Output directory (provided by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------
# Read source CSV (all columns as character, then coerce needed ones)
mcsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/phuse-mcsl/mcsl.csv"

raw <- read_csv(mcsl_path, col_types = cols(.default = col_character()))

# Standardise column names to lower case (SAS is case‑insensitive)
raw <- raw %>% rename_all(tolower)

# Coerce numeric columns that are needed as numbers
raw <- raw %>%
  mutate(
    trt01an = as.numeric(trt01an),
    age     = as.numeric(age),
    racen   = as.numeric(racen),
    bmibl   = as.numeric(bmibl),
    heightbl= as.numeric(heightbl),
    weightbl= as.numeric(weightbl)
  )

# ------------------------------------------------------------
# Create numeric sex variable (sexn) – SAS used 'M'/'F' in variable SEX
mcsl <- raw %>%
  mutate(
    sexn = case_when(
      sex == "M" ~ 1L,
      sex == "F" ~ 2L,
      TRUE       ~ NA_integer_
    )
  )

# ------------------------------------------------------------
# ---------- Age categorisation ----------
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

# Age counts per treatment (cat = 1)
agenums <- age1 %>%
  group_by(cat = 1L, subcat = agecat, trt01an, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

# Age totals across all treatments (cat = 1)
agetots <- age1 %>%
  group_by(cat = 1L, subcat = agecat, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(trt01an = 99L)   # SAS uses 99 for the total column

agecat_df <- bind_rows(agenums, agetots) %>%
  mutate(text = paste0("  ", fmt_age(agecat))) %>%
  arrange(cat, subcat, desc(agecat))

# Transpose to wide format (one row per age category)
age_wide <- agecat_df %>%
  select(cat, subcat, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an,
              values_from = num,
              names_prefix = "trt") %>%
  # Ensure the four expected treatment columns exist
  mutate(trt0  = ifelse(!"trt0"  %in% names(.), NA_integer_, trt0),
         trt54 = ifelse(!"trt54" %in% names(.), NA_integer_, trt54),
         trt81 = ifelse(!"trt81" %in% names(.), NA_integer_, trt81),
         trt99 = ifelse(!"trt99" %in% names(.), NA_integer_, trt99))

# ------------------------------------------------------------
# ---------- Gender (sex) ----------
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
# ---------- Combine age and sex ----------
combined <- bind_rows(age_wide, sex_wide) %>%
  arrange(cat, subcat)

# ------------------------------------------------------------
# ---------- Insert total rows exactly as SAS does ----------
# SAS writes the first data row, then immediately writes a total row,
# then the remaining rows for that category.
make_total_row <- function(cat_val, label) {
  tibble(
    cat    = cat_val,
    subcat = 0L,
    text   = label,
    trt0   = NA_integer_,
    trt54  = NA_integer_,
    trt81  = NA_integer_,
    trt99  = NA_integer_
  )
}

# Helper to split first row and the rest for a given cat
split_first_rest <- function(df, cat_val) {
  df_cat <- df %>% filter(cat == cat_val)
  if (nrow(df_cat) == 0) return(tibble())
  first_row <- df_cat %>% slice(1)
  rest_rows <- df_cat %>% slice(-1)
  bind_rows(first_row, make_total_row(cat_val,
                                      ifelse(cat_val == 1L, "Age", "Gender")),
            rest_rows)
}

final_ordered <- bind_rows(
  split_first_rest(combined, 1L),
  split_first_rest(combined, 2L)
) %>%
  arrange(cat, subcat)

# ------------------------------------------------------------
# ---------- Build final reporting table (character counts) ----------
final_tbl <- final_ordered %>%
  mutate(
    col1 = ifelse(is.na(trt0),  "", as.character(trt0)),
    col2 = ifelse(is.na(trt54), "", as.character(trt54)),
    col3 = ifelse(is.na(trt81), "", as.character(trt81)),
    col4 = ifelse(is.na(trt99), "", as.character(trt99))
  ) %>%
  select(cat, subcat, text, col1, col2, col3, col4)

# ------------------------------------------------------------
# ---------- Write outputs ----------
# CSV version (numeric values left blank where SAS would show blanks)
write.csv(final_tbl,
          file = file.path(out_dir, "P041.csv"),
          row.names = FALSE,
          na = "")

# Plain‑text listing mimicking PROC REPORT output
txt_path <- file.path(out_dir, "P041.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

# Title
writeLines("Demography", con)
writeLines("", con)

# Header line (aligned to PROC REPORT column widths)
header <- sprintf("%-30s%12s%12s%12s%12s",
                 " ", "Group 1", "Group 2", "Group 3", "Total")
writeLines(header, con)
writeLines(strrep("-", nchar(header)), con)

# Data rows
for (i in seq_len(nrow(final_tbl))) {
  line <- sprintf("%-30s%12s%12s%12s%12s",
                  final_tbl$text[i],
                  final_tbl$col1[i],
                  final_tbl$col2[i],
                  final_tbl$col3[i],
                  final_tbl$col4[i])
  writeLines(line, con)
}
writeLines("", con)
writeLines("NOTE: Numbers in parentheses are percentages", con)

close(con)

# ------------------------------------------------------------
# End of script
```