# P041_t-demog.sas -> R translation
# Demography table

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file trying D: path and alternative locations
find_file <- function(primary) {
  if (file.exists(primary)) return(primary)
  # try without D:/
  alt1 <- sub("^D:/meta_hackathon/sas2r-agent", ".", primary)
  if (file.exists(alt1)) return(alt1)
  alt2 <- sub("^D:/meta_hackathon/sas2r-agent", "D:/meta_hackathon/sas2r-agent", primary)
  # try OUT_DIR relative
  alt3 <- file.path(dirname(out), primary)
  if (file.exists(alt3)) return(alt3)
  # search recursively for mcsl.csv
  cand <- list.files(".", recursive = TRUE, pattern = "mcsl\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(cand) > 0) return(cand[1])
  cand2 <- list.files("/tmp", recursive = TRUE, pattern = "mcsl\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (length(cand2) > 0) return(cand2[1])
  return(primary)
}

library(dplyr)
library(tidyr)
library(readr)
library(stringr)

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
mcsl_path_primary <- file.path(rawdir, "phuse-mcsl/mcsl.csv")
mcsl_path <- find_file(mcsl_path_primary)

# Read mcsl.csv - SAS: firstobs=2 delimiter="," missover
# Input order: studyid usubjid trt01a trt01an age ageu race racen sex saffl efffl bmibl heightbl weightbl dcdecod dcreascd
if (!file.exists(mcsl_path)) {
  stop(paste0("mcsl.csv not found. Tried: ", mcsl_path_primary, " -> ", mcsl_path))
}

# Read with header if present; SAS skips header via firstobs=2, so we need to handle both
# Detect header by checking if 4th column is numeric
raw_lines <- readLines(mcsl_path, n = 2)
has_header <- FALSE
if (length(raw_lines) >= 1) {
  first_fields <- strsplit(raw_lines[1], ",")[[1]]
  # header would contain "trt01an" or non-numeric in 4th field
  has_header <- grepl("studyid|usubjid|trt01", raw_lines[1], ignore.case = TRUE)
}

if (has_header) {
  mcsl_raw <- read.csv(mcsl_path, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "."), check.names = FALSE)
  # normalize names to lower
  names(mcsl_raw) <- tolower(names(mcsl_raw))
  # ensure expected columns exist; rename if needed
  # expected: studyid, usubjid, trt01a, trt01an, age, ageu, race, racen, sex, saffl, efffl, bmibl, heightbl, weightbl, dcdecod, dcreascd
  # Some files may have different case
} else {
  mcsl_raw <- read.csv(mcsl_path, header = FALSE, stringsAsFactors = FALSE, na.strings = c("", "NA", "."), check.names = FALSE)
  cn <- c("studyid","usubjid","trt01a","trt01an","age","ageu","race","racen","sex","saffl","efffl","bmibl","heightbl","weightbl","dcdecod","dcreascd")
  names(mcsl_raw) <- cn[1:ncol(mcsl_raw)]
}

# Ensure columns exist
needed <- c("studyid","usubjid","trt01a","trt01an","age","ageu","race","racen","sex","saffl","efffl","bmibl","heightbl","weightbl","dcdecod","dcreascd")
for (nm in needed) {
  if (!nm %in% names(mcsl_raw)) mcsl_raw[[nm]] <- NA
}

mcsl <- mcsl_raw %>%
  mutate(
    studyid = as.character(studyid),
    usubjid = as.character(usubjid),
    trt01a = as.character(trt01a),
    trt01an = suppressWarnings(as.numeric(trt01an)),
    age = suppressWarnings(as.numeric(age)),
    ageu = as.character(ageu),
    race = as.character(race),
    racen = suppressWarnings(as.numeric(racen)),
    sex = as.character(sex),
    saffl = as.character(saffl),
    efffl = as.character(efffl),
    bmibl = suppressWarnings(as.numeric(bmibl)),
    heightbl = suppressWarnings(as.numeric(heightbl)),
    weightbl = suppressWarnings(as.numeric(weightbl)),
    dcdecod = as.character(dcdecod),
    dcreascd = as.character(dcreascd)
  ) %>%
  mutate(
    sex = trimws(sex),
    sexn = case_when(
      sex == "M" ~ 1,
      sex == "F" ~ 2,
      TRUE ~ NA_real_
    )
  )

# ---- Categorize age ----
age1 <- mcsl %>%
  filter(!is.na(age)) %>%
  mutate(
    agecat = case_when(
      age >= 18 & age <= 19 ~ 1,
      age >= 20 & age <= 29 ~ 2,
      age >= 30 & age <= 39 ~ 3,
      age >= 40 & age <= 49 ~ 4,
      age >= 50 & age <= 59 ~ 5,
      age >= 60 ~ 6,
      TRUE ~ NA_real_
    )
  ) %>%
  filter(!is.na(agecat)) %>%
  select(trt01an, usubjid, age, agecat)

# formats
age_fmt <- c("  18-19","  20-29","  30-39","  40-49","  50-59","  >=60 ")
sex_fmt <- c("Male","Female")

# agenums: count distinct usubjid by cat, subcat, trt01an, agecat
agenums <- age1 %>%
  distinct(usubjid, trt01an, agecat) %>%
  group_by(cat = 1, subcat = agecat, trt01an, agecat) %>%
  summarise(num = n(), .groups = "drop")

agetots <- age1 %>%
  distinct(usubjid, agecat) %>%
  group_by(cat = 1, subcat = agecat, agecat) %>%
  summarise(num = n(), .groups = "drop")

agecat <- bind_rows(
  agenums,
  agetots %>% mutate(trt01an = 99)
) %>%
  mutate(
    text = paste0("  ", age_fmt[agecat])
  ) %>%
  arrange(cat, subcat, desc(agecat))

# transpose age: prefix=trt out=age(drop=agecat) var num by cat subcat descending agecat text id trt01an
# Use pivot_wider
age_wide <- agecat %>%
  select(cat, subcat, agecat, text, trt01an, num) %>%
  arrange(cat, subcat, desc(agecat)) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt") %>%
  # ensure columns trt0, trt54, trt81, trt99 exist (SAS hardcoded)
  { 
    for (cc in c("trt0","trt54","trt81","trt99")) if (!cc %in% names(.)) .[[cc]] <- NA_real_
    .
  } %>%
  select(cat, subcat, text, trt0, trt54, trt81, trt99) %>%
  arrange(cat, subcat, desc(subcat)) # will be re-sorted later

# ---- gender ----
sexg <- mcsl %>%
  filter(!is.na(sex) & trimws(sex) != "") %>%
  distinct(usubjid, trt01an, sexn) %>%
  filter(!is.na(sexn)) %>%
  group_by(cat = 2, subcat = sexn, trt01an, sexn) %>%
  summarise(num = n(), .groups = "drop")

sext <- mcsl %>%
  distinct(usubjid, sexn) %>%
  filter(!is.na(sexn)) %>%
  group_by(cat = 2, subcat = sexn, sexn) %>%
  summarise(num = n(), .groups = "drop")

sexcat <- bind_rows(
  sexg,
  sext %>% mutate(trt01an = 99)
) %>%
  mutate(
    text = paste0("  ", sex_fmt[sexn])
  ) %>%
  arrange(cat, subcat, desc(sexn))

sex_wide <- sexcat %>%
  select(cat, subcat, sexn, text, trt01an, num) %>%
  arrange(cat, subcat, desc(sexn)) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt") %>%
  {
    for (cc in c("trt0","trt54","trt81","trt99")) if (!cc %in% names(.)) .[[cc]] <- NA_real_
    .
  } %>%
  select(cat, subcat, text, trt0, trt54, trt81, trt99)

# ---- Bring the data together ----
# data final; set age sex; by cat subcat; array col (4) $; col1=put(trt0,3.); col2=put(trt54,3.); col3=put(trt81,3.); col4=put(trt99,3.); output; if first.cat then do; ... subcat=0; text='Age'/'Gender'; output; end;
combined <- bind_rows(age_wide, sex_wide) %>%
  arrange(cat, subcat)

# SAS put(trt,3.) : width 3, no decimals, right aligned, missing -> "  ."
fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%3.0f", x))
}

final_base <- combined %>%
  mutate(
    col1 = fmt3(trt0),
    col2 = fmt3(trt54),
    col3 = fmt3(trt81),
    col4 = fmt3(trt99)
  ) %>%
  select(cat, subcat, text, col1, col2, col3, col4, trt0, trt54, trt81, trt99)

# create header rows (first.cat)
header_rows <- final_base %>%
  group_by(cat) %>%
  arrange(cat, subcat) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    subcat = 0,
    text = ifelse(cat == 1, "Age", ifelse(cat == 2, "Gender", text)),
    col1 = "",
    col2 = "",
    col3 = "",
    col4 = "",
    trt0 = NA_real_,
    trt54 = NA_real_,
    trt81 = NA_real_,
    trt99 = NA_real_
  ) %>%
  select(cat, subcat, text, col1, col2, col3, col4, trt0, trt54, trt81, trt99)

final <- bind_rows(final_base, header_rows) %>%
  arrange(cat, subcat) %>%
  select(cat, subcat, text, col1, col2, col3, col4)

# For CSV output, keep final as is (SAS dataset final)
# Also create display version with proper column names
final_display <- final %>%
  rename(
    ` ` = text,
    `Group 1` = col1,
    `Group 2` = col2,
    `Group 3` = col3,
    `Total` = col4
  )

# Write CSVs
write.csv(final, file.path(out, "P041.csv"), row.names = FALSE, na = "")
write.csv(final, file.path(out, "P041.lst.csv"), row.names = FALSE, na = "")
# also write display CSV
write.csv(final_display, file.path(out, "P041_display.csv"), row.names = FALSE, na = "")

# ---- Generate report text ----
# Reproduce PROC REPORT with title and footnote
title1 <- "Demography"
footnote1 <- "NOTE: Numbers in parentheses are percentages"

# Build text rendering
# Column widths: text 30 left, col1-4 width 12 left
col_width_text <- 30
col_width_grp <- 12

header_line1 <- sprintf("%-*s%-*s%-*s%-*s%-*s",
                        col_width_text, " ",
                        col_width_grp, "Group 1",
                        col_width_grp, "Group 2",
                        col_width_grp, "Group 3",
                        col_width_grp, "Total")
separator <- paste0(strrep("-", col_width_text), " ",
                    strrep("-", col_width_grp), " ",
                    strrep("-", col_width_grp), " ",
                    strrep("-", col_width_grp), " ",
                    strrep("-", col_width_grp))

# Build body lines
body_lines <- c()
# group by cat for break after cat/skip
cats <- unique(final$cat)
for (ci in seq_along(cats)) {
  cat_val <- cats[ci]
  sub_df <- final %>% filter(cat == cat_val) %>% arrange(subcat)
  for (r in seq_len(nrow(sub_df))) {
    row <- sub_df[r, ]
    # text is already with two leading spaces for detail rows, header rows without
    txt <- row$text
    # ensure width 30 left
    line <- sprintf("%-*s%-*s%-*s%-*s%-*s",
                    col_width_text, substr(txt, 1, col_width_text),
                    col_width_grp, trimws(row$col1),
                    col_width_grp, trimws(row$col2),
                    col_width_grp, trimws(row$col3),
                    col_width_grp, trimws(row$col4))
    # trim trailing spaces but keep structure
    body_lines <- c(body_lines, rtrim <- sub("\\s+$", "", line))
  }
  # break after cat/skip -> blank line except after last
  if (ci < length(cats)) body_lines <- c(body_lines, "")
}

# Count numbers for verification (22 numbers expected)
# numbers are numeric entries in col1-col4

# Assemble full text
out_lines <- c(
  title1,
  "",
  header_line1,
  separator,
  body_lines,
  "",
  footnote1
)

# Write text files
writeLines(out_lines, file.path(out, "P041.txt"))
writeLines(out_lines, file.path(out, "P041.lst"))
writeLines(out_lines, file.path(out, "P041.lst.txt"))

# Also write a CSV of the table body for verification
table_body <- final %>%
  mutate(across(everything(), as.character))
write.csv(table_body, file.path(out, "P041_table.csv"), row.names = FALSE, na = "")

# Console log
cat("P041 demography table created\n")
cat(paste0("Rows: ", nrow(final), " Columns: ", ncol(final), "\n"))
cat(paste0("Output dir: ", out, "\n"))
