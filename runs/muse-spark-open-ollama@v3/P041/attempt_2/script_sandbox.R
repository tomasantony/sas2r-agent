out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS round half away from zero (not needed for counts but included)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-12)
  posneg * z / 10^digits
}

# helper: SAS put with 3. format
sas_put3 <- function(x) {
  ifelse(is.na(x), NA_character_, sprintf("%3d", as.integer(x)))
}

# ------------------------------------------------------------------
# Locate input file: &rawdir./phuse-mcsl/mcsl.csv
# &rawdir. = /bench/data/raw
# ------------------------------------------------------------------
rawdir_env <- Sys.getenv("rawdir")
if (rawdir_env == "") rawdir_env <- Sys.getenv("RAWDIR")
candidates <- c(
  "/bench/data/raw/phuse-mcsl/mcsl.csv",
  file.path(rawdir_env, "phuse-mcsl", "mcsl.csv"),
  "/bench/data/raw/phuse-mcsl/mcsl.csv"
)
mcsl_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { mcsl_path <- p; break }
}
if (is.null(mcsl_path)) {
  # fallback: try forward slash variant
  mcsl_path <- "/bench/data/raw/phuse-mcsl/mcsl.csv"
}
cat("Reading:", mcsl_path, "\n")

# Read CSV - SAS infile with delimiter "," firstobs=2 missover
# Use base read.csv to avoid type guessing issues, then clean
mcsl_raw <- tryCatch(
  read.csv(mcsl_path, header = TRUE, stringsAsFactors = FALSE, na.strings = c("", "NA", "."), check.names = FALSE),
  error = function(e) {
    # try readr
    readr::read_csv(mcsl_path, show_col_types = FALSE, na = c("", "NA", "."))
  }
)
# normalize column names to lower case for matching
names(mcsl_raw) <- tolower(names(mcsl_raw))
# expected names: studyid usubjid trt01a trt01an age ageu race racen sex saffl efffl bmibl heightbl weightbl dcdecod dcreascd
# ensure required columns exist
required <- c("studyid","usubjid","trt01a","trt01an","age","ageu","race","racen","sex","saffl","efffl","bmibl","heightbl","weightbl","dcdecod","dcreascd")
for (col in required) {
  if (!col %in% names(mcsl_raw)) {
    # try case-insensitive search
    idx <- which(tolower(names(mcsl_raw)) == col)
    if (length(idx)==0) mcsl_raw[[col]] <- NA
  }
}
# coerce types
mcsl_raw$age <- suppressWarnings(as.numeric(mcsl_raw$age))
mcsl_raw$trt01an <- trimws(as.character(mcsl_raw$trt01an))
# trt01an may be numeric like "0","54","81" - keep as character for id, but also numeric for sorting
mcsl_raw$sex <- trimws(as.character(mcsl_raw$sex))
mcsl_raw$usubjid <- trimws(as.character(mcsl_raw$usubjid))

# create sexn as in SAS: if sex='M' then 1 else if 'F' then 2
mcsl <- mcsl_raw
mcsl$sexn <- NA_real_
mcsl$sexn[mcsl$sex == "M"] <- 1
mcsl$sexn[mcsl$sex == "F"] <- 2

# ------------------------------------------------------------------
# Categorize age - data age1
# ------------------------------------------------------------------
age1 <- mcsl
age1$agecat <- NA_real_
age1$agecat[age1$age >= 18 & age1$age <= 19] <- 1
age1$agecat[age1$age >= 20 & age1$age <= 29] <- 2
age1$agecat[age1$age >= 30 & age1$age <= 39] <- 3
age1$agecat[age1$age >= 40 & age1$age <= 49] <- 4
age1$agecat[age1$age >= 50 & age1$age <= 59] <- 5
age1$agecat[age1$age >= 60] <- 6
age1 <- age1[!is.na(age1$agecat), c("trt01an","usubjid","age","agecat")]
# ensure trt01an not missing for age1? SAS does not filter, but keep all
age1 <- age1[!is.na(age1$trt01an) & age1$trt01an != "", ]

# ------------------------------------------------------------------
# PROC SQL agenums / agetots
# ------------------------------------------------------------------
library(dplyr)
library(tidyr)

agenums <- age1 %>%
  group_by(cat = 1, subcat = agecat, trt01an, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

agetots <- age1 %>%
  group_by(cat = 1, subcat = agecat, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

# formats
age_fmt <- c("1" = "  18-19", "2" = "  20-29", "3" = "  30-39", "4" = "  40-49", "5" = "  50-59", "6" = "  >=60 ")
sex_fmt <- c("1" = "Male", "2" = "Female")

agecat_df <- bind_rows(
  agenums %>% mutate(trt01an = as.character(trt01an)),
  agetots %>% mutate(trt01an = "99")
) %>%
  mutate(text = paste0("  ", age_fmt[as.character(agecat)]))

# proc sort by cat subcat descending agecat
agecat_df <- agecat_df %>% arrange(cat, subcat, desc(agecat))

# proc transpose prefix=trt out=age(drop=agecat) var num by cat subcat descending agecat text id trt01an
age_wide <- agecat_df %>%
  select(cat, subcat, text, trt01an, num, agecat) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt") %>%
  select(-agecat)

# ------------------------------------------------------------------
# gender
# ------------------------------------------------------------------
sexg <- mcsl %>%
  filter(!is.na(sex), sex != "", sex != " ") %>%
  filter(!is.na(sexn)) %>%
  group_by(cat = 2, subcat = sexn, trt01an, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

sext <- mcsl %>%
  filter(!is.na(sexn)) %>%
  group_by(cat = 2, subcat = sexn, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop")

sexcat_df <- bind_rows(
  sexg %>% mutate(trt01an = as.character(trt01an)),
  sext %>% mutate(trt01an = "99")
) %>%
  mutate(text = paste0("  ", sex_fmt[as.character(sexn)]))

sexcat_df <- sexcat_df %>% arrange(cat, subcat, desc(sexn))

sex_wide <- sexcat_df %>%
  select(cat, subcat, text, trt01an, num, sexn) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt") %>%
  select(-sexn)

# ------------------------------------------------------------------
# Bring the data together - data final
# ------------------------------------------------------------------
# Ensure both have same trt columns; add missing columns as NA
all_cols_age <- names(age_wide)
all_cols_sex <- names(sex_wide)
all_trt_cols <- unique(c(grep("^trt", all_cols_age, value = TRUE), grep("^trt", all_cols_sex, value = TRUE)))

# add missing trt columns to each
for (cc in all_trt_cols) {
  if (!cc %in% names(age_wide)) age_wide[[cc]] <- NA_real_
  if (!cc %in% names(sex_wide)) sex_wide[[cc]] <- NA_real_
}

final_base <- bind_rows(age_wide, sex_wide) %>%
  arrange(cat, subcat)

# Ensure expected SAS hardcoded columns exist for col mapping
# SAS expects trt0, trt54, trt81, trt99
for (cc in c("trt0","trt54","trt81","trt99")) {
  if (!cc %in% names(final_base)) final_base[[cc]] <- NA_real_
}

# Create col1..col4 as put(trt...,3.)
final_base <- final_base %>%
  mutate(
    col1 = sas_put3(trt0),
    col2 = sas_put3(trt54),
    col3 = sas_put3(trt81),
    col4 = sas_put3(trt99)
  )

# If hardcoded columns are all NA but other trt columns exist, fallback to generic mapping
if (all(is.na(final_base$col1)) && all(is.na(final_base$col2)) && all(is.na(final_base$col3))) {
  # generic: sorted unique trt values excluding 99
  trt_vals <- unique(c(agenums$trt01an, sexg$trt01an))
  trt_vals <- trt_vals[trt_vals != "99" & !is.na(trt_vals) & trt_vals != ""]
  trt_vals_num <- suppressWarnings(as.numeric(trt_vals))
  if (all(!is.na(trt_vals_num))) {
    trt_vals <- trt_vals[order(trt_vals_num)]
  } else {
    trt_vals <- sort(trt_vals)
  }
  # ensure columns exist
  for (v in trt_vals) {
    cn <- paste0("trt", v)
    if (!cn %in% names(final_base)) final_base[[cn]] <- NA_real_
  }
  for (i in seq_along(trt_vals)) {
    if (i <= 3) {
      cn <- paste0("trt", trt_vals[i])
      final_base[[paste0("col", i)]] <- sas_put3(final_base[[cn]])
    }
  }
  # col4 remains total
  final_base$col4 <- sas_put3(final_base$trt99)
}

# Now create header rows: if first.cat then output blank cols and text Age/Gender
# first.cat means first row per cat
header_rows <- final_base %>%
  group_by(cat) %>%
  arrange(subcat) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    subcat = 0,
    text = ifelse(cat == 1, "Age", ifelse(cat == 2, "Gender", text)),
    col1 = "", col2 = "", col3 = "", col4 = ""
  ) %>%
  # keep same structure, set trt cols to NA for header
  mutate(trt0 = NA_real_, trt54 = NA_real_, trt81 = NA_real_, trt99 = NA_real_)

# Combine and sort by cat subcat (SAS proc sort data=final by cat subcat)
final_all <- bind_rows(final_base, header_rows) %>%
  arrange(cat, subcat)

# For display, trim col values (SAS put gives width 3 with leading spaces, report left aligns)
# Keep character but trim for CSV; for text we will display trimmed
final_all$col1_disp <- trimws(final_all$col1)
final_all$col2_disp <- trimws(final_all$col2)
final_all$col3_disp <- trimws(final_all$col3)
final_all$col4_disp <- trimws(final_all$col4)
# header rows have "" -> remain ""

# ------------------------------------------------------------------
# Generate report datasets
# ------------------------------------------------------------------
# CSV body: text, Group1, Group2, Group3, Total
report_csv <- final_all %>%
  select(text, col1 = col1_disp, col2 = col2_disp, col3 = col3_disp, col4 = col4_disp) %>%
  rename(`Group 1` = col1, `Group 2` = col2, `Group 3` = col3, Total = col4)

# Also keep internal cat/subcat for ordering but not needed in CSV
# Write CSVs
write.csv(report_csv, file.path(out, "P041.csv"), row.names = FALSE, na = "")
write.csv(report_csv, file.path(out, "P041.lst.csv"), row.names = FALSE, na = "")
# also write dataset version with cat/subcat for traceability
write.csv(final_all %>% select(cat, subcat, text, col1_disp, col2_disp, col3_disp, col4_disp, trt0, trt54, trt81, trt99),
          file.path(out, "P041_dataset.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Text rendering - mimic PROC REPORT
# ------------------------------------------------------------------
title1 <- "Demography"
footnote1 <- "NOTE: Numbers in parentheses are percentages"

# Build fixed-width text
# define widths: text 30 left, col1..col4 12 left
col_width_text <- 30
col_width_grp <- 12

header_line1 <- sprintf(paste0("%-", col_width_text, "s%-", col_width_grp, "s%-", col_width_grp, "s%-", col_width_grp, "s%-", col_width_grp, "s"),
                        "", "Group 1", "Group 2", "Group 3", "Total")
sep_line <- paste(rep("-", col_width_text + 4*col_width_grp), collapse = "")

txt_lines <- c(
  title1,
  "",
  header_line1,
  sep_line
)

# iterate by cat to add break after cat/skip (blank line)
cats <- sort(unique(final_all$cat))
for (c in cats) {
  sub_df <- final_all %>% filter(cat == c) %>% arrange(subcat)
  for (i in seq_len(nrow(sub_df))) {
    row <- sub_df[i, ]
    # text column: keep as is (includes leading spaces for detail rows)
    txt_col <- row$text
    # ensure txt_col is not NA
    if (is.na(txt_col)) txt_col <- ""
    line <- sprintf(paste0("%-", col_width_text, "s%-", col_width_grp, "s%-", col_width_grp, "s%-", col_width_grp, "s%-", col_width_grp, "s"),
                    txt_col,
                    ifelse(is.na(row$col1_disp) || row$col1_disp=="", "", row$col1_disp),
                    ifelse(is.na(row$col2_disp) || row$col2_disp=="", "", row$col2_disp),
                    ifelse(is.na(row$col3_disp) || row$col3_disp=="", "", row$col3_disp),
                    ifelse(is.na(row$col4_disp) || row$col4_disp=="", "", row$col4_disp))
    # trim trailing spaces for cleaner file but keep structure
    txt_lines <- c(txt_lines, rtrim <- sub("\\s+$", "", line))
  }
  # break after cat/skip -> blank line
  txt_lines <- c(txt_lines, "")
}
txt_lines <- c(txt_lines, footnote1, "")

# Write text files
writeLines(txt_lines, file.path(out, "P041.txt"))
writeLines(txt_lines, file.path(out, "P041.lst.txt"))
writeLines(txt_lines, file.path(out, "P041.lst"))

cat("Outputs written to", out, "\n")
print(report_csv)
