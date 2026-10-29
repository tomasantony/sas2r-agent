# P041 t-demog.sas -> R translation
# Demography table (Age + Gender by treatment)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

# SAS round half away from zero (for completeness; counts are integers)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put_3 <- function(x) {
  # SAS PUT(num,3.) : width 3 right aligned; missing -> blanks
  ifelse(is.na(x), "   ", sprintf("%3.0f", round_half_up(x, 0)))
}

mcsl_path <- "/bench/data/raw/phuse-mcsl/mcsl.csv"
cat("Reading:", mcsl_path, "\n")

df0 <- utils::read.csv(mcsl_path, header = TRUE, stringsAsFactors = FALSE,
  na.strings = c(""), check.names = FALSE, strip.white = FALSE,
  blank.lines.skip = TRUE)

expected <- c("studyid","usubjid","trt01a","trt01an","age","ageu","race","racen",
  "sex","saffl","efffl","bmibl","heightbl","weightbl","dcdecod","dcreascd")

lnames <- tolower(trimws(names(df0)))
if (all(expected %in% lnames)) {
  # reorder to expected order
  idx <- match(expected, lnames)
  df0 <- df0[, idx, drop = FALSE]
  names(df0) <- expected
} else {
  # fallback: positional assignment (SAS INPUT order, FIRSTOBS=2 skips header)
  # df0 already read with header; if header mismatch, re-read without header?
  # Assume first 16 columns correspond to expected order
  if (ncol(df0) >= 16) {
    df0 <- df0[, 1:16, drop = FALSE]
    names(df0) <- expected
  } else {
    stop("Unexpected mcsl.csv structure")
  }
}

# Coerce types + LENGTH truncation (SAS LENGTH)
mcsl <- df0 %>%
  mutate(
    studyid = substr(as.character(studyid), 1, 20),
    usubjid = substr(as.character(usubjid), 1, 20),
    trt01a  = substr(as.character(trt01a), 1, 20),
    trt01an = suppressWarnings(as.numeric(trt01an)),
    age     = suppressWarnings(as.numeric(age)),
    ageu    = substr(as.character(ifelse(is.na(ageu), "", ageu)), 1, 10),
    race    = substr(as.character(ifelse(is.na(race), "", race)), 1, 25),
    racen   = suppressWarnings(as.numeric(racen)),
    sex     = substr(as.character(ifelse(is.na(sex), "", sex)), 1, 1),
    saffl   = substr(as.character(ifelse(is.na(saffl), "", saffl)), 1, 1),
    efffl   = substr(as.character(ifelse(is.na(efffl), "", efffl)), 1, 1),
    bmibl   = suppressWarnings(as.numeric(bmibl)),
    heightbl= suppressWarnings(as.numeric(heightbl)),
    weightbl= suppressWarnings(as.numeric(weightbl)),
    dcdecod = substr(as.character(ifelse(is.na(dcdecod), "", dcdecod)), 1, 50),
    dcreascd= substr(as.character(ifelse(is.na(dcreascd), "", dcreascd)), 1, 50)
  )
# SAS missing character is "" (blank); convert "" to NA where needed for logic
# sexn
mcsl$sexn <- ifelse(!is.na(mcsl$sex) & mcsl$sex == "M", 1,
             ifelse(!is.na(mcsl$sex) & mcsl$sex == "F", 2, NA_real_))
# SAS: if sex=' ' missing -> sexn missing; our "" already gives NA
mcsl$sex[mcsl$sex == ""] <- NA_character_

cat("N mcsl:", nrow(mcsl), "\n")

# ---- Categorize age ----
age1 <- mcsl %>%
  mutate(agecat = case_when(
    !is.na(age) & age >= 18 & age <= 19 ~ 1,
    !is.na(age) & age >= 20 & age <= 29 ~ 2,
    !is.na(age) & age >= 30 & age <= 39 ~ 3,
    !is.na(age) & age >= 40 & age <= 49 ~ 4,
    !is.na(age) & age >= 50 & age <= 59 ~ 5,
    !is.na(age) & age >= 60             ~ 6,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(agecat)) %>%
  select(trt01an, usubjid, age, agecat)

# ---- Age counts ----
agenums <- age1 %>%
  group_by(trt01an, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 1L, subcat = agecat)

agetots <- age1 %>%
  group_by(agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 1L)

age_fmt <- c("1" = "  18-19", "2" = "  20-29", "3" = "  30-39",
             "4" = "  40-49", "5" = "  50-59", "6" = "  >=60 ")

agecat <- bind_rows(
  agenums %>% select(cat, subcat, trt01an, agecat, num),
  agetots %>% mutate(trt01an = 99, subcat = agecat) %>% select(cat, subcat, trt01an, agecat, num)
) %>%
  mutate(text = paste0("  ", age_fmt[as.character(agecat)])) %>%
  arrange(cat, subcat, desc(agecat))

# transpose prefix=trt out=age
age_wide <- agecat %>%
  select(cat, subcat, agecat, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt")

# ---- Gender ----
sexg <- mcsl %>%
  filter(!is.na(sex) & sex != "" & sex != " ") %>%
  group_by(trt01an, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 2L, subcat = sexn)

sext <- mcsl %>%
  filter(!is.na(sexn)) %>%
  group_by(sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 2L)

sex_fmt <- c("1" = "Male", "2" = "Female")

sexcat <- bind_rows(
  sexg %>% select(cat, subcat, trt01an, sexn, num),
  sext %>% mutate(trt01an = 99, subcat = sexn) %>% select(cat, subcat, trt01an, sexn, num)
) %>%
  mutate(text = paste0("  ", sex_fmt[as.character(sexn)])) %>%
  arrange(cat, subcat, desc(sexn))

sex_wide <- sexcat %>%
  select(cat, subcat, sexn, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt")

# Ensure all trt columns exist
for (cn in c("trt0","trt54","trt81","trt99")) {
  if (!cn %in% names(age_wide)) age_wide[[cn]] <- NA_real_
  if (!cn %in% names(sex_wide)) sex_wide[[cn]] <- NA_real_
}

age_part <- age_wide %>% select(cat, subcat, text, trt0, trt54, trt81, trt99)
sex_part <- sex_wide %>% select(cat, subcat, text, trt0, trt54, trt81, trt99)

final_detail <- bind_rows(age_part, sex_part) %>%
  mutate(
    col1 = sas_put_3(trt0),
    col2 = sas_put_3(trt54),
    col3 = sas_put_3(trt81),
    col4 = sas_put_3(trt99)
  )

# Header rows (FIRST.cat)
headers <- data.frame(
  cat = c(1L, 2L), subcat = c(0L, 0L),
  text = c("Age", "Gender"),
  trt0 = NA_real_, trt54 = NA_real_, trt81 = NA_real_, trt99 = NA_real_,
  col1 = "", col2 = "", col3 = "", col4 = "",
  stringsAsFactors = FALSE
)

final <- bind_rows(final_detail, headers) %>%
  arrange(cat, subcat)

cat("Final rows:", nrow(final), "\n")

# ---- Generate report text (PROC REPORT mimic) ----
# Column headers: text=" ", col1="Group 1", col2="Group 2", col3="Group 3", col4="Total"
hdr_text <- sprintf("%-30s %-12s %-12s %-12s %-12s", "", "Group 1", "Group 2", "Group 3", "Total")
lines <- c("Demography", "", hdr_text)
for (cc in sort(unique(final$cat))) {
  sub <- final %>% filter(cat == cc) %>% arrange(subcat)
  for (i in seq_len(nrow(sub))) {
    # text width 30 left, cols width 12 left (trim leading blanks for display but keep numbers)
    # Use col strings trimmed then left-aligned to mimic REPORT LEFT
    t <- sub$text[i]
    c1 <- trimws(sub$col1[i], which = "both")
    c2 <- trimws(sub$col2[i], which = "both")
    c3 <- trimws(sub$col3[i], which = "both")
    c4 <- trimws(sub$col4[i], which = "both")
    lines <- c(lines, sprintf("%-30s %-12s %-12s %-12s %-12s", t, c1, c2, c3, c4))
  }
  lines <- c(lines, "")  # break after cat / skip
}
lines <- c(lines, "NOTE: Numbers in parentheses are percentages")

txt_path <- file.path(out, "P041.txt")
writeLines(lines, txt_path)
cat("Wrote", txt_path, "\n")

# ---- CSV of table body ----
csv_df <- final %>%
  transmute(
    cat = cat,
    subcat = subcat,
    text = text,
    Group_1 = trt0,
    Group_2 = trt54,
    Group_3 = trt81,
    Total = trt99
  ) %>%
  arrange(cat, subcat)

csv_path <- file.path(out, "P041.csv")
utils::write.csv(csv_df, csv_path, row.names = FALSE, na = "")
cat("Wrote", csv_path, "\n")
