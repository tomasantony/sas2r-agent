out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
})

# SAS round half away from zero helper (not needed for counts but kept)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/raw/phuse-mcsl/mcsl.csv"

# ---- Bring in data (SAS: infile delimiter="," firstobs=2 missover) ----
# SAS INPUT order:
cnames <- c("studyid","usubjid","trt01a","trt01an","age","ageu","race","racen",
            "sex","saffl","efffl","bmibl","heightbl","weightbl","dcdecod","dcreascd")
# read skipping header line (firstobs=2), by position
mcsl_raw <- read.csv(raw_path, header = FALSE, skip = 1, stringsAsFactors = FALSE,
                     fill = TRUE, na.strings = c(), colClasses = "character")
# keep first 16 columns, pad if short
if (ncol(mcsl_raw) < 16) {
  for (j in (ncol(mcsl_raw)+1):16) mcsl_raw[[j]] <- NA_character_
}
mcsl_raw <- mcsl_raw[, 1:16]
names(mcsl_raw) <- cnames

mcsl <- mcsl_raw
# LENGTH truncations
mcsl$studyid <- substr(mcsl$studyid, 1, 20)
mcsl$usubjid <- substr(mcsl$usubjid, 1, 20)
mcsl$trt01a  <- substr(mcsl$trt01a, 1, 20)
mcsl$ageu    <- substr(mcsl$ageu, 1, 10)
mcsl$race    <- substr(mcsl$race, 1, 25)
mcsl$sex     <- substr(mcsl$sex, 1, 1)
mcsl$saffl   <- substr(mcsl$saffl, 1, 1)
mcsl$efffl   <- substr(mcsl$efffl, 1, 1)
mcsl$dcdecod <- substr(mcsl$dcdecod, 1, 50)
mcsl$dcreascd<- substr(mcsl$dcreascd, 1, 50)
# numeric conversions like SAS INPUT
mcsl$trt01an <- suppressWarnings(as.numeric(mcsl$trt01an))
mcsl$age     <- suppressWarnings(as.numeric(mcsl$age))
mcsl$racen   <- suppressWarnings(as.numeric(mcsl$racen))
mcsl$bmibl   <- suppressWarnings(as.numeric(mcsl$bmibl))
mcsl$heightbl<- suppressWarnings(as.numeric(mcsl$heightbl))
mcsl$weightbl<- suppressWarnings(as.numeric(mcsl$weightbl))
# trim spaces for character comparisons (SAS would keep blanks; strip for safety on sex)
# sex comparison is case-sensitive; keep as-is but treat "" as blank
mcsl$sex[is.na(mcsl$sex)] <- ""
# sexn
mcsl$sexn <- NA_real_
mcsl$sexn[mcsl$sex == "M"] <- 1
mcsl$sexn[mcsl$sex == "F"] <- 2

# ---- Categorize age ----
age1 <- mcsl
age1$agecat <- NA_real_
age1$agecat[age1$age >= 18 & age1$age <= 19] <- 1
age1$agecat[age1$age >= 20 & age1$age <= 29] <- 2
age1$agecat[age1$age >= 30 & age1$age <= 39] <- 3
age1$agecat[age1$age >= 40 & age1$age <= 49] <- 4
age1$agecat[age1$age >= 50 & age1$age <= 59] <- 5
age1$agecat[age1$age >= 60 & !is.na(age1$age)] <- 6
age1 <- age1[!is.na(age1$agecat), c("trt01an","usubjid","age","agecat")]

# formats
age_fmt <- c("1" = "  18-19", "2" = "  20-29", "3" = "  30-39",
             "4" = "  40-49", "5" = "  50-59", "6" = "  >=60 ")
sex_fmt <- c("1" = "Male", "2" = "Female")

# ---- agenums / agetots (PROC SQL count distinct usubjid) ----
agenums <- age1 %>%
  group_by(trt01an, agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 1, subcat = agecat) %>%
  select(cat, subcat, trt01an, agecat, num)

agetots <- age1 %>%
  group_by(agecat) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 1, subcat = agecat) %>%
  select(cat, subcat, agecat, num)

agecat <- bind_rows(
  agenums,
  agetots %>% mutate(trt01an = 99)
)
agecat$text <- paste0("  ", age_fmt[as.character(agecat$agecat)])

# sort by cat subcat descending agecat
agecat <- agecat %>% arrange(cat, subcat, desc(agecat))

# transpose prefix=trt out=age(drop=agecat); var num; by cat subcat descending agecat text; id trt01an
age_wide <- agecat %>%
  select(cat, subcat, agecat, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt")
# ensure expected columns exist (SAS references trt0 trt54 trt81 trt99)
for (cc in c("trt0","trt54","trt81","trt99")) {
  if (!cc %in% names(age_wide)) age_wide[[cc]] <- NA_real_
}

# ---- gender ----
# sexg where sex^=' ' ; sext where sexn^=.
sexg_base <- mcsl[mcsl$sex != "" & !is.na(mcsl$sex), ]
sexg <- sexg_base %>%
  group_by(trt01an, sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 2, subcat = sexn) %>%
  select(cat, subcat, trt01an, sexn, num)

sext_base <- mcsl[!is.na(mcsl$sexn), ]
sext <- sext_base %>%
  group_by(sexn) %>%
  summarise(num = n_distinct(usubjid), .groups = "drop") %>%
  mutate(cat = 2, subcat = sexn) %>%
  select(cat, subcat, sexn, num)

sexcat <- bind_rows(
  sexg,
  sext %>% mutate(trt01an = 99)
)
sexcat$text <- paste0("  ", ifelse(is.na(sexcat$sexn), "",
                    ifelse(as.character(sexcat$sexn) %in% names(sex_fmt),
                           sex_fmt[as.character(sexcat$sexn)], "")))
sexcat <- sexcat %>% arrange(cat, subcat, desc(sexn))

sex_wide <- sexcat %>%
  select(cat, subcat, sexn, text, trt01an, num) %>%
  pivot_wider(names_from = trt01an, values_from = num, names_prefix = "trt")
for (cc in c("trt0","trt54","trt81","trt99")) {
  if (!cc %in% names(sex_wide)) sex_wide[[cc]] <- NA_real_
}

# ---- Bring the data together ----
fmt3 <- function(x) {
  # SAS PUT(x,3.) : width 3 right-aligned; missing -> "  ."
  ifelse(is.na(x), "  .", sprintf("%3d", as.integer(x)))
}

age_part <- age_wide %>% select(cat, subcat, text, trt0, trt54, trt81, trt99)
sex_part <- sex_wide %>% select(cat, subcat, text, trt0, trt54, trt81, trt99)
combined <- bind_rows(age_part, sex_part)
combined <- combined %>% arrange(cat, subcat)

combined$col1 <- fmt3(combined$trt0)
combined$col2 <- fmt3(combined$trt54)
combined$col3 <- fmt3(combined$trt81)
combined$col4 <- fmt3(combined$trt99)

final_detail <- combined %>% select(cat, subcat, text, col1, col2, col3, col4)

# header rows: if first.cat then output with subcat=0, text Age/Gender, cols blank
cats <- sort(unique(final_detail$cat))
headers <- data.frame(cat = cats, subcat = 0,
  text = ifelse(cats == 1, "Age", ifelse(cats == 2, "Gender", "")),
  col1 = "", col2 = "", col3 = "", col4 = "", stringsAsFactors = FALSE)

final <- bind_rows(final_detail, headers)
final <- final %>% arrange(cat, subcat)
stopifnot(all(c("cat","subcat","text","col1","col2","col3","col4") %in% names(final)))

# ---- Generate report (PROC REPORT) ----
title1 <- "Demography"
footnote1 <- "NOTE: Numbers in parentheses are percentages"

# Build text rendering similar to SAS REPORT with headline/headskip, break after cat/skip
# Column headers: " " for text, Group 1/2/3, Total
hdr_text <- sprintf("%-30s %-12s %-12s %-12s %-12s", "", "Group 1", "Group 2", "Group 3", "Total")
sep_line <- paste(rep("-", nchar(hdr_text)), collapse = "")

lines <- c(title1, "", hdr_text, sep_line)
for (i in seq_len(nrow(final))) {
  r <- final[i, ]
  # text width 30 left, cols width 12 left
  line <- sprintf("%-30s %-12s %-12s %-12s %-12s",
                  r$text, trimws(r$col1), trimws(r$col2), trimws(r$col3), trimws(r$col4))
  # keep trailing spaces trimmed for display but preserve leading spaces in text
  lines <- c(lines, line)
  # break after cat / skip : blank line after last row of each cat
  if (i < nrow(final)) {
    if (final$cat[i] != final$cat[i+1]) lines <- c(lines, "")
  }
}
lines <- c(lines, "", footnote1)

txt_path <- file.path(out, "P041.txt")
writeLines(lines, txt_path)
# also write .lst copy for benchmark expecting P041.lst
lst_path <- file.path(out, "P041.lst")
writeLines(lines, lst_path)

# CSV of table body (same numbers)
csv_body <- final
# keep display order
names(csv_body) <- c("cat","subcat","text","col1","col2","col3","col4")
write.csv(csv_body, file.path(out, "P041.csv"), row.names = FALSE, na = "")
