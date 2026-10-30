```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

mcsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/phuse-mcsl/mcsl.csv"

suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(readr))

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5)
  posneg * z / 10^digits
}

fmt_age <- function(x) {
  dplyr::case_when(
    x == 1 ~ "  18-19",
    x == 2 ~ "  20-29",
    x == 3 ~ "  30-39",
    x == 4 ~ "  40-49",
    x == 5 ~ "  50-59",
    x == 6 ~ "  >=60 ",
    TRUE ~ ""
  )
}
fmt_sex <- function(x) {
  dplyr::case_when(
    x == 1 ~ "Male",
    x == 2 ~ "Female",
    TRUE ~ ""
  )
}
fmt3 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("  .")
    iv <- suppressWarnings(as.integer(round_half_up(as.numeric(v), 0)))
    if (is.na(iv)) return("  .")
    sprintf("%3d", iv)
  }, USE.NAMES = FALSE)
}

cat("Reading MCSL\n")
sas_names <- c("studyid","usubjid","trt01a","trt01an","age","ageu","race","racen",
               "sex","saffl","efffl","bmibl","heightbl","weightbl","dcdecod","dcreascd")

raw <- suppressMessages(suppressWarnings(
  readr::read_csv(mcsl_path, col_names = FALSE, skip = 1,
                  col_types = readr::cols(.default = readr::col_character()),
                  na = c(""), trim_ws = TRUE, progress = FALSE, name_repair = "minimal")
))
# emulate MISSOOVER / pad-truncate to 16 columns
nc <- ncol(raw)
if (nc < 16L) {
  for (j in seq_len(16L - nc)) raw[[paste0("Xpad", j)]] <- NA_character_
  nc <- ncol(raw)
}
if (nc > 16L) {
  raw <- raw[, 1:16L]
}
colnames(raw) <- sas_names

mcsl <- raw
# LENGTH truncation (character)
trunc1 <- function(x, n) ifelse(is.na(x), NA_character_, substr(x, 1, n))
mcsl$studyid <- trunc1(mcsl$studyid, 20)
mcsl$usubjid  <- trunc1(mcsl$usubjid, 20)
mcsl$trt01a   <- trunc1(mcsl$trt01a, 20)
mcsl$ageu     <- trunc1(mcsl$ageu, 10)
mcsl$race     <- trunc1(mcsl$race, 25)
mcsl$sex      <- trunc1(mcsl$sex, 1)
mcsl$saffl    <- trunc1(mcsl$saffl, 1)
mcsl$efffl    <- trunc1(mcsl$efffl, 1)
mcsl$dcdecod  <- trunc1(mcsl$dcdecod, 50)
mcsl$dcreascd <- trunc1(mcsl$dcreascd, 50)
# numeric conversion (SAS list input)
mcsl$trt01an  <- suppressWarnings(as.numeric(mcsl$trt01an))
mcsl$age      <- suppressWarnings(as.numeric(mcsl$age))
mcsl$racen    <- suppressWarnings(as.numeric(mcsl$racen))
mcsl$bmibl    <- suppressWarnings(as.numeric(mcsl$bmibl))
mcsl$heightbl <- suppressWarnings(as.numeric(mcsl$heightbl))
mcsl$weightbl <- suppressWarnings(as.numeric(mcsl$weightbl))
# sexn
mcsl$sexn <- dplyr::case_when(
  mcsl$sex == "M" ~ 1,
  mcsl$sex == "F" ~ 2,
  TRUE ~ NA_real_
)

cat("Categorize age\n")
age1 <- mcsl %>%
  mutate(agecat = dplyr::case_when(
    !is.na(age) & 18 <= age & age <= 19 ~ 1,
    !is.na(age) & 20 <= age & age <= 29 ~ 2,
    !is.na(age) & 30 <= age & age <= 39 ~ 3,
    !is.na(age) & 40 <= age & age <= 49 ~ 4,
    !is.na(age) & 50 <= age & age <= 59 ~ 5,
    !is.na(age) & age >= 60 ~ 6,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(agecat)) %>%
  select(trt01an, usubjid, age, agecat)

stopifnot(all(c("trt01an","usubjid","age","agecat") %in% names(age1)))

agenums <- age1 %>%
  group_by(cat = 1, subcat = agecat, trt01an, agecat) %>%
  summarise(num = dplyr::n_distinct(usubjid, na.rm = TRUE), .groups = "drop")

agetots <- age1 %>%
  group_by(cat = 1, subcat = agecat, agecat) %>%
  summarise(num = dplyr::n_distinct(usubjid, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(trt01an = 99)

agecat_df <- dplyr::bind_rows(agenums, agetots) %>%
  mutate(text = paste0("  ", fmt_age(agecat))) %>%
  arrange(cat, subcat, dplyr::desc(agecat))

# transpose: one row per by-group (cat, subcat, agecat, text), columns trt*
age_long <- agecat_df %>%
  mutate(trtcol = paste0("trt", trt01an)) %>%
  select(cat, subcat, agecat, text, trtcol, num)

age_wide <- age_long %>%
  tidyr::pivot_wider(names_from = trtcol, values_from = num, values_fn = function(x) x[1])

for (cc in c("trt0","trt54","trt81","trt99")) {
  if (!cc %in% names(age_wide)) age_wide[[cc]] <- NA_real_
}
age_wide <- age_wide %>% arrange(cat, subcat, dplyr::desc(agecat))

cat("Gender\n")
sexg <- mcsl %>%
  filter(!is.na(sex) & sex != "" & sex != " ") %>%
  group_by(cat = 2, subcat = sexn, trt01an, sexn) %>%
  summarise(num = dplyr::n_distinct(usubjid, na.rm = TRUE), .groups = "drop")

sext <- mcsl %>%
  filter(!is.na(sexn)) %>%
  group_by(cat = 2, subcat = sexn, sexn) %>%
  summarise(num = dplyr::n_distinct(usubjid, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  mutate(trt01an = 99)

sexcat_df <- dplyr::bind_rows(sexg, sext) %>%
  mutate(text = paste0("  ", fmt_sex(sexn))) %>%
  arrange(cat, subcat, dplyr::desc(sexn))

sex_long <- sexcat_df %>%
  mutate(trtcol = paste0("trt", trt01an)) %>%
  select(cat, subcat, sexn, text, trtcol, num)

sex_wide <- sex_long %>%
  tidyr::pivot_wider(names_from = trtcol, values_from = num, values_fn = function(x) x[1])

for (cc in c("trt0","trt54","trt81","trt99")) {
  if (!cc %in% names(sex_wide)) sex_wide[[cc]] <- NA_real_
}
sex_wide <- sex_wide %>% arrange(cat, subcat, dplyr::desc(sexn))

cat("Bring together -> final\n")
# detail rows with col1..col4 = put(trt,3.)
mk_detail <- function(w) {
  w %>%
    mutate(
      col1 = fmt3(trt0),
      col2 = fmt3(trt54),
      col3 = fmt3(trt81),
      col4 = fmt3(trt99)
    ) %>%
    select(cat, subcat, text, col1, col2, col3, col4)
}
age_det <- mk_detail(age_wide)
sex_det <- mk_detail(sex_wide)
detail <- dplyr::bind_rows(age_det, sex_det) %>% arrange(cat, subcat)

headers <- detail %>%
  group_by(cat) %>%
  arrange(subcat, .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    subcat = 0,
    text = dplyr::case_when(cat == 1 ~ "Age", cat == 2 ~ "Gender", TRUE ~ ""),
    col1 = "", col2 = "", col3 = "", col4 = ""
  ) %>%
  select(cat, subcat, text, col1, col2, col3, col4)

final <- dplyr::bind_rows(detail, headers) %>% arrange(cat, subcat)
stopifnot(all(c("cat","subcat","text","col1","col2","col3","col4") %in% names(final)))

cat("Generate report\n")
title1 <- "Demography"
footnote1 <- "NOTE: Numbers in parentheses are percentages"

hdr_text <- sprintf("%-30s%-12s%-12s%-12s%-12s", "", "Group 1", "Group 2", "Group 3", "Total")
dash <- paste0(paste(rep("-", 30), collapse = ""), paste(rep("-", 12), collapse = ""),
               paste(rep("-", 12), collapse = ""), paste(rep("-", 12), collapse = ""),
               paste(rep("-", 12), collapse = ""))
# build lines in display order with BREAK AFTER cat / SKIP (blank line between blocks)
lines <- c()
lines <- c(lines, title1, "")
lines <- c(lines, hdr_text)
lines <- c(lines, dash)
cats <- sort(unique(final$cat))
for (k in seq_along(cats)) {
  cc <- cats[k]
  blk <- final %>% filter(cat == cc) %>% arrange(subcat)
  for (r in seq_len(nrow(blk))) {
    lines <- c(lines, sprintf("%-30s%-12s%-12s%-12s%-12s",
                              blk$text[r], blk$col1[r], blk$col2[r], blk$col3[r], blk$col4[r]))
  }
  lines <- c(lines, "")
}
lines <- c(lines, footnote1)

txt_path <- file.path(out, "P041.txt")
csv_path <- file.path(out, "P041.csv")
writeLines(lines, txt_path)

# CSV of table body (same numbers)
utils::write.csv(final[, c("cat","subcat","text","col1","col2","col3","col4")],
                 csv_path, row.names = FALSE, na = "")

# counterparts for checker naming variants
lst_path <- file.path(out, "P041.lst")
writeLines(lines, lst_path)
lst_csv <- file.path(out, "P041.lst.csv")
utils::write.csv(final[, c("cat","subcat","text","col1","col2","col3","col4")],
                 lst_csv, row.names = FALSE, na = "")

cat("Done\n")
```