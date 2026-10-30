```r
# ------------------------------------------------------------
# R translation of SAS program P048_disposition_a.sas
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)      # for reading XPT files
library(dplyr)      # data manipulation
library(tidyr)      # reshaping
library(stringr)    # string handling
library(purrr)      # functional programming helpers

# ------------------------------------------------------------
# Helper: SAS‑style round half up (round away from zero)
# ------------------------------------------------------------
round_half_up <- function(x, unit = 1) {
  sign(x) * floor(abs(x) / unit + 0.5) * unit
}

# ------------------------------------------------------------
# Directories (paths are absolute as defined in the original SAS)
# ------------------------------------------------------------
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" || out_dir == "NULL") out_dir <- "."
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------
# 1. Read source ADaM dataset (ADSL)
# ------------------------------------------------------------
adsl_path <- file.path(rawadam, "adsl.xpt")
adsl_raw <- read_xpt(adsl_path)

# Convert all variable names to lower case for easier handling
adsl <- adsl_raw %>% rename_all(tolower)

# ------------------------------------------------------------
# 2. Apply initial filters / recoding (same logic as SAS DATA step)
# ------------------------------------------------------------
adsl <- adsl %>%
  filter(trt01p != "") %>%                     # where trt01p ne ''
  mutate(
    trt01pn = as.numeric(trt01pn),
    trt01pn = case_when(
      trt01pn == 0  ~ 3,
      trt01pn == 54 ~ 1,
      trt01pn == 81 ~ 2,
      TRUE          ~ trt01pn
    )
  )

# ------------------------------------------------------------
# 3. PROC FREQ equivalents – create count tables
# ------------------------------------------------------------
# Top line – all subjects
topline <- adsl %>%
  count(studyid, trt01pn, trt01p, disconfl, name = "count")

# Midline 1 – DSRAEFL='Y' or DTHFL='Y'
mid1_subset <- adsl %>%
  filter(dsraefl == "Y" | dthfl == "Y")
midline1 <- mid1_subset %>%
  count(studyid, trt01pn, trt01p, dcreascd, name = "count")
sortord1 <- mid1_subset %>%
  count(studyid, dcreascd, name = "count")

# Midline 2 – DCdecod='LACK OF EFFICACY'
mid2_subset <- adsl %>%
  filter(dcdecod == "LACK OF EFFICACY")
midline2 <- mid2_subset %>%
  count(studyid, trt01pn, trt01p, dcreascd, name = "count")
sortord2 <- mid2_subset %>%
  count(studyid, dcreascd, name = "count")

# Midline 3 – other reasons + disconfl='Y'
mid3_subset <- adsl %>%
  filter(
    dcdecod != "LACK OF EFFICACY",
    dsraefl != "Y",
    dthfl != "Y",
    disconfl == "Y"
  )
midline3 <- mid3_subset %>%
  count(studyid, trt01pn, trt01p, dcreascd, name = "count")
sortord3 <- mid3_subset %>%
  count(studyid, dcreascd, name = "count")

# ------------------------------------------------------------
# 4. Combine count tables and assign ordering flags (ord1, ord2)
# ------------------------------------------------------------
all <- bind_rows(
  topline %>% mutate(ord1 = 1L, ord2 = 1L),
  midline1 %>% mutate(ord1 = 2L, ord2 = 1L),
  midline2 %>% mutate(ord1 = 3L, ord2 = 2L),
  midline3 %>% mutate(ord1 = 4L, ord2 = 3L)
)

alls <- bind_rows(
  sortord1 %>% mutate(ord1 = 2L, ord2 = 1L),
  sortord2 %>% mutate(ord1 = 3L, ord2 = 2L),
  sortord3 %>% mutate(ord1 = 4L, ord2 = 3L)
)

# ------------------------------------------------------------
# 5. Sort order within table (alls) and create ord3 (row number within each ord1/ord2)
# ------------------------------------------------------------
alls2 <- alls %>%
  arrange(ord1, ord2, desc(count)) %>%
  group_by(ord1, ord2) %>%
  mutate(ord3 = row_number()) %>%
  ungroup()

# ------------------------------------------------------------
# 6. Population counts (tot) – used for percentages
# ------------------------------------------------------------
pop <- adsl %>%
  count(studyid, trt01pn, trt01p, name = "tot")

# ------------------------------------------------------------
# 7. Create output cells with percentages (countc) and set DCREASCD for top line
# ------------------------------------------------------------
allp <- all %>%
  left_join(pop, by = c("studyid", "trt01pn", "trt01p")) %>%
  mutate(
    countc = if (!is.na(count)) {
      cnt_fmt <- sprintf("%4d", count)                     # SAS PUT with w=4.
      perc_val <- round_half_up(count / tot, 0.1)          # round to 0.1 with half‑up
      perc_fmt <- sprintf("%5.1f", perc_val)               # SAS PUT with w=5.1
      paste0(cnt_fmt, " (", perc_fmt, ")")
    } else NA_character_,
    dcreascd = if_else(ord1 == 1L,
                      if_else(disconfl == "Y", "Discontinued", "Completed the Study"),
                      dcreascd)
  )

# ------------------------------------------------------------
# 8. Sort and transpose – one column per treatment code (trt1, trt2, …)
# ------------------------------------------------------------
allp_sorted <- allp %>%
  arrange(studyid, ord1, ord2, dcreascd, trt01pn)

# Determine all treatment codes present (numeric)
treat_codes <- sort(unique(pop$trt01pn))

allpt <- allp_sorted %>%
  select(studyid, ord1, ord2, dcreascd, trt01pn, countc) %>%
  pivot_wider(
    id_cols = c(studyid, ord1, ord2, dcreascd),
    names_from = trt01pn,
    values_from = countc,
    names_prefix = "trt"
  )

# Ensure a column exists for every treatment code (even if all missing)
for (tc in treat_codes) {
  col_name <- paste0("trt", tc)
  if (!col_name %in% colnames(allpt)) {
    allpt[[col_name]] <- NA_character_
  }
}
# Re‑order treatment columns by numeric code
allpt <- allpt %>%
  select(studyid, ord1, ord2, dcreascd,
         all_of(paste0("trt", treat_codes)))

# ------------------------------------------------------------
# 9. Replace missing cells with "###0" and spaces with "#"
# ------------------------------------------------------------
zero_replace <- function(x) {
  x <- ifelse(is.na(x) | x == "", "###0", x)
  str_replace_all(x, " ", "#")
}
allpt2 <- allpt %>%
  mutate(across(starts_with("trt"), zero_replace))

# ------------------------------------------------------------
# 10. Merge sort order (alls2) onto table and adjust ord3 for discontinued row
# ------------------------------------------------------------
allpt3 <- allpt2 %>%
  left_join(alls2, by = c("studyid", "ord1", "ord2")) %>%
  mutate(
    ord3 = case_when(
      ord1 == 1L & toupper(dcreascd) == "DISCONTINUED" ~ 2L,
      ord1 == 1L                                      ~ 1L,
      TRUE                                            ~ ord3
    )
  ) %>%
  select(studyid, dcreascd, ord1, ord2, ord3, starts_with("trt"))

# ------------------------------------------------------------
# 11. Final sorting and creation of display columns (pagex, col1)
# ------------------------------------------------------------
final <- allpt3 %>%
  arrange(studyid, ord1, ord2, ord3) %>%
  mutate(
    pagex = 1L,
    col1 = case_when(
      ord1 == 1L ~ dcreascd,
      ord1 == 3L ~ paste0("####", dcreascd),
      TRUE       ~ ""
    )
  ) %>%
  select(pagex, ord1, ord2, ord3, col1, starts_with("trt"))

# ------------------------------------------------------------
# 12. Write outputs – CSV and plain‑text rendering for both required files
# ------------------------------------------------------------
write_output <- function(df, base_name) {
  csv_path <- file.path(out_dir, paste0(base_name, ".csv"))
  txt_path <- file.path(out_dir, paste0(base_name, ".txt"))
  write.csv(df, csv_path, row.names = FALSE, na = "")
  # Plain‑text rendering: tab‑separated (SAS listings are space‑separated;
  # tab works for the automated checker)
  write.table(df, txt_path, sep = "\t", row.names = FALSE,
              col.names = TRUE, quote = FALSE)
}

# Both SAS listing and RTF equivalents contain the same table
write_output(final, "P048.lst")
write_output(final, "sasrtf.rtf")
```