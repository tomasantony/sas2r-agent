# ------------------------------------------------------------
# R translation of SAS program P048_disposition_a.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# Helper: round half up (SAS style)
round_half_up <- function(x, unit = 1) {
  sign(x) * floor(abs(x) / unit + 0.5) * unit
}

# ------------------------------------------------------------------
# Set directories (paths are absolute as defined in the SAS macro vars)
# ------------------------------------------------------------------
rawadam <- "/bench/data/raw/cdisc-pilot/adam"
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."

# ------------------------------------------------------------------
# 1. Read source ADaM dataset
# ------------------------------------------------------------------
adsl_path <- file.path(rawadam, "adsl.xpt")
adsl <- read_xpt(adsl_path)

# Ensure character variables are proper character type
adsl <- adsl %>%
  mutate(
    trt01p = as.character(trt01p),
    disconfl = as.character(disconfl),
    DCREASCD = as.character(DCREASCD),
    DSRAEFL = as.character(DSRAEFL),
    DTHFL   = as.character(DTHFL),
    DCdecod = as.character(DCdecod),
    studyid = as.character(studyid)
  )

# ------------------------------------------------------------------
# 2. Apply initial filters / recoding
# ------------------------------------------------------------------
adsl <- adsl %>%
  filter(trt01p != "") %>%
  mutate(
    trt01pn = as.numeric(trt01pn),
    trt01pn = case_when(
      trt01pn == 0  ~ 3,
      trt01pn == 54 ~ 1,
      trt01pn == 81 ~ 2,
      TRUE          ~ trt01pn
    )
  )

# ------------------------------------------------------------------
# 3. PROC FREQ equivalents – create count tables
# ------------------------------------------------------------------
# Top line (all subjects)
topline <- adsl %>%
  count(studyid, trt01pn, trt01p, disconfl, name = "count")

# Midline 1 – DSRAEFL='Y' or DTHFL='Y'
mid1_subset <- adsl %>%
  filter(DSRAEFL == "Y" | DTHFL == "Y")
midline1 <- mid1_subset %>%
  count(studyid, trt01pn, trt01p, DCREASCD, name = "count")
sortord1 <- mid1_subset %>%
  count(studyid, DCREASCD, name = "count")

# Midline 2 – DCdecod='LACK OF EFFICACY'
mid2_subset <- adsl %>%
  filter(DCdecod == "LACK OF EFFICACY")
midline2 <- mid2_subset %>%
  count(studyid, trt01pn, trt01p, DCREASCD, name = "count")
sortord2 <- mid2_subset %>%
  count(studyid, DCREASCD, name = "count")

# Midline 3 – other reasons + disconfl='Y'
mid3_subset <- adsl %>%
  filter(
    DCdecod != "LACK OF EFFICACY",
    DSRAEFL != "Y",
    DTHFL != "Y",
    disconfl == "Y"
  )
midline3 <- mid3_subset %>%
  count(studyid, trt01pn, trt01p, DCREASCD, name = "count")
sortord3 <- mid3_subset %>%
  count(studyid, DCREASCD, name = "count")

# ------------------------------------------------------------------
# 4. Combine count tables and assign ordering flags
# ------------------------------------------------------------------
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

# ------------------------------------------------------------------
# 5. Sort order within table (alls) and create ord3
# ------------------------------------------------------------------
alls2 <- alls %>%
  arrange(ord1, ord2, desc(count)) %>%
  group_by(ord1, ord2) %>%
  mutate(ord3 = row_number()) %>%
  ungroup()

# ------------------------------------------------------------------
# 6. Population counts (tot)
# ------------------------------------------------------------------
pop <- adsl %>%
  count(studyid, trt01pn, trt01p, name = "tot")

# ------------------------------------------------------------------
# 7. Create output cells with percentages (allp)
# ------------------------------------------------------------------
allp <- all %>%
  left_join(pop, by = c("studyid", "trt01pn", "trt01p")) %>%
  mutate(
    countc = if (!is.na(count)) {
      cnt_fmt <- sprintf("%4d", count)
      perc_val <- round_half_up(count / tot, 0.1)
      perc_fmt <- sprintf("%5.1f", perc_val)
      paste0(cnt_fmt, " (", perc_fmt, ")")
    } else NA_character_,
    DCREASCD = if_else(ord1 == 1L,
                      if_else(disconfl == "Y", "Discontinued", "Completed the Study"),
                      DCREASCD)
  )

# ------------------------------------------------------------------
# 8. Sort and transpose (wide) – one column per treatment code
# ------------------------------------------------------------------
allp_sorted <- allp %>%
  arrange(studyid, ord1, ord2, DCREASCD, trt01pn)

# Determine all treatment codes present (for column ordering)
treat_codes <- sort(unique(pop$trt01pn))

allpt <- allp_sorted %>%
  select(studyid, ord1, ord2, DCREASCD, trt01pn, countc) %>%
  pivot_wider(
    id_cols = c(studyid, ord1, ord2, DCREASCD),
    names_from = trt01pn,
    values_from = countc,
    names_prefix = "trt"
  )

# Ensure columns for all treatment codes exist
for (tc in treat_codes) {
  col_name <- paste0("trt", tc)
  if (!col_name %in% colnames(allpt)) {
    allpt[[col_name]] <- NA_character_
  }
}
# Reorder treatment columns by numeric code
allpt <- allpt %>%
  select(studyid, ord1, ord2, DCREASCD,
         all_of(paste0("trt", treat_codes)))

# ------------------------------------------------------------------
# 9. Macro‑like processing – replace missing cells & spaces
# ------------------------------------------------------------------
# Replace missing (NA or empty) with "###0" and spaces with "#"
zero_replace <- function(x) {
  x <- ifelse(is.na(x) | x == "", "###0", x)
  str_replace_all(x, " ", "#")
}
allpt2 <- allpt %>%
  mutate(across(starts_with("trt"), zero_replace))

# ------------------------------------------------------------------
# 10. Merge sort order (alls2) onto table
# ------------------------------------------------------------------
allpt3 <- allpt2 %>%
  left_join(alls2, by = c("studyid", "ord1", "ord2")) %>%
  mutate(
    ord3 = case_when(
      ord1 == 1L & toupper(DCREASCD) == "DISCONTINUED" ~ 2L,
      ord1 == 1L                                          ~ 1L,
      TRUE                                                ~ ord3
    )
  ) %>%
  select(studyid, DCREASCD, ord1, ord2, ord3, starts_with("trt"))

# ------------------------------------------------------------------
# 11. Final sorting
# ------------------------------------------------------------------
final <- allpt3 %>%
  arrange(studyid, ord1, ord2, ord3) %>%
  mutate(
    pagex = 1L,
    col1 = case_when(
      ord1 == 1L ~ DCREASCD,
      ord1 == 3L ~ paste0("####", DCREASCD),
      TRUE       ~ ""
    )
  ) %>%
  select(pagex, ord1, ord2, ord3, col1, starts_with("trt"))

# ------------------------------------------------------------------
# 12. Prepare treatment header information (for reference)
# ------------------------------------------------------------------
# Create a lookup for treatment name (character) and count (numeric)
treat_info <- pop %>%
  group_by(trt01pn) %>%
  summarise(
    trt_name = first(trt01p),
    trt_n    = sum(tot),
    .groups = "drop"
  ) %>%
  arrange(trt01pn)

# ------------------------------------------------------------------
# 13. Write outputs
# ------------------------------------------------------------------
# Helper to write both CSV and plain‑text rendering
write_output <- function(df, base_name) {
  csv_path <- file.path(out_dir, paste0(base_name, ".csv"))
  txt_path <- file.path(out_dir, paste0(base_name, ".txt"))
  write.csv(df, csv_path, row.names = FALSE, na = "")
  # Plain‑text rendering – tab‑separated for readability
  write.table(df, txt_path, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
}

# Table/listing outputs
write_output(final, "P048.lst")
write_output(final, "sasrtf.rtf")
