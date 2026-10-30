```r
# P049_disposition_b1.R
# Translation of SAS program P049_disposition_b1.sas to R
# Produces the disposition table as CSV and plain‑text listing

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------------------------------------------------
# Helper: SAS‑style round half up (used for percentages)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  posneg * rounded
}

# ----------------------------------------------------------------------
# Paths
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

rawadam   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adsl_path <- file.path(rawadam, "adsl.xpt")

# ----------------------------------------------------------------------
# Read ADaM source dataset
adsl <- read_xpt(adsl_path) %>% 
  as_tibble() %>% 
  mutate(across(everything(),
                ~ if (is.factor(.)) as.character(.) else .))

# ----------------------------------------------------------------------
# Keep only ITT population
adsl_itt <- adsl %>% filter(ITTFL == "Y")

# ----------------------------------------------------------------------
# 1) Build DISP table (union of two queries)

# Part 1: Completed vs Discontinued (ord = 1)
disp_part1 <- adsl_itt %>%
  mutate(status = ifelse(DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, status) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  mutate(ord = 1L)

# Part 2: each non‑Completed reason (ord = 2)
disp_part2 <- adsl_itt %>%
  filter(DCREASCD != "Completed") %>%
  group_by(TRT01P, TRT01PN, DCREASCD) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  rename(status = DCREASCD) %>%
  mutate(ord = 2L)

disp <- bind_rows(disp_part1, disp_part2) %>%
  arrange(TRT01PN, ord, status)

# ----------------------------------------------------------------------
# 2) Build FREQ table (total subjects per treatment)
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = n(), .groups = "drop") %>%
  arrange(TRT01PN)

# ----------------------------------------------------------------------
# 3) Merge DISP with FREQ and create formatted strings
disp_2 <- disp %>%
  left_join(freq, by = "TRT01PN") %>%
  mutate(
    treat = str_c(str_trim(TRT01P), " (N=", tot_cnt, ")"),
    pct   = round_half_up(cnt / tot_cnt * 100, 1),
    cnt_prcnt = str_c(cnt, " (", format(pct, nsmall = 1), "%)")
  ) %>%
  arrange(status, ord)

# ----------------------------------------------------------------------
# 4) Transpose to wide format (like PROC TRANSPOSE)
#   - rows: one per status/ord
#   - columns: one per TRT01PN, values = cnt_prcnt
trans_wide <- disp_2 %>%
  select(status, ord, TRT01PN, cnt_prcnt) %>%
  pivot_wider(names_from = TRT01PN, values_from = cnt_prcnt) %>%
  arrange(ord, status)

# Mapping from TRT01PN to the treatment label (used as column headings)
treat_map <- disp_2 %>%
  distinct(TRT01PN, treat) %>%
  arrange(TRT01PN)

# ----------------------------------------------------------------------
# 5) Output ----------------------------------------------------------------

# 5a) CSV of the table body (status + treatment columns, column names = treatment labels)
csv_body <- trans_wide %>%
  select(-ord) %>%
  rename_with(~ treat_map$treat[match(., treat_map$TRT01PN)], .cols = -status)

csv_path <- file.path(out_dir, "P049.csv")
write.csv(csv_body, csv_path, row.names = FALSE, na = "")

# 5b) Plain‑text listing (mimicking PROC REPORT output)
txt_path <- file.path(out_dir, "P049.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

# Title line
writeLines("Subject Disposition", con)

# Header line: first column "Subject Disposition", then treatment labels in TRT01PN order
header_vals <- c("Subject Disposition", treat_map$treat)
writeLines(paste(header_vals, collapse = "\t"), con)

# Table rows, inserting a blank line after each change in ORD (BREAK AFTER ORD)
prev_ord <- NA_integer_
for (i in seq_len(nrow(trans_wide))) {
  row <- trans_wide[i, ]
  # Build row: status + each cnt_prcnt value in TRT01PN order
  row_vals <- c(row$status,
                as.character(row[ , treat_map$TRT01PN]))
  writeLines(paste(row_vals, collapse = "\t"), con)
  # Blank line after ord changes
  if (!is.na(prev_ord) && row$ord != prev_ord) {
    writeLines("", con)
  }
  prev_ord <- row$ord
}

# Footer (abbreviation note) – same as SAS compute after _page_
writeLines("", con)
writeLines("Abbreviations:  N = number of subjects in the population; n=number of subjects specified category", con)
writeLines("% = Percentage of subjects with N as denominator", con)

close(con)

# ----------------------------------------------------------------------
# End of script
```