# P049_disposition_b1.R
# Translation of SAS program P049_disposition_b1.sas to R
# Generates the final disposition table as CSV and plain‑text listing

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ----------------------------------------------------------------------
# Helper: round half up (SAS default)
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

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adsl_path <- file.path(rawadam, "adsl.xpt")

# ----------------------------------------------------------------------
# Read source ADaM dataset
adsl <- read_xpt(adsl_path) %>% 
  as_tibble() %>% 
  # ensure character columns are plain character (haven may give labelled)
  mutate(across(everything(), ~ if(is.factor(.)) as.character(.) else .))

# ----------------------------------------------------------------------
# Filter to ITT population
adsl_itt <- adsl %>% filter(ITTFL == "Y")

# ----------------------------------------------------------------------
# 1) Build DISP table (union of two queries)
# First part: Completed vs Discontinued (ord = 1)
disp_part1 <- adsl_itt %>%
  group_by(TRT01P, TRT01PN) %>%
  summarise(
    cnt = n(),
    status = ifelse(DCREASCD == "Completed", "Completed", "Discontinued"),
    .groups = "drop"
  ) %>%
  mutate(ord = 1L)

# Second part: each non‑Completed reason (ord = 2)
disp_part2 <- adsl_itt %>%
  filter(DCREASCD != "Completed") %>%
  group_by(TRT01P, TRT01PN, DCREASCD) %>%
  summarise(
    cnt = n(),
    .groups = "drop"
  ) %>%
  rename(status = DCREASCD) %>%
  mutate(ord = 2L)

disp <- bind_rows(disp_part1, disp_part2) %>%
  arrange(TRT01PN, ord, status)

# ----------------------------------------------------------------------
# 2) Build FREQ table (total per treatment)
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
# 4) Transpose (wide) – mimic PROC TRANSPOSE with ID=TRT01PN, IDLABEL=treat
# Determine the order of TRT01PN (character sort, case‑sensitive)
trt_order <- disp_2 %>% distinct(TRT01PN) %>% arrange(TRT01PN) %>% pull(TRT01PN)

# Create a list of columns (tr1, tr2, …) in the same order as TRT01PN
trans_wide <- disp_2 %>%
  select(status, ord, TRT01PN, treat, cnt_prcnt) %>%
  mutate(col_idx = match(TRT01PN, trt_order)) %>%
  mutate(tr_name = str_c("tr", col_idx)) %>%
  select(status, ord, tr_name, cnt_prcnt) %>%
  pivot_wider(names_from = tr_name, values_from = cnt_prcnt) %>%
  arrange(ord, status)

# ----------------------------------------------------------------------
# 5) Output -------------------------------------------------------------
# 5a) CSV of the transposed table (dataset output)
csv_path <- file.path(out_dir, "P049.csv")
write.csv(trans_wide, csv_path, row.names = FALSE, na = "")

# 5b) Plain‑text listing (table output)
txt_path <- file.path(out_dir, "P049.lst")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

# Title line (as in PROC REPORT)
writeLines("Subject Disposition", con)

# Header line (status + each treatment column)
header_vals <- names(trans_wide)
# Remove the internal ordering columns from header display (ord is hidden in SAS report)
display_header <- header_vals[!header_vals %in% c("ord")]
writeLines(paste(display_header, collapse = "\t"), con)

# Table rows
for (i in seq_len(nrow(trans_wide))) {
  row_vals <- trans_wide[i, ]
  # Omit ord column from display
  display_vals <- row_vals %>% select(-ord) %>% as.character()
  writeLines(paste(display_vals, collapse = "\t"), con)
  # Insert a blank line after each change in ord (mimic BREAK AFTER ORD)
  if (i < nrow(trans_wide) && trans_wide$ord[i] != trans_wide$ord[i + 1]) {
    writeLines("", con)
  }
}

# Footer (abbreviation note) – same as SAS compute after _page_
writeLines("", con)
writeLines("Abbreviations:  N = number of subjects in the population; n=number of subjects specified category", con)
writeLines("% = Percentage of subjects with N as denominator", con)

close(con)

# ----------------------------------------------------------------------
# End of script
