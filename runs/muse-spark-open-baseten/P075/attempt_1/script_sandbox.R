# P075_t_vs.sas -> R translation
# Table 14.3.5 Summary of Vital Signs by Visit (Safety Analysis Set)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(lubridate)
})

# helper: round half away from zero (SAS)
round_half_away <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}
sas_put <- function(x, digits = 1) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  rx <- round_half_away(x, digits)
  # format with exactly digits decimals, trim
  fmt <- formatC(rx, format = "f", digits = digits, flag = "")
  trimws(fmt)
}
sas_put0 <- function(x) {
  if (is.na(x)) return("")
  rx <- round_half_away(x, 0)
  trimws(formatC(rx, format = "f", digits = 0, flag = ""))
}

# helper to read adam dataset (sas7bdat or xpt)
read_adam <- function(name) {
  # try sas7bdat
  f1 <- file.path(adam_path, paste0(name, ".sas7bdat"))
  f2 <- file.path(adam_path, paste0(tolower(name), ".sas7bdat"))
  f3 <- file.path(adam_path, paste0(name, ".xpt"))
  f4 <- file.path(adam_path, paste0(tolower(name), ".xpt"))
  # also list files case-insensitive
  if (file.exists(f1)) return(haven::read_sas(f1))
  if (file.exists(f2)) return(haven::read_sas(f2))
  if (file.exists(f3)) return(haven::read_xpt(f3))
  if (file.exists(f4)) return(haven::read_xpt(f4))
  # fallback: list dir
  allf <- list.files(adam_path, full.names = TRUE, ignore.case = TRUE)
  cand <- allf[grepl(paste0("^", name, "\\."), basename(allf), ignore.case = TRUE)]
  if (length(cand) > 0) {
    if (grepl("\\.sas7bdat$", cand[1], ignore.case = TRUE)) return(haven::read_sas(cand[1]))
    if (grepl("\\.xpt$", cand[1], ignore.case = TRUE)) return(haven::read_xpt(cand[1]))
  }
  stop(paste("Cannot find dataset", name, "in", adam_path))
}

cat("Reading ADSL and ADVS...\n")
adsl <- read_adam("adsl")
advs <- read_adam("advs")

# normalize names to uppercase for consistency (haven preserves case)
names(adsl) <- toupper(names(adsl))
names(advs) <- toupper(names(advs))

# Ensure character variables trimmed
trim_char <- function(df) {
  df %>% mutate(across(where(is.character), ~trimws(.x)))
}
adsl <- trim_char(adsl)
advs <- trim_char(advs)

# Step 1: Get Big N (for log, not directly displayed but keep)
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  distinct(USUBJID, TRTA) %>%
  group_by(TRTA) %>%
  summarise(BIGN = n(), .groups = "drop") %>%
  arrange(TRTA)
bign_tot <- adsl %>% filter(toupper(SAFFL) == "Y") %>% distinct(USUBJID) %>% nrow()
cat("Big N:\n")
print(bign_df)
cat(paste0("Total N=", bign_tot, "\n"))

# Step 2: Prepare analysis data
# inner join advs with adsl on USUBJID where SAFFL='Y' and PARAMCD in list and ANL01FL='Y'
param_keep <- c("SYSBP","DIABP","PULSE","TEMP","RESP","WEIGHT")

# ensure required columns exist
# ADVS should have USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, AVAL, BASE, CHG, ABLFL, ANL01FL
# ADSL should have USUBJID, SAFFL

vs <- advs %>%
  inner_join(adsl %>% filter(toupper(SAFFL) == "Y") %>% select(USUBJID), by = "USUBJID") %>%
  filter(PARAMCD %in% param_keep) %>%
  filter(toupper(ANL01FL) == "Y") %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, AVAL, BASE, CHG, ABLFL, ANL01FL) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Ensure numeric types
vs <- vs %>%
  mutate(
    TRTAN = as.numeric(TRTAN),
    PARAMN = as.numeric(PARAMN),
    AVISITN = as.numeric(AVISITN),
    AVAL = as.numeric(AVAL),
    CHG = as.numeric(CHG)
  )

# Step 3: Descriptive stats by parameter, visit, treatment
# Actual value
actual <- vs %>%
  filter(!is.na(AVAL)) %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    n = n(),
    mean = mean(AVAL, na.rm = TRUE),
    std = ifelse(n() > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    median = median(AVAL, na.rm = TRUE),
    min = min(AVAL, na.rm = TRUE),
    max = max(AVAL, na.rm = TRUE),
    .groups = "drop"
  )

# Change from baseline: where CHG ne . and ABLFL ne 'Y'
change <- vs %>%
  filter(!is.na(CHG)) %>%
  filter(is.na(ABLFL) | toupper(ABLFL) != "Y") %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    n_chg = n(),
    mean_chg = mean(CHG, na.rm = TRUE),
    std_chg = ifelse(n() > 1, sd(CHG, na.rm = TRUE), NA_real_),
    median_chg = median(CHG, na.rm = TRUE),
    min_chg = min(CHG, na.rm = TRUE),
    max_chg = max(CHG, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Merge actual and change
stats <- actual %>%
  left_join(change, by = c("PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","TRTA","TRTAN")) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Step 5: Format for display
report <- stats %>%
  rowwise() %>%
  mutate(
    stat1 = sas_put0(n),
    stat2 = ifelse(is.na(mean), "", sas_put(mean, 1)),
    stat3 = ifelse(is.na(std), "", sas_put(std, 2)),
    stat4 = ifelse(is.na(median), "", sas_put(median, 1)),
    stat5 = ifelse(is.na(min) | is.na(max), "", paste0(sas_put(min, 1), ", ", sas_put(max, 1))),
    chg1 = ifelse(is.na(n_chg) | n_chg == 0, "", sas_put0(n_chg)),
    chg2 = ifelse(is.na(n_chg) | n_chg == 0 | is.na(mean_chg), "", sas_put(mean_chg, 2)),
    chg3 = ifelse(is.na(n_chg) | n_chg == 0 | is.na(std_chg), "", sas_put(std_chg, 2)),
    chg4 = ifelse(is.na(n_chg) | n_chg == 0 | is.na(median_chg), "", sas_put(median_chg, 2)),
    chg5 = ifelse(is.na(n_chg) | n_chg == 0 | is.na(min_chg) | is.na(max_chg), "", paste0(sas_put(min_chg, 1), ", ", sas_put(max_chg, 1)))
  ) %>%
  ungroup() %>%
  # replace NA in chg columns with ""
  mutate(
    chg1 = ifelse(is.na(chg1), "", chg1),
    chg2 = ifelse(is.na(chg2), "", chg2),
    chg3 = ifelse(is.na(chg3), "", chg3),
    chg4 = ifelse(is.na(chg4), "", chg4),
    chg5 = ifelse(is.na(chg5), "", chg5)
  ) %>%
  select(PARAMN, PARAM, AVISITN, AVISIT, TRTA, TRTAN,
         stat1, stat2, stat3, stat4, stat5,
         chg1, chg2, chg3, chg4, chg5,
         n, mean, std, median, min, max,
         n_chg, mean_chg, std_chg, median_chg, min_chg, max_chg,
         PARAMCD) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# For CSV output, keep display columns plus identifiers
report_csv <- report %>%
  select(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN,
         stat1, stat2, stat3, stat4, stat5,
         chg1, chg2, chg3, chg4, chg5)

# Write CSV (table body)
write.csv(report_csv, file.path(out, "t_vs.csv"), row.names = FALSE, na = "")

# Also write a dataset CSV with numeric stats for traceability (optional, but keep one file as required)
# The main CSV is t_vs.csv; we also ensure t_vs dataset is available

# Step 6: Generate text rendering (mimic RTF)
# Titles from u_titles.sas for T_VS
title1 <- "Table 14.3.5"
title2 <- "Summary of Vital Signs by Visit"
title3 <- "Safety Analysis Set"

# Build header strings with Big N
# Create header labels like "Placebo (N=xx)"
hdr_map <- bign_df %>% mutate(hdr = paste0(TRTA, " (N=", BIGN, ")"))
hdr_lines <- paste(hdr_map$hdr, collapse = " | ")

txt_path <- file.path(out, "t_vs.txt")
con <- file(txt_path, open = "wt")

writeLines(paste0(title1), con)
writeLines(paste0(title2), con)
writeLines(paste0(title3), con)
writeLines("", con)
writeLines(paste0("Treatment groups: ", hdr_lines, " | Total (N=", bign_tot, ")"), con)
writeLines(paste0("Data cutoff: [DDMONYYYY]   Source: ADaM datasets"), con)
writeLines(strrep("-", 180), con)

# Column headers
header1 <- sprintf("%-25s %-20s %-20s %8s %8s %8s %8s %12s | %8s %8s %8s %8s %12s",
                   "Parameter", "Visit", "Treatment",
                   "n", "Mean", "SD", "Median", "Min, Max",
                   "n", "Mean", "SD", "Median", "Min, Max")
header2 <- sprintf("%-25s %-20s %-20s %8s %8s %8s %8s %12s | %8s %8s %8s %8s %12s",
                   "", "", "",
                   "", "", "", "", "",
                   "", "", "", "", "")
subheader <- sprintf("%-25s %-20s %-20s %-58s | %-58s",
                     "", "", "",
                     "Actual Value", "Change from Baseline")
writeLines(subheader, con)
writeLines(header1, con)
writeLines(strrep("-", 180), con)

# Iterate by PARAM (page break)
params <- report %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN)
for (i in seq_len(nrow(params))) {
  pnam <- params$PARAM[i]
  pnum <- params$PARAMN[i]
  writeLines(paste0("Parameter: ", pnam, " (", pnum, ")"), con)
  sub <- report %>% filter(PARAMN == pnum) %>% arrange(AVISITN, TRTAN)
  for (r in seq_len(nrow(sub))) {
    row <- sub[r, ]
    line <- sprintf("%-25s %-20s %-20s %8s %8s %8s %8s %12s | %8s %8s %8s %8s %12s",
                    ifelse(r == 1 || sub$AVISIT[r] != sub$AVISIT[r-1], substr(row$PARAM,1,25), ""),
                    substr(row$AVISIT,1,20),
                    substr(row$TRTA,1,20),
                    row$stat1, row$stat2, row$stat3, row$stat4, row$stat5,
                    row$chg1, row$chg2, row$chg3, row$chg4, row$chg5)
    writeLines(line, con)
  }
  writeLines("", con)
  writeLines(strrep("-", 180), con)
}

writeLines("", con)
writeLines("Footnotes:", con)
writeLines("Data cutoff: [DDMONYYYY]", con)
writeLines("Source: ADaM datasets", con)
writeLines(paste0("Program: P075_t_vs.sas | Run: ", format(Sys.Date(), "%d%b%Y")), con)

close(con)

cat(paste0("Outputs written to ", out, "\n"))
cat(paste0("CSV rows: ", nrow(report_csv), "  TXT: ", txt_path, "\n"))

# Also ensure a copy named t_vs.rtf.txt if needed? The spec expects t_vs.rtf -> we provide txt and csv
# Create an additional .rtf text placeholder for compatibility (plain text)
rtf_txt_path <- file.path(out, "t_vs.rtf")
if (!file.exists(rtf_txt_path)) {
  file.copy(txt_path, rtf_txt_path, overwrite = TRUE)
}

# Clean up work datasets (not needed in R)
cat("P075 t_vs completed successfully.\n")
