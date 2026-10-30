```r
# ------------------------------------------------------------
# P045_ae_serious – translation of SAS program to R
# ------------------------------------------------------------
# Packages (pre‑installed in the execution environment)
library(haven)      # read_xpt / read_sas
library(dplyr)      # data manipulation
library(tidyr)      # pivot_* helpers
library(purrr)      # map / reduce
library(stringr)   # string handling
library(readr)      # write_csv

# ------------------------------------------------------------
# 0. Paths (mirroring the SAS macro variables)
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
out_dir <- Sys.getenv("OUT_DIR")   # e.g. /work/out

# ------------------------------------------------------------
# 1. Read source ADaE and ADSL XPT files
adae_raw <- read_xpt(file.path(rawadam, "adae.xpt"))
adsl_raw <- read_xpt(file.path(rawadam, "adsl.xpt"))

# Convert all variable names to lower case for case‑insensitive handling
names(adae_raw) <- tolower(names(adae_raw))
names(adsl_raw) <- tolower(names(adsl_raw))

# ------------------------------------------------------------
# 2. Work.ADAE – keep only serious adverse events (SAE)
adae <- adae_raw %>%
  select(usubjid, trtan, aedecod, aeser, aocc02fl, aocc04fl, saffl) %>%
  filter((aocc02fl == "Y" | aocc04fl == "Y") &
           aeser == "Y" &
           saffl == "Y")

# ------------------------------------------------------------
# 3. Work.ADSL – keep treatment arm, drop missing
adsl <- adsl_raw %>%
  select(usubjid, trt01an) %>%
  filter(!is.na(trt01an)) %>%          # trt01an ne .
  distinct(usubjid, .keep_all = TRUE) # PROC SORT NODUPKEY

# ------------------------------------------------------------
# 4. Merge to obtain ADaE_SL (subjects with SAE + treatment)
adae_sl <- inner_join(adsl, adae, by = "usubjid")

# ------------------------------------------------------------
# 5. Denominator calculation (counts per treatment arm)
sum_1 <- adsl %>%
  count(trt01an, name = "count") %>%   # PROC FREQ NOPRINT
  arrange(trt01an)

# Macro‑like helpers (used later)
trt_counts <- setNames(sum_1$count, sum_1$trt01an)          # N per treatment
trt_codes  <- sum_1$trt01an %>% as.character()             # treatment codes as character
numtrt     <- length(trt_codes)                            # number of distinct treatments
trtnum_vec <- setNames(trt_codes, seq_len(numtrt))        # index → treatment code

# ------------------------------------------------------------
# 6. Remove duplicate subject‑AE rows (PROC SORT NODUPKEY)
adae_sl2 <- adae_sl %>%
  distinct(usubjid, aedecod, .keep_all = TRUE) %>%
  arrange(usubjid, aedecod)

# ------------------------------------------------------------
# 7. List of AE terms that have aocc02fl = 'Y' (ADAe_DUMMY)
adae_dummy <- adae_sl %>%
  filter(aocc02fl == "Y") %>%
  distinct(aedecod) %>%
  arrange(aedecod)

# ------------------------------------------------------------
# 8. Keep only AE terms that appear in the dummy list
adae_sl3 <- inner_join(adae_sl2, adae_dummy, by = "aedecod")

# ------------------------------------------------------------
# 9. Frequency of AE term by treatment (SUM_2)
sum_2 <- adae_sl3 %>%
  count(aedecod, trtan, name = "count")

# ------------------------------------------------------------
# 10. Dummy_2 – all combinations of AE term × treatment
dummy_2 <- expand_grid(aedecod = adae_dummy$aedecod,
                       trtan   = trt_codes) %>%
  arrange(aedecod, trtan)

# ------------------------------------------------------------
# 11. Merge dummy_2 with sum_2 (ADAe_SL4)
adae_sl4 <- left_join(dummy_2, sum_2, by = c("aedecod", "trtan"))

# ------------------------------------------------------------
# 12. Add denominator (BIGN) per treatment (ADAe_SL5)
adae_sl5 <- left_join(adae_sl4,
                      sum_1 %>% rename(trtan = trt01an, bign = count),
                      by = "trtan")

# ------------------------------------------------------------
# 13. Build two rows per AE term (resp = 0,1) with weighted counts (ADAe_SL6)
adae_sl6 <- adae_sl5 %>%
  arrange(trtan, aedecod) %>%
  group_by(trtan, aedecod) %>%
  do({
    df <- .
    rows <- lapply(0:1, function(j) {
      resp <- j
      count_final <- if (resp == 1) {
        if (!is.na(df$count)) df$count else 0
      } else {
        if (!is.na(df$count)) df$bign - df$count else df$bign
      }
      tibble(aedecod = df$aedecod,
             trtan    = df$trtan,
             resp     = resp,
             count_final = count_final)
    })
    bind_rows(rows)
  }) %>%
  ungroup()

# ------------------------------------------------------------
# 14. Weighted frequencies per AE term (SUM_FINAL) and Fisher exact p‑values
sum_final_df <- adae_sl6 %>%
  group_by(aedecod, trtan, resp) %>%
  summarise(count = sum(count_final), .groups = "drop")

# Fisher exact test for each AE term (produces one p‑value per aedecod)
fish_df <- sum_final_df %>%
  group_by(aedecod) %>%
  summarise(cvalue1 = {
    mat <- pivot_wider(cur_data(),
                       names_from = resp,
                       values_from = count,
                       values_fill = list(count = 0)) %>%
      arrange(trtan) %>%
      select(`0`, `1`) %>%
      as.matrix()
    pval <- tryCatch(fisher.test(mat)$p.value, error = function(e) NA_real_)
    sprintf("%.4f", pval)
  }, .groups = "drop")

# ------------------------------------------------------------
# 15. Prepare the table of event counts (resp = 1) – same as SAS CP variable
sum_final_evt <- sum_final_df %>%
  filter(resp == 1) %>%
  mutate(cp = paste0(count, "(", sprintf("%.1f", count), ")"),
         ord = ifelse(trtan == 81, 1L, 2L)) %>%   # trtan=81 gets ord=1, else 2
  arrange(aedecod, ord)

# Determine count_max (the count for trtan = 81) for sorting
count_max_df <- sum_final_evt %>%
  filter(ord == 1) %>%
  select(aedecod, count_max = count)

sum_final_evt <- left_join(sum_final_evt, count_max_df, by = "aedecod")

# Transpose to wide format with prefix trt (SUM_FINAL2)
sum_final2 <- sum_final_evt %>%
  select(aedecod, trtan, cp, count_max) %>%
  pivot_wider(names_from = trtan,
              values_from = cp,
              names_prefix = "trt",
              values_fill = list(cp = ""))

# Ensure the three expected treatment columns exist
for (code in c(0, 54, 81)) {
  col_name <- paste0("trt", code)
  if (!col_name %in% names(sum_final2)) sum_final2[[col_name]] <- ""
}

# Merge p‑value (FISH2) and order as in SAS
sum_final2 <- left_join(sum_final2, fish_df, by = "aedecod") %>%
  arrange(desc(count_max), aedecod) %>%
  select(aedecod, trt0, trt54, trt81, cvalue1)

# ------------------------------------------------------------
# 16. Subject‑level analysis – at least one SAE per subject
# Subjects with at least one SAE (aocc02fl = 'Y')
adae_sl_sub <- adae_sl %>%
  filter(aocc02fl == "Y") %>%
  distinct(usubjid)

# Flag per subject (FLG)
adsl_flg <- adsl %>%
  left_join(adae_sl_sub, by = "usubjid", suffix = c("", ".y")) %>%
  mutate(flg = ifelse(!is.na(usubjid.y), 1L, 0L)) %>%
  select(usubjid, trt01an, flg) %>%
  rename(trtan = trt01an)

# Frequency table for Fisher test (subject level)
sum_final_sub_df <- adsl_flg %>%
  count(trtan, flg, name = "count")

# Fisher exact test (overall, not by aedecod)
ft_sub_mat <- sum_final_sub_df %>%
  pivot_wider(names_from = flg,
              values_from = count,
              values_fill = list(count = 0)) %>%
  arrange(trtan) %>%
  select(`0`, `1`) %>%
  as.matrix()
p_sub <- tryCatch(fisher.test(ft_sub_mat)$p.value, error = function(e) NA_real_)
p_sub_fmt <- sprintf("%.4f", p_sub)

# Build the row for "Number of subjects reporting serious adverse events"
sum_final_sub <- sum_final_sub_df %>%
  filter(flg == 1) %>%
  mutate(cp = paste0(count, "(", sprintf("%.1f", count), ")"),
         aedecod = "Number of subjects reporting serious adverse events") %>%
  select(aedecod, trtan, cp)

# Transpose to wide format (SUM_FINAL_SUB2)
sum_final_sub2 <- sum_final_sub %>%
  pivot_wider(names_from = trtan,
              values_from = cp,
              names_prefix = "trt",
              values_fill = list(cp = ""))

# Ensure expected treatment columns exist
for (code in c(0, 54, 81)) {
  col_name <- paste0("trt", code)
  if (!col_name %in% names(sum_final_sub2)) sum_final_sub2[[col_name]] <- ""
}

# Add p‑value column
sum_final_sub2 <- sum_final_sub2 %>%
  mutate(cvalue1 = p_sub_fmt) %>%
  select(aedecod, trt0, trt54, trt81, cvalue1)

# ------------------------------------------------------------
# 17. Final report (stack subject‑level row on top of AE rows)
final_report <- bind_rows(sum_final_sub2, sum_final2)

# ------------------------------------------------------------
# 18. Write outputs ------------------------------------------------
# CSV of the final report (dataset output)
write.csv(final_report,
          file = file.path(out_dir, "P045.csv"),
          row.names = FALSE,
          na = "")

# Text listing (PROC REPORT style) – name must match expected .lst file
lst_path <- file.path(out_dir, "P045.lst")
con <- file(lst_path, open = "wt")
writeLines(c(
  "Table 7.5 Summary of Serious Adverse Events",
  "Safety Population",
  "-"), con)

# Write the table body as tab‑separated values (mirrors PROC REPORT output)
write.table(final_report,
            con,
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

writeLines(c(
  "-",
  "Denominator for each % is treatment column N",
  "*b = p-values are from Fisher's Exact Test"), con)
close(con)

# Also write a CSV version of the listing body (optional, aids verification)
write.csv(final_report,
          file = file.path(out_dir, "P045_listing.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```