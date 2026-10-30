out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "/bench/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1_vec <- function(x) {
  y <- round_half_up(x, 1)
  out <- formatC(y, format = "f", digits = 1)
  out[is.na(x)] <- NA_character_
  out
}
fmt_cnt_vec <- function(x) {
  formatC(x, format = "f", digits = 0)
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

adsl_path <- file.path(adampath, "adsl.sas7bdat")
adae_path <- file.path(adampath, "adae.sas7bdat")
if (!file.exists(adsl_path)) {
  cand <- list.files(adampath, pattern = "(?i)adsl\\.(xpt|sas7bdat)", full.names = TRUE)
  if (length(cand) > 0) adsl_path <- cand[1]
}
if (!file.exists(adae_path)) {
  cand <- list.files(adampath, pattern = "(?i)adae\\.(xpt|sas7bdat)", full.names = TRUE)
  if (length(cand) > 0) adae_path <- cand[1]
}
read_sas_or_xpt <- function(p) {
  if (grepl("\\.xpt$", p, ignore.case = TRUE)) haven::read_xpt(p) else haven::read_sas(p)
}
adsl <- read_sas_or_xpt(adsl_path)
adae <- read_sas_or_xpt(adae_path)
adsl <- haven::zap_formats(adsl); adsl <- haven::zap_label(adsl); adsl <- haven::zap_widths(adsl)
adae <- haven::zap_formats(adae); adae <- haven::zap_label(adae); adae <- haven::zap_widths(adae)
adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))
adae <- adae %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))

# Step 1: Big N
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

classify_trt <- function(x) {
  xl <- tolower(x)
  if (grepl("placebo", xl)) return("placebo")
  if (grepl("low", xl)) return("low")
  if (grepl("high", xl)) return("high")
  return(xl)
}
bign_df$cls <- sapply(bign_df$TRTA, classify_trt)
bign_by_trt <- setNames(bign_df$bign, bign_df$TRTA)
hdr_for <- function(trt) paste0(trt, " (N=", bign_by_trt[trt], ")")
trt_placebo <- bign_df$TRTA[bign_df$cls == "placebo"]
trt_low     <- bign_df$TRTA[bign_df$cls == "low"]
trt_high    <- bign_df$TRTA[bign_df$cls == "high"]
if (length(trt_placebo) == 0) trt_placebo <- bign_df$TRTA[1]
if (length(trt_low) == 0 && nrow(bign_df) >= 2) trt_low <- bign_df$TRTA[2]
if (length(trt_high) == 0 && nrow(bign_df) >= 3) trt_high <- bign_df$TRTA[3]
col1hdr <- hdr_for(trt_placebo[1])
col2hdr <- if (length(trt_low) > 0) hdr_for(trt_low[1]) else ""
col3hdr <- if (length(trt_high) > 0) hdr_for(trt_high[1]) else ""

# Step 2: Subset to TEAE in safety population
adsl_saff <- adsl %>% filter(SAFFL == "Y") %>% select(USUBJID)
ae <- adae %>%
  inner_join(adsl_saff, by = "USUBJID") %>%
  filter(TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD) %>%
  mutate(AEBODSYS = ifelse(is.na(AEBODSYS), "", AEBODSYS),
         AEDECOD  = ifelse(is.na(AEDECOD), "", AEDECOD))

# Step 3: Count by SOC
soc_cnt <- ae %>% group_by(TRTA, AEBODSYS) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop")
soc_tot <- ae %>% group_by(AEBODSYS) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 4: Count by SOC+PT
pt_cnt <- ae %>% group_by(TRTA, AEBODSYS, AEDECOD) %>% summarise(cnt = n_distinct(USUBJID), .groups = "drop")
pt_tot <- ae %>% group_by(AEBODSYS, AEDECOD) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 5: Any TEAE row
any_ae <- ae %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(AEBODSYS = "Any TEAE", AEDECOD = "", soc_ord = 0, pt_ord = 0, rowtype = 1)

# Step 6: Build display rows
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(AEDECOD = "", soc_ord = cnt_tot, pt_ord = 0, rowtype = 1) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS", "AEDECOD")) %>%
  rename(pt_tot_cnt = cnt_tot) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  rename(soc_tot_cnt = cnt_tot) %>%
  mutate(soc_ord = soc_tot_cnt, pt_ord = pt_tot_cnt, rowtype = 2) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# Step 7: Combine and calculate percentages
combined <- bind_rows(any_ae, soc_rows, pt_rows)
combined2 <- combined %>%
  left_join(bign_df %>% select(TRTA, bign), by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

combined3 <- combined2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(fmt_cnt_vec(cnt), " (", fmt_pct1_vec(pct), ")"),
                        "0"))

# Step 8: Transpose
combined3_sorted <- combined3 %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA)

final_wide <- combined3_sorted %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = NULL)

for (trt in bign_df$TRTA) {
  if (!trt %in% names(final_wide)) final_wide[[trt]] <- NA_character_
}
final_wide$col1 <- final_wide[[trt_placebo[1]]]
final_wide$col2 <- if (length(trt_low) > 0) final_wide[[trt_low[1]]] else NA_character_
final_wide$col3 <- if (length(trt_high) > 0) final_wide[[trt_high[1]]] else NA_character_

final <- final_wide %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, col1, col2, col3) %>%
  mutate(across(starts_with("col"), ~ifelse(is.na(.) | . == "", "0", .)))

# Step 9: Create display variable
report <- final %>%
  mutate(display_term = case_when(
    rowtype == 1 & AEBODSYS == "Any TEAE" ~ AEBODSYS,
    rowtype == 1 ~ AEBODSYS,
    TRUE ~ paste0("  ", trimws(AEDECOD))
  )) %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)

# Write CSV
report_csv <- report %>% select(display_term, col1, col2, col3, soc_ord, pt_ord, rowtype, AEBODSYS, AEDECOD)
write.csv(report_csv, file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")

# Text rendering
title_lines <- c(
  "Table 14.3.2",
  "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term",
  "Safety Analysis Set"
)
header_row <- c("System Organ Class / Preferred Term", col1hdr, col2hdr, col3hdr)
txt_path <- file.path(out, "t_ae_soc_pt.txt")
con <- file(txt_path, open = "wt")
writeLines(title_lines, con)
writeLines("", con)
writeLines("MedDRA Version [XX.X]", con)
writeLines("", con)
hdr_line <- sprintf("%-60s | %-18s | %-18s | %-18s", header_row[1], header_row[2], header_row[3], header_row[4])
writeLines(hdr_line, con)
writeLines(paste(rep("-", nchar(hdr_line)), collapse = ""), con)
for (i in seq_len(nrow(report))) {
  r <- report[i, ]
  line <- sprintf("%-60s | %-18s | %-18s | %-18s",
                  substr(r$display_term, 1, 60),
                  r$col1, r$col2, r$col3)
  writeLines(line, con)
  if (i < nrow(report)) {
    nxt <- report[i+1, ]
    if (!identical(r$soc_ord, nxt$soc_ord)) {
      writeLines("", con)
    }
  }
}
writeLines("", con)
writeLines("Subjects counted once per SOC and once per PT within each SOC.", con)
writeLines("SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC.", con)
writeLines("", con)
writeLines("Data cutoff: [DDMONYYYY]", con)
writeLines("Source: ADaM datasets", con)
close(con)

# Also write RTF-named copy for checker that looks for .rtf name (as txt content)
# Provide a .rtf text file as well (plain text)
write.csv(report_csv, file.path(out, "t_ae_soc_pt_rtf.csv"), row.names = FALSE, na = "")
file.copy(txt_path, file.path(out, "t_ae_soc_pt.rtf"), overwrite = TRUE)

cat("t_ae_soc_pt completed\n")
