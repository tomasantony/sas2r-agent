out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
# helper: SAS PUT with w.d (round half away)
sas_put_wd <- function(x, w, d) {
  if (is.na(x)) return("")
  v <- sas_round(x, d)
  fmt <- paste0("%.", d, "f")
  sprintf(fmt, v)
}
# helper: strip = trim both
strip <- function(x) trimws(x, which = "both")

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# read ADSL and ADCM
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

adsl_file <- file.path(adam_path, "adsl.sas7bdat")
adcm_file <- file.path(adam_path, "adcm.sas7bdat")
# fallback to .xpt if needed
if (!file.exists(adsl_file)) {
  adsl_file <- file.path(adam_path, "adsl.xpt")
  adsl <- haven::read_xpt(adsl_file)
} else {
  adsl <- haven::read_sas(adsl_file)
}
if (!file.exists(adcm_file)) {
  adcm_file <- file.path(adam_path, "adcm.xpt")
  adcm <- haven::read_xpt(adcm_file)
} else {
  adcm <- haven::read_sas(adcm_file)
}

# ensure character columns are character (haven may give labelled)
adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))
adcm <- adcm %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))

# Step 1: Big N (SAFFL='Y')
# SAS: where upcase(SAFFL) = "Y" group by TRTA order by TRTA
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# also need TRTAN for ordering; get mapping TRTA -> TRTAN
trt_map <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  select(TRTA, TRTAN) %>%
  distinct() %>%
  arrange(TRTAN)

# total N
bign_tot <- adsl %>% filter(toupper(SAFFL) == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)

# Build header strings in TRTAN order (Placebo, Low, High) to match transpose rename
# SAS headers are in TRTA alphabetical order, but report columns are col1=Placebo, col2=Low, col3=High
# We create headers correctly for display
hdr_df <- trt_map %>% left_join(bign_df, by = "TRTA") %>%
  mutate(hdr = paste0(TRTA, " (N=", bign, ")"))
# ensure order by TRTAN
hdr_df <- hdr_df %>% arrange(TRTAN)
col_hdrs <- hdr_df$hdr
# pad to 3
while(length(col_hdrs) < 3) col_hdrs <- c(col_hdrs, paste0("TRT", length(col_hdrs)+1))

col1hdr <- col_hdrs[1]
col2hdr <- col_hdrs[2]
col3hdr <- col_hdrs[3]

# Step 2: Prepare CM data
# inner join adcm with adsl on USUBJID where SAFFL='Y' and CONCOMFL='Y'
# SAS: select a.USUBJID, a.TRTA, a.TRTAN, a.CMCLAS, a.CMDECOD, a.CMINDC, a.CMSTDTC, a.CMENDTC
# from adam.adcm a inner join adam.adsl b on a.USUBJID=b.USUBJID where b.SAFFL='Y' and a.CONCOMFL='Y'
cm <- adcm %>%
  inner_join(adsl %>% filter(toupper(SAFFL) == "Y") %>% select(USUBJID) %>% distinct(), by = "USUBJID") %>%
  filter(toupper(CONCOMFL) == "Y") %>%
  select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD, CMINDC, CMSTDTC, CMENDTC) %>%
  arrange(TRTAN, CMCLAS, CMDECOD)

# Step 3: Any CM row
any_df <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0L,
         pref_ord = 0L,
         rowtype = 0L,
         CMCLAS = NA_character_,
         CMDECOD = NA_character_)

# Step 4: ATC class counts
atc <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# SAS does proc sort by CMCLAS then merge by CMCLAS
atc_rows <- atc %>%
  left_join(atc_tot, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0L,
         rowtype = 1L,
         CMDECOD = NA_character_) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)

# Step 5: Preferred name counts
pref <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  rename(pref_ord = cnt_tot)

# need atc_tot for atc_ord
pref_rows <- pref %>%
  left_join(pref_tot, by = c("CMCLAS", "CMDECOD")) %>%
  left_join(atc_tot %>% rename(atc_ord = cnt_tot), by = "CMCLAS") %>%
  mutate(display_term = paste0("  ", trimws(CMDECOD)),
         rowtype = 2L) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)

# Step 6: Combine and calculate percentages
combined <- bind_rows(any_df %>% select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype),
                      atc_rows,
                      pref_rows)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)) %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA)

# value formatting: cat(strip(put(cnt,8.)), ' (', strip(put(pct,5.1)), ')') else '0'
combined3 <- combined2 %>%
  mutate(value = ifelse(cnt > 0,
                         paste0(strip(as.character(cnt)), " (", strip(sas_put_wd(pct, 5, 1)), ")"),
                         "0"))

# Step 7: Transpose
# SAS: proc sort nodupkey by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA
# then proc transpose by descending atc_ord CMCLAS rowtype descending pref_ord display_term; id TRTA; var value
# Need to handle duplicate keys: nodupkey keeps first per sort key
combined3_sorted <- combined3 %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA) %>%
  distinct(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA, .keep_all = TRUE)

# For transpose, we need to pivot wider
# Identify TRTA values in TRTAN order to map to col1,col2,col3
trt_order <- trt_map %>% arrange(TRTAN) %>% pull(TRTA)
# pivot
final_wide <- combined3_sorted %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# Ensure columns for all TRTs exist
for (tt in trt_order) {
  if (!tt %in% names(final_wide)) final_wide[[tt]] <- NA_character_
}
# Rename to col1,col2,col3 in TRTAN order
# SAS rename: colPlacebo=col1 colDrug_X_Low_Dose=col2 colDrug_X_High_Dose=col3
# We map by TRTAN order
final <- final_wide %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, all_of(trt_order))

# rename to col1,col2,col3 for convenience
names(final)[names(final) == trt_order[1]] <- "col1"
if (length(trt_order) >= 2) names(final)[names(final) == trt_order[2]] <- "col2"
if (length(trt_order) >= 3) names(final)[names(final) == trt_order[3]] <- "col3"
# if more than 3, keep as is
# Ensure col1-3 exist
if (!"col1" %in% names(final)) final$col1 <- NA_character_
if (!"col2" %in% names(final)) final$col2 <- NA_character_
if (!"col3" %in% names(final)) final$col3 <- NA_character_

# Replace NA with "" for display (SAS blank)
final$col1[is.na(final$col1)] <- ""
final$col2[is.na(final$col2)] <- ""
final$col3[is.na(final$col3)] <- ""

# Final sort as SAS: by descending atc_ord CMCLAS rowtype descending pref_ord display_term
final <- final %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term)

# Write CSV dataset
write.csv(final, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# Prepare text rendering
# Titles from u_titles.sas for T_CM
title1 <- "[Sponsor Name]                                                  Page 1 of 1"
title2 <- "Protocol: [Protocol Number]"
title3 <- ""
title4 <- "Table 14.2.1"
title5 <- "Summary of Concomitant Medications by ATC Class and Preferred Name"
title6 <- "Safety Analysis Set"

# Footnotes
foot1 <- "Data cutoff: [DDMONYYYY]"
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: P070_t_cm.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
foot4 <- "WHO Drug Dictionary [Version]"
foot5 <- "Subjects counted once per ATC class and once per preferred name within ATC class."

# Build text table
# Header row
hdr_line1 <- sprintf("%-50s | %-20s | %-20s | %-20s", "ATC Class", col1hdr, col2hdr, col3hdr)
hdr_line2 <- sprintf("%-50s | %-20s | %-20s | %-20s", "  Preferred Name", "", "", "")
sep <- paste(rep("-", 120), collapse = "")

# Body rows
body_lines <- c()
for (i in seq_len(nrow(final))) {
  r <- final[i, ]
  # display_term already has leading spaces for pref rows
  dt <- r$display_term
  # bold for rowtype 1 is not representable in plain text; we keep as is
  line <- sprintf("%-50s | %-20s | %-20s | %-20s", dt, r$col1, r$col2, r$col3)
  body_lines <- c(body_lines, line)
}

txt_content <- c(
  title1,
  title2,
  title3,
  title4,
  title5,
  title6,
  "",
  hdr_line1,
  hdr_line2,
  sep,
  body_lines,
  sep,
  "",
  foot1,
  foot2,
  foot3,
  foot4,
  foot5
)

writeLines(txt_content, file.path(out, "t_cm.txt"))
# also write rtf-like copy (plain text with .rtf extension for expected output)
writeLines(txt_content, file.path(out, "t_cm.rtf"))

# Also write plotted data CSV for figure compliance (no figure here, but write final data)
# Already wrote t_cm.csv

# Log progress
cat("P070 t_cm completed\n")
cat(paste0("Big N: ", paste(bign_df$TRTA, bign_df$bign, collapse=", "), "\n"))
cat(paste0("Total N: ", bign_tot, "\n"))
cat(paste0("Rows in final: ", nrow(final), "\n"))
