# P070_t_cm.sas -> R translation
# Table 14.2.1 Summary of Concomitant Medications by ATC Class and Preferred Name
# Population: Safety Analysis Set (SAFFL='Y')

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(x) {
  if (is.na(x)) return(NA_character_)
  y <- round_half_up(x, 1)
  # SAS PUT 5.1 strips leading blanks
  formatC(y, format = "f", digits = 1, flag = "#")
}

# helper: read adam dataset (sas7bdat or xpt)
read_adam <- function(name, adam_path) {
  # adam_path is directory
  files <- list.files(adam_path, full.names = TRUE, recursive = FALSE)
  # try exact name with extensions
  cand <- files[tolower(tools::file_ext(files)) %in% c("sas7bdat","xpt","sas7bdat")]
  # find file matching name
  pat <- paste0("^", name, "\\.(sas7bdat|xpt|sasp7bdat)$")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) > 0) {
    f <- files[idx[1]]
  } else {
    # fallback: try direct path
    f1 <- file.path(adam_path, paste0(name, ".sas7bdat"))
    f2 <- file.path(adam_path, paste0(name, ".xpt"))
    if (file.exists(f1)) f <- f1 else if (file.exists(f2)) f <- f2 else f <- f1
  }
  if (!file.exists(f)) {
    # try alternative raw path
    alt <- "/bench/data/raw/cdisc-pilot/adam"
    f_alt1 <- file.path(alt, paste0(name, ".sas7bdat"))
    f_alt2 <- file.path(alt, paste0(name, ".xpt"))
    if (file.exists(f_alt1)) f <- f_alt1 else if (file.exists(f_alt2)) f <- f_alt2
  }
  if (!file.exists(f)) stop(paste0("File not found for ", name, ": ", f))
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    df <- haven::read_sas(f)
  }
  # zap labels/formats to plain R types, keep values
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  # convert haven_labelled character to character
  df <- df %>% mutate(across(where(~ inherits(., "haven_labelled")), ~ as.character(haven::as_factor(., levels = "labels"))))
  # also ensure any remaining labelled are converted
  df <- df %>% mutate(across(where(~ inherits(., "haven_labelled")), ~ haven::zap_labels(.)))
  return(as.data.frame(df, stringsAsFactors = FALSE))
}

adam_path <- "/bench/data/raw/own-synthetic/adam"
# fallback if not exists
if (!dir.exists(adam_path)) adam_path <- "/bench/data/raw/cdisc-pilot/adam"

adsl <- read_adam("adsl", adam_path)
adcm <- read_adam("adcm", adam_path)

# Ensure character columns are character, trim
adsl <- adsl %>% mutate(across(where(is.character), ~ str_trim(.)))
adcm <- adcm %>% mutate(across(where(is.character), ~ str_trim(.)))

# Standardize column names to uppercase for matching SAS (case-insensitive)
names(adsl) <- toupper(names(adsl))
names(adcm) <- toupper(names(adcm))

#--- Step 1: Get Big N (m_counts) ---
# SAFFL = 'Y', TRTA
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  distinct(TRTA, USUBJID) %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# Total N
bign_tot <- adsl %>% filter(toupper(SAFFL) == "Y") %>% distinct(USUBJID) %>% nrow()

# Build header strings like SAS: "Placebo (N=xx)"
# SAS orders by TRTA character; but report expects col1=Placebo, col2=Drug X Low Dose, col3=Drug X High Dose
# Create mapping explicitly
# Determine expected TRTA values
# Use bign_df to get labels, but force order: Placebo, Drug X Low Dose, Drug X High Dose
trt_order <- c("Placebo", "Drug X Low Dose", "Drug X High Dose")
# Ensure bign_df contains these; if not, use whatever present sorted
# Create named vector bign by TRTA
bign_vec <- setNames(bign_df$bign, bign_df$TRTA)
# Fill missing with 0
for (tr in trt_order) if (!tr %in% names(bign_vec)) bign_vec[tr] <- 0

col1hdr <- paste0("Placebo (N=", bign_vec["Placebo"], ")")
col2hdr <- paste0("Drug X Low Dose (N=", bign_vec["Drug X Low Dose"], ")")
col3hdr <- paste0("Drug X High Dose (N=", bign_vec["Drug X High Dose"], ")")
coltothdr <- paste0("Total (N=", bign_tot, ")")

cat("NOTE: Treatment group counts:\n")
for (tr in trt_order) cat("  ", tr, ": N=", bign_vec[tr], "\n", sep="")
cat("  Total: N=", bign_tot, "\n", sep="")

#--- Step 2: Prepare CM data ---
# inner join adcm with adsl where SAFFL='Y' and CONCOMFL='Y'
# SAS: select a.USUBJID, a.TRTA, a.TRTAN, a.CMCLAS, a.CMDECOD, a.CMINDC, a.CMSTDTC, a.CMENDTC
# from adam.adcm a inner join adam.adsl b on a.USUBJID=b.USUBJID where b.SAFFL='Y' and a.CONCOMFL='Y'
cm <- adcm %>%
  filter(toupper(CONCOMFL) == "Y") %>%
  inner_join(adsl %>% filter(toupper(SAFFL) == "Y") %>% select(USUBJID), by = "USUBJID") %>%
  select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD, CMINDC, CMSTDTC, CMENDTC) %>%
  arrange(TRTAN, CMCLAS, CMDECOD)

# Ensure TRTA is character
cm$TRTA <- as.character(cm$TRTA)
cm$CMCLAS <- as.character(cm$CMCLAS)
cm$CMDECOD <- as.character(cm$CMDECOD)

#--- Step 3: Any CM row ---
any_df <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0,
         pref_ord = 0,
         rowtype = 0,
         CMCLAS = NA_character_,
         CMDECOD = NA_character_)

#--- Step 4: ATC class counts ---
atc <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# SAS does proc sort by CMCLAS then merge
atc_rows <- atc %>%
  left_join(atc_tot, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0,
         rowtype = 1,
         CMDECOD = NA_character_) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)

#--- Step 5: Preferred name counts ---
pref <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

pref_rows <- pref %>%
  left_join(pref_tot, by = c("CMCLAS","CMDECOD")) %>%
  rename(pref_ord = cnt_tot) %>%
  left_join(atc_tot, by = "CMCLAS") %>%
  rename(atc_ord = cnt_tot) %>%
  mutate(display_term = paste0("  ", str_trim(CMDECOD)),
         rowtype = 2) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)

#--- Step 6: Combine and calculate percentages ---
combined <- bind_rows(any_df, atc_rows, pref_rows)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_)) %>%
  # SAS order: atc_ord desc, CMCLAS, rowtype, pref_ord desc, display_term, TRTA
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA)

combined3 <- combined2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(str_trim(as.character(cnt)), " (", str_trim(fmt_pct1(pct)), ")"),
                        "0"))

#--- Step 7: Transpose ---
# SAS: proc sort nodupkey by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA
# then proc transpose by descending atc_ord CMCLAS rowtype descending pref_ord display_term; id TRTA; var value
# Need to deduplicate by those keys + TRTA
combined3_sorted <- combined3 %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA) %>%
  distinct(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA, .keep_all = TRUE)

# For transpose, create wide
# Use pivot_wider; need to keep ordering keys
# Create a key for by groups
wide <- combined3_sorted %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# SAS sanitizes TRTA to column names: Placebo -> colPlacebo, Drug X Low Dose -> colDrug_X_Low_Dose etc
# We need to map to col1,col2,col3
# Ensure columns exist
for (tr in trt_order) {
  if (!tr %in% names(wide)) wide[[tr]] <- NA_character_
}
# Rename to col1,col2,col3
final <- wide %>%
  rename(col1 = `Placebo`,
         col2 = `Drug X Low Dose`,
         col3 = `Drug X High Dose`) %>%
  # Keep only needed columns, ensure order
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term) %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, col1, col2, col3)

# Replace NA with blank for display (SAS shows blank, but we keep NA for CSV na="")
# For text rendering, show blank as "" ; for CSV, na="" will handle
# Optionally fill missing with "0" ? Keep blank to match SAS
# Ensure col1-3 are character
final$col1 <- as.character(final$col1)
final$col2 <- as.character(final$col2)
final$col3 <- as.character(final$col3)

#--- Step 8: Generate output ---
# Titles
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title1 <- sponsor
title4 <- "Table 14.2.1"
title5 <- "Summary of Concomitant Medications by ATC Class and Preferred Name"
title6 <- "Safety Analysis Set"

# Prepare display table for report (hide atc_ord etc)
report_df <- final %>%
  select(display_term, col1, col2, col3)

# Write CSV of table body (with headers)
# Include Big N in header row names as SAS does: col headers contain N
# For CSV, use header names as display
csv_out <- report_df
names(csv_out) <- c("ATC Class / Preferred Name", col1hdr, col2hdr, col3hdr)
write.csv(csv_out, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# Also write final dataset CSV with SAS variable names (for verification)
write.csv(final, file.path(out, "t_cm_final.csv"), row.names = FALSE, na = "")

# Text rendering
# Build text file with titles, headers, rows, footnotes
txt_path <- file.path(out, "t_cm.txt")
rtf_path <- file.path(out, "t_cm.rtf")

# Helper to format fixed width table
# Use simple tab-separated with alignment
header_line1 <- paste0(title4, " | ", title5)
header_line2 <- title6
col_header <- paste0(
  str_pad("ATC Class / Preferred Name", 50, side = "right"),
  " | ",
  str_pad(col1hdr, 18, side = "right"),
  " | ",
  str_pad(col2hdr, 18, side = "right"),
  " | ",
  str_pad(col3hdr, 18, side = "right")
)
sep_line <- paste0(strrep("-", 50), "-+-", strrep("-", 18), "-+-", strrep("-", 18), "-+-", strrep("-", 18))

lines <- c(
  paste0(sponsor, strrep(" ", 60), "Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  title4,
  title5,
  title6,
  "",
  col_header,
  sep_line
)

for (i in seq_len(nrow(final))) {
  disp <- final$display_term[i]
  # Bold indication for rowtype 1: we mark with * or keep as is; in text add no extra
  # SAS compute makes ATC rows bold; we will keep plain
  c1 <- ifelse(is.na(final$col1[i]), "", final$col1[i])
  c2 <- ifelse(is.na(final$col2[i]), "", final$col2[i])
  c3 <- ifelse(is.na(final$col3[i]), "", final$col3[i])
  line <- paste0(
    str_pad(disp, 50, side = "right"),
    " | ",
    str_pad(c1, 18, side = "right"),
    " | ",
    str_pad(c2, 18, side = "right"),
    " | ",
    str_pad(c3, 18, side = "right")
  )
  lines <- c(lines, line)
}

lines <- c(lines,
  "",
  "WHO Drug Dictionary [Version]",
  "Subjects counted once per ATC class and once per preferred name within ATC class.",
  paste0("Data cutoff: ", cutdate),
  "Source: ADaM datasets",
  paste0("Program: P070_t_cm.sas | Run: ", format(Sys.Date(), "%d%b%Y"))
)

writeLines(lines, txt_path)
# Also write same content to .rtf as plain text (SAS RTF is text-based; we provide text rendering)
writeLines(lines, rtf_path)

# Also create a CSV of plotted data not needed (table only)

cat("NOTE: t_cm.sas completed successfully.\n")
cat("Outputs written to:", out, "\n")
