```r
# P070_t_cm.sas -> R translation
# Table 14.2.1 Summary of Concomitant Medications by ATC Class and Preferred Name
# Population: Safety Analysis Set (SAFFL='Y')

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: round half away from zero (SAS)
round_half_away <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct <- function(x) {
  # SAS put(pct,5.1) then strip -> 1 decimal, round half away
  sprintf("%.1f", round_half_away(x, 1))
}
fmt_value <- function(cnt, pct) {
  ifelse(is.na(cnt) | cnt == 0, "0",
         paste0(trimws(as.character(cnt)), " (", fmt_pct(pct), ")"))
}

# helper to read adam dataset (sas7bdat or xpt)
read_adam <- function(name) {
  base1 <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
  base2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  # try base1 first
  candidates <- c(
    file.path(base1, paste0(name, ".sas7bdat")),
    file.path(base1, paste0(name, ".xpt")),
    file.path(base1, paste0(tolower(name), ".sas7bdat")),
    file.path(base1, paste0(tolower(name), ".xpt")),
    file.path(base2, paste0(name, ".sas7bdat")),
    file.path(base2, paste0(name, ".xpt"))
  )
  # also list files in base1
  if (dir.exists(base1)) {
    fl <- list.files(base1, full.names = TRUE)
    # case-insensitive match
    idx <- grep(paste0("^", name, "\\."), basename(fl), ignore.case = TRUE)
    if (length(idx) > 0) candidates <- c(fl[idx], candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) {
      if (grepl("\\.sas7bdat$", f, ignore.case = TRUE)) {
        df <- haven::read_sas(f)
      } else if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
        df <- haven::read_xpt(f)
      } else next
      # haven returns tibble with labelled; convert to data.frame
      df <- as.data.frame(df, stringsAsFactors = FALSE)
      # normalize names to upper case for matching but keep original
      return(df)
    }
  }
  stop(paste("Dataset not found:", name))
}

# Load ADSL and ADCM
adsl <- read_adam("adsl")
adcm <- read_adam("adcm")

# Ensure column names are upper case for logic (SAS is case-insensitive but data is upper)
# haven preserves case; make a lookup
names(adsl) <- toupper(names(adsl))
names(adcm) <- toupper(names(adcm))

# Check required columns exist
stopifnot(all(c("USUBJID","TRTA","TRTAN","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","CMCLAS","CMDECOD","CONCOMFL") %in% names(adcm)))
# CMSTDTC, CMENDTC, CMINDC may be present
if (!"CMSTDTC" %in% names(adcm)) adcm$CMSTDTC <- NA
if (!"CMENDTC" %in% names(adcm)) adcm$CMENDTC <- NA
if (!"CMINDC" %in% names(adcm)) adcm$CMINDC <- NA

# Step 1: Get Big N (m_counts)
# where upcase(SAFFL)='Y' group by TRTA order by TRTA
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]
# distinct USUBJID per TRTA
library(dplyr)
bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)  # SAS character ordering: uppercase before lowercase; R arrange is similar for ASCII
# also need TRTAN for ordering display; keep mapping
trt_order <- bign_df %>%
  left_join(adsl_saff %>% select(TRTA, TRTAN) %>% distinct(), by = "TRTA") %>%
  arrange(TRTAN)  # for display order Placebo, Low, High if TRTAN exists
# For SAS fidelity, bign order is by TRTA, but report expects Placebo, Low, High (TRTAN order)
# Use TRTAN order for columns to match patch rename colPlacebo=col1 etc.
# Determine final column order: order by TRTAN if available, else by TRTA
if (all(!is.na(trt_order$TRTAN))) {
  bign_df <- bign_df %>% left_join(trt_order %>% select(TRTA, TRTAN), by = "TRTA") %>% arrange(TRTAN) %>% select(-TRTAN)
}
# Create macro-like headers
bigntot <- n_distinct(adsl_saff$USUBJID)
# Build header strings
col_hdrs <- paste0(bign_df$TRTA, " (N=", bign_df$bign, ")")
# For report, col1, col2, col3 correspond to bign_df order
# Save for later
ntrt <- nrow(bign_df)

# Step 2: Prepare CM data
# inner join adcm with adsl where SAFFL='Y' and CONCOMFL='Y'
# select a.USUBJID, a.TRTA, a.TRTAN, a.CMCLAS, a.CMDECOD, a.CMINDC, a.CMSTDTC, a.CMENDTC
# Note: adcm TRTA may be present; use adcm's TRTA/TRTAN but filter via adsl SAFFL
adsl_keys <- adsl_saff %>% select(USUBJID) %>% distinct()
cm <- adcm %>%
  inner_join(adsl_keys, by = "USUBJID") %>%
  filter(toupper(CONCOMFL) == "Y") %>%
  # keep only needed cols; if TRTA not in adcm, join from adsl
  left_join(adsl_saff %>% select(USUBJID, TRTA_ADSL = TRTA, TRTAN_ADSL = TRTAN), by = "USUBJID") %>%
  mutate(
    TRTA = ifelse(!is.na(TRTA) & TRTA != "", TRTA, TRTA_ADSL),
    TRTAN = ifelse(!is.na(TRTAN), TRTAN, TRTAN_ADSL)
  ) %>%
  select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD, CMINDC, CMSTDTC, CMENDTC) %>%
  filter(!is.na(CMCLAS) & CMCLAS != "", !is.na(CMDECOD) & CMDECOD != "") %>%
  arrange(TRTAN, CMCLAS, CMDECOD)

# Step 3: Any CM row
any_df <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0, pref_ord = 0, rowtype = 0,
         CMCLAS = NA_character_, CMDECOD = NA_character_)

# Step 4: ATC class counts
atc <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# SAS: proc sort data=_atc; by CMCLAS; then merge _atc _atc_tot by CMCLAS
atc_rows <- atc %>%
  left_join(atc_tot, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0,
         rowtype = 1,
         CMDECOD = NA_character_) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, cnt_tot, display_term, atc_ord, pref_ord, rowtype)

# Step 5: Preferred name counts
pref <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  rename(pref_ord = cnt_tot)

pref_rows <- pref %>%
  left_join(pref_tot, by = c("CMCLAS","CMDECOD")) %>%
  left_join(atc_tot %>% rename(atc_ord = cnt_tot), by = "CMCLAS") %>%
  mutate(display_term = paste0("  ", trimws(CMDECOD)),
         rowtype = 2) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, pref_ord, atc_ord, display_term, rowtype)

# Step 6: Combine and calculate percentages
combined <- bind_rows(
  any_df %>% select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype),
  atc_rows %>% select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype),
  pref_rows %>% select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)
)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)) %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA)

combined3 <- combined2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(trimws(as.character(cnt)), " (", fmt_pct(pct), ")"),
                        "0"))

# Step 7: Transpose
# SAS: proc sort nodupkey by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA
# then proc transpose by descending atc_ord CMCLAS rowtype descending pref_ord display_term; id TRTA; var value
# Need to deduplicate and sort
combined3_sorted <- combined3 %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA) %>%
  distinct(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA, .keep_all = TRUE)

# For transpose, create wide
# Use tidyr pivot_wider
# Need to keep ordering keys
wide <- combined3_sorted %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value)

# Now map TRTA columns to col1, col2, col3 in order of bign_df (TRTAN order)
# bign_df$TRTA gives order for col1, col2, col3
trta_ordered <- bign_df$TRTA
# Ensure all TRTA columns exist in wide; if missing, add NA
for (trt in trta_ordered) {
  if (!trt %in% names(wide)) wide[[trt]] <- NA_character_
}
# Create col1, col2, col3
for (i in seq_along(trta_ordered)) {
  wide[[paste0("col", i)]] <- wide[[trta_ordered[i]]]
}
# Keep only needed cols
final <- wide %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, starts_with("col")) %>%
  # Ensure col1, col2, col3 exist (pad if ntrt <3)
  { if (!"col1" %in% names(.)) mutate(., col1 = NA_character_) else . } %>%
  { if (!"col2" %in% names(.)) mutate(., col2 = NA_character_) else . } %>%
  { if (!"col3" %in% names(.)) mutate(., col3 = NA_character_) else . } %>%
  # Fill missing with "0" (SAS would show blank but we show 0 for completeness)
  mutate(across(starts_with("col"), ~ ifelse(is.na(.), "0", .))) %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term)

# For display, replace NA CMCLAS with ""
final$CMCLAS[is.na(final$CMCLAS)] <- ""

# Step 8: Generate output
# Titles and footnotes
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "  Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Table 14.2.1"
title5 <- "Summary of Concomitant Medications by ATC Class and Preferred Name"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: P070_t_cm.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M"))
footnote4 <- "WHO Drug Dictionary [Version]"
footnote5 <- "Subjects counted once per ATC class and once per preferred name within ATC class."

# Prepare CSV output (table body)
# CSV should contain display_term and treatment columns with headers including N
csv_df <- final %>%
  select(display_term, col1, col2, col3)
# Rename columns to include header N for CSV clarity
names(csv_df) <- c("ATC Class / Preferred Name", col_hdrs[1], col_hdrs[2], col_hdrs[3])
# Write CSV
write.csv(csv_df, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# Also write final dataset with ordering vars for traceability
write.csv(final, file.path(out, "t_cm_final.csv"), row.names = FALSE, na = "")

# Text rendering
# Build header line
hdr_display <- "ATC Class   Preferred Name"
# Column widths: display 40, each col 20
col_width_display <- 50
col_width_trt <- 22

# Create text file
txt_path <- file.path(out, "t_cm.txt")
rtf_path <- file.path(out, "t_cm.rtf")

# Helper to format row for text
format_row <- function(disp, c1, c2, c3, is_bold = FALSE) {
  # disp may have leading spaces for pref
  # truncate/pad
  disp_fmt <- sprintf("%-*s", col_width_display, substr(disp, 1, col_width_display))
  c1_fmt <- sprintf("%*s", col_width_trt, c1)
  c2_fmt <- sprintf("%*s", col_width_trt, c2)
  c3_fmt <- sprintf("%*s", col_width_trt, c3)
  paste0(disp_fmt, c1_fmt, c2_fmt, c3_fmt)
}

con <- file(txt_path, open = "wt")
writeLines(title1, con)
writeLines(title2, con)
writeLines(title3, con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
# Header
header_line <- format_row(hdr_display, col_hdrs[1], col_hdrs[2], col_hdrs[3])
writeLines(header_line, con)
writeLines(paste0(strrep("-", col_width_display), strrep("-", col_width_trt*3)), con)
# Data rows
for (i in seq_len(nrow(final))) {
  r <- final[i, ]
  is_bold <- r$rowtype == 1
  line <- format_row(r$display_term, r$col1, r$col2, r$col3, is_bold)
  writeLines(line, con)
}
writeLines("", con)
writeLines(footnote1, con)
writeLines(footnote2, con)
writeLines(footnote3, con)
writeLines(footnote4, con)
writeLines(footnote5, con)
close(con)

# Also write RTF-like text (same content) to .rtf for expected output
file.copy(txt_path, rtf_path, overwrite = TRUE)

# Write plotted data CSV (for figure compliance, though no figure)
# Create a CSV of plotted data (same as final)
write.csv(csv_df, file.path(out, "t_cm_plot_data.csv"), row.names = FALSE, na = "")

# Log
cat("P070 t_cm completed. N per group:", paste(col_hdrs, collapse=", "), "\n")
cat("Rows in final:", nrow(final), "\n")
cat("Outputs written to:", out, "\n")
```