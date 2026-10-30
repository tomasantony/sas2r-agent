# P068_t_ae_soc_pt.R
# Translation of P068_t_ae_soc_pt.sas - Table 14.3.2 TEAEs by SOC and PT
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (half up)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: format pct like SAS put(pct, 5.1) -> one decimal, SAS rounding
fmt_pct <- function(pct) {
  # SAS put with 5.1 uses round half away from zero
  r <- sas_round(pct, 1)
  # SAS put 5.1: width 5, 1 decimal, right aligned then stripped
  # We need strip(put(pct,5.1)) -> removes leading blanks
  # So just format with 1 decimal
  sprintf("%.1f", r)
}

# Locate adam library
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

find_dataset <- function(libpath, name) {
  # try .sas7bdat, .xpt, .sas7bdat lowercase
  candidates <- c(
    file.path(libpath, paste0(name, ".sas7bdat")),
    file.path(libpath, paste0(tolower(name), ".sas7bdat")),
    file.path(libpath, paste0(name, ".xpt")),
    file.path(libpath, paste0(tolower(name), ".xpt")),
    file.path(libpath, paste0(name, ".sas7bdat".toLower <- tolower(name)))
  )
  # also list files
  if (dir.exists(libpath)) {
    files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
    # case-insensitive match
    idx <- which(tolower(basename(tools::file_path_sans_ext(files))) == tolower(name))
    if (length(idx) > 0) candidates <- c(files[idx], candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) return(f)
  }
  return(NA)
}

read_adam <- function(name) {
  f <- find_dataset(adam_path, name)
  if (is.na(f)) {
    # try alternative adam path from raw/cdisc-pilot
    alt <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
    f2 <- find_dataset(alt, name)
    if (!is.na(f2)) f <- f2
  }
  if (is.na(f) || !file.exists(f)) {
    stop(paste("Dataset not found:", name, "searched in", adam_path))
  }
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    stop(paste("Unknown extension:", f))
  }
  # Convert haven labelled to plain, keep names as is
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven may keep labelled; convert to character/numeric
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      # keep labels for TRTA etc but convert to character if character
      df[[nm]] <- haven::as_factor(df[[nm]], levels = "labels")
      # Actually we want original values; if labelled numeric, keep numeric
      # For TRTA which is character, as_factor will give label; better use zap_labels
      # Let's try zap
    }
  }
  # Re-read with zap if needed: use haven::zap_labels
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  # Ensure character columns are character
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

# Read ADSL and ADAE
adsl <- read_adam("adsl")
adae <- read_adam("adae")

# Ensure column names uppercase for matching SAS (haven may preserve case)
names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

# Step 1: Get Big N (SAFFL='Y')
# SAS: where upcase(SAFFL) = "Y"
adsl_saffl <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

# TRTA values: need to check distinct
# SAS orders by TRTA (character ordering, uppercase before lowercase)
# Let's get Big N per TRTA
library(dplyr)

bign_df <- adsl_saffl %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# Ensure TRTA ordering matches SAS: character ordering
# SAS sorts with uppercase before lowercase; R default is similar for ASCII
# bign_df already arranged by TRTA

# Total N
bign_tot <- n_distinct(adsl_saffl$USUBJID)

# Build header strings like SAS macro
# col1hdr etc: "Placebo (N=xx)" etc
# Need to map TRTA to labels; SAS uses TRTA values directly as labels? In m_counts it does call symputx trtlbl = TRTA
# So headers are TRTA values
# But we need to know order: TRTA sorted ascending
# Let's capture
trt_levels <- sort(unique(bign_df$TRTA))  # SAS order by TRTA
# Ensure bign_df is in that order
bign_df <- bign_df %>% arrange(match(TRTA, trt_levels))

# Create named vector bign by TRTA
bign_vec <- setNames(bign_df$bign, bign_df$TRTA)

# For display headers
col_hdrs <- sapply(trt_levels, function(trt) {
  paste0(trt, " (N=", bign_vec[trt], ")")
})
# Total header
col_tot_hdr <- paste0("Total (N=", bign_tot, ")")

# Step 2: Subset to TEAE in safety population
# SAS: inner join adae with adsl on USUBJID where b.SAFFL='Y' and a.TRTEMFL='Y'
# Note: SAS uses case-sensitive 'Y' for SAFFL and TRTEMFL
# Use adsl_saffl USUBJIDs
saffl_ids <- unique(adsl_saffl$USUBJID)
ae <- adae %>%
  filter(USUBJID %in% saffl_ids, TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD, AESOC, AESOCCD)

# Ensure TRTA in ae is from ADAE (should match ADSL TRTA, but SAS uses a.TRTA)
# Keep as is

# Step 3: Count subjects by SOC
soc_cnt <- ae %>%
  group_by(TRTA, AEBODSYS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

soc_tot <- ae %>%
  group_by(AEBODSYS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 4: Count subjects by SOC + PT
pt_cnt <- ae %>%
  group_by(TRTA, AEBODSYS, AEDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pt_tot <- ae %>%
  group_by(AEBODSYS, AEDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 5: Any TEAE row
any_ae <- ae %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(AEBODSYS = "Any TEAE",
         AEDECOD = "",
         soc_ord = 0,
         pt_ord = 0,
         rowtype = 0)

# Step 6: Build display rows
# SOC rows with sort order
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(AEDECOD = "",
         soc_ord = cnt_tot,
         pt_ord = 0,
         rowtype = 1) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# PT rows with sort order
pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS", "AEDECOD")) %>%
  rename(pt_cnt_tot = cnt_tot) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  rename(soc_cnt_tot = cnt_tot) %>%
  mutate(soc_ord = soc_cnt_tot,
         pt_ord = pt_cnt_tot,
         rowtype = 2) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# Step 7: Combine and calculate percentages
combined <- bind_rows(any_ae, soc_rows, pt_rows)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

# Order as SAS: order by soc_ord desc, AEBODSYS, rowtype, pt_ord desc, AEDECOD, TRTA
combined2 <- combined2 %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA)

combined3 <- combined2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(trimws(as.character(cnt)), " (", fmt_pct(pct), ")"),
                        "0"))

# Step 8: Transpose
# SAS: proc sort nodupkey by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD TRTA
# Then proc transpose by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD; id TRTA; var value
# Need to ensure unique keys

# Sort for transpose
combined3_sorted <- combined3 %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA)

# Transpose using tidyr
# Create wide
final_wide <- combined3_sorted %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value)

# The SAS transpose names columns colPlacebo etc; we need to map to col1 col2 col3 in TRTA order
# trt_levels is sorted TRTA order; col1 corresponds to first TRTA, etc.
# Rename columns to col1, col2, col3 for report
# Ensure all trt_levels present as columns
for (i in seq_along(trt_levels)) {
  trt <- trt_levels[i]
  colname <- paste0("col", i)
  if (trt %in% names(final_wide)) {
    final_wide[[colname]] <- final_wide[[trt]]
  } else {
    final_wide[[colname]] <- NA_character_
  }
}
# Keep only needed columns
final <- final_wide %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, starts_with("col"))

# Replace missing with "0" later, but also ensure ordering
# SAS final order is by descending soc_ord, AEBODSYS, rowtype, descending pt_ord, AEDECOD
# Already in that order, but need to ensure distinct
final <- final %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)

# Step 9: Create display variable
report <- final %>%
  mutate(display_term = case_when(
    rowtype == 0 ~ AEBODSYS,
    rowtype == 1 ~ AEBODSYS,
    TRUE ~ paste0("  ", trimws(AEDECOD))
  ))

# Replace missing with "0"
col_cols <- grep("^col\\d+$", names(report), value = TRUE)
for (cc in col_cols) {
  report[[cc]][is.na(report[[cc]]) | report[[cc]] == ""] <- "0"
}

# Ensure report is sorted correctly for display
report <- report %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)

# For output, we need to create CSV and TXT rendering
# The table should have columns: System Organ Class / Preferred Term, and treatment columns
# Prepare display dataframe for CSV
# Headers: col_hdrs in order
# Create output table body
out_table <- report %>%
  select(display_term, all_of(paste0("col", seq_along(trt_levels))))

# Rename columns to headers for CSV
header_names <- c("System Organ Class / Preferred Term", col_hdrs)
names(out_table) <- header_names

# Write CSV of table body
write.csv(out_table, file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")

# Also write datasets as CSV per spec: each output dataset as CSV
# The main dataset is report; also write bign etc for traceability? But required is t_ae_soc_pt.csv
# Write additional dataset for verification
write.csv(report, file.path(out, "t_ae_soc_pt_dataset.csv"), row.names = FALSE, na = "")

# Create text rendering (like RTF plain text)
# Titles and footnotes as per u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

# Build text file
txt_path <- file.path(out, "t_ae_soc_pt.txt")
con <- file(txt_path, open = "wt")

writeLines(paste0(sponsor, strrep(" ", 60), "Page 1 of 1"), con)
writeLines(paste0("Protocol: ", protocol), con)
writeLines("", con)
writeLines("Table 14.3.2", con)
writeLines("Treatment-Emergent Adverse Events by System Organ Class and Preferred Term", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)
# Header row
# Format: display_term left 50 chars, each col centered 20 chars
# Use simple tab-separated for readability but also formatted
hdr_line <- sprintf("%-50s", header_names[1])
for (i in 2:length(header_names)) {
  hdr_line <- paste0(hdr_line, sprintf(" | %-20s", header_names[i]))
}
writeLines(hdr_line, con)
writeLines(paste0(strrep("-", 50), "-+-", paste(rep(strrep("-", 20), length(trt_levels)), collapse = "-+-")), con)

# Write rows with blank line between SOCs (compute after soc_ord)
# Need to detect SOC changes
prev_soc_ord <- NA
prev_aebodsys <- NA
for (r in seq_len(nrow(report))) {
  row <- report[r, ]
  # display_term with bold indication for SOC rows: we can add * for bold
  disp <- row$display_term
  # For SOC rows (rowtype 1), mark bold with uppercase? Keep as is
  line <- sprintf("%-50s", substr(disp, 1, 50))
  for (i in seq_along(trt_levels)) {
    colval <- row[[paste0("col", i)]]
    line <- paste0(line, sprintf(" | %-20s", colval))
  }
  writeLines(line, con)
  # Blank line between SOCs: after each SOC group (when next row has different soc_ord/AEBODSYS)
  if (r < nrow(report)) {
    next_row <- report[r+1, ]
    # SAS: compute after soc_ord -> blank line after each soc_ord group
    if (row$soc_ord != next_row$soc_ord || row$AEBODSYS != next_row$AEBODSYS) {
      # But not after Any TEAE? Any TEAE has soc_ord 0, its own group
      # SAS does blank line after every soc_ord, including Any TEAE
      writeLines("", con)
    }
  }
}
writeLines("", con)
writeLines(paste0("MedDRA Version [XX.X]"), con)
writeLines("Subjects counted once per SOC and once per PT within each SOC.", con)
writeLines("SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC.", con)
writeLines("", con)
writeLines(paste0("Data cutoff: ", cutdate, "."), con)
writeLines("Source: ADaM datasets", con)
writeLines(paste0("Program: t_ae_soc_pt.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M")), con)

close(con)

# Also create RTF-like text file with .rtf extension as expected output (plain text RTF)
# Copy txt to rtf for compatibility
file.copy(txt_path, file.path(out, "t_ae_soc_pt.rtf"), overwrite = TRUE)

# Verify 107 numbers: count numeric patterns in table
# For logging
cat("P068 t_ae_soc_pt completed\n")
cat("Big N per TRTA:\n")
print(bign_df)
cat("Total N:", bign_tot, "\n")
cat("Report rows:", nrow(report), "\n")
cat("Output written to:", out, "\n")
