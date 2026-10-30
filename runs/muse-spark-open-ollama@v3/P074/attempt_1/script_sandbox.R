# P074 t_lb_shift translation
# Reads synthetic ADaM and reproduces SAS shift table logic

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: read ADaM dataset from directory (sas7bdat or xpt)
read_adam <- function(libpath, dsname) {
  dsname_low <- tolower(dsname)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # find file matching dsname
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == dsname_low)
  if (length(idx) == 0) {
    # try case-insensitive contains
    idx <- grep(paste0("^", dsname_low, "\\."), tolower(basename(files)))
  }
  if (length(idx) == 0) stop(paste0("Dataset not found: ", dsname, " in ", libpath))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "xpt" || grepl("\\.xpt$", tolower(f))) {
    df <- haven::read_xpt(f)
  } else {
    # try both
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <<- haven::read_xpt(f))
  }
  # zap labels/formats to plain vectors, keep character as character
  df <- haven::zap_formats(df)
  df <- haven::zap_label(df)
  # convert labelled to appropriate type
  df <- df %>% mutate(across(where(haven::is.labelled), haven::zap_labels))
  # ensure character columns are character (not factor)
  df <- df %>% mutate(across(where(is.factor), as.character))
  names(df) <- toupper(names(df))
  df
}

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

# fallback to derived path if synthetic not present
if (!dir.exists(adam_path)) adam_path <- "/bench/data/raw/cdisc-pilot/adam"
if (!dir.exists(sdtm_path)) sdtm_path <- "/bench/data/raw/cdisc-pilot/sdtm"

adsl <- read_adam(adam_path, "adsl")
adlb <- read_adam(adam_path, "adlb")

# Ensure expected columns exist
# Standardize character trimming
trim_chr <- function(x) if (is.character(x)) str_trim(x) else x
adsl <- adsl %>% mutate(across(where(is.character), ~str_trim(.)))
adlb <- adlb %>% mutate(across(where(is.character), ~str_trim(.)))

# Coerce TRTAN, AVISITN to numeric if needed
if ("TRTAN" %in% names(adsl)) adsl$TRTAN <- as.numeric(adsl$TRTAN)
if ("TRTAN" %in% names(adlb)) adlb$TRTAN <- as.numeric(adlb$TRTAN)
if ("AVISITN" %in% names(adlb)) adlb$AVISITN <- as.numeric(adlb$AVISITN)

# Treatment labels from u_setup
trt_labels <- c("Placebo", "[Drug] Low Dose", "[Drug] High Dose")

# Step 1: Get Big N (m_counts)
# SAFFL = Y (case-insensitive)
adsl_saffl <- adsl %>% filter(toupper(SAFFL) == "Y")
bign_df <- adsl_saffl %>%
  group_by(TRTA, TRTAN) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN)

# Total N
bign_tot <- n_distinct(adsl_saffl$USUBJID)

# Ensure ntrt = 3 as per macro
ntrt <- 3
# If bign_df has fewer than 3, pad (should not happen)
# Build header strings (for logging)
for (i in seq_len(nrow(bign_df))) {
  assign(paste0("bign", i), bign_df$BIGN[i])
}

# Step 2: Derive lb (join adlb with adsl SAFFL=Y, filter PARAMCD)
param_list <- c('ALT', 'AST', 'BILI', 'CREAT', 'HGB', 'WBC', 'PLAT', 'NEUT')

# inner join adlb with adsl on USUBJID where SAFFL=Y
# Use distinct SAFFL subjects
saffl_subj <- adsl_saffl %>% select(USUBJID) %>% distinct()

lb <- adlb %>%
  inner_join(saffl_subj, by = "USUBJID") %>%
  filter(PARAMCD %in% param_list) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

# Ensure ATOXGR, BTOXGR are character
lb <- lb %>%
  mutate(
    ATOXGR = as.character(ATOXGR),
    BTOXGR = as.character(BTOXGR),
    ABLFL = as.character(ABLFL),
    ATOXGR = ifelse(is.na(ATOXGR), "", str_trim(ATOXGR)),
    BTOXGR = ifelse(is.na(BTOXGR), "", str_trim(BTOXGR)),
    ABLFL = ifelse(is.na(ABLFL), "", str_trim(ABLFL))
  )

# Step 3: Get baseline grade per subject per parameter
bl_grade <- lb %>%
  filter(ABLFL == "Y", BTOXGR != "") %>%
  distinct(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR) %>%
  rename(BL_GRADE = BTOXGR)

# Step 4: Get worst post-baseline grade
# where ABLFL ne 'Y' and ATOXGR ne '' and AVISITN > 0
worst_grade <- lb %>%
  filter(ABLFL != "Y", ATOXGR != "", !is.na(AVISITN), AVISITN > 0) %>%
  mutate(ATOXGR_N = suppressWarnings(as.numeric(ATOXGR))) %>%
  filter(!is.na(ATOXGR_N)) %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(WORST_GRADE_N = max(ATOXGR_N, na.rm = TRUE), .groups = "drop")

worst_grade2 <- worst_grade %>%
  mutate(WORST_GRADE = as.character(WORST_GRADE_N)) %>%
  mutate(WORST_GRADE = ifelse(WORST_GRADE_N == 0, "0", WORST_GRADE)) %>%
  select(-WORST_GRADE_N)

# Step 5: Merge baseline and worst post-baseline
shift <- bl_grade %>%
  inner_join(worst_grade2, by = c("USUBJID", "PARAMCD", "TRTA")) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE, WORST_GRADE)

# Step 6: Count subjects in each shift cell
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, BL_GRADE, WORST_GRADE) %>%
  summarise(CNT = n_distinct(USUBJID), .groups = "drop")

# Step 7: Create all possible combinations
grades <- tibble(GRADE = as.character(0:4))

# distinct TRTA/TRTAN/PARAMCD/PARAM from shift_cnt
distinct_cells <- shift_cnt %>% distinct(TRTA, TRTAN, PARAMCD, PARAM)

all_cells <- distinct_cells %>%
  cross_join(grades %>% rename(BL_GRADE = GRADE)) %>%
  cross_join(grades %>% rename(WORST_GRADE = GRADE))

shift_full <- all_cells %>%
  left_join(shift_cnt, by = c("TRTA", "PARAMCD", "BL_GRADE", "WORST_GRADE")) %>%
  mutate(CNT = coalesce(CNT, 0L)) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE, WORST_GRADE)

# Step 8: Transpose worst_grade into columns
# pivot_wider
shift_t <- shift_full %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, WORST_GRADE, CNT) %>%
  pivot_wider(names_from = WORST_GRADE, values_from = CNT, names_prefix = "WG", values_fill = 0)

# Ensure all WG columns exist (WG0-WG4)
for (g in 0:4) {
  col <- paste0("WG", g)
  if (!col %in% names(shift_t)) shift_t[[col]] <- 0L
}
# Order columns
shift_t <- shift_t %>% select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, WG0, WG1, WG2, WG3, WG4)

report <- shift_t %>%
  mutate(
    BL_LABEL = case_when(
      BL_GRADE == "0" ~ "Normal",
      BL_GRADE == "1" ~ "Grade 1",
      BL_GRADE == "2" ~ "Grade 2",
      BL_GRADE == "3" ~ "Grade 3",
      BL_GRADE == "4" ~ "Grade 4",
      TRUE ~ BL_GRADE
    )
  ) %>%
  mutate(across(starts_with("WG"), ~coalesce(.x, 0L))) %>%
  mutate(TOTAL = WG0 + WG1 + WG2 + WG3 + WG4) %>%
  arrange(TRTAN, PARAMCD, as.numeric(BL_GRADE))

# Ensure ordering of BL_GRADE numeric
report <- report %>%
  mutate(BL_GRADE_N = as.numeric(BL_GRADE)) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE_N) %>%
  select(-BL_GRADE_N)

# Prepare output dataset for CSV (as SAS would have)
# Keep variable names similar to SAS report dataset
report_out <- report %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, BL_LABEL, WG0, WG1, WG2, WG3, WG4, TOTAL)

# Write CSV
write.csv(report_out, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")

# Also write shift_full and shift_cnt for traceability (optional)
# But main required is t_lb_shift.csv

# Generate text rendering (RTF-like plain text)
# Titles/footnotes from u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
studyid <- "[STUDYID]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "  Page 1 of N")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.3.4"
title5 <- "Shift Table for Laboratory Parameters"
title6 <- "Safety Analysis Set"

# Build header strings for Big N
# Map TRTAN to label and N
bign_map <- bign_df %>% arrange(TRTAN)
# Ensure we have 3 groups
hdr_lines <- c()
for (i in seq_len(nrow(bign_map))) {
  hdr_lines <- c(hdr_lines, paste0(bign_map$TRTA[i], " (N=", bign_map$BIGN[i], ")"))
}
hdr_total <- paste0("Total (N=", bign_tot, ")")

# Create text file
txt_path <- file.path(out, "t_lb_shift.txt")
con <- file(txt_path, open = "wt")

writeLines(title1, con)
writeLines(title2, con)
writeLines("", con)
writeLines(title4, con)
writeLines(title5, con)
writeLines(title6, con)
writeLines("", con)
writeLines(paste0("Data cutoff: ", cutdate), con)
writeLines("Source: ADaM datasets", con)
writeLines("", con)

# For each treatment group, print table
# Mimic PROC REPORT with break after PARAMCD / page
# We'll print per TRTAN

# Helper to format table section
for (t in sort(unique(report_out$TRTAN))) {
  trta_label <- bign_map$TRTA[bign_map$TRTAN == t]
  if (length(trta_label) == 0) trta_label <- paste0("TRTA=", t)
  bign_val <- bign_map$BIGN[bign_map$TRTAN == t]
  if (length(bign_val) == 0) bign_val <- NA
  writeLines(paste0("Treatment Group: ", trta_label, " (N=", bign_val, ")"), con)
  writeLines(paste0(strrep("-", 100)), con)
  # Header row
  hdr <- sprintf("%-25s %-12s %8s %8s %8s %8s %8s %8s",
                 "Parameter", "Baseline Grade", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")
  writeLines(hdr, con)
  writeLines(paste0(strrep("-", 100)), con)
  sub <- report_out %>% filter(TRTAN == t) %>% arrange(PARAMCD, as.numeric(BL_GRADE))
  # Group by PARAMCD
  params <- unique(sub$PARAMCD)
  for (p in params) {
    psub <- sub %>% filter(PARAMCD == p)
    param_name <- unique(psub$PARAM)
    # First row for parameter shows PARAM, subsequent rows blank for PARAM column but we repeat for clarity
    for (r in seq_len(nrow(psub))) {
      row <- psub[r, ]
      # PARAM column only on first row of parameter block to mimic ORDER
      param_disp <- if (r == 1) paste0(row$PARAMCD, " (", row$PARAM, ")") else ""
      line <- sprintf("%-25s %-12s %8d %8d %8d %8d %8d %8d",
                      substr(param_disp, 1, 25),
                      row$BL_LABEL,
                      row$WG0, row$WG1, row$WG2, row$WG3, row$WG4, row$TOTAL)
      writeLines(line, con)
    }
    writeLines("", con)
  }
  writeLines("", con)
}

writeLines("Grades based on NCI-CTCAE v[X.X]", con)
writeLines("Shift from baseline grade to worst post-baseline grade.", con)
writeLines("", con)
writeLines(paste0("Footnotes: Data cutoff: ", cutdate, " | Source: ADaM datasets"), con)
close(con)

# Also write RTF placeholder (copy txt to rtf for compatibility)
rtf_path <- file.path(out, "t_lb_shift.rtf")
# Simple RTF wrapper with plain text
rtf_header <- "{\\rtf1\\ansi\\deff0 {\\fonttbl {\\f0 Courier New;}}\n\\viewkind4\\uc1\\pard\\f0\\fs18\n"
rtf_footer <- "}"
txt_content <- readLines(txt_path, warn = FALSE)
# Escape RTF special chars
escape_rtf <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x
}
rtf_body <- paste(escape_rtf(txt_content), collapse = "\\par\n")
writeLines(paste0(rtf_header, rtf_body, "\n", rtf_footer), rtf_path)

# Verify counts
# 969 numbers check: count numeric values in report_out
# (not enforced, just for log)
num_count <- nrow(report_out) * 6  # WG0-4 + TOTAL per row
message("Report rows: ", nrow(report_out), " numbers in table body: ", num_count)
message("Big N per treatment:")
print(bign_df)
message("Total N: ", bign_tot)
message("Outputs written to: ", out)
