# P071_t_disp.sas -> R translation
# Table 14.1.2 Subject Disposition - All Randomized Subjects

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

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

# Helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         {
           mult <- 10^digits
           sign_x <- sign(x)
           abs_x <- abs(x)
           # floor(abs*mult + 0.5) / mult
           rounded <- floor(abs_x * mult + 0.5 + 1e-9) / mult
           sign_x * rounded
         })
}
# SAS put with w.d : round half up then format
sas_put <- function(x, w = 8, d = 0) {
  if (is.na(x)) return(NA_character_)
  rx <- round_half_up(x, d)
  fmt <- paste0("%", w, ".", d, "f")
  s <- sprintf(fmt, rx)
  trimws(s)
}
# pct format 5.1
fmt_pct <- function(p) {
  if (is.na(p)) return(NA_character_)
  rp <- round_half_up(p, 1)
  # SAS 5.1 would give width 5, but strip removes leading spaces
  s <- sprintf("%.1f", rp)
  s
}

# Locate ADSL
adam_path1 <- "/bench/data/raw/own-synthetic/adam"
adam_path2 <- Sys.getenv("rawadam")
if (adam_path2 == "") adam_path2 <- "/bench/data/raw/cdisc-pilot/adam"
adam_paths <- unique(c(adam_path1, adam_path2))

adsl_file <- NULL
for (p in adam_paths) {
  if (dir.exists(p)) {
    cand <- list.files(p, pattern = "^adsl\\.(sas7bdat|xpt)$", ignore.case = TRUE, full.names = TRUE)
    if (length(cand) > 0) { adsl_file <- cand[1]; break }
    # also try without extension case
    cand2 <- list.files(p, pattern = "adsl", ignore.case = TRUE, full.names = TRUE)
    if (length(cand2) > 0) {
      # filter to files
      cand2 <- cand2[!dir.exists(cand2)]
      if (length(cand2) > 0) { adsl_file <- cand2[1]; break }
    }
  }
}
if (is.null(adsl_file) || !file.exists(adsl_file)) {
  # fallback: search recursively
  for (p in adam_paths) {
    if (dir.exists(p)) {
      cand <- list.files(p, recursive = TRUE, pattern = "adsl", ignore.case = TRUE, full.names = TRUE)
      if (length(cand) > 0) { adsl_file <- cand[1]; break }
    }
  }
}
if (is.null(adsl_file) || !file.exists(adsl_file)) {
  stop("ADSL file not found in adam paths: ", paste(adam_paths, collapse="; "))
}

# Read ADSL
ext <- tolower(tools::file_ext(adsl_file))
if (ext == "xpt") {
  adsl_raw <- haven::read_xpt(adsl_file)
} else if (ext == "sas7bdat") {
  adsl_raw <- haven::read_sas(adsl_file)
} else {
  # try both
  tryCatch({ adsl_raw <- haven::read_sas(adsl_file) }, error = function(e) { adsl_raw <<- haven::read_xpt(adsl_file) })
}
adsl <- adsl_raw
# Convert haven_labelled to character/numeric where needed
adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
# Ensure character columns are character
# Standardize column names to uppercase for matching SAS (SAS is case-insensitive)
names(adsl) <- toupper(names(adsl))

# Check required columns exist
required <- c("USUBJID","TRTA","RANDFL","ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")
for (col in required) {
  if (!col %in% names(adsl)) {
    # create empty column if missing
    adsl[[col]] <- NA_character_
  }
}

# Step 1: m_counts - Big N for RANDFL=Y
bign_df <- adsl %>%
  filter(toupper(RANDFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTA)

bigntot <- adsl %>% filter(toupper(RANDFL) == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)

# Determine display order for treatment columns
# SAS sorts by TRTA (character, uppercase before lowercase). Use same.
# But for display we want Placebo first, then Low, then High if identifiable
trt_levels_sorted <- sort(unique(bign_df$TRTA))
# Identify by substring
find_trt <- function(pattern, levels) {
  idx <- grep(pattern, levels, ignore.case = TRUE)
  if (length(idx) > 0) levels[idx[1]] else NA_character_
}
placebo_lbl <- find_trt("placebo", trt_levels_sorted)
low_lbl <- find_trt("low", trt_levels_sorted)
high_lbl <- find_trt("high", trt_levels_sorted)

if (!is.na(placebo_lbl) && !is.na(low_lbl) && !is.na(high_lbl)) {
  display_order <- c(placebo_lbl, low_lbl, high_lbl)
  # ensure all levels covered
  remaining <- setdiff(trt_levels_sorted, display_order)
  display_order <- c(display_order, remaining)
} else {
  display_order <- trt_levels_sorted
}
# Reorder bign_df to display_order for headers
bign_df <- bign_df %>% mutate(TRTA = factor(TRTA, levels = display_order)) %>% arrange(TRTA) %>% mutate(TRTA = as.character(TRTA))

# Build header strings like "Placebo (N=xx)"
headers <- bign_df %>% mutate(hdr = paste0(TRTA, " (N=", bign, ")")) %>% pull(hdr)
# Map to col1, col2, col3
col_hdrs <- setNames(headers, paste0("col", seq_along(headers)))
# Ensure 3 columns
while (length(col_hdrs) < 3) {
  col_hdrs <- c(col_hdrs, setNames(paste0("Missing (N=0)"), paste0("col", length(col_hdrs)+1)))
}
col1hdr <- col_hdrs[["col1"]]
col2hdr <- col_hdrs[["col2"]]
col3hdr <- col_hdrs[["col3"]]

# Step 2: Prepare disp
disp <- adsl %>% filter(RANDFL == "Y")

# Helper pop_count
pop_count <- function(flag, label, ord) {
  if (!flag %in% names(disp)) return(tibble(TRTA=character(), cnt=integer(), category=character(), ord=integer()))
  disp %>%
    filter(toupper(.data[[flag]]) == "Y") %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
    mutate(category = label, ord = ord)
}

pop_randfl  <- pop_count("RANDFL",  "Randomized", 1)
pop_ittfl   <- pop_count("ITTFL",   "  Intent-to-Treat Set", 2)
pop_saffl   <- pop_count("SAFFL",   "  Safety Analysis Set", 3)
pop_efffl   <- pop_count("EFFFL",   "  Efficacy Analysis Set", 4)
pop_pprotfl <- pop_count("PPROTFL", "  Per-Protocol Set", 5)

# Step 4: Completion / Discontinuation
completed <- disp %>%
  filter(EOSSTT == "COMPLETED") %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(category = "Completed Study", ord = 10)

discont <- disp %>%
  filter(EOSSTT == "DISCONTINUED") %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(category = "Discontinued Study", ord = 11)

# Step 5: Reasons
reasons <- disp %>%
  filter(EOSSTT == "DISCONTINUED" & !is.na(DCSREAS) & DCSREAS != "") %>%
  group_by(TRTA, DCSREAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop")

reasons2 <- reasons %>%
  mutate(category = paste0("  ", str_trim(DCSREAS)),
         ord = case_when(
           toupper(DCSREAS) == "ADVERSE EVENT" ~ 12L,
           toupper(DCSREAS) == "DEATH" ~ 13L,
           toupper(DCSREAS) == "LACK OF EFFICACY" ~ 14L,
           toupper(DCSREAS) == "LOST TO FOLLOW-UP" ~ 15L,
           toupper(DCSREAS) == "PHYSICIAN DECISION" ~ 16L,
           toupper(DCSREAS) == "PROTOCOL VIOLATION" ~ 17L,
           toupper(DCSREAS) == "WITHDRAWAL BY SUBJECT" ~ 18L,
           TRUE ~ 19L
         )) %>%
  select(TRTA, cnt, category, ord)

# Step 6: Combine
all_disp <- bind_rows(pop_randfl, pop_ittfl, pop_saffl, pop_efffl, pop_pprotfl,
                      completed, discont, reasons2)

# Step 7: Add percentages
all_disp2 <- all_disp %>%
  left_join(bign_df, by="TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_)) %>%
  arrange(ord, TRTA)

# Format value
all_disp3 <- all_disp2 %>%
  mutate(value = case_when(
    ord == 1 ~ as.character(cnt),
    cnt > 0 ~ paste0(trimws(as.character(cnt)), " (", fmt_pct(pct), ")"),
    TRUE ~ "0"
  ))

# Step 8: Transpose
# Need to ensure all ord/category combinations have rows for all TRTA
# Create wide via pivot_wider
# First sort
all_disp3 <- all_disp3 %>% arrange(ord, category)

# Pivot
wide <- all_disp3 %>%
  select(ord, category, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

# Ensure columns for display_order exist, fill missing with "0" or blank? SAS would be blank, but we fill "0" for consistency
for (trt in display_order) {
  if (!trt %in% names(wide)) {
    wide[[trt]] <- NA_character_
  }
}
# Reorder columns to display_order
wide <- wide %>% select(ord, category, all_of(display_order))

# Rename to col1, col2, col3 as report expects
# Map display_order[1] -> col1 etc.
col_map <- setNames(paste0("col", seq_along(display_order)), display_order)
wide_renamed <- wide
for (trt in display_order) {
  wide_renamed[[col_map[[trt]]]] <- wide_renamed[[trt]]
  wide_renamed[[trt]] <- NULL
}
# Ensure col1-col3 exist
for (i in 1:3) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide_renamed)) wide_renamed[[cn]] <- NA_character_
}
# Fill NA with "0" for display (SAS would be blank, but we use "0" to show numbers)
for (cn in paste0("col", 1:3)) {
  wide_renamed[[cn]][is.na(wide_renamed[[cn]])] <- "0"
  # Also if empty string, keep as "0"? Keep blank as "0" for counts
}

# Sort by ord, category (SAS sort by ord category)
wide_renamed <- wide_renamed %>% arrange(ord, category)

# Step 9: Section headers
report <- wide_renamed %>%
  mutate(section = case_when(
    ord == 1 ~ "Analysis Populations",
    ord == 10 ~ "Study Completion",
    ord >= 12 ~ "  Reason for Discontinuation",
    TRUE ~ ""
  )) %>%
  arrange(ord)

# Retain logic for _show_section
report <- report %>%
  mutate(`_show_section` = NA_character_)
lastsect <- ""
show_vec <- character(nrow(report))
for (i in seq_len(nrow(report))) {
  sec <- report$section[i]
  if (sec != lastsect && sec != "") {
    show_vec[i] <- sec
    lastsect <- sec
  } else {
    show_vec[i] <- ""
  }
}
report$`_show_section` <- show_vec

# For display, we want to keep ord for ordering but not print
# Create final display order: include section header rows as separate?
# In SAS, section is not printed as separate row, but _show_section would be a column (noprint)
# For R rendering, we will show section as a bold row spanning categories

# Prepare final table for CSV and TXT
# CSV should contain the table body as displayed: Category, col1, col2, col3, plus section info
final_csv <- report %>%
  select(ord, category, col1, col2, col3, section, `_show_section`)

# Write CSV
write.csv(final_csv, file.path(out, "t_disp.csv"), row.names = FALSE, na = "")

# Also write bign dataset for reference (optional)
write.csv(bign_df, file.path(out, "_bign.csv"), row.names = FALSE, na = "")

# Step 10: Generate text rendering
# Titles from u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
studyid <- "[STUDYID]"

title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title3 <- ""
title4 <- "Table 14.1.2"
title5 <- "Subject Disposition"
title6 <- "All Randomized Subjects"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
# Program name and run date
prog <- "P071_t_disp.sas"
rundate <- format(Sys.Date(), "%d%b%Y")
runtime <- format(Sys.time(), "%H:%M")
footnote3 <- paste0("Program: ", prog, " | Run: ", rundate, " ", runtime)

# Build text file
txt_path <- file.path(out, "t_disp.txt")
con <- file(txt_path, open="wt")

cat(title1, "\n", file=con)
cat(title2, "\n", file=con)
if (title3 != "") cat(title3, "\n", file=con) else cat("\n", file=con)
cat(title4, "\n", file=con)
cat(title5, "\n", file=con)
cat(title6, "\n", file=con)
cat("\n", file=con)

# Header row
hdr_line <- sprintf("%-40s %-20s %-20s %-20s", "Category", col1hdr, col2hdr, col3hdr)
cat(hdr_line, "\n", file=con)
cat(paste(rep("-", nchar(hdr_line)), collapse=""), "\n", file=con)

# Rows: iterate report in ord order
for (i in seq_len(nrow(report))) {
  sec_show <- report$`_show_section`[i]
  if (sec_show != "" && !is.na(sec_show)) {
    # Section header row - bold simulation with uppercase
    cat("\n", file=con)
    cat(sec_show, "\n", file=con)
  }
  cat(sprintf("%-40s %-20s %-20s %-20s",
              report$category[i],
              ifelse(is.na(report$col1[i]), "", report$col1[i]),
              ifelse(is.na(report$col2[i]), "", report$col2[i]),
              ifelse(is.na(report$col3[i]), "", report$col3[i])), "\n", file=con)
}

cat("\n", file=con)
cat(footnote1, "\n", file=con)
cat(footnote2, "\n", file=con)
cat(footnote3, "\n", file=con)

close(con)

# Also create a more detailed CSV of plotted data not needed (no figure)
# Ensure t_disp.rtf placeholder text file is also considered - we have txt

# Log completion
cat("NOTE: t_disp.sas completed successfully.\n")
cat(paste0("Big N: ", paste(bign_df$TRTA, bign_df$bign, collapse=", "), " Total=", bigntot, "\n"))
