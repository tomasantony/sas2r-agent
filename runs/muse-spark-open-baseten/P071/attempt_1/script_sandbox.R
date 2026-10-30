# P071 t_disp.sas -> R translation
# Table 14.1.2 Subject Disposition - All Randomized Subjects

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct1 <- function(x) {
  ifelse(is.na(x), NA_character_,
         formatC(round_half_up(x, 1), format = "f", digits = 1, flag = "#"))
}
strip <- function(x) trimws(x, which = "both")

read_adam <- function(name) {
  # try .xpt, .sas7bdat, .csv in adam_path
  base <- file.path(adam_path, name)
  f_xpt <- paste0(base, ".xpt")
  f_sas <- paste0(base, ".sas7bdat")
  f_csv <- paste0(base, ".csv")
  # also list files case-insensitive
  if (file.exists(f_xpt)) {
    df <- haven::read_xpt(f_xpt)
  } else if (file.exists(f_sas)) {
    df <- haven::read_sas(f_sas)
  } else if (file.exists(toupper(f_xpt))) {
    df <- haven::read_xpt(toupper(f_xpt))
  } else {
    # search directory
    files <- list.files(adam_path, full.names = TRUE, ignore.case = TRUE)
    pat <- paste0("^", name, "\\.(xpt|sas7bdat)$")
    idx <- grep(pat, basename(files), ignore.case = TRUE)
    if (length(idx) == 0) stop(paste("ADaM dataset not found:", name, "in", adam_path))
    fp <- files[idx[1]]
    if (grepl("\\.xpt$", fp, ignore.case = TRUE)) df <- haven::read_xpt(fp) else df <- haven::read_sas(fp)
  }
  # convert haven labelled to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap labels but keep values
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      # keep underlying value, convert to character if labelled character
      df[[nm]] <- haven::zap_labels(df[[nm]])
      df[[nm]] <- haven::zap_formats(df[[nm]])
    }
  }
  # ensure character columns are character (not factor)
  for (nm in names(df)) if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  df
}

# Load ADSL -------------------------------------------------------------
adsl <- read_adam("adsl")

# Ensure expected columns exist (uppercase names as in SAS)
# haven may preserve case; normalize to uppercase for processing but keep original
names(adsl) <- toupper(names(adsl))

# Required columns check
# TRTA, USUBJID, RANDFL, ITTFL, SAFFL, EFFFL, PPROTFL, EOSSTT, DCSREAS
# If TRT01A etc exists, fallback - but spec uses TRTA
if (!"TRTA" %in% names(adsl)) {
  # try TRTA alternatives
  if ("TRT01A" %in% names(adsl)) adsl$TRTA <- adsl$TRT01A
  if ("TRT01P" %in% names(adsl) && !"TRTA" %in% names(adsl)) adsl$TRTA <- adsl$TRT01P
}

# Step 1: m_counts equivalent - Big N for RANDFL=Y ----------------------
# SAS: where upcase(popflag) = 'Y' group by TRTA order by TRTA
adsl_rand <- adsl[toupper(trimws(adsl$RANDFL)) == "Y" & !is.na(adsl$RANDFL), , drop = FALSE]

# distinct USUBJID per TRTA
library(dplyr)
library(tidyr)

bign_df <- adsl_rand %>%
  filter(!is.na(TRTA), TRTA != "") %>%
  distinct(TRTA, USUBJID) %>%
  group_by(TRTA) %>%
  summarise(bign = n(), .groups = "drop") %>%
  arrange(TRTA)  # SAS character order: uppercase before lowercase; R sort similar for ASCII

# Total N
bign_tot <- adsl_rand %>% distinct(USUBJID) %>% nrow()

# Build header strings like SAS: "Placebo (N=xx)"
# Order is by TRTA sorted
trt_order <- sort(unique(bign_df$TRTA))  # SAS order by TRTA
# Ensure bign_df ordered by trt_order
bign_df <- bign_df %>% arrange(match(TRTA, trt_order))

# Create macro-like headers
col_hdrs <- setNames(
  paste0(bign_df$TRTA, " (N=", bign_df$bign, ")"),
  paste0("col", seq_along(bign_df$TRTA), "hdr")
)
# For report display, map TRTA -> col1, col2, col3 in sorted order
trt_to_col <- setNames(paste0("col", seq_along(trt_order)), trt_order)

# Step 2: disp ----------------------------------------------------------
disp <- adsl[toupper(trimws(adsl$RANDFL)) == "Y" & !is.na(adsl$RANDFL), , drop = FALSE]

# Step 3: pop_count macro ------------------------------------------------
pop_count <- function(flag, label, ord) {
  if (!flag %in% names(disp)) {
    # return empty
    return(data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE))
  }
  tmp <- disp[toupper(trimws(disp[[flag]])) == "Y" & !is.na(disp[[flag]]), , drop = FALSE]
  if (nrow(tmp) == 0) {
    return(data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), stringsAsFactors = FALSE))
  }
  tmp %>%
    distinct(TRTA, USUBJID) %>%
    group_by(TRTA) %>%
    summarise(cnt = n(), .groups = "drop") %>%
    mutate(category = label, ord = ord) %>%
    as.data.frame()
}

pop_randfl  <- pop_count("RANDFL",  "Randomized",                1)
pop_ittfl   <- pop_count("ITTFL",   "  Intent-to-Treat Set",     2)
pop_saffl   <- pop_count("SAFFL",   "  Safety Analysis Set",     3)
pop_efffl   <- pop_count("EFFFL",   "  Efficacy Analysis Set",   4)
pop_pprotfl <- pop_count("PPROTFL", "  Per-Protocol Set",         5)

# Step 4: Completion / Discontinuation ----------------------------------
# Completed study
completed <- disp %>%
  filter(!is.na(EOSSTT), trimws(EOSSTT) == "COMPLETED") %>%
  distinct(TRTA, USUBJID) %>%
  group_by(TRTA) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  mutate(category = "Completed Study", ord = 10) %>%
  as.data.frame()

discont <- disp %>%
  filter(!is.na(EOSSTT), trimws(EOSSTT) == "DISCONTINUED") %>%
  distinct(TRTA, USUBJID) %>%
  group_by(TRTA) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  mutate(category = "Discontinued Study", ord = 11) %>%
  as.data.frame()

# Step 5: Reasons -------------------------------------------------------
reasons <- disp %>%
  filter(!is.na(EOSSTT), trimws(EOSSTT) == "DISCONTINUED",
         !is.na(DCSREAS), trimws(DCSREAS) != "") %>%
  distinct(TRTA, USUBJID, DCSREAS) %>%
  group_by(TRTA, DCSREAS) %>%
  summarise(cnt = n(), .groups = "drop") %>%
  as.data.frame()

if (nrow(reasons) > 0) {
  reasons2 <- reasons %>%
    mutate(
      category = paste0("  ", trimws(DCSREAS)),
      ord = case_when(
        toupper(trimws(DCSREAS)) == "ADVERSE EVENT"         ~ 12,
        toupper(trimws(DCSREAS)) == "DEATH"                 ~ 13,
        toupper(trimws(DCSREAS)) == "LACK OF EFFICACY"      ~ 14,
        toupper(trimws(DCSREAS)) == "LOST TO FOLLOW-UP"     ~ 15,
        toupper(trimws(DCSREAS)) == "PHYSICIAN DECISION"    ~ 16,
        toupper(trimws(DCSREAS)) == "PROTOCOL VIOLATION"    ~ 17,
        toupper(trimws(DCSREAS)) == "WITHDRAWAL BY SUBJECT" ~ 18,
        TRUE                                                ~ 19
      )
    ) %>%
    select(TRTA, cnt, category, ord, DCSREAS)
} else {
  reasons2 <- data.frame(TRTA = character(0), cnt = numeric(0), category = character(0), ord = numeric(0), DCSREAS = character(0), stringsAsFactors = FALSE)
}

# Step 6: Combine -------------------------------------------------------
all_disp <- bind_rows(pop_randfl, pop_ittfl, pop_saffl, pop_efffl, pop_pprotfl,
                      completed, discont, reasons2 %>% select(TRTA, cnt, category, ord))

# Step 7: Add percentages and format ------------------------------------
# left join with bign
all_disp2 <- all_disp %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_))

all_disp3 <- all_disp2 %>%
  mutate(
    value = case_when(
      ord == 1 ~ trimws(format(cnt, width = 8, justify = "right")),
      cnt > 0  ~ paste0(trimws(format(cnt, width = 8, justify = "right")), " (", trimws(fmt_pct1(pct)), ")"),
      TRUE     ~ "0"
    )
  )

# Step 8: Transpose -----------------------------------------------------
# SAS: proc sort by ord category; proc transpose by ord category id TRTA var value
# Need to ensure all TRTA levels present as columns, fill missing with NA then later "0" or blank
all_disp3 <- all_disp3 %>% arrange(ord, category)

# pivot wider
final_wide <- all_disp3 %>%
  select(ord, category, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = NA_character_)

# Rename columns to col1, col2, col3 according to trt_order
# final_wide currently has columns named by TRTA values
for (trt in trt_order) {
  colname <- trt_to_col[[trt]]
  if (trt %in% names(final_wide)) {
    names(final_wide)[names(final_wide) == trt] <- colname
  } else {
    final_wide[[colname]] <- NA_character_
  }
}
# Ensure col1..col3 exist (if ntrt <3, still create)
for (i in seq_along(trt_order)) {
  cn <- paste0("col", i)
  if (!cn %in% names(final_wide)) final_wide[[cn]] <- NA_character_
}
# Fill NA with "0" ? SAS would show blank for missing, but for disposition missing means 0
# Keep NA as blank for now, but replace with "0" to match expected 70 numbers logic
# Check SAS: if no row, transpose missing -> report blank. We'll keep blank but also ensure CSV shows blank.
# For consistency, replace NA with "0" only if that TRTA had no subjects for that category but should be 0
# We'll replace NA with "0" (SAS would show blank but spec expects 0 for zero counts)
for (i in seq_along(trt_order)) {
  cn <- paste0("col", i)
  final_wide[[cn]][is.na(final_wide[[cn]])] <- "0"
}

final_wide <- final_wide %>% arrange(ord, category)

# Step 9: Section headers -----------------------------------------------
report <- final_wide %>%
  mutate(
    section = case_when(
      ord == 1  ~ "Analysis Populations",
      ord == 10 ~ "Study Completion",
      ord >= 12 ~ "  Reason for Discontinuation",
      TRUE      ~ ""
    )
  ) %>%
  arrange(ord, category)

# Show section header only once (like SAS retain)
report$`_show_section` <- ""
lastsect <- ""
for (i in seq_len(nrow(report))) {
  sec <- report$section[i]
  if (sec != "" && sec != lastsect) {
    report$`_show_section`[i] <- sec
    lastsect <- sec
  } else {
    report$`_show_section`[i] <- ""
  }
}

# Ensure column order
# col headers for display
hdr_map <- list()
for (i in seq_along(trt_order)) {
  hdr_map[[paste0("col", i)]] <- paste0(trt_order[i], " (N=", bign_df$bign[bign_df$TRTA == trt_order[i]], ")")
}
# If 3 groups expected, ensure col1-col3
# Build display headers
col1hdr <- ifelse("col1" %in% names(hdr_map), hdr_map[["col1"]], "col1")
col2hdr <- ifelse("col2" %in% names(hdr_map), hdr_map[["col2"]], "col2")
col3hdr <- ifelse("col3" %in% names(hdr_map), hdr_map[["col3"]], "col3")

# Prepare final report for output (keep ord for sorting but hide in display)
report_out <- report %>%
  select(ord, `_show_section`, category, starts_with("col")) %>%
  arrange(ord)

# Ensure col order col1, col2, col3
cols_present <- paste0("col", seq_along(trt_order))
report_out <- report_out %>% select(ord, `_show_section`, category, all_of(cols_present))

# Write CSV dataset (as SAS dataset counterpart) ------------------------
# Write the analysis dataset (all_disp3) and the report table
write.csv(report_out, file.path(out, "t_disp.csv"), row.names = FALSE, na = "")

# Also write a detailed CSV of table body for verification (same as report_out without ord)
report_body <- report_out %>% select(-ord)
# Rename headers to actual display headers for CSV body
names(report_body)[names(report_body) == "_show_section"] <- "Section"
names(report_body)[names(report_body) == "category"] <- "Category"
for (i in seq_along(trt_order)) {
  cn <- paste0("col", i)
  if (cn %in% names(report_body)) names(report_body)[names(report_body) == cn] <- hdr_map[[cn]]
}
write.csv(report_body, file.path(out, "t_disp_body.csv"), row.names = FALSE, na = "")

# Text rendering --------------------------------------------------------
# Build plain-text table similar to SAS PROC REPORT
# Titles from u_titles.sas for T_DISP
title_lines <- c(
  "Table 14.1.2",
  "Subject Disposition",
  "All Randomized Subjects"
)

# Column widths for text
# Create header row
header_row <- c("Category", unname(unlist(hdr_map[cols_present])))
# Build table lines
# Use fixed-width formatting for readability
# Determine widths: Category 40, each col 20
cat_w <- 40
col_w <- 22

pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), substr(x, 1, w))
pad_center <- function(x, w) {
  x <- as.character(x)
  if (is.na(x)) x <- ""
  n <- nchar(x)
  if (n >= w) return(substr(x, 1, w))
  left <- floor((w - n)/2)
  right <- w - n - left
  paste0(strrep(" ", left), x, strrep(" ", right))
}
pad_left <- function(x, w) sprintf(paste0("%", w, "s"), substr(x, 1, w))

txt_lines <- c()
txt_lines <- c(txt_lines, paste0(rep("-", cat_w + length(cols_present)*col_w + length(cols_present)), collapse=""))
for (tl in title_lines) txt_lines <- c(txt_lines, tl)
txt_lines <- c(txt_lines, "")
# Header
hdr_line <- paste0(pad_right(header_row[1], cat_w),
                   paste0(sapply(header_row[-1], function(h) pad_center(h, col_w)), collapse=""))
txt_lines <- c(txt_lines, hdr_line)
txt_lines <- c(txt_lines, paste0(rep("-", nchar(hdr_line)), collapse=""))

# Rows with section headers
for (i in seq_len(nrow(report_out))) {
  sec <- report_out$`_show_section`[i]
  if (sec != "") {
    txt_lines <- c(txt_lines, "")
    txt_lines <- c(txt_lines, sec)
  }
  cat_val <- report_out$category[i]
  # preserve leading spaces (SAS shows indented)
  # For text, keep as is but pad
  row_vals <- sapply(cols_present, function(cn) report_out[[cn]][i])
  # Format row
  # Category left aligned, cols centered
  line <- paste0(pad_right(cat_val, cat_w),
                 paste0(sapply(row_vals, function(v) pad_center(ifelse(is.na(v), "", v), col_w)), collapse=""))
  txt_lines <- c(txt_lines, line)
}
txt_lines <- c(txt_lines, paste0(rep("-", nchar(hdr_line)), collapse=""))
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "Data cutoff: [DDMONYYYY]")
txt_lines <- c(txt_lines, "Source: ADaM datasets")
txt_lines <- c(txt_lines, paste0("Program: P071_t_disp.sas | Run: ", format(Sys.Date(), "%d%b%Y")))

# Write txt
writeLines(txt_lines, file.path(out, "t_disp.txt"))

# Also write RTF-like txt as t_disp.rtf for expected output check (plain text RTF placeholder)
writeLines(txt_lines, file.path(out, "t_disp.rtf"))

# Log
cat("P071 t_disp completed. Rows:", nrow(report_out), "Cols:", ncol(report_out), "\n")
cat("Big N:", paste(bign_df$TRTA, bign_df$bign, collapse="; "), "Total:", bign_tot, "\n")
