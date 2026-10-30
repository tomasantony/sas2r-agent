```r
# P074_t_lb_shift.sas -> R translation
# Table 14.3.4 - Shift Table for Laboratory Parameters

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
adampath <- file.path(root, "data/raw/own-synthetic/adam")
sdtmpath <- file.path(root, "data/raw/own-synthetic/sdtm")

# helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}

# helper: read ADaM dataset (sas7bdat or xpt)
read_adam <- function(name, path = adampath) {
  # normalize name
  lname <- tolower(name)
  files <- list.files(path, full.names = TRUE, recursive = FALSE)
  # case-insensitive match
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == lname)
  f <- NULL
  if (length(idx) > 0) f <- files[idx[1]]
  # also try explicit extensions
  if (is.null(f) || !file.exists(f)) {
    cand <- c(file.path(path, paste0(name, ".sas7bdat")),
              file.path(path, paste0(tolower(name), ".sas7bdat")),
              file.path(path, paste0(name, ".xpt")),
              file.path(path, paste0(tolower(name), ".xpt")),
              file.path(path, paste0(name, ".sas7bdat".toUpper <- toupper(name))),
              file.path(path, paste0(toupper(name), ".sas7bdat")),
              file.path(path, paste0(toupper(name), ".xpt")))
    cand <- unique(cand)
    for (cc in cand) if (file.exists(cc)) { f <- cc; break }
  }
  if (is.null(f) || !file.exists(f)) {
    stop(paste0("File not found for dataset ", name, " in ", path, " ; files: ", paste(basename(files), collapse=", ")))
  }
  cat("Reading:", f, "\n")
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    # try both
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <<- haven::read_xpt(f))
  }
  # convert haven_labelled to R types, keep character as character
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap labels but keep values
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      # if numeric labelled, keep numeric; if character labelled, keep character
      # haven::zap_labels preserves type
      df[[nm]] <- haven::zap_labels(df[[nm]])
      # if still labelled (haven_labelled), convert via as_factor for character
      if (inherits(df[[nm]], "haven_labelled")) {
        df[[nm]] <- as.character(haven::as_factor(df[[nm]], levels = "labels"))
      }
    }
    if (inherits(df[[nm]], "labelled")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  # uppercase names for SAS-like processing
  names(df) <- toupper(names(df))
  df
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

cat("Step 1: Get Big N\n")
adsl <- read_adam("adsl", adampath)
adlb <- read_adam("adlb", adampath)

# Ensure needed columns exist
# ADSL: USUBJID, TRTA, TRTAN, SAFFL
# ADLB: USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL

# Big N per TRTA where SAFFL='Y'
adsl_saffl <- adsl %>% filter(!is.na(SAFFL) & toupper(trimws(SAFFL)) == "Y")
bign_df <- adsl_saffl %>%
  group_by(TRTA, TRTAN) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTAN, TRTA)

# Total Big N
bign_tot <- n_distinct(adsl_saffl$USUBJID)

# Build header strings (for titles)
ntrt <- nrow(bign_df)
if (ntrt == 0) ntrt <- 3
cat("Big N:\n")
print(bign_df)
cat("Total N:", bign_tot, "\n")

# Step 2: Derive lb
cat("Step 2: Derive lb\n")
# Filter ADSL SAFFL=Y for join
# Keep only needed ADSL columns for join
adsl_join <- adsl %>% filter(!is.na(SAFFL) & toupper(trimws(SAFFL)) == "Y") %>% select(USUBJID)

param_list <- c('ALT', 'AST', 'BILI', 'CREAT', 'HGB', 'WBC', 'PLAT', 'NEUT')

# Ensure ADLB columns exist, create missing as NA
needed_lb <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL","ANL01FL")
for (cc in needed_lb) if (!cc %in% names(adlb)) adlb[[cc]] <- NA

lb <- adlb %>%
  inner_join(adsl_join, by = "USUBJID") %>%
  filter(!is.na(PARAMCD) & toupper(trimws(PARAMCD)) %in% param_list) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

# Step 3: Get baseline grade per subject per parameter
cat("Step 3: bl_grade\n")
bl_grade <- lb %>%
  filter(!is.na(ABLFL) & trimws(ABLFL) == "Y",
         !is.na(BTOXGR) & trimws(BTOXGR) != "") %>%
  distinct(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR) %>%
  rename(BL_GRADE = BTOXGR)

# Step 4: Get worst post-baseline grade
cat("Step 4: worst_grade\n")
worst_grade <- lb %>%
  filter((is.na(ABLFL) | trimws(ABLFL) != "Y"),
         !is.na(ATOXGR) & trimws(ATOXGR) != "",
         !is.na(AVISITN) & AVISITN > 0) %>%
  mutate(ATOXGR_N = suppressWarnings(as.numeric(trimws(ATOXGR)))) %>%
  filter(!is.na(ATOXGR_N)) %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(WORST_GRADE_N = max(ATOXGR_N, na.rm = TRUE), .groups = "drop")

worst_grade2 <- worst_grade %>%
  mutate(WORST_GRADE = trimws(as.character(WORST_GRADE_N))) %>%
  mutate(WORST_GRADE = ifelse(WORST_GRADE_N == 0, "0", WORST_GRADE)) %>%
  select(-WORST_GRADE_N)

# Step 5: Merge baseline and worst
cat("Step 5: shift\n")
shift <- bl_grade %>%
  inner_join(worst_grade2, by = c("USUBJID", "PARAMCD", "TRTA")) %>%
  # keep TRTAN from bl_grade (should match worst_grade TRTAN, but use bl_grade's)
  arrange(TRTAN, PARAMCD, BL_GRADE, WORST_GRADE)

# Step 6: Count subjects in each shift cell
cat("Step 6: shift_cnt\n")
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, BL_GRADE, WORST_GRADE) %>%
  summarise(CNT = n_distinct(USUBJID), .groups = "drop")

# Step 7: Create all possible combinations
cat("Step 7: all_cells\n")
grades <- tibble(GRADE = as.character(0:4))

# distinct TRTA/TRTAN/PARAMCD/PARAM from shift_cnt
distinct_cells <- shift_cnt %>% distinct(TRTA, TRTAN, PARAMCD, PARAM)

if (nrow(distinct_cells) == 0) {
  # fallback: if no shift data, create empty structure to avoid error
  all_cells <- tibble(TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character(), BL_GRADE=character(), WORST_GRADE=character())
} else {
  all_cells <- distinct_cells %>%
    cross_join(grades %>% rename(BL_GRADE = GRADE)) %>%
    cross_join(grades %>% rename(WORST_GRADE = GRADE))
}

# Merge actual counts
shift_full <- all_cells %>%
  left_join(shift_cnt, by = c("TRTA","TRTAN","PARAMCD","PARAM","BL_GRADE","WORST_GRADE")) %>%
  mutate(CNT = ifelse(is.na(CNT), 0L, CNT)) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE, WORST_GRADE)

# Step 8: Format and transpose for display
cat("Step 8: transpose\n")
# pivot wider: worst_grade into columns wg0..wg4
shift_t <- shift_full %>%
  mutate(WORST_GRADE = paste0("WG", trimws(WORST_GRADE))) %>%
  pivot_wider(id_cols = c(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE),
              names_from = WORST_GRADE,
              values_from = CNT,
              values_fill = 0)

# Ensure all wg columns exist
for (g in paste0("WG", 0:4)) {
  if (!g %in% names(shift_t)) shift_t[[g]] <- 0L
}

# Ensure column order
shift_t <- shift_t %>% select(TRTAN, TRTA, PARAMCD, PARAM, BL_GRADE, WG0, WG1, WG2, WG3, WG4)

report <- shift_t %>%
  mutate(BL_LABEL = case_when(
    trimws(BL_GRADE) == "0" ~ "Normal",
    trimws(BL_GRADE) == "1" ~ "Grade 1",
    trimws(BL_GRADE) == "2" ~ "Grade 2",
    trimws(BL_GRADE) == "3" ~ "Grade 3",
    trimws(BL_GRADE) == "4" ~ "Grade 4",
    TRUE ~ trimws(BL_GRADE)
  )) %>%
  # Replace missing with 0 (already filled)
  mutate(across(starts_with("WG"), ~ ifelse(is.na(.), 0, .))) %>%
  mutate(TOTAL = WG0 + WG1 + WG2 + WG3 + WG4) %>%
  arrange(TRTAN, PARAMCD, BL_GRADE)

# Ensure numeric integer
report <- report %>%
  mutate(across(c(WG0,WG1,WG2,WG3,WG4,TOTAL), ~ as.integer(.)))

cat("Report rows:", nrow(report), "\n")
print(head(report))

# Step 9: Generate output
# Titles/footnotes (from u_titles.sas)
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

# Write CSV of table body
out_csv <- file.path(out, "t_lb_shift.csv")
write.csv(report, out_csv, row.names = FALSE, na = "")

cat("Wrote CSV:", out_csv, "\n")

# Build text rendering
out_txt <- file.path(out, "t_lb_shift.txt")
con <- file(out_txt, open = "wt")

writeLines(paste0(sponsor, strrep(" ", 60), "Page 1 of 1"), con)
writeLines(paste0("Protocol: ", protocol), con)
writeLines("", con)
writeLines("Table 14.3.4", con)
writeLines("Shift Table for Laboratory Parameters", con)
writeLines("Safety Analysis Set", con)
writeLines(strrep("-", 120), con)
writeLines(paste0("Data cutoff: ", cutdate), con)
writeLines("Source: ADaM datasets (ADLB, ADSL)", con)
writeLines("", con)

# For each treatment group, print header and tables per parameter
# Create lookup for Big N header
bign_lookup <- bign_df %>% arrange(TRTAN)
# If missing TRTAN labels, create generic
if (nrow(bign_lookup) == 0) {
  bign_lookup <- tibble(TRTA = c("Placebo","[Drug] Low Dose","[Drug] High Dose"), TRTAN = 1:3, BIGN = c(NA,NA,NA))
}

for (t in sort(unique(report$TRTAN))) {
  trt_label <- bign_lookup %>% filter(TRTAN == t) %>% pull(TRTA)
  if (length(trt_label) == 0) trt_label <- paste0("TRTAN=", t)
  bign_val <- bign_lookup %>% filter(TRTAN == t) %>% pull(BIGN)
  if (length(bign_val)==0) bign_val <- NA
  hdr <- if (!is.na(bign_val)) paste0(trt_label, " (N=", bign_val, ")") else trt_label
  writeLines("", con)
  writeLines(paste0("Treatment Group: ", hdr, "  [TRTAN=", t, "]"), con)
  writeLines(strrep("=", 120), con)

  sub_rep <- report %>% filter(TRTAN == t) %>% arrange(PARAMCD, BL_GRADE)

  # If no data for this TRTAN, note
  if (nrow(sub_rep) == 0) {
    writeLines("No data for this treatment group.", con)
    next
  }

  params <- unique(sub_rep$PARAMCD)
  for (pcd in params) {
    p_sub <- sub_rep %>% filter(PARAMCD == pcd)
    p_name <- unique(p_sub$PARAM)
    if (length(p_name)==0) p_name <- pcd
    writeLines("", con)
    writeLines(paste0("Parameter: ", pcd, " - ", p_name), con)
    # Header row
    hdr_line <- sprintf("%-15s | %8s | %8s | %8s | %8s | %8s | %8s",
                        "Baseline Grade", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")
    writeLines(hdr_line, con)
    writeLines(paste0(strrep("-",15), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8)), con)
    for (i in seq_len(nrow(p_sub))) {
      row <- p_sub[i,]
      line <- sprintf("%-15s | %8d | %8d | %8d | %8d | %8d | %8d",
                      row$BL_LABEL, row$WG0, row$WG1, row$WG2, row$WG3, row$WG4, row$TOTAL)
      writeLines(line, con)
    }
    # Column totals for this parameter (optional, but SAS total row is per baseline grade; we already have row totals)
    # Add parameter total row
    tot_wg0 <- sum(p_sub$WG0)
    tot_wg1 <- sum(p_sub$WG1)
    tot_wg2 <- sum(p_sub$WG2)
    tot_wg3 <- sum(p_sub$WG3)
    tot_wg4 <- sum(p_sub$WG4)
    tot_tot <- sum(p_sub$TOTAL)
    writeLines(paste0(strrep("-",15), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8), "-+-", strrep("-",8)), con)
    line_tot <- sprintf("%-15s | %8d | %8d | %8d | %8d | %8d | %8d",
                        "Total", tot_wg0, tot_wg1, tot_wg2, tot_wg3, tot_wg4, tot_tot)
    writeLines(line_tot, con)
  }
}

writeLines("", con)
writeLines(strrep("-", 120), con)
writeLines("Grades based on NCI-CTCAE v[X.X]", con)
writeLines("Shift from baseline grade to worst post-baseline grade.", con)
writeLines(paste0("Program: P074_t_lb_shift.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M")), con)
close(con)
cat("Wrote TXT:", out_txt, "\n")

# Also create RTF copy (plain text inside RTF wrapper) for expected output name
out_rtf <- file.path(out, "t_lb_shift.rtf")
# Simple RTF wrapper containing the same text
rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0\\fmodern Courier;}}\n\\viewkind4\\uc1\\pard\\f0\\fs18\n"
rtf_footer <- "\n\\par\n}"
txt_content <- readLines(out_txt, warn = FALSE)
# Escape RTF special chars
esc <- function(x) {
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\\{", "\\\\{", x)
  x <- gsub("\\}", "\\\\}", x)
  x
}
rtf_body <- paste(esc(txt_content), collapse="\\par\n")
writeLines(paste0(rtf_header, rtf_body, rtf_footer), out_rtf)
cat("Wrote RTF:", out_rtf, "\n")

cat("P074 t_lb_shift completed successfully.\n")
```