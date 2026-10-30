# P074 t_lb_shift.sas -> R translation
# Table 14.3.4 Shift Table for Laboratory Parameters

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

# SAS round half away from zero helper
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

read_adam <- function(dir, name) {
  files <- list.files(dir, pattern = paste0("^", name, "\\."), ignore.case = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    stop(paste0("Dataset ", name, " not found in ", dir))
  }
  # prefer sas7bdat > xpt > sas7bdat-like > csv
  ord <- order(match(tolower(tools::file_ext(files)), c("sas7bdat", "xpt", "xptv8", "csv", "sas7bdat ")))
  files <- files[ord]
  f <- files[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext %in% c("xpt", "xptv8")) {
    df <- haven::read_xpt(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE, guess_max = 100000)
    df <- as.data.frame(df)
  } else {
    df <- haven::read_sas(f)
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap haven labels/formats but keep values
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      # keep underlying values, drop labels
      v <- as.vector(haven::zap_labels(haven::zap_formats(x)))
      df[[nm]] <- v
    }
  }
  df
}

cat("NOTE: Reading ADSL and ADLB\n")
adsl <- read_adam(adam_dir, "adsl")
adlb <- read_adam(adam_dir, "adlb")

# Ensure expected columns exist (case-insensitive check, SAS case-insensitive)
up_names_adsl <- toupper(names(adsl))
up_names_adlb <- toupper(names(adlb))
stopifnot(all(c("USUBJID","TRTA","TRTAN","SAFFL") %in% up_names_adsl))
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL") %in% up_names_adlb))
# standardize names to uppercase for processing (keep original case mapping)
names(adsl) <- toupper(names(adsl))
names(adlb) <- toupper(names(adlb))

# Helper to treat SAS missing char: NA or "" or all blanks
is_sas_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
is_sas_present_c <- function(x) !is_sas_missing_c(x)

#--- Step 1: Get Big N (m_counts) ---
cat("NOTE: Deriving Big N\n")
adsl_saff <- adsl %>% filter(!is.na(SAFFL) & toupper(trimws(SAFFL)) == "Y")
# distinct subjects per TRTA
bign_df <- adsl_saff %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop")
# need TRTAN for ordering: get min TRTAN per TRTA
trt_map <- adsl %>% group_by(TRTA) %>% summarise(TRTAN = min(TRTAN, na.rm = TRUE), .groups = "drop")
bign_df <- bign_df %>% left_join(trt_map, by = "TRTA") %>% arrange(TRTAN)
bigntot <- n_distinct(adsl_saff$USUBJID)
ntrt <- nrow(bign_df)
cat(paste0("NOTE: ntrt=", ntrt, " bigntot=", bigntot, "\n"))
for (i in seq_len(ntrt)) {
  cat(paste0("NOTE:   ", bign_df$TRTA[i], ": N=", bign_df$bign[i], "\n"))
}

# Treatment order by TRTAN
trt_order <- bign_df %>% arrange(TRTAN)
# Also ensure all TRTAN 1..ntrt mapping; use adsl distinct TRTAN/TRTA
trt_labels_full <- adsl %>% select(TRTA, TRTAN) %>% distinct() %>% arrange(TRTAN)

#--- Step 2: Derive lb ---
cat("NOTE: Deriving LB\n")
param_list <- c('ALT', 'AST', 'BILI', 'CREAT', 'HGB', 'WBC', 'PLAT', 'NEUT')
# inner join adlb with adsl SAFFL=Y on USUBJID ; filter PARAMCD
saff_subj <- adsl_saff %>% select(USUBJID) %>% distinct()
# drop/rename overlapping non-key columns before join to avoid .x/.y
# adlb has TRTA/TRTAN, adsl also has them; keep adlb's, so drop from saff_subj (only USUBJID kept)
lb <- adlb %>%
  inner_join(saff_subj, by = "USUBJID") %>%
  filter(PARAMCD %in% param_list)
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD") %in% names(lb)))
lb <- lb %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, ANL01FL) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

#--- Step 3: baseline grade ---
cat("NOTE: Baseline grades\n")
bl_grade <- lb %>%
  filter(!is.na(ABLFL) & trimws(as.character(ABLFL)) == "Y" & is_sas_present_c(BTOXGR)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, BTOXGR) %>%
  rename(bl_grade = BTOXGR) %>%
  # SAS SELECT DISTINCT + strip? keep as trimmed? SAS keeps original with blanks; grades are '0'..'4'
  # trim blanks to match join
  mutate(bl_grade = trimws(as.character(bl_grade))) %>%
  distinct()

#--- Step 4: worst post-baseline grade ---
cat("NOTE: Worst grades\n")
# ABLFL ne 'Y' in SAS includes missing; so condition is (is.na(ABLFL) | ABLFL != 'Y')
worst_src <- lb %>%
  filter((is.na(ABLFL) | trimws(as.character(ABLFL)) != "Y") &
           is_sas_present_c(ATOXGR) &
           !is.na(AVISITN) & AVISITN > 0)
# input(ATOXGR,best.) -> numeric
worst_src <- worst_src %>%
  mutate(.atox_n = suppressWarnings(as.numeric(trimws(as.character(ATOXGR)))))
# PROC SQL max ignores nulls; group by
worst_grade <- worst_src %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(worst_grade_n = if (all(is.na(.atox_n))) NA_real_ else max(.atox_n, na.rm = TRUE), .groups = "drop")
# worst_grade2
worst_grade2 <- worst_grade %>%
  mutate(worst_grade = ifelse(is.na(worst_grade_n), ".", trimws(as.character(as.integer(worst_grade_n))))) %>%
  # handle non-integer? use strip(put(best.)) -> for 0-4 same; fallback to as.character
  mutate(worst_grade = ifelse(is.na(worst_grade_n), ".",
                       ifelse(worst_grade_n == 0, "0", trimws(as.character(worst_grade_n))))) %>%
  # worst_grade_n integer-like; ensure '0'..'4'
  select(USUBJID, TRTA, TRTAN, PARAMCD, worst_grade_n, worst_grade)

#--- Step 5: Merge baseline and worst ---
cat("NOTE: Merging shift\n")
# inner join on USUBJID, PARAMCD, TRTA
# drop TRTAN from one side except keys? keep a's TRTAN; rename b's to avoid suffix
wg2r <- worst_grade2 %>% rename(TRTA_b = TRTA, TRTAN_b = TRTAN)
shift <- bl_grade %>%
  inner_join(wg2r, by = c("USUBJID" = "USUBJID", "PARAMCD" = "PARAMCD", "TRTA" = "TRTA_b")) %>%
  # keep a.TRTA etc; worst_grade from b
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  # trim
  mutate(bl_grade = trimws(as.character(bl_grade)),
         worst_grade = trimws(as.character(worst_grade))) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

#--- Step 6: Count ---
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

#--- Step 7: all cells ---
grades <- tibble(grade = as.character(0:4))
distinct_combos <- shift_cnt %>% select(TRTA, TRTAN, PARAMCD, PARAM) %>% distinct()
# cross join
all_cells <- distinct_combos %>%
  cross_join(grades %>% rename(bl_grade = grade)) %>%
  cross_join(grades %>% rename(worst_grade = grade))

shift_full <- all_cells %>%
  left_join(shift_cnt %>% select(TRTA, PARAMCD, bl_grade, worst_grade, cnt),
            by = c("TRTA" = "TRTA", "PARAMCD" = "PARAMCD", "bl_grade" = "bl_grade", "worst_grade" = "worst_grade")) %>%
  mutate(cnt = ifelse(is.na(cnt), 0L, as.integer(cnt))) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

#--- Step 8: transpose ---
# pivot wider
shift_t <- shift_full %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, worst_grade, cnt) %>%
  arrange(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  pivot_wider(names_from = worst_grade, values_from = cnt, names_prefix = "wg", values_fill = 0)

# ensure wg0..wg4 exist
for (g in paste0("wg", 0:4)) {
  if (!g %in% names(shift_t)) shift_t[[g]] <- 0L
}
# replace missing with 0 (should be none)
for (g in paste0("wg", 0:4)) {
  shift_t[[g]][is.na(shift_t[[g]])] <- 0L
}
report <- shift_t %>%
  mutate(bl_label = case_when(
    bl_grade == "0" ~ "Normal",
    bl_grade == "1" ~ "Grade 1",
    bl_grade == "2" ~ "Grade 2",
    bl_grade == "3" ~ "Grade 3",
    bl_grade == "4" ~ "Grade 4",
    TRUE ~ bl_grade
  )) %>%
  mutate(total = wg0 + wg1 + wg2 + wg3 + wg4) %>%
  # order by TRTAN, PARAMCD, bl_grade (SAS char order)
  arrange(TRTAN, PARAMCD, bl_grade) %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, bl_label, wg0, wg1, wg2, wg3, wg4, total)

# Write CSV of table body (report dataset)
write.csv(report, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")

#--- Step 9: text rendering ---
# Titles/footnotes verbatim
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)
run_time <- format(Sys.time(), "%H:%M")

title1_left <- sponsor
title2_left <- paste0("Protocol: ", protocol)
title4 <- "Table 14.3.4"
title5 <- "Shift Table for Laboratory Parameters"
title6 <- "Safety Analysis Set"
foot1 <- paste0("Data cutoff: ", cutdate, ".")
foot2 <- "Source: ADaM datasets"
foot3 <- paste0("Program: P074_t_lb_shift.sas | Run: ", run_date, " ", run_time)
foot4 <- "Grades based on NCI-CTCAE v[X.X]"
foot5 <- "Shift from baseline grade to worst post-baseline grade."

# Build text lines
lines <- c()
# header titles (first page)
# We will paginate: one page per treatment x parameter (break after PARAMCD / page)
# Enumerate pages
# Get ordered treatment list present in report
trt_present <- report %>% select(TRTAN, TRTA) %>% distinct() %>% arrange(TRTAN)
# attach Big N for header
trt_present <- trt_present %>% left_join(bign_df %>% select(TRTA, bign), by = "TRTA")
params_present <- report %>% select(PARAMCD, PARAM) %>% distinct() %>% arrange(PARAMCD)

# total pages = ntrt_present * nparam
page_total <- nrow(trt_present) * nrow(params_present)
page_no <- 0L

# helper to format header
add_page_header <- function(pno, ptot) {
  c(
    paste0(title1_left, "                                                  Page ", pno, " of ", ptot),
    title2_left,
    "",
    paste0("  ", title4),
    paste0("  ", title5),
    paste0("  ", title6),
    ""
  )
}

# column header lines
col_hdr1 <- sprintf("%-30s %-14s %8s %8s %8s %8s %8s %8s", "Parameter", "Baseline", "Worst Post-Baseline Grade", "", "", "", "", "")
col_hdr2 <- sprintf("%-30s %-14s %8s %8s %8s %8s %8s %8s", "", "Grade", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")

for (ti in seq_len(nrow(trt_present))) {
  t_trtan <- trt_present$TRTAN[ti]
  t_trta <- trt_present$TRTA[ti]
  t_bign <- trt_present$bign[ti]
  # treatment header string
  trt_hdr <- paste0("Treatment: ", t_trta, " (N=", t_bign, ")")
  # params for this treatment
  pars <- report %>% filter(TRTAN == t_trtan) %>% select(PARAMCD, PARAM) %>% distinct() %>% arrange(PARAMCD)
  for (pi in seq_len(nrow(pars))) {
    page_no <- page_no + 1L
    pcd <- pars$PARAMCD[pi]
    par <- pars$PARAM[pi]
    hdr <- add_page_header(page_no, page_total)
    lines <- c(lines, hdr, trt_hdr, paste0("Parameter: ", par, " (", pcd, ")"), "", col_hdr1, col_hdr2,
               paste0(strrep("-", 30), " ", strrep("-", 14), " ", strrep("-", 8), " ", strrep("-", 8), " ", strrep("-", 8), " ", strrep("-", 8), " ", strrep("-", 8), " ", strrep("-", 8)))
    sub <- report %>% filter(TRTAN == t_trtan, PARAMCD == pcd) %>% arrange(bl_grade)
    for (r in seq_len(nrow(sub))) {
      # Parameter shown only on first row of block? SAS PROC REPORT with ORDER shows grouped; emulate: show PARAM on first row, blank after
      param_cell <- if (r == 1) sprintf("%-30s", substr(par, 1, 30)) else sprintf("%-30s", "")
      line <- sprintf("%s %-14s %8d %8d %8d %8d %8d %8d",
                      param_cell,
                      sprintf("%-14s", sub$bl_label[r]),
                      as.integer(sub$wg0[r]), as.integer(sub$wg1[r]), as.integer(sub$wg2[r]),
                      as.integer(sub$wg3[r]), as.integer(sub$wg4[r]), as.integer(sub$total[r]))
      lines <- c(lines, line)
    }
    lines <- c(lines, "", foot1, foot2, foot3, foot4, foot5, "", paste0(strrep("=", 100)), "")
  }
}

# write txt
writeLines(lines, file.path(out, "t_lb_shift.txt"))

cat("NOTE: t_lb_shift completed successfully.\n")
