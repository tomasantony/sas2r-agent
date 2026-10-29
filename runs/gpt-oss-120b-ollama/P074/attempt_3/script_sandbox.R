# ------------------------------------------------------------
# R translation of SAS program: P074_t_lb_shift.sas
# Generates t_lb_shift table (CSV and TXT) in OUT_DIR
# ------------------------------------------------------------

# Load required packages (pre‑installed)
library(haven)      # read_sas
library(dplyr)      # data manipulation
library(tidyr)      # pivot_wider, crossing
library(stringr)    # string handling
library(readr)      # write_csv (optional)
library(purrr)      # map functions
library(tibble)     # tibble utilities

# ------------------------------------------------------------------
# Helper: ensure output directory exists
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" ) stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Read ADaM datasets
# ------------------------------------------------------------------
adam_path <- "/bench/data/raw/own-synthetic/adam"

adsl <- read_sas(file.path(adam_path, "adsl.sas7bdat"))
adlb <- read_sas(file.path(adam_path, "adlb.sas7bdat"))

# ------------------------------------------------------------------
# 2. Big N counts (Safety Analysis Set, SAFFL='Y')
# ------------------------------------------------------------------
big_n <- adsl %>%
  filter(toupper(as.character(SAFFL)) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# Total N across all treatments
big_n_total <- adsl %>%
  filter(toupper(as.character(SAFFL)) == "Y") %>%
  summarise(bign = n_distinct(USUBJID)) %>%
  pull(bign)

# Number of treatment groups (macro variable ntrt)
ntrt <- nrow(big_n)

# ------------------------------------------------------------------
# 3. Filter lab data to safety set and selected parameters
# ------------------------------------------------------------------
param_list <- c('ALT', 'AST', 'BILI', 'CREAT', 'HGB', 'WBC', 'PLAT', 'NEUT')

# Bring SAFFL from adsl and rename to avoid duplicate column names
adsl_saffl <- adsl %>% select(USUBJID, SAFFL) %>% rename(SAFFL_adsl = SAFFL)

lb <- adlb %>%
  inner_join(adsl_saffl, by = "USUBJID") %>%               # adds SAFFL_adsl
  filter(toupper(as.character(SAFFL_adsl)) == "Y",
         PARAMCD %in% param_list) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)

# ------------------------------------------------------------------
# 4. Baseline grade per subject/parameter
# ------------------------------------------------------------------
bl_grade <- lb %>%
  filter(ABLFL == "Y",
         !is.na(BTOXGR),
         BTOXGR != "") %>%
  distinct(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, .keep_all = TRUE) %>%
  transmute(
    USUBJID,
    TRTA,
    TRTAN,
    PARAMCD,
    PARAM,
    bl_grade = as.character(BTOXGR)
  )

# ------------------------------------------------------------------
# 5. Worst post‑baseline grade
# ------------------------------------------------------------------
worst_grade <- lb %>%
  filter(ABLFL != "Y",
         !is.na(ATOXGR),
         ATOXGR != "",
         AVISITN > 0) %>%
  mutate(
    worst_grade_n = suppressWarnings(as.numeric(ATOXGR))
  ) %>%
  filter(!is.na(worst_grade_n)) %>%                     # drop rows where conversion failed
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(
    worst_grade_n = max(worst_grade_n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    worst_grade = ifelse(worst_grade_n == 0, "0",
                         as.character(worst_grade_n))
  ) %>%
  select(-worst_grade_n)

# ------------------------------------------------------------------
# 6. Merge baseline and worst grades
# ------------------------------------------------------------------
shift <- bl_grade %>%
  inner_join(worst_grade,
             by = c("USUBJID", "TRTA", "TRTAN", "PARAMCD")) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

# ------------------------------------------------------------------
# 7. Count subjects in each shift cell
# ------------------------------------------------------------------
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

# ------------------------------------------------------------------
# 8. Create all possible grade combinations (0‑4) for each
#    treatment/parameter pair
# ------------------------------------------------------------------
grades <- tibble(grade = as.character(0:4))

# Distinct treatment‑parameter combos present in the data
param_combos <- shift_cnt %>%
  distinct(TRTA, TRTAN, PARAMCD, PARAM)

# Cross‑join to generate every cell
all_cells <- param_combos %>%
  crossing(bl_grade = grades$grade, worst_grade = grades$grade)

# Merge actual counts, replace missing with 0
shift_full <- all_cells %>%
  left_join(shift_cnt,
            by = c("TRTA", "TRTAN", "PARAMCD", "PARAM",
                   "bl_grade", "worst_grade")) %>%
  mutate(cnt = replace_na(cnt, 0L)) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

# ------------------------------------------------------------------
# 9. Pivot worst grades into separate columns (wg0‑wg4)
# ------------------------------------------------------------------
shift_t <- shift_full %>%
  select(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade, cnt) %>%
  pivot_wider(
    id_cols = c(TRTA, TRTAN, PARAMCD, PARAM, bl_grade),
    names_from = worst_grade,
    values_from = cnt,
    names_prefix = "wg",
    values_fill = list(cnt = 0L)
  )

# Ensure columns wg0‑wg4 exist (pivot_wider may omit columns with all zeros)
for (g in 0:4) {
  col_name <- paste0("wg", g)
  if (!col_name %in% colnames(shift_t)) {
    shift_t[[col_name]] <- 0L
  }
}

# ------------------------------------------------------------------
# 10. Add baseline label, row totals
# ------------------------------------------------------------------
report <- shift_t %>%
  mutate(
    bl_label = case_when(
      bl_grade == "0" ~ "Normal",
      bl_grade == "1" ~ "Grade 1",
      bl_grade == "2" ~ "Grade 2",
      bl_grade == "3" ~ "Grade 3",
      bl_grade == "4" ~ "Grade 4",
      TRUE           ~ bl_grade
    ),
    total = wg0 + wg1 + wg2 + wg3 + wg4
  ) %>%
  select(
    TRTAN, TRTA, PARAMCD, PARAM, bl_label,
    wg0, wg1, wg2, wg3, wg4, total
  ) %>%
  arrange(TRTAN, PARAMCD, bl_label)

# ------------------------------------------------------------------
# 11. Write CSV output (table body)
# ------------------------------------------------------------------
csv_path <- file.path(out_dir, "t_lb_shift.csv")
write.csv(report, csv_path, row.names = FALSE, na = "")

# ------------------------------------------------------------------
# 12. Create plain‑text rendering (approximate PROC REPORT layout)
# ------------------------------------------------------------------
txt_path <- file.path(out_dir, "t_lb_shift.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

# Title lines (mirroring SAS titles)
writeLines("Table 14.3.4  Shift Table for Laboratory Parameters", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)  # blank line

# Fixed column widths for text output
col_w <- list(
  PARAM   = 20,
  bl_label= 12,
  wg0     = 8,
  wg1     = 8,
  wg2     = 8,
  wg3     = 8,
  wg4     = 8,
  total   = 8
)

# Header line
header_line <- sprintf(
  "%-*s %-*s %*s %*s %*s %*s %*s %*s",
  col_w$PARAM,   "Parameter",
  col_w$bl_label,"Baseline Grade",
  col_w$wg0,    "Normal",
  col_w$wg1,    "Grade 1",
  col_w$wg2,    "Grade 2",
  col_w$wg3,    "Grade 3",
  col_w$wg4,    "Grade 4",
  col_w$total,  "Total"
)

sep_line <- strrep("-", sum(unlist(col_w)) + 6)

# Loop over treatment groups
for (t in sort(unique(report$TRTAN))) {
  trt_label <- unique(report$TRTA[report$TRTAN == t])
  writeLines(paste0("Treatment Group: ", trt_label, " (TRTAN=", t, ")"), con)
  writeLines(header_line, con)
  writeLines(sep_line, con)
  
  sub_tbl <- report %>% filter(TRTAN == t) %>% arrange(PARAMCD, bl_label)
  params <- unique(sub_tbl$PARAMCD)
  
  for (p in params) {
    rows_p <- sub_tbl %>% filter(PARAMCD == p)
    for (i in seq_len(nrow(rows_p))) {
      r <- rows_p[i, ]
      line <- sprintf(
        "%-*s %-*s %*d %*d %*d %*d %*d %*d %*d",
        col_w$PARAM,   as.character(r$PARAM),
        col_w$bl_label,as.character(r$bl_label),
        col_w$wg0,   r$wg0,
        col_w$wg1,   r$wg1,
        col_w$wg2,   r$wg2,
        col_w$wg3,   r$wg3,
        col_w$wg4,   r$wg4,
        col_w$total, r$total
      )
      writeLines(line, con)
    }
    writeLines("", con)  # blank line between parameters
  }
  writeLines("", con)  # extra space before next treatment
}

close(con)

# ------------------------------------------------------------------
# 13. Completion messages (console)
# ------------------------------------------------------------------
cat("t_lb_shift table generated: CSV ->", csv_path, "\n")
cat("t_lb_shift table generated: TXT ->", txt_path, "\n")
