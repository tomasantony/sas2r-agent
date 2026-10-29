```r
# ------------------------------------------------------------
# R translation of SAS program P069_t_ae_sum.sas
# Produces table t_ae_sum as CSV and plain-text files
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)      # for reading SAS datasets
library(dplyr)      # data manipulation
library(tidyr)      # reshaping
library(stringr)   # string handling
library(purrr)      # functional programming
library(readr)      # writing files

# Helper: SAS-style round (half away from zero)
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  pos  <- x >= 0
  res  <- ifelse(pos,
                 floor(x * mult + 0.5) / mult,
                 ceiling(x * mult - 0.5) / mult)
  res
}

# ------------------------------------------------------------------
# Set up paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") out_dir <- "."   # fallback to current directory

# ADaM library location (as defined in the SAS libname)
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# ------------------------------------------------------------------
# Read input ADaM datasets
# ------------------------------------------------------------------
adsl <- read_sas(file.path(adam_path, "adsl.sas7bdat"))
adae <- read_sas(file.path(adam_path, "adae.sas7bdat"))

# Ensure character columns are plain character (haven may give labelled vectors)
adsl <- mutate_all(adsl, as.character)
adae <- mutate_all(adae, as.character)

# ------------------------------------------------------------------
# Step 1: Big N (total subjects per treatment in safety set)
# ------------------------------------------------------------------
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

# Total N (used only for header, not for the table itself)
bigntot <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  summarise(N = n_distinct(USUBJID)) %>%
  pull(N)

# Build column header strings (SAS uses "~" as split character)
col_hdrs <- paste0(bign_df$TRTA, " (N=", bign_df$bign, ")")
col1hdr <- col_hdrs[1]
col2hdr <- col_hdrs[2]
col3hdr <- col_hdrs[3]

# ------------------------------------------------------------------
# Step 2 & 3: AE categories
# ------------------------------------------------------------------
# Base safety set with TEAE flag = 'Y'
adae_teae <- adae %>%
  inner_join(adsl %>% filter(toupper(SAFFL) == "Y"),
             by = "USUBJID",
             suffix = c(".ae", ".adsl")) %>%
  filter(TRTEMFL == "Y")

# Category definitions (order, label, filter expression)
cat_defs <- list(
  list(ord = 0, label = "Subjects with no TEAE", filter_expr = NULL),
  list(ord = 1, label = "Subjects with at least one TEAE", filter_expr = quote(TRUE)),
  list(ord = 2, label = "  Related TEAE",
       filter_expr = quote(AEREL %in% c('POSSIBLE','PROBABLE','DEFINITE','RELATED'))),
  list(ord = 3, label = "  Grade >=3 TEAE",
       filter_expr = quote(!is.na(AETOXGR) & as.numeric(AETOXGR) >= 3)),
  list(ord = 4, label = "  Serious Adverse Event",
       filter_expr = quote(AESER == 'Y')),
  list(ord = 5, label = "  Related Serious Adverse Event",
       filter_expr = quote(AESER == 'Y' &
                           AEREL %in% c('POSSIBLE','PROBABLE','DEFINITE','RELATED'))),
  list(ord = 6, label = "  TEAE Leading to Study Drug Discontinuation",
       filter_expr = quote(AEACN == 'DRUG WITHDRAWN')),
  list(ord = 7, label = "  TEAE Leading to Dose Modification",
       filter_expr = quote(AEACN %in% c('DOSE REDUCED','DRUG INTERRUPTED','DOSE INCREASED'))),
  list(ord = 8, label = "  TEAE with Outcome of Death",
       filter_expr = quote(AEOUT == 'FATAL'))
)

# Function to compute counts per treatment for a given filter
calc_counts <- function(df, filter_expr) {
  if (is.null(filter_expr)) {
    # special case for "no TEAE"
    df %>%
      left_join(
        adae %>% filter(TRTEMFL == 'Y') %>% distinct(USUBJID),
        by = "USUBJID"
      ) %>%
      filter(is.na(USUBJID.y)) %>%
      group_by(TRTA) %>%
      summarise(cnt = n_distinct(USUBJID), .groups = "drop")
  } else {
    df %>%
      filter(eval(filter_expr, envir = .)) %>%
      group_by(TRTA) %>%
      summarise(cnt = n_distinct(USUBJID), .groups = "drop")
  }
}

# Compute each category and bind together
all_ae_list <- map(cat_defs, function(cd) {
  cnt_df <- if (cd$ord == 0) {
    # subjects with no TEAE
    calc_counts(adsl %>% filter(toupper(SAFFL) == "Y"), NULL)
  } else {
    calc_counts(adae_teae, cd$filter_expr)
  }
  cnt_df %>%
    mutate(category = cd$label,
           ord = cd$ord) %>%
    select(TRTA, cnt, category, ord)
})

all_ae <- bind_rows(all_ae_list)

# ------------------------------------------------------------------
# Step 5: Percentages and formatted value strings
# ------------------------------------------------------------------
all_ae2 <- all_ae %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

all_ae3 <- all_ae2 %>%
  mutate(pct_fmt = ifelse(is.na(pct), "",
                           sprintf("%.1f", round_half_up(pct, 1))),
         value = ifelse(cnt > 0,
                        paste0(cnt, " (", pct_fmt, ")"),
                        "0")) %>%
  select(ord, category, TRTA, value)

# ------------------------------------------------------------------
# Step 6: Transpose to wide format (columns per treatment)
# ------------------------------------------------------------------
report_wide <- all_ae3 %>%
  pivot_wider(names_from = TRTA,
              values_from = value,
              values_fill = list(value = "0")) %>%
  arrange(ord)

# Ensure columns appear in the same order as the Big N table
treat_order <- bign_df$TRTA
report_wide <- report_wide %>%
  select(ord, category, all_of(treat_order))

# Rename treatment columns to col1, col2, col3 (expected by downstream code)
col_names <- c("ord", "category", "col1", "col2", "col3")
names(report_wide) <- col_names

# ------------------------------------------------------------------
# Step 7: Output files
# ------------------------------------------------------------------
# CSV output (dataset)
write.csv(report_wide,
          file = file.path(out_dir, "t_ae_sum.csv"),
          row.names = FALSE,
          na = "")

# Plain‑text rendering (tab‑separated)
write.table(report_wide,
            file = file.path(out_dir, "t_ae_sum.txt"),
            sep = "\t",
            quote = FALSE,
            row.names = FALSE)

# (Optional) Save the header strings for reference – not required by spec
# writeLines(c(col1hdr, col2hdr, col3hdr), file.path(out_dir, "t_ae_sum_headers.txt"))

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
```