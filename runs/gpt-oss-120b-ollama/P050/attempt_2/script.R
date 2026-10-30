# ------------------------------------------------------------
# R translation of P050_disposition_b2.sas (attempt 2)
# Produces dataset: disp.csv
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(stringr)
library(rlang)

# ------------------------------------------------------------------
# Helper: round half up (SAS default rounding)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor <- 10 ^ digits
  floor(absx * factor + 0.5) / factor * posneg
}

# ------------------------------------------------------------------
# Paths (hard‑coded from macro variables in the SAS program)
rawadam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
out_dir      <- Sys.getenv("OUT_DIR")

# ------------------------------------------------------------------
# 1. Read ADSL data (keep only needed variables, filter ITT)
adsl_raw <- read_xpt(file.path(rawadam_path, "adsl.xpt"))

adsl <- adsl_raw %>%
  select(USUBJID, TRT01AN, TRT01A, DCREASCD, ITTFL) %>%
  filter(ITTFL == "Y")

# ------------------------------------------------------------------
# 2. Create adsl1 (original rows + duplicate rows with Total label)
adsl_total <- adsl %>%
  mutate(TRT01AN = 999,
         TRT01A  = "Total")

adsl1 <- bind_rows(adsl, adsl_total)

# ------------------------------------------------------------------
# 3. Header N Count – total distinct subjects per treatment
tot <- adsl1 %>%
  group_by(TRT01AN) %>%
  summarise(tot = n_distinct(USUBJID), .groups = "drop")

# ------------------------------------------------------------------
# 4. Function to generate the three intermediate tables
make_disp <- function(filter_expr, const_label = NULL, col_name = NULL) {
  # filter rows according to the supplied expression (character)
  filtered <- adsl1 %>% filter(!!parse_expr(filter_expr))
  
  # attach total per treatment
  filtered <- filtered %>% left_join(tot, by = "TRT01AN")
  
  if (!is.null(const_label)) {
    # constant label supplied
    out <- filtered %>%
      group_by(TRT01AN, TRT01A, tot) %>%
      summarise(
        cnt  = n_distinct(USUBJID),
        per  = (cnt * 100) / tot,
        col1 = const_label,
        .groups = "drop"
      )
  } else if (!is.null(col_name)) {
    # use a column value (e.g., DCREASCD)
    out <- filtered %>%
      group_by(TRT01AN, TRT01A, tot, !!sym(col_name)) %>%
      summarise(
        cnt  = n_distinct(USUBJID),
        per  = (cnt * 100) / tot,
        col1 = !!sym(col_name),
        .groups = "drop"
      )
  } else {
    stop("Either const_label or col_name must be provided")
  }
  out
}

# ------------------------------------------------------------------
# 5. Generate the three component tables
comp <- make_disp(
  filter_expr = 'toupper(DCREASCD) == "COMPLETED"',
  const_label = "Completed the study"
) %>% mutate(_src = "comp")

disc <- make_disp(
  filter_expr = 'toupper(DCREASCD) != "COMPLETED"',
  const_label = "Discontinued"
) %>% mutate(_src = "disc")

all_ <- make_disp(
  filter_expr = 'toupper(DCREASCD) != "COMPLETED"',
  col_name = "DCREASCD"
) %>% mutate(_src = "all")

# ------------------------------------------------------------------
# 6. Combine tables, prepend two blanks to col1 for rows from "all"
disp <- bind_rows(comp, disc, all_) %>%
  arrange(TRT01AN, TRT01A, col1) %>%
  mutate(col1 = if_else(_src == "all",
                         paste0("  ", str_trim(col1)),
                         col1))

# ------------------------------------------------------------------
# 7. Create formatted nper string (SAS style: cnt(per%))
disp <- disp %>%
  mutate(
    per  = round_half_up(per, 1),
    cnt_fmt = sprintf("%3d", cnt),
    per_fmt = sprintf("%5.1f", per),
    nper   = paste0(cnt_fmt, "(", per_fmt, ")")
  ) %>%
  select(cnt, per, col1, TRT01AN, TRT01A, nper)

# ------------------------------------------------------------------
# 8. Write the final dataset
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write.csv(
  disp,
  file = file.path(out_dir, "disp.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
