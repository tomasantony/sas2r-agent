```r
# ------------------------------------------------------------
# R translation of SAS program P063_event_gant_excel.sas
# Generates the GANT dataset (CSV) matching SAS output
# ------------------------------------------------------------

# Load required packages (pre‑installed)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# ------------------------------------------------------------------
# Setup output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------------
# 1. Create the test dummy data (as in the SAS DATA step)
# ------------------------------------------------------------------
dummy <- tibble::tribble(
  ~SUBJID, ~AETERM,               ~AESTDY, ~AEENDY,
  "AAA",   "Adverse Event1",      1,        5,
  "AAA",   "Adverse Event2",      3,        8,
  "BBB",   "Adverse Event1",      2,       30,
  "BBB",   "Adverse Event2",     -2,        3,
  "BBB",   "Adverse Event3",      4,       NA_real_,
  "CCC",   "Adverse Event1",     15,       20
)

# ------------------------------------------------------------------
# 2. Determine overall min and max day values (PROC SQL logic)
# ------------------------------------------------------------------
# Minimum of (AESTDY, 1) across all rows
min_val <- dummy %>%
  mutate(min_candidate = pmin(AESTDY, 1, na.rm = TRUE)) %>%
  summarise(min_overall = min(min_candidate, na.rm = TRUE)) %>%
  pull(min_overall)

# Maximum of (AESTDY, AEENDY) across all rows
max_val <- dummy %>%
  mutate(max_candidate = pmax(AESTDY,
                              ifelse(is.na(AEENDY), -Inf, AEENDY),
                              na.rm = TRUE)) %>%
  summarise(max_overall = max(max_candidate, na.rm = TRUE)) %>%
  pull(max_overall)

# Compute the array length exactly as SAS does:
# range = (max - min) + (0 < min)
range_len <- (max_val - min_val) + ifelse(min_val > 0, 1, 0)
range_len <- as.integer(range_len)   # should be 32 for the test data

# ------------------------------------------------------------------
# 3. Build the work dataset (__wk1) – create the Y/O flag array
# ------------------------------------------------------------------
make_ar_vec <- function(st, en, len) {
  ar <- rep("", len)   # empty string = missing in SAS
  for (i in seq_len(len)) {
    if (!is.na(st) && !is.na(en) && st <= i && i <= en) {
      ar[i] <- "Y"
    } else if (!is.na(st) && is.na(en) && st == i) {
      ar[i] <- "Y"
    } else if (!is.na(st) && is.na(en) && st <= i) {
      ar[i] <- "O"
    }
  }
  ar
}

wk1 <- dummy %>%
  rowwise() %>%
  mutate(
    # Adjust start (st) and end (en) when overall min < 1, mimicking SAS logic
    st = if (min_val < 1) {
      AESTDY - min_val + as.integer(AESTDY < 1)
    } else {
      AESTDY
    },
    en = if (min_val < 1) {
      if (!is.na(AEENDY)) {
        AEENDY - min_val + as.integer(AEENDY < 1)
      } else {
        NA_real_
      }
    } else {
      AEENDY
    },
    ar_vec = list(make_ar_vec(st, en, range_len))
  ) %>%
  ungroup()

# Expand the list‑column of flags into separate columns ar1 … arN
ar_wide <- wk1 %>%
  select(ar_vec) %>%
  mutate(row_id = row_number()) %>%
  unnest_wider(ar_vec, names_sep = "")

# Rename ar columns to ar1, ar2, … ar<range_len>
colnames(ar_wide)[-1] <- paste0("ar", seq_len(range_len))

# Combine original variables with the expanded array columns
gant_raw <- wk1 %>%
  select(st, en, SUBJID, AETERM, AESTDY, AEENDY) %>%
  bind_cols(ar_wide %>% select(-row_id))

# ------------------------------------------------------------------
# 4. Rename columns to match SAS output
#    - ID, EVENT, STDY, ENDY
#    - Day columns named by the actual day numbers (excluding 0)
# ------------------------------------------------------------------
# Day numbers sequence from min to max, dropping 0
day_seq <- seq(min_val, max_val)
day_seq <- day_seq[day_seq != 0]          # SAS skips day 0
stopifnot(length(day_seq) == range_len)  # sanity check

# Identify the ar columns
ar_names <- paste0("ar", seq_len(range_len))

# Rename ar columns to the day numbers (as character strings)
gant_renamed <- gant_raw %>%
  rename(ID = SUBJID,
         EVENT = AETERM,
         STDY = AESTDY,
         ENDY = AEENDY) %>%
  select(-st, -en) %>%                     # drop temporary variables
  rename_at(vars(all_of(ar_names)), ~ as.character(day_seq))

# Reorder columns: ID, EVENT, STDY, ENDY, then day columns in order
final_cols <- c("ID", "EVENT", "STDY", "ENDY", as.character(day_seq))
gant_df <- gant_renamed %>% select(all_of(final_cols))

# ------------------------------------------------------------------
# 5. Write the GANT dataset to CSV (as required output)
# ------------------------------------------------------------------
write_csv(gant_df, file.path(out_dir, "gant.csv"), na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```