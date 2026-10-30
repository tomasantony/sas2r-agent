# ------------------------------------------------------------
# R translation of SAS program P063_event_gant_excel.sas
# Generates the GANT dataset as CSV in the OUT_DIR directory
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# ------------------------------------------------------------------
# Setup output directory
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
# Ensure the directory exists (it should, but just in case)
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
# 2. Determine the overall min and max day values (PROC SQL logic)
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

# Compute the range length as SAS does:
# range = (max - min) + (0 < min)
range_len <- (max_val - min_val) + ifelse(min_val > 0, 1, 0)
range_len <- as.integer(range_len)  # ensure integer

# ------------------------------------------------------------------
# 3. Build the __wk1 work dataset (array of Y/O flags)
# ------------------------------------------------------------------
# Helper function to generate the character array for a single row
make_ar_vec <- function(st, en, range_len) {
  # Initialise with empty strings (equivalent to missing in SAS)
  ar <- rep("", range_len)

  for (i in seq_len(range_len)) {
    if (!is.na(st) && !is.na(en) && st <= i && i <= en) {
      ar[i] <- "Y"
    } else if (!is.na(st) && is.na(en) && st == i) {
      ar[i] <- "Y"
    } else if (!is.na(st) && is.na(en) && st <= i) {
      ar[i] <- "O"
    }
    # otherwise leave as empty string
  }
  ar
}

# Apply the SAS logic row‑by‑row
wk1 <- dummy %>%
  rowwise() %>%
  mutate(
    # Adjust start (st) and end (en) when overall min < 1
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
    # Generate the array of flags
    ar_vec = list(make_ar_vec(st, en, range_len))
  ) %>%
  ungroup()

# Expand the ar_vec list‑column into separate ar1 … arN columns
ar_cols <- wk1 %>%
  mutate(row_id = row_number()) %>%
  select(row_id, ar_vec) %>%
  unnest_wider(ar_vec, names_sep = "")

# Rename ar columns to ar1, ar2, … ar_range_len
colnames(ar_cols)[-1] <- paste0("ar", seq_len(range_len))

# Combine everything together
gant_df <- wk1 %>%
  select(st, en, SUBJID, AETERM, AESTDY, AEENDY) %>%
  bind_cols(ar_cols %>% select(-row_id))

# ------------------------------------------------------------------
# 4. Write the GANT dataset to CSV (as required output)
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "gant.csv")
write_csv(gant_df, output_path, na = "")

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
