```r
# ------------------------------------------------------------
# R translation of %SUPP2PAR_v1 macro for AE + SUPPAE
# Expected output: aefull.csv in the directory given by OUT_DIR
# ------------------------------------------------------------

# Load required packages (already installed in the environment)
library(haven)   # read SAS datasets
library(dplyr)   # data manipulation
library(tidyr)   # pivot_wider
library(stringr) # string handling
library(readr)   # write CSV
library(purrr)   # functional helpers
library(lubridate)

# ------------------------------------------------------------
# Helper: round half up (SAS default)
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------
# Environment variables supplied by the benchmark
root_dir   <- "D:/meta_hackathon/sas2r-agent"
inlib_dir  <- file.path(root_dir, "data/derived/cdisc-pilot/adam")   # psdtm points here
out_dir    <- Sys.getenv("OUT_DIR")   # where all outputs must be written
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ------------------------------------------------------------
# Macro parameters (hard‑coded as in the driver at the end of the SAS file)
parent_name <- "AE"
supp_name   <- "SUPPAE"
out_name    <- "AEFULL"   # already upper‑case, matches SAS outname

# ------------------------------------------------------------
# Build full file paths (SAS datasets are .sas7bdat)
parent_path <- file.path(inlib_dir, paste0(parent_name, ".sas7bdat"))
supp_path   <- file.path(inlib_dir, paste0(supp_name, ".sas7bdat"))

# ------------------------------------------------------------
# ----------------------------------------------------------------
# 1.  Basic existence / non‑empty checks (early termination if needed)
# ----------------------------------------------------------------
if (!file.exists(parent_path)) {
  stop("Parent dataset does not exist: ", parent_path)
}
if (!file.exists(supp_path)) {
  # According to the macro, if SUPP does not exist we simply copy the parent.
  parent_df <- read_sas(parent_path) %>% as_tibble()
  write_csv(parent_df, file.path(out_dir, "aefull.csv"), na = "")
  quit(save = "no")
}

parent_df <- read_sas(parent_path) %>% as_tibble()
supp_df   <- read_sas(supp_path)   %>% as_tibble()

if (nrow(parent_df) == 0) {
  stop("Parent dataset is empty")
}
if (nrow(supp_df) == 0) {
  # Empty supplemental → simple copy of parent
  write_csv(parent_df, file.path(out_dir, "aefull.csv"), na = "")
  quit(save = "no")
}

# ------------------------------------------------------------
# 2. Required variables on parent: STUDYID, DOMAIN, USUBJID
required_parent_vars <- c("STUDYID", "DOMAIN", "USUBJID")
missing_parent <- setdiff(required_parent_vars, toupper(names(parent_df)))
if (length(missing_parent) > 0) {
  stop("Parent dataset missing required variable(s): ", paste(missing_parent, collapse = ", "))
}

# 3. Required variables on supplemental: STUDYID, RDOMAIN, USUBJID,
#    IDVAR, IDVARVAL, QNAM, QVAL, QLABEL
required_supp_vars <- c("STUDYID", "RDOMAIN", "USUBJID",
                       "IDVAR", "IDVARVAL", "QNAM", "QVAL", "QLABEL")
missing_supp <- setdiff(required_supp_vars, toupper(names(supp_df)))
if (length(missing_supp) > 0) {
  stop("Supplemental dataset missing required variable(s): ", paste(missing_supp, collapse = ", "))
}

# ------------------------------------------------------------
# 4. Keep only variables of interest and standardise column names to upper case
parent_df <- parent_df %>% rename_with(~ toupper(.x))
supp_df   <- supp_df   %>% rename_with(~ toupper(.x))

# ------------------------------------------------------------
# 5. Restrict supplemental to records that belong to the parent domain
supp_df <- supp_df %>% filter(RDOMAIN == toupper(parent_name))

if (nrow(supp_df) == 0) {
  # No matching RDOMAIN → simple copy of parent
  write_csv(parent_df, file.path(out_dir, "aefull.csv"), na = "")
  quit(save = "no")
}

# ------------------------------------------------------------
# 6. Separate records with missing key fields (RDOMAIN, USUBJID, QNAM)
supp_ignoring <- supp_df %>%
  filter(str_trim(RDOMAIN) == "" |
         str_trim(USUBJID) == "" |
         str_trim(QNAM)   == "")
supp_accept   <- supp_df %>%
  filter(!(str_trim(RDOMAIN) == "" |
           str_trim(USUBJID) == "" |
           str_trim(QNAM)   == ""))

if (nrow(supp_ignoring) > 0) {
  warning("Supplemental dataset contains ", nrow(supp_ignoring),
          " record(s) with empty RDOMAIN, USUBJID or QNAM – they are ignored.")
  # In SAS these ignored records are written to a dataset; we do not need to output them.
}

# ------------------------------------------------------------
# 7. Check for duplicate keys (USUBJID,RDOMAIN,IDVAR,IDVARVAL,QNAM)
dup_keys <- supp_accept %>%
  group_by(USUBJID, RDOMAIN, IDVAR, IDVARVAL, QNAM) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(dup_keys) > 0) {
  stop("Duplicate records found in supplemental on keys USUBJID,RDOMAIN,IDVAR,IDVARVAL,QNAM")
}

# ------------------------------------------------------------
# 8. Check that each QNAM has a unique QLABEL
qn_label_counts <- supp_accept %>%
  distinct(QNAM, QLABEL) %>%
  group_by(QNAM) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(qn_label_counts) > 0) {
  stop("Same QNAM occurs with different QLABEL values")
}

# ------------------------------------------------------------
# 9. Handle empty IDVAR / IDVARVAL cases (same logic as SAS)
supp_accept <- supp_accept %>%
  mutate(
    IDVAR   = str_trim(IDVAR),
    IDVARVAL= str_trim(IDVARVAL)
  ) %>%
  mutate(
    # case 1: both empty → replace with USUBJID
    IDVAR   = if_else(IDVAR == "" & IDVARVAL == "", "USUBJID", IDVAR),
    IDVARVAL= if_else(IDVAR == "USUBJID" & IDVARVAL == "", USUBJID, IDVARVAL)
  )

# Detect the error condition where IDVAR empty but IDVARVAL not empty (after above step)
if (any(supp_accept$IDVAR == "" & supp_accept$IDVARVAL != "")) {
  stop("IDVAR empty while IDVARVAL not empty – cannot continue")
}

# ------------------------------------------------------------
# 10. Determine distinct IDVAR values present in the supplemental
idvar_vals <- supp_accept %>%
  distinct(IDVAR) %>%
  pull(IDVAR)

# ------------------------------------------------------------
# 11. Verify that each IDVAR (except USUBJID) exists as a variable in the parent
parent_vars <- toupper(names(parent_df))
missing_idvars <- setdiff(idvar_vals[ idvar_vals != "USUBJID" ], parent_vars)
if (length(missing_idvars) > 0) {
  stop("IDVAR(s) not present on parent domain: ", paste(missing_idvars, collapse = ", "))
}

# ------------------------------------------------------------
# 12. Process each IDVAR subset, transpose, and merge back to parent
merged_list <- list()

for (cur_idvar in idvar_vals) {
  # Subset of supplemental for this IDVAR
  supp_sub <- supp_accept %>%
    filter(IDVAR == cur_idvar)

  # Determine the type of the ID variable on the parent (numeric vs character)
  if (cur_idvar == "USUBJID") {
    # No extra column needed – merge on USUBJID only
    supp_sub <- supp_sub %>%
      select(STUDYID, RDOMAIN, USUBJID, QNAM, QVAL)
  } else {
    # Parent variable type
    parent_type <- typeof(parent_df[[cur_idvar]])
    # Convert IDVARVAL to appropriate type
    supp_sub <- supp_sub %>%
      mutate(
        !!cur_idvar := if (parent_type %in% c("double", "integer")) {
          as.numeric(IDVARVAL)
        } else {
          as.character(IDVARVAL)
        }
      ) %>%
      select(STUDYID, RDOMAIN, USUBJID, !!sym(cur_idvar), QNAM, QVAL)
  }

  # Pivot QNAM -> wide columns with QVAL as values
  supp_wide <- supp_sub %>%
    pivot_wider(
      id_cols = c("STUDYID", "RDOMAIN", "USUBJID", if (cur_idvar != "USUBJID") cur_idvar else NULL),
      names_from = QNAM,
      values_from = QVAL,
      values_fn = list
    ) %>%
    # flatten possible list columns (should be length 1)
    mutate(across(everything(), ~ if (is.list(.x)) .x[[1]] else .x))

  # Prepare join keys
  join_by <- c("STUDYID", "DOMAIN", "USUBJID")
  if (cur_idvar != "USUBJID") join_by <- c(join_by, cur_idvar)

  # Ensure parent has a DOMAIN column (it should)
  if (!"DOMAIN" %in% names(parent_df)) {
    parent_df <- parent_df %>% mutate(DOMAIN = toupper(parent_name))
  }

  # Perform left join (parent rows kept)
  merged <- parent_df %>%
    left_join(supp_wide, by = join_by)

  merged_list[[cur_idvar]] <- merged
}

# ------------------------------------------------------------
# 13. Combine results from all IDVAR subsets
final_df <- bind_rows(merged_list) %>%
  # Remove possible duplicate rows that could arise when multiple IDVAR values map to same parent row
  distinct()

# ------------------------------------------------------------
# 14. Write the final dataset to CSV (no row names, missing values as empty strings)
output_path <- file.path(out_dir, "aefull.csv")
write_csv(final_df, output_path, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
```