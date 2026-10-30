# ------------------------------------------------------------
# R translation of %SUPP2PAR_v1 macro for AE + SUPPAE
# Expected output: aefull.csv in the directory given by OUT_DIR
# ------------------------------------------------------------

# Load required packages (pre‑installed in the execution environment)
library(haven)   # read SAS datasets
library(dplyr)   # data manipulation
library(tidyr)   # pivot_wider
library(stringr) # string handling
library(readr)   # write CSV
library(purrr)   # functional helpers
library(lubridate)

# ------------------------------------------------------------
# Helper: round half up (SAS default) – retained for completeness
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# ------------------------------------------------------------
# Environment variables supplied by the benchmark
root_dir   <- "D:/meta_hackathon/sas2r-agent"
rawadam_dir   <- file.path(root_dir, "data/raw/cdisc-pilot/adam")
rawsdtm_dir   <- file.path(root_dir, "data/raw/cdisc-pilot/sdtm")
derivedadam_dir <- file.path(root_dir, "data/derived/cdisc-pilot/adam")
out_dir    <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------
# Macro parameters (hard‑coded as in the driver at the end of the SAS file)
parent_name <- "AE"
supp_name   <- "SUPPAE"
out_name    <- "AEFULL"   # final dataset name (SAS would create AEFULL)

# ------------------------------------------------------------
# Helper to locate a SAS dataset across possible library directories
find_sas_path <- function(dsn) {
  # Order mirrors the macro's libref resolution:
  # 1) the library supplied via INLIB (psdtm) – in this benchmark that points to raw SDTM
  # 2) the derived ADaM directory (fallback used by the macro when INLIB is not supplied)
  # 3) the raw ADaM directory (just in case)
  candidates <- c(
    file.path(rawsdtm_dir, paste0(dsn, ".sas7bdat")),   # psdtm (raw SDTM)
    file.path(derivedadam_dir, paste0(dsn, ".sas7bdat")), # derived ADaM
    file.path(rawadam_dir, paste0(dsn, ".sas7bdat"))      # raw ADaM
  )
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  stop("Dataset ", dsn, " not found in any known library location")
}

# ------------------------------------------------------------
# Resolve full file paths for parent and supplemental datasets
parent_path <- find_sas_path(parent_name)
supp_path   <- find_sas_path(supp_name)

# ------------------------------------------------------------
# ----------------------------------------------------------------
# 1. Basic existence / non‑empty checks (early termination if needed)
# ----------------------------------------------------------------
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
parent_df <- parent_df %>% rename_with(~ toupper(.x))
missing_parent <- setdiff(required_parent_vars, toupper(names(parent_df)))
if (length(missing_parent) > 0) {
  stop("Parent dataset missing required variable(s): ", paste(missing_parent, collapse = ", "))
}

# 3. Required variables on supplemental: STUDYID, RDOMAIN, USUBJID,
#    IDVAR, IDVARVAL, QNAM, QVAL, QLABEL
required_supp_vars <- c("STUDYID", "RDOMAIN", "USUBJID",
                       "IDVAR", "IDVARVAL", "QNAM", "QVAL", "QLABEL")
supp_df <- supp_df %>% rename_with(~ toupper(.x))
missing_supp <- setdiff(required_supp_vars, toupper(names(supp_df)))
if (length(missing_supp) > 0) {
  stop("Supplemental dataset missing required variable(s): ", paste(missing_supp, collapse = ", "))
}

# ------------------------------------------------------------
# 4. Restrict supplemental to records that belong to the parent domain
supp_df <- supp_df %>% filter(RDOMAIN == toupper(parent_name))

if (nrow(supp_df) == 0) {
  # No matching RDOMAIN → simple copy of parent
  write_csv(parent_df, file.path(out_dir, "aefull.csv"), na = "")
  quit(save = "no")
}

# ------------------------------------------------------------
# 5. Separate records with missing key fields (RDOMAIN, USUBJID, QNAM)
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
  # SAS writes these to a *_IGNOR dataset; we omit that output here.
}

# ------------------------------------------------------------
# 6. Check for duplicate keys (USUBJID,RDOMAIN,IDVAR,IDVARVAL,QNAM)
dup_keys <- supp_accept %>%
  group_by(USUBJID, RDOMAIN, IDVAR, IDVARVAL, QNAM) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(dup_keys) > 0) {
  stop("Duplicate records found in supplemental on keys USUBJID,RDOMAIN,IDVAR,IDVARVAL,QNAM")
}

# ------------------------------------------------------------
# 7. Ensure each QNAM maps to a unique QLABEL
qn_label_counts <- supp_accept %>%
  distinct(QNAM, QLABEL) %>%
  group_by(QNAM) %>%
  filter(n() > 1) %>%
  ungroup()
if (nrow(qn_label_counts) > 0) {
  stop("Same QNAM occurs with different QLABEL values")
}

# ------------------------------------------------------------
# 8. Handle empty IDVAR / IDVARVAL cases (mirrors SAS logic)
supp_accept <- supp_accept %>%
  mutate(
    IDVAR   = str_trim(IDVAR),
    IDVARVAL= str_trim(IDVARVAL)
  ) %>%
  mutate(
    # If both empty → replace with USUBJID
    IDVAR   = if_else(IDVAR == "" & IDVARVAL == "", "USUBJID", IDVAR),
    IDVARVAL= if_else(IDVAR == "USUBJID" & IDVARVAL == "", USUBJID, IDVARVAL)
  )

# Detect the SAS error condition where IDVAR empty but IDVARVAL not empty
if (any(supp_accept$IDVAR == "" & supp_accept$IDVARVAL != "")) {
  stop("IDVAR empty while IDVARVAL not empty – cannot continue")
}

# ------------------------------------------------------------
# 9. Determine distinct IDVAR values present in the supplemental
idvar_vals <- supp_accept %>%
  distinct(IDVAR) %>%
  pull(IDVAR)

# ------------------------------------------------------------
# 10. Verify that each IDVAR (except USUBJID) exists as a variable in the parent
parent_vars <- toupper(names(parent_df))
missing_idvars <- setdiff(idvar_vals[idvar_vals != "USUBJID"], parent_vars)
if (length(missing_idvars) > 0) {
  stop("IDVAR(s) not present on parent domain: ", paste(missing_idvars, collapse = ", "))
}

# ------------------------------------------------------------
# 11. Process each IDVAR subset, transpose, and merge back to parent
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
    # Flatten possible list columns (should be length 1)
    mutate(across(everything(), ~ if (is.list(.x)) .x[[1]] else .x))

  # Ensure parent has a DOMAIN column (add if missing)
  if (!"DOMAIN" %in% names(parent_df)) {
    parent_df <- parent_df %>% mutate(DOMAIN = toupper(parent_name))
  }

  # Prepare join keys
  join_by <- c("STUDYID", "DOMAIN", "USUBJID")
  if (cur_idvar != "USUBJID") join_by <- c(join_by, cur_idvar)

  # Perform left join (parent rows retained)
  merged <- parent_df %>%
    left_join(supp_wide, by = join_by)

  merged_list[[cur_idvar]] <- merged
}

# ------------------------------------------------------------
# 12. Combine results from all IDVAR subsets and remove duplicates
final_df <- bind_rows(merged_list) %>%
  distinct()

# ------------------------------------------------------------
# 13. Write the final dataset to CSV (no row names, missing values as empty strings)
output_path <- file.path(out_dir, "aefull.csv")
write_csv(final_df, output_path, na = "")

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
