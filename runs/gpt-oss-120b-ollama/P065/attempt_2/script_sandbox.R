# P065_adlb_qc translation to R (attempt 2)
# ----------------------------------------------------------
# Reproduces the SAS program P065_adlb_qc.sas.
# Reads SDTM LB and ADaM ADSL, applies the same data step logic,
# and writes the ADLB_QC dataset as CSV to OUT_DIR.
# ----------------------------------------------------------

# Load required packages (pre‑installed)
library(haven)      # read SAS datasets
library(dplyr)      # data manipulation
library(lubridate)  # date handling
library(purrr)      # functional helpers
library(stringr)    # string utilities (if needed)

# ------------------------------------------------------------------
# 1. Define input paths (hard‑coded as in the SAS program)
# ------------------------------------------------------------------
root <- "/bench"

lb_path   <- file.path(root, "data/derived/wxzhai/sdtm",   "lb.sas7bdat")
adsl_path <- file.path(root, "data/derived/wxzhai/adam",  "adsl.sas7bdat")

# Output directory (provided by the harness)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("Environment variable OUT_DIR is not set.")

# ------------------------------------------------------------------
# 2. Read source datasets
# ------------------------------------------------------------------
lb   <- read_sas(lb_path)   # sdtm.lb
adsl <- read_sas(adsl_path) # adam.adsl

# ------------------------------------------------------------------
# 3. Sort datasets exactly as SAS would
#    - Missing values sort first (na.last = FALSE)
#    - Character ordering is case‑sensitive (default in R)
# ------------------------------------------------------------------
lb_qc <- lb[order(lb$USUBJID,
                  lb$PARAM,
                  lb$LBDTC,
                  na.last = FALSE,
                  method = "radix"), ]

adsl_qc <- adsl[order(adsl$USUBJID,
                      na.last = FALSE,
                      method = "radix"), ]

# Keep only needed variables from ADSL
adsl_qc <- adsl_qc %>% select(USUBJID, RFSTDTC, TRT01P)

# ------------------------------------------------------------------
# 4. Merge (inner join) by USUBJID, keeping only rows present in both
# ------------------------------------------------------------------
merged <- inner_join(lb_qc, adsl_qc, by = "USUBJID")

# ------------------------------------------------------------------
# 5. Create SAS‑style date variables (LBDTC and RFSTDTC are yymmdd10.)
# ------------------------------------------------------------------
merged <- merged %>%
  mutate(
    lbdt   = ymd(LBDTC, quiet = TRUE),   # input(LBDTC, yymmdd10.)
    rfstdt = ymd(RFSTDTC, quiet = TRUE) # input(RFSTDTC, yymmdd10.)
  )

# ------------------------------------------------------------------
# 6. Compute BASE, ABLFL and CHG following the SAS DATA step logic
# ------------------------------------------------------------------
compute_baseline <- function(df) {
  # df is already sorted by LBDTC within each USUBJID/PARAM
  n <- nrow(df)
  BASE_vec <- rep(NA_real_, n)
  cur_base <- NA_real_
  for (i in seq_len(n)) {
    # Reset at first observation of each PARAM (SAS: if first.PARAM then BASE = .)
    if (i == 1) cur_base <- NA_real_
    # Update BASE when lbdt < rfstdt and AVAL not missing
    if (!is.na(df$lbdt[i]) && !is.na(df$rfstdt[i]) &&
        df$lbdt[i] < df$rfstdt[i] && !is.na(df$AVAL[i])) {
      cur_base <- df$AVAL[i]
    }
    BASE_vec[i] <- cur_base
  }
  df$BASE   <- BASE_vec
  df$ABLFL  <- ifelse(!is.na(df$lbdt) & !is.na(df$rfstdt) &
                        df$lbdt < df$rfstdt & df$AVAL == df$BASE,
                      "Y", "")
  df$CHG    <- ifelse(!is.na(df$BASE), df$AVAL - df$BASE, NA_real_)
  df
}

adlb_qc <- merged %>%
  arrange(USUBJID, PARAM, LBDTC) %>%          # ensure correct order for FIRST.PARAM logic
  group_by(USUBJID, PARAM) %>%
  group_modify(~ compute_baseline(.x)) %>%
  ungroup() %>%
  select(-lbdt, -rfstdt)                       # drop temporary SAS variables

# ------------------------------------------------------------------
# 7. Re‑order columns to match SAS output order
#    (all LB variables first, then RFSTDTC, TRT01P, BASE, ABLFL, CHG)
# ------------------------------------------------------------------
lb_vars <- names(lb)                     # original LB variable order
final_vars <- c(lb_vars, "RFSTDTC", "TRT01P", "BASE", "ABLFL", "CHG")
adlb_qc <- adlb_qc %>% select(all_of(final_vars))

# ------------------------------------------------------------------
# 8. Write the final ADLB_QC dataset as CSV
# ------------------------------------------------------------------
output_path <- file.path(out_dir, "adlb_qc.csv")
write.csv(
  adlb_qc,
  file = output_path,
  row.names = FALSE,
  na = ""   # SAS writes missing as blank
)

# End of script
