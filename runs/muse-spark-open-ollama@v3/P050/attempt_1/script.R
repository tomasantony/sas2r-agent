# P050_disposition_b2.sas -> R translation
# Output: dataset disp -> disp.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (ROUND)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  # SAS does half away from zero
  abs_x <- abs(x)
  # floor(abs_x*mult + 0.5 + 1e-9) / mult  (tiny epsilon for floating error)
  rounded_abs <- floor(abs_x * mult + 0.5 + 1e-9) / mult
  sign_x * rounded_abs
}

# Helper: SAS PUT with w.d format (right-aligned, width w, d decimals, round half up)
sas_put_wd <- function(x, w, d) {
  if (is.na(x)) return(strrep(" ", w))
  # round half up to d decimals
  rx <- round_half_up(x, d)
  # format with exactly d decimals
  # Use sprintf with fixed decimals, then pad left to width w
  # For d=0, no decimal point
  if (d == 0) {
    fmt <- sprintf("%.*f", 0, rx)
    # remove decimal part if any (sprintf with 0 gives no decimal)
    # Ensure no scientific
  } else {
    fmt <- sprintf(paste0("%.", d, "f"), rx)
  }
  # SAS width includes sign, digits, decimal point
  # If formatted string longer than w, SAS would overflow (print full), but we truncate? In practice w is enough.
  # Pad left with spaces to width w
  if (nchar(fmt, type = "bytes") < w) {
    fmt <- paste0(strrep(" ", w - nchar(fmt, type = "bytes")), fmt)
  }
  fmt
}

# Helper: find file among candidates
find_file <- function(candidates) {
  for (p in candidates) {
    if (file.exists(p)) return(p)
  }
  return(NA_character_)
}

# Locate adsl.xpt
candidates_adsl <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/adsl.xpt",
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/adsl.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adsl.xpt"),
  "data/raw/cdisc-pilot/adam/adsl.xpt",
  "data/derived/cdisc-pilot/adam/adsl.xpt",
  "./data/raw/cdisc-pilot/adam/adsl.xpt",
  "../data/raw/cdisc-pilot/adam/adsl.xpt",
  file.path(getwd(), "data/raw/cdisc-pilot/adam/adsl.xpt"),
  "/tmp/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
)
# Also try to search recursively for adsl.xpt under current working directory
search_adsl <- function() {
  # look in common locations
  possible_roots <- c(getwd(), dirname(getwd()), "D:/meta_hackathon/sas2r-agent", "/github/workspace", ".")
  for (r in possible_roots) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA_character_)
}

adsl_path <- find_file(candidates_adsl)
if (is.na(adsl_path)) {
  adsl_path <- search_adsl()
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  # Try haven's pharmaversesdtm or other fallback: search entire filesystem for adsl.xpt (limited)
  # Use Sys.getenv rawadam
  rawadam_env <- Sys.getenv("rawadam")
  if (rawadam_env != "" && file.exists(file.path(rawadam_env, "adsl.xpt"))) {
    adsl_path <- file.path(rawadam_env, "adsl.xpt")
  } else {
    rawadam2 <- Sys.getenv("RAWADAM")
    if (rawadam2 != "" && file.exists(file.path(rawadam2, "adsl.xpt"))) {
      adsl_path <- file.path(rawadam2, "adsl.xpt")
    }
  }
}
if (is.na(adsl_path) || !file.exists(adsl_path)) {
  stop(paste0("adsl.xpt not found. Tried candidates: ", paste(candidates_adsl, collapse="; ")))
}

message("Reading ADSL from: ", adsl_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# Read ADSL
adsl_raw <- haven::read_xpt(adsl_path)

# Normalize column names to lower case for matching (SAS is case-insensitive, haven preserves case)
names(adsl_raw) <- tolower(names(adsl_raw))

# Ensure required columns exist
required_cols <- c("usubjid", "trt01an", "trt01a", "dcreascd", "ittfl")
missing_cols <- setdiff(required_cols, names(adsl_raw))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in ADSL:", paste(missing_cols, collapse=", ")))
}

# Keep only needed and filter ittfl = "Y" (case-sensitive, SAS where ittfl="Y")
adsl <- adsl_raw %>%
  select(usubjid, trt01an, trt01a, dcreascd, ittfl) %>%
  filter(ittfl == "Y") %>%
  select(usubjid, trt01an, trt01a, dcreascd)

# Convert haven_labelled to plain vectors
adsl <- adsl %>%
  mutate(
    usubjid = as.character(usubjid),
    trt01an = as.numeric(trt01an),
    trt01a = as.character(trt01a),
    dcreascd = as.character(dcreascd)
  )

# Create total: adsl1 = adsl + total rows
adsl1 <- bind_rows(
  adsl,
  adsl %>% mutate(trt01an = 999, trt01a = "Total")
)

# Header N Count: tot table
tot <- adsl1 %>%
  group_by(trt01an) %>%
  summarise(tot = n_distinct(usubjid), .groups = "drop") %>%
  arrange(trt01an)

# For later join, keep tot per trt01an
# Also need to capture ntrt etc. not needed for output but for completeness

# Dummy dataset (not used in final disp, but create for fidelity)
dummy <- tibble(
  col1 = c("Completed the study",
           "Discontinued",
           " ",
           "  Death",
           "  Adverse Event",
           "  Lack of Efficacy",
           "  Lost to Follow-up",
           "  Non-compliance with Study Drug",
           "  Pregnancy",
           "  Protocal Violation",
           "  Physician Decision",
           "  Withdrawl by Subject",
           "  Withdrawl by Parent/Guardian",
           "  Recovery",
           "  Technical Problems",
           "  Other"),
  ord = 1:16
)

# Helper to emulate %disp macro
disp_macro <- function(where_expr, var_expr, var_is_literal = FALSE, literal_value = NULL) {
  # where_expr: function that takes df and returns logical vector
  # var_expr: either literal string or column name
  # Returns tibble with cnt, per, col1, trt01an, trt01a
  filtered <- adsl1 %>% filter(where_expr(.))
  if (nrow(filtered) == 0) {
    return(tibble(cnt = integer(0), per = numeric(0), col1 = character(0), trt01an = numeric(0), trt01a = character(0)))
  }
  if (var_is_literal) {
    filtered <- filtered %>% mutate(col1 = literal_value)
  } else {
    # var is column name, e.g., "dcreascd"
    filtered <- filtered %>% mutate(col1 = as.character(.data[[var_expr]]))
  }
  # Join tot to get denominator per trt01an
  # In SAS: from adsl1 as a, tot as b where a.trt01an = b.trt01an group by a.trt01an, trt01a, tot, col1
  # So cnt is distinct usubjid per trt01an, trt01a, col1
  # per = cnt*100/tot
  out <- filtered %>%
    left_join(tot, by = "trt01an") %>%
    group_by(trt01an, trt01a, tot, col1) %>%
    summarise(cnt = n_distinct(usubjid), .groups = "drop") %>%
    mutate(per = (cnt * 100) / tot) %>%
    select(cnt, per, col1, trt01an, trt01a) %>%
    arrange(trt01an, col1)
  out
}

# Define where conditions: upcase(dcreascd) = "COMPLETED" and ^= "COMPLETED"
# SAS upcase handles missing as "" -> not equal
where_completed <- function(df) {
  toupper(df$dcreascd) == "COMPLETED"
}
where_not_completed <- function(df) {
  # SAS ^= means not equal; missing will be not equal (since "" != "COMPLETED")
  # Also need to handle NA: in R, toupper(NA) is NA, comparison is NA -> filter will drop NA rows
  # In SAS, missing character is "" not NA, so it would be counted as discontinued
  # So we need to treat NA as not completed
  up <- toupper(df$dcreascd)
  is.na(up) | up != "COMPLETED"
}

comp <- disp_macro(where_completed, var_is_literal = TRUE, literal_value = "Completed the study")
disc <- disp_macro(where_not_completed, var_is_literal = TRUE, literal_value = "Discontinued")
all_df <- disp_macro(where_not_completed, var_expr = "dcreascd", var_is_literal = FALSE)

# Final disp: set comp disc all(in=a); by trt01an trt01a; if a then col1 = "  " || trim(left(col1)); nper = put(cnt,3.) || "(" || put(per,5.1) || ")"
# In R, we need to replicate: comp and disc keep col1 as is, all gets two leading spaces + trim(left(col1))
# trim(left(col1)) in SAS: left aligns (removes leading spaces) then trim removes trailing spaces
# So effectively trimws(col1, which="both") but left then trim only removes leading then trailing, same as trimws
# Then prepend two spaces

# For all_df, modify col1
all_mod <- all_df %>%
  mutate(col1 = paste0("  ", trimws(col1, which = "both")))

# Combine in order comp, disc, all_mod, then sort by trt01an, trt01a as SAS BY does
# SAS BY interleaves, but final order is by trt01an ascending, and within each trt01an, comp/disc/all order
# To replicate SAS interleaving, we can bind and arrange by trt01an, and then by a custom order:
# comp first, disc second, all third, and within all, alphabetical by col1 (which is already sorted)
# However SAS BY would interleave by trt01an, so we need to sort by trt01an, then by source order

# Create source indicator
comp <- comp %>% mutate(src_ord = 1)
disc <- disc %>% mutate(src_ord = 2)
all_mod <- all_mod %>% mutate(src_ord = 3)

combined <- bind_rows(comp, disc, all_mod) %>%
  arrange(trt01an, src_ord, col1) %>%
  select(-src_ord)

# Now create nper with SAS formatting
combined <- combined %>%
  mutate(
    nper = paste0(sas_put_wd(cnt, 3, 0), "(", sas_put_wd(per, 5, 1), ")")
  )

# Reorder columns to match SAS: cnt, per, col1, trt01an, trt01a, nper
# SAS dataset outds.disp has variables: cnt, per, col1, trt01an, trt01a, nper (in that order from proc sql + data step)
# Check: proc sql creates cnt, per, col1, trt01an, trt01a ; data step adds nper
disp <- combined %>%
  select(cnt, per, col1, trt01an, trt01a, nper)

# Ensure types: cnt integer, per numeric, col1 character, trt01an numeric, trt01a character, nper character
# Sort final by trt01an, and within each, keep SAS order (comp, disc, then all alphabetical)
# Already sorted

# Write dataset as CSV
write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

# Also write a plain-text rendering for verification (optional but helpful)
# Create txt with same numbers
txt_path <- file.path(out, "disp.txt")
sink(txt_path)
cat("Dataset: disp - Subject Disposition\n")
cat("====================================\n\n")
# Print tot header
cat("Header N counts (tot):\n")
print(tot)
cat("\n")
cat("Disp dataset:\n")
# Format per to 1 decimal for display
print(as.data.frame(disp), row.names = FALSE)
sink()

message("Wrote ", file.path(out, "disp.csv"), " with ", nrow(disp), " rows")
