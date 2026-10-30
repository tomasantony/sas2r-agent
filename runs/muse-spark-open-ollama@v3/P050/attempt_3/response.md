```r
# P050_disposition_b2.sas -> R translation
# Output: dataset disp -> disp.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# Helper: SAS round half away from zero (ROUND) - vectorized
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  rounded_abs <- floor(abs_x * mult + 0.5 + 1e-9) / mult
  result <- sign_x * rounded_abs
  result[is.na(x)] <- NA_real_
  result
}

# Helper: SAS PUT with w.d format (right-aligned, width w, d decimals, round half up) - vectorized
sas_put_wd <- function(x, w, d) {
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi)) return(strrep(" ", w))
    rx <- round_half_up(xi, d)
    rx1 <- rx[1]
    if (d == 0) {
      fmt <- sprintf("%.0f", rx1)
    } else {
      fmt <- sprintf(paste0("%.", d, "f"), rx1)
    }
    if (nchar(fmt, type = "bytes") < w) {
      paste0(strrep(" ", w - nchar(fmt, type = "bytes")), fmt)
    } else {
      fmt
    }
  }, character(1))
}

# Helper: find file among candidates
find_file <- function(candidates) {
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
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
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adsl.xpt"
)
search_adsl <- function() {
  possible_roots <- c(getwd(), dirname(getwd()), "D:/meta_hackathon/sas2r-agent", "/github/workspace", ".", "/bench")
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
  filtered <- adsl1 %>% filter(where_expr(.))
  if (nrow(filtered) == 0) {
    return(tibble(cnt = integer(0), per = numeric(0), col1 = character(0), trt01an = numeric(0), trt01a = character(0)))
  }
  if (var_is_literal) {
    filtered <- filtered %>% mutate(col1 = literal_value)
  } else {
    filtered <- filtered %>% mutate(col1 = as.character(.data[[var_expr]]))
  }
  out <- filtered %>%
    left_join(tot, by = "trt01an") %>%
    group_by(trt01an, trt01a, tot, col1) %>%
    summarise(cnt = n_distinct(usubjid), .groups = "drop") %>%
    mutate(per = (cnt * 100) / tot) %>%
    select(cnt, per, col1, trt01an, trt01a) %>%
    arrange(trt01an, col1)
  out
}

where_completed <- function(df) {
  toupper(df$dcreascd) == "COMPLETED"
}
where_not_completed <- function(df) {
  up <- toupper(df$dcreascd)
  is.na(up) | up != "COMPLETED"
}

comp <- disp_macro(where_completed, var_is_literal = TRUE, literal_value = "Completed the study")
disc <- disp_macro(where_not_completed, var_is_literal = TRUE, literal_value = "Discontinued")
all_df <- disp_macro(where_not_completed, var_expr = "dcreascd", var_is_literal = FALSE)

# For all_df, modify col1: "  " || trim(left(col1))
# SAS: data outds.disp; set comp disc all(in=a); by trt01an trt01a; if a then col1 = "  " || trim(left(col1));
# Variable attributes (length) are taken from first dataset in SET (comp).
# comp col1 = "Completed the study" length 19, so col1 length is 19 and longer values are truncated.
# Reproduce SAS truncation: col1 length 19
sas_col1_len <- nchar("Completed the study", type = "bytes")  # 19

all_mod <- all_df %>%
  mutate(col1 = paste0("  ", trimws(col1, which = "both"))) %>%
  mutate(col1 = substr(col1, 1, sas_col1_len))

# Also ensure comp/disc col1 truncated to same length (no effect but for fidelity)
comp <- comp %>% mutate(col1 = substr(col1, 1, sas_col1_len))
disc <- disc %>% mutate(col1 = substr(col1, 1, sas_col1_len))

# Combine in order comp, disc, all_mod, then sort by trt01an
comp <- comp %>% mutate(src_ord = 1)
disc <- disc %>% mutate(src_ord = 2)
all_mod <- all_mod %>% mutate(src_ord = 3)

combined <- bind_rows(comp, disc, all_mod) %>%
  arrange(trt01an, src_ord, col1) %>%
  select(-src_ord)

# Create nper with SAS formatting: put(cnt,3.) || "(" || put(per,5.1) || ")"
combined <- combined %>%
  mutate(
    nper = paste0(sas_put_wd(cnt, 3, 0), "(", sas_put_wd(per, 5, 1), ")")
  )

# Reorder columns to match SAS: cnt, per, col1, trt01an, trt01a, nper
disp <- combined %>%
  select(cnt, per, col1, trt01an, trt01a, nper)

# Write dataset as CSV
write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

# Also write a plain-text rendering for verification
txt_path <- file.path(out, "disp.txt")
sink(txt_path)
cat("Dataset: disp - Subject Disposition\n")
cat("====================================\n\n")
cat("Header N counts (tot):\n")
print(tot)
cat("\n")
cat("Disp dataset:\n")
print(as.data.frame(disp), row.names = FALSE)
sink()

message("Wrote ", file.path(out, "disp.csv"), " with ", nrow(disp), " rows")
```