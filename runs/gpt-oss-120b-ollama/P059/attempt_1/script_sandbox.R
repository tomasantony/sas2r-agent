# P059_pk_param_summary.R
# Reimplementation of SAS program P059_pk_param_summary.sas in R
# ---------------------------------------------------------------
# Required libraries (already installed in the environment)
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(tidyr)      # reshape
library(stringr)   # string handling
library(lubridate) # dates (not used here)
library(purrr)      # functional programming
library(readr)      # write_csv
library(tibble)    # tibble helpers

# ------------------------------------------------------------------
# Helper functions -------------------------------------------------
# Round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  pos <- sign(x)
  absx <- abs(x)
  factor <- 10 ^ digits
  rounded <- floor(absx * factor + 0.5) / factor
  pos * rounded
}

# Format a number according to SAS picture width and decimals
# width is ignored for the string output (padding not required for CSV)
fmt_num <- function(x, width = NA, dec = 0) {
  if (is.na(x) || is.nan(x)) return("")
  x <- round_half_up(x, dec)
  fmt <- paste0("%.", dec, "f")
  sprintf(fmt, x)
}

# ------------------------------------------------------------------
# Paths ------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
xpt_path <- file.path(rawadam, "adpp.xpt")

# ------------------------------------------------------------------
# 1. Read source data -----------------------------------------------
adpp_raw <- read_xpt(xpt_path) %>% as_tibble()

# ------------------------------------------------------------------
# 2. Apply SAS WHERE clause -----------------------------------------
adpp <- adpp_raw %>%
  filter(
    PKFN == 1,
    ANL01FN != 1,
    paramcd %in% c("AUCINFO", "AUCLAST", "CMAX", "TMAX", "MRTLAST", "LAMZHL"),
    !is.na(PPSTRESN),
    str_replace_all(PPCATL, "\\s+", "") == "ANAL2"
  )

# ------------------------------------------------------------------
# 3. Create _PARAM variable (using $PK format) ----------------------
pk_fmt <- c(
  AUCINFO = "AUC",
  AUCTAU  = "AUCtau",
  AUCLAST = "AUC(0-tlast)",
  CMAX    = "Cmax",
  CMAXD   = "Cmax/Dose",
  MRTLAST = "MRT",
  TLAST   = "tlast",
  TMAX    = "tmax",
  CLRFO   = "CL/F",
  VZFO    = "VzF",
  LAMZ    = "lambdaZ",
  LAMZHL  = "t1/2",
  CLAST   = "Clast"
)

adpp <- adpp %>%
  mutate(
    _PARAM = {
      pk_part <- pk_fmt[as.character(PPTESTCD)]
      pp_resu_part <- str_trim(as.character(PPORRESU))
      raw <- paste0(pk_part, " (", pp_resu_part, ")")
      # COMPBL: compress multiple blanks to a single blank
      str_squish(raw)
    },
    AVAL_log = log(AVAL)
  )

# ------------------------------------------------------------------
# 4. Sort as SAS PROC SORT would (by character case‑sensitive order)
adpp <- adpp %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# ------------------------------------------------------------------
# 5. PROC UNIVARIATE equivalent (by groups) -------------------------
# Compute stats for AVAL and AVAL_log
adpp_sum <- adpp %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1   = sum(!is.na(AVAL)),
    n2   = sum(!is.na(AVAL_log)),
    mn1  = ifelse(n1 > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    mn2  = ifelse(n2 > 0, mean(AVAL_log, na.rm = TRUE), NA_real_),
    std1 = ifelse(n1 > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    std2 = ifelse(n2 > 1, sd(AVAL_log, na.rm = TRUE), NA_real_),
    cv1  = ifelse(mn1 != 0, (std1 / mn1) * 100, NA_real_),
    cv2  = ifelse(mn2 != 0, (std2 / mn2) * 100, NA_real_),
    med1 = ifelse(n1 > 0, median(AVAL, na.rm = TRUE), NA_real_),
    med2 = ifelse(n2 > 0, median(AVAL_log, na.rm = TRUE), NA_real_),
    min1 = ifelse(n1 > 0, min(AVAL, na.rm = TRUE), NA_real_),
    min2 = ifelse(n2 > 0, min(AVAL_log, na.rm = TRUE), NA_real_),
    max1 = ifelse(n1 > 0, max(AVAL, na.rm = TRUE), NA_real_),
    max2 = ifelse(n2 > 0, max(AVAL_log, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# ------------------------------------------------------------------
# 6. Transpose to the SAS “adpp_sum_t” layout ----------------------
# Helper to detect parameter class
param_type <- function(param) {
  p <- tolower(param)
  if (grepl("auc", p)) return("AUC")
  if (grepl("cmax", p)) return("CMAX")
  if (grepl("mrt", p)) return("MRT")
  if (grepl("t1/2", p) || grepl("t12", p)) return("HALF")
  if (grepl("tmax", p)) return("TMAX")
  return(NA_character_)
}

adpp_sum_t <- adpp_sum %>%
  rowwise() %>%
  mutate(
    ptype = param_type(`_PARAM`)
  ) %>%
  ungroup() %>%
  # Build rows for each s_order
  mutate(
    # 1 = n
    s_order = 1,
    value = fmt_num(n1, dec = 0)
  ) %>%
  bind_rows(
    # 2 = mean (sd)
    adpp_sum %>%
      rowwise() %>%
      mutate(
        s_order = 2,
        value = {
          p <- param_type(`_PARAM`)
          if (p == "AUC") {
            paste0(fmt_num(mn1, dec = 0), " (", fmt_num(std1, dec = 0), ")")
          } else if (p == "CMAX") {
            paste0(fmt_num(mn1, dec = 0), " (", fmt_num(std1, dec = 0), ")")
          } else if (p == "MRT") {
            paste0(fmt_num(mn1, dec = 1), " (", fmt_num(std1, dec = 2), ")")
          } else if (p == "HALF") {
            paste0(fmt_num(mn1, dec = 2), " (", fmt_num(std1, dec = 2), ")")
          } else {
            ""
          }
        }
      ) %>%
      select(-p) %>%
      ungroup()
  ) %>%
  bind_rows(
    # 3 = CV% mean (skip tmax)
    adpp_sum %>%
      rowwise() %>%
      mutate(
        s_order = 3,
        value = if (!grepl("tmax", tolower(`_PARAM`))) fmt_num(cv1, dec = 1) else ""
      ) %>%
      ungroup()
  ) %>%
  bind_rows(
    # 4 = geo-mean
    adpp_sum %>%
      rowwise() %>%
      mutate(
        s_order = 4,
        value = {
          p <- param_type(`_PARAM`)
          gm <- exp(mn2)
          if (p == "AUC") fmt_num(gm, dec = 0)
          else if (p == "CMAX") fmt_num(gm, dec = 0)
          else if (p == "MRT") fmt_num(gm, dec = 1)
          else if (p == "HALF") fmt_num(gm, dec = 2)
          else ""
        }
      ) %>%
      ungroup()
  ) %>%
  bind_rows(
    # 5 = CV% geo-mean (skip tmax)
    adpp_sum %>%
      rowwise() %>%
      mutate(
        s_order = 5,
        value = if (!grepl("tmax", tolower(`_PARAM`))) {
          cv_geo <- sqrt(exp(std2^2) - 1) * 100
          fmt_num(cv_geo, dec = 1)
        } else ""
      ) %>%
      ungroup()
  ) %>%
  bind_rows(
    # 6 = median
    adpp_sum %>%
      rowwise() %>%
      mutate(
        s_order = 6,
        value = {
          p <- param_type(`_PARAM`)
          if (p == "AUC") fmt_num(med1, dec = 0)
          else if (p == "CMAX") fmt_num(med1, dec = 0)
          else if (p == "MRT") fmt_num(med1, dec = 1)
          else if (p == "HALF") fmt_num(med1, dec = 2)
          else if (grepl("tmax", tolower(`_PARAM`))) fmt_num(med1, dec = 2)
          else ""
        }
      ) %>%
      ungroup()
  ) %>%
  bind_rows(
    # 7 = [min; max]
    adpp_sum %>%
      rowwise() %>%
      mutate(
        s_order = 7,
        value = {
          p <- param_type(`_PARAM`)
          if (p == "AUC") {
            paste0("[", fmt_num(min1, dec = 0), ";", fmt_num(max1, dec = 0), "]")
          } else if (p == "CMAX") {
            paste0("[", fmt_num(min1, dec = 0), ";", fmt_num(max1, dec = 0), "]")
          } else if (p == "MRT") {
            paste0("[", fmt_num(min1, dec = 2), ";", fmt_num(max1, dec = 1), "]")
          } else if (p == "HALF") {
            paste0("[", fmt_num(min1, dec = 2), ";", fmt_num(max1, dec = 1), "]")
          } else if (grepl("tmax", tolower(`_PARAM`))) {
            paste0("[", fmt_num(min1, dec = 2), ";", fmt_num(max1, dec = 2), "]")
          } else {
            ""
          }
        }
      ) %>%
      ungroup()
  ) %>%
  # Keep original grouping variables
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# ------------------------------------------------------------------
# 7. Write outputs --------------------------------------------------
# Dataset output (CSV)
write.csv(
  adpp_sum_t,
  file = file.path(out_dir, "adpp_sum_t.csv"),
  row.names = FALSE,
  na = ""
)

# Text rendering of the table (simple fixed‑width representation)
txt_file <- file.path(out_dir, "P059.lst.txt")
con <- file(txt_file, open = "wt", encoding = "UTF-8")
writeLines("P059 - Summary statistics for PK parameters", con)
writeLines(strrep("=", 80), con)

# Header line
header <- sprintf(
  "%-8s %-8s %-15s %-20s %-8s %2s %-15s %s",
  "ANLPRNT", "PPSPECL", "PPCATL", "TRTAN", "PPSDY", "St", "_PARAM", "VALUE"
)
writeLines(header, con)
writeLines(strrep("-", nchar(header)), con)

# Body
apply(adpp_sum_t, 1, function(row) {
  line <- sprintf(
    "%-8s %-8s %-15s %-20s %-8s %2s %-15s %s",
    row["ANLPRNT"], row["PPSPECL"], row["PPCATL"], row["TRTAN"],
    row["PPSDY"], row["s_order"], row["_PARAM"], row["value"]
  )
  writeLines(line, con)
})

close(con)

# Also write a CSV version of the listing (same as dataset)
write.csv(
  adpp_sum_t,
  file = file.path(out_dir, "P059.lst.csv"),
  row.names = FALSE,
  na = ""
)

# End of script
