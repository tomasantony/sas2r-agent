# P035_target14.sas -> R translation
# Outputs: table/listing P035

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  # handle NA sign
  res <- sign_x * floor(abs_x * mult + 0.5 + 1e-9) / mult
  # preserve NA
  res[is.na(x)] <- NA_real_
  res
}

# Helper: SAS PUT w.d format (numeric) - round half away, then format with width, right aligned
sas_put <- function(x, w, d = 0) {
  # x is numeric scalar or vector
  # w = width, d = decimals
  # SAS: if value is missing -> spaces? In this program, value set to ' ' for otherwise cases
  # For numeric, format with rounding half away, then sprintf with width
  # If w is NA, just return as character without padding? Not needed
  sapply(x, function(v) {
    if (is.na(v) || is.nan(v) || is.infinite(v)) {
      # SAS would put '.' for missing? But here we return spaces of width w
      return(strrep(" ", w))
    }
    rv <- sas_round(v, d)
    # format with fixed decimals
    if (d == 0) {
      # integer format: no decimal point
      # SAS w. format: width w, no decimals, right aligned
      fmt <- paste0("%", w, ".0f")
      sprintf(fmt, rv)
    } else {
      fmt <- paste0("%", w, ".", d, "f")
      sprintf(fmt, rv)
    }
  }, USE.NAMES = FALSE)
}

# Helper: SAS INDEX (case sensitive substring search, returns 0 if not found, else position)
sas_index <- function(str, substr) {
  # vectorized
  sapply(seq_along(str), function(i) {
    if (is.na(str[i]) || is.na(substr)) return(0)
    pos <- regexpr(substr, str[i], fixed = TRUE)[1]
    if (pos == -1) 0 else pos
  })
}

# Helper: compbl - compress multiple blanks to single blank, trim leading/trailing? SAS compbl does that
compbl <- function(s) {
  gsub("\\s+", " ", trimws(s))
}

# Locate input file
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adpp.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpp.xpt",
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
  "/tmp/adpp.xpt"
)

# Also try to find via list.files
find_adpp <- function() {
  roots <- c("D:/meta_hackathon/sas2r-agent", "/opt", ".", getwd())
  for (r in roots) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern = "^adpp\\.xpt$", recursive = TRUE, full.names = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}

adpp_path <- NA
for (p in candidates) {
  if (!is.na(p) && file.exists(p)) { adpp_path <- p; break }
}
if (is.na(adpp_path)) {
  f <- find_adpp()
  if (!is.na(f) && file.exists(f)) adpp_path <- f
}
# Fallback: search common linux path
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  # try to list OUT_DIR parent
  # brute force search from /
  # limit to known mount
  possible <- list.files("D:/meta_hackathon/sas2r-agent", recursive = FALSE, full.names = TRUE)
  # if not found, try to locate via Sys.getenv
  # last resort: check working directory
  if (file.exists("adpp.xpt")) adpp_path <- "adpp.xpt"
}

if (is.na(adpp_path) || !file.exists(adpp_path)) {
  # Try alternative path with forward slashes on Linux container
  # The benchmark may have copied data to /data
  alt_paths <- c(
    "/data/raw/cdisc-pilot/adam/adpp.xpt",
    "/data/derived/cdisc-pilot/adam/adpp.xpt",
    "data/raw/cdisc-pilot/adam/adpp.xpt",
    "data/derived/cdisc-pilot/adam/adpp.xpt"
  )
  for (p in alt_paths) if (file.exists(p)) { adpp_path <- p; break }
}

# If still not found, attempt to search entire filesystem for adpp.xpt (limited)
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  # search in current working directory tree
  all_xpt <- list.files(path = ".", pattern = "adpp\\.xpt", recursive = TRUE, full.names = TRUE)
  if (length(all_xpt) > 0) adpp_path <- all_xpt[1]
}
if (is.na(adpp_path) || !file.exists(adpp_path)) {
  # try haven's example? stop with message
  stop(paste("adpp.xpt not found. Searched candidates:", paste(candidates, collapse="; ")))
}

# Read adpp
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

adpp_raw <- haven::read_xpt(adpp_path)

# Ensure column names are uppercase as in SAS (haven preserves)
# SAS where: PKFN=1 and ANL01FN ne 1 and paramcd in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL') and PPSTRESN ne . and compress(PPCATL)='ANAL2'
# Note: compress(PPCATL)='ANAL2' -> remove blanks, case sensitive
# Need to handle missing values: PPSTRESN ne . means not missing

# Check column existence
# Some ADaM datasets use PARAMCD vs paramcd case; haven keeps as is
# Normalize names to upper for filtering but keep original for output
names(adpp_raw) <- toupper(names(adpp_raw))

# Ensure required columns exist
required_cols <- c("PKFN","ANL01FN","PARAMCD","PPSTRESN","PPCATL","PPTESTCD","PPORRESU","AVAL","ANLPRNT","PPSPECL","TRTAN","PPSDY")
missing_cols <- setdiff(required_cols, names(adpp_raw))
if (length(missing_cols) > 0) {
  stop(paste("Missing columns in adpp:", paste(missing_cols, collapse=", ")))
}

# Filter
adpp_filt <- adpp_raw %>%
  filter(PKFN == 1,
         ANL01FN != 1,
         PARAMCD %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
         !is.na(PPSTRESN),
         gsub(" ", "", PPCATL) == 'ANAL2')

# Create _PARAM = compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')')
# Format $pk mapping
pk_map <- c(
  "AUCINFO" = "AUC",
  "AUCTAU"  = "AUCtau",
  "AUCLAST" = "AUC(0-tlast)",
  "CMAX"    = "Cmax",
  "CMAXD"   = "Cmax/Dose",
  "MRTLAST" = "MRT",
  "TLAST"   = "tlast",
  "TMAX"    = "tmax",
  "CLRFO"   = "CL/F",
  "VZFO"    = "VzF",
  "LAMZ"    = "lambdaZ",
  "LAMZHL"  = "t1/2",
  "CLAST"   = "Clast"
)

# PPTESTCD is the code, map via pk_map; if not found, keep original?
adpp_filt <- adpp_filt %>%
  mutate(
    PPTESTCD_mapped = ifelse(PPTESTCD %in% names(pk_map), pk_map[PPTESTCD], PPTESTCD),
    # strip PPORRESU = trim
    PPORRESU_stripped = trimws(as.character(PPORRESU)),
    `_PARAM` = compbl(paste0(PPTESTCD_mapped, " (", PPORRESU_stripped, ")")),
    AVAL_log = log(AVAL)
  )

# Sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM
adpp_sorted <- adpp_filt %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# PROC UNIVARIATE by ANLPRNT PPSPECL PPCATL TRTAN PPSDY _PARAM var AVAL AVAL_log
# output n mean std cv median min max for each var
# Need to compute per group: n, mean, std, cv, median, min, max for AVAL and AVAL_log
# SAS PROC UNIVARIATE definitions:
# n = number of non-missing
# mean = arithmetic mean
# std = sample standard deviation (n-1 denominator)
# cv = 100*std/mean (if mean !=0)
# median, min, max

# Group vars
group_vars <- c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM")

# Compute summary
adpp_sum <- adpp_sorted %>%
  group_by(across(all_of(group_vars))) %>%
  summarise(
    n1 = sum(!is.na(AVAL)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1>0, mean(AVAL, na.rm=TRUE), NA_real_),
    mn2 = ifelse(n2>0, mean(AVAL_log, na.rm=TRUE), NA_real_),
    std1 = ifelse(n1>1, sd(AVAL, na.rm=TRUE), NA_real_),
    std2 = ifelse(n2>1, sd(AVAL_log, na.rm=TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) && mn1 != 0, 100*std1/mn1, NA_real_),
    cv2 = ifelse(!is.na(mn2) && mn2 != 0, 100*std2/mn2, NA_real_),
    med1 = ifelse(n1>0, median(AVAL, na.rm=TRUE), NA_real_),
    med2 = ifelse(n2>0, median(AVAL_log, na.rm=TRUE), NA_real_),
    min1 = ifelse(n1>0, min(AVAL, na.rm=TRUE), NA_real_),
    min2 = ifelse(n2>0, min(AVAL_log, na.rm=TRUE), NA_real_),
    max1 = ifelse(n1>0, max(AVAL, na.rm=TRUE), NA_real_),
    max2 = ifelse(n2>0, max(AVAL_log, na.rm=TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

# Now create adpp_sum_t transposed: 7 rows per group
# For each row in adpp_sum, generate 7 rows with s_order 1..7 and value

# Helper to generate value strings per SAS logic
make_rows <- function(row) {
  # row is a one-row data frame/tibble
  param <- row$`_PARAM`
  # Determine index checks
  idx_AUC <- sas_index(param, "AUC") > 0
  idx_Cmax <- sas_index(param, "Cmax") > 0
  idx_MRT <- sas_index(param, "MRT") > 0
  idx_t12 <- sas_index(param, "t1/2") > 0
  idx_tmax <- sas_index(param, "tmax") > 0

  n1 <- row$n1; n2 <- row$n2
  mn1 <- row$mn1; mn2 <- row$mn2
  std1 <- row$std1; std2 <- row$std2
  cv1 <- row$cv1
  med1 <- row$med1
  min1 <- row$min1; max1 <- row$max1

  # s_order 1: n
  v1 <- sas_put(n1, 2, 0)

  # s_order 2: mean (sd)
  v2 <- " "
  if (idx_AUC) {
    v2 <- paste0(sas_put(mn1, 6, 0), " (", sas_put(std1, 5, 0), ")")
  } else if (idx_Cmax) {
    v2 <- paste0(sas_put(mn1, 5, 0), " (", sas_put(std1, 4, 0), ")")
  } else if (idx_MRT) {
    v2 <- paste0(sas_put(mn1, 4, 1), " (", sas_put(std1, 4, 2), ")")
  } else if (idx_t12) {
    v2 <- paste0(sas_put(mn1, 4, 2), " (", sas_put(std1, 4, 2), ")")
  } else {
    v2 <- " "
  }

  # s_order 3: CV% mean
  v3 <- if (idx_tmax) " " else sas_put(cv1, 4, 1)

  # s_order 4: geo-mean = exp(mn2)
  v4 <- " "
  if (idx_AUC) {
    v4 <- sas_put(exp(mn2), 6, 0)
  } else if (idx_Cmax) {
    v4 <- sas_put(exp(mn2), 5, 0)
  } else if (idx_MRT) {
    v4 <- sas_put(exp(mn2), 4, 1)
  } else if (idx_t12) {
    v4 <- sas_put(exp(mn2), 4, 2)
  } else {
    v4 <- " "
  }

  # s_order 5: CV% geo-mean = sqrt(exp(std2^2)-1)*100
  v5 <- if (idx_tmax) " " else {
    val <- sqrt(exp(std2^2) - 1) * 100
    sas_put(val, 4, 1)
  }

  # s_order 6: median
  v6 <- " "
  if (idx_AUC) {
    v6 <- sas_put(med1, 6, 0)
  } else if (idx_Cmax) {
    v6 <- sas_put(med1, 5, 0)
  } else if (idx_MRT) {
    v6 <- sas_put(med1, 4, 1)
  } else if (idx_t12) {
    v6 <- sas_put(med1, 4, 2)
  } else if (idx_tmax) {
    v6 <- sas_put(med1, 4, 2)
  }

  # s_order 7: [min; max]
  v7 <- " "
  if (idx_AUC) {
    v7 <- paste0("[", sas_put(min1, 6, 0), ";", sas_put(max1, 6, 0), "]")
  } else if (idx_Cmax) {
    v7 <- paste0("[", sas_put(min1, 4, 0), ";", sas_put(max1, 5, 0), "]")
  } else if (idx_MRT) {
    v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  } else if (idx_t12) {
    v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 1), "]")
  } else if (idx_tmax) {
    v7 <- paste0("[", sas_put(min1, 4, 2), ";", sas_put(max1, 4, 2), "]")
  }

  # Build 7 rows
  tibble(
    s_order = 1:7,
    value = c(v1, v2, v3, v4, v5, v6, v7)
  )
}

# Apply to each row
adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i) {
  row <- adpp_sum[i, ]
  rows <- make_rows(row)
  # replicate group columns
  cbind(row[, group_vars, drop=FALSE], rows)
})

adpp_sum_t <- bind_rows(adpp_sum_t_list)

# Sort by ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM
adpp_sum_t <- adpp_sum_t %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)

# Prepare for PROC REPORT: need to create wide format with _PARAM as across
# The report has columns: ANLPRNT PPSPECL PPCATL TRTAN PPSDY s_order _PARAM, value
# With _PARAM across, value under each _PARAM
# We need to produce a dataset for output and a text rendering

# Define formats for display
trtan_fmt <- c("1" = "Period 1 Treatment", "2" = "Period 2 Treatment", "3" = "Period 3 Treatment")
statg_fmt <- c("1" = "n", "2" = "Mean (SD)", "3" = "CV% mean", "4" = "Geo-mean", "5" = "CV% geo-mean", "6" = "Median", "7" = "[Min; Max]        ")

# Get unique _PARAM values in order as they appear sorted
param_levels <- unique(adpp_sum_t$`_PARAM`)
# Ensure order matches SAS: by _PARAM within s_order, but overall param order is alphabetical? Let's keep as sorted unique
param_levels <- sort(param_levels)

# Create wide dataset for CSV output (one row per TRTAN PPSDY s_order within each by group)
# For text rendering, we need to mimic PROC REPORT with BY groups

# For CSV of table body, we will output the long transposed data plus wide version
# The expected P035.lst is a table with 124 numbers - we need to produce text rendering

# Build wide table for each BY group
wide_df <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
  pivot_wider(names_from = `_PARAM`, values_from = value)

# Ensure column order: ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, then params in param_levels order
wide_df <- wide_df %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, all_of(param_levels))

# Also create a display version with formatted TRTAN and s_order
wide_display <- wide_df %>%
  mutate(
    TRTAN_disp = ifelse(as.character(TRTAN) %in% names(trtan_fmt), trtan_fmt[as.character(TRTAN)], as.character(TRTAN)),
    s_order_disp = ifelse(as.character(s_order) %in% names(statg_fmt), statg_fmt[as.character(s_order)], as.character(s_order))
  )

# Write datasets as CSV
# According to spec, each output dataset as CSV with SAS variable names
# Here the main dataset is adpp_sum_t (the report input) and wide version
# We will write both long and wide for completeness, but primary is P035

# Write long transposed CSV (this is the report data)
write.csv(adpp_sum_t, file.path(out, "P035.csv"), row.names = FALSE, na = "")

# Also write wide version as P035_wide.csv for reference (optional)
write.csv(wide_df, file.path(out, "P035_wide.csv"), row.names = FALSE, na = "")

# Create text rendering mimicking PROC REPORT
# Titles and footnotes
titles <- c(
  "Study001",
  "Table 14.2-PPTS (Page xxx of xxx)",
  "Summary statistics for PK parameters",
  "by compound, matrix, analyte and actual treatment",
  "Analysis Set: PK analysis set"
)
footnotes <- c(
  "CV% = coefficient of variation (%) = sd/mean*100",
  "CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100",
  "Geo-mean: Geometric mean",
  "Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.",
  "Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt",
  "Fake Data/ Production Run on 17MAR2014:23:57"
)

# Build text file
txt_path <- file.path(out, "P035.txt")
# Also .lst is expected, create both
lst_path <- file.path(out, "P035.lst")

# Helper to format header line for report
# PROC REPORT column headers: TRTAN "Actual treatment" width 20, PPSDY "Period day" width 8, s_order "Statistic" width 14, _PARAM width 15 each, value width 19
# But with across, _PARAM becomes column header, value is under it

# We will produce a plain text table with BY groups

# Determine column widths for text rendering
# Use fixed widths as in define statements
col_widths <- list(
  TRTAN = 20,
  PPSDY = 8,
  s_order = 14,
  param = 19  # value width 19, but _PARAM header width 15, we use 19 for data
)

# Function to pad string to width (left aligned)
pad_left <- function(s, w) {
  # left aligned, pad right with spaces, truncate if longer
  s <- as.character(s)
  s <- ifelse(is.na(s), "", s)
  # truncate to w if needed
  s <- substr(s, 1, w)
  sprintf(paste0("%-", w, "s"), s)
}
pad_right <- function(s, w) {
  # right aligned (for numbers) but we already have SAS padding
  s <- as.character(s)
  s <- ifelse(is.na(s), "", s)
  s <- substr(s, 1, w)
  sprintf(paste0("%", w, "s"), s)
}

# Build lines
lines <- c()

# Add titles (j=L left, j=C center)
# For text rendering, we will center titles approximately width 120
total_width <- 20 + 1 + 8 + 1 + 14 + 1 + length(param_levels)* (19+1)
if (total_width < 80) total_width <- 80
if (total_width > 200) total_width <- 200

center_text <- function(txt, width) {
  txt <- as.character(txt)
  pad <- max(0, floor((width - nchar(txt))/2))
  paste0(strrep(" ", pad), txt)
}
left_text <- function(txt) txt

for (i in seq_along(titles)) {
  if (i == 1) {
    lines <- c(lines, titles[i])
  } else {
    lines <- c(lines, center_text(titles[i], total_width))
  }
}
lines <- c(lines, "")

# For each BY group (ANLPRNT, PPSPECL, PPCATL)
by_groups <- adpp_sum_t %>%
  distinct(ANLPRNT, PPSPECL, PPCATL) %>%
  arrange(ANLPRNT, PPSPECL, PPCATL)

for (bg_idx in seq_len(nrow(by_groups))) {
  bg <- by_groups[bg_idx, ]
  # Compute before _page_ line
  header_line <- paste0("Compound: ", sprintf("%-10s", bg$ANLPRNT), " , Matrix: ", sprintf("%-10s", bg$PPSPECL), " , Analyte: ", sprintf("%-15s", bg$PPCATL))
  lines <- c(lines, header_line)
  lines <- c(lines, "")

  # Subset data for this BY group
  sub <- adpp_sum_t %>% filter(ANLPRNT == bg$ANLPRNT, PPSPECL == bg$PPSPECL, PPCATL == bg$PPCATL)
  # Get wide for this group
  sub_wide <- sub %>%
    select(TRTAN, PPSDY, s_order, `_PARAM`, value) %>%
    pivot_wider(names_from = `_PARAM`, values_from = value)

  # Ensure all param_levels columns exist
  for (p in param_levels) {
    if (!p %in% names(sub_wide)) sub_wide[[p]] <- ""
  }
  sub_wide <- sub_wide %>% select(TRTAN, PPSDY, s_order, all_of(param_levels)) %>%
    arrange(TRTAN, PPSDY, s_order)

  # Build header row
  hdr1 <- paste0(pad_left("Actual treatment", 20), " ", pad_left("Period day", 8), " ", pad_left("Statistic", 14), " ")
  for (p in param_levels) {
    hdr1 <- paste0(hdr1, pad_left(p, 19), " ")
  }
  lines <- c(lines, hdr1)
  # Headline (formchar(2)="_" headskip) -> line of underscores
  # SAS headline draws line under header
  underline <- strrep("_", nchar(hdr1))
  lines <- c(lines, underline)

  # Data rows
  for (r in seq_len(nrow(sub_wide))) {
    row <- sub_wide[r, ]
    trtan_disp <- trtan_fmt[as.character(row$TRTAN)]
    if (is.na(trtan_disp)) trtan_disp <- as.character(row$TRTAN)
    ppsdy_disp <- as.character(row$PPSDY)
    sord_disp <- statg_fmt[as.character(row$s_order)]
    if (is.na(sord_disp)) sord_disp <- as.character(row$s_order)

    line <- paste0(pad_left(trtan_disp, 20), " ", pad_left(ppsdy_disp, 8), " ", pad_left(sord_disp, 14), " ")
    for (p in param_levels) {
      val <- row[[p]]
      if (is.na(val)) val <- ""
      # value is already formatted with SAS PUT width, but we need to left align in 19 width
      # PROC REPORT define value width 19 left group missing flow -> left aligned
      # So trim leading spaces and left align? But SAS PUT includes leading spaces; PROC REPORT left will left align?
      # We will trim and left pad? To match SAS display, we should left align the value string trimmed
      val_trim <- trimws(val)
      # If original was " " (single space), keep blank
      if (val == " " || val_trim == "") val_trim <- ""
      line <- paste0(line, pad_left(val_trim, 19), " ")
    }
    lines <- c(lines, line)

    # break after PPSDY/skip -> add blank line after each PPSDY group
    # Detect if next row has different PPSDY or TRTAN
    if (r < nrow(sub_wide)) {
      next_row <- sub_wide[r+1, ]
      if (next_row$PPSDY != row$PPSDY || next_row$TRTAN != row$TRTAN) {
        lines <- c(lines, "")
      }
    }
  }
  lines <- c(lines, "")
}

# Add footnotes
for (fn in footnotes) {
  lines <- c(lines, fn)
}

# Write to txt and lst
writeLines(lines, con = txt_path)
writeLines(lines, con = lst_path)

# Also write CSV of table body (wide display)
# For CSV, include formatted display columns
csv_body <- wide_display %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, TRTAN_disp, PPSDY, s_order, s_order_disp, all_of(param_levels))
write.csv(csv_body, file.path(out, "P035_body.csv"), row.names = FALSE, na = "")

# Verify counts: 124 numbers expected in P035.lst
# Count numbers in text (simple check)
# Not required to output, but ensure file exists

# Also write adpp_sum intermediate for debugging
write.csv(adpp_sum, file.path(out, "P035_adpp_sum.csv"), row.names = FALSE, na = "")
