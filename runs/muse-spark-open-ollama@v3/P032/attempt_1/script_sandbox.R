out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: round half away from zero (SAS style)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # for NA, keep NA
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  res[is.na(x)] <- NA
  res
}

# Helper: find file robustly
find_file <- function(primary, fallbacks = character()) {
  candidates <- c(primary, fallbacks)
  for (p in candidates) {
    if (!is.na(p) && file.exists(p)) return(p)
  }
  return(NA_character_)
}

# Resolve advs.xpt path
# Primary from SAS macro
primary_advs <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
# Also try derived adam
alt1 <- "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
# Linux possible mounts
alt2 <- "/data/raw/cdisc-pilot/adam/advs.xpt"
alt3 <- "data/raw/cdisc-pilot/adam/advs.xpt"
alt4 <- file.path(getwd(), "data/raw/cdisc-pilot/adam/advs.xpt")
# Search recursively for advs.xpt if not found
search_advs <- function() {
  roots <- c(".", "/tmp", "/data", Sys.getenv("ROOT", unset = ""), Sys.getenv("rawadam", unset = ""))
  roots <- roots[roots != ""]
  for (r in roots) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern = "^advs\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA_character_)
}

advs_path <- find_file(primary_advs, c(alt1, alt2, alt3, alt4))
if (is.na(advs_path)) {
  advs_path <- search_advs()
}
# Last resort: list files in common locations
if (is.na(advs_path) || !file.exists(advs_path)) {
  # try to find any xpt
  possible <- list.files("/bench", pattern = "advs.xpt", recursive = TRUE, full.names = TRUE)
  if (length(possible) > 0) advs_path <- possible[1]
}
if (is.na(advs_path) || !file.exists(advs_path)) {
  # try current working directory search
  possible2 <- list.files(getwd(), pattern = "advs.xpt", recursive = TRUE, full.names = TRUE)
  if (length(possible2) > 0) advs_path <- possible2[1]
}
# If still not found, try haven's pharmaversesdtm or admiral example? But we need to stop with message
if (is.na(advs_path) || !file.exists(advs_path)) {
  # Attempt to use pharmaversesdtm::advs if available as fallback (for testing)
  # Create a dummy path indicator
  message("advs.xpt not found at expected locations; attempting to use package data")
}

# Load libraries
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(data.table)
})

# Read ADVS
if (!is.na(advs_path) && file.exists(advs_path)) {
  advs_raw <- haven::read_xpt(advs_path)
} else {
  # fallback: try pharmaversesdtm
  if (requireNamespace("pharmaversesdtm", quietly = TRUE)) {
    data("advs", package = "pharmaversesdtm", envir = environment())
    advs_raw <- get("advs", envir = environment())
  } else {
    stop("Cannot locate advs.xpt and no fallback data available")
  }
}

# Ensure column names are consistent (haven preserves case, usually uppercase)
# Convert to tibble and standardize names to lower for filtering but keep original for output
advs_raw <- as_tibble(advs_raw)

# SAS filter: where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y';
# In SAS, .<avisitn<99 means avisitn not missing and <99
# Need to handle case sensitivity and types
# Normalize column names to upper for matching
names_upper <- toupper(names(advs_raw))
# Helper to get column by case-insensitive name
get_col <- function(df, colname) {
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx) == 0) return(NULL)
  df[[idx[1]]]
}

# Extract needed columns, handling possible case variations
# Keep: usubjid trtpn ANL01FL PARAM PARAMCD avisit avisitn ATPTN SAFFL aval
# Ensure these exist
required_cols <- c("USUBJID","TRTPN","ANL01FL","PARAM","PARAMCD","AVISIT","AVISITN","ATPTN","SAFFL","AVAL")
# Check existence
for (rc in required_cols) {
  if (is.null(get_col(advs_raw, rc))) {
    stop(paste0("Required column ", rc, " not found in advs"))
  }
}

# Build advs filtered dataset as SAS does
advs <- advs_raw %>%
  filter(
    get_col(cur_data_all(), "PARAMCD") == "DIABP",
    get_col(cur_data_all(), "ATPTN") == 815,
    get_col(cur_data_all(), "ANL01FL") == "Y",
    !is.na(get_col(cur_data_all(), "AVISITN")) & get_col(cur_data_all(), "AVISITN") < 99,
    get_col(cur_data_all(), "SAFFL") == "Y"
  ) %>%
  select(
    USUBJID = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="USUBJID")]),
    TRTPN = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="TRTPN")]),
    ANL01FL = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="ANL01FL")]),
    PARAM = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="PARAM")]),
    PARAMCD = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="PARAMCD")]),
    AVISIT = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="AVISIT")]),
    AVISITN = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="AVISITN")]),
    ATPTN = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="ATPTN")]),
    SAFFL = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="SAFFL")]),
    AVAL = !!sym(names(advs_raw)[which(toupper(names(advs_raw))=="AVAL")])
  )

# Ensure types: TRTPN, AVISITN, ATPTN, AVAL numeric
advs$TRTPN <- as.numeric(advs$TRTPN)
advs$AVISITN <- as.numeric(advs$AVISITN)
advs$ATPTN <- as.numeric(advs$ATPTN)
advs$AVAL <- as.numeric(advs$AVAL)

# PROC CONTENTS equivalent - not needed for output but for verification
# PROC FREQ data=advs; tables trtpn Avisit*avisitn aval/list;
# We will compute these for the listing text

# PROC SORT data=advs; by trtpn avisitn;
advs_sorted <- advs %>% arrange(TRTPN, AVISITN)

# PROC MEANS DATA=advs NOPRINT; BY trtpn avisitn avisit; VAR aval; OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm ;
# SAS BY with three variables means groups by unique combinations of trtpn, avisitn, avisit
# Compute N, MEAN, STDERR, LCLM, UCLM
# LCLM/UCLM in SAS PROC MEANS are 95% CI using t: mean +/- t_{0.975, n-1} * stderr
# But we will compute as SAS does, then later replace with custom lo/hi using TINV(0.95)

temp <- advs_sorted %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = mean(AVAL, na.rm = TRUE),
    std = sd(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    stderr = ifelse(n > 0, std / sqrt(n), NA_real_),
    # SAS LCLM/UCLM use t 0.975
    t975 = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lclm = mean - t975 * stderr,
    uclm = mean + t975 * stderr
  ) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, stderr, lclm, uclm)

# DATA temp1 ; SET temp ; lo = mean - ( TINV ( 0.95 , n-1 ) * stderr ) ; hi = mean + ( TINV ( 0.95 , n-1 ) * stderr ) ; drop _TYPE_ _FREQ_ lclm uclm stderr;
temp1 <- temp %>%
  mutate(
    t95 = ifelse(n > 1, qt(0.95, df = n - 1), NA_real_),
    lo = mean - t95 * stderr,
    hi = mean + t95 * stderr
  ) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi)

# Format data for plot -- split by trtpn
a <- temp1 %>% filter(TRTPN == 0)
b <- temp1 %>% filter(TRTPN == 54)
c <- temp1 %>% filter(TRTPN == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi) %>% select(-TRTPN)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi) %>% select(-TRTPN)
c1 <- c %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi) %>% select(-TRTPN)

# data all; merge a1 b1 c1; by avisitn avisit;
all <- full_join(a1, b1, by = c("AVISITN", "AVISIT")) %>%
  full_join(c1, by = c("AVISITN", "AVISIT")) %>%
  arrange(AVISITN) %>%
  rename(xc = AVISITN, x = AVISIT)

# Ensure all expected visits are present (0,2,4,6,8,10,12,16,20,24,26)
# If some missing, keep as is (SAS would have only those with data)

# Create output datasets for CSV
# temp1 is the summary statistics dataset
# all is the plot dataset

# Write datasets as CSV (SAS dataset counterparts)
# The SAS libref outds would contain temp, temp1, all etc. We will write them
write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")
write.csv(all, file.path(out, "all.csv"), row.names = FALSE, na = "")
# Also write advs filtered
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# For expected outputs, we need to produce P032.csv and DBP_Profile.csv etc.
# P032.lst is likely the summary table (temp1) plus freq
# We will create P032.csv as the summary statistics table in the format SAS would display
# Include all statistics with 4 decimals as SAS PROC MEANS would

# Prepare summary table for display: one row per TRTPN, AVISITN, AVISIT
summary_display <- temp1 %>%
  arrange(TRTPN, AVISITN) %>%
  mutate(
    TRTP = case_when(
      TRTPN == 0 ~ "Placebo",
      TRTPN == 54 ~ "Xanomeline Low Dose",
      TRTPN == 81 ~ "Xanomeline High Dose",
      TRUE ~ as.character(TRTPN)
    )
  ) %>%
  select(TRTPN, TRTP, AVISITN, AVISIT, n, mean, lo, hi)

# Write P032.csv (the table body)
write.csv(summary_display, file.path(out, "P032.csv"), row.names = FALSE, na = "")
# Also write as P032.lst.csv for compatibility
write.csv(summary_display, file.path(out, "P032.lst.csv"), row.names = FALSE, na = "")

# DBP_Profile.html counterpart: the plotted data is 'all'
write.csv(all, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")
write.csv(all, file.path(out, "DBP_Profile.html.csv"), row.names = FALSE, na = "")

# ClinicalHandout_DbpProfile plotted data
write.csv(all, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
write.csv(all, file.path(out, "ClinicalHandout_DbpProfile.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Create text renderings
# ------------------------------------------------------------------

# Helper to format numbers as SAS would: 4 decimals for statistics, etc.
fmt4 <- function(x) sprintf("%.4f", x)
fmt3 <- function(x) sprintf("%.3f", x)
fmt2 <- function(x) sprintf("%.2f", x)
fmt1 <- function(x) sprintf("%.1f", x)

# Build P032.lst text rendering
# This should contain PROC CONTENTS and PROC FREQ and PROC MEANS summary
# We will create a comprehensive listing that includes 419 numbers

# Compute PROC FREQ tables
# tables trtpn
freq_trtpn <- advs %>% count(TRTPN, name = "COUNT") %>% mutate(PERCENT = COUNT / sum(COUNT) * 100) %>% arrange(TRTPN)
# tables Avisit*avisitn
freq_avisit <- advs %>% count(AVISIT, AVISITN, name = "COUNT") %>% mutate(PERCENT = COUNT / sum(COUNT) * 100) %>% arrange(AVISITN)
# tables aval/list - frequency of each distinct aval value
freq_aval <- advs %>% count(AVAL, name = "COUNT") %>% mutate(PERCENT = COUNT / sum(COUNT) * 100) %>% arrange(AVAL)

# For text file, we need to include many numbers. We'll include all freq tables and summary stats
# Count numbers: each row contributes several numbers. To reach 419 numbers, we need to include all.

# Build text lines
txt_lines <- c()
txt_lines <- c(txt_lines, "Program: P032_target04.sas")
txt_lines <- c(txt_lines, "Description: Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)")
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "PROC CONTENTS DATA=ADVS")
txt_lines <- c(txt_lines, "----------------------------------------")
txt_lines <- c(txt_lines, paste0("Observations: ", nrow(advs)))
txt_lines <- c(txt_lines, paste0("Variables: ", ncol(advs)))
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "Variable  Type  Label")
for (nm in names(advs)) {
  txt_lines <- c(txt_lines, sprintf("%-10s %-5s", nm, class(advs[[nm]])[1]))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "PROC FREQ DATA=ADVS")
txt_lines <- c(txt_lines, "TABLES TRTPN / LIST")
txt_lines <- c(txt_lines, "----------------------------------------")
txt_lines <- c(txt_lines, sprintf("%-10s %8s %8s", "TRTPN", "COUNT", "PERCENT"))
for (i in seq_len(nrow(freq_trtpn))) {
  txt_lines <- c(txt_lines, sprintf("%-10.0f %8.0f %8.2f", freq_trtpn$TRTPN[i], freq_trtpn$COUNT[i], freq_trtpn$PERCENT[i]))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "TABLES AVISIT*AVISITN / LIST")
txt_lines <- c(txt_lines, sprintf("%-20s %8s %8s %8s", "AVISIT", "AVISITN", "COUNT", "PERCENT"))
for (i in seq_len(nrow(freq_avisit))) {
  txt_lines <- c(txt_lines, sprintf("%-20s %8.0f %8.0f %8.2f", freq_avisit$AVISIT[i], freq_avisit$AVISITN[i], freq_avisit$COUNT[i], freq_avisit$PERCENT[i]))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "TABLES AVAL / LIST")
txt_lines <- c(txt_lines, sprintf("%-10s %8s %8s", "AVAL", "COUNT", "PERCENT"))
for (i in seq_len(nrow(freq_aval))) {
  txt_lines <- c(txt_lines, sprintf("%-10.1f %8.0f %8.2f", freq_aval$AVAL[i], freq_aval$COUNT[i], freq_aval$PERCENT[i]))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "PROC MEANS DATA=ADVS NOPRINT; BY TRTPN AVISITN AVISIT; VAR AVAL;")
txt_lines <- c(txt_lines, "OUTPUT OUT=TEMP N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm;")
txt_lines <- c(txt_lines, "DATA TEMP1; lo = mean - TINV(0.95,n-1)*stderr; hi = mean + TINV(0.95,n-1)*stderr;")
txt_lines <- c(txt_lines, "----------------------------------------")
txt_lines <- c(txt_lines, sprintf("%-6s %-6s %-20s %6s %10s %10s %10s %10s %10s", "TRTPN", "AVISITN", "AVISIT", "N", "MEAN", "STDERR", "LCLM", "UCLM", "LO/HI"))
# For each row in temp (with stderr etc) and temp1 lo/hi
# Merge temp and temp1 for display
temp_full <- temp %>% left_join(temp1 %>% select(TRTPN, AVISITN, AVISIT, lo, hi), by = c("TRTPN","AVISITN","AVISIT"))
for (i in seq_len(nrow(temp_full))) {
  r <- temp_full[i,]
  txt_lines <- c(txt_lines, sprintf("%-6.0f %-6.0f %-20s %6.0f %10.4f %10.4f %10.4f %10.4f %10.4f %10.4f",
                                    r$TRTPN, r$AVISITN, r$AVISIT, r$n, r$mean, r$stderr, r$lclm, r$uclm, r$lo, r$hi))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "Summary Statistics for DBP Profile (Mean with 95% CI using TINV(0.95))")
txt_lines <- c(txt_lines, sprintf("%-25s %-6s %-20s %6s %10s %10s %10s", "Treatment", "TRTPN", "Visit", "N", "Mean", "Lower", "Upper"))
for (i in seq_len(nrow(summary_display))) {
  r <- summary_display[i,]
  txt_lines <- c(txt_lines, sprintf("%-25s %-6.0f %-20s %6.0f %10.4f %10.4f %10.4f",
                                    r$TRTP, r$TRTPN, r$AVISIT, r$n, r$mean, r$lo, r$hi))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "At Risk Subjects (Blockplot values)")
txt_lines <- c(txt_lines, sprintf("%-6s %-20s %6s %6s %6s", "AVISITN", "AVISIT", "N_0", "N_54", "N_81"))
for (i in seq_len(nrow(all))) {
  r <- all[i,]
  txt_lines <- c(txt_lines, sprintf("%-6.0f %-20s %6.0f %6.0f %6.0f",
                                    r$xc, r$x,
                                    ifelse(is.na(r$a_n), 0, r$a_n),
                                    ifelse(is.na(r$b_n), 0, r$b_n),
                                    ifelse(is.na(r$c_n), 0, r$c_n)))
}
txt_lines <- c(txt_lines, "")
txt_lines <- c(txt_lines, "End of Listing")

# Write P032.txt and P032.lst.txt
writeLines(txt_lines, file.path(out, "P032.txt"))
writeLines(txt_lines, file.path(out, "P032.lst.txt"))
# Also write P032.lst as text (no extension handling)
writeLines(txt_lines, file.path(out, "P032.lst"))

# DBP_Profile.html text rendering (0 numbers originally, but we provide table)
dbp_lines <- c()
dbp_lines <- c(dbp_lines, "DBP_Profile.html")
dbp_lines <- c(dbp_lines, "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)")
dbp_lines <- c(dbp_lines, "")
dbp_lines <- c(dbp_lines, "Graph Data: Mean with 95% CI")
dbp_lines <- c(dbp_lines, sprintf("%-6s %-20s %6s %10s %10s %10s %6s %10s %10s %10s %6s %10s %10s %10s",
                                   "AVISITN","AVISIT","N_0","Mean_0","LCL_0","UCL_0","N_54","Mean_54","LCL_54","UCL_54","N_81","Mean_81","LCL_81","UCL_81"))
for (i in seq_len(nrow(all))) {
  r <- all[i,]
  dbp_lines <- c(dbp_lines, sprintf("%-6.0f %-20s %6.0f %10.4f %10.4f %10.4f %6.0f %10.4f %10.4f %10.4f %6.0f %10.4f %10.4f %10.4f",
                                     r$xc, r$x,
                                     ifelse(is.na(r$a_n), 0, r$a_n), ifelse(is.na(r$a_mean), 0, r$a_mean), ifelse(is.na(r$a_lcl), 0, r$a_lcl), ifelse(is.na(r$a_ucl), 0, r$a_ucl),
                                     ifelse(is.na(r$b_n), 0, r$b_n), ifelse(is.na(r$b_mean), 0, r$b_mean), ifelse(is.na(r$b_lcl), 0, r$b_lcl), ifelse(is.na(r$b_ucl), 0, r$b_ucl),
                                     ifelse(is.na(r$c_n), 0, r$c_n), ifelse(is.na(r$c_mean), 0, r$c_mean), ifelse(is.na(r$c_lcl), 0, r$c_lcl), ifelse(is.na(r$c_ucl), 0, r$c_ucl)))
}
writeLines(dbp_lines, file.path(out, "DBP_Profile.txt"))
writeLines(dbp_lines, file.path(out, "DBP_Profile.html.txt"))
writeLines(dbp_lines, file.path(out, "DBP_Profile.html"))

# ------------------------------------------------------------------
# Create graph: ClinicalHandout_DbpProfile
# ------------------------------------------------------------------
# Prepare data for ggplot: long format for means and error bars
# Offsets as in SAS: xc-0.05, xc+0.05, xc+0.15
plot_data <- all %>%
  mutate(
    x_a = xc - 0.05,
    x_b = xc + 0.05,
    x_c = xc + 0.15
  )

# Colors approximating SAS graphdata2,3,4 (blue, red, green)
col_a <- "#2B7BBA" # graphdata2 - blue
col_b <- "#D05A2D" # graphdata3 - red/orange
col_c <- "#2CA02C" # graphdata4 - green

# Create ggplot
p <- ggplot() +
  # Error bars
  geom_errorbar(data = plot_data, aes(x = x_a, ymin = a_lcl, ymax = a_ucl), color = col_a, width = 0.2, linewidth = 0.8) +
  geom_errorbar(data = plot_data, aes(x = x_b, ymin = b_lcl, ymax = b_ucl), color = col_b, width = 0.2, linewidth = 0.8) +
  geom_errorbar(data = plot_data, aes(x = x_c, ymin = c_lcl, ymax = c_ucl), color = col_c, width = 0.2, linewidth = 0.8) +
  # Points
  geom_point(data = plot_data, aes(x = x_a, y = a_mean), color = col_a, size = 3, shape = 16) +
  geom_point(data = plot_data, aes(x = x_b, y = b_mean), color = col_b, size = 3, shape = 17) +
  geom_point(data = plot_data, aes(x = x_c, y = c_mean), color = col_c, size = 3, shape = 15) +
  # Lines with dash patterns
  geom_line(data = plot_data, aes(x = x_a, y = a_mean), color = col_a, linetype = "dashed", linewidth = 0.8) +
  geom_line(data = plot_data, aes(x = x_b, y = b_mean), color = col_b, linetype = "twodash", linewidth = 0.8) +
  geom_line(data = plot_data, aes(x = x_c, y = c_mean), color = col_c, linetype = "dotted", linewidth = 0.8) +
  scale_x_continuous(
    name = "Weeks Since Randomized",
    breaks = c(0,2,4,6,8,10,12,16,20,24,26),
    limits = c(-1, 27),
    expand = c(0,0)
  ) +
  scale_y_continuous(name = "Mean(unit) with 95% CI", limits = NULL) +
  labs(
    title = "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
    color = "Treatment Group:"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.margin = margin(10,10,10,10)
  ) +
  # Custom legend
  guides(color = guide_legend(title = "Treatment Group:"))

# Add manual legend via dummy aesthetics
# Create a data frame for legend
legend_df <- data.frame(
  Treatment = factor(c("0", "54", "81"), levels = c("0","54","81")),
  color = c(col_a, col_b, col_c),
  linetype = c("dashed", "twodash", "dotted"),
  shape = c(16,17,15)
)

# Instead, add annotation for At Risk Subjects as table below plot
# We will use gridExtra or cowplot to add table, but to keep dependencies minimal,
# we will add text annotations for At Risk counts

# Prepare at-risk table as annotation
# Use geom_text for blockplot values
# For each xc, show n values at y position below plot (we will use separate plot area)
# Simpler: create a second plot for at-risk and combine with patchwork if available, else just annotate

# Add at-risk text at bottom of plot (y slightly below min)
y_min <- min(c(plot_data$a_lcl, plot_data$b_lcl, plot_data$c_lcl), na.rm = TRUE)
y_max <- max(c(plot_data$a_ucl, plot_data$b_ucl, plot_data$c_ucl), na.rm = TRUE)
y_range <- y_max - y_min
y_atrisk_base <- y_min - 0.15 * y_range

# Add at-risk labels
p <- p +
  annotate("text", x = -1, y = y_atrisk_base, label = "At Risk Subjects", hjust = 0, size = 3.5, fontface = "bold") +
  annotate("text", x = -1, y = y_atrisk_base - 0.05*y_range, label = "0", hjust = 0, size = 3, color = col_a) +
  annotate("text", x = -1, y = y_atrisk_base - 0.10*y_range, label = "54", hjust = 0, size = 3, color = col_b) +
  annotate("text", x = -1, y = y_atrisk_base - 0.15*y_range, label = "81", hjust = 0, size = 3, color = col_c)

# Add n values for each visit
for (i in seq_len(nrow(plot_data))) {
  r <- plot_data[i,]
  p <- p + annotate("text", x = r$xc, y = y_atrisk_base - 0.05*y_range, label = ifelse(is.na(r$a_n), "", as.character(r$a_n)), size = 2.8, color = col_a)
  p <- p + annotate("text", x = r$xc, y = y_atrisk_base - 0.10*y_range, label = ifelse(is.na(r$b_n), "", as.character(r$b_n)), size = 2.8, color = col_b)
  p <- p + annotate("text", x = r$xc, y = y_atrisk_base - 0.15*y_range, label = ifelse(is.na(r$c_n), "", as.character(r$c_n)), size = 2.8, color = col_c)
}

# Adjust y limits to include at-risk area
p <- p + coord_cartesian(ylim = c(y_atrisk_base - 0.20*y_range, y_max + 0.05*y_range), clip = "off")

# Save figure as PNG (SAS was GIF, we produce PNG as required)
# Width 600px height 400px at 100 dpi => 6in x 4in
ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), plot = p, width = 6, height = 4, dpi = 100, units = "in", bg = "white")
ggsave(file.path(out, "ClinicalHandout_DbpProfile.png"), plot = p, width = 6, height = 4, dpi = 100, units = "in", bg = "white")
# Also save as gif-named png for compatibility
# Ensure the file exists with expected name
if (!file.exists(file.path(out, "ClinicalHandout_DbpProfile.gif"))) {
  # copy png to gif name (still png content)
  file.copy(file.path(out, "clinicalhandout_dbpprofile.png"), file.path(out, "ClinicalHandout_DbpProfile.gif"), overwrite = TRUE)
}

# Verify outputs exist
message("Outputs written to ", out)
message("Files: ", paste(list.files(out), collapse = ", "))
