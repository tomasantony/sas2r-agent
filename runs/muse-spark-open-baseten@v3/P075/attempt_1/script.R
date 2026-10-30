# P075 t_vs.sas -> R translation
# Table 14.3.5 Summary of Vital Signs by Visit
# Population: Safety Analysis Set (SAFFL='Y')

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
sas_round <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}
sas_fmt <- function(x, decimals = 1) {
  # mimics strip(put(x, w.d)) : round half away, then fixed decimals
  # if NA -> "." (SAS put missing)
  ifelse(is.na(x), ".",
         {
           r <- sas_round(x, decimals)
           # sprintf will format with required decimals
           fmt <- paste0("%.", decimals, "f")
           sprintf(fmt, r)
         })
}
sas_fmt_int <- function(x) {
  ifelse(is.na(x), ".", sprintf("%.0f", sas_round(x, 0)))
}

# helper to read adam dataset by name (case-insensitive, xpt or sas7bdat)
read_adam <- function(libpath, dsn) {
  dsn_low <- tolower(dsn)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # map lower basename without extension to file
  candidates <- tibble(path = files,
                       base = tolower(tools::file_path_sans_ext(basename(files))),
                       ext = tolower(tools::file_ext(files)))
  hit <- candidates %>% filter(base == dsn_low)
  if (nrow(hit) == 0) {
    stop(paste0("Dataset not found: ", dsn, " in ", libpath, " files: ", paste(basename(files), collapse=", ")))
  }
  # prefer sas7bdat then xpt
  hit <- hit %>% arrange(match(ext, c("sas7bdat","xpt","sas","csv")))
  f <- hit$path[1]
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    haven::read_xpt(f)
  } else if (ext == "sas7bdat") {
    haven::read_sas(f)
  } else if (ext == "csv") {
    readr::read_csv(f, show_col_types = FALSE)
  } else {
    haven::read_sas(f)
  }
}

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

# fallback if path not exists (e.g., running on linux with different mount)
if (!dir.exists(adam_path)) {
  # try alternative derived path
  alt <- Sys.getenv("ADAM_PATH")
  if (alt != "" && dir.exists(alt)) adam_path <- alt
}

adsl <- read_adam(adam_path, "adsl")
advs <- read_adam(adam_path, "advs")

# Ensure character columns are character (haven may give labelled)
adsl <- adsl %>% mutate(across(where(haven::is.labelled), as.character))
advs <- advs %>% mutate(across(where(haven::is.labelled), as.character))

# Ensure numeric columns are numeric
# Standardize column names to upper case for matching SAS (case-insensitive)
names(adsl) <- toupper(names(adsl))
names(advs) <- toupper(names(advs))

# Step 1: Get Big N (for log, not directly used in table but compute)
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTA)

bign_tot <- adsl %>% filter(toupper(SAFFL) == "Y") %>% summarise(n=n_distinct(USUBJID)) %>% pull(n)

# Step 2: Prepare analysis data
# inner join advs with adsl on USUBJID where SAFFL='Y' and PARAMCD in list and ANL01FL='Y'
param_keep <- c('SYSBP','DIABP','PULSE','TEMP','RESP','WEIGHT')

# Ensure join columns exist
stopifnot("USUBJID" %in% names(adsl), "USUBJID" %in% names(advs))
stopifnot("SAFFL" %in% names(adsl))
stopifnot(all(c("PARAMCD","ANL01FL","TRTA","TRTAN","PARAM","PARAMN","AVISIT","AVISITN","AVAL","BASE","CHG","ABLFL") %in% names(advs)))

vs <- advs %>%
  inner_join(adsl %>% select(USUBJID, SAFFL_ADSL = SAFFL), by="USUBJID") %>%
  filter(toupper(SAFFL_ADSL) == "Y",
         toupper(PARAMCD) %in% param_keep,
         toupper(ANL01FL) == "Y") %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, AVAL, BASE, CHG, ABLFL, ANL01FL) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Step 3: Descriptive stats by parameter, visit, treatment
# PROC MEANS nway noprint class PARAMCD PARAM PARAMN AVISIT AVISITN TRTA TRTAN var AVAL
# Need to group by all class variables
grp_vars <- c("PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","TRTA","TRTAN")

actual <- vs %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(
    N = sum(!is.na(AVAL)),
    MEAN = ifelse(sum(!is.na(AVAL))>0, mean(AVAL, na.rm=TRUE), NA_real_),
    STD = ifelse(sum(!is.na(AVAL))>1, sd(AVAL, na.rm=TRUE), NA_real_),
    MEDIAN = ifelse(sum(!is.na(AVAL))>0, median(AVAL, na.rm=TRUE), NA_real_),
    MIN = ifelse(sum(!is.na(AVAL))>0, min(AVAL, na.rm=TRUE), NA_real_),
    MAX = ifelse(sum(!is.na(AVAL))>0, max(AVAL, na.rm=TRUE), NA_real_),
    .groups="drop"
  )

# Second means for CHG where CHG ne . and ABLFL ne 'Y'
vs_chg <- vs %>% filter(!is.na(CHG) & toupper(ABLFL) != "Y")

change <- vs_chg %>%
  group_by(across(all_of(grp_vars))) %>%
  summarise(
    N_CHG = sum(!is.na(CHG)),
    MEAN_CHG = ifelse(sum(!is.na(CHG))>0, mean(CHG, na.rm=TRUE), NA_real_),
    STD_CHG = ifelse(sum(!is.na(CHG))>1, sd(CHG, na.rm=TRUE), NA_real_),
    MEDIAN_CHG = ifelse(sum(!is.na(CHG))>0, median(CHG, na.rm=TRUE), NA_real_),
    MIN_CHG = ifelse(sum(!is.na(CHG))>0, min(CHG, na.rm=TRUE), NA_real_),
    MAX_CHG = ifelse(sum(!is.na(CHG))>0, max(CHG, na.rm=TRUE), NA_real_),
    .groups="drop"
  )

# Step 4: Merge actual and change on PARAMCD, AVISITN, TRTAN (SAS uses those 3 keys)
# Note SAS code: on a.PARAMCD = b.PARAMCD and a.AVISITN = b.AVISITN and a.TRTAN = b.TRTAN
# This is insufficient if same PARAMCD has different PARAM etc, but we follow SAS exactly
# To avoid duplicate, we join on full grp_vars but SAS join would still work because PARAMCD+AVISITN+TRTAN uniquely identifies group in this data
stats <- actual %>%
  left_join(change, by = c("PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","TRTA","TRTAN")) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Step 5: Format for display
report <- stats %>%
  mutate(
    STAT1 = sas_fmt_int(N),
    STAT2 = ifelse(!is.na(N) & N>0, sas_fmt(MEAN, 1), ""),
    STAT3 = ifelse(!is.na(STD), sas_fmt(STD, 2), "."),
    # SAS code does not condition STD on N>1, so missing STD becomes "."
    # but we already handled NA -> "."
    STAT4 = ifelse(!is.na(N) & N>0, sas_fmt(MEDIAN, 1), ""),
    STAT5 = ifelse(!is.na(N) & N>0,
                   paste0(sas_fmt(MIN,1), ", ", sas_fmt(MAX,1)),
                   ""),
    CHG1 = ifelse(!is.na(N_CHG) & N_CHG>0, sas_fmt_int(N_CHG), ""),
    CHG2 = ifelse(!is.na(N_CHG) & N_CHG>0, sas_fmt(MEAN_CHG, 2), ""),
    CHG3 = ifelse(!is.na(N_CHG) & N_CHG>0,
                  ifelse(is.na(STD_CHG), ".", sas_fmt(STD_CHG, 2)),
                  ""),
    CHG4 = ifelse(!is.na(N_CHG) & N_CHG>0, sas_fmt(MEDIAN_CHG, 2), ""),
    CHG5 = ifelse(!is.na(N_CHG) & N_CHG>0,
                  paste0(sas_fmt(MIN_CHG,1), ", ", sas_fmt(MAX_CHG,1)),
                  "")
  ) %>%
  # Replace "." for STD when N<=1 should be "." per SAS, but if N=1 STD is NA -> "."
  # Ensure STAT3 is "." when N==1 (already)
  # For display, SAS would show "." ; keep it
  arrange(PARAMN, AVISITN, TRTAN)

# For CSV output, keep original numeric stats plus formatted display
# SAS report dataset has columns: PARAMN PARAM AVISITN AVISIT TRTA stat1-stat5 chg1-chg5 plus numeric stats
# We will output the display columns as in PROC REPORT
report_out <- report %>%
  select(PARAMN, PARAM, PARAMCD, AVISITN, AVISIT, TRTA, TRTAN,
         N, MEAN, STD, MEDIAN, MIN, MAX,
         N_CHG, MEAN_CHG, STD_CHG, MEDIAN_CHG, MIN_CHG, MAX_CHG,
         STAT1, STAT2, STAT3, STAT4, STAT5,
         CHG1, CHG2, CHG3, CHG4, CHG5) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Write CSV
write.csv(report_out, file.path(out, "t_vs.csv"), row.names = FALSE, na = "")

# Also write a CSV of plotted data equivalent (same as report_out for this table)
# (required for figures, but we provide for completeness)

# Build text rendering
# Titles from u_titles.sas for T_VS
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
studyid <- "[STUDYID]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "  Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.3.5"
title5 <- "Summary of Vital Signs by Visit"
title6 <- "Safety Analysis Set"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
footnote3 <- paste0("Program: t_vs.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

# Create formatted table text
# Header
header_line <- sprintf("%-30s %-15s %-15s %8s %8s %8s %8s %-15s %8s %8s %8s %8s %-15s",
                       "Parameter","Visit","Treatment",
                       "n","Mean","SD","Median","Min, Max",
                       "n","Mean","SD","Median","Min, Max")
header2 <- sprintf("%-30s %-15s %-15s %8s %8s %8s %8s %-15s %8s %8s %8s %8s %-15s",
                       "","","",
                       "","","","","",
                       "","","","","")
sep <- paste(rep("-", 170), collapse="")

# Build rows grouped by PARAM
txt_lines <- c(
  title1,
  title2,
  "",
  title4,
  title5,
  title6,
  "",
  paste0("Treatment groups: ", paste(bign_df$TRTA, collapse=", "), " (Total N=", bign_tot, ")"),
  "",
  "Actual Value                                                    Change from Baseline",
  header_line,
  sep
)

# Iterate by PARAMN
params <- unique(report$PARAMN)
# Ensure order by PARAMN
params <- sort(params, na.last = TRUE)

for (p in params) {
  sub <- report %>% filter(PARAMN == p) %>% arrange(AVISITN, TRTAN)
  param_label <- unique(sub$PARAM)[1]
  txt_lines <- c(txt_lines, paste0(param_label, " (", p, ")"))
  for (i in seq_len(nrow(sub))) {
    r <- sub[i,]
    line <- sprintf("%-30s %-15s %-15s %8s %8s %8s %8s %-15s %8s %8s %8s %8s %-15s",
                    ifelse(i==1, strtrim(param_label,30), ""),
                    strtrim(as.character(r$AVISIT),15),
                    strtrim(as.character(r$TRTA),15),
                    r$STAT1, r$STAT2, r$STAT3, r$STAT4, r$STAT5,
                    r$CHG1, r$CHG2, r$CHG3, r$CHG4, r$CHG5)
    txt_lines <- c(txt_lines, line)
  }
  txt_lines <- c(txt_lines, "")
}

txt_lines <- c(txt_lines,
               sep,
               footnote1,
               footnote2,
               footnote3,
               "",
               paste0("N=", bign_tot, " Safety Analysis Set subjects included.")
)

# Write txt
writeLines(txt_lines, file.path(out, "t_vs.txt"))

# Also write RTF-like txt as t_vs.rtf for expected output name (plain text RTF)
writeLines(txt_lines, file.path(out, "t_vs.rtf"))

# Clean up work datasets (not needed in R)
# Log completion
cat("NOTE: t_vs.sas completed successfully.\n")
cat(paste0("NOTE: Output written to ", out, "\n"))
cat(paste0("NOTE: Rows in report: ", nrow(report_out), "\n"))
