```r
# P075 t_vs.sas translation to R
# Table 14.3.5 - Summary of Vital Signs by Visit

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# Helper: round half away from zero (SAS behavior)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5 + 1e-9) / mult
}
fmt_n <- function(x) {
  if (is.na(x)) return("")
  # SAS put(n,8.) -> integer, strip
  as.character(as.integer(round_half_up(x, 0)))
}
fmt_8_1 <- function(x) {
  if (is.na(x)) return(".")
  sprintf("%.1f", round_half_up(x, 1))
}
fmt_8_2 <- function(x) {
  if (is.na(x)) return(".")
  sprintf("%.2f", round_half_up(x, 2))
}
fmt_minmax <- function(mn, mx) {
  if (is.na(mn) || is.na(mx)) return("")
  paste0(fmt_8_1(mn), ", ", fmt_8_1(mx))
}

# Locate ADaM datasets
adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

find_and_read <- function(libpath, name) {
  # name lower case e.g. adsl, advs
  # try .sas7bdat, .xpt, .sas7bdat in subdir
  candidates <- list.files(libpath, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  # filter by name
  idx <- grep(paste0("(^|/)", name, "\\.(sas7bdat|xpt)$"), candidates, ignore.case = TRUE)
  if (length(idx) == 0) {
    # try without extension search
    idx <- grep(paste0(name), candidates, ignore.case = TRUE)
  }
  if (length(idx) == 0) stop(paste0("Dataset not found: ", name, " in ", libpath))
  f <- candidates[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    # try both
    tryCatch({ df <- haven::read_sas(f) }, error = function(e) df <<- haven::read_xpt(f))
  }
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  df <- haven::zap_label(df)
  # standardize names to upper case for SAS semantics
  names(df) <- toupper(names(df))
  df
}

adsl <- find_and_read(adam_path, "adsl")
advs <- find_and_read(adam_path, "advs")

# Ensure character columns are character (haven may give labelled)
char_cols_adsl <- c("USUBJID","TRTA","SAFFL")
for (cn in intersect(char_cols_adsl, names(adsl))) {
  adsl[[cn]] <- as.character(adsl[[cn]])
}
char_cols_advs <- c("USUBJID","TRTA","PARAMCD","PARAM","AVISIT","ABLFL","ANL01FL")
for (cn in intersect(char_cols_advs, names(advs))) {
  advs[[cn]] <- as.character(advs[[cn]])
}

# Step 1: Big N (for reference, not displayed but computed)
bign_df <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]
bign_df <- aggregate(USUBJID ~ TRTA, data = bign_df, FUN = function(x) length(unique(x)))
names(bign_df)[2] <- "BIGN"
bign_df <- bign_df[order(bign_df$TRTA), ]
bign_tot <- length(unique(adsl$USUBJID[toupper(adsl$SAFFL) == "Y"]))

# Step 2: Prepare analysis data vs
# inner join advs with adsl SAFFL=Y
adsl_saff <- adsl[toupper(adsl$SAFFL) == "Y", c("USUBJID"), drop = FALSE]
adsl_saff$USUBJID <- as.character(adsl_saff$USUBJID)
advs$USUBJID <- as.character(advs$USUBJID)

vs <- merge(advs, adsl_saff, by = "USUBJID", all = FALSE)
# filter PARAMCD and ANL01FL
vs <- vs[vs$PARAMCD %in% c("SYSBP","DIABP","PULSE","TEMP","RESP","WEIGHT") & vs$ANL01FL == "Y", ]

# Ensure numeric columns
num_cols <- c("PARAMN","AVISITN","TRTAN","AVAL","BASE","CHG")
for (cn in intersect(num_cols, names(vs))) {
  vs[[cn]] <- as.numeric(vs[[cn]])
}

# Order by PARAMN, AVISITN, TRTAN
vs <- vs[order(vs$PARAMN, vs$AVISITN, vs$TRTAN), ]

# Step 3: Descriptive stats by parameter, visit, treatment
# Actual value stats
# Use dplyr for grouping
library(dplyr)

# Helper to compute stats per group
actual_stats <- vs %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = ifelse(sum(!is.na(AVAL)) > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    std = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    median = ifelse(sum(!is.na(AVAL)) > 0, median(AVAL, na.rm = TRUE), NA_real_),
    min = ifelse(sum(!is.na(AVAL)) > 0, min(AVAL, na.rm = TRUE), NA_real_),
    max = ifelse(sum(!is.na(AVAL)) > 0, max(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# Change stats: where CHG ne . and ABLFL ne 'Y'
vs_chg <- vs[!is.na(vs$CHG) & (is.na(vs$ABLFL) | vs$ABLFL != "Y"), ]

change_stats <- vs_chg %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    n_chg = sum(!is.na(CHG)),
    mean_chg = ifelse(sum(!is.na(CHG)) > 0, mean(CHG, na.rm = TRUE), NA_real_),
    std_chg = ifelse(sum(!is.na(CHG)) > 1, sd(CHG, na.rm = TRUE), NA_real_),
    median_chg = ifelse(sum(!is.na(CHG)) > 0, median(CHG, na.rm = TRUE), NA_real_),
    min_chg = ifelse(sum(!is.na(CHG)) > 0, min(CHG, na.rm = TRUE), NA_real_),
    max_chg = ifelse(sum(!is.na(CHG)) > 0, max(CHG, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# Step 4: Merge actual and change on PARAMCD, AVISITN, TRTAN (SAS keys)
stats <- merge(actual_stats, change_stats,
               by = c("PARAMCD","AVISITN","TRTAN"),
               all.x = TRUE, suffixes = c("", ".y"))

# After merge, the PARAM/PARAMN/AVISIT etc from actual are kept; but need to ensure
# if change had different PARAM values (should be same) we keep actual's
# stats already has PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN, n, mean, std, median, min, max
# plus n_chg etc
# Order by PARAMN, AVISITN, TRTAN
stats <- stats[order(stats$PARAMN, stats$AVISITN, stats$TRTAN), ]

# Step 5: Format for display
report <- stats
report$stat1 <- sapply(report$n, fmt_n)
report$stat2 <- sapply(report$mean, fmt_8_1)
report$stat3 <- sapply(report$std, fmt_8_2)
report$stat4 <- sapply(report$median, fmt_8_1)
report$stat5 <- mapply(fmt_minmax, report$min, report$max)

# Change stats: only if n_chg >0 else blank
report$chg1 <- ifelse(!is.na(report$n_chg) & report$n_chg > 0, sapply(report$n_chg, fmt_n), "")
report$chg2 <- ifelse(!is.na(report$n_chg) & report$n_chg > 0, sapply(report$mean_chg, fmt_8_2), "")
report$chg3 <- ifelse(!is.na(report$n_chg) & report$n_chg > 0, sapply(report$std_chg, fmt_8_2), "")
report$chg4 <- ifelse(!is.na(report$n_chg) & report$n_chg > 0, sapply(report$median_chg, fmt_8_2), "")
report$chg5 <- ifelse(!is.na(report$n_chg) & report$n_chg > 0, mapply(fmt_minmax, report$min_chg, report$max_chg), "")

# For actual stats where n=0, SAS would show "." for mean/std etc, but we already handle NA -> "."
# Ensure stat2-5 for n=0 show "."? Our fmt returns "." for NA, which matches SAS put(.,8.1) -> "."
# For n=0, mean is NA, so stat2=".", etc. That's correct.

# Keep only needed columns for display, but retain order vars
report_display <- report[, c("PARAMN","PARAM","AVISITN","AVISIT","TRTA","TRTAN",
                             "stat1","stat2","stat3","stat4","stat5",
                             "chg1","chg2","chg3","chg4","chg5",
                             "n","mean","std","median","min","max",
                             "n_chg","mean_chg","std_chg","median_chg","min_chg","max_chg",
                             "PARAMCD")]

# Sort final
report_display <- report_display[order(report_display$PARAMN, report_display$AVISITN, report_display$TRTAN), ]

# Write CSV of table body (as per spec: CSV of same table)
# The CSV should contain displayed values
csv_out <- report_display[, c("PARAM","AVISIT","TRTA","stat1","stat2","stat3","stat4","stat5","chg1","chg2","chg3","chg4","chg5")]
# Rename to match SAS display headers
names(csv_out) <- c("PARAM","AVISIT","TRTA","n_actual","Mean_actual","SD_actual","Median_actual","MinMax_actual",
                    "n_change","Mean_change","SD_change","Median_change","MinMax_change")
write.csv(csv_out, file.path(out, "t_vs.csv"), row.names = FALSE, na = "")

# Also write full stats dataset for traceability
write.csv(report_display, file.path(out, "t_vs_stats.csv"), row.names = FALSE, na = "")

# Step 6: Generate text rendering (one page per parameter)
# Titles and footnotes
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"
sysin <- "P075_t_vs.sas"
run_date <- format(Sys.Date(), "%d%b%Y")
run_time <- format(Sys.time(), "%H:%M")

titles <- c(
  paste0(sponsor, paste(rep(" ", 60), collapse=""), "Page 1 of 1"),
  paste0("Protocol: ", protocol),
  "",
  "Table 14.3.5",
  "Summary of Vital Signs by Visit",
  "Safety Analysis Set",
  ""
)
footnotes <- c(
  paste0("Data cutoff: ", cutdate, "."),
  "Source: ADaM datasets",
  paste0("Program: ", sysin, " | Run: ", run_date, " ", run_time)
)

# Build text file
txt_path <- file.path(out, "t_vs.txt")
rtf_path <- file.path(out, "t_vs.rtf")
con <- file(txt_path, open = "wt")

writeLines(titles, con)
writeLines("", con)

# Header line for table
header1 <- sprintf("%-22s %-18s %-18s | %-5s %-8s %-8s %-8s %-14s | %-5s %-8s %-8s %-8s %-14s",
                   "Parameter", "Visit", "Treatment",
                   "n", "Mean", "SD", "Median", "Min, Max",
                   "n", "Mean", "SD", "Median", "Min, Max")
header2 <- sprintf("%-22s %-18s %-18s | %-5s %-8s %-8s %-8s %-14s | %-5s %-8s %-8s %-8s %-14s",
                   "", "", "",
                   "", "", "", "", "",
                   "", "", "", "", "")
sep <- paste(rep("-", 150), collapse="")

# Group by PARAM
params <- unique(report_display[, c("PARAMN","PARAM")])
params <- params[order(params$PARAMN), ]

for (pi in seq_len(nrow(params))) {
  pnam <- params$PARAM[pi]
  pnum <- params$PARAMN[pi]
  # Page break indicator
  if (pi > 1) {
    writeLines("", con)
    writeLines(paste0("--- Page break after ", params$PARAM[pi-1], " ---"), con)
    writeLines("", con)
  }
  writeLines(paste0("Parameter: ", pnam), con)
  writeLines(sep, con)
  writeLines(header1, con)
  writeLines(paste0(sprintf("%-22s %-18s %-18s |", "", "", ""), 
                    sprintf(" %-5s %-8s %-8s %-8s %-14s |", "Actual Value", "", "", "", ""),
                    sprintf(" %-5s %-8s %-8s %-8s %-14s", "Change from Baseline", "", "", "", "")), con)
  writeLines(sep, con)
  sub <- report_display[report_display$PARAM == pnam, ]
  sub <- sub[order(sub$AVISITN, sub$TRTAN), ]
  # For order display, show AVISIT only on first row per visit
  prev_visit <- ""
  for (i in seq_len(nrow(sub))) {
    row <- sub[i, ]
    visit_disp <- ifelse(row$AVISIT == prev_visit, "", row$AVISIT)
    # Truncate/pad to widths
    param_disp <- ifelse(i==1, substr(pnam,1,22), "")
    line <- sprintf("%-22s %-18s %-18s | %-5s %-8s %-8s %-8s %-14s | %-5s %-8s %-8s %-8s %-14s",
                    param_disp,
                    substr(visit_disp,1,18),
                    substr(row$TRTA,1,18),
                    row$stat1, row$stat2, row$stat3, row$stat4, row$stat5,
                    row$chg1, row$chg2, row$chg3, row$chg4, row$chg5)
    writeLines(line, con)
    prev_visit <- row$AVISIT
  }
  writeLines(sep, con)
}

writeLines("", con)
writeLines(footnotes, con)
close(con)

# Also create RTF copy (plain text inside RTF wrapper for compatibility)
# Simple RTF: copy txt content into RTF
txt_content <- readLines(txt_path)
rtf_header <- "{\\rtf1\\ansi\\deff0{\\fonttbl{\\f0 Courier New;}}\\viewkind4\\uc1\\pard\\f0\\fs18 "
rtf_footer <- "}"
rtf_body <- paste(txt_content, collapse="\\par\n")
# Escape RTF special chars
rtf_body <- gsub("\\\\", "\\\\\\\\", rtf_body)
rtf_body <- gsub("\\{", "\\\\{", rtf_body)
rtf_body <- gsub("\\}", "\\\\}", rtf_body)
writeLines(paste0(rtf_header, rtf_body, rtf_footer), rtf_path)

# Clean up work datasets (not needed in R)

cat("P075 t_vs completed successfully. Outputs in", out, "\n")
```