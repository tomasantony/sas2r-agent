# P037_target21.sas -> R translation
# Vital Signs Treatment-Emergent Abnormal High or Low

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: round half away from zero (SAS ROUND)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  # for 0, sign 0 -> treat as 1
  sign_x[sign_x == 0] <- 1
  trunc(abs(x) * mult + 0.5) / mult * sign_x
}
fmt_5_1 <- function(x) {
  if (is.na(x)) return("")
  v <- round_half_up(x, 1)
  # SAS 5.1: width 5, 1 decimal, strip later
  # Use sprintf with 1 decimal, then trim
  trimws(sprintf("%5.1f", v))
}
fmt_pvalue5.3 <- function(x) {
  if (is.na(x)) return("")
  # SAS pvalue5.3: <0.001 displayed as <.001
  if (x < 0.0005) { # will round to 0.000 -> SAS shows <.001
    # threshold for 3 decimals is 0.001
    if (x < 0.001) return("<.001")
  }
  v <- round_half_up(x, 3)
  if (v < 0.001) return("<.001")
  # width 5, 3 decimals
  trimws(sprintf("%5.3f", v))
}

# locate advs.xpt
rawadam_env <- Sys.getenv("rawadam")
candidates <- c(
  file.path(rawadam_env, "advs.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADVS.xpt",
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/advs.xpt")
)
src <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { src <- p; break }
}
if (is.null(src)) {
  # try haven inlib path
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat"
  if (file.exists(alt)) src <- alt else stop("advs source not found")
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

if (grepl("\\.xpt$", src, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(src)
} else {
  advs_raw <- haven::read_sas(src)
}

# zap labels to plain vectors
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_formats(advs_raw)

# ensure character columns are character, trim
char_cols <- c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL")
for (cc in char_cols) {
  if (cc %in% names(advs_raw)) {
    advs_raw[[cc]] <- as.character(advs_raw[[cc]])
    advs_raw[[cc]][is.na(advs_raw[[cc]])] <- ""
    advs_raw[[cc]] <- trimws(advs_raw[[cc]])
    # convert "" back to NA for missing checks where needed, but keep "" as missing
  }
}
# numeric cols
num_cols <- c("TRTPN","ATPTN","ADY","AVAL")
for (nc in num_cols) {
  if (nc %in% names(advs_raw)) advs_raw[[nc]] <- as.numeric(advs_raw[[nc]])
}

# Filter: WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y",
         (CRIT1FL != "" & !is.na(CRIT1FL)) | (CRIT2FL != "" & !is.na(CRIT2FL))
  ) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# Helper to count distinct USUBJID
# totn_low: where not missing crit1fl
totn_low <- advs %>%
  filter(CRIT1FL != "" & !is.na(CRIT1FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")

totn_high <- advs %>%
  filter(CRIT2FL != "" & !is.na(CRIT2FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort and merge
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

all_n <- full_join(totn, te_n, by = c("TRTPN","PARAM","tefl")) %>%
  mutate(pct = n / totn * 100)

# Fisher Exact p-value per flag
# function to compute fisher p for a given flag column
compute_fisher <- function(flag_col, tefl_label) {
  # flag_col: "CRIT1FL" or "CRIT2FL"
  # SAS: sort by usubjid flag, then last.usubjid
  # missing sorts first, so Y > N > "" 
  df <- advs %>%
    select(USUBJID, TRTPN, PARAM, all_of(flag_col)) %>%
    mutate(.flag = .data[[flag_col]]) %>%
    mutate(.flag = ifelse(is.na(.flag), "", .flag)) %>%
    arrange(USUBJID, .flag) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    filter(.flag != "")  # PROC FREQ excludes missing by default
  # Build contingency table: TRTPN x flag
  # Need to ensure all TRTPN levels present
  if (nrow(df) == 0) return(data.frame(tefl = tefl_label, nvalue1 = NA_real_))
  tab <- table(df$TRTPN, df$.flag)
  # fisher.test requires at least 2 rows and 2 cols
  # If only one column (e.g., all N or all Y), fisher p is 1? SAS would still compute?
  # Use fisher.test if possible, else NA
  pval <- NA_real_
  if (nrow(tab) >= 2 && ncol(tab) >= 2) {
    ft <- tryCatch(fisher.test(tab), error = function(e) NULL)
    if (!is.null(ft)) pval <- ft$p.value
  } else if (nrow(tab) >= 1 && ncol(tab) >= 1) {
    # degenerate table -> p=1 if no variation? Keep NA to match SAS missing?
    # SAS would still output something; for 1 column, Fisher not defined -> missing
    pval <- NA_real_
  }
  data.frame(tefl = tefl_label, nvalue1 = pval, stringsAsFactors = FALSE)
}

fisher_low <- compute_fisher("CRIT1FL", "Low")
fisher_high <- compute_fisher("CRIT2FL", "High")

all_fisher <- bind_rows(fisher_low, fisher_high) %>%
  arrange(tefl)

# Merge Fisher with N/n
all_n_sorted <- all_n %>% arrange(tefl, TRTPN)
all_fisher_sorted <- all_fisher %>% arrange(tefl)

table_vals <- full_join(all_n_sorted, all_fisher_sorted, by = "tefl") %>%
  mutate(
    pctc = paste0("(", sapply(pct, fmt_5_1), ")"),
    nc = as.character(n),
    # handle NA n
    nc = ifelse(is.na(n), "", nc),
    pctc = ifelse(is.na(pct), "", pctc)
  )

# Sort by param descending tefl nvalue1 (SAS: by param descending tefl nvalue1)
# descending tefl means Low before High (L > H)
table_vals <- table_vals %>%
  arrange(PARAM, desc(tefl), nvalue1, TRTPN)

# Only want to display first p-value in final table
# SAS: by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=put(...)
# Since nvalue1 same per tefl, first per tefl group
table_vals <- table_vals %>%
  group_by(PARAM, tefl, nvalue1) %>%
  mutate(
    is_first = row_number() == 1,
    nvalue1c = ifelse(is_first, sapply(nvalue1, fmt_pvalue5.3), "")
  ) %>%
  ungroup() %>%
  # ensure ordering for display: param, descending tefl, TRTPN
  arrange(PARAM, desc(tefl), TRTPN)

# For display, order TRTPN ascending within each tefl
# SAS proc report order=data group will show in data order; we have arranged

# Write dataset CSV (all_n + fisher)
write.csv(table_vals, file.path(out, "P037.csv"), row.names = FALSE, na = "")

# Also write all_n intermediate for traceability
write.csv(all_n, file.path(out, "P037_all_n.csv"), row.names = FALSE, na = "")

# Build text rendering similar to PROC REPORT
# Columns: param, tefl, trtpn, totn, nc, pctc, nvalue1c
# Titles
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"

# Prepare display table
disp <- table_vals %>%
  select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c) %>%
  mutate(
    PARAM = as.character(PARAM),
    tefl = as.character(tefl),
    TRTPN = as.character(TRTPN),
    totn = as.character(totn)
  )

# Create fixed-width text
header1 <- sprintf("%-30s %-12s %-12s %-6s %-6s %-10s %-10s",
                   "Vital Sign (unit)", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
header2 <- sprintf("%-30s %-12s %-12s %-6s %-6s %-10s %-10s",
                   "", "Direction", "", "", "", "", "")
sep <- paste(rep("-", 90), collapse = "")

lines <- c(title1, title2, "", header1, header2, sep)

for (i in seq_len(nrow(disp))) {
  r <- disp[i, ]
  line <- sprintf("%-30s %-12s %-12s %-6s %-6s %-10s %-10s",
                  r$PARAM, r$tefl, r$TRTPN, r$totn, r$nc, r$pctc, r$nvalue1c)
  lines <- c(lines, line)
}
lines <- c(lines, sep,
           "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
           "baseline and at least one post-baseline measure, n = number of patients with an abnormal post-baseline result in the",
           "specified category.",
           "*P values are from Fisher's Exact test."
)

# Write .txt
writeLines(lines, file.path(out, "P037.txt"))
# Also write .lst for compatibility
writeLines(lines, file.path(out, "P037.lst"))

# Write CSV of table body (same as disp plus pct and pvalue numeric)
write.csv(disp, file.path(out, "P037_table.csv"), row.names = FALSE, na = "")

# Also ensure P037.lst.csv exists? Provide generic
# Console log
cat("P037 completed: ", nrow(table_vals), " rows, p-values: ",
    paste(all_fisher$tefl, round(all_fisher$nvalue1,4), collapse="; "), "\n")
