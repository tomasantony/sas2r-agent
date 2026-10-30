# P037 - Vital Signs Treatment-Emergent Abnormal High or Low
# SAS: P037_target21.sas
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  # SAS rounds half away from zero
  res <- x
  pos <- !is.na(x) & x >= 0
  neg <- !is.na(x) & x < 0
  res[pos] <- floor(x[pos] * mult + 0.5 + 1e-9) / mult
  res[neg] <- ceiling(x[neg] * mult - 0.5 - 1e-9) / mult
  res
}

fmt_5_1 <- function(x) {
  # SAS PUT(pct,5.1) then STRIP
  r <- round_half_up(x, 1)
  sprintf("%.1f", r)
}

fmt_pvalue5.3 <- function(x) {
  # SAS PVALUE5.3 : <.001 / >0.999 handling
  if (is.na(x)) return("")
  # thresholds: values that would round to 0.000 or 1.000
  if (x < 0.0005) return("<.001")
  if (x > 0.9995) return(">0.999")
  r <- round_half_up(x, 3)
  # re-check after rounding
  if (r < 0.0005) return("<.001")
  if (r > 0.9995) return(">0.999")
  sprintf("%.3f", r)
}

is_cmiss <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
message("Reading ", advs_path)
advs_raw <- haven::read_xpt(advs_path)
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_formats(advs_raw)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
message(paste0("advs_raw rows=", nrow(advs_raw)))

# Ensure expected columns exist (case-insensitive check, SAS names uppercase)
nm <- toupper(names(advs_raw))
names(advs_raw) <- nm
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL") %in% nm))

# Normalise character columns: haven may give NA; keep as character
charcols <- c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL")
for (cc in charcols) {
  advs_raw[[cc]] <- as.character(advs_raw[[cc]])
}
# Numeric
advs_raw$TRTPN <- as.numeric(advs_raw$TRTPN)
advs_raw$ATPTN <- as.numeric(advs_raw$ATPTN)
advs_raw$ADY <- as.numeric(advs_raw$ADY)
advs_raw$AVAL <- as.numeric(advs_raw$AVAL)

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw %>%
  filter(trimws(PARAMCD) == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ADY) & ADY > 1,
         !is.na(SAFFL) & trimws(SAFFL) == "Y",
         (!is_cmiss(CRIT1FL) | !is_cmiss(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)
message(paste0("advs filtered rows=", nrow(advs)))
stopifnot(all(c("USUBJID","TRTPN","PARAM","CRIT1FL","CRIT2FL") %in% names(advs)))
# SAS trims trailing blanks for grouping; strip PARAM leading/trailing blanks only for grouping consistency
# Keep PARAM display value trimmed of trailing blanks (SAS would display without extra blanks)
advs$PARAM <- trimws(advs$PARAM)
advs$CRIT1FL <- ifelse(is.na(advs$CRIT1FL), NA, trimws(advs$CRIT1FL))
advs$CRIT2FL <- ifelse(is.na(advs$CRIT2FL), NA, trimws(advs$CRIT2FL))
# Convert empty string to NA for easier handling, but remember SAS missing sorts first
advs$CRIT1FL[advs$CRIT1FL == ""] <- NA
advs$CRIT2FL[advs$CRIT2FL == ""] <- NA

# PROC SQL counts
# totn_low: where not missing(crit1fl)
totn_low <- advs %>%
  filter(!is.na(CRIT1FL) & trimws(CRIT1FL) != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")
n_low <- advs %>%
  filter(!is.na(CRIT1FL) & trimws(CRIT1FL) == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")
totn_high <- advs %>%
  filter(!is.na(CRIT2FL) & trimws(CRIT2FL) != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")
n_high <- advs %>%
  filter(!is.na(CRIT2FL) & trimws(CRIT2FL) == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort by trtpn param (SAS)
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# Combine N and n, calculate pct
# SAS: merge totn te_n; by trtpn param;
# drop/rename overlapping non-key cols before join (only totn/n distinct, tefl is extra but needed)
# Keys: TRTPN, PARAM ; TEFL should match but SAS merges only by TRTPN PARAM (tefl coincidentally matches)
# Do full join on TRTPN PARAM TEFL to be safe and equivalent
all_n <- full_join(totn, te_n, by = c("TRTPN","PARAM","tefl"))
stopifnot(all(c("TRTPN","PARAM","tefl","totn","n") %in% names(all_n)))
all_n$pct <- all_n$n / all_n$totn * 100

# Fisher Exact p-value
# macro: sort by usubjid flag, keep last.usubjid, then proc freq tables param*trtpn*flag / fisher, keep XP2_FISH
fisher_one <- function(flag_col, tefl_label) {
  flg <- advs[[flag_col]]
  # SAS sort order: missing first, then 'N' < 'Y' (uppercase ordering)
  # Create sort key: NA -> "", then alphabetical
  flg_clean <- ifelse(is.na(flg), "", flg)
  df <- data.frame(USUBJID = advs$USUBJID, TRTPN = advs$TRTPN, PARAM = advs$PARAM,
                   FLG = flg_clean, stringsAsFactors = FALSE)
  # sort by USUBJID, FLG (character ordering: "" < "N" < "Y")
  df <- df[order(df$USUBJID, df$FLG, na.last = FALSE), ]
  # last per USUBJID
  # use dplyr
  df_last <- df %>% group_by(USUBJID) %>% filter(row_number() == n()) %>% ungroup() %>% as.data.frame()
  # Exclude missing FLG (PROC FREQ excludes missing by default)
  df_nm <- df_last %>% filter(FLG != "" & !is.na(FLG))
  # Build contingency table TRTPN x FLG (for single PARAM; if multiple PARAMs, SAS stratifies - here single)
  # If multiple PARAM values, compute per PARAM then? SAS would give one p per PARAM stratum.
  # Our data has single PARAM, so one table.
  tab <- table(df_nm$TRTPN, df_nm$FLG)
  message(paste0("Fisher ", tefl_label, " table:"))
  print(tab)
  # Fisher exact (Freeman-Halton for RxC)
  ft <- fisher.test(tab)
  pval <- ft$p.value
  data.frame(tefl = tefl_label, nvalue1 = pval, stringsAsFactors = FALSE)
}

fisher_crit1fl <- fisher_one("CRIT1FL", "Low")
fisher_crit2fl <- fisher_one("CRIT2FL", "High")
all_fisher <- bind_rows(fisher_crit1fl, fisher_crit2fl)
all_fisher <- all_fisher %>% arrange(tefl)

# sort all_n by tefl trtpn (SAS)
all_n <- all_n %>% arrange(tefl, TRTPN)

# Merge Fisher p-value with N and n values; by tefl
table_vals <- left_join(all_n, all_fisher, by = "tefl")
stopifnot(all(c("tefl","nvalue1") %in% names(table_vals)))
# Create pctc and nc
table_vals$pctc <- paste0("(", sapply(table_vals$pct, fmt_5_1), ")")
table_vals$nc <- as.character(table_vals$n)

# sort by param descending tefl nvalue1 (SAS: proc sort by param descending tefl nvalue1)
# descending tefl => "Low" before "High"
# Within ties, keep TRTPN ascending for determinism (matches merge order)
table_vals <- table_vals[order(table_vals$PARAM, -xtfrm(table_vals$tefl), table_vals$nvalue1, table_vals$TRTPN), ]
# Only want to display first p-value
# by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=put(nvalue1,pvalue5.3) else ""
table_vals$nvalue1c <- ""
# first per (PARAM, tefl, nvalue1) group -> first row per tefl since nvalue1 constant within tefl
# Implement: order as above, then mark first row of each PARAM+tefl+nvalue1
table_vals <- table_vals %>%
  group_by(PARAM, tefl, nvalue1) %>%
  mutate(nvalue1c = ifelse(row_number() == 1, sapply(nvalue1, fmt_pvalue5.3), "")) %>%
  ungroup() %>% as.data.frame()
# Re-apply SAS sort order for display (already ordered)
table_vals <- table_vals[order(table_vals$PARAM, -xtfrm(table_vals$tefl), table_vals$nvalue1, table_vals$TRTPN), ]

# Final report columns: param tefl trtpn totn nc pctc nvalue1c
report_df <- table_vals %>% select(param = PARAM, tefl, trtpn = TRTPN, totn, nc, pctc, nvalue1c)

# Write CSV of table body (and dataset)
write.csv(report_df, file.path(out, "P037.csv"), row.names = FALSE, na = "")

# Build text rendering mimicking PROC REPORT with titles/footnotes
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
foot1 <- "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)"
foot2 <- "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the"
foot3 <- "specified category."
foot4 <- "*P values are from Fisher's Exact test."

# Header labels with split "|": two-line headers
# param: Vital Sign / (unit) ; tefl: Abnormality / Direction ; trtpn: Treatment ; totn: N ; nc: n ; pctc: (%) ; nvalue1c: P value*
# Create fixed-width text
# Determine column widths
# Use simple padded table
col_names_l1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
col_names_l2 <- c("(unit)", "Direction", "", "", "", "", "")
# Prepare body strings
fmt_cell <- function(x, w) {
  x <- ifelse(is.na(x), "", as.character(x))
  # left justify like SAS just=left
  sprintf(paste0("%-", w, "s"), x)
}
# widths: param 35, tefl 12, trtpn 10, totn 8, nc 8, pctc 10, pval 10
w_param <- max(nchar(c("Vital Sign","(unit)", report_df$param)), na.rm = TRUE) + 2
w_param <- max(w_param, 30)
w_tefl <- max(nchar(c("Abnormality","Direction", report_df$tefl)), na.rm = TRUE) + 2
w_trtpn <- max(nchar(c("Treatment", as.character(report_df$trtpn))), na.rm = TRUE) + 2
w_totn <- max(nchar(c("N", as.character(report_df$totn))), na.rm = TRUE) + 2
w_nc <- max(nchar(c("n", report_df$nc)), na.rm = TRUE) + 2
w_pctc <- max(nchar(c("(%)", report_df$pctc)), na.rm = TRUE) + 2
w_pval <- max(nchar(c("P value*", report_df$nvalue1c)), na.rm = TRUE) + 2
w_pval <- max(w_pval, 10)

header1 <- paste0(fmt_cell("Vital Sign", w_param), fmt_cell("Abnormality", w_tefl),
                  fmt_cell("Treatment", w_trtpn), fmt_cell("N", w_totn),
                  fmt_cell("n", w_nc), fmt_cell("(%)", w_pctc), fmt_cell("P value*", w_pval))
header2 <- paste0(fmt_cell("(unit)", w_param), fmt_cell("Direction", w_tefl),
                  fmt_cell("", w_trtpn), fmt_cell("", w_totn),
                  fmt_cell("", w_nc), fmt_cell("", w_pctc), fmt_cell("", w_pval))
sep_line <- paste0(rep("_", nchar(header1)), collapse = "")

lines <- c(title1, title2, "", header1, header2, sep_line)
for (i in seq_len(nrow(report_df))) {
  r <- report_df[i, ]
  line <- paste0(fmt_cell(r$param, w_param), fmt_cell(r$tefl, w_tefl),
                 fmt_cell(r$trtpn, w_trtpn), fmt_cell(r$totn, w_totn),
                 fmt_cell(r$nc, w_nc), fmt_cell(r$pctc, w_pctc), fmt_cell(r$nvalue1c, w_pval))
  lines <- c(lines, line)
}
lines <- c(lines, "", foot1, foot2, foot3, foot4)

writeLines(lines, file.path(out, "P037.txt"))
message("Wrote P037.csv and P037.txt")
print(report_df)
