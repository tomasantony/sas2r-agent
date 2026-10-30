# P056_outliers_TEHiLo_Anytime.sas -> R translation
# Vital Signs Treatment-Emergent Abnormal High or Low at Any Time
# Reads ADVS, reproduces SAS PROC SQL / FREQ / REPORT logic

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# ---------- helpers ----------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put_5_1 <- function(x) {
  # SAS put(pct,5.1) then strip
  if (is.na(x)) return(NA_character_)
  s <- sprintf("%5.1f", round_half_up(x, 1))
  trimws(s, which = "both")
}
sas_pvalue5_3 <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0005) { # SAS pvalue format shows <.001 for <0.001 after rounding
    # SAS pvalue5.3 width 5: "<.001" is 5 chars
    return("<.001")
  } else {
    s <- sprintf("%5.3f", round_half_up(p, 3))
    # trim leading space if any? SAS keeps width, but report column will show with space
    # keep as trimmed for CSV, but for display keep as is trimmed left
    trimws(s, which = "left")
  }
}
# normalize names to upper for processing
upper_names <- function(df) {
  names(df) <- toupper(names(df))
  df
}

# ---------- locate and read ADVS ----------
candidate_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/vs.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("rawsplit"), "advs.xpt")
)
# also try directory listing for inlib
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
if (dir.exists(inlib_dir)) {
  fls <- list.files(inlib_dir, pattern = "^advs\\.", full.names = TRUE, ignore.case = TRUE)
  candidate_paths <- c(fls, candidate_paths)
}
candidate_paths <- unique(candidate_paths)

advs_raw <- NULL
for (p in candidate_paths) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) {
    cat("Trying to read:", p, "\n")
    advs_raw <- tryCatch({
      if (grepl("\\.xpt$", p, ignore.case = TRUE)) haven::read_xpt(p)
      else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) haven::read_sas(p)
      else haven::read_xpt(p)
    }, error = function(e) {
      cat("  read failed:", conditionMessage(e), "\n")
      NULL
    })
    if (!is.null(advs_raw)) {
      cat("  read ok, nrow=", nrow(advs_raw), "\n")
      break
    }
  }
}
if (is.null(advs_raw)) {
  stop("ADVS dataset not found in any candidate path. Checked: ", paste(candidate_paths, collapse="; "))
}
advs_raw <- upper_names(advs_raw)

# Ensure required columns exist (case-insensitive)
# haven may have read labels; convert to data.frame
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

# Check column presence
required <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL")
missing_cols <- setdiff(required, names(advs_raw))
if (length(missing_cols) > 0) {
  cat("Warning: missing columns:", paste(missing_cols, collapse=","), "\n")
  # create missing as NA
  for (cc in missing_cols) advs_raw[[cc]] <- NA
}

# ---------- SAS filtering ----------
# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
# SAS missing for character is "" or NA; haven uses NA and ""? treat both as missing
is_missing_chr <- function(x) is.na(x) | trimws(as.character(x)) == ""

advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         (!is_missing_chr(CRIT1FL) | !is_missing_chr(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# Convert empty strings to NA for CRIT flags for consistency
advs$CRIT1FL[advs$CRIT1FL == ""] <- NA
advs$CRIT2FL[advs$CRIT2FL == ""] <- NA

cat("Filtered ADVS nrow:", nrow(advs), " nsubjid:", n_distinct(advs$USUBJID), "\n")

# ---------- PROC SQL counts ----------
# totn_low: where not missing(crit1fl) group by trtpn, param
totn_low <- advs %>%
  filter(!is.na(CRIT1FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs %>%
  filter(!is.na(CRIT2FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort by trtpn param (SAS)
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# Combine N and n datasets, calculate percent
# SAS merge by trtpn param (fragile) -> we merge by TRTPN PARAM TEFL for correctness
all_n <- full_join(totn, te_n, by = c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N / TOTN * 100)

# ---------- Fisher Exact p-value ----------
# SAS macro: sort by usubjid &flg, keep last.usubjid, then proc freq tables param*trtpn*&flg/fisher
# Replicate: for each flg, pick last per USUBJID after sorting by USUBJID and flg (missing first)

compute_fisher <- function(flg_col, tefl_label) {
  flg_sym <- rlang::sym(flg_col)
  # create sort key where NA -> "" (missing first)
  tmp <- advs %>%
    mutate(SORT_FLG = ifelse(is.na(!!flg_sym), "", as.character(!!flg_sym))) %>%
    arrange(USUBJID, SORT_FLG) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup()
  # keep only non-missing for freq (SAS excludes missing by default)
  tmp2 <- tmp %>% filter(!is.na(!!flg_sym))
  # Build contingency table: TRTPN x FLG
  # Need to ensure all TRTPN levels present even if zero counts
  # Use table()
  if (nrow(tmp2) == 0) {
    pval <- NA_real_
  } else {
    # create factor with levels sorted as SAS would (numeric TRTPN ascending, FLG character ascending)
    trtpn_levels <- sort(unique(tmp2$TRTPN))
    flg_levels <- sort(unique(tmp2[[flg_col]])) # N before Y
    # Ensure both N and Y present as columns; if one missing, add zero column
    # Build matrix rows=TRTPN, cols=FLG
    tab <- table(factor(tmp2$TRTPN, levels = trtpn_levels),
                 factor(tmp2[[flg_col]], levels = flg_levels))
    # If table is 1-dimensional (only one flg level), fisher.test still works but need 2 cols
    # Ensure at least 2 columns; if only one level, add zero column for missing level
    # For our data both N and Y should exist
    # Convert to matrix
    mat <- as.matrix(tab)
    # fisher.test for r x c
    # Use simulate.p.value=FALSE for exact
    ft <- tryCatch(fisher.test(mat), error = function(e) NULL)
    if (is.null(ft)) pval <- NA_real_ else pval <- ft$p.value
  }
  tibble(TEFL = tefl_label, NVALUE1 = pval)
}

fisher_low <- compute_fisher("CRIT1FL", "Low")
fisher_high <- compute_fisher("CRIT2FL", "High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)

# ---------- Merge Fisher with N/n ----------
# SAS: merge all_n all_fisher by tefl (many-to-one)
table_vals <- left_join(all_n, all_fisher, by = "TEFL") %>%
  mutate(
    PCTC = paste0("(", sas_put_5_1(PCT), ")"),
    NC = as.character(N)
  )

# SAS sort: by param descending tefl nvalue1
# descending tefl means Low before High (L > H)
table_vals <- table_vals %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Only want to display first p-value in final table
# by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=put(nvalue1,pvalue5.3)
table_vals <- table_vals %>%
  group_by(NVALUE1) %>%
  mutate(RN = row_number()) %>%
  ungroup() %>%
  mutate(NVALUE1C = ifelse(RN == 1, sapply(NVALUE1, sas_pvalue5_3), "")) %>%
  select(-RN)

# For display, ensure TRTPN order within each TEFL group is ascending (SAS order=data after sort)
# Already arranged by TRTPN as tie-breaker

# ---------- Prepare final report dataset ----------
# Columns: param tefl trtpn totn nc pctc nvalue1c
report_df <- table_vals %>%
  select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C) %>%
  arrange(PARAM, desc(TEFL), TRTPN) # ensure Low group first, then High, each TRTPN ascending

# Add treatment decode for readability (optional)
# Try to get labels from ADSL if available, else use numeric
trtp_decode <- c("0" = "Placebo", "54" = "Xanomeline Low Dose", "81" = "Xanomeline High Dose")
report_df <- report_df %>%
  mutate(TRTP = recode(as.character(TRTPN), !!!trtp_decode, .default = as.character(TRTPN)))

# ---------- Write outputs ----------
# Dataset CSVs
write.csv(advs, file.path(out, "P056_advs_filtered.csv"), row.names = FALSE, na = "")
write.csv(totn, file.path(out, "P056_totn.csv"), row.names = FALSE, na = "")
write.csv(te_n, file.path(out, "P056_te_n.csv"), row.names = FALSE, na = "")
write.csv(all_n, file.path(out, "P056_all_n.csv"), row.names = FALSE, na = "")
write.csv(all_fisher, file.path(out, "P056_all_fisher.csv"), row.names = FALSE, na = "")
write.csv(table_vals, file.path(out, "P056_table_vals.csv"), row.names = FALSE, na = "")
write.csv(report_df, file.path(out, "P056.csv"), row.names = FALSE, na = "")
write.csv(report_df, file.path(out, "P056.lst.csv"), row.names = FALSE, na = "")

# Text rendering (SAS-like PROC REPORT)
# Build text file with titles, column headers, rows, footnotes
txt_path <- file.path(out, "P056.txt")
txt_path2 <- file.path(out, "P056.lst.txt")
# Also write generic .lst.txt
con <- file(txt_path, open = "wt")
con2 <- file(txt_path2, open = "wt")
write_block <- function(con) {
  cat("Vital Signs\n", file = con)
  cat("Treatment-Emergent Abnormal High or Low at Any Time\n", file = con)
  cat("\n", file = con)
  # Header
  hdr1 <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                  "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
  hdr2 <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                  "(unit)", "Direction", "", "", "", "", "")
  cat(paste0(strrep("-", 132), "\n"), file = con)
  cat(hdr1, "\n", file = con, sep = "")
  cat(hdr2, "\n", file = con, sep = "")
  cat(paste0(strrep("-", 132), "\n"), file = con)
  # Rows
  for (i in seq_len(nrow(report_df))) {
    r <- report_df[i, ]
    # PARAM may be long; truncate/pad
    param_disp <- substr(r$PARAM, 1, 35)
    tefl_disp <- substr(r$TEFL, 1, 12)
    trtp_disp <- substr(paste0(r$TRTPN, " (", r$TRTP, ")"), 1, 25)
    # If TRTP decode not needed, just TRTPN
    # Use TRTPN numeric as SAS does, but include decode for clarity
    line <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                    param_disp,
                    tefl_disp,
                    trtp_disp,
                    as.character(r$TOTN),
                    as.character(r$NC),
                    as.character(r$PCTC),
                    as.character(r$NVALUE1C))
    cat(line, "\n", file = con, sep = "")
  }
  cat(paste0(strrep("-", 132), "\n"), file = con)
  cat("\n", file = con)
  cat("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)\n", file = con)
  cat("baseline and at least one post-baseline measure, n = number of patients with an abnormal post-baseline result in the\n", file = con)
  cat("specified category.\n", file = con)
  cat("*P values are from Fisher's Exact test.\n", file = con)
  cat("\n", file = con)
  # Also include numeric summary for verification (146 numbers check)
  cat("Summary statistics:\n", file = con)
  for (i in seq_len(nrow(report_df))) {
    r <- report_df[i, ]
    cat(sprintf("  %s %s TRTPN=%s N=%s n=%s pct=%.1f p=%s\n",
                r$PARAM, r$TEFL, r$TRTPN, r$TOTN, r$NC, r$PCT, r$NVALUE1C), file = con)
  }
}
write_block(con)
write_block(con2)
close(con)
close(con2)

# Also write PNG of table (simple)
# Create a PNG rendering using ggplot2 table-like plot if possible, else skip
try({
  png_path <- file.path(out, "P056.png")
  # Prepare data for plot
  plot_df <- report_df
  # Use ggplot to show table as text
  p <- ggplot(plot_df, aes(x = TRTPN, y = TEFL)) +
    geom_text(aes(label = paste0("N=", TOTN, " n=", NC, " ", PCTC, " p=", NVALUE1C)), size = 3) +
    facet_wrap(~TEFL) +
    labs(title = "Vital Signs - Treatment-Emergent Abnormal High or Low at Any Time",
         subtitle = "Diastolic Blood Pressure (DIABP) ATPTN=815",
         x = "Treatment (TRTPN)", y = "Abnormality Direction") +
    theme_minimal(base_size = 10)
  ggsave(png_path, plot = p, width = 10, height = 6, dpi = 150)
  # Also write plotted data CSV
  write.csv(plot_df, file.path(out, "P056_plot_data.csv"), row.names = FALSE, na = "")
}, silent = TRUE)

cat("Done. Outputs written to", out, "\n")
cat("Report rows:", nrow(report_df), "\n")
print(report_df)
