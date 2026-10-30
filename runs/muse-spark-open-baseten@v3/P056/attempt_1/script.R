# P056_outliers_TEHiLo_Anytime.sas -> R translation
# Output: P056 table (Vital Signs TE abnormal High/Low)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  # SAS ROUND half away from zero
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
sas_put_5_1 <- function(x) {
  # SAS put(pct, 5.1) : width 5, 1 decimal, round half away from zero
  if (is.na(x)) return("")
  v <- round_half_up(x, 1)
  # format with 1 decimal, width 5 (SAS pads left with spaces, strip removes)
  sprintf("%5.1f", v)
}
sas_pvalue5.3 <- function(p) {
  if (is.na(p)) return("")
  # SAS pvalue5.3 : values < 0.001 printed as "<.001", else 0.xxx with 3 decimals
  # threshold = 10^-d = 0.001
  if (p < 0.001) {
    return("<.001")
  } else {
    v <- round_half_up(p, 3)
    # ensure 3 decimals, leading zero
    sprintf("%5.3f", v)
  }
}
strip <- function(x) trimws(x, which = "both")

# locate ADVS -----------------------------------------------------------
# SAS librefs: inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam
# rawadam -> D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
candidates <- c(
  Sys.getenv("rawadam"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam",
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam")
)
candidates <- unique(candidates[candidates != ""])
advs_path <- NULL
for (d in candidates) {
  for (f in c("advs.xpt", "advs.sas7bdat", "ADVS.xpt", "ADVS.sas7bdat")) {
    p <- file.path(d, f)
    if (file.exists(p)) { advs_path <- p; break }
  }
  if (!is.null(advs_path)) break
}
# also try inlib env
inlib_dir <- Sys.getenv("inlib")
if (is.null(advs_path) && inlib_dir != "" && dir.exists(inlib_dir)) {
  for (f in c("advs.xpt","advs.sas7bdat")) {
    p <- file.path(inlib_dir, f)
    if (file.exists(p)) { advs_path <- p; break }
  }
}
# fallback hard-coded
if (is.null(advs_path)) {
  p1 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat"
  p2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
  p3 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
  for (p in c(p1,p2,p3)) if (file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) stop("ADVS file not found in candidates")

message("Reading ADVS from: ", advs_path)
if (grepl("\\.xpt$", advs_path, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(advs_path)
} else {
  advs_raw <- haven::read_sas(advs_path)
}
# zap labels/formats to plain vectors
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_formats(advs_raw)

# ensure character columns are character
char_cols <- c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL")
for (cn in char_cols) if (cn %in% names(advs_raw)) advs_raw[[cn]] <- as.character(advs_raw[[cn]])

# filter as SAS: WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
# ATPTN may be numeric or character; coerce
if (!"ATPTN" %in% names(advs_raw)) stop("ATPTN not in ADVS")
advs_raw$ATPTN <- suppressWarnings(as.numeric(advs_raw$ATPTN))
advs_raw$ADY <- suppressWarnings(as.numeric(advs_raw$ADY))
advs_raw$TRTPN <- suppressWarnings(as.numeric(advs_raw$TRTPN))

advs <- advs_raw[
  advs_raw$PARAMCD == "DIABP" &
  !is.na(advs_raw$ATPTN) & advs_raw$ATPTN == 815 &
  !is.na(advs_raw$ADY) & advs_raw$ADY > 1 &
  advs_raw$SAFFL == "Y" &
  (!is_missing_c(advs_raw$CRIT1FL) | !is_missing_c(advs_raw$CRIT2FL)),
  c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL")
]

message("ADVS filtered rows: ", nrow(advs))

# helper to count distinct USUBJID
count_distinct <- function(df) {
  length(unique(df$USUBJID))
}

# totn_low : where not missing(crit1fl) group by trtpn, param
library(dplyr)

# ensure PARAM exists
if (!"PARAM" %in% names(advs)) advs$PARAM <- "Diastolic Blood Pressure (mmHg)"

totn_low <- advs %>%
  filter(!is_missing_c(CRIT1FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "Low")

totn_high <- advs %>%
  filter(!is_missing_c(CRIT2FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(tefl = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort and merge to get all_n with pct
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# SAS merge by trtpn param : need to keep all combinations from totn and te_n
# In SAS, merge totn te_n by trtpn param; if n missing then pct = ./.
all_n <- full_join(totn, te_n, by = c("TRTPN","PARAM","tefl")) %>%
  mutate(pct = n / totn * 100)

# Fisher exact p-value --------------------------------------------------
# SAS macro: sort by usubjid &flg, keep last.usubjid, then proc freq tables param*trtpn*&flg / fisher
# For each flag, deduplicate to one row per USUBJID taking max flag value (Y > N > missing)

dedup_last <- function(df, flag_col) {
  # flag_col is string "CRIT1FL" or "CRIT2FL"
  # sort by USUBJID, flag (missing first, N, Y)
  # Use SAS character ordering: "" < "N" < "Y"
  tmp <- df[, c("USUBJID","TRTPN","PARAM", flag_col), drop = FALSE]
  # create sort key: missing -> 0, N->1, Y->2, others -> 1.5
  flag_vals <- tmp[[flag_col]]
  ord <- ifelse(is_missing_c(flag_vals), 0,
                ifelse(flag_vals == "N", 1,
                       ifelse(flag_vals == "Y", 2, 1)))
  tmp$ord <- ord
  tmp <- tmp[order(tmp$USUBJID, tmp$ord), ]
  # keep last per USUBJID
  tmp <- tmp[!duplicated(tmp$USUBJID, fromLast = TRUE), ]
  tmp$ord <- NULL
  tmp
}

fisher_for_flag <- function(flag_col, tefl_label) {
  d <- dedup_last(advs, flag_col)
  # exclude missing flag (SAS PROC FREQ excludes missing by default)
  d <- d[!is_missing_c(d[[flag_col]]), ]
  if (nrow(d) == 0) {
    return(data.frame(tefl = tefl_label, nvalue1 = NA_real_, stringsAsFactors = FALSE))
  }
  # For each PARAM level, compute fisher.test on table TRTPN x flag
  # Here PARAM is single value, but handle generally
  params <- unique(d$PARAM)
  res <- lapply(params, function(p) {
    sub <- d[d$PARAM == p, ]
    tab <- table(sub$TRTPN, sub[[flag_col]])
    # Ensure table is at least 2x2; if only one column, fisher not defined -> NA
    if (nrow(tab) < 2 || ncol(tab) < 2) {
      pval <- NA_real_
    } else {
      # fisher.test for R x C
      ft <- suppressWarnings(fisher.test(tab))
      pval <- ft$p.value
    }
    data.frame(PARAM = p, tefl = tefl_label, nvalue1 = pval, stringsAsFactors = FALSE)
  })
  do.call(rbind, res)
}

fisher_low <- fisher_for_flag("CRIT1FL", "Low")
fisher_high <- fisher_for_flag("CRIT2FL", "High")

all_fisher <- bind_rows(fisher_low, fisher_high)
# SAS keeps only where name1="XP2_FISH" -> one row per tefl (per param)
# For single param, one row per tefl
# Collapse to one per tefl (if multiple params, would be per param; but SAS merges by tefl only)
# In SAS they merge by tefl only, so if multiple params with different p-values, merge would be ambiguous.
# Here we have single param, so take first
all_fisher_by_tefl <- all_fisher %>%
  group_by(tefl) %>%
  summarise(nvalue1 = first(nvalue1), .groups = "drop")

# Merge Fisher with all_n by tefl (SAS retain behavior)
# SAS: proc sort all_fisher by tefl; proc sort all_n by tefl trtpn; merge by tefl
all_n_sorted <- all_n %>% arrange(tefl, TRTPN)
all_fisher_sorted <- all_fisher_by_tefl %>% arrange(tefl)

table_vals <- left_join(all_n_sorted, all_fisher_sorted, by = "tefl")

# Create pctc and nc as SAS
table_vals <- table_vals %>%
  mutate(
    pctc = paste0("(", strip(sas_put_5_1(pct)), ")"),
    nc = as.character(n)
  )

# Sort by param descending tefl nvalue1 (SAS: by param descending tefl nvalue1)
# param descending: single value, no effect; tefl descending: Low > High, so Low first
table_vals <- table_vals %>%
  arrange(PARAM, desc(tefl), nvalue1, TRTPN)

# Only want to display first p-value in final table: by param descending tefl nvalue1, if first.nvalue1 then nvalue1c
table_vals <- table_vals %>%
  group_by(PARAM, tefl, nvalue1) %>%
  mutate(
    rn = row_number(),
    nvalue1c = ifelse(rn == 1, sas_pvalue5.3(nvalue1), "")
  ) %>%
  ungroup() %>%
  select(-rn)

# For display, ensure TRTPN is integer and totn, n are integer
table_vals$TRTPN <- as.integer(table_vals$TRTPN)
table_vals$totn <- as.integer(table_vals$totn)
table_vals$n <- as.integer(table_vals$n)

# Reorder columns to match PROC REPORT: param tefl trtpn totn nc pctc nvalue1c
table_vals_out <- table_vals %>%
  select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c, nvalue1, pct, n) %>%
  arrange(desc(PARAM), desc(tefl), TRTPN)

# For CSV output, keep display columns plus numeric for verification
# Write dataset CSV (as SAS dataset counterpart)
write.csv(table_vals_out, file.path(out, "P056.csv"), row.names = FALSE, na = "")

# Also write table body CSV (same)
write.csv(table_vals_out %>% select(PARAM, tefl, TRTPN, totn, nc, pctc, nvalue1c),
          file.path(out, "P056_table.csv"), row.names = FALSE, na = "")

# Text rendering --------------------------------------------------------
# Titles and footnotes verbatim
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
footnote1 <- "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)"
footnote2 <- "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the"
footnote3 <- "specified category."
footnote4 <- "*P values are from Fisher's Exact test."

# Build text table
# Column headers as in PROC REPORT
hdr1 <- c("Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- c("(unit)", "Direction", "", "", "", "", "")

# Format rows
# Determine column widths for monospaced rendering
# Use simple fixed widths approximating SAS style
col_widths <- c(30, 12, 12, 6, 6, 8, 10)

fmt_row <- function(vals, widths) {
  # left justify each column, truncate/pad
  sapply(seq_along(vals), function(i) {
    v <- as.character(vals[i])
    if (is.na(v)) v <- ""
    # truncate if longer than width
    if (nchar(v) > widths[i]) v <- substr(v, 1, widths[i])
    sprintf(paste0("%-", widths[i], "s"), v)
  }) %>% paste(collapse = " ")
}

lines <- c()
lines <- c(lines, title1, title2, "")
# header lines
lines <- c(lines, fmt_row(hdr1, col_widths))
lines <- c(lines, fmt_row(hdr2, col_widths))
lines <- c(lines, paste(rep("-", sum(col_widths) + length(col_widths) -1), collapse=""))

for (i in seq_len(nrow(table_vals_out))) {
  r <- table_vals_out[i, ]
  vals <- c(
    as.character(r$PARAM),
    as.character(r$tefl),
    as.character(r$TRTPN),
    as.character(r$totn),
    as.character(r$nc),
    as.character(r$pctc),
    as.character(r$nvalue1c)
  )
  lines <- c(lines, fmt_row(vals, col_widths))
}
lines <- c(lines, "")
lines <- c(lines, footnote1, footnote2, footnote3, footnote4)

# Write lst/txt
writeLines(lines, file.path(out, "P056.lst"))
writeLines(lines, file.path(out, "P056.txt"))

message("Outputs written to ", out)
