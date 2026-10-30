# P037_target21.sas -> R translation
# Vital Signs - Treatment-Emergent Abnormal High or Low at Any Time
# Outputs: P037.csv + P037.txt in OUT_DIR

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
})

# helper: SAS round half away from zero (for display)
sas_round <- function(x, d=0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

# helper: SAS PVALUEw.d format
sas_pvalue_fmt <- function(p, w=5, d=3) {
  if (is.na(p)) return("")
  thresh <- 10^(-d)
  if (p < thresh) {
    # "<.001" for d=3, "<.0001" for d=4 etc.
    zeros <- paste0(rep("0", d-1), collapse="")
    lab <- paste0("<.", zeros, "1")
    # pad to width w (SAS right aligns)
    sprintf(paste0("%", w, "s"), lab)
  } else {
    sprintf(paste0("%", w, ".", d, "f"), p)
  }
}
sas_pvalue_trim <- function(p, w=5, d=3) trimws(sas_pvalue_fmt(p, w, d))

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# locate advs.xpt
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADVS.xpt",
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  "data/raw/cdisc-pilot/adam/advs.xpt",
  "../data/raw/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) {
  # fallback to &root. default
  advs_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
}
# also try forward slashes variant
advs_path <- gsub("\\\\", "/", advs_path)

advs_raw <- haven::read_xpt(advs_path)

# zap labels/formats to plain vectors
advs_raw <- haven::zap_labels(advs_raw)
advs_raw <- haven::zap_formats(advs_raw)
advs_raw <- haven::zap_label(advs_raw)

# standardise names to upper for logic (case-insensitive matching)
names(advs_raw) <- toupper(names(advs_raw))

# ensure character columns are character and trim
char_cols <- c("USUBJID","PARAM","PARAMCD","CRIT1","CRIT1FL","CRIT2","CRIT2FL","SAFFL")
for (cc in char_cols) {
  if (cc %in% names(advs_raw)) {
    advs_raw[[cc]] <- as.character(advs_raw[[cc]])
    advs_raw[[cc]][is.na(advs_raw[[cc]])] <- ""
    advs_raw[[cc]] <- trimws(advs_raw[[cc]])
    # haven may have "" for missing; keep "" as missing
  }
}
# numeric cols
num_cols <- c("TRTPN","ATPTN","ADY","AVAL")
for (nc in num_cols) if (nc %in% names(advs_raw)) advs_raw[[nc]] <- as.numeric(advs_raw[[nc]])

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         (CRIT1FL != "" | CRIT2FL != "")) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# ---- counts ----
# totn_low: not missing crit1fl
totn_low <- advs %>%
  filter(CRIT1FL != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "Low")

n_low <- advs %>%
  filter(CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs %>%
  filter(CRIT2FL != "") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "High")

n_high <- advs %>%
  filter(CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups="drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort like SAS (by TRTPN PARAM) - not strictly needed if we join by TEFL
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# Combine N and n - merge by TRTPN PARAM TEFL (SAS merges by TRTPN PARAM but pairing is by order High/Low)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N / TOTN * 100)

# ---- Fisher Exact p-value ----
# helper to get last per USUBJID after sort by USUBJID, FLG (missing first)
get_last_per_subject <- function(df, flg_col) {
  # flg_col is "CRIT1FL" or "CRIT2FL"
  # ensure missing ("") sorts first: arrange will put "" before "N","Y"
  df2 <- df %>%
    mutate(.flg = .data[[flg_col]]) %>%
    mutate(.flg = ifelse(is.na(.flg), "", .flg))
  # arrange by USUBJID, .flg
  df2 <- df2 %>% arrange(USUBJID, .flg)
  # last per USUBJID
  df2 %>% group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup()
}

fisher_one <- function(flg_col, tefl_label) {
  dlast <- get_last_per_subject(advs, flg_col)
  # PROC FREQ excludes missing by default
  dlast <- dlast %>% filter(.flg != "" & !is.na(.flg))
  # For each PARAM, build table TRTPN x FLG
  params <- unique(dlast$PARAM)
  res <- tibble()
  for (p in params) {
    sub <- dlast %>% filter(PARAM == p)
    # create contingency table
    # ensure TRTPN and FLG are factors to keep all levels
    tab <- table(sub$TRTPN, sub$.flg)
    # if table has <2 rows or <2 cols, fisher still computable but need at least 2x2
    # Use fisher.test
    if (nrow(tab) >= 2 && ncol(tab) >= 2) {
      ft <- fisher.test(tab)
      pval <- ft$p.value
    } else {
      pval <- NA_real_
    }
    res <- bind_rows(res, tibble(TEFL = tefl_label, PARAM = p, NVALUE1 = pval))
  }
  res
}

fisher_crit1fl <- fisher_one("CRIT1FL", "Low")
fisher_crit2fl <- fisher_one("CRIT2FL", "High")

all_fisher <- bind_rows(fisher_crit1fl, fisher_crit2fl) %>%
  arrange(TEFL)

# sort all_n by TEFL TRTPN (SAS)
all_n <- all_n %>% arrange(TEFL, TRTPN)

# Merge Fisher with N/n by TEFL (SAS: merge all_n all_fisher by tefl)
# This replicates many-to-one: each TEFL p-value replicated across TRTPN
table_vals <- left_join(all_n, all_fisher, by=c("TEFL","PARAM"))

# Create display variables
table_vals <- table_vals %>%
  mutate(
    # put(pct,5.1) then strip then parentheses
    PCT_FMT = ifelse(is.na(PCT), "", trimws(sprintf("%5.1f", PCT))),
    PCTC = ifelse(PCT_FMT == "", "", paste0("(", PCT_FMT, ")")),
    NC = ifelse(is.na(N), "", trimws(as.character(N))),
    # keep NVALUE1 for sorting
    NVALUE1 = NVALUE1
  )

# sort by param descending tefl nvalue1 (SAS)
table_vals <- table_vals %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Only first p-value per BY group displayed
# BY param descending tefl nvalue1
table_vals <- table_vals %>%
  group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(is_first = row_number() == 1) %>%
  ungroup() %>%
  mutate(NVALUE1C = ifelse(is_first, sas_pvalue_trim(NVALUE1, 5, 3), "")) %>%
  select(-is_first)

# For display, ensure columns order as in PROC REPORT
# columns param tefl trtpn totn nc pctc nvalue1c
final_tbl <- table_vals %>%
  select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, PCT, N, NVALUE1) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# ---- Write CSV of table body (and dataset) ----
# CSV for dataset: use SAS variable names
out_csv <- final_tbl %>%
  select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C)
# also include numeric versions for completeness
write.csv(out_csv, file.path(out, "P037.csv"), row.names=FALSE, na="")

# ---- Text rendering (P037.txt) ----
# Build text file mimicking PROC REPORT
txt_path <- file.path(out, "P037.txt")
con <- file(txt_path, open="wt")

writeLines("Vital Signs", con)
writeLines("Treatment-Emergent Abnormal High or Low at Any Time", con)
writeLines("", con)
# header lines with split "|"
# Define header: Vital Sign|(unit) | Abnormality|Direction | Treatment | N | n | (%) | P value*
# We'll create two header rows
hdr1 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
                "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
                "(unit)", "Direction", "", "", "", "", "")
writeLines(paste(rep("-", 132), collapse=""), con)
writeLines(hdr1, con)
writeLines(hdr2, con)
writeLines(paste(rep("-", 132), collapse=""), con)

# Data rows: order as final_tbl
for (i in seq_len(nrow(final_tbl))) {
  r <- final_tbl[i,]
  # format TRTPN and TOTN as integers without decimals
  trtpn_disp <- ifelse(is.na(r$TRTPN), "", as.character(as.integer(r$TRTPN)))
  totn_disp <- ifelse(is.na(r$TOTN), "", as.character(as.integer(r$TOTN)))
  line <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
                  substr(r$PARAM,1,35),
                  r$TEFL,
                  trtpn_disp,
                  totn_disp,
                  r$NC,
                  r$PCTC,
                  trimws(r$NVALUE1C))
  writeLines(line, con)
}
writeLines(paste(rep("-", 132), collapse=""), con)
writeLines("", con)
writeLines("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)", con)
writeLines("baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the", con)
writeLines("specified category.", con)
writeLines("*P values are from Fisher's Exact test.", con)
close(con)

# Also write a detailed CSV with all numeric values for verification
detail_csv <- final_tbl %>%
  mutate(PCT = round(PCT,4),
         NVALUE1 = round(NVALUE1,4)) %>%
  select(PARAM, TEFL, TRTPN, TOTN, N, PCT, PCTC, NC, NVALUE1, NVALUE1C)
write.csv(detail_csv, file.path(out, "P037_detail.csv"), row.names=FALSE, na="")

# Ensure at least P037.csv and P037.txt exist
stopifnot(file.exists(file.path(out, "P037.csv")))
stopifnot(file.exists(file.path(out, "P037.txt")))
