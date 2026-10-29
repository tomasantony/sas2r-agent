```r
# P056_outliers_TEHiLo_Anytime.sas -> R translation
# Vital Signs Treatment-Emergent Abnormal High or Low at Any Time (DIABP, ATPTN=815)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

root <- "D:/meta_hackathon/sas2r-agent"
inlib_dir <- file.path(root, "data/derived/cdisc-pilot/adam")
rawadam_dir <- file.path(root, "data/raw/cdisc-pilot/adam")

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}
strip_sas <- function(x) trimws(x)

sas_round <- function(x, digits) {
  # SAS ROUND: half away from zero
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / (10^digits))
}
fmt_pct_5_1 <- function(x) {
  if (is.na(x)) return(NA_character_)
  r <- sas_round(x, 1)
  sprintf("%.1f", r)
}
fmt_best <- function(x) {
  if (is.na(x)) return(".")
  # SAS BEST. for integers: plain integer string
  # use format with no scientific for typical counts
  formatC(x, format = "fg", digits = 12, flag = "#")
}
fmt_pvalue <- function(p, d = 3) {
  if (is.na(p)) return("")
  # SAS PVALUEw.d : small p shown as <.001 etc.
  thresh <- 10^(-d)
  # SAS displays "<.001" when p < thresh (or rounds to 0)
  # Use threshold rule: if p < thresh then "<.xxx"
  if (!is.na(p) && p < thresh) {
    # build "<.001" for d=3
    s <- paste0("<.", paste0(rep("0", d - 1), collapse = ""), "1")
    return(s)
  }
  r <- sas_round(p, d)
  if (!is.na(r) && r < thresh) {
    s <- paste0("<.", paste0(rep("0", d - 1), collapse = ""), "1")
    return(s)
  }
  sprintf(paste0("%.", d, "f"), r)
}

# ---- Read ADVS ----
advs <- NULL
cand1 <- file.path(inlib_dir, "advs.sas7bdat")
cand2 <- file.path(inlib_dir, "advs.xpt")
cand3 <- file.path(rawadam_dir, "advs.xpt")
cand4 <- file.path(inlib_dir, "ADVS.sas7bdat")
if (file.exists(cand1)) {
  advs <- haven::read_sas(cand1)
} else if (file.exists(cand4)) {
  advs <- haven::read_sas(cand4)
} else if (file.exists(cand2)) {
  advs <- haven::read_xpt(cand2)
} else if (file.exists(cand3)) {
  advs <- haven::read_xpt(cand3)
} else {
  # fallback: list directory
  fl <- list.files(inlib_dir, pattern = "(?i)advs.*", full.names = TRUE)
  if (length(fl) > 0) {
    if (grepl("\\.sas7bdat$", fl[1], ignore.case = TRUE)) advs <- haven::read_sas(fl[1])
    else advs <- haven::read_xpt(fl[1])
  } else stop("ADVS not found")
}
# Normalize names to uppercase for SAS-like handling, keep original
names(advs) <- toupper(names(advs))

# Ensure needed columns exist
# Keep USUBJID TRTPN PARAM PARAMCD AVAL CRIT1 CRIT1FL CRIT2 CRIT2FL ATPTN ADY SAFFL
# Convert haven_labelled to plain
for (cc in names(advs)) {
  if (haven::is.labelled(advs[[cc]])) {
    # keep numeric values; for character labelled keep as character
    advs[[cc]] <- haven::zap_labels(haven::zap_formats(advs[[cc]]))
  }
}
# Coerce types
advs$USUBJID <- as.character(advs$USUBJID)
advs$PARAM <- as.character(advs$PARAM)
advs$PARAMCD <- as.character(advs$PARAMCD)
advs$CRIT1 <- as.character(if ("CRIT1" %in% names(advs)) advs$CRIT1 else NA)
advs$CRIT1FL <- as.character(advs$CRIT1FL)
advs$CRIT2 <- as.character(if ("CRIT2" %in% names(advs)) advs$CRIT2 else NA)
advs$CRIT2FL <- as.character(advs$CRIT2FL)
advs$SAFFL <- as.character(advs$SAFFL)
advs$TRTPN <- as.numeric(advs$TRTPN)
advs$ATPTN <- as.numeric(advs$ATPTN)
advs$ADY <- as.numeric(advs$ADY)
advs$AVAL <- as.numeric(if ("AVAL" %in% names(advs)) advs$AVAL else NA)

# WHERE PARAMCD="DIABP" and ATPTN=815 and ADY > 1 and SAFFL="Y" and (not missing(CRIT1FL) or not missing(CRIT2FL))
advs_f <- advs %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ADY > 1,
         SAFFL == "Y",
         (!is_missing_c(CRIT1FL) | !is_missing_c(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# ---- PROC SQL counts ----
# totn_low: where not missing crit1fl
totn_low <- advs_f %>%
  filter(!is_missing_c(CRIT1FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

n_low <- advs_f %>%
  filter(!is.na(CRIT1FL) & trimws(CRIT1FL) == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs_f %>%
  filter(!is_missing_c(CRIT2FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

n_high <- advs_f %>%
  filter(!is.na(CRIT2FL) & trimws(CRIT2FL) == "Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

# Combine N and n, calculate percent (SAS MERGE by TRTPN PARAM => full join)
all_n <- full_join(totn, te_n, by = c("TRTPN", "PARAM", "TEFL")) %>%
  # SAS sorts: totn by trtpn param ; te_n by trtpn param ; merge keeps order by keys
  arrange(TRTPN, PARAM) %>%
  mutate(PCT = N / TOTN * 100)

# ---- Fisher Exact p-value ----
# Helper: last.usubjid after sort by USUBJID FLG (missing first, SAS order blank < N < Y)
last_per_subject <- function(df, flg) {
  # df = advs_f
  # Determine per-subject last value mimicking SAS sort
  # SAS char order: missing/blank first, then 'N' < 'Y'
  tmp <- df %>% select(USUBJID, TRTPN, PARAM, ALLFLG = all_of(flg))
  tmp <- tmp %>% mutate(FLGNORM = ifelse(is_missing_c(ALLFLG), NA_character_, trimws(ALLFLG)))
  # per subject: if any Y -> Y, else if any N -> N, else NA (covers blank/missing)
  # Also need TRTPN/PARAM from last record: TRTPN constant per subject; take first non-missing
  subj_flg <- tmp %>%
    group_by(USUBJID) %>%
    summarise(
      FLGVAL = if (any(FLGNORM == "Y", na.rm = TRUE)) "Y"
               else if (any(FLGNORM == "N", na.rm = TRUE)) "N"
               else NA_character_,
      TRTPN = suppressWarnings(first(na.omit(TRTPN))),
      PARAM = first(PARAM),
      .groups = "drop"
    )
  # PROC FREQ excludes missing FLG by default
  subj_flg %>% filter(!is.na(FLGVAL))
}

fisher_for_flg <- function(df, flg, tefl) {
  subj <- last_per_subject(df, flg)
  # tables param*trtpn*flg : strata PARAM, contingency TRTPN x FLG
  # Only one PARAM value expected; compute per PARAM then keep (tefl single p per param)
  params <- unique(subj$PARAM)
  res <- lapply(params, function(p) {
    s <- subj %>% filter(PARAM == p)
    tab <- table(factor(s$TRTPN), factor(s$FLGVAL))
    # Ensure at least 2x2; fisher.test handles RxC
    ft <- suppressWarnings(fisher.test(tab))
    data.frame(PARAM = p, TEFL = tefl, NVALUE1 = as.numeric(ft$p.value), stringsAsFactors = FALSE)
  })
  bind_rows(res)
}

fisher_low <- fisher_for_flg(advs_f, "CRIT1FL", "Low")
fisher_high <- fisher_for_flg(advs_f, "CRIT2FL", "High")

all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)

# Sort all_n by tefl trtpn (SAS)
all_n <- all_n %>% arrange(TEFL, TRTPN)

# Merge Fisher p-value with N/n by tefl (many-to-one)
table_vals <- left_join(all_n, all_fisher, by = c("TEFL", "PARAM")) %>%
  mutate(
    PCTC = ifelse(is.na(PCT), "", paste0("(", sapply(PCT, fmt_pct_5_1), ")")),
    NC = ifelse(is.na(N), ".", sapply(N, fmt_best))
  )

# SAS puts missing N as "." and PCT as "(.)" - handle edge: if N is NA, PCTC should be "(.)" per SAS logic
# Our code above gives "" for missing PCT; adjust to SAS faithful when N missing but TOTN present:
table_vals$PCTC[is.na(table_vals$PCT)] <- "(.)"
table_vals$NC[is.na(table_vals$N)] <- "."

# proc sort by param descending tefl nvalue1
# Descending TEFL: Low before High
table_vals <- table_vals %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Only display first p-value per param/tefl/nvalue1 group
# by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=put(...)
table_vals <- table_vals %>%
  group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(
    NVALUE1C = ifelse(row_number() == 1, sapply(NVALUE1, fmt_pvalue, d = 3), "")
  ) %>%
  ungroup()

# Final order already param, desc tefl, nvalue1 ; within group by TRTPN ascending to match report
table_vals <- table_vals %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Reorder columns to report order
table_vals <- table_vals %>% select(PARAM, TEFL, TRTPN, TOTN, N, PCT, NVALUE1, NC, PCTC, NVALUE1C)

# ---- Write dataset CSV (standalone dataset counterpart) ----
write.csv(table_vals, file.path(out, "P056_table_vals.csv"), row.names = FALSE, na = "")

# CSV of table body (report columns)
report_body <- table_vals %>% select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C)
names(report_body) <- c("PARAM", "TEFL", "TRTPN", "TOTN", "NC", "PCTC", "NVALUE1C")
write.csv(report_body, file.path(out, "P056.csv"), row.names = FALSE, na = "")

# ---- Text rendering mimicking PROC REPORT (P056.lst) ----
# Titles
t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"

# Header two-line due to split="|"
# Columns: Vital Sign (unit) | Abnormality Direction | Treatment | N | n | (%) | P value*
# Build fixed-width text
hdr1 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s", "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
hdr2 <- sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s", "(unit)", "Direction", "", "", "", "", "")
sep <- paste0(rep("-", 35 + 1 + 12 + 1 + 12 + 1 + 6 + 1 + 6 + 1 + 8 + 1 + 8), collapse = "")

lines <- c(t1, t2, "", hdr1, hdr2, sep)

for (i in seq_len(nrow(report_body))) {
  r <- report_body[i, ]
  # PARAM may be long; truncate/pad to 35
  pstr <- substr(as.character(r$PARAM), 1, 35)
  lines <- c(lines, sprintf("%-35s %-12s %-12s %-6s %-6s %-8s %-8s",
    pstr,
    as.character(r$TEFL),
    as.character(r$TRTPN),
    as.character(r$TOTN),
    as.character(r$NC),
    as.character(r$PCTC),
    as.character(r$NVALUE1C)))
}
lines <- c(lines, sep, "",
  "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
  "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
  "  specified category.",
  "  *P values are from Fisher's Exact test.")

txt_path <- file.path(out, "P056.txt")
con <- file(txt_path, open = "wt")
writeLines(lines, con)
close(con)

# Also write .lst-named copy for benchmark compatibility
lst_copy <- file.path(out, "P056.lst.txt")
con2 <- file(lst_copy, open = "wt")
writeLines(lines, con2)
close(con2)

# CSV copy with .lst prefix variant (same body) for safety
write.csv(report_body, file.path(out, "P056_lst.csv"), row.names = FALSE, na = "")
```