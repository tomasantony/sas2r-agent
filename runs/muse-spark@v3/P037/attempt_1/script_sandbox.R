# P037_target21.sas -> R translation
# Vital Signs Treatment-Emergent Abnormal High or Low

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  # SAS rounds half away from zero
  res <- x
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi)) { res[i] <- NA_real_; next }
    f <- 10^digits
    s <- ifelse(xi < 0, -1, 1)
    # tiny epsilon to counter floating error, SAS-like
    res[i] <- s * floor(abs(xi) * f + 0.5 + 1e-9) / f
  }
  res
}

fmt_5_1_strip <- function(x) {
  # SAS put(pct, 5.1) then strip
  vapply(x, function(xi) {
    if (is.na(xi)) return("")
    r <- round_half_up(xi, 1)
    sprintf("%.1f", r)
  }, character(1))
}

fmt_pvalue5.3 <- function(x) {
  vapply(x, function(xi) {
    if (is.na(xi)) return("")
    # SAS PVALUE5.3 : threshold <.001 and >.999
    if (xi < 0.0005) return("<.001")
    if (xi > 0.9995) return(">.999")
    r <- round_half_up(xi, 3)
    # handle boundary after rounding
    if (r < 0.0005) return("<.001")
    if (r > 0.9995) return(">.999")
    sprintf("%.3f", r)
  }, character(1))
}

is_missing_c <- function(x) {
  is.na(x) | trimws(as.character(x)) == ""
}

# ---- read ADVS ----
p1 <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
p2 <- "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
advs_path <- if (file.exists(p1)) p1 else p2
advs_raw <- haven::read_xpt(advs_path)

# normalise names to upper for SAS-like handling
names(advs_raw) <- toupper(names(advs_raw))
# ensure needed cols exist
stopifnot(all(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL") %in% names(advs_raw)))

advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y",
         (!is_missing_c(CRIT1FL) | !is_missing_c(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)

# ---- PROC SQL counts ----
# totn_low : not missing crit1fl, tefl Low
totn_low <- advs %>%
  filter(!is_missing_c(CRIT1FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

n_low <- advs %>%
  filter(!is.na(CRIT1FL) & CRIT1FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "Low")

totn_high <- advs %>%
  filter(!is_missing_c(CRIT2FL)) %>%
  group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

n_high <- advs %>%
  filter(!is.na(CRIT2FL) & CRIT2FL == "Y") %>%
  group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(TEFL = "High")

totn <- bind_rows(totn_high, totn_low)
te_n <- bind_rows(n_high, n_low)

# sort by trtpn param (SAS)
totn <- totn %>% arrange(TRTPN, PARAM)
te_n <- te_n %>% arrange(TRTPN, PARAM)

# Combine N and n, calculate pct : merge by trtpn param (+tefl to keep direction correct)
all_n <- full_join(totn, te_n, by = c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N / TOTN * 100) %>%
  arrange(TEFL, TRTPN)

# ---- Fisher Exact p-value ----
# helper: last.usubjid after sort by usubjid & flg (missing first)
get_last <- function(df, flg) {
  df %>%
    mutate(.sk = ifelse(is_missing_c(.data[[flg]]), "", as.character(.data[[flg]]))) %>%
    arrange(USUBJID, .sk) %>%
    group_by(USUBJID) %>%
    slice_tail(n = 1) %>%
    ungroup() %>%
    select(-.sk)
}

fisher_p <- function(df_last, flg) {
  # PROC FREQ default excludes missing
  d <- df_last %>% filter(!is_missing_c(.data[[flg]]))
  # order rows TRTPN ascending numeric, cols flg alphabetical (N,Y)
  tr_levels <- sort(unique(d$TRTPN))
  fl_levels <- sort(unique(as.character(d[[flg]])))
  # build matrix TRTPN x FLG
  mat <- matrix(0, nrow = length(tr_levels), ncol = length(fl_levels))
  rownames(mat) <- as.character(tr_levels)
  colnames(mat) <- fl_levels
  for (i in seq_along(tr_levels)) {
    for (j in seq_along(fl_levels)) {
      mat[i, j] <- sum(d$TRTPN == tr_levels[i] & as.character(d[[flg]]) == fl_levels[j])
    }
  }
  # Fisher exact (two-sided Pr<=P, same as SAS XP2_FISH)
  ft <- fisher.test(mat)
  as.numeric(ft$p.value)
}

last_crit1fl <- get_last(advs, "CRIT1FL")
last_crit2fl <- get_last(advs, "CRIT2FL")

p_low <- fisher_p(last_crit1fl, "CRIT1FL")
p_high <- fisher_p(last_crit2fl, "CRIT2FL")

all_fisher <- tibble::tibble(
  TEFL = c("Low", "High"),
  NVALUE1 = c(p_low, p_high)
) %>% arrange(TEFL)

# sort all_n by tefl trtpn (SAS)
all_n <- all_n %>% arrange(TEFL, TRTPN)
all_fisher <- all_fisher %>% arrange(TEFL)

# Merge Fisher p-value with N and n (by tefl, one-to-many retain)
table_vals0 <- left_join(all_n, all_fisher, by = "TEFL") %>%
  mutate(
    PCTC = paste0("(", fmt_5_1_strip(PCT), ")"),
    NC = ifelse(is.na(N), "", as.character(N))
  )

# sort by param descending tefl nvalue1
# SAS: by param descending tefl nvalue1
table_vals0 <- table_vals0 %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Only want to display first p-value in final table
# by param descending tefl nvalue1; if first.nvalue1 then nvalue1c=put(nvalue1,pvalue5.3)
table_vals <- table_vals0 %>%
  group_by(PARAM, TEFL, NVALUE1) %>%
  mutate(
    .rn = row_number(),
    NVALUE1C = ifelse(.rn == 1, fmt_pvalue5.3(NVALUE1), "")
  ) %>%
  ungroup() %>%
  select(-.rn) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# ---- outputs ----
# CSV of table body (displayed columns)
out_csv <- table_vals %>%
  transmute(
    PARAM = PARAM,
    TEFL = TEFL,
    TRTPN = TRTPN,
    TOTN = TOTN,
    NC = NC,
    PCTC = PCTC,
    NVALUE1C = NVALUE1C,
    N = N,
    PCT = round_half_up(PCT, 1),
    NVALUE1 = NVALUE1
  )

write.csv(out_csv, file.path(out, "P037.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC REPORT
# Titles
t1 <- "Vital Signs"
t2 <- "Treatment-Emergent Abnormal High or Low at Any Time"

# Header two-line due to split="|"
h1 <- c("Vital Sign", "(unit)")
h2 <- c("Abnormality", "Direction")

# Build fixed-width-ish text
# Determine PARAM display (single value)
param_val <- unique(table_vals$PARAM)

lines <- c()
lines <- c(lines, t1)
lines <- c(lines, t2)
lines <- c(lines, "")
# header
# Use simple aligned columns
col_names_l1 <- sprintf("%-35s %-12s %-10s %-6s %-6s %-8s %-8s",
                        "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
col_names_l2 <- sprintf("%-35s %-12s %-10s %-6s %-6s %-8s %-8s",
                        "(unit)", "Direction", "", "", "", "", "")
lines <- c(lines, col_names_l1)
lines <- c(lines, col_names_l2)
lines <- c(lines, paste0(strrep("_", 100)))

for (i in seq_len(nrow(table_vals))) {
  r <- table_vals[i, ]
  # param only shown for group? PROC REPORT group shows repeated? Show each row full for clarity
  line <- sprintf("%-35s %-12s %-10s %-6s %-6s %-8s %-8s",
                  as.character(r$PARAM),
                  as.character(r$TEFL),
                  as.character(r$TRTPN),
                  as.character(r$TOTN),
                  as.character(r$NC),
                  as.character(r$PCTC),
                  as.character(r$NVALUE1C))
  lines <- c(lines, line)
}
lines <- c(lines, "")
lines <- c(lines, "  Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)")
lines <- c(lines, "  baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the")
lines <- c(lines, "  specified category.")
lines <- c(lines, "  *P values are from Fisher's Exact test.")

writeLines(lines, file.path(out, "P037.txt"))
# also write .lst copy for benchmark that expects P037.lst
writeLines(lines, file.path(out, "P037.lst"))
