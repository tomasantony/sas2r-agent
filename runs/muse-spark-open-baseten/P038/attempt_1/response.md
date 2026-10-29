```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# --- helpers to mimic SAS semantics ---
round_half_up <- function(x, digits = 0) {
  # SAS ROUND half away from zero
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  # handle NA
  res <- sign_x * floor(abs_x * mult + 0.5) / mult
  res[is.na(x)] <- NA_real_
  res
}

sas_put_5_1 <- function(x) {
  # SAS put with format 5.1 : width 5, 1 decimal, right aligned
  sprintf("%5.1f", x)
}

prop_case <- function(x) {
  # SAS PROPCASE: first letter of each word upper, rest lower
  # word boundaries are non-alphanumeric; approximate with str_to_title
  ifelse(is.na(x), NA_character_, str_to_title(tolower(x)))
}

catx <- function(delim, ...) {
  args <- list(...)
  # remove NA/empty, trim
  args <- lapply(args, function(a) {
    if (is.null(a)) return(NULL)
    a <- as.character(a)
    a <- trimws(a)
    a[a == ""] <- NA_character_
    a
  })
  # vectorised catx for vectors of same length
  # use purrr::pmap_chr
  n <- max(sapply(args, length))
  args <- lapply(args, function(a) rep_len(a, n))
  do.call(paste, c(args, sep = delim)) |>
    trimws() |>
    gsub(paste0("\\", delim, "\\", delim, "+"), delim, x = _, perl = TRUE) |>
    trimws()
  # simpler: paste with delim, but need to handle NA
  # We'll implement row-wise
}

# vectorised catx for two vectors
catx_vec <- function(delim, a, b) {
  a <- trimws(as.character(a))
  b <- trimws(as.character(b))
  a[a == ""] <- NA_character_
  b[b == ""] <- NA_character_
  res <- ifelse(is.na(a) & is.na(b), NA_character_,
         ifelse(is.na(a), b,
         ifelse(is.na(b), a,
                paste(a, b, sep = delim))))
  res
}

# --- read input ---
advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
if (!file.exists(advs_path)) {
  # fallback to derived path if needed
  advs_path2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
  if (file.exists(advs_path2)) advs_path <- advs_path2
}
advs_raw <- haven::read_xpt(advs_path)

# Convert to data.frame with original case names (haven keeps uppercase)
# Ensure character columns are character
advs <- advs_raw

# param = catx(' ', param, propcase(atpt)); paramn = atptn;
# Note: SAS variables are PARAM, ATPT, ATPTN, PARAMN
advs <- advs %>%
  mutate(
    PARAM = catx_vec(" ", PARAM, prop_case(ATPT)),
    PARAMN = ATPTN
  )

# Keep only needed vars and filter
# invar = USUBJID TRTPN TRTP PARAM PARAMCD PARAMN ATPT ATPTN AVAL AVISITN ANL01FL BNRIND ANRIND SHIFT1 CHGCAT1 SAFFL
# inwhere = WHERE PARAMCD='DIABP' and ANL01FL='Y' and AVISITN=99 and SAFFL='Y' and BNRIND ne 'Missing' and ANRIND ne 'Missing'

inds <- advs %>%
  select(USUBJID, TRTPN, TRTP, PARAM, PARAMCD, PARAMN, ATPT, ATPTN, AVAL, AVISITN, ANL01FL, BNRIND, ANRIND, SHIFT1, CHGCAT1, SAFFL) %>%
  filter(PARAMCD == "DIABP",
         ANL01FL == "Y",
         AVISITN == 99,
         SAFFL == "Y",
         BNRIND != "Missing", ANRIND != "Missing",
         !is.na(BNRIND), !is.na(ANRIND))

# Build treatment group display format
# trtcomb distinct TRTPN as start, TRTP as label
trtcomb <- inds %>%
  distinct(TRTPN, TRTP) %>%
  arrange(TRTPN)

# helper to format TRTPN via TRTFT
trt_format <- setNames(trtcomb$TRTP, as.character(trtcomb$TRTPN))
fmt_trtft <- function(x) {
  # x numeric TRTPN
  lbl <- trt_format[as.character(x)]
  unname(lbl)
}

# Count patients by baseline/post-baseline shift category
# PROC SUMMARY with CLASS param paramn trtpn bnrind anrind ; n(aval)=n
# Need cube counts for 4 grouping sets where PARAM,PARAMN,TRTPN always present

count_by <- function(df, groups) {
  df %>%
    filter(!is.na(AVAL)) %>%
    group_by(across(all_of(groups))) %>%
    summarise(n = n(), .groups = "drop")
}

groups_list <- list(
  c("PARAM","PARAMN","TRTPN","BNRIND","ANRIND"),
  c("PARAM","PARAMN","TRTPN","BNRIND"),
  c("PARAM","PARAMN","TRTPN","ANRIND"),
  c("PARAM","PARAMN","TRTPN")
)

counts_list <- lapply(groups_list, function(g) count_by(inds, g))
counts <- bind_rows(counts_list)

# filter where param, paramn, trtpn not missing (SAS where clause)
counts <- counts %>%
  filter(!is.na(PARAM), !is.na(PARAMN), !is.na(TRTPN))

# sort by param paramn trtpn (SAS)
counts <- counts %>%
  arrange(PARAM, PARAMN, TRTPN)

# Fill in total indicator, add numeric shift categories, and split off denominator records
shiftord_map <- c("Low"=1L, "Normal"=2L, "High"=3L, "Total"=4L)
shifttx_map <- c("1"="Low","2"="Normal","3"="High","4"="Total")

counts2 <- counts %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND), "Total", BNRIND),
    ANRIND = ifelse(is.na(ANRIND), "Total", ANRIND),
    BNRINDN = as.integer(shiftord_map[BNRIND]),
    ANRINDN = as.integer(shiftord_map[ANRIND])
  )

denom <- counts2 %>%
  filter(BNRIND == "Total" & ANRIND == "Total") %>%
  select(PARAM, PARAMN, TRTPN, denom = n)

fixtot <- counts2 %>%
  filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  select(PARAM, PARAMN, TRTPN, ANRIND, BNRIND, ANRINDN, BNRINDN, n)

fixtot <- fixtot %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# Create full dataset to account for 0 cells
maxcat <- 4L
allcat <- fixtot %>%
  distinct(PARAM, PARAMN, TRTPN)

full <- allcat %>%
  crossing(BNRINDN = 1:maxcat, ANRINDN = 1:maxcat) %>%
  mutate(n = 0L)

# Merge back with counts to get complete dataset
allrecs <- full %>%
  left_join(fixtot %>% select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, n) %>% rename(n_fixtot = n),
            by = c("PARAM","PARAMN","TRTPN","BNRINDN","ANRINDN")) %>%
  mutate(n = ifelse(!is.na(n_fixtot), n_fixtot, n)) %>%
  select(-n_fixtot) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN)

# Add denominators and repopulate shift categories
alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM","PARAMN","TRTPN")) %>%
  filter(!is.na(denom)) %>%
  mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  )

# Get category names for column headings (ordered by anrindn)
pbcat <- alldenom %>%
  distinct(ANRIND, ANRINDN) %>%
  arrange(ANRINDN) %>%
  pull(ANRIND)
# pbcat1..pbcat4
pbcat1 <- pbcat[1]; pbcat2 <- pbcat[2]; pbcat3 <- pbcat[3]; pbcat4 <- pbcat[4]

# Transpose to 1 record per baseline category and compute percentage
# For each PARAM,PARAMN,TRTPN,BNRINDN, compute n1..n4 and pct1..pct4

alldenom2 <- alldenom %>%
  mutate(
    pct_raw = ifelse(denom > 0, 100 * n / denom, 0),
    pct_rounded = round_half_up(pct_raw, 1),
    pct_fmt = paste0("(", sas_put_5_1(pct_rounded), ")")
  )

# pivot wider for n and pct
trans_n <- alldenom2 %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, BNRIND, denom, ANRINDN, n) %>%
  pivot_wider(names_from = ANRINDN, values_from = n, names_prefix = "n", values_fill = 0)

trans_pct <- alldenom2 %>%
  select(PARAM, PARAMN, TRTPN, BNRINDN, ANRINDN, pct_fmt) %>%
  pivot_wider(names_from = ANRINDN, values_from = pct_fmt, names_prefix = "pct")

trans <- trans_n %>%
  left_join(trans_pct, by = c("PARAM","PARAMN","TRTPN","BNRINDN")) %>%
  # ensure all pct columns exist
  mutate(
    n1 = coalesce(n1, 0L), n2 = coalesce(n2, 0L), n3 = coalesce(n3, 0L), n4 = coalesce(n4, 0L),
    pct1 = ifelse(is.na(pct1), "(  0.0)", pct1),
    pct2 = ifelse(is.na(pct2), "(  0.0)", pct2),
    pct3 = ifelse(is.na(pct3), "(  0.0)", pct3),
    pct4 = ifelse(is.na(pct4), "(  0.0)", pct4)
  ) %>%
  # trttx = catx(' ', put(trtpn, trtft.), ('(N = ' || compress(put(denom,12.)) || ')'))
  mutate(
    TRTP_label = fmt_trtft(TRTPN),
    denom_fmt = trimws(format(denom, width = 12)),
    # compress removes spaces
    denom_fmt = gsub(" ", "", denom_fmt, fixed = TRUE),
    trttx = paste0(TRTP_label, " (N = ", denom_fmt, ")")
  ) %>%
  arrange(PARAM, PARAMN, TRTPN, BNRINDN) %>%
  select(PARAM, PARAMN, TRTPN, trttx, BNRINDN, BNRIND, n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom)

# --- Generate output table ---
# Prepare CSV of table body (one row per baseline category per treatment)
out_csv <- trans %>%
  select(PARAM, PARAMN, TRTPN, trttx, BNRIND, n1, pct1, n2, pct2, n3, pct3, n4, pct4, denom)

# Write CSVs
write.csv(out_csv, file.path(out, "P038.csv"), row.names = FALSE, na = "")
write.csv(out_csv, file.path(out, "P038.lst.csv"), row.names = FALSE, na = "")

# Also write dataset CSV as required (same)
# For dataset output, also write trans as dataset
write.csv(trans, file.path(out, "P038_dataset.csv"), row.names = FALSE, na = "")

# Build text rendering mimicking PROC REPORT
# Header
txt_lines <- c()
txt_lines <- c(txt_lines, "Shift Table of Diastolic Blood Pressure - Baseline vs Post-Baseline")
txt_lines <- c(txt_lines, paste0("Parameter: ", unique(trans$PARAM)[1]))
txt_lines <- c(txt_lines, "")

# For each BY group (paramn param) - in this data likely one group
by_groups <- trans %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN)

for (i in seq_len(nrow(by_groups))) {
  cur_paramn <- by_groups$PARAMN[i]
  cur_param <- by_groups$PARAM[i]
  sub <- trans %>% filter(PARAMN == cur_paramn, PARAM == cur_param) %>% arrange(TRTPN, BNRINDN)

  txt_lines <- c(txt_lines, paste0("PARAMN=", cur_paramn, " PARAM=", cur_param))
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))
  # Column headers
  # Treatment | Baseline Result | Post-Baseline Result (Low, Normal, High, Total) each with n and %
  hdr1 <- sprintf("%-30s %-15s %12s %12s %12s %12s",
                  "Treatment", "Baseline", "Post-Baseline Result", "", "", "")
  hdr2 <- sprintf("%-30s %-15s %12s %12s %12s %12s",
                  "", "Result", pbcat1, pbcat2, pbcat3, pbcat4)
  hdr3 <- sprintf("%-30s %-15s %6s %6s %6s %6s %6s %6s %6s %6s",
                  "", "", "n", "%", "n", "%", "n", "%", "n", "%")
  txt_lines <- c(txt_lines, hdr1, hdr2, hdr3)
  txt_lines <- c(txt_lines, paste0(strrep("-", 120)))

  # Rows grouped by treatment
  trt_vals <- unique(sub$TRTPN)
  for (tv in trt_vals) {
    sub_trt <- sub %>% filter(TRTPN == tv) %>% arrange(BNRINDN)
    # trttx is same for all rows of this trt
    trt_label <- unique(sub_trt$trttx)
    txt_lines <- c(txt_lines, paste0(trt_label))
    for (r in seq_len(nrow(sub_trt))) {
      row <- sub_trt[r, ]
      # Format n as right aligned width 3? SAS n right
      # pct already includes parentheses with width 5.1
      line <- sprintf("%-30s %-15s %6d %-7s %6d %-7s %6d %-7s %6d %-7s",
                      "", row$BNRIND,
                      row$n1, row$pct1,
                      row$n2, row$pct2,
                      row$n3, row$pct3,
                      row$n4, row$pct4)
      txt_lines <- c(txt_lines, line)
    }
    txt_lines <- c(txt_lines, "") # skip after trttx
  }
  txt_lines <- c(txt_lines, "")
}

# Add footer with counts verification
txt_lines <- c(txt_lines, paste0("Note: Percentages are 100*n/denom rounded to 1 decimal (SAS ROUND)."))
txt_lines <- c(txt_lines, paste0("Denominators are total patients per treatment with non-missing baseline and post-baseline."))

# Write text files
writeLines(txt_lines, file.path(out, "P038.txt"))
writeLines(txt_lines, file.path(out, "P038.lst.txt"))
# Also write .lst without extension for compatibility
writeLines(txt_lines, file.path(out, "P038.lst"))

# Also write plotted data CSV for figure placeholder (no figure in this program, but create empty)
# Ensure at least one PNG if expected? This program has no figure, so skip.

cat("P038 translation complete. Rows:", nrow(trans), " Treatments:", length(unique(trans$TRTPN)), "\n")
```