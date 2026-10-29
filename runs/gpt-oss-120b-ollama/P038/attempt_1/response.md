```r
# P038_target22.R
# Reimplementation of SAS program P038_target22.sas in R
# Generates the table listing (P038.lst) and dataset (P038.csv) in OUT_DIR

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ----------------------------------------------------------------------
# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  factor <- 10 ^ digits
  pos <- x >= 0
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                ceiling(x * factor - 0.5) / factor)
  # Preserve NA
  res[is.na(x)] <- NA
  res
}

# ----------------------------------------------------------------------
# Environment
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# ----------------------------------------------------------------------
# 1. Read source data (advs.xpt)
advs_path <- file.path(Sys.getenv("rawadam"), "advs.xpt")
advs_raw <- read_xpt(advs_path)

# 2. Create working advs dataset
advs <- advs_raw %>%
  mutate(
    param = if_else(is.na(atpt),
                    param,
                    str_c(param, str_to_title(atpt), sep = " ")),
    paramn = atptn
  )

# ----------------------------------------------------------------------
# Macro variables (hard‑coded as in the SAS program)
invar  <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN",
            "ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND",
            "ANRIND","SHIFT1","CHGCAT1","SAFFL")
inwhere <- quote(PARAMCD == "DIABP" &
                 ANL01FL == "Y" &
                 AVISITN == 99 &
                 SAFFL == "Y" &
                 BNRIND != "Missing" &
                 ANRIND != "Missing")
trtvar <- "TRTPN"
maxcat <- 4

# ----------------------------------------------------------------------
# 3. Subset and filter
inds <- advs %>%
  select(all_of(invar)) %>%
  filter(eval(inwhere))

# ----------------------------------------------------------------------
# 4. Build treatment format lookup (TRTFT)
# In SAS: start = trtpn, label = trtp
trt_lookup <- inds %>%
  distinct(!!sym(trtvar), TRTP) %>%
  rename(start = !!sym(trtvar), label = TRTP)

# ----------------------------------------------------------------------
# 5. Count patients by baseline/post‑baseline shift category
counts <- inds %>%
  group_by(PARAM, PARAMN, !!sym(trtvar), BNRIND, ANRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(!is.na(PARAM) & !is.na(PARAMN) & !is.na(!!sym(trtvar)))

# ----------------------------------------------------------------------
# 6. Fill in total indicator and numeric shift categories
shiftord <- c("Low" = 1, "Normal" = 2, "High" = 3, "Total" = 4)

fixtot <- counts %>%
  mutate(
    BNRIND = if_else(is.na(BNRIND), "Total", BNRIND),
    ANRIND = if_else(is.na(ANRIND), "Total", ANRIND),
    bnrindn = shiftord[BNRIND],
    anrindn = shiftord[ANRIND]
  ) %>%
  # Separate denominator rows (both Total) from regular rows
  mutate(is_denom = (BNRIND == "Total" & ANRIND == "Total")) %>%
  select(-is_denom)

denom <- fixtot %>%
  filter(BNRIND == "Total", ANRIND == "Total") %>%
  select(PARAM, PARAMN, !!sym(trtvar), n) %>%
  rename(denom = n)

fixtot <- fixtot %>%
  filter(!(BNRIND == "Total" & ANRIND == "Total")) %>%
  select(PARAM, PARAMN, !!sym(trtvar), ANRIND, BNRIND,
         anrindn, bnrindn, n)

# ----------------------------------------------------------------------
# 7. Create all possible category combinations (including zero cells)
allcat <- fixtot %>%
  distinct(PARAM, PARAMN, !!sym(trtvar))

full <- allcat %>%
  crossing(
    bnrindn = 1:maxcat,
    anrindn = 1:maxcat
  ) %>%
  mutate(n = 0L)

# ----------------------------------------------------------------------
# 8. Merge counts back to obtain complete dataset
allrecs <- full %>%
  left_join(fixtot,
            by = c("PARAM","PARAMN",trtvar,"bnrindn","anrindn")) %>%
  mutate(n = coalesce(n.y, n.x)) %>%
  select(-n.x, -n.y)

# ----------------------------------------------------------------------
# 9. Add denominators and repopulate shift category text
shifttx <- c("Low","Normal","High","Total")  # index matches 1:4

alldenom <- allrecs %>%
  left_join(denom,
            by = c("PARAM","PARAMN",trtvar)) %>%
  filter(!is.na(denom)) %>%
  mutate(
    BNRIND = shifttx[bnrindn],
    ANRIND = shifttx[anrindn]
  )

# ----------------------------------------------------------------------
# 10. Determine ordered post‑baseline category names (pbcat1‑pbcat4)
pbcat <- alldenom %>%
  distinct(ANRIND, anrindn) %>%
  arrange(anrindn) %>%
  pull(ANRIND)

# ----------------------------------------------------------------------
# 11. Transpose to one record per baseline category and compute percentages
trans <- alldenom %>%
  arrange(PARAM, PARAMN, !!sym(trtvar), bnrindn, anrindn) %>%
  group_by(PARAM, PARAMN, !!sym(trtvar), bnrindn, BNRIND, denom) %>%
  summarise(
    # collect counts for each anrindn
    n_vec = list(setNames(n, anrindn)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # Build n1‑n4
    n1 = n_vec[["1"]] %||% 0L,
    n2 = n_vec[["2"]] %||% 0L,
    n3 = n_vec[["3"]] %||% 0L,
    n4 = n_vec[["4"]] %||% 0L,
    # Percentages with SAS rounding (0.1 unit)
    pct_fmt = function(cnt) {
      if (denom > 0) {
        val <- round_half_up(100 * cnt / denom, 1)
        sprintf("(%5.1f)", val)
      } else {
        "(  0.0)"
      }
    },
    pct1 = pct_fmt(n1),
    pct2 = pct_fmt(n2),
    pct3 = pct_fmt(n3),
    pct4 = pct_fmt(n4),
    # Treatment label (TRTFT) + denominator
    trt_label = trt_lookup$label[match(!!sym(trtvar), trt_lookup$start)],
    trttx = str_c(trt_label, sprintf("(N = %s)", denom), sep = " ")
  ) %>%
  ungroup() %>%
  select(PARAM, PARAMN, !!sym(trtvar), trttx, bnrindn, BNRIND,
         n1, pct1, n2, pct2, n3, pct3, n4, pct4)

# ----------------------------------------------------------------------
# 12. Write outputs -------------------------------------------------------
# 12a. CSV dataset (named after the program)
write.csv(trans,
          file = file.path(out_dir, "P038.csv"),
          row.names = FALSE,
          na = "")

# 12b. Text rendering of the table (P038.lst)
txt_file <- file.path(out_dir, "P038.lst")
con <- file(txt_file, open = "wt", encoding = "UTF-8")

# Header line
header <- sprintf("%-30s %-15s %6s %7s %6s %7s %6s %7s %6s %7s",
                  "Treatment", "Baseline Result",
                  paste0(pbcat[1], " n"), paste0(pbcat[1], " %"),
                  paste0(pbcat[2], " n"), paste0(pbcat[2], " %"),
                  paste0(pbcat[3], " n"), paste0(pbcat[3], " %"),
                  paste0(pbcat[4], " n"), paste0(pbcat[4], " %"))
writeLines(header, con)

# Separator
writeLines(str_repeat("-", nchar(header)), con)

# Body rows (ordered as in PROC REPORT: by paramn param, then trttx, then bnrindn)
trans %>%
  arrange(PARAMN, PARAM, trttx, bnrindn) %>%
  rowwise() %>%
  mutate(
    line = sprintf("%-30s %-15s %6d %7s %6d %7s %6d %7s %6d %7s",
                   trttx, BNRIND,
                   n1, pct1,
                   n2, pct2,
                   n3, pct3,
                   n4, pct4)
  ) %>%
  pull(line) %>%
  writeLines(con)

close(con)

# ----------------------------------------------------------------------
# End of script
```