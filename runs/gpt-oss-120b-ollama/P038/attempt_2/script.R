# P038_target22.R
# Reimplementation of SAS program P038_target22.sas in R
# Produces:
#   - P038.csv          : the final analysis dataset (same as SAS "dataset" output)
#   - P038_table.csv    : CSV rendering of the listing/table body
#   - P038.txt          : plain‑text rendering of the listing/table
# All files are written to the directory given by the OUT_DIR environment variable.

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
  res[is.na(x)] <- NA
  res
}

# ----------------------------------------------------------------------
# Environment
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

# rawadam may be supplied as RAWADAM or rawadam
rawadam_dir <- Sys.getenv("RAWADAM")
if (rawadam_dir == "") rawadam_dir <- Sys.getenv("rawadam")
if (rawadam_dir == "") stop("rawadam environment variable not set")

# ----------------------------------------------------------------------
# 1. Read source data (advs.xpt)
advs_path <- file.path(rawadam_dir, "advs.xpt")
if (!file.exists(advs_path)) stop("advs.xpt not found at: ", advs_path)
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
# Macro‑like variables (as defined in the SAS program)
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
shifttx <- c("Low","Normal","High","Total")   # index 1:4

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
    n_vec = list(setNames(n, anrindn)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    n1 = n_vec[["1"]] %||% 0L,
    n2 = n_vec[["2"]] %||% 0L,
    n3 = n_vec[["3"]] %||% 0L,
    n4 = n_vec[["4"]] %||% 0L,
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
    trt_label = trt_lookup$label[match(!!sym(trtvar), trt_lookup$start)],
    trttx = str_c(trt_label, sprintf("(N = %s)", denom), sep = " ")
  ) %>%
  ungroup() %>%
  select(PARAM, PARAMN, !!sym(trtvar), trttx, bnrindn, BNRIND,
         n1, pct1, n2, pct2, n3, pct3, n4, pct4)

# ----------------------------------------------------------------------
# 12. Write outputs -------------------------------------------------------
# 12a. Analysis dataset (same as SAS "dataset" output)
write.csv(trans,
          file = file.path(out_dir, "P038.csv"),
          row.names = FALSE,
          na = "")

# 12b. CSV rendering of the table body
table_csv <- trans %>%
  select(trttx, BNRIND,
         n1, pct1, n2, pct2, n3, pct3, n4, pct4)
write.csv(table_csv,
          file = file.path(out_dir, "P038_table.csv"),
          row.names = FALSE,
          na = "")

# 12c. Plain‑text rendering of the table (P038.txt)
txt_path <- file.path(out_dir, "P038.txt")
con <- file(txt_path, open = "wt", encoding = "UTF-8")

# Header line (mirrors PROC REPORT column headings)
header <- sprintf("%-30s %-15s %6s %7s %6s %7s %6s %7s %6s %7s",
                  "Treatment", "Baseline Result",
                  paste0(pbcat[1], " n"), paste0(pbcat[1], " %"),
                  paste0(pbcat[2], " n"), paste0(pbcat[2], " %"),
                  paste0(pbcat[3], " n"), paste0(pbcat[3], " %"),
                  paste0(pbcat[4], " n"), paste0(pbcat[4], " %"))
writeLines(header, con)
writeLines(str_repeat("-", nchar(header)), con)

# Body rows: ordered by PARAMN, PARAM, trttx, bnrindn (as PROC REPORT does)
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
