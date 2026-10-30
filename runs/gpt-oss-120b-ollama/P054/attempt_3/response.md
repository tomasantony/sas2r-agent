```r
# P054_outliers_Shift_Table.R
# Reproduces SAS program P054_outliers_Shift_Table.sas
# Requires environment variable OUT_DIR to be set.

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---------- Helper: SAS style round (half away from zero) ----------
round_half_up <- function(x, digits = 0) {
  factor <- 10 ^ digits
  pos <- x >= 0
  ifelse(pos,
         floor(x * factor + 0.5) / factor,
         ceiling(x * factor - 0.5) / factor)
}

# ---------- Paths ----------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------- Input ----------
source_xpt <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- read_xpt(source_xpt)

# Work with lower‑case column names for simplicity
advs <- advs_raw %>% rename_all(tolower)

# ---------- Data preparation ----------
advs <- advs %>%
  mutate(
    param = ifelse(is.na(param), "", param),
    atpt  = ifelse(is.na(atpt),  "", atpt),
    param = str_trim(paste(param, str_to_title(atpt), sep = " ")),
    paramn = atptn
  )

# Variables to keep (lower case)
invar <- c("usubjid","trtpn","trtp","param","paramcd","paramn",
           "atpt","atptn","aval","avisitn","anl01fl","bnrind",
           "anrind","shift1","chgcat1","saffl")

inds <- advs %>%
  select(all_of(invar)) %>%
  filter(
    paramcd == "DIABP",
    anl01fl == "Y",
    avisitn == 99,
    saffl == "Y",
    bnrind != "Missing",
    anrind != "Missing"
  )

# ---------- Treatment format ----------
trtvar   <- "trtpn"
trtvartx <- substr(trtvar, 1, nchar(trtvar) - 1)   # "trtp"

# Create treatment lookup (numeric code -> label)
trtcomb <- inds %>%
  distinct(!!sym(trtvar), !!sym(trtvartx)) %>%
  rename(start = !!sym(trtvar), label = !!sym(trtvartx))

# ---------- Summary counts ----------
counts <- inds %>%
  group_by(param, paramn, !!sym(trtvar), bnrind, anrind) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(!is.na(param) & !is.na(paramn) & !is.na(!!sym(trtvar)))

# ---------- Fill totals ----------
shiftord_map <- c(Low = 1, Normal = 2, High = 3, Total = 4)

tmp_split <- counts %>%
  mutate(
    bnrind = ifelse(is.na(bnrind), "Total", bnrind),
    anrind = ifelse(is.na(anrind), "Total", anrind),
    bnrindn = shiftord_map[bnrind],
    anrindn = shiftord_map[anrind]
  ) %>%
  mutate(is_denom = (bnrind == "Total" & anrind == "Total")) %>%
  {
    denom   <- filter(., is_denom) %>%
      select(param, paramn, !!sym(trtvar), n) %>%
      rename(denom = n)
    fixtot  <- filter(., !is_denom) %>%
      select(param, paramn, !!sym(trtvar), bnrind, anrind,
             bnrindn, anrindn, n)
    list(fixtot = fixtot, denom = denom)
  }

fixtot <- tmp_split$fixtot
denom   <- tmp_split$denom

# ---------- Ensure all category combinations exist ----------
maxcat <- 4
allcat <- fixtot %>%
  distinct(param, paramn, !!sym(trtvar))

full <- allcat %>%
  rowwise() %>%
  do({
    cur <- .
    expand_grid(
      param   = cur$param,
      paramn  = cur$paramn,
      !!sym(trtvar) := cur[[trtvar]],
      bnrindn = 1:maxcat,
      anrindn = 1:maxcat,
      n = 0
    )
  }) %>%
  ungroup()

# Merge counts with full grid (fill missing cells with 0)
allrecs <- full %>%
  left_join(fixtot,
            by = c("param","paramn","trtpn","bnrindn","anrindn")) %>%
  mutate(
    n = coalesce(n.y, n.x)
  ) %>%
  select(param, paramn, !!sym(trtvar), bnrindn, anrindn, n)

# Add denominators
alldenom <- allrecs %>%
  left_join(denom,
            by = c("param","paramn","trtpn")) %>%
  filter(!is.na(denom))

# Map numeric categories back to text
shifttx_map <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
alldenom <- alldenom %>%
  mutate(
    bnrind = shifttx_map[as.character(bnrindn)],
    anrind = shifttx_map[as.character(anrindn)]
  )

# ---------- Column headings (post‑baseline categories) ----------
pbcat <- alldenom %>%
  distinct(anrind, anrindn) %>%
  arrange(anrindn) %>%
  pull(anrind)

if (length(pbcat) < maxcat) pbcat <- c(pbcat, rep("", maxcat - length(pbcat)))

# ---------- Transpose and compute percentages ----------
trans <- alldenom %>%
  arrange(param, paramn, !!sym(trtvar), bnrindn, anrindn) %>%
  group_by(param, paramn, !!sym(trtvar), bnrindn, bnrind, denom) %>%
  summarise(
    n_vec = list(setNames(n, anrindn)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    n1 = n_vec[["1"]], n2 = n_vec[["2"]], n3 = n_vec[["3"]], n4 = n_vec[["4"]],
    pct1 = if (denom > 0) {
      v <- round_half_up(100 * n1 / denom, 1)
      paste0("(", sprintf("%5.1f", v), ")")
    } else "(  0.0)",
    pct2 = if (denom > 0) {
      v <- round_half_up(100 * n2 / denom, 1)
      paste0("(", sprintf("%5.1f", v), ")")
    } else "(  0.0)",
    pct3 = if (denom > 0) {
      v <- round_half_up(100 * n3 / denom, 1)
      paste0("(", sprintf("%5.1f", v), ")")
    } else "(  0.0)",
    pct4 = if (denom > 0) {
      v <- round_half_up(100 * n4 / denom, 1)
      paste0("(", sprintf("%5.1f", v), ")")
    } else "(  0.0)",
    trttx = {
      lbl <- trtcomb$label[match(!!sym(trtvar), trtcomb$start)]
      if (is.na(lbl)) lbl <- ""
      paste0(lbl, " (N = ", denom, ")")
    }
  ) %>%
  ungroup() %>%
  select(param, paramn, !!sym(trtvar), bnrindn, bnrind,
         n1, n2, n3, n4,
         pct1, pct2, pct3, pct4,
         denom, trttx) %>%
  arrange(paramn, param, !!sym(trtvar), bnrindn)

# ---------- Write dataset output ----------
write.csv(trans,
          file = file.path(out_dir, "trans.csv"),
          row.names = FALSE,
          na = "")

# ---------- Write listing (text) ----------
lst_path <- file.path(out_dir, "P054.lst")
con <- file(lst_path, open = "wt", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

# Header line
header <- c("Treatment", "Baseline Result",
            pbcat[1], pbcat[2], pbcat[3], pbcat[4])
cat(paste(header, collapse = "  "), "\n", file = con)

# Sub‑header for n and % under each category
subhdr <- c("", "",
            "n", "%", "n", "%", "n", "%", "n", "%")
cat(paste(subhdr, collapse = "  "), "\n", file = con)

# Data rows
trans %>%
  arrange(paramn, param, !!sym(trtvar), bnrindn) %>%
  select(trttx, bnrind,
         n1, pct1, n2, pct2, n3, pct3, n4, pct4) %>%
  mutate(across(everything(), as.character)) %>%
  pwalk(function(...) {
    fields <- list(...)
    cat(paste(fields, collapse = "  "), "\n", file = con)
  })

close(con)

# ---------- Write CSV version of the listing ----------
write.csv(trans %>%
            select(trttx, bnrind,
                   n1, pct1, n2, pct2, n3, pct3, n4, pct4),
          file = file.path(out_dir, "P054.csv"),
          row.names = FALSE,
          na = "")

# ---------- End of script ----------
```