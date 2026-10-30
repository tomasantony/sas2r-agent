```r
# P054_outliers_Shift_Table.R
# Reproduces the SAS program P054_outliers_Shift_Table.sas
# Requires the environment variable OUT_DIR to be set.

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# ---------- Helper functions ----------
round_half_up <- function(x, digits = 0) {
  # SAS round: half away from zero
  pos <- x >= 0
  factor <- 10^digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                ceiling(x * factor - 0.5) / factor)
  return(res)
}

# ---------- Paths ----------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------- Input ----------
source_xpt <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- read_xpt(source_xpt)

# ---------- Data preparation ----------
advs <- advs_raw %>%
  mutate(
    param = ifelse(is.na(param), "", param),
    atpt = ifelse(is.na(atpt), "", atpt),
    param = str_trim(paste(param, str_to_title(atpt), sep = " ")),
    paramn = atptn
  )

# Keep only required variables
invar <- c("USUBJID","TRTPN","TRTP","PARAM","PARAMCD","PARAMN",
           "ATPT","ATPTN","AVAL","AVISITN","ANL01FL","BNRIND",
           "ANRIND","SHIFT1","CHGCAT1","SAFFL")
inds <- advs %>%
  select(all_of(invar)) %>%
  filter(
    PARAMCD == "DIABP",
    ANL01FL == "Y",
    AVISITN == 99,
    SAFFL == "Y",
    BNRIND != "Missing",
    ANRIND != "Missing"
  )

# ---------- Treatment format ----------
trtvar   <- "TRTPN"
trtvartx <- substr(trtvar, 1, nchar(trtvar) - 1)  # "TRTP"

# Create treatment lookup (numeric -> label)
trtcomb <- inds %>%
  distinct(!!sym(trtvar), !!sym(trtvartx)) %>%
  rename(start = !!sym(trtvar), label = !!sym(trtvartx))

# ---------- Summary counts ----------
counts <- inds %>%
  group_by(PARAM, PARAMN, !!sym(trtvar), BNRIND, ANRIND) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(!is.na(PARAM) & !is.na(PARAMN) & !is.na(!!sym(trtvar)))

# ---------- Fill totals ----------
shiftord_map <- c(Low = 1, Normal = 2, High = 3, Total = 4)

fixtot <- counts %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND), "Total", BNRIND),
    ANRIND = ifelse(is.na(ANRIND), "Total", ANRIND),
    BNRINDN = shiftord_map[BNRIND],
    ANRINDN = shiftord_map[ANRIND]
  ) %>%
  # Separate denominator rows (both Total) from others
  mutate(is_denom = (BNRIND == "Total" & ANRIND == "Total")) %>%
  select(-is_denom) %>%
  # Split
  {
    denom   <- filter(., BNRIND == "Total" & ANRIND == "Total") %>%
      select(PARAM, PARAMN, !!sym(trtvar), n) %>%
      rename(denom = n)
    fixtot  <- filter(., !(BNRIND == "Total" & ANRIND == "Total")) %>%
      select(PARAM, PARAMN, !!sym(trtvar), ANRIND, BNRIND,
             ANRINDN, BNRINDN, n)
    list(fixtot = fixtot, denom = denom)
  }

fixtot <- fixtot$fixtot
denom   <- fixtot$denom

# ---------- Ensure all categories exist ----------
maxcat <- 4
allcat <- fixtot %>%
  distinct(PARAM, PARAMN, !!sym(trtvar))

full <- allcat %>%
  rowwise() %>%
  do({
    cur <- .
    expand_grid(
      PARAM = cur$PARAM,
      PARAMN = cur$PARAMN,
      !!sym(trtvar) := cur[[trtvar]],
      BNRINDN = 1:maxcat,
      ANRINDN = 1:maxcat,
      n = 0
    )
  }) %>%
  ungroup()

# Merge counts with full grid
allrecs <- full %>%
  left_join(fixtot, by = c("PARAM","PARAMN",trtvar="TRTPN",
                           "BNRINDN","ANRINDN")) %>%
  mutate(
    n = coalesce(n.y, n.x)
  ) %>%
  select(PARAM, PARAMN, !!sym(trtvar), BNRINDN, ANRINDN, n)

# Add denominators
alldenom <- allrecs %>%
  left_join(denom, by = c("PARAM","PARAMN",trtvar = "TRTPN")) %>%
  filter(!is.na(denom))

# Map numeric categories back to text
shifttx_map <- c("1" = "Low", "2" = "Normal", "3" = "High", "4" = "Total")
alldenom <- alldenom %>%
  mutate(
    BNRIND = shifttx_map[as.character(BNRINDN)],
    ANRIND = shifttx_map[as.character(ANRINDN)]
  )

# ---------- Column headings (post‑baseline categories) ----------
pbcat <- alldenom %>%
  distinct(ANRIND, ANRINDN) %>%
  arrange(ANRINDN) %>%
  pull(ANRIND)

# Ensure we have exactly maxcat headings
if (length(pbcat) < maxcat) pbcat <- c(pbcat, rep("", maxcat - length(pbcat)))

# ---------- Transpose and compute percentages ----------
trans <- alldenom %>%
  arrange(PARAM, PARAMN, !!sym(trtvar), BNRINDN, ANRINDN) %>%
  group_by(PARAM, PARAMN, !!sym(trtvar), BNRINDN, BNRIND, denom) %>%
  summarise(
    n_vec = list(setNames(n, ANRINDN)),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(
    # create n1‑n4
    n1 = n_vec[["1"]], n2 = n_vec[["2"]], n3 = n_vec[["3"]], n4 = n_vec[["4"]],
    # percentages
    pct1 = if (denom > 0) {
      val <- round_half_up(100 * n1 / denom, 1)
      paste0("(", sprintf("%5.1f", val), ")")
    } else "(  0.0)",
    pct2 = if (denom > 0) {
      val <- round_half_up(100 * n2 / denom, 1)
      paste0("(", sprintf("%5.1f", val), ")")
    } else "(  0.0)",
    pct3 = if (denom > 0) {
      val <- round_half_up(100 * n3 / denom, 1)
      paste0("(", sprintf("%5.1f", val), ")")
    } else "(  0.0)",
    pct4 = if (denom > 0) {
      val <- round_half_up(100 * n4 / denom, 1)
      paste0("(", sprintf("%5.1f", val), ")")
    } else "(  0.0)",
    # treatment text
    trttx = {
      label <- trtcomb$label[match(!!sym(trtvar), trtcomb$start)]
      if (is.na(label)) label <- ""
      paste0(label, " (N = ", denom, ")")
    }
  ) %>%
  ungroup() %>%
  select(PARAM, PARAMN, !!sym(trtvar), BNRINDN, BNRIND,
         n1, n2, n3, n4,
         pct1, pct2, pct3, pct4,
         denom, trttx) %>%
  arrange(PARAMN, PARAM, !!sym(trtvar), BNRINDN)

# ---------- Write dataset output ----------
write.csv(trans,
          file = file.path(out_dir, "trans.csv"),
          row.names = FALSE,
          na = "")

# ---------- Generate text report (PROC REPORT style) ----------
txt_file <- file.path(out_dir, "P054.lst")
con <- file(txt_file, open = "wt", encoding = "UTF-8")
on.exit(close(con), add = TRUE)

# Helper to print a line with fixed widths
print_line <- function(cols, widths, align = rep("left", length(cols))) {
  line <- mapply(function(txt, w, a) {
    txt <- if (is.na(txt)) "" else as.character(txt)
    if (a == "right") {
      sprintf(paste0("%", w, "s"), txt)
    } else if (a == "center") {
      sprintf(paste0("%", w, "s"), txt)  # simple left for now
    } else {
      sprintf(paste0("%-", w, "s"), txt)
    }
  }, cols, widths, align, SIMPLIFY = FALSE)
  cat(paste0(line, collapse = " "), "\n", file = con)
}

# Header
header_cols <- c("Treatment", "Baseline Result",
                pbcat[1], pbcat[2], pbcat[3], pbcat[4])
header_widths <- c(30, 15, 12, 12, 12, 12)
header_align  <- c("left", "left", rep("center", 4))
print_line(header_cols, header_widths, header_align)

# Sub‑header for n and % under each category
sub_cols <- c("", "",
              "n", "%", "n", "%", "n", "%", "n", "%")
sub_widths <- c(30, 15,
                5, 7, 5, 7, 5, 7, 5, 7)
sub_align  <- c(rep("left",2), rep(c("right","left"),4))
print_line(sub_cols, sub_widths, sub_align)

# Data rows
trans %>%
  arrange(PARAMN, PARAM, !!sym(trtvar), BNRINDN) %>%
  group_by(PARAMN, PARAM) %>%
  do({
    grp <- .
    # For each treatment within the param group
    grp %>%
      group_by(!!sym(trtvar), trttx) %>%
      do({
        trt_block <- .
        # Print a blank line before each new treatment block (except first)
        if (nrow(trt_block) > 0) {
          # treatment line already in trttx column of each row
        }
        # Print rows for each baseline category
        apply(trt_block, 1, function(row) {
          print_line(
            cols = c(row["trttx"], row["BNRIND"],
                     row["n1"], row["pct1"],
                     row["n2"], row["pct2"],
                     row["n3"], row["pct3"],
                     row["n4"], row["pct4"]),
            widths = c(30,15,5,7,5,7,5,7,5,7),
            align = c("left","left","right","left","right","left","right","left","right","left")
          )
        })
        NULL
      })
    NULL
  })

# Footer line (optional)
cat("\n", file = con)

close(con)

# ---------- End of script ----------
```