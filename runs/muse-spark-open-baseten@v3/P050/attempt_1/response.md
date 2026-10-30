```r
# P050_disposition_b2.sas -> R translation
# Output: dataset disp -> OUT_DIR/disp.csv

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)
library(tibble)

# --- helpers: SAS semantics ---
round_sas <- function(x, digits = 0) {
  # round half away from zero (SAS)
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_cnt_3 <- function(x) {
  # PUT(cnt,3.)  width 3, right aligned
  sprintf("%3d", as.integer(x))
}
fmt_per_5_1 <- function(x) {
  # PUT(per,5.1) width 5, 1 decimal, SAS rounding
  sprintf("%5.1f", round_sas(x, 1))
}

# --- locate adsl.xpt ---
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt",
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adsl.xpt"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adsl.xpt"),
  Sys.getenv("rawadam"),
  file.path(Sys.getenv("rawadam"), "adsl.xpt"),
  "/tmp/data/raw/cdisc-pilot/adam/adsl.xpt"
)
# also try OUT_DIR relative search
candidates <- unique(candidates[candidates != ""])
adsl_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adsl_path <- p; break }
  # haven can handle forward slashes on Windows; also try with backslashes normalized
  if (!is.na(p) && p != "" && file.exists(normalizePath(p, mustWork = FALSE))) {
    if (file.exists(normalizePath(p, mustWork = FALSE))) { adsl_path <- normalizePath(p, mustWork = FALSE); break }
  }
}
if (is.null(adsl_path)) {
  # fallback: search under D:/meta_hackathon
  poss <- list.files("D:/meta_hackathon", pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(poss) > 0) adsl_path <- poss[1]
}
if (is.null(adsl_path)) {
  poss2 <- list.files(".", pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(poss2) > 0) adsl_path <- poss2[1]
}
if (is.null(adsl_path)) stop("adsl.xpt not found in candidates: ", paste(candidates, collapse="; "))

adsl_raw <- haven::read_xpt(adsl_path)

# zap haven labels/formats to plain R types
adsl_raw <- haven::zap_formats(adsl_raw)
adsl_raw <- haven::zap_labels(adsl_raw)
adsl_raw <- haven::zap_label(adsl_raw)

# normalize column names to lower case for matching (SAS is case-insensitive)
names(adsl_raw) <- tolower(names(adsl_raw))

# keep required columns, ensure they exist
stopifnot(all(c("usubjid","trt01an","trt01a","dcreascd","ittfl") %in% names(adsl_raw)))

adsl <- adsl_raw %>%
  transmute(
    usubjid = as.character(usubjid),
    trt01an = as.numeric(trt01an),
    trt01a  = as.character(trt01a),
    dcreascd = as.character(dcreascd),
    ittfl = as.character(ittfl)
  ) %>%
  filter(ittfl == "Y")

# Create total (adsl1): duplicate with trt01an=999, trt01a="Total"
adsl1 <- bind_rows(
  adsl,
  adsl %>% mutate(trt01an = 999, trt01a = "Total")
)

# Header N Count : tot
tot <- adsl1 %>%
  group_by(trt01an) %>%
  summarise(tot = n_distinct(usubjid), .groups = "drop") %>%
  arrange(trt01an)

# dummy dataset (created in SAS but not merged; kept for fidelity, not used downstream)
dummy <- tibble::tibble(
  col1 = c("Completed the study","Discontinued"," ","  Death","  Adverse Event","  Lack of Efficacy",
           "  Lost to Follow-up","  Non-compliance with Study Drug","  Pregnancy","  Protocal Violation",
           "  Physician Decision","  Withdrawl by Subject","  Withdrawl by Parent/Guardian","  Recovery",
           "  Technical Problems","  Other"),
  ord = 1:16
)

# helper to compute upcase with SAS missing handling (missing -> "")
up <- function(x) {
  x2 <- ifelse(is.na(x), "", x)
  toupper(x2)
}

# disp macro emulation
# comp: upcase(dcreascd) = "COMPLETED", var = "Completed the study"
comp <- adsl1 %>%
  filter(up(dcreascd) == "COMPLETED") %>%
  group_by(trt01an, trt01a) %>%
  summarise(cnt = n_distinct(usubjid), .groups = "drop") %>%
  mutate(col1 = "Completed the study") %>%
  left_join(tot, by = "trt01an") %>%
  mutate(per = (cnt * 100) / tot) %>%
  select(cnt, per, col1, trt01an, trt01a)

# disc: upcase(dcreascd) ^= "COMPLETED", var = "Discontinued"
disc <- adsl1 %>%
  filter(up(dcreascd) != "COMPLETED") %>%
  group_by(trt01an, trt01a) %>%
  summarise(cnt = n_distinct(usubjid), .groups = "drop") %>%
  mutate(col1 = "Discontinued") %>%
  left_join(tot, by = "trt01an") %>%
  mutate(per = (cnt * 100) / tot) %>%
  select(cnt, per, col1, trt01an, trt01a)

# all: upcase(dcreascd) ^= "COMPLETED", var = dcreascd
all_tbl <- adsl1 %>%
  filter(up(dcreascd) != "COMPLETED") %>%
  group_by(trt01an, trt01a, col1 = dcreascd) %>%
  summarise(cnt = n_distinct(usubjid), .groups = "drop") %>%
  left_join(tot, by = "trt01an") %>%
  mutate(per = (cnt * 100) / tot) %>%
  select(cnt, per, col1, trt01an, trt01a)

# Combine as in SAS: set comp disc all(in=a); by trt01an trt01a; if a then col1 = "  " || trim(left(col1));
# Mark 'a' rows (from all)
comp2 <- comp %>% mutate(.src = "comp")
disc2 <- disc %>% mutate(.src = "disc")
all2  <- all_tbl %>% mutate(.src = "all")

disp_combined <- bind_rows(comp2, disc2, all2)

# Apply SAS prefix for all rows only
disp_combined <- disp_combined %>%
  mutate(col1 = ifelse(.src == "all",
                       paste0("  ", trimws(col1, which = "left")),
                       col1))

# Create nper = put(cnt,3.) || "(" || put(per,5.1) || ")"
disp_combined <- disp_combined %>%
  mutate(
    nper = paste0(fmt_cnt_3(cnt), "(", fmt_per_5_1(per), ")")
  )

# Order to emulate SAS BY trt01an trt01a and set order comp->disc->all
# Within each trt01an, keep comp first, disc second, then all sorted by col1 (group by order)
disp_combined <- disp_combined %>%
  mutate(ord_src = case_when(.src == "comp" ~ 1L, .src == "disc" ~ 2L, .src == "all" ~ 3L, TRUE ~ 9L)) %>%
  arrange(trt01an, ord_src, col1) %>%
  select(cnt, per, col1, trt01an, trt01a, nper)

# Ensure types match SAS: cnt integer, per numeric, trt01an numeric
disp <- disp_combined %>%
  mutate(
    cnt = as.integer(cnt),
    per = as.numeric(per),
    trt01an = as.numeric(trt01an),
    trt01a = as.character(trt01a),
    col1 = as.character(col1),
    nper = as.character(nper)
  )

# Write dataset disp -> OUT_DIR/disp.csv
# Dates as YYYY-MM-DD (no dates in this dataset), na = ""
write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

# Also write a plain-text rendering for verification (optional but helpful)
# Contains same numbers SAS would show
txt_path <- file.path(out, "disp.txt")
con <- file(txt_path, open = "wt")
writeLines("Subject Disposition (disp)", con)
writeLines(paste0("Source: ", adsl_path), con)
writeLines("", con)
# header N per tot
tot_lines <- tot %>% arrange(trt01an) %>% mutate(line = paste0("trt01an=", trt01an, " N=", tot))
for (ln in tot_lines$line) writeLines(ln, con)
writeLines("", con)
# table body
# format as fixed width for readability
hdr <- sprintf("%-40s %6s %8s %10s %s", "col1", "trt01an", "trt01a", "cnt", "nper")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
for (i in seq_len(nrow(disp))) {
  r <- disp[i,]
  line <- sprintf("%-40s %6d %8s %10d %s", r$col1, r$trt01an, r$trt01a, r$cnt, r$nper)
  writeLines(line, con)
}
close(con)
```