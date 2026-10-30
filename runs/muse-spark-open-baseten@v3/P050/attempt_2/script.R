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
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_cnt_3 <- function(x) {
  sprintf("%3d", as.integer(x))
}
fmt_per_5_1 <- function(x) {
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
candidates <- unique(candidates[candidates != ""])
adsl_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { adsl_path <- p; break }
  if (!is.na(p) && p != "" && file.exists(normalizePath(p, mustWork = FALSE))) {
    if (file.exists(normalizePath(p, mustWork = FALSE))) { adsl_path <- normalizePath(p, mustWork = FALSE); break }
  }
}
if (is.null(adsl_path)) {
  poss <- list.files("D:/meta_hackathon", pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(poss) > 0) adsl_path <- poss[1]
}
if (is.null(adsl_path)) {
  poss2 <- list.files(".", pattern = "^adsl\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(poss2) > 0) adsl_path <- poss2[1]
}
if (is.null(adsl_path)) stop("adsl.xpt not found in candidates: ", paste(candidates, collapse="; "))

adsl_raw <- haven::read_xpt(adsl_path)
adsl_raw <- haven::zap_formats(adsl_raw)
adsl_raw <- haven::zap_labels(adsl_raw)
adsl_raw <- haven::zap_label(adsl_raw)
names(adsl_raw) <- tolower(names(adsl_raw))
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

dummy <- tibble::tibble(
  col1 = c("Completed the study","Discontinued"," ","  Death","  Adverse Event","  Lack of Efficacy",
           "  Lost to Follow-up","  Non-compliance with Study Drug","  Pregnancy","  Protocal Violation",
           "  Physician Decision","  Withdrawl by Subject","  Withdrawl by Parent/Guardian","  Recovery",
           "  Technical Problems","  Other"),
  ord = 1:16
)

up <- function(x) {
  x2 <- ifelse(is.na(x), "", x)
  toupper(x2)
}

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

comp2 <- comp %>% mutate(.src = "comp")
disc2 <- disc %>% mutate(.src = "disc")
all2  <- all_tbl %>% mutate(.src = "all")

disp_combined <- bind_rows(comp2, disc2, all2)

# SAS: SET comp disc all(in=a); BY trt01an trt01a; IF a THEN col1 = "  " || TRIM(LEFT(col1));
# Variable attributes (length) are taken from first dataset in SET (comp).
# comp col1 = "Completed the study" => length 19. So all subsequent col1 values are truncated to 19.
disp_combined <- disp_combined %>%
  mutate(col1 = ifelse(.src == "all",
                       paste0("  ", trimws(col1, which = "left")),
                       col1)) %>%
  mutate(col1 = substr(col1, 1, 19))

# Create nper = put(cnt,3.) || "(" || put(per,5.1) || ")"
disp_combined <- disp_combined %>%
  mutate(
    nper = paste0(fmt_cnt_3(cnt), "(", fmt_per_5_1(per), ")")
  )

disp_combined <- disp_combined %>%
  mutate(ord_src = case_when(.src == "comp" ~ 1L, .src == "disc" ~ 2L, .src == "all" ~ 3L, TRUE ~ 9L)) %>%
  arrange(trt01an, ord_src, col1) %>%
  select(cnt, per, col1, trt01an, trt01a, nper)

disp <- disp_combined %>%
  mutate(
    cnt = as.integer(cnt),
    per = as.numeric(per),
    trt01an = as.numeric(trt01an),
    trt01a = as.character(trt01a),
    col1 = as.character(col1),
    nper = as.character(nper)
  )

write.csv(disp, file.path(out, "disp.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "disp.txt")
con <- file(txt_path, open = "wt")
writeLines("Subject Disposition (disp)", con)
writeLines(paste0("Source: ", adsl_path), con)
writeLines("", con)
tot_lines <- tot %>% arrange(trt01an) %>% mutate(line = paste0("trt01an=", trt01an, " N=", tot))
for (ln in tot_lines$line) writeLines(ln, con)
writeLines("", con)
hdr <- sprintf("%-40s %6s %8s %10s %s", "col1", "trt01an", "trt01a", "cnt", "nper")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
for (i in seq_len(nrow(disp))) {
  r <- disp[i,]
  line <- sprintf("%-40s %6d %8s %10d %s", r$col1, r$trt01an, r$trt01a, r$cnt, r$nper)
  writeLines(line, con)
}
close(con)
