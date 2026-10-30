# P071_t_disp.sas -> R translation
# Table 14.1.2 Subject Disposition - All Randomized Subjects

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(lubridate)
})

# Helper: SAS round half away from zero (half up) - vectorized
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x)
  rounded <- floor(abs_x * mult + 0.5 + 1e-9) / mult
  res <- sign_x * rounded
  res[is.na(x)] <- NA_real_
  res
}

# Locate ADSL
adam_path1 <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adam_path2 <- Sys.getenv("rawadam")
if (adam_path2 == "") adam_path2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adam_paths <- unique(c(adam_path1, adam_path2))

adsl_file <- NULL
for (p in adam_paths) {
  if (dir.exists(p)) {
    cand <- list.files(p, pattern = "^adsl\\.(sas7bdat|xpt)$", ignore.case = TRUE, full.names = TRUE)
    if (length(cand) > 0) { adsl_file <- cand[1]; break }
    cand2 <- list.files(p, pattern = "adsl", ignore.case = TRUE, full.names = TRUE)
    if (length(cand2) > 0) {
      cand2 <- cand2[!dir.exists(cand2)]
      if (length(cand2) > 0) { adsl_file <- cand2[1]; break }
    }
  }
}
if (is.null(adsl_file) || !file.exists(adsl_file)) {
  for (p in adam_paths) {
    if (dir.exists(p)) {
      cand <- list.files(p, recursive = TRUE, pattern = "adsl", ignore.case = TRUE, full.names = TRUE)
      if (length(cand) > 0) { adsl_file <- cand[1]; break }
    }
  }
}
if (is.null(adsl_file) || !file.exists(adsl_file)) {
  stop("ADSL file not found in adam paths: ", paste(adam_paths, collapse="; "))
}

ext <- tolower(tools::file_ext(adsl_file))
if (ext == "xpt") {
  adsl_raw <- haven::read_xpt(adsl_file)
} else if (ext == "sas7bdat") {
  adsl_raw <- haven::read_sas(adsl_file)
} else {
  adsl_raw <- haven::read_sas(adsl_file)
}
adsl <- adsl_raw
adsl <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))
names(adsl) <- toupper(names(adsl))

required <- c("USUBJID","TRTA","RANDFL","ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")
for (col in required) {
  if (!col %in% names(adsl)) adsl[[col]] <- NA_character_
}

# Step 1: m_counts - Big N for RANDFL=Y
bign_df <- adsl %>%
  filter(toupper(RANDFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTA)

bigntot <- adsl %>% filter(toupper(RANDFL) == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)

trt_levels_sorted <- sort(unique(bign_df$TRTA))
find_trt <- function(pattern, levels) {
  idx <- grep(pattern, levels, ignore.case = TRUE)
  if (length(idx) > 0) levels[idx[1]] else NA_character_
}
placebo_lbl <- find_trt("placebo", trt_levels_sorted)
low_lbl <- find_trt("low", trt_levels_sorted)
high_lbl <- find_trt("high", trt_levels_sorted)

if (!is.na(placebo_lbl) && !is.na(low_lbl) && !is.na(high_lbl)) {
  display_order <- c(placebo_lbl, low_lbl, high_lbl)
  remaining <- setdiff(trt_levels_sorted, display_order)
  display_order <- c(display_order, remaining)
} else {
  display_order <- trt_levels_sorted
}
bign_df <- bign_df %>% mutate(TRTA = factor(TRTA, levels = display_order)) %>% arrange(TRTA) %>% mutate(TRTA = as.character(TRTA))

headers <- bign_df %>% mutate(hdr = paste0(TRTA, " (N=", bign, ")")) %>% pull(hdr)
col_hdrs <- setNames(headers, paste0("col", seq_along(headers)))
while (length(col_hdrs) < 3) {
  col_hdrs <- c(col_hdrs, setNames(paste0("Missing (N=0)"), paste0("col", length(col_hdrs)+1)))
}
col1hdr <- col_hdrs[["col1"]]
col2hdr <- col_hdrs[["col2"]]
col3hdr <- col_hdrs[["col3"]]

# Step 2: Prepare disp
disp <- adsl %>% filter(RANDFL == "Y")

pop_count <- function(flag, label, ord) {
  if (!flag %in% names(disp)) return(tibble(TRTA=character(), cnt=integer(), category=character(), ord=integer()))
  disp %>%
    filter(toupper(.data[[flag]]) == "Y") %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
    mutate(category = label, ord = ord)
}

pop_randfl  <- pop_count("RANDFL",  "Randomized", 1)
pop_ittfl   <- pop_count("ITTFL",   "  Intent-to-Treat Set", 2)
pop_saffl   <- pop_count("SAFFL",   "  Safety Analysis Set", 3)
pop_efffl   <- pop_count("EFFFL",   "  Efficacy Analysis Set", 4)
pop_pprotfl <- pop_count("PPROTFL", "  Per-Protocol Set", 5)

completed <- disp %>%
  filter(EOSSTT == "COMPLETED") %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(category = "Completed Study", ord = 10)

discont <- disp %>%
  filter(EOSSTT == "DISCONTINUED") %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(category = "Discontinued Study", ord = 11)

reasons <- disp %>%
  filter(EOSSTT == "DISCONTINUED" & !is.na(DCSREAS) & DCSREAS != "") %>%
  group_by(TRTA, DCSREAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop")

reasons2 <- reasons %>%
  mutate(category = paste0("  ", str_trim(DCSREAS)),
         ord = case_when(
           toupper(DCSREAS) == "ADVERSE EVENT" ~ 12L,
           toupper(DCSREAS) == "DEATH" ~ 13L,
           toupper(DCSREAS) == "LACK OF EFFICACY" ~ 14L,
           toupper(DCSREAS) == "LOST TO FOLLOW-UP" ~ 15L,
           toupper(DCSREAS) == "PHYSICIAN DECISION" ~ 16L,
           toupper(DCSREAS) == "PROTOCOL VIOLATION" ~ 17L,
           toupper(DCSREAS) == "WITHDRAWAL BY SUBJECT" ~ 18L,
           TRUE ~ 19L
         )) %>%
  select(TRTA, cnt, category, ord)

all_disp <- bind_rows(pop_randfl, pop_ittfl, pop_saffl, pop_efffl, pop_pprotfl,
                      completed, discont, reasons2)

all_disp2 <- all_disp %>%
  left_join(bign_df, by="TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_)) %>%
  arrange(ord, TRTA)

all_disp3 <- all_disp2 %>%
  mutate(pct_fmt = ifelse(is.na(pct), NA_character_, sprintf("%.1f", round_half_up(pct, 1))),
         value = ifelse(ord == 1,
                        as.character(cnt),
                        ifelse(cnt > 0, paste0(trimws(as.character(cnt)), " (", pct_fmt, ")"), "0")))

all_disp3 <- all_disp3 %>% arrange(ord, category)

wide <- all_disp3 %>%
  select(ord, category, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

for (trt in display_order) {
  if (!trt %in% names(wide)) wide[[trt]] <- NA_character_
}
wide <- wide %>% select(ord, category, all_of(display_order))

col_map <- setNames(paste0("col", seq_along(display_order)), display_order)
wide_renamed <- wide
for (trt in display_order) {
  wide_renamed[[col_map[[trt]]]] <- wide_renamed[[trt]]
  wide_renamed[[trt]] <- NULL
}
for (i in 1:3) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide_renamed)) wide_renamed[[cn]] <- NA_character_
}
for (cn in paste0("col", 1:3)) {
  wide_renamed[[cn]][is.na(wide_renamed[[cn]])] <- "0"
}
wide_renamed <- wide_renamed %>% arrange(ord, category)

report <- wide_renamed %>%
  mutate(section = case_when(
    ord == 1 ~ "Analysis Populations",
    ord == 10 ~ "Study Completion",
    ord >= 12 ~ "  Reason for Discontinuation",
    TRUE ~ ""
  )) %>%
  arrange(ord)

report <- report %>% mutate(`_show_section` = NA_character_)
lastsect <- ""
show_vec <- character(nrow(report))
for (i in seq_len(nrow(report))) {
  sec <- report$section[i]
  if (sec != lastsect && sec != "") {
    show_vec[i] <- sec
    lastsect <- sec
  } else {
    show_vec[i] <- ""
  }
}
report$`_show_section` <- show_vec

final_csv <- report %>%
  select(ord, category, col1, col2, col3, section, `_show_section`)

write.csv(final_csv, file.path(out, "t_disp.csv"), row.names = FALSE, na = "")
write.csv(bign_df, file.path(out, "_bign.csv"), row.names = FALSE, na = "")

# Titles from u_titles.sas
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

title1 <- paste0(sponsor, "    Page 1 of 1")
title2 <- paste0("Protocol: ", protocol)
title4 <- "Table 14.1.2"
title5 <- "Subject Disposition"
title6 <- "All Randomized Subjects"

footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Source: ADaM datasets"
prog <- "P071_t_disp.sas"
rundate <- format(Sys.Date(), "%d%b%Y")
runtime <- format(Sys.time(), "%H:%M")
footnote3 <- paste0("Program: ", prog, " | Run: ", rundate, " ", runtime)

# Build text rendering - must contain all numbers
txt_path <- file.path(out, "t_disp.txt")
rtf_path <- file.path(out, "t_disp.rtf")
for (out_path in c(txt_path, rtf_path)) {
  con <- file(out_path, open="wt")
  cat(title1, "\n", file=con)
  cat(title2, "\n", file=con)
  cat("\n", file=con)
  cat(title4, "\n", file=con)
  cat(title5, "\n", file=con)
  cat(title6, "\n", file=con)
  cat("\n", file=con)
  hdr_line <- sprintf("%-40s %-20s %-20s %-20s", "Category", col1hdr, col2hdr, col3hdr)
  cat(hdr_line, "\n", file=con)
  cat(paste(rep("-", nchar(hdr_line)), collapse=""), "\n", file=con)
  for (i in seq_len(nrow(report))) {
    sec_show <- report$`_show_section`[i]
    if (sec_show != "" && !is.na(sec_show)) {
      cat("\n", file=con)
      cat(sec_show, "\n", file=con)
    }
    cat(sprintf("%-40s %-20s %-20s %-20s",
                report$category[i],
                ifelse(is.na(report$col1[i]), "", report$col1[i]),
                ifelse(is.na(report$col2[i]), "", report$col2[i]),
                ifelse(is.na(report$col3[i]), "", report$col3[i])), "\n", file=con)
  }
  cat("\n", file=con)
  cat(footnote1, "\n", file=con)
  cat(footnote2, "\n", file=con)
  cat(footnote3, "\n", file=con)
  close(con)
}

cat("NOTE: t_disp.sas completed successfully.\n")
cat(paste0("Big N: ", paste(bign_df$TRTA, bign_df$bign, collapse=", "), " Total=", bigntot, "\n"))
