# P071 t_disp.sas -> R translation
# Table 14.1.2 Subject Disposition, All Randomized Subjects
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

# ---- helpers: SAS semantics ----
round_half_up <- function(x, digits = 0) {
  # SAS rounds half away from zero
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}
fmt_5_1 <- function(x) {
  # SAS PUT(.,5.1): round half away, 1 decimal, then strip
  if (is.na(x)) return(NA_character_)
  v <- round_half_up(x, 1)
  # sprintf to 1 decimal
  s <- sprintf("%.1f", v)
  trimws(s)
}
# SAS character sort: uppercase before lowercase (byte order, C locale)
sas_sort_char <- function(v) {
  uv <- unique(v)
  uv <- uv[!is.na(uv)]
  # order by byte values: use xtfrm with C locale via order(method="radix")
  # radix sorts bytes; enforce C collation
  o <- order(uv, method = "radix")
  uv[o]
}

# ---- locate ADSL ----
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
adsl_file <- NULL
if (dir.exists(adam_dir)) {
  fl <- list.files(adam_dir, full.names = TRUE)
  # find adsl case-insensitive
  idx <- grep("adsl", basename(fl), ignore.case = TRUE)
  if (length(idx) > 0) adsl_file <- fl[idx[1]]
}
# fallbacks
if (is.null(adsl_file)) {
  cand <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/ADSL.xpt"
  )
  for (c in cand) if (file.exists(c)) adsl_file <- c
}
stopifnot(!is.null(adsl_file), file.exists(adsl_file))
message("Reading ADSL: ", adsl_file)
if (grepl("\\.xpt$", adsl_file, ignore.case = TRUE)) {
  adsl <- haven::read_xpt(adsl_file)
} else {
  adsl <- haven::read_sas(adsl_file)
}
# normalize names to uppercase for matching, keep original values
names(adsl) <- toupper(names(adsl))
stopifnot(all(c("USUBJID","TRTA","RANDFL") %in% names(adsl)))
for (c in c("ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")) {
  if (!c %in% names(adsl)) adsl[[c]] <- NA_character_
}
# ensure character
for (c in c("USUBJID","TRTA","RANDFL","ITTFL","SAFFL","EFFFL","PPROTFL","EOSSTT","DCSREAS")) {
  adsl[[c]] <- as.character(adsl[[c]])
}

# ---- Step 1: Big N (all randomized, upcase RANDFL='Y'), order by TRTA ----
bign_df <- adsl %>%
  filter(!is.na(RANDFL), toupper(RANDFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop")
trt_order <- sas_sort_char(bign_df$TRTA)
bign_df <- bign_df %>% arrange(match(TRTA, trt_order))
bign_tot <- adsl %>% filter(!is.na(RANDFL), toupper(RANDFL)=="Y") %>%
  summarise(n = n_distinct(USUBJID)) %>% pull(n)
message("TRT order: ", paste(trt_order, collapse=" | "))
message("BigN: ", paste(bign_df$bign, collapse=", "), " total=", bign_tot)
stopifnot(length(trt_order) == 3)

bign1 <- bign_df$bign[match(trt_order[1], bign_df$TRTA)]
bign2 <- bign_df$bign[match(trt_order[2], bign_df$TRTA)]
bign3 <- bign_df$bign[match(trt_order[3], bign_df$TRTA)]
col1hdr <- paste0(trt_order[1], " (N=", bign1, ")")
col2hdr <- paste0(trt_order[2], " (N=", bign2, ")")
col3hdr <- paste0(trt_order[3], " (N=", bign3, ")")

# ---- Step 2: disp where RANDFL='Y' (case-sensitive as in SAS) ----
disp <- adsl %>% filter(!is.na(RANDFL), RANDFL == "Y")
stopifnot(nrow(disp) > 0)

# ---- Step 3: population counts ----
pop_count <- function(flag, label, ord) {
  disp %>% filter(!is.na(.data[[flag]]), toupper(.data[[flag]]) == "Y") %>%
    group_by(TRTA) %>%
    summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
    mutate(category = label, ord = ord)
}
p_rand  <- pop_count("RANDFL",  "Randomized", 1)
p_itt   <- pop_count("ITTFL",   "  Intent-to-Treat Set", 2)
p_saff  <- pop_count("SAFFL",   "  Safety Analysis Set", 3)
p_efff  <- pop_count("EFFFL",   "  Efficacy Analysis Set", 4)
p_pprot <- pop_count("PPROTFL", "  Per-Protocol Set", 5)

# ---- Step 4: completion / discontinuation (exact match) ----
completed <- disp %>% filter(!is.na(EOSSTT), EOSSTT == "COMPLETED") %>%
  group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(category = "Completed Study", ord = 10)
discont <- disp %>% filter(!is.na(EOSSTT), EOSSTT == "DISCONTINUED") %>%
  group_by(TRTA) %>% summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(category = "Discontinued Study", ord = 11)

# ---- Step 5: reasons ----
reasons <- disp %>%
  filter(!is.na(EOSSTT), EOSSTT == "DISCONTINUED",
         !is.na(DCSREAS), trimws(DCSREAS) != "") %>%
  group_by(TRTA, DCSREAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop")
reasons2 <- reasons %>%
  mutate(category = paste0("  ", trimws(DCSREAS)),
         ord = case_when(
           toupper(trimws(DCSREAS)) == "ADVERSE EVENT" ~ 12,
           toupper(trimws(DCSREAS)) == "DEATH" ~ 13,
           toupper(trimws(DCSREAS)) == "LACK OF EFFICACY" ~ 14,
           toupper(trimws(DCSREAS)) == "LOST TO FOLLOW-UP" ~ 15,
           toupper(trimws(DCSREAS)) == "PHYSICIAN DECISION" ~ 16,
           toupper(trimws(DCSREAS)) == "PROTOCOL VIOLATION" ~ 17,
           toupper(trimws(DCSREAS)) == "WITHDRAWAL BY SUBJECT" ~ 18,
           TRUE ~ 19
         )) %>%
  select(TRTA, cnt, category, ord)

# ---- Step 6: combine ----
all_disp <- bind_rows(p_rand, p_itt, p_saff, p_efff, p_pprot,
                      completed, discont, reasons2)
stopifnot(nrow(all_disp) > 0, all(c("TRTA","cnt","category","ord") %in% names(all_disp)))

# ---- Step 7: percentages and value formatting ----
all_disp2 <- all_disp %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_)) %>%
  arrange(ord, TRTA)
stopifnot("bign" %in% names(all_disp2))

all_disp3 <- all_disp2 %>%
  rowwise() %>%
  mutate(value = if (ord == 1) trimws(as.character(cnt))
         else if (!is.na(cnt) && cnt > 0) paste0(trimws(as.character(cnt)), " (", fmt_5_1(pct), ")")
         else "0") %>%
  ungroup()

# ---- Step 8: sort + transpose ----
# SAS sort by ord category (byte order)
# enforce SAS order for category within each ord
all_disp3 <- all_disp3 %>% arrange(ord, category)
# pivot: by ord category, id TRTA, var value
wide <- all_disp3 %>%
  select(ord, category, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = "")
# rename TRTA columns to col1..col3 in trt_order sequence
for (i in seq_along(trt_order)) {
  tn <- trt_order[i]
  cn <- paste0("col", i)
  if (tn %in% names(wide)) {
    names(wide)[names(wide) == tn] <- cn
  } else {
    wide[[cn]] <- ""
  }
}
# ensure col1..col3 exist
for (i in 1:3) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide)) wide[[cn]] <- ""
}
# order rows by ord then category (SAS byte order)
wide <- wide %>% arrange(ord, category)
final <- wide

# ---- Step 9: section headers ----
report <- final %>%
  arrange(ord) %>%
  mutate(section = case_when(
    ord == 1 ~ "Analysis Populations",
    ord == 10 ~ "Study Completion",
    ord >= 12 ~ "  Reason for Discontinuation",
    TRUE ~ ""
  ))
# _show_section only once per section
report[["_show_section"]] <- ""
lastsect <- ""
for (r in seq_len(nrow(report))) {
  s <- report$section[r]
  if (!is.na(s) && s != "" && s != lastsect) {
    report[["_show_section"]][r] <- s
    lastsect <- s
  } else {
    report[["_show_section"]][r] <- ""
  }
}

# ---- Step 10: outputs ----
# CSV of table body (visible columns)
csv_df <- report %>% select(ord, category, col1, col2, col3)
# use header labels with N for col names in a second header row? Keep simple col names but store labels separately
# For CSV, use display headers as column names
names(csv_df) <- c("ord", "Category", col1hdr, col2hdr, col3hdr)
write.csv(csv_df, file.path(out, "t_disp.csv"), row.names = FALSE, na = "")

# Text rendering mimicking RTF / PROC REPORT
# Titles from u_titles.sas for T_DISP, population All Randomized Subjects
t1 <- "[Sponsor Name]                                                              Page 1 of N"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Table 14.1.2"
t5 <- "Subject Disposition"
t6 <- "All Randomized Subjects"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- "Program: P071_t_disp.sas"

# Build fixed-width text table
# Column widths: Category 40, each trt 22
w_cat <- 42
w_trt <- 22
hdr_cat <- "Category"
# col headers may contain " (N=)": split with ~ in SAS; show on one line
pad_c <- function(x, w, align="left") {
  x <- ifelse(is.na(x), "", as.character(x))
  if (align=="left") sprintf(paste0("%-", w, "s"), substr(x, 1, w))
  else sprintf(paste0("%", w, "s"), substr(x, 1, w))
}
lines <- c()
lines <- c(lines, t1, t2, "", t4, t5, t6, "")
# header row
lines <- c(lines,
  paste0(pad_c(hdr_cat, w_cat, "left"),
         pad_c(col1hdr, w_trt, "center"),
         pad_c(col2hdr, w_trt, "center"),
         pad_c(col3hdr, w_trt, "center")),
  paste0(paste(rep("-", w_cat), collapse=""), " ",
         paste(rep("-", w_trt), collapse=""), " ",
         paste(rep("-", w_trt), collapse=""), " ",
         paste(rep("-", w_trt), collapse=""))
)
# section + category rows: show section as bold standalone row when _show_section != ""
for (r in seq_len(nrow(report))) {
  sh <- report[["_show_section"]][r]
  if (!is.na(sh) && trimws(sh) != "") {
    lines <- c(lines, trimws(sh))
  }
  catv <- report$category[r]
  lines <- c(lines, paste0(pad_c(catv, w_cat, "left"),
                           pad_c(report$col1[r], w_trt, "center"),
                           pad_c(report$col2[r], w_trt, "center"),
                           pad_c(report$col3[r], w_trt, "center")))
}
lines <- c(lines, "", f1, f2, f3)
writeLines(lines, file.path(out, "t_disp.txt"))
message("Wrote t_disp.csv and t_disp.txt to ", out)
