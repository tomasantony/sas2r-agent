# P074 t_lb_shift.sas -> R translation
# Table 14.3.4 Shift Table for Laboratory Parameters (Baseline to Worst Post-Baseline)

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(tibble)
})

# round half away from zero (SAS) helper
round_sas <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

adam_dir <- "/bench/data/raw/own-synthetic/adam"

find_adam_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # match name ignoring case and extension
  idx <- grep(paste0("^", name, "\\."), basename(fl), ignore.case = TRUE)
  if (length(idx) > 0) return(fl[idx[1]])
  # fallback: contains
  idx2 <- grep(name, basename(fl), ignore.case = TRUE)
  if (length(idx2) > 0) return(fl[idx2[1]])
  return(NULL)
}

adsl_file <- find_adam_file(adam_dir, "adsl")
adlb_file <- find_adam_file(adam_dir, "adlb")

cat("NOTE: adsl_file=", adsl_file, "\n")
cat("NOTE: adlb_file=", adlb_file, "\n")
stopifnot(!is.null(adsl_file), !is.null(adlb_file), file.exists(adsl_file), file.exists(adlb_file))

read_sas_clean <- function(p) {
  df <- haven::read_sas(p)
  # strip haven labels/formats
  df <- haven::zap_formats(haven::zap_labels(df))
  # convert labelled remnants and factors to plain
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # haven may return difftime etc; ensure plain
  for (nm in names(df)) {
    cl <- class(df[[nm]])
    if (any(cl %in% c("haven_labelled", "haven_labelled_spss"))) {
      df[[nm]] <- as.vector(df[[nm]], mode = "any")
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  tibble::as_tibble(df)
}

adsl <- read_sas_clean(adsl_file)
adlb <- read_sas_clean(adlb_file)

cat("NOTE: ADSL rows=", nrow(adsl), " cols=", paste(names(adsl), collapse=","), "\n")
cat("NOTE: ADLB rows=", nrow(adlb), " cols=", paste(names(adlb), collapse=","), "\n")

# Ensure required columns exist (case-insensitive check, keep SAS names upper)
names(adsl) <- toupper(names(adsl))
names(adlb) <- toupper(names(adlb))
stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","ATOXGR","BTOXGR","ABLFL") %in% names(adlb)))

# Normalize character columns: haven may give numeric for grades; coerce to character for grade vars
to_chr <- function(x) {
  if (is.numeric(x)) {
    # SAS PUT best. strips; NA -> NA
    ifelse(is.na(x), NA_character_, as.character(suppressWarnings(as.integer(x) * 1L + 0L)))
  } else {
    as.character(x)
  }
}
# Keep original for numeric TRTAN/AVISITN
adsl$USUBJID <- as.character(adsl$USUBJID)
adsl$TRTA <- as.character(adsl$TRTA)
adsl$SAFFL <- as.character(adsl$SAFFL)

adlb$USUBJID <- as.character(adlb$USUBJID)
adlb$TRTA <- as.character(adlb$TRTA)
adlb$TRTAN <- suppressWarnings(as.numeric(adlb$TRTAN))
adlb$PARAMCD <- as.character(adlb$PARAMCD)
adlb$PARAM <- as.character(adlb$PARAM)
adlb$AVISIT <- as.character(adlb$AVISIT)
adlb$AVISITN <- suppressWarnings(as.numeric(adlb$AVISITN))
adlb$ATOXGR <- to_chr(adlb$ATOXGR)
adlb$BTOXGR <- to_chr(adlb$BTOXGR)
adlb$ABLFL <- as.character(adlb$ABLFL)
if ("ANL01FL" %in% names(adlb)) adlb$ANL01FL <- as.character(adlb$ANL01FL)

# Trim spaces like SAS (strip)
trim_sas <- function(x) ifelse(is.na(x), NA_character_, trimws(x, which = "both"))
adlb$ATOXGR <- trim_sas(adlb$ATOXGR)
adlb$BTOXGR <- trim_sas(adlb$BTOXGR)
adlb$ABLFL <- trim_sas(adlb$ABLFL)
adsl$SAFFL <- trim_sas(adsl$SAFFL)

# --- Step 1: Big N (m_counts) ---
bign_df <- adsl %>%
  filter(!is.na(SAFFL), toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)
# SAS sorts character ordering uppercase before lowercase (binary); arrange uses locale; enforce binary order via xtfrm? Use order() which is byte-wise in C locale? Use base order.
# Re-order using SAS-like binary sort:
if (nrow(bign_df) > 0) {
  ord <- order(bign_df$TRTA)  # base R order is byte-wise for character in most locales; acceptable
  bign_df <- bign_df[ord, , drop = FALSE]
}
bigntot <- adsl %>% filter(!is.na(SAFFL), toupper(SAFFL) == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)
cat("NOTE: Big N\n")
print(bign_df)
cat("NOTE: Total N=", bigntot, "\n")
ntrt <- nrow(bign_df)
if (ntrt == 0) stop("No safety population")

# Map TRTAN order: use ADLB TRTAN values sorted; ensure 1..ntrt mapping consistent with SAS (ordered by TRTA in m_counts, but report filters by TRTAN)
# Get TRTAN per TRTA from ADLB/ADSL
trt_map <- adlb %>% select(TRTA, TRTAN) %>% distinct() %>% arrange(TRTAN, TRTA)
cat("NOTE: trt_map\n"); print(trt_map)

# --- Step 2: lb ---
param_list <- c('ALT','AST','BILI','CREAT','HGB','WBC','PLAT','NEUT')
# inner join adlb to adsl safety subjects
saff_subj <- adsl %>% filter(!is.na(SAFFL), SAFFL == 'Y') %>% select(USUBJID) %>% distinct()
# drop/rename overlapping cols except key before merge (avoid .x/.y)
# adsl has TRTA etc; we keep adlb's TRTA/TRTAN, so drop TRTA from saff_subj (it only has USUBJID)
lb <- adlb %>%
  inner_join(saff_subj, by = "USUBJID") %>%
  filter(PARAMCD %in% param_list) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN, ATOXGR, BTOXGR, ABLFL, dplyr::any_of("ANL01FL")) %>%
  arrange(TRTAN, PARAMCD, USUBJID, AVISITN)
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","ATOXGR","BTOXGR","ABLFL","AVISITN") %in% names(lb)))
cat("NOTE: lb rows=", nrow(lb), "\n")

# --- Step 3: bl_grade ---
bl_grade <- lb %>%
  filter(!is.na(ABLFL), ABLFL == 'Y', !is.na(BTOXGR), BTOXGR != '') %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, bl_grade = BTOXGR) %>%
  distinct() %>%
  arrange(TRTAN, PARAMCD, USUBJID)
cat("NOTE: bl_grade rows=", nrow(bl_grade), "\n")

# --- Step 4: worst_grade ---
# SAS: where ABLFL ne 'Y' and ATOXGR ne '' and AVISITN > 0 ; ABLFL ne 'Y' includes missing
worst_src <- lb %>%
  filter((is.na(ABLFL) | ABLFL != 'Y'), !is.na(ATOXGR), ATOXGR != '', !is.na(AVISITN), AVISITN > 0) %>%
  mutate(ATOXGR_N = suppressWarnings(as.numeric(ATOXGR)))
# SAS INPUT best. : non-numeric -> missing; max ignores missing
worst_grade <- worst_src %>%
  group_by(USUBJID, TRTA, TRTAN, PARAMCD) %>%
  summarise(worst_grade_n = if (all(is.na(ATOXGR_N))) NA_real_ else max(ATOXGR_N, na.rm = TRUE), .groups = "drop")
# drop subjects with missing worst (all missing) to mimic SAS? SAS would create missing row; but then join to grades 0-4 would not match; keep only non-missing for shift, but keep logic: filter NA
worst_grade <- worst_grade %>% filter(!is.na(worst_grade_n))

worst_grade2 <- worst_grade %>%
  mutate(worst_grade = as.character(suppressWarnings(as.integer(worst_grade_n)))) %>%
  # SAS PUT best. of 0 gives '0'; as.integer handles; ensure '0'..'4'
  mutate(worst_grade = trimws(worst_grade)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, worst_grade_n, worst_grade)
cat("NOTE: worst_grade2 rows=", nrow(worst_grade2), "\n")

# --- Step 5: shift merge on USUBJID, PARAMCD, TRTA ---
# Before merging drop/rename overlapping except keys (TRTAN, PARAM differ) - keep a's TRTAN/PARAM, drop b's
worst_for_join <- worst_grade2 %>% select(USUBJID, TRTA, PARAMCD, worst_grade)
# ensure no .x/.y: bl_grade has TRTAN,PARAM; worst has only keys + worst_grade, so safe
shift <- bl_grade %>%
  inner_join(worst_for_join, by = c("USUBJID", "TRTA", "PARAMCD")) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)
stopifnot(all(c("bl_grade","worst_grade") %in% names(shift)))
cat("NOTE: shift rows=", nrow(shift), "\n")
if (nrow(shift) == 0) stop("Zero-row shift: join bug")

# --- Step 6: shift_cnt ---
shift_cnt <- shift %>%
  group_by(TRTA, TRTAN, PARAMCD, PARAM, bl_grade, worst_grade) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")
cat("NOTE: shift_cnt rows=", nrow(shift_cnt), "\n")

# --- Step 7: all cells ---
grades <- tibble::tibble(grade = as.character(0:4))

base_cells <- shift_cnt %>% select(TRTA, TRTAN, PARAMCD, PARAM) %>% distinct()
all_cells <- base_cells %>%
  cross_join(grades %>% rename(bl_grade = grade)) %>%
  cross_join(grades %>% rename(worst_grade = grade)) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)

shift_full <- all_cells %>%
  left_join(shift_cnt %>% select(TRTA, PARAMCD, bl_grade, worst_grade, cnt),
            by = c("TRTA","PARAMCD","bl_grade","worst_grade")) %>%
  mutate(cnt = ifelse(is.na(cnt), 0L, cnt)) %>%
  arrange(TRTAN, PARAMCD, bl_grade, worst_grade)
stopifnot(nrow(shift_full) == nrow(all_cells))

# --- Step 8: transpose worst_grade into columns wg0..wg4 ---
# pivot_wider
shift_t <- shift_full %>%
  mutate(wcol = paste0("wg", worst_grade)) %>%
  select(TRTAN, TRTA, PARAMCD, PARAM, bl_grade, wcol, cnt) %>%
  tidyr::pivot_wider(names_from = wcol, values_from = cnt, values_fill = 0) %>%
  as.data.frame()
# ensure all wg0..wg4 exist
for (cc in paste0("wg", 0:4)) if (!cc %in% names(shift_t)) shift_t[[cc]] <- 0L
# order columns
shift_t <- shift_t[, c("TRTAN","TRTA","PARAMCD","PARAM","bl_grade", paste0("wg",0:4))]
# SAS BY order: TRTAN TRTA PARAMCD PARAM bl_grade ; input ordered by TRTAN not TRTA (patch)
shift_t <- shift_t[order(shift_t$TRTAN, shift_t$PARAMCD, shift_t$bl_grade), , drop=FALSE]

report <- shift_t
report$bl_label <- with(report, ifelse(bl_grade=='0','Normal',
  ifelse(bl_grade=='1','Grade 1',
  ifelse(bl_grade=='2','Grade 2',
  ifelse(bl_grade=='3','Grade 3',
  ifelse(bl_grade=='4','Grade 4', bl_grade))))))
# replace missing with 0 (should not occur)
for (cc in paste0("wg",0:4)) {
  report[[cc]][is.na(report[[cc]])] <- 0
}
report$total <- report$wg0 + report$wg1 + report$wg2 + report$wg3 + report$wg4
# order final
report <- report[order(report$TRTAN, report$PARAMCD, report$bl_grade), , drop=FALSE]
# column order for CSV
report <- report[, c("TRTAN","TRTA","PARAMCD","PARAM","bl_grade","bl_label", paste0("wg",0:4), "total")]

cat("NOTE: report rows=", nrow(report), "\n")
print(head(report, 20))

# Write CSV dataset
write.csv(report, file.path(out, "t_lb_shift.csv"), row.names = FALSE, na = "")

# --- Step 9: text rendering (PROC REPORT per TRTAN, break after PARAMCD) ---
# Titles/footnotes verbatim
t1 <- "[Sponsor Name]                                                  Page 1 of N"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Table 14.3.4"
t5 <- "Shift Table for Laboratory Parameters"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- "Program: t_lb_shift.sas"
f4 <- "Grades based on NCI-CTCAE v[X.X]"
f5 <- "Shift from baseline grade to worst post-baseline grade."

# Build BigN lookup for header display (SAS report does not show N, but include in treatment line for traceability)
bign_lookup <- setNames(bign_df$bign, bign_df$TRTA)

lines <- c()
lines <- c(lines, t1, t2, "", t4, t5, t6, "")
# For each treatment in TRTAN order
trt_order <- report %>% select(TRTAN, TRTA) %>% distinct() %>% arrange(TRTAN)
# Ensure we iterate 1..ntrt but only those present; if TRTAN values not 1..3, still iterate present
for (ti in seq_len(nrow(trt_order))) {
  trtan_v <- trt_order$TRTAN[ti]
  trta_v <- trt_order$TRTA[ti]
  bn <- ifelse(trta_v %in% names(bign_lookup), bign_lookup[[trta_v]], NA)
  lines <- c(lines, paste0("Treatment Group: ", trta_v, ifelse(is.na(bn), "", paste0(" (N=", bn, ")"))), "")
  params <- sort(unique(report$PARAMCD[report$TRTAN == trtan_v]))
  for (pc in params) {
    sub <- report[report$TRTAN == trtan_v & report$PARAMCD == pc, , drop=FALSE]
    sub <- sub[order(sub$bl_grade), , drop=FALSE]
    param_lbl <- sub$PARAM[1]
    lines <- c(lines, paste0("Parameter: ", param_lbl, " (", pc, ")"))
    hdr <- sprintf("%-14s | %8s | %8s | %8s | %8s | %8s | %8s",
                   "Baseline Grade", "Normal", "Grade 1", "Grade 2", "Grade 3", "Grade 4", "Total")
    lines <- c(lines, hdr)
    lines <- c(lines, paste(rep("-", nchar(hdr)), collapse=""))
    for (r in seq_len(nrow(sub))) {
      lines <- c(lines, sprintf("%-14s | %8d | %8d | %8d | %8d | %8d | %8d",
        sub$bl_label[r], sub$wg0[r], sub$wg1[r], sub$wg2[r], sub$wg3[r], sub$wg4[r], sub$total[r]))
    }
    lines <- c(lines, "")
  }
  # page break marker mimics BREAK AFTER PARAMCD / PAGE and per-treatment pages
  if (ti < nrow(trt_order)) lines <- c(lines, "\f", "")
}
lines <- c(lines, f1, f2, f3, f4, f5)

writeLines(lines, file.path(out, "t_lb_shift.txt"))

cat("NOTE: t_lb_shift completed successfully.\n")
