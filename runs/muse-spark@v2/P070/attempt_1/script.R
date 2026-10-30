# P070 t_cm - Summary of Concomitant Medications by ATC Class and Preferred Name
# Standalone R translation of P070_t_cm.sas
suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-10) / 10^digits)
}
fmt_pct1 <- function(x) {
  r <- round_half_up(x, 1)
  sprintf("%.1f", r)
}

find_dataset <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) stop(paste0("no files in ", dir))
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(name))
  if (length(idx) == 0) stop(paste0("dataset ", name, " not found in ", dir,
    " files: ", paste(basename(files), collapse=",")))
  files[idx[1]]
}
read_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    df <- haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(path)
  } else if (ext %in% c("xls","xlsx")) {
    df <- readxl::read_excel(path)
  } else if (ext == "csv") {
    df <- readr::read_csv(path, show_col_types = FALSE)
  } else {
    stop(paste0("unknown ext: ", path))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # strip haven labels/formats
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "labelled")) {
      # zap labels
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      # haven may return labelled still as class; coerce
      if (is.character(v) || is.numeric(v) || is.integer(v)) df[[nm]] <- v
      else df[[nm]] <- as.character(v)
    }
    if (inherits(v, "labelled_spss")) df[[nm]] <- as.character(v)
  }
  names(df) <- toupper(names(df))
  df
}

# Try C collation for SAS-like ordering (uppercase before lowercase)
try(suppressWarnings(Sys.setlocale("LC_COLLATE", "C")), silent = TRUE)

adsl_path <- find_dataset(adam_path, "adsl")
adcm_path <- find_dataset(adam_path, "adcm")
adsl <- read_dataset(adsl_path)
adcm <- read_dataset(adcm_path)

# Ensure required columns exist
stopifnot(all(c("USUBJID","TRTA","TRTAN","SAFFL") %in% names(adsl)))
# ADCM may or may not have TRTA/TRTAN; check
has_adcm_trta <- all(c("TRTA","TRTAN") %in% names(adcm))
stopifnot(all(c("USUBJID","CMCLAS","CMDECOD","CONCOMFL") %in% names(adcm)))

# Normalise character columns to character
to_chr <- function(x) {
  if (is.factor(x)) as.character(x) else if (is.character(x)) x else as.character(x)
}
adsl$USUBJID <- to_chr(adsl$USUBJID)
adsl$TRTA <- to_chr(adsl$TRTA)
adsl$TRTAN <- suppressWarnings(as.numeric(adsl$TRTAN))
adsl$SAFFL <- to_chr(adsl$SAFFL)
adcm$USUBJID <- to_chr(adcm$USUBJID)
adcm$CMCLAS <- to_chr(adcm$CMCLAS)
adcm$CMDECOD <- to_chr(adcm$CMDECOD)
adcm$CONCOMFL <- to_chr(adcm$CONCOMFL)
if ("TRTA" %in% names(adcm)) adcm$TRTA <- to_chr(adcm$TRTA)
if ("TRTAN" %in% names(adcm)) adcm$TRTAN <- suppressWarnings(as.numeric(adcm$TRTAN))

# --- Step 1: Big N (m_counts: upcase(SAFFL)='Y', group by TRTA order by TRTA) ---
adsl_saf <- adsl %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y")
# distinct subjects per TRTA
bign_df <- adsl_saf %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop")
# SAS sort: character ordering uppercase before lowercase, missing first
# Use base order with C locale; missing first
bign_df <- bign_df[order(bign_df$TRTA, na.last = FALSE), , drop = FALSE]
bigntot <- n_distinct(adsl_saf$USUBJID)

# Headers in sorted-TRTA order (SAS semantics)
hdr_sorted_trta <- bign_df$TRTA
hdr_sorted_bign <- bign_df$bign
hdr_sorted <- paste0(hdr_sorted_trta, " (N=", hdr_sorted_bign, ")")
# Ensure length 3 (pad if needed)
# ntrt
ntrt <- nrow(bign_df)

# Treatment order for data columns: TRTAN order (Placebo, Low, High)
trt_map_trtan <- adsl_saf %>%
  select(TRTA, TRTAN) %>%
  distinct() %>%
  arrange(TRTAN, TRTA)
# If ADCM TRTA differs, use ADCM distinct joined? Use TRTAN order from ADSL
data_col_trta_in_trtan_order <- trt_map_trtan$TRTA
# Fallback if ntrt !=3, still handle generically
# For this study expect 3
# Map col1..col3 data TRTA:
# col1 = first in TRTAN order, etc.
col_data_trta <- data_col_trta_in_trtan_order

# --- Step 2: Prepare CM data (inner join, b.SAFFL='Y' case-sensitive, a.CONCOMFL='Y') ---
adsl_keys <- adsl %>% filter(!is.na(SAFFL) & SAFFL == "Y") %>% select(USUBJID) %>% distinct()
# inner join
cm0 <- adcm %>%
  filter(!is.na(CONCOMFL) & CONCOMFL == "Y") %>%
  inner_join(adsl_keys, by = "USUBJID")
# Select TRTA/TRTAN from ADCM if present else from ADSL
if (has_adcm_trta) {
  cm <- cm0 %>% select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD)
} else {
  tmp <- adsl %>% select(USUBJID, TRTA, TRTAN)
  # drop overlapping except keys before merge (avoid .x/.y)
  cm0_nodup <- cm0 %>% select(USUBJID, CMCLAS, CMDECOD)
  cm <- cm0_nodup %>% left_join(tmp, by = "USUBJID") %>%
    select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD)
}
# Order by TRTAN, CMCLAS, CMDECOD (SAS order by TRTAN...)
cm <- cm[order(cm$TRTAN, cm$CMCLAS, cm$CMDECOD, na.last = FALSE), , drop=FALSE]

# --- Step 3: Any CM row ---
any_df <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0L, pref_ord = 0L, rowtype = 0L,
         CMCLAS = NA_character_, CMDECOD = NA_character_)

# --- Step 4: ATC class counts ---
atc_df <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")
atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")
# sort by CMCLAS for merge (SAS patch)
atc_df <- atc_df[order(atc_df$CMCLAS, na.last=FALSE), , drop=FALSE]
atc_tot_s <- atc_tot[order(atc_tot$CMCLAS, na.last=FALSE), , drop=FALSE]
atc_rows <- atc_df %>%
  left_join(atc_tot_s, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0L,
         rowtype = 1L,
         CMDECOD = NA_character_) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)

# --- Step 5: Preferred name counts ---
pref_df <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")
pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop") %>%
  rename(pref_ord_tmp = cnt_tot)
pref_rows <- pref_df %>%
  left_join(pref_tot, by = c("CMCLAS","CMDECOD")) %>%
  left_join(atc_tot %>% rename(atc_ord_tmp = cnt_tot), by = "CMCLAS") %>%
  mutate(display_term = paste0("  ", trimws(CMDECOD)),
         atc_ord = atc_ord_tmp,
         pref_ord = pref_ord_tmp,
         rowtype = 2L) %>%
  select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype)

# --- Step 6: Combine and percentages ---
combined <- bind_rows(
  any_df %>% select(TRTA, CMCLAS, CMDECOD, cnt, display_term, atc_ord, pref_ord, rowtype),
  atc_rows,
  pref_rows
)
# left join bign
combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_))
# order by atc_ord desc, CMCLAS, rowtype, pref_ord desc, display_term, TRTA
# Use base order to control NA first and desc
ord <- order(-combined2$atc_ord,
             combined2$CMCLAS,
             combined2$rowtype,
             -combined2$pref_ord,
             combined2$display_term,
             combined2$TRTA,
             na.last = FALSE)
combined2 <- combined2[ord, , drop = FALSE]

combined3 <- combined2 %>%
  mutate(value = ifelse(!is.na(cnt) & cnt > 0,
                        paste0(trimws(as.character(cnt)), " (", fmt_pct1(pct), ")"),
                        ifelse(!is.na(cnt) & cnt == 0, "0", NA_character_)))

# --- Step 7: Transpose (BY atc_ord CMCLAS rowtype pref_ord display_term, ID TRTA, VAR value) ---
# nodupkey sort by descending atc_ord CMCLAS rowtype descending pref_ord display_term TRTA
combined3_s <- combined3 %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA)
# pivot wider
# Create BY keys
wide <- combined3_s %>%
  select(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, value) %>%
  tidyr::pivot_wider(names_from = TRTA, values_from = value, values_fn = function(x) x[1])

# Rename TRTA columns to col1..col3 per TRTAN order mapping + SAS patch mapping
# SAS patch: colPlacebo=col1, colDrug_X_Low_Dose=col2, colDrug_X_High_Dose=col3
# Generalise: col for TRTAN=1 -> col1, TRTAN=2 -> col2, TRTAN=3 -> col3
# Identify actual TRTA strings in TRTAN order
# Ensure all expected columns exist; if missing treatment for a row -> NA remains
for (i in seq_along(col_data_trta)) {
  trt_val <- col_data_trta[i]
  newnm <- paste0("col", i)
  if (trt_val %in% names(wide)) {
    names(wide)[names(wide) == trt_val] <- newnm
  } else {
    wide[[newnm]] <- NA_character_
  }
}
# Keep only expected cols plus BY vars; drop any extra TRTA columns (if more than 3?)
keep_cols <- c("atc_ord","CMCLAS","rowtype","pref_ord","display_term",
               paste0("col", seq_along(col_data_trta)))
# If ntrt==3, keep col1..col3
extra <- setdiff(names(wide), keep_cols)
if (length(extra) > 0) {
  # drop extras (should not happen)
  wide <- wide %>% select(-all_of(extra))
}
# Ensure col1..col3 exist
for (i in seq_along(col_data_trta)) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide)) wide[[cn]] <- NA_character_
}
# Order rows by atc_ord desc, CMCLAS, rowtype, pref_ord desc, display_term
wide <- wide[order(-wide$atc_ord, wide$CMCLAS, wide$rowtype, -wide$pref_ord, wide$display_term, na.last=FALSE), , drop=FALSE]
final <- wide

# Replace NA display cells with "" for report (SAS blank for missing)
for (cn in paste0("col", seq_along(col_data_trta))) {
  final[[cn]][is.na(final[[cn]])] <- ""
}

# Display headers: SAS bug replication - headers in sorted-TRTA order assigned to col1..col3
# hdr_sorted[1] -> col1, etc.
disp_hdr <- hdr_sorted
# If lengths mismatch (e.g., TRTAN order differs), still assign sorted headers to col1..col3 in order
# Build header labels for report
col_hdrs <- disp_hdr[seq_along(col_data_trta)]

# For CSV column names, use headers (contain N= numbers)
csv_df <- final %>% select(display_term, starts_with("col"))
# Rename col columns to header strings for CSV to preserve numbers in header?
# Keep simple names but also include N in separate header row? Instead set names to headers.
hdr_names <- c("ATC Class / Preferred Name", col_hdrs)
names(csv_df) <- hdr_names
# Write CSV of table body
write.csv(csv_df, file.path(out, "t_cm.csv"), row.names = FALSE, na = "")

# --- Step 8: Text rendering ---
# Titles/footnotes verbatim
t1 <- "[Sponsor Name]                                                              Page 1 of 1"
t2 <- "Protocol: [Protocol Number]"
t3 <- ""
t4 <- "Table 14.2.1"
t5 <- "Summary of Concomitant Medications by ATC Class and Preferred Name"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
# Use fixed run date text (reproduce formula text generically)
f3 <- "Program: t_cm.sas"
f4 <- "WHO Drug Dictionary [Version]"
f5 <- "Subjects counted once per ATC class and once per preferred name within ATC class."

# Build text table
# Column widths
w_term <- 60
w_col <- 22
header_line1 <- sprintf(paste0("%-", w_term, "s%-", w_col, "s%-", w_col, "s%-", w_col, "s"),
                        "ATC Class", col_hdrs[1], col_hdrs[2], col_hdrs[3])
header_line2 <- sprintf(paste0("%-", w_term, "s%-", w_col, "s%-", w_col, "s%-", w_col, "s"),
                        "  Preferred Name", "", "", "")
sep <- paste(rep("-", w_term + w_col*3), collapse="")

con <- file(file.path(out, "t_cm.txt"), open = "wt")
writeLines(t1, con)
writeLines(t2, con)
if (t3 != "") writeLines(t3, con)
writeLines(t4, con)
writeLines(t5, con)
writeLines(t6, con)
writeLines("", con)
writeLines(header_line1, con)
writeLines(header_line2, con)
writeLines(sep, con)
for (r in seq_len(nrow(final))) {
  term <- final$display_term[r]
  # Truncate to width like SAS? keep full but pad/truncate
  term_show <- ifelse(nchar(term) > w_term, substr(term, 1, w_term), term)
  c1 <- final$col1[r]; c2 <- final$col2[r]; c3 <- final$col3[r]
  line <- sprintf(paste0("%-", w_term, "s%-", w_col, "s%-", w_col, "s%-", w_col, "s"),
                  term_show, c1, c2, c3)
  writeLines(line, con)
}
writeLines(sep, con)
writeLines("", con)
writeLines(f1, con)
writeLines(f2, con)
writeLines(f3, con)
writeLines(f4, con)
writeLines(f5, con)
close(con)

# Console progress only
message("t_cm completed: rows=", nrow(final), " cols=", length(col_data_trta))
