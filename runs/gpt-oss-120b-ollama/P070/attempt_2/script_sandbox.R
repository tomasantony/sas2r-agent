# ------------------------------------------------------------
# R translation of SAS program P070_t_cm.sas (attempt 2)
# Generates the Concomitant Medications summary table (t_cm)
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)

# ------------------------------------------------------------
# Helper: round half away from zero (SAS style)
# ------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  pos  <- x >= 0
  res  <- ifelse(pos,
                 floor(x * mult + 0.5) / mult,
                 ceiling(x * mult - 0.5) / mult)
  res
}

# ------------------------------------------------------------
# Helper: read ADaM dataset (tries .sas7bdat then .xpt)
# ------------------------------------------------------------
read_adam <- function(dir, name) {
  # possible extensions
  files <- list.files(dir, pattern = paste0("^", name, "(\\.sas7bdat|\\.xpt)$"), full.names = TRUE, ignore.case = TRUE)
  if (length(files) == 0) stop(paste0("Dataset ", name, " not found in ", dir))
  f <- files[1]
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    read_xpt(f)
  } else {
    read_sas(f)
  }
}

# ------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "" ) stop("OUT_DIR environment variable not set")

adam_path <- "/bench/data/raw/own-synthetic/adam"

# ------------------------------------------------------------
# 1. Read ADaM datasets
# ------------------------------------------------------------
adsl <- read_adam(adam_path, "adsl")
adcm <- read_adam(adam_path, "adcm")

# Ensure column names are as‑is (haven keeps case); we will refer to them exactly.

# ------------------------------------------------------------
# 2. Big N (treatment group counts) – Safety Analysis Set
# ------------------------------------------------------------
bign <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

bigntot <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  summarise(bign = n_distinct(USUBJID)) %>%
  pull(bign)

# Header strings for the three treatment columns (order by TRTA)
col_hdrs <- bign %>%
  mutate(hdr = paste0(TRTA, "~(N=", bign, ")")) %>%
  arrange(TRTA)

# Expect exactly three treatment groups (as per macro variable ntrt=3)
if (nrow(col_hdrs) < 3) stop("Less than 3 treatment groups found")
col1hdr <- col_hdrs$hdr[1]
col2hdr <- col_hdrs$hdr[2]
col3hdr <- col_hdrs$hdr[3]
coltothdr <- paste0("Total~(N=", bigntot, ")")

# ------------------------------------------------------------
# 3. Prepare CM data (Safety set, concomitant flag = Y)
# ------------------------------------------------------------
cm <- adcm %>%
  inner_join(adsl %>% select(USUBJID, SAFFL), by = "USUBJID") %>%
  filter(toupper(SAFFL) == "Y", toupper(CONCOMFL) == "Y") %>%
  select(USUBJID, TRTA, TRTAN, CMCLAS, CMDECOD,
         CMINDC, CMSTDTC, CMENDTC) %>%
  arrange(TRTAN, CMCLAS, CMDECOD)

# ------------------------------------------------------------
# 4. Any Concomitant Medication row (per treatment)
# ------------------------------------------------------------
any_tbl <- cm %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(display_term = "Any Concomitant Medication",
         atc_ord = 0L,
         pref_ord = 0L,
         rowtype = 0L)

# ------------------------------------------------------------
# 5. ATC class counts
# ------------------------------------------------------------
atc <- cm %>%
  group_by(TRTA, CMCLAS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

atc_tot <- cm %>%
  group_by(CMCLAS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

atc_rows <- atc %>%
  left_join(atc_tot, by = "CMCLAS") %>%
  mutate(display_term = CMCLAS,
         atc_ord = cnt_tot,
         pref_ord = 0L,
         rowtype = 1L) %>%
  select(TRTA, CMCLAS, cnt, atc_ord, pref_ord, rowtype, display_term)

# ------------------------------------------------------------
# 6. Preferred name counts
# ------------------------------------------------------------
pref <- cm %>%
  group_by(TRTA, CMCLAS, CMDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pref_tot <- cm %>%
  group_by(CMCLAS, CMDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

pref_rows <- pref %>%
  left_join(pref_tot, by = c("CMCLAS", "CMDECOD")) %>%
  left_join(atc_tot, by = "CMCLAS", suffix = c("_pref", "_atc")) %>%
  mutate(display_term = paste0("  ", CMDECOD),
         atc_ord = cnt_tot_atc,
         pref_ord = cnt_tot_pref,
         rowtype = 2L) %>%
  select(TRTA, CMCLAS, cnt, atc_ord, pref_ord, rowtype, display_term)

# ------------------------------------------------------------
# 7. Combine rows and calculate percentages
# ------------------------------------------------------------
combined <- bind_rows(any_tbl, atc_rows, pref_rows)

combined2 <- combined %>%
  left_join(bign, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_))

combined3 <- combined2 %>%
  mutate(pct_fmt = ifelse(!is.na(pct),
                         sprintf("%.1f", round_half_up(pct, 1)),
                         "")) %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(cnt, " (", pct_fmt, ")"),
                        "0"))

# ------------------------------------------------------------
# 8. Sort and remove duplicate keys (PROC SORT NODUPKEY)
# ------------------------------------------------------------
combined3 <- combined3 %>%
  arrange(desc(atc_ord), CMCLAS, rowtype, desc(pref_ord), display_term, TRTA) %>%
  distinct(atc_ord, CMCLAS, rowtype, pref_ord, display_term, TRTA, .keep_all = TRUE)

# ------------------------------------------------------------
# 9. Transpose to wide format (columns per treatment)
# ------------------------------------------------------------
wide <- combined3 %>%
  select(TRTA, display_term, value, atc_ord, rowtype, pref_ord, CMCLAS) %>%
  pivot_wider(names_from = TRTA,
              values_from = value,
              names_prefix = "col")

# Determine treatment order (alphabetical, same as col_hdrs)
trt_order <- col_hdrs$TRTA

# Rename to col1‑col3 according to that order
wide <- wide %>%
  rename(col1 = !!sym(paste0("col", trt_order[1])),
         col2 = !!sym(paste0("col", trt_order[2])),
         col3 = !!sym(paste0("col", trt_order[3]))) %>%
  select(atc_ord, rowtype, pref_ord, display_term, col1, col2, col3)

# Final dataset name matches SAS output (t_cm)
t_cm <- wide

# ------------------------------------------------------------
# 10. Write outputs
# ------------------------------------------------------------
# CSV of the final table
write.csv(t_cm,
          file.path(out_dir, "t_cm.csv"),
          row.names = FALSE,
          na = "")

# Plain‑text rendering of the table (titles, header, rows, footnotes)
txt_lines <- character()

# Titles (as defined in %set_titles for T_CM)
txt_lines <- c(txt_lines,
               "Table 14.2.1",
               "Summary of Concomitant Medications by ATC Class and Preferred Name",
               "Safety Analysis Set",
               "")

# Header line (using the same spacing as SAS PROC REPORT)
header_fmt <- sprintf("%-45s %12s %12s %12s",
                      "ATC Class  Preferred Name",
                      col1hdr, col2hdr, col3hdr)
txt_lines <- c(txt_lines, header_fmt)

# Table rows
for (i in seq_len(nrow(t_cm))) {
  r <- t_cm[i, ]
  line <- sprintf("%-45s %12s %12s %12s",
                  r$display_term,
                  ifelse(is.na(r$col1), "", r$col1),
                  ifelse(is.na(r$col2), "", r$col2),
                  ifelse(is.na(r$col3), "", r$col3))
  txt_lines <- c(txt_lines, line)
}

# Footnotes (as in the SAS program)
txt_lines <- c(txt_lines, "",
               "WHO Drug Dictionary [Version]",
               "Subjects counted once per ATC class and once per preferred name within ATC class.")

writeLines(txt_lines, file.path(out_dir, "t_cm.txt"))

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
