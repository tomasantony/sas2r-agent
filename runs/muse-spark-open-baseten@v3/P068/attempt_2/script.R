# P068_t_ae_soc_pt.sas -> R translation
# Table 14.3.2 - TEAEs by System Organ Class and Preferred Term

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: SAS round half away from zero (ROUND)
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# Helper: SAS PUT with w.d format (round half away from zero, then format)
sas_put_wd <- function(x, w, d) {
  r <- sas_round(x, d)
  fmt <- paste0("%", w, ".", d, "f")
  sprintf(fmt, r)
}

sas_put_8 <- function(x) {
  trimws(sprintf("%8.0f", sas_round(x, 0)))
}

sas_put_5_1 <- function(x) {
  trimws(sas_put_wd(x, 5, 1))
}

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

adsl_file_sas <- file.path(adam_path, "adsl.sas7bdat")
adsl_file_xpt <- file.path(adam_path, "adsl.xpt")
adae_file_sas <- file.path(adam_path, "adae.sas7bdat")
adae_file_xpt <- file.path(adam_path, "adae.xpt")

read_adam <- function(sas_path, xpt_path) {
  if (file.exists(sas_path)) {
    haven::read_sas(sas_path)
  } else if (file.exists(xpt_path)) {
    haven::read_xpt(xpt_path)
  } else {
    files <- list.files(adam_path, full.names = TRUE)
    stop(paste("File not found:", sas_path, " available:", paste(files, collapse=", ")))
  }
}

adsl <- read_adam(adsl_file_sas, adsl_file_xpt)
adae <- read_adam(adae_file_sas, adae_file_xpt)

adsl <- adsl %>% mutate(across(where(~ inherits(., "haven_labelled")), as.character))
adae <- adae %>% mutate(across(where(~ inherits(., "haven_labelled")), as.character))

names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

stopifnot("USUBJID" %in% names(adsl))
stopifnot("TRTA" %in% names(adsl))
stopifnot("SAFFL" %in% names(adsl))
stopifnot("USUBJID" %in% names(adae))
stopifnot("TRTA" %in% names(adae))
stopifnot("AEBODSYS" %in% names(adae))
stopifnot("AEDECOD" %in% names(adae))
stopifnot("TRTEMFL" %in% names(adae))

# Step 1: Get Big N (m_counts)
bign_df <- adsl %>%
  filter(toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

bign_df <- bign_df %>% mutate(TRTA = trimws(TRTA))
bign_tot <- adsl %>% filter(toupper(SAFFL) == "Y") %>% summarise(n = n_distinct(USUBJID)) %>% pull(n)
bign_lookup <- setNames(bign_df$bign, bign_df$TRTA)

# Step 2: Subset to TEAE in safety population
safety_usubjid <- adsl %>% filter(SAFFL == "Y") %>% pull(USUBJID) %>% unique()
ae <- adae %>%
  filter(USUBJID %in% safety_usubjid, TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD, AESOC, AESOCCD) %>%
  mutate(
    AEBODSYS = trimws(as.character(AEBODSYS)),
    AEDECOD = trimws(as.character(AEDECOD)),
    TRTA = trimws(as.character(TRTA))
  ) %>%
  filter(!is.na(AEBODSYS), AEBODSYS != "")

# Step 3: Count subjects by SOC
soc_cnt <- ae %>%
  group_by(TRTA, AEBODSYS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

soc_tot <- ae %>%
  group_by(AEBODSYS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 4: Count subjects by SOC + PT
pt_cnt <- ae %>%
  group_by(TRTA, AEBODSYS, AEDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pt_tot <- ae %>%
  group_by(AEBODSYS, AEDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 5: Any TEAE row
any_ae <- ae %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(
    AEBODSYS = "Any TEAE",
    AEDECOD = "",
    soc_ord = 0,
    pt_ord = 0,
    rowtype = 0
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# Step 6: Build display rows
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(
    AEDECOD = "",
    soc_ord = cnt_tot,
    pt_ord = 0,
    rowtype = 1
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS", "AEDECOD")) %>%
  rename(pt_cnt_tot = cnt_tot) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  rename(soc_cnt_tot = cnt_tot) %>%
  mutate(
    soc_ord = soc_cnt_tot,
    pt_ord = pt_cnt_tot,
    rowtype = 2
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# Step 7: Combine and calculate percentages
combined <- bind_rows(any_ae, soc_rows, pt_rows)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(
    pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)
  ) %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA)

combined3 <- combined2 %>%
  mutate(
    value = ifelse(cnt > 0,
                   paste0(trimws(sas_put_8(cnt)), " (", trimws(sas_put_5_1(pct)), ")"),
                   "0")
  )

# Step 8: Transpose
combined3_sorted <- combined3 %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA) %>%
  distinct(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, TRTA, .keep_all = TRUE)

detect_col <- function(trta_name) {
  tn <- tolower(trta_name)
  if (grepl("placebo", tn)) return("col1")
  if (grepl("low", tn)) return("col2")
  if (grepl("high", tn)) return("col3")
  return(NA)
}

wide <- combined3_sorted %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value, values_fill = "")

rename_vec <- sapply(names(wide), function(nm) {
  if (nm %in% c("soc_ord", "AEBODSYS", "rowtype", "pt_ord", "AEDECOD")) return(nm)
  col <- detect_col(nm)
  if (!is.na(col)) return(col)
  return(nm)
})
names(wide) <- rename_vec

for (c in c("col1","col2","col3")) {
  if (!c %in% names(wide)) wide[[c]] <- ""
}

final <- wide %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, col1, col2, col3) %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)

# Step 9: Create display variable
report <- final %>%
  mutate(
    display_term = case_when(
      rowtype == 0 ~ AEBODSYS,
      rowtype == 1 ~ AEBODSYS,
      TRUE ~ paste0("  ", trimws(AEDECOD))
    )
  ) %>%
  mutate(
    col1 = ifelse(is.na(col1) | col1 == "", "0", col1),
    col2 = ifelse(is.na(col2) | col2 == "", "0", col2),
    col3 = ifelse(is.na(col3) | col3 == "", "0", col3)
  )

# Build ordered report: Any TEAE first, then SOCs sorted by total desc, each SOC followed by its PTs
any_row <- report %>% filter(rowtype == 0)
soc_pt_rows <- report %>% filter(rowtype != 0)
ordered_soc_pt <- soc_pt_rows %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)
report_ordered <- bind_rows(any_row, ordered_soc_pt)

# Headers
header_lookup <- list()
for (i in seq_len(nrow(bign_df))) {
  trta_val <- bign_df$TRTA[i]
  n_val <- bign_df$bign[i]
  col_name <- detect_col(trta_val)
  if (!is.na(col_name)) {
    header_lookup[[col_name]] <- paste0(trta_val, " (N=", n_val, ")")
  }
}
for (c in c("col1","col2","col3")) {
  if (is.null(header_lookup[[c]])) {
    idx <- as.integer(substr(c, 4, 4))
    if (idx <= nrow(bign_df)) {
      header_lookup[[c]] <- paste0(bign_df$TRTA[idx], " (N=", bign_df$bign[idx], ")")
    } else {
      header_lookup[[c]] <- c
    }
  }
}
col1hdr <- header_lookup[["col1"]]
col2hdr <- header_lookup[["col2"]]
col3hdr <- header_lookup[["col3"]]
coltothdr <- paste0("Total (N=", bign_tot, ")")

# Output datasets
output_df <- report_ordered %>%
  select(display_term, col1, col2, col3, soc_ord, pt_ord, rowtype, AEBODSYS, AEDECOD)

write.csv(output_df %>% select(display_term, col1, col2, col3),
          file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")

write.csv(report_ordered,
          file.path(out, "t_ae_soc_pt_detail.csv"), row.names = FALSE, na = "")

write.csv(bign_df, file.path(out, "_bign.csv"), row.names = FALSE, na = "")

# Text rendering
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

txt_path <- file.path(out, "t_ae_soc_pt.txt")
con <- file(txt_path, "w")

writeLines(paste0(sponsor, strrep(" ", 60), "Page 1 of 1"), con)
writeLines(paste0("Protocol: ", protocol), con)
writeLines("", con)
writeLines("Table 14.3.2", con)
writeLines("Treatment-Emergent Adverse Events by System Organ Class and Preferred Term", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)
header_line <- sprintf("%-50s %-20s %-20s %-20s", "System Organ Class", col1hdr, col2hdr, col3hdr)
writeLines(header_line, con)
writeLines(paste0(strrep("-", 50), " ", strrep("-", 20), " ", strrep("-", 20), " ", strrep("-", 20)), con)
writeLines(sprintf("%-50s %-20s %-20s %-20s", "  Preferred Term", "", "", ""), con)
writeLines(strrep("-", 115), con)

for (i in seq_len(nrow(report_ordered))) {
  row <- report_ordered[i, ]
  term <- row$display_term
  term_disp <- sprintf("%-50s", substr(term, 1, 50))
  line <- sprintf("%-50s %-20s %-20s %-20s", term_disp, row$col1, row$col2, row$col3)
  writeLines(line, con)
  if (row$rowtype != 0) {
    if (i < nrow(report_ordered)) {
      next_ae <- report_ordered$AEBODSYS[i+1]
      curr_ae <- row$AEBODSYS
      if (next_ae != curr_ae) {
        writeLines("", con)
      }
    }
  } else {
    writeLines("", con)
  }
}

writeLines("", con)
writeLines(strrep("-", 115), con)
writeLines(paste0("Data cutoff: ", cutdate, "."), con)
writeLines("Source: ADaM datasets", con)
writeLines(paste0("Program: t_ae_soc_pt.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M")), con)
writeLines("MedDRA Version [XX.X]", con)
writeLines("Subjects counted once per SOC and once per PT within each SOC.", con)
writeLines("SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC.", con)

close(con)

cat("P068 t_ae_soc_pt completed\n")
cat(paste0("Big N: ", paste(bign_df$TRTA, bign_df$bign, collapse=", "), " Total=", bign_tot, "\n"))
cat(paste0("Rows in report: ", nrow(report_ordered), "\n"))
cat(paste0("Columns: ", col1hdr, " | ", col2hdr, " | ", col3hdr, "\n"))
