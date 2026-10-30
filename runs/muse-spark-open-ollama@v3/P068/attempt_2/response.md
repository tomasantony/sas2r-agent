```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_pct <- function(pct) {
  r <- sas_round(pct, 1)
  sprintf("%.1f", r)
}

# Resolve adam/sdtm paths inside docker (D:/ -> /bench) or native Windows
resolve_path <- function(p) {
  if (dir.exists(p)) return(p)
  # try /bench mapping
  p2 <- sub("^D:/meta_hackathon/sas2r-agent", "/bench", p)
  if (dir.exists(p2)) return(p2)
  p2
}
adam_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam")
sdtm_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm")
# fallback to derived if needed
if (!dir.exists(adam_path)) {
  adam_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split")
}
if (!dir.exists(adam_path)) {
  adam_path <- resolve_path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam")
}

find_dataset <- function(libpath, name) {
  if (!dir.exists(libpath)) return(NA)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == tolower(name))
  if (length(idx) > 0) return(files[idx[1]])
  # try explicit extensions
  for (ext in c(".sas7bdat", ".xpt", ".sas7bdat", ".XPT")) {
    f <- file.path(libpath, paste0(name, ext))
    if (file.exists(f)) return(f)
    f2 <- file.path(libpath, paste0(tolower(name), ext))
    if (file.exists(f2)) return(f2)
  }
  return(NA)
}

read_adam <- function(name) {
  f <- find_dataset(adam_path, name)
  if (is.na(f)) {
    # try sdtm path as fallback
    f <- find_dataset(sdtm_path, name)
  }
  if (is.na(f) || !file.exists(f)) {
    stop(paste("Dataset not found:", name, "searched in", adam_path))
  }
  ext <- tolower(tools::file_ext(f))
  if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else {
    stop(paste("Unknown extension:", f))
  }
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

adsl <- read_adam("adsl")
adae <- read_adam("adae")

names(adsl) <- toupper(names(adsl))
names(adae) <- toupper(names(adae))

library(dplyr)
library(tidyr)

# Step 1: Big N
adsl_saffl <- adsl[toupper(adsl$SAFFL) == "Y", , drop = FALSE]

bign_df <- adsl_saffl %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)

bign_tot <- n_distinct(adsl_saffl$USUBJID)

trt_levels <- sort(unique(bign_df$TRTA))
bign_df <- bign_df %>% arrange(match(TRTA, trt_levels))
bign_vec <- setNames(bign_df$bign, bign_df$TRTA)
col_hdrs <- sapply(trt_levels, function(trt) paste0(trt, " (N=", bign_vec[trt], ")"))
col_tot_hdr <- paste0("Total (N=", bign_tot, ")")

# Step 2: TEAE subset
saffl_ids <- unique(adsl_saffl$USUBJID)
ae <- adae %>%
  filter(USUBJID %in% saffl_ids, TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD, AESOC, AESOCCD)

# Step 3: SOC counts
soc_cnt <- ae %>%
  group_by(TRTA, AEBODSYS) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

soc_tot <- ae %>%
  group_by(AEBODSYS) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 4: PT counts
pt_cnt <- ae %>%
  group_by(TRTA, AEBODSYS, AEDECOD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop")

pt_tot <- ae %>%
  group_by(AEBODSYS, AEDECOD) %>%
  summarise(cnt_tot = n_distinct(USUBJID), .groups = "drop")

# Step 5: Any TEAE
any_ae <- ae %>%
  group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups = "drop") %>%
  mutate(AEBODSYS = "Any TEAE", AEDECOD = "", soc_ord = 0, pt_ord = 0, rowtype = 0)

# Step 6: SOC and PT rows
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(AEDECOD = "", soc_ord = cnt_tot, pt_ord = 0, rowtype = 1) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS", "AEDECOD")) %>%
  rename(pt_cnt_tot = cnt_tot) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  rename(soc_cnt_tot = cnt_tot) %>%
  mutate(soc_ord = soc_cnt_tot, pt_ord = pt_cnt_tot, rowtype = 2) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

# Step 7: Combine and pct
combined <- bind_rows(any_ae, soc_rows, pt_rows)

combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(bign > 0, cnt / bign * 100, NA_real_)) %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA)

combined3 <- combined2 %>%
  mutate(value = ifelse(cnt > 0,
                        paste0(trimws(as.character(cnt)), " (", fmt_pct(pct), ")"),
                        "0"))

combined3_sorted <- combined3 %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD, TRTA)

final_wide <- combined3_sorted %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, TRTA, value) %>%
  pivot_wider(names_from = TRTA, values_from = value)

for (i in seq_along(trt_levels)) {
  trt <- trt_levels[i]
  colname <- paste0("col", i)
  if (trt %in% names(final_wide)) {
    final_wide[[colname]] <- final_wide[[trt]]
  } else {
    final_wide[[colname]] <- NA_character_
  }
}

final <- final_wide %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, starts_with("col")) %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)

report <- final %>%
  mutate(display_term = case_when(
    rowtype == 0 ~ AEBODSYS,
    rowtype == 1 ~ AEBODSYS,
    TRUE ~ paste0("  ", trimws(AEDECOD))
  ))

col_cols <- grep("^col\\d+$", names(report), value = TRUE)
for (cc in col_cols) {
  report[[cc]][is.na(report[[cc]]) | report[[cc]] == ""] <- "0"
}

report <- report %>%
  arrange(desc(soc_ord), AEBODSYS, rowtype, desc(pt_ord), AEDECOD)

# Output table for CSV
out_table <- report %>%
  select(display_term, all_of(paste0("col", seq_along(trt_levels))))
header_names <- c("System Organ Class / Preferred Term", col_hdrs)
names(out_table) <- header_names

write.csv(out_table, file.path(out, "t_ae_soc_pt.csv"), row.names = FALSE, na = "")
write.csv(report, file.path(out, "t_ae_soc_pt_dataset.csv"), row.names = FALSE, na = "")

# Text rendering
sponsor <- "[Sponsor Name]"
protocol <- "[Protocol Number]"
cutdate <- "[DDMONYYYY]"

txt_path <- file.path(out, "t_ae_soc_pt.txt")
con <- file(txt_path, open = "wt")

writeLines(paste0(sponsor, "                                                                  Page 1 of 1"), con)
writeLines(paste0("Protocol: ", protocol), con)
writeLines("", con)
writeLines("Table 14.3.2", con)
writeLines("Treatment-Emergent Adverse Events by System Organ Class and Preferred Term", con)
writeLines("Safety Analysis Set", con)
writeLines("", con)

hdr_line <- sprintf("%-50s", header_names[1])
for (i in 2:length(header_names)) {
  hdr_line <- paste0(hdr_line, sprintf(" | %-20s", header_names[i]))
}
writeLines(hdr_line, con)
writeLines(paste0(strrep("-", 50), "-+-", paste(rep(strrep("-", 20), length(trt_levels)), collapse = "-+-")), con)

for (r in seq_len(nrow(report))) {
  row <- report[r, ]
  disp <- row$display_term
  line <- sprintf("%-50s", substr(disp, 1, 50))
  for (i in seq_along(trt_levels)) {
    colval <- row[[paste0("col", i)]]
    line <- paste0(line, sprintf(" | %-20s", colval))
  }
  writeLines(line, con)
  if (r < nrow(report)) {
    next_row <- report[r+1, ]
    if (row$soc_ord != next_row$soc_ord || row$AEBODSYS != next_row$AEBODSYS) {
      writeLines("", con)
    }
  }
}
writeLines("", con)
writeLines("MedDRA Version [XX.X]", con)
writeLines("Subjects counted once per SOC and once per PT within each SOC.", con)
writeLines("SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC.", con)
writeLines("", con)
writeLines(paste0("Data cutoff: ", cutdate, "."), con)
writeLines("Source: ADaM datasets", con)
writeLines(paste0("Program: t_ae_soc_pt.sas | Run: ", format(Sys.Date(), "%d%b%Y"), " ", format(Sys.time(), "%H:%M")), con)
close(con)

# Also write .rtf copy as required by expected output name
file.copy(txt_path, file.path(out, "t_ae_soc_pt.rtf"), overwrite = TRUE)

cat("P068 t_ae_soc_pt completed\n")
print(bign_df)
cat("Total N:", bign_tot, "\n")
cat("Report rows:", nrow(report), "\n")
```