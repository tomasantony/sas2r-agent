out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(tibble)
})

# C collation => SAS binary order (uppercase before lowercase), missing first
try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE)

round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  s <- sign(x)
  a <- abs(x)
  r <- floor(a * mult + 0.5 + 1e-9) / mult
  out <- s * r
  out[x == 0] <- 0
  out
}

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  idx <- grep(paste0("^", name, "\\.(xpt|sas7bdat)$"), basename(files), ignore.case = TRUE)
  if (length(idx) == 0) stop(paste0("file not found: ", name, " in ", dir))
  files[idx[1]]
}
read_sas_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") haven::read_xpt(path) else haven::read_sas(path)
}

adsl_path <- find_file(adam_dir, "adsl")
adae_path <- find_file(adam_dir, "adae")
adsl_raw <- read_sas_file(adsl_path)
adae_raw <- read_sas_file(adae_path)
names(adsl_raw) <- toupper(names(adsl_raw))
names(adae_raw) <- toupper(names(adae_raw))

stopifnot(all(c("USUBJID","TRTA","SAFFL") %in% names(adsl_raw)))
stopifnot(all(c("USUBJID","TRTA","AEBODSYS","AEDECOD","TRTEMFL") %in% names(adae_raw)))

adsl <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
adae <- as.data.frame(adae_raw, stringsAsFactors = FALSE)

# Step 1: Big N (m_counts: upcase(SAFFL)="Y", group by TRTA, order by TRTA)
adsl_saf <- adsl %>% filter(!is.na(SAFFL) & toupper(SAFFL) == "Y")
bign_df <- adsl_saf %>%
  group_by(TRTA) %>%
  summarise(bign = n_distinct(USUBJID), .groups = "drop") %>%
  arrange(TRTA)
# enforce SAS C-locale order explicitly
trt_order <- sort(unique(bign_df$TRTA), na.last = FALSE)
# reorder bign_df by trt_order
bign_df <- bign_df %>% arrange(match(TRTA, trt_order))
bigntot <- n_distinct(adsl_saf$USUBJID)
ntrt <- nrow(bign_df)
message("Big N: ", paste(paste0(bign_df$TRTA, "=", bign_df$bign), collapse=", "), " Total=", bigntot)

# Step 2: TEAE in safety population (inner join, b.SAFFL='Y' exact, a.TRTEMFL='Y' exact)
saf_ids <- adsl %>% filter(!is.na(SAFFL) & SAFFL == "Y") %>% select(USUBJID) %>% distinct()
ae <- adae %>%
  inner_join(saf_ids, by = "USUBJID") %>%
  filter(!is.na(TRTEMFL) & TRTEMFL == "Y") %>%
  select(USUBJID, TRTA, AEBODSYS, AEDECOD) %>%
  filter(!is.na(USUBJID))
stopifnot(all(c("USUBJID","TRTA","AEBODSYS","AEDECOD") %in% names(ae)))
stopifnot(nrow(ae) > 0)

# Step 3: SOC counts
soc_cnt <- ae %>% group_by(TRTA, AEBODSYS) %>% summarise(cnt = n_distinct(USUBJID), .groups="drop")
soc_tot <- ae %>% group_by(AEBODSYS) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups="drop")

# Step 4: PT counts
pt_cnt <- ae %>% group_by(TRTA, AEBODSYS, AEDECOD) %>% summarise(cnt = n_distinct(USUBJID), .groups="drop")
pt_tot <- ae %>% group_by(AEBODSYS, AEDECOD) %>% summarise(cnt_tot = n_distinct(USUBJID), .groups="drop")

# Step 5: Any TEAE row (rowtype=1 as coded, soc_ord=0)
any_ae <- ae %>% group_by(TRTA) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(AEBODSYS = "Any TEAE", AEDECOD = "", soc_ord = 0, pt_ord = 0, rowtype = 1)

# Step 6: SOC + PT rows with sort order
soc_rows <- soc_cnt %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  mutate(AEDECOD = "", soc_ord = cnt_tot, pt_ord = 0, rowtype = 1) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

pt_rows <- pt_cnt %>%
  left_join(pt_tot, by = c("AEBODSYS","AEDECOD")) %>%
  rename(pt_ord = cnt_tot) %>%
  left_join(soc_tot, by = "AEBODSYS") %>%
  rename(soc_ord = cnt_tot) %>%
  mutate(rowtype = 2) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype)

combined <- bind_rows(any_ae %>% select(TRTA,AEBODSYS,AEDECOD,cnt,soc_ord,pt_ord,rowtype),
                     soc_rows, pt_rows)

# Step 7: percentages
combined2 <- combined %>%
  left_join(bign_df, by = "TRTA") %>%
  mutate(pct = ifelse(!is.na(bign) & bign > 0, cnt / bign * 100, NA_real_))

combined3 <- combined2 %>%
  mutate(
    pct_r = round_half_up(pct, 1),
    pct_s = sprintf("%.1f", pct_r),
    value = ifelse(!is.na(cnt) & cnt > 0, paste0(trimws(as.character(cnt)), " (", trimws(pct_s), ")"), "0")
  ) %>%
  select(TRTA, AEBODSYS, AEDECOD, cnt, soc_ord, pt_ord, rowtype, bign, pct, value)

# SAS sort nodupkey: by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD TRTA
ord_idx <- order(-combined3$soc_ord, combined3$AEBODSYS, combined3$rowtype,
                 -combined3$pt_ord, combined3$AEDECOD, combined3$TRTA,
                 na.last = FALSE, method = "radix")
combined3 <- combined3[ord_idx, , drop = FALSE]

# Step 8: Transpose (pivot_wider), col1..colN in TRTA sorted order
combined3 <- combined3 %>%
  mutate(colname = paste0("col", match(TRTA, trt_order)))
wide <- combined3 %>%
  select(soc_ord, AEBODSYS, rowtype, pt_ord, AEDECOD, colname, value) %>%
  pivot_wider(names_from = colname, values_from = value, values_fn = function(x) x[1])

# ensure col1..colN exist
for (i in seq_len(ntrt)) {
  cn <- paste0("col", i)
  if (!cn %in% names(wide)) wide[[cn]] <- "0"
}
wide[is.na(wide)] <- "0"
# replace empty strings with 0 (SAS blank -> 0)
for (cn in paste0("col", seq_len(ntrt))) {
  wide[[cn]][wide[[cn]] == ""] <- "0"
}

# Step 9: display variable, sort as BY
report <- wide %>%
  mutate(display_term = ifelse(rowtype %in% c(0,1), AEBODSYS, paste0("  ", trimws(AEDECOD))))
# order by descending soc_ord AEBODSYS rowtype descending pt_ord AEDECOD
report <- report[order(-report$soc_ord, report$AEBODSYS, report$rowtype,
                       -report$pt_ord, report$AEDECOD, na.last = FALSE, method = "radix"), , drop = FALSE]

# Column headers
hdrs <- sapply(seq_len(ntrt), function(i) paste0(trt_order[i], " (N=", bign_df$bign[match(trt_order[i], bign_df$TRTA)], ")"))
# pad to 3 for display (program expects 3)
while (length(hdrs) < 3) hdrs <- c(hdrs, paste0("Col", length(hdrs)+1, " (N=0)"))

# Titles / footnotes verbatim
t1 <- "[Sponsor Name]                                                  Page ^{pageof}"
t2 <- "Protocol: [Protocol Number]"
t3 <- " "
t4 <- "Table 14.3.2"
t5 <- "Treatment-Emergent Adverse Events by System Organ Class and Preferred Term"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]"
f2 <- "Source: ADaM datasets"
f3 <- "Program: P068_t_ae_soc_pt.sas | Run: generated by R"
f4 <- "MedDRA Version [XX.X]"
f5 <- "Subjects counted once per SOC and once per PT within each SOC."
f6 <- "SOCs sorted by descending frequency of Total column; PTs sorted by descending frequency within SOC."

# Build text rendering with blank line between SOCs (compute after soc_ord)
# Determine display widths
col_term_w <- 60
col_w <- 22
header_term <- "System Organ Class / Preferred Term"
# header labels split on ~ in SAS: use full hdrs
lines <- c(t1, t2, t3, t4, t5, t6, "",
           paste0(strrep(" ", col_term_w), " | ", paste(sprintf(paste0("%-", col_w, "s"), hdrs[1:min(3, ntrt)]), collapse=" | ")),
           paste0(strrep("-", col_term_w), "-+-", paste(rep(strrep("-", col_w), min(3, ntrt)), collapse="-+-")))

# iterate rows, add blank line after each soc_ord group
prev_soc <- NA
for (r in seq_len(nrow(report))) {
  term <- report$display_term[r]
  # truncate/pad for display but keep full in CSV
  term_disp <- if (nchar(term) > col_term_w) substr(term, 1, col_term_w) else sprintf(paste0("%-", col_term_w, "s"), term)
  vals <- sapply(paste0("col", 1:min(3, ntrt)), function(cn) sprintf(paste0("%-", col_w, "s"), report[[cn]][r]))
  lines <- c(lines, paste0(term_disp, " | ", paste(vals, collapse=" | ")))
  # blank line between SOCs: after last row of a soc_ord group -> look ahead
  cur_soc <- report$soc_ord[r]
  nxt_soc <- if (r < nrow(report)) report$soc_ord[r+1] else NA
  if (!is.na(cur_soc) && (is.na(nxt_soc) || nxt_soc != cur_soc)) {
    lines <- c(lines, "")
  }
}
lines <- c(lines, "", f1, f2, f3, f4, f5, f6)

txt_path <- file.path(out, "t_ae_soc_pt.txt")
writeLines(lines, txt_path)

# CSV of table body (same numbers)
csv_df <- report %>% select(display_term, col1, col2, col3, soc_ord, rowtype, pt_ord, AEBODSYS, AEDECOD)
# rename col headers to include N for traceability? keep col1..col3 plus header mapping in separate? Keep body as displayed.
# For clarity, set names to header strings sanitized? Keep col1..col3 per SAS report.
csv_body <- report %>% select(display_term, col1, col2, col3)
names(csv_body) <- c("display_term", hdrs[1:3])
csv_path <- file.path(out, "t_ae_soc_pt.csv")
write.csv(csv_body, csv_path, row.names = FALSE, na = "")

message("Wrote ", txt_path, " rows=", nrow(report))
message("Wrote ", csv_path)
