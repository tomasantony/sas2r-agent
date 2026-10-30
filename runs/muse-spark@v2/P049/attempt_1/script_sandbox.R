out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
})

round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-9) / mult
  y * s
}

cat("Reading ADSL\n")
p1 <- "/bench/data/raw/cdisc-pilot/adam/adsl.xpt"
p2 <- "/bench/data/derived/cdisc-pilot/adam/adsl.sas7bdat"
p3 <- "/bench/data/derived/cdisc-pilot/adam/adsl.xpt"
adsl_path <- NULL
for (p in c(p1, p2, p3)) {
  if (file.exists(p)) { adsl_path <- p; break }
}
if (is.null(adsl_path)) adsl_path <- p1
cat(paste0("Using ADSL: ", adsl_path, "\n"))
if (grepl("\\.xpt$", adsl_path, ignore.case = TRUE)) {
  adsl_raw <- haven::read_xpt(adsl_path)
} else {
  adsl_raw <- haven::read_sas(adsl_path)
}
names(adsl_raw) <- toupper(names(adsl_raw))
stopifnot(all(c("USUBJID","TRT01P","TRT01PN","DCREASCD","ITTFL") %in% names(adsl_raw)))
adsl <- adsl_raw
# ensure types
adsl$TRT01PN <- as.numeric(adsl$TRT01PN)
adsl$TRT01P <- as.character(adsl$TRT01P)
adsl$DCREASCD <- as.character(adsl$DCREASCD)
adsl$ITTFL <- as.character(adsl$ITTFL)
adsl$USUBJID <- as.character(adsl$USUBJID)

# --- PROC SQL create table disp ---
adsl_itt <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")
stopifnot(all(c("TRT01P","TRT01PN","DCREASCD","USUBJID") %in% names(adsl_itt)))

disp_part1 <- adsl_itt %>%
  mutate(STATUS = ifelse(!is.na(DCREASCD) & DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, STATUS) %>%
  summarise(cnt = sum(!is.na(USUBJID)), .groups = "drop") %>%
  rename(status = STATUS) %>%
  mutate(ord = 1L)

disp_part2 <- adsl_itt %>%
  filter(is.na(DCREASCD) | DCREASCD != "Completed") %>%
  group_by(TRT01P, TRT01PN, DCREASCD) %>%
  summarise(cnt = sum(!is.na(USUBJID)), .groups = "drop") %>%
  rename(status = DCREASCD) %>%
  mutate(ord = 2L)

disp <- bind_rows(disp_part1, disp_part2)
# order by TRT01PN, ord, status (SAS binary order, use radix)
disp <- disp %>% slice(order(TRT01PN, ord, status, method = "radix"))

# --- create table freq (no ITT filter, per SAS) ---
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = sum(!is.na(USUBJID)), .groups = "drop")
freq <- freq %>% slice(order(TRT01PN, method = "radix"))

# --- data disp_2; merge disp freq; by TRT01PN ---
# drop/rename overlapping non-key cols: only TRT01PN is key, no overlap otherwise
stopifnot("TRT01PN" %in% names(disp), "TRT01PN" %in% names(freq))
stopifnot("TRT01P" %in% names(disp), "cnt" %in% names(disp))
disp_2 <- left_join(disp, freq, by = "TRT01PN")
stopifnot(all(c("TRT01P","TRT01PN","cnt","ord","status","tot_cnt") %in% names(disp_2)))
stopifnot(all(!is.na(disp_2$tot_cnt)))

disp_2 <- disp_2 %>%
  mutate(
    treat = paste0(trimws(TRT01P), " (N=", trimws(as.character(tot_cnt)), ")"),
    pct_raw = cnt / tot_cnt * 100,
    pct_r = round_half_up(pct_raw, 1),
    pct_s = sprintf("%.1f%%", pct_r),
    cnt_prcnt = paste0(trimws(as.character(cnt)), " (", trimws(pct_s), ")")
  ) %>%
  select(TRT01P, TRT01PN, cnt, ord, status, tot_cnt, treat, cnt_prcnt)

# proc sort by status ord
disp_2_s <- disp_2 %>% slice(order(status, ord, method = "radix"))

# proc transpose by status ord; id TRT01PN; var cnt_prcnt
trt_order <- sort(unique(disp_2_s$TRT01PN))
treat_map <- disp_2_s %>% distinct(TRT01PN, treat) %>% arrange(TRT01PN)
# ensure treat consistent per TRT01PN
trans_wide <- disp_2_s %>%
  select(status, ord, TRT01PN, cnt_prcnt) %>%
  pivot_wider(id_cols = c(status, ord), names_from = TRT01PN, values_from = cnt_prcnt, names_prefix = "tr")

# ensure all tr columns present in order
expected_cols <- paste0("tr", trt_order)
for (cc in expected_cols) {
  if (!cc %in% names(trans_wide)) trans_wide[[cc]] <- NA_character_
}
trans_wide <- trans_wide %>% select(status, ord, all_of(expected_cols))

# proc sort by ord status
trans_2 <- trans_wide %>% slice(order(ord, status, method = "radix"))
trans_2 <- as.data.frame(trans_2, stringsAsFactors = FALSE)

cat("Transposed rows:\n")
cat(paste0(nrow(trans_2), " rows, ", ncol(trans_2), " cols\n"))

# Build display table (PROC REPORT columns ord status tr:)
# ord noprint, status grouped, tr: display, break after ord/skip
treat_labels <- sapply(trt_order, function(v) {
  x <- treat_map$treat[treat_map$TRT01PN == v]
  if (length(x) == 0) paste0("tr", v) else x[1]
})
names(treat_labels) <- expected_cols

col1_header <- "Subject Disposition"
# column widths for text rendering (spacing=2)
w1 <- max(nchar(col1_header), max(nchar(trans_2$status), na.rm = TRUE))
wtrs <- sapply(expected_cols, function(cc) {
  vals <- ifelse(is.na(trans_2[[cc]]), "", trans_2[[cc]])
  max(nchar(treat_labels[[cc]]), max(nchar(vals), na.rm = TRUE))
})

fmt_cell <- function(x, w) sprintf(paste0("%-", w, "s"), ifelse(is.na(x), "", x))

title_line <- "Subject Disposition"
header_line <- paste(c(fmt_cell(col1_header, w1), mapply(fmt_cell, treat_labels, wtrs, USE.NAMES = FALSE)), collapse = "  ")
sep_line <- paste(c(paste(rep("-", w1), collapse = ""), sapply(wtrs, function(w) paste(rep("-", w), collapse = ""))), collapse = "  ")

lines <- c()
lines <- c(lines, title_line, "")
lines <- c(lines, header_line)
lines <- c(lines, sep_line)
lines <- c(lines, "")  # headskip
prev_ord <- NA
for (i in seq_len(nrow(trans_2))) {
  cur_ord <- trans_2$ord[i]
  if (!is.na(prev_ord) && cur_ord != prev_ord) {
    lines <- c(lines, "")  # break after ord / skip
  }
  row_cells <- c(fmt_cell(trans_2$status[i], w1), mapply(function(cc, w) fmt_cell(trans_2[[cc]][i], w), expected_cols, wtrs, USE.NAMES = FALSE))
  lines <- c(lines, paste(row_cells, collapse = "  "))
  prev_ord <- cur_ord
}
lines <- c(lines, "")
lines <- c(lines, "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category")
lines <- c(lines, "% = Percentage of subjects with N as denominator")

txt_path <- file.path(out, "P049.txt")
writeLines(lines, txt_path)
# also write .lst copy for exact name match
lst_path <- file.path(out, "P049.lst")
writeLines(lines, lst_path)
cat(paste0("Wrote ", txt_path, "\n"))

# CSV of table body (displayed columns)
csv_df <- data.frame(stringsAsFactors = FALSE, check.names = FALSE)
csv_df[["Subject Disposition"]] <- trans_2$status
for (cc in expected_cols) {
  lab <- treat_labels[[cc]]
  vals <- ifelse(is.na(trans_2[[cc]]), "", trans_2[[cc]])
  csv_df[[lab]] <- vals
}
csv_path <- file.path(out, "P049.csv")
write.csv(csv_df, csv_path, row.names = FALSE, na = "")
cat(paste0("Wrote ", csv_path, "\n"))
