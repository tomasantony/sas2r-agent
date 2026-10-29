# P049_disposition_b1.sas -> R translation
# Subject Disposition (ITTFL='Y', Completed vs Discontinued + reasons)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adsl_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"

cat("Reading ADSL...\n")
adsl_raw <- haven::read_xpt(adsl_path)
adsl <- as.data.frame(adsl_raw, stringsAsFactors = FALSE)
# zap haven labels to plain vectors (keep values)
for (nm in names(adsl)) {
  x <- adsl[[nm]]
  if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
    adsl[[nm]] <- haven::zap_labels(x)
  }
}
# ensure expected columns exist (uppercase)
# haven preserves case; normalize lookup case-insensitively
nm_up <- toupper(names(adsl))
names(adsl) <- nm_up
# keep needed vars
# USUBJID, TRT01P, TRT01PN, DCREASCD, ITTFL
adsl$USUBJID  <- as.character(adsl$USUBJID)
adsl$TRT01P   <- as.character(adsl$TRT01P)
adsl$TRT01PN  <- suppressWarnings(as.numeric(adsl$TRT01PN))
adsl$DCREASCD <- as.character(adsl$DCREASCD)
adsl$ITTFL    <- as.character(adsl$ITTFL)

# SAS round half away from zero helper (not used directly; percent uses integer method)
round_half_up <- function(x, digits = 0) {
  mult <- 10 ^ digits
  sx <- sign(x)
  ax <- abs(x)
  sx * floor(ax * mult + 0.5 + 1e-9) / 1
  # corrected below for digits
  sx * floor(ax * mult + 0.5 + 1e-9) / mult
}

# ASCII sort key to mimic SAS (uppercase before lowercase, missing first)
ascii_key <- function(x) {
  sapply(seq_along(x), function(i) {
    v <- x[i]
    if (is.na(v)) return("000")
    s <- as.character(v)
    if (is.na(s)) return("000")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = ":")
  }, USE.NAMES = FALSE)
}

# ------------------------------------------------------------------
# proc sql; create table disp
# part1: Completed vs Discontinued, ord=1, WHERE ITTFL='Y'
# ------------------------------------------------------------------
itt <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")

disp1 <- itt %>%
  mutate(status = ifelse(!is.na(DCREASCD) & DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, status) %>%
  summarise(cnt = sum(!is.na(USUBJID)), .groups = "drop") %>%
  mutate(ord = 1L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# part2: individual reasons excluding Completed, ord=2
itt_disc <- itt %>% filter(is.na(DCREASCD) | DCREASCD != "Completed")

disp2 <- itt_disc %>%
  mutate(status = DCREASCD, ord = 2L) %>%
  group_by(TRT01P, TRT01PN, status, ord) %>%
  summarise(cnt = sum(!is.na(USUBJID)), .groups = "drop") %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

disp <- bind_rows(disp1, disp2)
# order by TRT01PN, ord, status (SAS)
disp <- disp %>% arrange(TRT01PN, ord, ascii_key(status))

# ------------------------------------------------------------------
# create table freq : total N per TRT01PN (all subjects, no ITT filter)
# ------------------------------------------------------------------
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = sum(!is.na(USUBJID)), .groups = "drop") %>%
  arrange(TRT01PN)

# ------------------------------------------------------------------
# data disp_2; merge disp freq; by TRT01PN;
# treat + cnt_prcnt with percent8.1 stripped
# ------------------------------------------------------------------
disp_2 <- disp %>% left_join(freq, by = "TRT01PN")

# percent formatting: SAS PUT(cnt/tot_cnt, percent8.1) stripped
# percent8.1 = 100*p, 1 decimal, half away from zero; strip removes blanks
fmt_pct <- function(cnt, tot) {
  # vectorized
  mapply(function(c1, t1) {
    if (is.na(c1) || is.na(t1) || t1 == 0) return(NA_character_)
    pct <- c1 / t1 * 100
    tenths_int <- floor(pct * 10 + 0.5 + 1e-8)
    whole <- tenths_int %/% 10
    dec <- tenths_int %% 10
    paste0(whole, ".", dec, "%")
  }, cnt, tot, USE.NAMES = FALSE)
}

disp_2$treat <- paste0(trimws(disp_2$TRT01P), " (N=", disp_2$tot_cnt, ")")
pct_str <- fmt_pct(disp_2$cnt, disp_2$tot_cnt)
disp_2$cnt_prcnt <- paste0(trimws(as.character(disp_2$cnt)), " (", trimws(pct_str), ")")

# proc sort data=disp_2; by status ord;
disp_2 <- disp_2[order(ascii_key(disp_2$status), disp_2$ord), , drop = FALSE]

# ------------------------------------------------------------------
# proc transpose out=trans_2 prefix=tr; by status ord; id TRT01PN; var cnt_prcnt;
# ------------------------------------------------------------------
trt_order <- sort(unique(disp_2$TRT01PN[!is.na(disp_2$TRT01PN)]))
# treat label per TRT01PN (idlabel)
trt_map <- disp_2 %>% distinct(TRT01PN, treat) %>% arrange(TRT01PN)
# ensure all trt_order have label; fallback to freq + TRT01P lookup if needed
if (nrow(trt_map) < length(trt_order)) {
  # already complete
  trt_map <- data.frame(TRT01PN = trt_order) %>% left_join(trt_map, by = "TRT01PN")
}

trans_long <- disp_2 %>% select(status, ord, TRT01PN, cnt_prcnt)

trans_2 <- trans_long %>%
  mutate(TRT01PN_c = as.character(TRT01PN)) %>%
  pivot_wider(id_cols = c(status, ord),
              names_from = TRT01PN_c,
              values_from = cnt_prcnt,
              names_prefix = "tr")

# ensure tr columns ordered by numeric TRT01PN ascending
tr_cols <- paste0("tr", trt_order)
# add missing columns if any reason lacks a treatment (fill NA)
for (cc in tr_cols) {
  if (!cc %in% names(trans_2)) trans_2[[cc]] <- NA_character_
}
trans_2 <- trans_2 %>% select(status, ord, all_of(tr_cols))

# proc sort data=trans_2; by ord status;
trans_2 <- trans_2[order(trans_2$ord, ascii_key(trans_2$status)), , drop = FALSE]

# ------------------------------------------------------------------
# proc report: columns ord(status noprint) tr: ; break after ord/skip
# ------------------------------------------------------------------
# headers = treat labels in trt_order
hdr_map <- trt_map[match(trt_order, trt_map$TRT01PN), , drop = FALSE]
treat_labels <- hdr_map$treat
# fallback if label missing: construct from adsl distinct
if (any(is.na(treat_labels))) {
  adsl_map <- adsl %>% distinct(TRT01PN, TRT01P) %>% arrange(TRT01PN)
  for (i in seq_along(trt_order)) {
    if (is.na(treat_labels[i])) {
      r <- adsl_map %>% filter(TRT01PN == trt_order[i])
      tot <- freq$tot_cnt[freq$TRT01PN == trt_order[i]]
      if (nrow(r) > 0 && length(tot) > 0) {
        treat_labels[i] <- paste0(trimws(r$TRT01P[1]), " (N=", tot[1], ")")
      } else {
        treat_labels[i] <- paste0("TRT", trt_order[i])
      }
    }
  }
}

headers <- c("Subject Disposition", treat_labels)

# body matrix (character, NA -> "")
n_r <- nrow(trans_2)
n_c_trt <- length(tr_cols)
body_status <- as.character(trans_2$status)
body_status[is.na(body_status)] <- ""
body_mat <- matrix("", nrow = n_r, ncol = 1 + n_c_trt)
body_mat[, 1] <- body_status
for (j in seq_along(tr_cols)) {
  v <- as.character(trans_2[[tr_cols[j]]])
  v[is.na(v)] <- ""
  body_mat[, j + 1] <- v
}

# column widths (max header/body length), left-aligned, spacing=2
all_rows <- rbind(headers, body_mat)
widths <- sapply(seq_along(headers), function(j) max(nchar(all_rows[, j], type = "chars"), na.rm = TRUE))

pad_right <- function(x, w) sprintf(paste0("%-", w, "s"), x)

fmt_line <- function(cells) {
  padded <- sapply(seq_along(cells), function(j) pad_right(cells[j], widths[j]), USE.NAMES = FALSE)
  paste(padded, collapse = "  ")
}

title_line <- "Subject Disposition"
header_line <- fmt_line(headers)
dash_cells <- sapply(widths, function(w) paste(rep("-", w), collapse = ""), USE.NAMES = FALSE)
dash_line <- paste(dash_cells, collapse = "  ")

out_lines <- c()
out_lines <- c(out_lines, title_line, "")
out_lines <- c(out_lines, header_line)
out_lines <- c(out_lines, dash_line)

# rows with break after ord/skip (blank line between ord groups)
ords <- trans_2$ord
for (i in seq_len(n_r)) {
  out_lines <- c(out_lines, fmt_line(body_mat[i, ]))
  if (i < n_r && ords[i] != ords[i + 1]) {
    out_lines <- c(out_lines, "")
  }
}
out_lines <- c(out_lines, "")
out_lines <- c(out_lines, "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category")
out_lines <- c(out_lines, "% = Percentage of subjects with N as denominator")

txt_path <- file.path(out, "P049.txt")
writeLines(out_lines, txt_path, useBytes = TRUE)
# also write .lst copy for benchmark compatibility
lst_path <- file.path(out, "P049.lst")
writeLines(out_lines, lst_path, useBytes = TRUE)

# CSV of table body (same numbers)
csv_df <- data.frame("Subject Disposition" = body_mat[, 1], stringsAsFactors = FALSE, check.names = FALSE)
for (j in seq_along(tr_cols)) {
  csv_df[[treat_labels[j]]] <- body_mat[, j + 1]
}
names(csv_df)[1] <- "Subject Disposition"
csv_path <- file.path(out, "P049.csv")
write.csv(csv_df, csv_path, row.names = FALSE, na = "")

cat("Wrote:", txt_path, "\n")
cat("Wrote:", csv_path, "\n")
cat("Wrote:", lst_path, "\n")
