```r
# P049_disposition_b1.sas -> R translation
# Subject Disposition (ITTFL='Y', DCREASCD Completed vs Discontinued + details)
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
})

round_half_up <- function(x, digits = 0) {
  # SAS rounds half away from zero
  s <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-9) / 10^digits
  s * z
}

sas_percent81 <- function(p) {
  # PUT(p, percent8.1): multiply by 100, 1 decimal, trailing %
  # strip() applied later by caller via trimws
  ifelse(is.na(p), "", sprintf("%.1f%%", round_half_up(p * 100, 1)))
}

sas_key <- function(x) {
  vapply(x, function(s) {
    if (is.na(s)) return("")
    if (s == "") return("")
    paste(sprintf("%04d", utf8ToInt(s)), collapse = "-")
  }, character(1), USE.NAMES = FALSE)
}

adsl_path_raw <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt"
adsl_path_der <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt"
adsl_path <- adsl_path_raw
if (!file.exists(adsl_path) && file.exists(adsl_path_der)) adsl_path <- adsl_path_der
message("Reading ADSL: ", adsl_path)
adsl <- haven::read_xpt(adsl_path)
# normalise names to uppercase (SAS case-insensitive)
names(adsl) <- toupper(names(adsl))
stopifnot(all(c("USUBJID","TRT01P","TRT01PN","DCREASCD","ITTFL") %in% names(adsl)))

# ---- PROC SQL create table disp ----
# first block: ord=1 Completed / Discontinued
adsl_itt <- adsl %>% filter(!is.na(ITTFL) & ITTFL == "Y")
stopifnot(nrow(adsl_itt) > 0)
stopifnot("DCREASCD" %in% names(adsl_itt))

disp1 <- adsl_itt %>%
  mutate(STATUS = ifelse(!is.na(DCREASCD) & DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, STATUS) %>%
  summarise(CNT = n(), .groups = "drop") %>%
  mutate(ORD = 1L) %>%
  rename(TRT01P = TRT01P, TRT01PN = TRT01PN, STATUS = STATUS, CNT = CNT)

# second block: ord=2 individual reasons excluding Completed
# SAS: WHERE ITTFL='Y' and DCREASCD^='Completed'  (blank/missing is true)
adsl_itt_disc <- adsl_itt %>% filter(is.na(DCREASCD) | DCREASCD != "Completed")
disp2 <- adsl_itt_disc %>%
  mutate(STATUS = ifelse(is.na(DCREASCD), "", DCREASCD)) %>%
  group_by(TRT01P, TRT01PN, STATUS) %>%
  summarise(CNT = n(), .groups = "drop") %>%
  mutate(ORD = 2L)

disp <- bind_rows(disp1, disp2)
# order by TRT01PN, ord, status (SAS binary: uppercase before lowercase)
disp <- disp %>% arrange(TRT01PN, ORD, sas_key(STATUS))

# ---- PROC SQL create table freq (no ITT filter, per SAS) ----
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(TOT_CNT = n(), .groups = "drop") %>%
  arrange(TRT01PN)
stopifnot(all(c("TRT01PN","TOT_CNT") %in% names(freq)))
stopifnot("TRT01PN" %in% names(disp))

# ---- DATA disp_2; merge disp freq; by TRT01PN ----
# drop/rename overlapping non-keys (none except key) to avoid .x/.y
disp_2 <- left_join(disp, freq, by = "TRT01PN")
stopifnot(all(c("TRT01P","TRT01PN","STATUS","CNT","ORD","TOT_CNT") %in% names(disp_2)))
stopifnot(all(!is.na(disp_2$TOT_CNT)))
disp_2 <- disp_2 %>%
  mutate(
    TREAT = paste0(trimws(as.character(TRT01P)), " (N=", trimws(as.character(TOT_CNT)), ")"),
    CNT_PRCNT = paste0(trimws(as.character(CNT)), " (", trimws(sas_percent81(CNT / TOT_CNT)), ")")
  )

# ---- PROC SORT disp_2; by status ord ----
disp_2 <- disp_2 %>% arrange(sas_key(STATUS), ORD)

# ---- PROC TRANSPOSE prefix=tr; by status ord; id TRT01PN; idlabel treat; var cnt_prcnt ----
trt_order <- sort(unique(disp_2$TRT01PN))
# treat label per TRT01PN (first occurrence)
treat_labels <- disp_2 %>%
  group_by(TRT01PN) %>%
  summarise(TREAT_LBL = first(TREAT), .groups = "drop") %>%
  arrange(TRT01PN)
treat_vec <- setNames(treat_labels$TREAT_LBL, as.character(treat_labels$TRT01PN))
col_names <- paste0("tr", trt_order)  # SAS names
# distinct BY groups
by_groups <- disp_2 %>% distinct(STATUS, ORD) %>% arrange(ORD, sas_key(STATUS))
trans_list <- lapply(seq_len(nrow(by_groups)), function(i) {
  st <- by_groups$STATUS[i]; od <- by_groups$ORD[i]
  sub <- disp_2 %>% filter(STATUS == st | (is.na(STATUS) & is.na(st)), ORD == od)
  # handle "" vs NA: distinct above uses "" for missing; filter exact
  row <- list(STATUS = st, ORD = od, `_NAME_` = "cnt_prcnt")
  for (t in trt_order) {
    v <- sub$CNT_PRCNT[sub$TRT01PN == t]
    if (length(v) == 0) v <- NA_character_
    else v <- v[1]
    row[[paste0("tr", t)]] <- v
  }
  as.data.frame(row, stringsAsFactors = FALSE, check.names = FALSE)
})
trans_2 <- bind_rows(trans_list)
# PROC SORT trans_2; by ord status
trans_2 <- trans_2 %>% arrange(ORD, sas_key(STATUS))

# Build display table: status + one column per treatment with header = treat label
# Keep SAS column order tr sorted by TRT01PN
display_df <- trans_2 %>% select(STATUS, ORD, all_of(col_names))
# For CSV of table body (displayed): columns = "Subject Disposition" + treat labels
csv_body <- display_df
# rename tr columns to treat labels for display CSV
new_names <- c("Subject Disposition", "ORD", treat_vec[as.character(trt_order)])
# csv_body currently STATUS, ORD, tr... -> map
names(csv_body) <- c("Subject Disposition", "ORD", treat_vec[as.character(trt_order)])
# ORD is noprint in PROC REPORT: drop from displayed CSV body
csv_display <- csv_body %>% select(-ORD)

message("Writing table CSV and text")
write.csv(csv_display, file.path(out, "P049.csv"), row.names = FALSE, na = "")

# Also write underlying dataset trans_2 as dataset CSV for traceability
write.csv(trans_2, file.path(out, "P049_trans_2.csv"), row.names = FALSE, na = "")

# ---- PROC REPORT text rendering ----
title_txt <- "Subject Disposition"
foot1 <- "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category"
foot2 <- "% = Percentage of subjects with N as denominator"

hdr_status <- "Subject Disposition"
hdr_treats <- as.character(treat_vec[as.character(trt_order)])

# column widths
status_vals <- display_df$STATUS
w_status <- max(nchar(c(hdr_status, ifelse(is.na(status_vals), "", status_vals))), na.rm = TRUE)
w_treats <- sapply(seq_along(trt_order), function(j) {
  col <- display_df[[col_names[j]]]
  max(nchar(c(hdr_treats[j], ifelse(is.na(col), "", col))), na.rm = TRUE)
})

pad <- function(x, w) sprintf(paste0("%-", w, "s"), ifelse(is.na(x), "", x))

header_line <- paste(c(pad(hdr_status, w_status), sapply(seq_along(w_treats), function(j) pad(hdr_treats[j], w_treats[j]))), collapse = "  ")
total_width <- nchar(header_line)
headline <- paste(rep("-", total_width), collapse = "")

lines <- c()
lines <- c(lines, title_txt, "")
lines <- c(lines, header_line)
lines <- c(lines, headline)
lines <- c(lines, "")  # headskip

# rows grouped by ORD with break after ord/skip (blank line between ord groups)
ords <- sort(unique(display_df$ORD))
for (oi in seq_along(ords)) {
  od <- ords[oi]
  sub <- display_df %>% filter(ORD == od) %>% arrange(sas_key(STATUS))
  for (r in seq_len(nrow(sub))) {
    cells <- c(pad(sub$STATUS[r], w_status),
               sapply(seq_along(w_treats), function(j) pad(sub[[col_names[j]]][r], w_treats[j])))
    lines <- c(lines, paste(cells, collapse = "  "))
  }
  if (oi < length(ords)) lines <- c(lines, "")  # break after ord/skip
}
lines <- c(lines, "")
lines <- c(lines, paste0("  ", foot1))
lines <- c(lines, paste0("  ", foot2))

writeLines(lines, file.path(out, "P049.txt"))
# exact copy with .lst extension to match SAS expected name
writeLines(lines, file.path(out, "P049.lst"))
message("Done")
```