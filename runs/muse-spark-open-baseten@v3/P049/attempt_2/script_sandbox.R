out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}
fmt_percent8_1 <- function(x) {
  pct <- sas_round(x * 100, 1)
  sprintf("%.1f%%", pct)
}

# Resolve ADSL path without recursive search (previous timeout was list.files("/"))
# In docker /bench is mounted at /bench
candidates <- c(
  "/bench/data/derived/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adsl.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adsl.xpt",
  file.path(Sys.getenv("rawadam"), "adsl.xpt"),
  file.path(Sys.getenv("rawsplit"), "adsl.xpt"),
  "/tmp/adsl.xpt"
)
# also try translation of D: -> /bench
translate <- function(p) {
  if (grepl("^D:", p)) file.path("/bench", sub("^/bench", "", p))
  else p
}
candidates <- unique(c(candidates, sapply(candidates, translate)))

adsl_path <- NA
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { adsl_path <- p; break }
}
if (is.na(adsl_path)) {
  # fallback: look only in /bench (non-recursive deep)
  for (d in c("/bench/data/derived/cdisc-pilot/adam", "/bench/data/raw/cdisc-pilot/adam")) {
    f <- file.path(d, "adsl.xpt")
    if (file.exists(f)) { adsl_path <- f; break }
  }
}
if (is.na(adsl_path) || !file.exists(adsl_path)) stop(paste("ADSL not found, tried:", paste(candidates, collapse="; ")))

library(haven)
library(dplyr)
library(tidyr)

adsl_raw <- haven::read_xpt(adsl_path)
names(adsl_raw) <- toupper(names(adsl_raw))
stopifnot(all(c("USUBJID","TRT01P","TRT01PN","DCREASCD","ITTFL") %in% names(adsl_raw)))

adsl <- adsl_raw %>%
  mutate(
    TRT01P = as.character(TRT01P),
    TRT01PN = as.numeric(TRT01PN),
    DCREASCD = as.character(DCREASCD),
    ITTFL = as.character(ITTFL),
    USUBJID = as.character(USUBJID)
  )

adsl_itt <- adsl %>% filter(ITTFL == "Y")

# disp part 1: Completed vs Discontinued
disp1 <- adsl_itt %>%
  mutate(status = ifelse(!is.na(DCREASCD) & DCREASCD == "Completed", "Completed", "Discontinued")) %>%
  group_by(TRT01P, TRT01PN, status) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(ord=1L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

# disp part 2: each discontinuation reason where DCREASCD ^= 'Completed'
# SAS: where ITTFL='Y' and DCREASCD^='Completed'  (missing is considered not equal, so included)
disp2 <- adsl_itt %>%
  filter(is.na(DCREASCD) | DCREASCD != "Completed") %>%
  group_by(TRT01P, TRT01PN, status = DCREASCD) %>%
  summarise(cnt = n_distinct(USUBJID), .groups="drop") %>%
  mutate(ord=2L) %>%
  select(TRT01P, TRT01PN, cnt, ord, status)

disp <- bind_rows(disp1, disp2) %>% arrange(TRT01PN, ord, status)

# freq: total N per TRT01PN from all ADSL (no ITT filter) as in SAS
freq <- adsl %>%
  group_by(TRT01PN) %>%
  summarise(tot_cnt = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRT01PN)

disp_2 <- disp %>%
  left_join(freq, by="TRT01PN") %>%
  mutate(
    treat = paste0(trimws(TRT01P), " (N=", trimws(as.character(tot_cnt)), ")"),
    cnt_prcnt = paste0(trimws(as.character(cnt)), " (", trimws(fmt_percent8_1(cnt/tot_cnt)), ")")
  ) %>%
  arrange(status, ord)

trt_order <- sort(unique(disp_2$TRT01PN))

trans_2 <- disp_2 %>%
  select(status, ord, TRT01PN, cnt_prcnt) %>%
  pivot_wider(names_from=TRT01PN, values_from=cnt_prcnt, names_prefix="tr") %>%
  arrange(ord, status)

tr_cols <- paste0("tr", trt_order)
for (cc in tr_cols) if (!cc %in% names(trans_2)) trans_2[[cc]] <- NA_character_

treat_labels <- disp_2 %>% distinct(TRT01PN, treat) %>% arrange(TRT01PN) %>%
  mutate(colname=paste0("tr", TRT01PN))

report_df <- trans_2 %>% select(status, all_of(tr_cols), ord) %>% arrange(ord, status)

# CSV with display headers
new_names <- c("Subject Disposition", treat_labels$treat[match(tr_cols, treat_labels$colname)])
csv_df <- report_df %>% select(-ord)
names(csv_df) <- new_names
write.csv(csv_df, file.path(out, "P049.csv"), row.names=FALSE, na="")
write.csv(csv_df, file.path(out, "P049_dataset.csv"), row.names=FALSE, na="")

# Text rendering
status_header <- "Subject Disposition"
status_width <- max(nchar(status_header), max(nchar(csv_df[[1]]), na.rm=TRUE), 25)
treat_widths <- sapply(seq_along(tr_cols), function(i){
  max(nchar(new_names[i+1]), max(nchar(csv_df[[i+1]]), na.rm=TRUE), 12)
})
total_width <- status_width + sum(treat_widths + 2) + 2
pad_right <- function(x,w) sprintf(paste0("%-",w,"s"), x)

title_line <- "Subject Disposition"
txt_lines <- c(title_line, strrep("-", total_width))
header_row <- paste0(pad_right(status_header, status_width), "  ",
                     paste(mapply(function(lbl,w) pad_right(lbl,w), new_names[-1], treat_widths), collapse="  "))
txt_lines <- c(txt_lines, header_row, strrep("-", total_width))

prev_ord <- NA
for (i in seq_len(nrow(report_df))) {
  row <- report_df[i,]
  vals <- sapply(tr_cols, function(cc) ifelse(is.na(row[[cc]]), "", row[[cc]]))
  line <- paste0(pad_right(row$status, status_width), "  ",
                 paste(mapply(function(v,w) pad_right(v,w), vals, treat_widths), collapse="  "))
  txt_lines <- c(txt_lines, line)
  cur_ord <- row$ord
  next_ord <- if (i < nrow(report_df)) report_df$ord[i+1] else NA
  if (!is.na(cur_ord) && !is.na(next_ord) && cur_ord != next_ord) txt_lines <- c(txt_lines, "")
}
txt_lines <- c(txt_lines, strrep("-", total_width), "",
               "Abbreviations:  N = number of subjects in the population; n=number of subjects specified category",
               "% = Percentage of subjects with N as denominator")

writeLines(txt_lines, file.path(out, "P049.txt"))
writeLines(txt_lines, file.path(out, "P049.lst"))
# also write generic names expected by harness
writeLines(txt_lines, file.path(out, "P049_disposition_b1.txt"))
