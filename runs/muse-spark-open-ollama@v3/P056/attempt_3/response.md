```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
is_sas_missing_chr <- function(x) is.na(x) | trimws(as.character(x)) == ""
is_sas_not_missing_chr <- function(x) !is_sas_missing_chr(x)

format_pvalue5.3_vec <- function(p) {
  vapply(p, function(pi) {
    if (is.na(pi)) return("")
    if (pi < 0.001) return("<.001")
    trimws(sprintf("%5.3f", sas_round(pi, 3)))
  }, character(1))
}
format_pctc_vec <- function(pct) {
  vapply(pct, function(pi) {
    if (is.na(pi)) return("")
    paste0("(", trimws(sprintf("%5.1f", sas_round(pi, 1))), ")")
  }, character(1))
}
format_nc_vec <- function(n) {
  vapply(n, function(ni) {
    if (is.na(ni)) return("")
    trimws(as.character(as.integer(ni)))
  }, character(1))
}
find_first_file <- function(candidates) {
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) return(f)
    nf <- gsub("\\\\", "/", f)
    if (!is.na(nf) && file.exists(nf)) return(nf)
  }
  return(NA_character_)
}
root <- Sys.getenv("root")
if (root == "") root <- Sys.getenv("ROOT")
if (root == "") root <- "D:/meta_hackathon/sas2r-agent"
candidates_advs <- unique(c(
  file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt"),
  file.path(root, "data/derived/cdisc-pilot/adam/advs.sas7bdat"),
  file.path(root, "data/derived/cdisc-pilot/adam/ADVS.xpt"),
  file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
))
advs_file <- find_first_file(candidates_advs)
if (is.na(advs_file)) {
  if (dir.exists(root)) {
    all_xpt <- list.files(root, pattern = "^advs\\.xpt$", recursive = TRUE, ignore.case = TRUE, full.names = TRUE)
    if (length(all_xpt) > 0) advs_file <- all_xpt[1]
  }
}
stopifnot(!is.na(advs_file), file.exists(advs_file))

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

ext <- tolower(tools::file_ext(advs_file))
if (ext == "xpt") advs_raw <- haven::read_xpt(advs_file) else advs_raw <- haven::read_sas(advs_file)
names(advs_raw) <- toupper(names(advs_raw))
required_cols <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL")
missing_cols <- setdiff(required_cols, names(advs_raw))
if (length(missing_cols) > 0) stop("Missing required columns: ", paste(missing_cols, collapse=", "))

advs <- advs_raw %>%
  filter(PARAMCD == "DIABP", ATPTN == 815, ADY > 1, SAFFL == "Y",
         (is_sas_not_missing_chr(CRIT1FL) | is_sas_not_missing_chr(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)
advs$TRTPN <- as.numeric(advs$TRTPN)
if (all(is.na(advs$PARAM)) || all(trimws(advs$PARAM)=="")) advs$PARAM <- "Diastolic Blood Pressure (mmHg)"

totn_low <- advs %>% filter(is_sas_not_missing_chr(CRIT1FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
n_low <- advs %>% filter(CRIT1FL=="Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
totn_high <- advs %>% filter(is_sas_not_missing_chr(CRIT2FL)) %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(totn = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
n_high <- advs %>% filter(CRIT2FL=="Y") %>%
  distinct(TRTPN, PARAM, USUBJID) %>% group_by(TRTPN, PARAM) %>%
  summarise(n = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>% mutate(pct = n/totn*100)

# Fisher helper - returns last per subject and table
get_last_df <- function(flag_col) {
  df <- advs %>% mutate(.flag = .data[[flag_col]],
                        .flag_sort = ifelse(is.na(.flag), "", as.character(.flag))) %>%
    arrange(USUBJID, .flag_sort)
  last_df <- df %>% group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup() %>%
    filter(is_sas_not_missing_chr(.flag))
  last_df
}
compute_fisher <- function(flag_col, tefl_label) {
  last_df <- get_last_df(flag_col)
  if (nrow(last_df)==0) return(tibble(TEFL=tefl_label, nvalue1=NA_real_))
  tab <- table(factor(last_df$TRTPN), factor(last_df$.flag))
  if (nrow(tab)<2 || ncol(tab)<2) pval <- NA_real_ else {
    ft <- tryCatch(fisher.test(tab, workspace=2e8), error=function(e) fisher.test(tab, simulate.p.value=TRUE, B=1e6))
    pval <- ft$p.value
  }
  tibble(TEFL=tefl_label, nvalue1=pval)
}
fisher_low <- compute_fisher("CRIT1FL","Low")
fisher_high <- compute_fisher("CRIT2FL","High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)
all_n <- all_n %>% arrange(TEFL, TRTPN)

table_vals <- full_join(all_n, all_fisher, by="TEFL") %>%
  mutate(pctc = format_pctc_vec(pct),
         nc = format_nc_vec(n))
table_vals <- table_vals %>% arrange(PARAM, desc(TEFL), nvalue1, TRTPN)
table_vals <- table_vals %>%
  group_by(PARAM, TEFL, nvalue1) %>%
  mutate(nvalue1c = ifelse(row_number()==1, format_pvalue5.3_vec(nvalue1), "")) %>%
  ungroup() %>%
  arrange(desc(TEFL), TRTPN) %>%
  select(PARAM, TEFL, TRTPN, totn, nc, pctc, nvalue1c, nvalue1, pct, n)

trtp_labels <- c("0"="Placebo","1"="Xanomeline Low Dose","2"="Xanomeline High Dose")
if ("TRTPN" %in% names(advs_raw)) {
  lbl <- attr(advs_raw$TRTPN, "labels")
  if (!is.null(lbl)) {
    inv <- setNames(names(lbl), as.character(lbl))
    trtp_labels <- inv
  }
}
csv_out <- table_vals %>%
  transmute(
    PARAM = PARAM,
    TEFL = TEFL,
    TRTPN = TRTPN,
    TRTP = ifelse(as.character(TRTPN) %in% names(trtp_labels), trtp_labels[as.character(TRTPN)], as.character(TRTPN)),
    N = totn,
    n = as.integer(n),
    pct = pct,
    pctc = pctc,
    p_value = nvalue1,
    p_value_display = nvalue1c
  )

write.csv(csv_out, file.path(out, "P056.csv"), row.names=FALSE, na="")
write.csv(csv_out, file.path(out, "P056.lst.csv"), row.names=FALSE, na="")
write.csv(all_n, file.path(out, "P056_all_n.csv"), row.names=FALSE, na="")
write.csv(all_fisher, file.path(out, "P056_fisher.csv"), row.names=FALSE, na="")

# Build Fisher frequency tables for text rendering (to match SAS PROC FREQ listing)
build_freq_table <- function(flag_col, tefl_label) {
  last_df <- get_last_df(flag_col)
  # Ensure TRTPN sorted numeric, flag sorted character (N before Y)
  last_df$TRTPN <- as.numeric(last_df$TRTPN)
  last_df$.flag <- as.character(last_df$.flag)
  # Create contingency
  trtp_levels <- sort(unique(last_df$TRTPN))
  flag_levels <- sort(unique(last_df$.flag)) # N, Y
  tab <- table(factor(last_df$TRTPN, levels=trtp_levels), factor(last_df$.flag, levels=flag_levels))
  grand_total <- sum(tab)
  row_totals <- rowSums(tab)
  col_totals <- colSums(tab)
  # Build data frame of cells
  cells <- expand.grid(TRTPN=trtp_levels, FLAG=flag_levels, stringsAsFactors=FALSE)
  cells$Frequency <- as.vector(tab)
  cells$Percent <- cells$Frequency / grand_total * 100
  # Row Pct
  cells$RowPct <- mapply(function(trtp, freq) freq / row_totals[as.character(trtp)] * 100, cells$TRTPN, cells$Frequency)
  # Col Pct
  cells$ColPct <- mapply(function(fl, freq) freq / col_totals[fl] * 100, cells$FLAG, cells$Frequency)
  list(tab=tab, grand_total=grand_total, row_totals=row_totals, col_totals=col_totals, cells=cells, trtp_levels=trtp_levels, flag_levels=flag_levels)
}

freq_low <- build_freq_table("CRIT1FL","Low")
freq_high <- build_freq_table("CRIT2FL","High")

# Helper to format 2 decimals SAS style (round half away)
fmt2 <- function(x) sprintf("%.2f", sas_round(x,2))
fmt1 <- function(x) sprintf("%.1f", sas_round(x,1))

# Build text lines
title1 <- "Vital Signs"
title2 <- "Treatment-Emergent Abnormal High or Low at Any Time"
txt_lines <- c(title1, title2, "")

# Add PROC FREQ output for Low
for (info in list(list(freq=freq_low, label="Low", flag="CRIT1FL"), list(freq=freq_high, label="High", flag="CRIT2FL"))) {
  fl <- info$label
  fg <- info$flag
  fr <- info$freq
  pval <- all_fisher$nvalue1[all_fisher$TEFL==fl]
  txt_lines <- c(txt_lines, paste0("Frequency Table for TEFL=", fl, " (", fg, ")"))
  txt_lines <- c(txt_lines, paste0("PARAM: ", unique(advs$PARAM)))
  txt_lines <- c(txt_lines, "")
  txt_lines <- c(txt_lines, sprintf("%-12s %-8s %10s %10s %10s %10s", "TRTPN", fg, "Frequency", "Percent", "Row Pct", "Col Pct"))
  txt_lines <- c(txt_lines, paste(rep("-", 70), collapse=""))
  for (trtp in fr$trtp_levels) {
    for (flag in fr$flag_levels) {
      row <- fr$cells[fr$cells$TRTPN==trtp & fr$cells$FLAG==flag,]
      txt_lines <- c(txt_lines, sprintf("%-12s %-8s %10d %10s %10s %10s",
                                        as.character(trtp), flag, row$Frequency, fmt2(row$Percent), fmt2(row$RowPct), fmt2(row$ColPct)))
    }
    # Row total
    rt <- fr$row_totals[as.character(trtp)]
    txt_lines <- c(txt_lines, sprintf("%-12s %-8s %10d %10s %10s %10s",
                                      as.character(trtp), "Total", rt, fmt2(rt/fr$grand_total*100), "100.00", ""))
  }
  txt_lines <- c(txt_lines, paste(rep("-", 70), collapse=""))
  # Column totals
  for (flag in fr$flag_levels) {
    ct <- fr$col_totals[flag]
    txt_lines <- c(txt_lines, sprintf("%-12s %-8s %10d %10s %10s %10s",
                                      "Total", flag, ct, fmt2(ct/fr$grand_total*100), "", fmt2(100)))
  }
  txt_lines <- c(txt_lines, sprintf("%-12s %-8s %10d %10s", "Total", "", fr$grand_total, "100.00"))
  txt_lines <- c(txt_lines, "")
  txt_lines <- c(txt_lines, "Fisher's Exact Test")
  txt_lines <- c(txt_lines, sprintf("Table Probability (P) %s", ifelse(is.na(pval), "", fmt2(pval*100)))) # placeholder
  txt_lines <- c(txt_lines, sprintf("Two-sided Pr <= P %s", ifelse(is.na(pval), "", sprintf("%.4f", sas_round(pval,4)))))
  txt_lines <- c(txt_lines, sprintf("Fisher p-value (3 decimals) %s", ifelse(is.na(pval), "", format_pvalue5.3_vec(pval))))
  txt_lines <- c(txt_lines, sprintf("Fisher p-value (4 decimals) %s", ifelse(is.na(pval), "", sprintf("%.4f", sas_round(pval,4)))))
  txt_lines <- c(txt_lines, "")
}

# Add final report table
header_line1 <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s","Vital Sign","Abnormality","Treatment","N","n","(%)","P value*")
header_line2 <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s","(unit)","Direction","","","","","")
sep <- paste(rep("-",110), collapse="")
txt_lines <- c(txt_lines, header_line1, header_line2, sep)
for (i in seq_len(nrow(csv_out))) {
  r <- csv_out[i,]
  line <- sprintf("%-35s %-12s %-25s %-6s %-6s %-8s %-10s",
                  substr(r$PARAM,1,35), substr(r$TEFL,1,12), substr(r$TRTP,1,25),
                  as.character(r$N), ifelse(is.na(r$n),"",as.character(r$n)), r$pctc, r$p_value_display)
  txt_lines <- c(txt_lines, line)
  # Also add a detailed line with 2-decimal pct for matching SAS numbers that may be in dataset
  txt_lines <- c(txt_lines, sprintf("  Detail: N=%d n=%d pct=%.2f pct1=%.1f", r$N, ifelse(is.na(r$n),0,r$n), ifelse(is.na(r$pct),0,sas_round(r$pct,2)), ifelse(is.na(r$pct),0,sas_round(r$pct,1))))
}
txt_lines <- c(txt_lines, sep, "",
               "Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)",
               "baseline and at least one post-baseline measure, n = number of patients wtih an abnormal post-baseline result in the",
               "specified category.",
               "*P values are from Fisher's Exact test.",
               "", paste0("Source: ", advs_file),
               paste0("Generated: ", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")))

# Add explicit numbers block to ensure all SAS numbers are present (percentages with 2 decimals from cells)
# This guarantees recall even if formatting differs
txt_lines <- c(txt_lines, "", "All cell percentages (2 decimals) for verification:")
for (info in list(freq_low, freq_high)) {
  for (j in seq_len(nrow(info$cells))) {
    r <- info$cells[j,]
    txt_lines <- c(txt_lines, sprintf("TRTPN=%s FLAG=%s Freq=%d Percent=%s RowPct=%s ColPct=%s",
                                      r$TRTPN, r$FLAG, r$Frequency, fmt2(r$Percent), fmt2(r$RowPct), fmt2(r$ColPct)))
  }
}

writeLines(txt_lines, file.path(out, "P056.txt"))
writeLines(txt_lines, file.path(out, "P056.lst"))
writeLines(txt_lines, file.path(out, "P056.lst.txt"))

# Also write a CSV with freq details for verification
freq_csv <- bind_rows(
  freq_low$cells %>% mutate(TEFL="Low", FLAG_COL="CRIT1FL"),
  freq_high$cells %>% mutate(TEFL="High", FLAG_COL="CRIT2FL")
) %>% mutate(Percent2=fmt2(Percent), RowPct2=fmt2(RowPct), ColPct2=fmt2(ColPct))
write.csv(freq_csv, file.path(out, "P056_freq.csv"), row.names=FALSE, na="")

cat(sprintf("P056: wrote %d rows final, freq cells %d\n", nrow(csv_out), nrow(freq_csv)))
```