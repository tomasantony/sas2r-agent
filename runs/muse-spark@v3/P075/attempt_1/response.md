```r
# P075 t_vs.sas -> R translation
# Table 14.3.5 Summary of Vital Signs by Visit (Safety Analysis Set)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
})

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_adam_file <- function(dir, ds) {
  fl <- list.files(dir, full.names = TRUE)
  if (length(fl) == 0) stop(paste0("No files in ", dir))
  base_noext <- tolower(sub("\\.[^.]*$", "", basename(fl)))
  idx <- which(base_noext == tolower(ds))
  if (length(idx) == 0) stop(paste0("Dataset ", ds, " not found in ", dir, " : ", paste(basename(fl), collapse=", ")))
  fl[idx[1]]
}
read_sas_any <- function(path) {
  ext <- tolower(sub("^.*\\.([^.]*)$", "\\1", basename(path)))
  if (ext == "xpt") df <- haven::read_xpt(path)
  else df <- haven::read_sas(path)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  # zap haven labels
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled")) {
      # convert to plain vector preserving values
      df[[nm]] <- haven::zap_labels(x)
      df[[nm]] <- haven::zap_formats(df[[nm]])
      # haven_labelled may remain; coerce
      if (inherits(df[[nm]], "haven_labelled")) class(df[[nm]]) <- setdiff(class(df[[nm]]), "haven_labelled")
    }
  }
  names(df) <- toupper(names(df))
  df
}

cat("Reading ADSL/ADVS...\n")
adsl_path <- find_adam_file(adam_dir, "adsl")
advs_path <- find_adam_file(adam_dir, "advs")
adsl <- read_sas_any(adsl_path)
advs <- read_sas_any(advs_path)
stopifnot(all(c("USUBJID","TRTA","TRTAN","SAFFL") %in% names(adsl)))
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PARAMN","AVISIT","AVISITN","AVAL","BASE","CHG","ABLFL","ANL01FL") %in% names(advs)))

# ---- Step 1: Big N (m_counts) ----
bign_df <- adsl %>%
  filter(!is.na(SAFFL) & toupper(SAFFL) == "Y") %>%
  group_by(TRTA) %>%
  summarise(BIGN = n_distinct(USUBJID), .groups="drop") %>%
  arrange(TRTA)
bign_tot <- n_distinct(adsl$USUBJID[!is.na(adsl$SAFFL) & toupper(adsl$SAFFL)=="Y"])
cat("Big N:\n")
print(bign_df)
cat(paste0("Total N=", bign_tot, "\n"))

# ---- Step 2: analysis data vs ----
wanted <- c('SYSBP','DIABP','PULSE','TEMP','RESP','WEIGHT')
# inner join advs with adsl on USUBJID, keep safety pop
# drop/rename overlapping cols except key before merge (avoid .x/.y)
adsl_keys <- adsl %>% select(USUBJID, SAFFL)
vs <- advs %>%
  inner_join(adsl_keys, by="USUBJID") %>%
  filter(SAFFL == 'Y',
         PARAMCD %in% wanted,
         ANL01FL == 'Y') %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, AVAL, BASE, CHG, ABLFL, ANL01FL) %>%
  arrange(PARAMN, AVISITN, TRTAN)
stopifnot(nrow(vs) > 0)
cat(paste0("VS rows: ", nrow(vs), "\n"))

# ---- helpers: SAS rounding half away from zero ----
round_half_up <- function(x, digits=0) {
  # vectorized
  res <- x
  for (i in seq_along(x)) {
    xi <- x[i]
    if (is.na(xi) || is.nan(xi) || is.infinite(xi)) { res[i] <- xi; next }
    p <- 10^digits
    res[i] <- sign(xi) * floor(abs(xi)*p + 0.5 + 1e-9) / p
  }
  res
}
sas_num <- function(x, dec) {
  # emulate strip(put(x, w.dec)): "." if missing else rounded with dec decimals
  sapply(seq_along(x), function(i) {
    xi <- x[i]
    if (length(xi)==0 || is.na(xi) || is.nan(xi)) return(".")
    if (is.infinite(xi)) return(".")
    y <- round_half_up(xi, dec)
    sprintf(paste0("%.", dec, "f"), y)
  }, USE.NAMES=FALSE)
}

# ---- Step 3: PROC MEANS NWAY ----
# Actual value stats: group by all class vars
actual <- vs %>%
  group_by(PARAMCD, PARAM, PARAMN, AVISIT, AVISITN, TRTA, TRTAN) %>%
  summarise(
    N = sum(!is.na(AVAL)),
    MEAN = ifelse(sum(!is.na(AVAL))>0, mean(AVAL, na.rm=TRUE), NA_real_),
    STD = ifelse(sum(!is.na(AVAL))>1, sd(AVAL, na.rm=TRUE), NA_real_),
    MEDIAN = ifelse(sum(!is.na(AVAL))>0, median(AVAL, na.rm=TRUE), NA_real_),
    MIN = ifelse(sum(!is.na(AVAL))>0, min(AVAL, na.rm=TRUE), NA_real_),
    MAX = ifelse(sum(!is.na(AVAL))>0, max(AVAL, na.rm=TRUE), NA_real_),
    .groups="drop"
  )

# Change stats: where CHG ne . and ABLFL ne 'Y' (SAS missing '' ne 'Y' is TRUE)
vs_chg_src <- vs %>%
  filter(!is.na(CHG) & (is.na(ABLFL) | ABLFL != 'Y'))
change <- vs_chg_src %>%
  group_by(PARAMCD, AVISITN, TRTAN) %>%
  summarise(
    N_CHG = sum(!is.na(CHG)),
    MEAN_CHG = ifelse(sum(!is.na(CHG))>0, mean(CHG, na.rm=TRUE), NA_real_),
    STD_CHG = ifelse(sum(!is.na(CHG))>1, sd(CHG, na.rm=TRUE), NA_real_),
    MEDIAN_CHG = ifelse(sum(!is.na(CHG))>0, median(CHG, na.rm=TRUE), NA_real_),
    MIN_CHG = ifelse(sum(!is.na(CHG))>0, min(CHG, na.rm=TRUE), NA_real_),
    MAX_CHG = ifelse(sum(!is.na(CHG))>0, max(CHG, na.rm=TRUE), NA_real_),
    .groups="drop"
  )

# ---- Step 4: Merge actual and change (SAS keys: PARAMCD, AVISITN, TRTAN) ----
stats <- actual %>%
  left_join(change, by=c("PARAMCD","AVISITN","TRTAN")) %>%
  arrange(PARAMN, AVISITN, TRTAN)

# ---- Step 5: Format for display ----
report <- stats %>%
  rowwise() %>%
  mutate(
    stat1 = as.character(N),
    stat2 = sas_num(MEAN, 1),
    stat3 = sas_num(STD, 2),
    stat4 = sas_num(MEDIAN, 1),
    stat5 = paste0(sas_num(MIN,1), ", ", sas_num(MAX,1)),
    chg1 = ifelse(!is.na(N_CHG) & N_CHG>0, as.character(N_CHG), ""),
    chg2 = ifelse(!is.na(N_CHG) & N_CHG>0, sas_num(MEAN_CHG,2), ""),
    chg3 = ifelse(!is.na(N_CHG) & N_CHG>0, sas_num(STD_CHG,2), ""),
    chg4 = ifelse(!is.na(N_CHG) & N_CHG>0, sas_num(MEDIAN_CHG,2), ""),
    chg5 = ifelse(!is.na(N_CHG) & N_CHG>0, paste0(sas_num(MIN_CHG,1), ", ", sas_num(MAX_CHG,1)), "")
  ) %>%
  ungroup() %>%
  arrange(PARAMN, AVISITN, TRTAN)

# Ensure character missing handling: SAS blank for chg when no data
report$chg1[is.na(report$chg1)] <- ""
report$chg2[is.na(report$chg2)] <- ""
report$chg3[is.na(report$chg3)] <- ""
report$chg4[is.na(report$chg4)] <- ""
report$chg5[is.na(report$chg5)] <- ""

# Order columns like PROC REPORT
report <- report %>% select(PARAMN, PARAM, AVISITN, AVISIT, TRTA, TRTAN, PARAMCD,
                            N, MEAN, STD, MEDIAN, MIN, MAX,
                            N_CHG, MEAN_CHG, STD_CHG, MEDIAN_CHG, MIN_CHG, MAX_CHG,
                            stat1, stat2, stat3, stat4, stat5,
                            chg1, chg2, chg3, chg4, chg5)

cat(paste0("Report rows: ", nrow(report), "\n"))

# ---- Step 6: outputs ----
# CSV of table body (SAS variable names)
csv_df <- report %>%
  select(PARAM, AVISIT, TRTA, stat1, stat2, stat3, stat4, stat5, chg1, chg2, chg3, chg4, chg5)
# keep ordering columns for traceability? include PARAMN/AVISITN/TRTAN as well for exactness
csv_full <- report
write.csv(csv_full, file.path(out, "t_vs.csv"), row.names=FALSE, na="")

# Text rendering with titles/footnotes verbatim
# Titles from u_titles.sas for T_VS, sponsor/protocol/cutdate placeholders
t1 <- "[Sponsor Name]                                                                Page 1 of N"
t2 <- "Protocol: [Protocol Number]"
t3 <- ""
t4 <- "Table 14.3.5"
t5 <- "Summary of Vital Signs by Visit"
t6 <- "Safety Analysis Set"
f1 <- "Data cutoff: [DDMONYYYY]."
f2 <- "Source: ADaM datasets"
f3 <- paste0("Program: t_vs.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

# Build header strings for Big N (for reference, not in SAS columns but include as note)
hdr_note <- paste0("Treatment groups: ",
                   paste(paste0(bign_df$TRTA, " (N=", bign_df$BIGN, ")"), collapse=" | "),
                   " | Total (N=", bign_tot, ")")

lines <- c(t1, t2, t3, t4, t5, t6, "", hdr_note, "")
col_hdr1 <- sprintf("%-28s %-18s %-18s | %-5s %-8s %-8s %-8s %-14s | %-5s %-8s %-8s %-8s %-14s",
                    "Parameter", "Visit", "Treatment",
                    "n", "Mean", "SD", "Median", "Min, Max",
                    "n", "Mean", "SD", "Median", "Min, Max")
col_hdr2 <- sprintf("%-28s %-18s %-18s | %-5s %-8s %-8s %-8s %-14s | %-5s %-8s %-8s %-8s %-14s",
                    "", "", "",
                    "", "", "", "", "",
                    "", "", "", "", "")
sub_hdr <- sprintf("%-28s %-18s %-18s | %-46s | %-46s",
                   "", "", "", "Actual Value", "Change from Baseline")
sep <- paste(rep("-", 150), collapse="")

lines <- c(lines, sub_hdr, col_hdr1, sep)

# One page per parameter (break after PARAM / page)
params_ordered <- report %>% distinct(PARAMN, PARAM) %>% arrange(PARAMN)
for (i in seq_len(nrow(params_ordered))) {
  pnam <- params_ordered$PARAM[i]
  pnum <- params_ordered$PARAMN[i]
  if (i > 1) {
    lines <- c(lines, "", "\f", t1, t2, t3, t4, t5, t6, "", hdr_note, "", sub_hdr, col_hdr1, sep)
  }
  lines <- c(lines, paste0("Parameter: ", pnam))
  sub <- report %>% filter(PARAM == pnam) %>% arrange(AVISITN, TRTAN)
  for (r in seq_len(nrow(sub))) {
    # truncate for display width but keep full numbers in CSV
    par_d <- substr(sub$PARAM[r], 1, 28)
    vis_d <- substr(sub$AVISIT[r], 1, 18)
    trt_d <- substr(sub$TRTA[r], 1, 18)
    line <- sprintf("%-28s %-18s %-18s | %-5s %-8s %-8s %-8s %-14s | %-5s %-8s %-8s %-8s %-14s",
                    par_d, vis_d, trt_d,
                    sub$stat1[r], sub$stat2[r], sub$stat3[r], sub$stat4[r], sub$stat5[r],
                    sub$chg1[r], sub$chg2[r], sub$chg3[r], sub$chg4[r], sub$chg5[r])
    lines <- c(lines, line)
  }
  lines <- c(lines, "")
}
lines <- c(lines, "", f1, f2, f3)

writeLines(lines, file.path(out, "t_vs.txt"))

cat("Done. Files written to OUT_DIR\n")
```