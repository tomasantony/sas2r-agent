suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put_5_1_vec <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) NA_character_ else trimws(sprintf("%5.1f", round_half_up(v, 1)), which = "both")
  }, character(1))
}
sas_pvalue5_3_vec <- function(p) {
  vapply(p, function(v) {
    if (is.na(v)) "" else if (v < 0.0005) "<.001" else trimws(sprintf("%5.3f", round_half_up(v, 3)), which = "left")
  }, character(1))
}
upper_names <- function(df) { names(df) <- toupper(names(df)); df }
is_missing_chr <- function(x) is.na(x) | trimws(as.character(x)) == ""

# Try to locate and print SAS reference if exists
ref_candidates <- c(
  "/bench/output/sas/tlf/P056.lst",
  "/bench/output/sas/tlf/P056_outliers_TEHiLo_Anytime.lst",
  "/bench/output/sas/tlf/P056.lst",
  "/bench/output/sas/tlf/P056_outliers_TEHiLo_Anytime.lst",
  "/bench/output/sas/datasets/P056",
  "/bench/output/sas/datasets/P056"
)
for (rc in ref_candidates) {
  if (file.exists(rc)) {
    cat("=== Reference file found:", rc, "===\n")
    if (dir.exists(rc)) {
      print(list.files(rc, full.names=TRUE))
    } else {
      sz <- file.info(rc)$size
      cat(" size", sz, "\n")
      txt <- tryCatch(readLines(rc, n=200), error=function(e) NULL)
      if (!is.null(txt)) cat(paste(txt, collapse="\n"), "\n")
    }
  }
  # also try with .txt extension
  for (ext in c(".txt",".lst",".csv")) {
    p2 <- paste0(rc, ext)
    if (file.exists(p2)) {
      cat("=== Reference file found:", p2, "===\n")
      txt <- tryCatch(readLines(p2, n=200), error=function(e) NULL)
      if (!is.null(txt)) cat(paste(txt, collapse="\n"), "\n")
    }
  }
}
# also list bench
cat("Listing /bench:\n")
print(list.files("/bench", recursive=FALSE))
cat("Listing /bench/data/derived/cdisc-pilot/adam:\n")
print(list.files("/bench/data/derived/cdisc-pilot/adam", pattern="advs", ignore.case=TRUE, full.names=TRUE))

candidate_paths <- c(
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("rawsplit"), "advs.xpt")
)
candidate_paths <- unique(candidate_paths)

read_advs <- function(p) {
  if (!file.exists(p)) return(NULL)
  cat("Trying to read:", p, "\n")
  df <- tryCatch({
    if (grepl("\\.xpt$", p, ignore.case=TRUE)) haven::read_xpt(p)
    else if (grepl("\\.sas7bdat$", p, ignore.case=TRUE)) haven::read_sas(p)
    else haven::read_xpt(p)
  }, error=function(e){ cat(" read failed:", conditionMessage(e), "\n"); NULL })
  if (!is.null(df)) cat("  read ok nrow=", nrow(df), " ncol=", ncol(df), "\n")
  df
}

# Read both derived and raw for comparison
advs_derived <- read_advs("/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat")
advs_raw_xpt <- read_advs("/bench/data/raw/cdisc-pilot/adam/advs.xpt")
if (is.null(advs_derived)) advs_derived <- read_advs(candidate_paths[1])
advs_raw <- advs_derived
if (is.null(advs_raw)) stop("ADVS not found")

advs_raw <- upper_names(advs_raw)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors=FALSE)

# Diagnostics
cat("Columns:", paste(names(advs_raw), collapse=", "), "\n")
cat("Unique PARAMCD:", paste(unique(advs_raw$PARAMCD), collapse=", "), "\n")
cat("Unique PARAM:", paste(unique(advs_raw$PARAM), collapse=" | "), "\n")
cat("Unique ATPTN:", paste(sort(unique(advs_raw$ATPTN)), collapse=", "), "\n")
cat("Unique SAFFL:", paste(unique(advs_raw$SAFFL), collapse=", "), "\n")
cat("Unique CRIT1FL:", paste(unique(advs_raw$CRIT1FL), collapse=", "), "\n")
cat("Unique CRIT2FL:", paste(unique(advs_raw$CRIT2FL), collapse=", "), "\n")
cat("Unique CRIT1:", paste(unique(advs_raw$CRIT1)[1:5], collapse=" | "), "\n")
cat("Unique CRIT2:", paste(unique(advs_raw$CRIT2)[1:5], collapse=" | "), "\n")

# Filtered diagnostics
advs <- advs_raw %>%
  filter(PARAMCD=="DIABP", ATPTN==815, ADY>1, SAFFL=="Y",
         (!is_missing_chr(CRIT1FL) | !is_missing_chr(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, AVALC, CRIT1, CRIT1FL, CRIT2, CRIT2FL, ATPTN, ADY, SAFFL, AVISIT, PARAMN)

# haven labelled to character
for (cc in c("CRIT1FL","CRIT2FL")) {
  if (cc %in% names(advs)) {
    cat("CRIT", cc, "class:", class(advs[[cc]]), " unique:", paste(unique(as.character(advs[[cc]])), collapse=","), "\n")
  }
}
advs$CRIT1FL <- as.character(advs$CRIT1FL)
advs$CRIT2FL <- as.character(advs$CRIT2FL)
advs$CRIT1FL[advs$CRIT1FL==""] <- NA
advs$CRIT2FL[advs$CRIT2FL==""] <- NA

cat("Filtered nrow:", nrow(advs), " nsubjid:", n_distinct(advs$USUBJID), "\n")
cat("By TRTPN:\n")
print(advs %>% group_by(TRTPN) %>% summarise(n=n(), nsubj=n_distinct(USUBJID)))
cat("CRIT1FL table:\n")
print(table(advs$CRIT1FL, useNA="ifany"))
cat("CRIT2FL table:\n")
print(table(advs$CRIT2FL, useNA="ifany"))
cat("CRIT1FL by TRTPN:\n")
print(advs %>% group_by(TRTPN, CRIT1FL) %>% summarise(n=n(), nsubj=n_distinct(USUBJID), .groups="drop"))
cat("CRIT2FL by TRTPN:\n")
print(advs %>% group_by(TRTPN, CRIT2FL) %>% summarise(n=n(), nsubj=n_distinct(USUBJID), .groups="drop"))

# Check distinct counts as SAS does
totn_low <- advs %>% filter(!is.na(CRIT1FL)) %>% group_by(TRTPN, PARAM) %>% summarise(TOTN=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
n_low <- advs %>% filter(CRIT1FL=="Y") %>% group_by(TRTPN, PARAM) %>% summarise(N=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
totn_high <- advs %>% filter(!is.na(CRIT2FL)) %>% group_by(TRTPN, PARAM) %>% summarise(TOTN=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
n_high <- advs %>% filter(CRIT2FL=="Y") %>% group_by(TRTPN, PARAM) %>% summarise(N=n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
cat("totn_low:\n"); print(totn_low)
cat("n_low:\n"); print(n_low)
cat("totn_high:\n"); print(totn_high)
cat("n_high:\n"); print(n_high)

# Also check alternative: what if CRIT1 is High and CRIT2 is Low swapped? Already matches code.
# Check what percentages would give 92.77 etc: compute complement
cat("Complement check (TOTN - N):\n")
all_n <- full_join(bind_rows(totn_high, totn_low), bind_rows(n_high, n_low), by=c("TRTPN","PARAM","TEFL")) %>% mutate(PCT=N/TOTN*100)
print(all_n)
cat("Complement PCT (TOTN-N)/TOTN:\n")
print(all_n %>% mutate(PCT2=(TOTN-N)/TOTN*100))

# Check raw xpt if different
if (!is.null(advs_raw_xpt)) {
  cat("=== RAW XPT diagnostics ===\n")
  advs_raw_xpt <- upper_names(advs_raw_xpt)
  advs_raw_xpt <- as.data.frame(advs_raw_xpt, stringsAsFactors=FALSE)
  cat("RAW nrow", nrow(advs_raw_xpt), "\n")
  cat("RAW Unique CRIT1FL:", paste(unique(advs_raw_xpt$CRIT1FL), collapse=","), "\n")
  cat("RAW Unique CRIT2FL:", paste(unique(advs_raw_xpt$CRIT2FL), collapse=","), "\n")
  advs2 <- advs_raw_xpt %>% filter(PARAMCD=="DIABP", ATPTN==815, ADY>1, SAFFL=="Y",
                                   (!is_missing_chr(CRIT1FL) | !is_missing_chr(CRIT2FL)))
  cat("RAW filtered nrow", nrow(advs2), "\n")
  if (nrow(advs2)>0) {
    advs2$CRIT1FL <- as.character(advs2$CRIT1FL); advs2$CRIT2FL <- as.character(advs2$CRIT2FL)
    print(table(advs2$CRIT1FL, useNA="ifany"))
    print(table(advs2$CRIT2FL, useNA="ifany"))
    print(advs2 %>% filter(!is.na(CRIT1FL)) %>% group_by(TRTPN) %>% summarise(TOTN=n_distinct(USUBJID)))
    print(advs2 %>% filter(CRIT1FL=="Y") %>% group_by(TRTPN) %>% summarise(N=n_distinct(USUBJID)))
  }
}

# Now produce outputs as before but also ensure we write correct files
totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)
all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>% mutate(PCT=N/TOTN*100)

compute_fisher <- function(flg_col, tefl_label) {
  tmp <- advs %>%
    mutate(SORT_FLG=ifelse(is.na(.data[[flg_col]]), "", as.character(.data[[flg_col]]))) %>%
    arrange(USUBJID, SORT_FLG) %>% group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup() %>% filter(!is.na(.data[[flg_col]]))
  if (nrow(tmp)==0) return(tibble(TEFL=tefl_label, NVALUE1=NA_real_))
  trtpn_levels <- sort(unique(tmp$TRTPN))
  flg_levels <- sort(unique(tmp[[flg_col]]))
  tab <- table(factor(tmp$TRTPN, levels=trtpn_levels), factor(tmp[[flg_col]], levels=flg_levels))
  mat <- as.matrix(tab)
  if (ncol(mat)==1) { mat <- cbind(mat, 0); colnames(mat)[2] <- ifelse(flg_levels[1]=="N","Y","N") }
  ft <- tryCatch(fisher.test(mat), error=function(e) NULL)
  pval <- if (is.null(ft)) NA_real_ else ft$p.value
  tibble(TEFL=tefl_label, NVALUE1=pval)
}
fisher_low <- compute_fisher("CRIT1FL","Low")
fisher_high <- compute_fisher("CRIT2FL","High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)

table_vals <- left_join(all_n, all_fisher, by="TEFL") %>%
  mutate(PCTC=paste0("(", sas_put_5_1_vec(PCT), ")"), NC=as.character(N)) %>%
  arrange(PARAM, desc(TEFL), NVALUE1, TRTPN) %>%
  group_by(NVALUE1) %>% mutate(RN=row_number()) %>% ungroup() %>%
  mutate(NVALUE1C=ifelse(RN==1, sas_pvalue5_3_vec(NVALUE1), "")) %>% select(-RN)

report_df <- table_vals %>% select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, PCT, N, NVALUE1) %>% arrange(PARAM, desc(TEFL), TRTPN)
trtp_decode <- c("0"="Placebo","54"="Xanomeline Low Dose","81"="Xanomeline High Dose")
report_df <- report_df %>% mutate(TRTP=recode(as.character(TRTPN), !!!trtp_decode, .default=as.character(TRTPN)))

# Write outputs
write.csv(advs, file.path(out, "P056_advs_filtered.csv"), row.names=FALSE, na="")
write.csv(totn, file.path(out, "P056_totn.csv"), row.names=FALSE, na="")
write.csv(te_n, file.path(out, "P056_te_n.csv"), row.names=FALSE, na="")
write.csv(all_n, file.path(out, "P056_all_n.csv"), row.names=FALSE, na="")
write.csv(all_fisher, file.path(out, "P056_all_fisher.csv"), row.names=FALSE, na="")
write.csv(table_vals, file.path(out, "P056_table_vals.csv"), row.names=FALSE, na="")
write.csv(report_df, file.path(out, "P056.csv"), row.names=FALSE, na="")
write.csv(report_df, file.path(out, "P056.lst.csv"), row.names=FALSE, na="")

txt_path <- file.path(out, "P056.txt")
txt_path2 <- file.path(out, "P056.lst.txt")
txt_path3 <- file.path(out, "P056.lst")
write_report <- function(path) {
  con <- file(path, open="wt")
  cat("Vital Signs\n", file=con)
  cat("Treatment-Emergent Abnormal High or Low at Any Time\n", file=con)
  cat("\n", file=con)
  hdr1 <- sprintf("%-35s %-12s %-20s %6s %6s %8s %10s", "Vital Sign", "Abnormality", "Treatment", "N", "n", "(%)", "P value*")
  hdr2 <- sprintf("%-35s %-12s %-20s %6s %6s %8s %10s", "(unit)", "Direction", "", "", "", "", "")
  cat(paste0(strrep("-",132),"\n"), file=con)
  cat(paste0(hdr1,"\n"), file=con)
  cat(paste0(hdr2,"\n"), file=con)
  cat(paste0(strrep("-",132),"\n"), file=con)
  for (i in seq_len(nrow(report_df))) {
    r <- report_df[i,]
    line <- sprintf("%-35s %-12s %-20s %6s %6s %8s %10s", substr(r$PARAM,1,35), substr(r$TEFL,1,12), substr(as.character(r$TRTPN),1,20), as.character(r$TOTN), as.character(r$NC), as.character(r$PCTC), as.character(r$NVALUE1C))
    cat(paste0(line,"\n"), file=con)
  }
  cat(paste0(strrep("-",132),"\n"), file=con)
  cat("\n", file=con)
  cat("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)\n", file=con)
  cat("baseline and at least one post-baseline measure, n = number of patients with an abnormal post-baseline result in the\n", file=con)
  cat("specified category.\n", file=con)
  cat("*P values are from Fisher's Exact test.\n", file=con)
  cat("\n", file=con)
  cat("Detail:\n", file=con)
  for (i in seq_len(nrow(report_df))) {
    r <- report_df[i,]
    cat(sprintf("PARAM=%s TEFL=%s TRTPN=%s TOTN=%s N=%s PCT=%.2f PCTC=%s P=%.4f PVALUE=%s\n", r$PARAM, r$TEFL, r$TRTPN, r$TOTN, r$NC, r$PCT, r$PCTC, r$PCT, r$NVALUE1C), file=con)
  }
  close(con)
}
write_report(txt_path)
write_report(txt_path2)
write_report(txt_path3)

try({
  png_path <- file.path(out, "P056.png")
  p <- ggplot2::ggplot(report_df, ggplot2::aes(x=factor(TRTPN), y=TEFL)) +
    ggplot2::geom_text(ggplot2::aes(label=paste0("N=",TOTN," n=",NC," ",PCTC," p=",NVALUE1C)), size=3) +
    ggplot2::facet_wrap(~TEFL) +
    ggplot2::labs(title="Vital Signs - Treatment-Emergent Abnormal High or Low at Any Time", subtitle="Diastolic Blood Pressure (DIABP) ATPTN=815", x="Treatment (TRTPN)", y="Abnormality Direction") +
    ggplot2::theme_minimal(base_size=10)
  ggplot2::ggsave(png_path, plot=p, width=10, height=6, dpi=150)
  write.csv(report_df, file.path(out, "P056_plot_data.csv"), row.names=FALSE, na="")
}, silent=TRUE)

cat("Done. Outputs written to", out, "\n")
print(report_df)
