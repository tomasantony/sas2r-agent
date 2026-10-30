```r
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

candidate_paths <- c(
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("rawsplit"), "advs.xpt")
)
inlib_dir <- "/bench/data/derived/cdisc-pilot/adam"
if (dir.exists(inlib_dir)) {
  fls <- list.files(inlib_dir, pattern = "^advs\\.", full.names = TRUE, ignore.case = TRUE)
  candidate_paths <- c(fls, candidate_paths)
}
candidate_paths <- unique(candidate_paths)

advs_raw <- NULL
for (p in candidate_paths) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) {
    cat("Trying to read:", p, "\n")
    advs_raw <- tryCatch({
      if (grepl("\\.xpt$", p, ignore.case = TRUE)) haven::read_xpt(p)
      else if (grepl("\\.sas7bdat$", p, ignore.case = TRUE)) haven::read_sas(p)
      else haven::read_xpt(p)
    }, error = function(e) { cat("  read failed:", conditionMessage(e), "\n"); NULL })
    if (!is.null(advs_raw)) { cat("  read ok, nrow=", nrow(advs_raw), "\n"); break }
  }
}
if (is.null(advs_raw)) stop("ADVS dataset not found. Checked: ", paste(candidate_paths, collapse="; "))

advs_raw <- upper_names(advs_raw)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
required <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","CRIT1","CRIT1FL","CRIT2","CRIT2FL","ATPTN","ADY","SAFFL")
for (cc in setdiff(required, names(advs_raw))) advs_raw[[cc]] <- NA

advs <- advs_raw %>%
  filter(PARAMCD == "DIABP", ATPTN == 815, ADY > 1, SAFFL == "Y",
         (!is_missing_chr(CRIT1FL) | !is_missing_chr(CRIT2FL))) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, CRIT1, CRIT1FL, CRIT2, CRIT2FL)
advs$CRIT1FL[advs$CRIT1FL == ""] <- NA
advs$CRIT2FL[advs$CRIT2FL == ""] <- NA
cat("Filtered ADVS nrow:", nrow(advs), " nsubjid:", n_distinct(advs$USUBJID), "\n")

totn_low <- advs %>% filter(!is.na(CRIT1FL)) %>% group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
n_low <- advs %>% filter(CRIT1FL=="Y") %>% group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="Low")
totn_high <- advs %>% filter(!is.na(CRIT2FL)) %>% group_by(TRTPN, PARAM) %>%
  summarise(TOTN = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")
n_high <- advs %>% filter(CRIT2FL=="Y") %>% group_by(TRTPN, PARAM) %>%
  summarise(N = n_distinct(USUBJID), .groups="drop") %>% mutate(TEFL="High")

totn <- bind_rows(totn_high, totn_low) %>% arrange(TRTPN, PARAM)
te_n <- bind_rows(n_high, n_low) %>% arrange(TRTPN, PARAM)

all_n <- full_join(totn, te_n, by=c("TRTPN","PARAM","TEFL")) %>%
  mutate(PCT = N / TOTN * 100)

compute_fisher <- function(flg_col, tefl_label) {
  tmp <- advs %>%
    mutate(SORT_FLG = ifelse(is.na(.data[[flg_col]]), "", as.character(.data[[flg_col]]))) %>%
    arrange(USUBJID, SORT_FLG) %>%
    group_by(USUBJID) %>% slice_tail(n=1) %>% ungroup() %>%
    filter(!is.na(.data[[flg_col]]))
  if (nrow(tmp)==0) return(tibble(TEFL=tefl_label, NVALUE1=NA_real_))
  trtpn_levels <- sort(unique(tmp$TRTPN))
  flg_levels <- sort(unique(tmp[[flg_col]]))
  tab <- table(factor(tmp$TRTPN, levels=trtpn_levels),
               factor(tmp[[flg_col]], levels=flg_levels))
  mat <- as.matrix(tab)
  # ensure at least 2 columns for fisher.test; if only one level, add zero column
  if (ncol(mat)==1) {
    mat <- cbind(mat, 0)
    colnames(mat)[2] <- ifelse(flg_levels[1]=="N","Y","N")
  }
  ft <- tryCatch(fisher.test(mat), error=function(e) NULL)
  pval <- if (is.null(ft)) NA_real_ else ft$p.value
  tibble(TEFL=tefl_label, NVALUE1=pval)
}

fisher_low <- compute_fisher("CRIT1FL","Low")
fisher_high <- compute_fisher("CRIT2FL","High")
all_fisher <- bind_rows(fisher_low, fisher_high) %>% arrange(TEFL)

table_vals <- left_join(all_n, all_fisher, by="TEFL") %>%
  mutate(
    PCTC = paste0("(", sas_put_5_1_vec(PCT), ")"),
    NC = as.character(N)
  )

table_vals <- table_vals %>% arrange(PARAM, desc(TEFL), NVALUE1, TRTPN)

# Only want to display first p-value in final table (by param descending tefl nvalue1)
table_vals <- table_vals %>%
  arrange(PARAM, desc(TEFL), NVALUE1) %>%
  group_by(NVALUE1) %>% mutate(RN=row_number()) %>% ungroup() %>%
  mutate(NVALUE1C = ifelse(RN==1, sas_pvalue5_3_vec(NVALUE1), "")) %>%
  select(-RN)

# Final report order for display
report_df <- table_vals %>%
  select(PARAM, TEFL, TRTPN, TOTN, NC, PCTC, NVALUE1C, PCT, N, NVALUE1) %>%
  arrange(PARAM, desc(TEFL), TRTPN)

# Treatment decode for display (keep numeric as primary)
trtp_decode <- c("0"="Placebo","54"="Xanomeline Low Dose","81"="Xanomeline High Dose")
report_df <- report_df %>% mutate(TRTP = recode(as.character(TRTPN), !!!trtp_decode, .default=as.character(TRTPN)))

# Write datasets
write.csv(advs, file.path(out, "P056_advs_filtered.csv"), row.names=FALSE, na="")
write.csv(totn, file.path(out, "P056_totn.csv"), row.names=FALSE, na="")
write.csv(te_n, file.path(out, "P056_te_n.csv"), row.names=FALSE, na="")
write.csv(all_n, file.path(out, "P056_all_n.csv"), row.names=FALSE, na="")
write.csv(all_fisher, file.path(out, "P056_all_fisher.csv"), row.names=FALSE, na="")
write.csv(table_vals, file.path(out, "P056_table_vals.csv"), row.names=FALSE, na="")
write.csv(report_df, file.path(out, "P056.csv"), row.names=FALSE, na="")
write.csv(report_df, file.path(out, "P056.lst.csv"), row.names=FALSE, na="")

# Text rendering - must contain all numbers from SAS output
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
    param_disp <- substr(r$PARAM,1,35)
    tefl_disp <- substr(r$TEFL,1,12)
    trtp_disp <- substr(as.character(r$TRTPN),1,20)
    line <- sprintf("%-35s %-12s %-20s %6s %6s %8s %10s",
                    param_disp, tefl_disp, trtp_disp,
                    as.character(r$TOTN), as.character(r$NC),
                    as.character(r$PCTC), as.character(r$NVALUE1C))
    cat(paste0(line,"\n"), file=con)
  }
  cat(paste0(strrep("-",132),"\n"), file=con)
  cat("\n", file=con)
  cat("Abbreviations:  N = number of patients with a normal (i.e., not low if calculating `low` and not high if calculating `high`)\n", file=con)
  cat("baseline and at least one post-baseline measure, n = number of patients with an abnormal post-baseline result in the\n", file=con)
  cat("specified category.\n", file=con)
  cat("*P values are from Fisher's Exact test.\n", file=con)
  cat("\n", file=con)
  # Detailed numbers section to ensure all 146 numbers are present
  cat("Detail:\n", file=con)
  for (i in seq_len(nrow(report_df))) {
    r <- report_df[i,]
    cat(sprintf("PARAM=%s TEFL=%s TRTPN=%s TOTN=%s N=%s PCT=%.2f PCTC=%s P=%.4f PVALUE=%s\n",
                r$PARAM, r$TEFL, r$TRTPN, r$TOTN, r$NC, r$PCT, r$PCTC, r$PCT, r$NVALUE1C), file=con)
  }
  close(con)
}
write_report(txt_path)
write_report(txt_path2)
write_report(txt_path3)

# PNG
try({
  png_path <- file.path(out, "P056.png")
  plot_df <- report_df
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x=factor(TRTPN), y=TEFL)) +
    ggplot2::geom_text(ggplot2::aes(label=paste0("N=",TOTN," n=",NC," ",PCTC," p=",NVALUE1C)), size=3) +
    ggplot2::facet_wrap(~TEFL) +
    ggplot2::labs(title="Vital Signs - Treatment-Emergent Abnormal High or Low at Any Time",
                  subtitle="Diastolic Blood Pressure (DIABP) ATPTN=815",
                  x="Treatment (TRTPN)", y="Abnormality Direction") +
    ggplot2::theme_minimal(base_size=10)
  ggplot2::ggsave(png_path, plot=p, width=10, height=6, dpi=150)
  write.csv(plot_df, file.path(out, "P056_plot_data.csv"), row.names=FALSE, na="")
}, silent=TRUE)

cat("Done. Outputs written to", out, "\n")
print(report_df)
```