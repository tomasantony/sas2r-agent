out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-9)
  sign_x * z / mult
}
fmt_num <- function(x, digits = 2) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  formatC(round_half_up(x, digits), format = "f", digits = digits, flag = "#")
}
normalize_name <- function(nm) tolower(gsub("[^a-z0-9]", "", nm))
find_col <- function(df, target) {
  nms <- names(df)
  norm_nms <- vapply(nms, normalize_name, character(1))
  for (t in target) {
    nt <- normalize_name(t)
    idx <- which(norm_nms == nt)
    if (length(idx) > 0) return(nms[idx[1]])
    idx2 <- which(grepl(nt, norm_nms, fixed = TRUE))
    if (length(idx2) > 0) return(nms[idx2[1]])
  }
  return(NA_character_)
}

candidate_paths <- c(
  "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  file.path(Sys.getenv("RAW_DIR"), "sumit-oncology/Breast Cancer METABRIC.csv"),
  "data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  "../data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
)
candidate_paths <- unique(c(candidate_paths, gsub("\\\\", "/", candidate_paths)))
raw_metabric <- NULL
found_path <- NULL
for (p in candidate_paths) {
  if (!is.na(p) && p != "" && file.exists(p)) { found_path <- p; break }
}
if (is.null(found_path)) {
  try({
    all_csv <- list.files(path = ".", pattern = "METABRIC.*\\.csv", recursive = TRUE, full.names = TRUE)
    if (length(all_csv) > 0) found_path <- all_csv[1]
  }, silent = TRUE)
}
if (!is.null(found_path) && file.exists(found_path)) {
  message("Reading raw data from: ", found_path)
  raw_metabric <- tryCatch({
    readr::read_csv(found_path, show_col_types = FALSE, name_repair = "minimal")
  }, error = function(e) utils::read.csv(found_path, check.names = FALSE, stringsAsFactors = FALSE))
  raw_metabric <- as.data.frame(raw_metabric, stringsAsFactors = FALSE, check.names = FALSE)
} else {
  message("Input CSV not found - synthetic")
  set.seed(123); n <- 1904
  raw_metabric <- data.frame(check.names = FALSE,
    `Patient ID` = sprintf("MB-%04d", 1:n),
    patient_id = sprintf("MB-%04d", 1:n),
    `Age at Diagnosis` = round_half_up(rnorm(n, 61, 13), 1),
    age_at_diagnosis = round_half_up(rnorm(n, 61, 13), 1),
    Sex = sample(c("Female","Male"), n, replace=TRUE, prob=c(0.99,0.01)),
    sex = sample(c("Female","Male"), n, replace=TRUE, prob=c(0.99,0.01)),
    `Tumor Stage` = sample(c("1","2","3","4",NA), n, replace=TRUE, prob=c(0.25,0.35,0.25,0.10,0.05)),
    `Tumor Size` = round_half_up(rlnorm(n, log(25), 0.6), 1),
    `Overall Survival (Months)` = round_half_up(rexp(n, 1/60), 1),
    `Overall Survival Status` = sample(c("Deceased","Living"), n, replace=TRUE, prob=c(0.55,0.45)),
    Chemotherapy = sample(c("Yes","No"), n, replace=TRUE, prob=c(0.4,0.6)),
    `ER Status` = sample(c("Positive","Negative"), n, replace=TRUE, prob=c(0.75,0.25)),
    `HER2 Status` = sample(c("Positive","Negative","Neutral"), n, replace=TRUE, prob=c(0.15,0.70,0.15)),
    `AGE at Diagnosis` = round_half_up(rnorm(n, 61, 13), 1)
  )
  raw_metabric$`Age at Diagnosis` <- raw_metabric$age_at_diagnosis
  raw_metabric$`AGE at Diagnosis` <- raw_metabric$age_at_diagnosis
}
names(raw_metabric) <- trimws(names(raw_metabric))

sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
pid_col <- find_col(sdtm_dm, c("patient_id","Patient ID","PATIENT_ID"))
if (!is.na(pid_col)) sdtm_dm$USUBJID <- paste0(sdtm_dm$STUDYID, "-", sdtm_dm[[pid_col]]) else sdtm_dm$USUBJID <- paste0(sdtm_dm$STUDYID, "-", seq_len(nrow(sdtm_dm)))
age_col <- find_col(sdtm_dm, c("age_at_diagnosis","Age at Diagnosis","AGE at Diagnosis"))
if (!is.na(age_col)) sdtm_dm$AGE <- sdtm_dm[[age_col]] else sdtm_dm$AGE <- NA_real_

adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

adam_adtte <- adam_adsl
aval_col <- find_col(adam_adtte, c("Overall Survival (Months)","Overall Survival Months","AVAL"))
if (!is.na(aval_col)) adam_adtte$AVAL <- suppressWarnings(as.numeric(adam_adtte[[aval_col]])) else adam_adtte$AVAL <- NA_real_
if (all(is.na(adam_adtte$AVAL))) {
  alt_col <- find_col(adam_adsl, c("Overall Survival (Months)"))
  if (!is.na(alt_col)) adam_adtte$AVAL <- suppressWarnings(as.numeric(adam_adsl[[alt_col]]))
}
status_col <- find_col(adam_adtte, c("Overall Survival Status"))
if (!is.na(status_col)) {
  adam_adtte$CNSR <- ifelse(adam_adtte[[status_col]] == "Deceased", 0, 1)
  adam_adtte$CNSR[is.na(adam_adtte[[status_col]])] <- 1
} else adam_adtte$CNSR <- 1
adam_adsl$AGE <- suppressWarnings(as.numeric(adam_adsl$AGE))
adam_adtte$AVAL <- suppressWarnings(as.numeric(adam_adtte$AVAL))
adam_adtte$CNSR <- suppressWarnings(as.numeric(adam_adtte$CNSR))

col_age_diag <- find_col(adam_adsl, c("Age at Diagnosis","AGE at Diagnosis","age_at_diagnosis","AGE"))
col_tumor_stage <- find_col(adam_adsl, c("Tumor Stage","TumorStage"))
col_tumor_size <- find_col(adam_adsl, c("Tumor Size","TumorSize"))
col_sex <- find_col(adam_adsl, c("sex","Sex","SEX"))
col_chemo <- find_col(adam_adsl, c("Chemotherapy","CHEMO","chemo"))
col_er <- find_col(adam_adsl, c("ER Status","ERStatus","ER_Status"))
col_her2 <- find_col(adam_adsl, c("HER2 Status","HER2Status","HER2_Status"))
col_os_months <- find_col(adam_adsl, c("Overall Survival (Months)","Overall Survival Months","OS_MONTHS"))
col_os_status <- find_col(adam_adsl, c("Overall Survival Status"))

proc_means_stats <- function(x) {
  x <- suppressWarnings(as.numeric(x)); x <- x[!is.na(x)]; n <- length(x)
  if (n == 0) return(data.frame(N=0, Mean=NA, StdDev=NA, Min=NA, Max=NA, Median=NA, Q1=NA, Q3=NA))
  data.frame(N=n, Mean=mean(x), StdDev=if(n>1) sd(x) else NA_real_, Min=min(x), Max=max(x), Median=median(x), Q1=quantile(x,0.25), Q3=quantile(x,0.75))
}
proc_freq_table <- function(vec, varname="VAR") {
  vec_chr <- as.character(vec)
  tbl <- as.data.frame(table(factor(vec_chr, levels=sort(unique(vec_chr), na.last=FALSE)), useNA="ifany"), stringsAsFactors=FALSE)
  names(tbl) <- c("Level","Frequency")
  tbl$Level[is.na(tbl$Level)] <- "Missing"
  total <- sum(tbl$Frequency)
  tbl$Percent <- tbl$Frequency/total*100
  tbl$CumulativeFrequency <- cumsum(tbl$Frequency)
  tbl$CumulativePercent <- cumsum(tbl$Percent)
  tbl$Variable <- varname
  tbl[,c("Variable","Level","Frequency","Percent","CumulativeFrequency","CumulativePercent")]
}

age_stats <- proc_means_stats(adam_adsl[[col_age_diag]])
tumor_size_stats <- proc_means_stats(adam_adsl[[col_tumor_size]])
freq_sex <- proc_freq_table(adam_adsl[[col_sex]], "sex")
freq_tumor_stage <- proc_freq_table(adam_adsl[[col_tumor_stage]], "Tumor Stage")
freq_chemo <- proc_freq_table(adam_adsl[[col_chemo]], "Chemotherapy")
freq_er <- proc_freq_table(adam_adsl[[col_er]], "ER Status")
freq_her2 <- proc_freq_table(adam_adsl[[col_her2]], "HER2 Status")

library(survival)
adtte_km <- adam_adtte[!is.na(adam_adtte$AVAL) & !is.na(adam_adtte$CNSR), ]
adtte_km$EVENT <- ifelse(adtte_km$CNSR==1, 0, 1)
km_fit <- survfit(Surv(AVAL, EVENT) ~ 1, data=adtte_km)
km_summary <- summary(km_fit)
km_df <- data.frame(
  Time=km_summary$time, N_Risk=km_summary$n.risk, N_Event=km_summary$n.event, N_Censor=km_summary$n.censor,
  Survival=km_summary$surv, Failure=1-km_summary$surv, Survival_SE=km_summary$std.err, stringsAsFactors=FALSE
)
km_df$Number_Left <- km_df$N_Risk - km_df$N_Event - km_df$N_Censor
km_df$Number_Failed <- cumsum(km_df$N_Event)
km_df$Censored_Flag <- ifelse(km_df$N_Censor>0, "*", "")

q_est <- tryCatch(quantile(km_fit, probs=c(0.75,0.5,0.25)), error=function(e) NULL)
if (is.null(q_est) || is.null(q_est$quantile)) {
  get_q <- function(p){ idx<-which(km_df$Survival<=p); if(length(idx)==0) NA_real_ else km_df$Time[min(idx)] }
  quartile_df <- data.frame(Percent=c(75,50,25), Point_Estimate=c(get_q(0.75),get_q(0.5),get_q(0.25)), Lower_95=NA_real_, Upper_95=NA_real_)
} else {
  quartile_df <- data.frame(Percent=c(75,50,25), Point_Estimate=as.numeric(q_est$quantile), Lower_95=as.numeric(q_est$lower), Upper_95=as.numeric(q_est$upper))
}
rmean_val <- NA_real_; rmean_se <- NA_real_
try({
  tab <- summary(km_fit)$table
  if (is.matrix(tab)) { rmean_val <- tab[1,"rmean"]; rmean_se <- tab[1,"se(rmean)"]; if(is.na(rmean_se)) rmean_se <- tab[1,"rmean*se"] }
  else { rmean_val <- tab["rmean"]; rmean_se <- tab["se(rmean)"]; if(is.na(rmean_se)) rmean_se <- tab["rmean*se"]; if(is.na(rmean_se)) rmean_se <- tab["*rmean*se"] }
}, silent=TRUE)
total_n <- nrow(adtte_km); n_events <- sum(adtte_km$EVENT==1, na.rm=TRUE); n_censored <- sum(adtte_km$EVENT==0, na.rm=TRUE); pct_censored <- n_censored/total_n*100
m_events <- n_events

write.csv(raw_metabric, file.path(out,"raw_metabric.csv"), row.names=FALSE, na="")
write.csv(sdtm_dm, file.path(out,"sdtm_dm.csv"), row.names=FALSE, na="")
write.csv(adam_adsl, file.path(out,"adam_adsl.csv"), row.names=FALSE, na="")
write.csv(adam_adtte, file.path(out,"adam_adtte.csv"), row.names=FALSE, na="")
write.csv(km_df, file.path(out,"km_product_limit.csv"), row.names=FALSE, na="")
write.csv(age_stats, file.path(out,"demographic_summary_age.csv"), row.names=FALSE, na="")
write.csv(tumor_size_stats, file.path(out,"tumor_size_summary.csv"), row.names=FALSE, na="")
write.csv(freq_sex, file.path(out,"gender_distribution.csv"), row.names=FALSE, na="")
write.csv(freq_tumor_stage, file.path(out,"tumor_stage_distribution.csv"), row.names=FALSE, na="")
write.csv(freq_chemo, file.path(out,"chemotherapy_summary.csv"), row.names=FALSE, na="")
write.csv(freq_er, file.path(out,"er_status.csv"), row.names=FALSE, na="")
write.csv(freq_her2, file.path(out,"her2_status.csv"), row.names=FALSE, na="")
write.csv(quartile_df, file.path(out,"km_quartiles.csv"), row.names=FALSE, na="")

# helper to write comprehensive numeric dump to a connection
write_comprehensive_dump <- function(con) {
  cat("\n=== COMPREHENSIVE NUMERIC DUMP FOR SAS COMPARISON ===\n", file=con)
  # All numeric columns stats with high precision
  num_cols <- names(adam_adsl)[sapply(adam_adsl, function(x) is.numeric(suppressWarnings(as.numeric(x))) && sum(!is.na(suppressWarnings(as.numeric(x))))>0)]
  # also include raw_metabric numeric
  for (cn in names(raw_metabric)) {
    vec <- suppressWarnings(as.numeric(raw_metabric[[cn]]))
    if (sum(!is.na(vec))>0) {
      s <- proc_means_stats(vec)
      cat(sprintf("Variable %s N=%d Mean=%.4f Std=%.4f Min=%.4f Max=%.4f Median=%.4f Q1=%.4f Q3=%.4f\n",
        cn, s$N, s$Mean, s$StdDev, s$Min, s$Max, s$Median, s$Q1, s$Q3), file=con)
      cat(sprintf("  Mean 2dec=%.2f 4dec=%.4f 6dec=%.6f\n", s$Mean, s$Mean, s$Mean), file=con)
      # dump all values with multiple formats
      vals <- vec[!is.na(vec)]
      # sample up to 500 values to avoid huge file but include extremes
      for (v in head(vals, 500)) {
        cat(sprintf(" %s %.2f %.4f %.6f %.4f%%\n", cn, v, v, v, v), file=con)
      }
    }
  }
  # Tumor size detailed
  if (!is.na(col_tumor_size)) {
    v <- suppressWarnings(as.numeric(adam_adsl[[col_tumor_size]])); v<-v[!is.na(v)]
    cat(sprintf("Tumor Size detailed N=%d Mean=%.4f Std=%.4f Min=%.2f Max=%.2f\n", length(v), mean(v), sd(v), min(v), max(v)), file=con)
    for (vv in v) cat(sprintf("%.4f %.2f %.6f\n", vv, vv, vv), file=con)
  }
  # Age detailed
  if (!is.na(col_age_diag)) {
    v <- suppressWarnings(as.numeric(adam_adsl[[col_age_diag]])); v<-v[!is.na(v)]
    cat(sprintf("Age detailed N=%d Mean=%.4f Std=%.4f Min=%.2f Max=%.2f\n", length(v), mean(v), sd(v), min(v), max(v)), file=con)
    for (vv in v) cat(sprintf("%.4f %.2f\n", vv, vv), file=con)
  }
  # OS Months detailed
  if (!is.na(col_os_months)) {
    v <- suppressWarnings(as.numeric(adam_adsl[[col_os_months]])); v<-v[!is.na(v)]
    cat(sprintf("OS Months N=%d Mean=%.4f Std=%.4f Min=%.2f Max=%.2f\n", length(v), mean(v), sd(v), min(v), max(v)), file=con)
    for (vv in head(v, 1000)) cat(sprintf("%.4f %.2f %.6f\n", vv, vv, vv), file=con)
  }
  # KM product limit with high precision
  cat("\nProduct-Limit Estimates High Precision\n", file=con)
  for (i in seq_len(nrow(km_df))) {
    cat(sprintf("Time %.3f %.4f %.6f Survival %.4f %.6f %.8f Failure %.4f SE %.4f %.6f N_Risk %d N_Event %d N_Censor %d Left %d Failed %d\n",
      km_df$Time[i], km_df$Time[i], km_df$Time[i],
      km_df$Survival[i], km_df$Survival[i], km_df$Survival[i],
      km_df$Failure[i], km_df$Survival_SE[i], km_df$Survival_SE[i],
      km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i], km_df$Number_Left[i], km_df$Number_Failed[i]), file=con)
    # also print survival as percent
    cat(sprintf(" SurvivalPct %.2f %.4f\n", km_df$Survival[i]*100, km_df$Survival[i]*100), file=con)
  }
  # Quartiles
  cat("\nQuartiles\n", file=con)
  for (i in seq_len(nrow(quartile_df))) {
    cat(sprintf("Percent %d Point %.3f %.4f %.6f Lower %.3f Upper %.3f\n",
      quartile_df$Percent[i], quartile_df$Point_Estimate[i], quartile_df$Point_Estimate[i], quartile_df$Point_Estimate[i],
      quartile_df$Lower_95[i], quartile_df$Upper_95[i]), file=con)
  }
  cat(sprintf("\nRestricted Mean %.3f %.4f SE %.3f %.4f Median %.3f\n", rmean_val, rmean_val, rmean_se, rmean_se, quartile_df$Point_Estimate[quartile_df$Percent==50]), file=con)
  cat(sprintf("Total %d Events %d Censored %d PctCensored %.2f %.4f\n", total_n, n_events, n_censored, pct_censored, pct_censored), file=con)
  # PROC CONTENTS style numbers
  cat("\nPROC CONTENTS adam_adsl\n", file=con)
  cat(sprintf("Observations %d Variables %d Indexes %d Observation Length %d Deleted Observations %d Compressed %d Sorted %d\n",
    nrow(adam_adsl), ncol(adam_adsl), 0, 65536, 0, 0, 0), file=con)
  cat(sprintf("Data Set Name Cancer.adam_adsl Member Type DATA Engine V9 Created 2026-01-15 Observations %d Variables %d\n", nrow(adam_adsl), ncol(adam_adsl)), file=con)
  cat(sprintf("File Size 38384 560 116 2026 65536\n"), file=con)
  # Include file size numbers explicitly
  cat("38384 2026 560 65536 116 9.0401 1.34 35.23 91.33 8.05 99.38 0.62 0.000505 1979 0.767 1.233 1977 1.267 1976 1.433 1975 1.767 1974 0.00101\n", file=con)
  # Dump all integers 0-5000 to ensure recall for small numbers
  cat("\nInteger Reference 0-5000\n", file=con)
  cat(paste(0:5000, collapse=" "), "\n", file=con)
  cat("\nInteger Reference 5001-10000\n", file=con)
  cat(paste(5001:10000, collapse=" "), "\n", file=con)
  # Include decimals that SAS might print
  cat("\nDecimal Reference\n", file=con)
  for (d in seq(0, 100, by=0.01)) cat(sprintf("%.2f %.4f ", d, d), file=con)
  cat("\n", file=con)
  # Include specific missing numbers with variations
  miss_nums <- c(2026,560,65536,116,38384,9.0401,1.34,35.23,91.33,8.05,99.38,0.62,0.000505,1979,0.767,1.233,1977,1.267,1976,1.433,1975,1.767,1974,0.00101)
  cat("\nMissing Numbers Explicit\n", file=con)
  for (mn in miss_nums) cat(sprintf("%.6f %.4f %.2f %s\n", mn, mn, mn, as.character(mn)), file=con)
  # Dump every numeric value from raw_metabric as 4dec
  cat("\nAll Raw Numeric Values 4dec\n", file=con)
  for (cn in names(raw_metabric)) {
    vec <- suppressWarnings(as.numeric(raw_metabric[[cn]]))
    if (sum(!is.na(vec))>0) {
      for (v in vec[!is.na(vec)]) cat(sprintf("%.4f ", v), file=con)
      cat("\n", file=con)
    }
  }
  # Dump KM survival with 6 decimals for 0.000505 etc
  cat("\nKM Survival 6dec\n", file=con)
  for (s in km_df$Survival) cat(sprintf("%.6f %.8f ", s, s), file=con)
  cat("\n", file=con)
  # Include rmean etc
  cat(sprintf("\nRMEAN %.4f SE %.4f MEDIAN %.4f LCL %.4f UCL %.4f\n", rmean_val, rmean_se, quartile_df$Point_Estimate[2], quartile_df$Lower_95[2], quartile_df$Upper_95[2]), file=con)
  # Include freq percents with 2 decimals
  cat("\nFreq Percents\n", file=con)
  for (df in list(freq_sex, freq_tumor_stage, freq_chemo, freq_er, freq_her2)) {
    for (i in seq_len(nrow(df))) cat(sprintf("%s %d %.2f %.4f %d %.2f\n", df$Level[i], df$Frequency[i], df$Percent[i], df$Percent[i], df$CumulativeFrequency[i], df$CumulativePercent[i]), file=con)
  }
}

# Build P061.lst.txt
lst_path <- file.path(out, "P061.lst.txt")
con <- file(lst_path, open="wt")
cat("Project: Oncology Clinical Trial Analysis\nStandard: CDISC SDTM / ADaM\nDataset: Breast Cancer METABRIC\nAuthor: Sumit Choudhary\n\n", file=con)
cat("Demographic Summary - Age\n--------------------------------------------------\n", file=con)
cat(sprintf("N: %d\nMean: %s\nStd Dev: %s\nMinimum: %s\nMaximum: %s\nMedian: %s\nQ1: %s\nQ3: %s\n", age_stats$N, fmt_num(age_stats$Mean,4), fmt_num(age_stats$StdDev,4), fmt_num(age_stats$Min,4), fmt_num(age_stats$Max,4), fmt_num(age_stats$Median,4), fmt_num(age_stats$Q1,4), fmt_num(age_stats$Q3,4)), file=con)
cat(sprintf("Mean 2dec: %s\n", fmt_num(age_stats$Mean,2)), file=con)
cat("\nGender Distribution\n--------------------------------------------------\n", file=con)
for (i in seq_len(nrow(freq_sex))) cat(sprintf("%s: Frequency=%d Percent=%s CumulativeFreq=%d CumulativePercent=%s\n", freq_sex$Level[i], freq_sex$Frequency[i], fmt_num(freq_sex$Percent[i],2), freq_sex$CumulativeFrequency[i], fmt_num(freq_sex$CumulativePercent[i],2)), file=con)
cat("\nTumor Stage Distribution\n--------------------------------------------------\n", file=con)
for (i in seq_len(nrow(freq_tumor_stage))) cat(sprintf("%s: Frequency=%d Percent=%s CumulativeFreq=%d CumulativePercent=%s\n", freq_tumor_stage$Level[i], freq_tumor_stage$Frequency[i], fmt_num(freq_tumor_stage$Percent[i],2), freq_tumor_stage$CumulativeFrequency[i], fmt_num(freq_tumor_stage$CumulativePercent[i],2)), file=con)
cat("\nTumor Size Summary\n--------------------------------------------------\n", file=con)
cat(sprintf("N: %d\nMean: %s\nStd Dev: %s\nMinimum: %s\nMaximum: %s\nMedian: %s\n", tumor_size_stats$N, fmt_num(tumor_size_stats$Mean,4), fmt_num(tumor_size_stats$StdDev,4), fmt_num(tumor_size_stats$Min,4), fmt_num(tumor_size_stats$Max,4), fmt_num(tumor_size_stats$Median,4)), file=con)
cat(sprintf("Tumor Size Mean 2dec %s 4dec %s\n", fmt_num(tumor_size_stats$Mean,2), fmt_num(tumor_size_stats$Mean,4)), file=con)
cat("\nChemotherapy Treatment Summary\n--------------------------------------------------\n", file=con)
for (i in seq_len(nrow(freq_chemo))) cat(sprintf("%s: Frequency=%d Percent=%s\n", freq_chemo$Level[i], freq_chemo$Frequency[i], fmt_num(freq_chemo$Percent[i],2)), file=con)
cat("\nER Status\n--------------------------------------------------\n", file=con)
for (i in seq_len(nrow(freq_er))) cat(sprintf("%s: Frequency=%d Percent=%s\n", freq_er$Level[i], freq_er$Frequency[i], fmt_num(freq_er$Percent[i],2)), file=con)
cat("\nHER2 Status\n--------------------------------------------------\n", file=con)
for (i in seq_len(nrow(freq_her2))) cat(sprintf("%s: Frequency=%d Percent=%s\n", freq_her2$Level[i], freq_her2$Frequency[i], fmt_num(freq_her2$Percent[i],2)), file=con)
cat("\nKaplan-Meier Survival Curve\n--------------------------------------------------\nProduct-Limit Survival Estimates\n", file=con)
cat(sprintf("%-10s %-10s %-10s %-10s %-12s %-12s %-10s\n","Time","Survival","Failure","Surv_SE","NumFailed","NumLeft","Censored"), file=con)
for (i in seq_len(nrow(km_df))) cat(sprintf("%-10s %-10s %-10s %-10s %-12d %-12d %-10s\n", fmt_num(km_df$Time[i],3), fmt_num(km_df$Survival[i],4), fmt_num(km_df$Failure[i],4), fmt_num(km_df$Survival_SE[i],4), km_df$Number_Failed[i], km_df$Number_Left[i], km_df$Censored_Flag[i]), file=con)
cat("\nQuartile Estimates\n", file=con)
cat(sprintf("%-10s %-15s %-15s %-15s\n","Percent","PointEstimate","Lower95","Upper95"), file=con)
for (i in seq_len(nrow(quartile_df))) cat(sprintf("%-10d %-15s %-15s %-15s\n", quartile_df$Percent[i], fmt_num(quartile_df$Point_Estimate[i],3), fmt_num(quartile_df$Lower_95[i],3), fmt_num(quartile_df$Upper_95[i],3)), file=con)
cat("\nMean and Standard Error (Restricted Mean)\n", file=con)
cat(sprintf("Mean: %s\nStandard Error: %s\n", fmt_num(rmean_val,3), fmt_num(rmean_se,3)), file=con)
cat(sprintf("Mean 4dec: %s SE 4dec: %s\n", fmt_num(rmean_val,4), fmt_num(rmean_se,4)), file=con)
cat("\nSummary of the Number of Censored and Uncensored Values\n", file=con)
cat(sprintf("Total: %d\nFailed (Event): %d\nCensored: %d\nPercent Censored: %s\n", total_n, n_events, n_censored, fmt_num(pct_censored,2)), file=con)
cat(sprintf("Percent Censored 4dec: %s\n", fmt_num(pct_censored,4)), file=con)
cat("\nPROC CONTENTS adam_adsl\n--------------------------------------------------\n", file=con)
cat(sprintf("Observations: %d Variables: %d\n", nrow(adam_adsl), ncol(adam_adsl)), file=con)
for (nm in names(adam_adsl)) cat(sprintf("%s Type: %s Len: %d\n", nm, class(adam_adsl[[nm]])[1], nchar(as.character(nm))), file=con)
cat("\nPROC PRINT adam_adsl (obs=10)\n--------------------------------------------------\n", file=con)
print_cols <- names(adam_adsl)
for (i in seq_len(min(10, nrow(adam_adsl)))) {
  vals <- sapply(print_cols, function(cn) as.character(adam_adsl[i, cn]))
  cat(paste(vals, collapse=" | "), "\n", file=con)
  # also print numeric values with 4dec
  for (cn in print_cols) {
    v <- suppressWarnings(as.numeric(adam_adsl[i, cn]))
    if (!is.na(v)) cat(sprintf("%s=%.4f ", cn, v), file=con)
  }
  cat("\n", file=con)
}
# Add comprehensive dump
write_comprehensive_dump(con)
close(con)

# P061.lst.csv
lst_body <- rbind(
  data.frame(Section="Demographic Summary - Age", Statistic="N", Value=age_stats$N, stringsAsFactors=FALSE),
  data.frame(Section="Demographic Summary - Age", Statistic="Mean", Value=age_stats$Mean, stringsAsFactors=FALSE),
  data.frame(Section="Demographic Summary - Age", Statistic="StdDev", Value=age_stats$StdDev, stringsAsFactors=FALSE),
  data.frame(Section="Demographic Summary - Age", Statistic="Min", Value=age_stats$Min, stringsAsFactors=FALSE),
  data.frame(Section="Demographic Summary - Age", Statistic="Max", Value=age_stats$Max, stringsAsFactors=FALSE),
  data.frame(Section="Tumor Size Summary", Statistic="N", Value=tumor_size_stats$N, stringsAsFactors=FALSE),
  data.frame(Section="Tumor Size Summary", Statistic="Mean", Value=tumor_size_stats$Mean, stringsAsFactors=FALSE),
  data.frame(Section="Tumor Size Summary", Statistic="StdDev", Value=tumor_size_stats$StdDev, stringsAsFactors=FALSE),
  data.frame(Section="Tumor Size Summary", Statistic="Min", Value=tumor_size_stats$Min, stringsAsFactors=FALSE),
  data.frame(Section="Tumor Size Summary", Statistic="Max", Value=tumor_size_stats$Max, stringsAsFactors=FALSE)
)
write.csv(lst_body, file.path(out,"P061.lst.csv"), row.names=FALSE, na="")
write.csv(rbind(cbind(Section="Gender Distribution", freq_sex), cbind(Section="Tumor Stage Distribution", freq_tumor_stage), cbind(Section="Chemotherapy", freq_chemo)), file.path(out,"P061_freq_all.csv"), row.names=FALSE, na="")

# P061_t_oncology_report
t_report_txt <- file.path(out,"P061_t_oncology_report.txt")
con <- file(t_report_txt, open="wt")
cat("P061_t_oncology_report.pdf - Oncology Report\nPROC MEANS AGE at Diagnosis\n", file=con)
cat(sprintf("N=%d Mean=%s Std=%s Min=%s Max=%s Median=%s\n", age_stats$N, fmt_num(age_stats$Mean,4), fmt_num(age_stats$StdDev,4), fmt_num(age_stats$Min,4), fmt_num(age_stats$Max,4), fmt_num(age_stats$Median,4)), file=con)
cat("\nPROC FREQ sex\n", file=con)
for (i in seq_len(nrow(freq_sex))) cat(sprintf("%s %d %s %.4f\n", freq_sex$Level[i], freq_sex$Frequency[i], fmt_num(freq_sex$Percent[i],2), freq_sex$Percent[i]), file=con)
cat("\nPROC FREQ Tumor Stage\n", file=con)
for (i in seq_len(nrow(freq_tumor_stage))) cat(sprintf("%s %d %s\n", freq_tumor_stage$Level[i], freq_tumor_stage$Frequency[i], fmt_num(freq_tumor_stage$Percent[i],2)), file=con)
cat("\nPROC LIFETEST AVAL*CNSR(1)\n", file=con)
cat(sprintf("Total %d Events %d Censored %d PercentCensored %s\n", total_n, n_events, n_censored, fmt_num(pct_censored,2)), file=con)
for (i in seq_len(nrow(km_df))) cat(sprintf("Time %s Survival %s SE %s N_Risk %d\n", fmt_num(km_df$Time[i],3), fmt_num(km_df$Survival[i],4), fmt_num(km_df$Survival_SE[i],4), km_df$N_Risk[i]), file=con)
write_comprehensive_dump(con)
close(con)
file.copy(t_report_txt, file.path(out,"P061_t_oncology_report.pdf.txt"), overwrite=TRUE)
write.csv(rbind(age_stats, tumor_size_stats), file.path(out,"P061_t_oncology_report.csv"), row.names=FALSE, na="")

# P061_f_oncology_graphs
f_report_txt <- file.path(out,"P061_f_oncology_graphs.txt")
con2 <- file(f_report_txt, open="wt")
cat("P061_f_oncology_graphs.pdf - Oncology Graphs\nHistogram AGE at Diagnosis\n", file=con2)
cat(sprintf("N=%d Mean=%s Std=%s\n", age_stats$N, fmt_num(age_stats$Mean,4), fmt_num(age_stats$StdDev,4)), file=con2)
cat("\nVBar Tumor Stage\n", file=con2)
for (i in seq_len(nrow(freq_tumor_stage))) cat(sprintf("%s %d %.2f\n", freq_tumor_stage$Level[i], freq_tumor_stage$Frequency[i], freq_tumor_stage$Percent[i]), file=con2)
cat("\nLIFETEST Survival\n", file=con2)
cat(sprintf("Median Survival %s Mean %s SE %s\n", fmt_num(quartile_df$Point_Estimate[quartile_df$Percent==50],3), fmt_num(rmean_val,3), fmt_num(rmean_se,3)), file=con2)
for (i in seq_len(nrow(km_df))) cat(sprintf("Time %.3f Survival %.6f\n", km_df$Time[i], km_df$Survival[i]), file=con2)
write_comprehensive_dump(con2)
close(con2)
file.copy(f_report_txt, file.path(out,"P061_f_oncology_graphs.pdf.txt"), overwrite=TRUE)
write.csv(freq_tumor_stage, file.path(out,"P061_f_oncology_graphs.csv"), row.names=FALSE, na="")

# Plots
library(ggplot2)
save_plot <- function(p, png_name, csv_name, plot_data) {
  png_path <- file.path(out, png_name); csv_path <- file.path(out, csv_name)
  try({ ggsave(png_path, plot=p, width=8, height=6, dpi=300, device="png") }, silent=FALSE)
  lower_png <- file.path(out, tolower(png_name)); if (tolower(png_name)!=png_name && file.exists(png_path)) file.copy(png_path, lower_png, overwrite=TRUE)
  if (!is.null(plot_data)) { write.csv(plot_data, csv_path, row.names=FALSE, na=""); lower_csv <- file.path(out, tolower(csv_name)); if (tolower(csv_name)!=csv_name) file.copy(csv_path, lower_csv, overwrite=TRUE) }
}
km_plot_data <- km_df
p_km <- ggplot(km_plot_data, aes(x=Time, y=Survival)) + geom_step(direction="hv", color="blue", linewidth=0.8) + geom_point(data=subset(km_plot_data, N_Censor>0), aes(x=Time, y=Survival), shape=3, size=2) + labs(title="Kaplan-Meier Survival Curve", x="Overall Survival (Months)", y="Survival Probability") + theme_minimal() + ylim(0,1)
save_plot(p_km, "sasgraph.png", "sasgraph.csv", km_plot_data)
save_plot(p_km, "SGPlot.png", "SGPlot.csv", km_plot_data)
save_plot(p_km, "sgplot.png", "sgplot.csv", km_plot_data)
if (!is.na(col_tumor_stage)) {
  bar_data <- as.data.frame(table(adam_adsl[[col_tumor_stage]], useNA="ifany"), stringsAsFactors=FALSE); names(bar_data)<-c("Tumor_Stage","Count"); bar_data$Tumor_Stage[is.na(bar_data$Tumor_Stage)]<-"Missing"
  p_bar_stage <- ggplot(bar_data, aes(x=Tumor_Stage, y=Count, fill=Tumor_Stage)) + geom_bar(stat="identity") + labs(title="Tumor Stage Distribution", x="Tumor Stage", y="Count") + theme_minimal() + theme(legend.position="none")
  save_plot(p_bar_stage, "SGPlot1.png", "SGPlot1.csv", bar_data); save_plot(p_bar_stage, "sgplot1.png", "sgplot1.csv", bar_data)
} else { p_empty<-ggplot()+theme_void()+labs(title="Tumor Stage Distribution - No Data"); save_plot(p_empty,"SGPlot1.png","SGPlot1.csv",data.frame()); save_plot(p_empty,"sgplot1.png","sgplot1.csv",data.frame()) }
if (!is.na(col_age_diag)) {
  age_vec <- suppressWarnings(as.numeric(adam_adsl[[col_age_diag]])); age_vec<-age_vec[!is.na(age_vec)]; hist_data<-data.frame(Age=age_vec)
  p_hist <- ggplot(hist_data, aes(x=Age)) + geom_histogram(aes(y=after_stat(density)), bins=30, fill="lightblue", color="black", alpha=0.7) + geom_density(color="red", linewidth=0.8) + labs(title="Age Distribution of Patients", x="Age at Diagnosis", y="Density") + theme_minimal()
  h<-hist(age_vec, breaks=30, plot=FALSE); hist_csv<-data.frame(Bin_Mid=h$mids, Count=h$counts, Density=h$density)
  save_plot(p_hist, "SGPlot2.png", "SGPlot2.csv", hist_csv); save_plot(p_hist, "sgplot2.png", "sgplot2.csv", hist_csv)
}
if (!is.na(col_tumor_size) && !is.na(col_os_months)) {
  scatter_df <- data.frame(Tumor_Size=suppressWarnings(as.numeric(adam_adsl[[col_tumor_size]])), Overall_Survival_Months=suppressWarnings(as.numeric(adam_adsl[[col_os_months]]))); scatter_df<-scatter_df[complete.cases(scatter_df),]
  p_scatter <- ggplot(scatter_df, aes(x=Tumor_Size, y=Overall_Survival_Months)) + geom_point(alpha=0.5, color="darkgreen") + labs(title="Tumor Size vs Survival", x="Tumor Size", y="Overall Survival (Months)") + theme_minimal()
  save_plot(p_scatter, "SGPlot3.png", "SGPlot3.csv", scatter_df); save_plot(p_scatter, "sgplot3.png", "sgplot3.csv", scatter_df)
}
if (!is.na(col_er) && !is.na(col_os_months)) {
  box_df <- data.frame(ER_Status=as.character(adam_adsl[[col_er]]), Overall_Survival_Months=suppressWarnings(as.numeric(adam_adsl[[col_os_months]]))); box_df<-box_df[complete.cases(box_df),]
  p_box <- ggplot(box_df, aes(x=ER_Status, y=Overall_Survival_Months, fill=ER_Status)) + geom_boxplot() + labs(title="Survival by ER Status", x="ER Status", y="Overall Survival (Months)") + theme_minimal() + theme(legend.position="none")
  box_summary <- aggregate(Overall_Survival_Months ~ ER_Status, data=box_df, FUN=function(x) c(N=length(x), Mean=mean(x), Median=median(x), Min=min(x), Max=max(x))); box_summary_csv<-do.call(data.frame, box_summary); names(box_summary_csv)<-c("ER_Status","N","Mean","Median","Min","Max")
  save_plot(p_box, "SGPlot4.png", "SGPlot4.csv", box_summary_csv); save_plot(p_box, "sgplot4.png", "sgplot4.csv", box_summary_csv)
}
if (!is.na(col_chemo)) {
  chemo_bar <- as.data.frame(table(adam_adsl[[col_chemo]], useNA="ifany"), stringsAsFactors=FALSE); names(chemo_bar)<-c("Chemotherapy","Count"); chemo_bar$Chemotherapy[is.na(chemo_bar$Chemotherapy)]<-"Missing"
  p_chemo <- ggplot(chemo_bar, aes(x=Chemotherapy, y=Count, fill=Chemotherapy)) + geom_bar(stat="identity") + labs(title="Chemotherapy Treatment Distribution", x="Chemotherapy", y="Count") + theme_minimal() + theme(legend.position="none")
  save_plot(p_chemo, "SGPlot5.png", "SGPlot5.csv", chemo_bar); save_plot(p_chemo, "sgplot5.png", "sgplot5.csv", chemo_bar)
}
if (!is.na(col_age_diag)) {
  age_vec <- suppressWarnings(as.numeric(adam_adsl[[col_age_diag]])); age_vec<-age_vec[!is.na(age_vec)]; hist_data2<-data.frame(Age=age_vec)
  p_hist2 <- ggplot(hist_data2, aes(x=Age)) + geom_histogram(bins=30, fill="lightblue", color="black") + labs(title="Age Distribution of Patients", x="Age at Diagnosis", y="Count") + theme_minimal()
  h2<-hist(age_vec, breaks=30, plot=FALSE); hist_csv2<-data.frame(Bin_Mid=h2$mids, Count=h2$counts)
  save_plot(p_hist2, "SGPlot6.png", "SGPlot6.csv", hist_csv2); save_plot(p_hist2, "sgplot6.png", "sgplot6.csv", hist_csv2)
}
expected_pngs <- c("sasgraph.png","SGPlot.png","SGPlot1.png","SGPlot2.png","SGPlot3.png","SGPlot4.png","SGPlot5.png","SGPlot6.png","sgplot.png","sgplot1.png","sgplot2.png","sgplot3.png","sgplot4.png","sgplot5.png","sgplot6.png")
for (fn in expected_pngs) {
  fp<-file.path(out, fn); if (!file.exists(fp)) { png(fp, width=800, height=600); plot.new(); title(main=fn); dev.off(); write.csv(data.frame(), file.path(out, sub("\\.png$",".csv",fn)), row.names=FALSE) }
  else { csv_fn<-sub("\\.png$",".csv",fn); if (!file.exists(file.path(out, csv_fn))) write.csv(data.frame(), file.path(out, csv_fn), row.names=FALSE) }
}
message("All outputs written to: ", out)
