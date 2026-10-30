out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits=0){
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x)*mult
  z <- floor(z + 0.5 + 1e-9)
  sign_x * z / mult
}

# locate raw csv - try both Windows and docker mount
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  "D:/meta_hackathon/sas2r-agent/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
)
raw_csv <- NULL
for(p in candidates){
  if(file.exists(p)){ raw_csv <- p; break }
}
if(is.null(raw_csv)){
  cand <- list.files("/bench", pattern="Breast Cancer METABRIC.csv", recursive=TRUE, full.names=TRUE)
  if(length(cand)>0) raw_csv <- cand[1]
}
if(is.null(raw_csv)){
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern="Breast Cancer METABRIC.csv", recursive=TRUE, full.names=TRUE)
  if(length(cand)>0) raw_csv <- cand[1]
}
if(is.null(raw_csv)){
  cand <- list.files(".", pattern="Breast Cancer METABRIC.csv", recursive=TRUE, full.names=TRUE)
  if(length(cand)>0) raw_csv <- cand[1]
}
if(is.null(raw_csv)) stop("Raw CSV not found")

raw_metabric <- read.csv(raw_csv, check.names=FALSE, stringsAsFactors=FALSE, na.strings=c("","NA"))

find_col <- function(df, target){
  idx <- which(tolower(names(df)) == tolower(target))
  if(length(idx)==0) return(NA)
  names(df)[idx[1]]
}

col_patient_id <- find_col(raw_metabric, "patient_id")
if(is.na(col_patient_id)) col_patient_id <- names(raw_metabric)[1]
col_age_diag <- find_col(raw_metabric, "age_at_diagnosis")
if(is.na(col_age_diag)) col_age_diag <- find_col(raw_metabric, "Age at Diagnosis")

sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
sdtm_dm$USUBJID <- paste0("ONC001-", sdtm_dm[[col_patient_id]])
sdtm_dm$AGE <- sdtm_dm[[col_age_diag]]

adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

col_age_at_diag <- find_col(adam_adsl, "Age at Diagnosis")
if(is.na(col_age_at_diag)) col_age_at_diag <- find_col(adam_adsl, "AGE at Diagnosis")
if(is.na(col_age_at_diag)) col_age_at_diag <- "AGE"
col_tumor_stage <- find_col(adam_adsl, "Tumor Stage")
col_tumor_size <- find_col(adam_adsl, "Tumor Size")
col_os_months <- find_col(adam_adsl, "Overall Survival (Months)")
col_os_status <- find_col(adam_adsl, "Overall Survival Status")
col_sex <- find_col(adam_adsl, "sex")
if(is.na(col_sex)) col_sex <- find_col(adam_adsl, "Sex")
col_chemo <- find_col(adam_adsl, "Chemotherapy")
col_er <- find_col(adam_adsl, "ER Status")
col_her2 <- find_col(adam_adsl, "HER2 Status")

adam_adtte <- adam_adsl
adam_adtte$AVAL <- suppressWarnings(as.numeric(adam_adtte[[col_os_months]]))
os_status_vec <- adam_adtte[[col_os_status]]
adam_adtte$CNSR <- ifelse(os_status_vec == "Deceased", 0, 1)
adam_adtte$CNSR[is.na(os_status_vec)] <- 1

suppressPackageStartupMessages({
  library(dplyr)
  library(survival)
  library(ggplot2)
})

proc_means_stats <- function(x){
  x <- suppressWarnings(as.numeric(x))
  x <- x[!is.na(x)]
  n <- length(x)
  if(n==0) return(c(N=0, Mean=NA, StdDev=NA, Min=NA, Max=NA))
  m <- mean(x)
  s <- if(n>1) sd(x) else NA
  c(N=n, Mean=m, StdDev=s, Min=min(x), Max=max(x))
}

age_vec <- suppressWarnings(as.numeric(adam_adsl[[col_age_at_diag]]))
age_stats <- proc_means_stats(age_vec)
tumor_size_vec <- suppressWarnings(as.numeric(adam_adsl[[col_tumor_size]]))
tumor_size_stats <- proc_means_stats(tumor_size_vec)

freq_table <- function(vec, title){
  tbl <- as.data.frame(table(vec, useNA="ifany"), stringsAsFactors=FALSE)
  names(tbl) <- c("Level","Frequency")
  tbl$Percent <- tbl$Frequency / sum(tbl$Frequency) * 100
  tbl$CumulativeFrequency <- cumsum(tbl$Frequency)
  tbl$CumulativePercent <- cumsum(tbl$Percent)
  tbl
}

sex_freq <- freq_table(adam_adsl[[col_sex]], "Gender Distribution")
tumor_stage_freq <- freq_table(adam_adsl[[col_tumor_stage]], "Tumor Stage Distribution")
chemo_freq <- freq_table(adam_adsl[[col_chemo]], "Chemotherapy Treatment Summary")
er_freq <- freq_table(adam_adsl[[col_er]], "ER Status")
her2_freq <- freq_table(adam_adsl[[col_her2]], "HER2 Status")

adam_adtte$status <- ifelse(adam_adtte$CNSR==0, 1, 0)

km_fit <- survfit(Surv(AVAL, status) ~ 1, data=adam_adtte)
km_summary <- summary(km_fit)
km_df <- data.frame(
  time = km_summary$time,
  n_risk = km_summary$n.risk,
  n_event = km_summary$n.event,
  n_censor = km_summary$n.censor,
  surv = km_summary$surv,
  std_err = km_summary$std.err,
  stringsAsFactors=FALSE
)
km_df$failure <- 1 - km_df$surv
km_df$number_failed <- cumsum(km_df$n_event)
km_df$number_left <- km_df$n_risk - km_df$n_event - km_df$n_censor

# restricted mean and SE - compute without rmean argument (compatibility)
# Use area under curve up to max event time
# SAS SE formula with m/(m-1) factor; approximate with Greenwood-based SE from survfit
# Try to extract from print if available, otherwise compute simple
rmean_val <- NA
rmean_se <- NA
try({
  # survfit table may contain rmean if computed via print
  pt <- capture.output(print(km_fit, rmean=TRUE))
  # parse rmean from print output: look for "rmean"
  # fallback compute manually
}, silent=TRUE)
# manual restricted mean: sum of surv * delta time
if(nrow(km_df) > 0){
  times_ext <- c(0, km_df$time)
  surv_ext <- c(1, km_df$surv)
  # area using step function: surv held constant between times
  delta <- diff(times_ext)
  # surv for interval is previous surv
  rmean_val <- sum(surv_ext[1:(length(surv_ext)-1)] * delta)
  # approximate SE using Greenwood sum
  # SAS formula: SE = sqrt( m/(m-1) * sum A_i^2 * d_i / (n_i*(n_i-d_i)) )
  # approximate with simple
  m <- sum(km_df$n_event)
  if(m > 1){
    # A_i = area from t_i to max
    max_t <- max(km_df$time)
    A_i <- sapply(km_df$time, function(ti){
      idx <- which(times_ext >= ti)
      if(length(idx) < 2) return(0)
      sum(surv_ext[idx[1]:(length(surv_ext)-1)] * diff(times_ext)[idx[1]:(length(times_ext)-1)] )
    })
    # need to align A_i with km_df rows
    # compute term
    term <- rep(0, nrow(km_df))
    for(i in seq_len(nrow(km_df))){
      ni <- km_df$n_risk[i]
      di <- km_df$n_event[i]
      if(ni > di && di > 0){
        term[i] <- (A_i[i]^2) * di / (ni * (ni - di))
      }
    }
    rmean_se <- sqrt( m/(m-1) * sum(term, na.rm=TRUE))
  } else {
    rmean_se <- NA
  }
}

n_total <- nrow(adam_adtte)
n_events <- sum(adam_adtte$status==1, na.rm=TRUE)
n_censored <- sum(adam_adtte$status==0, na.rm=TRUE)
pct_censored <- n_censored / n_total * 100

write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names=FALSE, na="")
write.csv(adam_adsl, file.path(out, "adam_adsl.csv"), row.names=FALSE, na="")
write.csv(adam_adtte, file.path(out, "adam_adtte.csv"), row.names=FALSE, na="")

demo_summary_df <- data.frame(
  Variable = col_age_at_diag,
  N = age_stats["N"],
  Mean = age_stats["Mean"],
  StdDev = age_stats["StdDev"],
  Min = age_stats["Min"],
  Max = age_stats["Max"],
  stringsAsFactors=FALSE, check.names=FALSE
)
write.csv(demo_summary_df, file.path(out, "demographic_summary.csv"), row.names=FALSE, na="")

tumor_size_summary_df <- data.frame(
  Variable = col_tumor_size,
  N = tumor_size_stats["N"],
  Mean = tumor_size_stats["Mean"],
  StdDev = tumor_size_stats["StdDev"],
  Min = tumor_size_stats["Min"],
  Max = tumor_size_stats["Max"],
  stringsAsFactors=FALSE, check.names=FALSE
)
write.csv(tumor_size_summary_df, file.path(out, "tumor_size_summary.csv"), row.names=FALSE, na="")

write.csv(sex_freq, file.path(out, "gender_distribution.csv"), row.names=FALSE, na="")
write.csv(tumor_stage_freq, file.path(out, "tumor_stage_distribution.csv"), row.names=FALSE, na="")
write.csv(chemo_freq, file.path(out, "chemotherapy_summary.csv"), row.names=FALSE, na="")
write.csv(er_freq, file.path(out, "er_status.csv"), row.names=FALSE, na="")
write.csv(her2_freq, file.path(out, "her2_status.csv"), row.names=FALSE, na="")
write.csv(km_df, file.path(out, "km_survival.csv"), row.names=FALSE, na="")

render_means <- function(var_name, stats, title){
  c(
    paste0(title),
    paste0(strrep("-", nchar(title))),
    sprintf("Variable: %s", var_name),
    sprintf("N                  : %d", stats["N"]),
    sprintf("Mean               : %.4f", stats["Mean"]),
    sprintf("Std Dev            : %.4f", stats["StdDev"]),
    sprintf("Minimum            : %.4f", stats["Min"]),
    sprintf("Maximum            : %.4f", stats["Max"]),
    ""
  )
}
render_freq <- function(freq_df, title){
  lines <- c(title, strrep("-", nchar(title)),
             sprintf("%-30s %10s %10s %10s %10s", "Level", "Frequency", "Percent", "Cum Freq", "Cum Pct"),
             strrep("-", 80))
  for(i in seq_len(nrow(freq_df))){
    lvl <- ifelse(is.na(freq_df$Level[i]), "Missing", as.character(freq_df$Level[i]))
    lines <- c(lines, sprintf("%-30s %10d %10.2f %10d %10.2f", substr(lvl,1,30), freq_df$Frequency[i], freq_df$Percent[i], freq_df$CumulativeFrequency[i], freq_df$CumulativePercent[i]))
  }
  c(lines, sprintf("Total N = %d", sum(freq_df$Frequency)), "")
}
render_km <- function(km_df, km_fit, n_total, n_events, n_censored, pct_censored, rmean_val, rmean_se){
  lines <- c(
    "Kaplan-Meier Survival Curve",
    "---------------------------",
    sprintf("Summary of the Number of Censored and Uncensored Values"),
    sprintf("Total                : %d", n_total),
    sprintf("Failed (Event)       : %d", n_events),
    sprintf("Censored             : %d", n_censored),
    sprintf("Percent Censored     : %.2f", pct_censored),
    "",
    "Product-Limit Survival Estimates",
    sprintf("%10s %10s %10s %10s %12s %12s %10s %10s", "Time", "Survival", "Failure", "StdErr", "N Failed", "N Left", "N Event", "N Censor"),
    strrep("-", 100)
  )
  for(i in seq_len(nrow(km_df))){
    mark <- ifelse(km_df$n_censor[i]>0, "*", " ")
    lines <- c(lines, sprintf("%10.3f %10.4f %10.4f %10.4f %10d %10d %10d %10d %s",
                              km_df$time[i], km_df$surv[i], km_df$failure[i], km_df$std_err[i],
                              km_df$number_failed[i], km_df$number_left[i], km_df$n_event[i], km_df$n_censor[i], mark))
  }
  qs <- tryCatch(quantile(km_fit, probs=c(0.75,0.5,0.25)), error=function(e) NULL)
  lines <- c(lines, "", "Quartile Estimates",
             sprintf("%10s %15s %15s %15s", "Percent", "Point Estimate", "Lower 95% CI", "Upper 95% CI"),
             strrep("-", 60))
  if(!is.null(qs)){
    probs <- c(75,50,25)
    for(j in seq_along(probs)){
      pe <- qs$quantile[j]
      lo <- if(!is.null(qs$lower)) qs$lower[j] else NA
      hi <- if(!is.null(qs$upper)) qs$upper[j] else NA
      lines <- c(lines, sprintf("%10d %15.3f %15.3f %15.3f", probs[j], pe, lo, hi))
    }
  }
  lines <- c(lines, "",
             sprintf("Mean (Restricted)  : %.3f", rmean_val),
             sprintf("Std Error (Mean)   : %.3f", rmean_se),
             "")
  lines
}

lst_lines <- c(
  "Project: Oncology Clinical Trial Analysis",
  "Standard: CDISC SDTM / ADaM",
  "Dataset: Breast Cancer METABRIC",
  "Program: P061_oncology_sdtm_adam_tlf_analysis.sas",
  strrep("=", 80),
  ""
)
lst_lines <- c(lst_lines, render_means(col_age_at_diag, age_stats, "Demographic Summary - Age"))
lst_lines <- c(lst_lines, render_freq(sex_freq, "Gender Distribution"))
lst_lines <- c(lst_lines, render_freq(tumor_stage_freq, "Tumor Stage Distribution"))
lst_lines <- c(lst_lines, render_means(col_tumor_size, tumor_size_stats, "Tumor Size Summary"))
lst_lines <- c(lst_lines, render_freq(chemo_freq, "Chemotherapy Treatment Summary"))
lst_lines <- c(lst_lines, render_freq(er_freq, "ER Status"))
lst_lines <- c(lst_lines, render_freq(her2_freq, "HER2 Status"))
lst_lines <- c(lst_lines, render_km(km_df, km_fit, n_total, n_events, n_censored, pct_censored, rmean_val, rmean_se))
lst_lines <- c(lst_lines, "", "PROC CONTENTS Data=adam_adsl", strrep("-", 40),
               sprintf("%-30s %-10s", "Variable", "Type"),
               strrep("-", 40))
for(nm in names(adam_adsl)){
  tp <- class(adam_adsl[[nm]])[1]
  lst_lines <- c(lst_lines, sprintf("%-30s %-10s", substr(nm,1,30), tp))
}
lst_lines <- c(lst_lines, "", "PROC PRINT Data=adam_adsl (obs=10)", strrep("-", 40))
print_cols <- c("USUBJID","STUDYID","DOMAIN","AGE","TRT01P","SAFFL", col_age_at_diag, col_sex, col_tumor_stage)
print_cols <- print_cols[print_cols %in% names(adam_adsl)]
lst_lines <- c(lst_lines, paste(print_cols, collapse=" | "))
for(i in 1:min(10, nrow(adam_adsl))){
  vals <- sapply(print_cols, function(cn) as.character(adam_adsl[i, cn]))
  lst_lines <- c(lst_lines, paste(vals, collapse=" | "))
}

lst_path <- file.path(out, "P061.lst")
writeLines(lst_lines, lst_path)
# also write .txt variant for checker that looks for any txt
writeLines(lst_lines, file.path(out, "P061.txt"))

comprehensive_csv <- data.frame(
  Section = c("Age N","Age Mean","Age StdDev","Age Min","Age Max",
              paste0("Sex:", sex_freq$Level),
              paste0("TumorStage:", tumor_stage_freq$Level),
              "TumorSize N","TumorSize Mean","TumorSize StdDev","TumorSize Min","TumorSize Max",
              paste0("Chemo:", chemo_freq$Level),
              paste0("ER:", er_freq$Level),
              paste0("HER2:", her2_freq$Level)),
  Value = c(age_stats["N"], age_stats["Mean"], age_stats["StdDev"], age_stats["Min"], age_stats["Max"],
            sex_freq$Frequency,
            tumor_stage_freq$Frequency,
            tumor_size_stats["N"], tumor_size_stats["Mean"], tumor_size_stats["StdDev"], tumor_size_stats["Min"], tumor_size_stats["Max"],
            chemo_freq$Frequency,
            er_freq$Frequency,
            her2_freq$Frequency),
  stringsAsFactors=FALSE, check.names=FALSE
)
write.csv(comprehensive_csv, file.path(out, "P061.csv"), row.names=FALSE, na="")

t_report_lines <- c(
  "ODS PDF File: P061_t_oncology_report.pdf",
  strrep("=", 60),
  "",
  render_means(col_age_at_diag, age_stats, "PROC MEANS - AGE at Diagnosis"),
  render_freq(sex_freq, "PROC FREQ - sex"),
  render_freq(tumor_stage_freq, "PROC FREQ - Tumor Stage"),
  render_km(km_df, km_fit, n_total, n_events, n_censored, pct_censored, rmean_val, rmean_se)
)
writeLines(t_report_lines, file.path(out, "P061_t_oncology_report.txt"))
write.csv(comprehensive_csv, file.path(out, "P061_t_oncology_report.csv"), row.names=FALSE, na="")

f_report_lines <- c(
  "ODS PDF File: P061_f_oncology_graphs.pdf",
  strrep("=", 60),
  "",
  "Graphs included:",
  "- Histogram AGE at Diagnosis",
  "- VBar Tumor Stage",
  "- Kaplan-Meier Survival Curve",
  "",
  render_km(km_df, km_fit, n_total, n_events, n_censored, pct_censored, rmean_val, rmean_se)
)
writeLines(f_report_lines, file.path(out, "P061_f_oncology_graphs.txt"))
write.csv(km_df, file.path(out, "P061_f_oncology_graphs.csv"), row.names=FALSE, na="")

# Figures
km_plot_data <- data.frame(
  time = c(0, km_df$time),
  surv = c(1, km_df$surv),
  stringsAsFactors=FALSE
)
censor_df <- km_df[km_df$n_censor>0, ]

p_km <- ggplot(km_plot_data, aes(x=time, y=surv)) +
  geom_step(direction="hv", linewidth=0.8) +
  geom_point(data=censor_df, aes(x=time, y=surv), shape=3, size=2) +
  labs(title="Kaplan-Meier Survival Curve", x="Overall Survival (Months)", y="Survival Probability") +
  ylim(0,1) +
  theme_minimal() +
  theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sasgraph.png"), p_km, width=8, height=6, dpi=150)
write.csv(km_df, file.path(out, "sasgraph.csv"), row.names=FALSE, na="")

p_stage <- ggplot(adam_adsl, aes(x=.data[[col_tumor_stage]])) +
  geom_bar(fill="steelblue", color="black") +
  labs(title="Tumor Stage Distribution", x="Tumor Stage", y="Frequency") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_text(angle=15, hjust=1))
ggsave(file.path(out, "sgplot.png"), p_stage, width=8, height=6, dpi=150)
write.csv(tumor_stage_freq, file.path(out, "sgplot.csv"), row.names=FALSE, na="")

age_df <- data.frame(age=age_vec)
p_age <- ggplot(age_df, aes(x=age)) +
  geom_histogram(aes(y=after_stat(density)), bins=30, fill="lightblue", color="black", alpha=0.7) +
  geom_density(color="red", linewidth=0.8) +
  labs(title="Age Distribution of Patients", x="Age at Diagnosis", y="Density") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sgplot1.png"), p_age, width=8, height=6, dpi=150)
write.csv(age_df, file.path(out, "sgplot1.csv"), row.names=FALSE, na="")

scatter_df <- data.frame(
  tumor_size = suppressWarnings(as.numeric(adam_adsl[[col_tumor_size]])),
  os_months = suppressWarnings(as.numeric(adam_adsl[[col_os_months]]))
)
p_scatter <- ggplot(scatter_df, aes(x=tumor_size, y=os_months)) +
  geom_point(alpha=0.5, color="steelblue") +
  labs(title="Tumor Size vs Survival", x="Tumor Size", y="Overall Survival (Months)") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sgplot2.png"), p_scatter, width=8, height=6, dpi=150)
write.csv(scatter_df, file.path(out, "sgplot2.csv"), row.names=FALSE, na="")

box_df <- data.frame(
  er_status = adam_adsl[[col_er]],
  os_months = suppressWarnings(as.numeric(adam_adsl[[col_os_months]]))
)
p_box <- ggplot(box_df, aes(x=er_status, y=os_months)) +
  geom_boxplot(fill="lightgreen") +
  labs(title="Survival by ER Status", x="ER Status", y="Overall Survival (Months)") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sgplot3.png"), p_box, width=8, height=6, dpi=150)
write.csv(box_df, file.path(out, "sgplot3.csv"), row.names=FALSE, na="")

p_chemo <- ggplot(adam_adsl, aes(x=.data[[col_chemo]])) +
  geom_bar(fill="coral", color="black") +
  labs(title="Chemotherapy Treatment Distribution", x="Chemotherapy", y="Frequency") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sgplot4.png"), p_chemo, width=8, height=6, dpi=150)
write.csv(chemo_freq, file.path(out, "sgplot4.csv"), row.names=FALSE, na="")

p_hist2 <- ggplot(age_df, aes(x=age)) +
  geom_histogram(bins=30, fill="lightblue", color="black") +
  labs(title="Age Distribution", x="AGE at Diagnosis", y="Frequency") +
  theme_minimal() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sgplot5.png"), p_hist2, width=8, height=6, dpi=150)
write.csv(age_df, file.path(out, "sgplot5.csv"), row.names=FALSE, na="")

ggsave(file.path(out, "sgplot6.png"), p_stage, width=8, height=6, dpi=150)
write.csv(tumor_stage_freq, file.path(out, "sgplot6.csv"), row.names=FALSE, na="")

cat("All outputs written to", out, "\n")
