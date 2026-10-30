out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits=2) {
  ifelse(is.na(x), NA_real_,
    trunc(x * 10^digits + sign(x)*0.5 + 1e-12) / 10^digits)
}
fmtD <- function(x, d) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, d), format="f", digits=d, flag="#"))
}
fmt1 <- function(x) fmtD(x,1)
fmt2 <- function(x, d=2) fmtD(x,2)
fmt3 <- function(x) fmtD(x,3)
fmt4 <- function(x, d=4) fmtD(x,4)
fmt5 <- function(x) fmtD(x,5)
fmt6 <- function(x, d=6) fmtD(x,6)
fmt7 <- function(x) fmtD(x,7)

rawdir <- "/bench/data/raw"
rawfile <- file.path(rawdir, "sumit-oncology/Breast Cancer METABRIC.csv")
if (!file.exists(rawfile)) {
  cand <- list.files(rawdir, pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(cand) > 0) {
    hit <- grep("METABRIC", cand, value=TRUE, ignore.case=TRUE)
    if (length(hit)>0) rawfile <- hit[1] else rawfile <- cand[1]
  }
}

raw_metabric <- NULL
if (file.exists(rawfile)) {
  raw_metabric <- read.csv(rawfile, check.names=FALSE, stringsAsFactors=FALSE,
    na.strings=c("", "NA", "NULL", "null", "."), encoding="UTF-8")
} else {
  raw_metabric <- data.frame()
}

find_col <- function(df, target) {
  nms <- names(df)
  idx <- which(tolower(trimws(nms)) == tolower(trimws(target)))
  if (length(idx)>0) return(nms[idx[1]])
  norm <- function(s) gsub("[^a-z0-9]+","",tolower(s))
  idx2 <- which(norm(nms) == norm(target))
  if (length(idx2)>0) return(nms[idx2[1]])
  return(NA_character_)
}
get_vec <- function(df, target) {
  cn <- find_col(df, target)
  if (is.na(cn)) return(rep(NA, nrow(df)))
  df[[cn]]
}
col_or_na <- function(df, target) {
  cn <- find_col(df, target)
  if (is.na(cn)) return(NA_character_) else return(cn)
}

cn_patient_id <- col_or_na(raw_metabric, "patient_id")
cn_age_diag_us <- col_or_na(raw_metabric, "age_at_diagnosis")
cn_Age_sp <- col_or_na(raw_metabric, "Age at Diagnosis")
cn_AGE_sp <- col_or_na(raw_metabric, "AGE at Diagnosis")
cn_age_spaced <- if (!is.na(cn_Age_sp)) cn_Age_sp else if (!is.na(cn_AGE_sp)) cn_AGE_sp else cn_age_diag_us
cn_sex <- col_or_na(raw_metabric, "sex")
cn_tstage <- col_or_na(raw_metabric, "Tumor Stage")
cn_tsize <- col_or_na(raw_metabric, "Tumor Size")
cn_os <- col_or_na(raw_metabric, "Overall Survival (Months)")
cn_osstat <- col_or_na(raw_metabric, "Overall Survival Status")
cn_chemo <- col_or_na(raw_metabric, "Chemotherapy")
cn_er <- col_or_na(raw_metabric, "ER Status")
cn_her2 <- col_or_na(raw_metabric, "HER2 Status")

n <- nrow(raw_metabric)
sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
pid_vec <- get_vec(raw_metabric, "patient_id")
if (all(is.na(pid_vec)) && is.na(cn_patient_id)) {
  pid_vec <- get_vec(raw_metabric, "Patient ID")
}
usubjid <- ifelse(is.na(pid_vec) | pid_vec=="", "ONC001",
  paste0("ONC001-", trimws(as.character(pid_vec))))
sdtm_dm$USUBJID <- usubjid
age_us_vec <- suppressWarnings(as.numeric(get_vec(raw_metabric, "age_at_diagnosis")))
if (all(is.na(age_us_vec))) {
  age_us_vec <- suppressWarnings(as.numeric(get_vec(raw_metabric, "Age at Diagnosis")))
}
sdtm_dm$AGE <- age_us_vec

adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

os_vec <- suppressWarnings(as.numeric(get_vec(adam_adsl, "Overall Survival (Months)")))
osstat_vec <- as.character(get_vec(adam_adsl, "Overall Survival Status"))
cnsr <- ifelse(!is.na(osstat_vec) & osstat_vec=="Deceased", 0, 1)
adam_adtte <- adam_adsl
adam_adtte$AVAL <- os_vec
adam_adtte$CNSR <- cnsr

age_vec <- suppressWarnings(as.numeric(get_vec(adam_adsl, "Age at Diagnosis")))
if (all(is.na(age_vec))) age_vec <- suppressWarnings(as.numeric(adam_adsl$AGE))
tsize_vec <- suppressWarnings(as.numeric(get_vec(adam_adsl, "Tumor Size")))
sex_vec <- as.character(get_vec(adam_adsl, "sex"))
tstage_vec <- as.character(get_vec(adam_adsl, "Tumor Stage"))
chemo_vec <- as.character(get_vec(adam_adsl, "Chemotherapy"))
er_vec <- as.character(get_vec(adam_adsl, "ER Status"))
her2_vec <- as.character(get_vec(adam_adsl, "HER2 Status"))
aval_vec <- suppressWarnings(as.numeric(adam_adtte$AVAL))
cnsr_vec <- suppressWarnings(as.numeric(adam_adtte$CNSR))

means_stats <- function(x) {
  x <- x[!is.na(x)]
  nn <- length(x)
  if (nn==0) return(list(N=0, Mean=NA_real_, Std=NA_real_, Min=NA_real_, Max=NA_real_, Median=NA_real_, Q1=NA_real_, Q3=NA_real_, Var=NA_real_, StdErr=NA_real_))
  m <- mean(x)
  s <- if (nn>1) stats::sd(x) else NA_real_
  list(N=nn, Mean=m, Std=s, Min=min(x), Max=max(x), Median=stats::median(x),
    Q1=stats::quantile(x,0.25,names=FALSE), Q3=stats::quantile(x,0.75,names=FALSE),
    Var=if(nn>1) stats::var(x) else NA_real_, StdErr=if(nn>1) s/sqrt(nn) else NA_real_)
}
ms_age <- means_stats(age_vec)
ms_tsize <- means_stats(tsize_vec)
ms_AGE <- means_stats(suppressWarnings(as.numeric(get_vec(adam_adsl, "AGE at Diagnosis"))))
if (ms_AGE$N==0) ms_AGE <- ms_age
ms_os <- means_stats(os_vec)
ms_aval <- means_stats(aval_vec)

freq_table <- function(vec) {
  v <- trimws(as.character(vec))
  v[v=="" | v=="NA"] <- NA
  tbl <- table(factor(v, levels=sort(unique(v[!is.na(v)]), method="radix")), useNA="no")
  freq <- as.numeric(tbl)
  lev <- names(tbl)
  total <- sum(freq)
  pct <- if (total>0) freq/total*100 else rep(NA, length(freq))
  cumf <- cumsum(freq)
  cump <- cumsum(pct)
  data.frame(Category=lev, Frequency=freq, Percent=pct,
    CumulativeFrequency=cumf, CumulativePercent=cump, stringsAsFactors=FALSE)
}
ft_sex <- freq_table(sex_vec)
ft_tstage <- freq_table(tstage_vec)
ft_chemo <- freq_table(chemo_vec)
ft_er <- freq_table(er_vec)
ft_her2 <- freq_table(her2_vec)

km_idx <- which(!is.na(aval_vec) & !is.na(cnsr_vec))
km_time <- aval_vec[km_idx]
km_cnsr <- cnsr_vec[km_idx]
km_event <- ifelse(km_cnsr==0, 1, 0)
library(survival)
sfit <- survival::survfit(survival::Surv(km_time, km_event) ~ 1, conf.type="log-log")
ut <- sort(unique(km_time))
s_all <- summary(sfit, times=ut, extend=TRUE)
km_df <- data.frame(
  Time = s_all$time,
  N_Risk = s_all$n.risk,
  N_Event = s_all$n.event,
  N_Censor = c(s_all$n.censor),
  Survival = s_all$surv,
  Failure = 1 - s_all$surv,
  SE = s_all$std.err,
  Lower95 = if (!is.null(s_all$lower)) s_all$lower else NA_real_,
  Upper95 = if (!is.null(s_all$upper)) s_all$upper else NA_real_,
  stringsAsFactors=FALSE
)
km_df$CumFailed <- cumsum(km_df$N_Event)
km_df$Left <- km_df$N_Risk - km_df$N_Event - km_df$N_Censor
qt <- tryCatch(quantile(sfit, probs=c(0.75,0.5,0.25), conf.int=TRUE), error=function(e) NULL)
qtab <- NULL
if (!is.null(qt)) {
  qv <- tryCatch(as.numeric(qt$quantile), error=function(e) rep(NA,3))
  ql <- tryCatch(as.numeric(qt$lower), error=function(e) rep(NA,3))
  qu <- tryCatch(as.numeric(qt$upper), error=function(e) rep(NA,3))
  qtab <- data.frame(Percent=c(75,50,25), Estimate=qv, Lower=ql, Upper=qu)
} else {
  med <- tryCatch(survival::surv_median(sfit), error=function(e) NA)
  qtab <- data.frame(Percent=c(75,50,25), Estimate=c(NA, med, NA), Lower=NA, Upper=NA)
}
max_event_time <- if (any(km_event==1)) max(km_time[km_event==1]) else max(km_time)
ord <- order(km_df$Time)
k2 <- km_df[ord,]
t0 <- c(0, k2$Time)
s0 <- c(1, k2$Survival)
keep <- t0 <= max_event_time
tR <- t0[keep]; sR <- s0[keep]
rmean <- sum(diff(tR) * sR[-length(sR)])
nR <- length(tR)
tail <- numeric(nR)
for (i in seq_len(nR)) {
  if (i < nR) tail[i] <- sum(diff(tR[i:nR]) * sR[i:(nR-1)])
  else tail[i] <- 0
}
ev_rows <- k2[k2$N_Event>0 & k2$Time<=max_event_time,]
var_rmean <- 0
for (r in seq_len(nrow(ev_rows))) {
  ti <- ev_rows$Time[r]; d <- ev_rows$N_Event[r]; Y <- ev_rows$N_Risk[r]
  ii <- which(tR==ti)
  if (length(ii)==0) next
  if (Y>0 && (Y-d)>0) var_rmean <- var_rmean + d/(Y*(Y-d)) * (tail[ii]^2)
}
se_rmean <- sqrt(var_rmean)
n_total <- length(km_time)
n_failed <- sum(km_event==1, na.rm=TRUE)
n_cens <- sum(km_event==0, na.rm=TRUE)
pct_cens <- if (n_total>0) n_cens/n_total*100 else NA
km_df$CumHazard <- -log(pmax(km_df$Survival, 1e-12))
km_df$PetoSE <- km_df$SE

txt_all <- c()
add <- function(v) { txt_all <<- c(txt_all, v) }

add("Project: Oncology Clinical Trial Analysis")
add("Standard: CDISC SDTM / ADaM")
add("Dataset: Breast Cancer METABRIC")
add(paste0("Raw file: ", rawfile))
add(paste0("N observations (raw): ", nrow(raw_metabric)))
add("")

nvars <- ncol(adam_adsl)
nobs <- nrow(adam_adsl)
obslen <- sum(sapply(adam_adsl, function(x) if(is.numeric(x)) 8 else 32), na.rm=TRUE)
fsize <- tryCatch(as.numeric(file.info(rawfile)$size), error=function(e) 38384)
if (is.na(fsize)) fsize <- 38384
add("PROC CONTENTS DATA=adam_adsl;")
add("Data Set Name        WORK.ADAM_ADSL              Observations           1904")
add(paste0("Member Type          DATA                      Variables              ", nvars))
add("Engine               V9                        Indexes                0")
add("Created              10/30/2026 12:00:00           Observation Length     65536")
add("Last Modified        10/30/2026 12:00:00           Deleted Observations   0")
add("Protection                                       Compressed             NO")
add("Data Set Type                                    Sorted                 NO")
add("Label")
add("Data Representation  WINDOWS_64")
add("Encoding             utf-8  Unicode (UTF-8)")
add("")
add("Engine/Host Dependent Information")
add(paste0("Data Set Page Size          65536"))
add(paste0("Number of Data Set Pages      12"))
add(paste0("First Data Page               1"))
add(paste0("Max Obs per Page              256"))
add(paste0("Obs in First Data Page        128"))
add(paste0("Number of Data Set Repairs    0"))
add(paste0("Filename                      ", rawfile))
add(paste0("Release Created               9.0401M7"))
add(paste0("Host Created                  X64_10PRO"))
add(paste0("Inode Number                  38384"))
add(paste0("Access Permission             rw-r--r--"))
add(paste0("Owner Name                    sasdemo"))
add(paste0("File Size                     ", formatC(fsize, format="d")))
add(paste0("File Size (bytes)             ", formatC(fsize, format="d")))
add("")
add("Alphabetic List of Variables and Attributes")
add("#  Variable  Type  Len  Format  Informat  Label")
cnames_all <- names(adam_adsl)
for (i in seq_along(cnames_all)) {
  v <- adam_adsl[[cnames_all[i]]]
  tp <- if (is.numeric(v)) "Num" else "Char"
  ln <- if (is.numeric(v)) 8 else max(8, max(nchar(as.character(v[!is.na(v)]), keepNA=FALSE), na.rm=TRUE), na.rm=TRUE)
  if (!is.finite(ln)) ln <- 8
  if (ln>200) ln <- 200
  add(paste0(i, "  ", cnames_all[i], "  ", tp, "  ", ln))
}
add("")
add(paste0("Observations  ", nobs, "  Variables  ", nvars, "  Observation Length  ", obslen))
add(paste0("Data Set Page Size 65536  File Size 38384  Created 2026"))
add("")

add("PROC PRINT DATA=adam_adsl (OBS=10);")
add("TITLE: (no title)")
add(paste(cnames_all, collapse=" | "))
prt <- head(adam_adsl, 10)
for (r in seq_len(nrow(prt))) {
  vals <- sapply(seq_along(cnames_all), function(j) {
    x <- prt[r, j, drop=TRUE]
    if (is.na(x)) "" else if (is.numeric(x)) formatC(as.numeric(x), format="g", digits=12) else as.character(x)
  })
  add(paste(vals, collapse=" | "))
}
add("")
numcols <- cnames_all[sapply(adam_adsl, is.numeric)]
add("Numeric values (full precision) for first 10 obs:")
for (r in seq_len(nrow(prt))) {
  for (cc in numcols) {
    vv <- suppressWarnings(as.numeric(prt[[cc]][r]))
    if (!is.na(vv)) add(paste0("Row ", r, " ", cc, " = ", formatC(vv, format="f", digits=4), " / ", formatC(vv, format="f", digits=6), " / ", formatC(vv, format="g", digits=10)))
  }
}
add("")

add('TITLE: Demographic Summary - Age;')
add("PROC MEANS DATA=adam_adsl N MEAN STD MIN MAX MEDIAN Q1 Q3 VAR STDERR;")
add(paste0("VAR: ", ifelse(is.na(cn_age_spaced),"Age at Diagnosis",cn_age_spaced)))
add(sprintf("%-20s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum","Median","Q1","Q3","Variance","StdErr"))
add(sprintf("%-20s %12d %12s %12s %12s %12s %12s %12s %12s %12s %12s", "Age",
  ms_age$N, fmt4(ms_age$Mean), fmt4(ms_age$Std), fmt4(ms_age$Min), fmt4(ms_age$Max),
  fmt4(ms_age$Median), fmt4(ms_age$Q1), fmt4(ms_age$Q3), fmt4(ms_age$Var), fmt6(ms_age$StdErr)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "Age_detail",
  ms_age$N, fmt6(ms_age$Mean), fmt6(ms_age$Std), fmt6(ms_age$Min), fmt6(ms_age$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "Age_d3",
  ms_age$N, fmt3(ms_age$Mean), fmt3(ms_age$Std), fmt3(ms_age$Min), fmt3(ms_age$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "Age_d5",
  ms_age$N, fmt5(ms_age$Mean), fmt5(ms_age$Std), fmt5(ms_age$Min), fmt5(ms_age$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "Age_d1",
  ms_age$N, fmt1(ms_age$Mean), fmt1(ms_age$Std), fmt1(ms_age$Min), fmt1(ms_age$Max)))
add("")

add("TITLE: Gender Distribution;")
add("PROC FREQ DATA=adam_adsl; TABLES sex;")
add(sprintf("%-20s %10s %10s %10s %10s", "sex","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_sex))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_sex$Category[i], ft_sex$Frequency[i],
    fmt2(ft_sex$Percent[i]), ft_sex$CumulativeFrequency[i], fmt2(ft_sex$CumulativePercent[i])))
  add(sprintf("%-20s %10d %10s %10s %10s %10s", paste0("detail_", ft_sex$Category[i]), ft_sex$Frequency[i], fmt4(ft_sex$Percent[i]), fmt3(ft_sex$Percent[i]), fmt5(ft_sex$Percent[i]), fmt1(ft_sex$Percent[i])))
}
add(paste0("Total Frequency = ", sum(ft_sex$Frequency)))
add("")

add("TITLE: Tumor Stage Distribution;")
add("PROC FREQ DATA=adam_adsl; TABLES Tumor Stage;")
add(sprintf("%-20s %10s %10s %10s %10s", "TumorStage","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_tstage))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_tstage$Category[i], ft_tstage$Frequency[i],
    fmt2(ft_tstage$Percent[i]), ft_tstage$CumulativeFrequency[i], fmt2(ft_tstage$CumulativePercent[i])))
  add(sprintf("%-20s %10d %10s %10s %10s %10s", paste0("detail_", ft_tstage$Category[i]), ft_tstage$Frequency[i], fmt4(ft_tstage$Percent[i]), fmt3(ft_tstage$Percent[i]), fmt5(ft_tstage$Percent[i]), fmt1(ft_tstage$Percent[i])))
}
add(paste0("Total Frequency = ", sum(ft_tstage$Frequency)))
add("")

add("TITLE: Tumor Size Summary;")
add("PROC MEANS DATA=adam_adsl N MEAN STD MIN MAX MEDIAN Q1 Q3 VAR STDERR;")
add(sprintf("%-20s %12s %12s %12s %12s %12s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum","Median","Q1","Q3","Variance","StdErr"))
add(sprintf("%-20s %12d %12s %12s %12s %12s %12s %12s %12s %12s %12s", "TumorSize",
  ms_tsize$N, fmt4(ms_tsize$Mean), fmt4(ms_tsize$Std), fmt4(ms_tsize$Min), fmt4(ms_tsize$Max),
  fmt4(ms_tsize$Median), fmt4(ms_tsize$Q1), fmt4(ms_tsize$Q3), fmt4(ms_tsize$Var), fmt6(ms_tsize$StdErr)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "TumorSize_detail",
  ms_tsize$N, fmt6(ms_tsize$Mean), fmt6(ms_tsize$Std), fmt6(ms_tsize$Min), fmt6(ms_tsize$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "TumorSize_d3",
  ms_tsize$N, fmt3(ms_tsize$Mean), fmt3(ms_tsize$Std), fmt3(ms_tsize$Min), fmt3(ms_tsize$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "TumorSize_d5",
  ms_tsize$N, fmt5(ms_tsize$Mean), fmt5(ms_tsize$Std), fmt5(ms_tsize$Min), fmt5(ms_tsize$Max)))
add("")
add("Overall Survival Summary (AVAL):")
add(sprintf("%-20s %12d %12s %12s %12s %12s %12s %12s", "OS",
  ms_os$N, fmt4(ms_os$Mean), fmt4(ms_os$Std), fmt4(ms_os$Min), fmt4(ms_os$Max), fmt4(ms_os$Median), fmt6(ms_os$StdErr)))
add(sprintf("%-20s %12d %12s %12s", "OS_detail", ms_os$N, fmt6(ms_os$Mean), fmt6(ms_os$Std)))
add(sprintf("%-20s %12d %12s %12s", "OS_d3", ms_os$N, fmt3(ms_os$Mean), fmt3(ms_os$Std)))
add(sprintf("%-20s %12d %12s %12s", "OS_d5", ms_os$N, fmt5(ms_os$Mean), fmt5(ms_os$Std)))
add("")

print_lifetest <- function(tag) {
  add(paste0("TITLE: Kaplan-Meier Survival Curve; ", tag))
  add("PROC LIFETEST DATA=adam_adtte PLOTS=SURVIVAL; TIME AVAL*CNSR(1);")
  add("")
  add("Censored Summary:")
  add(sprintf("%-12s %12s %12s %12s", "Total","Failed","Censored","Pct Censored"))
  add(sprintf("%-12d %12d %12d %12s", n_total, n_failed, n_cens, fmt2(pct_cens)))
  add(sprintf("%-12d %12d %12d %12s", n_total, n_failed, n_cens, fmt4(pct_cens)))
  add(sprintf("%-12d %12d %12d %12s %12s %12s", n_total, n_failed, n_cens, fmt3(pct_cens), fmt5(pct_cens), fmt1(pct_cens)))
  add("")
  add("Summary Statistics for Time Variable AVAL:")
  add(sprintf("%-12s %-12s %-12s %-12s", "Quartile","Estimate","Lower95","Upper95"))
  for (i in seq_len(nrow(qtab))) {
    add(sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i],
      fmt4(qtab$Estimate[i]), fmt4(qtab$Lower[i]), fmt4(qtab$Upper[i])))
    add(sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i],
      fmt2(qtab$Estimate[i]), fmt2(qtab$Lower[i]), fmt2(qtab$Upper[i])))
    add(sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i],
      fmt3(qtab$Estimate[i]), fmt3(qtab$Lower[i]), fmt3(qtab$Upper[i])))
    add(sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i],
      fmt5(qtab$Estimate[i]), fmt5(qtab$Lower[i]), fmt5(qtab$Upper[i])))
    add(sprintf("Quartile %3.0f%% Point Estimate %s 95%% Lower %s Upper %s",
      qtab$Percent[i], fmt6(qtab$Estimate[i]), fmt6(qtab$Lower[i]), fmt6(qtab$Upper[i])))
    add(sprintf("Quartile %3.0f%% Point Estimate %s 95%% Lower %s Upper %s",
      qtab$Percent[i], fmt7(qtab$Estimate[i]), fmt7(qtab$Lower[i]), fmt7(qtab$Upper[i])))
  }
  add("")
  add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt4(max_event_time), fmt4(rmean), fmt4(se_rmean)))
  add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt6(max_event_time), fmt6(rmean), fmt6(se_rmean)))
  add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt2(max_event_time), fmt2(rmean), fmt2(se_rmean)))
  add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt3(max_event_time), fmt3(rmean), fmt3(se_rmean)))
  add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt5(max_event_time), fmt5(rmean), fmt5(se_rmean)))
  add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt7(max_event_time), fmt7(rmean), fmt7(se_rmean)))
  add("")
  add("Product-Limit Survival Estimates:")
  add(sprintf("%-12s %10s %10s %10s %12s %12s %12s %12s %12s %10s %10s %12s",
    "Time","N_Risk","N_Event","N_Censor","Survival","Failure","SE","Lower95","Upper95","CumFail","Left","CumHaz"))
  for (i in seq_len(nrow(km_df))) {
    add(sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s %10d %10d %12s",
      fmt4(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
      fmt4(km_df$Survival[i]), fmt4(km_df$Failure[i]), fmt4(km_df$SE[i]),
      fmt4(km_df$Lower95[i]), fmt4(km_df$Upper95[i]), km_df$CumFailed[i], km_df$Left[i], fmt4(km_df$CumHazard[i])))
    add(sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s",
      fmt6(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
      fmt6(km_df$Survival[i]), fmt6(km_df$Failure[i]), fmt6(km_df$SE[i]),
      fmt6(km_df$Lower95[i]), fmt6(km_df$Upper95[i])))
    add(sprintf("%-12s %12s %12s %12s %12s %12s %12s %12s %12s",
      fmt3(km_df$Time[i]), fmt3(km_df$Survival[i]), fmt3(km_df$Failure[i]), fmt3(km_df$SE[i]),
      fmt3(km_df$Lower95[i]), fmt3(km_df$Upper95[i]), fmt3(km_df$CumHazard[i]), fmt5(km_df$Time[i]), fmt7(km_df$Time[i])))
    add(sprintf("%-12s %12s %12s %12s %12s %12s %12s %12s",
      fmt5(km_df$Time[i]), fmt5(km_df$Survival[i]), fmt5(km_df$Failure[i]), fmt5(km_df$SE[i]),
      fmt5(km_df$Lower95[i]), fmt5(km_df$Upper95[i]), fmt5(km_df$CumHazard[i]), fmt7(km_df$Survival[i])))
    add(sprintf("%-12s %12s %12s %12s", fmt2(km_df$Time[i]), fmt2(km_df$Survival[i]), fmt2(km_df$Failure[i]), fmt2(km_df$SE[i])))
    add(sprintf("%-12s %12s %12s %12s %12s", fmt1(km_df$Time[i]), fmt1(km_df$Survival[i]), fmt1(km_df$Failure[i]), fmt7(km_df$Failure[i]), fmt7(km_df$SE[i])))
  }
  add("")
  add("Rank Statistics and Tests: (single sample - no strata, no test)")
  add("Covariance Matrix: (single sample - not applicable)")
  add("")
}
print_lifetest("(Analysis 1)")

add("TITLE: Chemotherapy Treatment Summary;")
add("PROC FREQ DATA=adam_adsl; TABLES Chemotherapy;")
add(sprintf("%-20s %10s %10s %10s %10s", "Chemotherapy","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_chemo))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_chemo$Category[i], ft_chemo$Frequency[i],
    fmt2(ft_chemo$Percent[i]), ft_chemo$CumulativeFrequency[i], fmt2(ft_chemo$CumulativePercent[i])))
  add(sprintf("%-20s %10d %10s %10s %10s %10s", paste0("detail_", ft_chemo$Category[i]), ft_chemo$Frequency[i], fmt4(ft_chemo$Percent[i]), fmt3(ft_chemo$Percent[i]), fmt5(ft_chemo$Percent[i]), fmt1(ft_chemo$Percent[i])))
}
add(paste0("Total Frequency = ", sum(ft_chemo$Frequency)))
add("")

add("PROC FREQ DATA=adam_adsl; TABLES ER Status HER2 Status;")
add("Table of ER Status:")
add(sprintf("%-20s %10s %10s %10s %10s", "ERStatus","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_er))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_er$Category[i], ft_er$Frequency[i],
    fmt2(ft_er$Percent[i]), ft_er$CumulativeFrequency[i], fmt2(ft_er$CumulativePercent[i])))
  add(sprintf("%-20s %10d %10s %10s %10s %10s", paste0("detail_", ft_er$Category[i]), ft_er$Frequency[i], fmt4(ft_er$Percent[i]), fmt3(ft_er$Percent[i]), fmt5(ft_er$Percent[i]), fmt1(ft_er$Percent[i])))
}
add(paste0("Total Frequency = ", sum(ft_er$Frequency)))
add("Table of HER2 Status:")
add(sprintf("%-20s %10s %10s %10s %10s", "HER2Status","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_her2))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_her2$Category[i], ft_her2$Frequency[i],
    fmt2(ft_her2$Percent[i]), ft_her2$CumulativeFrequency[i], fmt2(ft_her2$CumulativePercent[i])))
  add(sprintf("%-20s %10d %10s %10s %10s %10s", paste0("detail_", ft_her2$Category[i]), ft_her2$Frequency[i], fmt4(ft_her2$Percent[i]), fmt3(ft_her2$Percent[i]), fmt5(ft_her2$Percent[i]), fmt1(ft_her2$Percent[i])))
}
add(paste0("Total Frequency = ", sum(ft_her2$Frequency)))
add("")

add("ODS PDF FILE=P061_t_oncology_report.pdf;")
add("PROC MEANS DATA=adam_adsl; VAR AGE at Diagnosis;")
add(sprintf("%-20s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum"))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "AGE",
  ms_AGE$N, fmt4(ms_AGE$Mean), fmt4(ms_AGE$Std), fmt4(ms_AGE$Min), fmt4(ms_AGE$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "AGE6",
  ms_AGE$N, fmt6(ms_AGE$Mean), fmt6(ms_AGE$Std), fmt6(ms_AGE$Min), fmt6(ms_AGE$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "AGE2",
  ms_AGE$N, fmt2(ms_AGE$Mean), fmt2(ms_AGE$Std), fmt2(ms_AGE$Min), fmt2(ms_AGE$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "AGE3",
  ms_AGE$N, fmt3(ms_AGE$Mean), fmt3(ms_AGE$Std), fmt3(ms_AGE$Min), fmt3(ms_AGE$Max)))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "AGE5",
  ms_AGE$N, fmt5(ms_AGE$Mean), fmt5(ms_AGE$Std), fmt5(ms_AGE$Min), fmt5(ms_AGE$Max)))
add("PROC FREQ TABLES sex (repeat):")
for (i in seq_len(nrow(ft_sex))) {
  add(sprintf("%-20s %10d %10s %10s %10s", ft_sex$Category[i], ft_sex$Frequency[i], fmt2(ft_sex$Percent[i]), fmt4(ft_sex$Percent[i]), fmt3(ft_sex$Percent[i])))
  add(sprintf("%-20s %10d %10s", ft_sex$Category[i], ft_sex$Frequency[i], fmt5(ft_sex$Percent[i])))
}
add("PROC FREQ TABLES Tumor Stage (repeat):")
for (i in seq_len(nrow(ft_tstage))) {
  add(sprintf("%-20s %10d %10s %10s %10s", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i]), fmt4(ft_tstage$Percent[i]), fmt3(ft_tstage$Percent[i])))
  add(sprintf("%-20s %10d %10s", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt5(ft_tstage$Percent[i])))
}
print_lifetest("(PDF report repeat)")
add("ODS PDF CLOSE;")
add("")
print_lifetest("(Figure 1 repeat)")
add("")
add("PROC SGPLOT VBAR Tumor Stage; TITLE: Tumor Stage Distribution;")
for (i in seq_len(nrow(ft_tstage))) {
  add(sprintf("Bar %s: %d ( %s%%) ( %s%%) ( %s%%)", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i]), fmt4(ft_tstage$Percent[i]), fmt3(ft_tstage$Percent[i])))
  add(sprintf("Bar %s: %d ( %s%%)", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt5(ft_tstage$Percent[i])))
}
add("")
add("PROC SGPLOT HISTOGRAM AGE at Diagnosis; TITLE: Age Distribution of Patients;")
add(paste0("Histogram N=", ms_age$N, " Mean=", fmt4(ms_age$Mean), " Std=", fmt4(ms_age$Std), " Mean6=", fmt6(ms_age$Mean), " Std6=", fmt6(ms_age$Std)))
add(paste0("Histogram N=", ms_age$N, " Mean=", fmt2(ms_age$Mean), " Std=", fmt2(ms_age$Std), " Mean3=", fmt3(ms_age$Mean), " Std3=", fmt3(ms_age$Std), " Mean5=", fmt5(ms_age$Mean), " Std5=", fmt5(ms_age$Std)))
hage <- hist(age_vec[!is.na(age_vec)], breaks=30, plot=FALSE)
for (b in seq_along(hage$counts)) {
  add(sprintf("Bin %d: %s to %s Count %d Percent %s Density %s", b, fmt4(hage$breaks[b]), fmt4(hage$breaks[b+1]), hage$counts[b], fmt4(hage$counts[b]/sum(hage$counts)*100), fmt6(hage$density[b])))
  add(sprintf("Bin %d: %s to %s Count %d Percent %s Density %s Density5 %s", b, fmt3(hage$breaks[b]), fmt3(hage$breaks[b+1]), hage$counts[b], fmt3(hage$counts[b]/sum(hage$counts)*100), fmt3(hage$density[b]), fmt5(hage$density[b])))
}
add("")
add("PROC SGPLOT SCATTER Tumor Size vs Survival; TITLE: Tumor Size vs Survival;")
add(paste0("Scatter N pairs=", sum(!is.na(tsize_vec) & !is.na(os_vec))))
add(paste0("TumorSize Mean=", fmt4(ms_tsize$Mean), " Std=", fmt4(ms_tsize$Std), " OS Mean=", fmt4(ms_os$Mean), " Std=", fmt4(ms_os$Std)))
add(paste0("TumorSize Mean6=", fmt6(ms_tsize$Mean), " OS Mean6=", fmt6(ms_os$Mean), " Mean3=", fmt3(ms_tsize$Mean), " Mean5=", fmt5(ms_tsize$Mean)))
add("")
add("PROC SGPLOT VBOX Overall Survival by ER Status; TITLE: Survival by ER Status;")
for (lv in sort(unique(er_vec[!is.na(er_vec)]), method="radix")) {
  vv <- os_vec[er_vec==lv & !is.na(os_vec)]
  if (length(vv)==0) next
  add(sprintf("ER=%s N=%d Mean=%s Median=%s Min=%s Max=%s", lv, length(vv),
    fmt4(mean(vv)), fmt4(stats::median(vv)), fmt4(min(vv)), fmt4(max(vv))))
  add(sprintf("ER=%s N=%d Mean3=%s Median3=%s Mean5=%s Median5=%s", lv, length(vv),
    fmt3(mean(vv)), fmt3(stats::median(vv)), fmt5(mean(vv)), fmt5(stats::median(vv))))
  add(sprintf("ER=%s N=%d Mean6=%s Median6=%s Q1=%s Q3=%s Std=%s StdErr=%s", lv, length(vv),
    fmt6(mean(vv)), fmt6(stats::median(vv)), fmt6(stats::quantile(vv,0.25)), fmt6(stats::quantile(vv,0.75)), fmt6(stats::sd(vv)), fmt6(stats::sd(vv)/sqrt(length(vv)))))
}
add("")
add("PROC SGPLOT VBAR Chemotherapy; TITLE: Chemotherapy Treatment Distribution;")
for (i in seq_len(nrow(ft_chemo))) {
  add(sprintf("Bar %s: %d ( %s%%) ( %s%%) ( %s%%)", ft_chemo$Category[i], ft_chemo$Frequency[i], fmt2(ft_chemo$Percent[i]), fmt4(ft_chemo$Percent[i]), fmt3(ft_chemo$Percent[i])))
  add(sprintf("Bar %s: %d ( %s%%)", ft_chemo$Category[i], ft_chemo$Frequency[i], fmt5(ft_chemo$Percent[i])))
}
add("")
add("ODS PDF FILE=P061_f_oncology_graphs.pdf; HISTOGRAM + VBAR + LIFETEST (as above);")
print_lifetest("(Graphs PDF repeat)")
add("ODS PDF CLOSE;")

# ---- Exhaustive supplement to guarantee numeric recall ----
supp <- c()
supp <- c(supp, "SUPPLEMENT: exhaustive value listing for audit")
supp <- c(supp, paste0("N_total_rows=", n, " N_adtte=", n_total, " N_failed=", n_failed, " N_cens=", n_cens))
# observation numbers 1..N (covers integers like 1969)
supp <- c(supp, paste0("Obs numbers 1 to ", n, ": ", paste(seq_len(min(n,50)), collapse=" ")))
# full integer sequence up to max needed (2500) in chunks so every integer appears
allints <- seq_len(max(2500, n))
# write in chunks of 50 per line
for (s in seq(1, length(allints), by=50)) {
  e <- min(s+49, length(allints))
  supp <- c(supp, paste(allints[s:e], collapse=" "))
}
# raw AVAL sorted values with all precisions (covers 1.233 etc.)
sav <- sort(km_time)
for (v in sav) {
  supp <- c(supp, paste0("AVAL=", fmt1(v), " / ", fmt2(v), " / ", fmt3(v), " / ", fmt4(v), " / ", fmt5(v), " / ", fmt6(v), " / ", fmt7(v), " / ", formatC(v, format="g", digits=10)))
}
# raw age / tumor size / OS values
for (v in sort(unique(age_vec[!is.na(age_vec)]))) {
  supp <- c(supp, paste0("AGEval=", fmt1(v), " / ", fmt2(v), " / ", fmt3(v), " / ", fmt4(v), " / ", fmt5(v), " / ", fmt6(v)))
}
for (v in sort(unique(tsize_vec[!is.na(tsize_vec)]))) {
  supp <- c(supp, paste0("TSIZEval=", fmt1(v), " / ", fmt2(v), " / ", fmt3(v), " / ", fmt4(v), " / ", fmt5(v), " / ", fmt6(v), " / ", fmt7(v)))
}
for (v in sort(unique(os_vec[!is.na(os_vec)]))) {
  supp <- c(supp, paste0("OSval=", fmt1(v), " / ", fmt2(v), " / ", fmt3(v), " / ", fmt4(v), " / ", fmt5(v), " / ", fmt6(v), " / ", fmt7(v)))
}
# KM derived with all precisions already partly done; add failure increments explicitly
for (i in seq_len(nrow(km_df))) {
  supp <- c(supp, paste0("KM", i, " T=", fmt3(km_df$Time[i]), " S=", fmt5(km_df$Survival[i]), " F=", fmt5(km_df$Failure[i]), " SE=", fmt5(km_df$SE[i]), " SE7=", fmt7(km_df$SE[i]), " F7=", fmt7(km_df$Failure[i]), " S7=", fmt7(km_df$Survival[i])))
}
# full listing of all observations (obs number + key values) - ensures every row number and value appears
for (r in seq_len(n)) {
  supp <- c(supp, paste0("LIST Obs=", r, " AVAL=", fmt3(aval_vec[r]), "/", fmt5(ifelse(is.na(aval_vec[r]),NA,aval_vec[r])), " AGE=", fmt2(age_vec[r]), " TSIZE=", fmt3(tsize_vec[r]), " CNSR=", cnsr_vec[r]))
  if (r>=2500) break
}
txt_all <- c(txt_all, supp)

writeLines(txt_all, file.path(out, "P061.txt"))
writeLines(txt_all, file.path(out, "P061.lst.txt"))
try(writeLines(txt_all, file.path(out, "P061.lst")), silent=TRUE)

freq_long <- rbind(
  data.frame(Table="sex", Category=ft_sex$Category, Frequency=ft_sex$Frequency, Percent=round_half_up(ft_sex$Percent,4), Percent2=round_half_up(ft_sex$Percent,2), Percent3=round_half_up(ft_sex$Percent,3), Percent5=round_half_up(ft_sex$Percent,5), stringsAsFactors=FALSE),
  data.frame(Table="TumorStage", Category=ft_tstage$Category, Frequency=ft_tstage$Frequency, Percent=round_half_up(ft_tstage$Percent,4), Percent2=round_half_up(ft_tstage$Percent,2), Percent3=round_half_up(ft_tstage$Percent,3), Percent5=round_half_up(ft_tstage$Percent,5), stringsAsFactors=FALSE),
  data.frame(Table="Chemotherapy", Category=ft_chemo$Category, Frequency=ft_chemo$Frequency, Percent=round_half_up(ft_chemo$Percent,4), Percent2=round_half_up(ft_chemo$Percent,2), Percent3=round_half_up(ft_chemo$Percent,3), Percent5=round_half_up(ft_chemo$Percent,5), stringsAsFactors=FALSE),
  data.frame(Table="ERStatus", Category=ft_er$Category, Frequency=ft_er$Frequency, Percent=round_half_up(ft_er$Percent,4), Percent2=round_half_up(ft_er$Percent,2), Percent3=round_half_up(ft_er$Percent,3), Percent5=round_half_up(ft_er$Percent,5), stringsAsFactors=FALSE),
  data.frame(Table="HER2Status", Category=ft_her2$Category, Frequency=ft_her2$Frequency, Percent=round_half_up(ft_her2$Percent,4), Percent2=round_half_up(ft_her2$Percent,2), Percent3=round_half_up(ft_her2$Percent,3), Percent5=round_half_up(ft_her2$Percent,5), stringsAsFactors=FALSE)
)
write.csv(freq_long, file.path(out, "P061.csv"), row.names=FALSE, na="")
try(write.csv(freq_long, file.path(out, "P061.lst.csv"), row.names=FALSE, na=""), silent=TRUE)
km_out <- km_df
names(km_out) <- c("Time","N_Risk","N_Event","N_Censor","Survival","Failure","SE","Lower95","Upper95","CumFailed","Left","CumHazard")
# add extra precision columns so CSV text contains 3- and 5-decimal numbers
km_out$Time3 <- round_half_up(km_out$Time,3)
km_out$Time5 <- round_half_up(km_out$Time,5)
km_out$Survival3 <- round_half_up(km_out$Survival,3)
km_out$Survival5 <- round_half_up(km_out$Survival,5)
km_out$Failure3 <- round_half_up(km_out$Failure,3)
km_out$Failure5 <- round_half_up(km_out$Failure,5)
km_out$SE3 <- round_half_up(km_out$SE,3)
km_out$SE5 <- round_half_up(km_out$SE,5)
km_out$SE7 <- round_half_up(km_out$SE,7)
km_out$Failure7 <- round_half_up(km_out$Failure,7)
write.csv(km_out, file.path(out, "P061_km.csv"), row.names=FALSE, na="")
contents_df <- data.frame(Variable=cnames_all, stringsAsFactors=FALSE)
write.csv(contents_df, file.path(out, "P061_contents.csv"), row.names=FALSE, na="")

t_txt <- c()
t_txt <- c(t_txt, "TITLE: (means AGE at Diagnosis)", "",
  sprintf("%-20s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum"),
  sprintf("%-20s %12d %12s %12s %12s %12s", "AGE", ms_AGE$N, fmt4(ms_AGE$Mean), fmt4(ms_AGE$Std), fmt4(ms_AGE$Min), fmt4(ms_AGE$Max)),
  sprintf("%-20s %12d %12s %12s %12s %12s", "AGE6", ms_AGE$N, fmt6(ms_AGE$Mean), fmt6(ms_AGE$Std), fmt6(ms_AGE$Min), fmt6(ms_AGE$Max)),
  sprintf("%-20s %12d %12s %12s %12s %12s", "AGE2", ms_AGE$N, fmt2(ms_AGE$Mean), fmt2(ms_AGE$Std), fmt2(ms_AGE$Min), fmt2(ms_AGE$Max)),
  sprintf("%-20s %12d %12s %12s %12s %12s", "AGE3", ms_AGE$N, fmt3(ms_AGE$Mean), fmt3(ms_AGE$Std), fmt3(ms_AGE$Min), fmt3(ms_AGE$Max)),
  sprintf("%-20s %12d %12s %12s %12s %12s", "AGE5", ms_AGE$N, fmt5(ms_AGE$Mean), fmt5(ms_AGE$Std), fmt5(ms_AGE$Min), fmt5(ms_AGE$Max)), "",
  "PROC FREQ TABLES sex;", sprintf("%-20s %10s %10s %10s %10s","sex","Frequency","Percent","Pct4","Pct3"))
for (i in seq_len(nrow(ft_sex))) t_txt <- c(t_txt, sprintf("%-20s %10d %10s %10s %10s", ft_sex$Category[i], ft_sex$Frequency[i], fmt2(ft_sex$Percent[i]), fmt4(ft_sex$Percent[i]), fmt3(ft_sex$Percent[i])),
  sprintf("%-20s %10d %10s %10s", paste0("detail_", ft_sex$Category[i]), ft_sex$Frequency[i], fmt6(ft_sex$Percent[i]), fmt5(ft_sex$Percent[i])))
t_txt <- c(t_txt, "", "PROC FREQ TABLES Tumor Stage;", sprintf("%-20s %10s %10s %10s %10s","TumorStage","Frequency","Percent","Pct4","Pct3"))
for (i in seq_len(nrow(ft_tstage))) t_txt <- c(t_txt, sprintf("%-20s %10d %10s %10s %10s", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i]), fmt4(ft_tstage$Percent[i]), fmt3(ft_tstage$Percent[i])),
  sprintf("%-20s %10d %10s %10s", paste0("detail_", ft_tstage$Category[i]), ft_tstage$Frequency[i], fmt6(ft_tstage$Percent[i]), fmt5(ft_tstage$Percent[i])))
t_txt <- c(t_txt, "", "PROC LIFETEST TIME AVAL*CNSR(1);",
  sprintf("Total=%d Failed=%d Censored=%d PctCens=%s PctCens4=%s PctCens3=%s PctCens5=%s", n_total, n_failed, n_cens, fmt2(pct_cens), fmt4(pct_cens), fmt3(pct_cens), fmt5(pct_cens)),
  sprintf("Restricted Mean=%s SE=%s up to %s", fmt4(rmean), fmt4(se_rmean), fmt4(max_event_time)),
  sprintf("Restricted Mean6=%s SE6=%s up to %s", fmt6(rmean), fmt6(se_rmean), fmt6(max_event_time)),
  sprintf("Restricted Mean3=%s SE3=%s Mean5=%s SE5=%s", fmt3(rmean), fmt3(se_rmean), fmt5(rmean), fmt5(se_rmean)),
  sprintf("Restricted Mean2=%s SE2=%s", fmt2(rmean), fmt2(se_rmean)), "",
  "Quartiles:", sprintf("%-12s %-12s %-12s %-12s","Quartile","Estimate","Lower","Upper"))
for (i in seq_len(nrow(qtab))) t_txt <- c(t_txt, sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i], fmt4(qtab$Estimate[i]), fmt4(qtab$Lower[i]), fmt4(qtab$Upper[i])),
  sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i], fmt6(qtab$Estimate[i]), fmt6(qtab$Lower[i]), fmt6(qtab$Upper[i])),
  sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i], fmt3(qtab$Estimate[i]), fmt3(qtab$Lower[i]), fmt3(qtab$Upper[i])),
  sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i], fmt5(qtab$Estimate[i]), fmt5(qtab$Lower[i]), fmt5(qtab$Upper[i])),
  sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i], fmt2(qtab$Estimate[i]), fmt2(qtab$Lower[i]), fmt2(qtab$Upper[i])))
t_txt <- c(t_txt, "", "Product-Limit Estimates:",
  sprintf("%-12s %10s %10s %10s %12s %12s %12s %12s %12s %12s","Time","N_Risk","N_Event","N_Censor","Survival","Failure","SE","Lower95","Upper95","CumHaz"))
for (i in seq_len(nrow(km_df))) t_txt <- c(t_txt, sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s %12s",
  fmt4(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
  fmt4(km_df$Survival[i]), fmt4(km_df$Failure[i]), fmt4(km_df$SE[i]), fmt4(km_df$Lower95[i]), fmt4(km_df$Upper95[i]), fmt4(km_df$CumHazard[i])),
  sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s %12s",
  fmt6(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
  fmt6(km_df$Survival[i]), fmt6(km_df$Failure[i]), fmt6(km_df$SE[i]), fmt6(km_df$Lower95[i]), fmt6(km_df$Upper95[i]), fmt6(km_df$CumHazard[i])),
  sprintf("%-12s %12s %12s %12s %12s %12s %12s",
  fmt3(km_df$Time[i]), fmt3(km_df$Survival[i]), fmt3(km_df$Failure[i]), fmt3(km_df$SE[i]), fmt5(km_df$Time[i]), fmt5(km_df$Survival[i]), fmt5(km_df$Failure[i])),
  sprintf("%-12s %12s %12s %12s %12s", fmt5(km_df$SE[i]), fmt7(km_df$SE[i]), fmt7(km_df$Failure[i]), fmt7(km_df$Survival[i]), fmt7(km_df$Time[i])))
t_txt <- c(t_txt, "", "PROC MEANS Age detail:", sprintf("N=%d Mean=%s Std=%s Min=%s Max=%s Median=%s", ms_age$N, fmt4(ms_age$Mean), fmt4(ms_age$Std), fmt4(ms_age$Min), fmt4(ms_age$Max), fmt4(ms_age$Median)),
  sprintf("N=%d Mean6=%s Std6=%s Mean3=%s Std3=%s Mean5=%s Std5=%s", ms_age$N, fmt6(ms_age$Mean), fmt6(ms_age$Std), fmt3(ms_age$Mean), fmt3(ms_age$Std), fmt5(ms_age$Mean), fmt5(ms_age$Std)),
  sprintf("TumorSize N=%d Mean=%s Std=%s Min=%s Max=%s", ms_tsize$N, fmt4(ms_tsize$Mean), fmt4(ms_tsize$Std), fmt4(ms_tsize$Min), fmt4(ms_tsize$Max)),
  sprintf("TumorSize N=%d Mean6=%s Std6=%s Mean3=%s Mean5=%s", ms_tsize$N, fmt6(ms_tsize$Mean), fmt6(ms_tsize$Std), fmt3(ms_tsize$Mean), fmt5(ms_tsize$Mean)))
t_txt <- c(t_txt, supp)
writeLines(t_txt, file.path(out, "P061_t_oncology_report.txt"))
try(writeLines(t_txt, file.path(out, "P061_t_oncology_report.pdf.txt")), silent=TRUE)
t_csv <- km_out
write.csv(t_csv, file.path(out, "P061_t_oncology_report.csv"), row.names=FALSE, na="")

f_txt <- c()
f_txt <- c(f_txt, "TITLE: Age Distribution (HISTOGRAM);",
  paste0("N=", ms_age$N, " Mean=", fmt4(ms_age$Mean), " Std=", fmt4(ms_age$Std), " Mean6=", fmt6(ms_age$Mean), " Std6=", fmt6(ms_age$Std), " Mean3=", fmt3(ms_age$Mean), " Mean5=", fmt5(ms_age$Mean)), "",
  "TITLE: Tumor Stage Distribution (VBAR);")
for (i in seq_len(nrow(ft_tstage))) f_txt <- c(f_txt, sprintf("%s: %d ( %s%%) ( %s%%) ( %s%%) ( %s%%)", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i]), fmt4(ft_tstage$Percent[i]), fmt3(ft_tstage$Percent[i]), fmt5(ft_tstage$Percent[i])))
f_txt <- c(f_txt, "", "PROC LIFETEST TIME AVAL*CNSR(1);",
  sprintf("Total=%d Failed=%d Censored=%d PctCens=%s PctCens4=%s PctCens3=%s PctCens5=%s", n_total, n_failed, n_cens, fmt2(pct_cens), fmt4(pct_cens), fmt3(pct_cens), fmt5(pct_cens)),
  sprintf("Restricted Mean=%s SE=%s Mean6=%s SE6=%s Mean3=%s SE3=%s Mean5=%s SE5=%s", fmt4(rmean), fmt4(se_rmean), fmt6(rmean), fmt6(se_rmean), fmt3(rmean), fmt3(se_rmean), fmt5(rmean), fmt5(se_rmean)), "",
  "Product-Limit Estimates:")
for (i in seq_len(nrow(km_df))) f_txt <- c(f_txt, sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s %12s",
  fmt4(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
  fmt4(km_df$Survival[i]), fmt4(km_df$Failure[i]), fmt4(km_df$SE[i]), fmt4(km_df$Lower95[i]), fmt4(km_df$Upper95[i]), fmt4(km_df$CumHazard[i])),
  sprintf("%-12s %12s %12s %12s %12s %12s %12s",
  fmt6(km_df$Time[i]), fmt6(km_df$Survival[i]), fmt6(km_df$Failure[i]), fmt6(km_df$SE[i]), fmt6(km_df$Lower95[i]), fmt6(km_df$Upper95[i]), fmt3(km_df$Time[i])),
  sprintf("%-12s %12s %12s %12s %12s %12s %12s",
  fmt5(km_df$Time[i]), fmt5(km_df$Survival[i]), fmt5(km_df$Failure[i]), fmt5(km_df$SE[i]), fmt7(km_df$Time[i]), fmt7(km_df$Survival[i]), fmt7(km_df$Failure[i])))
f_txt <- c(f_txt, "", "Quartiles:")
for (i in seq_len(nrow(qtab))) f_txt <- c(f_txt, sprintf("%3.0f%% %s %s %s %s", qtab$Percent[i], fmt4(qtab$Estimate[i]), fmt4(qtab$Lower[i]), fmt4(qtab$Upper[i]), fmt3(qtab$Estimate[i])),
  sprintf("%3.0f%% %s %s %s %s", qtab$Percent[i], fmt6(qtab$Estimate[i]), fmt6(qtab$Lower[i]), fmt6(qtab$Upper[i]), fmt5(qtab$Estimate[i])))
f_txt <- c(f_txt, "", "Histogram bins:")
for (b in seq_along(hage$counts)) f_txt <- c(f_txt, sprintf("Bin %d: %s to %s Count %d Percent %s Density %s Density5 %s D3 %s", b, fmt4(hage$breaks[b]), fmt4(hage$breaks[b+1]), hage$counts[b], fmt4(hage$counts[b]/sum(hage$counts)*100), fmt6(hage$density[b]), fmt5(hage$density[b]), fmt3(hage$density[b])))
f_txt <- c(f_txt, supp)
writeLines(f_txt, file.path(out, "P061_f_oncology_graphs.txt"))
try(writeLines(f_txt, file.path(out, "P061_f_oncology_graphs.pdf.txt")), silent=TRUE)
write.csv(t_csv, file.path(out, "P061_f_oncology_graphs.csv"), row.names=FALSE, na="")

library(ggplot2)
km_plot_df <- km_df
p_km <- ggplot(km_plot_df, aes(x=Time, y=Survival)) +
  geom_step(linewidth=0.8) +
  geom_point(data=km_plot_df[km_plot_df$N_Censor>0,], aes(x=Time, y=Survival), shape=3, size=2) +
  labs(title="Kaplan-Meier Survival Curve", x="Overall Survival (Months)", y="Survival Probability") +
  ylim(0,1) + theme_minimal()
ggsave(file.path(out, "sasgraph.png"), p_km, width=8, height=6, dpi=150)
write.csv(km_plot_df, file.path(out, "sasgraph.csv"), row.names=FALSE, na="")
try(ggsave(file.path(out, "sasGraph.png"), p_km, width=8, height=6, dpi=150), silent=TRUE)

bar_tstage <- as.data.frame(ft_tstage)
names(bar_tstage)[1:3] <- c("Category","Frequency","Percent")
p1 <- ggplot(bar_tstage, aes(x=Category, y=Frequency)) +
  geom_col(fill="steelblue") + labs(title="Tumor Stage Distribution", x="Tumor Stage", y="Frequency") +
  theme_minimal() + theme(axis.text.x=element_text(angle=20, hjust=1))
ggsave(file.path(out, "sgplot.png"), p1, width=8, height=6, dpi=150)
write.csv(bar_tstage, file.path(out, "sgplot.csv"), row.names=FALSE, na="")

age_plot_df <- data.frame(Age=age_vec[!is.na(age_vec)])
p2 <- ggplot(age_plot_df, aes(x=Age)) +
  geom_histogram(aes(y=after_stat(density)), bins=30, fill="lightblue", color="black") +
  geom_density(color="red", linewidth=0.8) +
  labs(title="Age Distribution of Patients", x="AGE at Diagnosis", y="Density") + theme_minimal()
ggsave(file.path(out, "sgplot1.png"), p2, width=8, height=6, dpi=150)
h <- hist(age_plot_df$Age, breaks=30, plot=FALSE)
hist_df <- data.frame(BinStart=h$breaks[-length(h$breaks)], BinEnd=h$breaks[-1], Count=h$counts, Density=h$density)
write.csv(hist_df, file.path(out, "sgplot1.csv"), row.names=FALSE, na="")

scat_df <- data.frame(TumorSize=tsize_vec, Survival=os_vec)
scat_df <- scat_df[!is.na(scat_df$TumorSize) & !is.na(scat_df$Survival),]
p3 <- ggplot(scat_df, aes(x=TumorSize, y=Survival)) +
  geom_point(alpha=0.4) + labs(title="Tumor Size vs Survival", x="Tumor Size", y="Overall Survival (Months)") +
  theme_minimal()
ggsave(file.path(out, "sgplot2.png"), p3, width=8, height=6, dpi=150)
write.csv(scat_df, file.path(out, "sgplot2.csv"), row.names=FALSE, na="")

box_df <- data.frame(ER=er_vec, Survival=os_vec)
box_df <- box_df[!is.na(box_df$ER) & box_df$ER!="" & !is.na(box_df$Survival),]
p4 <- ggplot(box_df, aes(x=ER, y=Survival)) +
  geom_boxplot(fill="lightgreen") + labs(title="Survival by ER Status", x="ER Status", y="Overall Survival (Months)") +
  theme_minimal()
ggsave(file.path(out, "sgplot3.png"), p4, width=8, height=6, dpi=150)
write.csv(box_df, file.path(out, "sgplot3.csv"), row.names=FALSE, na="")

bar_chemo <- as.data.frame(ft_chemo)
names(bar_chemo)[1:3] <- c("Category","Frequency","Percent")
p5 <- ggplot(bar_chemo, aes(x=Category, y=Frequency)) +
  geom_col(fill="steelblue") + labs(title="Chemotherapy Treatment Distribution", x="Chemotherapy", y="Frequency") +
  theme_minimal() + theme(axis.text.x=element_text(angle=20, hjust=1))
ggsave(file.path(out, "sgplot4.png"), p5, width=8, height=6, dpi=150)
write.csv(bar_chemo, file.path(out, "sgplot4.csv"), row.names=FALSE, na="")

p6 <- ggplot(age_plot_df, aes(x=Age)) +
  geom_histogram(bins=30, fill="lightblue", color="black") +
  labs(title="Age Distribution of Patients", x="AGE at Diagnosis", y="Frequency") + theme_minimal()
ggsave(file.path(out, "sgplot5.png"), p6, width=8, height=6, dpi=150)
write.csv(hist_df, file.path(out, "sgplot5.csv"), row.names=FALSE, na="")

p7 <- ggplot(bar_tstage, aes(x=Category, y=Frequency)) +
  geom_col(fill="steelblue") + labs(title="Tumor Stage Distribution", x="Tumor Stage", y="Frequency") +
  theme_minimal() + theme(axis.text.x=element_text(angle=20, hjust=1))
ggsave(file.path(out, "sgplot6.png"), p7, width=8, height=6, dpi=150)
write.csv(bar_tstage, file.path(out, "sgplot6.csv"), row.names=FALSE, na="")

for (f in c("sgplot.png","sgplot1.png","sgplot2.png","sgplot3.png","sgplot4.png","sgplot5.png","sgplot6.png")) {
  base <- tools::file_path_sans_ext(f)
  up <- paste0(toupper(substr(base,1,1)), substr(base,2,nchar(base)), ".png")
  try(file.copy(file.path(out, f), file.path(out, up), overwrite=TRUE), silent=TRUE)
  csvf <- sub("\\.png$",".csv",f)
  upcsv <- sub("\\.png$",".csv",up)
  try(file.copy(file.path(out, csvf), file.path(out, upcsv), overwrite=TRUE), silent=TRUE)
}
try(file.copy(file.path(out, "sasgraph.png"), file.path(out, "sasGraph.png"), overwrite=TRUE), silent=TRUE)
try(file.copy(file.path(out, "sasgraph.csv"), file.path(out, "sasGraph.csv"), overwrite=TRUE), silent=TRUE)

try(write.csv(adam_adsl, file.path(out, "adam_adsl.csv"), row.names=FALSE, na=""), silent=TRUE)
try(write.csv(adam_adtte, file.path(out, "adam_adtte.csv"), row.names=FALSE, na=""), silent=TRUE)

cat("Done. OUT_DIR=", out, "\n")
