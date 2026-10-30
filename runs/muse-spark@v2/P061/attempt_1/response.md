```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits=2) {
  ifelse(is.na(x), NA_real_,
    trunc(x * 10^digits + sign(x)*0.5 + 1e-12) / 10^digits)
}
fmt2 <- function(x, d=2) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, d), format="f", digits=d, flag="#"))
}
fmt4 <- function(x, d=4) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, d), format="f", digits=d))
}

rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawfile <- file.path(rawdir, "sumit-oncology/Breast Cancer METABRIC.csv")
# fallback: list
if (!file.exists(rawfile)) {
  cand <- list.files(rawdir, pattern="\\.csv$", recursive=TRUE, full.names=TRUE)
  if (length(cand) > 0) {
    # pick METABRIC if present
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

# helper: find column case-insensitive exact, else normalized
find_col <- function(df, target) {
  nms <- names(df)
  # exact case-insensitive
  idx <- which(tolower(trimws(nms)) == tolower(trimws(target)))
  if (length(idx)>0) return(nms[idx[1]])
  # normalized: remove non-alnum, lower
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
# effective age column for means: prefer spaced
cn_age_spaced <- if (!is.na(cn_Age_sp)) cn_Age_sp else if (!is.na(cn_AGE_sp)) cn_AGE_sp else cn_age_diag_us
cn_sex <- col_or_na(raw_metabric, "sex")
cn_tstage <- col_or_na(raw_metabric, "Tumor Stage")
cn_tsize <- col_or_na(raw_metabric, "Tumor Size")
cn_os <- col_or_na(raw_metabric, "Overall Survival (Months)")
cn_osstat <- col_or_na(raw_metabric, "Overall Survival Status")
cn_chemo <- col_or_na(raw_metabric, "Chemotherapy")
cn_er <- col_or_na(raw_metabric, "ER Status")
cn_her2 <- col_or_na(raw_metabric, "HER2 Status")

# Build SDTM DM / ADaM ADSL
n <- nrow(raw_metabric)
sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
# USUBJID = catx("-", STUDYID, patient_id)
pid_vec <- get_vec(raw_metabric, "patient_id")
# if patient_id col missing, try Patient ID
if (all(is.na(pid_vec)) && is.na(cn_patient_id)) {
  pid_vec <- get_vec(raw_metabric, "Patient ID")
}
usubjid <- ifelse(is.na(pid_vec) | pid_vec=="", "ONC001",
  paste0("ONC001-", trimws(as.character(pid_vec))))
sdtm_dm$USUBJID <- usubjid
# AGE = age_at_diagnosis
age_us_vec <- suppressWarnings(as.numeric(get_vec(raw_metabric, "age_at_diagnosis")))
if (all(is.na(age_us_vec))) {
  # fallback to spaced age
  age_us_vec <- suppressWarnings(as.numeric(get_vec(raw_metabric, "Age at Diagnosis")))
}
sdtm_dm$AGE <- age_us_vec

adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

# Survival dataset
os_vec <- suppressWarnings(as.numeric(get_vec(adam_adsl, "Overall Survival (Months)")))
osstat_vec <- as.character(get_vec(adam_adsl, "Overall Survival Status"))
cnsr <- ifelse(!is.na(osstat_vec) & osstat_vec=="Deceased", 0, 1)
# SAS: if Deceased then 0 else 1 (missing ->1)
adam_adtte <- adam_adsl
adam_adtte$AVAL <- os_vec
adam_adtte$CNSR <- cnsr

# Extract vectors for analysis (use spaced names as SAS does)
age_vec <- suppressWarnings(as.numeric(get_vec(adam_adsl, "Age at Diagnosis")))
# if still all NA try AGE
if (all(is.na(age_vec))) age_vec <- suppressWarnings(as.numeric(adam_adsl$AGE))
tsize_vec <- suppressWarnings(as.numeric(get_vec(adam_adsl, "Tumor Size")))
sex_vec <- as.character(get_vec(adam_adsl, "sex"))
tstage_vec <- as.character(get_vec(adam_adsl, "Tumor Stage"))
chemo_vec <- as.character(get_vec(adam_adsl, "Chemotherapy"))
er_vec <- as.character(get_vec(adam_adsl, "ER Status"))
her2_vec <- as.character(get_vec(adam_adsl, "HER2 Status"))
aval_vec <- suppressWarnings(as.numeric(adam_adtte$AVAL))
cnsr_vec <- suppressWarnings(as.numeric(adam_adtte$CNSR))

# ---------- PROC MEANS helper ----------
means_stats <- function(x) {
  x <- x[!is.na(x)]
  nn <- length(x)
  if (nn==0) return(list(N=0, Mean=NA_real_, Std=NA_real_, Min=NA_real_, Max=NA_real_))
  m <- mean(x)
  s <- if (nn>1) stats::sd(x) else NA_real_
  list(N=nn, Mean=m, Std=s, Min=min(x), Max=max(x))
}
ms_age <- means_stats(age_vec)
ms_tsize <- means_stats(tsize_vec)
ms_AGE <- means_stats(suppressWarnings(as.numeric(get_vec(adam_adsl, "AGE at Diagnosis"))))
# if AGE spaced missing, use age_vec
if (ms_AGE$N==0) ms_AGE <- ms_age

# ---------- PROC FREQ helper ----------
freq_table <- function(vec) {
  # SAS: missing? By default PROC FREQ excludes missing unless / MISSING. Assume exclude missing for display but show N.
  # Keep character, treat "" and NA as missing
  v <- trimws(as.character(vec))
  v[v=="" | v=="NA"] <- NA
  # SAS sorts by formatted? Default order is data? Actually PROC FREQ default order=DATA? No, default is alphabetical? For character, order of appearance? Default ORDER=DATA? No default is ORDER=DATA? Let's use sorted character ordering (uppercase before lowercase) to match SAS sort.
  # We'll sort unique values with SAS ordering: sort() in R with locale? Use base sort which is locale-dependent; force C locale ordering via xtfrm? Use order with sorting binary.
  # Simple: sort unique using base R sort (ASCII: uppercase before lowercase in C locale)
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

# ---------- PROC LIFETEST (KM) ----------
# Exclude missing AVAL
km_idx <- which(!is.na(aval_vec) & !is.na(cnsr_vec))
km_time <- aval_vec[km_idx]
km_cnsr <- cnsr_vec[km_idx]
# event = 1 if CNSR=0
km_event <- ifelse(km_cnsr==0, 1, 0)
library(survival)
sfit <- survival::survfit(survival::Surv(km_time, km_event) ~ 1, conf.type="log-log")
ssum <- summary(sfit, times=sort(unique(km_time)), extend=TRUE)
# Build product-limit table: one row per unique time
ut <- sort(unique(km_time))
# Use survfit summary at all times
s_all <- summary(sfit, times=ut, extend=TRUE)
# n.risk, n.event, n.censor, survival, std.err, lower, upper
# s_all$n.risk etc may be length ut
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
# Number failed / left: cumulative
km_df$CumFailed <- cumsum(km_df$N_Event)
km_df$Left <- km_df$N_Risk - km_df$N_Event - km_df$N_Censor
# Quartiles with CI (SAS prints 25,50,75 with 95% CI)
qt <- tryCatch(quantile(sfit, probs=c(0.75,0.5,0.25), conf.int=TRUE), error=function(e) NULL)
# quantile returns survival quantiles: 75% survival time etc. SAS labels Percent censored? Actually quartiles: 75,50,25 survival.
qtab <- NULL
if (!is.null(qt)) {
  qv <- tryCatch(as.numeric(qt$quantile), error=function(e) rep(NA,3))
  ql <- tryCatch(as.numeric(qt$lower), error=function(e) rep(NA,3))
  qu <- tryCatch(as.numeric(qt$upper), error=function(e) rep(NA,3))
  qtab <- data.frame(Percent=c(75,50,25), Estimate=qv, Lower=ql, Upper=qu)
} else {
  # fallback via survfit median
  med <- tryCatch(survival::surv_median(sfit), error=function(e) NA)
  qtab <- data.frame(Percent=c(75,50,25), Estimate=c(NA, med, NA), Lower=NA, Upper=NA)
}
# Mean restricted to largest event time + SE (SAS: restricted to largest event time)
# survfit print gives restricted mean
# compute via integration of step function up to max event time
max_event_time <- if (any(km_event==1)) max(km_time[km_event==1]) else max(km_time)
# Use survfit mean: summary print
sprint <- tryCatch(capture.output(print(sfit, print.rmean="common")), error=function(e) "")
# compute restricted mean manually: area under curve up to max_event_time
# order times including 0
ord <- order(km_df$Time)
k2 <- km_df[ord,]
# prepend time 0 survival 1
t0 <- c(0, k2$Time)
s0 <- c(1, k2$Survival)
# restrict to <= max_event_time
keep <- t0 <= max_event_time
# need survival at max_event_time: already included
tR <- t0[keep]; sR <- s0[keep]
# if last tR < max_event_time, extend? but max included
rmean <- sum(diff(tR) * sR[-length(sR)])
# SE via Greenwood integral (approx using survfit std.err): use delta method approx
# Use formula: var = sum(d_i / (Y_i*(Y_i-d_i)) * (integral from t_i to tau S)^2 )
# compute
tau <- max_event_time
# integral from each time to tau
# S integral tail
# compute tail integral for each interval
nR <- length(tR)
# area from t_i to tau
tail <- numeric(nR)
# sR length nR
for (i in seq_len(nR)) {
  # sum_{j>=i} (t_{j+1}-t_j)*s_j where t_{nR+1}=tau? Actually tR includes tau as last; need handling
  if (i < nR) tail[i] <- sum(diff(tR[i:nR]) * sR[i:(nR-1)])
  else tail[i] <- 0
}
# map to km_df rows <= tau
# Greenwood terms: for each event time ti with d, Y
# Use km_df event rows
ev_rows <- k2[k2$N_Event>0 & k2$Time<=tau,]
var_rmean <- 0
for (r in seq_len(nrow(ev_rows))) {
  ti <- ev_rows$Time[r]; d <- ev_rows$N_Event[r]; Y <- ev_rows$N_Risk[r]
  # find index in tR
  ii <- which(tR==ti)
  if (length(ii)==0) next
  # tail from ti: integral from ti to tau = tail[ii]
  if (Y>0 && (Y-d)>0) var_rmean <- var_rmean + d/(Y*(Y-d)) * (tail[ii]^2)
}
se_rmean <- sqrt(var_rmean)
# Censored summary
n_total <- length(km_time)
n_failed <- sum(km_event==1, na.rm=TRUE)
n_cens <- sum(km_event==0, na.rm=TRUE)
pct_cens <- if (n_total>0) n_cens/n_total*100 else NA

# Rank statistics not applicable (single sample) -> note

# ---------- Text rendering builders ----------
line <- function(s="") s
txt_all <- c()
add <- function(v) { txt_all <<- c(txt_all, v) }

add("Project: Oncology Clinical Trial Analysis")
add("Standard: CDISC SDTM / ADaM")
add("Dataset: Breast Cancer METABRIC")
add(paste0("Raw file: ", rawfile))
add(paste0("N observations (raw): ", nrow(raw_metabric)))
add("")

# PROC CONTENTS
add("PROC CONTENTS DATA=adam_adsl;")
add("Variables:")
cnames <- names(adam_adsl)
ctypes <- sapply(adam_adsl, function(x) if (is.numeric(x)) "Num" else "Char")
for (i in seq_along(cnames)) {
  add(paste0(i, "  ", cnames[i], "  ", ctypes[i]))
}
add("")

# PROC PRINT obs=10
add("PROC PRINT DATA=adam_adsl (OBS=10);")
add("TITLE: (no title)")
# show first 10 rows selected cols
sel_cols <- c("STUDYID","DOMAIN","USUBJID","AGE","TRT01P","SAFFL")
sel_cols <- sel_cols[sel_cols %in% names(adam_adsl)]
# also include key raw cols if present
extra <- c()
for (cc in c(cn_age_spaced, cn_sex, cn_tstage, cn_tsize, cn_os, cn_osstat, cn_chemo, cn_er, cn_her2)) {
  if (!is.na(cc) && cc %in% names(adam_adsl) && !(cc %in% sel_cols)) extra <- c(extra, cc)
}
sel_cols <- c(sel_cols, extra)
prt <- head(adam_adsl[, sel_cols, drop=FALSE], 10)
add(paste(sel_cols, collapse=" | "))
for (r in seq_len(nrow(prt))) {
  vals <- sapply(prt[r,,drop=FALSE], function(x) ifelse(is.na(x),"",as.character(x)))
  add(paste(vals, collapse=" | "))
}
add("")

# PROC MEANS Age
add('TITLE: Demographic Summary - Age;')
add("PROC MEANS DATA=adam_adsl N MEAN STD MIN MAX;")
add(paste0("VAR: ", ifelse(is.na(cn_age_spaced),"Age at Diagnosis",cn_age_spaced)))
add(sprintf("%-20s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum"))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "Age",
  ms_age$N, fmt2(ms_age$Mean), fmt2(ms_age$Std), fmt2(ms_age$Min), fmt2(ms_age$Max)))
add("")

# PROC FREQ sex
add("TITLE: Gender Distribution;")
add("PROC FREQ DATA=adam_adsl; TABLES sex;")
add(sprintf("%-20s %10s %10s %10s %10s", "sex","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_sex))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_sex$Category[i], ft_sex$Frequency[i],
    fmt2(ft_sex$Percent[i]), ft_sex$CumulativeFrequency[i], fmt2(ft_sex$CumulativePercent[i])))
}
add(paste0("Total Frequency = ", sum(ft_sex$Frequency)))
add("")

# PROC FREQ Tumor Stage
add("TITLE: Tumor Stage Distribution;")
add("PROC FREQ DATA=adam_adsl; TABLES Tumor Stage;")
add(sprintf("%-20s %10s %10s %10s %10s", "TumorStage","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_tstage))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_tstage$Category[i], ft_tstage$Frequency[i],
    fmt2(ft_tstage$Percent[i]), ft_tstage$CumulativeFrequency[i], fmt2(ft_tstage$CumulativePercent[i])))
}
add(paste0("Total Frequency = ", sum(ft_tstage$Frequency)))
add("")

# PROC MEANS Tumor Size
add("TITLE: Tumor Size Summary;")
add("PROC MEANS DATA=adam_adsl N MEAN STD MIN MAX;")
add(sprintf("%-20s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum"))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "TumorSize",
  ms_tsize$N, fmt2(ms_tsize$Mean), fmt2(ms_tsize$Std), fmt2(ms_tsize$Min), fmt2(ms_tsize$Max)))
add("")

# PROC LIFETEST
add("TITLE: Kaplan-Meier Survival Curve;")
add("PROC LIFETEST DATA=adam_adtte PLOTS=SURVIVAL; TIME AVAL*CNSR(1);")
add("")
add("Censored Summary:")
add(sprintf("%-12s %12s %12s %12s", "Total","Failed","Censored","Pct Censored"))
add(sprintf("%-12d %12d %12d %12s", n_total, n_failed, n_cens, fmt2(pct_cens)))
add("")
add("Summary Statistics for Time Variable AVAL:")
add(sprintf("%-12s %-12s %-12s %-12s", "Quartile","Estimate","Lower95","Upper95"))
for (i in seq_len(nrow(qtab))) {
  add(sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i],
    fmt2(qtab$Estimate[i]), fmt2(qtab$Lower[i]), fmt2(qtab$Upper[i])))
}
add("")
add(sprintf("Restricted Mean (up to %s): %s  SE=%s", fmt2(max_event_time), fmt2(rmean), fmt4(se_rmean)))
add("")
add("Product-Limit Survival Estimates:")
add(sprintf("%-12s %10s %10s %10s %12s %12s %12s %12s %12s %10s %10s",
  "Time","N_Risk","N_Event","N_Censor","Survival","Failure","SE","Lower95","Upper95","CumFail","Left"))
for (i in seq_len(nrow(km_df))) {
  add(sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s %10d %10d",
    fmt2(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
    fmt4(km_df$Survival[i]), fmt4(km_df$Failure[i]), fmt4(km_df$SE[i]),
    fmt4(km_df$Lower95[i]), fmt4(km_df$Upper95[i]), km_df$CumFailed[i], km_df$Left[i]))
}
add("")
add("Rank Statistics and Tests: (single sample - no strata, no test)")
add("Covariance Matrix: (single sample - not applicable)")
add("")

# Treatment freq
add("TITLE: Chemotherapy Treatment Summary;")
add("PROC FREQ DATA=adam_adsl; TABLES Chemotherapy;")
add(sprintf("%-20s %10s %10s %10s %10s", "Chemotherapy","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_chemo))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_chemo$Category[i], ft_chemo$Frequency[i],
    fmt2(ft_chemo$Percent[i]), ft_chemo$CumulativeFrequency[i], fmt2(ft_chemo$CumulativePercent[i])))
}
add(paste0("Total Frequency = ", sum(ft_chemo$Frequency)))
add("")

# ER / HER2
add("PROC FREQ DATA=adam_adsl; TABLES ER Status HER2 Status;")
add("Table of ER Status:")
add(sprintf("%-20s %10s %10s %10s %10s", "ERStatus","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_er))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_er$Category[i], ft_er$Frequency[i],
    fmt2(ft_er$Percent[i]), ft_er$CumulativeFrequency[i], fmt2(ft_er$CumulativePercent[i])))
}
add(paste0("Total Frequency = ", sum(ft_er$Frequency)))
add("Table of HER2 Status:")
add(sprintf("%-20s %10s %10s %10s %10s", "HER2Status","Frequency","Percent","Cum Freq","Cum Pct"))
for (i in seq_len(nrow(ft_her2))) {
  add(sprintf("%-20s %10d %10s %10d %10s", ft_her2$Category[i], ft_her2$Frequency[i],
    fmt2(ft_her2$Percent[i]), ft_her2$CumulativeFrequency[i], fmt2(ft_her2$CumulativePercent[i])))
}
add(paste0("Total Frequency = ", sum(ft_her2$Frequency)))
add("")

# TLF PDF section repeats
add("ODS PDF FILE=P061_t_oncology_report.pdf;")
add("PROC MEANS DATA=adam_adsl; VAR AGE at Diagnosis;")
add(sprintf("%-20s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum"))
add(sprintf("%-20s %12d %12s %12s %12s %12s", "AGE",
  ms_AGE$N, fmt2(ms_AGE$Mean), fmt2(ms_AGE$Std), fmt2(ms_AGE$Min), fmt2(ms_AGE$Max)))
add("PROC FREQ TABLES sex (repeat):")
for (i in seq_len(nrow(ft_sex))) {
  add(sprintf("%-20s %10d %10s", ft_sex$Category[i], ft_sex$Frequency[i], fmt2(ft_sex$Percent[i])))
}
add("PROC FREQ TABLES Tumor Stage (repeat):")
for (i in seq_len(nrow(ft_tstage))) {
  add(sprintf("%-20s %10d %10s", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i])))
}
add("PROC LIFETEST (repeat in PDF) - see estimates above")
add("ODS PDF CLOSE;")
add("")
add("PROC LIFETEST Figure 1 repeat - TITLE: Kaplan-Meier Survival Curve; (estimates as above)")
add("")
add("PROC SGPLOT VBAR Tumor Stage; TITLE: Tumor Stage Distribution;")
for (i in seq_len(nrow(ft_tstage))) {
  add(sprintf("Bar %s: %d ( %s%%)", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i])))
}
add("")
add("PROC SGPLOT HISTOGRAM AGE at Diagnosis; TITLE: Age Distribution of Patients;")
add(paste0("Histogram N=", ms_age$N, " Mean=", fmt2(ms_age$Mean), " Std=", fmt2(ms_age$Std)))
add("")
add("PROC SGPLOT SCATTER Tumor Size vs Survival; TITLE: Tumor Size vs Survival;")
add(paste0("Scatter N pairs=", sum(!is.na(tsize_vec) & !is.na(os_vec))))
add("")
add("PROC SGPLOT VBOX Overall Survival by ER Status; TITLE: Survival by ER Status;")
# box stats per ER
er_levels <- unique(er_vec[!is.na(er_vec)])
for (lv in sort(unique(er_vec[!is.na(er_vec)]), method="radix")) {
  vv <- os_vec[er_vec==lv & !is.na(os_vec)]
  if (length(vv)==0) next
  add(sprintf("ER=%s N=%d Mean=%s Median=%s Min=%s Max=%s", lv, length(vv),
    fmt2(mean(vv)), fmt2(stats::median(vv)), fmt2(min(vv)), fmt2(max(vv))))
}
add("")
add("PROC SGPLOT VBAR Chemotherapy; TITLE: Chemotherapy Treatment Distribution;")
for (i in seq_len(nrow(ft_chemo))) {
  add(sprintf("Bar %s: %d ( %s%%)", ft_chemo$Category[i], ft_chemo$Frequency[i], fmt2(ft_chemo$Percent[i])))
}
add("")
add("ODS PDF FILE=P061_f_oncology_graphs.pdf; HISTOGRAM + VBAR + LIFETEST (as above); ODS PDF CLOSE;")

# Write P061 lst text
writeLines(txt_all, file.path(out, "P061.txt"))
writeLines(txt_all, file.path(out, "P061.lst.txt"))
# also exact name P061.lst if allowed (no extension issue)
try(writeLines(txt_all, file.path(out, "P061.lst")), silent=TRUE)

# CSV of table body for P061: combine freq/means/km? Provide long CSV
# Build generic CSV with Section, Category, Frequency, Percent, Statistic, Value
csv_rows <- list()
csv_rows[[1]] <- data.frame(Section="MEANS_Age", Variable="Age", N=ms_age$N, Mean=round_half_up(ms_age$Mean,4),
  Std=round_half_up(ms_age$Std,4), Min=ms_age$Min, Max=ms_age$Max, stringsAsFactors=FALSE)
csv_rows[[2]] <- data.frame(Section="MEANS_TumorSize", Variable="TumorSize", N=ms_tsize$N, Mean=round_half_up(ms_tsize$Mean,4),
  Std=round_half_up(ms_tsize$Std,4), Min=ms_tsize$Min, Max=ms_tsize$Max, stringsAsFactors=FALSE)
# freq combined
freq_long <- rbind(
  data.frame(Table="sex", Category=ft_sex$Category, Frequency=ft_sex$Frequency, Percent=round_half_up(ft_sex$Percent,2), stringsAsFactors=FALSE),
  data.frame(Table="TumorStage", Category=ft_tstage$Category, Frequency=ft_tstage$Frequency, Percent=round_half_up(ft_tstage$Percent,2), stringsAsFactors=FALSE),
  data.frame(Table="Chemotherapy", Category=ft_chemo$Category, Frequency=ft_chemo$Frequency, Percent=round_half_up(ft_chemo$Percent,2), stringsAsFactors=FALSE),
  data.frame(Table="ERStatus", Category=ft_er$Category, Frequency=ft_er$Frequency, Percent=round_half_up(ft_er$Percent,2), stringsAsFactors=FALSE),
  data.frame(Table="HER2Status", Category=ft_her2$Category, Frequency=ft_her2$Frequency, Percent=round_half_up(ft_her2$Percent,2), stringsAsFactors=FALSE)
)
write.csv(freq_long, file.path(out, "P061.csv"), row.names=FALSE, na="")
try(write.csv(freq_long, file.path(out, "P061.lst.csv"), row.names=FALSE, na=""), silent=TRUE)
# KM csv
km_out <- km_df
names(km_out) <- c("Time","N_Risk","N_Event","N_Censor","Survival","Failure","SE","Lower95","Upper95","CumFailed","Left")
write.csv(km_out, file.path(out, "P061_km.csv"), row.names=FALSE, na="")
# contents csv
contents_df <- data.frame(Variable=cnames, Type=ctypes, stringsAsFactors=FALSE)
write.csv(contents_df, file.path(out, "P061_contents.csv"), row.names=FALSE, na="")

# ---------- Sub-reports ----------
# t_oncology_report: means AGE, freq sex, freq tstage, lifetest
t_txt <- c()
t_txt <- c(t_txt, "TITLE: (means AGE at Diagnosis)", "",
  sprintf("%-20s %12s %12s %12s %12s %12s", "Variable","N","Mean","Std Dev","Minimum","Maximum"),
  sprintf("%-20s %12d %12s %12s %12s %12s", "AGE", ms_AGE$N, fmt2(ms_AGE$Mean), fmt2(ms_AGE$Std), fmt2(ms_AGE$Min), fmt2(ms_AGE$Max)), "",
  "PROC FREQ TABLES sex;", sprintf("%-20s %10s %10s","sex","Frequency","Percent"))
for (i in seq_len(nrow(ft_sex))) t_txt <- c(t_txt, sprintf("%-20s %10d %10s", ft_sex$Category[i], ft_sex$Frequency[i], fmt2(ft_sex$Percent[i])))
t_txt <- c(t_txt, "", "PROC FREQ TABLES Tumor Stage;", sprintf("%-20s %10s %10s","TumorStage","Frequency","Percent"))
for (i in seq_len(nrow(ft_tstage))) t_txt <- c(t_txt, sprintf("%-20s %10d %10s", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i])))
t_txt <- c(t_txt, "", "PROC LIFETEST TIME AVAL*CNSR(1);",
  sprintf("Total=%d Failed=%d Censored=%d PctCens=%s", n_total, n_failed, n_cens, fmt2(pct_cens)),
  sprintf("Restricted Mean=%s SE=%s up to %s", fmt2(rmean), fmt4(se_rmean), fmt2(max_event_time)), "",
  "Quartiles:", sprintf("%-12s %-12s %-12s %-12s","Quartile","Estimate","Lower","Upper"))
for (i in seq_len(nrow(qtab))) t_txt <- c(t_txt, sprintf("%-12.0f %-12s %-12s %-12s", qtab$Percent[i], fmt2(qtab$Estimate[i]), fmt2(qtab$Lower[i]), fmt2(qtab$Upper[i])))
t_txt <- c(t_txt, "", "Product-Limit Estimates:",
  sprintf("%-12s %10s %10s %10s %12s %12s %12s %12s %12s","Time","N_Risk","N_Event","N_Censor","Survival","Failure","SE","Lower95","Upper95"))
for (i in seq_len(nrow(km_df))) t_txt <- c(t_txt, sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s",
  fmt2(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
  fmt4(km_df$Survival[i]), fmt4(km_df$Failure[i]), fmt4(km_df$SE[i]), fmt4(km_df$Lower95[i]), fmt4(km_df$Upper95[i])))
writeLines(t_txt, file.path(out, "P061_t_oncology_report.txt"))
try(writeLines(t_txt, file.path(out, "P061_t_oncology_report.pdf.txt")), silent=TRUE)
t_csv <- km_out
write.csv(t_csv, file.path(out, "P061_t_oncology_report.csv"), row.names=FALSE, na="")

f_txt <- c()
f_txt <- c(f_txt, "TITLE: Age Distribution (HISTOGRAM);",
  paste0("N=", ms_age$N, " Mean=", fmt2(ms_age$Mean), " Std=", fmt2(ms_age$Std)), "",
  "TITLE: Tumor Stage Distribution (VBAR);")
for (i in seq_len(nrow(ft_tstage))) f_txt <- c(f_txt, sprintf("%s: %d ( %s%%)", ft_tstage$Category[i], ft_tstage$Frequency[i], fmt2(ft_tstage$Percent[i])))
f_txt <- c(f_txt, "", "PROC LIFETEST TIME AVAL*CNSR(1);",
  sprintf("Total=%d Failed=%d Censored=%d PctCens=%s", n_total, n_failed, n_cens, fmt2(pct_cens)),
  sprintf("Restricted Mean=%s SE=%s", fmt2(rmean), fmt4(se_rmean)), "",
  "Product-Limit Estimates:")
for (i in seq_len(nrow(km_df))) f_txt <- c(f_txt, sprintf("%-12s %10d %10d %10d %12s %12s %12s %12s %12s",
  fmt2(km_df$Time[i]), km_df$N_Risk[i], km_df$N_Event[i], km_df$N_Censor[i],
  fmt4(km_df$Survival[i]), fmt4(km_df$Failure[i]), fmt4(km_df$SE[i]), fmt4(km_df$Lower95[i]), fmt4(km_df$Upper95[i])))
writeLines(f_txt, file.path(out, "P061_f_oncology_graphs.txt"))
try(writeLines(f_txt, file.path(out, "P061_f_oncology_graphs.pdf.txt")), silent=TRUE)
write.csv(t_csv, file.path(out, "P061_f_oncology_graphs.csv"), row.names=FALSE, na="")

# ---------- Figures ----------
library(ggplot2)
# KM plot data
km_plot_df <- km_df
p_km <- ggplot(km_plot_df, aes(x=Time, y=Survival)) +
  geom_step(linewidth=0.8) +
  geom_point(data=km_plot_df[km_plot_df$N_Censor>0,], aes(x=Time, y=Survival), shape=3, size=2) +
  labs(title="Kaplan-Meier Survival Curve", x="Overall Survival (Months)", y="Survival Probability") +
  ylim(0,1) + theme_minimal()
ggsave(file.path(out, "sasgraph.png"), p_km, width=8, height=6, dpi=150)
write.csv(km_plot_df, file.path(out, "sasgraph.csv"), row.names=FALSE, na="")
# also uppercase alias
try(ggsave(file.path(out, "sasGraph.png"), p_km, width=8, height=6, dpi=150), silent=TRUE)

# SGPLOTs in order: vbar tstage, hist age, scatter, vbox, vbar chemo, hist age repeat, vbar tstage repeat
# 1: vbar tstage
bar_tstage <- as.data.frame(ft_tstage)
names(bar_tstage)[1:3] <- c("Category","Frequency","Percent")
p1 <- ggplot(bar_tstage, aes(x=Category, y=Frequency)) +
  geom_col(fill="steelblue") + labs(title="Tumor Stage Distribution", x="Tumor Stage", y="Frequency") +
  theme_minimal() + theme(axis.text.x=element_text(angle=20, hjust=1))
ggsave(file.path(out, "sgplot.png"), p1, width=8, height=6, dpi=150)
write.csv(bar_tstage, file.path(out, "sgplot.csv"), row.names=FALSE, na="")

# 2: histogram age + density
age_plot_df <- data.frame(Age=age_vec[!is.na(age_vec)])
p2 <- ggplot(age_plot_df, aes(x=Age)) +
  geom_histogram(aes(y=after_stat(density)), bins=30, fill="lightblue", color="black") +
  geom_density(color="red", linewidth=0.8) +
  labs(title="Age Distribution of Patients", x="AGE at Diagnosis", y="Density") + theme_minimal()
ggsave(file.path(out, "sgplot1.png"), p2, width=8, height=6, dpi=150)
# histogram binned data for csv
h <- hist(age_plot_df$Age, breaks=30, plot=FALSE)
hist_df <- data.frame(BinStart=h$breaks[-length(h$breaks)], BinEnd=h$breaks[-1], Count=h$counts, Density=h$density)
write.csv(hist_df, file.path(out, "sgplot1.csv"), row.names=FALSE, na="")

# 3: scatter tumor size vs survival
scat_df <- data.frame(TumorSize=tsize_vec, Survival=os_vec)
scat_df <- scat_df[!is.na(scat_df$TumorSize) & !is.na(scat_df$Survival),]
p3 <- ggplot(scat_df, aes(x=TumorSize, y=Survival)) +
  geom_point(alpha=0.4) + labs(title="Tumor Size vs Survival", x="Tumor Size", y="Overall Survival (Months)") +
  theme_minimal()
ggsave(file.path(out, "sgplot2.png"), p3, width=8, height=6, dpi=150)
write.csv(scat_df, file.path(out, "sgplot2.csv"), row.names=FALSE, na="")

# 4: vbox survival by ER
box_df <- data.frame(ER=er_vec, Survival=os_vec)
box_df <- box_df[!is.na(box_df$ER) & box_df$ER!="" & !is.na(box_df$Survival),]
p4 <- ggplot(box_df, aes(x=ER, y=Survival)) +
  geom_boxplot(fill="lightgreen") + labs(title="Survival by ER Status", x="ER Status", y="Overall Survival (Months)") +
  theme_minimal()
ggsave(file.path(out, "sgplot3.png"), p4, width=8, height=6, dpi=150)
write.csv(box_df, file.path(out, "sgplot3.csv"), row.names=FALSE, na="")

# 5: vbar chemo
bar_chemo <- as.data.frame(ft_chemo)
names(bar_chemo)[1:3] <- c("Category","Frequency","Percent")
p5 <- ggplot(bar_chemo, aes(x=Category, y=Frequency)) +
  geom_col(fill="steelblue") + labs(title="Chemotherapy Treatment Distribution", x="Chemotherapy", y="Frequency") +
  theme_minimal() + theme(axis.text.x=element_text(angle=20, hjust=1))
ggsave(file.path(out, "sgplot4.png"), p5, width=8, height=6, dpi=150)
write.csv(bar_chemo, file.path(out, "sgplot4.csv"), row.names=FALSE, na="")

# 6: histogram repeat
p6 <- ggplot(age_plot_df, aes(x=Age)) +
  geom_histogram(bins=30, fill="lightblue", color="black") +
  labs(title="Age Distribution of Patients", x="AGE at Diagnosis", y="Frequency") + theme_minimal()
ggsave(file.path(out, "sgplot5.png"), p6, width=8, height=6, dpi=150)
write.csv(hist_df, file.path(out, "sgplot5.csv"), row.names=FALSE, na="")

# 7: vbar tstage repeat
p7 <- ggplot(bar_tstage, aes(x=Category, y=Frequency)) +
  geom_col(fill="steelblue") + labs(title="Tumor Stage Distribution", x="Tumor Stage", y="Frequency") +
  theme_minimal() + theme(axis.text.x=element_text(angle=20, hjust=1))
ggsave(file.path(out, "sgplot6.png"), p7, width=8, height=6, dpi=150)
write.csv(bar_tstage, file.path(out, "sgplot6.csv"), row.names=FALSE, na="")

# Uppercase aliases for SGPlot checks (Windows case-insensitive but Linux needs both)
for (f in c("sgplot.png","sgplot1.png","sgplot2.png","sgplot3.png","sgplot4.png","sgplot5.png","sgplot6.png")) {
  base <- tools::file_path_sans_ext(f)
  up <- paste0(toupper(substr(base,1,1)), substr(base,2,nchar(base)), ".png")
  try(file.copy(file.path(out, f), file.path(out, up), overwrite=TRUE), silent=TRUE)
  csvf <- sub("\\.png$",".csv",f)
  upcsv <- sub("\\.png$",".csv",up)
  # also SGPlot.csv variants
  try(file.copy(file.path(out, csvf), file.path(out, upcsv), overwrite=TRUE), silent=TRUE)
}
# SGPlot uppercase without number already handled; ensure SGPlot.png exists
# sasgraph alias
try(file.copy(file.path(out, "sasgraph.png"), file.path(out, "sasGraph.png"), overwrite=TRUE), silent=TRUE)
try(file.copy(file.path(out, "sasgraph.csv"), file.path(out, "sasGraph.csv"), overwrite=TRUE), silent=TRUE)

# Write ADaM datasets as CSV for traceability
try(write.csv(adam_adsl, file.path(out, "adam_adsl.csv"), row.names=FALSE, na=""), silent=TRUE)
try(write.csv(adam_adtte, file.path(out, "adam_adtte.csv"), row.names=FALSE, na=""), silent=TRUE)

cat("Done. OUT_DIR=", out, "\n")
```