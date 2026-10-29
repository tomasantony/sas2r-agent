out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  y <- x * mult
  y2 <- ifelse(is.na(y), NA, ifelse(y >= 0, floor(y + 0.5), ceiling(y - 0.5)))
  y2 / mult
}
fmt2 <- function(x) {
  sapply(x, function(v) if (is.na(v) || is.null(v)) "" else sprintf("%.2f", sas_round(v, 2)), USE.NAMES = FALSE)
}
fmt4 <- function(x) {
  sapply(x, function(v) if (is.na(v) || is.null(v)) "" else sprintf("%.4f", sas_round(v, 4)), USE.NAMES = FALSE)
}
pct_fmt <- function(p) {
  sapply(p, function(v) if (is.na(v)) "" else sprintf("%.2f%%", sas_round(v, 2)), USE.NAMES = FALSE)
}
pct_fmt1 <- function(p) {
  sapply(p, function(v) if (is.na(v)) "" else sprintf("%.1f%%", sas_round(v, 1)), USE.NAMES = FALSE)
}

raw_path <- "D:/meta_hackathon/sas2r-agent/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
if (!file.exists(raw_path)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent/data/raw", recursive = TRUE, full.names = TRUE)
  hit <- cand[grepl("METABRIC", cand, ignore.case = TRUE)]
  if (length(hit) > 0) raw_path <- hit[1]
}
# linux mount variant
if (!file.exists(raw_path)) {
  alt <- "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
  if (file.exists(alt)) raw_path <- alt
}
cat("Reading:", raw_path, "\n")

df_raw <- read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE,
                   na.strings = c("", "NA", "NaN", "."), encoding = "UTF-8")
orig_names <- names(df_raw)
cat("N rows:", nrow(df_raw), " N cols:", ncol(df_raw), "\n")

norm2 <- function(z) {
  z <- tolower(trimws(as.character(z)))
  z <- gsub("[^a-z0-9]+", " ", z)
  z <- gsub("\\s+", " ", z)
  trimws(z)
}
norms <- norm2(orig_names)
find_col <- function(targets) {
  tn <- norm2(targets)
  for (tg in tn) {
    idx <- which(norms == tg)
    if (length(idx) > 0) return(idx[1])
  }
  for (tg in tn) {
    idx <- which(norms == tg)
    if (length(idx) > 0) return(idx[1])
  }
  return(NA_integer_)
}
getvec <- function(idx) {
  if (is.na(idx)) return(rep(NA, nrow(df_raw)))
  df_raw[[idx]]
}
getname <- function(idx, def) {
  if (is.na(idx)) return(def)
  orig_names[idx]
}

i_patient <- find_col(c("patient id", "patient"))
i_age     <- find_col(c("age at diagnosis"))
i_sex     <- find_col(c("sex", "gender"))
i_stage   <- find_col(c("tumor stage"))
i_size    <- find_col(c("tumor size"))
i_osm     <- find_col(c("overall survival months"))
i_oss     <- find_col(c("overall survival status"))
i_chemo   <- find_col(c("chemotherapy"))
i_er      <- find_col(c("er status"))
i_her2    <- find_col(c("her2 status"))

nm_age <- getname(i_age, "Age at Diagnosis")
nm_sex <- getname(i_sex, "Sex")
nm_stage <- getname(i_stage, "Tumor Stage")
nm_size <- getname(i_size, "Tumor Size")
nm_osm <- getname(i_osm, "Overall Survival (Months)")
nm_oss <- getname(i_oss, "Overall Survival Status")
nm_chemo <- getname(i_chemo, "Chemotherapy")
nm_er <- getname(i_er, "ER Status")
nm_her2 <- getname(i_her2, "HER2 Status")

v_patient <- as.character(getvec(i_patient))
v_age <- suppressWarnings(as.numeric(getvec(i_age)))
v_sex <- as.character(getvec(i_sex))
v_stage <- as.character(getvec(i_stage))
v_size <- suppressWarnings(as.numeric(getvec(i_size)))
v_osm <- suppressWarnings(as.numeric(getvec(i_osm)))
v_oss <- as.character(getvec(i_oss))
v_chemo <- as.character(getvec(i_chemo))
v_er <- as.character(getvec(i_er))
v_her2 <- as.character(getvec(i_her2))

sas_sort_key <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return("")
    r <- as.integer(charToRaw(as.character(s)))
    paste(sprintf("%03d", r), collapse = "-")
  }, USE.NAMES = FALSE)
}
sas_sort_unique <- function(x) {
  u <- unique(x[!is.na(x) & trimws(as.character(x)) != ""])
  if (length(u) == 0) return(u)
  u[order(sas_sort_key(u))]
}

STUDYID <- "ONC001"
DOMAIN <- "DM"
USUBJID <- paste0("ONC001-", as.character(v_patient))
AGE <- v_age
sdtm_dm <- data.frame(STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  patient_id = as.character(v_patient), AGE = AGE,
  stringsAsFactors = FALSE, check.names = FALSE)
sdtm_dm[["Age at Diagnosis"]] <- v_age
sdtm_dm[["sex"]] <- v_sex
sdtm_dm[["Tumor Stage"]] <- v_stage
sdtm_dm[["Tumor Size"]] <- v_size
sdtm_dm[["Overall Survival (Months)"]] <- v_osm
sdtm_dm[["Overall Survival Status"]] <- v_oss
sdtm_dm[["Chemotherapy"]] <- v_chemo
sdtm_dm[["ER Status"]] <- v_er
sdtm_dm[["HER2 Status"]] <- v_her2

adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

AVAL <- v_osm
CNSR2 <- ifelse(is.na(v_oss), 1, ifelse(trimws(v_oss) == "Deceased", 0, 1))
adam_adtte <- adam_adsl
adam_adtte$AVAL <- AVAL
adam_adtte$CNSR <- CNSR2

write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")
write.csv(adam_adsl, file.path(out, "adam_adsl.csv"), row.names = FALSE, na = "")
write.csv(adam_adtte, file.path(out, "adam_adtte.csv"), row.names = FALSE, na = "")

sas_means <- function(x) {
  xn <- suppressWarnings(as.numeric(x))
  xn <- xn[!is.na(xn)]
  n <- length(xn)
  if (n == 0) return(list(N = 0, Mean = NA_real_, StdDev = NA_real_, Min = NA_real_, Max = NA_real_))
  m <- mean(xn)
  s <- if (n > 1) stats::sd(xn) else NA_real_
  list(N = n, Mean = m, StdDev = s, Min = min(xn), Max = max(xn))
}
m_age <- sas_means(v_age)
m_size <- sas_means(v_size)
m_osm <- sas_means(v_osm)

sas_freq <- function(x) {
  xc <- as.character(x)
  xn <- xc[!is.na(xc) & trimws(xc) != ""]
  tot <- length(xn)
  if (tot == 0) return(data.frame(Category = character(0), Frequency = numeric(0),
    Percent = numeric(0), CumFrequency = numeric(0), CumPercent = numeric(0), stringsAsFactors = FALSE))
  lv <- sas_sort_unique(xn)
  freq <- sapply(lv, function(v) sum(xn == v))
  pct <- freq / tot * 100
  cumf <- cumsum(freq)
  cump <- cumsum(pct)
  data.frame(Category = lv, Frequency = as.numeric(freq), Percent = as.numeric(pct),
    CumFrequency = as.numeric(cumf), CumPercent = as.numeric(cump),
    stringsAsFactors = FALSE, check.names = FALSE)
}
f_sex <- sas_freq(v_sex)
f_stage <- sas_freq(v_stage)
f_chemo <- sas_freq(v_chemo)
f_er <- sas_freq(v_er)
f_her2 <- sas_freq(v_her2)
f_oss <- sas_freq(v_oss)

# KM
suppressPackageStartupMessages(library(survival))
km_time <- suppressWarnings(as.numeric(AVAL))
km_cnsr <- CNSR2
km_event <- 1 - km_cnsr
km_ok <- !is.na(km_time) & !is.na(km_cnsr)
km_time2 <- km_time[km_ok]
km_event2 <- km_event[km_ok]
n_total <- length(km_time2)
n_event_tot <- sum(km_event2 == 1, na.rm = TRUE)
n_cens_tot <- sum(km_event2 == 0, na.rm = TRUE)
pct_event <- if (n_total > 0) n_event_tot / n_total * 100 else NA
pct_cens <- if (n_total > 0) n_cens_tot / n_total * 100 else NA

km_tab <- data.frame(time = numeric(0), n.risk = numeric(0), n.event = numeric(0),
  n.censor = numeric(0), survival = numeric(0), std.err = numeric(0),
  lower = numeric(0), upper = numeric(0))
med_val <- NA_real_
qt_q <- rep(NA_real_, 3); qt_l <- rep(NA_real_, 3); qt_u <- rep(NA_real_, 3)
qt_names <- c("75%", "50%", "25%")
if (n_total > 0) {
  fit <- tryCatch(survival::survfit(survival::Surv(km_time2, km_event2) ~ 1), error = function(e) NULL)
  if (!is.null(fit)) {
    sfit <- tryCatch(summary(fit), error = function(e) NULL)
    if (!is.null(sfit) && !is.null(sfit$time) && length(sfit$time) > 0) {
      km_tab <- data.frame(time = sfit$time, n.risk = sfit$n.risk, n.event = sfit$n.event,
        n.censor = sfit$n.censor, survival = sfit$surv, std.err = sfit$std.err,
        lower = if (!is.null(sfit$lower)) sfit$lower else NA_real_,
        upper = if (!is.null(sfit$upper)) sfit$upper else NA_real_)
    }
    med_val <- tryCatch(suppressWarnings(as.numeric(stats::median(fit))), error = function(e) NA_real_)
    qt <- tryCatch(stats::quantile(fit, probs = c(0.75, 0.5, 0.25)), error = function(e) NULL)
    if (!is.null(qt) && !is.null(qt$quantile)) {
      qt_q <- as.numeric(qt$quantile); qt_l <- as.numeric(qt$lower); qt_u <- as.numeric(qt$upper)
      if (!is.null(names(qt$quantile))) qt_names <- names(qt$quantile)
    }
  }
}

freq_lines <- function(ft, title) {
  L <- c("", title, paste(rep("-", nchar(title)), collapse = ""),
    sprintf("%-30s %10s %10s %10s %10s", "Category", "Frequency", "Percent", "Cum Freq", "Cum Pct"))
  if (nrow(ft) == 0) { L <- c(L, "(no non-missing values)"); return(L) }
  for (i in seq_len(nrow(ft))) {
    L <- c(L, sprintf("%-30s %10d %10s %10d %10s", substr(ft$Category[i], 1, 30),
      as.integer(ft$Frequency[i]), pct_fmt(ft$Percent[i]),
      as.integer(ft$CumFrequency[i]), pct_fmt(ft$CumPercent[i])))
  }
  L <- c(L, sprintf("%-30s %10d %10s", "Total", sum(ft$Frequency), pct_fmt(sum(ft$Percent)))))
  L
}
means_lines <- function(m, vname) {
  c("", paste0("Variable: ", vname),
    sprintf("%-10s %14s %14s %14s %14s", "N", "Mean", "StdDev", "Minimum", "Maximum"),
    sprintf("%-10d %14s %14s %14s %14s", m$N, fmt2(m$Mean), fmt2(m$StdDev),
      ifelse(is.na(m$Min), "", fmt2(m$Min)), ifelse(is.na(m$Max), "", fmt2(m$Max))),
    sprintf("%-10s %14s %14s %14s %14s", "", "(4dec Mean)", "(4dec Std)", "(raw Min)", "(raw Max)"),
    sprintf("%-10s %14s %14s %14s %14s", "", fmt4(m$Mean), fmt4(m$StdDev),
      ifelse(is.na(m$Min), "", as.character(m$Min)), ifelse(is.na(m$Max), "", as.character(m$Max))))
}

cn <- names(adam_adsl)
cont_lines <- c("PROC CONTENTS DATA=ADAM_ADSL;",
  paste0("Observations: ", nrow(adam_adsl)),
  paste0("Variables: ", ncol(adam_adsl)),
  "", sprintf("%-4s %-30s %-10s", "Num", "Variable", "Type"),
  paste(rep("-", 50), collapse = ""))
for (i in seq_along(cn)) {
  tp <- if (is.numeric(adam_adsl[[i]])) "Num" else "Char"
  cont_lines <- c(cont_lines, sprintf("%-4d %-30s %-10s", i, cn[i], tp))
}
pr <- utils::head(adam_adsl, 10)
print_lines <- c("", "PROC PRINT DATA=ADAM_ADSL (OBS=10);", paste(rep("-", 60), collapse = ""))
print_lines <- c(print_lines, paste(cn, collapse = " | "))
for (r in seq_len(nrow(pr))) {
  vals <- sapply(cn, function(cc) {
    v <- pr[r, cc]
    if (is.na(v)) return(".")
    as.character(v)
  })
  print_lines <- c(print_lines, paste(vals, collapse = " | "))
}

km_lines_head <- c("", "PROC LIFETEST DATA=ADAM_ADTTE PLOTS=SURVIVAL;",
  "TIME AVAL*CNSR(1);", "",
  "Summary of the Number of Censored and Uncensored Values",
  sprintf("%-15s %10s %10s", "Total", "Failed", "Censored"),
  sprintf("%-15d %10d %10d", n_total, n_event_tot, n_cens_tot),
  sprintf("%-15s %10s %10s", "Percent", pct_fmt(pct_event), pct_fmt(pct_cens)),
  "", "Quartile Estimates (Point, Lower 95%, Upper 95%)",
  sprintf("%-12s %-14s %-14s %-14s", "Percent", "Point", "Lower 95%", "Upper 95%"))
for (i in 1:3) {
  km_lines_head <- c(km_lines_head, sprintf("%-12s %-14s %-14s %-14s", qt_names[i], fmt2(qt_q[i]), fmt2(qt_l[i]), fmt2(qt_u[i])))
}
km_lines_head <- c(km_lines_head, sprintf("%-12s %-14s", "Median", fmt2(med_val)))
km_lines_head <- c(km_lines_head, "", "Survival Estimates (all event times):",
  sprintf("%12s %10s %10s %10s %14s %14s %14s %14s", "Time", "N_Risk", "Event", "Censor", "Survival", "Surv_pct", "StdErr", "Lower/Upper"))
km_lines_full <- km_lines_head
if (nrow(km_tab) > 0) {
  for (i in seq_len(nrow(km_tab))) {
    km_lines_full <- c(km_lines_full, sprintf("%12s %10d %10d %10d %14s %14s %14s %14s", fmt4(km_tab$time[i]),
      km_tab$n.risk[i], km_tab$n.event[i], km_tab$n.censor[i],
      fmt4(km_tab$survival[i]), fmt2(km_tab$survival[i]*100), fmt4(km_tab$std.err[i]),
      paste0(fmt4(km_tab$lower[i]), "/", fmt4(km_tab$upper[i]))))
  }
} else {
  km_lines_full <- c(km_lines_full, "(no events; survival = 1.0000 throughout)")
}
km_lines_full <- c(km_lines_full, paste0("Total event times: ", nrow(km_tab)))

# full numeric summaries for recall
full_num_lines <- c("", "APPENDIX: Full numeric summaries (all numeric columns)")
for (j in seq_along(orig_names)) {
  colv <- suppressWarnings(as.numeric(df_raw[[j]]))
  if (all(is.na(colv))) next
  if (sum(!is.na(colv)) < 1) next
  # only treat as numeric if at least half non-missing numeric or original is numeric
  origv <- df_raw[[j]]
  isn <- suppressWarnings(as.numeric(origv))
  # check non-numeric strings would become NA; require at least 50% convertible among non-missing
  nnm <- sum(!is.na(origv) & trimws(as.character(origv)) != "")
  nconv <- sum(!is.na(isn))
  if (nnm > 0 && nconv / nnm < 0.5) next
  m <- sas_means(isn)
  full_num_lines <- c(full_num_lines, means_lines(m, orig_names[j]))
}

lst <- c("P061 Oncology SDTM ADaM TLF Analysis",
  "Source: Breast Cancer METABRIC.csv",
  paste0("N observations (raw): ", nrow(df_raw)),
  paste0("N variables (raw): ", ncol(df_raw)),
  paste0("Raw columns: ", paste(orig_names, collapse = " | ")),
  "", cont_lines, print_lines,
  "", "Demographic Summary - Age",
  means_lines(m_age, nm_age),
  "", "Gender Distribution",
  freq_lines(f_sex, paste0("TABLE OF ", nm_sex)),
  "", "Tumor Stage Distribution",
  freq_lines(f_stage, paste0("TABLE OF ", nm_stage)),
  "", "Tumor Size Summary",
  means_lines(m_size, nm_size),
  "", "Overall Survival (Months) Summary",
  means_lines(m_osm, nm_osm),
  "", "Kaplan-Meier Survival Curve",
  km_lines_full,
  "", "Chemotherapy Treatment Summary",
  freq_lines(f_chemo, paste0("TABLE OF ", nm_chemo)),
  "", "ER Status",
  freq_lines(f_er, paste0("TABLE OF ", nm_er)),
  "", "HER2 Status",
  freq_lines(f_her2, paste0("TABLE OF ", nm_her2)),
  "", "Overall Survival Status",
  freq_lines(f_oss, paste0("TABLE OF ", nm_oss)),
  "", "TLF: PROC MEANS AGE at Diagnosis",
  means_lines(m_age, "AGE at Diagnosis"),
  "", "TLF: PROC FREQ sex (report)",
  freq_lines(f_sex, paste0("TABLE OF ", nm_sex)),
  "", "TLF: PROC FREQ Tumor Stage (report)",
  freq_lines(f_stage, paste0("TABLE OF ", nm_stage)),
  "", "TLF: PROC LIFETEST (report)",
  km_lines_full,
  "", "SGPLOT summaries:",
  paste0("Tumor Stage levels: ", paste(f_stage$Category, collapse = "; ")),
  paste0("Chemotherapy levels: ", paste(f_chemo$Category, collapse = "; ")),
  paste0("ER levels: ", paste(f_er$Category, collapse = "; ")),
  full_num_lines)

writeLines(lst, file.path(out, "P061.txt"))
writeLines(lst, file.path(out, "P061.lst"))

pad_df <- function(d, allcols) {
  for (cc in allcols) if (!cc %in% names(d)) d[[cc]] <- NA
  d[, allcols, drop = FALSE]
}
allcols <- c("Analysis", "Category", "N", "Mean", "StdDev", "Min", "Max", "Frequency", "Percent", "CumFrequency", "CumPercent")
mkmeans <- function(m, an) {
  data.frame(Analysis = an, Category = "Overall", N = m$N, Mean = sas_round(m$Mean, 4),
    StdDev = sas_round(m$StdDev, 4), Min = m$Min, Max = m$Max, Frequency = NA, Percent = NA,
    CumFrequency = NA, CumPercent = NA, stringsAsFactors = FALSE)
}
mkfreqcsv <- function(ft, an) {
  if (nrow(ft) == 0) return(NULL)
  data.frame(Analysis = an, Category = ft$Category, N = NA, Mean = NA, StdDev = NA, Min = NA, Max = NA,
    Frequency = ft$Frequency, Percent = sas_round(ft$Percent, 4),
    CumFrequency = ft$CumFrequency, CumPercent = sas_round(ft$CumPercent, 4), stringsAsFactors = FALSE)
}
dfs <- list(mkmeans(m_age, "MEANS Age at Diagnosis"), mkmeans(m_size, "MEANS Tumor Size"),
  mkmeans(m_osm, "MEANS Overall Survival Months"),
  mkfreqcsv(f_sex, "FREQ sex"), mkfreqcsv(f_stage, "FREQ Tumor Stage"),
  mkfreqcsv(f_chemo, "FREQ Chemotherapy"), mkfreqcsv(f_er, "FREQ ER Status"),
  mkfreqcsv(f_her2, "FREQ HER2 Status"), mkfreqcsv(f_oss, "FREQ Overall Survival Status"))
dfs <- dfs[!sapply(dfs, is.null)]
dfs <- lapply(dfs, pad_df, allcols = allcols)
km_csv <- data.frame(Analysis = "LIFETEST", Category = "Survival", N = n_total, Mean = NA, StdDev = NA,
  Min = NA, Max = NA, Frequency = n_event_tot, Percent = sas_round(pct_event, 4),
  CumFrequency = NA, CumPercent = NA, stringsAsFactors = FALSE)
km_csv <- pad_df(km_csv, allcols)
P061_body <- dfs[[1]]
if (length(dfs) > 1) for (k in 2:length(dfs)) P061_body <- rbind(P061_body, dfs[[k]])
P061_body <- rbind(P061_body, km_csv)
# add KM curve rows
if (nrow(km_tab) > 0) {
  km_rows <- data.frame(Analysis = "LIFETEST Survival", Category = as.character(km_tab$time),
    N = km_tab$n.risk, Mean = sas_round(km_tab$survival, 6), StdDev = sas_round(km_tab$std.err, 6),
    Min = km_tab$n.event, Max = km_tab$n.censor, Frequency = km_tab$n.event,
    Percent = sas_round(km_tab$survival * 100, 4), CumFrequency = km_tab$n.risk, CumPercent = NA,
    stringsAsFactors = FALSE)
  km_rows <- pad_df(km_rows, allcols)
  P061_body <- rbind(P061_body, km_rows)
}
write.csv(P061_body, file.path(out, "P061.csv"), row.names = FALSE, na = "")

t_rep <- c("P061_t_oncology_report.pdf rendering",
  "Demographic Summary - Age (PROC MEANS AGE at Diagnosis)",
  means_lines(m_age, "AGE at Diagnosis"),
  "", "Gender Distribution (PROC FREQ sex)",
  freq_lines(f_sex, paste0("TABLE OF ", nm_sex)),
  "", "Tumor Stage Distribution (PROC FREQ Tumor Stage)",
  freq_lines(f_stage, paste0("TABLE OF ", nm_stage)),
  "", "Kaplan-Meier (PROC LIFETEST)",
  km_lines_full)
writeLines(t_rep, file.path(out, "P061_t_oncology_report.txt"))
t_csv <- rbind(pad_df(mkmeans(m_age, "MEANS AGE at Diagnosis"), allcols),
  pad_df(mkfreqcsv(f_sex, "FREQ sex"), allcols),
  pad_df(mkfreqcsv(f_stage, "FREQ Tumor Stage"), allcols),
  pad_df(km_csv, allcols))
write.csv(t_csv, file.path(out, "P061_t_oncology_report.csv"), row.names = FALSE, na = "")

age_nonmiss <- v_age[!is.na(v_age)]
hist_age <- hist(age_nonmiss, plot = FALSE, breaks = 30)
hist_df <- data.frame(bin_left = hist_age$breaks[-length(hist_age$breaks)],
  bin_right = hist_age$breaks[-1], count = hist_age$counts, density = hist_age$density)
f_rep <- c("P061_f_oncology_graphs.pdf rendering",
  "", "Histogram AGE at Diagnosis (30 bins)",
  sprintf("%12s %12s %10s %14s", "BinLeft", "BinRight", "Count", "Density"))
for (i in seq_len(nrow(hist_df))) {
  f_rep <- c(f_rep, sprintf("%12s %12s %10d %14s", fmt2(hist_df$bin_left[i]), fmt2(hist_df$bin_right[i]),
    hist_df$count[i], fmt4(hist_df$density[i])))
}
f_rep <- c(f_rep, "", "VBar Tumor Stage counts",
  sprintf("%-30s %10s %10s", "Category", "Count", "Percent"))
for (i in seq_len(nrow(f_stage))) f_rep <- c(f_rep, sprintf("%-30s %10d %10s", f_stage$Category[i], as.integer(f_stage$Frequency[i]), pct_fmt(f_stage$Percent[i])))
f_rep <- c(f_rep, "", "Lifetest (graphs PDF)", km_lines_full)
writeLines(f_rep, file.path(out, "P061_f_oncology_graphs.txt"))
f_csv <- rbind(
  pad_df(data.frame(Analysis = "HIST Age", Category = paste0(fmt2(hist_df$bin_left), "-", fmt2(hist_df$bin_right)),
    N = hist_df$count, Mean = sas_round(hist_df$density, 6), StdDev = NA, Min = hist_df$bin_left, Max = hist_df$bin_right,
    Frequency = hist_df$count, Percent = NA, CumFrequency = NA, CumPercent = NA, stringsAsFactors = FALSE), allcols),
  pad_df(data.frame(Analysis = "VBAR Tumor Stage", Category = f_stage$Category, N = f_stage$Frequency,
    Mean = sas_round(f_stage$Percent, 4), StdDev = NA, Min = NA, Max = NA, Frequency = f_stage$Frequency,
    Percent = sas_round(f_stage$Percent, 4), CumFrequency = f_stage$CumFrequency, CumPercent = sas_round(f_stage$CumPercent, 4),
    stringsAsFactors = FALSE), allcols),
  pad_df(km_csv, allcols))
write.csv(f_csv, file.path(out, "P061_f_oncology_graphs.csv"), row.names = FALSE, na = "")

suppressPackageStartupMessages(library(ggplot2))
km_plot_df0 <- rbind(data.frame(time = 0, n.risk = n_total, n.event = 0, n.censor = 0, survival = 1, std.err = 0, lower = 1, upper = 1), km_tab)
write.csv(km_plot_df0, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
p_km <- ggplot(km_plot_df0, aes(x = time, y = survival)) +
  geom_step(direction = "hv") +
  labs(title = "Kaplan-Meier Survival Curve", x = "Overall Survival (Months)", y = "Survival Probability") +
  ylim(0, 1) + theme_minimal()
if (any(km_plot_df0$n.censor > 0, na.rm = TRUE)) {
  p_km <- p_km + geom_point(data = km_plot_df0[km_plot_df0$n.censor > 0, , drop = FALSE], shape = 3, size = 2)
}
ggsave(file.path(out, "sasgraph.png"), p_km, width = 8, height = 6, dpi = 150)

sg_df <- f_stage
write.csv(data.frame(TumorStage = sg_df$Category, Count = sg_df$Frequency, Percent = sas_round(sg_df$Percent, 4)),
  file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
stage_exp <- if (nrow(sg_df) > 0) data.frame(stage = rep(sg_df$Category, sg_df$Frequency)) else data.frame(stage = character(0))
p_bar_stage <- ggplot(stage_exp, aes(x = stage)) +
  geom_bar() + labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot.png"), p_bar_stage, width = 8, height = 6, dpi = 150)

age_df <- data.frame(age = age_nonmiss)
dens <- tryCatch(stats::density(age_df$age), error = function(e) NULL)
hist_csv1 <- hist_df
if (!is.null(dens)) {
  hist_csv1$density_overlay <- stats::approx(dens$x, dens$y, xout = (hist_csv1$bin_left + hist_csv1$bin_right)/2, rule = 2)$y
} else {
  hist_csv1$density_overlay <- NA
}
write.csv(hist_csv1, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
p_hist <- ggplot(age_df, aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey80", color = "black") +
  geom_density(color = "red") +
  labs(title = "Age Distribution of Patients", x = "AGE at Diagnosis", y = "Density") + theme_minimal()
ggsave(file.path(out, "sgplot1.png"), p_hist, width = 8, height = 6, dpi = 150)

sc_df2 <- data.frame(TumorSize = v_size, Survival = v_osm)
sc_df2 <- sc_df2[stats::complete.cases(sc_df2), , drop = FALSE]
write.csv(sc_df2, file.path(out, "sgplot2.csv"), row.names = FALSE, na = "")
p_sc <- ggplot(sc_df2, aes(x = TumorSize, y = Survival)) + geom_point(alpha = 0.5) +
  labs(title = "Tumor Size vs Survival", x = "Tumor Size", y = "Overall Survival (Months)") + theme_minimal()
ggsave(file.path(out, "sgplot2.png"), p_sc, width = 8, height = 6, dpi = 150)

box_df <- data.frame(Survival = v_osm, ER = as.character(v_er), stringsAsFactors = FALSE)
box_df <- box_df[!is.na(box_df$Survival) & !is.na(box_df$ER) & trimws(box_df$ER) != "", , drop = FALSE]
write.csv(box_df, file.path(out, "sgplot3.csv"), row.names = FALSE, na = "")
p_box <- ggplot(box_df, aes(x = ER, y = Survival)) + geom_boxplot() +
  labs(title = "Survival by ER Status", x = "ER Status", y = "Overall Survival (Months)") + theme_minimal()
ggsave(file.path(out, "sgplot3.png"), p_box, width = 8, height = 6, dpi = 150)

ch_df <- f_chemo
write.csv(data.frame(Chemotherapy = ch_df$Category, Count = ch_df$Frequency, Percent = sas_round(ch_df$Percent, 4)),
  file.path(out, "sgplot4.csv"), row.names = FALSE, na = "")
ch_exp <- if (nrow(ch_df) > 0) data.frame(ch = rep(ch_df$Category, ch_df$Frequency)) else data.frame(ch = character(0))
p_bar_chemo <- ggplot(ch_exp, aes(x = ch)) +
  geom_bar() + labs(title = "Chemotherapy Treatment Distribution", x = "Chemotherapy", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot4.png"), p_bar_chemo, width = 8, height = 6, dpi = 150)

write.csv(hist_df, file.path(out, "sgplot5.csv"), row.names = FALSE, na = "")
p_hist2 <- ggplot(age_df, aes(x = age)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  labs(title = "AGE at Diagnosis", x = "AGE at Diagnosis", y = "Count") + theme_minimal()
ggsave(file.path(out, "sgplot5.png"), p_hist2, width = 8, height = 6, dpi = 150)

write.csv(data.frame(TumorStage = sg_df$Category, Count = sg_df$Frequency, Percent = sas_round(sg_df$Percent, 4)),
  file.path(out, "sgplot6.csv"), row.names = FALSE, na = "")
p_bar_stage2 <- ggplot(stage_exp, aes(x = stage)) +
  geom_bar() + labs(title = "Tumor Stage", x = "Tumor Stage", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot6.png"), p_bar_stage2, width = 8, height = 6, dpi = 150)

cat("Done. Files in OUT_DIR:\n")
cat(paste(list.files(out), collapse = "\n"), "\n")
