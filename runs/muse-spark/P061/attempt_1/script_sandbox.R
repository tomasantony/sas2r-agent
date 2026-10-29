out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

sas_round <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  y <- x * mult
  y2 <- ifelse(is.na(y), NA,
    ifelse(y >= 0, floor(y + 0.5), ceiling(y - 0.5)))
  y2 / mult
}
fmt2 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f", sas_round(x, 2)))
}
fmt1 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.1f", sas_round(x, 1)))
}
pct_fmt <- function(p) sprintf("%.2f%%", sas_round(p, 2))

raw_path <- "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
if (!file.exists(raw_path)) {
  alt1 <- "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
  cand <- list.files("/bench/data/raw", recursive = TRUE, full.names = TRUE)
  cat("Raw path not found. Candidates:\n")
  cat(paste(head(cand, 50), collapse = "\n"), "\n")
  hit <- cand[grepl("METABRIC", cand, ignore.case = TRUE)]
  if (length(hit) > 0) raw_path <- hit[1]
}
cat("Reading:", raw_path, "\n")

df_raw <- read.csv(raw_path, check.names = FALSE, stringsAsFactors = FALSE,
                   na.strings = c("", "NA", "NaN", "."), encoding = "UTF-8")
orig_names <- names(df_raw)
cat("N rows:", nrow(df_raw), " N cols:", ncol(df_raw), "\n")
cat("Cols:", paste(orig_names, collapse = " | "), "\n")

norm_name <- function(z) {
  z2 <- gsub("[\r\n]+", " ", z)
  z2 <- gsub("[_\\.]+", " ", z2)
  z2 <- gsub("\\s+", " ", z2)
  tolower(trimws(z2))
}
norms <- norm_name(orig_names)

find_col <- function(targets) {
  for (tg in targets) {
    idx <- which(norms == tg)
    if (length(idx) > 0) return(idx[1])
  }
  for (tg in targets) {
    idx <- which(grepl(tg, norms, fixed = TRUE))
    if (length(idx) > 0) return(idx[1])
  }
  return(NA_integer_)
}

i_patient <- find_col(c("patient id", "patientid", "patient"))
i_age     <- find_col(c("age at diagnosis"))
i_sex     <- find_col(c("sex", "gender"))
i_stage   <- find_col(c("tumor stage"))
i_size    <- find_col(c("tumor size"))
i_osm     <- find_col(c("overall survival months", "overall survival month", "survival months", "survival month"))
i_oss     <- find_col(c("overall survival status", "survival status"))
i_chemo   <- find_col(c("chemotherapy"))
i_er      <- find_col(c("er status"))
i_her2    <- find_col(c("her2 status"))

getvec <- function(idx) {
  if (is.na(idx)) return(rep(NA, nrow(df_raw)))
  df_raw[[idx]]
}
getname <- function(idx, def) {
  if (is.na(idx)) return(def)
  orig_names[idx]
}
nm_patient <- getname(i_patient, "patient_id")
nm_age <- getname(i_age, "Age at Diagnosis")
nm_sex <- getname(i_sex, "sex")
nm_stage <- getname(i_stage, "Tumor Stage")
nm_size <- getname(i_size, "Tumor Size")
nm_osm <- getname(i_osm, "Overall Survival (Months)")
nm_oss <- getname(i_oss, "Overall Survival Status")
nm_chemo <- getname(i_chemo, "Chemotherapy")
nm_er <- getname(i_er, "ER Status")
nm_her2 <- getname(i_her2, "HER2 Status")

v_patient <- getvec(i_patient)
v_age <- suppressWarnings(as.numeric(getvec(i_age)))
v_sex <- as.character(getvec(i_sex))
v_stage <- as.character(getvec(i_stage))
v_size <- suppressWarnings(as.numeric(getvec(i_size)))
v_osm <- suppressWarnings(as.numeric(getvec(i_osm)))
v_oss <- as.character(getvec(i_oss))
v_chemo <- as.character(getvec(i_chemo))
v_er <- as.character(getvec(i_er))
v_her2 <- as.character(getvec(i_her2))

# SAS-like ASCII sort (uppercase before lowercase)
sas_sort_key <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return("")
    r <- as.integer(charToRaw(as.character(s)))
    paste(sprintf("%03d", r), collapse = "-")
  }, USE.NAMES = FALSE)
}
sas_sort_unique <- function(x) {
  u <- unique(x[!is.na(x) & x != ""])
  # keep NA handling separate; SAS excludes missing by default
  if (length(u) == 0) return(u)
  u[order(sas_sort_key(u))]
}

# SDTM DM
STUDYID <- "ONC001"
DOMAIN <- "DM"
USUBJID <- paste0("ONC001-", as.character(v_patient))
AGE <- v_age
sdtm_dm <- data.frame(STUDYID = STUDYID, DOMAIN = DOMAIN, USUBJID = USUBJID,
  patient_id = as.character(v_patient), AGE = AGE,
  stringsAsFactors = FALSE, check.names = FALSE)
# keep original vars for traceability
sdtm_dm[["Age at Diagnosis"]] <- v_age
sdtm_dm[["sex"]] <- v_sex
sdtm_dm[["Tumor Stage"]] <- v_stage
sdtm_dm[["Tumor Size"]] <- v_size
sdtm_dm[["Overall Survival (Months)"]] <- v_osm
sdtm_dm[["Overall Survival Status"]] <- v_oss
sdtm_dm[["Chemotherapy"]] <- v_chemo
sdtm_dm[["ER Status"]] <- v_er
sdtm_dm[["HER2 Status"]] <- v_her2

# ADaM ADSL
adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

# ADaM ADTTE
AVAL <- v_osm
CNSR <- ifelse(is.na(v_oss), NA, ifelse(v_oss == "Deceased", 0, 1))
# SAS: if Deceased then 0 else 1 (includes missing -> 1 if not Deceased? keep SAS logic: else 1)
CNSR2 <- ifelse(is.na(v_oss), 1, ifelse(v_oss == "Deceased", 0, 1))
adam_adtte <- adam_adsl
adam_adtte$AVAL <- AVAL
adam_adtte$CNSR <- CNSR2

write.csv(sdtm_dm, file.path(out, "sdtm_dm.csv"), row.names = FALSE, na = "")
write.csv(adam_adsl, file.path(out, "adam_adsl.csv"), row.names = FALSE, na = "")
write.csv(adam_adtte, file.path(out, "adam_adtte.csv"), row.names = FALSE, na = "")

sas_means <- function(x) {
  xn <- x[!is.na(x)]
  n <- length(xn)
  if (n == 0) return(list(N = 0, Mean = NA, StdDev = NA, Min = NA, Max = NA))
  m <- mean(xn)
  s <- if (n > 1) stats::sd(xn) else NA
  list(N = n, Mean = m, StdDev = s, Min = min(xn), Max = max(xn))
}
m_age <- sas_means(v_age)
m_size <- sas_means(v_size)
m_osm <- sas_means(v_osm)

sas_freq <- function(x) {
  xn <- x[!is.na(x) & trimws(as.character(x)) != ""]
  tot <- length(xn)
  if (tot == 0) return(data.frame(Category = character(0), Frequency = numeric(0),
    Percent = numeric(0), CumFrequency = numeric(0), CumPercent = numeric(0), stringsAsFactors = FALSE))
  lv <- sas_sort_unique(as.character(xn))
  freq <- sapply(lv, function(v) sum(as.character(xn) == v))
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

# Lifetest KM
library(survival)
km_time <- AVAL
km_cnsr <- CNSR2
km_event <- 1 - km_cnsr
km_ok <- !is.na(km_time) & !is.na(km_cnsr)
km_time2 <- km_time[km_ok]
km_event2 <- km_event[km_ok]
fit <- survival::survfit(survival::Surv(km_time2, km_event2) ~ 1)
sfit <- summary(fit)
km_tab <- data.frame(time = sfit$time, n.risk = sfit$n.risk, n.event = sfit$n.event,
  n.censor = sfit$n.censor, survival = sfit$surv, std.err = sfit$std.err,
  lower = if (!is.null(sfit$lower)) sfit$lower else NA,
  upper = if (!is.null(sfit$upper)) sfit$upper else NA)
med <- tryCatch(stats::median(fit), error = function(e) NA)
# quartiles via quantile
qt <- tryCatch(stats::quantile(fit, probs = c(0.75, 0.5, 0.25)), error = function(e) NULL)
n_total <- length(km_time2)
n_event_tot <- sum(km_event2 == 1, na.rm = TRUE)
n_cens_tot <- sum(km_event2 == 0, na.rm = TRUE)
pct_event <- n_event_tot / n_total * 100
pct_cens <- n_cens_tot / n_total * 100

# ---- text rendering helpers ----
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
    sprintf("%-10s %12s %12s %12s %12s", "N", "Mean", "StdDev", "Minimum", "Maximum"),
    sprintf("%-10d %12s %12s %12s %12s", m$N, fmt2(m$Mean), fmt2(m$StdDev),
      ifelse(is.na(m$Min), "", fmt2(m$Min)), ifelse(is.na(m$Max), "", fmt2(m$Max))))
}

# CONTENTS lines
cont_lines <- c("PROC CONTENTS DATA=ADAM_ADSL;",
  paste0("Observations: ", nrow(adam_adsl)),
  paste0("Variables: ", ncol(adam_adsl)),
  "", sprintf("%-4s %-30s %-10s", "Num", "Variable", "Type"),
  paste(rep("-", 50), collapse = ""))
cn <- names(adam_adsl)
for (i in seq_along(cn)) {
  tp <- if (is.numeric(adam_adsl[[i]])) "Num" else "Char"
  cont_lines <- c(cont_lines, sprintf("%-4d %-30s %-10s", i, cn[i], tp))
}

# PRINT obs=10
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

km_lines <- c("", "PROC LIFETEST DATA=ADAM_ADTTE PLOTS=SURVIVAL;",
  "TIME AVAL*CNSR(1);", "",
  "Summary of the Number of Censored and Uncensored Values",
  sprintf("%-15s %10s %10s", "Total", "Failed", "Censored"),
  sprintf("%-15d %10d %10d", n_total, n_event_tot, n_cens_tot),
  sprintf("%-15s %10s %10s", "Percent", pct_fmt(pct_event), pct_fmt(pct_cens)),
  "", "Quartile Estimates",
  sprintf("%-12s %-12s %-12s %-12s", "Percent", "Point", "Lower 95%", "Upper 95%"))
if (!is.null(qt) && !is.null(qt$quantile)) {
  # qt$quantile named
  qn <- names(qt$quantile)
  for (i in seq_along(qt$quantile)) {
    km_lines <- c(km_lines, sprintf("%-12s %-12s %-12s %-12s", qn[i],
      fmt2(qt$quantile[i]), fmt2(qt$lower[i]), fmt2(qt$upper[i])))
  }
} else {
  km_lines <- c(km_lines, sprintf("%-12s %-12s", "Median", fmt2(as.numeric(med))))
}
km_lines <- c(km_lines, "", "Survival Estimates (first 20 rows):",
  sprintf("%8s %8s %8s %10s %10s", "Time", "N_Risk", "Event", "Survival", "StdErr"))
nr <- min(20, nrow(km_tab))
for (i in seq_len(nr)) {
  km_lines <- c(km_lines, sprintf("%8s %8d %8d %10s %10s", fmt2(km_tab$time[i]),
    km_tab$n.risk[i], km_tab$n.event[i], fmt2(km_tab$survival[i]*100), fmt2(km_tab$std.err[i])))
}
km_lines <- c(km_lines, paste0("Total event times: ", nrow(km_tab)))

lst <- c("P061 Oncology SDTM ADaM TLF Analysis",
  "Source: Breast Cancer METABRIC.csv",
  paste0("N observations (raw): ", nrow(df_raw)),
  "", cont_lines, print_lines,
  "", "Demographic Summary - Age",
  means_lines(m_age, nm_age),
  "", "Gender Distribution",
  freq_lines(f_sex, "TABLE OF sex"),
  "", "Tumor Stage Distribution",
  freq_lines(f_stage, "TABLE OF Tumor Stage"),
  "", "Tumor Size Summary",
  means_lines(m_size, nm_size),
  "", "Overall Survival (Months) Summary",
  means_lines(m_osm, nm_osm),
  "", "Kaplan-Meier Survival Curve",
  km_lines,
  "", "Chemotherapy Treatment Summary",
  freq_lines(f_chemo, "TABLE OF Chemotherapy"),
  "", "ER Status",
  freq_lines(f_er, "TABLE OF ER Status"),
  "", "HER2 Status",
  freq_lines(f_her2, "TABLE OF HER2 Status"),
  "", "Overall Survival Status",
  freq_lines(f_oss, "TABLE OF Overall Survival Status"),
  "", "TLF: PROC MEANS AGE at Diagnosis",
  means_lines(m_age, "AGE at Diagnosis"),
  "", "TLF: PROC FREQ sex (report)",
  freq_lines(f_sex, "TABLE OF sex"),
  "", "TLF: PROC FREQ Tumor Stage (report)",
  freq_lines(f_stage, "TABLE OF Tumor Stage"),
  "", "TLF: PROC LIFETEST (report)",
  km_lines,
  "", "SGPLOT summaries:",
  paste0("Tumor Stage levels: ", paste(f_stage$Category, collapse = "; ")),
  paste0("Chemotherapy levels: ", paste(f_chemo$Category, collapse = "; ")),
  paste0("ER levels: ", paste(f_er$Category, collapse = "; ")))

writeLines(lst, file.path(out, "P061.txt"))
writeLines(lst, file.path(out, "P061.lst"))

# CSV of table body for P061
csv_rows <- list()
csv_rows[[1]] <- data.frame(Analysis = "MEANS Age at Diagnosis", Category = "Overall",
  N = m_age$N, Mean = sas_round(m_age$Mean, 2), StdDev = sas_round(m_age$StdDev, 2),
  Min = m_age$Min, Max = m_age$Max, stringsAsFactors = FALSE)
csv_rows[[2]] <- data.frame(Analysis = "MEANS Tumor Size", Category = "Overall",
  N = m_size$N, Mean = sas_round(m_size$Mean, 2), StdDev = sas_round(m_size$StdDev, 2),
  Min = m_size$Min, Max = m_size$Max, stringsAsFactors = FALSE)
csv_rows[[3]] <- data.frame(Analysis = "MEANS Overall Survival Months", Category = "Overall",
  N = m_osm$N, Mean = sas_round(m_osm$Mean, 2), StdDev = sas_round(m_osm$StdDev, 2),
  Min = m_osm$Min, Max = m_osm$Max, stringsAsFactors = FALSE)
mkfreqcsv <- function(ft, an) {
  if (nrow(ft) == 0) return(NULL)
  data.frame(Analysis = an, Category = ft$Category, N = NA, Mean = NA, StdDev = NA, Min = NA, Max = NA,
    Frequency = ft$Frequency, Percent = sas_round(ft$Percent, 2),
    CumFrequency = ft$CumFrequency, CumPercent = sas_round(ft$CumPercent, 2), stringsAsFactors = FALSE)
}
csv_rows[[4]] <- mkfreqcsv(f_sex, "FREQ sex")
csv_rows[[5]] <- mkfreqcsv(f_stage, "FREQ Tumor Stage")
csv_rows[[6]] <- mkfreqcsv(f_chemo, "FREQ Chemotherapy")
csv_rows[[7]] <- mkfreqcsv(f_er, "FREQ ER Status")
csv_rows[[8]] <- mkfreqcsv(f_her2, "FREQ HER2 Status")
csv_rows[[9]] <- mkfreqcsv(f_oss, "FREQ Overall Survival Status")
km_csv <- data.frame(Analysis = "LIFETEST", Category = "Survival",
  N = n_total, Mean = NA, StdDev = NA, Min = NA, Max = NA,
  Frequency = n_event_tot, Percent = sas_round(pct_event, 2),
  CumFrequency = NA, CumPercent = NA, stringsAsFactors = FALSE)
allcols <- c("Analysis", "Category", "N", "Mean", "StdDev", "Min", "Max", "Frequency", "Percent", "CumFrequency", "CumPercent")
pad <- function(d) {
  if (is.null(d)) return(NULL)
  for (cc in allcols) if (!cc %in% names(d)) d[[cc]] <- NA
  d[, allcols]
}
P061_body <- do.call(plyr_rbind <- function(...) NULL, list())
# manual rbind allowing missing cols
dfs <- lapply(csv_rows, pad)
dfs <- dfs[!sapply(dfs, is.null)]
P061_body <- dfs[[1]]
if (length(dfs) > 1) for (k in 2:length(dfs)) P061_body <- merge_rows(P061_body, dfs[[k]])
merge_rows <- function(a, b) {
  # defined after use? redefine properly via rbind with union
  suppressWarnings(dplyr::bind_rows(a, b))
}
P061_body <- dfs[[1]]
if (length(dfs) > 1) for (k in 2:length(dfs)) P061_body <- suppressWarnings(dplyr::bind_rows(P061_body, dfs[[k]]))
P061_body <- suppressWarnings(dplyr::bind_rows(P061_body, pad(km_csv)))
write.csv(P061_body, file.path(out, "P061.csv"), row.names = FALSE, na = "")

# t_oncology_report
t_rep <- c("P061_t_oncology_report.pdf rendering",
  "Demographic Summary - Age (PROC MEANS AGE at Diagnosis)",
  means_lines(m_age, "AGE at Diagnosis"),
  "", "Gender Distribution (PROC FREQ sex)",
  freq_lines(f_sex, "TABLE OF sex"),
  "", "Tumor Stage Distribution (PROC FREQ Tumor Stage)",
  freq_lines(f_stage, "TABLE OF Tumor Stage"),
  "", "Kaplan-Meier (PROC LIFETEST)",
  km_lines)
writeLines(t_rep, file.path(out, "P061_t_oncology_report.txt"))
t_csv <- suppressWarnings(dplyr::bind_rows(pad(csv_rows[[1]]), pad(csv_rows[[4]]), pad(csv_rows[[5]]), pad(km_csv)))
write.csv(t_csv, file.path(out, "P061_t_oncology_report.csv"), row.names = FALSE, na = "")

# f_oncology_graphs text: histogram bins, bar counts, KM
hist_age <- hist(v_age[!is.na(v_age)], plot = FALSE, breaks = 30)
hist_df <- data.frame(bin_left = hist_age$breaks[-length(hist_age$breaks)],
  bin_right = hist_age$breaks[-1], count = hist_age$counts,
  density = hist_age$density)
f_rep <- c("P061_f_oncology_graphs.pdf rendering",
  "", "Histogram AGE at Diagnosis (30 bins)",
  sprintf("%12s %12s %10s %12s", "BinLeft", "BinRight", "Count", "Density"))
for (i in seq_len(nrow(hist_df))) {
  f_rep <- c(f_rep, sprintf("%12s %12s %10d %12s", fmt2(hist_df$bin_left[i]), fmt2(hist_df$bin_right[i]),
    hist_df$count[i], fmt2(hist_df$density[i])))
}
f_rep <- c(f_rep, "", "VBar Tumor Stage counts",
  sprintf("%-30s %10s", "Category", "Count"))
for (i in seq_len(nrow(f_stage))) f_rep <- c(f_rep, sprintf("%-30s %10d", f_stage$Category[i], as.integer(f_stage$Frequency[i])))
f_rep <- c(f_rep, "", "Lifetest (graphs PDF)", km_lines)
writeLines(f_rep, file.path(out, "P061_f_oncology_graphs.txt"))
f_csv <- suppressWarnings(dplyr::bind_rows(
  data.frame(Analysis = "HIST Age", Category = paste0(fmt2(hist_df$bin_left), "-", fmt2(hist_df$bin_right)),
    N = hist_df$count, Mean = hist_df$density, stringsAsFactors = FALSE),
  data.frame(Analysis = "VBAR Tumor Stage", Category = f_stage$Category, N = f_stage$Frequency, Mean = sas_round(f_stage$Percent, 2), stringsAsFactors = FALSE),
  data.frame(Analysis = "LIFETEST", Category = "Survival", N = n_total, Mean = n_event_tot, stringsAsFactors = FALSE)
))
write.csv(f_csv, file.path(out, "P061_f_oncology_graphs.csv"), row.names = FALSE, na = "")

# ---- Figures ----
library(ggplot2)
# KM data for plot
km_plot_df <- km_tab
# add time 0
km_plot_df0 <- rbind(data.frame(time = 0, n.risk = n_total, n.event = 0, n.censor = 0, survival = 1, std.err = 0, lower = 1, upper = 1), km_plot_df)
write.csv(km_plot_df0, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
p_km <- ggplot(km_plot_df0, aes(x = time, y = survival)) +
  geom_step(direction = "hv") +
  geom_point(data = km_plot_df0[km_plot_df0$n.censor > 0, , drop = FALSE], shape = 3, size = 2) +
  labs(title = "Kaplan-Meier Survival Curve", x = "Overall Survival (Months)", y = "Survival Probability") +
  ylim(0, 1) + theme_minimal()
ggsave(file.path(out, "sasgraph.png"), p_km, width = 8, height = 6, dpi = 150)

# sgplot: vbar Tumor Stage
sg_df <- f_stage
write.csv(data.frame(TumorStage = sg_df$Category, Count = sg_df$Frequency, Percent = sas_round(sg_df$Percent, 2)),
  file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
p_bar_stage <- ggplot(data.frame(stage = rep(sg_df$Category, sg_df$Frequency)), aes(x = stage)) +
  geom_bar() + labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot.png"), p_bar_stage, width = 8, height = 6, dpi = 150)

# sgplot1: histogram Age + density
age_df <- data.frame(age = v_age[!is.na(v_age)])
dens <- density(age_df$age)
hist_csv1 <- hist_df
hist_csv1$density_overlay <- stats::approx(dens$x, dens$y, xout = (hist_csv1$bin_left + hist_csv1$bin_right)/2)$y
write.csv(hist_csv1, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
p_hist <- ggplot(age_df, aes(x = age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey80", color = "black") +
  geom_density(color = "red") +
  labs(title = "Age Distribution of Patients", x = "AGE at Diagnosis", y = "Density") + theme_minimal()
ggsave(file.path(out, "sgplot1.png"), p_hist, width = 8, height = 6, dpi = 150)

# sgplot2: scatter Tumor Size vs Survival
sc_df <- data.frame(TumorSize = v_size, Survival = v_osm)
sc_df2 <- sc_df[stats::complete.cases(sc_df), ]
write.csv(sc_df2, file.path(out, "sgplot2.csv"), row.names = FALSE, na = "")
p_sc <- ggplot(sc_df2, aes(x = TumorSize, y = Survival)) + geom_point(alpha = 0.5) +
  labs(title = "Tumor Size vs Survival", x = "Tumor Size", y = "Overall Survival (Months)") + theme_minimal()
ggsave(file.path(out, "sgplot2.png"), p_sc, width = 8, height = 6, dpi = 150)

# sgplot3: vbox Survival by ER Status
box_df <- data.frame(Survival = v_osm, ER = as.character(v_er))
box_df <- box_df[!is.na(box_df$Survival) & !is.na(box_df$ER) & box_df$ER != "", ]
# box stats csv
bst <- boxplot(Survival ~ ER, data = box_df, plot = FALSE)
box_csv <- data.frame(ER = colnames(bst$stats),
  ymin = bst$stats[1, ], lower = bst$stats[2, ], median = bst$stats[3, ], upper = bst$stats[4, ], ymax = bst$stats[5, ], n = bst$n)
write.csv(box_csv, file.path(out, "sgplot3.csv"), row.names = FALSE, na = "")
# also write raw for fidelity? keep stats
p_box <- ggplot(box_df, aes(x = ER, y = Survival)) + geom_boxplot() +
  labs(title = "Survival by ER Status", x = "ER Status", y = "Overall Survival (Months)") + theme_minimal()
ggsave(file.path(out, "sgplot3.png"), p_box, width = 8, height = 6, dpi = 150)

# sgplot4: vbar Chemotherapy
ch_df <- f_chemo
write.csv(data.frame(Chemotherapy = ch_df$Category, Count = ch_df$Frequency, Percent = sas_round(ch_df$Percent, 2)),
  file.path(out, "sgplot4.csv"), row.names = FALSE, na = "")
p_bar_chemo <- ggplot(data.frame(ch = rep(ch_df$Category, ch_df$Frequency)), aes(x = ch)) +
  geom_bar() + labs(title = "Chemotherapy Treatment Distribution", x = "Chemotherapy", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot4.png"), p_bar_chemo, width = 8, height = 6, dpi = 150)

# sgplot5: histogram for PDF (no density, no title in SAS but add title for clarity)
write.csv(hist_df, file.path(out, "sgplot5.csv"), row.names = FALSE, na = "")
p_hist2 <- ggplot(age_df, aes(x = age)) +
  geom_histogram(bins = 30, fill = "grey80", color = "black") +
  labs(title = "AGE at Diagnosis", x = "AGE at Diagnosis", y = "Count") + theme_minimal()
ggsave(file.path(out, "sgplot5.png"), p_hist2, width = 8, height = 6, dpi = 150)

# sgplot6: vbar Tumor Stage for PDF
write.csv(data.frame(TumorStage = sg_df$Category, Count = sg_df$Frequency, Percent = sas_round(sg_df$Percent, 2)),
  file.path(out, "sgplot6.csv"), row.names = FALSE, na = "")
p_bar_stage2 <- ggplot(data.frame(stage = rep(sg_df$Category, sg_df$Frequency)), aes(x = stage)) +
  geom_bar() + labs(title = "Tumor Stage", x = "Tumor Stage", y = "Count") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot6.png"), p_bar_stage2, width = 8, height = 6, dpi = 150)

cat("Done. Files in OUT_DIR:\n")
cat(paste(list.files(out), collapse = "\n"), "\n")
