## P061 Oncology SDTM/ADaM TLF Analysis - R translation
## Runs non-interactively with Rscript on Windows

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(jsonlite)
  library(data.table)
  library(ggplot2)
  library(survival)
  library(survminer)
  library(broom)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(gt)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

## ---------- helpers ----------
sas_round <- function(x, digits = 0) {
  y <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  xx <- x[ok]
  y[ok] <- sign(xx) * floor(abs(xx) * 10^digits + 0.5) / 10^digits
  ## preserve -0 as 0
  y[y == 0] <- 0
  y
}
fmt4 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.4f", sas_round(x, 4)))
}
fmt3 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.3f", sas_round(x, 3)))
}
fmt2 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f", sas_round(x, 2)))
}
fmt_int <- function(x) {
  ifelse(is.na(x), "", formatC(x, format = "d"))
}

norm_key <- function(x) tolower(gsub("[^a-z0-9]", "", x))
find_col <- function(df, target) {
  tn <- norm_key(target)
  cn <- norm_key(names(df))
  idx <- which(cn == tn)
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}

## ---------- locate raw ----------
raw_path <- "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
if (!file.exists(raw_path)) {
  cand2 <- file.path(Sys.getenv("rawdir"), "sumit-oncology", "Breast Cancer METABRIC.csv")
  if (file.exists(cand2)) raw_path <- cand2
}
if (!file.exists(raw_path)) {
  found <- list.files(getwd(), pattern = "Breast Cancer METABRIC\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(found) > 0) raw_path <- found[1]
}
if (!file.exists(raw_path)) {
  found2 <- list.files("/bench", pattern = "Breast Cancer METABRIC\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(found2) > 0) raw_path <- found2[1]
}
message("Reading: ", raw_path)
stopifnot(file.exists(raw_path))

raw_metabric <- suppressMessages(readr::read_csv(raw_path, show_col_types = FALSE, name_repair = "minimal", guess_max = 10000))
## keep original names exactly
message(paste0("raw rows=", nrow(raw_metabric), " cols=", ncol(raw_metabric)))

## resolve columns (case/space insensitive)
cn_patient  <- find_col(raw_metabric, "patient_id")
cn_age_diag <- find_col(raw_metabric, "age_at_diagnosis")
if (is.na(cn_age_diag)) cn_age_diag <- find_col(raw_metabric, "Age at Diagnosis")
cn_sex      <- find_col(raw_metabric, "sex")
cn_stage    <- find_col(raw_metabric, "Tumor Stage")
cn_size     <- find_col(raw_metabric, "Tumor Size")
cn_os       <- find_col(raw_metabric, "Overall Survival (Months)")
cn_osstat   <- find_col(raw_metabric, "Overall Survival Status")
cn_chemo    <- find_col(raw_metabric, "Chemotherapy")
cn_er       <- find_col(raw_metabric, "ER Status")
cn_her2     <- find_col(raw_metabric, "HER2 Status")

## fallback: if sex not found try Gender
if (is.na(cn_sex)) cn_sex <- find_col(raw_metabric, "Gender")
stopifnot(!is.na(cn_age_diag), !is.na(cn_os), !is.na(cn_osstat))
stopifnot(!is.na(cn_sex), !is.na(cn_stage), !is.na(cn_size), !is.na(cn_chemo), !is.na(cn_er), !is.na(cn_her2), !is.na(cn_patient))

## coerce numerics
num_age  <- suppressWarnings(as.numeric(raw_metabric[[cn_age_diag]]))
num_size <- suppressWarnings(as.numeric(raw_metabric[[cn_size]]))
num_os   <- suppressWarnings(as.numeric(raw_metabric[[cn_os]]))

## ---------- SDTM DM ----------
sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
sdtm_dm$USUBJID <- paste0("ONC001-", as.character(sdtm_dm[[cn_patient]]))
sdtm_dm$AGE <- num_age

## ---------- ADaM ADSL ----------
adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"

## ---------- ADaM ADTTE ----------
adam_adtte <- adam_adsl
adam_adtte$AVAL <- num_os
osstat <- as.character(adam_adsl[[cn_osstat]])
adam_adtte$CNSR <- ifelse(is.na(osstat), 1L, ifelse(osstat == "Deceased", 0L, 1L))
stopifnot("AVAL" %in% names(adam_adtte), "CNSR" %in% names(adam_adtte))

## vectors for analysis
vec_age   <- num_age
vec_size  <- num_size
vec_os    <- num_os
vec_sex   <- as.character(raw_metabric[[cn_sex]])
vec_stage <- as.character(raw_metabric[[cn_stage]])
vec_chemo <- as.character(raw_metabric[[cn_chemo]])
vec_er    <- as.character(raw_metabric[[cn_er]])
vec_her2  <- as.character(raw_metabric[[cn_her2]])
vec_osstat<- osstat

## ---------- PROC MEANS helper ----------
proc_means <- function(x) {
  xx <- x[!is.na(x)]
  n <- length(xx)
  if (n == 0) return(list(N = 0, Mean = NA_real_, StdDev = NA_real_, Min = NA_real_, Max = NA_real_))
  m <- mean(xx)
  s <- if (n > 1) sd(xx) else NA_real_
  list(N = n, Mean = m, StdDev = s, Min = min(xx), Max = max(xx))
}
means_age  <- proc_means(vec_age)
means_size <- proc_means(vec_size)

## ---------- PROC FREQ one-way helper ----------
freq_oneway <- function(vec) {
  total <- sum(!is.na(vec))
  nmiss <- sum(is.na(vec))
  v <- vec[!is.na(vec)]
  ## SAS character ordering: uppercase before lowercase -> C locale byte order
  levs_sorted <- stringr::str_sort(unique(v), locale = "C")
  freq <- sapply(levs_sorted, function(l) sum(v == l))
  tbl <- data.frame(Level = levs_sorted, Frequency = as.integer(freq), stringsAsFactors = FALSE)
  tbl$Percent <- tbl$Frequency / total * 100
  tbl$CumulativeFrequency <- cumsum(tbl$Frequency)
  tbl$CumulativePercent <- cumsum(tbl$Percent)
  list(table = tbl, N = total, NMiss = nmiss)
}
f_sex   <- freq_oneway(vec_sex)
f_stage <- freq_oneway(vec_stage)
f_chemo <- freq_oneway(vec_chemo)
f_er    <- freq_oneway(vec_er)
f_her2  <- freq_oneway(vec_her2)

## ---------- PROC LIFETEST KM ----------
lifetest_compute <- function(time, cens) {
  ok <- !is.na(time) & !is.na(cens)
  time <- time[ok]; cens <- cens[ok]
  N <- length(time)
  nEvent <- sum(cens == 0)
  nCens <- sum(cens == 1)
  pctCens <- if (N > 0) nCens / N * 100 else NA_real_
  uniq <- sort(unique(time))
  surv <- 1
  green <- 0
  cumEvents <- 0
  rows <- vector("list", length(uniq))
  for (i in seq_along(uniq)) {
    t <- uniq[i]
    n_risk <- sum(time >= t)
    d <- sum(time == t & cens == 0)
    cc <- sum(time == t & cens == 1)
    if (d > 0) {
      term <- if (n_risk == d) 0 else d / (n_risk * (n_risk - d))
      green <- green + term
      surv <- surv * (1 - d / n_risk)
      se <- if (surv == 0) 0 else surv * sqrt(green)
      if (is.na(se) || !is.finite(se)) se <- 0
    } else {
      se <- if (cumEvents == 0) 0 else surv * sqrt(green)
      if (is.na(se) || !is.finite(se)) se <- 0
    }
    cumEvents <- cumEvents + d
    n_left <- n_risk - d - cc
    rows[[i]] <- data.frame(Time = t, Survival = surv, Failure = 1 - surv,
                            SE = se, Failed = cumEvents, Left = n_left,
                            d = d, c = cc, n_risk = n_risk, stringsAsFactors = FALSE)
  }
  km <- do.call(rbind, rows)
  km$Censored <- km$d == 0
  ## quartiles point estimates
  get_q <- function(p_surv) {
    idx <- which(km$Survival <= p_surv)
    if (length(idx) == 0) return(NA_real_)
    km$Time[min(idx)]
  }
  q75pt <- get_q(0.75); q50pt <- get_q(0.5); q25pt <- get_q(0.25)
  ## CIs via survival log-log
  q25_lo <- q25_hi <- q50_lo <- q50_hi <- q75_lo <- q75_hi <- NA_real_
  q25e <- q50e <- q75e <- NA_real_
  try({
    fit <- survival::survfit(survival::Surv(time, 1 - cens) ~ 1, conf.type = "log-log")
    qq <- survival::quantile(fit, probs = c(0.25, 0.5, 0.75), conf.int = TRUE)
    ## qq is list with quantile/lower/upper named by prob
    if (!is.null(qq$quantile)) {
      q25e <- as.numeric(qq$quantile["25%"])
      q50e <- as.numeric(qq$quantile["50%"])
      q75e <- as.numeric(qq$quantile["75%"])
    }
    if (!is.null(qq$lower)) {
      q25_lo <- as.numeric(qq$lower["25%"]); q50_lo <- as.numeric(qq$lower["50%"]); q75_lo <- as.numeric(qq$lower["75%"])
    }
    if (!is.null(qq$upper)) {
      q25_hi <- as.numeric(qq$upper["25%"]); q50_hi <- as.numeric(qq$upper["50%"]); q75_hi <- as.numeric(qq$upper["75%"])
    }
  }, silent = TRUE)
  ## if survival estimates failed, fall back to point estimates
  if (is.na(q25e)) q25e <- q25pt
  if (is.na(q50e)) q50e <- q50pt
  if (is.na(q75e)) q75e <- q75pt
  ## restricted mean up to max event time
  mean_val <- NA_real_; se_mean <- NA_real_
  try({
    ev_times <- sort(unique(time[cens == 0]))
    if (length(ev_times) > 0) {
      km_ev <- km[km$d > 0, , drop = FALSE]
      km_ev <- km_ev[order(km_ev$Time), , drop = FALSE]
      S_vec <- km_ev$Survival; t_vec <- km_ev$Time
      t_all <- c(0, t_vec); S_all <- c(1, S_vec)
      mean_val <- sum(S_all[1:length(t_vec)] * diff(t_all))
      m <- nrow(km_ev)
      if (m > 1) {
        A <- numeric(m)
        for (k in 1:m) {
          if (k == m) A[k] <- 0
          else A[k] <- sum(S_vec[k:(m-1)] * diff(t_vec[k:m]))
        }
        terms <- A^2 * km_ev$d / (km_ev$n_risk * (km_ev$n_risk - km_ev$d))
        terms[km_ev$n_risk == km_ev$d] <- 0
        terms[!is.finite(terms)] <- 0
        var_mean <- m / (m - 1) * sum(terms, na.rm = TRUE)
        se_mean <- sqrt(var_mean)
      } else {
        se_mean <- NA_real_
      }
    } else {
      mean_val <- NA_real_; se_mean <- NA_real_
    }
  }, silent = TRUE)
  list(km = km, N = N, nEvent = nEvent, nCens = nCens, pctCens = pctCens,
       q75 = q75e, q75_lo = q75_lo, q75_hi = q75_hi,
       q50 = q50e, q50_lo = q50_lo, q50_hi = q50_hi,
       q25 = q25e, q25_lo = q25_lo, q25_hi = q25_hi,
       mean = mean_val, se_mean = se_mean)
}
lt <- lifetest_compute(vec_os, adam_adtte$CNSR)

## ---------- text rendering helpers ----------
line_means <- function(title, varlabel, ms) {
  c(paste0("Title: ", title),
    paste0("Variable: ", varlabel),
    " N  Mean  Std Dev  Minimum  Maximum",
    paste0(fmt_int(ms$N), "  ", fmt4(ms$Mean), "  ", fmt4(ms$StdDev), "  ", fmt4(ms$Min), "  ", fmt4(ms$Max)),
    "")
}
line_freq <- function(title, varlabel, f) {
  h <- c(paste0("Title: ", title),
         paste0("Table of ", varlabel),
         paste0("Sample Size = ", f$N),
         "Level  Frequency  Percent  Cumulative Frequency  Cumulative Percent")
  for (i in seq_len(nrow(f$table))) {
    r <- f$table[i, ]
    h <- c(h, paste0(r$Level, "  ", fmt_int(r$Frequency), "  ", fmt2(r$Percent), "  ", fmt_int(r$CumulativeFrequency), "  ", fmt2(r$CumulativePercent)))
  }
  if (f$NMiss > 0) h <- c(h, paste0("Missing = ", f$NMiss))
  c(h, "")
}
line_lifetest <- function(title, ltobj) {
  h <- c(paste0("Title: ", title),
         "The LIFETEST Procedure",
         "Product-Limit Survival Estimates",
         "Time  Survival  Failure  Survival Standard Error  Number Failed  Number Left  Censored")
  km <- ltobj$km
  for (i in seq_len(nrow(km))) {
    r <- km[i, ]
    flag <- if (r$Censored) "*" else " "
    h <- c(h, paste0(fmt3(r$Time), "  ", fmt4(r$Survival), "  ", fmt4(r$Failure), "  ", fmt4(r$SE), "  ", fmt_int(r$Failed), "  ", fmt_int(r$Left), " ", flag))
  }
  h <- c(h, "",
         "Quartile Estimates",
         "Percent  Point Estimate  95% Confidence Interval Transform [Lower, Upper)",
         paste0("75  ", fmt3(ltobj$q75), "  [", fmt3(ltobj$q75_lo), ", ", fmt3(ltobj$q75_hi), "]"),
         paste0("50  ", fmt3(ltobj$q50), "  [", fmt3(ltobj$q50_lo), ", ", fmt3(ltobj$q50_hi), "]"),
         paste0("25  ", fmt3(ltobj$q25), "  [", fmt3(ltobj$q25_lo), ", ", fmt3(ltobj$q25_hi), "]"),
         "",
         "Mean and Standard Error",
         "Mean  Standard Error",
         paste0(fmt3(ltobj$mean), "  ", fmt3(ltobj$se_mean)),
         "",
         "Summary of the Number of Censored and Uncensored Values",
         "Total  Failed  Censored  Percent Censored",
         paste0(fmt_int(ltobj$N), "  ", fmt_int(ltobj$nEvent), "  ", fmt_int(ltobj$nCens), "  ", fmt2(ltobj$pctCens)),
         "")
  h
}

## ---------- build full listing ----------
txt <- c("Project: Oncology Clinical Trial Analysis",
         "Standard: CDISC SDTM / ADaM",
         "Dataset: Breast Cancer METABRIC",
         "")

## CONTENTS
txt <- c(txt, "Procedure: CONTENTS Data=ADAM_ADSL", "Variables in Creation Order",
         "Num  Variable  Type  Length")
for (i in seq_along(names(adam_adsl))) {
  nm <- names(adam_adsl)[i]
  cl <- class(adam_adsl[[i]])[1]
  tp <- if (cl %in% c("numeric","integer","double")) "Num" else "Char"
  txt <- c(txt, paste0(i, "  ", nm, "  ", tp))
}
txt <- c(txt, "")

## PRINT obs=10
txt <- c(txt, "Procedure: PRINT Data=ADAM_ADSL (obs=10)", paste0("Obs  ", paste(names(adam_adsl), collapse = " | ")))
for (i in 1:min(10, nrow(adam_adsl))) {
  vals <- sapply(adam_adsl[i, , drop = FALSE], function(v) {
    vv <- v[[1]]
    if (is.na(vv)) "" else as.character(vv)
  })
  txt <- c(txt, paste0(i, "  ", paste(vals, collapse = " | ")))
}
txt <- c(txt, "")

txt <- c(txt, line_means("Demographic Summary - Age", cn_age_diag, means_age))
txt <- c(txt, line_freq("Gender Distribution", cn_sex, f_sex))
txt <- c(txt, line_freq("Tumor Stage Distribution", cn_stage, f_stage))
txt <- c(txt, line_means("Tumor Size Summary", cn_size, means_size))
txt <- c(txt, line_lifetest("Kaplan-Meier Survival Curve", lt))
txt <- c(txt, line_freq("Chemotherapy Treatment Summary", cn_chemo, f_chemo))
txt <- c(txt, line_freq("", cn_er, f_er))
txt <- c(txt, line_freq("", cn_her2, f_her2))
## report PDF contents (repeated)
txt <- c(txt, line_means("", cn_age_diag, means_age))
txt <- c(txt, line_freq("", cn_sex, f_sex))
txt <- c(txt, line_freq("", cn_stage, f_stage))
txt <- c(txt, line_lifetest("", lt))
## Figure 1 lifetest repeat
txt <- c(txt, line_lifetest("Kaplan-Meier Survival Curve", lt))
## SGPlot notes
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Tumor Stage Distribution  VBAR Tumor Stage",
         paste0("Levels: ", paste(f_stage$table$Level, collapse = ", ")), "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Age Distribution of Patients  HISTOGRAM + DENSITY Age", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Tumor Size vs Survival  SCATTER", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Survival by ER Status  VBOX", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Chemotherapy Treatment Distribution  VBAR Chemotherapy", "")
## second PDF graphs
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  HISTOGRAM Age", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  VBAR Tumor Stage", "")
txt <- c(txt, line_lifetest("", lt))

writeLines(txt, file.path(out, "P061.txt"))
writeLines(txt, file.path(out, "P061.lst"))

## report text (subset)
rep_txt <- c("Project: Oncology Clinical Trial Analysis  Report: P061_t_oncology_report.pdf",
             "",
             line_means("", cn_age_diag, means_age),
             line_freq("", cn_sex, f_sex),
             line_freq("", cn_stage, f_stage),
             line_lifetest("", lt))
writeLines(rep_txt, file.path(out, "P061_t_oncology_report.txt"))

## graphs text
gr_txt <- c("Project: Oncology Clinical Trial Analysis  Report: P061_f_oncology_graphs.pdf",
            "Procedure: SGPLOT Data=ADAM_ADSL  HISTOGRAM Age",
            "Procedure: SGPLOT Data=ADAM_ADSL  VBAR Tumor Stage",
            "",
            line_lifetest("", lt))
writeLines(gr_txt, file.path(out, "P061_f_oncology_graphs.txt"))

## ---------- CSVs for tables ----------
## combined long CSV for P061
mk_freq_rows <- function(tname, varlab, f) {
  df <- f$table
  data.frame(Table = tname, Variable = varlab, Level_Time = df$Level,
             N = f$N, Mean = NA_real_, StdDev = NA_real_, Min = NA_real_, Max = NA_real_,
             Frequency = df$Frequency, Percent = sas_round(df$Percent, 2),
             CumFreq = df$CumulativeFrequency, CumPercent = sas_round(df$CumulativePercent, 2),
             Survival = NA_real_, Failure = NA_real_, SurvivalSE = NA_real_,
             NumberFailed = NA_integer_, NumberLeft = NA_integer_,
             PointEstimate = NA_real_, LowerCI = NA_real_, UpperCI = NA_real_,
             stringsAsFactors = FALSE)
}
mk_means_row <- function(tname, varlab, ms) {
  data.frame(Table = tname, Variable = varlab, Level_Time = varlab,
             N = ms$N, Mean = sas_round(ms$Mean, 4), StdDev = sas_round(ms$StdDev, 4),
             Min = sas_round(ms$Min, 4), Max = sas_round(ms$Max, 4),
             Frequency = NA_integer_, Percent = NA_real_, CumFreq = NA_integer_, CumPercent = NA_real_,
             Survival = NA_real_, Failure = NA_real_, SurvivalSE = NA_real_,
             NumberFailed = NA_integer_, NumberLeft = NA_integer_,
             PointEstimate = NA_real_, LowerCI = NA_real_, UpperCI = NA_real_,
             stringsAsFactors = FALSE)
}
mk_km_rows <- function(tname, ltobj) {
  km <- ltobj$km
  df <- data.frame(Table = tname, Variable = "AVAL", Level_Time = sas_round(km$Time, 3),
                   N = ltobj$N, Mean = NA_real_, StdDev = NA_real_, Min = NA_real_, Max = NA_real_,
                   Frequency = NA_integer_, Percent = NA_real_, CumFreq = NA_integer_, CumPercent = NA_real_,
                   Survival = sas_round(km$Survival, 4), Failure = sas_round(km$Failure, 4),
                   SurvivalSE = sas_round(km$SE, 4),
                   NumberFailed = km$Failed, NumberLeft = km$Left,
                   PointEstimate = NA_real_, LowerCI = NA_real_, UpperCI = NA_real_,
                   stringsAsFactors = FALSE)
  qrows <- data.frame(
    Table = tname, Variable = c("Quartile75","Quartile50","Quartile25","Mean"),
    Level_Time = c("75","50","25","Mean"),
    N = ltobj$N, Mean = c(NA, NA, NA, sas_round(ltobj$mean, 3)),
    StdDev = c(NA, NA, NA, sas_round(ltobj$se_mean, 3)),
    Min = NA_real_, Max = NA_real_, Frequency = NA_integer_, Percent = c(75, 50, 25, sas_round(ltobj$pctCens, 2)),
    CumFreq = NA_integer_, CumPercent = NA_real_,
    Survival = NA_real_, Failure = NA_real_, SurvivalSE = NA_real_,
    NumberFailed = c(NA, NA, NA, ltobj$nEvent), NumberLeft = c(NA, NA, NA, ltobj$nCens),
    PointEstimate = c(sas_round(ltobj$q75, 3), sas_round(ltobj$q50, 3), sas_round(ltobj$q25, 3), sas_round(ltobj$mean, 3)),
    LowerCI = c(sas_round(ltobj$q75_lo, 3), sas_round(ltobj$q50_lo, 3), sas_round(ltobj$q25_lo, 3), NA_real_),
    UpperCI = c(sas_round(ltobj$q75_hi, 3), sas_round(ltobj$q50_hi, 3), sas_round(ltobj$q25_hi, 3), NA_real_),
    stringsAsFactors = FALSE)
  rbind(df, qrows)
}
combined <- rbind(
  mk_means_row("MEANS_Age", cn_age_diag, means_age),
  mk_freq_rows("FREQ_Sex", cn_sex, f_sex),
  mk_freq_rows("FREQ_Stage", cn_stage, f_stage),
  mk_means_row("MEANS_Size", cn_size, means_size),
  mk_km_rows("LIFETEST_KM", lt),
  mk_freq_rows("FREQ_Chemo", cn_chemo, f_chemo),
  mk_freq_rows("FREQ_ER", cn_er, f_er),
  mk_freq_rows("FREQ_HER2", cn_her2, f_her2)
)
write.csv(combined, file.path(out, "P061.csv"), row.names = FALSE, na = "")

rep_comb <- rbind(
  mk_means_row("MEANS_Age", cn_age_diag, means_age),
  mk_freq_rows("FREQ_Sex", cn_sex, f_sex),
  mk_freq_rows("FREQ_Stage", cn_stage, f_stage),
  mk_km_rows("LIFETEST_KM", lt)
)
write.csv(rep_comb, file.path(out, "P061_t_oncology_report.csv"), row.names = FALSE, na = "")

gr_comb <- mk_km_rows("LIFETEST_KM", lt)
write.csv(gr_comb, file.path(out, "P061_f_oncology_graphs.csv"), row.names = FALSE, na = "")

## ---------- Figures ----------
## sasgraph: KM curve
km_plot_df <- lt$km
km_plot_df$SurvPlot <- km_plot_df$Survival
p_km <- ggplot(km_plot_df, aes(x = Time, y = Survival)) +
  geom_step(direction = "hv", linewidth = 0.8) +
  geom_point(data = subset(km_plot_df, Censored), aes(x = Time, y = Survival), shape = 3, size = 2) +
  labs(title = "Kaplan-Meier Survival Curve", x = "Overall Survival (Months)", y = "Survival Probability") +
  theme_minimal()
ggsave(file.path(out, "sasgraph.png"), p_km, width = 8, height = 6, dpi = 150)
write.csv(km_plot_df[, c("Time","Survival","Failure","SE","Failed","Left","Censored")],
          file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

## sgplot: vbar Tumor Stage
stage_df <- f_stage$table
colnames(stage_df)[1] <- "Stage"
p_stage <- ggplot(stage_df, aes(x = Stage, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Frequency") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot.png"), p_stage, width = 8, height = 6, dpi = 150)
write.csv(stage_df, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

## sgplot1: histogram Age + density
age_df <- data.frame(Age = vec_age)
age_df <- age_df[!is.na(age_df$Age), , drop = FALSE]
p_hist <- ggplot(age_df, aes(x = Age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey80", colour = "black") +
  geom_density(linewidth = 0.8) +
  labs(title = "Age Distribution of Patients", x = "Age at Diagnosis", y = "Density") +
  theme_minimal()
ggsave(file.path(out, "sgplot1.png"), p_hist, width = 8, height = 6, dpi = 150)
h1 <- hist(age_df$Age, breaks = 30, plot = FALSE)
hist_df <- data.frame(BinStart = head(h1$breaks, -1), BinEnd = tail(h1$breaks, -1), Count = h1$counts, Density = h1$density)
write.csv(hist_df, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")

## sgplot2: scatter Tumor Size vs Survival
sc_df <- data.frame(TumorSize = vec_size, Survival = vec_os)
sc_df <- sc_df[complete.cases(sc_df), ]
p_sc <- ggplot(sc_df, aes(x = TumorSize, y = Survival)) +
  geom_point(alpha = 0.5) +
  labs(title = "Tumor Size vs Survival", x = "Tumor Size", y = "Overall Survival (Months)") +
  theme_minimal()
ggsave(file.path(out, "sgplot2.png"), p_sc, width = 8, height = 6, dpi = 150)
write.csv(sc_df, file.path(out, "sgplot2.csv"), row.names = FALSE, na = "")

## sgplot3: boxplot Survival by ER Status
box_df <- data.frame(ERStatus = vec_er, Survival = vec_os)
box_df <- box_df[complete.cases(box_df), ]
p_box <- ggplot(box_df, aes(x = ERStatus, y = Survival)) +
  geom_boxplot() +
  labs(title = "Survival by ER Status", x = "ER Status", y = "Overall Survival (Months)") +
  theme_minimal()
ggsave(file.path(out, "sgplot3.png"), p_box, width = 8, height = 6, dpi = 150)
write.csv(box_df, file.path(out, "sgplot3.csv"), row.names = FALSE, na = "")

## sgplot4: vbar Chemotherapy
chemo_df <- f_chemo$table
colnames(chemo_df)[1] <- "Chemotherapy"
p_chemo <- ggplot(chemo_df, aes(x = Chemotherapy, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Chemotherapy Treatment Distribution", x = "Chemotherapy", y = "Frequency") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
ggsave(file.path(out, "sgplot4.png"), p_chemo, width = 8, height = 6, dpi = 150)
write.csv(chemo_df, file.path(out, "sgplot4.csv"), row.names = FALSE, na = "")

## sgplot5: histogram Age (second PDF, histogram only)
p_hist2 <- ggplot(age_df, aes(x = Age)) +
  geom_histogram(bins = 30, fill = "grey80", colour = "black") +
  labs(title = "Age Distribution of Patients", x = "Age at Diagnosis", y = "Frequency") +
  theme_minimal()
ggsave(file.path(out, "sgplot5.png"), p_hist2, width = 8, height = 6, dpi = 150)
write.csv(hist_df, file.path(out, "sgplot5.csv"), row.names = FALSE, na = "")

## sgplot6: vbar Tumor Stage (second PDF)
p_stage2 <- ggplot(stage_df, aes(x = Stage, y = Frequency)) +
  geom_bar(stat = "identity") +
  labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Frequency") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(out, "sgplot6.png"), p_stage2, width = 8, height = 6, dpi = 150)
write.csv(stage_df, file.path(out, "sgplot6.csv"), row.names = FALSE, na = "")

message("Done P061")
