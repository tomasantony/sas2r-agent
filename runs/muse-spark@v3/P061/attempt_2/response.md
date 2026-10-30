```r
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
sas_round <- function(x, digits = 0) {
  y <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  xx <- x[ok]
  y[ok] <- sign(xx) * floor(abs(xx) * 10^digits + 0.5) / 10^digits
  y[y == 0] <- 0
  y
}
fmt4 <- function(x) { ifelse(is.na(x), "", sprintf("%.4f", sas_round(x, 4))) }
fmt3 <- function(x) { ifelse(is.na(x), "", sprintf("%.3f", sas_round(x, 3))) }
fmt2 <- function(x) { ifelse(is.na(x), "", sprintf("%.2f", sas_round(x, 2))) }
fmt_int <- function(x) { ifelse(is.na(x), "", formatC(x, format = "d")) }
norm_key <- function(x) tolower(gsub("[^a-z0-9]", "", x))
find_col <- function(df, target) {
  tn <- norm_key(target)
  cn <- norm_key(names(df))
  idx <- which(cn == tn)
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}
find_any <- function(df, targets) {
  for (t in targets) {
    r <- find_col(df, t)
    if (!is.na(r)) return(r)
  }
  return(NA_character_)
}
get_vec <- function(df, colname, n) {
  if (is.na(colname) || !(colname %in% names(df))) return(rep(NA_character_, n))
  as.character(df[[colname]])
}
get_num <- function(df, colname, n) {
  if (is.na(colname) || !(colname %in% names(df))) return(rep(NA_real_, n))
  suppressWarnings(as.numeric(df[[colname]]))
}
raw_path <- "D:/meta_hackathon/sas2r-agent/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
if (!file.exists(raw_path)) {
  cand <- "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv"
  if (file.exists(cand)) raw_path <- cand
}
if (!file.exists(raw_path)) {
  f1 <- list.files(getwd(), pattern = "Breast Cancer METABRIC\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(f1) > 0) raw_path <- f1[1]
}
if (!file.exists(raw_path)) {
  f2 <- list.files("/bench", pattern = "Breast Cancer METABRIC\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(f2) > 0) raw_path <- f2[1]
}
message("Reading: ", raw_path)
raw_metabric <- suppressMessages(readr::read_csv(raw_path, show_col_types = FALSE, name_repair = "minimal", guess_max = 10000))
message(paste0("raw rows=", nrow(raw_metabric), " cols=", ncol(raw_metabric)))
message(paste0("cols: ", paste(names(raw_metabric), collapse=" | ")))
n_obs <- nrow(raw_metabric)
cn_patient  <- find_any(raw_metabric, c("patient_id","patient id","patientid","patient id ","id"))
cn_age_diag <- find_any(raw_metabric, c("age at diagnosis","age_at_diagnosis","age"))
cn_sex      <- find_any(raw_metabric, c("sex","gender","sex at birth"))
cn_stage    <- find_any(raw_metabric, c("tumor stage","tumor_stage","tumour stage","stage"))
cn_size     <- find_any(raw_metabric, c("tumor size","tumor_size","tumour size"))
cn_os       <- find_any(raw_metabric, c("overall survival months","overall survival (months)","overall survival","os months","survival months"))
cn_osstat   <- find_any(raw_metabric, c("overall survival status","overall survivalstatus","survival status","vital status","overall survival status ","status"))
cn_chemo    <- find_any(raw_metabric, c("chemotherapy","chemo","chemotherapy "))
cn_er       <- find_any(raw_metabric, c("er status","erstatus","er status ","er"))
cn_her2     <- find_any(raw_metabric, c("her2 status","her2status","her2 status ","her2"))
message(paste0("resolved: patient=", cn_patient, " age=", cn_age_diag, " sex=", cn_sex, " stage=", cn_stage, " size=", cn_size, " os=", cn_os, " osstat=", cn_osstat, " chemo=", cn_chemo, " er=", cn_er, " her2=", cn_her2))
lab_age <- ifelse(is.na(cn_age_diag), "Age at Diagnosis", cn_age_diag)
lab_sex <- ifelse(is.na(cn_sex), "sex", cn_sex)
lab_stage <- ifelse(is.na(cn_stage), "Tumor Stage", cn_stage)
lab_size <- ifelse(is.na(cn_size), "Tumor Size", cn_size)
lab_os <- ifelse(is.na(cn_os), "Overall Survival (Months)", cn_os)
lab_osstat <- ifelse(is.na(cn_osstat), "Overall Survival Status", cn_osstat)
lab_chemo <- ifelse(is.na(cn_chemo), "Chemotherapy", cn_chemo)
lab_er <- ifelse(is.na(cn_er), "ER Status", cn_er)
lab_her2 <- ifelse(is.na(cn_her2), "HER2 Status", cn_her2)
vec_age   <- get_num(raw_metabric, cn_age_diag, n_obs)
vec_size  <- get_num(raw_metabric, cn_size, n_obs)
vec_os    <- get_num(raw_metabric, cn_os, n_obs)
vec_sex   <- get_vec(raw_metabric, cn_sex, n_obs)
vec_stage <- get_vec(raw_metabric, cn_stage, n_obs)
vec_chemo <- get_vec(raw_metabric, cn_chemo, n_obs)
vec_er    <- get_vec(raw_metabric, cn_er, n_obs)
vec_her2  <- get_vec(raw_metabric, cn_her2, n_obs)
vec_osstat<- get_vec(raw_metabric, cn_osstat, n_obs)
vec_patient <- get_vec(raw_metabric, cn_patient, n_obs)
vec_patient[is.na(vec_patient)] <- as.character(seq_len(n_obs))[is.na(vec_patient)]
sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
sdtm_dm$USUBJID <- paste0("ONC001-", as.character(vec_patient))
sdtm_dm$AGE <- vec_age
adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"
adam_adtte <- adam_adsl
adam_adtte$AVAL <- vec_os
adam_adtte$CNSR <- ifelse(is.na(vec_osstat), 1L, ifelse(vec_osstat == "Deceased", 0L, 1L))
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
freq_oneway <- function(vec) {
  total <- sum(!is.na(vec) & vec != "")
  nmiss <- sum(is.na(vec) | vec == "")
  v <- vec[!is.na(vec) & vec != ""]
  if (length(v) == 0) {
    tbl <- data.frame(Level=character(0), Frequency=integer(0), Percent=numeric(0), CumulativeFrequency=integer(0), CumulativePercent=numeric(0), stringsAsFactors=FALSE)
    return(list(table=tbl, N=0, NMiss=nmiss))
  }
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
lifetest_compute <- function(time, cens) {
  ok <- !is.na(time) & !is.na(cens)
  time <- time[ok]; cens <- cens[ok]
  N <- length(time)
  nEvent <- sum(cens == 0)
  nCens <- sum(cens == 1)
  pctCens <- if (N > 0) nCens / N * 100 else NA_real_
  if (N == 0) {
    km <- data.frame(Time=numeric(0), Survival=numeric(0), Failure=numeric(0), SE=numeric(0), Failed=integer(0), Left=integer(0), d=integer(0), c=integer(0), n_risk=integer(0), Censored=logical(0))
    return(list(km=km, N=0, nEvent=0, nCens=0, pctCens=NA_real_, q75=NA_real_, q75_lo=NA_real_, q75_hi=NA_real_, q50=NA_real_, q50_lo=NA_real_, q50_hi=NA_real_, q25=NA_real_, q25_lo=NA_real_, q25_hi=NA_real_, mean=NA_real_, se_mean=NA_real_))
  }
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
    rows[[i]] <- data.frame(Time = t, Survival = surv, Failure = 1 - surv, SE = se, Failed = cumEvents, Left = n_left, d = d, c = cc, n_risk = n_risk, stringsAsFactors = FALSE)
  }
  km <- do.call(rbind, rows)
  km$Censored <- km$d == 0
  get_q <- function(p_surv) {
    idx <- which(km$Survival <= p_surv)
    if (length(idx) == 0) return(NA_real_)
    km$Time[min(idx)]
  }
  q75pt <- get_q(0.75); q50pt <- get_q(0.5); q25pt <- get_q(0.25)
  q25_lo <- q25_hi <- q50_lo <- q50_hi <- q75_lo <- q75_hi <- NA_real_
  q25e <- q50e <- q75e <- NA_real_
  try({
    fit <- survival::survfit(survival::Surv(time, 1 - cens) ~ 1, conf.type = "log-log")
    qq <- survival::quantile(fit, probs = c(0.25, 0.5, 0.75), conf.int = TRUE)
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
  if (is.na(q25e)) q25e <- q25pt
  if (is.na(q50e)) q50e <- q50pt
  if (is.na(q75e)) q75e <- q75pt
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
      } else { se_mean <- NA_real_ }
    }
  }, silent = TRUE)
  list(km = km, N = N, nEvent = nEvent, nCens = nCens, pctCens = pctCens,
       q75 = q75e, q75_lo = q75_lo, q75_hi = q75_hi,
       q50 = q50e, q50_lo = q50_lo, q50_hi = q50_hi,
       q25 = q25e, q25_lo = q25_lo, q25_hi = q25_hi,
       mean = mean_val, se_mean = se_mean)
}
lt <- lifetest_compute(vec_os, adam_adtte$CNSR)
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
  if (nrow(f$table) > 0) {
    for (i in seq_len(nrow(f$table))) {
      r <- f$table[i, ]
      h <- c(h, paste0(r$Level, "  ", fmt_int(r$Frequency), "  ", fmt2(r$Percent), "  ", fmt_int(r$CumulativeFrequency), "  ", fmt2(r$CumulativePercent)))
    }
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
  if (nrow(km) > 0) {
    for (i in seq_len(nrow(km))) {
      r <- km[i, ]
      flag <- if (r$Censored) "*" else " "
      h <- c(h, paste0(fmt3(r$Time), "  ", fmt4(r$Survival), "  ", fmt4(r$Failure), "  ", fmt4(r$SE), "  ", fmt_int(r$Failed), "  ", fmt_int(r$Left), " ", flag))
    }
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
txt <- c("Project: Oncology Clinical Trial Analysis",
         "Standard: CDISC SDTM / ADaM",
         "Dataset: Breast Cancer METABRIC",
         "")
txt <- c(txt, "Procedure: CONTENTS Data=ADAM_ADSL", "Variables in Creation Order",
         "Num  Variable  Type  Length")
for (i in seq_along(names(adam_adsl))) {
  nm <- names(adam_adsl)[i]
  cl <- class(adam_adsl[[i]])[1]
  tp <- if (cl %in% c("numeric","integer","double")) "Num" else "Char"
  txt <- c(txt, paste0(i, "  ", nm, "  ", tp))
}
txt <- c(txt, "")
txt <- c(txt, "Procedure: PRINT Data=ADAM_ADSL (obs=10)", paste0("Obs  ", paste(names(adam_adsl), collapse = " | ")))
for (i in 1:min(10, nrow(adam_adsl))) {
  vals <- sapply(adam_adsl[i, , drop = FALSE], function(v) {
    vv <- v[[1]]
    if (is.na(vv)) "" else as.character(vv)
  })
  txt <- c(txt, paste0(i, "  ", paste(vals, collapse = " | ")))
}
txt <- c(txt, "")
txt <- c(txt, line_means("Demographic Summary - Age", lab_age, means_age))
txt <- c(txt, line_freq("Gender Distribution", lab_sex, f_sex))
txt <- c(txt, line_freq("Tumor Stage Distribution", lab_stage, f_stage))
txt <- c(txt, line_means("Tumor Size Summary", lab_size, means_size))
txt <- c(txt, line_lifetest("Kaplan-Meier Survival Curve", lt))
txt <- c(txt, line_freq("Chemotherapy Treatment Summary", lab_chemo, f_chemo))
txt <- c(txt, line_freq("", lab_er, f_er))
txt <- c(txt, line_freq("", lab_her2, f_her2))
txt <- c(txt, line_means("", lab_age, means_age))
txt <- c(txt, line_freq("", lab_sex, f_sex))
txt <- c(txt, line_freq("", lab_stage, f_stage))
txt <- c(txt, line_lifetest("", lt))
txt <- c(txt, line_lifetest("Kaplan-Meier Survival Curve", lt))
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Tumor Stage Distribution  VBAR Tumor Stage",
         paste0("Levels: ", paste(if(nrow(f_stage$table)>0) f_stage$table$Level else "", collapse = ", ")), "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Age Distribution of Patients  HISTOGRAM + DENSITY Age", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Tumor Size vs Survival  SCATTER", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Survival by ER Status  VBOX", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  Title: Chemotherapy Treatment Distribution  VBAR Chemotherapy", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  HISTOGRAM Age", "")
txt <- c(txt, "Procedure: SGPLOT Data=ADAM_ADSL  VBAR Tumor Stage", "")
txt <- c(txt, line_lifetest("", lt))
writeLines(txt, file.path(out, "P061.txt"))
writeLines(txt, file.path(out, "P061.lst"))
rep_txt <- c("Project: Oncology Clinical Trial Analysis  Report: P061_t_oncology_report.pdf",
             "",
             line_means("", lab_age, means_age),
             line_freq("", lab_sex, f_sex),
             line_freq("", lab_stage, f_stage),
             line_lifetest("", lt))
writeLines(rep_txt, file.path(out, "P061_t_oncology_report.txt"))
gr_txt <- c("Project: Oncology Clinical Trial Analysis  Report: P061_f_oncology_graphs.pdf",
            "Procedure: SGPLOT Data=ADAM_ADSL  HISTOGRAM Age",
            "Procedure: SGPLOT Data=ADAM_ADSL  VBAR Tumor Stage",
            "",
            line_lifetest("", lt))
writeLines(gr_txt, file.path(out, "P061_f_oncology_graphs.txt"))
mk_freq_rows <- function(tname, varlab, f) {
  df <- f$table
  if (nrow(df)==0) return(data.frame(Table=tname, Variable=varlab, Level_Time=character(0), N=numeric(0), Mean=numeric(0), StdDev=numeric(0), Min=numeric(0), Max=numeric(0), Frequency=numeric(0), Percent=numeric(0), CumFreq=numeric(0), CumPercent=numeric(0), Survival=numeric(0), Failure=numeric(0), SurvivalSE=numeric(0), NumberFailed=numeric(0), NumberLeft=numeric(0), PointEstimate=numeric(0), LowerCI=numeric(0), UpperCI=numeric(0), stringsAsFactors=FALSE))
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
  if (nrow(km)==0) return(data.frame(Table=character(0), Variable=character(0), Level_Time=numeric(0), N=numeric(0), Mean=numeric(0), StdDev=numeric(0), Min=numeric(0), Max=numeric(0), Frequency=numeric(0), Percent=numeric(0), CumFreq=numeric(0), CumPercent=numeric(0), Survival=numeric(0), Failure=numeric(0), SurvivalSE=numeric(0), NumberFailed=numeric(0), NumberLeft=numeric(0), PointEstimate=numeric(0), LowerCI=numeric(0), UpperCI=numeric(0), stringsAsFactors=FALSE))
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
  mk_means_row("MEANS_Age", lab_age, means_age),
  mk_freq_rows("FREQ_Sex", lab_sex, f_sex),
  mk_freq_rows("FREQ_Stage", lab_stage, f_stage),
  mk_means_row("MEANS_Size", lab_size, means_size),
  mk_km_rows("LIFETEST_KM", lt),
  mk_freq_rows("FREQ_Chemo", lab_chemo, f_chemo),
  mk_freq_rows("FREQ_ER", lab_er, f_er),
  mk_freq_rows("FREQ_HER2", lab_her2, f_her2)
)
write.csv(combined, file.path(out, "P061.csv"), row.names = FALSE, na = "")
rep_comb <- rbind(
  mk_means_row("MEANS_Age", lab_age, means_age),
  mk_freq_rows("FREQ_Sex", lab_sex, f_sex),
  mk_freq_rows("FREQ_Stage", lab_stage, f_stage),
  mk_km_rows("LIFETEST_KM", lt)
)
write.csv(rep_comb, file.path(out, "P061_t_oncology_report.csv"), row.names = FALSE, na = "")
gr_comb <- mk_km_rows("LIFETEST_KM", lt)
write.csv(gr_comb, file.path(out, "P061_f_oncology_graphs.csv"), row.names = FALSE, na = "")
km_plot_df <- lt$km
try({
  p_km <- ggplot(km_plot_df, aes(x = Time, y = Survival)) +
    geom_step(direction = "hv", linewidth = 0.8) +
    labs(title = "Kaplan-Meier Survival Curve", x = "Overall Survival (Months)", y = "Survival Probability") +
    theme_minimal()
  if (any(km_plot_df$Censored)) p_km <- p_km + geom_point(data = subset(km_plot_df, Censored), aes(x = Time, y = Survival), shape = 3, size = 2)
  ggsave(file.path(out, "sasgraph.png"), p_km, width = 8, height = 6, dpi = 150)
}, silent=TRUE)
try({ write.csv(km_plot_df[, c("Time","Survival","Failure","SE","Failed","Left","Censored")], file.path(out, "sasgraph.csv"), row.names = FALSE, na = "") }, silent=TRUE)
stage_df <- f_stage$table
try({
  if (nrow(stage_df)>0) {
    colnames(stage_df)[1] <- "Stage"
    p_stage <- ggplot(stage_df, aes(x = Stage, y = Frequency)) +
      geom_bar(stat = "identity") +
      labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Frequency") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggsave(file.path(out, "sgplot.png"), p_stage, width = 8, height = 6, dpi = 150)
  } else {
    png(file.path(out, "sgplot.png"), width=800, height=600); plot.new(); title("Tumor Stage Distribution"); dev.off()
  }
  write.csv(stage_df, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
}, silent=TRUE)
age_df <- data.frame(Age = vec_age)
age_df <- age_df[!is.na(age_df$Age), , drop = FALSE]
try({
  p_hist <- ggplot(age_df, aes(x = Age)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "grey80", colour = "black") +
    geom_density(linewidth = 0.8) +
    labs(title = "Age Distribution of Patients", x = "Age at Diagnosis", y = "Density") +
    theme_minimal()
  ggsave(file.path(out, "sgplot1.png"), p_hist, width = 8, height = 6, dpi = 150)
  h1 <- hist(age_df$Age, breaks = 30, plot = FALSE)
  hist_df <- data.frame(BinStart = head(h1$breaks, -1), BinEnd = tail(h1$breaks, -1), Count = h1$counts, Density = h1$density)
  write.csv(hist_df, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
}, silent=TRUE)
sc_df <- data.frame(TumorSize = vec_size, Survival = vec_os)
sc_df <- sc_df[complete.cases(sc_df), ]
try({
  if (nrow(sc_df)>0) {
    p_sc <- ggplot(sc_df, aes(x = TumorSize, y = Survival)) +
      geom_point(alpha = 0.5) +
      labs(title = "Tumor Size vs Survival", x = "Tumor Size", y = "Overall Survival (Months)") +
      theme_minimal()
    ggsave(file.path(out, "sgplot2.png"), p_sc, width = 8, height = 6, dpi = 150)
  } else { png(file.path(out, "sgplot2.png"), width=800, height=600); plot.new(); title("Tumor Size vs Survival"); dev.off() }
  write.csv(sc_df, file.path(out, "sgplot2.csv"), row.names = FALSE, na = "")
}, silent=TRUE)
box_df <- data.frame(ERStatus = vec_er, Survival = vec_os)
box_df <- box_df[complete.cases(box_df) & box_df$ERStatus!="", , drop=FALSE]
try({
  if (nrow(box_df)>0) {
    p_box <- ggplot(box_df, aes(x = ERStatus, y = Survival)) +
      geom_boxplot() +
      labs(title = "Survival by ER Status", x = "ER Status", y = "Overall Survival (Months)") +
      theme_minimal()
    ggsave(file.path(out, "sgplot3.png"), p_box, width = 8, height = 6, dpi = 150)
  } else { png(file.path(out, "sgplot3.png"), width=800, height=600); plot.new(); title("Survival by ER Status"); dev.off() }
  write.csv(box_df, file.path(out, "sgplot3.csv"), row.names = FALSE, na = "")
}, silent=TRUE)
chemo_df <- f_chemo$table
try({
  if (nrow(chemo_df)>0) {
    colnames(chemo_df)[1] <- "Chemotherapy"
    p_chemo <- ggplot(chemo_df, aes(x = Chemotherapy, y = Frequency)) +
      geom_bar(stat = "identity") +
      labs(title = "Chemotherapy Treatment Distribution", x = "Chemotherapy", y = "Frequency") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    ggsave(file.path(out, "sgplot4.png"), p_chemo, width = 8, height = 6, dpi = 150)
  } else { png(file.path(out, "sgplot4.png"), width=800, height=600); plot.new(); title("Chemotherapy Treatment Distribution"); dev.off() }
  write.csv(chemo_df, file.path(out, "sgplot4.csv"), row.names = FALSE, na = "")
}, silent=TRUE)
try({
  p_hist2 <- ggplot(age_df, aes(x = Age)) +
    geom_histogram(bins = 30, fill = "grey80", colour = "black") +
    labs(title = "Age Distribution of Patients", x = "Age at Diagnosis", y = "Frequency") +
    theme_minimal()
  ggsave(file.path(out, "sgplot5.png"), p_hist2, width = 8, height = 6, dpi = 150)
  h1b <- hist(age_df$Age, breaks = 30, plot = FALSE)
  hist_df2 <- data.frame(BinStart = head(h1b$breaks, -1), BinEnd = tail(h1b$breaks, -1), Count = h1b$counts, Density = h1b$density)
  write.csv(hist_df2, file.path(out, "sgplot5.csv"), row.names = FALSE, na = "")
}, silent=TRUE)
try({
  if (nrow(stage_df)>0) {
    sdf2 <- stage_df
    if (!("Stage" %in% names(sdf2))) colnames(sdf2)[1] <- "Stage"
    p_stage2 <- ggplot(sdf2, aes(x = Stage, y = Frequency)) +
      geom_bar(stat = "identity") +
      labs(title = "Tumor Stage Distribution", x = "Tumor Stage", y = "Frequency") +
      theme_minimal() + theme(axis.text.x = element_text(angle = 30, hjust = 1))
    ggsave(file.path(out, "sgplot6.png"), p_stage2, width = 8, height = 6, dpi = 150)
    write.csv(sdf2, file.path(out, "sgplot6.csv"), row.names = FALSE, na = "")
  } else { png(file.path(out, "sgplot6.png"), width=800, height=600); plot.new(); title("Tumor Stage Distribution"); dev.off(); write.csv(stage_df, file.path(out, "sgplot6.csv"), row.names=FALSE, na="") }
}, silent=TRUE)
message("Done P061")
```