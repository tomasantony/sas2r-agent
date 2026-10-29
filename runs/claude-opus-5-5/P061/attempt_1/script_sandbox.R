# P061 - Oncology SDTM/ADaM/TLF analysis (METABRIC) - R translation
suppressPackageStartupMessages({
  library(readr); library(dplyr); library(ggplot2); library(survival)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rhu <- function(x, d = 0) { m <- 10^d; sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m }
fmtn <- function(x, d) ifelse(is.na(x), ".", formatC(rhu(x, d), format = "f", digits = d))

# ---------------- Import raw data ----------------
raw_metabric <- readr::read_csv(
  "/bench/data/raw/sumit-oncology/Breast Cancer METABRIC.csv",
  show_col_types = FALSE, na = c("", "NA"), guess_max = 20)
raw_metabric <- as.data.frame(raw_metabric, check.names = FALSE)
cat("Read", nrow(raw_metabric), "rows\n")

# ---------------- SDTM DM ----------------
# Note: under validvarname=any, patient_id / age_at_diagnosis do not exist in the
# imported data -> new numeric missing variables; catx skips missing -> "ONC001".
sdtm_dm <- raw_metabric
sdtm_dm$STUDYID <- "ONC001"
sdtm_dm$DOMAIN <- "DM"
sdtm_dm$patient_id <- NA_real_
sdtm_dm$USUBJID <- "ONC001"
sdtm_dm$age_at_diagnosis <- NA_real_
sdtm_dm$AGE <- NA_real_

# ---------------- ADaM ADSL ----------------
adam_adsl <- sdtm_dm
adam_adsl$TRT01P <- "CHEMO"
adam_adsl$SAFFL <- "Y"
write.csv(adam_adsl, file.path(out, "adam_adsl.csv"), row.names = FALSE, na = "")

# ---------------- ADTTE ----------------
adam_adtte <- adam_adsl
adam_adtte$AVAL <- adam_adtte[["Overall Survival (Months)"]]
oss <- adam_adtte[["Overall Survival Status"]]
adam_adtte$CNSR <- ifelse(!is.na(oss) & oss == "Deceased", 0, 1)
write.csv(adam_adtte, file.path(out, "adam_adtte.csv"), row.names = FALSE, na = "")

# ---------------- Helpers producing SAS-like output ----------------
means_tab <- function(df, var) {
  x <- df[[var]]; x <- x[!is.na(x)]
  data.frame(Variable = var, N = length(x), Mean = mean(x), Std_Dev = sd(x),
             Minimum = min(x), Maximum = max(x), check.names = FALSE)
}
means_txt <- function(mt, title = "") {
  c(title, "", "The MEANS Procedure", "", paste("Analysis Variable :", mt$Variable), "",
    sprintf("%8s %14s %14s %14s %14s", "N", "Mean", "Std Dev", "Minimum", "Maximum"),
    sprintf("%8d %14s %14s %14s %14s", mt$N, fmtn(mt$Mean, 7), fmtn(mt$Std_Dev, 7),
            fmtn(mt$Minimum, 7), fmtn(mt$Maximum, 7)), "")
}
freq_tab <- function(df, var) {
  x <- df[[var]]
  if (is.character(x)) x[!is.na(x) & trimws(x) == ""] <- NA
  nmiss <- sum(is.na(x)); x <- x[!is.na(x)]
  tb <- table(x)
  lv <- names(tb)
  if (is.numeric(x)) ord <- order(as.numeric(lv)) else ord <- order(lv, method = "radix")
  f <- as.numeric(tb)[ord]; lv <- lv[ord]
  res <- data.frame(Variable = var, Value = lv, Frequency = f,
                    Percent = 100 * f / sum(f), Cumulative_Frequency = cumsum(f),
                    Cumulative_Percent = 100 * cumsum(f) / sum(f), stringsAsFactors = FALSE)
  attr(res, "nmiss") <- nmiss
  res
}
freq_txt <- function(ft, title = "") {
  c(title, "", "The FREQ Procedure", "",
    sprintf("%-25s %10s %9s %12s %12s", ft$Variable[1], "Frequency", "Percent", "Cumulative", "Cumulative"),
    sprintf("%-25s %10s %9s %12s %12s", "", "", "", "Frequency", "Percent"),
    sprintf("%-25s %10d %9s %12d %12s", ft$Value, as.integer(ft$Frequency), fmtn(ft$Percent, 2),
            as.integer(ft$Cumulative_Frequency), fmtn(ft$Cumulative_Percent, 2)),
    if (attr(ft, "nmiss") > 0) paste("Frequency Missing =", attr(ft, "nmiss")) else NULL, "")
}

# ---------------- Lifetest ----------------
lt <- adam_adtte %>% filter(!is.na(AVAL)) %>% arrange(AVAL, CNSR)
N <- nrow(lt); ninv <- sum(is.na(adam_adtte$AVAL))
S <- 1; gw <- 0; nf <- 0
pl <- data.frame(AVAL = 0, Censor = "", Survival = 1, Failure = 0, SurvStdErr = 0,
                 Failed = 0, Left = N, stringsAsFactors = FALSE)
rows <- vector("list", N)
for (i in seq_len(N)) {
  nr <- N - i + 1; ev <- lt$CNSR[i] == 0
  if (ev) {
    S <- S * (1 - 1 / nr); nf <- nf + 1
    if (nr > 1) gw <- gw + 1 / (nr * (nr - 1))
  }
  last_tie <- ev && (i == N || lt$AVAL[i + 1] != lt$AVAL[i] || lt$CNSR[i + 1] != 0)
  se <- if (S <= 0) 0 else S * sqrt(gw)
  rows[[i]] <- data.frame(AVAL = lt$AVAL[i], Censor = ifelse(ev, "", "*"),
                          Survival = ifelse(last_tie, S, NA), Failure = ifelse(last_tie, 1 - S, NA),
                          SurvStdErr = ifelse(last_tie, se, NA), Failed = nf, Left = N - i,
                          stringsAsFactors = FALSE)
}
pl <- rbind(pl, do.call(rbind, rows))

fit <- survfit(Surv(AVAL, 1 - CNSR) ~ 1, data = lt, conf.type = "log-log")
qq <- quantile(fit, probs = c(0.25, 0.5, 0.75))
quart <- data.frame(Percent = c(75, 50, 25), Estimate = as.numeric(qq$quantile),
                    Lower = as.numeric(qq$lower), Upper = as.numeric(qq$upper))
st <- summary(fit, rmean = "common")$table
nfail <- sum(lt$CNSR == 0)
cens <- data.frame(Total = N, Failed = nfail, Censored = N - nfail,
                   Percent_Censored = 100 * (N - nfail) / N)

fmtb <- function(x, d) ifelse(is.na(x), "", formatC(rhu(x, d), format = "f", digits = d))
lifetest_txt <- function(title = "") {
  c(title, "", "The LIFETEST Procedure", "", "Product-Limit Survival Estimates", "",
    sprintf("%10s %1s %10s %10s %10s %8s %8s", "AVAL", "", "Survival", "Failure",
            "StdErr", "Failed", "Left"),
    sprintf("%10s %1s %10s %10s %10s %8d %8d", fmtn(pl$AVAL, 3), pl$Censor,
            fmtb(pl$Survival, 4), fmtb(pl$Failure, 4), fmtb(pl$SurvStdErr, 4),
            as.integer(pl$Failed), as.integer(pl$Left)),
    "", "NOTE: The marked survival times are censored observations.", "",
    "Quartile Estimates", sprintf("%8s %10s %10s %10s", "Percent", "Estimate", "Lower", "Upper"),
    sprintf("%8d %10s %10s %10s", as.integer(quart$Percent), fmtn(quart$Estimate, 3),
            fmtn(quart$Lower, 3), fmtn(quart$Upper, 3)), "",
    sprintf("Mean %10s   Standard Error %10s", fmtn(st["rmean"], 3), fmtn(st["se(rmean)"], 3)), "",
    "Summary of the Number of Censored and Uncensored Values",
    sprintf("%8s %8s %8s %10s", "Total", "Failed", "Censored", "Percent"),
    sprintf("%8d %8d %8d %10s", N, nfail, N - nfail, fmtn(cens$Percent_Censored, 2)),
    if (ninv > 0) paste("NOTE:", ninv, "observations with invalid time, censoring, or strata values were deleted.") else NULL,
    "")
}

# ---------------- Main listing (P061.lst) ----------------
m_age <- means_tab(adam_adsl, "Age at Diagnosis")
m_tsz <- means_tab(adam_adsl, "Tumor Size")
f_sex <- freq_tab(adam_adsl, "Sex")
f_stg <- freq_tab(adam_adsl, "Tumor Stage")
f_chm <- freq_tab(adam_adsl, "Chemotherapy")
f_er  <- freq_tab(adam_adsl, "ER Status")
f_her <- freq_tab(adam_adsl, "HER2 Status")

contents <- data.frame(Num = seq_along(adam_adsl), Variable = names(adam_adsl),
                       Type = ifelse(sapply(adam_adsl, is.numeric), "Num", "Char"))
p10 <- head(adam_adsl, 10)
lst <- c("The CONTENTS Procedure", paste("Observations:", nrow(adam_adsl)),
         paste("Variables:", ncol(adam_adsl)), "",
         sprintf("%4d %-40s %s", contents$Num, contents$Variable, contents$Type), "",
         "PROC PRINT (obs=10)", capture.output(print(p10, row.names = TRUE)), "",
         means_txt(m_age, "Demographic Summary - Age"),
         freq_txt(f_sex, "Gender Distribution"),
         freq_txt(f_stg, "Tumor Stage Distribution"),
         means_txt(m_tsz, "Tumor Size Summary"),
         lifetest_txt("Kaplan-Meier Survival Curve"),
         freq_txt(f_chm, "Chemotherapy Treatment Summary"),
         freq_txt(f_er, "Chemotherapy Treatment Summary"),
         freq_txt(f_her, "Chemotherapy Treatment Summary"),
         lifetest_txt("Kaplan-Meier Survival Curve"))
writeLines(lst, file.path(out, "P061.txt"))

allfreq <- rbind(f_sex, f_stg, f_chm, f_er, f_her)
write.csv(rbind(m_age, m_tsz), file.path(out, "P061_means.csv"), row.names = FALSE, na = "")
write.csv(allfreq, file.path(out, "P061_freq.csv"), row.names = FALSE, na = "")
write.csv(pl, file.path(out, "P061_product_limit.csv"), row.names = FALSE, na = "")
write.csv(quart, file.path(out, "P061_quartiles.csv"), row.names = FALSE, na = "")
write.csv(cens, file.path(out, "P061_censor_summary.csv"), row.names = FALSE, na = "")
write.csv(pl, file.path(out, "P061.csv"), row.names = FALSE, na = "")

# ---------------- PDF table report ----------------
rep <- c(means_txt(m_age, "Kaplan-Meier Survival Curve"),
         freq_txt(f_sex, "Kaplan-Meier Survival Curve"),
         freq_txt(f_stg, "Kaplan-Meier Survival Curve"),
         lifetest_txt("Kaplan-Meier Survival Curve"))
writeLines(rep, file.path(out, "P061_t_oncology_report.txt"))
write.csv(rbind(f_sex, f_stg), file.path(out, "P061_t_oncology_report.csv"), row.names = FALSE, na = "")

# ---------------- Figures ----------------
km_df <- data.frame(time = c(0, fit$time), surv = c(1, fit$surv),
                    n.censor = c(0, fit$n.censor))
km_plot <- ggplot(km_df, aes(time, surv)) + geom_step() +
  geom_point(data = subset(km_df, n.censor > 0), shape = 3, size = 1) +
  ylim(0, 1) + labs(x = "AVAL", y = "Survival Probability",
                    title = "Product-Limit Survival Estimate") + theme_bw()
ggsave(file.path(out, "sasgraph.png"), km_plot, width = 6.4, height = 4.8, dpi = 100)
write.csv(km_df, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

bar_stage <- function(title) {
  d <- adam_adsl %>% count(`Tumor Stage`) %>% mutate(Stage = factor(ifelse(is.na(`Tumor Stage`), ".", `Tumor Stage`)))
  d <- d %>% filter(!is.na(`Tumor Stage`))
  list(d = d, p = ggplot(d, aes(Stage, n)) + geom_col(fill = "steelblue") +
         labs(x = "Tumor Stage", y = "Frequency", title = title) + theme_bw())
}
hist_age <- function(title, dens = TRUE) {
  d <- data.frame(age = adam_adsl[["Age at Diagnosis"]]); d <- d[!is.na(d$age), , drop = FALSE]
  p <- ggplot(d, aes(age)) + geom_histogram(aes(y = after_stat(density) * 100), bins = 20,
                                            fill = "steelblue", color = "black")
  if (dens) p <- p + stat_function(fun = function(x) dnorm(x, mean(d$age), sd(d$age)) * 100)
  list(d = d, p = p + labs(x = "Age at Diagnosis", y = "Percent", title = title) + theme_bw())
}
save_fig <- function(obj, name) {
  ggsave(file.path(out, paste0(name, ".png")), obj$p, width = 6.4, height = 4.8, dpi = 100)
  write.csv(obj$d, file.path(out, paste0(name, ".csv")), row.names = FALSE, na = "")
}

save_fig(bar_stage("Tumor Stage Distribution"), "sgplot")
save_fig(hist_age("Age Distribution of Patients"), "sgplot1")

sc <- data.frame(TumorSize = adam_adsl[["Tumor Size"]], OS = adam_adsl[["Overall Survival (Months)"]])
save_fig(list(d = sc, p = ggplot(na.omit(sc), aes(TumorSize, OS)) + geom_point(shape = 1) +
                labs(x = "Tumor Size", y = "Overall Survival (Months)", title = "Tumor Size vs Survival") +
                theme_bw()), "sgplot2")

bx <- data.frame(ER = adam_adsl[["ER Status"]], OS = adam_adsl[["Overall Survival (Months)"]])
bx <- bx[!is.na(bx$ER) & !is.na(bx$OS), ]
save_fig(list(d = bx, p = ggplot(bx, aes(ER, OS)) + geom_boxplot() +
                stat_summary(fun = mean, geom = "point", shape = 5) +
                labs(x = "ER Status", y = "Overall Survival (Months)", title = "Survival by ER Status") +
                theme_bw()), "sgplot3")

ch <- adam_adsl %>% filter(!is.na(Chemotherapy)) %>% count(Chemotherapy)
save_fig(list(d = ch, p = ggplot(ch, aes(Chemotherapy, n)) + geom_col(fill = "steelblue") +
                labs(y = "Frequency", title = "Chemotherapy Treatment Distribution") + theme_bw()), "sgplot4")

save_fig(hist_age("Chemotherapy Treatment Distribution", dens = FALSE), "sgplot5")
save_fig(bar_stage("Chemotherapy Treatment Distribution"), "sgplot6")

# ---------------- PDF graphs report (text content) ----------------
g_txt <- c("Chemotherapy Treatment Distribution", "", "Histogram: Age at Diagnosis",
           sprintf("N = %d", nrow(hist_age("", FALSE)$d)), "",
           "Bar chart: Tumor Stage",
           sprintf("%-10s %10d", as.character(bar_stage("")$d$Stage), bar_stage("")$d$n), "",
           lifetest_txt("Chemotherapy Treatment Distribution"))
writeLines(g_txt, file.path(out, "P061_f_oncology_graphs.txt"))
write.csv(pl, file.path(out, "P061_f_oncology_graphs.csv"), row.names = FALSE, na = "")

cat("P061 done\n")
