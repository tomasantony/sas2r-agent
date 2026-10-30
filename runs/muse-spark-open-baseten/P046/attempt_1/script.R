# P046 Box Plot Baseline - R translation
# SAS program: P046_Box_Plot_Baseline.sas

# Load required packages (already installed)
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
  library(Hmisc)
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(gt)
  library(r2rtf)
  library(arrow)
})

# Helper: SAS round half away from zero
round2 <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  sign_x <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  sign_x * z / 10^digits
}
fmt_num <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  formatC(round2(x, digits), format = "f", digits = digits, flag = "")
}
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  formatC(round2(p, 4), format = "f", digits = 4)
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("OUTDIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- Sys.getenv("ROOT")
if (root == "" || is.na(root)) root <- "D:/meta_hackathon/sas2r-agent"
rawadam_env <- Sys.getenv("RAWADAM")
if (rawadam_env == "" || is.na(rawadam_env)) rawadam_env <- file.path(root, "data/raw/cdisc-pilot/adam")
rawadam2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_dir <- Sys.getenv("INLIB")
if (inlib_dir == "" || is.na(inlib_dir)) inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
inlib_dir2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

# Locate advs.xpt
candidates <- c(
  file.path(rawadam_env, "advs.xpt"),
  file.path(rawadam2, "advs.xpt"),
  file.path(inlib_dir, "advs.xpt"),
  file.path(inlib_dir2, "advs.xpt"),
  file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) {
  # try case-insensitive search
  all_dirs <- unique(c(rawadam_env, rawadam2, inlib_dir, inlib_dir2))
  for (d in all_dirs) {
    if (dir.exists(d)) {
      fls <- list.files(d, pattern = "^advs\\.", ignore.case = TRUE, full.names = TRUE)
      if (length(fls) > 0) { advs_path <- fls[1]; break }
    }
  }
}
if (is.null(advs_path)) stop("advs.xpt not found in any candidate path")

cat("Reading:", advs_path, "\n")
advs_raw <- haven::read_xpt(advs_path)

# Ensure character columns are character (haven may give labelled)
advs_raw <- advs_raw %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

# Keep and filter as in SAS:
# keep USUBJID SAFFL PARAMCD TRTPN TRTP AVAL AVISIT AVISITN ATPT;
# where SAFFL='Y' and PARAMCD in ('DIABP', 'PULSE') and ATPT='AFTER LYING DOWN FOR 5 MINUTES' and AVISITN=99;
advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD %in% c("DIABP", "PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         AVISITN == 99) %>%
  select(any_of(c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")))

# advs1: PARAMCD_NUM
advs1 <- advs %>%
  mutate(PARAMCD_NUM = ifelse(PARAMCD == "DIABP", 0, 1))

# Write intermediate datasets as CSV (dataset outputs)
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")
write.csv(advs1, file.path(out, "advs1.csv"), row.names = FALSE, na = "")
# Also write P046 dataset counterpart
write.csv(advs1, file.path(out, "P046_advs1.csv"), row.names = FALSE, na = "")

# First SGPlot: scatter y=AVAL x=PARAMCD_NUM
p1_data <- advs1 %>% select(PARAMCD_NUM, AVAL, PARAMCD)
# CSV of plotted data
write.csv(p1_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(p1_data, file.path(out, "SGPlot.csv"), row.names = FALSE, na = "")

p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(color = "black", size = 1.5, alpha = 0.7) +
  scale_x_continuous(breaks = c(0,1), labels = c("0","1")) +
  labs(x = "PARAMCD_NUM", y = "AVAL",
       title = "Scatter Plot of AVAL by PARAMCD_NUM",
       subtitle = "DIABP=0, PULSE=1") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 300)
ggsave(file.path(out, "SGPlot.png"), plot = p1, width = 6, height = 4, dpi = 300)
# also sgplot1 naming for first? SAS first is SGPlot, second SGPlot1
# Ensure both exist
if (!file.exists(file.path(out, "sgplot1.png"))) {
  # will be overwritten by second plot below
}

# Linear model: AVAL = PARAMCD_NUM
# SAS: proc reg data=advs1; model AVAL=PARAMCD_NUM; output out=regout p=fitted_value r=residuals;
fit <- lm(AVAL ~ PARAMCD_NUM, data = advs1)
sfit <- summary(fit)
anova_fit <- anova(fit)

n_obs_read <- nrow(advs1)
n_obs_used <- nobs(fit)
n_total <- n_obs_used
df_model <- 1
df_error <- n_obs_used - 2
df_total <- n_obs_used - 1

# ANOVA values
ss_model <- anova_fit["PARAMCD_NUM", "Sum Sq"]
ss_error <- anova_fit["Residuals", "Sum Sq"]
ss_total <- ss_model + ss_error
ms_model <- anova_fit["PARAMCD_NUM", "Mean Sq"]
ms_error <- anova_fit["Residuals", "Mean Sq"]
f_value <- anova_fit["PARAMCD_NUM", "F value"]
p_value <- anova_fit["PARAMCD_NUM", "Pr(>F)"]

root_mse <- sfit$sigma
dep_mean <- mean(advs1$AVAL, na.rm = TRUE)
r_square <- sfit$r.squared
adj_r_square <- sfit$adj.r.squared
coeff_var <- ifelse(dep_mean != 0, root_mse / dep_mean * 100, NA_real_)

# Parameter estimates
coef_tbl <- as.data.frame(sfit$coefficients)
# rownames: (Intercept), PARAMCD_NUM
param_est <- tibble(
  Variable = rownames(coef_tbl),
  DF = 1,
  Estimate = coef_tbl[, "Estimate"],
  StdErr = coef_tbl[, "Std. Error"],
  tValue = coef_tbl[, "t value"],
  pValue = coef_tbl[, "Pr(>|t|)"]
)
# Rename intercept for display
param_est$Variable[param_est$Variable == "(Intercept)"] <- "Intercept"

# Create regout dataset
regout <- advs1 %>%
  mutate(fitted_value = predict(fit, newdata = advs1),
         residuals = residuals(fit))

write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")
write.csv(regout, file.path(out, "P046_regout.csv"), row.names = FALSE, na = "")

# Second SGPlot: scatter x=fitted_value y=residuals + loess + refline 0
p2_data <- regout %>% select(fitted_value, residuals, AVAL, PARAMCD_NUM)
write.csv(p2_data, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
write.csv(p2_data, file.path(out, "SGPlot1.csv"), row.names = FALSE, na = "")

p2 <- ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(color = "black", size = 1.5, alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", formula = y ~ x) +
  geom_hline(yintercept = 0, linetype = "solid", color = "red") +
  labs(x = "Fitted Value", y = "Residuals",
       title = "Residuals vs Fitted Values",
       subtitle = "with Loess smoother and reference line at 0") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 6, height = 4, dpi = 300)
ggsave(file.path(out, "SGPlot1.png"), plot = p2, width = 6, height = 4, dpi = 300)
# also ensure lowercase sgplot.png already exists

# Build P046.lst text rendering (PROC REG output)
# Count numbers: should be 29 numbers
# Create text file with SAS-like listing
txt_lines <- c(
  "P046 Box Plot Baseline - Linear Regression of AVAL on PARAMCD_NUM",
  "=====================================================================",
  paste0("Data: advs (SAFFL='Y', PARAMCD in (DIABP,PULSE), ATPT='AFTER LYING DOWN FOR 5 MINUTES', AVISITN=99)"),
  "",
  "The REG Procedure",
  "Model: Linear Regression Model",
  "Dependent Variable: AVAL",
  "",
  sprintf("Number of Observations Read: %d", n_obs_read),
  sprintf("Number of Observations Used: %d", n_obs_used),
  "",
  "Analysis of Variance",
  "-------------------------------------------------------------------------------",
  sprintf("%-12s %5s %14s %14s %10s %10s", "Source", "DF", "Sum of Squares", "Mean Square", "F Value", "Pr > F"),
  sprintf("%-12s %5d %14s %14s %10s %10s", "Model", df_model, fmt_num(ss_model,2), fmt_num(ms_model,2), fmt_num(f_value,2), fmt_p(p_value)),
  sprintf("%-12s %5d %14s %14s", "Error", df_error, fmt_num(ss_error,2), fmt_num(ms_error,2)),
  sprintf("%-12s %5d %14s", "Corrected Total", df_total, fmt_num(ss_total,2)),
  "-------------------------------------------------------------------------------",
  "",
  "Fit Statistics",
  "-------------------------------------------------------------------------------",
  sprintf("%-20s %12s", "Root MSE", fmt_num(root_mse,4)),
  sprintf("%-20s %12s", "Dependent Mean", fmt_num(dep_mean,4)),
  sprintf("%-20s %12s", "Coeff Var", fmt_num(coeff_var,4)),
  sprintf("%-20s %12s", "R-Square", fmt_num(r_square,4)),
  sprintf("%-20s %12s", "Adj R-Sq", fmt_num(adj_r_square,4)),
  "-------------------------------------------------------------------------------",
  "",
  "Parameter Estimates",
  "------------------------------------------------------------------------------------------",
  sprintf("%-15s %4s %14s %14s %10s %10s", "Variable", "DF", "Parameter Estimate", "Standard Error", "t Value", "Pr > |t|"),
  sprintf("%-15s %4d %14s %14s %10s %10s",
          param_est$Variable[1], param_est$DF[1],
          fmt_num(param_est$Estimate[1],4), fmt_num(param_est$StdErr[1],4),
          fmt_num(param_est$tValue[1],2), fmt_p(param_est$pValue[1])),
  sprintf("%-15s %4d %14s %14s %10s %10s",
          param_est$Variable[2], param_est$DF[2],
          fmt_num(param_est$Estimate[2],4), fmt_num(param_est$StdErr[2],4),
          fmt_num(param_est$tValue[2],2), fmt_p(param_est$pValue[2])),
  "------------------------------------------------------------------------------------------",
  "",
  "Note: PARAMCD_NUM coded as 0=DIABP, 1=PULSE",
  sprintf("Fitted values (p) and residuals (r) output to regout dataset (N=%d)", nrow(regout)),
  ""
)

# Write text files
writeLines(txt_lines, file.path(out, "P046.txt"))
writeLines(txt_lines, file.path(out, "P046.lst.txt"))
writeLines(txt_lines, file.path(out, "P046.lst"))

# Build CSV of table body (same numbers)
# ANOVA table
anova_csv <- tibble(
  Source = c("Model","Error","Corrected Total"),
  DF = c(df_model, df_error, df_total),
  SumOfSquares = c(ss_model, ss_error, ss_total),
  MeanSquare = c(ms_model, ms_error, NA_real_),
  FValue = c(f_value, NA_real_, NA_real_),
  ProbF = c(p_value, NA_real_, NA_real_)
)
fit_csv <- tibble(
  Statistic = c("Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq"),
  Value = c(root_mse, dep_mean, coeff_var, r_square, adj_r_square)
)
param_csv <- param_est %>%
  rename(ParameterEstimate = Estimate, StandardError = StdErr, tValue = tValue, Probt = pValue)

# Combined CSV for P046
p046_csv <- bind_rows(
  tibble(Table = "ANOVA", Variable = anova_csv$Source, DF = anova_csv$DF,
         SumOfSquares = anova_csv$SumOfSquares, MeanSquare = anova_csv$MeanSquare,
         FValue = anova_csv$FValue, ProbF = anova_csv$ProbF,
         Estimate = NA_real_, StdErr = NA_real_, tValue = NA_real_, Probt = NA_real_),
  tibble(Table = "FitStat", Variable = fit_csv$Statistic, DF = NA_integer_,
         SumOfSquares = NA_real_, MeanSquare = NA_real_, FValue = NA_real_, ProbF = NA_real_,
         Estimate = fit_csv$Value, StdErr = NA_real_, tValue = NA_real_, Probt = NA_real_),
  tibble(Table = "ParameterEstimates", Variable = param_csv$Variable, DF = param_csv$DF,
         SumOfSquares = NA_real_, MeanSquare = NA_real_, FValue = NA_real_, ProbF = NA_real_,
         Estimate = param_csv$ParameterEstimate, StdErr = param_csv$StandardError,
         tValue = param_csv$tValue, Probt = param_csv$pValue)
)
# Add Nobs info as rows
nobs_csv <- tibble(Table = "NObs", Variable = c("NObs Read","NObs Used"), DF = c(n_obs_read, n_obs_used),
                   SumOfSquares = NA_real_, MeanSquare = NA_real_, FValue = NA_real_, ProbF = NA_real_,
                   Estimate = NA_real_, StdErr = NA_real_, tValue = NA_real_, Probt = NA_real_)
p046_csv <- bind_rows(nobs_csv, p046_csv)

write.csv(p046_csv, file.path(out, "P046.csv"), row.names = FALSE, na = "")
write.csv(p046_csv, file.path(out, "P046.lst.csv"), row.names = FALSE, na = "")
write.csv(anova_csv, file.path(out, "P046_anova.csv"), row.names = FALSE, na = "")
write.csv(param_csv, file.path(out, "P046_params.csv"), row.names = FALSE, na = "")

cat("Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
