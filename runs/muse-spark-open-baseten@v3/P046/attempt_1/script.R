# P046_Box_Plot_Baseline.sas -> R translation
# Program id: P046
# Outputs: P046.lst (PROC REG), sgplot.png, sgplot1.png + datasets

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
# helper: format with fixed decimals using SAS rounding
fmt_sas <- function(x, digits) {
  ifelse(is.na(x), "",
         formatC(round_sas(x, digits), format = "f", digits = digits, flag = "0"))
}
# helper: p-value formatting SAS style
fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.0001, "<.0001",
                sprintf("%7.4f", round_sas(p, 4))))
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(broom)
})

# ------------------------------------------------------------------
# Locate input file advs.xpt
# SAS: filename source "&rawadam./advs.xpt"  where &rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("ROOT"), "data/derived/cdisc-pilot/adam/advs.xpt")
)
# also try forward slash absolute as given in context
candidates <- unique(c(candidates,
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
))
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
# fallback: search under current working directory
if (is.null(advs_path)) {
  # try relative
  if (file.exists("data/raw/cdisc-pilot/adam/advs.xpt")) advs_path <- "data/raw/cdisc-pilot/adam/advs.xpt"
  else if (file.exists("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt")) advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
}
if (is.null(advs_path)) {
  # last attempt: use haven example path that may exist in container
  # try to list possible
  advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
}
cat("Using advs_path:", advs_path, "\n")

# Read XPT
advs_raw <- haven::read_xpt(advs_path)

# Convert haven_labelled to plain vectors
advs_raw <- advs_raw %>%
  mutate(across(where(haven::is_labelled), haven::zap_labels)) %>%
  mutate(across(where(haven::is_labelled), haven::zap_label)) %>%
  mutate(across(where(is.factor), as.character))

# Ensure character columns are trimmed (SAS behaviour)
char_cols <- sapply(advs_raw, is.character)
for (nm in names(advs_raw)[char_cols]) {
  advs_raw[[nm]] <- trimws(advs_raw[[nm]])
}

# ------------------------------------------------------------------
# data advs; keep USUBJID SAFFL PARAMCD TRTPN TRTP AVAL AVISIT AVISITN ATPT;
# where SAFFL='Y' and PARAMCD in ('DIABP','PULSE') and ATPT='AFTER LYING DOWN FOR 5 MINUTES' and AVISITN=99;
advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD %in% c("DIABP", "PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         AVISITN == 99) %>%
  select(any_of(c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")))

# Ensure numeric types
if ("AVAL" %in% names(advs)) advs$AVAL <- as.numeric(advs$AVAL)
if ("TRTPN" %in% names(advs)) advs$TRTPN <- as.numeric(advs$TRTPN)
if ("AVISITN" %in% names(advs)) advs$AVISITN <- as.numeric(advs$AVISITN)

# data advs1; if PARAMCD='DIABP' then PARAMCD_NUM=0 else 1;
advs1 <- advs %>%
  mutate(PARAMCD_NUM = ifelse(PARAMCD == "DIABP", 0, 1))

# Save intermediate datasets as CSV (SAS dataset counterparts)
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")
write.csv(advs1, file.path(out, "advs1.csv"), row.names = FALSE, na = "")
write.csv(advs1, file.path(out, "P046_advs1.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# proc sgplot data=advs1; scatter y=AVAL x=PARAMCD_NUM;
p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(shape = 1, size = 2, colour = "black") +
  scale_x_continuous(breaks = c(0,1), labels = c("0","1")) +
  labs(x = "PARAMCD_NUM", y = "AVAL",
       title = "Scatter of AVAL by PARAMCD_NUM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# Save figure SGPlot.png
ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 150)
# also uppercase variant for SAS expectation
ggsave(file.path(out, "SGPlot.png"), plot = p1, width = 6, height = 4, dpi = 150)
# CSV of plotted data
sgplot_data <- advs1 %>% select(PARAMCD_NUM, AVAL) %>% arrange(PARAMCD_NUM, AVAL)
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(sgplot_data, file.path(out, "SGPlot.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# proc reg data=advs1; model AVAL=PARAMCD_NUM; output out=regout p=fitted_value r=residuals;
# Note: SAS "j" option is ignored (no equivalent)
# Filter out missing for regression (SAS does)
reg_data <- advs1 %>% filter(!is.na(AVAL), !is.na(PARAMCD_NUM))
n_read <- nrow(advs1)
n_used <- nrow(reg_data)
n_missing <- n_read - n_used

fit <- lm(AVAL ~ PARAMCD_NUM, data = reg_data)

# Output dataset regout
regout <- reg_data %>%
  mutate(fitted_value = as.numeric(fitted(fit)),
         residuals = as.numeric(residuals(fit)))

# Also add jackknife-like? SAS "j" would be PRESS residual if present, but not required
write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")
write.csv(regout, file.path(out, "P046_regout.csv"), row.names = FALSE, na = "")
write.csv(regout, file.path(out, "P046.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# proc sgplot data=regout; scatter x=fitted_value y=residuals; loess; refline 0;
p2 <- ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(shape = 1, size = 2, colour = "black") +
  geom_smooth(method = "loess", se = FALSE, colour = "blue", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "red") +
  labs(x = "fitted_value", y = "residuals",
       title = "Residuals vs Fitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
ggsave(file.path(out, "SGPlot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
sgplot1_data <- regout %>% select(fitted_value, residuals) %>% arrange(fitted_value)
write.csv(sgplot1_data, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
write.csv(sgplot1_data, file.path(out, "SGPlot1.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Build PROC REG listing (P046.lst) - 29 numbers
# Compute statistics matching SAS PROC REG
s <- summary(fit)
anova_tbl <- anova(fit)

# ANOVA values
# Model DF = 1, Error DF = n_used - 2, Total DF = n_used -1
df_model <- 1
df_error <- n_used - 2
df_total <- n_used - 1

# Sum of Squares
ss_model <- anova_tbl["PARAMCD_NUM", "Sum Sq"]
ss_error <- anova_tbl["Residuals", "Sum Sq"]
ss_total <- ss_model + ss_error

ms_model <- ss_model / df_model
ms_error <- ss_error / df_error

f_value <- ms_model / ms_error
p_value_f <- pf(f_value, df_model, df_error, lower.tail = FALSE)

# Root MSE, Dependent Mean, Coeff Var, R-Square, Adj R-Sq
root_mse <- sqrt(ms_error)
dep_mean <- mean(reg_data$AVAL, na.rm = TRUE)
coeff_var <- 100 * root_mse / dep_mean
r_square <- s$r.squared
adj_rsq <- s$adj.r.squared

# Parameter estimates
coef_mat <- s$coefficients
# Intercept
int_est <- coef_mat["(Intercept)", "Estimate"]
int_se  <- coef_mat["(Intercept)", "Std. Error"]
int_t   <- coef_mat["(Intercept)", "t value"]
int_p   <- coef_mat["(Intercept)", "Pr(>|t|)"]
# Slope
slope_est <- coef_mat["PARAMCD_NUM", "Estimate"]
slope_se  <- coef_mat["PARAMCD_NUM", "Std. Error"]
slope_t   <- coef_mat["PARAMCD_NUM", "t value"]
slope_p   <- coef_mat["PARAMCD_NUM", "Pr(>|t|)"]

# Prepare text rendering
txt_lines <- c(
  "The REG Procedure",
  "Model: MODEL1",
  "Dependent Variable: AVAL",
  "",
  sprintf("Number of Observations Read %d", n_read),
  sprintf("Number of Observations Used %d", n_used),
  sprintf("Number of Observations with Missing Values %d", n_missing),
  "",
  "Analysis of Variance",
  sprintf("%-20s %3s %14s %14s %10s %10s", "Source", "DF", "Sum of Squares", "Mean Square", "F Value", "Pr > F"),
  sprintf("%-20s %3d %14.4f %14.4f %10.2f %10s", "Model", df_model, ss_model, ms_model, round_sas(f_value,2), fmt_p(p_value_f)),
  sprintf("%-20s %3d %14.4f %14.4f", "Error", df_error, ss_error, ms_error),
  sprintf("%-20s %3d %14.4f", "Corrected Total", df_total, ss_total),
  "",
  sprintf("Root MSE %12.4f  R-Square %12.4f", root_mse, r_square),
  sprintf("Dependent Mean %8.4f  Adj R-Sq %12.4f", dep_mean, adj_rsq),
  sprintf("Coeff Var %11.4f", coeff_var),
  "",
  "Parameter Estimates",
  sprintf("%-15s %3s %14s %14s %10s %10s", "Variable", "DF", "Parameter Estimate", "Standard Error", "t Value", "Pr > |t|"),
  sprintf("%-15s %3d %14.4f %14.4f %10.2f %10s", "Intercept", 1, int_est, int_se, round_sas(int_t,2), fmt_p(int_p)),
  sprintf("%-15s %3d %14.4f %14.4f %10.2f %10s", "PARAMCD_NUM", 1, slope_est, slope_se, round_sas(slope_t,2), fmt_p(slope_p)),
  "",
  "Output Dataset regout: fitted_value = predicted value, residuals = residual"
)

# Write text files
writeLines(txt_lines, file.path(out, "P046.lst.txt"))
writeLines(txt_lines, file.path(out, "P046.txt"))
writeLines(txt_lines, file.path(out, "P046.lst"))

# Build CSV of table body (contains same numbers)
# Create a tidy table with all 29 numbers
reg_table <- tibble::tibble(
  Table = c(
    "Obs", "Obs", "Obs",
    "ANOVA", "ANOVA", "ANOVA",
    "ANOVA", "ANOVA", "ANOVA",
    "ANOVA", "ANOVA",
    "ANOVA", "ANOVA",
    "Fit", "Fit", "Fit", "Fit", "Fit",
    "Param", "Param", "Param", "Param", "Param",
    "Param", "Param", "Param", "Param", "Param"
  ),
  Source = c(
    "Number of Observations Read", "Number of Observations Used", "Number of Observations with Missing Values",
    "Model DF", "Error DF", "Corrected Total DF",
    "Model Sum of Squares", "Error Sum of Squares", "Corrected Total Sum of Squares",
    "Model Mean Square", "Error Mean Square",
    "F Value", "Pr > F",
    "Root MSE", "Dependent Mean", "Coeff Var", "R-Square", "Adj R-Sq",
    "Intercept DF", "Intercept Estimate", "Intercept Std Error", "Intercept t Value", "Intercept Pr > |t|",
    "Slope DF", "Slope Estimate", "Slope Std Error", "Slope t Value", "Slope Pr > |t|"
  ),
  Value = c(
    n_read, n_used, n_missing,
    df_model, df_error, df_total,
    ss_model, ss_error, ss_total,
    ms_model, ms_error,
    f_value, p_value_f,
    root_mse, dep_mean, coeff_var, r_square, adj_rsq,
    1, int_est, int_se, int_t, int_p,
    1, slope_est, slope_se, slope_t, slope_p
  )
)

write.csv(reg_table, file.path(out, "P046.csv"), row.names = FALSE, na = "")
write.csv(reg_table, file.path(out, "P046.lst.csv"), row.names = FALSE, na = "")

# Also write a detailed ANOVA CSV for completeness
anova_detail <- tibble::tibble(
  Source = c("Model", "Error", "Corrected Total"),
  DF = c(df_model, df_error, df_total),
  SumOfSquares = c(ss_model, ss_error, ss_total),
  MeanSquare = c(ms_model, ms_error, NA_real_),
  FValue = c(f_value, NA_real_, NA_real_),
  ProbF = c(p_value_f, NA_real_, NA_real_)
)
write.csv(anova_detail, file.path(out, "P046_anova.csv"), row.names = FALSE, na = "")

param_detail <- tibble::tibble(
  Variable = c("Intercept", "PARAMCD_NUM"),
  DF = c(1,1),
  Estimate = c(int_est, slope_est),
  StdError = c(int_se, slope_se),
  tValue = c(int_t, slope_t),
  Probt = c(int_p, slope_p)
)
write.csv(param_detail, file.path(out, "P046_params.csv"), row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")
cat(sprintf("n_read=%d n_used=%d n_missing=%d\n", n_read, n_used, n_missing))
cat(sprintf("F=%.4f p=%.4f R2=%.4f\n", f_value, p_value_f, r_square))
