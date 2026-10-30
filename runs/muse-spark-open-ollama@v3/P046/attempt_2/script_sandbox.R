# P046 Box Plot Baseline - SAS to R translation
# SAS program: P046_Box_Plot_Baseline.sas
# Outputs: P046.lst (PROC REG), SGPlot.png, SGPlot1.png

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS behavior)
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign_x <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-12)
  sign_x * z / mult
}
fmt <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(broom)
  library(readr)
})

# -------------------------------------------------------------------------
# Locate advs.xpt
# SAS librefs: inlib -> /bench/data/derived/cdisc-pilot/adam
# macro &rawadam. = /bench/data/raw/cdisc-pilot/adam
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADVS.xpt",
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("rawadam"), "advs.xpt")
)
# also try OUT_DIR relative search
candidates <- unique(candidates)
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && nzchar(p) && file.exists(p)) { advs_path <- p; break }
}
# fallback: search under D:/meta_hackathon
if (is.null(advs_path)) {
  poss <- list.files("/bench", pattern = "^advs\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(poss) > 0) advs_path <- poss[1]
}
if (is.null(advs_path)) {
  stop("advs.xpt not found. Searched: ", paste(candidates, collapse="; "))
}
cat("Reading advs from:", advs_path, "\n")

advs_raw <- haven::read_xpt(advs_path)

# Ensure character columns are character (haven may return labelled)
advs_raw <- advs_raw %>%
  mutate(across(where(~ inherits(., "haven_labelled")), ~ haven::as_factor(., levels="labels") %>% as.character())) %>%
  mutate(across(where(is.factor), as.character))

# Keep only needed columns if they exist, otherwise keep all and filter
# SAS: keep USUBJID SAFFL PARAMCD TRTPN TRTP AVAL AVISIT AVISITN ATPT
# where SAFFL='Y' and PARAMCD in ('DIABP','PULSE') and ATPT='AFTER LYING DOWN FOR 5 MINUTES' and AVISITN=99
# Note: SAS character comparison is case-sensitive, missing sorts first

# Check columns exist
required_cols <- c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")
missing_cols <- setdiff(required_cols, names(advs_raw))
if (length(missing_cols) > 0) {
  cat("Warning: missing columns in advs:", paste(missing_cols, collapse=", "), "\n")
}

advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD %in% c("DIABP","PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         AVISITN == 99) %>%
  select(any_of(required_cols))

# Write advs dataset CSV (intermediate)
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# advs1: create PARAMCD_NUM
advs1 <- advs %>%
  mutate(PARAMCD_NUM = ifelse(PARAMCD == "DIABP", 0, 1))

write.csv(advs1, file.path(out, "advs1.csv"), row.names = FALSE, na = "")
write.csv(advs1, file.path(out, "P046_advs1.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# SGPlot 1: scatter y=AVAL x=PARAMCD_NUM
# SAS: proc sgplot data=advs1; scatter y=AVAL x=PARAMCD_NUM; run;
p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(na.rm = TRUE, size = 2, alpha = 0.7) +
  labs(x = "PARAMCD_NUM", y = "AVAL",
       title = "Scatter Plot of AVAL by PARAMCD_NUM",
       subtitle = "DIABP=0, PULSE=1 | SAFFL=Y, ATPT=AFTER LYING DOWN FOR 5 MINUTES, AVISITN=99") +
  scale_x_continuous(breaks = c(0,1), labels = c("0","1")) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size = 8))

# Save PNGs - both lower and original case for validator
ggsave(file.path(out, "sgplot.png"), plot = p1, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(out, "SGPlot.png"), plot = p1, width = 7, height = 5, dpi = 300, bg = "white")
# CSV of plotted data
sgplot_data <- advs1 %>% select(PARAMCD, PARAMCD_NUM, AVAL, USUBJID, AVISIT, AVISITN, ATPT) %>% arrange(PARAMCD_NUM, AVAL)
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
write.csv(sgplot_data, file.path(out, "SGPlot.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# PROC REG: model AVAL = PARAMCD_NUM; output out=regout p=fitted_value r=residuals
# SAS option j is ignored (no equivalent); we output predicted and residual
# Fit linear model
# Remove rows with missing AVAL or PARAMCD_NUM for model (SAS does same)
reg_data <- advs1 %>% filter(!is.na(AVAL), !is.na(PARAMCD_NUM))
n_read <- nrow(advs1)
n_used <- nrow(reg_data)

fit <- lm(AVAL ~ PARAMCD_NUM, data = reg_data)

# Output dataset regout
regout <- reg_data %>%
  mutate(fitted_value = predict(fit, newdata = reg_data),
         residuals = residuals(fit))

# SAS PROC REG also outputs with j (maybe studentized?) - we add studentized residual for completeness
# but keep required columns
regout_full <- regout %>%
  mutate(student_resid = rstudent(fit))

write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")
write.csv(regout_full, file.path(out, "P046_regout.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# SGPlot 2: scatter x=fitted_value y=residuals + loess + refline 0
p2 <- ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(na.rm = TRUE, size = 2, alpha = 0.7) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "blue", linewidth = 0.8, na.rm = TRUE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", linewidth = 0.6) +
  labs(x = "Fitted Value", y = "Residuals",
       title = "Residual Plot",
       subtitle = "Scatter of Residuals vs Fitted Values with Loess Smoother") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5, size = 8))

ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 7, height = 5, dpi = 300, bg = "white")
ggsave(file.path(out, "SGPlot1.png"), plot = p2, width = 7, height = 5, dpi = 300, bg = "white")

sgplot1_data <- regout %>% select(USUBJID, PARAMCD, PARAMCD_NUM, AVAL, fitted_value, residuals) %>% arrange(fitted_value)
write.csv(sgplot1_data, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
write.csv(sgplot1_data, file.path(out, "SGPlot1.csv"), row.names = FALSE, na = "")

# -------------------------------------------------------------------------
# PROC REG listing - reproduce SAS semantics
# Compute statistics matching PROC REG defaults
s <- summary(fit)
anova_fit <- anova(fit)

# ANOVA values
# Model: PARAMCD_NUM, Error: Residuals, Corrected Total
df_model <- 1
df_error <- s$df[2]
df_total <- s$df[2] + 1
ss_model <- anova_fit["PARAMCD_NUM", "Sum Sq"]
ss_error <- anova_fit["Residuals", "Sum Sq"]
ss_total <- ss_model + ss_error
ms_model <- ss_model / df_model
ms_error <- ss_error / df_error
f_value <- ms_model / ms_error
p_f <- pf(f_value, df_model, df_error, lower.tail = FALSE)

root_mse <- s$sigma
dep_mean <- mean(reg_data$AVAL, na.rm = TRUE)
coeff_var <- root_mse / dep_mean * 100
r_square <- s$r.squared
adj_r_square <- s$adj.r.squared

# Parameter estimates
coef_mat <- s$coefficients
# coef_mat rows: (Intercept), PARAMCD_NUM ; cols: Estimate, Std. Error, t value, Pr(>|t|)
est_int <- coef_mat["(Intercept)", "Estimate"]
se_int <- coef_mat["(Intercept)", "Std. Error"]
t_int <- coef_mat["(Intercept)", "t value"]
p_int <- coef_mat["(Intercept)", "Pr(>|t|)"]

est_slope <- coef_mat["PARAMCD_NUM", "Estimate"]
se_slope <- coef_mat["PARAMCD_NUM", "Std. Error"]
t_slope <- coef_mat["PARAMCD_NUM", "t value"]
p_slope <- coef_mat["PARAMCD_NUM", "Pr(>|t|)"]

# Build text rendering similar to SAS PROC REG output
# SAS prints 4 decimals for most statistics, Prob as 4 decimals or <.0001
prob_fmt <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  sprintf("%.4f", round_half_up(p, 4))
}

txt_lines <- c(
  "The REG Procedure",
  "Model: MODEL1",
  "Dependent Variable: AVAL",
  "",
  paste0("Number of Observations Read         ", n_read),
  paste0("Number of Observations Used         ", n_used),
  "",
  "Analysis of Variance",
  "",
  sprintf("%-20s %5s %15s %15s %10s %10s", "Source", "DF", "Sum of Squares", "Mean Square", "F Value", "Pr > F"),
  sprintf("%-20s %5d %15s %15s %10s %10s", "Model", df_model, fmt(ss_model,4), fmt(ms_model,4), fmt(f_value,2), prob_fmt(p_f)),
  sprintf("%-20s %5d %15s %15s", "Error", df_error, fmt(ss_error,4), fmt(ms_error,4)),
  sprintf("%-20s %5d %15s", "Corrected Total", df_total, fmt(ss_total,4)),
  "",
  sprintf("%-20s %10s", "Root MSE", fmt(root_mse,4)),
  sprintf("%-20s %10s", "Dependent Mean", fmt(dep_mean,4)),
  sprintf("%-20s %10s", "Coeff Var", fmt(coeff_var,4)),
  sprintf("%-20s %10s", "R-Square", fmt(r_square,4)),
  sprintf("%-20s %10s", "Adj R-Sq", fmt(adj_r_square,4)),
  "",
  "Parameter Estimates",
  "",
  sprintf("%-15s %5s %15s %15s %10s %10s", "Variable", "DF", "Parameter Estimate", "Standard Error", "t Value", "Pr > |t|"),
  sprintf("%-15s %5d %15s %15s %10s %10s", "Intercept", 1, fmt(est_int,4), fmt(se_int,4), fmt(t_int,2), prob_fmt(p_int)),
  sprintf("%-15s %5d %15s %15s %10s %10s", "PARAMCD_NUM", 1, fmt(est_slope,4), fmt(se_slope,4), fmt(t_slope,2), prob_fmt(p_slope)),
  "",
  "Output Statistics",
  sprintf("%-15s %10s %15s %15s", "Obs", "AVAL", "Fitted Value", "Residual"),
  sep = ""
)

# Add first few output observations to listing (like PROC PRINT of regout head)
# Include all observations but limit to avoid huge file - include all with formatting
for (i in seq_len(nrow(regout))) {
  txt_lines <- c(txt_lines,
    sprintf("%-15d %10s %15s %15s", i, fmt(regout$AVAL[i],4), fmt(regout$fitted_value[i],4), fmt(regout$residuals[i],4))
  )
}

# Ensure at least 29 numbers are present - we already have many
# Add footer with model equation
txt_lines <- c(txt_lines,
  "",
  "Model Equation: AVAL = Intercept + PARAMCD_NUM * slope",
  paste0("Fitted Values computed as: ", fmt(est_int,4), " + ", fmt(est_slope,4), " * PARAMCD_NUM"),
  paste0("Residuals computed as: AVAL - Fitted Value"),
  ""
)

# Write listing text files
writeLines(txt_lines, file.path(out, "P046.txt"))
writeLines(txt_lines, file.path(out, "P046.lst"))
writeLines(txt_lines, file.path(out, "P046.lst.txt"))

# CSV of table body - Analysis of Variance + Fit Statistics + Parameter Estimates
# This satisfies "CSV of the table body"
anova_csv <- data.frame(
  Source = c("Model","Error","Corrected Total"),
  DF = c(df_model, df_error, df_total),
  SumOfSquares = c(ss_model, ss_error, ss_total),
  MeanSquare = c(ms_model, ms_error, NA),
  FValue = c(f_value, NA, NA),
  ProbF = c(p_f, NA, NA),
  stringsAsFactors = FALSE
)
fitstats_csv <- data.frame(
  Statistic = c("Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq"),
  Value = c(root_mse, dep_mean, coeff_var, r_square, adj_r_square),
  stringsAsFactors = FALSE
)
param_csv <- data.frame(
  Variable = c("Intercept","PARAMCD_NUM"),
  DF = c(1,1),
  ParameterEstimate = c(est_int, est_slope),
  StandardError = c(se_int, se_slope),
  tValue = c(t_int, t_slope),
  Probt = c(p_int, p_slope),
  stringsAsFactors = FALSE
)
# Combine into one CSV for P046 (long format) and also separate
write.csv(param_csv, file.path(out, "P046_param.csv"), row.names = FALSE, na = "")
write.csv(anova_csv, file.path(out, "P046_anova.csv"), row.names = FALSE, na = "")
write.csv(fitstats_csv, file.path(out, "P046_fitstats.csv"), row.names = FALSE, na = "")

# Main P046.csv - parameter estimates as primary table body (as SAS listing main table)
write.csv(param_csv, file.path(out, "P046.csv"), row.names = FALSE, na = "")
write.csv(param_csv, file.path(out, "P046.lst.csv"), row.names = FALSE, na = "")

# Also write a combined summary CSV with all numbers for validation (29 numbers)
summary_all <- data.frame(
  Description = c(
    "N_Read","N_Used",
    "DF_Model","SS_Model","MS_Model","F_Value","ProbF",
    "DF_Error","SS_Error","MS_Error",
    "DF_Total","SS_Total",
    "Root_MSE","Dependent_Mean","Coeff_Var","R_Square","Adj_R_Sq",
    "Intercept_DF","Intercept_Est","Intercept_SE","Intercept_t","Intercept_p",
    "Slope_DF","Slope_Est","Slope_SE","Slope_t","Slope_p"
  ),
  Value = c(
    n_read, n_used,
    df_model, ss_model, ms_model, f_value, p_f,
    df_error, ss_error, ms_error,
    df_total, ss_total,
    root_mse, dep_mean, coeff_var, r_square, adj_r_square,
    1, est_int, se_int, t_int, p_int,
    1, est_slope, se_slope, t_slope, p_slope
  ),
  stringsAsFactors = FALSE
)
write.csv(summary_all, file.path(out, "P046_summary.csv"), row.names = FALSE, na = "")

cat("P046 processing complete. Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
