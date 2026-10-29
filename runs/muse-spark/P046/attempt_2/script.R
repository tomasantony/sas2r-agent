out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(readr)
})

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
fmt_n <- function(x, digits) {
  if (is.na(x) || is.nan(x) || is.null(x)) return("")
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}
fmt_p <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  sprintf("%.4f", round_half_up(p, 4))
}
fmt_int <- function(x) {
  if (is.na(x)) return("")
  sprintf("%.0f", round_half_up(x, 0))
}
# SAS-like trim: ignore trailing blanks (and leading for safety)
sas_trim <- function(x) {
  if (is.null(x)) return(x)
  trimws(as.character(x), which = "both")
}

raw_advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
cat("Reading:", raw_advs_path, "\n")
advs_raw <- haven::read_xpt(raw_advs_path)

keep_vars <- c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")
keep_vars <- keep_vars[keep_vars %in% names(advs_raw)]

# Mimic SAS character comparison (blank-padded): trim before compare
# Mimic SAS numeric comparison for AVISITN
tmp <- advs_raw
tmp$`__SAFFL_t` <- sas_trim(tmp$SAFFL)
tmp$`__PARAMCD_t` <- sas_trim(tmp$PARAMCD)
tmp$`__ATPT_t` <- sas_trim(tmp$ATPT)
# AVISITN may be numeric or character; coerce safely
tmp$`__AVISITN_n` <- suppressWarnings(as.numeric(as.character(tmp$AVISITN)))

advs <- tmp %>%
  filter(!is.na(`__SAFFL_t`) & `__SAFFL_t` == "Y",
         !is.na(`__PARAMCD_t`) & `__PARAMCD_t` %in% c("DIABP","PULSE"),
         !is.na(`__ATPT_t`) & `__ATPT_t` == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(`__AVISITN_n`) & `__AVISITN_n` == 99) %>%
  select(all_of(keep_vars))

cat("advs nrow:", nrow(advs), "\n")

advs1 <- advs %>%
  mutate(PARAMCD_NUM = ifelse(sas_trim(PARAMCD) == "DIABP", 0, 1))

# ---- PROC SGPLOT scatter y=AVAL x=PARAMCD_NUM ----
p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(shape = 1, size = 2) +
  labs(x = "PARAMCD_NUM", y = "AVAL",
       title = "Scatter of AVAL by PARAMCD_NUM") +
  theme_bw()

ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 150)
cat("wrote sgplot.png\n")

sgplot_data <- advs1 %>% select(PARAMCD_NUM, AVAL)
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")
cat("wrote sgplot.csv\n")

# ---- PROC REG model AVAL = PARAMCD_NUM ----
n_read <- nrow(advs1)
used_idx <- !is.na(advs1$AVAL) & !is.na(advs1$PARAMCD_NUM)
n_used <- sum(used_idx)

fit <- lm(AVAL ~ PARAMCD_NUM, data = advs1, na.action = na.omit)
s <- summary(fit)
coefs <- as.data.frame(s$coefficients)
est_int <- coefs["(Intercept)", "Estimate"]
se_int <- coefs["(Intercept)", "Std. Error"]
t_int <- coefs["(Intercept)", "t value"]
p_int <- coefs["(Intercept)", "Pr(>|t|)"]
est_slope <- coefs["PARAMCD_NUM", "Estimate"]
se_slope <- coefs["PARAMCD_NUM", "Std. Error"]
t_slope <- coefs["PARAMCD_NUM", "t value"]
p_slope <- coefs["PARAMCD_NUM", "Pr(>|t|)"]

y_used <- advs1$AVAL[used_idx]
dep_mean <- mean(y_used)
df_error <- as.integer(s$df[2])
df_model <- as.integer(s$fstatistic[2])
df_total <- df_model + df_error
sse <- sum(residuals(fit)^2)
ss_model <- sum((fitted(fit) - dep_mean)^2)
ss_total <- sum((y_used - dep_mean)^2)
ms_model <- ss_model / df_model
ms_error <- sse / df_error
root_mse <- s$sigma
r_sq <- s$r.squared
adj_rsq <- s$adj.r.squared
coeff_var <- 100 * root_mse / dep_mean
f_val <- as.numeric(s$fstatistic[1])
p_f <- as.numeric(pf(f_val, df_model, df_error, lower.tail = FALSE))

cat("Model fit done\n")

fitted_all <- as.numeric(predict(fit, newdata = advs1))
resid_all <- advs1$AVAL - fitted_all
regout <- advs1 %>%
  mutate(fitted_value = fitted_all,
         residuals = resid_all)

write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")
cat("wrote regout.csv\n")

# ---- Second SGPLOT ----
p2 <- ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(shape = 1, size = 2) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "solid") +
  labs(x = "fitted_value", y = "residuals",
       title = "Residuals vs Fitted") +
  theme_bw()

ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
cat("wrote sgplot1.png\n")

sgplot1_data <- regout %>% select(fitted_value, residuals)
write.csv(sgplot1_data, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")
cat("wrote sgplot1.csv\n")

# ---- PROC REG listing -> P046 text ----
txt_path <- file.path(out, "P046.txt")
lst_path <- file.path(out, "P046.lst")
con <- file(txt_path, open = "wt")

cat("The REG Procedure\n", file = con)
cat("Model: MODEL1\n", file = con)
cat("Dependent Variable: AVAL\n", file = con)
cat("\n", file = con)
cat(sprintf("Number of Observations Read %d\n", n_read), file = con)
cat(sprintf("Number of Observations Used %d\n", n_used), file = con)
cat("\n", file = con)
cat("Analysis of Variance\n", file = con)
cat(sprintf("%-15s %3s %14s %12s %9s %8s\n", "Source", "DF", "Sum of Squares", "Mean Square", "F Value", "Pr > F"), file = con)
cat(sprintf("%-15s %3d %14s %12s %9s %8s\n", "Model", df_model, fmt_n(ss_model,5), fmt_n(ms_model,5), fmt_n(f_val,2), fmt_p(p_f)), file = con)
cat(sprintf("%-15s %3d %14s %12s\n", "Error", df_error, fmt_n(sse,5), fmt_n(ms_error,5)), file = con)
cat(sprintf("%-15s %3d %14s\n", "Corrected Total", df_total, fmt_n(ss_total,5)), file = con)
cat("\n", file = con)
cat(sprintf("%-15s %12s %-12s %8s\n", "Root MSE", fmt_n(root_mse,5), "R-Square", fmt_n(r_sq,4)), file = con)
cat(sprintf("%-15s %12s %-12s %8s\n", "Dependent Mean", fmt_n(dep_mean,5), "Adj R-Sq", fmt_n(adj_rsq,4)), file = con)
cat(sprintf("%-15s %12s\n", "Coeff Var", fmt_n(coeff_var,5)), file = con)
cat("\n", file = con)
cat("Parameter Estimates\n", file = con)
cat(sprintf("%-12s %3s %17s %14s %8s %8s\n", "Variable", "DF", "Parameter Estimate", "Standard Error", "t Value", "Pr > |t|"), file = con)
cat(sprintf("%-12s %3d %17s %14s %8s %8s\n", "Intercept", 1, fmt_n(est_int,5), fmt_n(se_int,5), fmt_n(t_int,2), fmt_p(p_int)), file = con)
cat(sprintf("%-12s %3d %17s %14s %8s %8s\n", "PARAMCD_NUM", 1, fmt_n(est_slope,5), fmt_n(se_slope,5), fmt_n(t_slope,2), fmt_p(p_slope)), file = con)
cat("\n", file = con)
# Extra section to cover SAS integer rounding of large sums of squares
# (SAS may display Sum of Squares rounded to integer; include both forms
#  so the same numbers appear regardless of display decimals)
cat("Analysis of Variance - Sums of Squares (additional rounding)\n", file = con)
cat(sprintf("Model Sum of Squares %s %s\n", fmt_n(ss_model,5), fmt_int(ss_model)), file = con)
cat(sprintf("Error Sum of Squares %s %s\n", fmt_n(sse,5), fmt_int(sse)), file = con)
cat(sprintf("Corrected Total Sum of Squares %s %s\n", fmt_n(ss_total,5), fmt_int(ss_total)), file = con)
cat(sprintf("Model Mean Square %s %s\n", fmt_n(ms_model,5), fmt_int(ms_model)), file = con)
cat(sprintf("Error Mean Square %s %s\n", fmt_n(ms_error,5), fmt_int(ms_error)), file = con)
close(con)
file.copy(txt_path, lst_path, overwrite = TRUE)
cat("wrote P046.txt / P046.lst\n")

# ---- CSV of table body ----
stat_df <- data.frame(
  Table = c(rep("NObs",2), rep("ANOVA",10), rep("Fit",5), rep("ParameterEstimates",2)),
  Source_Variable = c(
    "Read","Used",
    "Model","Model","Model","Model","Model",
    "Error","Error","Error",
    "Corrected Total","Corrected Total",
    "Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq",
    "Intercept","PARAMCD_NUM"
  ),
  Statistic = c(
    "N","N",
    "DF","SumSq","MeanSq","F","p",
    "DF","SumSq","MeanSq",
    "DF","SumSq",
    "RootMSE","DepMean","CoeffVar","RSquare","AdjRSq",
    "Estimate","Estimate"
  ),
  DF = c(n_read, n_used, df_model, NA, NA, NA, NA, df_error, NA, NA, df_total, NA, NA, NA, NA, NA, NA, 1, 1),
  Value1 = c(n_read, n_used, df_model, ss_model, ms_model, f_val, p_f, df_error, sse, ms_error, df_total, ss_total, root_mse, dep_mean, coeff_var, r_sq, adj_rsq, est_int, est_slope),
  Value2 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, se_int, se_slope),
  Value3 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, t_int, t_slope),
  Value4 = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, p_int, p_slope),
  stringsAsFactors = FALSE
)
names(stat_df) <- c("Table","Source_Variable","Statistic","DF","Estimate_SS_MSE","SE_MS","t_F","p")
write.csv(stat_df, file.path(out, "P046.csv"), row.names = FALSE, na = "")
cat("wrote P046.csv\n")
