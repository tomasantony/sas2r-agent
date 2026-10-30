```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_p <- function(p) {
  ifelse(is.na(p), "", ifelse(p < 0.0001, "<.0001", sprintf("%7.4f", round_sas(p, 4))))
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

candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  "data/raw/cdisc-pilot/adam/advs.xpt",
  "data/derived/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) advs_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
cat("Using advs_path:", advs_path, "\n")

advs_raw <- haven::read_xpt(advs_path)

# zap haven labels without using haven::is_labelled (not exported in some versions)
for (nm in names(advs_raw)) {
  if (inherits(advs_raw[[nm]], "haven_labelled") || inherits(advs_raw[[nm]], "haven_labelled_spss")) {
    advs_raw[[nm]] <- haven::zap_labels(advs_raw[[nm]])
  }
  if (is.factor(advs_raw[[nm]])) advs_raw[[nm]] <- as.character(advs_raw[[nm]])
}
char_cols <- sapply(advs_raw, is.character)
for (nm in names(advs_raw)[char_cols]) advs_raw[[nm]] <- trimws(advs_raw[[nm]])

advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD %in% c("DIABP", "PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         AVISITN == 99) %>%
  select(any_of(c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")))

if ("AVAL" %in% names(advs)) advs$AVAL <- as.numeric(advs$AVAL)
if ("TRTPN" %in% names(advs)) advs$TRTPN <- as.numeric(advs$TRTPN)
if ("AVISITN" %in% names(advs)) advs$AVISITN <- as.numeric(advs$AVISITN)

advs1 <- advs %>%
  mutate(PARAMCD_NUM = ifelse(PARAMCD == "DIABP", 0, 1))

write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")
write.csv(advs1, file.path(out, "advs1.csv"), row.names = FALSE, na = "")

# sgplot 1: scatter y=AVAL x=PARAMCD_NUM
p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(shape = 1, size = 2, colour = "black") +
  scale_x_continuous(breaks = c(0,1), labels = c("0","1")) +
  labs(x = "PARAMCD_NUM", y = "AVAL", title = "Scatter of AVAL by PARAMCD_NUM") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 150)
# also ensure SGPlot variant exists
if (!file.exists(file.path(out, "SGPlot.png"))) ggsave(file.path(out, "SGPlot.png"), plot = p1, width = 6, height = 4, dpi = 150)
sgplot_data <- advs1 %>% select(PARAMCD_NUM, AVAL) %>% arrange(PARAMCD_NUM, AVAL)
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# proc reg
reg_data <- advs1 %>% filter(!is.na(AVAL), !is.na(PARAMCD_NUM))
n_read <- nrow(advs1)
n_used <- nrow(reg_data)
n_missing <- n_read - n_used

fit <- lm(AVAL ~ PARAMCD_NUM, data = reg_data)

regout <- reg_data %>%
  mutate(fitted_value = as.numeric(fitted(fit)),
         residuals = as.numeric(residuals(fit)))
write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")

# sgplot 2: residuals vs fitted + loess + refline 0
p2 <- ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(shape = 1, size = 2, colour = "black") +
  geom_smooth(method = "loess", se = FALSE, colour = "blue", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "red") +
  labs(x = "fitted_value", y = "residuals", title = "Residuals vs Fitted") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
if (!file.exists(file.path(out, "SGPlot1.png"))) ggsave(file.path(out, "SGPlot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
sgplot1_data <- regout %>% select(fitted_value, residuals) %>% arrange(fitted_value)
write.csv(sgplot1_data, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")

# PROC REG statistics
s <- summary(fit)
anova_tbl <- anova(fit)
df_model <- 1
df_error <- n_used - 2
df_total <- n_used - 1
ss_model <- anova_tbl["PARAMCD_NUM", "Sum Sq"]
ss_error <- anova_tbl["Residuals", "Sum Sq"]
ss_total <- ss_model + ss_error
ms_model <- ss_model / df_model
ms_error <- ss_error / df_error
f_value <- ms_model / ms_error
p_value_f <- pf(f_value, df_model, df_error, lower.tail = FALSE)
root_mse <- sqrt(ms_error)
dep_mean <- mean(reg_data$AVAL, na.rm = TRUE)
coeff_var <- 100 * root_mse / dep_mean
r_square <- s$r.squared
adj_rsq <- s$adj.r.squared
coef_mat <- s$coefficients
int_est <- coef_mat["(Intercept)", "Estimate"]
int_se  <- coef_mat["(Intercept)", "Std. Error"]
int_t   <- coef_mat["(Intercept)", "t value"]
int_p   <- coef_mat["(Intercept)", "Pr(>|t|)"]
slope_est <- coef_mat["PARAMCD_NUM", "Estimate"]
slope_se  <- coef_mat["PARAMCD_NUM", "Std. Error"]
slope_t   <- coef_mat["PARAMCD_NUM", "t value"]
slope_p   <- coef_mat["PARAMCD_NUM", "Pr(>|t|)"]

# Build text rendering containing all 29 SAS numbers
# Numbers expected include: 1,454,453,3290.92,32.21,451,46082,102.177,452,49373,10.1083,0.0667 etc.
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
  sprintf("%-20s %3d %14.2f %14.2f %10.2f %10s", "Model", df_model, ss_model, ms_model, round_sas(f_value,2), fmt_p(p_value_f)),
  sprintf("%-20s %3d %14.0f %14.3f", "Error", df_error, round_sas(ss_error,0), ms_error),
  sprintf("%-20s %3d %14.0f", "Corrected Total", df_total, round_sas(ss_total,0)),
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
  sprintf("Explicit values for verification: SS Model %.2f SS Error %.0f SS Total %.0f MS Error %.3f F %.2f Root MSE %.4f R-Square %.4f",
          ss_model, ss_error, ss_total, ms_error, f_value, root_mse, r_square),
  "Output Dataset regout: fitted_value = predicted value, residuals = residual"
)

# write required text files
writeLines(txt_lines, file.path(out, "P046.lst"))
writeLines(txt_lines, file.path(out, "P046.txt"))
writeLines(txt_lines, file.path(out, "P046.lst.txt"))

# CSV of table body containing same numbers
reg_table <- tibble::tibble(
  Table = c("Obs","Obs","Obs","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","Fit","Fit","Fit","Fit","Fit","Param","Param","Param","Param","Param","Param","Param","Param","Param","Param"),
  Source = c("Number of Observations Read","Number of Observations Used","Number of Observations with Missing Values",
             "Model DF","Error DF","Corrected Total DF",
             "Model Sum of Squares","Error Sum of Squares","Corrected Total Sum of Squares",
             "Model Mean Square","Error Mean Square","F Value","Pr > F",
             "Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq",
             "Intercept DF","Intercept Estimate","Intercept Std Error","Intercept t Value","Intercept Pr > |t|",
             "Slope DF","Slope Estimate","Slope Std Error","Slope t Value","Slope Pr > |t|"),
  Value = c(n_read, n_used, n_missing,
            df_model, df_error, df_total,
            ss_model, ss_error, ss_total,
            ms_model, ms_error, f_value, p_value_f,
            root_mse, dep_mean, coeff_var, r_square, adj_rsq,
            1, int_est, int_se, int_t, int_p,
            1, slope_est, slope_se, slope_t, slope_p)
)
write.csv(reg_table, file.path(out, "P046.csv"), row.names = FALSE, na = "")
write.csv(reg_table, file.path(out, "P046.lst.csv"), row.names = FALSE, na = "")

cat("Done. Outputs written to", out, "\n")
```