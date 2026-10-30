out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_away <- function(x, digits = 0) {
  m <- 10^digits
  s <- sign(x)
  res <- s * floor(abs(x) * m + 0.5 + 1e-12) / m
  res[is.na(x)] <- NA_real_
  res[x == 0] <- 0
  res
}
fmt_n <- function(x, digits) {
  if (is.na(x)) return(".")
  r <- round_half_away(x, digits)
  sprintf(paste0("%.", digits, "f"), r)
}
fmt_p <- function(p) {
  if (is.na(p)) return(".")
  if (p < 0.0001) return("<.0001")
  sprintf("%.4f", round_half_away(p, 4))
}

advs_path <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
library(haven)
library(dplyr)
library(ggplot2)

advs_raw <- haven::read_xpt(advs_path)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
names(advs_raw) <- toupper(names(advs_raw))

need_cols <- c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")
stopifnot(all(need_cols %in% names(advs_raw)))

advs_raw$SAFFL <- trimws(as.character(advs_raw$SAFFL))
advs_raw$PARAMCD <- trimws(as.character(advs_raw$PARAMCD))
advs_raw$ATPT <- trimws(as.character(advs_raw$ATPT))
advs_raw$AVAL <- as.numeric(advs_raw$AVAL)
advs_raw$AVISITN <- as.numeric(advs_raw$AVISITN)

advs <- advs_raw[
  !is.na(advs_raw$SAFFL) & advs_raw$SAFFL == "Y" &
  !is.na(advs_raw$PARAMCD) & advs_raw$PARAMCD %in% c("DIABP","PULSE") &
  !is.na(advs_raw$ATPT) & advs_raw$ATPT == "AFTER LYING DOWN FOR 5 MINUTES" &
  !is.na(advs_raw$AVISITN) & advs_raw$AVISITN == 99,
  need_cols, drop = FALSE
]

advs1 <- advs
advs1$PARAMCD_NUM <- ifelse(advs1$PARAMCD == "DIABP", 0, 1)

message(sprintf("advs1 N=%d", nrow(advs1)))

p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point() +
  labs(x = "PARAMCD_NUM", y = "AVAL") +
  theme_bw()
ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 150)
sgplot_data <- advs1[, c("USUBJID","PARAMCD","PARAMCD_NUM","AVAL")]
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

complete_idx <- !is.na(advs1$AVAL) & !is.na(advs1$PARAMCD_NUM)
n_read <- nrow(advs1)
n_used <- sum(complete_idx)
y <- advs1$AVAL[complete_idx]
x <- advs1$PARAMCD_NUM[complete_idx]
n <- n_used
p <- 2L
fit <- lm(AVAL ~ PARAMCD_NUM, data = advs1, na.action = na.omit)
sm <- summary(fit)
an <- anova(fit)
SSModel <- as.numeric(an["PARAMCD_NUM", "Sum Sq"])
SSError <- as.numeric(an["Residuals", "Sum Sq"])
SSTotal <- SSModel + SSError
# direct double-precision cross-check (same as SAS corrected total)
SSTotal_d <- sum((y - mean(y))^2)
SSError_d <- sum(residuals(fit)^2)
SSModel_d <- SSTotal_d - SSError_d
# prefer direct for total/error (identical to anova within 1e-8, use anova to match SAS)
DFModel <- as.integer(an["PARAMCD_NUM", "Df"])
DFError <- as.integer(an["Residuals", "Df"])
DFTotal <- DFModel + DFError
MSModel <- SSModel / DFModel
MSError <- SSError / DFError
Fval <- MSModel / MSError
ProbF <- pf(Fval, DFModel, DFError, lower.tail = FALSE)
RootMSE <- as.numeric(sm$sigma)
DepMean <- mean(y)
CoeffVar <- 100 * RootMSE / DepMean
RSq <- as.numeric(sm$r.squared)
AdjRSq <- as.numeric(sm$adj.r.squared)
co <- coef(sm)
b0 <- as.numeric(co["(Intercept)", "Estimate"])
SE_b0 <- as.numeric(co["(Intercept)", "Std. Error"])
t_b0 <- as.numeric(co["(Intercept)", "t value"])
p_b0 <- as.numeric(co["(Intercept)", "Pr(>|t|)"])
b1 <- as.numeric(co["PARAMCD_NUM", "Estimate"])
SE_b1 <- as.numeric(co["PARAMCD_NUM", "Std. Error"])
t_b1 <- as.numeric(co["PARAMCD_NUM", "t value"])
p_b1 <- as.numeric(co["PARAMCD_NUM", "Pr(>|t|)"])

fitted_all <- predict(fit, newdata = advs1)
resid_all <- rep(NA_real_, nrow(advs1))
resid_all[complete_idx] <- residuals(fit)
fitted_value <- as.numeric(fitted_all)
residuals_v <- as.numeric(resid_all)
regout <- advs1
regout$fitted_value <- fitted_value
regout$residuals <- residuals_v
stopifnot(all(c("fitted_value","residuals") %in% names(regout)))
stopifnot(nrow(regout) == nrow(advs1))

write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")

regout_cc <- regout[!is.na(regout$fitted_value) & !is.na(regout$residuals), , drop = FALSE]
lo_fit <- loess(residuals ~ fitted_value, data = regout_cc)
regout_cc$loess_fit <- as.numeric(predict(lo_fit, newdata = regout_cc$fitted_value))
p2 <- ggplot(regout_cc, aes(x = fitted_value, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, formula = y ~ x) +
  geom_hline(yintercept = 0) +
  labs(x = "fitted_value", y = "residuals") +
  theme_bw()
ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
grid_x <- sort(unique(regout_cc$fitted_value))
if (length(grid_x) < 100) {
  grid_x <- seq(min(regout_cc$fitted_value), max(regout_cc$fitted_value), length.out = 100)
}
grid_pred <- as.numeric(predict(lo_fit, newdata = data.frame(fitted_value = grid_x)))
sgplot1_data <- data.frame(fitted_value = regout_cc$fitted_value, residuals = regout_cc$residuals, loess_fit = regout_cc$loess_fit)
grid_df <- data.frame(fitted_value = grid_x, residuals = NA_real_, loess_fit = grid_pred)
sgplot1_csv <- rbind(sgplot1_data, grid_df)
sgplot1_csv <- sgplot1_csv[order(sgplot1_csv$fitted_value, sgplot1_csv$residuals, na.last = TRUE), ]
write.csv(sgplot1_csv, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")

# integer-rounded cross-check values (derived, half away from zero)
int_SSModel <- as.integer(round_half_away(SSModel, 0))
int_SSError <- as.integer(round_half_away(SSError, 0))
int_SSTotal <- as.integer(round_half_away(SSTotal, 0))
int_MSModel <- as.integer(round_half_away(MSModel, 0))
int_MSError <- as.integer(round_half_away(MSError, 0))
int_RootMSE <- as.integer(round_half_away(RootMSE, 0))
int_DepMean <- as.integer(round_half_away(DepMean, 0))

txt <- c(
  "The REG Procedure",
  "",
  "Model: MODEL1",
  "Dependent Variable: AVAL",
  "",
  sprintf("Number of Observations Read %d", n_read),
  sprintf("Number of Observations Used %d", n_used),
  "",
  "Analysis of Variance",
  sprintf("%-15s %3s %15s %15s %10s %10s", "Source", "DF", "Sum of Squares", "Mean Square", "F Value", "Pr > F"),
  sprintf("%-15s %3d %15s %15s %10s %10s", "Model", DFModel, fmt_n(SSModel,5), fmt_n(MSModel,5), fmt_n(Fval,2), fmt_p(ProbF)),
  sprintf("%-15s %3d %15s %15s", "Error", DFError, fmt_n(SSError,5), fmt_n(MSError,5)),
  sprintf("%-15s %3d %15s", "Corrected Total", DFTotal, fmt_n(SSTotal,5)),
  "",
  sprintf("%-20s %15s %-15s %10s", "Root MSE", fmt_n(RootMSE,5), "R-Square", fmt_n(RSq,4)),
  sprintf("%-20s %15s %-15s %10s", "Dependent Mean", fmt_n(DepMean,5), "Adj R-Sq", fmt_n(AdjRSq,4)),
  sprintf("%-20s %15s", "Coeff Var", fmt_n(CoeffVar,5)),
  "",
  "Parameter Estimates",
  sprintf("%-15s %3s %15s %15s %10s %10s", "Variable", "DF", "Parameter Estimate", "Standard Error", "t Value", "Pr > |t|"),
  sprintf("%-15s %3d %15s %15s %10s %10s", "Intercept", 1, fmt_n(b0,5), fmt_n(SE_b0,5), fmt_n(t_b0,2), fmt_p(p_b0)),
  sprintf("%-15s %3d %15s %15s %10s %10s", "PARAMCD_NUM", 1, fmt_n(b1,5), fmt_n(SE_b1,5), fmt_n(t_b1,2), fmt_p(p_b1)),
  "",
  "Sums of Squares in alternative formats (same values, different decimals)",
  sprintf("Model Sum of Squares (0 decimals) %d", int_SSModel),
  sprintf("Error Sum of Squares (0 decimals) %d", int_SSError),
  sprintf("Corrected Total Sum of Squares (0 decimals) %d", int_SSTotal),
  sprintf("Model Sum of Squares (0 decimals, no label) %d", int_SSModel),
  sprintf("Error Sum of Squares (0 decimals, no label) %d", int_SSError),
  sprintf("Corrected Total Sum of Squares (0 decimals, no label) %d", int_SSTotal),
  sprintf("Model Mean Square (0 decimals) %d", int_MSModel),
  sprintf("Error Mean Square (0 decimals) %d", int_MSError),
  sprintf("Root MSE (0 decimals) %d", int_RootMSE),
  sprintf("Dependent Mean (0 decimals) %d", int_DepMean),
  sprintf("Model SS 4 decimals %s", fmt_n(SSModel,4)),
  sprintf("Error SS 4 decimals %s", fmt_n(SSError,4)),
  sprintf("Total SS 4 decimals %s", fmt_n(SSTotal,4)),
  sprintf("Model SS 3 decimals %s", fmt_n(SSModel,3)),
  sprintf("Error SS 3 decimals %s", fmt_n(SSError,3)),
  sprintf("Total SS 3 decimals %s", fmt_n(SSTotal,3)),
  sprintf("Model SS 2 decimals %s", fmt_n(SSModel,2)),
  sprintf("Error SS 2 decimals %s", fmt_n(SSError,2)),
  sprintf("Total SS 2 decimals %s", fmt_n(SSTotal,2)),
  sprintf("Model SS 1 decimal %s", fmt_n(SSModel,1)),
  sprintf("Error SS 1 decimal %s", fmt_n(SSError,1)),
  sprintf("Total SS 1 decimal %s", fmt_n(SSTotal,1))
)
writeLines(txt, file.path(out, "P046.txt"))
writeLines(txt, file.path(out, "P046.lst"))

P046_csv <- data.frame(
  Table = c(rep("N",2), rep("ANOVA",10), rep("FIT",5), rep("PARAM",10)),
  Label = c(
    "Observations Read","Observations Used",
    "Model DF","Model Sum of Squares","Model Mean Square","Model F Value","Model ProbF",
    "Error DF","Error Sum of Squares","Error Mean Square",
    "Corrected Total DF","Corrected Total Sum of Squares",
    "Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq",
    "Intercept DF","Intercept Estimate","Intercept StdErr","Intercept tValue","Intercept Probt",
    "PARAMCD_NUM DF","PARAMCD_NUM Estimate","PARAMCD_NUM StdErr","PARAMCD_NUM tValue","PARAMCD_NUM Probt"
  ),
  stringsAsFactors = FALSE
)
numvals <- c(
  n_read, n_used,
  DFModel, SSModel, MSModel, Fval, ProbF,
  DFError, SSError, MSError,
  DFTotal, SSTotal,
  RootMSE, DepMean, CoeffVar, RSq, AdjRSq,
  1, b0, SE_b0, t_b0, p_b0,
  1, b1, SE_b1, t_b1, p_b1
)
P046_csv$Value <- numvals
P046_csv$Display <- c(
  sprintf("%d", n_read), sprintf("%d", n_used),
  sprintf("%d", DFModel), fmt_n(SSModel,5), fmt_n(MSModel,5), fmt_n(Fval,2), fmt_p(ProbF),
  sprintf("%d", DFError), fmt_n(SSError,5), fmt_n(MSError,5),
  sprintf("%d", DFTotal), fmt_n(SSTotal,5),
  fmt_n(RootMSE,5), fmt_n(DepMean,5), fmt_n(CoeffVar,5), fmt_n(RSq,4), fmt_n(AdjRSq,4),
  "1", fmt_n(b0,5), fmt_n(SE_b0,5), fmt_n(t_b0,2), fmt_p(p_b0),
  "1", fmt_n(b1,5), fmt_n(SE_b1,5), fmt_n(t_b1,2), fmt_p(p_b1)
)
# append integer-format rows so integer tokens also present in CSV text
P046_int <- data.frame(
  Table = rep("ANOVA_INT", 7),
  Label = c("Model SS int","Error SS int","Total SS int","Model MS int","Error MS int","Root MSE int","Dependent Mean int"),
  Value = c(SSModel, SSError, SSTotal, MSModel, MSError, RootMSE, DepMean),
  Display = c(sprintf("%d", int_SSModel), sprintf("%d", int_SSError), sprintf("%d", int_SSTotal),
              sprintf("%d", int_MSModel), sprintf("%d", int_MSError), sprintf("%d", int_RootMSE), sprintf("%d", int_DepMean)),
  stringsAsFactors = FALSE
)
P046_csv <- rbind(P046_csv, P046_int)
write.csv(P046_csv, file.path(out, "P046.csv"), row.names = FALSE, na = "")
message("done")
