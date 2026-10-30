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

advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
library(haven)
library(dplyr)
library(ggplot2)

advs_raw <- haven::read_xpt(advs_path)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)
names(advs_raw) <- toupper(names(advs_raw))

need_cols <- c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT")
stopifnot(all(need_cols %in% names(advs_raw)))

# explicit SAS-like type handling: character keys trimmed, numerics numeric
advs_raw$SAFFL <- trimws(as.character(advs_raw$SAFFL))
advs_raw$PARAMCD <- trimws(as.character(advs_raw$PARAMCD))
advs_raw$ATPT <- trimws(as.character(advs_raw$ATPT))
advs_raw$AVAL <- as.numeric(advs_raw$AVAL)
advs_raw$AVISITN <- as.numeric(advs_raw$AVISITN)

# data advs; where SAFFL='Y' and PARAMCD in ('DIABP','PULSE')
#   and ATPT='AFTER LYING DOWN FOR 5 MINUTES' and AVISITN=99;
advs <- advs_raw[
  !is.na(advs_raw$SAFFL) & advs_raw$SAFFL == "Y" &
  !is.na(advs_raw$PARAMCD) & advs_raw$PARAMCD %in% c("DIABP","PULSE") &
  !is.na(advs_raw$ATPT) & advs_raw$ATPT == "AFTER LYING DOWN FOR 5 MINUTES" &
  !is.na(advs_raw$AVISITN) & advs_raw$AVISITN == 99,
  need_cols, drop = FALSE
]

# data advs1; PARAMCD_NUM
advs1 <- advs
advs1$PARAMCD_NUM <- ifelse(advs1$PARAMCD == "DIABP", 0, 1)

message(sprintf("advs1 N=%d", nrow(advs1)))

# ---- SGPlot 1: scatter y=AVAL x=PARAMCD_NUM ----
p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point() +
  labs(x = "PARAMCD_NUM", y = "AVAL") +
  theme_bw()
ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 150)
sgplot_data <- advs1[, c("USUBJID","PARAMCD","PARAMCD_NUM","AVAL")]
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

# ---- PROC REG: model AVAL = PARAMCD_NUM; output out=regout p=fitted_value r=residuals ----
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
# ANOVA sums of squares directly from lm/anova (most precise, matches SAS)
SSModel <- as.numeric(an["PARAMCD_NUM", "Sum Sq"])
SSError <- as.numeric(an["Residuals", "Sum Sq"])
SSTotal <- SSModel + SSError
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

# ---- SGPlot 2: scatter x=fitted_value y=residuals + loess + refline 0 ----
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

# ---- PROC REG text rendering (P046.lst) ----
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
  sprintf("%-15s %3d %15s %15s %10s %10s", "PARAMCD_NUM", 1, fmt_n(b1,5), fmt_n(SE_b1,5), fmt_n(t_b1,2), fmt_p(p_b1))
)
writeLines(txt, file.path(out, "P046.txt"))
writeLines(txt, file.path(out, "P046.lst"))

# ---- CSV of table body (P046.csv) ----
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
write.csv(P046_csv, file.path(out, "P046.csv"), row.names = FALSE, na = "")
message("done")
