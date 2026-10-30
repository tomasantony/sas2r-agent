suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"

# Format a p-value the way SAS PVALUE6.4 does
fmt_p <- function(p) ifelse(p < 0.0001, "<.0001", sprintf("%.4f", p))

# ---- data advs ----
src <- read_xpt(file.path(rawadam, "advs.xpt"))
keepv <- c("USUBJID", "SAFFL", "PARAMCD", "TRTPN", "TRTP",
           "AVAL", "AVISIT", "AVISITN", "ATPT")
keepv <- names(src)[toupper(names(src)) %in% keepv]

advs <- src %>%
  filter(SAFFL == "Y", PARAMCD %in% c("DIABP", "PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN), AVISITN == 99) %>%
  select(all_of(keepv))
advs <- as.data.frame(lapply(advs, function(x) {
  attr(x, "label") <- NULL
  attr(x, "format.sas") <- NULL
  x
}), stringsAsFactors = FALSE)

# ---- data advs1 ----
advs1 <- advs
advs1$PARAMCD_NUM <- ifelse(advs1$PARAMCD == "DIABP", 0, 1)

write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")
write.csv(advs1, file.path(out, "advs1.csv"), row.names = FALSE, na = "")

# ---- sgplot 1: scatter ----
p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(shape = 1, colour = "#445694") +
  theme_bw()
ggsave(file.path(out, "sgplot.png"), p1, width = 6.4, height = 4.8, dpi = 100)
write.csv(advs1[, c("PARAMCD_NUM", "AVAL")], file.path(out, "sgplot.csv"),
          row.names = FALSE, na = "")

# ---- proc reg ----
nread <- nrow(advs1)
used <- advs1[!is.na(advs1$AVAL) & !is.na(advs1$PARAMCD_NUM), ]
nused <- nrow(used)
fit <- lm(AVAL ~ PARAMCD_NUM, data = used)
sm <- summary(fit)
av <- anova(fit)

dfm <- av$Df[1]; dfe <- av$Df[2]; dft <- dfm + dfe
ssm <- av$`Sum Sq`[1]; sse <- av$`Sum Sq`[2]; sst <- ssm + sse
msm <- ssm / dfm; mse <- sse / dfe
fval <- msm / mse; pf <- pf(fval, dfm, dfe, lower.tail = FALSE)
rmse <- sqrt(mse); dmean <- mean(used$AVAL); cv <- 100 * rmse / dmean
r2 <- sm$r.squared; ar2 <- sm$adj.r.squared
co <- sm$coefficients

# Output dataset: predicted/residual for all obs (missing where not computable)
regout <- advs1
pred <- coef(fit)[1] + coef(fit)[2] * advs1$PARAMCD_NUM
regout$fitted_value <- pred
regout$residuals <- advs1$AVAL - pred
write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")

# ---- text rendering of PROC REG output ----
lines <- c(
  "The REG Procedure", "Model: MODEL1", "Dependent Variable: AVAL", "",
  sprintf("Number of Observations Read  %d", nread),
  sprintf("Number of Observations Used  %d", nused), "",
  "Analysis of Variance", "",
  sprintf("%-16s %4s %14s %12s %8s %8s",
          "Source", "DF", "Sum of Squares", "Mean Square", "F Value", "Pr > F"),
  sprintf("%-16s %4d %14.5f %12.5f %8.2f %8s",
          "Model", dfm, ssm, msm, fval, fmt_p(pf)),
  sprintf("%-16s %4d %14.5f %12.5f", "Error", dfe, sse, mse),
  sprintf("%-16s %4d %14.5f", "Corrected Total", dft, sst), "",
  sprintf("Root MSE        %10.5f    R-Square  %7.4f", rmse, r2),
  sprintf("Dependent Mean  %10.5f    Adj R-Sq  %7.4f", dmean, ar2),
  sprintf("Coeff Var       %10.5f", cv), "",
  "Parameter Estimates", "",
  sprintf("%-12s %4s %12s %12s %8s %8s",
          "Variable", "DF", "Estimate", "Std Error", "t Value", "Pr > |t|"),
  sprintf("%-12s %4d %12.5f %12.5f %8.2f %8s", "Intercept", 1L,
          co[1, 1], co[1, 2], co[1, 3], fmt_p(co[1, 4])),
  sprintf("%-12s %4d %12.5f %12.5f %8.2f %8s", "PARAMCD_NUM", 1L,
          co[2, 1], co[2, 2], co[2, 3], fmt_p(co[2, 4]))
)
writeLines(lines, file.path(out, "P046.txt"))

# ---- CSV of the table body ----
tab <- data.frame(
  section = c("Obs", "Obs",
              "ANOVA", "ANOVA", "ANOVA",
              "Fit", "Fit", "Fit", "Fit", "Fit",
              "Param", "Param"),
  item = c("Number of Observations Read", "Number of Observations Used",
           "Model", "Error", "Corrected Total",
           "Root MSE", "Dependent Mean", "Coeff Var", "R-Square", "Adj R-Sq",
           "Intercept", "PARAMCD_NUM"),
  DF = c(NA, NA, dfm, dfe, dft, NA, NA, NA, NA, NA, 1, 1),
  value = c(nread, nused, NA, NA, NA, rmse, dmean, cv, r2, ar2, NA, NA),
  SS = c(NA, NA, ssm, sse, sst, rep(NA, 7)),
  MS = c(NA, NA, msm, mse, rep(NA, 8)),
  FValue = c(NA, NA, fval, rep(NA, 9)),
  ProbF = c(NA, NA, pf, rep(NA, 9)),
  Estimate = c(rep(NA, 10), co[, 1]),
  StdErr = c(rep(NA, 10), co[, 2]),
  tValue = c(rep(NA, 10), co[, 3]),
  Probt = c(rep(NA, 10), co[, 4]),
  stringsAsFactors = FALSE
)
write.csv(tab, file.path(out, "P046.csv"), row.names = FALSE, na = "")

# ---- sgplot 2: residuals vs fitted with loess + refline ----
rp <- regout[!is.na(regout$fitted_value) & !is.na(regout$residuals), ]
p2 <- ggplot(rp, aes(x = fitted_value, y = residuals)) +
  geom_point(shape = 1, colour = "#445694") +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
              colour = "#445694", method.args = list(degree = 1)) +
  geom_hline(yintercept = 0, colour = "grey40") +
  theme_bw()
ggsave(file.path(out, "sgplot1.png"), p2, width = 6.4, height = 4.8, dpi = 100)
write.csv(rp[, c("fitted_value", "residuals")], file.path(out, "sgplot1.csv"),
          row.names = FALSE, na = "")

cat("P046 done\n")
