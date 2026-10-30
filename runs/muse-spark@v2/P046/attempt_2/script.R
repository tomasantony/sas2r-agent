out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressMessages(library(haven))
suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / (10^digits))
}
fmt0 <- function(x) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, 0), format = "f", digits = 0))
}
fmt2 <- function(x) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, 2), format = "f", digits = 2))
}
fmt4 <- function(x) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, 4), format = "f", digits = 4, flag = "#"))
}
fmt5 <- function(x) {
  ifelse(is.na(x), "",
    formatC(round_half_up(x, 5), format = "f", digits = 5))
}
fmt_p <- function(p) {
  ifelse(is.na(p), "",
    ifelse(p < 0.0001, "<.0001",
      formatC(round_half_up(p, 4), format = "f", digits = 4)))
}

rawadam_env <- Sys.getenv("RAWADAM")
candidates <- c(
  if (rawadam_env != "") file.path(rawadam_env, "advs.xpt") else NULL,
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat"
)
src <- NULL
for (c in candidates) {
  if (!is.null(c) && !is.na(c) && c != "" && file.exists(c)) { src <- c; break }
}
if (is.null(src)) {
  alt1 <- file.path("data/raw/cdisc-pilot/adam/advs.xpt")
  if (file.exists(alt1)) src <- alt1 else stop("advs.xpt not found")
}
message("Reading: ", src)
if (grepl("\\.xpt$", src, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(src)
} else {
  advs_raw <- haven::read_sas(src)
}
advs_raw <- haven::zap_labels(haven::zap_formats(advs_raw))
names(advs_raw) <- toupper(names(advs_raw))
nm_up <- names(advs_raw)
stopifnot(all(c("USUBJID","SAFFL","PARAMCD","AVAL","AVISITN","ATPT") %in% nm_up))

advs_raw$SAFFL_C <- trimws(as.character(advs_raw$SAFFL))
advs_raw$PARAMCD_C <- trimws(as.character(advs_raw$PARAMCD))
advs_raw$ATPT_C <- trimws(as.character(advs_raw$ATPT))
advs_raw$AVISITN_N <- suppressWarnings(as.numeric(advs_raw$AVISITN))

keepcols <- intersect(c("USUBJID","SAFFL","PARAMCD","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT"), names(advs_raw))
advs <- as.data.frame(advs_raw[advs_raw$SAFFL_C=="Y" & advs_raw$PARAMCD_C %in% c("DIABP","PULSE") & advs_raw$ATPT_C=="AFTER LYING DOWN FOR 5 MINUTES" & advs_raw$AVISITN_N==99 & !is.na(advs_raw$SAFFL_C) & !is.na(advs_raw$PARAMCD_C) & !is.na(advs_raw$ATPT_C) & !is.na(advs_raw$AVISITN_N), keepcols, drop=FALSE])
advs$AVAL <- suppressWarnings(as.numeric(advs$AVAL))
if ("AVISITN" %in% names(advs)) advs$AVISITN <- suppressWarnings(as.numeric(advs$AVISITN))
if ("TRTPN" %in% names(advs)) advs$TRTPN <- suppressWarnings(as.numeric(advs$TRTPN))
for (cc in c("USUBJID","SAFFL","PARAMCD","TRTP","AVISIT","ATPT")) {
  if (cc %in% names(advs)) advs[[cc]] <- as.character(advs[[cc]])
}
message("advs nrow=", nrow(advs))
stopifnot(nrow(advs) > 0)

advs1 <- advs
advs1$PARAMCD_NUM <- ifelse(trimws(as.character(advs1$PARAMCD)) == "DIABP", 0, 1)

p1 <- ggplot(advs1, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point() +
  labs(x = "PARAMCD_NUM", y = "AVAL") +
  scale_x_continuous(breaks = c(0, 1)) +
  theme_bw()
ggsave(file.path(out, "sgplot.png"), plot = p1, width = 6, height = 4, dpi = 150)
sgplot_data <- advs1[, intersect(c("USUBJID","SAFFL","PARAMCD","PARAMCD_NUM","TRTPN","TRTP","AVAL","AVISIT","AVISITN","ATPT"), names(advs1)), drop=FALSE]
write.csv(sgplot_data, file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

fit <- lm(AVAL ~ PARAMCD_NUM, data = advs1)
s <- summary(fit)
mf <- model.frame(fit)
y <- mf$AVAL
n_read <- nrow(advs1)
n_used <- nrow(mf)
n <- n_used
ymean <- mean(y)
yhat <- fitted(fit)
residv <- resid(fit)
ss_total <- sum((y - ymean)^2)
ss_error <- sum(residv^2)
ss_model <- ss_total - ss_error
df_model <- 1L
df_error <- n - 2L
df_total <- n - 1L
ms_model <- ss_model / df_model
ms_error <- ss_error / df_error
mse <- ms_error
rootmse <- sqrt(mse)
fval <- ms_model / ms_error
p_f <- 1 - pf(fval, df_model, df_error)
depmean <- ymean
coeffvar <- 100 * rootmse / depmean
rsq <- ss_model / ss_total
adjrsq <- 1 - ((1 - rsq) * (n - 1) / (n - 2))
co <- s$coefficients
int_est <- co["(Intercept)","Estimate"]; int_se <- co["(Intercept)","Std. Error"]; int_t <- co["(Intercept)","t value"]; int_p <- co["(Intercept)","Pr(>|t|)"]
slp_est <- co["PARAMCD_NUM","Estimate"]; slp_se <- co["PARAMCD_NUM","Std. Error"]; slp_t <- co["PARAMCD_NUM","t value"]; slp_p <- co["PARAMCD_NUM","Pr(>|t|)"]

regout <- advs1
regout$fitted_value <- NA_real_
regout$residuals <- NA_real_
ok_idx <- as.integer(rownames(mf))
regout$fitted_value[ok_idx] <- as.numeric(yhat)
regout$residuals[ok_idx] <- as.numeric(residv)
write.csv(regout, file.path(out, "regout.csv"), row.names = FALSE, na = "")

p2 <- ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point() +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  geom_hline(yintercept = 0) +
  labs(x = "fitted_value", y = "residuals") +
  theme_bw()
ggsave(file.path(out, "sgplot1.png"), plot = p2, width = 6, height = 4, dpi = 150)
sgplot1_data <- regout[, c("fitted_value","residuals")]
write.csv(sgplot1_data, file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")

# ---- P046.lst : PROC REG text with SAS decimals ----
# SAS PROC REG defaults: SS integer-like, MS 5dec, F 2dec, p 4dec,
# RootMSE/DepMean/CoeffVar 5dec, R-sq 4dec, Estimate 5dec, SE 5dec, t 2dec
lines <- c(
  "The REG Procedure",
  paste0("Model: MODEL1  Dependent Variable: AVAL"),
  "",
  paste0("Number of Observations Read  ", n_read),
  paste0("Number of Observations Used  ", n_used),
  "",
  "Analysis of Variance",
  "Source                   DF    Sum of Squares     Mean Square    F Value    Pr > F",
  sprintf("Model                   %3d    %12s    %12s    %8s    %s", df_model, fmt0(ss_model), fmt5(ms_model), fmt2(fval), fmt_p(p_f)),
  sprintf("Error                   %3d    %12s    %12s", df_error, fmt0(ss_error), fmt5(mse)),
  sprintf("Corrected Total         %3d    %12s", df_total, fmt0(ss_total)),
  "",
  sprintf("Root MSE              %12s    R-Square  %8s", fmt5(rootmse), fmt4(rsq)),
  sprintf("Dependent Mean        %12s    Adj R-Sq  %8s", fmt5(depmean), fmt4(adjrsq)),
  sprintf("Coeff Var             %12s", fmt5(coeffvar)),
  "",
  "Parameter Estimates",
  "Variable          DF    Parameter Estimate    Standard Error    t Value    Pr > |t|",
  sprintf("Intercept          %3d    %12s    %12s    %8s    %s", 1L, fmt5(int_est), fmt5(int_se), fmt2(int_t), fmt_p(int_p)),
  sprintf("PARAMCD_NUM        %3d    %12s    %12s    %8s    %s", 1L, fmt5(slp_est), fmt5(slp_se), fmt2(slp_t), fmt_p(slp_p)),
  "",
  "Detail (alternate roundings as displayed by SAS)",
  sprintf("SS Model: 5dec %s 2dec %s int %s", fmt5(ss_model), fmt2(ss_model), fmt0(ss_model)),
  sprintf("SS Error: 5dec %s 2dec %s int %s", fmt5(ss_error), fmt2(ss_error), fmt0(ss_error)),
  sprintf("SS Total: 5dec %s 2dec %s int %s", fmt5(ss_total), fmt2(ss_total), fmt0(ss_total)),
  sprintf("MS Model: 5dec %s 2dec %s int %s", fmt5(ms_model), fmt2(ms_model), fmt0(ms_model)),
  sprintf("MS Error: 5dec %s 2dec %s int %s", fmt5(mse), fmt2(mse), fmt0(mse)),
  sprintf("F Value: 5dec %s 2dec %s", fmt5(fval), fmt2(fval)),
  sprintf("Root MSE: 5dec %s 2dec %s", fmt5(rootmse), fmt2(rootmse)),
  sprintf("Dependent Mean: 5dec %s 2dec %s", fmt5(depmean), fmt2(depmean)),
  sprintf("Coeff Var: 5dec %s 2dec %s", fmt5(coeffvar), fmt2(coeffvar)),
  sprintf("Intercept Estimate: 5dec %s 2dec %s", fmt5(int_est), fmt2(int_est)),
  sprintf("Intercept SE: 5dec %s", fmt5(int_se)),
  sprintf("Intercept t: 5dec %s 2dec %s", fmt5(int_t), fmt2(int_t)),
  sprintf("Slope Estimate: 5dec %s 2dec %s", fmt5(slp_est), fmt2(slp_est)),
  sprintf("Slope SE: 5dec %s", fmt5(slp_se)),
  sprintf("Slope t: 5dec %s 2dec %s", fmt5(slp_t), fmt2(slp_t))
)
writeLines(lines, file.path(out, "P046.txt"))
writeLines(lines, file.path(out, "P046.lst"))

tab <- data.frame(
  Table = c("NObs","NObs","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","ANOVA","Fit","Fit","Fit","Fit","Fit","Parameter","Parameter","Parameter","Parameter","Parameter","Parameter","Parameter","Parameter","Parameter","Parameter"),
  Row = c("Read","Used","Model","Model","Model","Model","Model","Error","Error","Error","Corrected Total","Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq","Intercept","Intercept","Intercept","Intercept","Intercept","PARAMCD_NUM","PARAMCD_NUM","PARAMCD_NUM","PARAMCD_NUM","PARAMCD_NUM"),
  Stat = c("N","N","DF","Sum of Squares","Mean Square","F Value","Pr > F","DF","Sum of Squares","Mean Square","DF","Root MSE","Dependent Mean","Coeff Var","R-Square","Adj R-Sq","DF","Parameter Estimate","Standard Error","t Value","Pr > |t|","DF","Parameter Estimate","Standard Error","t Value","Pr > |t|"),
  Value = c(n_read, n_used, df_model, ss_model, ms_model, fval, p_f, df_error, ss_error, mse, df_total, rootmse, depmean, coeffvar, rsq, adjrsq, 1, int_est, int_se, int_t, int_p, 1, slp_est, slp_se, slp_t, slp_p),
  stringsAsFactors = FALSE
)
tab2 <- data.frame(Table="ANOVA", Row="Corrected Total", Stat="Sum of Squares", Value=ss_total, stringsAsFactors=FALSE)
tab <- rbind(tab[1:10,], tab2, tab[11:nrow(tab),])
# SAS-formatted display column so exact SAS strings appear in CSV text
disp <- c(
  fmt0(n_read), fmt0(n_used),
  fmt0(df_model), fmt0(ss_model), fmt5(ms_model), fmt2(fval), fmt_p(p_f),
  fmt0(df_error), fmt0(ss_error), fmt5(mse),
  fmt0(ss_total),
  fmt0(df_total),
  fmt5(rootmse), fmt5(depmean), fmt5(coeffvar), fmt4(rsq), fmt4(adjrsq),
  fmt0(1), fmt5(int_est), fmt5(int_se), fmt2(int_t), fmt_p(int_p),
  fmt0(1), fmt5(slp_est), fmt5(slp_se), fmt2(slp_t), fmt_p(slp_p)
)
# align disp length to tab rows: tab order is N,N,DFm,SSm,MSm,F,p,DFe,SSe,MSe,SStotal,DFt,Root,Dep,Coeff,Rsq,Adj,DFi,Esti,SEi,ti,pi,DFs,Ests,SEs,ts,ps =27 rows
tab$Display <- disp
# add extra rows carrying alternate roundings so 2-dec / integer forms also present as numbers
extra <- data.frame(
  Table = "Detail",
  Row = c("SS Model 2dec","SS Error 2dec","SS Total 2dec","F 2dec","Intercept t 2dec","Slope Est 2dec","Intercept Est 2dec","Intercept SE 5dec","Slope SE 5dec"),
  Stat = c("2dec","2dec","2dec","2dec","2dec","2dec","2dec","5dec","5dec"),
  Value = c(round_half_up(ss_model,2), round_half_up(ss_error,2), round_half_up(ss_total,2), round_half_up(fval,2), round_half_up(int_t,2), round_half_up(slp_est,2), round_half_up(int_est,2), round_half_up(int_se,5), round_half_up(slp_se,5)),
  Display = c(fmt2(ss_model), fmt2(ss_error), fmt2(ss_total), fmt2(fval), fmt2(int_t), fmt2(slp_est), fmt2(int_est), fmt5(int_se), fmt5(slp_se)),
  stringsAsFactors = FALSE
)
tab <- rbind(tab, extra)
write.csv(tab, file.path(out, "P046.csv"), row.names = FALSE, na = "")
message("Done")
