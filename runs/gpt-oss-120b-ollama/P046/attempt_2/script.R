# P046_Box_Plot_Baseline.R
# ------------------------------------------------------------
# Reimplementation of SAS program P046 in R
# ------------------------------------------------------------
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(ggplot2)    # graphics
library(broom)      # regression tidying
library(stringr)    # string handling
library(lubridate)  # (loaded per policy)

# ------------------------------------------------------------
# Helper: SAS style rounding (half away from zero)
round_half_up <- function(x, digits = 0) {
  sign_x <- sign(x)
  abs_x  <- abs(x)
  factor <- 10^digits
  rounded <- floor(abs_x * factor + 0.5) / factor
  sign_x * rounded
}
fmt_sas <- function(x, digits) {
  sprintf(paste0("%.", digits, "f"), round_half_up(x, digits))
}

# ------------------------------------------------------------
# Paths (hard‑coded as in the original SAS program)
rawadam   <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
source_xpt <- file.path(rawadam, "advs.xpt")

# Output directory (provided by the driver)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------
# 1. Read source XPORT file
advs_raw <- read_xpt(source_xpt)

# ------------------------------------------------------------
# 2. KEEP and WHERE processing (exactly as SAS)
advs <- advs_raw %>%
  select(USUBJID, SAFFL, PARAMCD, TRTPN, TRTP,
         AVAL, AVISIT, AVISITN, ATPT) %>%
  filter(SAFFL == "Y",
         PARAMCD %in% c("DIABP", "PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         AVISITN == 99)

# ------------------------------------------------------------
# 3. Create numeric version of PARAMCD
advs1 <- advs %>%
  mutate(PARAMCD_NUM = if_else(PARAMCD == "DIABP", 0L, 1L))

# ------------------------------------------------------------
# 4. First SGPlot – scatter of AVAL vs PARAMCD_NUM
sgplot_data <- advs1 %>% select(PARAMCD_NUM, AVAL)

sgplot_file <- file.path(out_dir, "sgplot.png")
ggplot(sgplot_data, aes(x = PARAMCD_NUM, y = AVAL)) +
  geom_point(color = "#2C3E50", size = 2) +
  labs(x = "PARAMCD_NUM", y = "AVAL",
       title = "Scatter of AVAL by PARAMCD_NUM") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(sgplot_file, width = 6, height = 4, dpi = 300)

write.csv(sgplot_data,
          file = file.path(out_dir, "sgplot_data.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# 5. Linear regression (PROC REG equivalent)
lm_fit <- lm(AVAL ~ PARAMCD_NUM, data = advs1)

# Augmented dataset (PROC REG OUTPUT OUT=)
regout <- augment(lm_fit) %>%
  rename(fitted_value = .fitted,
         residuals    = .resid) %>%
  select(fitted_value, residuals, everything())

write.csv(regout,
          file = file.path(out_dir, "regout.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# 6. Second SGPlot – residuals vs fitted values with LOESS
sgplot1_file <- file.path(out_dir, "sgplot1.png")
ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(color = "#7F8C8D", size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#E74C3C") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Fitted Value", y = "Residuals",
       title = "Residuals vs Fitted Values with LOESS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(sgplot1_file, width = 6, height = 4, dpi = 300)

# ------------------------------------------------------------
# 7. Produce SAS‑style PROC REG listing (P046.lst)
n_obs   <- nrow(advs1)
df_mod  <- 1
df_err  <- n_obs - 2
df_tot  <- n_obs - 1

y       <- advs1$AVAL
fitted  <- fitted(lm_fit)
resid   <- residuals(lm_fit)

sst     <- sum((y - mean(y))^2)          # Total Sum of Squares
ssm     <- sum((fitted - mean(y))^2)     # Model Sum of Squares
sse     <- sum(resid^2)                  # Error Sum of Squares

msm     <- ssm / df_mod
mse     <- sse / df_err
f_val   <- msm / mse
p_f     <- pf(f_val, df_mod, df_err, lower.tail = FALSE)

r_sq    <- ssm / sst
adj_rsq <- 1 - (1 - r_sq) * (n_obs - 1) / (n_obs - 2)
root_mse <- sqrt(mse)

# Coefficients and related statistics
coefs   <- summary(lm_fit)$coefficients
est_int <- coefs[1, "Estimate"]
est_slope <- coefs[2, "Estimate"]
se_int  <- coefs[1, "Std. Error"]
se_slope<- coefs[2, "Std. Error"]
t_int   <- coefs[1, "t value"]
t_slope <- coefs[2, "t value"]
p_int   <- coefs[1, "Pr(>|t|)"]
p_slope <- coefs[2, "Pr(>|t|)"]

# Build the text lines (numbers formatted to match SAS defaults)
lines <- c(
  paste0("Number of Observations Used = ", n_obs),
  "Dependent Variable: AVAL",
  "",
  "Analysis of Variance",
  "Source          DF   Sum of Squares   Mean Square   F Value   Pr > F",
  paste0(
    sprintf("%-15s", "Model"),
    sprintf("%3d", df_mod), "   ",
    fmt_sas(ssm, 0), "   ",
    fmt_sas(msm, 4), "   ",
    fmt_sas(f_val, 4), "   ",
    fmt_sas(p_f, 5)
  ),
  paste0(
    sprintf("%-15s", "Error"),
    sprintf("%3d", df_err), "   ",
    fmt_sas(sse, 0), "   ",
    fmt_sas(mse, 4)
  ),
  paste0(
    sprintf("%-15s", "Corrected Total"),
    sprintf("%3d", df_tot), "   ",
    fmt_sas(sst, 0)
  ),
  "",
  paste0("Root MSE        ", fmt_sas(root_mse, 4)),
  paste0("R-Square        ", fmt_sas(r_sq, 5)),
  paste0("Adj R-Sq        ", fmt_sas(adj_rsq, 5)),
  "",
  "Parameter Estimates",
  "Variable    Estimate    Std Error    t Value    Pr > |t|",
  paste0(
    sprintf("%-10s", "Intercept"),
    fmt_sas(est_int, 2), "   ",
    fmt_sas(se_int, 4), "   ",
    fmt_sas(t_int, 4), "   ",
    fmt_sas(p_int, 5)
  ),
  paste0(
    sprintf("%-10s", "PARAMCD_NUM"),
    fmt_sas(est_slope, 2), "   ",
    fmt_sas(se_slope, 4), "   ",
    fmt_sas(t_slope, 4), "   ",
    fmt_sas(p_slope, 5)
  )
)

writeLines(lines, con = file.path(out_dir, "P046.lst"))

# ------------------------------------------------------------
# 8. Create CSV versions of the key tables (for completeness)
coeff_tbl <- tibble::tibble(
  Variable = c("Intercept", "PARAMCD_NUM"),
  Estimate = c(est_int, est_slope),
  StdError = c(se_int, se_slope),
  tValue   = c(t_int, t_slope),
  PrgtT    = c(p_int, p_slope)
)
write.csv(coeff_tbl,
          file.path(out_dir, "P046_coefficients.csv"),
          row.names = FALSE, na = "")

anova_tbl <- tibble::tibble(
  Source = c("Model", "Error", "Corrected Total"),
  DF     = c(df_mod, df_err, df_tot),
  SumSq  = c(ssm, sse, sst),
  MeanSq = c(msm, mse, NA_real_),
  FValue = c(f_val, NA_real_, NA_real_),
  PrF    = c(p_f, NA_real_, NA_real_)
)
write.csv(anova_tbl,
          file.path(out_dir, "P046_anova.csv"),
          row.names = FALSE, na = "")

fit_stats <- tibble::tibble(
  RootMSE = root_mse,
  RSquare = r_sq,
  AdjRSquare = adj_rsq
)
write.csv(fit_stats,
          file.path(out_dir, "P046_fitstats.csv"),
          row.names = FALSE, na = "")

# Combined CSV (mirrors the text listing order)
combined <- bind_rows(
  tibble::tibble(Section = "ANOVA", Source = "Model", DF = df_mod, SumSq = ssm,
                 MeanSq = msm, FValue = f_val, PrF = p_f),
  tibble::tibble(Section = "ANOVA", Source = "Error", DF = df_err, SumSq = sse,
                 MeanSq = mse, FValue = NA_real_, PrF = NA_real_),
  tibble::tibble(Section = "ANOVA", Source = "Corrected Total", DF = df_tot,
                 SumSq = sst, MeanSq = NA_real_, FValue = NA_real_, PrF = NA_real_),
  tibble::tibble(Section = "FIT", RootMSE = root_mse, RSquare = r_sq,
                 AdjRSquare = adj_rsq),
  tibble::tibble(Section = "COEF", Variable = "Intercept", Estimate = est_int,
                 StdError = se_int, tValue = t_int, PrgtT = p_int),
  tibble::tibble(Section = "COEF", Variable = "PARAMCD_NUM", Estimate = est_slope,
                 StdError = se_slope, tValue = t_slope, PrgtT = p_slope)
)
write.csv(combined,
          file.path(out_dir, "P046.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script – all required outputs are now in OUT_DIR
