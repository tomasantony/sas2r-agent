# P046_Box_Plot_Baseline.R
# Reimplementation of SAS program P046 in R
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)      # read_xpt
library(dplyr)      # data manipulation
library(ggplot2)    # graphics
library(broom)      # regression tidying
library(stringr)    # string handling
library(lubridate) # date handling (not used here but loaded per policy)

# ------------------------------------------------------------
# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor  <- 10^digits
  rounded <- floor(absx * factor + 0.5) / factor
  return(rounded * posneg)
}

# ------------------------------------------------------------
# Define paths from macro variables (hard‑coded as in the SAS program)
rawadam   <- "/bench/data/raw/cdisc-pilot/adam"
source_xpt <- file.path(rawadam, "advs.xpt")

# Output directory (set by the benchmark driver)
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") {
  stop("Environment variable OUT_DIR is not set.")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ------------------------------------------------------------
# 1. Read source data (XPORT transport file)
advs_raw <- read_xpt(source_xpt)

# ------------------------------------------------------------
# 2. Apply the same KEEP and WHERE logic as the SAS DATA step
advs <- advs_raw %>%
  # Keep only the variables listed in the KEEP statement
  select(USUBJID, SAFFL, PARAMCD, TRTPN, TRTP,
         AVAL, AVISIT, AVISITN, ATPT) %>%
  # Apply the WHERE clause
  filter(SAFFL == "Y",
         PARAMCD %in% c("DIABP", "PULSE"),
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         AVISITN == 99)

# ------------------------------------------------------------
# 3. Create the numeric version of PARAMCD (PARAMCD_NUM)
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
ggsave(filename = sgplot_file, width = 6, height = 4, dpi = 300)

# CSV of the plotted data
write.csv(sgplot_data,
          file = file.path(out_dir, "sgplot_data.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# 5. Linear regression (PROC REG equivalent)
lm_fit <- lm(AVAL ~ PARAMCD_NUM, data = advs1)

# Augment with fitted values and residuals (PROC REG OUTPUT OUT=)
regout <- augment(lm_fit) %>%
  rename(fitted_value = .fitted,
         residuals    = .resid) %>%
  select(fitted_value, residuals, everything())

# Write the regression output dataset
write.csv(regout,
          file = file.path(out_dir, "regout.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# 6. Second SGPlot – residuals vs fitted values with LOESS and reference line
sgplot1_file <- file.path(out_dir, "sgplot1.png")
ggplot(regout, aes(x = fitted_value, y = residuals)) +
  geom_point(color = "#7F8C8D", size = 1.5) +
  geom_smooth(method = "loess", se = FALSE, color = "#E74C3C") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(x = "Fitted Value", y = "Residuals",
       title = "Residuals vs Fitted Values with LOESS") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
ggsave(filename = sgplot1_file, width = 6, height = 4, dpi = 300)

# ------------------------------------------------------------
# 7. Produce the listing (PROC REG textual output)
# Capture the standard R summary of the linear model – this mirrors the SAS PROC REG output
reg_summary_txt <- capture.output({
  cat("=== PROC REG OUTPUT (R equivalent) ===\n\n")
  print(summary(lm_fit))
})

listing_txt_path <- file.path(out_dir, "P046.lst")
writeLines(reg_summary_txt, con = listing_txt_path)

# Additionally, create a CSV version of the key numeric results
# We'll combine coefficient table, fit statistics and ANOVA into one data frame
coeff_tbl <- tidy(lm_fit) %>%
  mutate(term = as.character(term)) %>%
  select(term, estimate, std.error, statistic, p.value)

fit_stats <- glance(lm_fit) %>%
  select(r.squared, adj.r.squared, sigma, statistic, p.value, df, logLik, AIC, BIC, deviance, df.residual)

anova_tbl <- anova(lm_fit) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Source") %>%
  rename(Df = Df, SumSq = `Sum Sq`, MeanSq = `Mean Sq`, Fvalue = `F value`, Pr = `Pr(>F)`)

# Write each component as separate CSV files (helps downstream validation)
write.csv(coeff_tbl, file.path(out_dir, "P046_coefficients.csv"), row.names = FALSE, na = "")
write.csv(fit_stats, file.path(out_dir, "P046_fitstats.csv"), row.names = FALSE, na = "")
write.csv(anova_tbl, file.path(out_dir, "P046_anova.csv"), row.names = FALSE, na = "")

# Also write a combined CSV (concatenated vertically) for the single‑file requirement
combined_listing <- bind_rows(
  tibble::tibble(Section = "Coefficients") %>% select(Section),
  coeff_tbl %>% mutate(Section = "Coefficients") %>% select(Section, everything()),
  tibble::tibble(Section = "FitStatistics") %>% select(Section),
  fit_stats %>% mutate(Section = "FitStatistics") %>% select(Section, everything()),
  tibble::tibble(Section = "ANOVA") %>% select(Section),
  anova_tbl %>% mutate(Section = "ANOVA") %>% select(Section, everything())
)

write.csv(combined_listing,
          file.path(out_dir, "P046.csv"),
          row.names = FALSE, na = "")

# ------------------------------------------------------------
# End of script – all required outputs have been written to OUT_DIR
