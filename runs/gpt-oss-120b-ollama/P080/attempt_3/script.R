# ------------------------------------------------------------
# R translation of P080_f_forest.sas (attempt 3)
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)
library(stringr)

# ------------------------------------------------------------------
# Helper: SAS round half away from zero (round half up)
# ------------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                -floor(abs(x) * factor + 0.5) / factor)
  res
}
fmt_num <- function(x, width = 5, dec = 2) {
  sprintf(paste0("%", width, ".", dec, "f"), round_half_up(x, dec))
}

# ------------------------------------------------------------------
# Paths
# ------------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# ------------------------------------------------------------------
# Helper to read a SAS data set (tries .sas7bdat then .xpt)
# ------------------------------------------------------------------
read_adam <- function(fname) {
  sas7bdat_path <- file.path(adam_path, paste0(fname, ".sas7bdat"))
  xpt_path      <- file.path(adam_path, paste0(fname, ".xpt"))
  if (file.exists(sas7bdat_path)) {
    df <- read_sas(sas7bdat_path)
  } else if (file.exists(xpt_path)) {
    df <- read_xpt(xpt_path)
  } else {
    stop("Neither .sas7bdat nor .xpt found for ", fname)
  }
  # make column names upper‑case to mimic SAS case‑insensitivity
  names(df) <- toupper(names(df))
  df
}

# ------------------------------------------------------------------
# Load ADaM datasets
# ------------------------------------------------------------------
adtte_raw <- read_adam("adtte")
adsl_raw  <- read_adam("adsl")

# ------------------------------------------------------------------
# Step 1: Prepare analysis dataset (eff)
# ------------------------------------------------------------------
eff <- adtte_raw %>%
  inner_join(adsl_raw, by = "USUBJID") %>%
  filter(EFFFL == "Y", PARAMCD == "OS") %>%
  select(USUBJID,
         TRTA,
         TRTAN,
         AVAL,
         CNSR,
         SEX,
         AGEGR1,
         RACE,
         REGION1,
         ECOGBL)

# ------------------------------------------------------------------
# Parameters
# ------------------------------------------------------------------
trt1lbl <- "Placebo"   # reference treatment label (matches SAS macro)
subgroups <- list(
  list(var = "SEX",     label = "Sex",        ord = 1L),
  list(var = "AGEGR1",  label = "Age Group",  ord = 2L),
  list(var = "RACE",    label = "Race",       ord = 3L),
  list(var = "REGION1", label = "Region",     ord = 4L)
)

# ------------------------------------------------------------------
# Function to run Cox PH per subgroup level (mirrors PROC PHREG BY)
# ------------------------------------------------------------------
run_subgroup_cox <- function(data, subgrp, subgrp_label, subgrp_ord) {
  # SAS sorts missing first, then character order (case‑sensitive)
  data <- data %>% arrange(.data[[subgrp]])
  levels_vec <- unique(data[[subgrp]])
  out <- vector("list", length(levels_vec))
  i <- 1L
  for (lvl in levels_vec) {
    sub_data <- data %>% filter(.data[[subgrp]] == lvl)
    sub_data <- sub_data %>%
      mutate(TRTA = factor(TRTA)) %>%
      mutate(TRTA = relevel(TRTA, ref = trt1lbl))
    # Cox model: Surv(time, event) where event = 1 - CNSR (CNSR=1 => censored)
    fit <- coxph(Surv(AVAL, 1 - CNSR) ~ TRTA, data = sub_data, robust = FALSE)
    coef_tbl <- summary(fit)$coefficients
    est  <- coef_tbl[1, "coef"]
    se   <- coef_tbl[1, "se(coef)"]
    pval <- coef_tbl[1, "Pr(>|z|)"]
    hr   <- exp(est)
    lcl  <- exp(est - 1.96 * se)
    ucl  <- exp(est + 1.96 * se)
    out[[i]] <- tibble(
      subgroup = subgrp_label,
      subgroup_level = as.character(lvl),
      subgrp_ord = subgrp_ord,
      HR = hr,
      HR_LCL = lcl,
      HR_UCL = ucl,
      hr_display = fmt_num(hr),
      ci_display = paste0("(", fmt_num(lcl), ", ", fmt_num(ucl), ")"),
      ProbChiSq = pval
    )
    i <- i + 1L
  }
  bind_rows(out)
}

# ------------------------------------------------------------------
# Run subgroup analyses
# ------------------------------------------------------------------
sg_dfs <- lapply(subgroups, function(sg) {
  run_subgroup_cox(eff, sg$var, sg$label, sg$ord)
})

# ------------------------------------------------------------------
# Overall HR (no BY)
# ------------------------------------------------------------------
eff <- eff %>%
  mutate(TRTA = factor(TRTA)) %>%
  mutate(TRTA = relevel(TRTA, ref = trt1lbl))

fit_overall <- coxph(Surv(AVAL, 1 - CNSR) ~ TRTA, data = eff, robust = FALSE)
coef_o <- summary(fit_overall)$coefficients
est_o  <- coef_o[1, "coef"]
se_o   <- coef_o[1, "se(coef)"]
pval_o <- coef_o[1, "Pr(>|z|)"]
hr_o   <- exp(est_o)
lcl_o  <- exp(est_o - 1.96 * se_o)
ucl_o  <- exp(est_o + 1.96 * se_o)

overall_df <- tibble(
  subgroup = "Overall",
  subgroup_level = "All Subjects",
  subgrp_ord = 0L,
  HR = hr_o,
  HR_LCL = lcl_o,
  HR_UCL = ucl_o,
  hr_display = fmt_num(hr_o),
  ci_display = paste0("(", fmt_num(lcl_o), ", ", fmt_num(ucl_o), ")"),
  ProbChiSq = pval_o
)

# ------------------------------------------------------------------
# Combine all rows, create ordering for plot
# ------------------------------------------------------------------
forest_data <- bind_rows(overall_df, bind_rows(sg_dfs)) %>%
  mutate(row = row_number(),
         yval = -row,
         y_label = paste0(subgroup, ": ", subgroup_level))

# ------------------------------------------------------------------
# Write dataset CSV (SAS name: forest_data)
# ------------------------------------------------------------------
write.csv(forest_data,
          file.path(out_dir, "forest_data.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# Plain‑text rendering of the table (as .txt)
# ------------------------------------------------------------------
txt_tbl <- forest_data %>%
  select(subgroup, subgroup_level, hr_display, ci_display, ProbChiSq) %>%
  mutate(ProbChiSq = formatC(ProbChiSq, format = "e", digits = 2))

write.table(txt_tbl,
            file = file.path(out_dir, "forest_data.txt"),
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE)

# ------------------------------------------------------------------
# Forest plot (PNG) and CSV of plotted data (f_forest)
# ------------------------------------------------------------------
plot_df <- forest_data

p <- ggplot(plot_df, aes(y = yval, x = HR)) +
  geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL),
                 height = 0.2, size = 1, colour = "darkblue") +
  geom_point(shape = 23, size = 3, fill = "darkblue") +   # diamondfilled
  geom_vline(xintercept = 1, linetype = "dashed", colour = "gray") +
  scale_x_continuous(name = "Hazard Ratio (95% CI)",
                     trans = "log2",
                     breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
                     limits = c(0.08, 5)) +
  scale_y_continuous(breaks = -seq_len(nrow(plot_df)),
                     labels = plot_df$y_label,
                     expand = expansion(mult = c(0, 0))) +
  ggtitle("Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

png_path <- file.path(out_dir, "f_forest.png")
ggsave(filename = png_path, plot = p, width = 10, height = 7, units = "in", dpi = 300)

# CSV of plotted data (named per figure)
write.csv(plot_df,
          file.path(out_dir, "f_forest.csv"),
          row.names = FALSE,
          na = "")

# ------------------------------------------------------------------
# End of script
# ------------------------------------------------------------------
