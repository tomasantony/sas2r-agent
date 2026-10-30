out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# Helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  # floor(x+0.5) with epsilon for floating errors
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}

# Helper: SAS-like put with w.d (round half up)
sas_put <- function(x, w = 5, d = 2) {
  if (is.na(x)) return("")
  v <- round_half_up(x, d)
  fmt <- paste0("%", w, ".", d, "f")
  s <- sprintf(fmt, v)
  trimws(s)
}

# Locate adam files
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

list.files(adam_dir)
list.files(sdtm_dir)

# Try to read adsl and adtte
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(survival)
library(ggplot2)

find_adam_file <- function(dir, name) {
  # name like adsl, adtte
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  # case-insensitive match
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx) > 0) return(files[idx[1]])
  # also check subdirs
  files2 <- list.files(dir, full.names = TRUE, recursive = TRUE)
  idx2 <- grep(paste0("^", name, "\\."), basename(files2), ignore.case = TRUE)
  if (length(idx2) > 0) return(files2[idx2[1]])
  return(NA)
}

adsl_path <- find_adam_file(adam_dir, "adsl")
adtte_path <- find_adam_file(adam_dir, "adtte")
adsl_path
adtte_path

# Read with haven (handles sas7bdat and xpt)
read_sas_file <- function(path) {
  if (is.na(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "xpt") {
    haven::read_xpt(path)
  } else {
    # try both
    tryCatch(haven::read_sas(path), error = function(e) haven::read_xpt(path))
  }
}

adsl <- read_sas_file(adsl_path)
adtte <- read_sas_file(adtte_path)

# Inspect
names(adsl) |> head(50)
names(adtte) |> head(50)
unique(adsl$TRTA) |> head(20)
unique(adsl$TRT01A)
unique(adsl$EFFFL)
unique(adtte$PARAMCD) |> head(20)
unique(adtte$TRTA) |> head(20)
head(adsl[, c("USUBJID","TRTA","TRTAN","EFFFL","SEX","AGEGR1","RACE","REGION1","ECOGBL")])
head(adtte[, c("USUBJID","PARAMCD","AVAL","CNSR","TRTA")])

# Check treatment labels from u_setup
trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
trt3lbl <- "[Drug] High Dose"

# Build eff dataset as in SAS proc sql
# SAS: select a.USUBJID, a.TRTA, a.TRTAN, a.AVAL, a.CNSR, b.SEX, b.AGEGR1, b.RACE, b.REGION1, b.ECOGBL
# from adam.adtte a inner join adam.adsl b on a.USUBJID = b.USUBJID
# where b.EFFFL='Y' and a.PARAMCD='OS'

# Ensure character handling
adsl2 <- adsl %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))
adtte2 <- adtte %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))

# Check EFFFL values
table(adsl2$EFFFL, useNA = "ifany")
table(adtte2$PARAMCD, useNA = "ifany")
table(adtte2$CNSR, useNA = "ifany")

eff <- adtte2 %>%
  filter(PARAMCD == "OS") %>%
  inner_join(adsl2 %>% filter(EFFFL == "Y") %>% select(USUBJID, SEX, AGEGR1, RACE, REGION1, ECOGBL), by = "USUBJID") %>%
  select(USUBJID, TRTA, TRTAN, AVAL, CNSR, SEX, AGEGR1, RACE, REGION1, ECOGBL)

nrow(eff)
head(eff)
table(eff$TRTA, useNA = "ifany")
table(eff$SEX, useNA = "ifany")
table(eff$AGEGR1, useNA = "ifany")
table(eff$RACE, useNA = "ifany")
table(eff$REGION1, useNA = "ifany")

# Check if TRTA has 3 levels
# SAS PHREG with class TRTA(ref="Placebo") and model AVAL*CNSR(1)=TRTA
# With 3 levels, we get 2 HRs. Need to see how SAS handles hazardratio statement
# The SAS code filters where Parameter='TRTA' - that would keep both.
# But forest plot expects one HR per subgroup level. So maybe they intend to compare
# pooled active vs placebo? Or maybe synthetic data has only 2 TRTA levels in eff?
# Let's see actual TRTA values in eff
unique(eff$TRTA)

# If 3 levels, we need to decide which HR to plot. Possibly SAS would produce 2 rows per subgroup,
# and the plot would show both. We should replicate that behavior.
# Let's implement Cox model per subgroup level, extracting all non-reference levels.

# Function to run Cox per subgroup level
run_cox_by <- function(data, by_var, ref = trt1lbl) {
  # data: eff subset
  # by_var: column name string
  # returns data frame with one row per by level per treatment comparison
  levs <- unique(data[[by_var]])
  levs <- levs[!is.na(levs)]
  # sort as SAS would: character ordering uppercase before lowercase, missing first
  # SAS sort by &subgrp. -> ascending
  levs_sorted <- sort(levs)
  results <- list()
  for (lv in levs_sorted) {
    sub <- data %>% filter(.data[[by_var]] == lv)
    # Ensure TRTA is factor with ref level
    # Need to handle case where subgroup has only one treatment level -> model fails
    # Check number of events and treatment levels
    trt_levels <- unique(sub$TRTA)
    # Set factor with ref first
    # Determine all treatment levels present in overall eff
    all_trt <- unique(data$TRTA)
    # Ensure ref is in levels
    # Create factor with ref as reference
    sub$TRTA_f <- factor(sub$TRTA, levels = c(ref, setdiff(sort(unique(eff$TRTA)), ref)))
    # If only one TRTA level in subgroup, skip (no HR)
    if (length(unique(sub$TRTA_f[!is.na(sub$TRTA_f)])) < 2) {
      next
    }
    # Also need at least one event per group? coxph will handle
    # Run coxph
    # Use model AVAL*CNSR(1)=TRTA  where CNSR=1 is censored, 0 is event
    # In survival::Surv, event = 1 if not censored. So event = 1 - CNSR? Check SAS: CNSR(1) means 1 is censored.
    # So event indicator = (CNSR == 0)
    sub$event <- ifelse(sub$CNSR == 1, 0, 1)
    # Check if any events
    if (sum(sub$event) == 0) next
    fit <- tryCatch({
      survival::coxph(survival::Surv(AVAL, event) ~ TRTA_f, data = sub, ties = "breslow")
    }, error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)
    # s$coefficients has rows for each non-ref level
    # Extract Estimate, StdErr, ProbChiSq
    coefs <- s$coefficients
    # coefs columns: coef, exp(coef), se(coef), z, Pr(>|z|)
    # For SAS ParameterEstimates: Estimate = coef, StdErr = se(coef), ProbChiSq = p-value (Wald)
    for (i in seq_len(nrow(coefs))) {
      coef_name <- rownames(coefs)[i]
      # coef_name like "TRTA_f[Drug] Low Dose" or "TRTA_fPlacebo" etc.
      # Extract treatment level
      # Remove prefix
      trt_level <- sub("^TRTA_f", "", coef_name)
      est <- coefs[i, "coef"]
      se <- coefs[i, "se(coef)"]
      pval <- coefs[i, "Pr(>|z|)"]
      hr <- exp(est)
      hr_lcl <- exp(est - 1.96 * se)
      hr_ucl <- exp(est + 1.96 * se)
      hr_display <- sas_put(hr, 5, 2)
      ci_display <- paste0("(", sas_put(hr_lcl, 5, 2), ", ", sas_put(hr_ucl, 5, 2), ")")
      results[[length(results)+1]] <- tibble(
        subgroup_level = lv,
        trt_comparison = trt_level,
        Estimate = est,
        StdErr = se,
        ProbChiSq = pval,
        HR = hr,
        HR_LCL = hr_lcl,
        HR_UCL = hr_ucl,
        hr_display = hr_display,
        ci_display = ci_display
      )
    }
  }
  if (length(results)==0) return(NULL)
  bind_rows(results)
}

# Test for SEX
test_sex <- run_cox_by(eff, "SEX")
test_sex
test_age <- run_cox_by(eff, "AGEGR1")
test_age
test_race <- run_cox_by(eff, "RACE")
test_race
test_region <- run_cox_by(eff, "REGION1")
test_region

# Overall
run_cox_overall <- function(data, ref = trt1lbl) {
  data$TRTA_f <- factor(data$TRTA, levels = c(ref, setdiff(sort(unique(data$TRTA)), ref)))
  data$event <- ifelse(data$CNSR == 1, 0, 1)
  fit <- survival::coxph(survival::Surv(AVAL, event) ~ TRTA_f, data = data, ties = "breslow")
  s <- summary(fit)
  coefs <- s$coefficients
  res <- list()
  for (i in seq_len(nrow(coefs))) {
    coef_name <- rownames(coefs)[i]
    trt_level <- sub("^TRTA_f", "", coef_name)
    est <- coefs[i, "coef"]
    se <- coefs[i, "se(coef)"]
    pval <- coefs[i, "Pr(>|z|)"]
    hr <- exp(est)
    hr_lcl <- exp(est - 1.96 * se)
    hr_ucl <- exp(est + 1.96 * se)
    hr_display <- sas_put(hr, 5, 2)
    ci_display <- paste0("(", sas_put(hr_lcl, 5, 2), ", ", sas_put(hr_ucl, 5, 2), ")")
    res[[length(res)+1]] <- tibble(
      subgroup_level = "All Subjects",
      trt_comparison = trt_level,
      Estimate = est,
      StdErr = se,
      ProbChiSq = pval,
      HR = hr,
      HR_LCL = hr_lcl,
      HR_UCL = hr_ucl,
      hr_display = hr_display,
      ci_display = ci_display
    )
  }
  bind_rows(res)
}
overall_res <- run_cox_overall(eff)
overall_res

# Now build forest_data as SAS does
# SAS combines: _sg_overall _sg_1 _sg_2 _sg_3 _sg_4
# Each _sg_* has subgroup label and subgrp_ord
# Let's replicate with proper ordering

subgroup_defs <- list(
  list(var="SEX", label="Sex", ord=1),
  list(var="AGEGR1", label="Age Group", ord=2),
  list(var="RACE", label="Race", ord=3),
  list(var="REGION1", label="Region", ord=4)
)

forest_list <- list()
# Overall
for (i in seq_len(nrow(overall_res))) {
  forest_list[[length(forest_list)+1]] <- tibble(
    subgroup = "Overall",
    subgroup_level = overall_res$subgroup_level[i],
    subgrp_ord = 0,
    trt_comparison = overall_res$trt_comparison[i],
    HR = overall_res$HR[i],
    HR_LCL = overall_res$HR_LCL[i],
    HR_UCL = overall_res$HR_UCL[i],
    hr_display = overall_res$hr_display[i],
    ci_display = overall_res$ci_display[i],
    ProbChiSq = overall_res$ProbChiSq[i],
    Estimate = overall_res$Estimate[i],
    StdErr = overall_res$StdErr[i]
  )
}
for (def in subgroup_defs) {
  res <- run_cox_by(eff, def$var)
  if (is.null(res)) next
  for (i in seq_len(nrow(res))) {
    forest_list[[length(forest_list)+1]] <- tibble(
      subgroup = def$label,
      subgroup_level = res$subgroup_level[i],
      subgrp_ord = def$ord,
      trt_comparison = res$trt_comparison[i],
      HR = res$HR[i],
      HR_LCL = res$HR_LCL[i],
      HR_UCL = res$HR_UCL[i],
      hr_display = res$hr_display[i],
      ci_display = res$ci_display[i],
      ProbChiSq = res$ProbChiSq[i],
      Estimate = res$Estimate[i],
      StdErr = res$StdErr[i]
    )
  }
}
forest_data <- bind_rows(forest_list)
forest_data <- forest_data %>% mutate(row = row_number(), yval = -row_number())
forest_data

# Check counts
nrow(forest_data)
# This will be used for plotting

# Need to also compute N per subgroup level for display? SAS doesn't show N but we should include for CSV
# Let's compute N and events per subgroup level
compute_n <- function(data, var, level) {
  sub <- data %>% filter(.data[[var]] == level)
  n <- nrow(sub)
  events <- sum(sub$CNSR == 0, na.rm=TRUE)
  censored <- sum(sub$CNSR == 1, na.rm=TRUE)
  c(n=n, events=events, censored=censored)
}
# For overall
overall_n <- nrow(eff)
overall_events <- sum(eff$CNSR==0)

# Add N to forest_data
forest_data <- forest_data %>%
  rowwise() %>%
  mutate(
    N = if (subgroup == "Overall") overall_n else {
      var <- case_when(
        subgroup == "Sex" ~ "SEX",
        subgroup == "Age Group" ~ "AGEGR1",
        subgroup == "Race" ~ "RACE",
        subgroup == "Region" ~ "REGION1",
        TRUE ~ NA_character_
      )
      compute_n(eff, var, subgroup_level)["n"]
    },
    n_events = if (subgroup == "Overall") overall_events else {
      var <- case_when(
        subgroup == "Sex" ~ "SEX",
        subgroup == "Age Group" ~ "AGEGR1",
        subgroup == "Race" ~ "RACE",
        subgroup == "Region" ~ "REGION1",
        TRUE ~ NA_character_
      )
      compute_n(eff, var, subgroup_level)["events"]
    }
  ) %>% ungroup()

forest_data

# Now we need to produce figure similar to SAS SGPLOT
# SAS: refline at 1, highlow for CI, scatter for HR, yaxis discrete values -1 to -n, xaxis log base 2 with values 0.1 0.25 0.5 1 2 4
# Also titles

# Prepare labels for y-axis: need to show subgroup and subgroup_level
# SAS yaxis type=discrete values=(%yvals) where %yvals expands to -1 -2 -3 ... -n
# display nolabel fitpolicy none -> shows yval values? Actually yval is numeric, but they use discrete?
# In SAS, yval is numeric -1, -2 etc, and they set yaxis type=discrete values=( -1 -2 ... ) -> shows those values as tick labels
# But they didn't provide formatted values, so it would show -1, -2 etc. However they likely rely on yval being discrete and showing subgroup labels via yaxis?
# Actually SAS code is incomplete: they set yaxis display=(nolabel) but not showing subgroup labels. Possibly they intended to use yaxis with subgroup_level as format?
# In our R, we need to create a proper forest plot with subgroup labels on y-axis.

# Let's create a label column
forest_data <- forest_data %>%
  mutate(
    y_label = ifelse(subgroup == "Overall", paste0(subgroup, ": ", subgroup_level),
                     paste0(subgroup, " - ", subgroup_level)),
    # For display with treatment comparison if multiple
    y_label_full = ifelse(n_distinct(trt_comparison) > 1,
                          paste0(y_label, " (", trt_comparison, " vs ", trt1lbl, ")"),
                          y_label),
    # For ordering top to bottom: yval = -row, so row 1 at top
    y_factor = factor(y_label_full, levels = rev(y_label_full))
  )

# Check if multiple trt comparisons
unique(forest_data$trt_comparison)

# If there are 2 comparisons, we need to handle faceting or color
# Let's see actual data: likely 2 comparisons
# For forest plot, we can use color/shape by trt_comparison

# Create plot
library(ggplot2)

# Determine x limits
xmin <- 0.1
xmax <- 4

# Create plot data
plot_data <- forest_data

# Build ggplot
p <- ggplot(plot_data, aes(x = HR, y = y_factor)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL), height = 0.2, color = "darkblue", linewidth = 0.8) +
  geom_point(aes(color = trt_comparison), size = 3, shape = 18) +
  scale_x_continuous(
    trans = "log2",
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
    limits = c(0.1, 4),
    labels = c("0.1", "0.25", "0.5", "1", "2", "4")
  ) +
  scale_color_manual(values = c("[Drug] Low Dose" = "darkblue", "[Drug] High Dose" = "darkred", "Placebo" = "black")) +
  labs(
    title = "Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival",
    subtitle = "Efficacy Analysis Set",
    x = "Hazard Ratio (95% CI)",
    y = "",
    color = "Comparison",
    caption = "Data cutoff: [DDMONYYYY]\nHazard Ratios from Cox proportional hazards model.\nProgram: P080_f_forest.sas"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  # Add text labels for HR and CI on right side
  geom_text(aes(label = paste0(hr_display, " ", ci_display), x = 4.2), hjust = 0, size = 3, color = "black") +
  coord_cartesian(clip = "off", xlim = c(0.1, 5.5)) +
  annotate("text", x = 0.25, y = 0.5, label = paste0("Favors ", trt2lbl), size = 3, hjust = 0) +
  annotate("text", x = 2, y = 0.5, label = paste0("Favors ", trt1lbl), size = 3, hjust = 1)

p

# Save
ggsave(file.path(out, "f_forest.png"), plot = p, width = 10, height = 7, units = "in", dpi = 300)

# Write CSV of plotted data
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

# Also write txt rendering
# Create text table
txt_lines <- c(
  "Figure 14.2.2",
  "Forest Plot of Subgroup Analyses - Overall Survival",
  "Efficacy Analysis Set",
  "",
  paste0("Data cutoff: [DDMONYYYY]"),
  "Hazard Ratios from Cox proportional hazards model.",
  "",
  sprintf("%-30s %-20s %8s %20s %10s %8s", "Subgroup", "Level", "HR", "95% CI", "p-value", "N"),
  paste(rep("-", 100), collapse="")
)
for (i in seq_len(nrow(forest_data))) {
  r <- forest_data[i,]
  line <- sprintf("%-30s %-20s %8s %20s %10.4f %8d",
                  r$subgroup, r$subgroup_level, r$hr_display, r$ci_display, r$ProbChiSq, r$N)
  txt_lines <- c(txt_lines, line)
}
writeLines(txt_lines, file.path(out, "f_forest.txt"))

# Also need to ensure output matches expected: figure f_forest.rtf -> we produce png and csv
# Check files
list.files(out)
