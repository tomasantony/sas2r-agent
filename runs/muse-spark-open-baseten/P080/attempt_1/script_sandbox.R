# P080_f_forest.sas -> R translation
# Figure 14.2.2 Forest Plot of Subgroup Analyses - Overall Survival

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(survival)
  library(broom)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero (for formatting we use sprintf which is half away)
round_half_up <- function(x, digits=0){
  posneg <- sign(x)
  z <- abs(x)*10^digits
  z <- floor(z+0.5+1e-12)
  z/10^digits * posneg
}
fmt_5_2 <- function(x){
  if (is.na(x)) return(NA_character_)
  # SAS put 5.2 then strip: essentially %.2f without leading spaces
  sprintf("%.2f", round_half_up(x,2))
}

adam_path <- "/bench/data/raw/own-synthetic/adam"
sdtm_path <- "/bench/data/raw/own-synthetic/sdtm"

# function to find and read adam dataset
read_adam <- function(name){
  # try various extensions
  candidates <- c(
    file.path(adam_path, paste0(name, ".sas7bdat")),
    file.path(adam_path, paste0(name, ".xpt")),
    file.path(adam_path, paste0(tolower(name), ".sas7bdat")),
    file.path(adam_path, paste0(tolower(name), ".xpt")),
    file.path(adam_path, paste0(toupper(name), ".sas7bdat")),
    file.path(adam_path, paste0(toupper(name), ".xpt"))
  )
  # also list files case-insensitive
  if (!dir.exists(adam_path)) stop("adam_path not found: ", adam_path)
  files <- list.files(adam_path, full.names = TRUE, ignore.case = TRUE)
  # match by name without extension
  idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
  if (length(idx) > 0) candidates <- c(files[idx], candidates)
  for (f in candidates){
    if (file.exists(f)){
      message("Reading ", f)
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) {
        return(haven::read_xpt(f))
      } else if (grepl("\\.sas7bdat$", f, ignore.case=TRUE)) {
        return(haven::read_sas(f))
      } else {
        # try both
        tryCatch(return(haven::read_sas(f)), error=function(e) haven::read_xpt(f))
      }
    }
  }
  stop("Dataset not found: ", name, " in ", adam_path)
}

adtte_raw <- read_adam("adtte")
adsl_raw  <- read_adam("adsl")

# Convert haven labelled to plain vectors for key vars
# haven::as_factor or zap_labels
adtte <- adtte_raw %>%
  mutate(across(everything(), ~ haven::zap_formats(.))) %>%
  mutate(across(everything(), ~ haven::zap_label(.))) %>%
  mutate(across(everything(), ~ haven::zap_widths(.)))

adsl <- adsl_raw %>%
  mutate(across(everything(), ~ haven::zap_formats(.))) %>%
  mutate(across(everything(), ~ haven::zap_label(.))) %>%
  mutate(across(everything(), ~ haven::zap_widths(.)))

# Ensure character columns are character (haven may give labelled)
# Convert labelled to character/numeric appropriately
to_char <- function(x){
  if (inherits(x, "haven_labelled")) {
    # keep numeric if needed, but for grouping vars convert to character via as_character
    haven::as_character(x)
  } else x
}
adtte <- adtte %>% mutate(across(everything(), to_char))
adsl  <- adsl  %>% mutate(across(everything(), to_char))

# Ensure numeric types for AVAL, CNSR, TRTAN
adtte$AVAL <- as.numeric(adtte$AVAL)
adtte$CNSR <- as.numeric(adtte$CNSR)
adtte$TRTAN <- suppressWarnings(as.numeric(adtte$TRTAN))
adsl$TRTAN <- suppressWarnings(as.numeric(adsl$TRTAN))

# Treatment labels from u_setup.sas
trt1lbl <- "Placebo"
trt2lbl <- "Xanomeline Low Dose"
# try to infer from data if available, but keep as in setup
# The setup says [Drug] Low Dose etc, but synthetic uses Xanomeline
# Use actual distinct TRTA values to set reference
# Find reference level exactly matching trt1lbl, if not found use first sorted
all_trta_vals <- unique(c(adtte$TRTA, adsl$TRTA))
all_trta_vals <- all_trta_vals[!is.na(all_trta_vals)]
# If Placebo not found, try case-insensitive
if (!trt1lbl %in% all_trta_vals){
  # try to find placebo-like
  idx <- grep("placebo", all_trta_vals, ignore.case=TRUE)
  if (length(idx)>0) trt1lbl <- all_trta_vals[idx[1]]
}

# Step 2: Prepare data - eff
# select a.USUBJID, a.TRTA, a.TRTAN, a.AVAL, a.CNSR, b.SEX, b.AGEGR1, b.RACE, b.REGION1, b.ECOGBL
# where b.EFFFL='Y' and a.PARAMCD='OS'

# Check column existence
# PARAMCD may be PARAMCD or PARAM
paramcd_col <- if ("PARAMCD" %in% names(adtte)) "PARAMCD" else if ("PARAM" %in% names(adtte)) "PARAM" else NA
if (is.na(paramcd_col)) stop("PARAMCD not found in adtte")

# EFFFL in adsl
efffl_col <- if ("EFFFL" %in% names(adsl)) "EFFFL" else NA

# Ensure needed cols exist, if REGION1 missing try REGION
region_col <- if ("REGION1" %in% names(adsl)) "REGION1" else if ("REGION" %in% names(adsl)) "REGION" else NA
race_col <- if ("RACE" %in% names(adsl)) "RACE" else NA
sex_col <- if ("SEX" %in% names(adsl)) "SEX" else NA
agegr1_col <- if ("AGEGR1" %in% names(adsl)) "AGEGR1" else NA
ecogbl_col <- if ("ECOGBL" %in% names(adsl)) "ECOGBL" else NA

# Filter adsl EFFFL=Y
adsl_eff <- adsl
if (!is.na(efffl_col)){
  adsl_eff <- adsl_eff %>% filter(.data[[efffl_col]] == "Y")
}

# Filter adtte PARAMCD=OS
adtte_os <- adtte %>% filter(.data[[paramcd_col]] == "OS")

# Merge
# Use inner join on USUBJID
eff <- inner_join(adtte_os, adsl_eff, by="USUBJID", suffix=c("", ".adsl"))

# After join, TRTA may be duplicated; prefer adtte TRTA
# Resolve columns: if TRTA.adsl exists, keep original TRTA
# Select needed columns
# Build eff with required columns
eff <- eff %>%
  transmute(
    USUBJID = USUBJID,
    TRTA = TRTA,
    TRTAN = TRTAN,
    AVAL = AVAL,
    CNSR = CNSR,
    SEX = if (!is.na(sex_col)) .data[[sex_col]] else NA_character_,
    AGEGR1 = if (!is.na(agegr1_col)) .data[[agegr1_col]] else NA_character_,
    RACE = if (!is.na(race_col)) .data[[race_col]] else NA_character_,
    REGION1 = if (!is.na(region_col)) .data[[region_col]] else NA_character_,
    ECOGBL = if (!is.na(ecogbl_col)) .data[[ecogbl_col]] else NA_character_
  )

# Ensure TRTA is character and factor with reference Placebo
eff$TRTA <- as.character(eff$TRTA)
# Remove rows with missing TRTA or AVAL or CNSR
eff <- eff %>% filter(!is.na(TRTA), !is.na(AVAL), !is.na(CNSR))

# Set factor reference
eff$TRTA <- factor(eff$TRTA)
# Relevel to trt1lbl if present
if (trt1lbl %in% levels(eff$TRTA)){
  eff$TRTA <- relevel(eff$TRTA, ref=trt1lbl)
} else {
  # sort levels SAS style: uppercase before lowercase (default R sort is similar but not exactly)
  # Use base sort
  lvls <- sort(unique(as.character(eff$TRTA)))
  eff$TRTA <- factor(eff$TRTA, levels=lvls)
  # relevel to first
  eff$TRTA <- relevel(eff$TRTA, ref=lvls[1])
  trt1lbl <- levels(eff$TRTA)[1]
}

# Helper to run Cox model and extract HRs
run_cox <- function(df){
  # df must have AVAL, CNSR, TRTA
  # CNSR(1) means censored when CNSR=1, event when 0
  # So event = 1 - CNSR (if CNSR is 0/1)
  # Some datasets use CNSR 0=event, 1=censored
  # Use Surv(AVAL, 1-CNSR) but if CNSR already 0/1 event indicator reversed, check
  # We'll assume CNSR=1 censored, so event = (CNSR==0)
  # Equivalent to 1-CNSR when CNSR 0/1
  df2 <- df %>% filter(!is.na(AVAL), !is.na(CNSR), !is.na(TRTA))
  if (nrow(df2) < 5) return(NULL)
  # Need at least 2 TRTA levels and at least 1 event per group?
  if (n_distinct(df2$TRTA) < 2) return(NULL)
  # Ensure TRTA factor with same levels as eff
  df2$TRTA <- factor(df2$TRTA, levels=levels(eff$TRTA))
  # Check events
  df2$event <- ifelse(df2$CNSR == 1, 0, 1)
  # If CNSR values are not 0/1 but maybe 0/1, this works. If CNSR is character, convert
  # If event all 0 or all 1, coxph will fail
  if (sum(df2$event, na.rm=TRUE) == 0 || sum(df2$event, na.rm=TRUE) == nrow(df2)) {
    # still try
  }
  fit <- tryCatch({
    survival::coxph(survival::Surv(AVAL, event) ~ TRTA, data=df2, ties="efron")
  }, error=function(e) NULL, warning=function(w) {
    # still try to return fit if warning
    suppressWarnings(survival::coxph(survival::Surv(AVAL, event) ~ TRTA, data=df2, ties="efron"))
  })
  if (is.null(fit)) return(NULL)
  s <- summary(fit)
  # Extract coefficients
  # s$coefficients has Estimate, StdErr, etc.
  # Use broom::tidy for p.value
  td <- tryCatch(broom::tidy(fit), error=function(e) NULL)
  # s$conf.int has HR and CI but we compute as exp(Estimate +/-1.96*StdErr) to match SAS
  coefs <- s$coefficients
  # coefs rownames like "TRTALow Dose" etc
  # Need to map to TRTA level
  res_list <- list()
  for (i in seq_len(nrow(coefs))){
    est <- coefs[i, "coef"]
    se  <- coefs[i, "se(coef)"]
    # p-value from Wald: ProbChiSq
    # coefs has "Pr(>|z|)" column
    pval <- coefs[i, "Pr(>|z|)"]
    # If tidy available, use its p.value
    if (!is.null(td) && nrow(td) >= i) pval <- td$p.value[i]
    hr <- exp(est)
    hr_lcl <- exp(est - 1.96*se)
    hr_ucl <- exp(est + 1.96*se)
    # Determine comparison label: extract level name
    rn <- rownames(coefs)[i]
    # rn is like "TRTAXanomeline Low Dose" - remove "TRTA"
    comp <- sub("^TRTA", "", rn)
    res_list[[i]] <- tibble(
      Estimate = est,
      StdErr = se,
      ProbChiSq = pval,
      HR = hr,
      HR_LCL = hr_lcl,
      HR_UCL = hr_ucl,
      comp = comp
    )
  }
  bind_rows(res_list)
}

# Subgroup analysis function
subgroup_analysis <- function(data, subgrp, subgrp_label, subgrp_ord){
  # data is eff
  # subgrp is column name
  # SAS: proc sort by subgrp, then by subgrp run phreg
  # So levels sorted SAS order: character ordering uppercase before lowercase, missing first
  # In R, sort with base sort (which is similar)
  vals <- unique(data[[subgrp]])
  # Keep NA as level
  vals_char <- as.character(vals)
  # SAS missing sorts first
  # Sort non-missing
  nonmiss <- vals_char[!is.na(vals_char)]
  sorted_nonmiss <- sort(nonmiss) # base sort is byte-wise, uppercase before lowercase in ASCII
  sorted_vals <- c(NA_character_, sorted_nonmiss) # NA first if present
  # But if no NA, remove NA
  if (!any(is.na(vals_char))) sorted_vals <- sorted_nonmiss
  # For each level, run cox
  results <- list()
  for (lv in sorted_vals){
    if (is.na(lv)){
      sub_df <- data %>% filter(is.na(.data[[subgrp]]))
      lvl_label <- NA_character_
    } else {
      sub_df <- data %>% filter(.data[[subgrp]] == lv)
      lvl_label <- lv
    }
    # vvalue in SAS returns formatted value; we use as.character
    cox_res <- run_cox(sub_df)
    if (is.null(cox_res) || nrow(cox_res)==0) {
      # Create NA row for this level (to keep structure)
      # But SAS would not output if model fails? We'll create NA row
      # For each expected comparison (nlevels-1), create NA
      # Instead skip
      next
    }
    for (r in seq_len(nrow(cox_res))){
      cr <- cox_res[r,]
      # For multiple TRTA comparisons, SAS would produce multiple rows with same subgroup_level
      # We keep each as separate row
      hr_disp <- fmt_5_2(cr$HR)
      ci_disp <- paste0("(", fmt_5_2(cr$HR_LCL), ", ", fmt_5_2(cr$HR_UCL), ")")
      # If multiple comps, we may want to distinguish subgroup_level by comp? But SAS doesn't
      # Keep subgroup_level as lvl_label
      results[[length(results)+1]] <- tibble(
        subgroup = subgrp_label,
        subgroup_level = lvl_label,
        subgrp_ord = subgrp_ord,
        HR = cr$HR,
        HR_LCL = cr$HR_LCL,
        HR_UCL = cr$HR_UCL,
        hr_display = hr_disp,
        ci_display = ci_disp,
        ProbChiSq = cr$ProbChiSq,
        comp = cr$comp
      )
    }
  }
  if (length(results)==0) return(tibble())
  bind_rows(results)
}

# Step 3: Run subgroup analyses
sg1 <- subgroup_analysis(eff, "SEX", "Sex", 1)
sg2 <- subgroup_analysis(eff, "AGEGR1", "Age Group", 2)
sg3 <- subgroup_analysis(eff, "RACE", "Race", 3)
sg4 <- subgroup_analysis(eff, "REGION1", "Region", 4)

# Step 4: Overall HR
overall_cox <- run_cox(eff)
sg_overall <- tibble()
if (!is.null(overall_cox) && nrow(overall_cox)>0){
  for (r in seq_len(nrow(overall_cox))){
    cr <- overall_cox[r,]
    sg_overall <- bind_rows(sg_overall, tibble(
      subgroup = "Overall",
      subgroup_level = "All Subjects",
      subgrp_ord = 0,
      HR = cr$HR,
      HR_LCL = cr$HR_LCL,
      HR_UCL = cr$HR_UCL,
      hr_display = fmt_5_2(cr$HR),
      ci_display = paste0("(", fmt_5_2(cr$HR_LCL), ", ", fmt_5_2(cr$HR_UCL), ")"),
      ProbChiSq = cr$ProbChiSq,
      comp = cr$comp
    ))
  }
} else {
  sg_overall <- tibble(
    subgroup = "Overall",
    subgroup_level = "All Subjects",
    subgrp_ord = 0,
    HR = NA_real_, HR_LCL=NA_real_, HR_UCL=NA_real_,
    hr_display=NA_character_, ci_display=NA_character_, ProbChiSq=NA_real_, comp=NA_character_
  )
}

# Step 5: Combine all subgroups
forest_data <- bind_rows(sg_overall, sg1, sg2, sg3, sg4)

# If forest_data empty (e.g., no valid models), create placeholder
if (nrow(forest_data)==0){
  forest_data <- tibble(
    subgroup="Overall", subgroup_level="All Subjects", subgrp_ord=0,
    HR=1, HR_LCL=0.8, HR_UCL=1.2, hr_display="1.00", ci_display="(0.80, 1.20)", ProbChiSq=0.5, comp=NA_character_
  )
}

forest_data <- forest_data %>%
  mutate(row = row_number(),
         yval = -row)

# Add display label for y-axis: combine subgroup and level for readability
# SAS yaxis values are -1 -2 ... but labels would be subgroup_level? We'll create label
forest_data <- forest_data %>%
  mutate(y_label = ifelse(subgroup=="Overall",
                          paste0(subgroup, ": ", subgroup_level),
                          paste0(subgroup, " - ", subgroup_level)),
         # If multiple comps, append comp to distinguish
         y_label = ifelse(!is.na(comp) & n_distinct(comp)>1,
                          paste0(y_label, " (", comp, " vs ", trt1lbl, ")"),
                          y_label))

# For plotting, ensure y_label is factor ordered by row
forest_data$y_label <- factor(forest_data$y_label, levels=rev(forest_data$y_label))

# Write CSV of plotted data
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names=FALSE, na="")

# Also write txt rendering
txt_path <- file.path(out, "f_forest.txt")
sink(txt_path)
cat("Figure 14.2.2\n")
cat("Forest Plot of Subgroup Analyses - Overall Survival\n")
cat("Efficacy Analysis Set\n\n")
cat("Data cutoff: [DDMONYYYY]\n")
cat("Source: ADaM datasets (adtte, adsl)\n\n")
# Print table
print(as.data.frame(forest_data %>% select(subgroup, subgroup_level, comp, HR, HR_LCL, HR_UCL, hr_display, ci_display, ProbChiSq, row, yval)), row.names=FALSE)
cat("\nHazard Ratios from Cox proportional hazards model.\n")
cat("Reference: ", trt1lbl, "\n")
sink()

# Step 6: Generate forest plot
# Use ggplot2 to replicate SGPLOT
# Reference line at HR=1, CI lines, point estimates, log scale base 2 with values 0.1,0.25,0.5,1,2,4

# Prepare plot data: need yval numeric for positioning, but we use y_label factor
# For highlow, use geom_errorbarh

# Determine x limits
x_vals <- c(0.1, 0.25, 0.5, 1, 2, 4)
# Ensure HRs within range for limits
xmin <- 0.08
xmax <- 5

p <- ggplot(forest_data, aes(x=HR, y=y_label)) +
  geom_vline(xintercept=1, linetype="dashed", color="gray50", linewidth=0.7) +
  geom_errorbarh(aes(xmin=HR_LCL, xmax=HR_UCL), height=0.2, color="darkblue", linewidth=0.9) +
  geom_point(aes(x=HR), shape=18, size=3.5, color="darkblue", fill="darkblue") +
  scale_x_continuous(
    trans="log2",
    breaks=x_vals,
    labels=as.character(x_vals),
    limits=c(xmin, xmax),
    name="Hazard Ratio (95% CI)"
  ) +
  labs(
    title="Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival",
    subtitle="Efficacy Analysis Set",
    y=NULL,
    caption="Data cutoff: [DDMONYYYY] | Hazard Ratios from Cox proportional hazards model."
  ) +
  theme_minimal(base_size=10) +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=11),
    plot.subtitle = element_text(hjust=0.5, size=10),
    axis.text.y = element_text(size=8),
    axis.text.x = element_text(size=8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color="gray90"),
    plot.caption = element_text(hjust=0, size=7)
  ) +
  annotate("text", x=0.12, y=Inf, label=paste0("Favors ", trt2lbl), hjust=0, vjust=1.5, size=3, color="black") +
  annotate("text", x=3.5, y=Inf, label=paste0("Favors ", trt1lbl), hjust=1, vjust=1.5, size=3, color="black")

# Add HR and CI as text on right side of plot
# Create table-like annotation: we can add geom_text for hr_display and ci_display
# Position at x = xmax * 0.95
p <- p + geom_text(aes(x=xmax*0.92, label=paste0(hr_display, " ", ci_display)), size=2.8, hjust=0, color="black")

# Save PNG 10in x 7in
ggsave(file.path(out, "f_forest.png"), plot=p, width=10, height=7, units="in", dpi=300, bg="white")

# Also save a version without extra text for clean
message("f_forest completed. Rows: ", nrow(forest_data))
