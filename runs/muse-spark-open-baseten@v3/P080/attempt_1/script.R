# P080_f_forest.sas -> R translation
# Figure 14.2.2 Forest Plot of Subgroup Analyses - Overall Survival

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
tlfpath  <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"

trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
trt3lbl <- "[Drug] High Dose"
cutdate <- "[DDMONYYYY]"

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  # for NA keep NA
  res <- sign_x * floor(abs(x) * mult + 0.5) / mult
  res[is.na(x)] <- NA_real_
  res
}
fmt_5.2 <- function(x) {
  if (is.na(x)) return("")
  # SAS put(x,5.2) rounds half away from zero
  y <- round_half_up(x, 2)
  # strip leading blanks
  trimws(sprintf("%5.2f", y))
}

# helper to read adam dataset (sas7bdat or xpt)
read_adam <- function(name, libpath) {
  f1 <- file.path(libpath, paste0(name, ".sas7bdat"))
  f2 <- file.path(libpath, paste0(name, ".sas7bdat"))
  f3 <- file.path(libpath, paste0(name, ".xpt"))
  f4 <- file.path(libpath, paste0(tolower(name), ".sas7bdat"))
  f5 <- file.path(libpath, paste0(tolower(name), ".xpt"))
  f6 <- file.path(libpath, paste0(toupper(name), ".sas7bdat"))
  f7 <- file.path(libpath, paste0(toupper(name), ".xpt"))
  candidates <- c(f1,f2,f3,f4,f5,f6,f7)
  # also list files case-insensitive
  if (dir.exists(libpath)) {
    allf <- list.files(libpath, full.names = TRUE)
    # find matching base name
    idx <- tolower(basename(tools::file_path_sans_ext(allf))) == tolower(name)
    if (any(idx)) candidates <- c(allf[idx], candidates)
  }
  for (f in candidates) {
    if (!is.na(f) && file.exists(f)) {
      ext <- tolower(tools::file_ext(f))
      if (ext == "xpt") {
        df <- haven::read_xpt(f)
        return(as.data.frame(df, stringsAsFactors = FALSE))
      } else if (ext == "sas7bdat") {
        df <- haven::read_sas(f)
        return(as.data.frame(df, stringsAsFactors = FALSE))
      }
    }
  }
  stop(paste0("Dataset not found: ", name, " in ", libpath))
}

# Load data
adsl  <- read_adam("adsl", adampath)
adtte <- read_adam("adtte", adampath)

# Ensure character columns are character (haven labelled -> convert)
to_char <- function(x) {
  if (haven::is.labelled(x)) x <- haven::as_factor(x, levels = "labels")
  as.character(x)
}
# Convert relevant columns if present
for (cn in intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","CNSR","EFFFL","SEX","AGEGR1","RACE","REGION1","ECOGBL"), names(adsl))) {
  # keep numeric for TRTAN etc but convert labelled
  if (is.numeric(adsl[[cn]]) && haven::is.labelled(adsl[[cn]])) {
    # keep numeric but also keep label? leave numeric
  } else {
    # try to keep as is
  }
}
# adtte columns
adtte$USUBJID <- to_char(adtte$USUBJID)
adtte$TRTA    <- to_char(adtte$TRTA)
adtte$PARAMCD <- to_char(adtte$PARAMCD)
if ("CNSR" %in% names(adtte)) {
  # CNSR may be numeric
  # keep numeric
}
adsl$USUBJID <- to_char(adsl$USUBJID)
adsl$TRTA    <- to_char(adsl$TRTA)
if ("EFFFL" %in% names(adsl)) adsl$EFFFL <- to_char(adsl$EFFFL)
if ("SEX" %in% names(adsl)) adsl$SEX <- to_char(adsl$SEX)
if ("AGEGR1" %in% names(adsl)) adsl$AGEGR1 <- to_char(adsl$AGEGR1)
if ("RACE" %in% names(adsl)) adsl$RACE <- to_char(adsl$RACE)
if ("REGION1" %in% names(adsl)) adsl$REGION1 <- to_char(adsl$REGION1)
if ("ECOGBL" %in% names(adsl)) adsl$ECOGBL <- to_char(adsl$ECOGBL)

# Step 2: Prepare eff = adtte join adsl where EFFFL='Y' and PARAMCD='OS'
# Drop/rename overlapping columns except keys to avoid .x/.y
# Keep from adtte: USUBJID, TRTA, TRTAN, AVAL, CNSR, PARAMCD
# From adsl: USUBJID, SEX, AGEGR1, RACE, REGION1, ECOGBL, EFFFL
adsl_sub <- adsl[, intersect(c("USUBJID","SEX","AGEGR1","RACE","REGION1","ECOGBL","EFFFL"), names(adsl)), drop = FALSE]
adtte_sub <- adtte[, intersect(c("USUBJID","TRTA","TRTAN","AVAL","CNSR","PARAMCD"), names(adtte)), drop = FALSE]

eff <- merge(adtte_sub, adsl_sub, by = "USUBJID", all = FALSE)
# filter
eff <- eff[eff$EFFFL == "Y" & eff$PARAMCD == "OS", , drop = FALSE]
stopifnot(nrow(eff) > 0)
stopifnot(all(c("AVAL","CNSR","TRTA") %in% names(eff)))

# Ensure TRTA factor with reference Placebo (SAS ref="&trt1lbl.")
# SAS sorts character ordering uppercase before lowercase; factor levels in table order
# For Cox model we set ref explicitly
eff$TRTA <- factor(eff$TRTA, levels = unique(c(trt1lbl, sort(unique(eff$TRTA)))) )
# Ensure Placebo is first level (reference)
if (trt1lbl %in% levels(eff$TRTA)) {
  eff$TRTA <- relevel(eff$TRTA, ref = trt1lbl)
}

# Helper to run Cox within each subgroup level
run_subgroup <- function(data, subgrp_var, subgrp_label, subgrp_ord) {
  # data must contain subgrp_var, TRTA, AVAL, CNSR
  if (!subgrp_var %in% names(data)) return(NULL)
  # remove rows with missing subgroup or TRTA or AVAL/CNSR
  d <- data[!is.na(data[[subgrp_var]]) & data[[subgrp_var]] != "" & !is.na(data$TRTA) & !is.na(data$AVAL) & !is.na(data$CNSR), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  # SAS proc sort by subgroup
  d <- d[order(d[[subgrp_var]]), , drop = FALSE]
  levs <- unique(d[[subgrp_var]])
  # SAS character ordering: sort with uppercase before lowercase (default R sort is similar for ASCII)
  levs <- sort(levs)
  out_list <- list()
  for (lv in levs) {
    subd <- d[d[[subgrp_var]] == lv, , drop = FALSE]
    # need at least 2 treatment levels and at least one event
    if (length(unique(subd$TRTA)) < 2) next
    # ensure TRTA factor has correct ref and drop unused levels
    subd$TRTA <- factor(subd$TRTA)
    # relevel to Placebo if present
    if (trt1lbl %in% levels(subd$TRTA)) subd$TRTA <- relevel(subd$TRTA, ref = trt1lbl)
    # run coxph
    fit <- tryCatch({
      survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = subd, ties = "breslow")
    }, error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)
    # s$coefficients has rows for each non-reference TRTA level
    # s$conf.int also
    coefs <- s$coefficients
    # rownames like "TRTALow Dose" or "TRTA[Drug] Low Dose"
    for (i in seq_len(nrow(coefs))) {
      est <- coefs[i, "coef"]
      se  <- coefs[i, "se(coef)"]
      pval <- coefs[i, "Pr(>|z|)"]
      # HR and CI using 1.96*SE (SAS)
      HR <- exp(est)
      HR_LCL <- exp(est - 1.96 * se)
      HR_UCL <- exp(est + 1.96 * se)
      # comparison label from rownames
      rn <- rownames(coefs)[i]
      # extract level name after TRTA
      comp <- sub("^TRTA", "", rn)
      # If multiple TRTA levels, we need to distinguish; SAS would have duplicate subgroup_level
      # We append comparison to keep uniqueness but also keep original SAS behavior
      # For single non-reference level, comp is the active arm
      subgroup_level_raw <- lv
      # If more than one coefficient, make level distinct
      if (nrow(coefs) > 1) {
        subgroup_level <- paste0(lv, " (", comp, " vs ", trt1lbl, ")")
      } else {
        subgroup_level <- subgroup_level_raw
      }
      hr_display <- fmt_5.2(HR)
      ci_display <- paste0("(", fmt_5.2(HR_LCL), ", ", fmt_5.2(HR_UCL), ")")
      out_list[[length(out_list)+1]] <- data.frame(
        subgroup = subgrp_label,
        subgroup_level = subgroup_level,
        subgrp_ord = subgrp_ord,
        HR = HR,
        HR_LCL = HR_LCL,
        HR_UCL = HR_UCL,
        hr_display = hr_display,
        ci_display = ci_display,
        ProbChiSq = pval,
        comparison = comp,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(out_list) == 0) return(NULL)
  do.call(rbind, out_list)
}

# Step 3: Run subgroup analyses
sg1 <- run_subgroup(eff, "SEX", "Sex", 1)
sg2 <- run_subgroup(eff, "AGEGR1", "Age Group", 2)
sg3 <- run_subgroup(eff, "RACE", "Race", 3)
sg4 <- run_subgroup(eff, "REGION1", "Region", 4)

# Step 4: Overall HR
overall_df <- NULL
{
  subd <- eff
  subd$TRTA <- factor(subd$TRTA)
  if (trt1lbl %in% levels(subd$TRTA)) subd$TRTA <- relevel(subd$TRTA, ref = trt1lbl)
  fit <- survival::coxph(survival::Surv(AVAL, 1 - CNSR) ~ TRTA, data = subd, ties = "breslow")
  s <- summary(fit)
  coefs <- s$coefficients
  for (i in seq_len(nrow(coefs))) {
    est <- coefs[i, "coef"]
    se  <- coefs[i, "se(coef)"]
    pval <- coefs[i, "Pr(>|z|)"]
    HR <- exp(est)
    HR_LCL <- exp(est - 1.96 * se)
    HR_UCL <- exp(est + 1.96 * se)
    rn <- rownames(coefs)[i]
    comp <- sub("^TRTA", "", rn)
    subgroup_level <- "All Subjects"
    if (nrow(coefs) > 1) subgroup_level <- paste0("All Subjects (", comp, " vs ", trt1lbl, ")")
    hr_display <- fmt_5.2(HR)
    ci_display <- paste0("(", fmt_5.2(HR_LCL), ", ", fmt_5.2(HR_UCL), ")")
    overall_df <- rbind(overall_df, data.frame(
      subgroup = "Overall",
      subgroup_level = subgroup_level,
      subgrp_ord = 0,
      HR = HR,
      HR_LCL = HR_LCL,
      HR_UCL = HR_UCL,
      hr_display = hr_display,
      ci_display = ci_display,
      ProbChiSq = pval,
      comparison = comp,
      stringsAsFactors = FALSE
    ))
  }
}

# Step 5: Combine all subgroups
forest_data <- do.call(rbind, c(list(overall_df), list(sg1), list(sg2), list(sg3), list(sg4)))
# Remove NULLs already handled
forest_data$row <- seq_len(nrow(forest_data))
forest_data$yval <- -forest_data$row

# Add display label for plotting
forest_data$label <- ifelse(forest_data$subgroup == "Overall",
                            forest_data$subgroup_level,
                            paste0(forest_data$subgroup, ": ", forest_data$subgroup_level))
# Order factor for y axis top-to-bottom (yval negative)
forest_data$label <- factor(forest_data$label, levels = rev(forest_data$label))

# Write CSV of plotted data
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

# Also write txt rendering
txt_file <- file.path(out, "f_forest.txt")
con <- file(txt_file, open = "wt")
writeLines(c(
  paste0("Figure 14.2.2 Forest Plot of Subgroup Analyses - Overall Survival"),
  paste0("Population: Efficacy Analysis Set"),
  paste0("Data cutoff: ", cutdate),
  "",
  "Hazard Ratios from Cox proportional hazards model (TRTA reference = Placebo)",
  paste0("Model: AVAL * CNSR(1) = TRTA"),
  ""
), con)
# table header
hdr <- sprintf("%-30s %-40s %8s %20s %10s", "Subgroup", "Level", "HR", "95% CI", "p-value")
writeLines(hdr, con)
writeLines(paste(rep("-", nchar(hdr)), collapse=""), con)
for (i in seq_len(nrow(forest_data))) {
  r <- forest_data[i,]
  line <- sprintf("%-30s %-40s %8s %20s %10.4f",
                  substr(r$subgroup,1,30),
                  substr(r$subgroup_level,1,40),
                  r$hr_display,
                  r$ci_display,
                  r$ProbChiSq)
  writeLines(line, con)
}
writeLines(c("", "Reference line at HR=1 (dashed).", "Hazard Ratios from Cox proportional hazards model."), con)
close(con)

# Step 6: Generate forest plot
# Prepare data for ggplot
library(ggplot2)

# For x-axis log scale with specified breaks
x_breaks <- c(0.1, 0.25, 0.5, 1, 2, 4)

p <- ggplot(forest_data, aes(y = label, x = HR)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL), height = 0.2, color = "darkblue", linewidth = 0.8) +
  geom_point(color = "darkblue", fill = "darkblue", shape = 18, size = 3) +
  scale_x_continuous(
    trans = "log2",
    breaks = x_breaks,
    labels = as.character(x_breaks),
    limits = c(0.08, 5)
  ) +
  labs(
    title = "Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival",
    subtitle = "Efficacy Analysis Set",
    x = "Hazard Ratio (95% CI)",
    y = NULL,
    caption = paste0("Data cutoff: ", cutdate, " | Hazard Ratios from Cox proportional hazards model. | Favors ", trt2lbl, " (left) vs Favors ", trt1lbl, " (right)")
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(color = "gray90"),
    plot.caption = element_text(hjust = 0, size = 7)
  ) +
  # add HR and CI as text on right side
  geom_text(aes(label = paste0(hr_display, " ", ci_display)), x = 4.2, hjust = 0, size = 2.8, color = "black") +
  annotate("text", x = 0.12, y = Inf, label = paste0("Favors ", trt2lbl), vjust = -1, hjust = 0, size = 3, color = "black") +
  annotate("text", x = 4, y = Inf, label = paste0("Favors ", trt1lbl), vjust = -1, hjust = 1, size = 3, color = "black")

# Save PNG
ggsave(file.path(out, "f_forest.png"), plot = p, width = 10, height = 7, units = "in", dpi = 300)

# Also ensure no leftover work datasets (not needed in R)
