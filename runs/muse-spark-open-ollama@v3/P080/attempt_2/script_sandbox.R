out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "output"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  abs_x <- abs(x) * mult
  rounded <- floor(abs_x + 0.5 + 1e-12) / mult
  sign_x * rounded
}
sas_put <- function(x, w = 5, d = 2) {
  if (is.na(x)) return("")
  v <- round_half_up(x, d)
  fmt <- paste0("%", w, ".", d, "f")
  trimws(sprintf(fmt, v))
}

# locate adam files (bench uses /bench, SAS spec uses D:/...)
candidate_dirs <- c(
  "/bench/data/raw/own-synthetic/adam",
  "/bench/data/raw/own-synthetic/adam",
  "/bench/data/raw/cdisc-pilot/adam",
  "/bench/data/raw/cdisc-pilot/adam"
)
find_adam_file <- function(name) {
  for (d in candidate_dirs) {
    if (!dir.exists(d)) next
    files <- list.files(d, full.names = TRUE, recursive = TRUE)
    idx <- grep(paste0("^", name, "\\."), basename(files), ignore.case = TRUE)
    if (length(idx) > 0) return(files[idx[1]])
  }
  return(NA)
}
adsl_path  <- find_adam_file("adsl")
adtte_path <- find_adam_file("adtte")

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(survival)
library(ggplot2)

read_sas_file <- function(path) {
  if (is.na(path)) stop(paste("File not found:", path))
  ext <- tolower(tools::file_ext(path))
  if (ext == "sas7bdat") haven::read_sas(path) else haven::read_xpt(path)
}

adsl  <- read_sas_file(adsl_path)
adtte <- read_sas_file(adtte_path)

# convert labelled to character (keep values)
adsl2  <- adsl  %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))
adtte2 <- adtte %>% mutate(across(where(haven::is.labelled), haven::as_factor)) %>% mutate(across(where(is.factor), as.character))

trt1lbl <- "Placebo"

# Step 2: Prepare data - replicate SAS proc sql exactly
# select a.USUBJID, a.TRTA, a.TRTAN, a.AVAL, a.CNSR, b.SEX, b.AGEGR1, b.RACE, b.REGION1, b.ECOGBL
# from adam.adtte a inner join adam.adsl b on a.USUBJID=b.USUBJID where b.EFFFL='Y' and a.PARAMCD='OS'
# adtte already contains SEX etc, so select only needed cols from adtte to avoid .x/.y suffixes
eff <- adtte2 %>%
  filter(PARAMCD == "OS") %>%
  select(USUBJID, TRTA, TRTAN, AVAL, CNSR) %>%
  inner_join(
    adsl2 %>% filter(EFFFL == "Y") %>% select(USUBJID, SEX, AGEGR1, RACE, REGION1, ECOGBL),
    by = "USUBJID"
  )

# helper to run Cox per subgroup level, returning one row per non-reference TRTA
run_cox_by <- function(data, by_var, ref = trt1lbl) {
  levs <- sort(unique(data[[by_var]]))
  levs <- levs[!is.na(levs)]
  all_trt_levels <- sort(unique(data$TRTA))
  # ensure ref first
  trt_levels_ordered <- c(ref, setdiff(all_trt_levels, ref))
  results <- list()
  for (lv in levs) {
    sub <- data %>% filter(.data[[by_var]] == lv)
    if (nrow(sub) == 0) next
    sub$TRTA_f <- factor(sub$TRTA, levels = trt_levels_ordered)
    if (length(unique(na.omit(sub$TRTA_f))) < 2) next
    sub$event <- ifelse(sub$CNSR == 1, 0, 1)
    if (sum(sub$event, na.rm = TRUE) == 0) next
    fit <- tryCatch(survival::coxph(survival::Surv(AVAL, event) ~ TRTA_f, data = sub, ties = "breslow"),
                    error = function(e) NULL)
    if (is.null(fit)) next
    s <- summary(fit)
    coefs <- s$coefficients
    if (is.null(coefs) || nrow(coefs) == 0) next
    for (i in seq_len(nrow(coefs))) {
      coef_name <- rownames(coefs)[i]
      trt_level <- sub("^TRTA_f", "", coef_name)
      est  <- coefs[i, "coef"]
      se   <- coefs[i, "se(coef)"]
      pval <- coefs[i, "Pr(>|z|)"]
      hr <- exp(est)
      hr_lcl <- exp(est - 1.96 * se)
      hr_ucl <- exp(est + 1.96 * se)
      results[[length(results)+1]] <- tibble(
        subgroup_level = lv,
        trt_comparison = trt_level,
        Estimate = est, StdErr = se, ProbChiSq = pval,
        HR = hr, HR_LCL = hr_lcl, HR_UCL = hr_ucl,
        hr_display = sas_put(hr, 5, 2),
        ci_display = paste0("(", sas_put(hr_lcl, 5, 2), ", ", sas_put(hr_ucl, 5, 2), ")")
      )
    }
  }
  if (length(results)==0) return(NULL)
  bind_rows(results)
}

run_cox_overall <- function(data, ref = trt1lbl) {
  all_trt_levels <- sort(unique(data$TRTA))
  trt_levels_ordered <- c(ref, setdiff(all_trt_levels, ref))
  data$TRTA_f <- factor(data$TRTA, levels = trt_levels_ordered)
  data$event <- ifelse(data$CNSR == 1, 0, 1)
  fit <- survival::coxph(survival::Surv(AVAL, event) ~ TRTA_f, data = data, ties = "breslow")
  s <- summary(fit)
  coefs <- s$coefficients
  res <- list()
  for (i in seq_len(nrow(coefs))) {
    coef_name <- rownames(coefs)[i]
    trt_level <- sub("^TRTA_f", "", coef_name)
    est  <- coefs[i, "coef"]
    se   <- coefs[i, "se(coef)"]
    pval <- coefs[i, "Pr(>|z|)"]
    hr <- exp(est)
    hr_lcl <- exp(est - 1.96 * se)
    hr_ucl <- exp(est + 1.96 * se)
    res[[length(res)+1]] <- tibble(
      subgroup_level = "All Subjects",
      trt_comparison = trt_level,
      Estimate = est, StdErr = se, ProbChiSq = pval,
      HR = hr, HR_LCL = hr_lcl, HR_UCL = hr_ucl,
      hr_display = sas_put(hr, 5, 2),
      ci_display = paste0("(", sas_put(hr_lcl, 5, 2), ", ", sas_put(hr_ucl, 5, 2), ")")
    )
  }
  bind_rows(res)
}

subgroup_defs <- list(
  list(var="SEX",     label="Sex",       ord=1),
  list(var="AGEGR1",  label="Age Group", ord=2),
  list(var="RACE",    label="Race",      ord=3),
  list(var="REGION1", label="Region",    ord=4)
)

forest_list <- list()
overall_res <- run_cox_overall(eff, ref = trt1lbl)
for (i in seq_len(nrow(overall_res))) {
  forest_list[[length(forest_list)+1]] <- tibble(
    subgroup = "Overall", subgroup_level = overall_res$subgroup_level[i], subgrp_ord = 0,
    trt_comparison = overall_res$trt_comparison[i],
    HR = overall_res$HR[i], HR_LCL = overall_res$HR_LCL[i], HR_UCL = overall_res$HR_UCL[i],
    hr_display = overall_res$hr_display[i], ci_display = overall_res$ci_display[i],
    ProbChiSq = overall_res$ProbChiSq[i], Estimate = overall_res$Estimate[i], StdErr = overall_res$StdErr[i]
  )
}
for (def in subgroup_defs) {
  res <- run_cox_by(eff, def$var, ref = trt1lbl)
  if (is.null(res)) next
  for (i in seq_len(nrow(res))) {
    forest_list[[length(forest_list)+1]] <- tibble(
      subgroup = def$label, subgroup_level = res$subgroup_level[i], subgrp_ord = def$ord,
      trt_comparison = res$trt_comparison[i],
      HR = res$HR[i], HR_LCL = res$HR_LCL[i], HR_UCL = res$HR_UCL[i],
      hr_display = res$hr_display[i], ci_display = res$ci_display[i],
      ProbChiSq = res$ProbChiSq[i], Estimate = res$Estimate[i], StdErr = res$StdErr[i]
    )
  }
}
forest_data <- bind_rows(forest_list) %>%
  mutate(row = row_number(), yval = -row_number())

# add N per subgroup level for CSV
compute_n <- function(data, var, level) {
  sub <- data %>% filter(.data[[var]] == level)
  c(n = nrow(sub), events = sum(sub$CNSR == 0, na.rm=TRUE))
}
overall_n <- nrow(eff)
overall_events <- sum(eff$CNSR == 0, na.rm=TRUE)
forest_data <- forest_data %>%
  rowwise() %>%
  mutate(
    N = if (subgroup == "Overall") overall_n else {
      var <- dplyr::case_when(subgroup=="Sex"~"SEX", subgroup=="Age Group"~"AGEGR1", subgroup=="Race"~"RACE", subgroup=="Region"~"REGION1", TRUE~NA_character_)
      as.integer(compute_n(eff, var, subgroup_level)["n"])
    },
    n_events = if (subgroup == "Overall") overall_events else {
      var <- dplyr::case_when(subgroup=="Sex"~"SEX", subgroup=="Age Group"~"AGEGR1", subgroup=="Race"~"RACE", subgroup=="Region"~"REGION1", TRUE~NA_character_)
      as.integer(compute_n(eff, var, subgroup_level)["events"])
    }
  ) %>% ungroup()

# y labels
forest_data <- forest_data %>%
  mutate(
    y_label = ifelse(subgroup=="Overall", paste0(subgroup, ": ", subgroup_level),
                     paste0(subgroup, " - ", subgroup_level)),
    y_label_full = ifelse(length(unique(trt_comparison)) > 1,
                          paste0(y_label, " (", trt_comparison, " vs ", trt1lbl, ")"),
                          y_label)
  )
# order factor top-to-bottom (row 1 at top)
forest_data$y_factor <- factor(forest_data$y_label_full, levels = rev(forest_data$y_label_full))

# plot ------------------------------------------------------------------
# colors for up to 2 active arms
cols <- c("Drug X Low Dose" = "darkblue", "Drug X High Dose" = "darkred",
          "[Drug] Low Dose" = "darkblue", "[Drug] High Dose" = "darkred")

p <- ggplot(forest_data, aes(x = HR, y = y_factor)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray60") +
  geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL, color = trt_comparison),
                 height = 0.2, linewidth = 0.8, show.legend = FALSE) +
  geom_point(aes(color = trt_comparison), size = 3, shape = 18) +
  scale_x_continuous(
    trans = "log2",
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
    limits = c(0.1, 4),
    labels = c("0.1","0.25","0.5","1","2","4")
  ) +
  scale_color_manual(values = cols, name = "Comparison") +
  labs(
    title = "Figure 14.2.2",
    subtitle = "Forest Plot of Subgroup Analyses - Overall Survival\nEfficacy Analysis Set",
    x = "Hazard Ratio (95% CI)",
    y = "",
    caption = "Data cutoff: [DDMONYYYY]\nHazard Ratios from Cox proportional hazards model.\nProgram: P080_f_forest.sas"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.text.y = element_text(size = 7),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 7)
  ) +
  coord_cartesian(clip = "off", xlim = c(0.1, 4)) +
  annotate("text", x = 0.35, y = 0.6, label = "Favors Drug X Low Dose", size = 3, hjust = 0.5) +
  annotate("text", x = 2.2, y = 0.6, label = "Favors Placebo", size = 3, hjust = 0.5)

# add HR text on right side using geom_text with x outside limits via annotation
# use secondary annotation: create label column
forest_data$label_hr <- paste0(forest_data$hr_display, " ", forest_data$ci_display)
p <- p + geom_text(data = forest_data,
                   aes(x = 4.1, y = y_factor, label = label_hr),
                   hjust = 0, size = 2.8, inherit.aes = FALSE)

ggsave(file.path(out, "f_forest.png"), plot = p, width = 10, height = 7, units = "in", dpi = 300)

# CSV of plotted data
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

# TXT rendering
txt_lines <- c(
  "Figure 14.2.2",
  "Forest Plot of Subgroup Analyses - Overall Survival",
  "Efficacy Analysis Set",
  "",
  "Data cutoff: [DDMONYYYY]",
  "Hazard Ratios from Cox proportional hazards model.",
  "",
  sprintf("%-20s %-25s %-25s %8s %20s %10s %6s", "Subgroup","Level","Comparison","HR","95% CI","p-value","N"),
  paste(rep("-", 120), collapse = "")
)
for (i in seq_len(nrow(forest_data))) {
  r <- forest_data[i,]
  txt_lines <- c(txt_lines,
    sprintf("%-20s %-25s %-25s %8s %20s %10.4f %6d",
            r$subgroup, r$subgroup_level, r$trt_comparison, r$hr_display, r$ci_display, r$ProbChiSq, r$N)
  )
}
writeLines(txt_lines, file.path(out, "f_forest.txt"))
