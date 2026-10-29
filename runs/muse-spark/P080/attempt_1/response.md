```r
# P080 f_forest.sas -> R translation
# Figure 14.2.2 Forest Plot of Subgroup Analyses - Overall Survival
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(data.table)
  library(ggplot2)
  library(survival)
})

cat("P080 forest plot: starting\n")

# ---- helpers: SAS semantics ----
# SAS rounds half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_fmt_52 <- function(x) {
  # PUT(x,5.2) then STRIP: 2 decimals, half-up
  ifelse(is.na(x), "",
    sprintf("%.2f", sas_round(x, 2)))
}
sas_sort_char <- function(v) {
  # SAS: missing (NA/"") first, then ASCII (uppercase before lowercase)
  uv <- unique(v)
  ismiss <- is.na(uv) | uv == ""
  miss_vals <- uv[ismiss]
  nonmiss <- uv[!ismiss]
  if (length(nonmiss) > 0) {
    # radix sort is byte-wise ~ ASCII
    ord <- order(nonmiss, method = "radix")
    nonmiss <- nonmiss[ord]
  }
  c(miss_vals, nonmiss)
}

trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
trt3lbl <- "[Drug] High Dose"

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_adam <- function(name) {
  # try .xpt then .sas7bdat then .csv
  f1 <- file.path(adam_path, paste0(name, ".xpt"))
  f2 <- file.path(adam_path, paste0(name, ".sas7bdat"))
  f3 <- file.path(adam_path, paste0(name, ".csv"))
  # also list case-insensitive
  if (file.exists(f1)) return(as.data.frame(haven::read_xpt(f1)))
  if (file.exists(f2)) return(as.data.frame(haven::read_sas(f2)))
  if (file.exists(f3)) return(as.data.frame(readr::read_csv(f3, show_col_types = FALSE)))
  # fallback: list files
  allf <- list.files(adam_path, full.names = TRUE)
  cand <- allf[tolower(tools::file_path_sans_ext(basename(allf))) == tolower(name)]
  if (length(cand) == 0) stop(paste0("dataset not found: ", name, " in ", adam_path))
  fp <- cand[1]
  ext <- tolower(tools::file_ext(fp))
  if (ext == "xpt") return(as.data.frame(haven::read_xpt(fp)))
  if (ext == "sas7bdat") return(as.data.frame(haven::read_sas(fp)))
  if (ext == "csv") return(as.data.frame(readr::read_csv(fp, show_col_types = FALSE)))
  stop(paste0("unknown ext: ", fp))
}

adtte <- read_adam("adtte")
adsl  <- read_adam("adsl")
cat(sprintf("adtte: %d rows, adsl: %d rows\n", nrow(adtte), nrow(adsl)))

# normalize names to uppercase for matching (SAS case-insensitive match, keep original case for output)
names(adtte) <- toupper(names(adtte))
names(adsl)  <- toupper(names(adsl))

# ---- Step 2: Prepare data (PROC SQL inner join) ----
# select a.USUBJID, a.TRTA, a.TRTAN, a.AVAL, a.CNSR, b.SEX, b.AGEGR1, b.RACE, b.REGION1, b.ECOGBL
# where b.EFFFL='Y' and a.PARAMCD='OS'
# Note: case-sensitive comparison
adtte_s <- adtte %>% filter(PARAMCD == "OS") %>%
  select(USUBJID, TRTA, TRTAN, AVAL, CNSR)
adsl_s <- adsl %>% filter(EFFFL == "Y") %>%
  select(USUBJID, SEX, AGEGR1, RACE, REGION1, ECOGBL)

eff <- inner_join(adtte_s, adsl_s, by = "USUBJID")
# ensure character
for (cc in c("USUBJID","TRTA","SEX","AGEGR1","RACE","REGION1","ECOGBL")) {
  if (cc %in% names(eff)) eff[[cc]] <- as.character(eff[[cc]])
}
for (cc in c("AVAL","CNSR","TRTAN")) {
  if (cc %in% names(eff)) eff[[cc]] <- suppressWarnings(as.numeric(eff[[cc]]))
}
# event: CNSR(1) censored
eff$EVENT <- ifelse(is.na(eff$CNSR), NA_real_, ifelse(eff$CNSR == 1, 0, 1))
cat(sprintf("eff: %d rows\n", nrow(eff)))

# determine TRTA levels: ref Placebo first, others ASCII sorted
trta_vals <- unique(eff$TRTA)
trta_vals <- trta_vals[!is.na(trta_vals)]
other_lvls <- setdiff(trta_vals, trt1lbl)
other_lvls_sorted <- sas_sort_char(other_lvls)
# if Placebo not present, use first sorted as ref
if (trt1lbl %in% trta_vals) {
  trta_levels <- c(trt1lbl, other_lvls_sorted)
} else {
  all_sorted <- sas_sort_char(trta_vals)
  trta_levels <- all_sorted
  trt1lbl_eff <- all_sorted[1]
  cat(sprintf("WARNING: Placebo not found, using ref %s\n", trt1lbl_eff))
}
cat(paste0("TRTA levels: ", paste(trta_levels, collapse=" | "), "\n"))

run_phreg_one <- function(dsub) {
  # returns data.frame of coef rows or NULL
  # need at least 2 TRTA levels with events?
  dsub <- dsub %>% filter(!is.na(AVAL), !is.na(EVENT), !is.na(TRTA))
  if (nrow(dsub) < 2) return(NULL)
  # factor with ref first
  # keep only levels present? SAS CLASS uses levels present in subset
  present <- intersect(trta_levels, unique(dsub$TRTA))
  if (length(present) < 2) return(NULL)
  dsub$TRTA_f <- factor(dsub$TRTA, levels = trta_levels)
  # drop unused? keep ref even if missing? coxph handles
  fit <- tryCatch(
    survival::coxph(survival::Surv(AVAL, EVENT) ~ TRTA_f, data = dsub, ties = "breslow"),
    error = function(e) NULL,
    warning = function(w) {
      # still try to return fit; suppress warnings but continue
      suppressWarnings(survival::coxph(survival::Surv(AVAL, EVENT) ~ TRTA_f, data = dsub, ties = "breslow"))
    }
  )
  if (is.null(fit)) return(NULL)
  s <- tryCatch(summary(fit), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  coefs <- tryCatch(stats::coef(fit), error = function(e) NULL)
  ses <- tryCatch(sqrt(diag(stats::vcov(fit))), error = function(e) NULL)
  if (is.null(coefs) || length(coefs) == 0) return(NULL)
  # names like TRTA_f[Drug] Low Dose
  outl <- lapply(seq_along(coefs), function(i) {
    est <- as.numeric(coefs[i]); se <- as.numeric(ses[i])
    if (is.na(est) || is.na(se)) {
      hr <- NA_real_; lcl <- NA_real_; ucl <- NA_real_; p <- NA_real_
    } else if (se == 0) {
      hr <- exp(est); lcl <- NA_real_; ucl <- NA_real_; p <- NA_real_
    } else {
      hr <- exp(est)
      lcl <- exp(est - 1.96 * se)
      ucl <- exp(est + 1.96 * se)
      chisq <- (est / se)^2
      p <- stats::pchisq(chisq, df = 1, lower.tail = FALSE)
    }
    cn <- names(coefs)[i]
    # derive comparison level: strip prefix TRTA_f
    comp <- sub("^TRTA_f", "", cn)
    data.frame(Estimate = est, StdErr = se, ProbChiSq = p,
               HR = hr, HR_LCL = lcl, HR_UCL = ucl,
               TRT_COMP = comp, stringsAsFactors = FALSE)
  })
  bind_rows(outl)
}

subgroup_analysis_r <- function(data, subgrp_col, subgrp_label, subgrp_ord) {
  vals_sorted <- sas_sort_char(data[[subgrp_col]])
  # SAS BY: missing group first (NA/"")
  res_list <- list()
  for (lv in vals_sorted) {
    if (is.na(lv)) {
      dsub <- data[is.na(data[[subgrp_col]]), , drop = FALSE]
      lvl_disp <- ""
    } else if (lv == "") {
      dsub <- data[is.na(data[[subgrp_col]]) | data[[subgrp_col]] == "", , drop = FALSE]
      lvl_disp <- ""
    } else {
      dsub <- data[!is.na(data[[subgrp_col]]) & data[[subgrp_col]] == lv, , drop = FALSE]
      lvl_disp <- lv
    }
    if (nrow(dsub) == 0) next
    pr <- run_phreg_one(dsub)
    if (is.null(pr) || nrow(pr) == 0) next
    pr$subgroup <- subgrp_label
    pr$subgroup_level <- lvl_disp
    pr$subgrp_ord <- subgrp_ord
    res_list[[length(res_list) + 1]] <- pr
  }
  if (length(res_list) == 0) return(NULL)
  bind_rows(res_list)
}

# --- Step 3: Run subgroup analyses ---
sg1 <- subgroup_analysis_r(eff, "SEX", "Sex", 1)
sg2 <- subgroup_analysis_r(eff, "AGEGR1", "Age Group", 2)
sg3 <- subgroup_analysis_r(eff, "RACE", "Race", 3)
sg4 <- subgroup_analysis_r(eff, "REGION1", "Region", 4)

# --- Step 4: Overall HR ---
overall_raw <- run_phreg_one(eff)
if (!is.null(overall_raw)) {
  overall_raw$subgroup <- "Overall"
  overall_raw$subgroup_level <- "All Subjects"
  overall_raw$subgrp_ord <- 0
} else {
  overall_raw <- NULL
}

# --- Step 5: Combine ---
parts <- list(overall_raw, sg1, sg2, sg3, sg4)
parts <- parts[!sapply(parts, is.null)]
forest_data <- bind_rows(parts)
if (is.null(forest_data) || nrow(forest_data) == 0) {
  forest_data <- data.frame(subgroup = character(0), subgroup_level = character(0),
    subgrp_ord = numeric(0), HR = numeric(0), HR_LCL = numeric(0), HR_UCL = numeric(0),
    hr_display = character(0), ci_display = character(0), ProbChiSq = numeric(0),
    TRT_COMP = character(0), row = numeric(0), yval = numeric(0))
} else {
  forest_data$hr_display <- sas_fmt_52(forest_data$HR)
  forest_data$ci_display <- paste0("(", sas_fmt_52(forest_data$HR_LCL), ", ",
                                   sas_fmt_52(forest_data$HR_UCL), ")")
  forest_data$row <- seq_len(nrow(forest_data))
  forest_data$yval <- -forest_data$row
  # order columns like SAS keep + extras
  forest_data <- forest_data %>%
    select(subgroup, subgroup_level, subgrp_ord, TRT_COMP, HR, HR_LCL, HR_UCL,
           hr_display, ci_display, ProbChiSq, Estimate, StdErr, row, yval)
}
cat(sprintf("forest_data: %d rows\n", nrow(forest_data)))

# ---- Write CSV of plotted data ----
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

# ---- Step 6: Generate forest plot (PNG) ----
# Mimic SGPLOT: refline 1 dashed gray, CI darkblue thick, scatter diamond filled darkblue,
# x log base2 breaks 0.1 0.25 0.5 1 2 4 grid, y discrete numeric
if (nrow(forest_data) > 0) {
  pd <- forest_data
  # y breaks like %yvals: -1..-N
  ybrks <- sort(unique(pd$yval))
  p <- ggplot(pd, aes(x = HR, y = yval)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.7) +
    geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL), height = 0.25,
                   color = "darkblue", linewidth = 1) +
    geom_point(shape = 18, size = 4, color = "darkblue") +
    scale_x_continuous(trans = "log2",
                       breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
                       labels = c("0.1", "0.25", "0.5", "1", "2", "4"),
                       limits = c(0.08, 5)) +
    scale_y_continuous(breaks = ybrks, labels = as.character(ybrks)) +
    labs(x = "Hazard Ratio (95% CI)", y = NULL,
         title = "Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival",
         subtitle = "Efficacy Analysis Set") +
    theme_bw(base_size = 11) +
    theme(panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)) +
    annotate("text", x = 0.09, y = min(ybrks) - 0.6,
             label = paste0("Favors ", trt2lbl), hjust = 0, size = 3) +
    annotate("text", x = 4.5, y = min(ybrks) - 0.6,
             label = paste0("Favors ", trt1lbl), hjust = 1, size = 3)
  # footnotes as caption
  p <- p + labs(caption = "Hazard Ratios from Cox proportional hazards model.")
  ggsave(file.path(out, "f_forest.png"), plot = p, width = 10, height = 7, units = "in", dpi = 300)
} else {
  # empty plot placeholder
  png(file.path(out, "f_forest.png"), width = 10, height = 7, units = "in", res = 300)
  plot.new()
  title("Figure 14.2.2\nForest Plot - No estimable subgroups")
  dev.off()
}
cat("PNG written\n")

# ---- Text rendering (RTF counterpart) ----
txt_path <- file.path(out, "f_forest.txt")
con <- file(txt_path, open = "wt")
writeLines("[Sponsor Name]                                                  Page 1 of 1", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Figure 14.2.2", con)
writeLines("Forest Plot of Subgroup Analyses - Overall Survival", con)
writeLines("Efficacy Analysis Set", con)
writeLines(strrep("-", 100), con)
if (nrow(forest_data) > 0) {
  writeLines(sprintf("%-15s | %-20s | %-22s | %8s | %20s | %10s",
                     "Subgroup", "Level", "Comparison", "HR", "95% CI", "p-value"), con)
  writeLines(strrep("-", 100), con)
  for (i in seq_len(nrow(forest_data))) {
    r <- forest_data[i, ]
    pstr <- ifelse(is.na(r$ProbChiSq), "", sprintf("%.4f", sas_round(r$ProbChiSq, 4)))
    # HR display already 2 decimals; ensure blank if missing
    hr_s <- ifelse(is.na(r$HR), "", r$hr_display)
    ci_s <- ifelse(is.na(r$HR), "", r$ci_display)
    writeLines(sprintf("%-15s | %-20s | %-22s | %8s | %20s | %10s",
                       substr(as.character(r$subgroup), 1, 15),
                       substr(as.character(r$subgroup_level), 1, 20),
                       substr(as.character(r$TRT_COMP), 1, 22),
                       hr_s, ci_s, pstr), con)
  }
} else {
  writeLines("No estimable hazard ratios.", con)
}
writeLines(strrep("-", 100), con)
writeLines("Hazard Ratios from Cox proportional hazards model (ties=BRESLOW, ref=Placebo).", con)
writeLines("HR = exp(Estimate); 95% CI = exp(Estimate +/- 1.96*StdErr); p = Wald Chi-Square.", con)
writeLines("X-axis log base 2 with reference line at HR=1 (0.1, 0.25, 0.5, 1, 2, 4).", con)
writeLines(paste0("Favors ", trt2lbl, " (left) vs Favors ", trt1lbl, " (right)."), con)
writeLines("Data cutoff: [DDMONYYYY].", con)
writeLines("Source: ADaM datasets (adtte PARAMCD=OS, EFFFL=Y).", con)
close(con)
cat("TXT written\n")

cat("P080 forest plot: completed successfully\n")
```