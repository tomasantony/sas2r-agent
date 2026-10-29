# P080 f_forest.sas -> R translation (attempt 2)
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

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_fmt_52 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f", sas_round(x, 2)))
}
sas_sort_char <- function(v) {
  uv <- unique(v)
  ismiss <- is.na(uv) | uv == ""
  miss_vals <- uv[ismiss]
  nonmiss <- uv[!ismiss]
  if (length(nonmiss) > 0) {
    ord <- order(nonmiss, method = "radix")
    nonmiss <- nonmiss[ord]
  }
  c(miss_vals, nonmiss)
}
sas_trim <- function(x) {
  if (is.character(x)) stringr::str_trim(x, side = "both") else x
}

trt1lbl <- "Placebo"

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
read_adam <- function(name) {
  f1 <- file.path(adam_path, paste0(name, ".xpt"))
  f2 <- file.path(adam_path, paste0(name, ".sas7bdat"))
  f3 <- file.path(adam_path, paste0(name, ".csv"))
  if (file.exists(f1)) return(as.data.frame(haven::read_xpt(f1)))
  if (file.exists(f2)) return(as.data.frame(haven::read_sas(f2)))
  if (file.exists(f3)) return(as.data.frame(readr::read_csv(f3, show_col_types = FALSE)))
  allf <- list.files(adam_path, full.names = TRUE)
  cand <- allf[tolower(tools::file_path_sans_ext(basename(allf))) == tolower(name)]
  if (length(cand) == 0) stop(paste0("dataset not found: ", name))
  fp <- cand[1]
  ext <- tolower(tools::file_ext(fp))
  if (ext == "xpt") return(as.data.frame(haven::read_xpt(fp)))
  if (ext == "sas7bdat") return(as.data.frame(haven::read_sas(fp)))
  if (ext == "csv") return(as.data.frame(readr::read_csv(fp, show_col_types = FALSE)))
  stop(paste0("unknown ext: ", fp))
}

adtte <- read_adam("adtte")
adsl  <- read_adam("adsl")
names(adtte) <- toupper(names(adtte))
names(adsl)  <- toupper(names(adsl))
# SAS blank-padding: trim all character vars
for (cc in names(adtte)) if (is.character(adtte[[cc]])) adtte[[cc]] <- sas_trim(adtte[[cc]])
for (cc in names(adsl))  if (is.character(adsl[[cc]]))  adsl[[cc]]  <- sas_trim(adsl[[cc]])
# haven labelled -> convert to plain
for (cc in names(adtte)) if (inherits(adtte[[cc]], "labelled")) adtte[[cc]] <- haven::as_factor(adtte[[cc]], levels="labels")
for (cc in names(adsl))  if (inherits(adsl[[cc]], "labelled"))  adsl[[cc]]  <- haven::as_factor(adsl[[cc]], levels="labels")
for (cc in names(adtte)) if (is.factor(adtte[[cc]])) adtte[[cc]] <- as.character(adtte[[cc]])
for (cc in names(adsl))  if (is.factor(adsl[[cc]]))  adsl[[cc]]  <- as.character(adsl[[cc]])
for (cc in names(adtte)) if (is.character(adtte[[cc]])) adtte[[cc]] <- sas_trim(adtte[[cc]])
for (cc in names(adsl))  if (is.character(adsl[[cc]]))  adsl[[cc]]  <- sas_trim(adsl[[cc]])

cat(sprintf("adtte: %d rows, adsl: %d rows\n", nrow(adtte), nrow(adsl)))

adtte_s <- adtte %>% filter(PARAMCD == "OS") %>%
  select(USUBJID, TRTA, TRTAN, AVAL, CNSR)
adsl_s <- adsl %>% filter(EFFFL == "Y") %>%
  select(USUBJID, SEX, AGEGR1, RACE, REGION1, ECOGBL)

eff <- inner_join(adtte_s, adsl_s, by = "USUBJID")
for (cc in c("USUBJID","TRTA","SEX","AGEGR1","RACE","REGION1","ECOGBL")) {
  if (cc %in% names(eff)) eff[[cc]] <- sas_trim(as.character(eff[[cc]]))
}
for (cc in c("AVAL","CNSR","TRTAN")) {
  if (cc %in% names(eff)) eff[[cc]] <- suppressWarnings(as.numeric(eff[[cc]]))
}
# SAS: empty string treated as missing for BY; normalize "" -> NA for grouping? keep "" display as "" but group together
# EVENT: CNSR(1) censored
eff$EVENT <- ifelse(is.na(eff$CNSR), NA_real_, ifelse(eff$CNSR == 1, 0, 1))
cat(sprintf("eff: %d rows\n", nrow(eff)))

trta_vals <- unique(eff$TRTA)
trta_vals <- trta_vals[!is.na(trta_vals) & trta_vals != ""]
other_lvls <- setdiff(trta_vals, trt1lbl)
other_lvls_sorted <- sas_sort_char(other_lvls)
if (trt1lbl %in% trta_vals) {
  trta_levels <- c(trt1lbl, other_lvls_sorted)
  ref_lvl <- trt1lbl
} else {
  all_sorted <- sas_sort_char(trta_vals)
  trta_levels <- all_sorted
  ref_lvl <- all_sorted[1]
}
cat(paste0("TRTA levels: ", paste(trta_levels, collapse=" | "), "\n"))

# Fit joint Cox (all non-ref dummies together) for a subset; return per-comparison estimates
fit_joint <- function(dsub, ref) {
  dsub <- dsub %>% filter(!is.na(AVAL), !is.na(EVENT), !is.na(TRTA), TRTA != "")
  if (nrow(dsub) < 2) return(NULL)
  present <- intersect(trta_levels, unique(dsub$TRTA))
  if (!(ref %in% present)) return(NULL)
  if (length(present) < 2) return(NULL)
  # need at least 1 event overall and variation
  if (sum(dsub$EVENT == 1, na.rm=TRUE) < 1) return(NULL)
  dsub$TRTA_f <- factor(dsub$TRTA, levels = trta_levels)
  # drop unused levels beyond present+ref? keep full to match SAS (SAS uses all levels present in BY data)
  # Use explicit dummy coding identical to SAS REF: create dummies for each non-ref present
  fit <- tryCatch(
    suppressWarnings(survival::coxph(survival::Surv(AVAL, EVENT) ~ TRTA_f, data = dsub, ties = "breslow", singular.ok = TRUE)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  coefs <- tryCatch(stats::coef(fit), error=function(e) NULL)
  if (is.null(coefs) || length(coefs)==0) return(NULL)
  vc <- tryCatch(stats::vcov(fit), error=function(e) NULL)
  ses <- if (!is.null(vc)) suppressWarnings(sqrt(diag(vc))) else rep(NA_real_, length(coefs))
  lapply(seq_along(coefs), function(i) {
    est <- as.numeric(coefs[i]); se <- as.numeric(ses[i])
    cn <- names(coefs)[i]
    comp <- sub("^TRTA_f", "", cn)
    if (is.na(est) || is.na(se) || !is.finite(est) || !is.finite(se) || se<=0) {
      list(comp=comp, est=est, se=se, ok=FALSE)
    } else {
      list(comp=comp, est=est, se=se, ok=TRUE)
    }
  })
}

fit_pairwise <- function(dsub, ref, comp) {
  d2 <- dsub %>% filter(TRTA %in% c(ref, comp), !is.na(AVAL), !is.na(EVENT))
  if (nrow(d2) < 2) return(NULL)
  if (length(unique(d2$TRTA)) < 2) return(NULL)
  if (sum(d2$EVENT==1, na.rm=TRUE) < 1) return(NULL)
  d2$TRTA_f <- factor(d2$TRTA, levels=c(ref, comp))
  fit <- tryCatch(
    suppressWarnings(survival::coxph(survival::Surv(AVAL, EVENT) ~ TRTA_f, data=d2, ties="breslow")),
    error=function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  co <- tryCatch(stats::coef(fit), error=function(e) NULL)
  if (is.null(co) || length(co)==0) return(NULL)
  se <- tryCatch(sqrt(diag(stats::vcov(fit))), error=function(e) NULL)
  list(est=as.numeric(co[1]), se=as.numeric(se[1]))
}

process_by_group <- function(dsub, subgroup_label, subgroup_level_disp, subgrp_ord) {
  # joint first
  jres <- fit_joint(dsub, ref_lvl)
  if (is.null(jres)) return(NULL)
  rows <- list()
  for (jr in jres) {
    comp <- jr$comp; est <- jr$est; se <- jr$se; ok <- jr$ok
    if (!ok || is.na(est) || is.na(se) || !is.finite(est) || !is.finite(se)) {
      # fallback to pairwise (SAS would still give finite in many separations)
      pw <- fit_pairwise(dsub, ref_lvl, comp)
      if (!is.null(pw) && is.finite(pw$est) && is.finite(pw$se) && pw$se>0) {
        est <- pw$est; se <- pw$se
      } else {
        # keep NA row to preserve _n_ ordering? SAS would output missing row; keep it
        # but still output row with missing HR (will be blank)
        est <- suppressWarnings(as.numeric(est)); se <- suppressWarnings(as.numeric(se))
        if (!is.finite(est)) est <- NA_real_
        if (!is.finite(se)) se <- NA_real_
      }
    }
    if (is.na(est) || is.na(se) || se<=0 || !is.finite(est) || !is.finite(se)) {
      hr <- NA_real_; lcl <- NA_real_; ucl <- NA_real_; p <- NA_real_
    } else {
      hr <- exp(est); lcl <- exp(est-1.96*se); ucl <- exp(est+1.96*se)
      chisq <- (est/se)^2
      p <- stats::pchisq(chisq, df=1, lower.tail=FALSE)
    }
    rows[[length(rows)+1]] <- data.frame(
      subgroup=subgroup_label, subgroup_level=subgroup_level_disp,
      subgrp_ord=subgrp_ord, TRT_COMP=comp,
      Estimate=ifelse(is.finite(est), est, NA_real_),
      StdErr=ifelse(is.finite(se), se, NA_real_),
      HR=hr, HR_LCL=lcl, HR_UCL=ucl, ProbChiSq=p,
      stringsAsFactors=FALSE)
  }
  dplyr::bind_rows(rows)
}

subgroup_analysis_r <- function(data, subgrp_col, subgrp_label, subgrp_ord) {
  # SAS BY ordering: missing first, then ASCII
  vals_sorted <- sas_sort_char(data[[subgrp_col]])
  res_list <- list()
  for (lv in vals_sorted) {
    if (is.na(lv)) {
      dsub <- data[is.na(data[[subgrp_col]]), , drop=FALSE]
      lvl_disp <- ""
    } else if (lv == "") {
      dsub <- data[is.na(data[[subgrp_col]]) | data[[subgrp_col]]=="", , drop=FALSE]
      lvl_disp <- ""
    } else {
      dsub <- data[!is.na(data[[subgrp_col]]) & data[[subgrp_col]]==lv, , drop=FALSE]
      lvl_disp <- stringr::str_trim(lv)
    }
    if (nrow(dsub)==0) next
    r <- process_by_group(dsub, subgrp_label, lvl_disp, subgrp_ord)
    if (!is.null(r)) res_list[[length(res_list)+1]] <- r
  }
  if (length(res_list)==0) return(NULL)
  dplyr::bind_rows(res_list)
}

sg1 <- subgroup_analysis_r(eff, "SEX", "Sex", 1)
sg2 <- subgroup_analysis_r(eff, "AGEGR1", "Age Group", 2)
sg3 <- subgroup_analysis_r(eff, "RACE", "Race", 3)
sg4 <- subgroup_analysis_r(eff, "REGION1", "Region", 4)
overall_raw <- process_by_group(eff, "Overall", "All Subjects", 0)

parts <- list(overall_raw, sg1, sg2, sg3, sg4)
parts <- parts[!sapply(parts, is.null)]
forest_data <- dplyr::bind_rows(parts)
if (is.null(forest_data) || nrow(forest_data)==0) {
  forest_data <- data.frame(subgroup=character(0), subgroup_level=character(0),
    subgrp_ord=numeric(0), HR=numeric(0), HR_LCL=numeric(0), HR_UCL=numeric(0),
    hr_display=character(0), ci_display=character(0), ProbChiSq=numeric(0),
    row=numeric(0), yval=numeric(0))
} else {
  forest_data$hr_display <- sas_fmt_52(forest_data$HR)
  forest_data$ci_display <- paste0("(", sas_fmt_52(forest_data$HR_LCL), ", ",
                                   sas_fmt_52(forest_data$HR_UCL), ")")
  # SAS row = _n_ in combined order overall,1,2,3,4
  forest_data$row <- seq_len(nrow(forest_data))
  forest_data$yval <- -forest_data$row
}
cat(sprintf("forest_data: %d rows\n", nrow(forest_data)))
print(forest_data %>% select(subgroup, subgroup_level, TRT_COMP, HR, HR_LCL, HR_UCL, ProbChiSq))

# CSV of plotted data (SAS names; keep TRT_COMP for traceability as extra col at end)
out_csv <- forest_data %>%
  select(subgroup, subgroup_level, subgrp_ord, HR, HR_LCL, HR_UCL, hr_display, ci_display, ProbChiSq, row, yval, TRT_COMP, Estimate, StdErr)
write.csv(out_csv, file.path(out, "f_forest.csv"), row.names=FALSE, na="")

# ---- Forest plot PNG (no truncation, log2 axis like SAS) ----
if (nrow(forest_data) > 0) {
  pd <- forest_data %>% filter(!is.na(HR), !is.na(HR_LCL), !is.na(HR_UCL), HR>0, HR_LCL>0, HR_UCL>0)
  # if all missing, use forest_data for empty plot
  if (nrow(pd)==0) pd <- forest_data
  ybrks <- sort(unique(forest_data$yval))
  # x range: include all finite CIs plus SAS ticks 0.1..4
  xmax <- suppressWarnings(max(c(pd$HR_UCL[is.finite(pd$HR_UCL)], 4), na.rm=TRUE))
  xmin <- suppressWarnings(min(c(pd$HR_LCL[is.finite(pd$HR_LCL)], 0.1), na.rm=TRUE))
  if (!is.finite(xmax)) xmax <- 4
  if (!is.finite(xmin)) xmin <- 0.1
  xmin <- max(min(xmin, 0.1)*0.9, 0.01)
  xmax <- min(max(xmax, 4)*1.1, 100)
  p <- ggplot(pd, aes(x=HR, y=yval)) +
    geom_vline(xintercept=1, linetype="dashed", color="gray50", linewidth=0.7) +
    geom_errorbar(aes(xmin=HR_LCL, xmax=HR_UCL), height=0.25, orientation="x",
                  color="darkblue", linewidth=1) +
    geom_point(shape=18, size=4, color="darkblue") +
    scale_x_continuous(trans="log2",
                       breaks=c(0.1,0.25,0.5,1,2,4),
                       labels=c("0.1","0.25","0.5","1","2","4")) +
    scale_y_continuous(breaks=ybrks, labels=as.character(ybrks)) +
    labs(x="Hazard Ratio (95% CI)", y=NULL,
         title="Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival",
         subtitle="Efficacy Analysis Set",
         caption="Hazard Ratios from Cox proportional hazards model.") +
    theme_bw(base_size=11) +
    theme(panel.grid.major=element_line(color="grey85", linewidth=0.4),
          panel.grid.minor=element_blank(),
          plot.title=element_text(hjust=0.5, face="bold"),
          plot.subtitle=element_text(hjust=0.5)) +
    annotate("text", x=xmin, y=min(ybrks)-0.6, label=paste0("Favors ", "[Drug] Low Dose"), hjust=0, size=3) +
    annotate("text", x=xmax, y=min(ybrks)-0.6, label=paste0("Favors ", "Placebo"), hjust=1, size=3)
  ggsave(file.path(out, "f_forest.png"), plot=p, width=10, height=7, units="in", dpi=300)
} else {
  png(file.path(out, "f_forest.png"), width=10, height=7, units="in", res=300)
  plot.new(); title("Figure 14.2.2\nForest Plot - No estimable subgroups"); dev.off()
}
cat("PNG written\n")

# ---- Text rendering ----
# Include run date like SAS footnote3 to capture date numbers
rundate <- format(Sys.Date(), "%d%b%Y")
rundate <- toupper(rundate)  # DATE9.
txt_path <- file.path(out, "f_forest.txt")
con <- file(txt_path, open="wt")
writeLines("[Sponsor Name]                                                  Page 1 of 1", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Figure 14.2.2", con)
writeLines("Forest Plot of Subgroup Analyses - Overall Survival", con)
writeLines("Efficacy Analysis Set", con)
writeLines(strrep("-", 110), con)
if (nrow(forest_data)>0) {
  writeLines(sprintf("%-15s | %-20s | %-22s | %8s | %22s | %10s",
                     "Subgroup","Level","Comparison","HR","95% CI","p-value"), con)
  writeLines(strrep("-", 110), con)
  for (i in seq_len(nrow(forest_data))) {
    r <- forest_data[i,]
    pstr <- ifelse(is.na(r$ProbChiSq), "", sprintf("%.4f", sas_round(r$ProbChiSq,4)))
    hr_s <- ifelse(is.na(r$HR), "", r$hr_display)
    ci_s <- ifelse(is.na(r$HR), "", r$ci_display)
    writeLines(sprintf("%-15s | %-20s | %-22s | %8s | %22s | %10s",
                       substr(as.character(r$subgroup),1,15),
                       substr(as.character(r$subgroup_level),1,20),
                       substr(as.character(r$TRT_COMP),1,22),
                       hr_s, ci_s, pstr), con)
  }
} else {
  writeLines("No estimable hazard ratios.", con)
}
writeLines(strrep("-", 110), con)
writeLines("X-axis log base 2 with values (0.1, 0.25, 0.5, 1, 2, 4); reference line at HR=1.", con)
writeLines("Hazard Ratios from Cox proportional hazards model.", con)
writeLines("HR = exp(Estimate); 95% CI = exp(Estimate +/- 1.96*StdErr); p = Wald Chi-Square.", con)
writeLines(paste0("Favors [Drug] Low Dose (left) vs Favors Placebo (right)."), con)
writeLines("Data cutoff: [DDMONYYYY].", con)
writeLines("Source: ADaM datasets (adtte PARAMCD=OS, EFFFL=Y).", con)
writeLines(paste0("Program: f_forest.sas | Run: ", rundate), con)
close(con)
cat("TXT written\n")
cat("P080 forest plot: completed successfully\n")
