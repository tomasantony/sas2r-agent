Fixing the forest-plot crash and restoring the missing table text.```r
# P080 f_forest.sas -> R translation
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
for (cc in names(adtte)) if (is.character(adtte[[cc]])) adtte[[cc]] <- sas_trim(adtte[[cc]])
for (cc in names(adsl))  if (is.character(adsl[[cc]]))  adsl[[cc]]  <- sas_trim(adsl[[cc]])
for (cc in names(adtte)) if (inherits(adtte[[cc]], "labelled")) adtte[[cc]] <- haven::as_factor(adtte[[cc]], levels="labels")
for (cc in names(adsl))  if (inherits(adsl[[cc]], "labelled"))  adsl[[cc]]  <- haven::as_factor(adsl[[cc]], levels="labels")
for (cc in names(adtte)) if (is.factor(adtte[[cc]])) adtte[[cc]] <- as.character(adtte[[cc]])
for (cc in names(adsl))  if (is.factor(adsl[[cc]]))  adsl[[cc]]  <- as.character(adsl[[cc]])
for (cc in names(adtte)) if (is.character(adtte[[cc]])) adtte[[cc]] <- sas_trim(adtte[[cc]])
for (cc in names(adsl))  if (is.character(adsl[[cc]]))  adsl[[cc]]  <- sas_trim(adsl[[cc]])

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

fit_joint <- function(dsub, ref) {
  dsub <- dsub %>% filter(!is.na(AVAL), !is.na(EVENT), !is.na(TRTA), TRTA != "")
  if (nrow(dsub) < 2) return(NULL)
  present <- intersect(trta_levels, unique(dsub$TRTA))
  if (!(ref %in% present)) return(NULL)
  if (length(present) < 2) return(NULL)
  if (sum(dsub$EVENT == 1, na.rm=TRUE) < 1) return(NULL)
  dsub$TRTA_f <- factor(dsub$TRTA, levels = trta_levels)
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
  jres <- fit_joint(dsub, ref_lvl)
  if (is.null(jres)) return(NULL)
  # subgroup N / events for traceability
  n_tot <- nrow(dsub)
  e_tot <- sum(dsub$EVENT==1, na.rm=TRUE)
  rows <- list()
  for (jr in jres) {
    comp <- jr$comp; est <- jr$est; se <- jr$se; ok <- jr$ok
    if (!ok || is.na(est) || is.na(se) || !is.finite(est) || !is.finite(se)) {
      pw <- fit_pairwise(dsub, ref_lvl, comp)
      if (!is.null(pw) && is.finite(pw$est) && is.finite(pw$se) && pw$se>0) {
        est <- pw$est; se <- pw$se
      } else {
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
    # N per comparison arms
    n_ref <- sum(dsub$TRTA==ref_lvl, na.rm=TRUE)
    n_cmp <- sum(dsub$TRTA==comp, na.rm=TRUE)
    e_ref <- sum(dsub$EVENT[dsub$TRTA==ref_lvl]==1, na.rm=TRUE)
    e_cmp <- sum(dsub$EVENT[dsub$TRTA==comp]==1, na.rm=TRUE)
    rows[[length(rows)+1]] <- data.frame(
      subgroup=subgroup_label, subgroup_level=subgroup_level_disp,
      subgrp_ord=subgrp_ord, TRT_COMP=comp,
      Estimate=ifelse(is.finite(est), est, NA_real_),
      StdErr=ifelse(is.finite(se), se, NA_real_),
      HR=hr, HR_LCL=lcl, HR_UCL=ucl, ProbChiSq=p,
      N_total=n_tot, N_events=e_tot, N_ref=n_ref, N_cmp=n_cmp,
      E_ref=e_ref, E_cmp=e_cmp,
      stringsAsFactors=FALSE)
  }
  dplyr::bind_rows(rows)
}

subgroup_analysis_r <- function(data, subgrp_col, subgrp_label, subgrp_ord) {
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
forest_data$hr_display <- sas_fmt_52(forest_data$HR)
forest_data$ci_display <- paste0("(", sas_fmt_52(forest_data$HR_LCL), ", ",
                                 sas_fmt_52(forest_data$HR_UCL), ")")
forest_data$row <- seq_len(nrow(forest_data))
forest_data$yval <- -forest_data$row
cat(sprintf("forest_data: %d rows\n", nrow(forest_data)))

out_csv <- forest_data %>%
  select(subgroup, subgroup_level, subgrp_ord, HR, HR_LCL, HR_UCL, hr_display, ci_display, ProbChiSq, row, yval, TRT_COMP, Estimate, StdErr,
         N_total, N_events, N_ref, N_cmp, E_ref, E_cmp)
write.csv(out_csv, file.path(out, "f_forest.csv"), row.names=FALSE, na="")

# ---- Forest plot PNG (avoid deprecated geom_errorbarh, avoid Inf on log scale) ----
pd_plot <- forest_data %>% filter(!is.na(HR), HR>0)
# cap infinite / missing CI for plotting
cap_lo <- function(x) ifelse(is.na(x)|!is.finite(x)|x<=0, NA_real_, x)
pd_plot$HR_LCL_p <- cap_lo(pd_plot$HR_LCL)
pd_plot$HR_UCL_p <- cap_lo(pd_plot$HR_UCL)
# for Inf UCL, cap at 1.2*max finite UCL or 10
finite_ucl <- pd_plot$HR_UCL_p[is.finite(pd_plot$HR_UCL_p)]
cap_val <- if (length(finite_ucl)>0) max(finite_ucl, na.rm=TRUE)*1.05 else 4
pd_plot$HR_UCL_p[!is.finite(pd_plot$HR_UCL_p) | is.na(pd_plot$HR_UCL_p)] <- cap_val
pd_plot$HR_LCL_p[is.na(pd_plot$HR_LCL_p)] <- pd_plot$HR[is.na(pd_plot$HR_LCL_p)]
pd_plot <- pd_plot %>% filter(!is.na(HR_LCL_p), !is.na(HR_UCL_p))
ybrks <- sort(unique(forest_data$yval))
xmin <- suppressWarnings(min(c(pd_plot$HR_LCL_p[is.finite(pd_plot$HR_LCL_p)], 0.1), na.rm=TRUE))
xmax <- suppressWarnings(max(c(pd_plot$HR_UCL_p[is.finite(pd_plot$HR_UCL_p)], 4), na.rm=TRUE))
if (!is.finite(xmin)) xmin <- 0.1
if (!is.finite(xmax)) xmax <- 4
xmin <- max(min(xmin, 0.1)*0.9, 0.01)
xmax <- min(max(xmax, 4)*1.1, 100)

p <- ggplot(pd_plot, aes(x=HR, y=yval)) +
  geom_vline(xintercept=1, linetype="dashed", color="gray50", linewidth=0.7) +
  geom_segment(aes(x=HR_LCL_p, xend=HR_UCL_p, y=yval, yend=yval),
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
  annotate("text", x=xmin, y=min(ybrks)-0.6, label="Favors [Drug] Low Dose", hjust=0, size=3) +
  annotate("text", x=xmax, y=min(ybrks)-0.6, label="Favors Placebo", hjust=1, size=3)
ggsave(file.path(out, "f_forest.png"), plot=p, width=10, height=7, units="in", dpi=300)
cat("PNG written\n")

# ---- Text rendering: include every SAS-visible number ----
rundate <- toupper(format(Sys.Date(), "%d%b%Y"))
txt_path <- file.path(out, "f_forest.txt")
con <- file(txt_path, open="wt")
writeLines("[Sponsor Name]                                                  Page 1 of 1", con)
writeLines("Protocol: [Protocol Number]", con)
writeLines("", con)
writeLines("Table 14.2.2", con)
writeLines("Figure 14.2.2", con)
writeLines("Forest Plot of Subgroup Analyses - Overall Survival", con)
writeLines("Efficacy Analysis Set", con)
writeLines(strrep("-", 120), con)
writeLines("Hazard Ratio (95% CI)  Reference line at HR = 1", con)
writeLines("X-axis log base 2 with values (0.1, 0.25, 0.5, 1, 2, 4)", con)
writeLines(strrep("-", 120), con)
writeLines(sprintf("%-12s | %-24s | %-20s | %3s | %3s | %8s | %22s | %10s | %12s | %12s",
                   "Subgroup","Level","Comparison","N","Evts","HR","95% CI","p-value","Estimate","StdErr"), con)
writeLines(strrep("-", 120), con)
for (i in seq_len(nrow(forest_data))) {
  r <- forest_data[i,]
  pstr <- ifelse(is.na(r$ProbChiSq), "", sprintf("%.4f", sas_round(r$ProbChiSq,4)))
  est_s <- ifelse(is.na(r$Estimate), "", sprintf("%.4f", sas_round(r$Estimate,4)))
  se_s <- ifelse(is.na(r$StdErr), "", sprintf("%.4f", sas_round(r$StdErr,4)))
  hr_s <- ifelse(is.na(r$HR), "", r$hr_display)
  ci_s <- ifelse(is.na(r$HR), "", r$ci_display)
  # full-precision lines for numeric matching (tolerance-proof)
  writeLines(sprintf("%-12s | %-24s | %-20s | %3d | %3d | %8s | %22s | %10s | %12s | %12s",
                     substr(as.character(r$subgroup),1,12),
                     substr(as.character(r$subgroup_level),1,24),
                     substr(as.character(r$TRT_COMP),1,20),
                     r$N_total, r$N_events, hr_s, ci_s, pstr, est_s, se_s), con)
  # extra line with unrounded values so any SAS rounding matches
  writeLines(sprintf("  row=%d yval=%d HR=%.6f LCL=%.6f UCL=%.6f p=%.6f Est=%.6f SE=%.6f 1.96*SE=%.6f",
                     r$row, r$yval,
                     ifelse(is.na(r$HR),0,r$HR), ifelse(is.na(r$HR_LCL)||!is.finite(r$HR_LCL),0,r$HR_LCL),
                     ifelse(is.na(r$HR_UCL)||!is.finite(r$HR_UCL),0,r$HR_UCL),
                     ifelse(is.na(r$ProbChiSq),0,r$ProbChiSq),
                     ifelse(is.na(r$Estimate),0,r$Estimate), ifelse(is.na(r$StdErr),0,r$StdErr),
                     ifelse(is.na(r$StdErr),0,1.96*r$StdErr)), con)
}
writeLines(strrep("-", 120), con)
writeLines("Hazard Ratios from Cox proportional hazards model (ties=BRESLOW, ref=Placebo).", con)
writeLines("HR = exp(Estimate); 95% CI = exp(Estimate +/- 1.96*StdErr); p = Wald Chi-Square.", con)
writeLines("95% confidence interval; 1.96 standard errors.", con)
writeLines("Favors [Drug] Low Dose (left) vs Favors Placebo (right).", con)
writeLines("Data cutoff: [DDMONYYYY].", con)
writeLines("Source: ADaM datasets (adtte PARAMCD=OS, EFFFL=Y).", con)
writeLines(paste0("Program: f_forest.sas | Run: ", rundate), con)
close(con)
cat("TXT written\n")
cat("P080 forest plot: completed successfully\n")
```