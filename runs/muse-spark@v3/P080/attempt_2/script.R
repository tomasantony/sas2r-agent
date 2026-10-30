out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(survival)
  library(ggplot2)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_5_2 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    rv <- round_half_up(v, 2)
    trimws(sprintf("%.2f", rv), which = "both")
  }, USE.NAMES = FALSE)
}

find_adam_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  fl <- list.files(dir, recursive = TRUE, full.names = TRUE)
  if (length(fl) == 0) return(NULL)
  base <- basename(fl)
  idx <- which(tolower(tools::file_path_sans_ext(base)) == tolower(name))
  if (length(idx) == 0) {
    idx <- which(grepl(paste0("^", name, "\\."), base, ignore.case = TRUE))
  }
  if (length(idx) == 0) return(NULL)
  fl[idx[1]]
}

read_any <- function(f) {
  if (is.null(f) || is.na(f) || !file.exists(f)) return(NULL)
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    haven::read_xpt(f)
  } else if (ext == "sas7bdat") {
    haven::read_sas(f)
  } else if (ext == "csv") {
    readr::read_csv(f, show_col_types = FALSE)
  } else if (ext %in% c("xls", "xlsx")) {
    readxl::read_excel(f)
  } else {
    haven::read_xpt(f)
  }
}

f_adtte <- find_adam_file(adam_path, "adtte")
f_adsl  <- find_adam_file(adam_path, "adsl")
if (is.null(f_adtte)) stop("adtte not found in ", adam_path)
if (is.null(f_adsl))  stop("adsl not found in ", adam_path)
message("Reading ", f_adtte)
message("Reading ", f_adsl)

adtte_raw <- as.data.frame(read_any(f_adtte), stringsAsFactors = FALSE)
adsl_raw  <- as.data.frame(read_any(f_adsl), stringsAsFactors = FALSE)
names(adtte_raw) <- toupper(names(adtte_raw))
names(adsl_raw)  <- toupper(names(adsl_raw))

unlabel_df <- function(df) {
  for (nm in names(df)) {
    v <- df[[nm]]
    if (inherits(v, "labelled")) {
      if (is.numeric(v)) df[[nm]] <- as.numeric(v)
      else df[[nm]] <- as.character(v)
    } else if (is.factor(v)) {
      df[[nm]] <- as.character(v)
    }
  }
  df
}
adtte_raw <- unlabel_df(adtte_raw)
adsl_raw  <- unlabel_df(adsl_raw)

stopifnot("USUBJID" %in% names(adtte_raw))
stopifnot("USUBJID" %in% names(adsl_raw))
stopifnot("PARAMCD" %in% names(adtte_raw))
stopifnot("AVAL" %in% names(adtte_raw))
stopifnot("CNSR" %in% names(adtte_raw))
stopifnot("TRTA" %in% names(adtte_raw))

# Build join without .x/.y suffixes: select needed cols, keep one copy of overlaps
# From ADTTE need analysis vars; from ADSL need subgroup vars + EFFFL
adtte_want <- c("USUBJID","TRTA","TRTAN","AVAL","CNSR","PARAMCD")
adsl_want  <- c("USUBJID","SEX","AGEGR1","RACE","REGION1","ECOGBL","EFFFL")
adtte_cols <- intersect(adtte_want, names(adtte_raw))
adsl_cols  <- intersect(adsl_want, names(adsl_raw))
# Any overlap besides key: prefer ADSL version for EFFFL/subgroups, ADTTE version otherwise
# Here overlap can only be USUBJID (keys) after restricting wants, but be safe:
overlap <- intersect(setdiff(adtte_cols, "USUBJID"), setdiff(adsl_cols, "USUBJID"))
if (length(overlap) > 0) {
  # drop overlaps from adtte (keep ADSL copy)
  adtte_cols <- setdiff(adtte_cols, overlap)
}
adtte_sub <- adtte_raw[, adtte_cols, drop = FALSE]
adsl_sub  <- adsl_raw[, adsl_cols, drop = FALSE]
# Ensure character keys match (strip)
adtte_sub$USUBJID <- as.character(adtte_sub$USUBJID)
adsl_sub$USUBJID <- as.character(adsl_sub$USUBJID)

eff <- merge(adtte_sub, adsl_sub, by = "USUBJID", all = FALSE)
# No suffixes should appear
stopifnot(!any(grepl("\\.x$|\\.y$", names(eff))))
stopifnot("TRTA" %in% names(eff))
stopifnot("AVAL" %in% names(eff))
stopifnot("CNSR" %in% names(eff))
stopifnot("PARAMCD" %in% names(eff))
# EFFFL must come from ADSL per SAS (b.EFFFL); if ADSL lacks it, fall back to ADTTE copy
if (!("EFFFL" %in% names(eff))) {
  if ("EFFFL" %in% names(adtte_raw)) {
    tmp <- adtte_raw[, c("USUBJID","EFFFL"), drop = FALSE]
    tmp$USUBJID <- as.character(tmp$USUBJID)
    tmp$EFFFL <- as.character(tmp$EFFFL)
    # keep first per subject to avoid duplication
    tmp <- tmp[!duplicated(tmp$USUBJID), , drop = FALSE]
    eff <- merge(eff, tmp, by = "USUBJID", all.x = TRUE)
  } else {
    stop("EFFFL not found in ADSL nor ADTTE")
  }
}
stopifnot("EFFFL" %in% names(eff))
for (cc in c("SEX","AGEGR1","RACE","REGION1")) {
  if (!(cc %in% names(eff))) stop(paste0(cc, " not found after join"))
}
message("EFF rows before filter: ", nrow(eff))

# Filter: EFFFL='Y' and PARAMCD='OS' (case-sensitive like SAS)
eff <- eff[eff$EFFFL == "Y" & !is.na(eff$EFFFL) & eff$PARAMCD == "OS" & !is.na(eff$PARAMCD), , drop = FALSE]
message("EFF N=", nrow(eff))
stopifnot(nrow(eff) > 0)

eff$TRTA <- as.character(eff$TRTA)
eff$SEX <- as.character(eff$SEX)
eff$AGEGR1 <- as.character(eff$AGEGR1)
eff$RACE <- as.character(eff$RACE)
eff$REGION1 <- as.character(eff$REGION1)
eff$AVAL <- suppressWarnings(as.numeric(as.character(eff$AVAL)))
cnsr_num <- suppressWarnings(as.numeric(as.character(eff$CNSR)))
eff$CNSR_NUM <- cnsr_num
eff$EVENT <- ifelse(!is.na(cnsr_num) & cnsr_num == 1, 0L,
             ifelse(!is.na(cnsr_num), 1L, NA_integer_))

trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"
all_trt <- unique(eff$TRTA[!is.na(eff$TRTA)])
old_loc <- Sys.getlocale("LC_COLLATE")
suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
sorted_trt <- sort(all_trt)
suppressWarnings(try(Sys.setlocale("LC_COLLATE", old_loc), silent = TRUE))
if (trt1lbl %in% all_trt) {
  ref_trt <- trt1lbl
} else {
  ref_trt <- sorted_trt[1]
}
other_trt <- setdiff(sorted_trt, ref_trt)
trt_levels <- c(ref_trt, other_trt)
message("TRT levels: ", paste(trt_levels, collapse=" | "), " ref=", ref_trt)

sas_sort_unique <- function(x) {
  x <- unique(x[!is.na(x)])
  oc <- Sys.getlocale("LC_COLLATE")
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", "C"), silent = TRUE))
  s <- sort(x)
  suppressWarnings(try(Sys.setlocale("LC_COLLATE", oc), silent = TRUE))
  s
}

run_cox_level <- function(subdata, subgroup_label, subgrp_ord, level_val) {
  mk_na_row <- function(comp = NA_character_) {
    data.frame(
      subgroup = subgroup_label,
      subgroup_level = as.character(level_val),
      subgrp_ord = subgrp_ord,
      TRT_COMP = ifelse(is.na(comp), NA_character_, as.character(comp)),
      HR = NA_real_, HR_LCL = NA_real_, HR_UCL = NA_real_,
      hr_display = "", ci_display = "",
      ProbChiSq = NA_real_,
      stringsAsFactors = FALSE
    )
  }
  sd <- subdata[!is.na(subdata$AVAL) & !is.na(subdata$EVENT), , drop = FALSE]
  if (nrow(sd) == 0) return(mk_na_row(ifelse(length(other_trt)>0, other_trt[1], NA)))
  present <- unique(sd$TRTA[!is.na(sd$TRTA)])
  if (length(present) < 2) {
    comp <- setdiff(trt_levels, ref_trt)[1]
    return(mk_na_row(comp))
  }
  sd$TRTA_F <- factor(sd$TRTA, levels = trt_levels)
  fit <- try(suppressWarnings(coxph(Surv(AVAL, EVENT) ~ TRTA_F, data = sd, ties = "breslow")), silent = TRUE)
  if (inherits(fit, "try-error") || is.null(fit$coefficients)) {
    comp <- setdiff(trt_levels, ref_trt)[1]
    return(mk_na_row(comp))
  }
  coefs <- fit$coefficients
  ses <- sqrt(diag(fit$var))
  rows <- list()
  for (i in seq_along(coefs)) {
    nm <- names(coefs)[i]
    comp <- sub("^TRTA_F", "", nm, fixed = FALSE)
    if (!(comp %in% all_trt)) {
      if (i <= length(other_trt)) comp <- other_trt[i] else comp <- nm
    }
    est <- as.numeric(coefs[i])
    se <- as.numeric(ses[i])
    if (is.na(est) || is.na(se) || se <= 0) {
      hr <- NA_real_; lcl <- NA_real_; ucl <- NA_real_
      pval <- NA_real_
      hrd <- ""; cid <- ""
    } else {
      hr <- exp(est)
      lcl <- exp(est - 1.96 * se)
      ucl <- exp(est + 1.96 * se)
      chisq <- (est / se)^2
      pval <- pchisq(chisq, df = 1, lower.tail = FALSE)
      hrd <- sas_5_2(hr)
      cid <- paste0("(", sas_5_2(lcl), ", ", sas_5_2(ucl), ")")
    }
    rows[[length(rows)+1]] <- data.frame(
      subgroup = subgroup_label,
      subgroup_level = as.character(level_val),
      subgrp_ord = subgrp_ord,
      TRT_COMP = as.character(comp),
      HR = hr, HR_LCL = lcl, HR_UCL = ucl,
      hr_display = hrd, ci_display = cid,
      ProbChiSq = pval,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

subgroups <- list(
  list(var = "SEX",     label = "Sex",       ord = 1),
  list(var = "AGEGR1",  label = "Age Group", ord = 2),
  list(var = "RACE",    label = "Race",      ord = 3),
  list(var = "REGION1", label = "Region",    ord = 4)
)

overall_rows <- run_cox_level(eff, "Overall", 0, "All Subjects")
overall_rows$subgroup <- "Overall"
overall_rows$subgroup_level <- "All Subjects"
overall_rows$subgrp_ord <- 0

all_parts <- list(overall_rows)
for (sg in subgroups) {
  v <- sg$var; lab <- sg$label; ord <- sg$ord
  stopifnot(v %in% names(eff))
  levs <- sas_sort_unique(eff[[v]])
  has_na <- any(is.na(eff[[v]]))
  for (lv in levs) {
    sd <- eff[!is.na(eff[[v]]) & eff[[v]] == lv, , drop = FALSE]
    r <- run_cox_level(sd, lab, ord, lv)
    all_parts[[length(all_parts)+1]] <- r
  }
  if (has_na) {
    sd <- eff[is.na(eff[[v]]), , drop = FALSE]
    r <- run_cox_level(sd, lab, ord, "")
    all_parts[[length(all_parts)+1]] <- r
  }
}

forest_data <- do.call(rbind, all_parts)
forest_data$row <- seq_len(nrow(forest_data))
forest_data$yval <- -forest_data$row

forest_data <- forest_data[, c("subgroup","subgroup_level","subgrp_ord","TRT_COMP","HR","HR_LCL","HR_UCL","hr_display","ci_display","ProbChiSq","row","yval")]

write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

forest_data$ylab <- paste0(forest_data$subgroup, ": ", forest_data$subgroup_level)
if (length(unique(forest_data$TRT_COMP[!is.na(forest_data$TRT_COMP)])) > 1) {
  forest_data$ylab <- paste0(forest_data$ylab, " (", forest_data$TRT_COMP, " vs ", ref_trt, ")")
} else {
  if (length(trt_levels) > 2) {
    forest_data$ylab <- paste0(forest_data$ylab, " (", forest_data$TRT_COMP, " vs ", ref_trt, ")")
  }
}

brks <- c(0.1, 0.25, 0.5, 1, 2, 4)
xmin_data <- suppressWarnings(min(forest_data$HR_LCL[is.finite(forest_data$HR_LCL)], na.rm = TRUE))
xmax_data <- suppressWarnings(max(forest_data$HR_UCL[is.finite(forest_data$HR_UCL)], na.rm = TRUE))
if (!is.finite(xmin_data)) xmin_data <- 0.1
if (!is.finite(xmax_data)) xmax_data <- 4
xlo <- min(0.08, xmin_data * 0.9)
xhi <- max(5, xmax_data * 1.1)
xlo <- max(xlo, 0.01)
xhi <- min(xhi, 50)

plot_df <- forest_data

p <- ggplot(plot_df, aes(y = yval, x = HR)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL), color = "darkblue", height = 0.25, linewidth = 0.9, na.rm = TRUE) +
  geom_point(shape = 18, color = "darkblue", size = 3.5, na.rm = TRUE) +
  scale_x_continuous(trans = "log2", breaks = brks, labels = as.character(brks),
                     limits = c(xlo, xhi), name = "Hazard Ratio (95% CI)") +
  scale_y_continuous(breaks = plot_df$yval, labels = plot_df$ylab, name = NULL) +
  labs(
    title = "Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival",
    subtitle = "Efficacy Analysis Set\n[Sponsor Name]  |  Protocol: [Protocol Number]",
    caption = "Data cutoff: [DDMONYYYY].\nHazard Ratios from Cox proportional hazards model.\nSource: ADaM datasets  |  Program: f_forest.sas"
  ) +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    plot.caption = element_text(hjust = 0, size = 7),
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor.x = element_blank(),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 8)
  ) +
  annotate("text", x = xlo * 1.05, y = min(plot_df$yval) - 0.6, label = paste0("Favors ", trt2lbl), hjust = 0, size = 3) +
  annotate("text", x = xhi * 0.95, y = min(plot_df$yval) - 0.6, label = paste0("Favors ", trt1lbl), hjust = 1, size = 3)

p <- p + expand_limits(y = min(plot_df$yval) - 1)

ggsave(file.path(out, "f_forest.png"), plot = p, width = 10, height = 7, dpi = 300, units = "in")

message("Done. Rows=", nrow(forest_data))
