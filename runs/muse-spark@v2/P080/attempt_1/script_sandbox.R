# P080 f_forest.sas -> R translation
# Forest Plot of Subgroup Analyses - Overall Survival
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/raw/own-synthetic/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(survival)
  library(ggplot2)
})

cat("P080 forest plot: reading inputs\n")

read_sas_dataset <- function(dir, name) {
  files <- list.files(dir, full.names = TRUE)
  if (length(files) == 0) stop(paste0("No files in ", dir))
  base_noext <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(base_noext == tolower(name))
  if (length(idx) == 0) stop(paste0("Dataset ", name, " not found in ", dir, " files: ", paste(basename(files), collapse=",")))
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext == "sas7bdat") {
    df <- haven::read_sas(f)
  } else if (ext == "csv") {
    df <- read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    stop(paste0("Unknown extension: ", f))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  # strip haven labels/formats but keep values
  for (nm in names(df)) {
    x <- df[[nm]]
    if (inherits(x, "haven_labelled") || inherits(x, "haven_labelled_spss")) {
      x <- haven::zap_labels(x)
      x <- haven::zap_formats(x)
      # haven labelled factor conversion: keep underlying values
      if (is.factor(x)) x <- as.character(x)
      df[[nm]] <- x
    }
    if (is.factor(df[[nm]])) df[[nm]] <- as.character(df[[nm]])
    if (inherits(df[[nm]], "POSIXt")) df[[nm]] <- as.character(df[[nm]])
  }
  df
}

adtte <- read_sas_dataset(adam_dir, "adtte")
adsl  <- read_sas_dataset(adam_dir, "adsl")

# uppercase names for processing (SAS case-insensitive match, case-sensitive values)
names(adtte) <- toupper(names(adtte))
names(adsl)  <- toupper(names(adsl))

cat(paste0("ADTTE rows=", nrow(adtte), " cols=", paste(names(adtte), collapse=","), "\n"))
cat(paste0("ADSL rows=", nrow(adsl), " cols=", paste(names(adsl), collapse=","), "\n"))

stopifnot(all(c("USUBJID","TRTA","AVAL","CNSR","PARAMCD") %in% names(adtte)))
stopifnot(all(c("USUBJID","EFFFL") %in% names(adsl)))
for (v in c("SEX","AGEGR1","RACE","REGION1")) {
  if (!v %in% names(adsl)) stop(paste0("Missing subgroup var in ADSL: ", v))
}

trt1lbl <- "Placebo"
trt2lbl <- "[Drug] Low Dose"

# SAS round half away from zero
sas_round <- function(x, d) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / (10^d))
}
sas_fmt_52 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    r <- sas_round(v, 2)
    sprintf("%.2f", r)
  }, USE.NAMES = FALSE)
}

# ASCII sort to mimic SAS (uppercase before lowercase)
sort_ascii <- function(v) {
  v <- as.character(v)
  if (length(v) <= 1) return(v)
  keys <- sapply(v, function(s) {
    if (is.na(s)) return("")
    paste(sprintf("%03d", utf8ToInt(s)), collapse = "-")
  }, USE.NAMES = FALSE)
  v[order(keys)]
}

# Prepare eff: inner join adtte PARAMCD=OS with adsl EFFFL=Y
a <- adtte[adtte$PARAMCD == "OS", , drop = FALSE]
b <- adsl[adsl$EFFFL == "Y", , drop = FALSE]
# drop overlapping cols except key before merge (no .x/.y)
common <- intersect(names(a), names(b))
common <- setdiff(common, "USUBJID")
if (length(common) > 0) {
  b <- b[, setdiff(names(b), common), drop = FALSE]
}
eff <- merge(a, b, by = "USUBJID", all = FALSE)
# SAS: select USUBJID, TRTA, TRTAN, AVAL, CNSR, SEX, AGEGR1, RACE, REGION1, ECOGBL
# keep what exists
wanted <- c("USUBJID","TRTA","TRTAN","AVAL","CNSR","SEX","AGEGR1","RACE","REGION1","ECOGBL")
wanted <- wanted[wanted %in% names(eff)]
eff <- eff[, wanted, drop = FALSE]
stopifnot(all(c("TRTA","AVAL","CNSR") %in% names(eff)))
stopifnot(nrow(eff) > 0)

# TRTA levels
eff$TRTA <- trimws(as.character(eff$TRTA))
all_trt <- sort_ascii(unique(eff$TRTA[!is.na(eff$TRTA) & eff$TRTA != ""]))
ref_level <- trt1lbl
if (!ref_level %in% all_trt) {
  # fallback to first sorted as ref if Placebo absent
  ref_level <- all_trt[1]
}
comp_levels <- setdiff(all_trt, ref_level)
comp_levels <- sort_ascii(comp_levels)
cat(paste0("TRTA levels: ", paste(all_trt, collapse=" | "), " ref=", ref_level, " comps=", paste(comp_levels, collapse=" | "), "\n"))

# event indicator: CNSR(1) => censored=1, event = CNSR~=1
eff$EVENT <- ifelse(is.na(eff$CNSR), NA_real_, ifelse(eff$CNSR == 1, 0, 1))
# factor with ref first
eff$TRTA_F <- factor(eff$TRTA, levels = c(ref_level, comp_levels))

fit_one_subset <- function(sub, subgroup, subgroup_level, subgrp_ord) {
  # returns data.frame with one row per comparison
  # attempt coxph
  res_list <- list()
  # filter to complete cases for model
  use <- sub[!is.na(sub$AVAL) & !is.na(sub$EVENT) & !is.na(sub$TRTA_F), , drop = FALSE]
  # need at least 2 trt levels and 1 event
  est_vec <- rep(NA_real_, length(comp_levels))
  se_vec <- rep(NA_real_, length(comp_levels))
  p_vec <- rep(NA_real_, length(comp_levels))
  names(est_vec) <- comp_levels
  fit_ok <- FALSE
  if (nrow(use) >= 2 && length(unique(as.character(use$TRTA_F))) >= 2 && sum(use$EVENT == 1, na.rm = TRUE) >= 1) {
    fit <- tryCatch({
      survival::coxph(survival::Surv(AVAL, EVENT) ~ TRTA_F, data = use, ties = "breslow")
    }, error = function(e) NULL, warning = function(w) {
      # still return fit; suppress warning but continue
      suppressWarnings(survival::coxph(survival::Surv(AVAL, EVENT) ~ TRTA_F, data = use, ties = "breslow"))
    })
    if (!is.null(fit)) {
      s <- tryCatch(summary(fit), error = function(e) NULL)
      if (!is.null(s) && !is.null(s$coefficients) && nrow(s$coefficients) >= 1) {
        coefs <- s$coefficients
        # rownames like TRTA_F<level>
        for (i in seq_len(nrow(coefs))) {
          rn <- rownames(coefs)[i]
          # strip prefix TRTA_F
          lvl <- sub("^TRTA_F", "", rn, fixed = FALSE)
          # exact match to comp_levels (handle special chars: use fixed)
          # rownames may be truncated? use pmatch
          matched <- NA
          for (cl in comp_levels) {
            if (rn == paste0("TRTA_F", cl)) { matched <- cl; break }
          }
          if (is.na(matched)) {
            # fallback: find which comp_level is suffix of rn
            for (cl in comp_levels) {
              if (endsWith(rn, cl)) { matched <- cl; break }
            }
          }
          if (!is.na(matched)) {
            est_vec[matched] <- coefs[i, "coef"]
            # se column name is "se(coef)"
            se_col <- grep("^se\\(coef\\)$", colnames(coefs), value = TRUE)
            if (length(se_col) == 1) se_vec[matched] <- coefs[i, se_col]
            p_col <- grep("^Pr\\(", colnames(coefs), value = TRUE)
            if (length(p_col) == 1) p_vec[matched] <- coefs[i, p_col]
            fit_ok <- TRUE
          }
        }
      }
    }
  }
  rows <- list()
  for (cl in comp_levels) {
    est <- est_vec[[cl]]
    se <- se_vec[[cl]]
    p <- p_vec[[cl]]
    if (is.na(est) || is.na(se)) {
      HR <- NA_real_; LCL <- NA_real_; UCL <- NA_real_
    } else {
      HR <- exp(est)
      LCL <- exp(est - 1.96 * se)
      UCL <- exp(est + 1.96 * se)
    }
    hr_s <- if (is.na(HR)) "" else sas_fmt_52(HR)
    lcl_s <- if (is.na(LCL)) "" else sas_fmt_52(LCL)
    ucl_s <- if (is.na(UCL)) "" else sas_fmt_52(UCL)
    ci_s <- paste0("(", lcl_s, ", ", ucl_s, ")")
    # if both missing, SAS would show "(., .)"? keep as "(, )" -> use "(. , .)"? keep computed
    # For NA case set ci to ""? Keep "(, )" to indicate missing; but use "(., .)" style? Use "(, )"
    # Keep as above
    rows[[cl]] <- data.frame(
      subgroup = subgroup,
      subgroup_level = subgroup_level,
      subgrp_ord = subgrp_ord,
      COMPARISON = cl,
      HR = HR, HR_LCL = LCL, HR_UCL = UCL,
      hr_display = hr_s, ci_display = ci_s,
      ProbChiSq = p,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

# Overall
overall_level <- "All Subjects"
overall_df <- fit_one_subset(eff, "Overall", overall_level, 0)

sub_defs <- list(
  list(var = "SEX",     label = "Sex",       ord = 1),
  list(var = "AGEGR1",  label = "Age Group", ord = 2),
  list(var = "RACE",    label = "Race",      ord = 3),
  list(var = "REGION1", label = "Region",    ord = 4)
)

all_parts <- list(overall_df)
for (sd in sub_defs) {
  v <- sd$var
  vals <- eff[[v]]
  # include missing as separate level? SAS BY includes missing first
  has_missing <- any(is.na(vals) | (is.character(vals) & trimws(vals) == ""))
  uniq_vals <- unique(vals[!is.na(vals) & trimws(as.character(vals)) != ""])
  uniq_vals <- sort_ascii(as.character(uniq_vals))
  # process non-missing levels in SAS sorted order
  for (lv in uniq_vals) {
    sub <- eff[!is.na(eff[[v]]) & as.character(eff[[v]]) == lv, , drop = FALSE]
    lvl_label <- trimws(as.character(lv))
    # strip = vvalue
    df1 <- fit_one_subset(sub, sd$label, lvl_label, sd$ord)
    all_parts[[length(all_parts) + 1]] <- df1
  }
  if (has_missing) {
    sub <- eff[is.na(eff[[v]]) | trimws(as.character(eff[[v]])) == "", , drop = FALSE]
    dfm <- fit_one_subset(sub, sd$label, "", sd$ord)
    # SAS missing sorts first, so prepend before non-missing? Insert at beginning of this subgroup
    # Our all_parts currently has overall + prior subgroups + current non-missing; need to move missing first
    # Simplest: remove just-added non-missing and re-add missing first
    # Instead rebuild: we already appended non-missing; move missing to before them
    # Extract last length(uniq_vals) parts and reorder
    nlv <- length(uniq_vals)
    if (nlv > 0) {
      tail_parts <- all_parts[(length(all_parts) - nlv + 1):length(all_parts)]
      all_parts <- all_parts[1:(length(all_parts) - nlv)]
      all_parts[[length(all_parts) + 1]] <- dfm
      for (tp in tail_parts) all_parts[[length(all_parts) + 1]] <- tp
    } else {
      all_parts[[length(all_parts) + 1]] <- dfm
    }
  }
}

forest_data <- do.call(rbind, all_parts)
rownames(forest_data) <- NULL
forest_data$row <- seq_len(nrow(forest_data))
forest_data$yval <- -forest_data$row

# Order columns to match SAS + comparison
forest_data <- forest_data[, c("subgroup","subgroup_level","subgrp_ord","COMPARISON","HR","HR_LCL","HR_UCL","hr_display","ci_display","ProbChiSq","row","yval")]

cat(paste0("forest_data rows=", nrow(forest_data), "\n"))

# Write CSV of plotted data
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

# Build plot labels
# y label includes subgroup, level, comparison if multiple comps, plus HR and CI
forest_data$base_label <- ifelse(forest_data$subgroup == "Overall",
  paste0("Overall: ", forest_data$subgroup_level),
  paste0(forest_data$subgroup, ": ", forest_data$subgroup_level))
if (length(comp_levels) > 1) {
  forest_data$base_label <- paste0(forest_data$base_label, " [", forest_data$COMPARISON, " vs ", ref_level, "]")
} else {
  # still note comparison in label for clarity if single comp and not placebo-only
  forest_data$base_label <- paste0(forest_data$base_label, " [", forest_data$COMPARISON, " vs ", ref_level, "]")
}
forest_data$ylabel <- paste0(forest_data$base_label, "  ", forest_data$hr_display, " ", forest_data$ci_display)

# For plot, filter? keep all rows; NA HR will not plot point but keep axis
plot_df <- forest_data

# Determine x limits: use log2 scale, include values 0.1 to 4 plus data range
# Expand to accommodate data
xmin_data <- suppressWarnings(min(plot_df$HR_LCL, na.rm = TRUE))
xmax_data <- suppressWarnings(max(plot_df$HR_UCL, na.rm = TRUE))
if (!is.finite(xmin_data)) xmin_data <- 0.1
if (!is.finite(xmax_data)) xmax_data <- 4
x_low <- min(0.08, xmin_data * 0.9, na.rm = TRUE)
x_high <- max(5, xmax_data * 1.1, na.rm = TRUE)
if (!is.finite(x_low)) x_low <- 0.05
if (!is.finite(x_high)) x_high <- 6

run_date <- format(Sys.Date(), "%d%b%Y")
run_date <- toupper(run_date)

main_title <- "Figure 14.2.2"
sub_title <- "Forest Plot of Subgroup Analyses - Overall Survival"
pop_title <- "Efficacy Analysis Set"
sponsor_txt <- "[Sponsor Name]"
protocol_txt <- "Protocol: [Protocol Number]"

p <- ggplot(plot_df, aes(x = HR, y = yval, xmin = HR_LCL, xmax = HR_UCL)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40", linewidth = 0.7) +
  geom_errorbarh(height = 0.25, color = "darkblue", linewidth = 0.9, na.rm = TRUE) +
  geom_point(shape = 18, size = 4, color = "darkblue", na.rm = TRUE) +
  scale_x_continuous(trans = "log2",
    breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
    labels = c("0.1", "0.25", "0.5", "1", "2", "4"),
    limits = c(x_low, x_high)) +
  scale_y_continuous(breaks = plot_df$yval, labels = plot_df$ylabel) +
  labs(
    title = paste0(main_title, "\n", sub_title, "\n", pop_title),
    subtitle = paste0(sponsor_txt, "    ", protocol_txt),
    x = "Hazard Ratio (95% CI)",
    y = NULL,
    caption = paste0("Data cutoff: [DDMONYYYY].\nHazard Ratios from Cox proportional hazards model.\nProgram: P080_f_forest.sas | Run: ", run_date)
  ) +
  theme_bw(base_size = 9) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 8),
    plot.caption = element_text(hjust = 0, size = 7),
    axis.text.y = element_text(size = 7, hjust = 0),
    axis.text.x = element_text(size = 8),
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.minor.x = element_blank()
  ) +
  annotate("text", x = 0.35, y = min(plot_df$yval) - 0.6, label = paste0("Favors ", trt2lbl), size = 3) +
  annotate("text", x = 2.2, y = min(plot_df$yval) - 0.6, label = paste0("Favors ", trt1lbl), size = 3)

# Height adjusts to number of rows to keep readable, but respect 7in baseline
h_use <- max(7, 0.45 * nrow(plot_df) + 3)

ggsave(file.path(out, "f_forest.png"), plot = p, width = 10, height = h_use, dpi = 300, units = "in")

cat("P080 completed successfully\n")
