# P081 f_mean_cfb - Mean Change from Baseline Over Time
# Inputs: adam.adeff, adam.adsl (own-synthetic)
# Output: f_mean_cfb.png + f_mean_cfb.csv (plotted data)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(NULL)
  fs <- list.files(dir, full.names = TRUE, recursive = FALSE)
  base <- tolower(tools::file_path_sans_ext(basename(fs)))
  idx <- which(base == tolower(name))
  if (length(idx) == 0) {
    idx <- grep(paste0("(?i)^", name), basename(fs), perl = TRUE)
  }
  if (length(idx) == 0) return(NULL)
  fs[idx[1]]
}

read_any <- function(path) {
  if (is.null(path) || length(path) == 0 || !file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else if (ext == "sas7bdat") {
    haven::read_sas(path)
  } else if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE) |> as.data.frame()
  } else {
    haven::read_sas(path)
  }
}

zap_all <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    v <- df[[nm]]
    if (haven::is.labelled(v)) {
      v <- haven::zap_labels(v)
      v <- haven::zap_formats(v)
      v <- haven::zap_label(v)
    }
    df[[nm]] <- as.vector(v)
  }
  df
}

adeff_path <- find_file(adam_dir, "adeff")
adsl_path  <- find_file(adam_dir, "adsl")

adeff <- zap_all(read_any(adeff_path))
adsl  <- zap_all(read_any(adsl_path))

# Ensure expected columns exist
stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","AVAL","BASE","CHG","ANL01FL") %in% names(adeff)))
stopifnot(all(c("USUBJID","EFFFL") %in% names(adsl)))

eff_param <- "ACTOT"
y_label <- "Change from Baseline in ADAS-Cog Total Score"

# Normalize character columns (SAS trims trailing blanks; comparison case-sensitive)
to_chr <- function(x) trimws(as.character(x))
adeff$USUBJID <- to_chr(adeff$USUBJID)
adeff$TRTA <- to_chr(adeff$TRTA)
adeff$PARAMCD <- to_chr(adeff$PARAMCD)
adeff$PARAM <- to_chr(adeff$PARAM)
adeff$AVISIT <- to_chr(adeff$AVISIT)
adeff$ANL01FL <- to_chr(adeff$ANL01FL)
adsl$USUBJID <- to_chr(adsl$USUBJID)
adsl$EFFFL <- to_chr(adsl$EFFFL)

adeff$TRTAN <- suppressWarnings(as.numeric(adeff$TRTAN))
adeff$AVISITN <- suppressWarnings(as.numeric(adeff$AVISITN))
adeff$AVAL <- suppressWarnings(as.numeric(adeff$AVAL))
adeff$BASE <- suppressWarnings(as.numeric(adeff$BASE))
adeff$CHG <- suppressWarnings(as.numeric(adeff$CHG))

# Step 1: Prepare analysis data (inner join, filter)
# SAS: select a.* , b.EFFFL where b.EFFFL='Y' -> keep b.EFFFL, drop overlapping cols from a
adsl_sub <- adsl[, c("USUBJID", "EFFFL"), drop = FALSE]
adsl_sub <- adsl_sub[!duplicated(adsl_sub$USUBJID), , drop = FALSE]
# Drop every column present in both tables except the key so no .x/.y suffixes appear
common <- intersect(names(adeff), names(adsl_sub))
common <- setdiff(common, "USUBJID")
if (length(common) > 0) {
  adeff <- adeff[, setdiff(names(adeff), common), drop = FALSE]
}

eff <- merge(adeff, adsl_sub, by = "USUBJID", all = FALSE)
stopifnot(all(c("EFFFL","ANL01FL","PARAMCD","CHG") %in% names(eff)))

eff <- eff[eff$EFFFL == "Y" & eff$PARAMCD == eff_param & eff$ANL01FL == "Y" & !is.na(eff$CHG), ,
           drop = FALSE]
eff <- eff[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN","AVAL","BASE","CHG","ANL01FL"), drop = FALSE]
# SAS order by TRTAN, AVISITN
eff <- eff[order(eff$TRTAN, eff$AVISITN, eff$TRTA, eff$AVISIT), , drop = FALSE]

# Step 2: PROC MEANS NWAY by TRTA TRTAN AVISIT AVISITN -> n mean stderr lclm uclm
# SAS CLM uses t critical: mean +/- t(0.975, n-1)*stderr
stats <- eff %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(
    n = sum(!is.na(CHG)),
    mean = ifelse(sum(!is.na(CHG)) > 0, mean(CHG, na.rm = TRUE), NA_real_),
    sd = ifelse(sum(!is.na(CHG)) > 1, sd(CHG, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  as.data.frame()
stats$se <- stats$sd / sqrt(stats$n)
stats$tcrit <- ifelse(stats$n > 1, qt(0.975, stats$n - 1), NA_real_)
stats$lcl <- stats$mean - stats$tcrit * stats$se
stats$ucl <- stats$mean + stats$tcrit * stats$se
# n=1 -> se/lcl/ucl missing like SAS
stats$se[stats$n <= 1] <- NA_real_
stats$lcl[stats$n <= 1] <- NA_real_
stats$ucl[stats$n <= 1] <- NA_real_
stats$sd <- NULL
stats$tcrit <- NULL
stats <- stats[order(stats$TRTAN, stats$AVISITN), , drop = FALSE]

# Step 3: offset
plot_data <- stats
plot_data$xpos <- with(plot_data, ifelse(TRTAN == 1, AVISITN - 0.15,
                                  ifelse(TRTAN == 2, AVISITN,
                                  ifelse(TRTAN == 3, AVISITN + 0.15, AVISITN))))
# order columns like SAS plot_data
plot_data <- plot_data[, c("TRTA","TRTAN","AVISIT","AVISITN","n","mean","se","lcl","ucl","xpos"), drop = FALSE]

# Titles / footnotes verbatim
title4 <- "Figure 14.2.4"
title5 <- "Mean Change from Baseline Over Time"
title6 <- "Efficacy Analysis Set"
fn1 <- "Data cutoff: [DDMONYYYY]."
fn2 <- "Error bars represent 95% confidence intervals."
fn3 <- "n = number of subjects with non-missing change from baseline."
run_date9 <- toupper(format(Sys.Date(), "%d%b%Y"))
fn4 <- paste0("Program: P081_f_mean_cfb.sas | Run: ", run_date9)

# Step 4: figure with ggplot2 emulating SGPLOT
brk <- sort(unique(plot_data$AVISITN))
lbl_df <- plot_data[, c("AVISITN","AVISIT"), drop = FALSE]
lbl_df <- lbl_df[!duplicated(lbl_df$AVISITN), , drop = FALSE]
lbl_df <- lbl_df[order(lbl_df$AVISITN), , drop = FALSE]
lbl <- lbl_df$AVISIT[match(brk, lbl_df$AVISITN)]
pd <- plot_data
trt_ord <- unique(pd[order(pd$TRTAN), "TRTA"])
pd$TRTA <- factor(pd$TRTA, levels = trt_ord)

p <- ggplot(pd, aes(x = xpos, y = mean, colour = TRTA, group = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.08, linewidth = 0.7, na.rm = TRUE) +
  geom_line(linewidth = 1.0, na.rm = TRUE) +
  geom_point(size = 3, shape = 19, na.rm = TRUE) +
  scale_x_continuous(breaks = brk, labels = lbl, minor_breaks = NULL) +
  labs(
    title = paste0(title4, "\n", title5, "\n", title6),
    x = "Visit (Week)",
    y = y_label,
    colour = NULL,
    caption = paste0(fn1, "\n", fn2, "\n", fn3, "\n", fn4)
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", lineheight = 1.1),
    plot.caption = element_text(hjust = 0, size = 8),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major = element_line(linetype = "dotted", colour = "lightgray"),
    panel.grid.minor = element_blank()
  ) +
  guides(colour = guide_legend(nrow = 1))

# 10in x 6.5in like ODS GRAPHICS
ggsave(file.path(out, "f_mean_cfb.png"), plot = p, width = 10, height = 6.5, units = "in", dpi = 300)

# CSV of plotted data (numeric unrounded, NA as "")
out_csv <- plot_data
out_csv$TRTA <- as.character(out_csv$TRTA)
write.csv(out_csv, file.path(out, "f_mean_cfb.csv"), row.names = FALSE, na = "")

cat("P081 completed: n groups =", nrow(out_csv), "\n")
