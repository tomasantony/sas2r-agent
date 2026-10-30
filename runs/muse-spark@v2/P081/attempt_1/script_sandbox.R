# P081 f_mean_cfb.sas -> R translation
# Figure 14.2.4 Mean Change from Baseline Over Time

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(tibble)
  library(ggplot2)
})

# round half away from zero (SAS) helper
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "/bench/data/raw/own-synthetic/adam"

find_adam_file <- function(libdir, dsn) {
  # case-insensitive search for dsn with extensions sas7bdat / xpt / sas7bxpt
  allf <- list.files(libdir, full.names = TRUE)
  # match basename without extension equal to dsn (case-insensitive)
  pat <- paste0("^", dsn, "\\.")
  idx <- grepl(pat, basename(allf), ignore.case = TRUE)
  if (any(idx)) return(allf[which(idx)[1]])
  # fallback: contains
  idx2 <- grepl(dsn, basename(allf), ignore.case = TRUE)
  if (any(idx2)) return(allf[which(idx2)[1]])
  return(file.path(libdir, paste0(dsn, ".sas7bdat")))
}

read_adam <- function(libdir, dsn) {
  f <- find_adam_file(libdir, dsn)
  message("Reading ", dsn, " from ", f)
  ext <- tolower(tools::file_ext(f))
  if (ext == "xpt") {
    df <- haven::read_xpt(f)
  } else if (ext %in% c("sas7bdat", "sas7bxpt", "ssd07")) {
    df <- haven::read_sas(f)
  } else if (ext == "csv") {
    df <- readr::read_csv(f, show_col_types = FALSE)
  } else {
    # try sas then xpt
    if (file.exists(f)) {
      df <- haven::read_sas(f)
    } else {
      stop(paste0("File not found for ", dsn, ": ", f))
    }
  }
  df <- haven::zap_formats(haven::zap_labels(df))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- toupper(names(df))
  tibble::as_tibble(df)
}

adeff_raw <- read_adam(adam_path, "adeff")
adsl_raw  <- read_adam(adam_path, "adsl")

message(paste0("adeff rows=", nrow(adeff_raw), " cols=", paste(names(adeff_raw), collapse=",")))
message(paste0("adsl rows=", nrow(adsl_raw), " cols=", paste(names(adsl_raw), collapse=",")))

# Ensure required columns exist
stopifnot("USUBJID" %in% names(adeff_raw))
stopifnot("PARAMCD" %in% names(adeff_raw))
stopifnot("ANL01FL" %in% names(adeff_raw))
stopifnot("CHG" %in% names(adeff_raw))
stopifnot("USUBJID" %in% names(adsl_raw))
stopifnot("EFFFL" %in% names(adsl_raw))

eff_param <- "ACTOT"
y_label <- "Change from Baseline in ADAS-Cog Total Score"

# Prepare ADSL subset: keep only keys + filter col to avoid .x/.y suffixes
adsl_sub <- adsl_raw %>%
  select(USUBJID, EFFFL) %>%
  filter(!is.na(EFFFL) & EFFFL == "Y")

# Prepare ADEFF subset: keep needed cols only
# Drop/rename overlapping cols except keys before join: adsl_sub has USUBJID,EFFFL;
# adeff has no EFFFL so no conflict except USUBJID (key)
adeff_sub <- adeff_raw %>%
  select(any_of(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM",
                  "AVISIT","AVISITN","AVAL","BASE","CHG","ANL01FL")))

# Check columns for filter exist after select
stopifnot(all(c("USUBJID","PARAMCD","ANL01FL","CHG") %in% names(adeff_sub)))
stopifnot("EFFFL" %in% names(adsl_sub))

# Step 1: inner join + filters (SAS: b.EFFFL='Y', PARAMCD, ANL01FL='Y', CHG ne .)
eff <- adeff_sub %>%
  inner_join(adsl_sub, by = "USUBJID") %>%
  filter(PARAMCD == eff_param,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(CHG)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN,
         AVAL, BASE, CHG, ANL01FL) %>%
  arrange(TRTAN, AVISITN, USUBJID)

message(paste0("eff rows=", nrow(eff)))
if (nrow(eff) == 0) stop("eff is zero rows: join/filter bug")

# Ensure types
eff <- eff %>%
  mutate(TRTAN = as.numeric(TRTAN),
         AVISITN = as.numeric(AVISITN),
         CHG = as.numeric(CHG),
         TRTA = as.character(TRTA),
         AVISIT = as.character(AVISIT))

# Step 2: PROC MEANS NWAY CLASS TRTA TRTAN AVISIT AVISITN VAR CHG -> n mean stderr lclm uclm
# SAS LCLM/UCLM = mean +/- t(0.975, n-1)*SE
stats <- eff %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(
    n = sum(!is.na(CHG)),
    mean = ifelse(sum(!is.na(CHG)) > 0, mean(CHG, na.rm = TRUE), NA_real_),
    sd = ifelse(sum(!is.na(CHG)) > 1, sd(CHG, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd / sqrt(n),
    tcrit = ifelse(!is.na(n) & n > 1, qt(0.975, df = n - 1), NA_real_),
    lcl = mean - tcrit * se,
    ucl = mean + tcrit * se
  ) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl) %>%
  arrange(TRTAN, AVISITN)

# Step 3: offset
plot_data <- stats %>%
  mutate(
    xpos = dplyr::case_when(
      TRTAN == 1 ~ AVISITN - 0.15,
      TRTAN == 2 ~ AVISITN,
      TRTAN == 3 ~ AVISITN + 0.15,
      TRUE ~ AVISITN
    )
  ) %>%
  arrange(TRTAN, AVISITN)

# Write CSV of plotted data
write.csv(plot_data, file.path(out, "f_mean_cfb.csv"), row.names = FALSE, na = "")

# Titles / footnotes verbatim
sponsor_lbl <- "[Sponsor Name]"
protocol_lbl <- "[Protocol Number]"
cutdate_lbl <- "[DDMONYYYY]"
run_date9 <- toupper(format(Sys.Date(), "%d%b%Y"))

title1_txt <- paste0(sponsor_lbl, "   Page 1")
title2_txt <- paste0("Protocol: ", protocol_lbl)
title4_txt <- "Figure 14.2.4"
title5_txt <- "Mean Change from Baseline Over Time"
title6_txt <- "Efficacy Analysis Set"

fn1 <- paste0("Data cutoff: ", cutdate_lbl, ".")
fn2 <- "Error bars represent 95% confidence intervals."
fn3 <- "n = number of subjects with non-missing change from baseline."
fn4 <- paste0("Program: f_mean_cfb.sas | Run: ", run_date9)

# Treatment order by TRTAN
trt_order <- plot_data %>% distinct(TRTA, TRTAN) %>% arrange(TRTAN)
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_order$TRTA)

# Visit tick labels: map AVISITN -> AVISIT (first)
visit_map <- plot_data %>% distinct(AVISITN, AVISIT) %>% arrange(AVISITN)
x_breaks <- sort(unique(plot_data$AVISITN))
x_labels <- sapply(x_breaks, function(v) {
  lab <- visit_map$AVISIT[visit_map$AVISITN == v]
  if (length(lab) == 0 || is.na(lab[1])) as.character(v) else lab[1]
})

full_title <- paste(title1_txt, title2_txt, title4_txt, title5_txt, title6_txt, sep = "\n")
full_caption <- paste(fn1, fn2, fn3, fn4, sep = "\n")

p <- ggplot(plot_data, aes(x = xpos, y = mean, group = TRTA, color = TRTA, shape = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.08, linewidth = 0.7, na.rm = TRUE) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 3, fill = "white", na.rm = TRUE) +
  scale_x_continuous(breaks = x_breaks, labels = x_labels) +
  labs(
    title = full_title,
    x = "Visit (Week)",
    y = y_label,
    color = "Treatment",
    shape = "Treatment",
    caption = full_caption
  ) +
  theme_bw(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid.major.y = element_line(linetype = "dotted", color = "lightgray"),
    panel.grid.minor.y = element_line(linetype = "dotted", color = "lightgray"),
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0, size = 8)
  ) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1))

# 10in x 6.5in as in ODS GRAPHICS
ggsave(file.path(out, "f_mean_cfb.png"), plot = p, width = 10, height = 6.5, units = "in", dpi = 300)

# Step 5: Supporting data table (n per visit per treatment) - PROC TABULATE TRTA, AVISIT*n*sum
# Long table CSV
ntable_long <- plot_data %>% select(TRTA, TRTAN, AVISIT, AVISITN, n) %>% arrange(TRTAN, AVISITN)
write.csv(ntable_long, file.path(out, "f_mean_cfb_table.csv"), row.names = FALSE, na = "")

# Wide matrix for text rendering
ntable_wide <- ntable_long %>%
  select(TRTA, AVISIT, n) %>%
  tidyr::pivot_wider(names_from = AVISIT, values_from = n)

# Build plain-text rendering containing same numbers SAS shows
txt_file <- file.path(out, "f_mean_cfb_table.txt")
con <- file(txt_file, open = "wt")
cat(paste0(title1_txt, "\n"), file = con)
cat(paste0(title2_txt, "\n"), file = con)
cat("\n", file = con)
cat(paste0(title4_txt, "\n"), file = con)
cat(paste0(title5_txt, "\n"), file = con)
cat(paste0(title6_txt, "\n"), file = con)
cat("\n", file = con)
cat("Supporting Data Table: n per Visit per Treatment (sum of n)\n", file = con)
cat(paste0("Parameter: ", eff_param, "\n"), file = con)
cat("\n", file = con)
# Detail rows: TRTA x AVISIT with n, mean, SE, 95% CI
detail <- plot_data %>%
  arrange(TRTAN, AVISITN) %>%
  mutate(
    mean_d = sprintf("%.2f", round_half_up(mean, 2)),
    se_d = ifelse(is.na(se), "", sprintf("%.2f", round_half_up(se, 2))),
    lcl_d = ifelse(is.na(lcl), "", sprintf("%.2f", round_half_up(lcl, 2))),
    ucl_d = ifelse(is.na(ucl), "", sprintf("%.2f", round_half_up(ucl, 2)))
  )
cat("Detail summary by treatment and visit:\n", file = con)
cat(sprintf("%-25s %-15s %6s %10s %10s %12s %12s\n",
            "Treatment", "Visit", "n", "Mean", "SE", "LCLM", "UCLM"), file = con)
for (i in seq_len(nrow(detail))) {
  cat(sprintf("%-25s %-15s %6d %10s %10s %12s %12s\n",
              substr(as.character(detail$TRTA[i]), 1, 25),
              substr(as.character(detail$AVISIT[i]), 1, 15),
              detail$n[i],
              detail$mean_d[i],
              detail$se_d[i],
              detail$lcl_d[i],
              detail$ucl_d[i]), file = con)
}
cat("\n", file = con)
cat("Tabulate (TRTA rows x AVISIT columns, sum of n):\n", file = con)
# header
cn <- names(ntable_wide)
cat(paste(cn, collapse = " | "), "\n", file = con)
for (i in seq_len(nrow(ntable_wide))) {
  vals <- sapply(ntable_wide[i, ], function(v) ifelse(is.na(v), ".", as.character(v)))
  cat(paste(vals, collapse = " | "), "\n", file = con)
}
cat("\n", file = con)
cat(paste0(fn1, "\n"), file = con)
cat(paste0(fn2, "\n"), file = con)
cat(paste0(fn3, "\n"), file = con)
cat(paste0(fn4, "\n"), file = con)
close(con)

message("f_mean_cfb completed successfully.")
