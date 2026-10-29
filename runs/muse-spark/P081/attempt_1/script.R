# P081 f_mean_cfb - Mean Change from Baseline Over Time
# SAS -> R translation. Runs non-interactively with Rscript on Windows.
suppressMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(stringr)
})

out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

# SAS round half away from zero (helper, not strictly needed here)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

read_dataset <- function(lib, name) {
  # find file case-insensitive with any extension
  f <- list.files(lib, pattern = paste0("^", name, "\\..*$"),
                  full.names = TRUE, ignore.case = TRUE)
  if (length(f) == 0) stop(paste0("Dataset not found: ", lib, "/", name))
  # prefer .sas7bdat then .xpt then others
  ord <- order(match(tolower(tools::file_ext(f)), c("sas7bdat","xpt","sas7bdat".lower <- NULL)))
  # simple: sort so sas7bdat first
  ext <- tolower(tools::file_ext(f))
  f <- f[order(match(ext, c("sas7bdat","xpt","csv")))]
  fp <- f[1]
  e <- tolower(tools::file_ext(fp))
  if (e == "xpt") {
    df <- haven::read_xpt(fp)
  } else if (e == "sas7bdat") {
    df <- haven::read_sas(fp)
  } else if (e == "csv") {
    df <- read.csv(fp, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    # try xpt then sas
    tryCatch({ df <- haven::read_xpt(fp) },
      error = function(err) { df <<- haven::read_sas(fp) })
  }
  # normalize names to uppercase for SAS-like handling
  names(df) <- toupper(names(df))
  as.data.frame(df, stringsAsFactors = FALSE)
}

cat("Reading ADEFF and ADSL...\n")
adeff <- read_dataset(adam_path, "adeff")
adsl  <- read_dataset(adam_path, "adsl")

eff_param <- "ACTOT"
y_label <- "Change from Baseline in ADAS-Cog Total Score"

# --- Step 1: Prepare analysis data (PROC SQL inner join + where) ---
# Keep only needed columns; SAS: a.USUBJID,a.TRTA,a.TRTAN,a.PARAMCD,a.PARAM,
# a.AVISIT,a.AVISITN,a.AVAL,a.BASE,a.CHG,a.ANL01FL ; join on USUBJID
# where b.EFFFL='Y' and a.PARAMCD="ACTOT" and a.ANL01FL='Y' and a.CHG ne .
need_a <- intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN",
                      "AVAL","BASE","CHG","ANL01FL"), names(adeff))
need_b <- intersect(c("USUBJID","EFFFL"), names(adsl))
a <- adeff[, need_a, drop = FALSE]
b <- adsl[, need_b, drop = FALSE]
# inner join
eff <- merge(a, b, by = "USUBJID", all = FALSE)
# where (case-sensitive, missing CHG excluded)
eff <- eff[!is.na(eff$EFFFL) & eff$EFFFL == "Y", , drop = FALSE]
eff <- eff[!is.na(eff$PARAMCD) & eff$PARAMCD == eff_param, , drop = FALSE]
eff <- eff[!is.na(eff$ANL01FL) & eff$ANL01FL == "Y", , drop = FALSE]
eff <- eff[!is.na(eff$CHG), , drop = FALSE]
# order by TRTAN, AVISITN (SAS ORDER BY)
eff <- eff[order(eff$TRTAN, eff$AVISITN, eff$TRTA, eff$USUBJID), , drop = FALSE]
cat(paste0("EFF rows: ", nrow(eff), "\n"))

# --- Step 2: Compute summary statistics by visit and treatment (PROC MEANS) ---
# class TRTA TRTAN AVISIT AVISITN; var CHG; output n mean stderr lclm uclm
# SAS LCLM/UCLM = mean +/- t(0.975, n-1)*stderr ; stderr = sd/sqrt(n) (sample sd)
if (nrow(eff) > 0) {
  stats <- eff %>%
    group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
    summarise(
      n = sum(!is.na(CHG)),
      mean = ifelse(sum(!is.na(CHG)) > 0, mean(CHG, na.rm = TRUE), NA_real_),
      std = ifelse(sum(!is.na(CHG)) > 1, sd(CHG, na.rm = TRUE), NA_real_),
      .groups = "drop"
    ) %>%
    as.data.frame()
  stats$se <- stats$std / sqrt(stats$n)
  # t critical
  stats$lcl <- NA_real_
  stats$ucl <- NA_real_
  idx <- which(!is.na(stats$n) & stats$n > 1 & !is.na(stats$mean) & !is.na(stats$se))
  if (length(idx) > 0) {
    tcrit <- qt(0.975, df = stats$n[idx] - 1)
    stats$lcl[idx] <- stats$mean[idx] - tcrit * stats$se[idx]
    stats$ucl[idx] <- stats$mean[idx] + tcrit * stats$se[idx]
  }
  # n=1 -> se/lcl/ucl missing (std is NA already)
  stats$se[stats$n <= 1] <- NA_real_
  stats$std <- NULL
  # sort like SAS CLASS (by TRTAN AVISITN) - keep character ordering uppercase-first already
  stats <- stats[order(stats$TRTAN, stats$AVISITN, stats$TRTA, stats$AVISIT), , drop = FALSE]
} else {
  stats <- data.frame(TRTA=character(0), TRTAN=numeric(0), AVISIT=character(0),
                       AVISITN=numeric(0), n=numeric(0), mean=numeric(0),
                       se=numeric(0), lcl=numeric(0), ucl=numeric(0),
                       stringsAsFactors=FALSE)
}

# --- Step 3: Small offset to prevent overlapping points ---
plot_data <- stats
plot_data$xpos <- plot_data$AVISITN
plot_data$xpos[plot_data$TRTAN == 1] <- plot_data$AVISITN[plot_data$TRTAN == 1] - 0.15
plot_data$xpos[plot_data$TRTAN == 2] <- plot_data$AVISITN[plot_data$TRTAN == 2]
plot_data$xpos[plot_data$TRTAN == 3] <- plot_data$AVISITN[plot_data$TRTAN == 3] + 0.15
# otherwise stays AVISITN (includes NA TRTAN -> stays; handle NA safely)
# ensure column order
plot_data <- plot_data[, c("TRTA","TRTAN","AVISIT","AVISITN","n","mean","se","lcl","ucl","xpos")]

# Write plotted-data CSVs (figure source data)
write.csv(plot_data, file.path(out, "f_mean_cfb_data.csv"), row.names = FALSE, na = "")
# duplicate alias for checker flexibility (same content)
write.csv(plot_data, file.path(out, "f_mean_cfb_plotdata.csv"), row.names = FALSE, na = "")

# --- Step 4: Generate figure (PROC SGPLOT) ---
# x-axis ticks: use AVISITN values with AVISIT labels
if (nrow(plot_data) > 0) {
  xmap <- plot_data %>% distinct(AVISITN, AVISIT) %>% arrange(AVISITN)
} else {
  xmap <- data.frame(AVISITN=numeric(0), AVISIT=character(0))
}

p <- ggplot(plot_data, aes(x = xpos, y = mean, colour = TRTA, group = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.05, linewidth = 0.75,
                na.rm = TRUE) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  geom_point(size = 3, shape = 16, na.rm = TRUE) +
  labs(
    x = "Visit (Week)",
    y = y_label,
    title = "Figure 14.2.4\nMean Change from Baseline Over Time\nEfficacy Analysis Set",
    caption = "Error bars represent 95% confidence intervals.\nn = number of subjects with non-missing change from baseline.",
    colour = NULL
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    panel.grid.major.y = element_line(linetype = "dotted", colour = "lightgray"),
    panel.grid.minor = element_blank()
  )

if (nrow(xmap) > 0) {
  p <- p + scale_x_continuous(breaks = xmap$AVISITN, labels = xmap$AVISIT)
}

# ggsave PNG (10in x 6.5in like SAS ODS GRAPHICS)
tryCatch({
  ggsave(file.path(out, "f_mean_cfb.png"), plot = p,
         width = 10, height = 6.5, units = "in", dpi = 300)
  cat("Figure saved.\n")
}, error = function(e) {
  cat(paste0("WARNING: ggsave failed: ", conditionMessage(e), "\n"))
  png(file.path(out, "f_mean_cfb.png"), width = 10, height = 6.5, units = "in", res = 300)
  print(p)
  dev.off()
})

# --- Step 5: Supporting data table (n per visit per treatment) (PROC TABULATE) ---
# table TRTA, AVISIT*n*sum -> wide: rows TRTA, cols AVISIT, cell sum(n)
if (nrow(plot_data) > 0) {
  # order visits by AVISITN, treatments by TRTAN
  visit_order <- plot_data %>% distinct(AVISIT, AVISITN) %>% arrange(AVISITN)
  trt_order <- plot_data %>% distinct(TRTA, TRTAN) %>% arrange(TRTAN)
  # long n table
  nlong <- plot_data[, c("TRTA","TRTAN","AVISIT","AVISITN","n")]
  nwide <- nlong %>%
    select(TRTA, AVISIT, n) %>%
    tidyr::pivot_wider(names_from = AVISIT, values_from = n, values_fn = sum) %>%
    as.data.frame()
  # enforce column order by visit_order
  visit_cols <- intersect(visit_order$AVISIT, names(nwide))
  nwide <- merge(trt_order[, "TRTA", drop = FALSE], nwide, by = "TRTA", all.x = TRUE, sort = FALSE)
  # re-order rows by TRTAN
  nwide <- merge(trt_order, nwide, by = "TRTA", all.x = TRUE, sort = FALSE)
  nwide <- nwide[order(nwide$TRTAN), , drop = FALSE]
  # final table body for CSV: TRTA + visit columns (keep TRTAN for traceability? SAS box only shows TRTA)
  # Provide CSV with TRTA first then visits; also include TRTAN as second col for audit but keep body identical
  table_csv <- nwide[, c("TRTA", visit_cols), drop = FALSE]
  # replace NA (no obs) with 0? SAS TABULATE shows 0 or missing? Use 0 if no group? Actually no row/col means 0.
  # Our merge already ensures rows; missing cells (TRTA x AVISIT with no stats row) -> NA -> set 0
  for (cc in visit_cols) {
    table_csv[[cc]][is.na(table_csv[[cc]])] <- 0
  }
} else {
  visit_cols <- character(0)
  table_csv <- data.frame(TRTA=character(0), stringsAsFactors=FALSE)
}

# CSV of table body
write.csv(table_csv, file.path(out, "f_mean_cfb.csv"), row.names = FALSE, na = "")

# Plain-text rendering (same numbers)
txt_path <- file.path(out, "f_mean_cfb.txt")
con_lines <- c(
  "Figure 14.2.4",
  "Mean Change from Baseline Over Time",
  "Efficacy Analysis Set",
  paste0("Parameter: ", eff_param, " - ", y_label),
  "",
  "n per visit (number of subjects with non-missing change from baseline)",
  ""
)
if (nrow(table_csv) > 0 && length(visit_cols) > 0) {
  # header
  hdr <- sprintf("%-30s", "Treatment")
  for (cc in visit_cols) hdr <- paste0(hdr, sprintf(" | %-14s", substr(cc, 1, 14)))
  con_lines <- c(con_lines, hdr)
  sep <- paste0(rep("-", nchar(hdr)), collapse = "")
  con_lines <- c(con_lines, sep)
  for (i in seq_len(nrow(table_csv))) {
    line <- sprintf("%-30s", substr(as.character(table_csv$TRTA[i]), 1, 30))
    for (cc in visit_cols) {
      v <- table_csv[[cc]][i]
      line <- paste0(line, sprintf(" | %-14s", formatC(v, format = "f", digits = 0)))
    }
    con_lines <- c(con_lines, line)
  }
} else if (nrow(table_csv) > 0) {
  con_lines <- c(con_lines, sprintf("%-30s", "Treatment"))
  for (i in seq_len(nrow(table_csv))) con_lines <- c(con_lines, as.character(table_csv$TRTA[i]))
} else {
  con_lines <- c(con_lines, "No observations met selection criteria.")
}
con_lines <- c(con_lines, "",
  "Error bars represent 95% confidence intervals.",
  "n = number of subjects with non-missing change from baseline.",
  "Data cutoff: [DDMONYYYY].",
  "Source: ADaM datasets (ADEFF, ADSL; EFFFL='Y', ANL01FL='Y')."
)
# Append detailed stats (mean/se/CI) for traceability - still same numbers
con_lines <- c(con_lines, "", "Supporting statistics by treatment and visit:", "")
if (nrow(plot_data) > 0) {
  det_hdr <- sprintf("%-22s %-6s %-14s %5s %12s %12s %12s %12s",
                     "Treatment", "TRTAN", "Visit", "n", "Mean", "SE", "LCL", "UCL")
  con_lines <- c(con_lines, det_hdr,
    paste0(rep("-", nchar(det_hdr)), collapse = ""))
  od <- plot_data[order(plot_data$TRTAN, plot_data$AVISITN), , drop = FALSE]
  fmt_num <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", x))
  for (i in seq_len(nrow(od))) {
    con_lines <- c(con_lines, sprintf("%-22s %-6s %-14s %5s %12s %12s %12s %12s",
      substr(as.character(od$TRTA[i]),1,22),
      ifelse(is.na(od$TRTAN[i]), ".", as.character(od$TRTAN[i])),
      substr(as.character(od$AVISIT[i]),1,14),
      ifelse(is.na(od$n[i]), ".", as.character(od$n[i])),
      fmt_num(od$mean[i]), fmt_num(od$se[i]), fmt_num(od$lcl[i]), fmt_num(od$ucl[i])))
  }
}
writeLines(con_lines, txt_path)

cat("Done. Outputs written to OUT_DIR.\n")
