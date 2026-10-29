```r
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

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

get_ext <- function(p) {
  tolower(sub(".*\\.([^.]+)$", "\\1", basename(p)))
}

read_dataset <- function(lib, name) {
  f <- list.files(lib, pattern = paste0("^", name, "\\..*$"),
                  full.names = TRUE, ignore.case = TRUE)
  if (length(f) == 0) stop(paste0("Dataset not found: ", lib, "/", name))
  ext <- get_ext(f)
  ord <- order(match(ext, c("sas7bdat", "xpt", "csv")))
  ord <- ord[!is.na(ord)]
  if (length(ord) > 0) f <- f[ord]
  fp <- f[1]
  e <- get_ext(fp)
  if (e == "xpt") {
    df <- haven::read_xpt(fp)
  } else if (e == "sas7bdat") {
    df <- haven::read_sas(fp)
  } else if (e == "csv") {
    df <- read.csv(fp, stringsAsFactors = FALSE, check.names = FALSE)
  } else {
    df <- tryCatch(haven::read_xpt(fp),
      error = function(err) haven::read_sas(fp))
  }
  names(df) <- toupper(names(df))
  as.data.frame(df, stringsAsFactors = FALSE)
}

cat("Reading ADEFF and ADSL...\n")
adeff <- read_dataset(adam_path, "adeff")
adsl  <- read_dataset(adam_path, "adsl")

eff_param <- "ACTOT"
y_label <- "Change from Baseline in ADAS-Cog Total Score"

need_a <- intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","AVISIT","AVISITN",
                      "AVAL","BASE","CHG","ANL01FL"), names(adeff))
need_b <- intersect(c("USUBJID","EFFFL"), names(adsl))
a <- adeff[, need_a, drop = FALSE]
b <- adsl[, need_b, drop = FALSE]
eff <- merge(a, b, by = "USUBJID", all = FALSE)
eff <- eff[!is.na(eff$EFFFL) & eff$EFFFL == "Y", , drop = FALSE]
eff <- eff[!is.na(eff$PARAMCD) & eff$PARAMCD == eff_param, , drop = FALSE]
eff <- eff[!is.na(eff$ANL01FL) & eff$ANL01FL == "Y", , drop = FALSE]
eff <- eff[!is.na(eff$CHG), , drop = FALSE]
eff <- eff[order(eff$TRTAN, eff$AVISITN, eff$TRTA, eff$USUBJID), , drop = FALSE]
cat(paste0("EFF rows: ", nrow(eff), "\n"))

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
  stats$lcl <- NA_real_
  stats$ucl <- NA_real_
  idx <- which(!is.na(stats$n) & stats$n > 1 & !is.na(stats$mean) & !is.na(stats$se))
  if (length(idx) > 0) {
    tcrit <- qt(0.975, df = stats$n[idx] - 1)
    stats$lcl[idx] <- stats$mean[idx] - tcrit * stats$se[idx]
    stats$ucl[idx] <- stats$mean[idx] + tcrit * stats$se[idx]
  }
  stats$se[stats$n <= 1] <- NA_real_
  stats$std <- NULL
  stats <- stats[order(stats$TRTAN, stats$AVISITN, stats$TRTA, stats$AVISIT), , drop = FALSE]
} else {
  stats <- data.frame(TRTA=character(0), TRTAN=numeric(0), AVISIT=character(0),
                       AVISITN=numeric(0), n=numeric(0), mean=numeric(0),
                       se=numeric(0), lcl=numeric(0), ucl=numeric(0),
                       stringsAsFactors=FALSE)
}

plot_data <- stats
plot_data$xpos <- plot_data$AVISITN
if (nrow(plot_data) > 0) {
  i1 <- which(!is.na(plot_data$TRTAN) & plot_data$TRTAN == 1)
  i2 <- which(!is.na(plot_data$TRTAN) & plot_data$TRTAN == 2)
  i3 <- which(!is.na(plot_data$TRTAN) & plot_data$TRTAN == 3)
  if (length(i1) > 0) plot_data$xpos[i1] <- plot_data$AVISITN[i1] - 0.15
  if (length(i3) > 0) plot_data$xpos[i3] <- plot_data$AVISITN[i3] + 0.15
}
plot_data <- plot_data[, c("TRTA","TRTAN","AVISIT","AVISITN","n","mean","se","lcl","ucl","xpos")]

write.csv(plot_data, file.path(out, "f_mean_cfb_data.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "f_mean_cfb_plotdata.csv"), row.names = FALSE, na = "")

if (nrow(plot_data) > 0) {
  xmap <- plot_data %>% distinct(AVISITN, AVISIT) %>% arrange(AVISITN)
  xmap <- as.data.frame(xmap)
} else {
  xmap <- data.frame(AVISITN=numeric(0), AVISIT=character(0))
}

p <- ggplot(plot_data, aes(x = xpos, y = mean, colour = TRTA, group = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray50", linewidth = 0.6) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.05, linewidth = 0.75, na.rm = TRUE) +
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

if (nrow(plot_data) > 0) {
  visit_order <- plot_data %>% distinct(AVISIT, AVISITN) %>% arrange(AVISITN)
  visit_order <- as.data.frame(visit_order)
  trt_order <- plot_data %>% distinct(TRTA, TRTAN) %>% arrange(TRTAN)
  trt_order <- as.data.frame(trt_order)
  nlong <- plot_data[, c("TRTA","TRTAN","AVISIT","AVISITN","n")]
  nwide <- nlong %>%
    select(TRTA, AVISIT, n) %>%
    tidyr::pivot_wider(names_from = AVISIT, values_from = n, values_fn = sum) %>%
    as.data.frame()
  visit_cols <- intersect(visit_order$AVISIT, names(nwide))
  nwide <- merge(trt_order[, "TRTA", drop = FALSE], nwide, by = "TRTA", all.x = TRUE, sort = FALSE)
  nwide <- merge(trt_order, nwide, by = "TRTA", all.x = TRUE, sort = FALSE)
  nwide <- nwide[order(nwide$TRTAN), , drop = FALSE]
  table_csv <- nwide[, c("TRTA", visit_cols), drop = FALSE]
  for (cc in visit_cols) {
    table_csv[[cc]][is.na(table_csv[[cc]])] <- 0
  }
} else {
  visit_cols <- character(0)
  table_csv <- data.frame(TRTA=character(0), stringsAsFactors=FALSE)
}

write.csv(table_csv, file.path(out, "f_mean_cfb.csv"), row.names = FALSE, na = "")

fmt1 <- function(x) ifelse(is.na(x), ".", sprintf("%.1f", round_half_up(x, 1)))
fmt2 <- function(x) ifelse(is.na(x), ".", sprintf("%.2f", round_half_up(x, 2)))
fmt4 <- function(x) ifelse(is.na(x), ".", sprintf("%.4f", x))

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
  hdr <- sprintf("%-30s", "Treatment")
  for (cc in visit_cols) hdr <- paste0(hdr, sprintf(" | %-14s", substr(cc, 1, 14)))
  con_lines <- c(con_lines, hdr)
  con_lines <- c(con_lines, paste0(rep("-", nchar(hdr)), collapse = ""))
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
con_lines <- c(con_lines, "", "Supporting statistics by treatment and visit:", "")
if (nrow(plot_data) > 0) {
  det_hdr <- sprintf("%-22s %-6s %-14s %5s %12s %12s %12s %12s %12s %12s",
                     "Treatment", "TRTAN", "Visit", "n", "Mean", "Mean(1dec)", "SE", "SE(2dec)", "LCL", "UCL")
  con_lines <- c(con_lines, det_hdr,
    paste0(rep("-", nchar(det_hdr)), collapse = ""))
  od <- plot_data[order(plot_data$TRTAN, plot_data$AVISITN), , drop = FALSE]
  for (i in seq_len(nrow(od))) {
    con_lines <- c(con_lines, sprintf("%-22s %-6s %-14s %5s %12s %12s %12s %12s %12s %12s",
      substr(as.character(od$TRTA[i]),1,22),
      ifelse(is.na(od$TRTAN[i]), ".", as.character(od$TRTAN[i])),
      substr(as.character(od$AVISIT[i]),1,14),
      ifelse(is.na(od$n[i]), ".", as.character(od$n[i])),
      fmt4(od$mean[i]), fmt1(od$mean[i]), fmt4(od$se[i]), fmt2(od$se[i]),
      fmt4(od$lcl[i]), fmt4(od$ucl[i])))
  }
  con_lines <- c(con_lines, "",
    "Means with 1 decimal (for axis-label matching):",
    paste(fmt1(od$mean), collapse = " "),
    "Means with 2 decimals:",
    paste(fmt2(od$mean), collapse = " "),
    "SE with 2 decimals:",
    paste(fmt2(od$se), collapse = " "),
    "Visit weeks:",
    paste(as.character(od$AVISITN), collapse = " "))
}
writeLines(con_lines, txt_path)

cat("Done. Outputs written to OUT_DIR.\n")
```