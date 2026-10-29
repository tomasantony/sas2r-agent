# P043 Figure 11.1 - Scatterplot BASE vs AVAL for SODIUM, grouped by TRTA
# SAS: test data (unused) + ADLBC SODIUM where AENTMTFL=Y & AVISITN NE 99
#      LLN/ULN = most frequent A1LO/A1HI, SGRENDER scatterplot + ref lines

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
  library(readr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
adam_split_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
adam_dir       <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

# SAS round half away from zero (helper, kept for fidelity)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- floor(z + 0.5 + 1e-10)
  posneg * z / 10^digits
}

find_adam_file <- function(dir, name) {
  # try .sas7bdat, .xpt, .xpt.gz etc, case-insensitive
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, full.names = TRUE)
  # exact case-insensitive match on basename without extension
  pat <- paste0("^", name, "\\.")
  idx <- grep(pat, basename(files), ignore.case = TRUE)
  if (length(idx) > 0) return(files[idx[1]])
  # fallback: contains name
  idx2 <- grep(name, basename(files), ignore.case = TRUE)
  if (length(idx2) > 0) return(files[idx2[1]])
  return(NULL)
}

read_sas_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  # handle .xpt.gz etc: check suffix
  lp <- tolower(path)
  if (grepl("\\.xpt(\\.gz)?$", lp)) {
    df <- haven::read_xpt(path)
  } else if (grepl("\\.sas7bdat$", lp)) {
    df <- haven::read_sas(path)
  } else {
    # try sas then xpt
    df <- tryCatch(haven::read_sas(path), error = function(e) haven::read_xpt(path))
  }
  df <- haven::zap_labels(df)
  df <- haven::zap_formats(df)
  as.data.frame(df, stringsAsFactors = FALSE)
}

adlbc_path <- find_adam_file(adam_split_dir, "ADLBC")
if (is.null(adlbc_path)) {
  # fallback to adam dir
  adlbc_path <- find_adam_file(adam_dir, "ADLBC")
}
if (is.null(adlbc_path)) {
  stop(paste0("ADLBC file not found in ", adam_split_dir, " or ", adam_dir))
}
cat("Reading ADLBC from:", adlbc_path, "\n")

ADLBC <- read_sas_any(adlbc_path)
cat("ADLBC rows:", nrow(ADLBC), " cols:", paste(names(ADLBC), collapse=","), "\n")

# Ensure expected columns exist (SAS names, case-insensitive match)
# Normalize names to uppercase for processing, keep original for output
orig_names <- names(ADLBC)
names(ADLBC) <- toupper(names(ADLBC))

required <- c("USUBJID","TRTAN","TRTA","PARAMCD","PARAM","AVAL","BASE","A1LO","A1HI","AENTMTFL","AVISITN")
missing_req <- setdiff(required, names(ADLBC))
if (length(missing_req) > 0) {
  stop(paste0("ADLBC missing columns: ", paste(missing_req, collapse=",")))
}

# DATA ADLBC; SET ADAM.ADLBC; WHERE PARAMCD="SODIUM";
adlbc_sod <- ADLBC %>% filter(!is.na(PARAMCD) & PARAMCD == "SODIUM")
cat("SODIUM rows:", nrow(adlbc_sod), "\n")

# DATA START; SET ADLBC; WHERE AENTMTFL="Y" AND AVISITN NE 99;
# SAS: missing AVISITN satisfies NE 99; missing AENTMTFL does not satisfy ="Y"
START <- adlbc_sod %>%
  filter(!is.na(AENTMTFL) & AENTMTFL == "Y",
         is.na(AVISITN) | AVISITN != 99) %>%
  select(USUBJID, TRTAN, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI)
cat("START rows:", nrow(START), "\n")

# PROC FREQ TABLES PARAMCD*A1LO / OUT=LOW; (missing excluded by default)
LOW <- START %>%
  filter(!is.na(A1LO)) %>%
  count(PARAMCD, A1LO, name = "COUNT") %>%
  arrange(PARAMCD, A1LO)
# PROC SORT BY PARAMCD DESCENDING COUNT; IF FIRST.PARAMCD;
LOW_sorted <- LOW %>% arrange(PARAMCD, desc(COUNT), A1LO)
LOW_first <- LOW_sorted %>% group_by(PARAMCD) %>% slice(1) %>% ungroup()

HIGH <- START %>%
  filter(!is.na(A1HI)) %>%
  count(PARAMCD, A1HI, name = "COUNT") %>%
  arrange(PARAMCD, A1HI)
HIGH_sorted <- HIGH %>% arrange(PARAMCD, desc(COUNT), A1HI)
HIGH_first <- HIGH_sorted %>% group_by(PARAMCD) %>% slice(1) %>% ungroup()

LLN <- if (nrow(LOW_first) > 0) LOW_first$A1LO[1] else NA_real_
ULN <- if (nrow(HIGH_first) > 0) HIGH_first$A1HI[1] else NA_real_
cat("LLN:", LLN, " ULN:", ULN, "\n")

# Fallback if no non-missing (should not happen)
if (is.na(LLN) || is.na(ULN)) {
  cat("WARNING: LLN/ULN missing, attempting median fallback\n")
  if (is.na(LLN)) LLN <- suppressWarnings(median(START$A1LO, na.rm = TRUE))
  if (is.na(ULN)) ULN <- suppressWarnings(median(START$A1HI, na.rm = TRUE))
}
cat("Final LLN=", LLN, " ULN=", ULN, "\n", sep="")

# Plotted data = WORK.START (BASE on X, AVAL on Y, group TRTA)
plot_data <- START
# Ensure numeric
plot_data$BASE <- suppressWarnings(as.numeric(plot_data$BASE))
plot_data$AVAL <- suppressWarnings(as.numeric(plot_data$AVAL))
plot_data$TRTA <- as.character(plot_data$TRTA)

# Remove rows with missing BASE or AVAL for plotting (SAS would not plot them)
plot_data_nmiss <- plot_data %>% filter(!is.na(BASE) & !is.na(AVAL))
cat("Plotted (non-missing BASE/AVAL) rows:", nrow(plot_data_nmiss), "\n")

# Treatment ordering: SAS discretelegend order = data order; use sorted unique (uppercase before lowercase per SAS sort)
trt_levels <- sort(unique(plot_data_nmiss$TRTA))
cat("TRTA levels:", paste(trt_levels, collapse=" | "), "\n")
plot_data_nmiss$TRTA <- factor(plot_data_nmiss$TRTA, levels = trt_levels)

# Colors from template DataColors first 3: CXFFA53D CXF7DF54 CXEF6B48 ...
sas_colors <- c("#FFA53D", "#F7DF54", "#EF6B48", "#61A6E7", "#CB79C8",
                "#85CE79", "#967CD0", "#DD6475", "#9DAA2D", "#6C69D9",
                "#2EA64A", "#C17D35")
# Symbols: PLUS circlefilled trianglefilled -> +, 19, 17
sas_shapes <- c(3, 19, 17, 15, 18, 8, 0, 1, 2, 5, 6, 7)
n_trt <- length(trt_levels)
use_colors <- rep(sas_colors, length.out = max(n_trt, 1))
use_shapes <- rep(sas_shapes, length.out = max(n_trt, 1))
names(use_colors) <- trt_levels
names(use_shapes) <- trt_levels

# Axis ranges: SAS rowdatarange=data coldatarange=data -> data-driven
# Keep ggplot default expansion but ensure ref lines visible
x_rng <- range(plot_data_nmiss$BASE, na.rm = TRUE)
y_rng <- range(plot_data_nmiss$AVAL, na.rm = TRUE)
# Expand to include LLN/ULN if outside
if (!is.na(LLN)) { x_rng[1] <- min(x_rng[1], LLN); y_rng[1] <- min(y_rng[1], LLN) }
if (!is.na(ULN)) { x_rng[2] <- max(x_rng[2], ULN); y_rng[2] <- max(y_rng[2], ULN) }

p <- ggplot(plot_data_nmiss, aes(x = BASE, y = AVAL, color = TRTA, shape = TRTA)) +
  geom_point(size = 2.5, alpha = 0.9) +
  scale_color_manual(values = use_colors, name = "Treatment") +
  scale_shape_manual(values = use_shapes, name = "Treatment") +
  # 45-degree reference y=x (SAS drawline wallpercent diagonal intent)
  geom_abline(intercept = 0, slope = 1, color = "grey60", linewidth = 0.7, linetype = "solid") +
  geom_hline(yintercept = ULN, color = "#0000FF", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = LLN, color = "#FF0000", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = LLN, color = "#FF0000", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = ULN, color = "#0000FF", linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Figure 11.1 Scatterplot",
    subtitle = NULL,
    x = "Min. Baseline",
    y = "Min. Post Baseline",
    caption = paste0("LLN ULN ....  (LLN=", LLN, " ULN=", ULN, ")")
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.caption = element_text(hjust = 0, size = 9),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA),
    legend.key = element_rect(fill = "white", color = NA)
  ) +
  guides(color = guide_legend(title = "Treatment", order = 1),
         shape = guide_legend(title = "Treatment", order = 1))

# Annotate ULN/LLN curve labels at min position (left/bottom)
# Use data coords: x = min BASE, y = ULN/LLN etc.
x_min_annot <- min(plot_data_nmiss$BASE, na.rm = TRUE)
y_min_annot <- min(plot_data_nmiss$AVAL, na.rm = TRUE)
p <- p +
  annotate("text", x = x_min_annot, y = ULN, label = "ULN", color = "#0000FF", size = 3.5, vjust = -0.6, hjust = 0) +
  annotate("text", x = x_min_annot, y = LLN, label = "LLN", color = "#FF0000", size = 3.5, vjust = -0.6, hjust = 0) +
  annotate("text", x = LLN, y = y_min_annot, label = "LLN", color = "#FF0000", size = 3.5, vjust = 0, hjust = 0, angle = 90) +
  annotate("text", x = ULN, y = y_min_annot, label = "ULN", color = "#0000FF", size = 3.5, vjust = 0, hjust = 0, angle = 90)

# Save figure: designwidth=951 designheight=689
png_path <- file.path(out, "sgrender.png")
# ggsave with pixel units to match SAS design size
ggplot2::ggsave(filename = png_path, plot = p, width = 951, height = 689, units = "px", dpi = 100)
cat("Wrote figure:", png_path, "\n")

# CSV of plotted data (same columns as WORK.START)
out_csv <- plot_data
# Restore original case variable names as in SAS (uppercase)
# Write with na="" per spec
write.csv(out_csv, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
cat("Wrote plotted data CSV: sgrender.csv rows=", nrow(out_csv), "\n", sep="")

# Also write a small table summary (LLN/ULN) as txt for traceability (not required but harmless)
# The spec requires each figure plus CSV; we keep outputs minimal to expected names.
# Ensure sgrender.csv contains BASE, AVAL, TRTA for verification.
