# P043 Figure 11.1 - Scatterplot (SAS -> R)
# Reads ADLBC (SODIUM, last post-baseline), derives LLN/ULN as most frequent
# A1LO/A1HI, renders scatterplot BASE vs AVAL by TRTA with reference lines.

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

root     <- "/bench"
rawsplit <- "/bench/data/derived/cdisc-pilot/adam-split"
rawadam  <- "/bench/data/raw/cdisc-pilot/adam"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

# SAS round half away from zero helper (for compliance)
round_half_up <- function(x, d = 0) {
  s <- sign(x)
  a <- abs(x)
  p <- 10^d
  s * floor(a * p + 0.5 + 1e-12) / p
}

find_dataset <- function(name, dirs) {
  for (d in dirs) {
    if (is.na(d) || d == "" || !dir.exists(d)) next
    f <- list.files(d, pattern = paste0("^", name, "\\.(sas7bdat|xpt)$"),
                    full.names = TRUE, ignore.case = TRUE)
    if (length(f) > 0) return(f[1])
    f2 <- list.files(d, pattern = paste0("^", name, "\\."),
                     full.names = TRUE, ignore.case = TRUE, recursive = TRUE)
    if (length(f2) > 0) return(f2[1])
  }
  return(NULL)
}

read_sas_dataset <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "xpt") {
    haven::read_xpt(path)
  } else {
    haven::read_sas(path)
  }
}

adlbc_path <- find_dataset("ADLBC", c(rawsplit, rawadam, file.path(root, "data/derived/cdisc-pilot/adam")))
cat("ADLBC path:", ifelse(is.null(adlbc_path), "NOT FOUND", adlbc_path), "\n")
stopifnot(!is.null(adlbc_path))

ADLBC <- read_sas_dataset(adlbc_path)
cat("ADLBC rows:", nrow(ADLBC), "cols:", ncol(ADLBC), "\n")

# Normalize names lookup (case-insensitive, keep original case)
nm <- names(ADLBC)
up <- toupper(nm)
getcol <- function(want) {
  idx <- match(toupper(want), up)
  if (is.na(idx)) return(NULL)
  nm[idx]
}
c_PARAMCD  <- getcol("PARAMCD")
c_AENTMTFL <- getcol("AENTMTFL")
c_AVISITN  <- getcol("AVISITN")
c_USUBJID  <- getcol("USUBJID")
c_TRTAN    <- getcol("TRTAN")
c_TRTA     <- getcol("TRTA")
c_PARAM    <- getcol("PARAM")
c_AVAL     <- getcol("AVAL")
c_BASE     <- getcol("BASE")
c_A1LO     <- getcol("A1LO")
c_A1HI     <- getcol("A1HI")
stopifnot(!is.null(c_PARAMCD), !is.null(c_AENTMTFL), !is.null(c_AVISITN),
          !is.null(c_USUBJID), !is.null(c_TRTA), !is.null(c_AVAL),
          !is.null(c_BASE), !is.null(c_A1LO), !is.null(c_A1HI))

# DATA ADLBC; SET ADAM.ADLBC; WHERE PARAMCD="SODIUM";
# DATA START; SET ADLBC; WHERE AENTMTFL="Y" AND AVISITN NE 99;
# KEEP USUBJID TRTAN TRTA PARAMCD PARAM AVAL BASE A1LO A1HI;
ADLBC_SOD <- ADLBC[ADLBC[[c_PARAMCD]] == "SODIUM", , drop = FALSE]

START <- ADLBC_SOD[ADLBC_SOD[[c_AENTMTFL]] == "Y" &
                   !is.na(ADLBC_SOD[[c_AVISITN]]) &
                   ADLBC_SOD[[c_AVISITN]] != 99, , drop = FALSE]

keep_want <- c("USUBJID","TRTAN","TRTA","PARAMCD","PARAM","AVAL","BASE","A1LO","A1HI")
keep_actual <- sapply(keep_want, function(w) getcol(w))
keep_actual <- keep_actual[!sapply(keep_actual, is.null)]
START <- START[, unique(keep_actual), drop = FALSE]
# rename to canonical uppercase names for output
names(START) <- toupper(names(START))
# ensure all keep columns exist (TRTAN/PARAM may be missing in some pilots -> add as NA if needed)
for (w in keep_want) {
  if (!(w %in% names(START))) START[[w]] <- NA
}
START <- START[, keep_want, drop = FALSE]
cat("START rows:", nrow(START), "\n")
stopifnot(exists("START"), nrow(START) > 0)
stopifnot(all(c("BASE","AVAL","TRTA") %in% names(START)))

# PROC FREQ DATA=START NOPRINT; TABLES PARAMCD*A1LO / OUT=LOW; TABLES PARAMCD*A1HI / OUT=HIGH;
# OUT contains COUNT; then sort BY PARAMCD DESCENDING COUNT; FIRST.PARAMCD -> LLN/ULN
freq_out <- function(df, var) {
  # SAS PROC FREQ excludes missing? Keep non-missing for mode; emulate counts of non-missing
  sub <- df[, c("PARAMCD", var), drop = FALSE]
  sub <- sub[!is.na(sub[[var]]), , drop = FALSE]
  if (nrow(sub) == 0) return(data.frame(PARAMCD=character(0), V=numeric(0), COUNT=integer(0)))
  tb <- as.data.frame(table(sub$PARAMCD, sub[[var]], useNA = "no"), stringsAsFactors = FALSE)
  names(tb) <- c("PARAMCD", "V", "COUNT")
  tb$COUNT <- as.integer(tb$COUNT)
  # convert V back to numeric if original numeric
  if (is.numeric(df[[var]])) tb$V <- suppressWarnings(as.numeric(tb$V)) else tb$V <- as.character(tb$V)
  tb <- tb[tb$COUNT > 0, , drop = FALSE]
  tb
}
LOW_raw  <- freq_out(START, "A1LO")
HIGH_raw <- freq_out(START, "A1HI")

# SAS sort: BY PARAMCD DESCENDING COUNT (character ordering uppercase before lowercase; missing first)
# For single PARAMCD this reduces to max COUNT; tie -> smallest formatted value first (PROC FREQ OUT order)
pick_mode <- function(tb) {
  if (nrow(tb) == 0) return(NA)
  # order by PARAMCD asc (SAS binary), COUNT desc
  # tie-break by V ascending to mimic FREQ OUT order
  tb <- tb[order(tb$PARAMCD, -tb$COUNT, tb$V), , drop = FALSE]
  # FIRST.PARAMCD
  tb <- tb[!duplicated(tb$PARAMCD), , drop = FALSE]
  tb
}
LOW_pick  <- pick_mode(LOW_raw)
HIGH_pick <- pick_mode(HIGH_raw)
LLN <- if (nrow(LOW_pick) > 0) LOW_pick$V[LOW_pick$PARAMCD == "SODIUM"][1] else NA
ULN <- if (nrow(HIGH_pick) > 0) HIGH_pick$V[HIGH_pick$PARAMCD == "SODIUM"][1] else NA
# fallback: if PARAMCD-specific missing, take first
if (length(LLN) == 0 || is.na(LLN)) LLN <- if (nrow(LOW_pick) > 0) LOW_pick$V[1] else NA
if (length(ULN) == 0 || is.na(ULN)) ULN <- if (nrow(HIGH_pick) > 0) HIGH_pick$V[1] else NA
cat("LLN =", LLN, " ULN =", ULN, "\n")
stopifnot(!is.na(LLN), !is.na(ULN))
LLN <- as.numeric(LLN); ULN <- as.numeric(ULN)

# ---- Figure: SGDESIGN equivalent ----
# dynamic _BASE="BASE" _POST="AVAL" _TRT="TRTA"
plot_df <- START
plot_df$BASE <- suppressWarnings(as.numeric(plot_df$BASE))
plot_df$AVAL <- suppressWarnings(as.numeric(plot_df$AVAL))
plot_df$TRTA <- as.character(plot_df$TRTA)
plot_df <- plot_df[!is.na(plot_df$BASE) & !is.na(plot_df$AVAL), , drop = FALSE]
stopifnot(nrow(plot_df) > 0)

trt_levels <- sort(unique(plot_df$TRTA))
cat("TRTA levels:", paste(trt_levels, collapse=" | "), "\n")

# DataColors first 3: CXFFA53D CXF7DF54 CXEF6B48 ; DataSymbols PLUS circlefilled trianglefilled
sas_colors <- c("#FFA53D", "#F7DF54", "#EF6B48", "#61A6E7", "#CB79C8", "#85CE79",
                "#967CD0", "#DD6475", "#9DAA2D", "#6C69D9", "#2EA64A", "#C17D35")
sas_shapes <- c(3, 16, 17, 15, 18, 8, 0, 1, 2, 5, 6, 4)
col_map <- setNames(sas_colors[seq_along(trt_levels)], trt_levels)
shp_map <- setNames(sas_shapes[seq_along(trt_levels)], trt_levels)

x_min_data <- suppressWarnings(min(plot_df$BASE, na.rm = TRUE))
x_max_data <- suppressWarnings(max(plot_df$BASE, na.rm = TRUE))
y_min_data <- suppressWarnings(min(plot_df$AVAL, na.rm = TRUE))
y_max_data <- suppressWarnings(max(plot_df$AVAL, na.rm = TRUE))
# include reference lines and 0-100 diagonal anchor in range consideration
x_lo <- suppressWarnings(min(c(x_min_data, LLN, ULN, 0), na.rm = TRUE))
x_hi <- suppressWarnings(max(c(x_max_data, LLN, ULN, 100), na.rm = TRUE))
y_lo <- suppressWarnings(min(c(y_min_data, LLN, ULN, 0), na.rm = TRUE))
y_hi <- suppressWarnings(max(c(y_max_data, LLN, ULN, 100), na.rm = TRUE))

p <- ggplot(plot_df, aes(x = BASE, y = AVAL, color = TRTA, shape = TRTA)) +
  geom_point(size = 3, alpha = 0.9) +
  geom_abline(intercept = 0, slope = 1, color = "grey40", linewidth = 0.7) +
  geom_hline(yintercept = ULN, color = "#0000FF", linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = LLN, color = "#FF0000", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = LLN, color = "#FF0000", linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = ULN, color = "#0000FF", linetype = "dashed", linewidth = 0.7) +
  annotate("text", x = x_lo, y = ULN, label = "ULN", hjust = 0, vjust = -0.4, color = "#0000FF", size = 3.5) +
  annotate("text", x = x_lo, y = LLN, label = "LLN", hjust = 0, vjust = -0.4, color = "#FF0000", size = 3.5) +
  annotate("text", x = LLN, y = y_lo, label = "LLN", hjust = -0.2, vjust = 0, color = "#FF0000", size = 3.5) +
  annotate("text", x = ULN, y = y_lo, label = "ULN", hjust = -0.2, vjust = 0, color = "#0000FF", size = 3.5) +
  scale_color_manual(values = col_map, name = "Treatment") +
  scale_shape_manual(values = shp_map, name = "Treatment") +
  labs(title = "Figure 11.1 Scatterplot",
       x = "Min. Baseline",
       y = "Min. Post Baseline",
       caption = "LLN ULN ....") +
  theme_bw() +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(color = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    legend.position = "right",
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA)
  )

png_path <- file.path(out, "sgrender.png")
ggsave(png_path, plot = p, width = 9.51, height = 6.89, dpi = 100, units = "in")
cat("Wrote", png_path, "\n")

# CSV of the plotted data (START keep list)
write.csv(START, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
cat("Wrote", file.path(out, "sgrender.csv"), "\n")
