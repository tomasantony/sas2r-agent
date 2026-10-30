# P043 Figure 11.1 - Scatterplot (SAS -> R)
# Reads ADLBC (SODIUM, AENTMTFL=Y, AVISITN NE 99) and reproduces SGRENDER scatterplot

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

# SAS round half away from zero helper (for completeness)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5
  z <- trunc(z)
  (z / 10^digits) * posneg
}

adam_split_dir <- "/bench/data/derived/cdisc-pilot/adam-split"
adam_dir       <- "/bench/data/derived/cdisc-pilot/adam"
rawadam_dir    <- "/bench/data/raw/cdisc-pilot/adam"

find_file <- function(dir, base) {
  if (!dir.exists(dir)) return(NULL)
  # case-insensitive search for base.xpt / base.sas7bdat
  files <- list.files(dir, full.names = TRUE, recursive = FALSE)
  if (length(files) == 0) return(NULL)
  low <- tolower(files)
  for (ext in c(".xpt", ".sas7bdat")) {
    idx <- which(low == tolower(file.path(dir, paste0(base, ext))))
    if (length(idx) > 0) return(files[idx[1]])
    # also try lowercase base
    idx2 <- which(basename(low) == tolower(paste0(base, ext)))
    if (length(idx2) > 0) return(files[idx2[1]])
  }
  return(NULL)
}

read_adam <- function(name) {
  f <- find_file(adam_split_dir, name)
  if (is.null(f)) f <- find_file(adam_dir, name)
  if (is.null(f)) f <- find_file(rawadam_dir, name)
  # also try recursive in split dir
  if (is.null(f) && dir.exists(adam_split_dir)) {
    allf <- list.files(adam_split_dir, full.names = TRUE, recursive = TRUE)
    low <- tolower(basename(allf))
    idx <- which(low %in% tolower(c(paste0(name, ".xpt"), paste0(name, ".sas7bdat"))))
    if (length(idx) > 0) f <- allf[idx[1]]
  }
  if (is.null(f)) stop(paste0("Dataset ", name, " not found in ", adam_split_dir, " / ", adam_dir))
  message("Reading ", f)
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) {
    haven::read_xpt(f)
  } else {
    haven::read_sas(f)
  }
}

ADLBC_raw <- read_adam("ADLBC")
message(paste0("ADLBC rows=", nrow(ADLBC_raw), " cols=", ncol(ADLBC_raw)))
# message(names)
message(paste(names(ADLBC_raw), collapse = ", "))

# Ensure expected columns exist (SAS WHERE needs them)
stopifnot("PARAMCD" %in% names(ADLBC_raw))
stopifnot("AVAL" %in% names(ADLBC_raw))
stopifnot("BASE" %in% names(ADLBC_raw))

# DATA ADLBC; SET ADAM.ADLBC; WHERE PARAMCD="SODIUM";
ADLBC <- ADLBC_raw %>% filter(PARAMCD == "SODIUM")
message(paste0("After PARAMCD=SODIUM rows=", nrow(ADLBC)))

# DATA START; SET ADLBC; WHERE AENTMTFL="Y" AND AVISITN NE 99;
# SAS: missing AVISITN satisfies NE 99 (missing sorts first / < number). R: keep NA as TRUE.
stopifnot("AENTMTFL" %in% names(ADLBC))
stopifnot("AVISITN" %in% names(ADLBC))
START <- ADLBC %>%
  filter(AENTMTFL == "Y", is.na(AVISITN) | AVISITN != 99)

message(paste0("START rows=", nrow(START)))

# KEEP USUBJID TRTAN TRTA PARAMCD PARAM AVAL BASE A1LO A1HI (keep if exists)
keep_vars <- c("USUBJID", "TRTAN", "TRTA", "PARAMCD", "PARAM", "AVAL", "BASE", "A1LO", "A1HI")
keep_exist <- keep_vars[keep_vars %in% names(START)]
# also keep AVISITN/AENTMTFL for traceability in CSV? SAS keeps only above; keep strictly those for CSV body
START_keep <- START %>% select(any_of(keep_vars))
# For plotting we need BASE, AVAL, TRTA
stopifnot("BASE" %in% names(START_keep))
stopifnot("AVAL" %in% names(START_keep))
stopifnot("TRTA" %in% names(START_keep))

# PROC FREQ DATA=START NOPRINT; TABLES PARAMCD*A1LO / OUT=LOW; TABLES PARAMCD*A1HI / OUT=HIGH;
# SAS PROC FREQ without MISSING excludes missing. Reproduce.
get_freq_out <- function(df, var) {
  df2 <- df %>% filter(!is.na(.data[[var]]), !is.na(PARAMCD))
  if (nrow(df2) == 0) return(data.frame(PARAMCD = character(0), stringsAsFactors = FALSE))
  f <- df2 %>% count(PARAMCD, .data[[var]], name = "COUNT") %>%
    rename(PERCENT = NULL) %>% as.data.frame()
  # compute percent of total per SAS? Not needed for mode, but compute for completeness
  tot <- sum(f$COUNT)
  f$PERCENT <- f$COUNT / tot * 100
  f
}

LOW <- get_freq_out(START_keep, "A1LO")
HIGH <- get_freq_out(START_keep, "A1HI")

# PROC SORT BY PARAMCD DESCENDING COUNT; DATA ... IF FIRST.PARAMCD; CALL SYMPUTX LLN/ULN
get_mode <- function(freq_df, valvar) {
  if (nrow(freq_df) == 0) return(NA_real_)
  # SAS sort: BY PARAMCD ASC, COUNT DESC. Stable; for ties keep smallest valvar to be deterministic
  # Order to mimic: PARAMCD asc (char, uppercase first - default R order differs but PARAMCD single value)
  # then COUNT desc
  ord <- order(freq_df$PARAMCD, -freq_df$COUNT, freq_df[[valvar]], na.last = TRUE)
  s <- freq_df[ord, , drop = FALSE]
  # FIRST.PARAMCD
  first <- s[!duplicated(s$PARAMCD), , drop = FALSE]
  # Single PARAMCD=SODIUM -> first row
  first[[valvar]][1]
}

LLN <- get_mode(LOW, "A1LO")
ULN <- get_mode(HIGH, "A1HI")

message(paste0("LLN=", LLN))
message(paste0("ULN=", ULN))

# Fallback if modes missing (should not happen): use median
if (is.na(LLN) && "A1LO" %in% names(START_keep)) LLN <- suppressWarnings(median(START_keep$A1LO, na.rm = TRUE))
if (is.na(ULN) && "A1HI" %in% names(START_keep)) ULN <- suppressWarnings(median(START_keep$A1HI, na.rm = TRUE))
message(paste0("Final LLN=", LLN, " ULN=", ULN))

# Prepare plot data: _BASE="BASE" _POST="AVAL" _TRT="TRTA"
plot_data <- START_keep %>%
  filter(!is.na(BASE), !is.na(AVAL)) %>%
  mutate(TRTA = as.character(TRTA))

# SAS character ordering uppercase before lowercase for legend order: sort unique TRTA with SAS order
# R default C locale differs; enforce byte-wise order via xtfrm in C locale? Approximate with order()
trt_levels <- sort(unique(plot_data$TRTA))
# SAS sorts uppercase before lowercase: use ASCII order which R order() already does in C locale;
# ensure by ordering on raw bytes
trt_levels <- unique(plot_data$TRTA)[order(unique(plot_data$TRTA))]
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_levels)

message(paste0("TRTA levels: ", paste(trt_levels, collapse = " | ")))

# Colors from template DataColors first 3: CXFFA53D CXF7DF54 CXEF6B48 ...
sas_colors <- c("#FFA53D", "#F7DF54", "#EF6B48", "#61A6E7", "#CB79C8", "#85CE79",
                "#967CD0", "#DD6475", "#9DAA2D", "#6C69D9", "#2EA64A", "#C17D35")
nlev <- length(trt_levels)
pal <- sas_colors[seq_len(min(nlev, length(sas_colors)))]
if (nlev > length(sas_colors)) pal <- rep(sas_colors, length.out = nlev)
names(pal) <- trt_levels

# Symbols: PLUS circlefilled trianglefilled -> map to ggplot shapes
# PLUS=3 (plus), circlefilled=16, trianglefilled=17
sym_map <- c(3, 16, 17)
shapes <- sym_map[seq_len(min(nlev, length(sym_map)))]
if (nlev > length(sym_map)) shapes <- rep(sym_map, length.out = nlev)
names(shapes) <- trt_levels

# Build graph matching SGDESIGN:
# title 'Figure 11.1 Scatterplot', footnote 'LLN ULN ....'
# x label 'Min. Baseline', y label 'Min. Post Baseline', grid on, wall white
# 45-degree line 0-100, h/v refs at ULN (blue CX0000FF SHORTDASH) and LLN (red CXFF0000 SHORTDASH)
p <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = TRTA, shape = TRTA)) +
  geom_abline(slope = 1, intercept = 0, colour = "grey55", linewidth = 0.7, linetype = "solid") +
  geom_point(size = 3, stroke = 1.1) +
  scale_colour_manual(values = pal, name = "Treatment") +
  scale_shape_manual(values = shapes, name = "Treatment") +
  labs(title = "Figure 11.1 Scatterplot",
       x = "Min. Baseline",
       y = "Min. Post Baseline",
       caption = "LLN ULN ....") +
  theme_bw(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "#FFFFFF", colour = "grey30"),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.grid.major = element_line(colour = "grey85", linewidth = 0.4),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "plain"),
    plot.caption = element_text(hjust = 0, size = 9),
    legend.position = "right",
    legend.background = element_rect(fill = "white", colour = NA),
    legend.key = element_rect(fill = "white", colour = NA)
  ) +
  guides(colour = guide_legend(title = "Treatment", order = 1),
         shape = guide_legend(title = "Treatment", order = 1))

# Add reference lines if non-missing
if (!is.na(ULN)) {
  p <- p + geom_hline(yintercept = ULN, colour = "#0000FF", linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = ULN, colour = "#0000FF", linetype = "dashed", linewidth = 0.7)
}
if (!is.na(LLN)) {
  p <- p + geom_hline(yintercept = LLN, colour = "#FF0000", linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = LLN, colour = "#FF0000", linetype = "dashed", linewidth = 0.7)
}

# Curvelabels at min position: annotate ULN/LLN near left/bottom edges
# Determine panel ranges from data (rowdatarange=data columndatarange=data)
xr <- range(plot_data$BASE, na.rm = TRUE)
yr <- range(plot_data$AVAL, na.rm = TRUE)
# Expand slightly like SAS default
x_min_ann <- xr[1]
y_min_ann <- yr[1]
if (!is.na(ULN)) {
  p <- p + annotate("text", x = x_min_ann, y = ULN, label = "ULN", hjust = 0, vjust = -0.6, size = 3.5, colour = "#0000FF")
  p <- p + annotate("text", x = ULN, y = y_min_ann, label = "ULN", hjust = -0.2, vjust = 0, size = 3.5, colour = "#0000FF", angle = 90)
}
if (!is.na(LLN)) {
  p <- p + annotate("text", x = x_min_ann, y = LLN, label = "LLN", hjust = 0, vjust = -0.6, size = 3.5, colour = "#FF0000")
  p <- p + annotate("text", x = LLN, y = y_min_ann, label = "LLN", hjust = -0.2, vjust = 0, size = 3.5, colour = "#FF0000", angle = 90)
}

# Save figure: designwidth=951 designheight=689 (pixels). Use ggsave with matching aspect.
# ggsave requires width/height in inches if dpi given; 951x689 at 100dpi
ggsave(file.path(out, "sgrender.png"), plot = p, width = 9.51, height = 6.89, dpi = 100, units = "in")
message("Wrote sgrender.png")

# CSV of plotted data (START keep vars) - SAS variable names, ISO dates not applicable (numeric)
# Ensure numeric unrounded
write.csv(START_keep, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
message("Wrote sgrender.csv")

# Also write a plain-text rendering of the figure metadata + summary (for table-type check)
# Contains same numbers SAS would show: N, LLN/ULN, treatment counts
sink(file.path(out, "sgrender.txt"))
cat("Figure 11.1 Scatterplot\n")
cat("LLN ULN ....\n\n")
cat(paste0("LLN = ", LLN, "\n"))
cat(paste0("ULN = ", ULN, "\n\n"))
cat(paste0("Analysis dataset: ADLBC where PARAMCD=SODIUM and AENTMTFL=Y and AVISITN NE 99; N=", nrow(START_keep), "\n"))
cat("X = BASE (Min. Baseline)  Y = AVAL (Min. Post Baseline)  Group = TRTA\n")
cat("45-degree reference line y=x (0 to 100 wall percent)\n")
cat("Horizontal/vertical reference lines at LLN (red shortdash) and ULN (blue shortdash)\n\n")
print(as.data.frame(table(START_keep$TRTA, useNA = "ifany")))
cat("\nSummary of plotted data:\n")
print(summary(as.data.frame(START_keep[, intersect(c("BASE","AVAL"), names(START_keep))])))
sink()
message("Wrote sgrender.txt")
