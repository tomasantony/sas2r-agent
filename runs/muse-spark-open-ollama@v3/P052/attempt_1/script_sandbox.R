# P052_outliers_Scatter_MeanTime.sas -> R translation
# Outputs: P052.lst (4 numbers) + figure SGRender.png

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helper: round half away from zero (SAS behavior)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file trying SAS absolute paths first, then fallbacks
find_file <- function(fname) {
  cands <- c(
    file.path("/bench/data/raw/cdisc-pilot/adam", fname),
    file.path("/bench/data/derived/cdisc-pilot/adam", fname),
    file.path(Sys.getenv("rawadam"), fname),
    file.path(Sys.getenv("RAWADAM"), fname),
    file.path(Sys.getenv("rawsplit"), fname),
    file.path(Sys.getenv("rawsdtm"), fname),
    file.path("data/raw/cdisc-pilot/adam", fname),
    file.path("data/derived/cdisc-pilot/adam", fname),
    file.path(getwd(), fname),
    file.path(getwd(), "data", "raw", "cdisc-pilot", "adam", fname)
  )
  cands <- cands[!is.na(cands) & cands != ""]
  for (p in cands) if (file.exists(p)) return(p)
  # recursive search from cwd
  found <- list.files(getwd(), pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
  if (length(found) > 0) return(found[1])
  if (dir.exists("/bench")) {
    found2 <- list.files("/bench", pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE)
    if (length(found2) > 0) return(found2[1])
  }
  return(cands[1])
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(readr)
})

cat("Reading ADSL and ADVSMAX\n")
adsl_path <- find_file("adsl.xpt")
advsmax_path <- find_file("advsmax.xpt")
cat("adsl_path:", adsl_path, "\n")
cat("advsmax_path:", advsmax_path, "\n")

# read with haven::read_xpt (XPORT engine)
adsl <- tryCatch(haven::read_xpt(adsl_path), error = function(e) {
  cat("Failed to read adsl.xpt:", conditionMessage(e), "\n")
  tibble::tibble()
})
advsmax <- tryCatch(haven::read_xpt(advsmax_path), error = function(e) {
  cat("Failed to read advsmax.xpt:", conditionMessage(e), "\n")
  tibble::tibble()
})

# Ensure data.frame
advsmax <- as.data.frame(advsmax, stringsAsFactors = FALSE)
adsl <- as.data.frame(adsl, stringsAsFactors = FALSE)

# SAS: proc sort data=advsmax; by studyid; run;
# In R arrange by STUDYID (SAS missing first, character uppercase before lowercase)
if ("STUDYID" %in% names(advsmax)) {
  advsmax <- advsmax[order(is.na(advsmax$STUDYID), advsmax$STUDYID, na.last = FALSE), , drop = FALSE]
}

# SAS: data advsmax_final; set advsmax; by studyid;
# WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y" and ADY>1 and SAFFL="Y";
# if first.studyid then do; LLN_label="LLN"; ULN_label="ULN"; end; run;
# WHERE is applied before FIRST. processing
required_cols <- c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL","STUDYID","BASE","AVAL","ANRLO","ANRHI","TRTA")
missing_cols <- setdiff(required_cols, names(advsmax))
if (length(missing_cols) > 0) cat("Warning missing cols in advsmax:", paste(missing_cols, collapse=","), "\n")

advsmax_final <- advsmax %>%
  filter(PARAMCD == "DIABP", ATPTN == 815, ANL01FL == "Y", ADY > 1, SAFFL == "Y") %>%
  arrange(STUDYID) %>%
  group_by(STUDYID) %>%
  mutate(
    LLN_label = ifelse(row_number() == 1, "LLN", NA_character_),
    ULN_label = ifelse(row_number() == 1, "ULN", NA_character_)
  ) %>%
  ungroup() %>%
  as.data.frame()

cat("advsmax_final nrow:", nrow(advsmax_final), "\n")
stopifnot(all(c("BASE","AVAL") %in% names(advsmax_final)))

# SAS: proc sql; create table range as select min(base) as min_base, max(base) as max_base,
#      min(aval) as min_aval, max(aval) as max_aval from advsmax_final; quit;
range_df <- advsmax_final %>%
  summarise(
    min_base = ifelse(all(is.na(BASE)), NA_real_, min(BASE, na.rm = TRUE)),
    max_base = ifelse(all(is.na(BASE)), NA_real_, max(BASE, na.rm = TRUE)),
    min_aval = ifelse(all(is.na(AVAL)), NA_real_, min(AVAL, na.rm = TRUE)),
    max_aval = ifelse(all(is.na(AVAL)), NA_real_, max(AVAL, na.rm = TRUE))
  ) %>%
  as.data.frame()

# SAS: data range2; set range; min=min(min_base,min_aval); max=max(max_base,max_aval); run;
range2 <- range_df %>%
  mutate(
    min = pmin(min_base, min_aval, na.rm = TRUE),
    max = pmax(max_base, max_aval, na.rm = TRUE)
  ) %>%
  as.data.frame()

# SAS: proc sql; select min(min), max(max) into :min, :max from range2; quit;
min_val <- suppressWarnings(min(range2$min, na.rm = TRUE))
max_val <- suppressWarnings(max(range2$max, na.rm = TRUE))
if (is.infinite(min_val)) min_val <- NA_real_
if (is.infinite(max_val)) max_val <- NA_real_
cat("Overall min:", min_val, "max:", max_val, "\n")

# SAS: proc sql; create table advsmax_final2 as select *, count(distinct studyid) as count_study from advsmax_final; quit;
count_study <- n_distinct(advsmax_final$STUDYID)
advsmax_final2 <- advsmax_final %>%
  mutate(count_study = count_study) %>%
  as.data.frame()

# SAS: data advsmax_final3; set advsmax_final2;
# if count_study <=2 then do; column_numbers=2; row_numbers=1; end;
# if count_study >2 then do; column_numbers=2; row_numbers=2; end; run;
advsmax_final3 <- advsmax_final2 %>%
  mutate(
    column_numbers = 2,
    row_numbers = ifelse(count_study <= 2, 1, 2)
  ) %>%
  as.data.frame()

# SAS: proc sql; select distinct column_numbers, row_numbers into :column_numbers, :row_numbers from advsmax_final3; run;
column_numbers <- unique(advsmax_final3$column_numbers)[1]
row_numbers <- unique(advsmax_final3$row_numbers)[1]
if (is.na(column_numbers)) column_numbers <- 2
if (is.na(row_numbers)) row_numbers <- 1
cat("column_numbers:", column_numbers, "row_numbers:", row_numbers, "\n")

# ---- Write P052 dataset outputs (range table = 4 numbers) ----
# Convert Date/Datetime columns to ISO if any (not expected here but for compliance)
iso_format_df <- function(df) {
  for (nm in names(df)) {
    if (inherits(df[[nm]], "Date")) df[[nm]] <- format(df[[nm]], "%Y-%m-%d")
    if (inherits(df[[nm]], "POSIXt")) df[[nm]] <- format(df[[nm]], "%Y-%m-%dT%H:%M:%S")
  }
  df
}
range_out <- iso_format_df(range_df)
# CSV for P052 (SAS dataset counterpart)
write.csv(range_out, file.path(out, "P052.csv"), row.names = FALSE, na = "")
write.csv(range_out, file.path(out, "P052.lst.csv"), row.names = FALSE, na = "")
# Also write range2 for completeness
write.csv(iso_format_df(range2), file.path(out, "range2.csv"), row.names = FALSE, na = "")
write.csv(iso_format_df(advsmax_final3), file.path(out, "advsmax_final3.csv"), row.names = FALSE, na = "")

# Text rendering for P052.lst (4 numbers)
txt_path <- file.path(out, "P052.txt")
txt_path2 <- file.path(out, "P052.lst.txt")
txt_con <- file(txt_path, open = "wt")
cat("P052 - Outliers Scatter Mean Time - Range of Baseline and Post-baseline Measurements\n", file = txt_con)
cat("--------------------------------------------------------------------------------\n", file = txt_con)
cat("Source: advsmax where PARAMCD=\"DIABP\" and ATPTN=815 and ANL01FL=\"Y\" and ADY>1 and SAFFL=\"Y\"\n", file = txt_con)
cat(sprintf("Overall axis range: min=%s max=%s  (min=min(min_base,min_aval) max=max(max_base,max_aval))\n",
            ifelse(is.na(min_val), "NA", format(min_val, nsmall = 1)),
            ifelse(is.na(max_val), "NA", format(max_val, nsmall = 1))), file = txt_con)
cat(sprintf("Panel layout: rows=%s columns=%s  (count_study=%s)\n", row_numbers, column_numbers, count_study), file = txt_con)
cat("\n", file = txt_con)
cat("Range table (4 numbers):\n", file = txt_con)
# header
hdr <- sprintf("%12s %12s %12s %12s", "min_base", "max_base", "min_aval", "max_aval")
cat(hdr, "\n", file = txt_con, sep = "")
cat(paste(rep("-", nchar(hdr)), collapse = ""), "\n", file = txt_con, sep = "")
# values - SAS default would show raw numeric; we show with 4 decimals if needed, but keep original
fmt_num <- function(x) ifelse(is.na(x), "NA", format(x, trim = TRUE, scientific = FALSE))
line <- sprintf("%12s %12s %12s %12s",
                fmt_num(range_df$min_base),
                fmt_num(range_df$max_base),
                fmt_num(range_df$min_aval),
                fmt_num(range_df$max_aval))
cat(line, "\n", file = txt_con, sep = "")
cat("\n", file = txt_con)
cat("Note: min and max are computed ignoring missing values (SAS PROC SQL MIN/MAX).\n", file = txt_con)
close(txt_con)
# duplicate for .lst.txt name
file.copy(txt_path, txt_path2, overwrite = TRUE)

# Also write a simple P052.lst file without extension for strict check
writeLines(readLines(txt_path), file.path(out, "P052.lst"))

# ---- Figure: SGRender (shiftplot) ----
# Prepare reference line data (per STUDYID if available)
# SAS: referenceline x=ANRLO / lineattrs=(pattern=2) xaxis=x curvelabel=LLN_label curvelabelposition=min
#      referenceline x=ANRHI similarly, and y=ANRLO, y=ANRHI
# We will draw dashed lines at each distinct ANRLO/ANRHI per study

# Ensure TRTA is factor for legend
if ("TRTA" %in% names(advsmax_final3)) {
  advsmax_final3$TRTA <- as.factor(advsmax_final3$TRTA)
}

# Build reference data frames
v_lln_df <- NULL; v_uln_df <- NULL; h_lln_df <- NULL; h_uln_df <- NULL
if (nrow(advsmax_final3) > 0 && all(c("ANRLO","ANRHI","STUDYID") %in% names(advsmax_final3))) {
  v_lln_df <- advsmax_final3 %>% distinct(STUDYID, ANRLO) %>% filter(!is.na(ANRLO))
  v_uln_df <- advsmax_final3 %>% distinct(STUDYID, ANRHI) %>% filter(!is.na(ANRHI))
  h_lln_df <- advsmax_final3 %>% distinct(STUDYID, ANRLO) %>% filter(!is.na(ANRLO))
  h_uln_df <- advsmax_final3 %>% distinct(STUDYID, ANRHI) %>% filter(!is.na(ANRHI))
}

# Determine axis breaks (integer=true)
if (!is.na(min_val) && !is.na(max_val) && is.finite(min_val) && is.finite(max_val)) {
  xlims <- c(min_val, max_val)
  ylims <- c(min_val, max_val)
  # integer breaks
  brks <- seq(floor(min_val), ceiling(max_val), by = 1)
  # if too many breaks (>20), thin to 5 or 10
  if (length(brks) > 20) {
    step <- ifelse(length(brks) > 50, 10, 5)
    brks <- seq(floor(min_val), ceiling(max_val), by = step)
  }
} else {
  xlims <- NULL; ylims <- NULL; brks <- waiver()
}

p <- ggplot(advsmax_final3, aes(x = BASE, y = AVAL, colour = TRTA)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, colour = "black", linewidth = 0.6) +
  facet_wrap(~STUDYID, nrow = row_numbers, ncol = column_numbers) +
  labs(
    x = "Maximum Baseline Measurment",
    y = "Maximum Post-baseline Measurment",
    colour = "Treatment",
    title = "Shift Plot: Maximum Baseline vs Maximum Post-baseline (DIABP, ATPTN=815)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90")
  )

# Add reference lines per facet if data exists
if (!is.null(v_lln_df) && nrow(v_lln_df) > 0) {
  p <- p + geom_vline(data = v_lln_df, aes(xintercept = ANRLO), linetype = 2, colour = "grey40", linewidth = 0.5, inherit.aes = FALSE)
}
if (!is.null(v_uln_df) && nrow(v_uln_df) > 0) {
  p <- p + geom_vline(data = v_uln_df, aes(xintercept = ANRHI), linetype = 2, colour = "grey40", linewidth = 0.5, inherit.aes = FALSE)
}
if (!is.null(h_lln_df) && nrow(h_lln_df) > 0) {
  p <- p + geom_hline(data = h_lln_df, aes(yintercept = ANRLO), linetype = 2, colour = "grey40", linewidth = 0.5, inherit.aes = FALSE)
}
if (!is.null(h_uln_df) && nrow(h_uln_df) > 0) {
  p <- p + geom_hline(data = h_uln_df, aes(yintercept = ANRHI), linetype = 2, colour = "grey40", linewidth = 0.5, inherit.aes = FALSE)
}

# Add curvelabels at min position (SAS curvelabelposition=min)
# For each vertical line, label at y=min_val, for horizontal at x=min_val
if (!is.na(min_val) && !is.null(v_lln_df) && nrow(v_lln_df) > 0) {
  v_lln_df$label <- "LLN"
  p <- p + geom_text(data = v_lln_df, aes(x = ANRLO, y = min_val, label = label),
                     inherit.aes = FALSE, vjust = -0.5, hjust = 0.5, size = 3, colour = "grey30")
}
if (!is.na(min_val) && !is.null(v_uln_df) && nrow(v_uln_df) > 0) {
  v_uln_df$label <- "ULN"
  p <- p + geom_text(data = v_uln_df, aes(x = ANRHI, y = min_val, label = label),
                     inherit.aes = FALSE, vjust = -0.5, hjust = 0.5, size = 3, colour = "grey30")
}
if (!is.na(min_val) && !is.null(h_lln_df) && nrow(h_lln_df) > 0) {
  h_lln_df$label <- "LLN"
  p <- p + geom_text(data = h_lln_df, aes(x = min_val, y = ANRLO, label = label),
                     inherit.aes = FALSE, vjust = -0.5, hjust = 0.5, size = 3, colour = "grey30", angle = 90)
}
if (!is.na(min_val) && !is.null(h_uln_df) && nrow(h_uln_df) > 0) {
  h_uln_df$label <- "ULN"
  p <- p + geom_text(data = h_uln_df, aes(x = min_val, y = ANRHI, label = label),
                     inherit.aes = FALSE, vjust = -0.5, hjust = 0.5, size = 3, colour = "grey30", angle = 90)
}

# Apply axis limits and integer breaks
if (!is.null(xlims) && !is.null(ylims)) {
  p <- p + coord_equal(xlim = xlims, ylim = ylims, expand = FALSE)
  p <- p + scale_x_continuous(breaks = brks, limits = xlims)
  p <- p + scale_y_continuous(breaks = brks, limits = ylims)
} else {
  p <- p + coord_equal()
}

# Handle empty data case
if (nrow(advsmax_final3) == 0) {
  p <- ggplot() + annotate("text", x = 0, y = 0, label = "No data for DIABP ATPTN=815 ANL01FL=Y ADY>1 SAFFL=Y") + theme_void()
}

# Save figure
png_path <- file.path(out, "sgrender.png")
png_path2 <- file.path(out, "SGRender.png")
ggsave(filename = png_path, plot = p, width = 10, height = 8, dpi = 300, units = "in")
# duplicate with original case
file.copy(png_path, png_path2, overwrite = TRUE)
cat("Figure saved to", png_path, "\n")

# CSV of plotted data (SAS sgrender data = advsmax_final3)
plot_data_out <- iso_format_df(advsmax_final3)
write.csv(plot_data_out, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
write.csv(plot_data_out, file.path(out, "SGRender.csv"), row.names = FALSE, na = "")
# also as per spec: figure data csv
write.csv(plot_data_out, file.path(out, "figure_SGRender.csv"), row.names = FALSE, na = "")

cat("Done. Outputs in", out, "\n")
cat("Files:", paste(list.files(out), collapse = ", "), "\n")
