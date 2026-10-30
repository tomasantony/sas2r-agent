```r
# P060_pk_subj_conc.sas -> R translation
# Figure 16.2.5-x.x Individual concentration-time profiles (GPLOT AVAL*eactm BY USUBJID parcat1)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# SAS round half away from zero helper (for compliance; not needed for plot)
round_half_up <- function(x, digits = 0) {
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
# In sandbox the bench is mounted at /bench
if (!file.exists(file.path(rawadam, "adpc.xpt"))) {
  alt <- "/bench/data/raw/cdisc-pilot/adam"
  if (file.exists(file.path(alt, "adpc.xpt"))) rawadam <- alt
}
adsl_path <- file.path(rawadam, "adsl.xpt")
adpc_path <- file.path(rawadam, "adpc.xpt")

message("Reading ADSL: ", adsl_path)
adsl <- haven::read_xpt(adsl_path)

message("Reading ADPC: ", adpc_path)
adpc <- haven::read_xpt(adpc_path)

# Resolve column names case-insensitively (SAS is case-insensitive)
nm <- names(adpc)
find_col <- function(want) {
  idx <- which(toupper(nm) == toupper(want))
  if (length(idx) == 0) return(NA_character_)
  nm[idx[1]]
}
c_USUBJID <- find_col("USUBJID")
c_PARCAT1 <- find_col("PARCAT1")
c_EACTM   <- find_col("EACTM")
c_AVAL    <- find_col("AVAL")

message("ADPC columns: ", paste(nm, collapse = ", "))
stopifnot(!is.na(c_USUBJID), !is.na(c_PARCAT1), !is.na(c_EACTM), !is.na(c_AVAL))
stopifnot(all(c(c_USUBJID, c_PARCAT1, c_EACTM, c_AVAL) %in% names(adpc)))

# PROC SORT DATA=WORK.ADPC OUT=WORK.SORTadpc; BY USUBJID parcat1 eactm; RUN;
# SAS: missing sorts first, character ordering uppercase before lowercase.
SORTadpc <- adpc %>%
  arrange(
    is.na(.data[[c_USUBJID]]), .data[[c_USUBJID]],
    is.na(.data[[c_PARCAT1]]), .data[[c_PARCAT1]],
    is.na(.data[[c_EACTM]]), .data[[c_EACTM]]
  )

message("SORTadpc rows: ", nrow(SORTadpc))

# Plotted data: BY vars + axes (keep sort order)
plot_data <- SORTadpc %>%
  select(all_of(c(c_USUBJID, c_PARCAT1, c_EACTM, c_AVAL))) %>%
  arrange(
    is.na(.data[[c_USUBJID]]), .data[[c_USUBJID]],
    is.na(.data[[c_PARCAT1]]), .data[[c_PARCAT1]],
    is.na(.data[[c_EACTM]]), .data[[c_EACTM]]
  )

# Standardize types for CSV/plot
plot_data[[c_EACTM]] <- suppressWarnings(as.numeric(plot_data[[c_EACTM]]))
plot_data[[c_AVAL]]  <- suppressWarnings(as.numeric(plot_data[[c_AVAL]]))

# ---- Write datasets as CSV ----
fmt_for_csv <- function(df) {
  df2 <- df
  for (cl in names(df2)) {
    v <- df2[[cl]]
    if (inherits(v, "POSIXt")) {
      df2[[cl]] <- format(v, "%Y-%m-%dT%H:%M:%S")
    } else if (inherits(v, "Date")) {
      df2[[cl]] <- format(v, "%Y-%m-%d")
    } else if (inherits(v, "labelled")) {
      df2[[cl]] <- haven::zap_labels(v)
      if (inherits(df2[[cl]], "POSIXt")) df2[[cl]] <- format(df2[[cl]], "%Y-%m-%dT%H:%M:%S")
      if (inherits(df2[[cl]], "Date")) df2[[cl]] <- format(df2[[cl]], "%Y-%m-%d")
    }
  }
  df2
}
SORTadpc_csv <- fmt_for_csv(SORTadpc)
write.csv(SORTadpc_csv, file.path(out, "SORTadpc.csv"), row.names = FALSE, na = "")

plot_csv <- fmt_for_csv(plot_data)
write.csv(plot_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
message("Wrote SORTadpc.csv and sasgraph.csv")

# ---- PROC GPLOT: PLOT AVAL * eactm / VAXIS Conc HAXIS Time FRAME; BY USUBJID parcat1 ----
# SYMBOL1 INTERPOL=JOIN VALUE=NONE LINE=1 WIDTH=2  => line only, no markers
# Titles:
#  "(page x of x)"
#  "Figure 16.2.5-x.x Individual concentration-time profiles"
#  "by compound, matrix, analyte and [actual/randomised][treatments/group]"
#  "Analysis Set : PK analysis set"

# Prepare facet label: USUBJID + PARCAT1 (BY variables)
draw_data <- plot_data %>%
  filter(!is.na(.data[[c_EACTM]]), !is.na(.data[[c_AVAL]]))

n_groups <- draw_data %>%
  distinct(.data[[c_USUBJID]], .data[[c_PARCAT1]]) %>%
  nrow()
if (is.na(n_groups) || n_groups == 0) n_groups <- 1
message("BY groups (USUBJID x PARCAT1): ", n_groups)

ncol_f <- if (n_groups <= 2) n_groups else if (n_groups <= 4) 2 else if (n_groups <= 9) 3 else 4
ncol_f <- max(1, min(4, n_groups))
nrow_f <- ceiling(n_groups / ncol_f)
# Keep figure within ggsave default 50-inch limit
w_in <- 12
h_in <- nrow_f * 2 + 2
h_in <- max(8, min(49, h_in))
message("Facet layout: ", ncol_f, " cols x ", nrow_f, " rows; size ", w_in, "x", h_in)

by_lev <- plot_data %>%
  distinct(.data[[c_USUBJID]], .data[[c_PARCAT1]]) %>%
  arrange(
    is.na(.data[[c_USUBJID]]), .data[[c_USUBJID]],
    is.na(.data[[c_PARCAT1]]), .data[[c_PARCAT1]]
  )
facet_lab <- paste0(by_lev[[c_USUBJID]], " / ", by_lev[[c_PARCAT1]])
draw_data$..FACET.. <- factor(
  paste0(draw_data[[c_USUBJID]], " / ", draw_data[[c_PARCAT1]]),
  levels = facet_lab
)

title_block <- paste(
  "(page x of x)",
  "Figure 16.2.5-x.x Individual concentration-time profiles",
  "by compound, matrix, analyte and [actual/randomised][treatments/group]",
  "Analysis Set : PK analysis set",
  sep = "\n"
)
foot_txt <- paste0("Generated by R on ", format(Sys.Date(), "%Y-%m-%d"))

p <- ggplot(draw_data, aes(x = .data[[c_EACTM]], y = .data[[c_AVAL]], group = ..FACET..)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ ..FACET.., ncol = ncol_f, scales = "free") +
  labs(
    title = title_block,
    x = "Time (hour)",
    y = "Conc (ug/mL)",
    caption = foot_txt
  ) +
  theme_bw(base_size = 10) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "plain"),
    axis.title = element_text(size = 10),
    strip.text = element_text(size = 8),
    strip.background = element_rect(fill = "grey95"),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", linewidth = 0.8)
  )

png_file <- file.path(out, "sasgraph.png")
ggsave(png_file, plot = p, width = w_in, height = h_in, dpi = 150, units = "in", limitsize = FALSE)
message("Wrote ", png_file)

# ---- Plain-text rendering of the figure data (table body counterpart) ----
txt_file <- file.path(out, "sasgraph.txt")
con <- file(txt_file, open = "wt")
writeLines("(page x of x)", con)
writeLines("Figure 16.2.5-x.x Individual concentration-time profiles", con)
writeLines("by compound, matrix, analyte and [actual/randomised][treatments/group]", con)
writeLines("Analysis Set : PK analysis set", con)
writeLines("", con)
writeLines(paste0("Plot: AVAL (Conc (ug/mL)) * EACTM (Time (hour))  BY USUBJID PARCAT1"), con)
writeLines(paste0("Number of BY groups: ", n_groups), con)
writeLines(paste0("Number of plotted observations (non-missing AVAL and EACTM): ", nrow(draw_data)), con)
writeLines("", con)
summ <- draw_data %>%
  group_by(..FACET..) %>%
  summarise(
    N = dplyr::n(),
    MIN_TIME = suppressWarnings(min(.data[[c_EACTM]], na.rm = TRUE)),
    MAX_TIME = suppressWarnings(max(.data[[c_EACTM]], na.rm = TRUE)),
    MIN_CONC = suppressWarnings(min(.data[[c_AVAL]], na.rm = TRUE)),
    MAX_CONC = suppressWarnings(max(.data[[c_AVAL]], na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  arrange(..FACET..)
writeLines("Summary by USUBJID / PARCAT1:", con)
writeLines(sprintf("%-40s %6s %12s %12s %12s %12s", "USUBJID / PARCAT1", "N", "MIN_TIME", "MAX_TIME", "MIN_CONC", "MAX_CONC"), con)
for (i in seq_len(nrow(summ))) {
  writeLines(sprintf("%-40s %6d %12.4f %12.4f %12.4f %12.4f",
                     as.character(summ$..FACET..[i]),
                     summ$N[i], summ$MIN_TIME[i], summ$MAX_TIME[i],
                     summ$MIN_CONC[i], summ$MAX_CONC[i]), con)
}
writeLines("", con)
writeLines(foot_txt, con)
close(con)
message("Wrote sasgraph.txt")
```