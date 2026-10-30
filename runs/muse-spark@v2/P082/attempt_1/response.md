```r
# P082 f_waterfall - Waterfall Plot of Best Percentage Change from Baseline
# SAS source: P082_f_waterfall.sas (own-tlf)
# Inputs: adam.adtr (or adam.adrs), adam.adsl  | Output: f_waterfall.png + csv

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
})

# SAS round half away from zero helper (for completeness)
round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}

find_file <- function(dir, name) {
  if (!dir.exists(dir)) return(character(0))
  pat <- paste0("(?i)^", name, "\\.(sas7bdat|xpt)$")
  list.files(dir, pattern = pat, full.names = TRUE)
}

read_sas_any <- function(f) {
  if (length(f) == 0) return(NULL)
  f <- f[1]
  if (grepl("\\.xpt$", f, ignore.case = TRUE)) haven::read_xpt(f)
  else haven::read_sas(f)
}

# ---- Read ADSL ----
adsl_file <- find_file(adam_dir, "adsl")
cat("ADSL file:", paste(adsl_file, collapse=";"), "\n")
adsl <- read_sas_any(adsl_file)
if (is.null(adsl)) stop("ADSL not found")
names(adsl) <- toupper(names(adsl))
stopifnot(all(c("USUBJID") %in% names(adsl)))
# EFFFL column check (case-sensitive exact name after upper)
if (!("EFFFL" %in% names(adsl))) stop("EFFFL not in ADSL")

# ---- Read ADTR ----
adtr_file <- find_file(adam_dir, "adtr")
cat("ADTR file:", paste(adtr_file, collapse=";"), "\n")
adtr <- read_sas_any(adtr_file)
if (!is.null(adtr)) names(adtr) <- toupper(names(adtr))

get_col <- function(df, nm) {
  if (nm %in% names(df)) df[[nm]] else rep(NA, nrow(df))
}
get_col_char <- function(df, nm) {
  v <- get_col(df, nm)
  as.character(v)
}
get_col_num <- function(df, nm) {
  v <- get_col(df, nm)
  suppressWarnings(as.numeric(v))
}

tumor <- NULL

if (!is.null(adtr) && all(c("USUBJID","PARAMCD") %in% names(adtr))) {
  # Build tumor as in PROC SQL: inner join adtr*adsl where EFFFL='Y' and PARAMCD='BPCHG'
  # Drop/rename overlapping non-key cols before join to avoid .x/.y
  # Keys: USUBJID
  adtr_keep <- tibble(
    USUBJID = get_col_char(adtr, "USUBJID"),
    TRTA    = get_col_char(adtr, "TRTA"),
    TRTAN   = get_col_num(adtr, "TRTAN"),
    PARAMCD = get_col_char(adtr, "PARAMCD"),
    PARAM   = get_col_char(adtr, "PARAM"),
    PCHG    = get_col_num(adtr, "PCHG"),
    AVALC   = get_col_char(adtr, "AVALC")
  )
  adsl_keep <- tibble(
    USUBJID = get_col_char(adsl, "USUBJID"),
    EFFFL   = get_col_char(adsl, "EFFFL")
  )
  # SAS inner join + where (case-sensitive, exact match, missing never matches)
  tumor_try <- adtr_keep %>%
    inner_join(adsl_keep, by = "USUBJID") %>%
    filter(!is.na(EFFFL) & EFFFL == "Y",
           !is.na(PARAMCD) & PARAMCD == "BPCHG")
  cat("ADTR BPCHG EFF rows:", nrow(tumor_try), "\n")
  if (nrow(tumor_try) > 0) tumor <- tumor_try
}

# ---- Fallback: derive from ADRS if ADTR not available / empty ----
if (is.null(tumor) || nrow(tumor) == 0) {
  adrs_file <- find_file(adam_dir, "adrs")
  cat("ADRS file:", paste(adrs_file, collapse=";"), "\n")
  adrs <- read_sas_any(adrs_file)
  if (!is.null(adrs)) {
    names(adrs) <- toupper(names(adrs))
    stopifnot("USUBJID" %in% names(adrs))
    adrs_keep <- tibble(
      USUBJID = get_col_char(adrs, "USUBJID"),
      TRTA    = get_col_char(adrs, "TRTA"),
      TRTAN   = get_col_num(adrs, "TRTAN"),
      PARAMCD = get_col_char(adrs, "PARAMCD"),
      PARAM   = get_col_char(adrs, "PARAM"),
      PCHG    = get_col_num(adrs, "PCHG"),
      AVALC   = get_col_char(adrs, "AVALC")
    )
    adsl_keep2 <- tibble(
      USUBJID = get_col_char(adsl, "USUBJID"),
      EFFFL   = get_col_char(adsl, "EFFFL")
    )
    # SAS alternative (commented): min(PCHG) per subject where PARAMCD='SUMLD'
    # If SUMLD not present, use any PCHG min per subject to avoid zero-row result
    has_sumld <- any(!is.na(adrs_keep$PARAMCD) & adrs_keep$PARAMCD == "SUMLD")
    if (has_sumld) {
      filt <- adrs_keep %>% filter(!is.na(PARAMCD) & PARAMCD == "SUMLD")
    } else {
      filt <- adrs_keep
    }
    tumor_try2 <- filt %>%
      inner_join(adsl_keep2, by = "USUBJID") %>%
      filter(!is.na(EFFFL) & EFFFL == "Y") %>%
      group_by(USUBJID, TRTA, TRTAN) %>%
      summarise(
        PCHG = if (all(is.na(PCHG))) NA_real_ else min(PCHG, na.rm = TRUE),
        AVALC = dplyr::first(AVALC),
        PARAMCD = dplyr::first(PARAMCD),
        PARAM = dplyr::first(PARAM),
        EFFFL = dplyr::first(EFFFL),
        .groups = "drop"
      ) %>%
      select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC, EFFFL)
    cat("ADRS fallback rows:", nrow(tumor_try2), "\n")
    tumor <- tumor_try2
  }
}

if (is.null(tumor)) tumor <- tibble(USUBJID=character(0), TRTA=character(0),
  TRTAN=numeric(0), PARAMCD=character(0), PARAM=character(0),
  PCHG=numeric(0), AVALC=character(0), EFFFL=character(0))

stopifnot(all(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL") %in% names(tumor)))

# ---- Step 1b: ORDER BY PCHG (SAS: missing first, ascending) ----
# SAS sorts uppercase before lowercase for char, but ordering here is numeric PCHG
ord <- order(tumor$PCHG, na.last = FALSE)
tumor <- tumor[ord, , drop = FALSE]

# ---- Step 2: waterfall with bar_order, bar_color, PCHG_display ----
waterfall <- tumor
waterfall$bar_order <- seq_len(nrow(waterfall))
# SAS: select(upcase(AVALC)); strip? SAS UPCASE keeps blanks; compare exact 'CR' etc.
# Trim to be safe but case-sensitive after upper.
up <- toupper(trimws(ifelse(is.na(waterfall$AVALC), "", waterfall$AVALC)))
waterfall$bar_color <- ifelse(up == "CR", "darkgreen",
  ifelse(up == "PR", "green",
  ifelse(up == "SD", "gold",
  ifelse(up == "PD", "red", "gray"))))
waterfall$PCHG_display <- ifelse(is.na(waterfall$PCHG), NA_real_,
  ifelse(waterfall$PCHG > 100, 100,
  ifelse(waterfall$PCHG < -100, -100, waterfall$PCHG)))

cat("Waterfall N:", nrow(waterfall), "\n")

# ---- Write plotted-data CSV ----
# Keep SAS variable names; add derived display vars
out_csv <- file.path(out, "f_waterfall.csv")
write.csv(waterfall, out_csv, row.names = FALSE, na = "")
cat("Wrote", out_csv, "\n")

# ---- Step 3: Waterfall plot (PROC SGPLOT replica) ----
# Titles / footnotes verbatim (macros resolved literally)
t1 <- "[Sponsor Name]"
t2 <- "Protocol: [Protocol Number]"
t4 <- "Figure 14.2.3"
t5 <- "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions"
t6 <- "Efficacy Analysis Set"
cutdate_txt <- "[DDMONYYYY]"
run_date9 <- format(Sys.Date(), "%d%b%Y")
run_date9 <- toupper(run_date9)
fn1 <- paste0("Data cutoff: ", cutdate_txt, ".")
fn2 <- "Each bar represents one subject."
fn3 <- "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease"
fn4 <- "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)"
fn5 <- paste0("Program: P082_f_waterfall.sas | Run: ", run_date9)

# Prepare fill factor: SAS GROUP=AVALC (as-is, case-sensitive, missing as own group)
waterfall$AVALC_disp <- ifelse(is.na(waterfall$AVALC) | trimws(waterfall$AVALC) == "",
  "(Missing)", waterfall$AVALC)
waterfall$AVALC_disp <- factor(waterfall$AVALC_disp,
  levels = sort(unique(waterfall$AVALC_disp)))

# Color map per displayed level using SAS bar_color logic
lvl_color <- sapply(levels(waterfall$AVALC_disp), function(lv) {
  if (lv == "(Missing)") return("gray")
  u <- toupper(trimws(lv))
  if (u == "CR") "darkgreen"
  else if (u == "PR") "green"
  else if (u == "SD") "gold"
  else if (u == "PD") "red"
  else "gray"
})
# Map R color names to hex approximating SAS defaults
col_hex <- c(darkgreen="darkgreen", green="green", gold="gold", red="red", gray="gray")
fill_vals <- unname(sapply(lvl_color, function(cn) col_hex[[cn]]))
names(fill_vals) <- levels(waterfall$AVALC_disp)

# y range: -100 to 100 by 20
plot_df <- waterfall

p <- ggplot(plot_df, aes(x = bar_order, y = PCHG_display, fill = AVALC_disp)) +
  geom_col(width = 1, colour = "black", linewidth = 0.25, na.rm = TRUE) +
  geom_hline(yintercept = -30, linetype = "dashed", colour = "blue", linewidth = 0.6) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "red", linewidth = 0.6) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.6) +
  scale_y_continuous(
    name = "Best Percentage Change from Baseline (%)",
    limits = c(-100, 100),
    breaks = seq(-100, 100, by = 20),
    expand = c(0, 0)
  ) +
  scale_x_continuous(name = " ", expand = c(0.01, 0.01)) +
  scale_fill_manual(name = "Best Overall Response", values = fill_vals, drop = FALSE) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.major.y = element_line(linetype = "dotted", colour = "lightgray"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 7)
  ) +
  labs(
    title = paste0(t4, "\n", t5, "\n", t6),
    subtitle = paste0(t1, "    ", t2),
    caption = paste(fn1, fn2, fn3, fn4, fn5, sep = "\n")
  )

# Annotate threshold labels near right edge (mimic REFLINE LABEL)
xmax <- if (nrow(plot_df) > 0) max(plot_df$bar_order, na.rm = TRUE) else 1
if (is.finite(xmax) && nrow(plot_df) > 0) {
  p <- p +
    annotate("text", x = xmax, y = -30 + 4, label = "-30% (PR threshold)",
             hjust = 1, vjust = 0, size = 2.5, colour = "blue") +
    annotate("text", x = xmax, y = 20 + 4, label = "+20% (PD threshold)",
             hjust = 1, vjust = 0, size = 2.5, colour = "red")
}

# N label
n_txt <- paste0("Efficacy Analysis Set (N=", nrow(plot_df), ")")
cat(n_txt, "\n")

out_png <- file.path(out, "f_waterfall.png")
ggsave(out_png, plot = p, width = 10, height = 6.5, units = "in", dpi = 300)
cat("Wrote", out_png, "\n")

# ---- Plain-text rendering of figure metadata (for traceability) ----
txt_path <- file.path(out, "f_waterfall.txt")
sink(txt_path)
cat(t1, "\n")
cat(t2, "\n")
cat("\n")
cat(t4, "\n")
cat(t5, "\n")
cat(t6, "\n")
cat("\n")
cat(n_txt, "\n")
cat("Y-axis: Best Percentage Change from Baseline (%)  (-100 to 100 by 20)\n")
cat("X-axis: subjects sorted by PCHG (display suppressed)\n")
cat("Reference lines: -30% (PR threshold, blue dash), +20% (PD threshold, red dash), 0 (black solid)\n")
cat("\nCounts by Best Overall Response (AVALC):\n")
if (nrow(plot_df) > 0) {
  tb <- as.data.frame(table(plot_df$AVALC_disp, useNA = "no"), stringsAsFactors = FALSE)
  names(tb) <- c("AVALC", "N")
  tb$PCT <- round_half_up(tb$N / sum(tb$N) * 100, 1)
  for (i in seq_len(nrow(tb))) cat(tb$AVALC[i], ": ", tb$N[i], " (", sprintf("%.1f%%", tb$PCT[i]), ")\n", sep = "")
} else {
  cat("(no subjects)\n")
}
cat("\n")
cat(fn1, "\n")
cat(fn2, "\n")
cat(fn3, "\n")
cat(fn4, "\n")
cat(fn5, "\n")
sink()
cat("Wrote", txt_path, "\n")

cat("NOTE: f_waterfall completed successfully.\n")
```