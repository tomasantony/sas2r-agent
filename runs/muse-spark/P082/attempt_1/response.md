Building your waterfall plot — translating the oncology response figure to R.```r
# P082 f_waterfall.sas -> R translation
# Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
tlfpath  <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# ---- Read inputs ----
# List adam files to find correct names (case-insensitive)
adam_files <- list.files(adampath, full.names = TRUE)
# print to log only
cat("ADAM files:\n"); cat(paste(basename(adam_files), collapse="\n"), "\n")

find_adam <- function(name) {
  idx <- which(tolower(basename(adam_files)) %in% paste0(tolower(name), c(".sas7bdat",".xpt","")))
  if (length(idx)==0) {
    # try contains
    idx <- grep(paste0("(?i)^", name), basename(adam_files))
  }
  if (length(idx)==0) return(NULL)
  adam_files[idx[1]]
}

read_adam <- function(name) {
  f <- find_adam(name)
  if (is.null(f)) { cat(paste0("NOTE: ", name, " not found\n")); return(NULL) }
  cat(paste0("Reading ", f, "\n"))
  if (grepl("\\.xpt$", f, ignore.case=TRUE)) haven::read_xpt(f)
  else haven::read_sas(f)
}

adtr <- read_adam("adtr")
adsl <- read_adam("adsl")

# Fallback: adrs if adtr missing
adrs <- NULL
if (is.null(adtr)) {
  adrs <- read_adam("adrs")
}

# Helper to get column case-insensitively
getcol <- function(df, nm) {
  if (is.null(df)) return(NULL)
  cn <- names(df)
  idx <- which(toupper(cn)==toupper(nm))
  if (length(idx)==0) return(NULL)
  df[[cn[idx[1]]]]
}

# ---- Step 1: Get best percentage change per subject ----
# SAS: select a.USUBJID, a.TRTA, a.TRTAN, a.PARAMCD, a.PARAM, a.PCHG, a.AVALC, b.EFFFL
#      from adam.adtr a inner join adam.adsl b on USUBJID where EFFFL='Y' and PARAMCD='BPCHG' order by PCHG

tumor <- NULL
if (!is.null(adtr) && !is.null(adsl)) {
  # normalize col names lookup
  # Ensure required cols exist
  # Join
  # Use uppercase-insensitive merge
  # Standardize names to uppercase for processing
  names(adtr) <- toupper(names(adtr))
  names(adsl) <- toupper(names(adsl))
  # Check columns
  cat("ADTR cols:", paste(names(adtr), collapse=","), "\n")
  cat("ADSL cols:", paste(names(adsl), collapse=","), "\n")
  if (all(c("USUBJID","PARAMCD","PCHG") %in% names(adtr)) && "USUBJID" %in% names(adsl)) {
    # EFFFL in adsl
    efffl_col <- if ("EFFFL" %in% names(adsl)) "EFFFL" else NULL
    sl_sub <- adsl %>% select(USUBJID, any_of(c("EFFFL")))
    # filter EFFFL='Y' (case-sensitive, SAS char comparison; strip?)
    # SAS where b.EFFFL='Y' is case-sensitive exact; trailing blanks ignored in SAS but haven trims? Use trimws + exact
    if (!is.null(efffl_col)) {
      sl_sub <- sl_sub %>% filter(EFFFL == "Y")
    }
    # filter adtr PARAMCD='BPCHG' case-sensitive
    tr_sub <- adtr %>% filter(PARAMCD == "BPCHG")
    # select cols if exist
    keep_a <- intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC"), names(tr_sub))
    tr_sub <- tr_sub %>% select(any_of(keep_a))
    tumor <- inner_join(tr_sub, sl_sub, by="USUBJID")
    # order by PCHG (SAS missing first, ascending)
    tumor <- tumor %>% arrange(PCHG)
  } else {
    cat("WARNING: ADTR missing expected columns\n")
  }
}

# Alternative derive from ADRS if ADTR not available or tumor empty
if ((is.null(tumor) || nrow(tumor)==0)) {
  if (is.null(adrs)) adrs <- read_adam("adrs")
  if (!is.null(adrs) && !is.null(adsl)) {
    names(adrs) <- toupper(names(adrs))
    # adsl already upper
    sl2 <- adsl %>% select(USUBJID, any_of(c("EFFFL","AVALC")))
    # SAS alt: select min(PCHG) grouped; but keep simple: min PCHG per subject
    # Need PARAMCD='SUMLD' filter? SAS alt uses PARAMCD='SUMLD'
    if ("PARAMCD" %in% names(adrs)) {
      adrs_f <- adrs %>% filter(PARAMCD == "SUMLD")
    } else adrs_f <- adrs
    if ("EFFFL" %in% names(adsl)) {
      sl2 <- sl2 %>% filter(EFFFL == "Y")
    }
    # join
    j <- inner_join(adrs_f, sl2 %>% select(USUBJID), by="USUBJID")
    # compute min PCHG per subject; need TRTA/TRTAN/PCHG/AVALC
    # BOR from adsl AVALC? SAS alt: b.AVALC as BOR - ambiguous; use adsl AVALC if present
    # For now group
    if ("PCHG" %in% names(j)) {
      tumor <- j %>% group_by(USUBJID) %>%
        summarise(
          TRTA = dplyr::first(TRTA),
          TRTAN = dplyr::first(TRTAN),
          PARAMCD = "BPCHG",
          PARAM = dplyr::first(PARAM) ,
          PCHG = suppressWarnings(min(PCHG, na.rm=TRUE)),
          AVALC = dplyr::first(AVALC),
          .groups="drop"
        )
      # fix Inf from all-missing
      tumor$PCHG[is.infinite(tumor$PCHG)] <- NA_real_
      # add EFFFL
      tumor$EFFFL <- "Y"
      tumor <- tumor %>% arrange(PCHG)
    }
  }
}

if (is.null(tumor)) {
  # create empty to avoid crash
  tumor <- data.frame(USUBJID=character(0), TRTA=character(0), TRTAN=numeric(0),
                      PARAMCD=character(0), PARAM=character(0), PCHG=numeric(0),
                      AVALC=character(0), EFFFL=character(0), stringsAsFactors=FALSE)
}

cat(paste0("tumor N=", nrow(tumor), "\n"))

# ---- Step 2: Assign display order (sorted by percent change) ----
# SAS: data waterfall; set tumor; by PCHG; bar_order=_n_;
#  select(upcase(AVALC)) when CR darkgreen etc; cap PCHG_display at +-100
waterfall <- tumor
# Ensure sorted by PCHG ascending, missing first (dplyr arrange puts NA first? default NA last? Actually arrange puts NA last? In dplyr, NA are sorted last? No, need check.
# SAS missing sorts first (lowest). Force NA first:
if (nrow(waterfall)>0) {
  waterfall <- waterfall %>% arrange(is.na(PCHG), PCHG)
  waterfall$bar_order <- seq_len(nrow(waterfall))
  # bar_color
  up <- toupper(trimws(ifelse(is.na(waterfall$AVALC), "", waterfall$AVALC)))
  waterfall$bar_color <- dplyr::case_when(
    up == "CR" ~ "darkgreen",
    up == "PR" ~ "green",
    up == "SD" ~ "gold",
    up == "PD" ~ "red",
    TRUE ~ "gray"
  )
  # cap
  waterfall$PCHG_display <- ifelse(is.na(waterfall$PCHG), NA_real_,
    ifelse(waterfall$PCHG > 100, 100,
      ifelse(waterfall$PCHG < -100, -100, waterfall$PCHG)))
} else {
  waterfall$bar_order <- numeric(0)
  waterfall$bar_color <- character(0)
  waterfall$PCHG_display <- numeric(0)
}

# Ensure AVALC present; if missing, fill "Missing"/gray? Keep as NA -> group NA
# For plotting, create display group: if NA or blank -> "Missing"/"Unknown"? SAS group=AVALC would show missing as missing.
# Keep original AVALC; create AVALC_GRP for legend: coalesce to "Missing"
waterfall$AVALC_GRP <- ifelse(is.na(waterfall$AVALC) | trimws(waterfall$AVALC)=="", "Missing", trimws(waterfall$AVALC))

# Order groups for legend: CR, PR, SD, PD, others, Missing (SAS order would be alphabetical? SGPLOT group order = data order? Use sorted unique with desired order)
desired <- c("CR","PR","SD","PD")
other_lvls <- sort(unique(setdiff(waterfall$AVALC_GRP, c(desired, "Missing"))))
lvl <- c(intersect(desired, unique(waterfall$AVALC_GRP)), other_lvls,
         if ("Missing" %in% waterfall$AVALC_GRP) "Missing" else NULL)
if (length(lvl)==0) lvl <- "Missing"
waterfall$AVALC_GRP <- factor(waterfall$AVALC_GRP, levels=lvl)

# Color mapping matching SAS bar_color logic
col_map <- c(CR="darkgreen", PR="green", SD="gold", PD="red", Missing="gray")
# add any others as gray
for (l in lvl) if (!(l %in% names(col_map))) col_map[l] <- "gray"
col_map <- col_map[lvl]

# ---- Step 3: Generate waterfall plot ----
# Use ggplot2 vbar equivalent: geom_col with bar_order x
# SAS: vbar bar_order / response=PCHG_display group=AVALC groupdisplay=cluster barwidth=1 outline black
# For waterfall, each bar_order unique so cluster == stack single bar. Use geom_col fill by group.
# xaxis display=none; yaxis -100 to 100 by 20 grid dotted

# Prepare plot data (exclude NA PCHG? SAS would have missing bar? SGPLOT would skip missing response. Keep but ggplot will drop NA.)
plot_data <- waterfall

p <- ggplot(plot_data, aes(x = bar_order, y = PCHG_display, fill = AVALC_GRP)) +
  geom_col(width = 1, colour = "black", linewidth = 0.25, na.rm = TRUE) +
  scale_fill_manual(values = col_map, name = "Best Overall Response", drop = FALSE) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.5) +
  geom_hline(yintercept = -30, colour = "blue", linetype = "dashed", linewidth = 0.6) +
  geom_hline(yintercept = 20, colour = "red", linetype = "dashed", linewidth = 0.6) +
  # threshold labels: place at right side
  annotate("text", x = max(plot_data$bar_order, na.rm=TRUE) * 1.0, y = -30 + 3,
           label = "-30% (PR threshold)", size = 2.8, hjust = 1, colour = "blue") +
  annotate("text", x = max(plot_data$bar_order, na.rm=TRUE) * 1.0, y = 20 + 3,
           label = "+20% (PD threshold)", size = 2.8, hjust = 1, colour = "red") +
  scale_x_continuous(expand = c(0, 0.5)) +
  scale_y_continuous(name = "Best Percentage Change from Baseline (%)",
                     limits = c(-100, 100), breaks = seq(-100, 100, by = 20)) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.major.y = element_line(linetype = "dotted", colour = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10)
  ) +
  labs(
    title = "Figure 14.2.3\nWaterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
    subtitle = "Efficacy Analysis Set",
    x = ""
  )

# Handle empty case: if no rows, create blank plot with message
if (nrow(plot_data)==0 || all(is.na(plot_data$PCHG_display))) {
  p <- ggplot() + annotate("text", x=0, y=0, label="No data for Efficacy Analysis Set (EFFFL='Y', PARAMCD='BPCHG')") +
    theme_void() +
    labs(title="Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions")
}

ggsave(file.path(out, "f_waterfall.png"), plot = p, width = 10, height = 6.5, units = "in", dpi = 300)

# ---- CSV of plotted data ----
# Reproduce SAS semantics: keep SAS dataset variable names
# waterfall contains USUBJID TRTA TRTAN PARAMCD PARAM PCHG AVALC EFFFL bar_order bar_color PCHG_display
csv_data <- waterfall %>%
  select(any_of(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL","bar_order","bar_color","PCHG_display")))
# Ensure numeric unrounded, dates none
write.csv(csv_data, file.path(out, "f_waterfall.csv"), row.names = FALSE, na = "")

# ---- Table / text rendering (f_waterfall.rtf -> .txt + CSV of table body) ----
# Expected: "table/listing text, 3 numbers" -> summary counts backing the figure.
# Produce summary by Best Overall Response (AVALC) with N, plus descriptive stats of PCHG?
# To guarantee 3 numbers, include N subjects, median PCHG, and % achieving PR threshold? Provide table with counts/percentages.
# Compute summary similar to efficacy table.

n_subj <- nrow(waterfall)
# Response category counts
if (n_subj > 0) {
  resp_tab <- waterfall %>%
    mutate(AVALC_DISP = as.character(AVALC_GRP)) %>%
    group_by(AVALC_DISP) %>%
    summarise(N = n(), .groups="drop") %>%
    mutate(PCT = ifelse(n_subj>0, 100*N/n_subj, NA_real_)) %>%
    arrange(match(AVALC_DISP, lvl))
  # stats of PCHG
  pchg_vals <- waterfall$PCHG_display[!is.na(waterfall$PCHG_display)]
  if (length(pchg_vals)>0) {
    med_pchg <- median(pchg_vals)
    mean_pchg <- mean(pchg_vals)
    min_pchg <- min(pchg_vals)
    max_pchg <- max(pchg_vals)
    sd_pchg <- if (length(pchg_vals)>1) sd(pchg_vals) else NA_real_
  } else {
    med_pchg <- mean_pchg <- min_pchg <- max_pchg <- sd_pchg <- NA_real_
  }
  n_pr <- sum(waterfall$PCHG_display <= -30, na.rm=TRUE)
  n_pd <- sum(waterfall$PCHG_display >= 20, na.rm=TRUE)
  pct_pr <- ifelse(n_subj>0, 100*n_pr/n_subj, NA_real_)
  pct_pd <- ifelse(n_subj>0, 100*n_pd/n_subj, NA_real_)
} else {
  resp_tab <- data.frame(AVALC_DISP=character(0), N=numeric(0), PCT=numeric(0))
  med_pchg <- mean_pchg <- min_pchg <- max_pchg <- sd_pchg <- NA_real_
  n_pr <- n_pd <- 0
  pct_pr <- pct_pd <- NA_real_
}

fmt_pct1 <- function(n, pct) {
  # SAS style "12 ( 34.5%)" - use 1 decimal
  if (is.na(n)) return("")
  if (is.na(pct)) return(as.character(n))
  sprintf("%d (%5.1f%%)", n, round_half_up(pct, 1))
}
fmt2 <- function(x) ifelse(is.na(x), "", sprintf("%.2f", round_half_up(x, 2)))
fmt1 <- function(x) ifelse(is.na(x), "", sprintf("%.1f", round_half_up(x, 1)))

# Build text rendering
lines <- c(
  "[Sponsor Name]                                                  Page 1 of 1",
  "Protocol: [Protocol Number]",
  "",
  "              Figure 14.2.3",
  "  Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
  "                  Efficacy Analysis Set",
  "",
  sprintf("  Number of subjects plotted: %d", n_subj),
  "",
  "  Best Overall Response (AVALC):",
  "  -------------------------------",
  "  Response      N      N (%)"
)
if (nrow(resp_tab)>0) {
  for (i in seq_len(nrow(resp_tab))) {
    lines <- c(lines, sprintf("  %-10s %4d   %s", resp_tab$AVALC_DISP[i], resp_tab$N[i],
                              fmt_pct1(resp_tab$N[i], resp_tab$PCT[i])))
  }
}
lines <- c(lines,
  "",
  "  Best Percentage Change from Baseline (%, capped at +/-100 for display):",
  sprintf("    N with non-missing PCHG : %d", sum(!is.na(waterfall$PCHG_display))),
  sprintf("    Mean (SD)               : %s (%s)", fmt2(mean_pchg), fmt2(sd_pchg)),
  sprintf("    Median                  : %s", fmt2(med_pchg)),
  sprintf("    Min, Max                : %s, %s", fmt1(min_pchg), fmt1(max_pchg)),
  "",
  sprintf("  Subjects with >=30%% shrinkage (PR threshold, PCHG <= -30): %s", fmt_pct1(n_pr, pct_pr)),
  sprintf("  Subjects with >=20%% growth (PD threshold, PCHG >= +20)   : %s", fmt_pct1(n_pd, pct_pd)),
  "",
  "  Each bar represents one subject (sorted by PCHG; bar_order 1..N).",
  "  Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%).",
  "  CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease",
  "",
  "  Data cutoff: [DDMONYYYY].",
  "  Source: ADaM datasets (ADTR PARAMCD=BPCHG joined to ADSL EFFFL=Y).",
  "  See f_waterfall.png for figure and f_waterfall.csv for plotted data."
)

writeLines(lines, file.path(out, "f_waterfall.txt"))

# CSV of table body: response counts + summary stats (the 3+ numbers)
# Provide two CSVs? Requirement: "as CSV of the table body" -> single CSV. Combine.
table_body <- data.frame(
  Category = c(as.character(resp_tab$AVALC_DISP), "PCHG Mean (SD)", "PCHG Median", "PCHG Min, Max",
               "PCHG <= -30% (PR threshold)", "PCHG >= +20% (PD threshold)"),
  N = c(resp_tab$N, sum(!is.na(waterfall$PCHG_display)), sum(!is.na(waterfall$PCHG_display)),
        sum(!is.na(waterfall$PCHG_display)), n_pr, n_pd),
  Percent = c(round_half_up(resp_tab$PCT,1), rep(NA_real_, 4)),
  Statistic = c(rep(NA_character_, nrow(resp_tab)),
                sprintf("%s (%s)", fmt2(mean_pchg), fmt2(sd_pchg)),
                fmt2(med_pchg),
                sprintf("%s, %s", fmt1(min_pchg), fmt1(max_pchg)),
                fmt_pct1(n_pr, pct_pr),
                fmt_pct1(n_pd, pct_pd)),
  stringsAsFactors = FALSE
)
# Also write separate summary? Keep one file: f_waterfall_table.csv? Spec says "<name>.csv" of table body.
# We already wrote f_waterfall.csv as plotted data. To satisfy "each table additionally as CSV of table body",
# write f_waterfall_table.csv as well (and keep f_waterfall.csv as plotted data).
write.csv(table_body, file.path(out, "f_waterfall_table.csv"), row.names = FALSE, na = "")

cat("Done. N subjects:", n_subj, "\n")
```