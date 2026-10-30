# P082 f_waterfall.sas -> R translation
# Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtmpath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
tlfpath  <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"

# helpers ---------------------------------------------------------------
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

read_adam <- function(libpath, memname) {
  # try various extensions case-insensitive
  if (!dir.exists(libpath)) return(NULL)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  # find file matching memname ignoring case and extension
  idx <- which(tolower(tools::file_path_sans_ext(basename(files))) == tolower(memname))
  if (length(idx) == 0) return(NULL)
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- NULL
  try({
    if (ext == "sas7bdat") df <- haven::read_sas(f)
    else if (ext == "xpt") df <- haven::read_xpt(f)
    else if (ext == "xpt2") df <- haven::read_xpt(f)
    else df <- haven::read_sas(f)
  }, silent = TRUE)
  if (is.null(df)) {
    try({ df <- haven::read_xpt(f) }, silent = TRUE)
  }
  if (!is.null(df)) {
    # convert haven labelled to plain
    df <- haven::zap_formats(df)
    df <- haven::zap_label(df)
    df <- haven::zap_widths(df)
  }
  df
}

# normalize names to uppercase for processing, keep original mapping
upper_names <- function(df) {
  if (is.null(df)) return(NULL)
  names(df) <- toupper(names(df))
  df
}

# Load ADSL and ADTR ----------------------------------------------------
adsl_raw <- read_adam(adampath, "adsl")
adtr_raw <- read_adam(adampath, "adtr")
adrs_raw <- read_adam(adampath, "adrs") # fallback

adsl <- upper_names(adsl_raw)
adtr <- upper_names(adtr_raw)
adrs <- upper_names(adrs_raw)

# Step 1: Get best percentage change per subject ------------------------
# SAS: select a.USUBJID, a.TRTA, a.TRTAN, a.PARAMCD, a.PARAM, a.PCHG, a.AVALC, b.EFFFL
#      from adam.adtr a inner join adam.adsl b on a.USUBJID=b.USUBJID
#      where b.EFFFL='Y' and a.PARAMCD='BPCHG' order by a.PCHG

tumor <- NULL

if (!is.null(adtr) && all(c("USUBJID","PARAMCD","PCHG","AVALC") %in% names(adtr)) &&
    !is.null(adsl) && "EFFFL" %in% names(adsl)) {

  # ensure required cols exist, add missing as NA
  for (col in c("TRTA","TRTAN","PARAM","PARAMCD","PCHG","AVALC","USUBJID")) {
    if (!col %in% names(adtr)) adtr[[col]] <- NA
  }
  for (col in c("USUBJID","EFFFL")) {
    if (!col %in% names(adsl)) adsl[[col]] <- NA
  }

  # inner join
  # keep only EFFFL='Y' subjects
  adsl_eff <- adsl[adsl$EFFFL == "Y" & !is.na(adsl$EFFFL), , drop = FALSE]

  # filter adtr PARAMCD == 'BPCHG'
  adtr_filt <- adtr[adtr$PARAMCD == "BPCHG" & !is.na(adtr$PARAMCD), , drop = FALSE]

  if (nrow(adsl_eff) > 0 && nrow(adtr_filt) > 0) {
    tumor <- merge(adtr_filt[, c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC"), drop=FALSE],
                   adsl_eff[, c("USUBJID","EFFFL"), drop=FALSE],
                   by = "USUBJID", all = FALSE)
    # order by PCHG (SAS missing first, ascending)
    # R: NA first when na.last=FALSE
    if (nrow(tumor) > 0) {
      tumor <- tumor[order(tumor$PCHG, na.last = FALSE), , drop = FALSE]
    }
  }
}

# Alternative derive from ADRS if ADTR not available or empty
if (is.null(tumor) || nrow(tumor) == 0) {
  if (!is.null(adrs) && !is.null(adsl)) {
    # SAS alternative commented: min(a.PCHG) as PCHG, b.AVALC as BOR grouped by USUBJID,TRTA,TRTAN,b.AVALC where EFFFL='Y' and PARAMCD='SUMLD'
    # Try to reproduce
    if (all(c("USUBJID","PARAMCD","PCHG") %in% names(adrs))) {
      adsl_eff2 <- adsl[adsl$EFFFL == "Y" & !is.na(adsl$EFFFL), , drop = FALSE]
      adrs_filt <- adrs[adrs$PARAMCD == "SUMLD" & !is.na(adrs$PARAMCD), , drop = FALSE]
      if (nrow(adsl_eff2) > 0 && nrow(adrs_filt) > 0) {
        # join to get BOR (AVALC from ADSL? SAS uses b.AVALC)
        # In ADSL, AVALC may not exist; use ADRS AVALC if needed
        # We'll merge and then aggregate min PCHG per USUBJID
        tmp <- merge(adrs_filt, adsl_eff2[, c("USUBJID","EFFFL"), drop=FALSE], by="USUBJID")
        # aggregate
        # need TRTA, TRTAN, AVALC
        for (col in c("TRTA","TRTAN","AVALC","PARAM","PARAMCD")) if (!col %in% names(tmp)) tmp[[col]] <- NA
        # use dplyr for grouping
        suppressMessages(library(dplyr))
        tumor <- tmp %>%
          group_by(USUBJID, TRTA, TRTAN, AVALC) %>%
          summarise(PCHG = suppressWarnings(min(PCHG, na.rm=TRUE)), .groups="drop") %>%
          mutate(PARAMCD = "BPCHG", PARAM = "Best Percent Change", EFFFL="Y") %>%
          arrange(PCHG) %>%
          as.data.frame()
        # handle Inf from all NA
        tumor$PCHG[is.infinite(tumor$PCHG)] <- NA_real_
        tumor <- tumor[order(tumor$PCHG, na.last=FALSE), , drop=FALSE]
      }
    }
  }
}

if (is.null(tumor) || nrow(tumor) == 0) {
  # create empty structure to avoid crash
  tumor <- data.frame(USUBJID=character(0), TRTA=character(0), TRTAN=numeric(0),
                      PARAMCD=character(0), PARAM=character(0), PCHG=numeric(0),
                      AVALC=character(0), EFFFL=character(0), stringsAsFactors=FALSE)
}

# Step 2: Assign display order (sorted by percent change) ---------------
# data waterfall; set tumor; by PCHG; bar_order=_n_;
# bar_color mapping; cap PCHG_display

waterfall <- tumor
waterfall$bar_order <- seq_len(nrow(waterfall))

# bar_color by AVALC upcase
waterfall$bar_color <- NA_character_
if (nrow(waterfall) > 0) {
  avalc_up <- toupper(waterfall$AVALC)
  waterfall$bar_color <- ifelse(avalc_up == "CR", "darkgreen",
                         ifelse(avalc_up == "PR", "green",
                         ifelse(avalc_up == "SD", "gold",
                         ifelse(avalc_up == "PD", "red", "gray"))))
  # handle NA AVALC -> gray
  waterfall$bar_color[is.na(waterfall$bar_color)] <- "gray"
  waterfall$bar_color[is.na(avalc_up)] <- "gray"
} else {
  waterfall$bar_color <- character(0)
}

# Cap at reasonable bounds
waterfall$PCHG_display <- waterfall$PCHG
if (nrow(waterfall) > 0) {
  waterfall$PCHG_display <- ifelse(!is.na(waterfall$PCHG) & waterfall$PCHG > 100, 100,
                            ifelse(!is.na(waterfall$PCHG) & waterfall$PCHG < -100, -100,
                                   waterfall$PCHG))
}

# Ensure AVALC factor for legend ordering: CR, PR, SD, PD, others
if (nrow(waterfall) > 0) {
  # preserve SAS character ordering for legend: sort unique AVALC
  # but enforce desired order CR, PR, SD, PD, then others alphabetically
  desired <- c("CR","PR","SD","PD")
  uniq_vals <- unique(waterfall$AVALC)
  uniq_vals <- uniq_vals[!is.na(uniq_vals)]
  other_vals <- setdiff(uniq_vals, desired)
  other_vals <- sort(other_vals)
  lev <- c(intersect(desired, uniq_vals), other_vals)
  # include NA level if present
  if (any(is.na(waterfall$AVALC))) lev <- c(lev, NA)
  waterfall$AVALC <- factor(waterfall$AVALC, levels = lev)
}

# Write CSV of plotted data ---------------------------------------------
# SAS variable names: USUBJID TRTA TRTAN PARAMCD PARAM PCHG AVALC EFFFL bar_order bar_color PCHG_display
# Keep same case (uppercase) for CSV; dates not present
out_csv <- file.path(out, "f_waterfall.csv")
# Convert factor AVALC back to character for CSV
waterfall_csv <- waterfall
if ("AVALC" %in% names(waterfall_csv) && is.factor(waterfall_csv$AVALC)) {
  waterfall_csv$AVALC <- as.character(waterfall_csv$AVALC)
}
# Ensure column order
cols_order <- c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL","bar_order","bar_color","PCHG_display")
cols_present <- intersect(cols_order, names(waterfall_csv))
cols_extra <- setdiff(names(waterfall_csv), cols_present)
waterfall_csv <- waterfall_csv[, c(cols_present, cols_extra), drop=FALSE]
write.csv(waterfall_csv, out_csv, row.names = FALSE, na = "")

# Step 3: Generate waterfall plot ---------------------------------------
suppressMessages(library(ggplot2))

# color mapping for fill
fill_colors <- c("CR"="darkgreen", "PR"="green", "SD"="gold", "PD"="red")
# add gray for any other
# Build palette for all levels present
if (nrow(waterfall) > 0) {
  levs <- levels(waterfall$AVALC)
  pal <- sapply(levs, function(x) {
    if (is.na(x)) return("gray")
    if (x %in% names(fill_colors)) return(fill_colors[[x]])
    else return("gray")
  })
  names(pal) <- levs
} else {
  pal <- fill_colors
}

# Create plot
# SAS: vbar bar_order / response=PCHG_display group=AVALC groupdisplay=cluster barwidth=1 outline
# Use geom_col with position dodge? SAS groupdisplay=cluster means side-by-side but each bar_order has one AVALC, so effectively stacked single bar per x.
# We'll use geom_col with fill=AVALC, width=1, color=black

p <- ggplot(waterfall, aes(x = bar_order, y = PCHG_display, fill = AVALC)) +
  geom_col(width = 1, colour = "black", linewidth = 0.25) +
  scale_fill_manual(values = pal, name = "Best Overall Response", drop = FALSE, na.value = "gray") +
  geom_hline(yintercept = -30, linetype = "dashed", colour = "blue", linewidth = 0.6) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "red", linewidth = 0.6) +
  geom_hline(yintercept = 0, linetype = "solid", colour = "black", linewidth = 0.6) +
  annotate("text", x = max(waterfall$bar_order, na.rm=TRUE) * 0.95, y = -30 + 3,
           label = "-30% (PR threshold)", size = 2.5, colour = "blue", hjust = 1) +
  annotate("text", x = max(waterfall$bar_order, na.rm=TRUE) * 0.95, y = 20 + 3,
           label = "+20% (PD threshold)", size = 2.5, colour = "red", hjust = 1) +
  scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 20),
                     name = "Best Percentage Change from Baseline (%)") +
  scale_x_continuous(expand = c(0,0)) +
  labs(
    title = "Figure 14.2.3",
    subtitle = "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
    x = NULL,
    caption = paste0("Data cutoff: [DDMONYYYY]\n",
                     "Each bar represents one subject.\n",
                     "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease\n",
                     "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)")
  ) +
  theme_bw(base_size = 10) +
  theme(
    panel.grid.major.y = element_line(linetype = "dotted", colour = "lightgray"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 7),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    plot.caption = element_text(hjust = 0, size = 7, lineheight = 0.9),
    plot.caption.position = "plot"
  ) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))

# Handle empty data case: still produce empty plot with axes
if (nrow(waterfall) == 0) {
  p <- ggplot() +
    scale_y_continuous(limits = c(-100, 100), breaks = seq(-100, 100, by = 20),
                       name = "Best Percentage Change from Baseline (%)") +
    labs(title = "Figure 14.2.3",
         subtitle = "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
         x = NULL) +
    theme_bw()
}

# Save PNG --------------------------------------------------------------
out_png <- file.path(out, "f_waterfall.png")
# width 10in height 6.5in as in SAS ods graphics
ggsave(filename = out_png, plot = p, width = 10, height = 6.5, units = "in", dpi = 300, bg = "white")

# Also write a plain-text rendering for completeness (table body)
out_txt <- file.path(out, "f_waterfall.txt")
txt_con <- file(out_txt, open = "wt")
writeLines(c(
  "Figure 14.2.3",
  "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
  "Efficacy Analysis Set",
  "",
  paste0("N subjects plotted: ", nrow(waterfall)),
  "",
  "Reference lines: -30% (PR threshold) dashed blue, +20% (PD threshold) dashed red, 0 solid black",
  "Y-axis: -100 to 100 by 20, X-axis: subjects sorted by PCHG (bar_order)",
  "",
  "Legend: Best Overall Response (AVALC) with colors: CR=darkgreen, PR=green, SD=gold, PD=red, other=gray",
  "",
  "Data cutoff: [DDMONYYYY]",
  "Each bar represents one subject.",
  "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease",
  "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)"
), con = txt_con)
close(txt_con)

# Clean up (mimic proc datasets delete)
rm(tumor)

cat("NOTE: f_waterfall.R completed successfully.\n")
cat(paste0("PNG: ", out_png, "\n"))
cat(paste0("CSV: ", out_csv, "\n"))
