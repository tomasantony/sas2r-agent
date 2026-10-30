```r
# P082 f_waterfall.sas -> R translation
# Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

adam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"
sdtm_path <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm"
tlfpath   <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(purrr)
  library(tibble)
  library(ggplot2)
  library(data.table)
})

# helper: round half away from zero (SAS)
round_sas <- function(x, digits=0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: read adam dataset by name (case-insensitive, sas7bdat or xpt)
read_adam <- function(libpath, dsn) {
  dsn_low <- tolower(dsn)
  if (!dir.exists(libpath)) return(NULL)
  files <- list.files(libpath, full.names = TRUE, recursive = FALSE)
  if (length(files)==0) return(NULL)
  # map lower basename without extension to file
  basenames <- tolower(tools::file_path_sans_ext(basename(files)))
  idx <- which(basenames == dsn_low)
  if (length(idx)==0) return(NULL)
  f <- files[idx[1]]
  ext <- tolower(tools::file_ext(f))
  df <- tryCatch({
    if (ext == "sas7bdat") haven::read_sas(f)
    else if (ext == "xpt") haven::read_xpt(f)
    else if (ext == "xpt2") haven::read_xpt(f)
    else haven::read_sas(f)
  }, error=function(e) {
    # try alternative extension
    tryCatch(haven::read_xpt(f), error=function(e2) NULL)
  })
  if (is.null(df)) return(NULL)
  # convert haven_labelled to plain, keep names as is
  df <- haven::zap_formats(df)
  df <- haven::zap_labels(df)
  # make names accessible case-insensitively: keep original but also create upper lookup
  df
}

# helper: get column by case-insensitive name
get_col <- function(df, colname) {
  if (is.null(df)) return(NULL)
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx)==0) return(NULL)
  df[[ idx[1] ]]
}
has_col <- function(df, colname) {
  if (is.null(df)) return(FALSE)
  toupper(colname) %in% toupper(names(df))
}
col_name <- function(df, colname) {
  # return actual name as stored
  idx <- which(toupper(names(df)) == toupper(colname))
  if (length(idx)==0) return(NA_character_)
  names(df)[idx[1]]
}

# Read ADSL and ADTR/ADRS
adsl <- read_adam(adam_path, "adsl")
adtr <- read_adam(adam_path, "adtr")
adrs <- read_adam(adam_path, "adrs")

# If adam_path not found, try alternative raw paths mentioned in header
if (is.null(adsl)) {
  alt1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
  adsl <- read_adam(alt1, "adsl")
  if (is.null(adtr)) adtr <- read_adam(alt1, "adtr")
  if (is.null(adrs)) adrs <- read_adam(alt1, "adrs")
}
if (is.null(adsl)) {
  alt2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
  if (is.null(adsl)) adsl <- read_adam(alt2, "adsl")
}

# Diagnostic
message("ADSL rows: ", ifelse(is.null(adsl), 0, nrow(adsl)))
message("ADTR rows: ", ifelse(is.null(adtr), 0, nrow(adtr)))
message("ADRS rows: ", ifelse(is.null(adrs), 0, nrow(adrs)))
if (!is.null(adsl)) message("ADSL cols: ", paste(names(adsl), collapse=", "))
if (!is.null(adtr)) message("ADTR cols: ", paste(names(adtr), collapse=", "))

tumor <- NULL

# Step 1: Get best percentage change per subject
# Primary path: adam.adtr where PARAMCD='BPCHG' and EFFFL='Y'
if (!is.null(adtr) && has_col(adtr, "PARAMCD") && has_col(adtr, "PCHG") && has_col(adtr, "USUBJID")) {
  # need ADSL for EFFFL
  if (!is.null(adsl) && has_col(adsl, "USUBJID") && has_col(adsl, "EFFFL")) {
    # standardize column names to upper for join
    adtr_u <- adtr
    adsl_u <- adsl
    names(adtr_u) <- toupper(names(adtr_u))
    names(adsl_u) <- toupper(names(adsl_u))
    # ensure character types for join keys
    adtr_u$USUBJID <- as.character(adtr_u$USUBJID)
    adsl_u$USUBJID <- as.character(adsl_u$USUBJID)
    # filter ADSL EFFFL='Y'
    adsl_eff <- adsl_u %>% filter(toupper(EFFFL) == "Y") %>% select(USUBJID, EFFFL)
    # filter ADTR PARAMCD='BPCHG'
    adtr_filt <- adtr_u %>% filter(toupper(PARAMCD) == "BPCHG")
    # inner join
    tumor_raw <- inner_join(adtr_filt, adsl_eff, by="USUBJID")
    if (nrow(tumor_raw) > 0) {
      # select required columns if exist, otherwise keep what we have
      keep <- intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL"), names(tumor_raw))
      tumor <- tumor_raw %>% select(all_of(keep)) %>% arrange(PCHG)
      # ensure PCHG numeric
      tumor$PCHG <- suppressWarnings(as.numeric(tumor$PCHG))
      # drop rows where PCHG is NA? SAS would keep but order missing first; keep them
      tumor <- tumor %>% arrange(PCHG)
    }
  } else {
    # no ADSL EFFFL, just filter ADTR alone if it has EFFFL
    if (has_col(adtr, "EFFFL")) {
      adtr_u <- adtr
      names(adtr_u) <- toupper(names(adtr_u))
      adtr_u$PCHG <- suppressWarnings(as.numeric(adtr_u$PCHG))
      tumor <- adtr_u %>% filter(toupper(EFFFL)=="Y", toupper(PARAMCD)=="BPCHG") %>% arrange(PCHG)
      keep <- intersect(c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL"), names(tumor))
      tumor <- tumor %>% select(all_of(keep))
    }
  }
}

# Alternative derivation from ADRS if ADTR not available or empty
if (is.null(tumor) || nrow(tumor)==0) {
  message("ADTR BPCHG not found or empty, trying ADRS derivation")
  if (!is.null(adrs) && has_col(adrs, "USUBJID") && has_col(adrs, "PCHG")) {
    adrs_u <- adrs
    adsl_u <- adsl
    if (!is.null(adsl_u)) names(adsl_u) <- toupper(names(adsl_u))
    names(adrs_u) <- toupper(names(adrs_u))
    adrs_u$USUBJID <- as.character(adrs_u$USUBJID)
    if (!is.null(adsl_u)) adsl_u$USUBJID <- as.character(adsl_u$USUBJID)
    # filter EFFFL
    if (!is.null(adsl_u) && "EFFFL" %in% names(adsl_u)) {
      adsl_eff <- adsl_u %>% filter(toupper(EFFFL)=="Y") %>% select(USUBJID, EFFFL)
      adrs_u <- inner_join(adrs_u, adsl_eff, by="USUBJID")
    }
    # filter PARAMCD = SUMLD if exists, otherwise take all
    if ("PARAMCD" %in% names(adrs_u)) {
      # SAS alternative commented: where PARAMCD='SUMLD'
      # keep only SUMLD if present
      if (any(toupper(adrs_u$PARAMCD)=="SUMLD", na.rm=TRUE)) {
        adrs_u <- adrs_u %>% filter(toupper(PARAMCD)=="SUMLD")
      }
    }
    adrs_u$PCHG <- suppressWarnings(as.numeric(adrs_u$PCHG))
    # group by USUBJID, TRTA, TRTAN, AVALC (BOR) and take min PCHG
    # Determine grouping cols available
    grp_cols <- intersect(c("USUBJID","TRTA","TRTAN"), names(adrs_u))
    # AVALC as BOR may be in ADSL or ADRS; try to get BOR
    # If ADRS has AVALC, use it; otherwise try ADSL AVALC
    if ("AVALC" %in% names(adrs_u)) {
      tumor_alt <- adrs_u %>%
        group_by(across(all_of(c(grp_cols, "AVALC")))) %>%
        summarise(PCHG = min(PCHG, na.rm=TRUE), .groups="drop")
      # need PARAMCD/PARAM placeholders
      tumor_alt$PARAMCD <- "BPCHG"
      tumor_alt$PARAM <- "Best Percent Change"
      if ("EFFFL" %in% names(adrs_u)) {
        # keep EFFFL
      } else {
        tumor_alt$EFFFL <- "Y"
      }
      tumor <- tumor_alt %>% arrange(PCHG)
    } else {
      tumor_alt <- adrs_u %>%
        group_by(across(all_of(grp_cols))) %>%
        summarise(PCHG = min(PCHG, na.rm=TRUE), .groups="drop") %>%
        arrange(PCHG)
      tumor_alt$PARAMCD <- "BPCHG"
      tumor_alt$PARAM <- "Best Percent Change"
      tumor_alt$AVALC <- NA_character_
      tumor_alt$EFFFL <- "Y"
      tumor <- tumor_alt
    }
  }
}

# If still null, create empty structure to avoid crash
if (is.null(tumor)) {
  tumor <- tibble(USUBJID=character(), TRTA=character(), TRTAN=numeric(), PARAMCD=character(), PARAM=character(), PCHG=numeric(), AVALC=character(), EFFFL=character())
}
# Ensure required columns exist
for (cn in c("USUBJID","TRTA","TRTAN","PARAMCD","PARAM","PCHG","AVALC","EFFFL")) {
  if (!cn %in% names(tumor)) tumor[[cn]] <- NA
}
# Ensure types
tumor$PCHG <- suppressWarnings(as.numeric(tumor$PCHG))
tumor$AVALC <- as.character(tumor$AVALC)
tumor$TRTA <- as.character(tumor$TRTA)
if (!is.numeric(tumor$TRTAN)) tumor$TRTAN <- suppressWarnings(as.numeric(tumor$TRTAN))

# Remove rows with missing USUBJID or PCHG? SAS would keep but plot would miss; keep all with PCHG
# Sort by PCHG ascending (SAS order by PCHG, missing first)
tumor <- tumor %>% arrange(PCHG)

# Step 2: Assign display order and colors
waterfall <- tumor %>%
  mutate(
    bar_order = row_number(),
    AVALC_up = toupper(AVALC),
    bar_color = case_when(
      AVALC_up == "CR" ~ "darkgreen",
      AVALC_up == "PR" ~ "green",
      AVALC_up == "SD" ~ "gold",
      AVALC_up == "PD" ~ "red",
      TRUE ~ "gray"
    ),
    PCHG_display = case_when(
      PCHG > 100 ~ 100,
      PCHG < -100 ~ -100,
      TRUE ~ PCHG
    )
  )

# Ensure AVALC factor for legend ordering: CR, PR, SD, PD, others
# Preserve SAS character ordering: use table order appearance but set desired order
resp_levels <- c("CR","PR","SD","PD")
present_levels <- unique(toupper(na.omit(waterfall$AVALC)))
# keep order CR, PR, SD, PD, then any others alphabetically
other_levels <- setdiff(present_levels, resp_levels)
other_levels <- sort(other_levels)
legend_levels <- c(intersect(resp_levels, present_levels), other_levels)
# If AVALC has mixed case, map to original case for display but use upper for matching
# Create display label as original AVALC value (first occurrence)
label_map <- waterfall %>%
  filter(!is.na(AVALC)) %>%
  distinct(AVALC_up, AVALC) %>%
  group_by(AVALC_up) %>% slice(1) %>% ungroup()
# Build factor
waterfall$AVALC_f <- factor(toupper(waterfall$AVALC), levels=legend_levels)
# For plotting, use AVALC_f but legend labels as original
# Create color mapping
color_map <- c("CR"="darkgreen", "PR"="green", "SD"="gold", "PD"="red")
# assign gray to others
for (lvl in legend_levels) {
  if (!lvl %in% names(color_map)) color_map[lvl] <- "gray"
}
# Handle NA level
if (any(is.na(waterfall$AVALC_f))) {
  # add NA as gray
  waterfall$AVALC_f <- forcats::fct_explicit_na(waterfall$AVALC_f, na_level="(Missing)")
  color_map["(Missing)"] <- "gray"
  legend_levels <- c(legend_levels, "(Missing)")
}

# Write CSV of plotted data (SAS dataset counterpart)
# Dates as YYYY-MM-DD not needed here; numeric unrounded
out_csv <- file.path(out, "f_waterfall.csv")
# Keep SAS variable names same case: USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC, EFFFL, bar_order, bar_color, PCHG_display
# Ensure column order as in SAS
csv_out <- waterfall %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC, EFFFL, bar_order, bar_color, PCHG_display)
write.csv(csv_out, out_csv, row.names=FALSE, na="")

# Also write waterfall.csv for compatibility
write.csv(csv_out, file.path(out, "waterfall.csv"), row.names=FALSE, na="")

# Step 3: Generate waterfall plot
# Titles and footnotes verbatim from SAS
title4 <- "Figure 14.2.3"
title5 <- "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions"
# Footnotes
cutdate <- "[DDMONYYYY]"  # placeholder as in u_setup
# Try to read cutdate from env if available
# Use sponsor/protocol placeholders
footnote1 <- paste0("Data cutoff: ", cutdate, ".")
footnote2 <- "Each bar represents one subject."
footnote3 <- "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease"
footnote4 <- "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)"
footnote5 <- paste0("Program: f_waterfall.sas | Run: ", format(Sys.Date(), "%d%b%Y"))

# Plot
# If no data, create empty plot with message
if (nrow(waterfall)==0) {
  p <- ggplot() +
    annotate("text", x=0.5, y=0.5, label="No data for Efficacy Analysis Set (EFFFL='Y') and PARAMCD='BPCHG'") +
    theme_void() +
    labs(title=title4, subtitle=title5, caption=paste(footnote1, footnote2, footnote3, footnote4, footnote5, sep="\n"))
} else {
  # Ensure bar_order is numeric for x axis
  p <- ggplot(waterfall, aes(x=bar_order, y=PCHG_display, fill=AVALC_f)) +
    geom_col(width=1, colour="black", linewidth=0.25) +
    scale_fill_manual(values=color_map, name="Best Overall Response", na.value="gray") +
    geom_hline(yintercept=-30, linetype="dashed", colour="blue", linewidth=0.7) +
    geom_hline(yintercept=20, linetype="dashed", colour="red", linewidth=0.7) +
    geom_hline(yintercept=0, colour="black", linewidth=0.5) +
    annotate("text", x= max(waterfall$bar_order, na.rm=TRUE)*0.95, y=-30, label="-30% (PR threshold)", size=2.8, hjust=1, vjust=-0.5, colour="blue") +
    annotate("text", x= max(waterfall$bar_order, na.rm=TRUE)*0.95, y=20, label="+20% (PD threshold)", size=2.8, hjust=1, vjust=-0.5, colour="red") +
    scale_y_continuous(
      name="Best Percentage Change from Baseline (%)",
      limits=c(-100, 100),
      breaks=seq(-100, 100, by=20),
      expand=c(0,0)
    ) +
    scale_x_continuous(expand=c(0,0)) +
    labs(
      title=title4,
      subtitle=title5,
      x=NULL,
      y="Best Percentage Change from Baseline (%)",
      caption=paste(footnote1, footnote2, footnote3, footnote4, footnote5, sep="\n")
    ) +
    theme_minimal(base_size=10) +
    theme(
      plot.title = element_text(hjust=0.5, face="bold", size=11),
      plot.subtitle = element_text(hjust=0.5, size=10),
      plot.caption = element_text(hjust=0, size=7, lineheight=0.9),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.y = element_line(linetype="dotted", colour="lightgray"),
      legend.position="bottom",
      legend.title=element_text(size=8),
      legend.text=element_text(size=7),
      plot.margin=margin(10,10,10,10)
    ) +
    guides(fill=guide_legend(nrow=1, byrow=TRUE))
}

# Save PNG
out_png <- file.path(out, "f_waterfall.png")
# width 10in height 6.5in as in SAS ods graphics
ggsave(filename=out_png, plot=p, width=10, height=6.5, units="in", dpi=300, bg="white")

# Also write plain-text rendering of figure data (table body)
out_txt <- file.path(out, "f_waterfall.txt")
txt_con <- file(out_txt, "w")
writeLines(c(
  "Figure 14.2.3",
  "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions",
  "Efficacy Analysis Set",
  "",
  paste0("Number of subjects plotted: ", nrow(waterfall)),
  "",
  "Reference lines: -30% (PR threshold, blue dashed), +20% (PD threshold, red dashed), 0% (solid)",
  "",
  "Data cutoff: [DDMONYYYY].",
  "Each bar represents one subject.",
  "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease",
  "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)",
  paste0("Program: f_waterfall.sas | Run: ", format(Sys.Date(), "%d%b%Y")),
  "",
  "Summary by Best Overall Response:",
  ""
), txt_con)
# summary counts
if (nrow(waterfall)>0) {
  summ <- waterfall %>%
    group_by(AVALC_f) %>%
    summarise(N=n(), Mean_PCHG=mean(PCHG, na.rm=TRUE), Median_PCHG=median(PCHG, na.rm=TRUE), Min=min(PCHG, na.rm=TRUE), Max=max(PCHG, na.rm=TRUE), .groups="drop")
  # write summary
  writeLines(paste0(sprintf("%-20s %5s %10s %10s %10s %10s", "Response","N","Mean","Median","Min","Max")), txt_con)
  for (i in seq_len(nrow(summ))) {
    writeLines(sprintf("%-20s %5d %10.2f %10.2f %10.2f %10.2f",
                       as.character(summ$AVALC_f[i]), summ$N[i], summ$Mean_PCHG[i], summ$Median_PCHG[i], summ$Min[i], summ$Max[i]), txt_con)
  }
  writeLines("", txt_con)
  writeLines("First 10 bars (sorted by PCHG):", txt_con)
  head10 <- head(waterfall %>% arrange(bar_order) %>% select(bar_order, USUBJID, TRTA, PCHG, PCHG_display, AVALC), 10)
  writeLines(paste0(sprintf("%-8s %-15s %-20s %10s %10s %-10s", "Order","USUBJID","TRTA","PCHG","PCHG_disp","AVALC")), txt_con)
  for (i in seq_len(nrow(head10))) {
    writeLines(sprintf("%-8d %-15s %-20s %10.2f %10.2f %-10s",
                       head10$bar_order[i], head10$USUBJID[i], head10$TRTA[i], head10$PCHG[i], head10$PCHG_display[i], head10$AVALC[i]), txt_con)
  }
}
close(txt_con)

message("f_waterfall.R completed successfully. Outputs in ", out)
```