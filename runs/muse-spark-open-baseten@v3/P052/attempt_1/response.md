```r
# P052_outliers_Scatter_MeanTime.sas -> R translation
# Outputs: scatter plot (SGRender) + listing of ranges (P052.lst)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find file if absolute Windows path not present on Linux runner
resolve_path <- function(win_path) {
  if (file.exists(win_path)) return(win_path)
  # try forward slash variant
  p2 <- gsub("D:/meta_hackathon/sas2r-agent", Sys.getenv("ROOT"), win_path, fixed = TRUE)
  if (!is.na(p2) && file.exists(p2)) return(p2)
  # try OUT_DIR relative search
  bn <- basename(win_path)
  # search under /data or current working dir
  candidates <- c(
    file.path("D:", "meta_hackathon", "sas2r-agent", "data", "raw", "cdisc-pilot", "adam", bn),
    file.path("/tmp", bn),
    file.path(getwd(), bn)
  )
  # also search recursively under root if exists
  root <- Sys.getenv("ROOT")
  if (root != "" && dir.exists(root)) {
    found <- list.files(root, pattern = bn, recursive = TRUE, full.names = TRUE)
    if (length(found) > 0) return(found[1])
  }
  # fallback: search under data
  if (dir.exists("data")) {
    found2 <- list.files("data", pattern = bn, recursive = TRUE, full.names = TRUE)
    if (length(found2) > 0) return(found2[1])
  }
  return(win_path)
}

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(purrr)
library(tibble)
library(ggplot2)

rawadam_win <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adsl_path <- resolve_path(file.path(rawadam_win, "adsl.xpt"))
advsmax_path <- resolve_path(file.path(rawadam_win, "advsmax.xpt"))

# If still not found, try inlib derived path
if (!file.exists(advsmax_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt"
  alt2 <- resolve_path(alt)
  if (file.exists(alt2)) advsmax_path <- alt2
}
if (!file.exists(adsl_path)) {
  alt <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/adsl.xpt"
  alt2 <- resolve_path(alt)
  if (file.exists(alt2)) adsl_path <- alt2
}

# Read XPT
adsl <- haven::read_xpt(adsl_path)
advsmax <- haven::read_xpt(advsmax_path)

# Ensure character columns are character (haven may give labelled)
advsmax <- advsmax %>% mutate(across(where(~ inherits(., "haven_labelled")), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

adsl <- adsl %>% mutate(across(where(~ inherits(., "haven_labelled")), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

# proc sort data=advsmax; by studyid;
advsmax <- advsmax %>% arrange(STUDYID)

# data advsmax_final; WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y" and ADY>1 and SAFFL="Y";
# if first.studyid then LLN_label="LLN"; ULN_label="ULN";
# Note: SAS variable names are uppercase; handle case-insensitive
# Normalize column names to uppercase for filtering, then keep original case for output
# Work with uppercase names
names(advsmax) <- toupper(names(advsmax))

advsmax_final <- advsmax %>%
  filter(PARAMCD == "DIABP" & ATPTN == 815 & ANL01FL == "Y" & ADY > 1 & SAFFL == "Y") %>%
  arrange(STUDYID)

# first.studyid logic
advsmax_final <- advsmax_final %>%
  group_by(STUDYID) %>%
  mutate(
    LLN_label = ifelse(row_number() == 1, "LLN", NA_character_),
    ULN_label = ifelse(row_number() == 1, "ULN", NA_character_)
  ) %>%
  ungroup()

# Creating ranges so that axis can be equated
# proc sql; create table range as select min(base) as min_base, max(base) as max_base, min(aval) as min_aval, max(aval) as max_aval from advsmax_final;
range_tbl <- advsmax_final %>%
  summarise(
    min_base = min(BASE, na.rm = TRUE),
    max_base = max(BASE, na.rm = TRUE),
    min_aval = min(AVAL, na.rm = TRUE),
    max_aval = max(AVAL, na.rm = TRUE)
  )

# data range2; min = min(min_base, min_aval); max = max(max_base, max_aval);
range2 <- range_tbl %>%
  mutate(
    min = pmin(min_base, min_aval, na.rm = TRUE),
    max = pmax(max_base, max_aval, na.rm = TRUE)
  )

min_val <- range2$min[1]
max_val <- range2$max[1]

# Finding out the amount of studyids
# proc sql; create table advsmax_final2 as select *, count(distinct studyid) as count_study from advsmax_final;
count_study <- n_distinct(advsmax_final$STUDYID)

advsmax_final2 <- advsmax_final %>%
  mutate(count_study = count_study)

# data advsmax_final3; if count_study <=2 then column_numbers=2 row_numbers=1; if count_study>2 then column_numbers=2 row_numbers=2;
advsmax_final3 <- advsmax_final2 %>%
  mutate(
    column_numbers = 2,
    row_numbers = ifelse(count_study <= 2, 1, 2)
  )

column_numbers <- unique(advsmax_final3$column_numbers)[1]
row_numbers <- unique(advsmax_final3$row_numbers)[1]

# ------------------------------------------------------------------
# Outputs: datasets as CSV (SAS variable names, ISO dates)
# Write advsmax_final3 as the plotted dataset
# Convert dates to YYYY-MM-DD if any date columns exist
format_dates_for_csv <- function(df) {
  df2 <- df
  for (nm in names(df2)) {
    if (inherits(df2[[nm]], "Date")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%d")
    } else if (inherits(df2[[nm]], "POSIXt")) {
      df2[[nm]] <- format(df2[[nm]], "%Y-%m-%dT%H:%M:%S")
    }
  }
  df2
}

# Write CSV of plotted data (for figure)
plotted_csv <- format_dates_for_csv(advsmax_final3)
write.csv(plotted_csv, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")
write.csv(plotted_csv, file.path(out, "SGRender.csv"), row.names = FALSE, na = "")
# also generic dataset output
write.csv(plotted_csv, file.path(out, "advsmax_final3.csv"), row.names = FALSE, na = "")

# Write range table as P052 dataset
p052_tbl <- range2 %>%
  mutate(
    column_numbers = column_numbers,
    row_numbers = row_numbers,
    count_study = count_study
  ) %>%
  select(min_base, max_base, min_aval, max_aval, min, max, column_numbers, row_numbers, count_study)

write.csv(format_dates_for_csv(p052_tbl), file.path(out, "P052.csv"), row.names = FALSE, na = "")
write.csv(format_dates_for_csv(p052_tbl), file.path(out, "P052.lst.csv"), row.names = FALSE, na = "")
write.csv(format_dates_for_csv(range_tbl), file.path(out, "range.csv"), row.names = FALSE, na = "")
write.csv(format_dates_for_csv(range2), file.path(out, "range2.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Text rendering for P052.lst (4 numbers)
# SAS listing would show the 4 range numbers; reproduce with titles
lst_lines <- c(
  "P052 - Outliers Scatter Mean Time",
  "==================================",
  "",
  sprintf("Source: %s", advsmax_path),
  sprintf("Filter: PARAMCD=\"DIABP\" and ATPTN=815 and ANL01FL=\"Y\" and ADY>1 and SAFFL=\"Y\""),
  "",
  "Range of Baseline and Post-baseline Maximum Measurements",
  "---------------------------------------------------------",
  sprintf("min_base = %s", format(range_tbl$min_base, nsmall = 1)),
  sprintf("max_base = %s", format(range_tbl$max_base, nsmall = 1)),
  sprintf("min_aval = %s", format(range_tbl$min_aval, nsmall = 1)),
  sprintf("max_aval = %s", format(range_tbl$max_aval, nsmall = 1)),
  "",
  sprintf("Equated axis min = %s", format(min_val, nsmall = 1)),
  sprintf("Equated axis max = %s", format(max_val, nsmall = 1)),
  "",
  sprintf("Distinct STUDYID count = %d", count_study),
  sprintf("Panel layout: rows = %d  columns = %d", row_numbers, column_numbers),
  "",
  "Note: Axis equated to min(min_base,min_aval) to max(max_base,max_aval) with integer ticks.",
  ""
)

# Write txt files
writeLines(lst_lines, file.path(out, "P052.txt"))
writeLines(lst_lines, file.path(out, "P052.lst.txt"))
writeLines(lst_lines, file.path(out, "P052.lst"))

# Also create a more tabular txt version
tab_txt <- capture.output({
  cat("P052 - Range Table\n")
  print(as.data.frame(p052_tbl), row.names = FALSE)
})
writeLines(tab_txt, file.path(out, "P052_table.txt"))

# ------------------------------------------------------------------
# Figure: SGRender.png
# Template: datapanel classvars=(studyid) rows=row_numbers columns=column_numbers
# rowaxisopts label="Maximum Post-baseline Measurment" viewmin=min viewmax=max integer=true
# columnaxisopts label="Maximum Baseline Measurment" same
# scatterplot x=base y=aval group=trta, lineparm slope 1, referenceline x=ANRLO/ANRHI and y=ANRLO/ANRHI

# Prepare reference lines data: distinct per STUDYID
ref_lines <- advsmax_final3 %>%
  group_by(STUDYID) %>%
  summarise(
    ANRLO = first(ANRLO),
    ANRHI = first(ANRHI),
    .groups = "drop"
  )

# Determine facet layout
n_study <- n_distinct(advsmax_final3$STUDYID)
# Use facet_wrap with nrow/ncol as per SAS
# SAS order=rowmajor rows=row_numbers columns=column_numbers

p <- ggplot(advsmax_final3, aes(x = BASE, y = AVAL, colour = TRTA)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, colour = "grey40", linewidth = 0.6) +
  # reference lines per facet: use geom_vline/hline with data=ref_lines
  geom_vline(data = ref_lines, aes(xintercept = ANRLO), linetype = "dashed", colour = "red", linewidth = 0.5, inherit.aes = FALSE) +
  geom_vline(data = ref_lines, aes(xintercept = ANRHI), linetype = "dashed", colour = "red", linewidth = 0.5, inherit.aes = FALSE) +
  geom_hline(data = ref_lines, aes(yintercept = ANRLO), linetype = "dashed", colour = "red", linewidth = 0.5, inherit.aes = FALSE) +
  geom_hline(data = ref_lines, aes(yintercept = ANRHI), linetype = "dashed", colour = "red", linewidth = 0.5, inherit.aes = FALSE) +
  facet_wrap(~ STUDYID, nrow = row_numbers, ncol = column_numbers) +
  scale_x_continuous(
    name = "Maximum Baseline Measurment",
    limits = c(min_val, max_val),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  scale_y_continuous(
    name = "Maximum Post-baseline Measurment",
    limits = c(min_val, max_val),
    breaks = scales::pretty_breaks(n = 6)
  ) +
  coord_equal() +
  labs(
    title = "Shift Plot: Maximum Baseline vs Maximum Post-baseline (DIABP, ATPTN=815)",
    colour = "Treatment",
    caption = "Reference lines: ANRLO (LLN) and ANRHI (ULN); diagonal line y=x"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90")
  )

# Add LLN/ULN labels at min position (curvelabelposition=min)
# Annotate per facet at x=min_val or y=min_val
# Use geom_text for LLN/ULN on axes
# For simplicity, add text annotations at the reference lines near min
label_data_x <- ref_lines %>%
  transmute(STUDYID, x = ANRLO, y = min_val, label = "LLN") %>%
  bind_rows(ref_lines %>% transmute(STUDYID, x = ANRHI, y = min_val, label = "ULN"))
label_data_y <- ref_lines %>%
  transmute(STUDYID, x = min_val, y = ANRLO, label = "LLN") %>%
  bind_rows(ref_lines %>% transmute(STUDYID, x = min_val, y = ANRHI, label = "ULN"))

p <- p +
  geom_text(data = label_data_x, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 3, vjust = -0.5, colour = "red") +
  geom_text(data = label_data_y, aes(x = x, y = y, label = label), inherit.aes = FALSE, size = 3, angle = 90, hjust = -0.2, colour = "red")

# Save PNG
# Determine size based on panel layout
png_width <- ifelse(column_numbers == 2, 10, 8)
png_height <- ifelse(row_numbers == 2, 8, 5)

ggsave(file.path(out, "sgrender.png"), plot = p, width = png_width, height = png_height, dpi = 300, units = "in")
ggsave(file.path(out, "SGRender.png"), plot = p, width = png_width, height = png_height, dpi = 300, units = "in")
# also generic name
ggsave(file.path(out, "figure.png"), plot = p, width = png_width, height = png_height, dpi = 300, units = "in")

# Ensure at least one device closed
invisible(dev.off)

# Stopifnot checks
stopifnot(file.exists(file.path(out, "sgrender.png")))
stopifnot(nrow(advsmax_final3) > 0)
stopifnot(all(c("BASE","AVAL","STUDYID","TRTA") %in% names(advsmax_final3)))
```