# ------------------------------------------------------------
# R translation of SAS program P036_target16.sas
# ------------------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# Helper: round half away from zero (SAS default)
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  res <- ifelse(pos,
                floor(x * factor + 0.5) / factor,
                ceiling(x * factor - 0.5) / factor)
  return(res)
}

# Helper: format numbers with commas (no scientific)
fmt_comma <- function(x) {
  formatC(x, format = "f", big.mark = ",", digits = 0, drop0trailing = TRUE)
}

# Helper: write a plain‑text rendering of a data frame
write_txt_table <- function(df, file) {
  con <- file(file, open = "wt")
  writeLines(paste(colnames(df), collapse = "\t"), con)
  apply(df, 1, function(row) {
    writeLines(paste(row, collapse = "\t"), con)
  })
  close(con)
}

# ------------------------------------------------------------
# Set up paths
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ------------------------------------------------------------
# 1. Read source XPT and create advsmax2
advsmax <- read_xpt(file.path(rawadam, "advsmax.xpt"))

advsmax2 <- advsmax %>%
  filter(
    PARAMCD == "DIABP",
    ATPTN == 815,
    ANL01FL == "Y",
    ADY > 1,
    SAFFL == "Y"
  ) %>%
  # keep only required columns (add any that may be missing)
  select(
    USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL,
    BNRIND, ANRIND, SHIFT1, CRIT1FL,
    ANRLO, ANRHI, BASE, Ref = BASE
  ) %>%
  mutate(
    Ref = if_else(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)
  )

# ------------------------------------------------------------
# 2. Baseline shift frequency table (TableA)

# Ensure missing values are treated as a separate level (blank string)
advsmax2 <- advsmax2 %>%
  mutate(
    BNRIND = if_else(is.na(BNRIND), "", BNRIND),
    ANRIND = if_else(is.na(ANRIND), "", ANRIND),
    TRTPN  = if_else(is.na(TRTPN),  .Machine$integer.max, TRTPN) # keep numeric NA as is for grouping
  )

# Cross‑tab BNRIND*ANRIND by TRTPN
cross_tab <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(TRTPN) %>%
  mutate(Percent = 100 * Count / sum(Count)) %>%
  ungroup()

# Create formatted ValueFmt
cross_tab <- cross_tab %>%
  mutate(
    ValueFmt = paste0(
      fmt_comma(Count), " (",
      formatC(round_half_up(Percent, 1), format = "f", digits = 1), "%)"
    )
  )

# Group totals (TRTPN alone)
group_tot <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(GroupN = n(), .groups = "drop") %>%
  mutate(GroupFmt = paste0(TRTPN, " (N = ", fmt_comma(GroupN), ")"))

# Merge group info
cross_tab <- cross_tab %>%
  left_join(group_tot %>% select(TRTPN, GroupN, GroupFmt), by = "TRTPN")

# Pivot to wide format (PROC TRANSPOSE equivalent)
TableA_wide <- cross_tab %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(
    names_from = ANRIND,
    values_from = ValueFmt,
    names_prefix = "ValueFmt_"
  ) %>%
  arrange(GroupFmt, BNRIND)

# ------------------------------------------------------------
# 3. Emergent high table

# Keep rows where CRIT1FL is not missing/blank
advsmax3 <- advsmax2 %>%
  filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  mutate(row_id = row_number(),
         n = as.integer((row_id %% 17) == 1)) %>%
  select(-row_id)

# Summary per TRTPN
EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(
    Nx = n(),
    n = sum(n),
    percent = mean(n),
    .groups = "drop"
  ) %>%
  mutate(percent = 100 * percent)

# Fisher's exact test (2×2) for n vs TRTPN
# Build contingency table
cont_tab <- table(advsmax3$n, advsmax3$TRTPN)
fisher_res <- fisher.test(cont_tab)
FishersP <- formatC(fisher_res$p.value, format = "f", digits = 4)

EmergentHigh <- EmergentHigh %>%
  mutate(FishersP = FishersP)

# ------------------------------------------------------------
# 4. Graph data and limits

summary_vals <- advsmax2 %>%
  summarise(
    mean_ANRLO = mean(ANRLO, na.rm = TRUE),
    mean_ANRHI = mean(ANRHI, na.rm = TRUE),
    mean_BASE  = mean(BASE,  na.rm = TRUE),
    mean_AVAL  = mean(AVAL,  na.rm = TRUE),
    MinValue   = min(AVAL, na.rm = TRUE),
    MaxValue   = max(AVAL, na.rm = TRUE)
  )

MAJOR <- 10
margin <- (summary_vals$MaxValue - summary_vals$MinValue) / 100
YAxisMin <- MAJOR * floor((summary_vals$MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((summary_vals$MaxValue + margin) / MAJOR)

# ------------------------------------------------------------
# 5. Produce outputs

# 5a. Baseline shift table (text & csv)
write.csv(TableA_wide,
          file.path(out_dir, "BaselineShiftTable.csv"),
          row.names = FALSE, na = "")
write_txt_table(TableA_wide,
                file.path(out_dir, "BaselineShiftTable.txt"))

# 5b. Emergent high table (text & csv)
write.csv(EmergentHigh,
          file.path(out_dir, "EmergentHigh.csv"),
          row.names = FALSE, na = "")
write_txt_table(EmergentHigh,
                file.path(out_dir, "EmergentHigh.txt"))

# 5c. Combined listing (P036.lst) – concatenate both tables
lst_file_txt <- file.path(out_dir, "P036.lst.txt")
con <- file(lst_file_txt, open = "wt")
writeLines("Baseline Shift Table", con)
write.table(TableA_wide, con, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
writeLines("\nEmergent High Table", con)
write.table(EmergentHigh, con, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
close(con)

# also CSV version of the combined listing (optional)
write.csv(
  bind_rows(
    mutate(TableA_wide, Section = "BaselineShift"),
    mutate(EmergentHigh, Section = "EmergentHigh")
  ),
  file.path(out_dir, "P036.lst.csv"),
  row.names = FALSE, na = ""
)

# 5d. Scatter plot (PNG) and plotted data CSV
plot_data <- advsmax2 %>%
  select(USUBJID, TRTPN, BASE, AVAL, Ref)

gg <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = factor(TRTPN), shape = factor(TRTPN))) +
  geom_point(size = 2) +
  geom_line(aes(y = Ref), linetype = "dashed", colour = "black") +
  geom_hline(yintercept = summary_vals$mean_ANRLO, colour = "blue", linetype = "dotted") +
  geom_hline(yintercept = summary_vals$mean_ANRHI, colour = "red",  linetype = "dotted") +
  scale_y_continuous(
    limits = c(YAxisMin, YAxisMax),
    breaks = seq(YAxisMin, YAxisMax, by = MAJOR)
  ) +
  labs(
    title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
    x = "Max. Baseline",
    y = "Max. Post-baseline",
    colour = "TRTPN",
    shape = "TRTPN"
  ) +
  theme_minimal(base_size = 10)

png_file <- file.path(out_dir, "sasgraph.png")
ggsave(filename = png_file, plot = gg, width = 6, height = 6, units = "in", dpi = 150)

# CSV of plotted data
write.csv(plot_data,
          file.path(out_dir, "sasgraph_data.csv"),
          row.names = FALSE, na = "")

# 5e. Target16 text rendering (tables only – graph is separate PNG)
target_txt <- file.path(out_dir, "Target16.txt")
con2 <- file(target_txt, open = "wt")
writeLines("Baseline Shift Table (as in Target16)", con2)
write.table(TableA_wide, con2, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
writeLines("\nEmergent High Table (as in Target16)", con2)
write.table(EmergentHigh, con2, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
close(con2)

# CSV version of Target16 tables
write.csv(
  bind_rows(
    mutate(TableA_wide, Section = "BaselineShift"),
    mutate(EmergentHigh, Section = "EmergentHigh")
  ),
  file.path(out_dir, "Target16.csv"),
  row.names = FALSE, na = ""
)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
