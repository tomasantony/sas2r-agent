# ------------------------------------------------------------
# R translation of SAS program P036_target16.sas
# ------------------------------------------------------------
# Load required packages (already installed in the environment)
library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------
# SAS uses "round half away from zero"
round_half_up <- function(x, digits = 0) {
  pos <- x >= 0
  factor <- 10 ^ digits
  ifelse(pos,
         floor(x * factor + 0.5) / factor,
         ceiling(x * factor - 0.5) / factor)
}

# Comma formatting without decimals (e.g., 1,234)
fmt_comma <- function(x) {
  formatC(x, format = "f", big.mark = ",", digits = 0, drop0trailing = TRUE)
}

# Write a plain‑text rendering of a data frame (tab‑separated)
write_txt_table <- function(df, file) {
  write.table(df,
              file = file,
              sep = "\t",
              row.names = FALSE,
              col.names = TRUE,
              quote = FALSE,
              na = "")
}

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ------------------------------------------------------------
# 1. Read source XPT and create advsmax2
# ------------------------------------------------------------
advsmax <- read_xpt(file.path(rawadam, "advsmax.xpt"))

advsmax2 <- advsmax %>%
  filter(
    PARAMCD == "DIABP",
    ATPTN == 815,
    ANL01FL == "Y",
    ADY > 1,
    SAFFL == "Y"
  ) %>%
  # Keep exactly the variables referenced later
  select(
    USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL,
    BNRIND, ANRIND, SHIFT1, CRIT1FL,
    ANRLO, ANRHI, BASE
  ) %>%
  mutate(
    # Create Ref = Base when both Base and AVAL are non‑missing (as in SAS)
    Ref = if_else(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)
  )

# ------------------------------------------------------------
# 2. Baseline shift frequency table (TableA)
# ------------------------------------------------------------
# Treat missing as a distinct level (blank string) for character vars
advsmax2 <- advsmax2 %>%
  mutate(
    BNRIND = if_else(is.na(BNRIND), "", BNRIND),
    ANRIND = if_else(is.na(ANRIND), "", ANRIND)
  )

# Cross‑tabulation BNRIND * ANRIND by TRTPN
cross_tab <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(TRTPN) %>%
  mutate(Percent = 100 * Count / sum(Count)) %>%
  ungroup()

# Formatted ValueFmt (e.g., "1,234 ( 31.6%)")
cross_tab <- cross_tab %>%
  mutate(
    ValueFmt = paste0(
      fmt_comma(Count), " (",
      formatC(round_half_up(Percent, 1), format = "f", digits = 1), "%)"
    )
  )

# Group totals for TRTPN (used for GroupFmt)
group_tot <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(GroupN = n(), .groups = "drop") %>%
  mutate(GroupFmt = paste0(TRTPN, " (N = ", fmt_comma(GroupN), ")"))

# Merge group info into cross_tab
cross_tab <- cross_tab %>%
  left_join(group_tot %>% select(TRTPN, GroupN, GroupFmt), by = "TRTPN")

# PROC TRANSPOSE equivalent: wide format with ANRIND as columns
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
# ------------------------------------------------------------
# Keep rows where CRIT1FL is not missing/blank
advsmax3 <- advsmax2 %>%
  filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  mutate(
    row_n = row_number(),
    n = as.integer((row_n %% 17) == 1)   # same logic as SAS: mod(_n_,17)=1
  ) %>%
  select(-row_n)

# Summary per TRTPN
EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(
    Nx = n(),                     # total rows for the treatment
    n  = sum(n),                  # sum of the indicator
    .groups = "drop"
  ) %>%
  mutate(
    percent = ifelse(Nx > 0, 100 * n / Nx, NA_real_)
  )

# Fisher's exact test for n vs TRTPN (2×2 table)
cont_tab <- table(advsmax3$n, advsmax3$TRTPN)
fisher_res <- fisher.test(cont_tab)
FishersP <- formatC(fisher_res$p.value, format = "f", digits = 4)

EmergentHigh <- EmergentHigh %>%
  mutate(FishersP = FishersP)

# ------------------------------------------------------------
# 4. Graph data and axis limits
# ------------------------------------------------------------
summary_vals <- advsmax2 %>%
  summarise(
    MinValue = min(AVAL, na.rm = TRUE),
    MaxValue = max(AVAL, na.rm = TRUE),
    ANRLO    = mean(ANRLO, na.rm = TRUE),   # reference limits (used for reference lines)
    ANRHI    = mean(ANRHI, na.rm = TRUE)
  )

MAJOR <- 10
margin <- (summary_vals$MaxValue - summary_vals$MinValue) / 100
YAxisMin <- MAJOR * floor((summary_vals$MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((summary_vals$MaxValue + margin) / MAJOR)

# ------------------------------------------------------------
# 5. Write outputs
# ------------------------------------------------------------

# 5a. Baseline shift table (CSV & TXT)
write.csv(TableA_wide,
          file.path(out_dir, "BaselineShiftTable.csv"),
          row.names = FALSE, na = "")
write_txt_table(TableA_wide,
                file.path(out_dir, "BaselineShiftTable.txt"))

# 5b. Emergent high table (CSV & TXT)
write.csv(EmergentHigh,
          file.path(out_dir, "EmergentHigh.csv"),
          row.names = FALSE, na = "")
write_txt_table(EmergentHigh,
                file.path(out_dir, "EmergentHigh.txt"))

# 5c. Combined listing P036.lst (text & csv)
lst_txt_path <- file.path(out_dir, "P036.lst.txt")
con_lst <- file(lst_txt_path, open = "wt")
writeLines("Baseline Shift Table", con_lst)
write.table(TableA_wide, con_lst, sep = "\t", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")
writeLines("\nEmergent High Table", con_lst)
write.table(EmergentHigh, con_lst, sep = "\t", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")
close(con_lst)

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

png_path <- file.path(out_dir, "sasgraph.png")
gg <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = factor(TRTPN), shape = factor(TRTPN))) +
  geom_point(size = 2) +
  # Reference lines (horizontal) at ANRLO and ANRHI
  geom_hline(yintercept = summary_vals$ANRLO, colour = "blue", linetype = "dotted") +
  geom_hline(yintercept = summary_vals$ANRHI, colour = "red",  linetype = "dotted") +
  # Overlay Ref (baseline) as a dashed line per subject
  geom_line(aes(y = Ref), linetype = "dashed", colour = "black") +
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

ggsave(filename = png_path, plot = gg, width = 6, height = 6, units = "in", dpi = 150)

# CSV of plotted data
write.csv(plot_data,
          file.path(out_dir, "sasgraph_data.csv"),
          row.names = FALSE, na = "")

# 5e. Target16 listing (text & csv) – contains the same two tables
target_txt_path <- file.path(out_dir, "Target16.txt")
con_tgt <- file(target_txt_path, open = "wt")
writeLines("Baseline Shift Table (Target16)", con_tgt)
write.table(TableA_wide, con_tgt, sep = "\t", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")
writeLines("\nEmergent High Table (Target16)", con_tgt)
write.table(EmergentHigh, con_tgt, sep = "\t", row.names = FALSE,
            col.names = TRUE, quote = FALSE, na = "")
close(con_tgt)

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
