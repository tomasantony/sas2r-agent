# P053_outliers_Scatter_Quant.R
# Reimplementation of SAS program P053 in R
# -------------------------------------------------
# Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Helper: round half up (SAS style)
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor  <- 10 ^ digits
  ((floor(absx * factor + 0.5)) / factor) * posneg
}

# Helper: format count with commas
fmt_count <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

# Helper: format percent with one decimal (SAS 10.1)
fmt_percent <- function(x) {
  sprintf("%.1f", round_half_up(x, 1))
}

# -------------------------------------------------
# Set up paths
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
source_xpt <- file.path(rawadam, "advsmax.xpt")

# -------------------------------------------------
# 1. Read source data
advsmax <- read_xpt(source_xpt)

# -------------------------------------------------
# 2. Create advsmax2
advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         ADY > 1,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL,
         ANL01FL, BNRIND, ANRIND, SHIFT1, CRIT1FL,
         ANRLO, ANRHI, BASE) %>%
  mutate(Ref = if_else(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_))

# -------------------------------------------------
# 3. Baseline shift frequency table (TableA)

# Ensure missing categories are kept as "." (SAS missing representation)
advsmax2 <- advsmax2 %>%
  mutate(BNRIND = if_else(is.na(BNRIND), ".", as.character(BNRIND)),
         ANRIND = if_else(is.na(ANRIND), ".", as.character(ANRIND)),
         TRTPN  = as.character(TRTPN))

# Total per treatment
tot_trt <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(GroupN = n(), .groups = "drop")

# Counts per BNRIND*ANRIND per treatment
freq_tbl <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(Count = n(), .groups = "drop") %>%
  left_join(tot_trt, by = "TRTPN") %>%
  mutate(Percent = (Count / GroupN) * 100,
         ValueFmt = paste0(fmt_count(Count), " (", fmt_percent(Percent), "%)"))

# Build GroupFmt
group_fmt <- tot_trt %>%
  mutate(GroupFmt = paste0(TRTPN, " (N = ", fmt_count(GroupN), ")")) %>%
  select(TRTPN, GroupFmt)

# Merge GroupFmt into frequency table
freq_tbl <- freq_tbl %>%
  left_join(group_fmt, by = "TRTPN") %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt)

# Transpose so ANRIND values become columns
tableA_wide <- freq_tbl %>%
  pivot_wider(names_from = ANRIND,
              values_from = ValueFmt,
              names_prefix = "ValueFmt") %>%
  arrange(GroupFmt, BNRIND)

# -------------------------------------------------
# 4. Write TableA outputs
tableA_csv_path <- file.path(out_dir, "P053.csv")
write.csv(tableA_wide, tableA_csv_path, row.names = FALSE, na = "")

tableA_txt_path <- file.path(out_dir, "P053.txt")
txt_con <- file(tableA_txt_path, open = "wt")
writeLines("Baseline Shift Frequency Table (TableA)", txt_con)
write.table(tableA_wide, txt_con, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
close(txt_con)

# -------------------------------------------------
# 5. Emergent High calculations

# advsmax3: keep rows where CRIT1FL not missing, then flag every 17th observation
advsmax3 <- advsmax2 %>%
  filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  mutate(rownum = row_number(),
         n = (rownum %% 17) == 1)

# Summary per treatment
emergent_sum <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(Nx = n(),
            n = sum(n),
            percent = mean(n), .groups = "drop") %>%
  mutate(percent = round_half_up(percent * 100, 1))   # convert to percent

# Fisher's Exact test (2x2 or Rx2)
cont_tab <- table(advsmax3$TRTPN, advsmax3$n)
fish_p <- tryCatch({
  fisher.test(cont_tab)$p.value
}, error = function(e) NA_real_)

pvalue_df <- data.frame(FishersP = fish_p)

# Merge
emergent_high <- emergent_sum %>%
  bind_cols(pvalue_df)

# -------------------------------------------------
# 6. Write Emergent High outputs (Target16)
emergent_csv_path <- file.path(out_dir, "Target16.csv")
write.csv(emergent_high, emergent_csv_path, row.names = FALSE, na = "")

emergent_txt_path <- file.path(out_dir, "Target16.txt")
txt_con2 <- file(emergent_txt_path, open = "wt")
writeLines("Emergent High Table", txt_con2)
write.table(emergent_high, txt_con2, sep = "\t", row.names = FALSE, quote = FALSE, na = "")
close(txt_con2)

# -------------------------------------------------
# 7. Graph preparation

# Summary stats for axis limits
adv_sum <- advsmax2 %>%
  summarise(ANRLO = mean(ANRLO, na.rm = TRUE),
            ANRHI = mean(ANRHI, na.rm = TRUE),
            MinValue = min(AVAL, na.rm = TRUE),
            MaxValue = max(AVAL, na.rm = TRUE))

MAJOR <- 10
margin <- (adv_sum$MaxValue - adv_sum$MinValue) / 100
YAxisMin <- MAJOR * floor((adv_sum$MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((adv_sum$MaxValue + margin) / MAJOR)

# Plot data
plot_data <- advsmax2 %>%
  mutate(TRTPN = factor(TRTPN),
         Ref = if_else(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_))

p <- ggplot(plot_data, aes(x = BASE, y = AVAL, color = TRTPN, shape = TRTPN)) +
  geom_point(size = 2) +
  # reference limits (horizontal)
  geom_hline(yintercept = adv_sum$ANRLO, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = adv_sum$ANRHI, linetype = "dashed", color = "red") +
  # reference limits (vertical)
  geom_vline(xintercept = adv_sum$ANRLO, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = adv_sum$ANRHI, linetype = "dashed", color = "red") +
  # diagonal line Ref = Base (45 degree)
  geom_line(aes(y = Ref), linetype = "solid", color = "black", size = 0.5) +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  scale_x_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline",
       y = "Max. Post baseline",
       color = "Treatment",
       shape = "Treatment") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# Save plot
graph_png_path <- file.path(out_dir, "sasgraph.png")
ggsave(filename = graph_png_path, plot = p, width = 6, height = 6, dpi = 300)

# Save plotted data used for the graph
graph_data_path <- file.path(out_dir, "sasgraph.csv")
write.csv(plot_data %>% select(USUBJID, TRTPN, BASE, AVAL, Ref),
          graph_data_path, row.names = FALSE, na = "")

# -------------------------------------------------
# End of script
