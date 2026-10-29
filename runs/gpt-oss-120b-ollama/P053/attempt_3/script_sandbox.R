# P053_outliers_Scatter_Quant.R
# -------------------------------------------------
# Reimplementation of SAS program P053 in R
# -------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# -------------------------------------------------
# Helper: SAS style round half up
round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  absx   <- abs(x)
  factor  <- 10 ^ digits
  ((floor(absx * factor + 0.5)) / factor) * posneg
}

# Helper: format count with commas (SAS COMMA8.)
fmt_count <- function(x) {
  formatC(x, format = "d", big.mark = ",")
}

# Helper: format percent with one decimal (SAS 10.1)
fmt_percent <- function(x) {
  sprintf("%.1f", round_half_up(x, 1))
}

# -------------------------------------------------
# Paths
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR not set")
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

# Keep missing as NA (SAS treats missing as blank, displayed as ".")
advsmax2 <- advsmax2 %>%
  mutate(
    BNRIND = as.character(BNRIND),
    ANRIND = as.character(ANRIND),
    TRTPN  = as.character(TRTPN)
  )

# Levels (including NA)
bnr_levels <- sort(unique(advsmax2$BNRIND))
anr_levels <- sort(unique(advsmax2$ANRIND))
trt_levels <- sort(unique(advsmax2$TRTPN))

# Total per treatment
group_tot <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(GroupN = n(), .groups = "drop")

# Full grid to ensure zero‑count rows
full_grid <- expand_grid(
  TRTPN = trt_levels,
  BNRIND = bnr_levels,
  ANRIND = anr_levels
)

# Count per combination
cnt_tbl <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(Count = n(), .groups = "drop") %>%
  right_join(full_grid, by = c("TRTPN", "BNRIND", "ANRIND")) %>%
  replace_na(list(Count = 0)) %>%
  left_join(group_tot, by = "TRTPN") %>%
  mutate(
    Percent = if_else(GroupN == 0, 0, (Count / GroupN) * 100)
  ) %>%
  left_join(
    group_tot %>%
      mutate(GroupFmt = paste0(TRTPN, " (N = ", fmt_count(GroupN), ")")) %>%
      select(TRTPN, GroupFmt),
    by = "TRTPN"
  ) %>%
  select(GroupFmt, BNRIND, ANRIND, Count, Percent)

# -------------------------------------------------
# 4. Build PROC REPORT‑style table (separate count & percent columns)

# Order ANRIND levels as they appear (character sort, NA first)
anr_levels_ordered <- sort(anr_levels, na.last = FALSE)

# Pivot counts
count_wide <- cnt_tbl %>%
  select(GroupFmt, BNRIND, ANRIND, Count) %>%
  pivot_wider(names_from = ANRIND,
              values_from = Count,
              names_prefix = "cnt_") %>%
  arrange(GroupFmt, BNRIND)

# Pivot percents (formatted with one decimal)
percent_wide <- cnt_tbl %>%
  mutate(PercentFmt = fmt_percent(Percent)) %>%
  select(GroupFmt, BNRIND, ANRIND, PercentFmt) %>%
  pivot_wider(names_from = ANRIND,
              values_from = PercentFmt,
              names_prefix = "pct_") %>%
  arrange(GroupFmt, BNRIND)

# Combine side‑by‑side, interleaving count and percent columns per ANRIND
report_tbl <- count_wide
for (lvl in anr_levels_ordered) {
  cnt_col <- paste0("cnt_", lvl)
  pct_col <- paste0("pct_", lvl)
  # If a level is NA, column names become "cnt_NA" etc; replace with "." for display later
  report_tbl[[cnt_col]] <- count_wide[[cnt_col]]
  report_tbl[[pct_col]] <- percent_wide[[pct_col]]
}
# Replace NA in BNRIND and ANRIND display with "."
report_tbl <- report_tbl %>%
  mutate(BNRIND = if_else(is.na(BNRIND), ".", BNRIND))

# -------------------------------------------------
# 5. Write TableA outputs (CSV and plain text)
write.csv(report_tbl,
          file.path(out_dir, "P053.csv"),
          row.names = FALSE, na = "")

write.table(report_tbl,
            file = file.path(out_dir, "P053.txt"),
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            na = "")

# -------------------------------------------------
# 6. Emergent High calculations (Target16)

# advsmax3: keep rows where CRIT1FL not missing, then flag every 17th observation
advsmax3 <- advsmax2 %>%
  filter(!is.na(CRIT1FL) & CRIT1FL != "") %>%
  mutate(rownum = row_number(),
         n = (rownum %% 17) == 1)

# Summary per treatment
emergent_sum <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(
    Nx = n(),
    n = sum(n),
    percent = round_half_up(mean(n) * 100, 1),
    .groups = "drop"
  )

# Fisher's exact test (overall 2‑by‑R table)
cont_tab <- table(advsmax3$TRTPN, advsmax3$n)
fish_p <- tryCatch(fisher.test(cont_tab)$p.value, error = function(e) NA_real_)

# Add Fisher's p‑value (same for all rows)
emergent_high <- emergent_sum %>%
  mutate(FishersP = fish_p)

# -------------------------------------------------
# 7. Write Emergent High outputs (Target16)

write.csv(emergent_high,
          file.path(out_dir, "Target16.csv"),
          row.names = FALSE, na = "")

# For the text version, format numbers as SAS would display
emergent_txt <- emergent_high %>%
  mutate(
    Nx = fmt_count(Nx),
    n  = fmt_count(n),
    percent = fmt_percent(percent),
    FishersP = ifelse(is.na(FishersP), "", formatC(FishersP, format = "f", digits = 4))
  )

write.table(emergent_txt,
            file = file.path(out_dir, "Target16.txt"),
            sep = "\t",
            row.names = FALSE,
            col.names = TRUE,
            quote = FALSE,
            na = "")

# -------------------------------------------------
# 8. Graph preparation

# Summary stats for axis limits
adv_sum <- advsmax2 %>%
  summarise(
    ANRLO = mean(ANRLO, na.rm = TRUE),
    ANRHI = mean(ANRHI, na.rm = TRUE),
    MinValue = min(AVAL, na.rm = TRUE),
    MaxValue = max(AVAL, na.rm = TRUE)
  )

MAJOR <- 10
margin <- (adv_sum$MaxValue - adv_sum$MinValue) / 100
YAxisMin <- MAJOR * floor((adv_sum$MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((adv_sum$MaxValue + margin) / MAJOR)

# Plot data (include Ref line)
plot_data <- advsmax2 %>%
  mutate(
    TRTPN = factor(TRTPN),
    Ref = if_else(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)
  )

p <- ggplot(plot_data, aes(x = BASE, y = AVAL, colour = TRTPN, shape = TRTPN)) +
  geom_point(size = 2) +
  geom_hline(yintercept = adv_sum$ANRLO, linetype = "dashed", colour = "blue") +
  geom_hline(yintercept = adv_sum$ANRHI, linetype = "dashed", colour = "red") +
  geom_vline(xintercept = adv_sum$ANRLO, linetype = "dashed", colour = "blue") +
  geom_vline(xintercept = adv_sum$ANRHI, linetype = "dashed", colour = "red") +
  geom_line(aes(y = Ref), linetype = "solid", colour = "black", size = 0.5) +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax),
                     breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  scale_x_continuous(limits = c(YAxisMin, YAxisMax),
                     breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(
    title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
    x = "Max. Baseline",
    y = "Max. Post baseline",
    colour = "Treatment",
    shape = "Treatment"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

ggsave(filename = file.path(out_dir, "sasgraph.png"),
       plot = p,
       width = 6, height = 6, dpi = 300)

# Save plotted data used for the graph
write.csv(
  plot_data %>% select(USUBJID, TRTPN, BASE, AVAL, Ref),
  file.path(out_dir, "sasgraph.csv"),
  row.names = FALSE, na = ""
)

# -------------------------------------------------
# End of script
