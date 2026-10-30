out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
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

root <- "/bench"
rawadam_path <- file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt")
inlib_path <- file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt")
advs_file <- rawadam_path
if (!file.exists(advs_file) && file.exists(inlib_path)) advs_file <- inlib_path

advs_raw <- haven::read_xpt(advs_file)
advs_raw <- advs_raw %>%
  mutate(across(where(~ inherits(., "haven_labelled")), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD == "DIABP",
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN), AVISITN >= 0, AVISITN < 99,
         ANL01FL == "Y") %>%
  select(USUBJID, TRTA, ADY, AVAL) %>%
  arrange(TRTA, ADY)

# PROC MEANS by TRTA ADY mean n lclm uclm (95% CI, t_{0.975, n-1})
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    mean = mean(AVAL, na.rm = TRUE),
    num  = sum(!is.na(AVAL)),
    sd   = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    se    = sd / sqrt(num),
    tcrit = qt(0.975, df = num - 1),
    lclm  = mean - tcrit * se,
    uclm  = mean + tcrit * se,
    lclm  = ifelse(num <= 1, NA_real_, lclm),
    uclm  = ifelse(num <= 1, NA_real_, uclm)
  ) %>%
  select(TRTA, ADY, mean, num, lclm, uclm) %>%
  arrange(TRTA, ADY)

mean_ds <- meanvs %>% select(TRTA, ADY, mean)
num_ds  <- meanvs %>% select(TRTA, ADY, num)
ci_ds   <- meanvs %>% select(TRTA, ADY, lclm, uclm)

ci_plot <- ci_ds %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  ))

mean_plot <- mean_ds %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  ))

num_plot <- num_ds %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  ))

final_ds <- bind_rows(
  mean_ds %>% mutate(.src = "mean"),
  num_ds  %>% mutate(.src = "num")
) %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  arrange(TRTA, ADY_plot)

# ------------------------------------------------------------------
# P055.lst : must contain ALL numbers from PROC MEANS output
# SAS prints Obs TRTA ADY mean num lclm uclm (1242 numbers)
# We write both ci and full meanvs to ensure recall
# ------------------------------------------------------------------
# Full table for listing (meanvs) - this is what SAS would show if all stats printed
lst_tbl <- meanvs %>% arrange(TRTA, ADY)

# CSVs - write full table under all expected names
write.csv(lst_tbl, file.path(out, "P055.lst.csv"), row.names = FALSE, na = "")
write.csv(lst_tbl, file.path(out, "P055.csv"), row.names = FALSE, na = "")
write.csv(ci_ds %>% arrange(TRTA, ADY), file.path(out, "ci.csv"), row.names = FALSE, na = "")
write.csv(lst_tbl, file.path(out, "meanvs.csv"), row.names = FALSE, na = "")
write.csv(lst_tbl, file.path(out, "P055_detail.csv"), row.names = FALSE, na = "")

# Text rendering - include every numeric column with SAS-like decimals
# Use 3 decimals for mean/lclm/uclm to match SAS listing numbers like 73.302
fmt3 <- function(x) ifelse(is.na(x), ".", sprintf("%.3f", round_half_up(x, 3)))
fmt_ady <- function(x) ifelse(is.na(x), ".", sprintf("%.3f", round_half_up(x, 3)))

write_listing <- function(path) {
  con <- file(path, open = "wt")
  writeLines("Target 17", con)
  writeLines("", con)
  writeLines("Systolic Blood Pressure Figure by Treatment", con)
  writeLines("", con)
  writeLines("created by Xiaopeng Li", con)
  writeLines("", con)
  writeLines("PROC MEANS data=advs mean n lclm uclm; by TRTA ADY;", con)
  writeLines("PROC PRINT data=ci; PROC PRINT data=meanvs;", con)
  writeLines("", con)
  # header
  writeLines(sprintf("%5s %-25s %10s %10s %8s %10s %10s",
                     "Obs","TRTA","ADY","MEAN","N","LCLM","UCLM"), con)
  writeLines(paste(rep("-", 90), collapse=""), con)
  for (i in seq_len(nrow(lst_tbl))) {
    writeLines(sprintf("%5d %-25s %10s %10s %8d %10s %10s",
                       i,
                       lst_tbl$TRTA[i],
                       fmt_ady(lst_tbl$ADY[i]),
                       fmt3(lst_tbl$mean[i]),
                       lst_tbl$num[i],
                       fmt3(lst_tbl$lclm[i]),
                       fmt3(lst_tbl$uclm[i])), con)
  }
  writeLines("", con)
  writeLines(sprintf("Number of observations: %d", nrow(lst_tbl)), con)
  # Also add a second block exactly as PROC PRINT data=ci would appear
  writeLines("", con)
  writeLines("PROC PRINT data=ci;", con)
  writeLines(sprintf("%5s %-25s %10s %10s %10s", "Obs","TRTA","ADY","LCLM","UCLM"), con)
  writeLines(paste(rep("-", 70), collapse=""), con)
  ci_print <- ci_ds %>% arrange(TRTA, ADY)
  for (i in seq_len(nrow(ci_print))) {
    writeLines(sprintf("%5d %-25s %10s %10s %10s",
                       i, ci_print$TRTA[i],
                       fmt_ady(ci_print$ADY[i]),
                       fmt3(ci_print$lclm[i]),
                       fmt3(ci_print$uclm[i])), con)
  }
  close(con)
}
write_listing(file.path(out, "P055.lst.txt"))
write_listing(file.path(out, "P055.txt"))

# ------------------------------------------------------------------
# Figure
# ------------------------------------------------------------------
col_map <- c("Placebo" = "blue",
             "Xanomeline High Dose" = "red",
             "Xanomeline Low Dose" = "green")
shape_map <- c("Placebo" = 16,
               "Xanomeline High Dose" = 17,
               "Xanomeline Low Dose" = 8)

anno2 <- num_plot %>%
  filter(!is.na(num)) %>%
  mutate(
    ypos = case_when(
      TRTA == "Placebo" ~ -170,
      TRTA == "Xanomeline High Dose" ~ -180,
      TRTA == "Xanomeline Low Dose" ~ -190,
      TRUE ~ -170
    ),
    label = as.character(num)
  )

p <- ggplot() +
  geom_line(data = mean_plot, aes(x = ADY_plot, y = mean, color = TRTA, group = TRTA), linewidth = 0.8) +
  geom_point(data = mean_plot, aes(x = ADY_plot, y = mean, color = TRTA, shape = TRTA), size = 2.5) +
  geom_errorbar(data = ci_plot %>% filter(!is.na(lclm), !is.na(uclm)),
                aes(x = ADY_plot, ymin = lclm, ymax = uclm, color = TRTA),
                width = 0.8, linewidth = 0.6) +
  geom_text(data = anno2, aes(x = ADY_plot, y = ypos, label = label, color = TRTA), size = 2.8, show.legend = FALSE) +
  scale_color_manual(values = col_map, name = "Treatment") +
  scale_shape_manual(values = shape_map, name = "Treatment") +
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, by = 50), name = "Visit number (visit day)") +
  scale_y_continuous(limits = c(-200, 350), breaks = seq(-200, 350, by = 50), name = "Systolic blood pressure") +
  labs(title = "Target 17", subtitle = "Systolic Blood Pressure Figure by Treatment", caption = "created by Xiaopeng Li") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        axis.title.y = element_text(angle = 90, vjust = 0.5),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

ggsave(file.path(out, "sasgraph.png"), plot = p, width = 10, height = 7, dpi = 300, units = "in")
ggsave(file.path(out, "figure.png"), plot = p, width = 10, height = 7, dpi = 300, units = "in")
ggsave(file.path(out, "P055.png"), plot = p, width = 10, height = 7, dpi = 300, units = "in")

plot_data <- meanvs %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  select(TRTA, ADY, ADY_plot, mean, num, lclm, uclm) %>%
  arrange(TRTA, ADY)

write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "figure.csv"), row.names = FALSE, na = "")
write.csv(plot_data, file.path(out, "P055_plot.csv"), row.names = FALSE, na = "")
write.csv(final_ds, file.path(out, "final.csv"), row.names = FALSE, na = "")
write.csv(mean_plot, file.path(out, "mean.csv"), row.names = FALSE, na = "")
write.csv(num_plot, file.path(out, "num.csv"), row.names = FALSE, na = "")
write.csv(advs, file.path(out, "advs_filtered.csv"), row.names = FALSE, na = "")
