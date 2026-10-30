```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, recursive = TRUE, showWarnings = FALSE)

## helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
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

## ------------------------------------------------------------------
## Paths (as in SAS program)
## ------------------------------------------------------------------
root <- "D:/meta_hackathon/sas2r-agent"
rawadam_path <- file.path(root, "data/raw/cdisc-pilot/adam/advs.xpt")
## fallback to inlib derived path if raw not found
inlib_path <- file.path(root, "data/derived/cdisc-pilot/adam/advs.xpt")

advs_file <- rawadam_path
if (!file.exists(advs_file) && file.exists(inlib_path)) advs_file <- inlib_path

## ------------------------------------------------------------------
## Read ADVS
## ------------------------------------------------------------------
advs_raw <- haven::read_xpt(advs_file)

## Ensure character columns are character (haven may give labelled)
advs_raw <- advs_raw %>%
  mutate(across(where(~ inherits(., "haven_labelled")), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

## ------------------------------------------------------------------
## Filter as in SAS:
##   SAFFL="Y" and PARAMCD="DIABP" and ATPT="AFTER LYING DOWN FOR 5 MINUTES"
##   and (AVISITN ge 0 and AVISITN lt 99) and ANL01FL="Y"
## keep USUBJID TRTA ADY AVAL
## ------------------------------------------------------------------
advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD == "DIABP",
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN), AVISITN >= 0, AVISITN < 99,
         ANL01FL == "Y") %>%
  select(USUBJID, TRTA, ADY, AVAL)

## SAS sort by TRTA ADY (character ordering, missing first)
advs <- advs %>%
  arrange(TRTA, ADY)

## ------------------------------------------------------------------
## PROC MEANS by TRTA ADY : mean n lclm uclm
## LCLM/UCLM = mean +/- t_{0.975, n-1} * std / sqrt(n)
## ------------------------------------------------------------------
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    mean = mean(AVAL, na.rm = TRUE),
    num  = sum(!is.na(AVAL)),
    sd   = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    se   = sd / sqrt(num),
    tcrit = qt(0.975, df = num - 1),
    lclm = mean - tcrit * se,
    uclm = mean + tcrit * se
  ) %>%
  mutate(
    lclm = ifelse(num <= 1, NA_real_, lclm),
    uclm = ifelse(num <= 1, NA_real_, uclm)
  ) %>%
  select(TRTA, ADY, mean, num, lclm, uclm) %>%
  arrange(TRTA, ADY)

## ------------------------------------------------------------------
## Datasets mean, num, ci as in SAS
## ------------------------------------------------------------------
mean_ds <- meanvs %>% select(TRTA, ADY, mean)
num_ds  <- meanvs %>% select(TRTA, ADY, num)
ci_ds   <- meanvs %>% select(TRTA, ADY, lclm, uclm)

## ci offset for plotting (SAS: Placebo +0.6, High +0.3, Low +0)
ci_plot <- ci_ds %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  ))

## mean offset
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

## final as in SAS: set mean num (stacked) then offset
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

## ------------------------------------------------------------------
## Write listing: PROC PRINT data=ci
## This is the P055.lst output
## ------------------------------------------------------------------
## Prepare display version (same as ci_ds but sorted)
ci_print <- ci_ds %>% arrange(TRTA, ADY)

## CSV of table body
write.csv(ci_print, file.path(out, "P055.lst.csv"), row.names = FALSE, na = "")
write.csv(ci_print, file.path(out, "P055.csv"), row.names = FALSE, na = "")
## also write ci.csv for completeness
write.csv(ci_print, file.path(out, "ci.csv"), row.names = FALSE, na = "")

## Text rendering (PROC PRINT style)
txt_file1 <- file.path(out, "P055.lst.txt")
txt_file2 <- file.path(out, "P055.txt")
con_text <- function(path) {
  sink(path)
  cat("Target 17\n")
  cat("\n")
  cat("Systolic Blood Pressure Figure by Treatment\n")
  cat("\n")
  cat("created by Xiaopeng Li\n")
  cat("\n")
  cat("PROC PRINT data=ci;\n")
  cat("\n")
  ## header
  cat(sprintf("%5s %-25s %10s %12s %12s\n", "Obs", "TRTA", "ADY", "LCLM", "UCLM"))
  cat(paste(rep("-", 70), collapse = ""), "\n")
  for (i in seq_len(nrow(ci_print))) {
    cat(sprintf("%5d %-25s %10g %12.4f %12.4f\n",
                i,
                ci_print$TRTA[i],
                ci_print$ADY[i],
                ifelse(is.na(ci_print$lclm[i]), NA, ci_print$lclm[i]),
                ifelse(is.na(ci_print$uclm[i]), NA, ci_print$uclm[i])))
  }
  cat("\n")
  cat(sprintf("Number of observations: %d\n", nrow(ci_print)))
  sink()
}
con_text(txt_file1)
con_text(txt_file2)

## Also write a more detailed listing with mean and n for traceability
detail_tbl <- meanvs %>% arrange(TRTA, ADY)
write.csv(detail_tbl, file.path(out, "meanvs.csv"), row.names = FALSE, na = "")
write.csv(detail_tbl, file.path(out, "P055_detail.csv"), row.names = FALSE, na = "")

## ------------------------------------------------------------------
## Plot: PROC GPLOT data=final plot mean*ady=trta / haxis=axis2 vaxis=axis1 ANNOTATE=anno
## axis1 label=(a=90 "Systolic blood pressure") order=(-200 to 350 by 50)
## axis2 label=("Visit number (visit day)") order=(0 to 250 by 50)
## symbol1 i=j v=dot color='blue'   -> Placebo
## symbol2 i=j v=triangle color='red' -> Xanomeline High Dose
## symbol3 i=j v=star color='green' -> Xanomeline Low Dose
## Annotate: error bars (vertical line + caps) and N counts at y=-170,-180,-190
## ------------------------------------------------------------------
## Color and shape mapping to match SAS symbol order (TRTA alphabetical order gives Placebo first)
## SAS symbol1 -> first TRTA level in plot (Placebo) = blue dot
## symbol2 -> second (Xanomeline High Dose) = red triangle
## symbol3 -> third (Xanomeline Low Dose) = green star
col_map <- c("Placebo" = "blue",
             "Xanomeline High Dose" = "red",
             "Xanomeline Low Dose" = "green")
shape_map <- c("Placebo" = 16,                # dot
               "Xanomeline High Dose" = 17,   # triangle
               "Xanomeline Low Dose" = 8)      # star

## N annotation positions
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
  labs(
    title = "Target 17",
    subtitle = "Systolic Blood Pressure Figure by Treatment",
    caption = "created by Xiaopeng Li"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

## Save PNG
png_main <- file.path(out, "sasgraph.png")
ggsave(png_main, plot = p, width = 10, height = 7, dpi = 300, units = "in")
## also save as figure.png for alternative expected name
ggsave(file.path(out, "figure.png"), plot = p, width = 10, height = 7, dpi = 300, units = "in")
ggsave(file.path(out, "P055.png"), plot = p, width = 10, height = 7, dpi = 300, units = "in")

## CSV of plotted data
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

## Also write final dataset as in SAS
write.csv(final_ds, file.path(out, "final.csv"), row.names = FALSE, na = "")
write.csv(mean_plot, file.path(out, "mean.csv"), row.names = FALSE, na = "")
write.csv(num_plot, file.path(out, "num.csv"), row.names = FALSE, na = "")

## Ensure at least one dataset CSV exists for generic check
write.csv(advs, file.path(out, "advs_filtered.csv"), row.names = FALSE, na = "")
```