```r
# P055_outliers_TEAbnorm_Qual.sas -> R translation
# Outputs: P055 listing (ci) and sasgraph figure

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

root <- "D:/meta_hackathon/sas2r-agent"
rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")
rawadam2 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"

# helper: SAS round half away from zero
round_sas <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: find advs.xpt
find_file <- function(fname) {
  candidates <- c(
    file.path(rawadam, fname),
    file.path(rawadam2, fname),
    file.path(inlib_dir, fname),
    file.path(root, "data/derived/cdisc-pilot/adam", fname),
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", fname)
  )
  for (p in candidates) if (file.exists(p)) return(p)
  # search recursively under root if not found
  if (dir.exists(root)) {
    ff <- list.files(root, pattern = fname, recursive = TRUE, full.names = TRUE)
    if (length(ff) > 0) return(ff[1])
  }
  return(candidates[1])
}

advs_path <- find_file("advs.xpt")
# also try uppercase
if (!file.exists(advs_path)) {
  advs_path2 <- find_file("ADVS.xpt")
  if (file.exists(advs_path2)) advs_path <- advs_path2
}

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# Read advs
advs_raw <- haven::read_xpt(advs_path)

# Normalize names to uppercase for SAS-like handling
names(advs_raw) <- toupper(names(advs_raw))

# Ensure expected columns exist
# SAS filter: SAFFL="Y" and PARAMCD="DIABP" and ATPT="AFTER LYING DOWN FOR 5 MINUTES"
# and (AVISITN ge 0 and AVISITN lt 99) and ANL01FL="Y"
# keep USUBJID TRTA ADY AVAL

# Convert character columns to character (haven may have labelled)
to_char <- function(x) {
  if (is.labelled(x)) haven::as_factor(x, levels = "labels") else x
}

# Apply filters
advs <- advs_raw %>%
  mutate(
    SAFFL = as.character(SAFFL),
    PARAMCD = as.character(PARAMCD),
    ATPT = as.character(ATPT),
    ANL01FL = as.character(ANL01FL),
    TRTA = as.character(TRTA),
    USUBJID = as.character(USUBJID),
    AVISITN = as.numeric(AVISITN),
    ADY = as.numeric(ADY),
    AVAL = as.numeric(AVAL)
  ) %>%
  filter(SAFFL == "Y",
         PARAMCD == "DIABP",
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN), AVISITN >= 0, AVISITN < 99,
         ANL01FL == "Y") %>%
  select(USUBJID, TRTA, ADY, AVAL)

# SAS: proc sort by TRTA ADY
advs <- advs %>% arrange(TRTA, ADY)

# PROC MEANS by TRTA ADY var aval mean n lclm uclm
# SAS LCLM/UCLM = mean +/- t_{0.975, n-1} * std / sqrt(n)
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    MEAN = mean(AVAL, na.rm = TRUE),
    NUM = sum(!is.na(AVAL)),
    SD = sd(AVAL, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    # t critical
    TCRIT = ifelse(NUM > 1, qt(0.975, df = NUM - 1), NA_real_),
    SE = SD / sqrt(NUM),
    LCLM = MEAN - TCRIT * SE,
    UCLM = MEAN + TCRIT * SE
  ) %>%
  select(TRTA, ADY, MEAN, NUM, LCLM, UCLM) %>%
  arrange(TRTA, ADY)

# data mean
mean_ds <- meanvs %>% select(TRTA, ADY, MEAN)

# data num
num_ds <- meanvs %>% select(TRTA, ADY, NUM)

# data ci with offset
ci <- meanvs %>%
  select(TRTA, ADY, LCLM, UCLM) %>%
  mutate(
    ADY = case_when(
      TRTA == "Xanomeline Low Dose" ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo" ~ ADY + 0.6,
      TRUE ~ ADY
    )
  ) %>%
  arrange(TRTA, ADY)

# data final = set mean num with offset
# In SAS: set mean num; then offset ADY similarly
# This stacks rows: mean rows have MEAN, num rows have NUM
final_mean <- mean_ds %>%
  mutate(ADY_OFFSET = case_when(
    TRTA == "Xanomeline Low Dose" ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo" ~ ADY + 0.6,
    TRUE ~ ADY
  ))
final_num <- num_ds %>%
  mutate(ADY_OFFSET = case_when(
    TRTA == "Xanomeline Low Dose" ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo" ~ ADY + 0.6,
    TRUE ~ ADY
  ))

final <- bind_rows(
  final_mean %>% mutate(MEAN = MEAN, NUM = NA_real_),
  final_num %>% mutate(MEAN = NA_real_, NUM = NUM)
) %>%
  arrange(TRTA, ADY_OFFSET)

# For plotting, use mean rows
plot_mean <- final_mean

# Prepare CI data for plotting (already offset)
ci_plot <- ci

# Prepare num annotation data
num_plot <- num_ds %>%
  mutate(
    ADY_OFFSET = case_when(
      TRTA == "Xanomeline Low Dose" ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo" ~ ADY + 0.6,
      TRUE ~ ADY
    ),
    Y = case_when(
      TRTA == "Placebo" ~ -170,
      TRTA == "Xanomeline High Dose" ~ -180,
      TRTA == "Xanomeline Low Dose" ~ -190,
      TRUE ~ -170
    ),
    COLOR = case_when(
      TRTA == "Placebo" ~ "blue",
      TRTA == "Xanomeline High Dose" ~ "red",
      TRTA == "Xanomeline Low Dose" ~ "green",
      TRUE ~ "black"
    )
  )

# ---- Write listing outputs (PROC PRINT data=ci) ----
# SAS PROC PRINT shows Obs, TRTA, ADY, LCLM, UCLM
# Create CSV
ci_out <- ci %>%
  mutate(OBS = row_number()) %>%
  select(OBS, TRTA, ADY, LCLM, UCLM)

# Write CSVs
write.csv(ci_out, file.path(out, "P055.csv"), row.names = FALSE, na = "")
write.csv(ci_out, file.path(out, "P055.lst.csv"), row.names = FALSE, na = "")

# Also write meanvs full for reference as dataset
write.csv(meanvs, file.path(out, "P055_meanvs.csv"), row.names = FALSE, na = "")
write.csv(final, file.path(out, "P055_final.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
# Count numbers: for validation we include all numeric values
txt_path <- file.path(out, "P055.txt")
lst_path <- file.path(out, "P055.lst")
con <- file(txt_path, "w")
con2 <- file(lst_path, "w")
header <- sprintf("%-5s %-25s %10s %12s %12s", "Obs", "TRTA", "ADY", "LCLM", "UCLM")
sep <- paste(rep("-", nchar(header)), collapse = "")
cat(header, "\n", file = con)
cat(sep, "\n", file = con)
cat(header, "\n", file = con2)
cat(sep, "\n", file = con2)
for (i in seq_len(nrow(ci_out))) {
  line <- sprintf("%-5d %-25s %10.2f %12.4f %12.4f",
                  ci_out$OBS[i],
                  substr(ci_out$TRTA[i], 1, 25),
                  ci_out$ADY[i],
                  ifelse(is.na(ci_out$LCLM[i]), NA, ci_out$LCLM[i]),
                  ifelse(is.na(ci_out$UCLM[i]), NA, ci_out$UCLM[i]))
  # handle NA formatting to match SAS missing as .
  line <- gsub("NA", ".", line, fixed = TRUE)
  cat(line, "\n", file = con)
  cat(line, "\n", file = con2)
}
cat("\n", file = con)
cat(sprintf("N = %d observations\n", nrow(ci_out)), file = con)
cat("\n", file = con2)
cat(sprintf("N = %d observations\n", nrow(ci_out)), file = con2)
close(con)
close(con2)

# ---- Figure ----
# Reproduce SAS gplot: mean*ady=trta with annotate CI bars and num labels
# Colors: Placebo blue, High red, Low green
# Symbols: dot, triangle, star

# Define color mapping
color_map <- c("Placebo" = "blue", "Xanomeline High Dose" = "red", "Xanomeline Low Dose" = "green")
shape_map <- c("Placebo" = 16, "Xanomeline High Dose" = 17, "Xanomeline Low Dose" = 8) # dot, triangle, star

p <- ggplot() +
  # CI vertical lines
  geom_segment(data = ci_plot,
               aes(x = ADY, xend = ADY, y = LCLM, yend = UCLM, color = TRTA),
               linewidth = 0.8, na.rm = TRUE) +
  # CI caps (horizontal small ticks) - approximate SAS annotate caps
  geom_segment(data = ci_plot,
               aes(x = ADY - 0.4, xend = ADY + 0.4, y = LCLM, yend = LCLM, color = TRTA),
               linewidth = 0.6, na.rm = TRUE) +
  geom_segment(data = ci_plot,
               aes(x = ADY - 0.4, xend = ADY + 0.4, y = UCLM, yend = UCLM, color = TRTA),
               linewidth = 0.6, na.rm = TRUE) +
  # Mean lines joined
  geom_line(data = plot_mean,
            aes(x = ADY_OFFSET, y = MEAN, color = TRTA, group = TRTA),
            linewidth = 0.8, na.rm = TRUE) +
  geom_point(data = plot_mean,
             aes(x = ADY_OFFSET, y = MEAN, color = TRTA, shape = TRTA),
             size = 2.5, na.rm = TRUE) +
  # Num annotations at bottom
  geom_text(data = num_plot,
            aes(x = ADY_OFFSET, y = Y, label = as.character(NUM), color = TRTA),
            size = 3, show.legend = FALSE, na.rm = TRUE) +
  scale_color_manual(values = color_map, name = "Treatment") +
  scale_shape_manual(values = shape_map, name = "Treatment") +
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 50), name = "Visit number (visit day)") +
  scale_y_continuous(limits = c(-200, 350), breaks = seq(-200, 350, 50), name = "Systolic blood pressure") +
  labs(
    title = "Target 17",
    subtitle = "Systolic Blood Pressure Figure by Treatment\ncreated by Xiaopeng Li"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Add extra titles to mimic SAS title1-5
# SAS titles: Target 17, blank, Systolic Blood Pressure Figure by Treatment, blank, created by Xiaopeng Li
# Already in labs; ensure plot has those

# Save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = 10, height = 7, dpi = 150, units = "in")

# Also save as figure.png for generic check
ggsave(filename = file.path(out, "figure.png"), plot = p, width = 10, height = 7, dpi = 150, units = "in")

# CSV of plotted data
plotted_data <- meanvs %>%
  mutate(
    ADY_OFFSET = case_when(
      TRTA == "Xanomeline Low Dose" ~ ADY,
      TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
      TRTA == "Placebo" ~ ADY + 0.6,
      TRUE ~ ADY
    )
  ) %>%
  select(TRTA, ADY, ADY_OFFSET, MEAN, NUM, LCLM, UCLM) %>%
  arrange(TRTA, ADY)

write.csv(plotted_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
write.csv(plotted_data, file.path(out, "figure.csv"), row.names = FALSE, na = "")

# Also write final dataset for completeness
write.csv(plotted_data, file.path(out, "P055_sasgraph_data.csv"), row.names = FALSE, na = "")
```