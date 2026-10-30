# P055_outliers_TEAbnorm_Qual.sas -> R translation
# Outputs: P055 listing (ci) + sasgraph figure

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

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helper: round half away from zero (SAS)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# locate advs.xpt
find_file <- function(fname = "advs.xpt") {
  candidates <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm/advs.xpt",
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
    file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
    "data/raw/cdisc-pilot/adam/advs.xpt",
    "data/derived/cdisc-pilot/adam/advs.xpt",
    "./advs.xpt"
  )
  for (p in candidates) {
    if (!is.na(p) && nzchar(p) && file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
    p2 <- gsub("\\\\", "/", p)
    if (file.exists(p2)) return(normalizePath(p2, winslash = "/", mustWork = FALSE))
  }
  # recursive search from current working directory
  found <- list.files(getwd(), pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(found) > 0) return(found[1])
  # search from OUT_DIR parent
  base <- dirname(out)
  if (dir.exists(base)) {
    found2 <- list.files(base, pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(found2) > 0) return(found2[1])
  }
  # last resort: search from /
  try({
    found3 <- list.files("/", pattern = paste0("^", fname, "$"), recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(found3) > 0) return(found3[1])
  }, silent = TRUE)
  return(NA)
}

advs_path <- find_file("advs.xpt")
if (is.na(advs_path) || !file.exists(advs_path)) {
  # try alternative: search any advs file
  alt <- list.files(".", pattern = "advs", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(alt) > 0) advs_path <- alt[1]
}
if (is.na(advs_path) || !file.exists(advs_path)) {
  stop(paste0("advs.xpt not found. Tried candidates; OUT_DIR=", out, " cwd=", getwd()))
}
message("Reading: ", advs_path)
advs_raw <- haven::read_xpt(advs_path)

# Ensure character columns are character (haven may give labelled)
advs_raw <- advs_raw %>%
  mutate(across(where(~ inherits(., "haven_labelled")), ~ haven::as_factor(., levels = "labels") %>% as.character())) %>%
  mutate(across(where(is.factor), as.character))

# Check required columns exist
# SAS filter: SAFFL="Y" and PARAMCD="DIABP" and ATPT="AFTER LYING DOWN FOR 5 MINUTES"
# and (AVISITN ge 0 and AVISITN lt 99) and ANL01FL="Y"
# keep USUBJID TRTA ADY AVAL
# Ensure AVISITN numeric
if (!"AVISITN" %in% names(advs_raw)) stop("AVISITN not in advs")
if (!"ADY" %in% names(advs_raw)) stop("ADY not in advs")
# Convert AVISITN if character
if (is.character(advs_raw$AVISITN)) {
  advs_raw$AVISITN <- suppressWarnings(as.numeric(advs_raw$AVISITN))
}

advs <- advs_raw %>%
  filter(SAFFL == "Y",
         PARAMCD == "DIABP",
         ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN), AVISITN >= 0, AVISITN < 99,
         ANL01FL == "Y") %>%
  select(USUBJID, TRTA, ADY, AVAL) %>%
  filter(!is.na(TRTA), !is.na(ADY), !is.na(AVAL))

# SAS sort by TRTA ADY (character ordering uppercase before lowercase, missing first)
# R arrange does similar for these values
advs <- advs %>% arrange(TRTA, ADY)

# PROC MEANS by TRTA ADY var aval mean n lclm uclm
# SAS LCLM/UCLM = mean +/- t_{0.975, n-1} * std / sqrt(n)
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = mean(AVAL, na.rm = TRUE),
    sd = ifelse(n > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    se = sd / sqrt(n),
    tcrit = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lclm = mean - tcrit * se,
    uclm = mean + tcrit * se
  ) %>%
  select(TRTA, ADY, mean, n, lclm, uclm) %>%
  arrange(TRTA, ADY)

# SAS datasets mean, num, ci
mean_df <- meanvs %>% select(TRTA, ADY, mean)
num_df  <- meanvs %>% select(TRTA, ADY, num = n)
ci_df <- meanvs %>%
  select(TRTA, ADY, lclm, uclm) %>%
  mutate(ADY = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  arrange(TRTA, ADY)

# final dataset: set mean num with offset
final_mean <- mean_df %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  ))
final_num <- num_df %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  ))

final <- bind_rows(
  final_mean %>% mutate(num = NA_real_),
  final_num  %>% mutate(mean = NA_real_)
) %>%
  arrange(TRTA, ADY_plot)

# For plotting we need mean rows only
plot_data <- final_mean %>%
  left_join(meanvs %>% select(TRTA, ADY, n, lclm, uclm), by = c("TRTA", "ADY")) %>%
  arrange(TRTA, ADY_plot)

# ci for error bars (offset)
ci_plot <- meanvs %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  filter(!is.na(lclm), !is.na(uclm))

# anno2: N labels at y = -170, -180, -190 with original ADY (SAS uses original ADY)
anno2 <- num_df %>%
  mutate(
    x = ADY,
    y = case_when(
      TRTA == "Placebo"              ~ -170,
      TRTA == "Xanomeline High Dose" ~ -180,
      TRTA == "Xanomeline Low Dose"  ~ -190,
      TRUE ~ NA_real_
    ),
    label = as.character(num),
    color = case_when(
      TRTA == "Placebo"              ~ "blue",
      TRTA == "Xanomeline High Dose" ~ "red",
      TRTA == "Xanomeline Low Dose"  ~ "green",
      TRUE ~ "black"
    )
  ) %>%
  filter(!is.na(num))

# Colors and shapes matching SAS symbol1-3
# symbol1 i=j v=dot color blue -> Placebo
# symbol2 i=j v=triangle color red -> Xanomeline High Dose
# symbol3 i=j v=star color green -> Xanomeline Low Dose
trt_levels <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
color_map <- c("Placebo" = "blue", "Xanomeline High Dose" = "red", "Xanomeline Low Dose" = "green")
shape_map <- c("Placebo" = 16, "Xanomeline High Dose" = 17, "Xanomeline Low Dose" = 8) # dot, triangle, star

# Ensure factor ordering for legend
plot_data$TRTA <- factor(plot_data$TRTA, levels = trt_levels)
ci_plot$TRTA <- factor(ci_plot$TRTA, levels = trt_levels)
anno2$TRTA <- factor(anno2$TRTA, levels = trt_levels)

# Build figure
p <- ggplot() +
  geom_line(data = plot_data, aes(x = ADY_plot, y = mean, color = TRTA, group = TRTA), linewidth = 0.8) +
  geom_point(data = plot_data, aes(x = ADY_plot, y = mean, color = TRTA, shape = TRTA), size = 2.5) +
  geom_errorbar(data = ci_plot, aes(x = ADY_plot, ymin = lclm, ymax = uclm, color = TRTA), width = 0.8, linewidth = 0.7) +
  # caps are handled by width; SAS draws separate horizontal caps
  geom_text(data = anno2, aes(x = x, y = y, label = label, color = TRTA), size = 3, show.legend = FALSE) +
  scale_color_manual(values = color_map, name = "Treatment") +
  scale_shape_manual(values = shape_map, name = "Treatment") +
  scale_x_continuous(name = "Visit number (visit day)", limits = c(0, 250), breaks = seq(0, 250, by = 50)) +
  scale_y_continuous(name = "Systolic blood pressure", limits = c(-200, 350), breaks = seq(-200, 350, by = 50)) +
  labs(
    title = "Target 17",
    subtitle = "Systolic Blood Pressure Figure by Treatment\ncreated by Xiaopeng Li"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.subtitle = element_text(hjust = 0.5, size = 10),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# Save figure
png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = 10, height = 7, dpi = 300, units = "in")
message("Saved figure: ", png_path)

# CSV of plotted data (SAS semantics: include mean, n, lclm, uclm, ADY original and offset)
plotted_csv <- meanvs %>%
  mutate(ADY_plot = case_when(
    TRTA == "Xanomeline Low Dose"  ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo"              ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  select(TRTA, ADY, ADY_plot, mean, n, lclm, uclm) %>%
  arrange(TRTA, ADY_plot)

write.csv(plotted_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
# also write final.csv for completeness
write.csv(final, file.path(out, "final.csv"), row.names = FALSE, na = "")
write.csv(meanvs, file.path(out, "meanvs.csv"), row.names = FALSE, na = "")

# Listing: PROC PRINT data=ci
# ci_df has TRTA, ADY (offset), lclm, uclm
ci_out <- ci_df %>%
  arrange(TRTA, ADY) %>%
  select(TRTA, ADY, lclm, uclm)

# Write CSV for listing
write.csv(ci_out, file.path(out, "P055.csv"), row.names = FALSE, na = "")
# also as P055.lst.csv for compatibility
write.csv(ci_out, file.path(out, "P055.lst.csv"), row.names = FALSE, na = "")

# Text rendering mimicking PROC PRINT
# SAS PROC PRINT prints Obs, TRTA, ADY, LCLM, UCLM with BEST12. formatting
# Create plain text file
txt_path <- file.path(out, "P055.txt")
lst_path <- file.path(out, "P055.lst")
con <- file(txt_path, open = "wt")
sink_lst <- file(lst_path, open = "wt")

header_line <- sprintf("%-5s %-25s %10s %15s %15s", "Obs", "TRTA", "ADY", "LCLM", "UCLM")
sep_line <- paste(rep("-", nchar(header_line)), collapse = "")

writeLines(header_line, con)
writeLines(sep_line, con)
writeLines(header_line, sink_lst)
writeLines(sep_line, sink_lst)

# Format numbers: ADY with 1 decimal (to show offset), LCLM/UCLM with 4 decimals like SAS
for (i in seq_len(nrow(ci_out))) {
  obs <- i
  trta <- ci_out$TRTA[i]
  ady <- ci_out$ADY[i]
  lclm <- ci_out$lclm[i]
  uclm <- ci_out$uclm[i]
  ady_fmt <- ifelse(is.na(ady), ".", sprintf("%.1f", ady))
  lclm_fmt <- ifelse(is.na(lclm), ".", sprintf("%.4f", lclm))
  uclm_fmt <- ifelse(is.na(uclm), ".", sprintf("%.4f", uclm))
  line <- sprintf("%-5d %-25s %10s %15s %15s", obs, substr(trta, 1, 25), ady_fmt, lclm_fmt, uclm_fmt)
  writeLines(line, con)
  writeLines(line, sink_lst)
}
# Footer with N
footer <- sprintf("\nNumber of observations: %d", nrow(ci_out))
writeLines(footer, con)
writeLines(footer, sink_lst)
close(con)
close(sink_lst)
message("Saved listing: ", txt_path, " and ", lst_path)

# Also write dataset outputs as required
write.csv(advs, file.path(out, "advs_filtered.csv"), row.names = FALSE, na = "")

# Ensure at least one dataset CSV exists for declared output type dataset
# Already have meanvs.csv, final.csv, etc.

message("Done. OUT_DIR=", out)
