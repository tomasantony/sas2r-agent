out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

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
})

# SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

cat("Reading ADVS\n")
advs_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
if (!file.exists(advs_path)) {
  advs_path2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
  if (file.exists(advs_path2)) advs_path <- advs_path2
}
advs_raw <- haven::read_xpt(advs_path)

# Ensure plain vectors (haven_labelled -> character/numeric)
# Keep needed vars, filter as SAS:
# if SAFFL="Y" and PARAMCD="DIABP" and ATPT="AFTER LYING DOWN FOR 5 MINUTES"
#    and (AVISITN ge 0 and AVISITN lt 99) and ANL01FL="Y";
advs <- advs_raw %>%
  mutate(
    SAFFL = as.character(SAFFL),
    PARAMCD = as.character(PARAMCD),
    ATPT = as.character(ATPT),
    ANL01FL = as.character(ANL01FL),
    TRTA = as.character(TRTA),
    USUBJID = as.character(USUBJID),
    ADY = suppressWarnings(as.numeric(ADY)),
    AVAL = suppressWarnings(as.numeric(AVAL)),
    AVISITN = suppressWarnings(as.numeric(AVISITN))
  ) %>%
  filter(!is.na(SAFFL) & SAFFL == "Y",
         !is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ATPT) & ATPT == "AFTER LYING DOWN FOR 5 MINUTES",
         !is.na(AVISITN) & AVISITN >= 0 & AVISITN < 99,
         !is.na(ANL01FL) & ANL01FL == "Y") %>%
  select(USUBJID, TRTA, ADY, AVAL)

# proc sort; by TRTA ADY; run;  (SAS: missing first, uppercase before lowercase)
advs <- advs %>% arrange(TRTA, ADY, USUBJID)

# proc means data=advs mean n lclm uclm noprint; by TRTA ADY; var aval;
# output out=meanvs mean=mean n=num lclm=lclm uclm=uclm;
# SAS LCLM/UCLM: two-sided 95% t-interval: mean +/- t(0.975,n-1)*std/sqrt(n)
meanvs <- advs %>%
  group_by(TRTA, ADY) %>%
  summarise(
    MEAN = mean(AVAL, na.rm = TRUE),
    NUM = sum(!is.na(AVAL)),
    STD = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    MEAN = ifelse(NUM == 0, NA_real_, MEAN),
    TCRIT = ifelse(!is.na(NUM) & NUM >= 2, qt(0.975, NUM - 1), NA_real_),
    SE = STD / sqrt(NUM),
    LCLM = MEAN - TCRIT * SE,
    UCLM = MEAN + TCRIT * SE
  ) %>%
  select(TRTA, ADY, MEAN, NUM, LCLM, UCLM) %>%
  arrange(TRTA, ADY)

# data mean; keep trta ady mean;
mean_df <- meanvs %>% select(TRTA, ADY, MEAN)

# data num; keep trta ady num;
num_df <- meanvs %>% select(TRTA, ADY, NUM)

# data ci; keep trta ady lclm uclm; + offset; run; proc sort; by trta ady;
ci <- meanvs %>% select(TRTA, ADY, LCLM, UCLM) %>%
  mutate(ADY = case_when(
    TRTA == "Xanomeline Low Dose" ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo" ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  arrange(TRTA, ADY)

# ---- PROC PRINT data=ci  -> P055.lst ----
# SAS PROC PRINT default columns: Obs TRTA ADY LCLM UCLM
ci_print <- ci

# Write CSV of table body
write.csv(ci_print, file.path(out, "P055.csv"), row.names = FALSE, na = "")

# Text rendering containing same numbers
txt_path <- file.path(out, "P055.txt")
lst_path <- file.path(out, "P055.lst")
con_txt <- file(txt_path, open = "wt")
sink(con_txt, split = FALSE)
cat("Target 17\n")
cat("Systolic Blood Pressure Figure by Treatment\n")
cat("PROC PRINT data=CI\n\n")
# Print with full precision similar to SAS BEST12.
# Use data.frame print
df_show <- as.data.frame(ci_print)
# Ensure Obs column
df_show <- cbind(Obs = seq_len(nrow(df_show)), df_show)
print(df_show, row.names = FALSE, right = TRUE)
sink()
close(con_txt)
# duplicate as .lst for benchmark expecting P055.lst
file.copy(txt_path, lst_path, overwrite = TRUE)
cat(paste0("Wrote table: ", txt_path, " rows=", nrow(ci_print), "\n"))

# ---- ANNOTATE / FINAL datasets (for figure) ----
# data final; set mean num; offset ady;
final <- bind_rows(
  mean_df %>% mutate(NUM = NA_real_),
  num_df %>% mutate(MEAN = NA_real_)
) %>%
  mutate(ADY_PLOT = case_when(
    TRTA == "Xanomeline Low Dose" ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo" ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  arrange(TRTA, ADY_PLOT)

# Plotted mean points (non-missing MEAN)
plot_mean <- final %>% filter(!is.na(MEAN))

# CI for error bars (non-missing LCLM/UCLM) - ci already offset; rename for merge
plot_ci <- ci %>% filter(!is.na(LCLM) & !is.na(UCLM))

# N annotations: anno2 uses original ADY (no offset), y by treatment
plot_n <- num_df %>%
  filter(!is.na(NUM)) %>%
  mutate(
    YPOS = case_when(
      TRTA == "Placebo" ~ -170,
      TRTA == "Xanomeline High Dose" ~ -180,
      TRTA == "Xanomeline Low Dose" ~ -190,
      TRUE ~ -170
    )
  )

# CSV of plotted data: join mean + ci + n on TRTA + original ADY
# Reconstruct original ADY for ci/plot_mean
plot_data_csv <- meanvs %>%
  mutate(ADY_PLOT = case_when(
    TRTA == "Xanomeline Low Dose" ~ ADY,
    TRTA == "Xanomeline High Dose" ~ ADY + 0.3,
    TRTA == "Placebo" ~ ADY + 0.6,
    TRUE ~ ADY
  )) %>%
  select(TRTA, ADY, ADY_PLOT, MEAN, NUM, LCLM, UCLM) %>%
  arrange(TRTA, ADY)

write.csv(plot_data_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
cat(paste0("Wrote plotted data CSV rows=", nrow(plot_data_csv), "\n"))

# ---- Figure: PROC GPLOT plot mean*ady=trta with ANNOTATE ----
# symbol1 i=j v=dot color=blue (Placebo)
# symbol2 i=j v=triangle color=red (High)
# symbol3 i=j v=star color=green (Low)
# axis1 order -200 to 350 by 50 label Systolic blood pressure
# axis2 order 0 to 250 by 50 label Visit number (visit day)

col_map <- c("Placebo" = "blue",
             "Xanomeline High Dose" = "red",
             "Xanomeline Low Dose" = "green")
shape_map <- c("Placebo" = 16,
               "Xanomeline High Dose" = 17,
               "Xanomeline Low Dose" = 8)

# Ensure factor order matches SAS sort: Placebo, High, Low
trt_order <- c("Placebo", "Xanomeline High Dose", "Xanomeline Low Dose")
plot_mean$TRTA <- factor(plot_mean$TRTA, levels = trt_order)
plot_ci$TRTA <- factor(plot_ci$TRTA, levels = trt_order)
plot_n$TRTA <- factor(plot_n$TRTA, levels = trt_order)
plot_data_csv$TRTA <- factor(plot_data_csv$TRTA, levels = trt_order)

p <- ggplot() +
  geom_line(data = plot_mean,
            aes(x = ADY_PLOT, y = MEAN, color = TRTA, group = TRTA),
            linewidth = 0.7) +
  geom_point(data = plot_mean,
             aes(x = ADY_PLOT, y = MEAN, color = TRTA, shape = TRTA),
             size = 2.2) +
  geom_errorbar(data = as.data.frame(plot_ci),
                aes(x = ADY, ymin = LCLM, ymax = UCLM, color = TRTA),
                width = 2, linewidth = 0.7) +
  geom_text(data = as.data.frame(plot_n),
            aes(x = ADY, y = YPOS, label = NUM, color = TRTA),
            size = 3, show.legend = FALSE) +
  scale_color_manual(values = col_map, name = "Treatment") +
  scale_shape_manual(values = shape_map, name = "Treatment") +
  scale_x_continuous(limits = c(0, 250), breaks = seq(0, 250, 50),
                     name = "Visit number (visit day)") +
  scale_y_continuous(limits = c(-200, 350), breaks = seq(-200, 350, 50),
                     name = "Systolic blood pressure") +
  labs(title = "Target 17\nSystolic Blood Pressure Figure by Treatment",
       subtitle = "created by Xiaopeng Li") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "bottom")

png_path <- file.path(out, "sasgraph.png")
ggsave(png_path, plot = p, width = 10, height = 7, dpi = 150)
cat(paste0("Wrote figure: ", png_path, "\n"))
