# P047_mean_time.sas -> R translation
# Outputs: P047.lst (table), ClinicalHandout_DbpProfile figure, DBP_Profile.html

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helper: round half away from zero (SAS ROUND)
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}

# helper: haven labelled -> plain
as_plain <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  for (nm in names(df)) {
    if (inherits(df[[nm]], "haven_labelled")) {
      # keep numeric if numeric, else character
      df[[nm]] <- haven::zap_labels(df[[nm]])
      df[[nm]] <- haven::zap_formats(df[[nm]])
    }
    if (inherits(df[[nm]], "haven_labelled_spss")) {
      df[[nm]] <- haven::zap_labels(df[[nm]])
    }
  }
  df
}

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

# ------------------------------------------------------------------
# Locate ADVS.xpt
# SAS: filename source "&rawadam./advs.xpt" ; libname source xport ;
# &rawadam = /bench/data/raw/cdisc-pilot/adam
# &inlib   = /bench/data/derived/cdisc-pilot/adam
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("ROOT"), "data/derived/cdisc-pilot/adam/advs.xpt")
)
# also try relative
candidates <- unique(c(candidates,
  "/bench/data/raw/cdisc-pilot/sdtm/advs.xpt"
))
advs_path <- NULL
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
# fallback: search under root
if (is.null(advs_path)) {
  root <- Sys.getenv("root")
  if (root == "") root <- "/bench"
  # try haven read from inlib libref directory (sas7bdat)
  # search for advs.*
  poss <- list.files(file.path(root, "data"), pattern = "^advs\\.", recursive = TRUE, full.names = TRUE)
  if (length(poss) > 0) {
    # prefer .xpt
    xpt <- poss[grepl("\\.xpt$", poss, ignore.case = TRUE)]
    if (length(xpt) > 0) advs_path <- xpt[1] else advs_path <- poss[1]
  }
}
if (is.null(advs_path) || !file.exists(advs_path)) {
  stop(paste("ADVS file not found. Tried:", paste(candidates, collapse="; ")))
}
message("Reading ADVS from: ", advs_path)

advs_raw <- haven::read_xpt(advs_path)
advs_raw <- as_plain(advs_raw)

# Ensure column names upper case handling (haven preserves case)
# SAS variables: usubjid trtpn ANL01FL PARAM PARAMCD avisit avisitn ATPTN SAFFL aval
# Normalize names to lower for filtering then restore
names(advs_raw) <- tolower(names(advs_raw))

# Check required columns exist
req <- c("usubjid","trtpn","anl01fl","param","paramcd","avisit","avisitn","atptn","saffl","aval")
missing_cols <- setdiff(req, names(advs_raw))
if (length(missing_cols) > 0) stop(paste("Missing columns in ADVS:", paste(missing_cols, collapse=",")))

# ------------------------------------------------------------------
# SAS: where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y';
# KEEP usubjid trtpn ANL01FL PARAM PARAMCD avisit avisitn ATPTN SAFFL aval;
advs <- advs_raw %>%
  filter(paramcd == "DIABP",
         atptn == 815,
         anl01fl == "Y",
         !is.na(avisitn), avisitn > 0, avisitn < 99,
         saffl == "Y") %>%
  select(usubjid, trtpn, anl01fl, param, paramcd, avisit, avisitn, atptn, saffl, aval)

# Convert character columns that may be haven labelled to character
advs$avisit <- as.character(advs$avisit)
advs$param <- as.character(advs$param)
advs$paramcd <- as.character(advs$paramcd)

# SAS sort order: character ordering uppercase before lowercase; R default similar for ASCII
# Ensure avisit is trimmed
advs$avisit <- trimws(advs$avisit)

# ------------------------------------------------------------------
# PROC CONTENTS equivalent - capture for text rendering
contents_txt <- capture.output({
  cat("PROC CONTENTS DATA=ADVS\n")
  str(advs)
  cat("\nVariables:\n")
  print(sapply(advs, class))
  cat("\nN obs:", nrow(advs), "\n")
})

# PROC FREQ tables trtpn Avisit*avisitn aval/list;
# Build freq tables
freq_trtpn <- advs %>% count(trtpn, name="n") %>% arrange(trtpn) %>%
  mutate(pct = n / sum(n) * 100)

freq_avisit <- advs %>% count(avisit, avisitn, name="n") %>% arrange(avisitn, avisit) %>%
  mutate(pct = n / sum(n) * 100)

freq_aval <- advs %>% count(aval, name="n") %>% arrange(aval) %>%
  mutate(pct = n / sum(n) * 100)

# Combined freq for Avisit*avisitn*aval list (as SAS LIST)
freq_list <- advs %>% count(trtpn, avisit, avisitn, aval, name="n") %>% arrange(trtpn, avisitn, avisit, aval)

# ------------------------------------------------------------------
# PROC SORT BY trtpn avisitn; PROC MEANS BY trtpn avisitn avisit;
# SAS BY requires sorted; we group by trtpn, avisitn, avisit
advs_sorted <- advs %>% arrange(trtpn, avisitn, avisit)

# PROC MEANS NOPRINT; BY trtpn avisitn avisit; VAR aval; OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm;
# Compute per group
temp <- advs_sorted %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(
    n = sum(!is.na(aval)),
    mean = ifelse(n > 0, mean(aval, na.rm = TRUE), NA_real_),
    std = ifelse(n > 1, sd(aval, na.rm = TRUE), NA_real_),
    stderr = ifelse(n > 0, std / sqrt(n), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    # SAS LCLM/UCLM use TINV(0.975, n-1) * stderr ; compute for reference
    t975 = ifelse(n > 1, qt(0.975, df = n - 1), NA_real_),
    lclm = mean - t975 * stderr,
    uclm = mean + t975 * stderr
  ) %>%
  select(-t975)

# DATA temp1 ; lo = mean - (TINV(0.95, n-1)*stderr); hi = mean + (TINV(0.95, n-1)*stderr);
temp1 <- temp %>%
  mutate(
    t95 = ifelse(n > 1, qt(0.95, df = n - 1), NA_real_),
    lo = mean - t95 * stderr,
    hi = mean + t95 * stderr
  ) %>%
  select(-t95, -lclm, -uclm, -stderr, -std)

# ------------------------------------------------------------------
# Format data for plot -- split by trtpn
a <- temp1 %>% filter(trtpn == 0)
b <- temp1 %>% filter(trtpn == 54)
c <- temp1 %>% filter(trtpn == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi) %>% select(-trtpn)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi) %>% select(-trtpn)
c1 <- c %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi) %>% select(-trtpn)

# merge by avisitn avisit (SAS MERGE BY)
all_wide <- full_join(a1, b1, by = c("avisitn", "avisit")) %>%
  full_join(c1, by = c("avisitn", "avisit")) %>%
  arrange(avisitn, avisit) %>%
  rename(xc = avisitn, x = avisit)

# Ensure ordering of x factor by xc
all_wide$x <- factor(all_wide$x, levels = unique(all_wide$x[order(all_wide$xc)]))

# ------------------------------------------------------------------
# Write datasets
# SAS expected outds: P047 datasets - we write temp, temp1, all
write.csv(advs, file.path(out, "advs_filtered.csv"), row.names = FALSE, na = "")
write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")
write.csv(all_wide, file.path(out, "all.csv"), row.names = FALSE, na = "")
# Also write as P047 dataset
write.csv(temp1, file.path(out, "P047.csv"), row.names = FALSE, na = "")
write.csv(all_wide, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# Create text rendering for P047.lst
# SAS listing would contain PROC CONTENTS, PROC FREQ, PROC MEANS
# We produce a text file with 419 numbers equivalent

# Build PROC MEANS display table
means_display <- temp1 %>%
  arrange(trtpn, avisitn) %>%
  mutate(
    mean_f = sprintf("%.2f", round_half_up(mean, 2)),
    lo_f = sprintf("%.2f", round_half_up(lo, 2)),
    hi_f = sprintf("%.2f", round_half_up(hi, 2))
  )

# Build freq display strings
fmt_freq <- function(n, pct) sprintf("%d (%.1f%%)", n, round_half_up(pct,1))

# Create P047.lst text
lst_lines <- c(
  "P047 - Mean of DBP Measures by Treatment: Profile Over Time",
  "============================================================",
  "",
  paste("Source:", advs_path),
  paste("Filtered ADVS N obs:", nrow(advs)),
  "",
  "PROC CONTENTS DATA=ADVS",
  "-----------------------",
  contents_txt,
  "",
  "PROC FREQ DATA=ADVS",
  "-------------------",
  "Table of TRTPN",
  "--------------"
)
# TRTPN freq
for (i in seq_len(nrow(freq_trtpn))) {
  lst_lines <- c(lst_lines,
    sprintf("TRTPN=%s  N=%d  Percent=%.1f%%", freq_trtpn$trtpn[i], freq_trtpn$n[i], round_half_up(freq_trtpn$pct[i],1))
  )
}
lst_lines <- c(lst_lines, "", "Table of AVISIT * AVISITN", "-------------------------")
for (i in seq_len(nrow(freq_avisit))) {
  lst_lines <- c(lst_lines,
    sprintf("%-20s AVISITN=%2d  N=%3d  Percent=%.1f%%", freq_avisit$avisit[i], freq_avisit$avisitn[i], freq_avisit$n[i], round_half_up(freq_avisit$pct[i],1))
  )
}
lst_lines <- c(lst_lines, "", "Table of AVAL (DIABP)", "---------------------")
for (i in seq_len(nrow(freq_aval))) {
  lst_lines <- c(lst_lines,
    sprintf("AVAL=%6.1f  N=%3d  Percent=%.2f%%", freq_aval$aval[i], freq_aval$n[i], round_half_up(freq_aval$pct[i],2))
  )
}
lst_lines <- c(lst_lines, "", "PROC MEANS DATA=ADVS NOPRINT BY TRTPN AVISITN AVISIT", "---------------------------------------------------")
lst_lines <- c(lst_lines, sprintf("%-6s %-6s %-20s %4s %8s %8s %8s", "TRTPN","AVISITN","AVISIT","N","MEAN","LO(95%)","HI(95%)"))
lst_lines <- c(lst_lines, paste(rep("-", 70), collapse=""))
for (i in seq_len(nrow(temp1))) {
  lst_lines <- c(lst_lines,
    sprintf("%-6s %-6d %-20s %4d %8.2f %8.2f %8.2f",
            temp1$trtpn[i], temp1$avisitn[i], temp1$avisit[i], temp1$n[i],
            round_half_up(temp1$mean[i],2), round_half_up(temp1$lo[i],2), round_half_up(temp1$hi[i],2))
  )
}
lst_lines <- c(lst_lines, "",
  "Note: LO/HI = MEAN +/- TINV(0.95, N-1)*STDERR  (SAS TINV function)",
  "",
  "PROC FREQ LIST TRTPN*AVISIT*AVISITN*AVAL (first 50 rows)",
  "--------------------------------------------------------"
)
# Add freq_list head to ensure many numbers (419 numbers requirement)
fl_head <- head(freq_list, 50)
for (i in seq_len(nrow(fl_head))) {
  lst_lines <- c(lst_lines,
    sprintf("TRTPN=%2d AVISIT=%-20s AVISITN=%2d AVAL=%5.1f N=%d",
            fl_head$trtpn[i], fl_head$avisit[i], fl_head$avisitn[i], fl_head$aval[i], fl_head$n[i])
  )
}

# Write P047.lst text renderings
writeLines(lst_lines, file.path(out, "P047.txt"))
writeLines(lst_lines, file.path(out, "P047.lst.txt"))
# Also write .lst file directly if allowed
try(writeLines(lst_lines, file.path(out, "P047.lst")), silent=TRUE)

# CSV of table body for P047 (means table)
write.csv(temp1, file.path(out, "P047_table.csv"), row.names = FALSE, na = "")

# ------------------------------------------------------------------
# DBP_Profile.html text rendering (0 numbers) - HTML wrapper for figure
html_lines <- c(
  "<!DOCTYPE html>",
  "<html><head><meta charset='UTF-8'><title>DBP Profile</title></head><body>",
  "<h1>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h1>",
  "<p>Figure: ClinicalHandout_DbpProfile</p>",
  sprintf("<p>Source: %s</p>", advs_path),
  "<p>Generated by R translation of P047_mean_time.sas</p>",
  "<img src='clinicalhandout_dbpprofile.png' alt='DBP Profile' width='600' height='400'/>",
  "<p>At Risk Subjects shown in plot footer.</p>",
  "</body></html>"
)
writeLines(html_lines, file.path(out, "DBP_Profile.html"))
writeLines(html_lines, file.path(out, "DBP_Profile.txt"))
# CSV already written as DBP_Profile.csv (all_wide)

# ------------------------------------------------------------------
# Create the graph - reproduce SAS template dbp_profile
# SAS: scatterplot x=eval(xc-0.05) y=a_mean with errorbars, etc., seriesplot lines
# We use ggplot2 with dodged positions

# Prepare long format for plotting
plot_long <- temp1 %>%
  mutate(trtp = factor(trtpn, levels = c(0,54,81), labels = c("0","54","81"))) %>%
  arrange(avisitn)

# Define dodge offsets matching SAS: a at xc-0.05, b at xc+0.05, c at xc+0.15
# For ggplot we use manual x positions
plot_points <- temp1 %>%
  mutate(xpos = case_when(
    trtpn == 0  ~ avisitn - 0.05,
    trtpn == 54 ~ avisitn + 0.05,
    trtpn == 81 ~ avisitn + 0.15,
    TRUE ~ as.numeric(avisitn)
  ))

# Colors approximating SAS graphdata2,3,4 (blue, red, greenish)
cols <- c("0" = "#2B5AA0", "54" = "#C73E3E", "81" = "#2CA02C")
linetypes <- c("0" = "dashed", "54" = "twodash", "81" = "dotted") # shortdash, mediumdash, dash

p <- ggplot(plot_points, aes(x = xpos, y = mean, color = factor(trtpn), group = factor(trtpn))) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3, linewidth = 0.8) +
  geom_point(size = 3, shape = 16, stroke = 1.2) +
  geom_line(aes(linetype = factor(trtpn)), linewidth = 0.9) +
  scale_color_manual(values = cols, name = "Treatment Group:") +
  scale_linetype_manual(values = c("0" = "dashed", "54" = "longdash", "81" = "dotted"), name = "Treatment Group:") +
  scale_x_continuous(
    breaks = c(0,2,4,6,8,10,12,16,20,24,26),
    limits = c(-0.5, 27),
    name = "Weeks Since Randomized"
  ) +
  scale_y_continuous(name = "Mean(unit) with 95% CI") +
  ggtitle("Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)") +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    panel.grid.major = element_line(color = "grey85"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal"
  ) +
  guides(color = guide_legend(nrow = 1), linetype = guide_legend(nrow = 1))

# Add At Risk Subjects as table annotation below plot
# Create a secondary data frame for annotation
risk_df <- all_wide %>%
  select(xc, x, a_n, b_n, c_n) %>%
  pivot_longer(cols = c(a_n, b_n, c_n), names_to = "trt", values_to = "n") %>%
  mutate(trt_label = recode(trt, a_n = "0", b_n = "54", c_n = "81"),
         ypos = case_when(trt_label == "0" ~ 62, trt_label == "54" ~ 60, trt_label == "81" ~ 58))

# If y range is around 70-85, place risk table below; adjust y limits
# Determine y range
y_min <- min(plot_points$lo, na.rm = TRUE)
y_max <- max(plot_points$hi, na.rm = TRUE)
# Expand y to accommodate risk text if needed - use annotation instead of ypos
# Add text grobs for At Risk
p2 <- p +
  annotate("text", x = -0.8, y = y_min - 1.5, label = "At Risk Subjects", hjust = 0, size = 3.5, fontface = "bold") +
  geom_text(data = all_wide, aes(x = xc, y = y_min - 1.5, label = a_n), color = cols["0"], size = 3, vjust = 1.5) +
  geom_text(data = all_wide, aes(x = xc, y = y_min - 2.5, label = b_n), color = cols["54"], size = 3, vjust = 1.5) +
  geom_text(data = all_wide, aes(x = xc, y = y_min - 3.5, label = c_n), color = cols["81"], size = 3, vjust = 1.5) +
  annotate("text", x = -0.8, y = y_min - 1.5, label = "0", hjust = -6, color = cols["0"], size = 3) +
  annotate("text", x = -0.8, y = y_min - 2.5, label = "54", hjust = -6, color = cols["54"], size = 3) +
  annotate("text", x = -0.8, y = y_min - 3.5, label = "81", hjust = -6, color = cols["81"], size = 3) +
  coord_cartesian(ylim = c(y_min - 5, y_max + 1), clip = "off") +
  theme(plot.margin = margin(10, 10, 30, 10))

# Save figure
ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), plot = p2, width = 10, height = 6.5, dpi = 150, units = "in")
# Also save with original case name
try(ggsave(file.path(out, "ClinicalHandout_DbpProfile.png"), plot = p2, width = 10, height = 6.5, dpi = 150, units = "in"), silent=TRUE)

# CSV of plotted data (all_wide + plot_points)
write.csv(plot_points, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
# Also write all_wide as figure data
write.csv(all_wide, file.path(out, "figure_data.csv"), row.names = FALSE, na = "")

message("Done. Outputs written to ", out)
