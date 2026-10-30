# ------------------------------------------------------------
# R translation of P032_target04.sas  (attempt 2)
# ------------------------------------------------------------
library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------
# SAS-style round half away from zero
round_half_up <- function(x, digits = 0) {
  sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits
}

# SAS TINV (upper‑tail inverse t)
tinv <- function(p, df) {
  qt(p, df, lower.tail = FALSE)
}

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------
rawadam <- "/bench/data/raw/cdisc-pilot/adam"
out_dir <- Sys.getenv("OUT_DIR")
if (out_dir == "") stop("OUT_DIR environment variable not set")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------
# 1. Read source ADVS transport file
# ------------------------------------------------------------
advs_raw <- read_xpt(file.path(rawadam, "advs.xpt"))

# ------------------------------------------------------------
# 2. Apply SAS data‑step filter and keep variables
# (haven converts names to lower case)
# ------------------------------------------------------------
advs <- advs_raw %>%
  # keep only needed columns (case‑insensitive)
  select(
    usubjid, trtpn, anl01fl, param, paramcd,
    avisit, avisitn, atptn, saffl, aval
  ) %>%
  # ensure numeric treatment code
  mutate(trtpn = as.numeric(trtpn)) %>%
  # SAS WHERE clause
  filter(
    paramcd == "DIABP",
    atptn == 815,
    anl01fl == "Y",
    !is.na(avisitn) & avisitn > 0 & avisitn < 99,
    saffl == "Y"
  )

# ------------------------------------------------------------
# 3. PROC MEANS equivalent (by trtpn avisitn avisit)
# ------------------------------------------------------------
means_tbl <- advs %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(
    n = n(),
    mean = mean(aval, na.rm = TRUE),
    stddev = sd(aval, na.rm = TRUE),
    stderr = ifelse(n > 0, stddev / sqrt(n), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    lclm = mean - tinv(0.975, n - 1) * stderr,
    uclm = mean + tinv(0.975, n - 1) * stderr
  )

# ------------------------------------------------------------
# 4. Compute lo/hi using SAS TINV(0.95, n‑1) as in the program
# ------------------------------------------------------------
summary_tbl <- means_tbl %>%
  mutate(
    tcrit = tinv(0.95, n - 1),
    lo = mean - tcrit * stderr,
    hi = mean + tcrit * stderr
  ) %>%
  select(trtpn, avisitn, avisit, n, mean, stderr, lclm, uclm, lo, hi)

# ------------------------------------------------------------
# 5. Split by treatment group (0, 54, 81) and rename columns
# ------------------------------------------------------------
a1 <- summary_tbl %>% filter(trtpn == 0) %>%
  rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi)

b1 <- summary_tbl %>% filter(trtpn == 54) %>%
  rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi)

c1 <- summary_tbl %>% filter(trtpn == 81) %>%
  rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi)

# ------------------------------------------------------------
# 6. Merge the three groups by visit
# ------------------------------------------------------------
all_df <- full_join(a1, b1, by = c("avisitn", "avisit")) %>%
  full_join(c1, by = c("avisitn", "avisit")) %>%
  rename(
    xc = avisitn,
    x  = avisit
  ) %>%
  select(-trtpn)   # not needed after merge

# ------------------------------------------------------------
# 7. Write the summary listing (P032.lst) – text and CSV
# ------------------------------------------------------------
lst_txt_path <- file.path(out_dir, "P032.lst.txt")
lst_csv_path <- file.path(out_dir, "P032.lst.csv")

# Text rendering (SAS‑style spacing, two‑decimal for numeric stats)
txt_con <- file(lst_txt_path, open = "wt")
writeLines("Summary of DBP (DIABP) by Treatment and Visit", txt_con)
header <- sprintf(
  "%-6s %-8s %-8s %8s %12s %12s %12s %12s %12s %12s",
  "TRTPN", "AVISITN", "AVISIT", "N",
  "MEAN", "STDERR", "LCLM", "UCLM", "LO", "HI"
)
writeLines(header, txt_con)

apply(summary_tbl, 1, function(row) {
  line <- sprintf(
    "%-6s %-8s %-8s %8d %12.2f %12.2f %12.2f %12.2f %12.2f %12.2f",
    row["trtpn"], row["avisitn"], row["avisit"], as.integer(row["n"]),
    row["mean"], row["stderr"], row["lclm"], row["uclm"],
    row["lo"], row["hi"]
  )
  writeLines(line, txt_con)
})
close(txt_con)

# CSV version (raw numbers, no rounding)
write_csv(summary_tbl, lst_csv_path, na = "")

# ------------------------------------------------------------
# 8. Write the merged dataset used for the plot
# ------------------------------------------------------------
write_csv(all_df, file.path(out_dir, "all.csv"), na = "")

# ------------------------------------------------------------
# 9. Create the plot (replicating the SAS graph)
# ------------------------------------------------------------
# Prepare block‑plot data (counts per group)
block_data <- all_df %>%
  select(xc, a_n, b_n, c_n) %>%
  pivot_longer(
    cols = starts_with("a_") | starts_with("b_") | starts_with("c_"),
    names_to = "group", values_to = "n"
  ) %>%
  mutate(
    group = case_when(
      grepl("^a_", group) ~ "0",
      grepl("^b_", group) ~ "54",
      grepl("^c_", group) ~ "81",
      TRUE                ~ NA_character_
    ),
    group_num = as.numeric(group)
  )

p <- ggplot() +
  # Scatter points with error bars
  geom_point(data = all_df, aes(x = xc - 0.05, y = a_mean), size = 3, colour = "#1f77b4") +
  geom_errorbar(data = all_df, aes(x = xc - 0.05, ymin = a_lcl, ymax = a_ucl), width = 0.02, colour = "#1f77b4") +
  geom_point(data = all_df, aes(x = xc + 0.05, y = b_mean), size = 3, colour = "#ff7f0e") +
  geom_errorbar(data = all_df, aes(x = xc + 0.05, ymin = b_lcl, ymax = b_ucl), width = 0.02, colour = "#ff7f0e") +
  geom_point(data = all_df, aes(x = xc + 0.15, y = c_mean), size = 3, colour = "#2ca02c") +
  geom_errorbar(data = all_df, aes(x = xc + 0.15, ymin = c_lcl, ymax = c_ucl), width = 0.02, colour = "#2ca02c") +
  # Series lines
  geom_line(data = all_df, aes(x = xc - 0.05, y = a_mean), linetype = "shortdash", colour = "#1f77b4") +
  geom_line(data = all_df, aes(x = xc + 0.05, y = b_mean), linetype = "mediumdash", colour = "#ff7f0e") +
  geom_line(data = all_df, aes(x = xc + 0.15, y = c_mean), linetype = "dash", colour = "#2ca02c") +
  # Block plot (counts)
  geom_tile(data = block_data, aes(x = xc, y = group_num, fill = n), colour = "black", width = 0.8, height = 0.8) +
  scale_fill_gradient(low = "white", high = "gray80", na.value = "transparent") +
  scale_y_continuous(
    name = "Mean(unit) with 95% CI",
    sec.axis = dup_axis(name = "At Risk Subjects")
  ) +
  scale_x_continuous(
    name = "Weeks Since Randomized",
    breaks = c(0,2,4,6,8,10,12,16,20,24,26),
    limits = c(min(all_df$xc, na.rm = TRUE) - 0.2, max(all_df$xc, na.rm = TRUE) + 0.2)
  ) +
  labs(
    title = "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

# Save PNG (SAS used GIF; PNG is acceptable per specification)
png_path <- file.path(out_dir, "clinicalhandout_dbpprofile.png")
ggsave(filename = png_path, plot = p, width = 10, height = 6.7, dpi = 100, units = "in")

# Also write the plotted data used for the figure
write_csv(all_df, file.path(out_dir, "clinicalhandout_dbpprofile.csv"), na = "")

# ------------------------------------------------------------
# 10. Create simple HTML wrapper (DBP_Profile.html)
# ------------------------------------------------------------
html_path <- file.path(out_dir, "DBP_Profile.html")
html_content <- sprintf(
  '<html>\n<head><title>DBP Profile</title></head>\n<body>\n<h2>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h2>\n<img src="%s" alt="DBP Profile Plot"/>\n</body>\n</html>',
  basename(png_path)
)
writeLines(html_content, con = html_path)

# ------------------------------------------------------------
# End of script
# ------------------------------------------------------------
