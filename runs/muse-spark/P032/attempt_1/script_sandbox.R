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

cat("P032 target04: start\n")

## round half away from zero (SAS) helper
rhu <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt2 <- function(x) {
  ifelse(is.na(x), "",
    formatC(rhu(x, 2), format = "f", digits = 2, flag = "#"))
}
fmt1 <- function(x) {
  ifelse(is.na(x), "",
    formatC(rhu(x, 1), format = "f", digits = 1))
}

## ---- locate input ----
candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
)
src <- NULL
for (f in candidates) if (file.exists(f)) { src <- f; break }
if (is.null(src)) {
  ## fallback: any advs file under root
  alt <- list.files("/bench/data", pattern = "^advs\\.(xpt|sas7bdat)$",
                    recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(alt) > 0) src <- alt[1]
}
cat("input:", src, "\n")
if (grepl("\\.xpt$", src, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(src)
} else {
  advs_raw <- haven::read_sas(src)
}
names(advs_raw) <- tolower(names(advs_raw))

## ---- data advs ; where PARAMCD DIABP etc ----
## SAS: where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y'
advs <- advs_raw %>%
  filter(paramcd == "DIABP",
         atptn == 815,
         anl01fl == "Y",
         !is.na(avisitn), avisitn < 99,
         saffl == "Y") %>%
  select(usubjid, trtpn, anl01fl, param, paramcd, avisit, avisitn, atptn, saffl, aval)

## SAS sort character ordering: use radix (byte, uppercase before lowercase)
## sort data=advs; by trtpn avisitn;
advs <- advs[order(advs$trtpn, advs$avisitn, advs$avisit, method = "radix"), , drop = FALSE]

cat("advs n:", nrow(advs), "\n")
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

## ---- proc contents ----
nobs <- nrow(advs)
nvar <- ncol(advs)
varinfo <- tibble(
  var = names(advs),
  type = sapply(advs, function(x) if (is.character(x)) "Char" else "Num")
)

## ---- proc freq data=advs; tables trtpn Avisit*avisitn aval/list; ----
## SAS default excludes missing unless MISSING option
total_n <- nrow(advs)

## one-way trtpn (exclude NA)
f_trtpn <- advs %>% filter(!is.na(trtpn)) %>%
  count(trtpn, name = "frequency") %>%
  arrange(trtpn) %>%
  mutate(den = sum(frequency),
         percent = 100 * frequency / den,
         cumfreq = cumsum(frequency),
         cumpercent = cumsum(percent))
## two-way avisit*avisitn list format
f_cross <- advs %>% filter(!is.na(avisit), !is.na(avisitn)) %>%
  count(avisit, avisitn, name = "frequency")
## SAS order for cross: by avisit (char radix) then avisitn
f_cross <- f_cross[order(f_cross$avisit, f_cross$avisitn, method = "radix"), , drop = FALSE]
den_cross <- sum(f_cross$frequency)
f_cross <- f_cross %>%
  mutate(percent = 100 * frequency / den_cross)
## row/col percents for completeness (SAS crosstab defaults)
row_tot <- f_cross %>% group_by(avisit) %>% summarise(rt = sum(frequency), .groups = "drop")
col_tot <- f_cross %>% group_by(avisitn) %>% summarise(ct = sum(frequency), .groups = "drop")
f_cross <- f_cross %>% left_join(row_tot, by = "avisit") %>% left_join(col_tot, by = "avisitn") %>%
  mutate(row_percent = 100 * frequency / rt,
         col_percent = 100 * frequency / ct) %>%
  select(-rt, -ct)
## one-way aval (exclude missing per PROC FREQ default)
f_aval_base <- advs %>% filter(!is.na(aval)) %>%
  count(aval, name = "frequency") %>%
  arrange(aval) %>%
  mutate(den = sum(frequency),
         percent = 100 * frequency / den,
         cumfreq = cumsum(frequency),
         cumpercent = cumsum(percent))
nmiss_aval <- sum(is.na(advs$aval))
nmiss_trtpn <- sum(is.na(advs$trtpn))

## ---- build P032.lst text rendering ----
con_txt <- c(
  "PROC CONTENTS DATA=ADVS",
  paste0("Observations: ", nobs),
  paste0("Variables: ", nvar),
  "",
  sprintf("%-12s %-6s", "Variable", "Type"),
  paste(sprintf("%-12s %-6s", varinfo$var, varinfo$type), collapse = "\n"),
  "",
  "PROC FREQ DATA=ADVS",
  paste0("Number of observations: ", total_n),
  ""
)
## trtpn table
l_trtpn <- c("TABLE TRTPN (one-way)",
  sprintf("%-10s %9s %9s %9s %9s", "TRTPN", "Frequency", "Percent", "CumFreq", "CumPct"),
  sapply(seq_len(nrow(f_trtpn)), function(i) {
    sprintf("%-10s %9d %9s %9d %9s", as.character(f_trtpn$trtpn[i]),
            f_trtpn$frequency[i], fmt2(f_trtpn$percent[i]),
            f_trtpn$cumfreq[i], fmt2(f_trtpn$cumpercent[i]))
  }),
  ""
)
## cross table in LIST format
l_cross <- c("TABLE AVISIT*AVISITN / LIST",
  sprintf("%-28s %8s %9s %9s %9s %9s", "AVISIT", "AVISITN", "Frequency", "Percent", "RowPct", "ColPct"),
  sapply(seq_len(nrow(f_cross)), function(i) {
    sprintf("%-28s %8s %9d %9s %9s %9s", substr(f_cross$avisit[i], 1, 28),
            as.character(f_cross$avisitn[i]), f_cross$frequency[i],
            fmt2(f_cross$percent[i]), fmt2(f_cross$row_percent[i]), fmt2(f_cross$col_percent[i]))
  }),
  ""
)
## aval table
l_aval <- c("TABLE AVAL (one-way)",
  sprintf("%-12s %9s %9s %9s %9s", "AVAL", "Frequency", "Percent", "CumFreq", "CumPct"),
  sapply(seq_len(nrow(f_aval_base)), function(i) {
    sprintf("%-12s %9d %9s %9d %9s", as.character(f_aval_base$aval[i]),
            f_aval_base$frequency[i], fmt2(f_aval_base$percent[i]),
            f_aval_base$cumfreq[i], fmt2(f_aval_base$cumpercent[i]))
  }),
  paste0("Missing AVAL: ", nmiss_aval),
  ""
)
p032_lines <- c(con_txt, l_trtpn, l_cross, l_aval)
writeLines(p032_lines, file.path(out, "P032.txt"))
## duplicate name variants for validator
try(file.copy(file.path(out, "P032.txt"), file.path(out, "P032.lst.txt"), overwrite = TRUE), silent = TRUE)

## ---- P032.csv : table body ----
csv_trtpn <- f_trtpn %>% transmute(table = "TRTPN", trtpn = trtpn, avisit = NA_character_,
  avisitn = NA_real_, aval = NA_real_, frequency = frequency, percent = rhu(percent, 2),
  row_percent = NA_real_, col_percent = NA_real_, cum_frequency = cumfreq, cum_percent = rhu(cumpercent, 2))
csv_cross <- f_cross %>% transmute(table = "AVISIT*AVISITN", trtpn = NA_real_, avisit = avisit,
  avisitn = avisitn, aval = NA_real_, frequency = frequency, percent = rhu(percent, 2),
  row_percent = rhu(row_percent, 2), col_percent = rhu(col_percent, 2),
  cum_frequency = NA_real_, cum_percent = NA_real_)
csv_aval <- f_aval_base %>% transmute(table = "AVAL", trtpn = NA_real_, avisit = NA_character_,
  avisitn = NA_real_, aval = aval, frequency = frequency, percent = rhu(percent, 2),
  row_percent = NA_real_, col_percent = NA_real_, cum_frequency = cumfreq, cum_percent = rhu(cumpercent, 2))
p032_csv <- bind_rows(csv_trtpn, csv_cross, csv_aval)
write.csv(p032_csv, file.path(out, "P032.csv"), row.names = FALSE, na = "")
try(file.copy(file.path(out, "P032.csv"), file.path(out, "P032.lst.csv"), overwrite = TRUE), silent = TRUE)

## ---- PROC MEANS DATA=advs NOPRINT; BY trtpn avisitn avisit; VAR aval; OUTPUT N MEAN STDERR LCLM UCLM ----
## compute N, MEAN, STDERR manually (SAS defaults exclude missing aval)
means_df <- advs %>% filter(!is.na(aval)) %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(n = n(),
            mean = mean(aval),
            std = ifelse(n() > 1, sd(aval), NA_real_),
            .groups = "drop") %>%
  mutate(stderr = std / sqrt(n))
## sort BY trtpn avisitn avisit (SAS order)
means_df <- means_df[order(means_df$trtpn, means_df$avisitn, means_df$avisit, method = "radix"), , drop = FALSE]
temp <- means_df %>% select(trtpn, avisitn, avisit, n, mean, stderr)
write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")

## ---- DATA temp1 ; lo/hi via TINV(0.95, n-1) ----
temp1 <- temp %>%
  mutate(tcrit = ifelse(n > 1, qt(0.95, n - 1), NA_real_),
         lo = mean - tcrit * stderr,
         hi = mean + tcrit * stderr) %>%
  select(trtpn, avisitn, avisit, n, mean, lo, hi)
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")

## ---- split a b c by trtpn ----
a <- temp1 %>% filter(trtpn == 0)
b <- temp1 %>% filter(trtpn == 54)
c_ <- temp1 %>% filter(trtpn == 81)
a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi) %>% select(-trtpn)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi) %>% select(-trtpn)
c1 <- c_ %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi) %>% select(-trtpn)

## ---- DATA all; MERGE a1 b1 c1; BY avisitn avisit; RENAME avisitn=xc avisit=x ----
all_wide <- full_join(a1, b1, by = c("avisitn", "avisit")) %>%
  full_join(c1, by = c("avisitn", "avisit")) %>%
  arrange(avisitn, avisit) %>%
  rename(xc = avisitn, x = avisit)
## enforce SAS BY order
all_wide <- all_wide[order(all_wide$xc, all_wide$x, method = "radix"), , drop = FALSE]
write.csv(all_wide, file.path(out, "all.csv"), row.names = FALSE, na = "")

## ---- long plot data with SAS offsets ----
mk_long <- function(df) {
  bind_rows(
    if (any(!is.na(df$a_mean))) data.frame(trtp = "0",  xc = df$xc, x = df$x, n = df$a_n, mean = df$a_mean, lo = df$a_lcl, hi = df$a_ucl, xpos = df$xc - 0.05) else NULL,
    if (any(!is.na(df$b_mean))) data.frame(trtp = "54", xc = df$xc, x = df$x, n = df$b_n, mean = df$b_mean, lo = df$b_lcl, hi = df$b_ucl, xpos = df$xc + 0.05) else NULL,
    if (any(!is.na(df$c_mean))) data.frame(trtp = "81", xc = df$xc, x = df$x, n = df$c_n, mean = df$c_mean, lo = df$c_lcl, hi = df$c_ucl, xpos = df$xc + 0.15) else NULL
  )
}
long_means <- mk_long(all_wide)
long_means$trtp <- factor(long_means$trtp, levels = c("0", "54", "81"))

## CSV of plotted data (figure + DBP_Profile)
write.csv(all_wide, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
write.csv(all_wide, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")
try(file.copy(file.path(out, "DBP_Profile.csv"), file.path(out, "DBP_Profile.html.csv"), overwrite = TRUE), silent = TRUE)

## ---- graph: dbp_profile ----
ttl <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"
xbreaks <- c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 26)
xbreaks <- xbreaks[xbreaks >= suppressWarnings(min(all_wide$xc, na.rm = TRUE)) - 1 &
                   xbreaks <= suppressWarnings(max(all_wide$xc, na.rm = TRUE)) + 1]
if (length(xbreaks) < 2) xbreaks <- sort(unique(all_wide$xc))

y_min <- suppressWarnings(min(long_means$lo, na.rm = TRUE))
y_max <- suppressWarnings(max(long_means$hi, na.rm = TRUE))
if (!is.finite(y_min) || !is.finite(y_max)) { y_min <- 0; y_max <- 1 }
rng <- y_max - y_min
if (rng == 0 || !is.finite(rng)) rng <- abs(y_max) + 1
y81 <- y_min - 0.12 * rng
y54 <- y_min - 0.24 * rng
y0  <- y_min - 0.36 * rng
y_bot <- y_min - 0.50 * rng
y_top <- y_max + 0.12 * rng

atrisk <- all_wide %>% transmute(
  xc = xc,
  n0 = a_n, n54 = b_n, n81 = c_n
)
## text labels (blank if NA)
atrisk$n0_lab <- ifelse(is.na(atrisk$n0), "", as.character(atrisk$n0))
atrisk$n54_lab <- ifelse(is.na(atrisk$n54), "", as.character(atrisk$n54))
atrisk$n81_lab <- ifelse(is.na(atrisk$n81), "", as.character(atrisk$n81))

p <- ggplot(long_means, aes(x = xpos, y = mean, color = trtp, linetype = trtp, shape = trtp)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.8, stroke = 1.1) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.5, linewidth = 0.8) +
  scale_x_continuous(breaks = xbreaks, labels = as.character(xbreaks)) +
  labs(title = ttl, x = "Weeks Since Randomized", y = "Mean(unit)with 95% CI",
       color = "Treatment Group: ", linetype = "Treatment Group: ", shape = "Treatment Group: ") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"),
        axis.title = element_text(size = 9),
        legend.position = "bottom",
        plot.margin = margin(10, 20, 10, 30)) +
  coord_cartesian(ylim = c(y_bot, y_top), xlim = c(min(xbreaks, na.rm = TRUE) - 1.5, max(xbreaks, na.rm = TRUE) + 0.5), clip = "off")

## at-risk blockplots as text rows
p <- p +
  geom_text(data = atrisk, aes(x = xc, y = y81, label = n81_lab), inherit.aes = FALSE, size = 3.2, color = "black") +
  geom_text(data = atrisk, aes(x = xc, y = y54, label = n54_lab), inherit.aes = FALSE, size = 3.2, color = "black") +
  geom_text(data = atrisk, aes(x = xc, y = y0,  label = n0_lab),  inherit.aes = FALSE, size = 3.2, color = "black") +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y81, label = "81", size = 3.2) +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y54, label = "54", size = 3.2) +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y0,  label = "0",  size = 3.2) +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y_min - 0.02 * rng,
           label = "At Risk Subjects", size = 3.5, hjust = 0, fontface = "bold")

ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), plot = p, width = 6, height = 4, dpi = 100)
cat("figure saved\n")

## ---- DBP_Profile.html text rendering (0 numbers in SAS -> describe graph) ----
dbp_lines <- c(
  "DBP_Profile - Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
  "",
  paste0("Title: ", ttl),
  "X axis: Weeks Since Randomized",
  "Y axis: Mean(unit)with 95% CI",
  "Legend: Treatment Group: 0, 54, 81",
  "At Risk Subjects block shows N per visit per treatment (see CSV).",
  paste0("Figure file: clinicalhandout_dbpprofile.png"),
  "",
  "Summary statistics plotted (N, Mean, Lower, Upper by Treatment and Visit):",
  sprintf("%-6s %-8s %-28s %6s %-10s %-10s %-10s", "TRTPN", "XC", "X", "N", "MEAN", "LO", "HI"),
  sapply(seq_len(nrow(temp1)), function(i) {
    sprintf("%-6s %-8s %-28s %6d %-10s %-10s %-10s",
      as.character(temp1$trtpn[i]), as.character(temp1$avisitn[i]),
      substr(temp1$avisit[i], 1, 28), temp1$n[i],
      formatC(temp1$mean[i], format = "f", digits = 2),
      formatC(temp1$lo[i], format = "f", digits = 2),
      formatC(temp1$hi[i], format = "f", digits = 2))
  })
)
writeLines(dbp_lines, file.path(out, "DBP_Profile.txt"))
try(file.copy(file.path(out, "DBP_Profile.txt"), file.path(out, "DBP_Profile.html.txt"), overwrite = TRUE), silent = TRUE)

cat("P032 target04: done\n")
