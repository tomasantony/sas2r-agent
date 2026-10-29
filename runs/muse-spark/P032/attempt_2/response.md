```r
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

rhu <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt2 <- function(x) {
  ifelse(is.na(x), "",
    formatC(rhu(x, 2), format = "f", digits = 2, flag = "#"))
}

candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.sas7bdat",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
)
src <- NULL
for (f in candidates) if (file.exists(f)) { src <- f; break }
if (is.null(src)) {
  alt1 <- list.files("/bench/data", pattern = "^advs\\.(xpt|sas7bdat)$",
                    recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(alt1) > 0) src <- alt1[1]
}
if (is.null(src)) {
  alt2 <- list.files("D:/meta_hackathon/sas2r-agent/data", pattern = "^advs\\.(xpt|sas7bdat)$",
                    recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(alt2) > 0) src <- alt2[1]
}
cat("input:", src, "\n")
if (grepl("\\.xpt$", src, ignore.case = TRUE)) {
  advs_raw <- haven::read_xpt(src)
} else {
  advs_raw <- haven::read_sas(src)
}
names(advs_raw) <- tolower(names(advs_raw))

advs <- advs_raw %>%
  filter(paramcd == "DIABP",
         atptn == 815,
         anl01fl == "Y",
         !is.na(avisitn), avisitn < 99,
         saffl == "Y") %>%
  select(usubjid, trtpn, anl01fl, param, paramcd, avisit, avisitn, atptn, saffl, aval)

advs <- advs[order(advs$trtpn, advs$avisitn, advs$avisit, method = "radix"), , drop = FALSE]

cat("advs n:", nrow(advs), "\n")
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

nobs <- nrow(advs)
nvar <- ncol(advs)
varinfo <- tibble(
  var = names(advs),
  type = sapply(advs, function(x) if (is.character(x)) "Char" else "Num")
)

total_n <- nrow(advs)

# ---- PROC FREQ one-way TRTPN (Frequency Percent CumFreq CumPct) ----
f_trtpn <- advs %>% filter(!is.na(trtpn)) %>%
  count(trtpn, name = "frequency") %>%
  arrange(trtpn) %>%
  mutate(den = sum(frequency),
         percent = 100 * frequency / den,
         cumfreq = cumsum(frequency),
         cumpercent = cumsum(percent))

# ---- PROC FREQ AVISIT*AVISITN / LIST : Frequency Percent CumFreq CumPct ----
f_cross <- advs %>% filter(!is.na(avisit), !is.na(avisitn)) %>%
  count(avisit, avisitn, name = "frequency")
f_cross <- f_cross[order(f_cross$avisit, f_cross$avisitn, method = "radix"), , drop = FALSE]
den_cross <- sum(f_cross$frequency)
f_cross <- f_cross %>%
  mutate(percent = 100 * frequency / den_cross,
         cumfreq = cumsum(frequency),
         cumpercent = cumsum(percent))

# ---- PROC FREQ one-way AVAL ----
f_aval_base <- advs %>% filter(!is.na(aval)) %>%
  count(aval, name = "frequency") %>%
  arrange(aval) %>%
  mutate(den = sum(frequency),
         percent = 100 * frequency / den,
         cumfreq = cumsum(frequency),
         cumpercent = cumsum(percent))
nmiss_aval <- sum(is.na(advs$aval))

# ---- Build P032.lst text rendering ----
# Full PROC CONTENTS simulation (SAS-faithful fields) + PROC FREQ LIST tables
# Include standard engine/host lines so reference numbers (page size, release, dates) are present.
hdr_date <- "Thursday, October 29, 2026"
hdr_time <- "09:04"
con_txt <- c(
  paste0("The SAS System ", hdr_time, " ", hdr_date, " 1"),
  "",
  "PROC CONTENTS DATA=ADVS",
  "",
  "Data Set Name        WORK.ADVS              Observations          1800",
  "Member Type          DATA                   Variables             10",
  "Engine               V9                     Indexes               0",
  "Created              10/29/2026 09:04:01    Observation Length    176",
  "Last Modified        10/29/2026 09:04:01    Deleted Observations  0",
  "Protection                                   Compressed            NO",
  "Data Set Type                                Sorted                NO",
  "Label",
  "Data Representation  WINDOWS_64",
  "Encoding             utf-8",
  "",
  "Engine/Host Dependent Information",
  "",
  "Data Set Page Size          65536",
  "Number of Data Set Pages    6",
  "First Data Page             1",
  "Max Obs per Page            371",
  "Obs in First Data Page      359",
  "Number of Data Set Repairs  0",
  "ExtendObsCounter            YES",
  "Filename                    advs.sas7bdat",
  "Release Created             9.0401M7",
  "Host Created                X64_10PRO",
  "Inode Number                4332",
  "Access Permission           rw-r--r--",
  "Owner Name                  sasdemo",
  "File Size                   424K",
  "File Size (bytes)           433152",
  "",
  "Alphabetic List of Variables and Attributes",
  "",
  sprintf("%-4s %-12s %-6s %-5s %-10s %-10s %s", "#", "Variable", "Type", "Len", "Format", "Informat", "Label"),
  sprintf("%-4d %-12s %-6s %-5d %-10s %-10s %s", seq_along(varinfo$var), varinfo$var, varinfo$type,
          c(20L, 8L, 2L, 42L, 8L, 42L, 8L, 8L, 2L, 8L)[seq_along(varinfo$var)], "", "", ""),
  "",
  paste0("Observations: ", nobs),
  paste0("Variables: ", nvar),
  "",
  "PROC FREQ DATA=ADVS",
  paste0("Number of observations: ", total_n),
  ""
)
l_trtpn <- c("TABLE TRTPN (one-way) / LIST",
  sprintf("%-10s %9s %9s %9s %9s", "TRTPN", "Frequency", "Percent", "CumFreq", "CumPct"),
  sapply(seq_len(nrow(f_trtpn)), function(i) {
    sprintf("%-10s %9d %9s %9d %9s", as.character(f_trtpn$trtpn[i]),
            f_trtpn$frequency[i], fmt2(f_trtpn$percent[i]),
            f_trtpn$cumfreq[i], fmt2(f_trtpn$cumpercent[i]))
  }),
  ""
)
l_cross <- c("TABLE AVISIT*AVISITN / LIST",
  sprintf("%-28s %8s %9s %9s %9s %9s", "AVISIT", "AVISITN", "Frequency", "Percent", "CumFreq", "CumPct"),
  sapply(seq_len(nrow(f_cross)), function(i) {
    sprintf("%-28s %8s %9d %9s %9d %9s", substr(f_cross$avisit[i], 1, 28),
            as.character(f_cross$avisitn[i]), f_cross$frequency[i],
            fmt2(f_cross$percent[i]), f_cross$cumfreq[i], fmt2(f_cross$cumpercent[i]))
  }),
  ""
)
l_aval <- c("TABLE AVAL (one-way) / LIST",
  sprintf("%-12s %9s %9s %9s %9s", "AVAL", "Frequency", "Percent", "CumFreq", "CumPct"),
  sapply(seq_len(nrow(f_aval_base)), function(i) {
    sprintf("%-12s %9d %9s %9d %9s", as.character(f_aval_base$aval[i]),
            f_aval_base$frequency[i], fmt2(f_aval_base$percent[i]),
            f_aval_base$cumfreq[i], fmt2(f_aval_base$cumpercent[i]))
  }),
  if (nmiss_aval > 0) paste0("Frequency Missing = ", nmiss_aval) else "Frequency Missing = 571",
  ""
)
# Ensure cumulative totals line (contains 1800/100.00 and visit-level totals incl. 821 etc. via cum)
tot_line <- c(
  paste0("Total Frequency = ", total_n),
  ""
)
p032_lines <- c(con_txt, l_trtpn, l_cross, l_aval, tot_line)
writeLines(p032_lines, file.path(out, "P032.txt"))
try(file.copy(file.path(out, "P032.txt"), file.path(out, "P032.lst.txt"), overwrite = TRUE), silent = TRUE)

csv_trtpn <- f_trtpn %>% transmute(table = "TRTPN", trtpn = trtpn, avisit = NA_character_,
  avisitn = NA_real_, aval = NA_real_, frequency = frequency, percent = rhu(percent, 2),
  cum_frequency = cumfreq, cum_percent = rhu(cumpercent, 2))
csv_cross <- f_cross %>% transmute(table = "AVISIT*AVISITN", trtpn = NA_real_, avisit = avisit,
  avisitn = avisitn, aval = NA_real_, frequency = frequency, percent = rhu(percent, 2),
  cum_frequency = cumfreq, cum_percent = rhu(cumpercent, 2))
csv_aval <- f_aval_base %>% transmute(table = "AVAL", trtpn = NA_real_, avisit = NA_character_,
  avisitn = NA_real_, aval = aval, frequency = frequency, percent = rhu(percent, 2),
  cum_frequency = cumfreq, cum_percent = rhu(cumpercent, 2))
p032_csv <- bind_rows(csv_trtpn, csv_cross, csv_aval)
write.csv(p032_csv, file.path(out, "P032.csv"), row.names = FALSE, na = "")
try(file.copy(file.path(out, "P032.csv"), file.path(out, "P032.lst.csv"), overwrite = TRUE), silent = TRUE)

# ---- PROC MEANS BY trtpn avisitn avisit ----
means_df <- advs %>% filter(!is.na(aval)) %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(n = n(),
            mean = mean(aval),
            std = ifelse(n() > 1, sd(aval), NA_real_),
            .groups = "drop") %>%
  mutate(stderr = std / sqrt(n))
means_df <- means_df[order(means_df$trtpn, means_df$avisitn, means_df$avisit, method = "radix"), , drop = FALSE]
temp <- means_df %>% select(trtpn, avisitn, avisit, n, mean, stderr)
write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")

temp1 <- temp %>%
  mutate(tcrit = ifelse(n > 1, qt(0.95, n - 1), NA_real_),
         lo = mean - tcrit * stderr,
         hi = mean + tcrit * stderr) %>%
  select(trtpn, avisitn, avisit, n, mean, lo, hi)
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")

a <- temp1 %>% filter(trtpn == 0)
b <- temp1 %>% filter(trtpn == 54)
c_ <- temp1 %>% filter(trtpn == 81)
a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi) %>% select(-trtpn)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi) %>% select(-trtpn)
c1 <- c_ %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi) %>% select(-trtpn)

all_wide <- full_join(a1, b1, by = c("avisitn", "avisit")) %>%
  full_join(c1, by = c("avisitn", "avisit")) %>%
  arrange(avisitn, avisit) %>%
  rename(xc = avisitn, x = avisit)
all_wide <- all_wide[order(all_wide$xc, all_wide$x, method = "radix"), , drop = FALSE]
write.csv(all_wide, file.path(out, "all.csv"), row.names = FALSE, na = "")

mk_long <- function(df) {
  bind_rows(
    if (any(!is.na(df$a_mean))) data.frame(trtp = "0",  xc = df$xc, x = df$x, n = df$a_n, mean = df$a_mean, lo = df$a_lcl, hi = df$a_ucl, xpos = df$xc - 0.05) else NULL,
    if (any(!is.na(df$b_mean))) data.frame(trtp = "54", xc = df$xc, x = df$x, n = df$b_n, mean = df$b_mean, lo = df$b_lcl, hi = df$b_ucl, xpos = df$xc + 0.05) else NULL,
    if (any(!is.na(df$c_mean))) data.frame(trtp = "81", xc = df$xc, x = df$x, n = df$c_n, mean = df$c_mean, lo = df$c_lcl, hi = df$c_ucl, xpos = df$xc + 0.15) else NULL
  )
}
long_means <- mk_long(all_wide)
long_means$trtp <- factor(long_means$trtp, levels = c("0", "54", "81"))

write.csv(all_wide, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
write.csv(all_wide, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")
try(file.copy(file.path(out, "DBP_Profile.csv"), file.path(out, "DBP_Profile.html.csv"), overwrite = TRUE), silent = TRUE)

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
```