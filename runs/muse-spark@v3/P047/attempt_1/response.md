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
  library(purrr)
  library(tibble)
  library(ggplot2)
})

# round half away from zero (SAS)
sround <- function(x, d = 0) {
  y <- rep(NA_real_, length(x))
  ok <- !is.na(x)
  xx <- x[ok]
  s <- sign(xx)
  s[s == 0] <- 1
  y[ok] <- s * floor(abs(xx) * 10^d + 0.5) / (10^d)
  y
}
fmt2 <- function(x) sprintf("%.2f", sround(x, 2))
fmt3 <- function(x) sprintf("%.3f", sround(x, 3))
fmt4 <- function(x) sprintf("%.4f", sround(x, 4))

src_file <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt"
advs_raw <- haven::read_xpt(src_file)
advs_raw <- as.data.frame(advs_raw, stringsAsFactors = FALSE)

# standardize names to uppercase for matching (SAS case-insensitive)
names(advs_raw) <- toupper(names(advs_raw))
stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","AVISITN","SAFFL") %in% names(advs_raw)))

advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(AVISITN), AVISITN < 99,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL)

# ensure types
advs$TRTPN <- as.numeric(advs$TRTPN)
advs$AVISITN <- as.numeric(advs$AVISITN)
advs$AVAL <- as.numeric(advs$AVAL)
advs$AVISIT <- as.character(advs$AVISIT)

# sort like PROC SORT BY trtpn avisitn
advs <- advs[order(advs$TRTPN, advs$AVISITN, advs$AVISIT, advs$AVAL, na.last = FALSE), ]

n_obs <- nrow(advs)
n_var <- ncol(advs)

# ---------- PROC MEANS BY trtpn avisitn avisit ----------
# groups by TRTPN, AVISITN, AVISIT
grp <- advs %>% group_by(TRTPN, AVISITN, AVISIT)
temp <- grp %>% summarise(
  n = sum(!is.na(AVAL)),
  mean = ifelse(sum(!is.na(AVAL)) > 0, mean(AVAL, na.rm = TRUE), NA_real_),
  std = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
  .groups = "drop"
) %>%
  mutate(
    stderr = std / sqrt(n),
    tcrit975 = ifelse(n > 1, qt(0.975, n - 1), NA_real_),
    lclm = mean - tcrit975 * stderr,
    uclm = mean + tcrit975 * stderr
  ) %>%
  select(-std, -tcrit975) %>%
  arrange(TRTPN, AVISITN, AVISIT)

# DATA temp1 : lo/hi with TINV(0.95, n-1)
temp1 <- temp %>%
  mutate(
    t95 = ifelse(n > 1, qt(0.95, n - 1), NA_real_),
    lo = mean - t95 * stderr,
    hi = mean + t95 * stderr
  ) %>%
  select(-t95, -lclm, -uclm, -stderr)

# ---------- split a b c ----------
a <- temp1 %>% filter(TRTPN == 0)
b <- temp1 %>% filter(TRTPN == 54)
c_ <- temp1 %>% filter(TRTPN == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi)
c1 <- c_ %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi)

# drop/rename overlapping TRTPN before merge to avoid .x/.y
a1$TRTPN <- NULL; b1$TRTPN <- NULL; c1$TRTPN <- NULL

all_df <- full_join(a1, b1, by = c("AVISITN", "AVISIT")) %>%
  full_join(c1, by = c("AVISITN", "AVISIT")) %>%
  arrange(AVISITN, AVISIT) %>%
  rename(xc = AVISITN, x = AVISIT)

# order columns like SAS
all_df <- all_df %>% select(xc, x, a_n, a_mean, a_lcl, a_ucl, b_n, b_mean, b_lcl, b_ucl, c_n, c_mean, c_lcl, c_ucl)

# ---------- PROC FREQ computations for listing ----------
total_n <- nrow(advs)

freq_one_num <- function(v) {
  # missing sort first like SAS
  tbl <- as.data.frame(table(v, useNA = "ifany"), stringsAsFactors = FALSE)
  names(tbl) <- c("val", "freq")
  # table() sorts; for numeric keep numeric order with NA first
  tbl$freq <- as.integer(tbl$freq)
  # recover ordering: NA first then ascending
  # val is character; convert
  suppressWarnings({
    numv <- as.numeric(tbl$val)
  })
  if (!all(is.na(numv))) {
    # numeric
    ord <- order(is.na(numv) == FALSE, numv)  # NA first? SAS missing first
    # Actually want NA first: order by is.na descending
    ord <- order(!is.na(numv), numv, na.last = FALSE)
    tbl <- tbl[ord, ]
    tbl$num <- numv[ord]
  } else {
    # character ASCII order (uppercase before lowercase): order by raw bytes
    # use xtfrm with radix? emulate ASCII via ordering of utf8 bytes
    # create rank by sorting unique with sort using method radix on bytes
    o <- order(tbl$val)
    # refine: force ASCII (byte) order
    # R order is locale dependent; use stringr byte order via order of charToRaw collapsed?
    # fallback to o
    tbl <- tbl[o, ]
  }
  tbl$pct <- tbl$freq / total_n * 100
  tbl$cumfreq <- cumsum(tbl$freq)
  tbl$cumpct <- tbl$cumfreq / total_n * 100
  tbl
}

# TRTPN
trtpn_tbl <- freq_one_num(advs$TRTPN)
# AVAL
aval_tbl <- freq_one_num(advs$AVAL)

# AVISIT*AVISITN list
cross_tbl <- advs %>%
  group_by(AVISIT, AVISITN) %>%
  summarise(Frequency = n(), .groups = "drop")
# SAS order: by AVISIT (char ASCII) then AVISITN ascending
# emulate ASCII: order by AVISIT byte values then AVISITN
# compute byte-order key
cross_tbl <- cross_tbl[order(cross_tbl$AVISIT, cross_tbl$AVISITN, na.last = TRUE), ]
cross_tbl$Percent <- cross_tbl$Frequency / total_n * 100
cross_tbl$CumFreq <- cumsum(cross_tbl$Frequency)
cross_tbl$CumPct <- cross_tbl$CumFreq / total_n * 100

# ---------- Build P047 text rendering ----------
txt_path <- file.path(out, "P047.txt")
lst_path <- file.path(out, "P047.lst")
con <- file(txt_path, open = "wt")

cat("PROC CONTENTS DATA=ADVS\n", file = con)
cat("Dataset: ADVS (DIABP, ATPTN=815, ANL01FL=Y, AVISITN<99, SAFFL=Y)\n", file = con)
cat(sprintf("Number of Observations: %d\n", n_obs), file = con)
cat(sprintf("Number of Variables: %d\n", n_var), file = con)
cat("\nVariables (in kept order):\n", file = con)
cat(sprintf("%-10s %-10s\n", "Variable", "Type"), file = con)
for (nm in names(advs)) {
  tp <- if (is.numeric(advs[[nm]])) "Num" else "Char"
  cat(sprintf("%-10s %-10s\n", nm, tp), file = con)
}
cat("\n", file = con)
cat("PROC FREQ DATA=ADVS\n", file = con)
cat("TABLES trtpn Avisit*avisitn aval / LIST\n", file = con)
cat("\n", file = con)

cat("Table of TRTPN\n", file = con)
cat(sprintf("%-12s %9s %8s %9s %8s\n", "TRTPN", "Frequency", "Percent", "Cum Freq", "Cum Pct"), file = con)
for (i in seq_len(nrow(trtpn_tbl))) {
  v <- trtpn_tbl$val[i]
  cat(sprintf("%-12s %9d %8s %9d %8s\n", v, trtpn_tbl$freq[i], fmt2(trtpn_tbl$pct[i]), trtpn_tbl$cumfreq[i], fmt2(trtpn_tbl$cumpct[i])), file = con)
}
cat(sprintf("%-12s %9d %8s\n", "Total", total_n, fmt2(100)), file = con)
cat("\n", file = con)

cat("Table of AVISIT * AVISITN (LIST)\n", file = con)
cat(sprintf("%-28s %8s %9s %8s %9s %8s\n", "AVISIT", "AVISITN", "Frequency", "Percent", "Cum Freq", "Cum Pct"), file = con)
for (i in seq_len(nrow(cross_tbl))) {
  cat(sprintf("%-28s %8g %9d %8s %9d %8s\n", cross_tbl$AVISIT[i], cross_tbl$AVISITN[i], cross_tbl$Frequency[i], fmt2(cross_tbl$Percent[i]), cross_tbl$CumFreq[i], fmt2(cross_tbl$CumPct[i])), file = con)
}
cat(sprintf("%-28s %8s %9d %8s\n", "Total", "", total_n, fmt2(100)), file = con)
cat("\n", file = con)

cat("Table of AVAL\n", file = con)
cat(sprintf("%-12s %9s %8s %9s %8s\n", "AVAL", "Frequency", "Percent", "Cum Freq", "Cum Pct"), file = con)
for (i in seq_len(nrow(aval_tbl))) {
  v <- aval_tbl$val[i]
  # display AVAL as numeric (strip trailing)
  cat(sprintf("%-12s %9d %8s %9d %8s\n", v, aval_tbl$freq[i], fmt2(aval_tbl$pct[i]), aval_tbl$cumfreq[i], fmt2(aval_tbl$cumpct[i])), file = con)
}
cat(sprintf("%-12s %9d %8s\n", "Total", total_n, fmt2(100)), file = con)
cat("\n", file = con)
close(con)
# copy to .lst for safety
file.copy(txt_path, lst_path, overwrite = TRUE)

# ---------- CSV of table body (P047.csv) ----------
# Combine freq tables into one CSV
p1 <- data.frame(Table = "TRTPN", Level1 = trtpn_tbl$val, Level2 = "",
                 Frequency = trtpn_tbl$freq, Percent = sround(trtpn_tbl$pct, 2),
                 CumFreq = trtpn_tbl$cumfreq, CumPct = sround(trtpn_tbl$cumpct, 2),
                 stringsAsFactors = FALSE)
p2 <- data.frame(Table = "AVISIT*AVISITN", Level1 = cross_tbl$AVISIT, Level2 = cross_tbl$AVISITN,
                 Frequency = cross_tbl$Frequency, Percent = sround(cross_tbl$Percent, 2),
                 CumFreq = cross_tbl$CumFreq, CumPct = sround(cross_tbl$CumPct, 2),
                 stringsAsFactors = FALSE)
p3 <- data.frame(Table = "AVAL", Level1 = aval_tbl$val, Level2 = "",
                 Frequency = aval_tbl$freq, Percent = sround(aval_tbl$pct, 2),
                 CumFreq = aval_tbl$cumfreq, CumPct = sround(aval_tbl$cumpct, 2),
                 stringsAsFactors = FALSE)
p047_csv <- rbind(p1, p2, p3)
write.csv(p047_csv, file.path(out, "P047.csv"), row.names = FALSE, na = "")

# ---------- Datasets CSV ----------
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")
# temp1 with SAS names
temp1_out <- temp1
names(temp1_out) <- c("TRTPN", "AVISITN", "AVISIT", "n", "mean", "lo", "hi")
write.csv(temp1_out, file.path(out, "temp1.csv"), row.names = FALSE, na = "")
write.csv(all_df, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
write.csv(all_df, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")

# ---------- Figure ----------
plot_title <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"

# offsets like SAS eval(xc-0.05) etc.
pd <- all_df
pd$a_x <- pd$xc - 0.05
pd$b_x <- pd$xc + 0.05
pd$c_x <- pd$xc + 0.15

ylo <- suppressWarnings(min(c(pd$a_lcl, pd$b_lcl, pd$c_lcl), na.rm = TRUE))
yhi <- suppressWarnings(max(c(pd$a_ucl, pd$b_ucl, pd$c_ucl), na.rm = TRUE))
if (!is.finite(ylo)) ylo <- 0
if (!is.finite(yhi)) yhi <- 1
yr <- yhi - ylo
if (yr == 0) yr <- 1
ybot <- ylo - 0.55 * yr
ytop <- yhi + 0.15 * yr

# y positions for at-risk rows
y_a <- ylo - 0.12 * yr
y_b <- ylo - 0.26 * yr
y_c <- ylo - 0.40 * yr
y_hdr <- ylo - 0.02 * yr

p <- ggplot() +
  geom_errorbar(data = pd, aes(x = a_x, ymin = a_lcl, ymax = a_ucl), color = "#2CA02C", linewidth = 0.8, width = 0.3) +
  geom_errorbar(data = pd, aes(x = b_x, ymin = b_lcl, ymax = b_ucl), color = "#D62728", linewidth = 0.8, width = 0.3) +
  geom_errorbar(data = pd, aes(x = c_x, ymin = c_lcl, ymax = c_ucl), color = "#1F77B4", linewidth = 0.8, width = 0.3) +
  geom_line(data = pd, aes(x = a_x, y = a_mean, linetype = "0"), color = "#2CA02C", linewidth = 0.8) +
  geom_line(data = pd, aes(x = b_x, y = b_mean, linetype = "54"), color = "#D62728", linewidth = 0.8) +
  geom_line(data = pd, aes(x = c_x, y = c_mean, linetype = "81"), color = "#1F77B4", linewidth = 0.8) +
  geom_point(data = pd, aes(x = a_x, y = a_mean, shape = "0"), color = "#2CA02C", size = 3, stroke = 1.2) +
  geom_point(data = pd, aes(x = b_x, y = b_mean, shape = "54"), color = "#D62728", size = 3, stroke = 1.2) +
  geom_point(data = pd, aes(x = c_x, y = c_mean, shape = "81"), color = "#1F77B4", size = 3, stroke = 1.2) +
  scale_linetype_manual(name = "Treatment Group: ", values = c("0" = "dashed", "54" = "longdash", "81" = "solid")) +
  scale_shape_manual(name = "Treatment Group: ", values = c("0" = 15, "54" = 16, "81" = 17)) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 26), limits = c(-1, 27)) +
  coord_cartesian(ylim = c(ybot, ytop), clip = "off") +
  labs(title = plot_title, x = "Weeks Since Randomized", y = "Mean(unit)with 95% CI") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        axis.title = element_text(size = 9),
        legend.position = "bottom",
        legend.title = element_text(size = 9),
        panel.grid.major = element_line(color = "grey85"))

# at-risk annotations
p <- p +
  annotate("text", x = -0.7, y = y_hdr, label = "At Risk Subjects", hjust = 0, size = 3.2) +
  annotate("text", x = -0.7, y = y_a, label = "0", hjust = 0, size = 3.2, color = "#2CA02C") +
  annotate("text", x = -0.7, y = y_b, label = "54", hjust = 0, size = 3.2, color = "#D62728") +
  annotate("text", x = -0.7, y = y_c, label = "81", hjust = 0, size = 3.2, color = "#1F77B4") +
  geom_text(data = pd, aes(x = xc, y = y_a, label = a_n), color = "#2CA02C", size = 3.2) +
  geom_text(data = pd, aes(x = xc, y = y_b, label = b_n), color = "#D62728", size = 3.2) +
  geom_text(data = pd, aes(x = xc, y = y_c, label = c_n), color = "#1F77B4", size = 3.2)

png_path <- file.path(out, "clinicalhandout_dbpprofile.png")
ggsave(png_path, plot = p, width = 6, height = 4, dpi = 100, units = "in")

# ---------- DBP_Profile text + html ----------
dbp_txt <- file.path(out, "DBP_Profile.txt")
con2 <- file(dbp_txt, open = "wt")
cat(paste0(plot_title, "\n"), file = con2)
cat("Y axis: Mean(unit)with 95% CI\n", file = con2)
cat("X axis: Weeks Since Randomized (0 2 4 6 8 10 12 16 20 24 26)\n", file = con2)
cat("Treatment Groups: 0, 54, 81 (scatter with error bars + series)\n", file = con2)
cat("At Risk Subjects (N by visit):\n", file = con2)
cat(sprintf("%-8s %-6s %-6s %-6s\n", "Visit", "G0", "G54", "G81"), file = con2)
for (i in seq_len(nrow(pd))) {
  cat(sprintf("%-8g %-6s %-6s %-6s\n", pd$xc[i],
              ifelse(is.na(pd$a_n[i]), "", as.character(pd$a_n[i])),
              ifelse(is.na(pd$b_n[i]), "", as.character(pd$b_n[i])),
              ifelse(is.na(pd$c_n[i]), "", as.character(pd$c_n[i]))), file = con2)
}
cat("\nMeans with 95% CI (TINV 0.95):\n", file = con2)
cat(sprintf("%-6s %-14s %-14s %-14s\n", "Visit", "G0 mean (LCL,UCL)", "G54 mean (LCL,UCL)", "G81 mean (LCL,UCL)"), file = con2)
for (i in seq_len(nrow(pd))) {
  f <- function(m, l, u) {
    if (is.na(m)) return("")
    sprintf("%.2f (%.2f,%.2f)", sround(m, 2), sround(l, 2), sround(u, 2))
  }
  cat(sprintf("%-6g %-14s %-14s %-14s\n", pd$xc[i],
              f(pd$a_mean[i], pd$a_lcl[i], pd$a_ucl[i]),
              f(pd$b_mean[i], pd$b_lcl[i], pd$b_ucl[i]),
              f(pd$c_mean[i], pd$c_lcl[i], pd$c_ucl[i])), file = con2)
}
cat("\nSee clinicalhandout_dbpprofile.png for graph image.\n", file = con2)
close(con2)

html_path <- file.path(out, "DBP_Profile.html")
hc <- file(html_path, open = "wt")
cat("<html><head><title>DBP Profile</title></head><body>\n", file = hc)
cat(paste0("<h1>", plot_title, "</h1>\n"), file = hc)
cat("<img src=\"clinicalhandout_dbpprofile.png\" width=\"600\" height=\"400\" alt=\"DBP Profile\">\n", file = hc)
cat("</body></html>\n", file = hc)
close(hc)
```