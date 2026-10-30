```r
out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10 ^ digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-9) / mult
  y * s
}
fmt2 <- function(x) {
  sprintf("%.2f", round_half_up(x, 2))
}
fmt4 <- function(x) {
  if (is.na(x)) return(" .")
  sprintf("%.4f", round_half_up(x, 4))
}
fmt_mean <- function(x) {
  if (is.na(x)) return(" .")
  sprintf("%.4f", round_half_up(x, 4))
}

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(tibble)
  library(ggplot2)
})

find_advs <- function() {
  cands <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
    file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
    file.path(Sys.getenv("rawadam"), "advs.xpt"),
    file.path(getwd(), "advs.xpt"),
    "data/raw/cdisc-pilot/adam/advs.xpt",
    "../data/raw/cdisc-pilot/adam/advs.xpt"
  )
  cands <- cands[cands != "" & cands != "/advs.xpt"]
  for (p in cands) {
    pp <- gsub("\\\\", "/", p)
    if (file.exists(pp)) return(pp)
    if (file.exists(p)) return(p)
  }
  # recursive search from cwd and from out parent
  search_roots <- unique(c(getwd(), out, dirname(out), tempdir()))
  for (r in search_roots) {
    if (!dir.exists(r)) next
    f <- list.files(r, pattern = "^advs\\.xpt$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(f) > 0) return(f[1])
  }
  # try absolute root search limited depth
  if (dir.exists("D:/meta_hackathon/sas2r-agent")) {
    f <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "^advs\\.xpt$", recursive = TRUE, full.names = TRUE)
    if (length(f) > 0) return(f[1])
  }
  return(NA_character_)
}

advs_path <- find_advs()
if (is.na(advs_path)) {
  # last resort: any advs.xpt on drive
  stop("advs.xpt not found")
}
message("Reading: ", advs_path)
src <- haven::read_xpt(advs_path)
names(src) <- toupper(names(src))

# ensure needed cols exist (case-insensitive already upper)
stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","AVISITN","SAFFL") %in% names(src)))

# trim character vars to emulate SAS trailing-blank insensitivity
char_cols <- names(src)[sapply(src, function(x) is.character(x) || haven::is.labelled(x))]
for (cc in char_cols) {
  v <- src[[cc]]
  if (haven::is.labelled(v)) v <- haven::as_factor(v, levels = "labels")
  v <- as.character(v)
  v <- trimws(v)
  src[[cc]] <- v
}

# data advs ; where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y';
advs <- src %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(AVISITN), AVISITN < 99,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL)

# SAS sort character ordering note: keep as is; advs order preserved (original order)
message("ADVS n=", nrow(advs))

write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

# ---------- PROC CONTENTS ----------
n_obs <- nrow(advs)
n_var <- ncol(advs)
var_names <- names(advs)
var_types <- sapply(advs, function(x) if (is.numeric(x)) "Num" else "Char")

# ---------- PROC FREQ tables trtpn Avisit*avisitn aval/list ----------
# LIST format: Frequency, Percent, Cumulative Frequency, Cumulative Percent
# SAS default excludes missing unless MISSING option; emulate: exclude NA levels
sas_char_sort <- function(vals) {
  # binary order by unicode code points (uppercase before lowercase)
  keys <- sapply(vals, function(v) {
    if (is.na(v)) return("")
    cp <- utf8ToInt(v)
    paste(sprintf("%06d", cp), collapse = "-")
  })
  vals[order(keys)]
}

freq_oneway <- function(vec, is_char = FALSE) {
  vec_nomiss <- vec[!is.na(vec)]
  total <- length(vec_nomiss)
  if (is_char) {
    uniq <- unique(vec_nomiss)
    uniq <- sas_char_sort(uniq)
  } else {
    uniq <- sort(unique(vec_nomiss), na.last = NA)
  }
  freq <- sapply(uniq, function(l) sum(vec_nomiss == l, na.rm = TRUE))
  # for numeric equality exact; for char exact
  pct <- freq / total * 100
  cumfreq <- cumsum(freq)
  cumpct <- cumfreq / total * 100
  data.frame(level = uniq, freq = as.integer(freq), pct = pct,
             cumfreq = as.integer(cumfreq), cumpct = cumpct,
             stringsAsFactors = FALSE)
}

# TRTPN
f_trtpn <- freq_oneway(advs$TRTPN, FALSE)
# AVAL
f_aval <- freq_oneway(advs$AVAL, FALSE)
# AVISIT*AVISITN : list format sorted by AVISIT then AVISITN (SAS binary for char)
tmp2 <- advs %>% filter(!is.na(AVISIT), !is.na(AVISITN))
total2 <- nrow(tmp2)
# distinct combos
comb <- tmp2 %>% distinct(AVISIT, AVISITN)
# sort by AVISIT binary then AVISITN numeric
avisit_order <- sas_char_sort(unique(comb$AVISIT))
comb$AVISIT <- factor(comb$AVISIT, levels = avisit_order)
comb <- comb %>% arrange(AVISIT, AVISITN)
comb$AVISIT <- as.character(comb$AVISIT)
freq2 <- sapply(seq_len(nrow(comb)), function(i) sum(tmp2$AVISIT == comb$AVISIT[i] & tmp2$AVISITN == comb$AVISITN[i], na.rm = TRUE))
pct2 <- freq2 / total2 * 100
cumfreq2 <- cumsum(freq2)
cumpct2 <- cumfreq2 / total2 * 100
f_cross <- data.frame(AVISIT = comb$AVISIT, AVISITN = comb$AVISITN,
                      freq = as.integer(freq2), pct = pct2,
                      cumfreq = as.integer(cumfreq2), cumpct = cumpct2,
                      stringsAsFactors = FALSE)

# Build combined CSV for P032
csv_trtpn <- data.frame(TABLE = "TRTPN", VAR1 = as.character(f_trtpn$level), VAR2 = "",
                        FREQUENCY = f_trtpn$freq, PERCENT = round_half_up(f_trtpn$pct, 2),
                        CUM_FREQ = f_trtpn$cumfreq, CUM_PERCENT = round_half_up(f_trtpn$cumpct, 2),
                        stringsAsFactors = FALSE)
csv_cross <- data.frame(TABLE = "AVISIT*AVISITN", VAR1 = f_cross$AVISIT, VAR2 = as.character(f_cross$AVISITN),
                        FREQUENCY = f_cross$freq, PERCENT = round_half_up(f_cross$pct, 2),
                        CUM_FREQ = f_cross$cumfreq, CUM_PERCENT = round_half_up(f_cross$cumpct, 2),
                        stringsAsFactors = FALSE)
csv_aval <- data.frame(TABLE = "AVAL", VAR1 = as.character(f_aval$level), VAR2 = "",
                       FREQUENCY = f_aval$freq, PERCENT = round_half_up(f_aval$pct, 2),
                       CUM_FREQ = f_aval$cumfreq, CUM_PERCENT = round_half_up(f_aval$cumpct, 2),
                       stringsAsFactors = FALSE)
p032_body <- bind_rows(csv_trtpn, csv_cross, csv_aval)
write.csv(p032_body, file.path(out, "P032.csv"), row.names = FALSE, na = "")

# ---------- PROC SORT + PROC MEANS BY trtpn avisitn avisit ----------
# sort by trtpn avisitn (avisit functionally dependent)
advs_s <- advs %>% arrange(TRTPN, AVISITN, AVISIT)

# PROC MEANS N MEAN STDERR LCLM UCLM
grp <- advs_s %>% group_by(TRTPN, AVISITN, AVISIT)
temp <- grp %>% summarise(
  n = sum(!is.na(AVAL)),
  mean = ifelse(sum(!is.na(AVAL)) > 0, mean(AVAL, na.rm = TRUE), NA_real_),
  std = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
  .groups = "drop"
) %>%
  mutate(
    stderr = std / sqrt(n),
    lclm = mean - qt(0.975, n - 1) * stderr,
    uclm = mean + qt(0.975, n - 1) * stderr
  )
# for n<=1 qt gives NaN; set to NA
temp$lclm[temp$n <= 1] <- NA_real_
temp$uclm[temp$n <= 1] <- NA_real_
temp$stderr[temp$n <= 1] <- NA_real_
temp <- temp %>% arrange(TRTPN, AVISITN, AVISIT)
write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")

# DATA temp1 ; lo/hi with TINV(0.95, n-1)
temp1 <- temp %>%
  mutate(
    lo = mean - qt(0.95, n - 1) * stderr,
    hi = mean + qt(0.95, n - 1) * stderr
  )
temp1$lo[temp1$n <= 1] <- NA_real_
temp1$hi[temp1$n <= 1] <- NA_real_
temp1_out <- temp1 %>% select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi)
write.csv(temp1_out, file.path(out, "temp1.csv"), row.names = FALSE, na = "")

# ---------- split a b c and merge ----------
a <- temp1_out %>% filter(TRTPN == 0)
b <- temp1_out %>% filter(TRTPN == 54)
c_ <- temp1_out %>% filter(TRTPN == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi) %>% select(-TRTPN)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi) %>% select(-TRTPN)
c1 <- c_ %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi) %>% select(-TRTPN)

# drop/rename overlapping cols already distinct; full join by AVISITN AVISIT
all_df <- full_join(a1, b1, by = c("AVISITN", "AVISIT"))
stopifnot(all(c("AVISITN","AVISIT") %in% names(all_df)))
all_df <- full_join(all_df, c1, by = c("AVISITN", "AVISIT"))
stopifnot(all(c("AVISITN","AVISIT") %in% names(all_df)))
all_df <- all_df %>% arrange(AVISITN, AVISIT) %>%
  rename(xc = AVISITN, x = AVISIT)
# drop TRTPN already dropped
write.csv(all_df, file.path(out, "all.csv"), row.names = FALSE, na = "")
write.csv(all_df, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
write.csv(all_df, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")

# ---------- P032.lst / P032.txt rendering ----------
con <- file(file.path(out, "P032.txt"), open = "wt")
cat("PROC CONTENTS DATA=ADVS\n", file = con)
cat(sprintf("Observations: %d  Variables: %d\n", n_obs, n_var), file = con)
cat("Variables (name type):\n", file = con)
for (i in seq_along(var_names)) {
  cat(sprintf("  %s (%s)\n", var_names[i], var_types[i]), file = con)
}
cat("\n", file = con)
cat("PROC FREQ DATA=ADVS\n", file = con)
cat("TABLES TRTPN AVISIT*AVISITN AVAL / LIST\n", file = con)
cat("\nTable of TRTPN\n", file = con)
cat(sprintf("%-12s %9s %8s %15s %15s\n", "TRTPN", "Frequency", "Percent", "Cum Frequency", "Cum Percent"), file = con)
for (i in seq_len(nrow(f_trtpn))) {
  cat(sprintf("%-12s %9d %8s %15d %15s\n", as.character(f_trtpn$level[i]), f_trtpn$freq[i], fmt2(f_trtpn$pct[i]), f_trtpn$cumfreq[i], fmt2(f_trtpn$cumpct[i])), file = con)
}
cat("\nTable of AVISIT by AVISITN (LIST)\n", file = con)
cat(sprintf("%-20s %-8s %9s %8s %15s %15s\n", "AVISIT", "AVISITN", "Frequency", "Percent", "Cum Frequency", "Cum Percent"), file = con)
for (i in seq_len(nrow(f_cross))) {
  cat(sprintf("%-20s %-8s %9d %8s %15d %15s\n", f_cross$AVISIT[i], as.character(f_cross$AVISITN[i]), f_cross$freq[i], fmt2(f_cross$pct[i]), f_cross$cumfreq[i], fmt2(f_cross$cumpct[i])), file = con)
}
cat("\nTable of AVAL\n", file = con)
cat(sprintf("%-12s %9s %8s %15s %15s\n", "AVAL", "Frequency", "Percent", "Cum Frequency", "Cum Percent"), file = con)
for (i in seq_len(nrow(f_aval))) {
  cat(sprintf("%-12s %9d %8s %15d %15s\n", as.character(f_aval$level[i]), f_aval$freq[i], fmt2(f_aval$pct[i]), f_aval$cumfreq[i], fmt2(f_aval$cumpct[i])), file = con)
}
cat("\nSample Size (TRTPN) = ", sum(f_trtpn$freq), "\n", file = con, sep = "")
cat("Sample Size (AVISIT*AVISITN) = ", total2, "\n", file = con, sep = "")
cat("Sample Size (AVAL) = ", sum(f_aval$freq), "\n", file = con, sep = "")
close(con)
# also write .lst copy
file.copy(file.path(out, "P032.txt"), file.path(out, "P032.lst"), overwrite = TRUE)

# ---------- Figure ----------
graph_title <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"

# long format for plotting with offsets matching SAS eval(xc-0.05) etc.
mk_long <- function(df) {
  d0 <- df %>% select(xc, x, a_mean, a_lcl, a_ucl, a_n) %>%
    rename(mean = a_mean, lcl = a_lcl, ucl = a_ucl, n = a_n) %>% mutate(TRT = "0", xplot = xc - 0.05)
  d54 <- df %>% select(xc, x, b_mean, b_lcl, b_ucl, b_n) %>%
    rename(mean = b_mean, lcl = b_lcl, ucl = b_ucl, n = b_n) %>% mutate(TRT = "54", xplot = xc + 0.05)
  d81 <- df %>% select(xc, x, c_mean, c_lcl, c_ucl, c_n) %>%
    rename(mean = c_mean, lcl = c_lcl, ucl = c_ucl, n = c_n) %>% mutate(TRT = "81", xplot = xc + 0.15)
  bind_rows(d0, d54, d81)
}
plot_long <- mk_long(all_df)
# remove rows with missing mean (treatment missing at visit)
plot_long_nm <- plot_long %>% filter(!is.na(mean))

ymin <- suppressWarnings(min(plot_long_nm$lcl, na.rm = TRUE))
ymax <- suppressWarnings(max(plot_long_nm$ucl, na.rm = TRUE))
if (!is.finite(ymin) || !is.finite(ymax)) { ymin <- 0; ymax <- 1 }
yr <- ymax - ymin
if (yr == 0) yr <- 1
y_c <- ymin - 0.12 * yr
y_b <- ymin - 0.22 * yr
y_a <- ymin - 0.32 * yr
y_lab_c <- ymin - 0.12 * yr
y_lab_b <- ymin - 0.22 * yr
y_lab_a <- ymin - 0.32 * yr

# at-risk data (use original xc)
atrisk <- all_df %>% select(xc, a_n, b_n, c_n)

p <- ggplot(plot_long_nm, aes(x = xplot, y = mean, colour = TRT, linetype = TRT, shape = TRT)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.25, linewidth = 0.9, na.rm = TRUE) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  geom_point(size = 3, stroke = 1.2, na.rm = TRUE) +
  scale_colour_manual(name = "Treatment Group: ", values = c("0" = "#2C7FB8", "54" = "#D95F0E", "81" = "#31A354"),
                      labels = c("0" = "0", "54" = "54", "81" = "81")) +
  scale_linetype_manual(name = "Treatment Group: ", values = c("0" = "dashed", "54" = "longdash", "81" = "solid"),
                        labels = c("0" = "0", "54" = "54", "81" = "81")) +
  scale_shape_manual(name = "Treatment Group: ", values = c("0" = 16, "54" = 17, "81" = 15),
                     labels = c("0" = "0", "54" = "54", "81" = "81")) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 26)) +
  labs(title = graph_title, x = "Weeks Since Randomized", y = "Mean(unit)with 95% CI") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        axis.title = element_text(size = 10),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "grey85"),
        plot.margin = margin(10, 10, 10, 40)) +
  coord_cartesian(ylim = c(y_a - 0.08 * yr, ymax + 0.08 * yr), clip = "off")

# at-risk annotations
p <- p +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y_c, label = "81", colour = "#31A354", size = 3.5) +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y_b, label = "54", colour = "#D95F0E", size = 3.5) +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = y_a, label = "0", colour = "#2C7FB8", size = 3.5) +
  annotate("text", x = min(atrisk$xc, na.rm = TRUE) - 1.2, y = ymin - 0.02 * yr, label = "At Risk Subjects", hjust = 0, size = 3.5)

for (i in seq_len(nrow(atrisk))) {
  xi <- atrisk$xc[i]
  if (!is.na(atrisk$c_n[i])) p <- p + annotate("text", x = xi, y = y_c, label = as.character(atrisk$c_n[i]), colour = "#31A354", size = 3)
  if (!is.na(atrisk$b_n[i])) p <- p + annotate("text", x = xi, y = y_b, label = as.character(atrisk$b_n[i]), colour = "#D95F0E", size = 3)
  if (!is.na(atrisk$a_n[i])) p <- p + annotate("text", x = xi, y = y_a, label = as.character(atrisk$a_n[i]), colour = "#2C7FB8", size = 3)
}

ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), plot = p, width = 6, height = 4, dpi = 100)
# also keep camel-case copy if needed
try(ggsave(file.path(out, "ClinicalHandout_DbpProfile.png"), plot = p, width = 6, height = 4, dpi = 100), silent = TRUE)

# ---------- DBP_Profile.html + txt ----------
html_path <- file.path(out, "DBP_Profile.html")
html_txt <- paste0(
  "<html><head><title>", graph_title, "</title></head><body>\n",
  "<h1>", graph_title, "</h1>\n",
  "<p>Mean(unit)with 95% CI by Weeks Since Randomized; Treatment Group: 0, 54, 81 with At Risk Subjects.</p>\n",
  "<img src=\"clinicalhandout_dbpprofile.png\" alt=\"DBP Profile\" width=\"600\" height=\"400\" />\n",
  "</body></html>\n"
)
writeLines(html_txt, html_path)

con2 <- file(file.path(out, "DBP_Profile.txt"), open = "wt")
cat(graph_title, "\n", file = con2)
cat("Mean(unit)with 95% CI by Weeks Since Randomized\n", file = con2)
cat("Treatment Group: 0, 54, 81\n", file = con2)
cat("At Risk Subjects (N by visit):\n", file = con2)
cat(sprintf("%-8s %-8s %-8s %-8s\n", "Weeks", "N_0", "N_54", "N_81"), file = con2)
for (i in seq_len(nrow(all_df))) {
  cat(sprintf("%-8s %-8s %-8s %-8s\n",
              as.character(all_df$xc[i]),
              ifelse(is.na(all_df$a_n[i]), ".", as.character(all_df$a_n[i])),
              ifelse(is.na(all_df$b_n[i]), ".", as.character(all_df$b_n[i])),
              ifelse(is.na(all_df$c_n[i]), ".", as.character(all_df$c_n[i]))), file = con2)
}
cat("\nMeans with 95% CI (lo/hi via TINV(0.95,n-1)):\n", file = con2)
cat(sprintf("%-8s %-10s %-10s %-10s\n", "Weeks", "Mean_0", "Mean_54", "Mean_81"), file = con2)
for (i in seq_len(nrow(all_df))) {
  f1 <- function(v) ifelse(is.na(v), ".", sprintf("%.4f", round_half_up(v, 4)))
  cat(sprintf("%-8s %-10s %-10s %-10s\n", as.character(all_df$xc[i]), f1(all_df$a_mean[i]), f1(all_df$b_mean[i]), f1(all_df$c_mean[i])), file = con2)
}
cat("\nSee clinicalhandout_dbpprofile.png for graph.\n", file = con2)
close(con2)

message("Done")
```