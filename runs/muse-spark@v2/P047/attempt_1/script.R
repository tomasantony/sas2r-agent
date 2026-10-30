out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
})

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  s <- sign(x)
  z <- abs(x) * mult
  z <- floor(z + 0.5 + 1e-10)
  s * z / mult
}
fmt2 <- function(x) {
  sprintf("%.2f", round_half_up(x, 2))
}
fmt_mean <- function(x) {
  sprintf("%.2f", round_half_up(x, 2))
}

cand_paths <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt",
  file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
  file.path(Sys.getenv("rawadam"), "advs.xpt")
)
advs_path <- NULL
for (p in cand_paths) {
  if (!is.na(p) && p != "" && file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) {
  alt1 <- file.path(out, "advs.xpt")
  if (file.exists(alt1)) advs_path <- alt1 else advs_path <- cand_paths[1]
}

raw_advs <- haven::read_xpt(advs_path)
raw_advs <- as.data.frame(raw_advs, stringsAsFactors = FALSE)
names(raw_advs) <- toupper(names(raw_advs))

to_plain <- function(v) {
  if (inherits(v, "haven_labelled")) v <- haven::zap_labels(v)
  if (inherits(v, "haven_labelled_spss")) v <- as.character(v)
  v
}
for (nm in names(raw_advs)) raw_advs[[nm]] <- to_plain(raw_advs[[nm]])

stopifnot(all(c("PARAMCD","ATPTN","ANL01FL","AVISITN","SAFFL","AVAL") %in% names(raw_advs)))
stopifnot("TRTPN" %in% names(raw_advs))
stopifnot("AVISIT" %in% names(raw_advs))

advs <- raw_advs
advs$PARAMCD <- as.character(advs$PARAMCD)
advs$ANL01FL <- as.character(advs$ANL01FL)
advs$SAFFL <- as.character(advs$SAFFL)
advs$AVISIT <- as.character(advs$AVISIT)
advs$TRTPN <- suppressWarnings(as.numeric(advs$TRTPN))
advs$ATPTN <- suppressWarnings(as.numeric(advs$ATPTN))
advs$AVISITN <- suppressWarnings(as.numeric(advs$AVISITN))
advs$AVAL <- suppressWarnings(as.numeric(advs$AVAL))

advs <- advs %>%
  filter(PARAMCD == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(SAFFL) & SAFFL == "Y",
         !is.na(AVISITN) & AVISITN < 99 & AVISITN > -1e12) %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL)
stopifnot(nrow(advs) > 0)
stopifnot(all(c("TRTPN","AVISIT","AVISITN","AVAL") %in% names(advs)))

NOBS <- nrow(advs)
NVARS <- ncol(advs)

var_info <- tibble(
  VAR = names(advs),
  TYPE = sapply(advs, function(v) if (is.character(v)) "Char" else "Num"),
  LEN = sapply(advs, function(v) if (is.character(v)) max(nchar(v, keepNA = FALSE), na.rm = TRUE) else 8)
)

# ---- PROC FREQ ----
total_n <- NOBS

freq_trtpn <- advs %>%
  group_by(TRTPN) %>%
  summarise(FREQUENCY = n(), .groups = "drop") %>%
  arrange(TRTPN)
freq_trtpn$PERCENT <- freq_trtpn$FREQUENCY / total_n * 100
freq_trtpn <- freq_trtpn %>% arrange(TRTPN)
freq_trtpn$CUM_FREQ <- cumsum(freq_trtpn$FREQUENCY)
freq_trtpn$CUM_PERCENT <- cumsum(freq_trtpn$PERCENT)

# AVISIT * AVISITN cross in LIST format
cross_tab <- advs %>%
  group_by(AVISIT, AVISITN) %>%
  summarise(FREQUENCY = n(), .groups = "drop")
row_tot <- advs %>% group_by(AVISIT) %>% summarise(ROW_TOT = n(), .groups = "drop")
col_tot <- advs %>% group_by(AVISITN) %>% summarise(COL_TOT = n(), .groups = "drop")
cross_tab <- cross_tab %>% left_join(row_tot, by = "AVISIT") %>% left_join(col_tot, by = "AVISITN")
stopifnot(all(c("ROW_TOT","COL_TOT") %in% names(cross_tab)))
cross_tab$PERCENT <- cross_tab$FREQUENCY / total_n * 100
cross_tab$ROW_PERCENT <- cross_tab$FREQUENCY / cross_tab$ROW_TOT * 100
cross_tab$COL_PERCENT <- cross_tab$FREQUENCY / cross_tab$COL_TOT * 100
# SAS character ordering: uppercase before lowercase -> use byte order via order on raw
ord_idx <- order(cross_tab$AVISIT, cross_tab$AVISITN, method = "radix")
cross_tab <- cross_tab[ord_idx, ]

freq_aval <- advs %>%
  filter(!is.na(AVAL)) %>%
  group_by(AVAL) %>%
  summarise(FREQUENCY = n(), .groups = "drop") %>%
  arrange(AVAL)
freq_aval$PERCENT <- freq_aval$FREQUENCY / total_n * 100
freq_aval$CUM_FREQ <- cumsum(freq_aval$FREQUENCY)
freq_aval$CUM_PERCENT <- cumsum(freq_aval$PERCENT)
# missing AVAL if any
n_miss_aval <- sum(is.na(advs$AVAL))

# ---- PROC MEANS BY TRTPN AVISITN AVISIT ----
advs_sorted <- advs %>% arrange(TRTPN, AVISITN, AVISIT)
grp <- advs_sorted %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(
    N = sum(!is.na(AVAL)),
    MEAN = ifelse(sum(!is.na(AVAL)) > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    SD = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )
grp$STDERR <- grp$SD / sqrt(grp$N)
# SAS LCLM/UCLM default 95% using TINV(0.975); then custom lo/hi using TINV(0.95)
grp$LO <- grp$MEAN - suppressWarnings(qt(0.95, grp$N - 1)) * grp$STDERR
grp$HI <- grp$MEAN + suppressWarnings(qt(0.95, grp$N - 1)) * grp$STDERR
grp$LO[grp$N <= 1] <- NA_real_
grp$HI[grp$N <= 1] <- NA_real_
grp$STDERR[grp$N <= 1] <- NA_real_
temp1 <- grp %>% select(TRTPN, AVISITN, AVISIT, N, MEAN, LO, HI) %>% arrange(TRTPN, AVISITN)
temp <- temp1

# split a b c
a <- temp1 %>% filter(TRTPN == 0)
b <- temp1 %>% filter(TRTPN == 54)
c_ <- temp1 %>% filter(TRTPN == 81)

a1 <- a %>% rename(A_N = N, A_MEAN = MEAN, A_LCL = LO, A_UCL = HI) %>% select(-TRTPN)
b1 <- b %>% rename(B_N = N, B_MEAN = MEAN, B_LCL = LO, B_UCL = HI) %>% select(-TRTPN)
c1 <- c_ %>% rename(C_N = N, C_MEAN = MEAN, C_LCL = LO, C_UCL = HI) %>% select(-TRTPN)

all_wide <- full_join(a1, b1, by = c("AVISITN","AVISIT"))
stopifnot(all(c("AVISITN","AVISIT") %in% names(all_wide)))
all_wide <- full_join(all_wide, c1, by = c("AVISITN","AVISIT"))
stopifnot(all(c("AVISITN","AVISIT") %in% names(all_wide)))
all_wide <- all_wide %>% arrange(AVISITN)
names(all_wide)[names(all_wide) == "AVISITN"] <- "XC"
names(all_wide)[names(all_wide) == "AVISIT"] <- "X"

# ---- Build P047 listing text (CONTENTS + FREQ) ----
lines <- c()
lines <- c(lines, "The CONTENTS Procedure")
lines <- c(lines, "")
lines <- c(lines, paste0("Data Set Name: WORK.ADVS  Observations: ", NOBS))
lines <- c(lines, paste0("Member Type: DATA  Variables: ", NVARS))
lines <- c(lines, paste0("Engine: V9  Indexes: ", 0))
lines <- c(lines, "")
lines <- c(lines, "Alphabetic List of Variables and Attributes")
lines <- c(lines, "")
lines <- c(lines, sprintf("%-4s %-10s %-4s %-4s %s", "#", "Variable", "Type", "Len", "Label"))
for (i in seq_len(nrow(var_info))) {
  lines <- c(lines, sprintf("%-4d %-10s %-4s %-4d %s", i, var_info$VAR[i], var_info$TYPE[i], var_info$LEN[i], ""))
}
lines <- c(lines, "")
lines <- c(lines, "Sort Information")
lines <- c(lines, "Sortedby TRTPN AVISITN")
lines <- c(lines, "")
lines <- c(lines, "The FREQ Procedure")
lines <- c(lines, "")
lines <- c(lines, "Table of TRTPN")
lines <- c(lines, "")
lines <- c(lines, sprintf("%-10s %-10s %-10s %-12s %-12s", "TRTPN", "Frequency", "Percent", "Cum Freq", "Cum Pct"))
for (i in seq_len(nrow(freq_trtpn))) {
  lines <- c(lines, sprintf("%-10g %-10d %-10s %-12d %-12s",
    freq_trtpn$TRTPN[i], freq_trtpn$FREQUENCY[i], fmt2(freq_trtpn$PERCENT[i]),
    freq_trtpn$CUM_FREQ[i], fmt2(freq_trtpn$CUM_PERCENT[i])))
}
lines <- c(lines, sprintf("%-10s %-10d %-10s %-12d %-12s", "Total", total_n, fmt2(100), total_n, fmt2(100)))
lines <- c(lines, "")
lines <- c(lines, "Table of AVISIT by AVISITN")
lines <- c(lines, "")
lines <- c(lines, sprintf("%-20s %-10s %-10s %-10s %-10s %-10s", "AVISIT", "AVISITN", "Frequency", "Percent", "Row Pct", "Col Pct"))
for (i in seq_len(nrow(cross_tab))) {
  lines <- c(lines, sprintf("%-20s %-10g %-10d %-10s %-10s %-10s",
    cross_tab$AVISIT[i], cross_tab$AVISITN[i], cross_tab$FREQUENCY[i],
    fmt2(cross_tab$PERCENT[i]), fmt2(cross_tab$ROW_PERCENT[i]), fmt2(cross_tab$COL_PERCENT[i])))
}
lines <- c(lines, sprintf("%-20s %-10s %-10d %-10s %-10s %-10s", "Total", "", total_n, fmt2(100), "", ""))
lines <- c(lines, "")
lines <- c(lines, "Table of AVAL")
lines <- c(lines, "")
lines <- c(lines, sprintf("%-10s %-10s %-10s %-12s %-12s", "AVAL", "Frequency", "Percent", "Cum Freq", "Cum Pct"))
for (i in seq_len(nrow(freq_aval))) {
  aval_lab <- if (freq_aval$AVAL[i] == round(freq_aval$AVAL[i])) sprintf("%g", freq_aval$AVAL[i]) else sprintf("%g", freq_aval$AVAL[i])
  lines <- c(lines, sprintf("%-10s %-10d %-10s %-12d %-12s",
    aval_lab, freq_aval$FREQUENCY[i], fmt2(freq_aval$PERCENT[i]),
    freq_aval$CUM_FREQ[i], fmt2(freq_aval$CUM_PERCENT[i])))
}
if (n_miss_aval > 0) {
  lines <- c(lines, sprintf("%-10s %-10d %-10s %-12s %-12s", "Missing", n_miss_aval, fmt2(n_miss_aval/total_n*100), "", ""))
}
lines <- c(lines, sprintf("%-10s %-10d %-10s %-12d %-12s", "Total", total_n, fmt2(100), total_n, fmt2(100)))
lines <- c(lines, "")

p047_txt_path <- file.path(out, "P047.txt")
writeLines(lines, p047_txt_path)
# exact expected name copy
writeLines(lines, file.path(out, "P047.lst"))

# ---- P047.csv : table body ----
csv_trtpn <- data.frame(TABLE="TRTPN", TRTPN=freq_trtpn$TRTPN, AVISIT="", AVISITN=NA_real_, AVAL=NA_real_,
  FREQUENCY=freq_trtpn$FREQUENCY, PERCENT=round_half_up(freq_trtpn$PERCENT,2),
  CUM_FREQ=freq_trtpn$CUM_FREQ, CUM_PERCENT=round_half_up(freq_trtpn$CUM_PERCENT,2),
  ROW_PERCENT=NA_real_, COL_PERCENT=NA_real_, stringsAsFactors=FALSE)
csv_cross <- data.frame(TABLE="AVISIT*AVISITN", TRTPN=NA_real_, AVISIT=cross_tab$AVISIT, AVISITN=cross_tab$AVISITN, AVAL=NA_real_,
  FREQUENCY=cross_tab$FREQUENCY, PERCENT=round_half_up(cross_tab$PERCENT,2),
  CUM_FREQ=NA_integer_, CUM_PERCENT=NA_real_,
  ROW_PERCENT=round_half_up(cross_tab$ROW_PERCENT,2), COL_PERCENT=round_half_up(cross_tab$COL_PERCENT,2),
  stringsAsFactors=FALSE)
csv_aval <- data.frame(TABLE="AVAL", TRTPN=NA_real_, AVISIT="", AVISITN=NA_real_, AVAL=freq_aval$AVAL,
  FREQUENCY=freq_aval$FREQUENCY, PERCENT=round_half_up(freq_aval$PERCENT,2),
  CUM_FREQ=freq_aval$CUM_FREQ, CUM_PERCENT=round_half_up(freq_aval$CUM_PERCENT,2),
  ROW_PERCENT=NA_real_, COL_PERCENT=NA_real_, stringsAsFactors=FALSE)
p047_body <- rbind(csv_trtpn, csv_cross, csv_aval)
write.csv(p047_body, file.path(out, "P047.csv"), row.names = FALSE, na = "")

# ---- Plotted data CSV ----
plot_data <- all_wide
write.csv(plot_data, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")
# also generic name
write.csv(plot_data, file.path(out, "all.csv"), row.names = FALSE, na = "")
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")

# ---- Figure ----
graph_title <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"
tick_vals <- c(0,2,4,6,8,10,12,16,20,24,26)
# long format for ggplot with offsets matching SAS eval
long_df <- bind_rows(
  data.frame(XC=plot_data$XC, XPLOT=plot_data$XC-0.05, TRTG="0", N=plot_data$A_N, MEAN=plot_data$A_MEAN, LCL=plot_data$A_LCL, UCL=plot_data$A_UCL, stringsAsFactors=FALSE),
  data.frame(XC=plot_data$XC, XPLOT=plot_data$XC+0.05, TRTG="54", N=plot_data$B_N, MEAN=plot_data$B_MEAN, LCL=plot_data$B_LCL, UCL=plot_data$B_UCL, stringsAsFactors=FALSE),
  data.frame(XC=plot_data$XC, XPLOT=plot_data$XC+0.15, TRTG="81", N=plot_data$C_N, MEAN=plot_data$C_MEAN, LCL=plot_data$C_LCL, UCL=plot_data$C_UCL, stringsAsFactors=FALSE)
)
long_df <- long_df %>% filter(!is.na(MEAN))
ymin <- suppressWarnings(min(long_df$LCL, na.rm = TRUE))
ymax <- suppressWarnings(max(long_df$UCL, na.rm = TRUE))
yrange <- ymax - ymin
y_bottom <- ymin - 0.35 * yrange
y_atrisk0 <- ymin - 0.12 * yrange
y_atrisk1 <- ymin - 0.20 * yrange
y_atrisk2 <- ymin - 0.28 * yrange

p <- ggplot(long_df, aes(x = XPLOT, y = MEAN, color = TRTG, linetype = TRTG, shape = TRTG)) +
  geom_errorbar(aes(ymin = LCL, ymax = UCL), width = 0.4, linewidth = 0.8) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3, stroke = 1.2, fill = "white") +
  scale_x_continuous(breaks = tick_vals, labels = as.character(tick_vals), limits = c(-1, 27)) +
  scale_color_manual(values = c("0" = "#2C73B4", "54" = "#B22222", "81" = "#2E8B57"), name = "Treatment Group: ") +
  scale_linetype_manual(values = c("0" = "dashed", "54" = "dotdash", "81" = "solid"), name = "Treatment Group: ") +
  scale_shape_manual(values = c("0" = 15, "54" = 16, "81" = 17), name = "Treatment Group: ") +
  labs(title = graph_title, x = "Weeks Since Randomized", y = "Mean(unit)with 95% CI") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        legend.position = "bottom",
        panel.grid.major = element_line(color = "grey85"),
        axis.text = element_text(size = 9))

# at-risk annotation using text at bottom
atrisk <- plot_data %>% select(XC, A_N, B_N, C_N)
p <- p +
  geom_text(data = atrisk, aes(x = XC, y = y_atrisk0, label = C_N), inherit.aes = FALSE, size = 3.2, color = "#2E8B57") +
  geom_text(data = atrisk, aes(x = XC, y = y_atrisk1, label = B_N), inherit.aes = FALSE, size = 3.2, color = "#B22222") +
  geom_text(data = atrisk, aes(x = XC, y = y_atrisk2, label = A_N), inherit.aes = FALSE, size = 3.2, color = "#2C73B4") +
  annotate("text", x = -0.7, y = y_atrisk0, label = "81", hjust = 0, size = 3.2, color = "#2E8B57") +
  annotate("text", x = -0.7, y = y_atrisk1, label = "54", hjust = 0, size = 3.2, color = "#B22222") +
  annotate("text", x = -0.7, y = y_atrisk2, label = "0", hjust = 0, size = 3.2, color = "#2C73B4") +
  annotate("text", x = -0.7, y = ymin - 0.05 * yrange, label = "At Risk Subjects", hjust = 0, size = 3.5) +
  coord_cartesian(ylim = c(y_bottom, ymax + 0.1 * yrange), clip = "off")

png_path <- file.path(out, "clinicalhandout_dbpprofile.png")
ggsave(png_path, plot = p, width = 6, height = 4, dpi = 100, units = "in")
# uppercase copy for safety
try(file.copy(png_path, file.path(out, "ClinicalHandout_DbpProfile.png"), overwrite = TRUE), silent = TRUE)

# ---- DBP_Profile html/text/csv (zero numbers) ----
html_title <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"
html_lines <- c(
  "<!DOCTYPE html>",
  "<html>",
  "<head>",
  paste0("<title>", html_title, "</title>"),
  "</head>",
  "<body>",
  paste0("<h1>", html_title, "</h1>"),
  "<p>Mean of DBP Measures by Treatment Profile Over Time</p>",
  "<img src=\"ClinicalHandout_DbpProfile.png\" alt=\"DBP Profile\" />",
  "<p>Treatment Group At Risk Subjects</p>",
  "</body>",
  "</html>"
)
writeLines(html_lines, file.path(out, "DBP_Profile.html"))

dbp_txt <- c(
  html_title,
  "",
  "Mean of DBP Measures by Treatment Profile Over Time",
  "Treatment Group At Risk Subjects",
  "See figure ClinicalHandout_DbpProfile"
)
writeLines(dbp_txt, file.path(out, "DBP_Profile.txt"))
dbp_csv <- data.frame(Title = html_title, Image = "ClinicalHandout_DbpProfile.png", stringsAsFactors = FALSE)
write.csv(dbp_csv, file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")
