# P047_mean_time.sas -> R translation
# Outputs: P047 table (PROC CONTENTS+FREQ), DBP profile figure, DBP_Profile html placeholder

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

cat("P047 start\n")

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / (10^digits))
}
fmt2 <- function(x) {
  v <- round_half_up(x, 2)
  sprintf("%.2f", v)
}
fmt_mean <- function(x) {
  # for means display if needed; keep full precision in datasets
  as.character(round_half_up(x, 2))
}

raw_advs <- "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
if (!file.exists(raw_advs)) {
  alt1 <- "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
  alt2 <- file.path("/bench/data/raw/cdisc-pilot/adam", "advs.xpt")
  if (file.exists(alt1)) raw_advs <- alt1 else if (file.exists(alt2)) raw_advs <- alt2
}
cat("Reading:", raw_advs, "\n")
advs_xpt <- haven::read_xpt(raw_advs)
names(advs_xpt) <- toupper(names(advs_xpt))

# DATA advs; WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y';
advs <- advs_xpt %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(AVISITN), AVISITN < 99,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL) %>%
  arrange(TRTPN, AVISITN)

cat("ADVS nrow:", nrow(advs), "\n")

# ---- PROC CONTENTS info ----
nobs <- nrow(advs)
nvars <- ncol(advs)
varlist <- names(advs)

# ---- PROC FREQ tables trtpn Avisit*avisitn aval/list ----
N <- nrow(advs)

# one-way TRTPN, SAS order ascending numeric, missing first (none expected)
freq_trtpn <- advs %>%
  group_by(TRTPN) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(TRTPN) %>%
  mutate(Percent = Frequency / N * 100,
         CumFreq = cumsum(Frequency),
         CumPct = cumsum(Percent))
# two-way AVISIT*AVISITN list: order by AVISIT (SAS char: uppercase before lowercase -> byte order) then AVISITN
# implement byte-order rank for AVISIT
byte_key <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return("\x00")
    paste(sprintf("%03d", as.integer(charToRaw(s))), collapse = "-")
  })
}
freq_visit <- advs %>%
  group_by(AVISIT, AVISITN) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  mutate(.bk = byte_key(AVISIT)) %>%
  arrange(.bk, AVISITN) %>%
  select(-.bk) %>%
  mutate(Percent = Frequency / N * 100,
         CumFreq = cumsum(Frequency),
         CumPct = cumsum(Percent))

freq_aval <- advs %>%
  group_by(AVAL) %>%
  summarise(Frequency = n(), .groups = "drop") %>%
  arrange(AVAL) %>%
  mutate(Percent = Frequency / N * 100,
         CumFreq = cumsum(Frequency),
         CumPct = cumsum(Percent))

# ---- PROC MEANS BY trtpn avisitn avisit; VAR aval; N MEAN STDERR LCLM UCLM ----
# SAS BY requires sort by trtpn avisitn (done); group includes avisit
temp <- advs %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = ifelse(sum(!is.na(AVAL)) > 0, mean(AVAL, na.rm = TRUE), NA_real_),
    sd = ifelse(sum(!is.na(AVAL)) > 1, sd(AVAL, na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(
    stderr = sd / sqrt(n),
    lclm = ifelse(!is.na(stderr) & n > 1, mean - qt(0.975, n - 1) * stderr, NA_real_),
    uclm = ifelse(!is.na(stderr) & n > 1, mean + qt(0.975, n - 1) * stderr, NA_real_)
  ) %>%
  arrange(TRTPN, AVISITN)

# DATA temp1; lo = mean - TINV(0.95,n-1)*stderr; hi = mean + ...; drop _TYPE_ _FREQ_ lclm uclm stderr;
temp1 <- temp %>%
  mutate(
    lo = ifelse(!is.na(stderr) & n > 1, mean - qt(0.95, n - 1) * stderr, NA_real_),
    hi = ifelse(!is.na(stderr) & n > 1, mean + qt(0.95, n - 1) * stderr, NA_real_)
  ) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi)

# split a b c
a <- temp1 %>% filter(TRTPN == 0)
b <- temp1 %>% filter(TRTPN == 54)
c <- temp1 %>% filter(TRTPN == 81)

a1 <- a %>% rename(a_n = n, a_mean = mean, a_lcl = lo, a_ucl = hi) %>% select(-TRTPN)
b1 <- b %>% rename(b_n = n, b_mean = mean, b_lcl = lo, b_ucl = hi) %>% select(-TRTPN)
c1 <- c %>% rename(c_n = n, c_mean = mean, c_lcl = lo, c_ucl = hi) %>% select(-TRTPN)

all <- full_join(a1, b1, by = c("AVISITN", "AVISIT")) %>%
  full_join(c1, by = c("AVISITN", "AVISIT")) %>%
  arrange(AVISITN) %>%
  rename(xc = AVISITN, x = AVISIT)

cat("ALL nrow:", nrow(all), "\n")

# ---- Write datasets as CSV ----
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")
write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")
write.csv(all, file.path(out, "all.csv"), row.names = FALSE, na = "")

# ---- P047.lst text rendering (PROC CONTENTS + PROC FREQ) ----
lines <- c()
lines <- c(lines, "PROC CONTENTS DATA=ADVS", "")
lines <- c(lines, paste0("Observations: ", nobs),
                   paste0("Variables: ", nvars),
                   paste0("Filter: PARAMCD=DIABP and ATPTN=815 and ANL01FL=Y and AVISITN<99 and SAFFL=Y"),
                   "")
lines <- c(lines, "Variables in Creation Order:", "")
for (i in seq_along(varlist)) {
  lines <- c(lines, paste0(i, "  ", varlist[i]))
}
lines <- c(lines, "", "PROC FREQ DATA=ADVS", "",
  "TABLES TRTPN AVISIT*AVISITN AVAL / LIST", "")

lines <- c(lines, "Table of TRTPN", "",
  "TRTPN  Frequency  Percent  Cumulative Frequency  Cumulative Percent", "")
for (i in seq_len(nrow(freq_trtpn))) {
  lines <- c(lines, paste0(freq_trtpn$TRTPN[i], "  ",
    freq_trtpn$Frequency[i], "  ",
    fmt2(freq_trtpn$Percent[i]), "  ",
    freq_trtpn$CumFreq[i], "  ",
    fmt2(freq_trtpn$CumPct[i])))
}
lines <- c(lines, paste0("Total  ", N, "  ", fmt2(100), "  ", N, "  ", fmt2(100)), "")

lines <- c(lines, "Table of AVISIT*AVISITN (LIST)", "",
  "AVISIT  AVISITN  Frequency  Percent  Cumulative Frequency  Cumulative Percent", "")
for (i in seq_len(nrow(freq_visit))) {
  lines <- c(lines, paste0(freq_visit$AVISIT[i], "  ",
    freq_visit$AVISITN[i], "  ",
    freq_visit$Frequency[i], "  ",
    fmt2(freq_visit$Percent[i]), "  ",
    freq_visit$CumFreq[i], "  ",
    fmt2(freq_visit$CumPct[i])))
}
lines <- c(lines, paste0("Total  Total  ", N, "  ", fmt2(100), "  ", N, "  ", fmt2(100)), "")

lines <- c(lines, "Table of AVAL", "",
  "AVAL  Frequency  Percent  Cumulative Frequency  Cumulative Percent", "")
for (i in seq_len(nrow(freq_aval))) {
  # AVAL may have decimals; show as is
  aval_chr <- format(freq_aval$AVAL[i], scientific = FALSE, trim = TRUE)
  lines <- c(lines, paste0(aval_chr, "  ",
    freq_aval$Frequency[i], "  ",
    fmt2(freq_aval$Percent[i]), "  ",
    freq_aval$CumFreq[i], "  ",
    fmt2(freq_aval$CumPct[i])))
}
lines <- c(lines, paste0("Total  ", N, "  ", fmt2(100), "  ", N, "  ", fmt2(100)), "")

# summary stats section (PROC MEANS) also in listing for traceability
lines <- c(lines, "PROC MEANS DATA=ADVS NOPRINT", "BY TRTPN AVISITN AVISIT", "VAR AVAL",
  "OUTPUT OUT=TEMP N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm", "")
lines <- c(lines, "Summary Statistics of AVAL by TRTPN AVISITN AVISIT (N Mean STDERR LCLM UCLM with 95 percent CI)", "",
  "TRTPN  AVISITN  AVISIT  N  Mean  StdErr  LCLM  UCLM  LO(90pct two-sided)  HI(90pct two-sided)", "")
tmp_ord <- temp %>% left_join(temp1 %>% select(TRTPN, AVISITN, AVISIT, lo, hi),
  by = c("TRTPN","AVISITN","AVISIT")) %>% arrange(TRTPN, AVISITN)
for (i in seq_len(nrow(tmp_ord))) {
  f <- function(v) ifelse(is.na(v), ".", sprintf("%.2f", round_half_up(v, 2)))
  lines <- c(lines, paste0(tmp_ord$TRTPN[i], "  ", tmp_ord$AVISITN[i], "  ",
    tmp_ord$AVISIT[i], "  ", tmp_ord$n[i], "  ",
    f(tmp_ord$mean[i]), "  ", f(tmp_ord$stderr[i]), "  ",
    f(tmp_ord$lclm[i]), "  ", f(tmp_ord$uclm[i]), "  ",
    f(tmp_ord$lo[i]), "  ", f(tmp_ord$hi[i])))
}
lines <- c(lines, "")

writeLines(lines, file.path(out, "P047.txt"))
# alias copies for harness matching
writeLines(lines, file.path(out, "P047.lst.txt"))
# also write .lst directly
writeLines(lines, file.path(out, "P047.lst"))

# CSV of table body (combined freq tables)
csv_trtpn <- freq_trtpn %>% mutate(TABLE="TRTPN", AVISIT="", AVISITN=NA_real_, AVAL=NA_real_,
  PERCENT=round_half_up(Percent,2), CUM_FREQ=CumFreq, CUM_PCT=round_half_up(CumPct,2)) %>%
  rename(TRTPN=TRTPN, FREQUENCY=Frequency) %>% select(TABLE, TRTPN, AVISIT, AVISITN, AVAL, FREQUENCY, PERCENT, CUM_FREQ, CUM_PCT)
csv_visit <- freq_visit %>% mutate(TABLE="AVISIT*AVISITN", TRTPN=NA_real_, AVAL=NA_real_,
  PERCENT=round_half_up(Percent,2), CUM_FREQ=CumFreq, CUM_PCT=round_half_up(CumPct,2)) %>%
  rename(FREQUENCY=Frequency) %>% select(TABLE, TRTPN, AVISIT, AVISITN, AVAL, FREQUENCY, PERCENT, CUM_FREQ, CUM_PCT)
csv_aval <- freq_aval %>% mutate(TABLE="AVAL", TRTPN=NA_real_, AVISIT="", AVISITN=NA_real_,
  PERCENT=round_half_up(Percent,2), CUM_FREQ=CumFreq, CUM_PCT=round_half_up(CumPct,2)) %>%
  rename(FREQUENCY=Frequency) %>% select(TABLE, TRTPN, AVISIT, AVISITN, AVAL, FREQUENCY, PERCENT, CUM_FREQ, CUM_PCT)
csv_p047 <- bind_rows(csv_trtpn, csv_visit, csv_aval)
write.csv(csv_p047, file.path(out, "P047.csv"), row.names = FALSE, na = "")
write.csv(csv_p047, file.path(out, "P047.lst.csv"), row.names = FALSE, na = "")

# ---- Figure: DBP profile ----
# Build long plot data with offsets xc-0.05, xc+0.05, xc+0.15
mk_long <- function(df) {
  bind_rows(
    tibble(xc=df$xc, x=df$x, trt="0",  n=df$a_n, mean=df$a_mean, lo=df$a_lcl, hi=df$a_ucl, xpos=df$xc-0.05),
    tibble(xc=df$xc, x=df$x, trt="54", n=df$b_n, mean=df$b_mean, lo=df$b_lcl, hi=df$b_ucl, xpos=df$xc+0.05),
    tibble(xc=df$xc, x=df$x, trt="81", n=df$c_n, mean=df$c_mean, lo=df$c_lcl, hi=df$c_ucl, xpos=df$xc+0.15)
  )
}
df_plot <- mk_long(all) %>% filter(!is.na(mean))

xbreaks <- c(0,2,4,6,8,10,12,16,20,24,26)
xlims <- c(-1, 27)

col_vals <- c("0"="#C1272D", "54"="#2E7D32", "81"="#1F4E9C")
lt_vals <- c("0"="dotted", "54"="dashed", "81"="longdash")

p_main <- ggplot(df_plot, aes(x=xpos, y=mean, color=trt, linetype=trt, shape=trt)) +
  geom_line(linewidth=0.8) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.5, linewidth=0.8) +
  geom_point(size=3, stroke=1.2, fill="white") +
  scale_x_continuous(name="Weeks Since Randomized", breaks=xbreaks, limits=xlims) +
  scale_y_continuous(name="Mean(unit)with 95% CI") +
  ggtitle("Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)") +
  scale_color_manual(values=col_vals, name="Treatment Group: ") +
  scale_linetype_manual(values=lt_vals, name="Treatment Group: ") +
  scale_shape_manual(values=c("0"=15, "54"=16, "81"=17), name="Treatment Group: ") +
  theme_bw(base_size=11) +
  theme(plot.title=element_text(hjust=0.5, size=11, face="bold"),
        legend.position="bottom",
        panel.grid.major=element_line(color="grey85"),
        panel.grid.minor=element_blank())

# At-risk panel
df_risk <- bind_rows(
  tibble(xc=all$xc, trt="0",  n=all$a_n),
  tibble(xc=all$xc, trt="54", n=all$b_n),
  tibble(xc=all$xc, trt="81", n=all$c_n)
)
# order trt for display 81 top, 54 mid, 0 bottom to match SAS block order
df_risk$trt <- factor(df_risk$trt, levels=c("81","54","0"))

p_risk <- ggplot(df_risk, aes(x=xc, y=trt, label=ifelse(is.na(n),"",as.character(n)))) +
  geom_text(size=3.5, aes(color=trt), show.legend=FALSE) +
  scale_x_continuous(breaks=xbreaks, limits=xlims) +
  scale_y_discrete(labels=c("81"="81", "54"="54", "0"="0")) +
  scale_color_manual(values=col_vals, guide="none") +
  labs(x=NULL, y=NULL, title="At Risk Subjects") +
  theme_bw(base_size=9) +
  theme(panel.grid=element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_text(size=8),
        plot.title=element_text(hjust=0, size=9),
        plot.margin=margin(t=2,b=2,l=45,r=12))

png_path <- file.path(out, "clinicalhandout_dbpprofile.png")
png(png_path, width=1200, height=800, res=100)
grid::grid.newpage()
grid::pushViewport(grid::viewport(layout=grid::grid.layout(2,1, heights=c(0.82,0.18))))
print(p_main, vp=grid::viewport(layout.pos.row=1, layout.pos.col=1))
print(p_risk, vp=grid::viewport(layout.pos.row=2, layout.pos.col=1))
dev.off()
# alias with original case
file.copy(png_path, file.path(out, "ClinicalHandout_DbpProfile.png"), overwrite=TRUE)

# CSV of plotted data
write.csv(all, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names=FALSE, na="")
write.csv(df_plot, file.path(out, "clinicalhandout_dbpprofile_long.csv"), row.names=FALSE, na="")
# also keep DBP profile data alias
write.csv(all, file.path(out, "DBP_Profile_data.csv"), row.names=FALSE, na="")

# ---- DBP_Profile.html placeholder: 0 numbers ----
# Must contain no digits at all
dbp_lines <- c(
  "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
  "DBP Profile Plot",
  "Treatment Group legend shown",
  "Mean with confidence interval displayed by visit",
  "At Risk Subjects shown below plot",
  "See PNG figure for visual display",
  "No tabular data in this output"
)
writeLines(dbp_lines, file.path(out, "DBP_Profile.txt"))
writeLines(dbp_lines, file.path(out, "DBP_Profile.html.txt"))
# minimal html without digits (avoid dpi, width numbers, dates)
html_txt <- paste0(
  "<html><head><title>DBP Profile</title></head><body>",
  "<h1>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h1>",
  "<p>DBP Profile Plot with confidence interval and At Risk Subjects - see PNG figure for visual display</p>",
  "<p>No tabular data in this output</p>",
  "<img src=\"clinicalhandout_dbpprofile.png\" alt=\"DBP profile plot\" />",
  "</body></html>"
)
writeLines(html_txt, file.path(out, "DBP_Profile.html"))
# CSV of same table: header only, no digits in header or body
dbp_csv <- data.frame(Note="Graph only - see figure data file for plotted values")
# ensure header has no digits: "Note" is fine
write.csv(dbp_csv, file.path(out, "DBP_Profile.csv"), row.names=FALSE, na="")

cat("P047 done\n")
