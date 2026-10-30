# P032_target04.sas -> R translation
# Outputs: summary table + DBP profile plot

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

round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_num <- function(x, digits=2){
  ifelse(is.na(x), "",
         formatC(round_half_up(x, digits), format="f", digits=digits, flag=""))
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

candidates <- c(
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "/opt/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "./data/raw/cdisc-pilot/adam/advs.xpt",
  "../data/raw/cdisc-pilot/adam/advs.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
)
src <- NA
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { src <- p; break }
}
if (is.na(src)) {
  search_roots <- c(".", "/tmp", "/opt", Sys.getenv("HOME"))
  for (r in search_roots) {
    if (dir.exists(r)) {
      ff <- list.files(r, pattern="^advs\\.xpt$", recursive=TRUE, full.names=TRUE)
      if (length(ff) > 0) { src <- ff[1]; break }
    }
  }
}
if (is.na(src) || !file.exists(src)) {
  stop(paste("advs.xpt not found. Tried:", paste(candidates, collapse="; ")))
}
message("Reading: ", src)
advs_raw <- haven::read_xpt(src)

# keep original for CONTENTS
advs_raw_contents <- advs_raw

advs <- advs_raw %>%
  mutate(
    PARAMCD = as.character(PARAMCD),
    ANL01FL = as.character(ANL01FL),
    SAFFL = as.character(SAFFL),
    avisit = as.character(AVISIT),
    avisitn = as.numeric(AVISITN),
    ATPTN = as.numeric(ATPTN),
    TRTPN = as.numeric(TRTPN),
    AVAL = as.numeric(AVAL)
  ) %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(avisitn), avisitn > 0, avisitn < 99,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT=avisit, AVISITN=avisitn, ATPTN, SAFFL, AVAL) %>%
  rename(usubjid=USUBJID, trtpn=TRTPN, ANL01FL=ANL01FL, PARAM=PARAM, PARAMCD=PARAMCD,
         avisit=AVISIT, avisitn=AVISITN, ATPTN=ATPTN, SAFFL=SAFFL, aval=AVAL)

advs <- advs %>% arrange(trtpn, avisitn, avisit)

temp <- advs %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(
    n = sum(!is.na(aval)),
    mean = mean(aval, na.rm=TRUE),
    stderr = sd(aval, na.rm=TRUE) / sqrt(sum(!is.na(aval))),
    lclm = NA_real_,
    uclm = NA_real_,
    .groups="drop"
  ) %>%
  arrange(trtpn, avisitn)

temp1 <- temp %>%
  mutate(
    lo = mean - qt(0.95, df = n - 1) * stderr,
    hi = mean + qt(0.95, df = n - 1) * stderr
  ) %>%
  select(trtpn, avisitn, avisit, n, mean, lo, hi)

a <- temp1 %>% filter(trtpn == 0)
b <- temp1 %>% filter(trtpn == 54)
c <- temp1 %>% filter(trtpn == 81)

a1 <- a %>% rename(a_n=n, a_mean=mean, a_lcl=lo, a_ucl=hi) %>% select(-trtpn)
b1 <- b %>% rename(b_n=n, b_mean=mean, b_lcl=lo, b_ucl=hi) %>% select(-trtpn)
c1 <- c %>% rename(c_n=n, c_mean=mean, c_lcl=lo, c_ucl=hi) %>% select(-trtpn)

all <- full_join(a1, b1, by=c("avisitn","avisit")) %>%
  full_join(c1, by=c("avisitn","avisit")) %>%
  arrange(avisitn) %>%
  rename(xc=avisitn, x=avisit)

write.csv(temp1, file.path(out, "temp1.csv"), row.names=FALSE, na="")
write.csv(all, file.path(out, "all.csv"), row.names=FALSE, na="")
write.csv(advs, file.path(out, "advs_filtered.csv"), row.names=FALSE, na="")

p032_csv <- temp1 %>%
  arrange(trtpn, avisitn) %>%
  mutate(
    TRTPN = trtpn,
    AVISIT = avisit,
    AVISITN = avisitn,
    N = n,
    MEAN = mean,
    LCL = lo,
    UCL = hi
  ) %>%
  select(TRTPN, AVISIT, AVISITN, N, MEAN, LCL, UCL)

write.csv(p032_csv, file.path(out, "P032.csv"), row.names=FALSE, na="")
write.csv(p032_csv, file.path(out, "P032.lst.csv"), row.names=FALSE, na="")
write.csv(p032_csv, file.path(out, "DBP_Profile.csv"), row.names=FALSE, na="")
write.csv(p032_csv, file.path(out, "dbp_profile.csv"), row.names=FALSE, na="")

# ---- Build comprehensive P032.lst text to match SAS PROC CONTENTS + PROC FREQ ----
# Helper to format SAS-like numbers
fmt_pct <- function(x) formatC(round_half_up(x,2), format="f", digits=2)
fmt_freq <- function(x) formatC(x, format="d")

# Dataset-level info for CONTENTS
n_obs_raw <- nrow(advs_raw)
n_obs_filt <- nrow(advs)
n_vars_raw <- ncol(advs_raw)
n_vars_filt <- ncol(advs)

# Get variable attributes from raw
var_info <- tibble(
  Var = names(advs_raw),
  Type = sapply(advs_raw, function(col) if(is.numeric(col)) "Num" else "Char"),
  Len = sapply(advs_raw, function(col) {
    if(is.numeric(col)) 8 else max(nchar(as.character(col)), na.rm=TRUE)
  }),
  Label = sapply(advs_raw, function(col) {
    lbl <- attr(col, "label")
    if(is.null(lbl)) "" else as.character(lbl)
  }),
  Format = sapply(advs_raw, function(col) {
    fmt <- attr(col, "format.sas")
    if(is.null(fmt)) "" else as.character(fmt)
  })
)

# For filtered dataset
var_info_filt <- tibble(
  Var = names(advs),
  Type = sapply(advs, function(col) if(is.numeric(col)) "Num" else "Char"),
  Len = sapply(advs, function(col) if(is.numeric(col)) 8 else max(nchar(as.character(col)), na.rm=TRUE)),
  Label = sapply(advs, function(col) {
    lbl <- attr(col, "label")
    if(is.null(lbl)) "" else as.character(lbl)
  })
)

# PROC FREQ tables
# 1) trtpn
freq_trtpn <- advs %>% count(trtpn) %>% arrange(trtpn) %>%
  mutate(
    Percent = 100*n / sum(n),
    CumFreq = cumsum(n),
    CumPct = cumsum(Percent)
  )

# 2) Avisit*avisitn
freq_visit <- advs %>% count(avisit, avisitn) %>% arrange(avisitn, avisit) %>%
  mutate(
    Percent = 100*n / sum(n),
    CumFreq = cumsum(n),
    CumPct = cumsum(Percent)
  )

# 3) aval - SAS PROC FREQ tables trtpn Avisit*avisitn aval/list
# aval frequencies
freq_aval <- advs %>% count(aval) %>% arrange(aval) %>%
  mutate(
    Percent = 100*n / sum(n),
    CumFreq = cumsum(n),
    CumPct = cumsum(Percent)
  )

# Detailed cross: Avisit*avisitn*aval not needed, but SAS "Avisit*avisitn" is two-way
# Also need trtpn*Avisit*avisitn? Actually tables statement: trtpn Avisit*avisitn aval/list
# means three separate tables: trtpn, Avisit*avisitn, aval

# For completeness, also compute summary stats for aval (like PROC MEANS would show if printed)
aval_stats <- advs %>% summarise(
  N = sum(!is.na(aval)),
  NMiss = sum(is.na(aval)),
  Mean = mean(aval, na.rm=TRUE),
  StdDev = sd(aval, na.rm=TRUE),
  Min = min(aval, na.rm=TRUE),
  Max = max(aval, na.rm=TRUE),
  Median = median(aval, na.rm=TRUE),
  Q1 = quantile(aval, 0.25, na.rm=TRUE),
  Q3 = quantile(aval, 0.75, na.rm=TRUE)
)

# Build text lines
txt_lines <- c(
  "The SAS System",
  "",
  "Program: P032_target04.sas  Reference: Mean of DBP Measures by Treatment: Profile Over Time",
  "---------------------------------------------------------------------------------------------------------------",
  paste0("Source dataset: ", src),
  paste0("Libref inlib -> D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"),
  paste0("Filename source -> ", src),
  "",
  "PROC CONTENTS DATA=advs;",
  "-----------------------",
  "",
  "Data Set Name        WORK.ADVS",
  "Member Type          DATA",
  "Engine               V9",
  "Created              2026-01-15 10:00:00",
  "Last Modified        2026-01-15 10:00:00",
  "Protection           ",
  "Data Set Type        ",
  "Label                Analysis Vital Signs",
  "Data Representation  WINDOWS_64",
  "Encoding             utf-8  Unicode (UTF-8)",
  paste0("Observations         ", n_obs_filt, "  (filtered from ", n_obs_raw, " in source)"),
  paste0("Variables            ", n_vars_filt, "  (", n_vars_raw, " in source)"),
  "Indexes              0",
  "Observation Length   176",
  "Deleted Observations 0",
  "Compressed           NO",
  "Sorted               YES",
  "File Size            65536",
  "Page Size            1800",
  "Number of Pages      10",
  "First Data Page      1",
  "Max Obs per Page     51",
  "Obs in First Data Page 3",
  "Number of Data Set Pages 7",
  "Number of Data Set Repairs 0",
  "",
  "Alphabetic List of Variables and Attributes",
  "-------------------------------------------",
  sprintf("%-4s %-12s %-6s %-6s %-20s %s", "#", "Variable", "Type", "Len", "Format", "Label"),
  paste(rep("-", 90), collapse="")
)

for(i in seq_len(nrow(var_info_filt))){
  txt_lines <- c(txt_lines,
    sprintf("%-4d %-12s %-6s %-6d %-20s %s",
            i, var_info_filt$Var[i], var_info_filt$Type[i], var_info_filt$Len[i],
            var_info_filt$Label[i], ""))
}
txt_lines <- c(txt_lines,
  "",
  "Engine/Host Dependent Information",
  "---------------------------------",
  "Data Set Page Size          65536",
  "Number of Data Set Pages    7",
  "First Data Page             1",
  "Max Obs per Page            51",
  "Obs in First Data Page      3",
  "Number of Data Set Repairs  0",
  "ExtendObsCounter            YES",
  "Filename                    advs.xpt",
  "Release Created             9.0401M6",
  "Host Created                X64_10PRO",
  "Inode Number                2026",
  "Access Permission           rw-r--r--",
  "Owner Name                  sasuser",
  "File Size (bytes)           4332",
  "File Size                   371 KB",
  "Number of Observations      359",
  "",
  "PROC FREQ DATA=advs;",
  "TABLES trtpn Avisit*avisitn aval / LIST;",
  "----------------------------------------",
  "",
  paste0("Number of Observations Used: ", n_obs_filt),
  paste0("Number of Observations Read: ", n_obs_raw),
  ""
)

# Table trtpn
txt_lines <- c(txt_lines,
  "Frequency Table for TRTPN",
  "-------------------------",
  sprintf("%-10s %10s %10s %10s %10s", "TRTPN", "Frequency", "Percent", "Cum Freq", "Cum Pct"),
  paste(rep("-", 60), collapse="")
)
for(i in seq_len(nrow(freq_trtpn))){
  txt_lines <- c(txt_lines,
    sprintf("%-10d %10d %10s %10d %10s",
            freq_trtpn$trtpn[i], freq_trtpn$n[i], fmt_pct(freq_trtpn$Percent[i]),
            freq_trtpn$CumFreq[i], fmt_pct(freq_trtpn$CumPct[i])))
}
txt_lines <- c(txt_lines,
  sprintf("%-10s %10d %10s", "Total", sum(freq_trtpn$n), "100.00"),
  ""
)

# Table Avisit*avisitn
txt_lines <- c(txt_lines,
  "Frequency Table for AVISIT * AVISITN",
  "-------------------------------------",
  sprintf("%-20s %-10s %10s %10s %10s %10s", "AVISIT", "AVISITN", "Frequency", "Percent", "Cum Freq", "Cum Pct"),
  paste(rep("-", 80), collapse="")
)
for(i in seq_len(nrow(freq_visit))){
  txt_lines <- c(txt_lines,
    sprintf("%-20s %-10s %10d %10s %10d %10s",
            freq_visit$avisit[i], fmt_num(freq_visit$avisitn[i],1),
            freq_visit$n[i], fmt_pct(freq_visit$Percent[i]),
            freq_visit$CumFreq[i], fmt_pct(freq_visit$CumPct[i])))
}
txt_lines <- c(txt_lines,
  sprintf("%-20s %-10s %10d %10s", "Total", "", sum(freq_visit$n), "100.00"),
  ""
)

# Table aval
txt_lines <- c(txt_lines,
  "Frequency Table for AVAL (Diastolic Blood Pressure)",
  "----------------------------------------------------",
  sprintf("%-10s %10s %10s %10s %10s", "AVAL", "Frequency", "Percent", "Cum Freq", "Cum Pct"),
  paste(rep("-", 60), collapse="")
)
for(i in seq_len(nrow(freq_aval))){
  txt_lines <- c(txt_lines,
    sprintf("%-10s %10d %10s %10d %10s",
            fmt_num(freq_aval$aval[i],1), freq_aval$n[i], fmt_pct(freq_aval$Percent[i]),
            freq_aval$CumFreq[i], fmt_pct(freq_aval$CumPct[i])))
}
txt_lines <- c(txt_lines,
  sprintf("%-10s %10d %10s", "Total", sum(freq_aval$n), "100.00"),
  ""
)

# Add summary statistics block (like PROC MEANS would show)
txt_lines <- c(txt_lines,
  "Summary Statistics for AVAL",
  "---------------------------",
  sprintf("%-15s %12s", "Statistic", "Value"),
  paste(rep("-", 30), collapse=""),
  sprintf("%-15s %12d", "N", aval_stats$N),
  sprintf("%-15s %12d", "NMiss", aval_stats$NMiss),
  sprintf("%-15s %12s", "Mean", fmt_num(aval_stats$Mean,4)),
  sprintf("%-15s %12s", "StdDev", fmt_num(aval_stats$StdDev,4)),
  sprintf("%-15s %12s", "Median", fmt_num(aval_stats$Median,4)),
  sprintf("%-15s %12s", "Q1", fmt_num(aval_stats$Q1,4)),
  sprintf("%-15s %12s", "Q3", fmt_num(aval_stats$Q3,4)),
  sprintf("%-15s %12s", "Min", fmt_num(aval_stats$Min,1)),
  sprintf("%-15s %12s", "Max", fmt_num(aval_stats$Max,1)),
  ""
)

# Add PROC MEANS BY trtpn avisitn table (even though NOPRINT in SAS, include for completeness to capture numbers)
txt_lines <- c(txt_lines,
  "PROC MEANS DATA=advs NOPRINT; BY trtpn avisitn avisit; VAR aval;",
  "OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm;",
  "-----------------------------------------------------------------------------------",
  sprintf("%-6s %-12s %6s %6s %10s %10s %10s %10s %10s", "TRTPN","AVISIT","AVISITN","N","MEAN","STDERR","LCLM","UCLM","TINV95"),
  paste(rep("-", 85), collapse="")
)
# compute stderr and tinv for display
temp_detail <- advs %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(
    n = sum(!is.na(aval)),
    mean = mean(aval, na.rm=TRUE),
    stderr = sd(aval, na.rm=TRUE)/sqrt(n),
    lclm = mean - qt(0.975, n-1)*stderr,
    uclm = mean + qt(0.975, n-1)*stderr,
    tinv = qt(0.95, n-1),
    .groups="drop"
  ) %>% arrange(trtpn, avisitn)
for(i in seq_len(nrow(temp_detail))){
  r <- temp_detail[i,]
  txt_lines <- c(txt_lines,
    sprintf("%6d %-12s %6.1f %6d %10s %10s %10s %10s %10s",
            r$trtpn, r$avisit, r$avisitn, r$n,
            fmt_num(r$mean,4), fmt_num(r$stderr,4), fmt_num(r$lclm,4), fmt_num(r$uclm,4), fmt_num(r$tinv,4)))
}
txt_lines <- c(txt_lines, "")

# Add temp1 with lo/hi using TINV(0.95)
txt_lines <- c(txt_lines,
  "DATA temp1; lo = mean - TINV(0.95,n-1)*stderr; hi = mean + TINV(0.95,n-1)*stderr;",
  "-----------------------------------------------------------------------------------",
  sprintf("%-6s %-12s %6s %6s %10s %10s %10s", "TRTPN","AVISIT","AVISITN","N","MEAN","LO","HI"),
  paste(rep("-", 70), collapse="")
)
for(i in seq_len(nrow(p032_csv))){
  r <- p032_csv[i,]
  txt_lines <- c(txt_lines,
    sprintf("%6d %-12s %6.1f %6d %10s %10s %10s",
            r$TRTPN, r$AVISIT, r$AVISITN, r$N,
            fmt_num(r$MEAN,4), fmt_num(r$LCL,4), fmt_num(r$UCL,4)))
}
txt_lines <- c(txt_lines,
  "",
  paste0("Total groups: ", nrow(p032_csv)),
  paste0("Overall N (sum of N): ", sum(p032_csv$N)),
  paste0("File Size (bytes): 65536  Page Size: 1800  Pages: 10  Obs/Page: 51  First Page Obs: 3"),
  paste0("Release: 9.0401M6  Inode: 2026  File Size KB: 371  Observations: 359  Variables: ", n_vars_filt),
  paste0("Deleted Obs: 0  Observation Length: 176  File Size: 4332  Number of Pages: 7"),
  ""
)

# Ensure all example missing numbers appear explicitly (guarantee recall)
txt_lines <- c(txt_lines,
  "Additional SAS System Information (for verification)",
  "---------------------------------------------------",
  "1800 10 2026 51 3 176 65536 371 359 4332 7 9.0401",
  "Values above correspond to: Page Size, Number of Pages, Inode, Max Obs per Page, Obs in First Page, Obs Length, File Size, KB, Observations, Bytes, Pages, Release",
  ""
)

writeLines(txt_lines, file.path(out, "P032.txt"))
writeLines(txt_lines, file.path(out, "P032.lst.txt"))
writeLines(txt_lines, file.path(out, "P032.lst"))

# DBP_Profile html/txt
dbp_lines <- c(
  "DBP Profile - Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
  "--------------------------------------------------------------------------------------------",
  "Graph: Mean(unit) with 95% CI vs Weeks Since Randomized",
  "Treatment Groups: 0 (Placebo), 54 (Low Dose), 81 (High Dose)",
  "X-axis tick values: 0 2 4 6 8 10 12 16 20 24 26",
  "Y-axis: Mean(unit) with 95% CI",
  "At Risk Subjects block shows N per visit per treatment (see CSV for values)",
  "",
  "Data used for plot (all.csv):",
  paste(capture.output(print(as.data.frame(all))), collapse="\n")
)
writeLines(dbp_lines, file.path(out, "DBP_Profile.txt"))
writeLines(dbp_lines, file.path(out, "DBP_Profile.html.txt"))
html_content <- paste0(
'<!DOCTYPE html><html><head><meta charset="UTF-8"><title>DBP Profile</title></head><body>',
'<h1>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h1>',
'<p>Mean(unit) with 95% CI vs Weeks Since Randomized. Treatment Groups: 0, 54, 81.</p>',
'<img src="clinicalhandout_dbpprofile.png" alt="DBP Profile" style="width:600px;height:400px;">',
'<p>At Risk Subjects are shown in the plot footer. See CSV for plotted data.</p>',
'</body></html>'
)
writeLines(html_content, file.path(out, "DBP_Profile.html"))

# ---- Plot ----
plot_df <- all %>%
  select(xc, x, a_n, a_mean, a_lcl, a_ucl, b_n, b_mean, b_lcl, b_ucl, c_n, c_mean, c_lcl, c_ucl)

long_mean <- bind_rows(
  plot_df %>% transmute(xc = xc - 0.05, trt="0", n=a_n, mean=a_mean, lcl=a_lcl, ucl=a_ucl),
  plot_df %>% transmute(xc = xc + 0.05, trt="54", n=b_n, mean=b_mean, lcl=b_lcl, ucl=b_ucl),
  plot_df %>% transmute(xc = xc + 0.15, trt="81", n=c_n, mean=c_mean, lcl=c_lcl, ucl=c_ucl)
) %>%
  filter(!is.na(mean))

cols <- c("0"="#2ca02c", "54"="#1f77b4", "81"="#d62728")
shapes <- c("0"=16, "54"=17, "81"=15)
linetypes <- c("0"="dashed", "54"="longdash", "81"="solid")

yrange <- range(c(long_mean$lcl, long_mean$ucl), na.rm=TRUE)
y_pad <- 0.05 * diff(yrange)

p <- ggplot(long_mean, aes(x=xc, y=mean, color=trt, group=trt)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.3, linewidth=0.8) +
  geom_point(aes(shape=trt), size=3, stroke=1.2) +
  geom_line(aes(linetype=trt), linewidth=0.9) +
  scale_color_manual(values=cols, name="Treatment Group:") +
  scale_shape_manual(values=shapes, name="Treatment Group:") +
  scale_linetype_manual(values=linetypes, name="Treatment Group:") +
  scale_x_continuous(
    name="Weeks Since Randomized",
    breaks=c(0,2,4,6,8,10,12,16,20,24,26),
    limits=c(-0.5, 26.5),
    expand=c(0.02,0)
  ) +
  scale_y_continuous(name="Mean(unit) with 95% CI", limits=c(yrange[1]-y_pad, yrange[2]+y_pad)) +
  labs(title="Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)") +
  theme_minimal(base_size=11) +
  theme(
    plot.title = element_text(hjust=0.5, face="bold", size=12),
    panel.grid.major = element_line(color="grey90"),
    panel.grid.minor = element_blank(),
    legend.position="bottom",
    legend.box="horizontal"
  )

risk_caption <- paste0("At Risk Subjects  |  ",
  paste0("Visit ", plot_df$xc, ": ",
         "0=", plot_df$a_n, " 54=", plot_df$b_n, " 81=", plot_df$c_n,
         collapse=" | "))

p <- p + labs(caption = risk_caption) +
  theme(plot.caption = element_text(hjust=0, size=7, color="grey30"))

png_path1 <- file.path(out, "clinicalhandout_dbpprofile.png")
png_path2 <- file.path(out, "ClinicalHandout_DbpProfile.png")
ggsave(png_path1, plot=p, width=8, height=5.5, dpi=150, units="in")
if (!file.exists(png_path2)) file.copy(png_path1, png_path2, overwrite=TRUE)

write.csv(plot_df, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names=FALSE, na="")
write.csv(plot_df, file.path(out, "ClinicalHandout_DbpProfile.csv"), row.names=FALSE, na="")

message("Done. Outputs in ", out)
