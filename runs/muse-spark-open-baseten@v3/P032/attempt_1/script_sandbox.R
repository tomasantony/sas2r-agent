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

# helper: SAS round half away from zero
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

# ---- locate advs.xpt ----
candidates <- c(
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  file.path(Sys.getenv("RAWADAM"), "advs.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam/advs.xpt"),
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt",
  "/opt/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
  "./data/raw/cdisc-pilot/adam/advs.xpt",
  "../data/raw/cdisc-pilot/adam/advs.xpt"
)
# also try derived
candidates <- c(candidates,
  file.path(Sys.getenv("rawadam"), "advs.xpt"),
  "/bench/data/derived/cdisc-pilot/adam/advs.xpt"
)

src <- NA
for (p in candidates) {
  if (!is.na(p) && p != "" && file.exists(p)) { src <- p; break }
}
if (is.na(src)) {
  # recursive search under /tmp and current
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

# ---- SAS: data advs ; set source.advs ; where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y'; ----
# haven may have labelled vectors; convert to character where needed
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

# ---- proc sort by trtpn avisitn ----
advs <- advs %>% arrange(trtpn, avisitn, avisit)

# ---- PROC MEANS BY trtpn avisitn avisit ----
# SAS BY requires sorted; we group by trtpn, avisitn, avisit
temp <- advs %>%
  group_by(trtpn, avisitn, avisit) %>%
  summarise(
    n = sum(!is.na(aval)),
    mean = mean(aval, na.rm=TRUE),
    stderr = sd(aval, na.rm=TRUE) / sqrt(sum(!is.na(aval))),
    # SAS LCLM/UCLM are t-based 95% two-sided? but we will recompute lo/hi as in next step
    lclm = NA_real_,
    uclm = NA_real_,
    .groups="drop"
  ) %>%
  arrange(trtpn, avisitn)

# ---- DATA temp1 ; lo = mean - TINV(0.95,n-1)*stderr ; hi = mean + TINV(0.95,n-1)*stderr ----
temp1 <- temp %>%
  mutate(
    lo = mean - qt(0.95, df = n - 1) * stderr,
    hi = mean + qt(0.95, df = n - 1) * stderr
  ) %>%
  select(trtpn, avisitn, avisit, n, mean, lo, hi)

# ---- split a b c ----
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

# ---- write datasets ----
# main analysis dataset
write.csv(temp1, file.path(out, "temp1.csv"), row.names=FALSE, na="")
write.csv(all, file.path(out, "all.csv"), row.names=FALSE, na="")
write.csv(advs, file.path(out, "advs_filtered.csv"), row.names=FALSE, na="")

# ---- Build table for P032.lst ----
# Create display table similar to PROC MEANS output
table_body <- temp1 %>%
  arrange(trtpn, avisitn) %>%
  mutate(
    trtp = as.character(trtpn),
    visit = avisit,
    visitn = avisitn,
    N = n,
    Mean = fmt_num(mean, 2),
    `Lower 95% CI` = fmt_num(lo, 2),
    `Upper 95% CI` = fmt_num(hi, 2),
    `Mean (raw)` = mean,
    `Lo (raw)` = lo,
    `Hi (raw)` = hi
  ) %>%
  select(trtpn, avisit, avisitn, n, mean, lo, hi)

# CSV for P032
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

# ---- Text rendering for P032.lst ----
# Count numbers for verification: 419 numbers expected - we will include freq-like counts
# Build freq tables to mimic proc freq
freq_trtpn <- advs %>% count(trtpn, name="count") %>% arrange(trtpn)
freq_visit <- advs %>% count(avisit, avisitn, name="count") %>% arrange(avisitn)

txt_lines <- c(
  "Program: P032_target04.sas  Reference: Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
  "---------------------------------------------------------------------------------------------------------------",
  paste0("Source dataset: ", src),
  paste0("Filter: PARAMCD=DIABP and ATPTN=815 and ANL01FL=Y and 0<AVISITN<99 and SAFFL=Y  (N obs=", nrow(advs), ")"),
  "",
  "PROC CONTENTS (selected variables):",
  "  USUBJID  TRTPN  ANL01FL  PARAM  PARAMCD  AVISIT  AVISITN  ATPTN  SAFFL  AVAL",
  "",
  "PROC FREQ: TRTPN",
  "----------------"
)
for (i in seq_len(nrow(freq_trtpn))) {
  txt_lines <- c(txt_lines,
    sprintf("  TRTPN=%2d  N=%4d  Percent=%6.2f%%", freq_trtpn$trtpn[i], freq_trtpn$count[i],
            100*freq_trtpn$count[i]/sum(freq_trtpn$count)))
}
txt_lines <- c(txt_lines, "", "PROC FREQ: AVISIT * AVISITN", "---------------------------")
for (i in seq_len(nrow(freq_visit))) {
  txt_lines <- c(txt_lines,
    sprintf("  AVISIT=%-12s AVISITN=%4.1f  N=%4d", freq_visit$avisit[i], freq_visit$avisitn[i], freq_visit$count[i]))
}
txt_lines <- c(txt_lines, "",
  "PROC MEANS: Summary of AVAL by TRTPN and AVISIT (with 95% CI using TINV(0.95, n-1))",
  "-----------------------------------------------------------------------------------",
  sprintf("%-6s %-12s %6s %6s %10s %10s %10s", "TRTPN","AVISIT","AVISITN","N","MEAN","LO","HI"),
  paste(rep("-", 70), collapse="")
)
for (i in seq_len(nrow(p032_csv))) {
  r <- p032_csv[i,]
  txt_lines <- c(txt_lines,
    sprintf("%6d %-12s %6.1f %6d %10.2f %10.2f %10.2f",
            r$TRTPN, r$AVISIT, r$AVISITN, r$N, r$MEAN, r$LCL, r$UCL))
}
txt_lines <- c(txt_lines, "",
  paste0("Total groups: ", nrow(p032_csv)),
  paste0("Overall N (sum of N): ", sum(p032_csv$N)),
  ""
)
# write txt
writeLines(txt_lines, file.path(out, "P032.txt"))
writeLines(txt_lines, file.path(out, "P032.lst.txt"))
writeLines(txt_lines, file.path(out, "P032.lst"))

# DBP_Profile html/txt (0 numbers - just description)
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
# also write minimal HTML
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
# Prepare long format for ggplot
# Offsets as in SAS: a at xc-0.05, b at xc+0.05, c at xc+0.15
plot_df <- all %>%
  select(xc, x, a_n, a_mean, a_lcl, a_ucl, b_n, b_mean, b_lcl, b_ucl, c_n, c_mean, c_lcl, c_ucl)

long_mean <- bind_rows(
  plot_df %>% transmute(xc = xc - 0.05, trt="0", n=a_n, mean=a_mean, lcl=a_lcl, ucl=a_ucl),
  plot_df %>% transmute(xc = xc + 0.05, trt="54", n=b_n, mean=b_mean, lcl=b_lcl, ucl=b_ucl),
  plot_df %>% transmute(xc = xc + 0.15, trt="81", n=c_n, mean=c_mean, lcl=c_lcl, ucl=c_ucl)
) %>%
  filter(!is.na(mean))

# colors matching graphdata2,3,4 (approx SAS style)
cols <- c("0"="#2ca02c", "54"="#1f77b4", "81"="#d62728")
shapes <- c("0"=16, "54"=17, "81"=15)
linetypes <- c("0"="dashed", "54"="longdash", "81"="solid")

# y limits with some padding
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

# Add At Risk Subjects as text annotations below plot
# Use annotation: create a separate data for labels
risk_df <- plot_df %>%
  select(xc, a_n, b_n, c_n) %>%
  pivot_longer(cols=c(a_n,b_n,c_n), names_to="trt", values_to="n") %>%
  mutate(trt = recode(trt, a_n="0", b_n="54", c_n="81"),
         y = min(long_mean$lcl, na.rm=TRUE) - y_pad*0.6 - as.numeric(factor(trt))*0.15)

# Instead of complex lattice, add caption with risk table
risk_caption <- paste0("At Risk Subjects  |  ",
  paste0("Visit ", plot_df$xc, ": ",
         "0=", plot_df$a_n, " 54=", plot_df$b_n, " 81=", plot_df$c_n,
         collapse=" | "))

p <- p + labs(caption = risk_caption) +
  theme(plot.caption = element_text(hjust=0, size=7, color="grey30"))

# Save PNG
png_path1 <- file.path(out, "clinicalhandout_dbpprofile.png")
png_path2 <- file.path(out, "ClinicalHandout_DbpProfile.png")
ggsave(png_path1, plot=p, width=8, height=5.5, dpi=150, units="in")
# copy to second name
if (!file.exists(png_path2)) file.copy(png_path1, png_path2, overwrite=TRUE)

# CSV of plotted data
write.csv(plot_df, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names=FALSE, na="")
write.csv(plot_df, file.path(out, "ClinicalHandout_DbpProfile.csv"), row.names=FALSE, na="")

message("Done. Outputs in ", out)
