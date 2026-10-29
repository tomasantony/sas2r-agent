# P032_target04.sas -> R translation
# Outputs: P032 table/listing, DBP_Profile html, ClinicalHandout_DbpProfile figure
suppressMessages({
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

# helper: SAS round half away from zero (half up)
round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_num <- function(x, digits=2){
  ifelse(is.na(x), "", formatC(round_half_up(x, digits), format="f", digits=digits, flag=""))
}

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "/bench/data/raw/cdisc-pilot/adam"
rawadam2 <- "/bench/data/derived/cdisc-pilot/adam"
inlib_dir <- "/bench/data/derived/cdisc-pilot/adam"

# locate advs.xpt
candidates <- c(
  file.path(rawadam, "advs.xpt"),
  file.path(rawadam, "ADVS.xpt"),
  file.path(rawadam2, "advs.xpt"),
  file.path(inlib_dir, "advs.xpt"),
  file.path(inlib_dir, "advs.sas7bdat"),
  "/bench/data/raw/cdisc-pilot/adam/advs.xpt"
)
advs_path <- NULL
for (p in candidates) {
  if (file.exists(p)) { advs_path <- p; break }
}
if (is.null(advs_path)) {
  # try list files
  if (dir.exists(rawadam)) {
    fl <- list.files(rawadam, pattern="advs", ignore.case=TRUE, full.names=TRUE)
    if (length(fl)>0) advs_path <- fl[1]
  }
}
if (is.null(advs_path) || !file.exists(advs_path)) {
  stop(paste("advs.xpt not found. Tried:", paste(candidates, collapse="; ")))
}
cat("Reading:", advs_path, "\n")

# read
if (grepl("\\.sas7bdat$", advs_path, ignore.case=TRUE)) {
  advs_raw <- haven::read_sas(advs_path)
} else {
  advs_raw <- haven::read_xpt(advs_path)
}

# normalize names to upper for processing, keep original mapping
orig_names <- names(advs_raw)
names(advs_raw) <- toupper(names(advs_raw))

# zap labels for easier handling but keep values
advs_raw <- advs_raw %>%
  mutate(across(where(haven::is.labelled), haven::zap_labels))

# Ensure expected columns exist
# PARAMCD, ATPTN, ANL01FL, AVISITN, SAFFL, USUBJID, TRTPN, PARAM, AVISIT, AVAL
# ATPTN may be numeric or character
if (!"ATPTN" %in% names(advs_raw)) {
  # try alternative
  if ("ATPT" %in% names(advs_raw)) names(advs_raw)[names(advs_raw)=="ATPT"] <- "ATPTN"
}

# Filter: where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y'
# SAS: .<avisitn<99 means not missing and <99
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(AVISITN), AVISITN < 99,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL)

cat("Filtered advs rows:", nrow(advs), "\n")

# Keep copy for CSV output (restore original case names)
advs_out <- advs
names(advs_out) <- c("USUBJID","TRTPN","ANL01FL","PARAM","PARAMCD","AVISIT","AVISITN","ATPTN","SAFFL","AVAL")
write.csv(advs_out, file.path(out, "advs.csv"), row.names=FALSE, na="")

# PROC CONTENTS equivalent
contents_df <- tibble(
  Variable = names(advs),
  Type = sapply(advs, function(x) class(x)[1]),
  N = sapply(advs, function(x) sum(!is.na(x))),
  Nmiss = sapply(advs, function(x) sum(is.na(x))),
  Mean = sapply(advs, function(x) if(is.numeric(x)) mean(x, na.rm=TRUE) else NA_real_),
  Min = sapply(advs, function(x) if(is.numeric(x)) min(x, na.rm=TRUE) else NA),
  Max = sapply(advs, function(x) if(is.numeric(x)) max(x, na.rm=TRUE) else NA)
)
write.csv(contents_df, file.path(out, "contents.csv"), row.names=FALSE, na="")

# PROC FREQ tables
# tables trtpn Avisit*avisitn aval/list
# TRTPN freq
freq_trtpn <- advs %>%
  group_by(TRTPN) %>%
  summarise(Frequency = n(), .groups="drop") %>%
  arrange(TRTPN) %>%
  mutate(Percent = Frequency / sum(Frequency) * 100,
         CumFreq = cumsum(Frequency),
         CumPercent = cumsum(Percent))

# AVISIT * AVISITN
freq_visit <- advs %>%
  group_by(AVISIT, AVISITN) %>%
  summarise(Frequency = n(), .groups="drop") %>%
  arrange(AVISITN, AVISIT) %>%
  mutate(Percent = Frequency / sum(Frequency) * 100)

# AVAL freq (list)
freq_aval <- advs %>%
  group_by(AVAL) %>%
  summarise(Frequency = n(), .groups="drop") %>%
  arrange(AVAL) %>%
  mutate(Percent = Frequency / sum(Frequency) * 100)

write.csv(freq_trtpn, file.path(out, "freq_trtpn.csv"), row.names=FALSE, na="")
write.csv(freq_visit, file.path(out, "freq_visit.csv"), row.names=FALSE, na="")
write.csv(freq_aval, file.path(out, "freq_aval.csv"), row.names=FALSE, na="")

# PROC SORT by trtpn avisitn
advs_sorted <- advs %>% arrange(TRTPN, AVISITN)

# PROC MEANS BY trtpn avisitn avisit VAR aval OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm
# SAS LCLM/UCLM uses t critical 0.975
temp <- advs_sorted %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = mean(AVAL, na.rm=TRUE),
    std = sd(AVAL, na.rm=TRUE),
    .groups="drop"
  ) %>%
  mutate(
    stderr = std / sqrt(n),
    # SAS PROC MEANS LCLM/UCLM (95% CI)
    t975 = qt(0.975, n-1),
    lclm = mean - t975 * stderr,
    uclm = mean + t975 * stderr
  ) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, stderr, lclm, uclm) %>%
  arrange(TRTPN, AVISITN)

write.csv(temp, file.path(out, "temp.csv"), row.names=FALSE, na="")
write.csv(temp, file.path(out, "P032_temp.csv"), row.names=FALSE, na="")

# DATA temp1 ; lo = mean - (TINV(0.95,n-1)*stderr); hi = mean + (TINV(0.95,n-1)*stderr);
temp1 <- temp %>%
  mutate(
    t95 = qt(0.95, n-1),
    lo = mean - t95 * stderr,
    hi = mean + t95 * stderr
  ) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi, stderr, lclm, uclm)

write.csv(temp1, file.path(out, "temp1.csv"), row.names=FALSE, na="")

# format data for plot -- split by trtpn
a <- temp1 %>% filter(TRTPN == 0)
b <- temp1 %>% filter(TRTPN == 54)
c <- temp1 %>% filter(TRTPN == 81)

a1 <- a %>% rename(a_n=n, a_mean=mean, a_lcl=lo, a_ucl=hi) %>% select(AVISITN, AVISIT, a_n, a_mean, a_lcl, a_ucl)
b1 <- b %>% rename(b_n=n, b_mean=mean, b_lcl=lo, b_ucl=hi) %>% select(AVISITN, AVISIT, b_n, b_mean, b_lcl, b_ucl)
c1 <- c %>% rename(c_n=n, c_mean=mean, c_lcl=lo, c_ucl=hi) %>% select(AVISITN, AVISIT, c_n, c_mean, c_lcl, c_ucl)

# merge by avisitn avisit
all_wide <- full_join(a1, b1, by=c("AVISITN","AVISIT")) %>%
  full_join(c1, by=c("AVISITN","AVISIT")) %>%
  arrange(AVISITN) %>%
  rename(xc=AVISITN, x=AVISIT)

# Ensure ordering and fill missing with NA
write.csv(all_wide, file.path(out, "all.csv"), row.names=FALSE, na="")
write.csv(all_wide, file.path(out, "DBP_Profile.csv"), row.names=FALSE, na="")
write.csv(all_wide, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names=FALSE, na="")

# Also create long version for plotting and for CSV
plot_long <- bind_rows(
  all_wide %>% select(xc, x, n=a_n, mean=a_mean, lo=a_lcl, hi=a_ucl) %>% mutate(TRTPN=0, TRTP="Placebo", xpos=xc-0.05),
  all_wide %>% select(xc, x, n=b_n, mean=b_mean, lo=b_lcl, hi=b_ucl) %>% mutate(TRTPN=54, TRTP="Xanomeline Low Dose", xpos=xc+0.05),
  all_wide %>% select(xc, x, n=c_n, mean=c_mean, lo=c_lcl, hi=c_ucl) %>% mutate(TRTPN=81, TRTP="Xanomeline High Dose", xpos=xc+0.15)
) %>% arrange(TRTPN, xc)

write.csv(plot_long, file.path(out, "plot_data.csv"), row.names=FALSE, na="")
write.csv(temp1, file.path(out, "P032.csv"), row.names=FALSE, na="")

# Create text rendering for P032.lst
# Build comprehensive listing text
txt_path <- file.path(out, "P032.txt")
lst_path <- file.path(out, "P032.lst.txt")
con <- file(txt_path, "w")
con2 <- file(lst_path, "w")
sink(con)

cat("P032 - Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)\n")
cat("================================================================================\n\n")
cat("Source: ", advs_path, "\n")
cat("Filter: PARAMCD='DIABP' and ATPTN=815 and ANL01FL='Y' and 0<AVISITN<99 and SAFFL='Y'\n")
cat("Analysis dataset: ADVS (", nrow(advs), " observations)\n\n")

cat("PROC CONTENTS DATA=advs;\n")
cat("------------------------\n")
print(as.data.frame(contents_df), row.names=FALSE)
cat("\n")

cat("PROC FREQ DATA=advs;\n")
cat("TABLES TRTPN / LIST;\n")
cat("--------------------\n")
print(as.data.frame(freq_trtpn %>% mutate(Percent=round_half_up(Percent,2), CumPercent=round_half_up(CumPercent,2))), row.names=FALSE)
cat("\nTotal N =", sum(freq_trtpn$Frequency), "\n\n")

cat("TABLES AVISIT*AVISITN / LIST;\n")
cat("-----------------------------\n")
print(as.data.frame(freq_visit %>% mutate(Percent=round_half_up(Percent,2))), row.names=FALSE)
cat("\n")

cat("TABLES AVAL / LIST;\n")
cat("-------------------\n")
# print all aval frequencies
print(as.data.frame(freq_aval %>% mutate(Percent=round_half_up(Percent,2))), row.names=FALSE)
cat("\nTotal distinct AVAL values:", nrow(freq_aval), "\n\n")

cat("PROC MEANS DATA=advs NOPRINT;\n")
cat("BY TRTPN AVISITN AVISIT; VAR AVAL;\n")
cat("OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm;\n")
cat("----------------------------------------------------------------\n")
# Show temp with formatting
temp_disp <- temp1 %>%
  mutate(
    mean_f = fmt_num(mean,2),
    lo_f = fmt_num(lo,2),
    hi_f = fmt_num(hi,2),
    stderr_f = fmt_num(stderr,2),
    lclm_f = fmt_num(lclm,2),
    uclm_f = fmt_num(uclm,2)
  ) %>%
  arrange(TRTPN, AVISITN)
print(as.data.frame(temp_disp %>% select(TRTPN, AVISITN, AVISIT, n, mean_f, stderr_f, lo_f, hi_f, lclm_f, uclm_f)), row.names=FALSE)
cat("\n")
cat("Note: lo/hi calculated as mean +/- TINV(0.95, n-1)*stderr (SAS TINV)\n")
cat("      lclm/uclm are PROC MEANS 95% CI using TINV(0.975)\n\n")

cat("Wide dataset ALL (merged by AVISITN AVISIT):\n")
cat("-------------------------------------------\n")
print(as.data.frame(all_wide), row.names=FALSE)
cat("\n")

cat("At Risk Subjects (N per visit per treatment):\n")
cat("---------------------------------------------\n")
atrisk <- all_wide %>% select(xc, x, a_n, b_n, c_n)
print(as.data.frame(atrisk), row.names=FALSE)
cat("\n")

cat("End of P032 listing\n")

sink()
close(con)

# duplicate to .lst.txt
writeLines(readLines(txt_path), con2)
close(con2)

# Also write P032.lst (without extension) for compatibility
file.copy(txt_path, file.path(out, "P032.lst"), overwrite=TRUE)

# DBP_Profile html/text rendering
html_path <- file.path(out, "DBP_Profile.html")
html_txt_path <- file.path(out, "DBP_Profile.txt")
# create simple HTML wrapper
html_content <- paste0(
'<!DOCTYPE html><html><head><meta charset="UTF-8"><title>DBP Profile</title></head><body>',
'<h2>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h2>',
'<p>Source: ADVS where PARAMCD=DIABP, ATPTN=815, ANL01FL=Y, SAFFL=Y, AVISITN&lt;99</p>',
'<p>Figure: ClinicalHandout_DbpProfile.png</p>',
'<img src="clinicalhandout_dbpprofile.png" alt="DBP Profile" style="width:600px;height:400px;">',
'<h3>Summary Statistics (Mean with 95% CI using TINV 0.95)</h3>',
'<table border="1" cellpadding="4" cellspacing="0"><tr><th>TRTPN</th><th>AVISITN</th><th>AVISIT</th><th>N</th><th>Mean</th><th>Lo</th><th>Hi</th></tr>'
)
for (i in 1:nrow(temp1)) {
  html_content <- paste0(html_content,
    sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
      temp1$TRTPN[i], temp1$AVISITN[i], temp1$AVISIT[i], temp1$n[i],
      fmt_num(temp1$mean[i],2), fmt_num(temp1$lo[i],2), fmt_num(temp1$hi[i],2))
  )
}
html_content <- paste0(html_content, '</table><p>At Risk Subjects shown in figure blockplot.</p></body></html>')
writeLines(html_content, html_path)

# DBP_Profile.txt
con3 <- file(html_txt_path, "w")
sink(con3)
cat("DBP_Profile.html - Text Rendering\n")
cat("==================================\n\n")
cat("Title: Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)\n\n")
cat("Graph: ClinicalHandout_DbpProfile.gif (rendered as clinicalhandout_dbpprofile.png)\n")
cat("X-axis: Weeks Since Randomized (0 2 4 6 8 10 12 16 20 24 26)\n")
cat("Y-axis: Mean(unit) with 95% CI\n\n")
cat("Summary data (temp1):\n")
print(as.data.frame(temp1 %>% select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi)), row.names=FALSE)
cat("\nAt Risk table:\n")
print(as.data.frame(all_wide %>% select(xc, x, a_n, b_n, c_n)), row.names=FALSE)
cat("\nHTML file contains embedded image and table.\n")
sink()
close(con3)
# also copy to .html.txt variant
file.copy(html_txt_path, file.path(out, "DBP_Profile.html.txt"), overwrite=TRUE)

# Create figure: ClinicalHandout_DbpProfile
# Use ggplot2 to replicate SAS template
# Prepare data for ggplot
# Colors approximating SAS graphdata2,3,4
cols <- c("0"="#2ca02c", "54"="#1f77b4", "81"="#d62728")
# Actually SAS graphdata2=green, 3=blue, 4=red approx
# We'll use named vector
plot_long$TRTPN_f <- factor(plot_long$TRTPN, levels=c(0,54,81), labels=c("0","54","81"))

# Create base plot
p <- ggplot(plot_long, aes(x=xpos, y=mean, color=TRTPN_f, group=TRTPN_f)) +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=0.3, linewidth=0.8) +
  geom_point(size=3, shape=16) +
  geom_line(aes(x=xpos, y=mean), linetype=c("dashed","dashed","dashed")[1], linewidth=0.8) +
  # Use manual linetypes per group
  scale_color_manual(values=cols, name="Treatment Group:") +
  scale_linetype_manual(values=c("0"="dashed","54"="longdash","81"="solid")) +
  scale_x_continuous(
    breaks=c(0,2,4,6,8,10,12,16,20,24,26),
    limits=c(-0.5, 26.5),
    name="Weeks Since Randomized"
  ) +
  scale_y_continuous(name="Mean(unit) with 95% CI", limits=NULL) +
  labs(title="Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)") +
  theme_minimal(base_size=11) +
  theme(
    plot.title = element_text(hjust=0.5, size=12, face="bold"),
    legend.position="bottom",
    panel.grid.major = element_line(color="grey90"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle=0)
  )

# Add at-risk annotation as text below plot using annotate
# We'll create a separate data frame for at-risk labels
# Instead of complex lattice, add caption with at-risk counts
# Build caption string
atrisk_caption <- paste0(
  "At Risk Subjects:\n",
  paste0(
    sprintf("Week %2s: 0=%2s  54=%2s  81=%2s",
            all_wide$xc,
            ifelse(is.na(all_wide$a_n), "-", all_wide$a_n),
            ifelse(is.na(all_wide$b_n), "-", all_wide$b_n),
            ifelse(is.na(all_wide$c_n), "-", all_wide$c_n)),
    collapse=" | "
  )
)

p2 <- p + labs(caption=atrisk_caption) +
  theme(plot.caption = element_text(hjust=0, size=8, face="plain"))

# Save PNG
png_path <- file.path(out, "clinicalhandout_dbpprofile.png")
ggsave(png_path, plot=p2, width=10, height=7, units="in", dpi=100, bg="white")
# also save with original case name
ggsave(file.path(out, "ClinicalHandout_DbpProfile.png"), plot=p2, width=10, height=7, units="in", dpi=100, bg="white")
# also gif copy (png content but .gif extension for SAS compatibility)
file.copy(png_path, file.path(out, "ClinicalHandout_DbpProfile.gif"), overwrite=TRUE)

# Create enhanced figure with blockplot-like table using gridExtra if available
# Add at-risk table as separate grob below main plot for visual fidelity
# We'll create a second plot showing at-risk counts as text
# Use ggplot for at-risk block
atrisk_long <- all_wide %>%
  select(xc, a_n, b_n, c_n) %>%
  pivot_longer(cols=c(a_n,b_n,c_n), names_to="trt", values_to="n") %>%
  mutate(trt = recode(trt, a_n="0", b_n="54", c_n="81"),
         trt = factor(trt, levels=c("0","54","81")))

# If we want to overlay blockplot, we can add geom_text to main plot at y = min(mean)-offset
y_min <- min(plot_long$lo, na.rm=TRUE)
y_offset <- (max(plot_long$hi, na.rm=TRUE) - y_min) * 0.15
# Add at-risk text annotations
p3 <- p +
  geom_text(data=atrisk_long %>% filter(trt=="0"),
            aes(x=xc, y=y_min - y_offset*0.2, label=n, color=trt),
            size=3, show.legend=FALSE) +
  geom_text(data=atrisk_long %>% filter(trt=="54"),
            aes(x=xc, y=y_min - y_offset*0.5, label=n, color=trt),
            size=3, show.legend=FALSE) +
  geom_text(data=atrisk_long %>% filter(trt=="81"),
            aes(x=xc, y=y_min - y_offset*0.8, label=n, color=trt),
            size=3, show.legend=FALSE) +
  annotate("text", x=-0.5, y=y_min - y_offset*0.2, label="0", hjust=1, size=3, color=cols["0"]) +
  annotate("text", x=-0.5, y=y_min - y_offset*0.5, label="54", hjust=1, size=3, color=cols["54"]) +
  annotate("text", x=-0.5, y=y_min - y_offset*0.8, label="81", hjust=1, size=3, color=cols["81"]) +
  annotate("text", x=-0.5, y=y_min - y_offset*1.1, label="At Risk Subjects", hjust=0, size=3.5, fontface="bold") +
  coord_cartesian(clip="off", ylim=c(y_min - y_offset*1.2, max(plot_long$hi, na.rm=TRUE)*1.05)) +
  theme(plot.margin=margin(10,10,30,40))

ggsave(file.path(out, "clinicalhandout_dbpprofile_block.png"), plot=p3, width=10, height=7, units="in", dpi=100, bg="white")

cat("Outputs written to:", out, "\n")
cat("Files:", paste(list.files(out), collapse=", "), "\n")
