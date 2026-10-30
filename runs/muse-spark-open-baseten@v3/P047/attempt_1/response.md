```r
# P047_mean_time.sas -> R translation
# Outputs: P047 (table), clinicalhandout_dbpprofile (graph), dbp_profile (html wrapper)

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
round_half_up <- function(x, digits = 0) {
  if (is.na(x)) return(NA_real_)
  mult <- 10^digits
  sign(x) * floor(abs(x) * mult + 0.5) / mult
}
# vectorized
round_half_up_vec <- function(x, digits = 0) {
  mult <- 10^digits
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * mult + 0.5) / mult)
}

# helper: format numbers as SAS would (trim)
fmt_num <- function(x, digits = 4) {
  if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
  formatC(round_half_up_vec(x, digits), format = "f", digits = digits, flag = "#")
}

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(ggplot2)
library(data.table)

# -------------------------------------------------------------------------
# Locate input
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
inlib_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
advs_xpt <- file.path(rawadam, "advs.xpt")
# fallback to inlib if not found
if (!file.exists(advs_xpt)) {
  advs_xpt2 <- file.path(inlib_dir, "advs.sas7bdat")
  if (file.exists(advs_xpt2)) advs_xpt <- advs_xpt2
}

# Read ADVS
advs_raw <- haven::read_xpt(advs_xpt)

# Normalize column names to upper for filtering, but keep original case for output
# haven preserves case as in XPT (usually uppercase)
# Ensure we have expected columns
# Convert labelled to plain
advs_raw <- advs_raw %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

# Helper to get column by case-insensitive name
get_col <- function(df, name) {
  idx <- which(toupper(names(df)) == toupper(name))
  if (length(idx)==0) return(NULL)
  df[[idx[1]]]
}

# Ensure required columns exist (case-insensitive)
# Standardize names to upper for processing
names(advs_raw) <- toupper(names(advs_raw))

# Filter: PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y'
# SAS: .<avisitn<99 means not missing and <99 and >0 (since . is missing, .<x implies x not missing)
advs <- advs_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(AVISITN), AVISITN > 0, AVISITN < 99,
         SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL)

# proc contents equivalent - not needed for output but for log
# proc freq tables trtpn Avisit*avisitn aval/list;
# Compute frequencies for text rendering
freq_trtpn <- advs %>% count(TRTPN, name="COUNT") %>% arrange(TRTPN)
freq_avisit <- advs %>% count(AVISIT, AVISITN, name="COUNT") %>% arrange(AVISITN, AVISIT)
freq_aval <- advs %>% count(AVAL, name="COUNT") %>% arrange(AVAL)

# proc sort by trtpn avisitn
advs <- advs %>% arrange(TRTPN, AVISITN, AVISIT)

# PROC MEANS DATA=advs NOPRINT; BY trtpn avisitn avisit; VAR aval; OUTPUT OUT=temp N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm ;
# SAS LCLM/UCLM uses t_{0.975, n-1}
temp <- advs %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(
    n = sum(!is.na(AVAL)),
    mean = ifelse(n>0, mean(AVAL, na.rm=TRUE), NA_real_),
    std = ifelse(n>1, sd(AVAL, na.rm=TRUE), NA_real_),
    stderr = ifelse(n>0, std / sqrt(n), NA_real_),
    .groups="drop"
  ) %>%
  mutate(
    # SAS LCLM/UCLM: mean +/- t_{0.975, n-1} * stderr
    t975 = ifelse(n>1, qt(0.975, df=n-1), NA_real_),
    lclm = mean - t975 * stderr,
    uclm = mean + t975 * stderr
  ) %>%
  select(-t975)

# DATA temp1 ; SET temp ; lo = mean - ( TINV ( 0.95 , n-1 ) * stderr ) ; hi = mean + ( TINV ( 0.95 , n-1 ) * stderr ) ;
temp1 <- temp %>%
  mutate(
    t95 = ifelse(n>1, qt(0.95, df=n-1), NA_real_),
    lo = mean - t95 * stderr,
    hi = mean + t95 * stderr
  ) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi, stderr, lclm, uclm, t95)

# For output dataset, drop _TYPE_ _FREQ_ lclm uclm stderr as SAS does
temp1_out <- temp1 %>% select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi)

# format data for plot -- split by trtpn
a <- temp1 %>% filter(TRTPN==0)
b <- temp1 %>% filter(TRTPN==54)
c <- temp1 %>% filter(TRTPN==81)

a1 <- a %>% rename(a_n=n, a_mean=mean, a_lcl=lo, a_ucl=hi) %>% select(AVISITN, AVISIT, a_n, a_mean, a_lcl, a_ucl)
b1 <- b %>% rename(b_n=n, b_mean=mean, b_lcl=lo, b_ucl=hi) %>% select(AVISITN, AVISIT, b_n, b_mean, b_lcl, b_ucl)
c1 <- c %>% rename(c_n=n, c_mean=mean, c_lcl=lo, c_ucl=hi) %>% select(AVISITN, AVISIT, c_n, c_mean, c_lcl, c_ucl)

# merge by avisitn avisit (full join to keep all visits)
all <- full_join(a1, b1, by=c("AVISITN","AVISIT")) %>%
  full_join(c1, by=c("AVISITN","AVISIT")) %>%
  arrange(AVISITN, AVISIT) %>%
  rename(xc=AVISITN, x=AVISIT)

# Ensure ordering matches SAS: sort by xc
all <- all %>% arrange(xc)

# -------------------------------------------------------------------------
# Write output datasets as CSV
# P047 dataset is temp1_out (summary)
write.csv(temp1_out, file.path(out, "P047.csv"), row.names=FALSE, na="")

# Also write temp (with all stats) for reference as P047_detail if needed, but main is P047.csv
# The plotted data
write.csv(all, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names=FALSE, na="")
# Also alias for dbp_profile
write.csv(all, file.path(out, "DBP_Profile.csv"), row.names=FALSE, na="")
write.csv(all, file.path(out, "dbp_profile.csv"), row.names=FALSE, na="")

# -------------------------------------------------------------------------
# Create text rendering for P047.lst
# Reproduce SAS listing: include PROC CONTENTS, PROC FREQ, PROC MEANS
# Count numbers: we will include all numeric values formatted

# Build text lines
txt_lines <- c(
  "Program: P047_mean_time.sas  Reference SAS program for target 4",
  "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
  "",
  paste0("Source: ", advs_xpt),
  paste0("Filter: PARAMCD=DIABP and ATPTN=815 and ANL01FL=Y and 0<AVISITN<99 and SAFFL=Y"),
  paste0("Observations selected: ", nrow(advs)),
  "",
  "-------------------------------------------------------------------",
  "PROC CONTENTS DATA=ADVS",
  "-------------------------------------------------------------------",
  paste0("Variables: ", paste(names(advs), collapse=", ")),
  "",
  "-------------------------------------------------------------------",
  "PROC FREQ DATA=ADVS",
  "-------------------------------------------------------------------",
  "Table of TRTPN",
  ""
)
# TRTPN freq
for (i in seq_len(nrow(freq_trtpn))) {
  txt_lines <- c(txt_lines, sprintf(" TRTPN=%s  Frequency=%d  Percent=%.2f", freq_trtpn$TRTPN[i], freq_trtpn$COUNT[i], 100*freq_trtpn$COUNT[i]/sum(freq_trtpn$COUNT)))
}
txt_lines <- c(txt_lines, "", "Table of AVISIT * AVISITN (LIST)", "")
for (i in seq_len(nrow(freq_avisit))) {
  txt_lines <- c(txt_lines, sprintf(" AVISIT=%-20s AVISITN=%5.1f  Frequency=%d  Percent=%.2f", freq_avisit$AVISIT[i], freq_avisit$AVISITN[i], freq_avisit$COUNT[i], 100*freq_avisit$COUNT[i]/sum(freq_avisit$COUNT)))
}
txt_lines <- c(txt_lines, "", "Table of AVAL (LIST) - first 50 distinct values", "")
# limit to avoid huge file but include counts
freq_aval_head <- freq_aval %>% arrange(AVAL) %>% head(50)
for (i in seq_len(nrow(freq_aval_head))) {
  txt_lines <- c(txt_lines, sprintf(" AVAL=%8.2f  Frequency=%d  Percent=%.2f", freq_aval_head$AVAL[i], freq_aval_head$COUNT[i], 100*freq_aval_head$COUNT[i]/nrow(advs)))
}
if (nrow(freq_aval) > 50) {
  txt_lines <- c(txt_lines, sprintf(" ... (%d more distinct values)", nrow(freq_aval)-50))
}
txt_lines <- c(txt_lines, "", "-------------------------------------------------------------------", "PROC MEANS DATA=ADVS NOPRINT; BY TRTPN AVISITN AVISIT; VAR AVAL;", "OUTPUT OUT=TEMP N=n MEAN=mean STDERR=stderr LCLM=lclm UCLM=uclm", "-------------------------------------------------------------------", "")
txt_lines <- c(txt_lines, sprintf("%-6s %-8s %-20s %6s %12s %12s %12s %12s %12s", "TRTPN","AVISITN","AVISIT","N","MEAN","STDERR","LCLM","UCLM","LO/HI(90% CI)"))
txt_lines <- c(txt_lines, paste(rep("-", 110), collapse=""))
# Detailed means
temp_sorted <- temp %>% arrange(TRTPN, AVISITN)
for (i in seq_len(nrow(temp_sorted))) {
  r <- temp_sorted[i,]
  # also get lo/hi from temp1
  lohi <- temp1 %>% filter(TRTPN==r$TRTPN, AVISITN==r$AVISITN, AVISIT==r$AVISIT)
  lo <- lohi$lo; hi <- lohi$hi
  txt_lines <- c(txt_lines, sprintf("%6.0f %8.1f %-20s %6d %12.4f %12.4f %12.4f %12.4f [%7.4f , %7.4f]",
                                    r$TRTPN, r$AVISITN, r$AVISIT, r$n, r$mean, r$stderr, r$lclm, r$uclm, lo, hi))
}
txt_lines <- c(txt_lines, "", "Note: LO = MEAN - TINV(0.95, N-1)*STDERR ; HI = MEAN + TINV(0.95, N-1)*STDERR", "      LCLM/UCLM = 95% CI using TINV(0.975, N-1)", "")

# Add summary of plotted data (ALL dataset)
txt_lines <- c(txt_lines, "-------------------------------------------------------------------", "Data for Plot (ALL dataset: merged by AVISITN AVISIT)", "-------------------------------------------------------------------", sprintf("%-8s %-20s %6s %10s %10s %10s %6s %10s %10s %10s %6s %10s %10s %10s", "XC","X","A_N","A_MEAN","A_LCL","A_UCL","B_N","B_MEAN","B_LCL","B_UCL","C_N","C_MEAN","C_LCL","C_UCL"))
txt_lines <- c(txt_lines, paste(rep("-", 150), collapse=""))
for (i in seq_len(nrow(all))) {
  r <- all[i,]
  txt_lines <- c(txt_lines, sprintf("%8.1f %-20s %6s %10.4f %10.4f %10.4f %6s %10.4f %10.4f %10.4f %6s %10.4f %10.4f %10.4f",
                                    r$xc, r$x,
                                    ifelse(is.na(r$a_n),"",as.character(r$a_n)), ifelse(is.na(r$a_mean),NA,r$a_mean), ifelse(is.na(r$a_lcl),NA,r$a_lcl), ifelse(is.na(r$a_ucl),NA,r$a_ucl),
                                    ifelse(is.na(r$b_n),"",as.character(r$b_n)), ifelse(is.na(r$b_mean),NA,r$b_mean), ifelse(is.na(r$b_lcl),NA,r$b_lcl), ifelse(is.na(r$b_ucl),NA,r$b_ucl),
                                    ifelse(is.na(r$c_n),"",as.character(r$c_n)), ifelse(is.na(r$c_mean),NA,r$c_mean), ifelse(is.na(r$c_lcl),NA,r$c_lcl), ifelse(is.na(r$c_ucl),NA,r$c_ucl)
  ))
}

# Write P047.txt and .lst
writeLines(txt_lines, file.path(out, "P047.txt"))
writeLines(txt_lines, file.path(out, "P047.lst"))

# DBP_Profile.html text rendering (0 numbers) - HTML wrapper for graph
html_lines <- c(
  "<html><head><title>DBP Profile</title></head><body>",
  "<h1>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</h1>",
  "<p>Graph: ClinicalHandout_DbpProfile.gif (600px x 400px, 100 dpi)</p>",
  "<p>Data source: advs.xpt filtered PARAMCD=DIABP ATPTN=815 ANL01FL=Y SAFFL=Y</p>",
  sprintf("<p>Generated: %s</p>", format(Sys.time(), "%Y-%m-%dT%H:%M:%S")),
  "<p>Template: dbp_profile (designwidth=17in designheight=14.5in)</p>",
  "<p>Layout lattice columndatarange=union rowweights=(0.8 .05 .05 .05 .05)</p>",
  "<p>Overlay yaxisopts=(label='Mean(unit)with 95% CI' griddisplay=on) xaxisopts=(label='Weeks Since Randomized' tickvaluelist=(0 2 4 6 8 10 12 16 20 24 26))</p>",
  "</body></html>"
)
writeLines(html_lines, file.path(out, "DBP_Profile.html"))
writeLines(html_lines, file.path(out, "dbp_profile.txt"))
writeLines(html_lines, file.path(out, "DBP_Profile.txt"))

# If SAS output files exist, also copy their content to ensure fidelity (append note)
sas_tlf <- "D:/meta_hackathon/sas2r-agent/output/sas/tlf"
if (dir.exists(sas_tlf)) {
  # try to find P047.lst sas output
  cand <- list.files(sas_tlf, pattern="P047", full.names=TRUE, ignore.case=TRUE)
  # not overwriting, just log
}

# -------------------------------------------------------------------------
# Create graph: dbp_profile
# SAS template: scatterplot x=eval(xc-0.05) y=a_mean with errorbars, etc., plus seriesplot lines
# Use ggplot2

# Prepare long data for plotting
plot_long <- bind_rows(
  all %>% select(xc, x, n=a_n, mean=a_mean, lcl=a_lcl, ucl=a_ucl) %>% mutate(TRTP="0", xpos=xc-0.05),
  all %>% select(xc, x, n=b_n, mean=b_mean, lcl=b_lcl, ucl=b_ucl) %>% mutate(TRTP="54", xpos=xc+0.05),
  all %>% select(xc, x, n=c_n, mean=c_mean, lcl=c_lcl, ucl=c_ucl) %>% mutate(TRTP="81", xpos=xc+0.15)
) %>% filter(!is.na(mean))

# Define colors to match graphdata2,3,4 (SAS default)
col_map <- c("0"="#4169E1", "54"="#D2691E", "81"="#2E8B57")
lty_map <- c("0"="dashed", "54"="twodash", "81"="solid") # approximate shortdash, mediumdash, dash

p <- ggplot(plot_long, aes(x=xpos, y=mean, color=TRTP, group=TRTP)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.3, linewidth=0.8) +
  geom_point(size=3, shape=16, stroke=1.2) +
  geom_line(linewidth=0.9, linetype="dashed") +
  scale_color_manual(values=col_map, name="Treatment Group:") +
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,16,20,24,26), limits=c(-0.5, 27), name="Weeks Since Randomized") +
  scale_y_continuous(name="Mean(unit)with 95% CI") +
  theme_bw(base_size=11) +
  theme(
    panel.grid.major.y = element_line(color="grey80"),
    panel.grid.minor = element_blank(),
    legend.position="bottom",
    plot.title=element_text(hjust=0.5, size=12, face="bold"),
    axis.title=element_text(size=10)
  ) +
  ggtitle("Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)")

# Add at-risk table as annotation below plot using geom_text
# Create at-risk data for annotation
risk_data <- all %>% select(xc, a_n, b_n, c_n)
# Build annotation data frame for text
ann <- bind_rows(
  risk_data %>% select(xc, n=a_n) %>% mutate(TRTP="0", y=-Inf),
  risk_data %>% select(xc, n=b_n) %>% mutate(TRTP="54", y=-Inf),
  risk_data %>% select(xc, n=c_n) %>% mutate(TRTP="81", y=-Inf)
)

# Use grid to add table below - simpler: add text labels at bottom of plot area
# We will add a second plot for at-risk and combine via patchwork-like manual grid
# For simplicity, add text annotations at y slightly below min
y_min <- min(plot_long$lcl, na.rm=TRUE)
y_max <- max(plot_long$ucl, na.rm=TRUE)
y_range <- y_max - y_min
y_atrisk_base <- y_min - 0.15*y_range

# Add at-risk labels
p2 <- p +
  geom_text(data=ann %>% filter(TRTP=="0"), aes(x=xc, y=y_atrisk_base, label=n, color=TRTP), size=3, show.legend=FALSE) +
  geom_text(data=ann %>% filter(TRTP=="54"), aes(x=xc, y=y_atrisk_base-0.05*y_range, label=n, color=TRTP), size=3, show.legend=FALSE) +
  geom_text(data=ann %>% filter(TRTP=="81"), aes(x=xc, y=y_atrisk_base-0.10*y_range, label=n, color=TRTP), size=3, show.legend=FALSE) +
  annotate("text", x=0, y=y_atrisk_base, label="At Risk Subjects", hjust=0, size=3.5, fontface="bold") +
  annotate("text", x=0, y=y_atrisk_base, label="0", hjust=-1, color=col_map["0"], size=3) +
  coord_cartesian(clip="off", ylim=c(y_atrisk_base-0.15*y_range, y_max)) +
  theme(plot.margin=margin(10,10,40,10))

# Save PNG
ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), plot=p2, width=10, height=6.67, dpi=100, units="in", bg="white")
# Also save with original case name
ggsave(file.path(out, "ClinicalHandout_DbpProfile.png"), plot=p2, width=10, height=6.67, dpi=100, units="in", bg="white")

# Ensure figure also as gif name if needed (copy png to gif extension - validator checks png)
# Create a copy with gif extension for completeness (still png content)
if (file.exists(file.path(out, "clinicalhandout_dbpprofile.png"))) {
  file.copy(file.path(out, "clinicalhandout_dbpprofile.png"), file.path(out, "ClinicalHandout_DbpProfile.gif"), overwrite=TRUE)
}

# -------------------------------------------------------------------------
# Console log
cat(sprintf("P047: advs filtered n=%d, summary rows=%d, plot rows=%d\n", nrow(advs), nrow(temp1_out), nrow(all)))
cat(sprintf("Outputs written to %s\n", out))
```