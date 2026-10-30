# P036_target16.sas -> R translation
# Outputs: baseline shift table, emergent high table, scatter plot

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# helpers ---------------------------------------------------------------
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a[1]) && a[1]!="") a else b
round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_comma <- function(x){
  ifelse(is.na(x), "",
         formatC(x, format="d", big.mark=","))
}
fmt_comma8 <- function(x){
  # SAS comma8. : width 8, comma, no decimals
  trimws(formatC(x, format="d", big.mark=","))
}
fmt_10_1 <- function(x){
  # SAS 10.1 : width 10, 1 decimal, round half away
  ifelse(is.na(x), "",
         trimws(formatC(round_half_up(x,1), format="f", digits=1, big.mark=",")))
}
fmt_comma10 <- function(x){
  trimws(formatC(x, format="d", big.mark=","))
}
fmt_pvalue <- function(x){
  # SAS pvalue. format: 4 decimals, <0.0001 etc, leading blank? We'll mimic
  ifelse(is.na(x), "",
         ifelse(x < 0.0001, "<.0001",
                sprintf("%6.4f", round_half_up(x,4))))
}
# SAS picture percpar low-high="009.9)" (prefix="(") truncates not rounds
# Implement as floor(x*10)/10 then sprintf("(%.1f)", ...)
fmt_percpar <- function(pct){
  # pct is already percent (0-100)
  ifelse(is.na(pct), "",
         sprintf("(%.1f)", floor(pct*10 + 1e-9)/10))
}
# For display in text table we need count and percent with percpar
# percent in TableA is 0-100 already

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(ggplot2)

# locate input ----------------------------------------------------------
root <- Sys.getenv("ROOT")
if (root=="" || is.na(root)) root <- "D:/meta_hackathon/sas2r-agent"
rawadam <- Sys.getenv("rawadam")
if (rawadam=="" || is.na(rawadam)) rawadam <- Sys.getenv("RAWADAM")
if (rawadam=="" || is.na(rawadam)) rawadam <- file.path(root, "data/raw/cdisc-pilot/adam")
rawsplit <- Sys.getenv("rawsplit")
inlib_dir <- file.path(root, "data/derived/cdisc-pilot/adam")

candidates <- c(
  file.path(rawadam, "advsmax.xpt"),
  file.path(rawadam, "ADVSMAX.xpt"),
  file.path(inlib_dir, "advsmax.xpt"),
  file.path(inlib_dir, "advsmax.sas7bdat"),
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt"
)
advsmax_path <- NULL
for (p in candidates) if (file.exists(p)) { advsmax_path <- p; break }

if (is.null(advsmax_path)) {
  # try list files
  allx <- list.files(rawadam, pattern="advsmax", ignore.case=TRUE, full.names=TRUE, recursive=TRUE)
  if (length(allx)>0) advsmax_path <- allx[1]
}
if (is.null(advsmax_path) || !file.exists(advsmax_path)) stop(paste("advsmax.xpt not found; tried", paste(candidates, collapse="; ")))

if (grepl("\\.xpt$", advsmax_path, ignore.case=TRUE)) {
  advsmax <- haven::read_xpt(advsmax_path)
} else {
  advsmax <- haven::read_sas(advsmax_path)
}

# Ensure column names uppercase as SAS
names(advsmax) <- toupper(names(advsmax))

# advsmax2 --------------------------------------------------------------
# where ((PARAMCD="DIABP") and (ATPTN=815) and (ANL01FL="Y") and (ADY > 1) and (SAFFL="Y"))
# keep USUBJID TRTPN PARAM PARAMCD AVAL ANL01FL BNRIND ANRIND SHIFT1 CRIT1FL ANRLO ANRHI BASE AVAL Ref
# if not nmiss(base, aval) then Ref = Base;

# Ensure needed cols exist
needed <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","ADY","ATPTN","SAFFL")
for (cc in needed) if (!cc %in% names(advsmax)) advsmax[[cc]] <- NA

advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y") %>%
  mutate(
    TRTPN = as.numeric(TRTPN),
    AVAL = as.numeric(AVAL),
    BASE = as.numeric(BASE),
    ANRLO = as.numeric(ANRLO),
    ANRHI = as.numeric(ANRHI),
    BNRIND = as.character(BNRIND),
    ANRIND = as.character(ANRIND),
    CRIT1FL = as.character(CRIT1FL)
  ) %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND) | BNRIND=="", "", BNRIND),
    ANRIND = ifelse(is.na(ANRIND) | ANRIND=="", "", ANRIND),
    CRIT1FL = ifelse(is.na(CRIT1FL), "", CRIT1FL)
  ) %>%
  mutate(Ref = ifelse(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL, BNRIND, ANRIND, SHIFT1, CRIT1FL, ANRLO, ANRHI, BASE, Ref)

# sort by TRTPN (SAS sort)
advsmax2 <- advsmax2 %>% arrange(TRTPN)

# Baseline shift frequency table ----------------------------------------
# PROC FREQ by TRTPN; table BNRIND*ANRIND / missing out=TableA
# Need COUNT and PERCENT per BY group

# Helper to compute freq for one BY group
compute_tableA <- function(df){
  # df is subset for one TRTPN
  trt <- unique(df$TRTPN)
  n_total <- nrow(df)
  # cross tab including missing (but we already converted missing to "")
  tab <- df %>%
    group_by(BNRIND, ANRIND) %>%
    summarise(COUNT=n(), .groups="drop") %>%
    mutate(PERCENT = COUNT / n_total * 100)
  tab$TRTPN <- trt
  tab
}
# Also need BNRIND, ANRIND, TRTPN marginal tables for completeness but not used except TRTPN for GroupN
trtpn_freq <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(COUNT=n(), .groups="drop") %>%
  mutate(PERCENT = COUNT / nrow(advsmax2) * 100)

# Build TableA
TableA_list <- lapply(sort(unique(advsmax2$TRTPN)), function(t){
  compute_tableA(filter(advsmax2, TRTPN==t))
})
TableA <- bind_rows(TableA_list) %>%
  arrange(TRTPN, BNRIND, ANRIND)

# SAS TableA has columns TRTPN BNRIND ANRIND COUNT PERCENT
# Add ValueFmt
TableA <- TableA %>%
  mutate(
    ValueFmt = paste0(trimws(fmt_comma8(COUNT)), " (", trimws(fmt_10_1(PERCENT)), ")")
  )

# Merge on Group Ns and add to group label
# TRTPN dataset rename Count=GroupN Percent=GroupPercent
TRTPN_lookup <- trtpn_freq %>% rename(GroupN=COUNT, GroupPercent=PERCENT) %>% select(TRTPN, GroupN, GroupPercent)

TableA <- TableA %>%
  left_join(TRTPN_lookup, by="TRTPN") %>%
  mutate(
    GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", trimws(fmt_comma10(GroupN)), ")")
  )

# Transpose (not strictly needed for report but create for CSV)
TableAt <- TableA %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from=ANRIND, values_from=ValueFmt, names_prefix="ValueFmt_")

# Treatment emergent high table -----------------------------------------
advsmax3 <- advsmax2 %>%
  filter(CRIT1FL != "") %>%
  mutate(n = as.integer((row_number() %% 17) == 1))  # mod(_n_,17)=1 ; _n_ is row number in this dataset

# PROC SUMMARY nway class TRTPN var n output n=Nx sum=n mean=percent
EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(
    Nx = sum(!is.na(n)),
    n = sum(n, na.rm=TRUE),
    percent = mean(n, na.rm=TRUE),
    .groups="drop"
  ) %>%
  arrange(TRTPN)

# Fisher's exact test for TRTPN * n
# Need to compute Fisher's exact p-value for 2xC? Actually TRTPN has 3 levels (0,1,2?) and n is 0/1 => 3x2 table
# SAS PROC FREQ with exact gives XP2_FISH (exact p-value for chi-square)
# We can compute via fisher.test on the contingency table
if (nrow(advsmax3) > 0) {
  tab_fisher <- table(advsmax3$TRTPN, advsmax3$n)
  # fisher.test with simulate? For larger than 2x2, fisher.test computes exact
  ft <- tryCatch(fisher.test(tab_fisher), error=function(e) NULL)
  fish_p <- if (!is.null(ft)) ft$p.value else NA_real_
} else {
  fish_p <- NA_real_
}
FishersExact <- data.frame(Name1="XP2_FISH", nValue1=fish_p)
PValue <- FishersExact %>% filter(Name1=="XP2_FISH") %>% rename(FishersP=nValue1)

# Merge and format percent
EmergentHigh2 <- EmergentHigh %>%
  mutate(FishersP = PValue$FishersP[1]) %>%
  mutate(percent = ifelse(!is.na(percent), 100*percent, NA_real_))

# For display, format percent 6.1 and FishersP pvalue.
# Keep numeric for CSV, but also create formatted versions for text

# Summary for graph axis ------------------------------------------------
MAJOR <- 10
# PROC SUMMARY nway var ANRLO ANRHI BASE AVAL output mean= min(AVAL)=MinValue max(AVAL)=MaxValue
ADVSMAXSum <- advsmax2 %>%
  summarise(
    ANRLO = mean(ANRLO, na.rm=TRUE),
    ANRHI = mean(ANRHI, na.rm=TRUE),
    BASE_mean = mean(BASE, na.rm=TRUE),
    AVAL_mean = mean(AVAL, na.rm=TRUE),
    MinValue = min(AVAL, na.rm=TRUE),
    MaxValue = max(AVAL, na.rm=TRUE),
    .groups="drop"
  )
anrlo_val <- ADVSMAXSum$ANRLO[1]
anrhi_val <- ADVSMAXSum$ANRHI[1]
minv <- ADVSMAXSum$MinValue[1]
maxv <- ADVSMAXSum$MaxValue[1]
margin <- (maxv - minv)/100
YAxisMin <- MAJOR * floor((minv - margin)/MAJOR)
YAxisMax <- MAJOR * ceiling((maxv + margin)/MAJOR)

# Graph -----------------------------------------------------------------
# Colors: blue, red, green for TRTPN levels sorted
trt_levels <- sort(unique(advsmax2$TRTPN))
# Map to colors: SAS goptions colors=(blue,red,green) and symbol1,2,3
# symbol1 triangle green, symbol2 circle red, symbol3 x blue
# But plot AVAL*base=TRTPN uses TRTPN as group; order is numeric ascending
col_map <- c("0"="blue", "1"="red", "2"="green")
shape_map <- c("0"=4, "1"=16, "2"=17) # x, circle, triangle
# Ensure mapping covers actual levels
pal_cols <- sapply(as.character(trt_levels), function(x) col_map[x] %||% "black")
pal_shapes <- sapply(as.character(trt_levels), function(x) shape_map[x] %||% 16)

p <- ggplot() +
  geom_point(data=advsmax2, aes(x=BASE, y=AVAL, color=factor(TRTPN), shape=factor(TRTPN)), size=2) +
  geom_line(data=advsmax2 %>% arrange(BASE), aes(x=BASE, y=Ref), color="black", linetype="dashed", linewidth=0.6) +
  geom_hline(yintercept=c(anrlo_val, anrhi_val), linetype="dashed", color=c("blue","red")) +
  geom_vline(xintercept=c(anrlo_val, anrhi_val), linetype="dashed", color=c("blue","red")) +
  scale_color_manual(values=pal_cols, name="TRTPN") +
  scale_shape_manual(values=pal_shapes, name="TRTPN") +
  scale_x_continuous(limits=c(YAxisMin, YAxisMax), breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  scale_y_continuous(limits=c(YAxisMin, YAxisMax), breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x="Max. Baseline", y="Max. Post baseline") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(hjust=0.5, size=11, face="bold"),
        legend.position="bottom")

# Save graph
ggsave(file.path(out, "sasgraph.png"), plot=p, width=8, height=6, dpi=300, bg="white")

# CSV of plotted data
plot_data <- advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, Ref, ANRLO, ANRHI)
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names=FALSE, na="")

# Prepare tables for text rendering -------------------------------------

# Baseline shift table: need to reproduce PROC REPORT with across ANRIND
# Columns: GroupFmt BNRIND ("Max Post Basline Result (shift from baseline)" (ANRIND,("N (%)" (count percent))))
# So for each GroupFmt/BNRIND, we have counts per ANRIND

# Get distinct ANRIND levels in SAS order (character ordering, uppercase before lowercase, missing first)
anrind_levels <- sort(unique(TableA$ANRIND))
bnrind_levels <- sort(unique(TableA$BNRIND))
group_levels <- unique(TableA$GroupFmt) # already in TRTPN order

# Build wide display matrix for text
# For each GroupFmt/BNRIND, we need count and percent per ANRIND
wide_counts <- TableA %>%
  select(GroupFmt, TRTPN, BNRIND, ANRIND, COUNT, PERCENT) %>%
  arrange(TRTPN, BNRIND, ANRIND)

# Create pivot for counts and percents
pivot_count <- wide_counts %>% select(GroupFmt, BNRIND, ANRIND, COUNT) %>% pivot_wider(names_from=ANRIND, values_from=COUNT, values_fill=0)
pivot_percent <- wide_counts %>% select(GroupFmt, BNRIND, ANRIND, PERCENT) %>% pivot_wider(names_from=ANRIND, values_from=PERCENT, values_fill=0)

# For CSV output of baseline shift (P036)
# Provide long format with formatted ValueFmt
write.csv(TableA %>% select(TRTPN, GroupFmt, GroupN, GroupPercent, BNRIND, ANRIND, COUNT, PERCENT, ValueFmt),
          file.path(out, "P036.csv"), row.names=FALSE, na="")

# Build text rendering for P036.lst (106 numbers)
# Count numbers: each cell has count and percent => 2 numbers per cell, plus GroupN etc.
# We'll create a plain text table similar to PROC REPORT

con_p036 <- file(file.path(out, "P036.txt"), open="wt")
cat("Baseline Shift Frequency Table\n", file=con_p036)
cat("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)\n", file=con_p036)
cat(paste0("Reference limits: ANRLO=", round_half_up(anrlo_val,2), " ANRHI=", round_half_up(anrhi_val,2),
           " YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax, " by ", MAJOR, "\n"), file=con_p036)
cat("\n", file=con_p036)
cat("Max Post Basline Result (shift from baseline)\n", file=con_p036)
# Header
header1 <- sprintf("%-20s %-20s", "Treatment", "Max. Baseline Result")
for (lvl in anrind_levels) {
  header1 <- paste0(header1, sprintf(" | %-18s", ifelse(lvl=="", "Missing", lvl)))
}
cat(header1, "\n", file=con_p036)
cat(paste(rep("-", nchar(header1)), collapse=""), "\n", file=con_p036)
# Rows
for (gf in group_levels) {
  for (br in bnrind_levels) {
    # find rows for this gf/br
    sub <- TableA %>% filter(GroupFmt==gf, BNRIND==br)
    if (nrow(sub)==0) next
    line <- sprintf("%-20s %-20s", gf, ifelse(br=="", "Missing", br))
    for (lvl in anrind_levels) {
      rec <- sub %>% filter(ANRIND==lvl)
      if (nrow(rec)==0) {
        cell <- sprintf("%4s %6s", "0", "(0.0)")
      } else {
        cnt <- rec$COUNT[1]
        pct <- rec$PERCENT[1]
        # SAS percpar truncates: use fmt_percpar
        # But TableA percent is already 0-100; need to format as "(x.x)"
        # Use truncation
        pct_str <- fmt_percpar(pct)
        cell <- sprintf("%4d %6s", cnt, pct_str)
      }
      line <- paste0(line, sprintf(" | %-18s", cell))
    }
    cat(line, "\n", file=con_p036)
  }
  cat("\n", file=con_p036)
}
cat("\n", file=con_p036)
cat("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.\n", file=con_p036)
cat(paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", anrlo_val, " mmol/L ULNx ", anrhi_val,
           " mmol/L) set 2 (Female, Age 18-65, LLN=", anrlo_val, " mmol/L ULN=", anrhi_val, " mmol/L).\n"), file=con_p036)
close(con_p036)

# Target16 table: Emergent High -----------------------------------------
# Columns TRTPN ("Treatment Emergent High" (Nx n percent FishersP))
# Write CSV
write.csv(EmergentHigh2 %>% select(TRTPN, Nx, n, percent, FishersP),
          file.path(out, "Target16.csv"), row.names=FALSE, na="")

# Text rendering for Target16.pdf (41 numbers)
con_t16 <- file(file.path(out, "Target16.txt"), open="wt")
cat("Treatment Emergent High Table\n", file=con_t16)
cat("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot\n", file=con_t16)
cat(paste0("Reference limits: ANRLO=", anrlo_val, " ANRHI=", anrhi_val, "\n"), file=con_t16)
cat("\n", file=con_t16)
cat(sprintf("%-10s %-10s %-10s %-10s %-12s\n", "TRTPN", "Nx", "n", "%", "FishersP"), file=con_t16)
cat(paste(rep("-", 55), collapse=""), "\n", file=con_t16)
for (i in seq_len(nrow(EmergentHigh2))) {
  r <- EmergentHigh2[i,]
  cat(sprintf("%-10s %-10d %-10d %-10s %-12s\n",
              as.character(r$TRTPN),
              r$Nx,
              r$n,
              sprintf("%.1f", round_half_up(r$percent,1)),
              fmt_pvalue(r$FishersP)), file=con_t16)
}
cat("\n", file=con_t16)
cat("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.\n", file=con_t16)
cat(paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", anrlo_val, " mmol/L ULNx ", anrhi_val,
           " mmol/L) set 2 (Female, Age 18-65, LLN=", anrlo_val, " mmol/L ULN=", anrhi_val, " mmol/L).\n"), file=con_t16)
cat("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot\n", file=con_t16)
close(con_t16)

# Also write dataset outputs as CSV (advsmax2, TableA, EmergentHigh)
write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out, "TableA.csv"), row.names=FALSE, na="")
write.csv(EmergentHigh2, file.path(out, "EmergentHigh.csv"), row.names=FALSE, na="")

# Ensure required output names exist (copy for expected names)
# Expected: P036.lst -> we have P036.txt/csv ; also create P036.lst.txt if needed
if (!file.exists(file.path(out, "P036.lst.txt"))) file.copy(file.path(out, "P036.txt"), file.path(out, "P036.lst.txt"), overwrite=TRUE)
if (!file.exists(file.path(out, "Target16.pdf.txt"))) file.copy(file.path(out, "Target16.txt"), file.path(out, "Target16.pdf.txt"), overwrite=TRUE)
