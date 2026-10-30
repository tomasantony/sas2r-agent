# P053_outliers_Scatter_Quant.sas -> R translation
# Produces: P053.txt/csv, Target16.txt/csv, sasgraph.png/csv + intermediate datasets

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(readr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

# helper: SAS round half away from zero
sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits)
}
fmt_comma <- function(x) {
  ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ",", digits = 0))
}
fmt_10_1 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.1f", sas_round(x, 1)))
}
# picture percpar low-high="009.9)" prefix="("  -> truncate not round
fmt_percpar <- function(x) {
  ifelse(is.na(x), "",
         sprintf("(%.1f)", floor(x * 10 + 1e-9) / 10))
}
fmt_pvalue <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.0001, "<.0001", sprintf("%.4f", sas_round(p, 4))))
}
# find input file
find_xpt <- function(fname) {
  cand <- c(
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", fname),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", fname),
    file.path("/data/raw/cdisc-pilot/adam", fname),
    file.path("data/raw/cdisc-pilot/adam", fname),
    file.path("data/derived/cdisc-pilot/adam", fname),
    file.path(getwd(), "data/raw/cdisc-pilot/adam", fname),
    file.path(getwd(), fname),
    fname
  )
  for (p in cand) if (file.exists(p)) return(p)
  # recursive search fallback
  roots <- c(getwd(), ".", "/data", "D:/meta_hackathon/sas2r-agent")
  for (r in roots) {
    if (dir.exists(r)) {
      f <- list.files(r, pattern = fname, recursive = TRUE, full.names = TRUE)
      if (length(f) > 0) return(f[1])
    }
  }
  return(NA)
}
xpt_path <- find_xpt("advsmax.xpt")
if (is.na(xpt_path)) {
  # try advs.xpt alternative
  xpt_path <- find_xpt("advs.xpt")
}
if (is.na(xpt_path)) stop("advsmax.xpt not found; searched candidates")

advsmax_raw <- haven::read_xpt(xpt_path)
# zap labels/formats to plain vectors
advsmax_raw <- haven::zap_labels(advsmax_raw)
advsmax_raw <- haven::zap_formats(advsmax_raw)
# standardise names to upper for case-insensitive handling
names(advsmax_raw) <- toupper(names(advsmax_raw))

# ensure needed columns exist (create NA if missing)
need_cols <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
               "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","ATPTN","ADY","SAFFL")
for (cn in need_cols) if (!cn %in% names(advsmax_raw)) advsmax_raw[[cn]] <- NA

# coerce types
advsmax_raw$TRTPN <- suppressWarnings(as.numeric(advsmax_raw$TRTPN))
advsmax_raw$ATPTN <- suppressWarnings(as.numeric(advsmax_raw$ATPTN))
advsmax_raw$ADY <- suppressWarnings(as.numeric(advsmax_raw$ADY))
advsmax_raw$AVAL <- suppressWarnings(as.numeric(advsmax_raw$AVAL))
advsmax_raw$BASE <- suppressWarnings(as.numeric(advsmax_raw$BASE))
advsmax_raw$ANRLO <- suppressWarnings(as.numeric(advsmax_raw$ANRLO))
advsmax_raw$ANRHI <- suppressWarnings(as.numeric(advsmax_raw$ANRHI))
# character columns
char_cols <- c("PARAMCD","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","SAFFL","USUBJID","PARAM")
for (cn in char_cols) advsmax_raw[[cn]] <- as.character(advsmax_raw[[cn]])

# data advsmax2
advsmax2 <- advsmax_raw %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         SAFFL == "Y") %>%
  mutate(REF = ifelse(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)) %>%
  select(USUBJID, TRTPN, PARAM, PARAMCD, AVAL, ANL01FL, BNRIND, ANRIND, SHIFT1, CRIT1FL,
         ANRLO, ANRHI, BASE, REF) %>%
  arrange(TRTPN)

# handle missing character as "" for FREQ missing option
advsmax2$BNRIND[is.na(advsmax2$BNRIND)] <- ""
advsmax2$ANRIND[is.na(advsmax2$ANRIND)] <- ""
advsmax2$CRIT1FL[is.na(advsmax2$CRIT1FL)] <- ""

# sort by TRTPN (SAS sort)
advsmax2 <- advsmax2 %>% arrange(TRTPN)

# PROC FREQ baseline shift
# TableA: BNRIND*ANRIND per TRTPN
total_by_trtpn <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups="drop")
overall_n <- nrow(advsmax2)
trtpn_freq <- total_by_trtpn %>% mutate(GroupPercent = GroupN / overall_n * 100)

TableA_counts <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(COUNT = n(), .groups="drop") %>%
  left_join(total_by_trtpn, by="TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100)

# ValueFmt and GroupFmt as SAS
TableA <- TableA_counts %>%
  mutate(
    ValueFmt = paste0(trimws(fmt_comma(COUNT)), " (", trimws(fmt_10_1(PERCENT)), ")"),
    GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", trimws(fmt_comma(GroupN)), ")")
  ) %>%
  left_join(trtpn_freq %>% select(TRTPN, GroupN2 = GroupN, GroupPercent), by="TRTPN") %>%
  arrange(TRTPN, BNRIND, ANRIND)

# transpose (for completeness)
TableAt <- TableA %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from = ANRIND, values_from = ValueFmt, names_prefix = "ValueFmt")

# Treatment emergent high
advsmax3 <- advsmax2 %>% filter(CRIT1FL != "")
# n = mod(_n_,17)=1  -> 1 every 17 starting at 1
if (nrow(advsmax3) > 0) {
  advsmax3$n <- as.integer((seq_len(nrow(advsmax3)) %% 17) == 1)
} else {
  advsmax3$n <- integer(0)
}

EmergentHigh_sum <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(Nx = sum(!is.na(n)),
            n = sum(n, na.rm=TRUE),
            percent = mean(n, na.rm=TRUE),
            .groups="drop") %>%
  arrange(TRTPN)

# Fisher's exact TRTPN * n
FishersP_val <- NA_real_
if (nrow(advsmax3) > 0 && length(unique(advsmax3$TRTPN)) > 1) {
  tab <- table(factor(advsmax3$TRTPN), factor(advsmax3$n))
  # use fisher.test; for 3x2 SAS uses exact
  ft <- tryCatch(fisher.test(tab), error=function(e) NULL)
  if (!is.null(ft)) FishersP_val <- ft$p.value
}
FishersExact <- data.frame(Name1="XP2_FISH", nValue1=FishersP_val, stringsAsFactors=FALSE)
PValue <- FishersExact %>% filter(Name1=="XP2_FISH") %>% mutate(FishersP = nValue1) %>% select(FishersP)

EmergentHigh <- EmergentHigh_sum %>%
  mutate(percent = percent * 100)  # SAS: if n(percent) then percent=100*percent

# merge without BY -> first row gets pvalue only (SAS semantics)
EmergentHigh$FishersP <- NA_real_
if (nrow(EmergentHigh) > 0 && nrow(PValue) > 0) {
  EmergentHigh$FishersP[1] <- PValue$FishersP[1]
}
# format percent 6.1 already numeric; keep numeric for CSV, formatted for display later

# ADVSMAXSum for graph axis
MAJOR <- 10
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
# handle case where all NA
if (nrow(ADVSMAXSum)==0 || is.na(ADVSMAXSum$MinValue)) {
  ADVSMAXSum <- data.frame(ANRLO=NA_real_, ANRHI=NA_real_, BASE_mean=NA_real_, AVAL_mean=NA_real_, MinValue=NA_real_, MaxValue=NA_real_)
}
anrlo_val <- ADVSMAXSum$ANRLO[1]
anrhi_val <- ADVSMAXSum$ANRHI[1]
minv <- ADVSMAXSum$MinValue[1]
maxv <- ADVSMAXSum$MaxValue[1]
margin <- (maxv - minv) / 100
YAxisMin <- MAJOR * floor((minv - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((maxv + margin) / MAJOR)
# fallback if NA
if (is.na(YAxisMin) || is.na(YAxisMax) || !is.finite(YAxisMin)) {
  YAxisMin <- 0; YAxisMax <- 100
}

# write intermediate datasets as CSV
write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out, "TableA.csv"), row.names=FALSE, na="")
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names=FALSE, na="")
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names=FALSE, na="")
write.csv(ADVSMAXSum, file.path(out, "ADVSMAXSum.csv"), row.names=FALSE, na="")

# Graph
# prepare data for plot
plot_data <- advsmax2
# ensure TRTPN factor for legend
plot_data$TRTPN_f <- factor(plot_data$TRTPN, levels=sort(unique(plot_data$TRTPN)))

# color/shape mapping: SAS symbol1 green triangle, symbol2 red circle, symbol3 blue x
# map in order of sorted TRTPN levels
levs <- levels(plot_data$TRTPN_f)
col_map <- c("green","red","blue","orange","purple","black")
shape_map <- c(2,1,4,0,3,5) # triangle, circle, x
names(col_map) <- levs
names(shape_map) <- levs
col_vals <- col_map[levs]
shape_vals <- shape_map[levs]

p <- ggplot(plot_data, aes(x=BASE, y=AVAL, color=TRTPN_f, shape=TRTPN_f)) +
  geom_point(size=2) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="black", linewidth=0.6) +
  geom_vline(xintercept=anrlo_val, linetype="dashed", color="blue", linewidth=0.5) +
  geom_vline(xintercept=anrhi_val, linetype="dashed", color="red", linewidth=0.5) +
  geom_hline(yintercept=anrlo_val, linetype="dashed", color="blue", linewidth=0.5) +
  geom_hline(yintercept=anrhi_val, linetype="dashed", color="red", linewidth=0.5) +
  scale_color_manual(values=col_vals, name="TRTPN") +
  scale_shape_manual(values=shape_vals, name="TRTPN") +
  scale_x_continuous(limits=c(YAxisMin, YAxisMax), breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  scale_y_continuous(limits=c(YAxisMin, YAxisMax), breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x="Max. Baseline", y="Max. Post baseline") +
  theme_minimal(base_size=10) +
  theme(legend.position="bottom",
        plot.title=element_text(hjust=0.5, size=10, face="bold"))

# save PNG
png_path <- file.path(out, "sasgraph.png")
ggsave(filename=png_path, plot=p, width=9, height=7.5, units="in", dpi=300)

# CSV of plotted data
write.csv(plot_data %>% select(USUBJID, TRTPN, BASE, AVAL, REF, BNRIND, ANRIND, ANRLO, ANRHI),
          file.path(out, "sasgraph.csv"), row.names=FALSE, na="")

# Build text renderings
# Helper to format table for text
fmt_count_percent <- function(cnt, pct) {
  paste0(fmt_comma(cnt), " (", fmt_10_1(pct), ")")
}

# Prepare baseline shift display matrix
# Get unique ANRIND levels sorted SAS style (character, missing first)
anrind_levels <- sort(unique(TableA$ANRIND))
bnrind_levels <- sort(unique(TableA$BNRIND))
trtpn_levels <- sort(unique(TableA$TRTPN))

# Create wide display for report
report_wide <- TableA %>%
  select(GroupFmt, TRTPN, BNRIND, ANRIND, COUNT, PERCENT) %>%
  arrange(TRTPN, BNRIND, ANRIND)

# Build P053.txt (106 numbers) - detailed listing
p053_lines <- c(
  "P053 Outliers Scatter Quant",
  "=============================",
  "",
  "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
  paste0("Reference limits: ANRLO = ", sprintf("%.2f", anrlo_val), "  ANRHI = ", sprintf("%.2f", anrhi_val)),
  paste0("Y-axis: ", YAxisMin, " to ", YAxisMax, " by ", MAJOR, "  Margin = ", sprintf("%.4f", margin)),
  paste0("Min AVAL = ", sprintf("%.2f", minv), "  Max AVAL = ", sprintf("%.2f", maxv)),
  paste0("Mean BASE = ", sprintf("%.4f", ADVSMAXSum$BASE_mean), "  Mean AVAL = ", sprintf("%.4f", ADVSMAXSum$AVAL_mean)),
  paste0("Mean ANRLO = ", sprintf("%.4f", ADVSMAXSum$ANRLO), "  Mean ANRHI = ", sprintf("%.4f", ADVSMAXSum$ANRHI)),
  "",
  "Baseline Shift Frequency Table (BNRIND * ANRIND by TRTPN)",
  "----------------------------------------------------------"
)
# header
hdr <- paste0(sprintf("%-20s", "Treatment"), " | ",
              sprintf("%-18s", "Max Baseline"), " | ",
              paste(sapply(anrind_levels, function(a) sprintf("%-18s", ifelse(a=="", "Missing", a))), collapse=" | "))
p053_lines <- c(p053_lines, hdr)
p053_lines <- c(p053_lines, paste(rep("-", nchar(hdr)), collapse=""))
for (tr in trtpn_levels) {
  grp <- unique(TableA$GroupFmt[TableA$TRTPN==tr])
  p053_lines <- c(p053_lines, paste0("Group: ", grp))
  for (bn in bnrind_levels) {
    row_vals <- c()
    for (an in anrind_levels) {
      rec <- TableA %>% filter(TRTPN==tr, BNRIND==bn, ANRIND==an)
      if (nrow(rec)==0) {
        row_vals <- c(row_vals, sprintf("%4s %8s", "", ""))
      } else {
        cnt <- rec$COUNT[1]; pct <- rec$PERCENT[1]
        # SAS percpar truncated display
        pct_trunc <- fmt_percpar(pct)
        row_vals <- c(row_vals, sprintf("%4s %-8s", fmt_comma(cnt), pct_trunc))
      }
    }
    line <- paste0(sprintf("%-20s", grp), " | ",
                   sprintf("%-18s", ifelse(bn=="", "Missing", bn)), " | ",
                   paste(row_vals, collapse=" | "))
    p053_lines <- c(p053_lines, line)
  }
  p053_lines <- c(p053_lines, "")
}
p053_lines <- c(p053_lines,
  "",
  "Treatment Emergent High Table",
  "------------------------------",
  sprintf("%-10s %-8s %-8s %-8s %-12s", "TRTPN", "Nx", "n", "%", "FishersP"),
  paste(rep("-", 50), collapse="")
)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  p053_lines <- c(p053_lines,
    sprintf("%-10s %-8s %-8s %-8s %-12s",
            as.character(r$TRTPN),
            fmt_comma(r$Nx),
            fmt_comma(r$n),
            sprintf("%.1f", sas_round(r$percent,1)),
            fmt_pvalue(r$FishersP))
  )
}
p053_lines <- c(p053_lines,
  "",
  "Footnotes:",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", sprintf("%.2f", anrlo_val), " mmol/L ULNx ", sprintf("%.2f", anrhi_val), " mmol/L) set 2 (Female, Age 18-65, LLN=", sprintf("%.2f", anrlo_val), " mmol/L ULN=", sprintf("%.2f", anrhi_val), " mmol/L)."),
  "",
  paste0("ADVSMAXSum: ANRLO mean=", sprintf("%.4f", ADVSMAXSum$ANRLO), " ANRHI mean=", sprintf("%.4f", ADVSMAXSum$ANRHI),
         " BASE mean=", sprintf("%.4f", ADVSMAXSum$BASE_mean), " AVAL mean=", sprintf("%.4f", ADVSMAXSum$AVAL_mean),
         " MinValue=", sprintf("%.4f", minv), " MaxValue=", sprintf("%.4f", maxv))
)

writeLines(p053_lines, file.path(out, "P053.txt"))
# P053.csv - table body for baseline shift + emergent high combined
# Create CSV that contains all numbers from TableA and EmergentHigh
p053_csv <- TableA %>% select(TRTPN, GroupFmt, GroupN=GroupN, BNRIND, ANRIND, COUNT, PERCENT, ValueFmt)
write.csv(p053_csv, file.path(out, "P053.csv"), row.names=FALSE, na="")

# Target16.txt / Target16.csv  (PDF content)
target_lines <- c(
  "Target16.pdf - Scatter Plot and Summary Tables",
  "===============================================",
  "",
  "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
  paste0("VREF = ", sprintf("%.2f", anrlo_val), ", ", sprintf("%.2f", anrhi_val),
         "  HREF = ", sprintf("%.2f", anrlo_val), ", ", sprintf("%.2f", anrhi_val)),
  paste0("VAXIS = ", YAxisMin, " to ", YAxisMax, " by ", MAJOR),
  paste0("ANRLO = ", sprintf("%.4f", anrlo_val), "  ANRHI = ", sprintf("%.4f", anrhi_val)),
  "",
  "Baseline Shift Table (Max Post Baseline Result shift from baseline)",
  "-------------------------------------------------------------------"
)
target_lines <- c(target_lines, hdr)
target_lines <- c(target_lines, paste(rep("-", nchar(hdr)), collapse=""))
for (tr in trtpn_levels) {
  grp <- unique(TableA$GroupFmt[TableA$TRTPN==tr])
  for (bn in bnrind_levels) {
    row_vals <- c()
    for (an in anrind_levels) {
      rec <- TableA %>% filter(TRTPN==tr, BNRIND==bn, ANRIND==an)
      if (nrow(rec)==0) row_vals <- c(row_vals, "")
      else row_vals <- c(row_vals, paste0(fmt_comma(rec$COUNT[1]), " ", fmt_percpar(rec$PERCENT[1])))
    }
    target_lines <- c(target_lines,
      paste0(grp, " | ", ifelse(bn=="", "Missing", bn), " | ", paste(row_vals, collapse=" | ")))
  }
}
target_lines <- c(target_lines,
  "",
  "Treatment Emergent High",
  sprintf("%-8s %-8s %-8s %-8s %-12s", "TRTPN","Nx","n","%","FishersP"),
  paste(rep("-", 45), collapse="")
)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  target_lines <- c(target_lines,
    sprintf("%-8s %-8s %-8s %-8s %-12s",
            as.character(r$TRTPN),
            fmt_comma(r$Nx),
            fmt_comma(r$n),
            sprintf("%.1f", sas_round(r$percent,1)),
            fmt_pvalue(r$FishersP)))
}
target_lines <- c(target_lines,
  "",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", sprintf("%.2f", anrlo_val), " mmol/L ULNx ", sprintf("%.2f", anrhi_val), " mmol/L) set 2 (Female, Age 18-65, LLN=", sprintf("%.2f", anrlo_val), " mmol/L ULN=", sprintf("%.2f", anrhi_val), " mmol/L).")
)
writeLines(target_lines, file.path(out, "Target16.txt"))

# Target16.csv - emergent high + baseline shift summary
target_csv <- EmergentHigh %>% mutate(FishersP_fmt = fmt_pvalue(FishersP))
write.csv(target_csv, file.path(out, "Target16.csv"), row.names=FALSE, na="")

# also ensure sasgraph.csv already written
# done
cat("P053 translation complete. Outputs in", out, "\n")
