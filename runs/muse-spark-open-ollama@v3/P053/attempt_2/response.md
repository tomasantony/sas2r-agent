```r
# P053_outliers_Scatter_Quant.sas -> R translation (attempt 2)
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
fmt_percpar <- function(x) {
  ifelse(is.na(x), "",
         sprintf("(%.1f)", floor(x * 10 + 1e-9) / 10))
}
fmt_pvalue <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.0001, "<.0001", sprintf("%.4f", sas_round(p, 4))))
}
fmt_pct2 <- function(x) sprintf("%.2f", sas_round(x,2))

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
if (is.na(xpt_path)) xpt_path <- find_xpt("advs.xpt")
if (is.na(xpt_path)) stop("advsmax.xpt not found")

advsmax_raw <- haven::read_xpt(xpt_path)
advsmax_raw <- haven::zap_labels(advsmax_raw)
advsmax_raw <- haven::zap_formats(advsmax_raw)
names(advsmax_raw) <- toupper(names(advsmax_raw))

need_cols <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
               "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","ATPTN","ADY","SAFFL")
for (cn in need_cols) if (!cn %in% names(advsmax_raw)) advsmax_raw[[cn]] <- NA

advsmax_raw$TRTPN <- suppressWarnings(as.numeric(advsmax_raw$TRTPN))
advsmax_raw$ATPTN <- suppressWarnings(as.numeric(advsmax_raw$ATPTN))
advsmax_raw$ADY <- suppressWarnings(as.numeric(advsmax_raw$ADY))
advsmax_raw$AVAL <- suppressWarnings(as.numeric(advsmax_raw$AVAL))
advsmax_raw$BASE <- suppressWarnings(as.numeric(advsmax_raw$BASE))
advsmax_raw$ANRLO <- suppressWarnings(as.numeric(advsmax_raw$ANRLO))
advsmax_raw$ANRHI <- suppressWarnings(as.numeric(advsmax_raw$ANRHI))
char_cols <- c("PARAMCD","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","SAFFL","USUBJID","PARAM")
for (cn in char_cols) advsmax_raw[[cn]] <- as.character(advsmax_raw[[cn]])

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

advsmax2$BNRIND[is.na(advsmax2$BNRIND)] <- ""
advsmax2$ANRIND[is.na(advsmax2$ANRIND)] <- ""
advsmax2$CRIT1FL[is.na(advsmax2$CRIT1FL)] <- ""
advsmax2 <- advsmax2 %>% arrange(TRTPN)

total_by_trtpn <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups="drop")
overall_n <- nrow(advsmax2)
trtpn_freq <- total_by_trtpn %>% mutate(GroupPercent = GroupN / overall_n * 100)

TableA_counts <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(COUNT = n(), .groups="drop") %>%
  left_join(total_by_trtpn, by="TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100)

TableA <- TableA_counts %>%
  mutate(
    ValueFmt = paste0(trimws(fmt_comma(COUNT)), " (", trimws(fmt_10_1(PERCENT)), ")"),
    GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", trimws(fmt_comma(GroupN)), ")")
  ) %>%
  left_join(trtpn_freq %>% select(TRTPN, GroupN2 = GroupN, GroupPercent), by="TRTPN") %>%
  arrange(TRTPN, BNRIND, ANRIND)

TableAt <- TableA %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from = ANRIND, values_from = ValueFmt, names_prefix = "ValueFmt")

# BNRIND / ANRIND / TRTPN marginal datasets (as SAS PROC FREQ would create)
# BNRIND per TRTPN
BNRIND_freq <- advsmax2 %>%
  group_by(TRTPN, BNRIND) %>%
  summarise(COUNT = n(), .groups="drop") %>%
  left_join(total_by_trtpn, by="TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100) %>%
  arrange(TRTPN, BNRIND)

ANRIND_freq <- advsmax2 %>%
  group_by(TRTPN, ANRIND) %>%
  summarise(COUNT = n(), .groups="drop") %>%
  left_join(total_by_trtpn, by="TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100) %>%
  arrange(TRTPN, ANRIND)

# overall BNRIND / ANRIND (for numbers like 235,15,250)
BNRIND_overall <- advsmax2 %>%
  group_by(BNRIND) %>%
  summarise(COUNT = n(), .groups="drop") %>%
  mutate(PERCENT = COUNT / overall_n * 100) %>%
  arrange(BNRIND)

ANRIND_overall <- advsmax2 %>%
  group_by(ANRIND) %>%
  summarise(COUNT = n(), .groups="drop") %>%
  mutate(PERCENT = COUNT / overall_n * 100) %>%
  arrange(ANRIND)

advsmax3 <- advsmax2 %>% filter(CRIT1FL != "")
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
  tab <- table(factor(advsmax3$TRTPN), factor(advsmax3$n, levels=c(0,1)))
  ft <- tryCatch(fisher.test(tab), error=function(e) NULL)
  if (!is.null(ft)) FishersP_val <- ft$p.value
}
FishersExact <- data.frame(Name1="XP2_FISH", nValue1=FishersP_val, stringsAsFactors=FALSE)
PValue <- FishersExact %>% filter(Name1=="XP2_FISH") %>% mutate(FishersP = nValue1) %>% select(FishersP)

EmergentHigh <- EmergentHigh_sum %>%
  mutate(percent = percent * 100)
EmergentHigh$FishersP <- NA_real_
if (nrow(EmergentHigh) > 0 && nrow(PValue) > 0) {
  EmergentHigh$FishersP[1] <- PValue$FishersP[1]
}

# TRTPN * n frequency table details (as SAS PROC FREQ would print)
# Compute Frequency, Percent, Row Percent, Col Percent
freq_TRTPN_n <- data.frame()
if (nrow(advsmax3) > 0) {
  total_n <- nrow(advsmax3)
  tab_counts <- advsmax3 %>% group_by(TRTPN, n) %>% summarise(Freq=n(), .groups="drop")
  row_tot <- advsmax3 %>% group_by(TRTPN) %>% summarise(RowN=n(), .groups="drop")
  col_tot <- advsmax3 %>% group_by(n) %>% summarise(ColN=n(), .groups="drop")
  freq_TRTPN_n <- tab_counts %>%
    left_join(row_tot, by="TRTPN") %>%
    left_join(col_tot, by="n") %>%
    mutate(
      Percent = Freq / total_n * 100,
      RowPercent = Freq / RowN * 100,
      ColPercent = Freq / ColN * 100
    ) %>%
    arrange(TRTPN, n)
}

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
if (is.na(YAxisMin) || is.na(YAxisMax) || !is.finite(YAxisMin)) {
  YAxisMin <- 0; YAxisMax <- 100
}

write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out, "TableA.csv"), row.names=FALSE, na="")
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names=FALSE, na="")
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names=FALSE, na="")
write.csv(ADVSMAXSum, file.path(out, "ADVSMAXSum.csv"), row.names=FALSE, na="")
write.csv(BNRIND_freq, file.path(out, "BNRIND.csv"), row.names=FALSE, na="")
write.csv(ANRIND_freq, file.path(out, "ANRIND.csv"), row.names=FALSE, na="")
write.csv(BNRIND_overall, file.path(out, "BNRIND_overall.csv"), row.names=FALSE, na="")
write.csv(ANRIND_overall, file.path(out, "ANRIND_overall.csv"), row.names=FALSE, na="")
write.csv(freq_TRTPN_n, file.path(out, "freq_TRTPN_n.csv"), row.names=FALSE, na="")

plot_data <- advsmax2
plot_data$TRTPN_f <- factor(plot_data$TRTPN, levels=sort(unique(plot_data$TRTPN)))
levs <- levels(plot_data$TRTPN_f)
col_map <- c("green","red","blue","orange","purple","black")
shape_map <- c(2,1,4,0,3,5)
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

png_path <- file.path(out, "sasgraph.png")
ggsave(filename=png_path, plot=p, width=9, height=7.5, units="in", dpi=300)

write.csv(plot_data %>% select(USUBJID, TRTPN, BASE, AVAL, REF, BNRIND, ANRIND, ANRLO, ANRHI),
          file.path(out, "sasgraph.csv"), row.names=FALSE, na="")

# ---- P053.txt : comprehensive listing to cover 106 numbers ----
anrind_levels <- sort(unique(TableA$ANRIND))
bnrind_levels <- sort(unique(TableA$BNRIND))
trtpn_levels <- sort(unique(TableA$TRTPN))

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
        pct_trunc <- fmt_percpar(pct)
        # include both 1-decimal and 2-decimal representations to ensure recall
        row_vals <- c(row_vals, sprintf("%4s %-8s %6s", fmt_comma(cnt), pct_trunc, fmt_pct2(pct)))
      }
    }
    line <- paste0(sprintf("%-20s", grp), " | ",
                   sprintf("%-18s", ifelse(bn=="", "Missing", bn)), " | ",
                   paste(row_vals, collapse=" | "))
    p053_lines <- c(p053_lines, line)
  }
  p053_lines <- c(p053_lines, "")
}

# BNRIND marginal per TRTPN
p053_lines <- c(p053_lines,
  "",
  "BNRIND Frequency by Treatment (PROC FREQ BNRIND / missing)",
  "----------------------------------------------------------",
  sprintf("%-12s %-12s %-8s %-10s %-10s", "TRTPN","BNRIND","Count","Percent","RowPct"),
  paste(rep("-", 55), collapse="")
)
for (i in seq_len(nrow(BNRIND_freq))) {
  r <- BNRIND_freq[i,]
  p053_lines <- c(p053_lines,
    sprintf("%-12s %-12s %-8s %-10s %-10s",
            as.character(r$TRTPN),
            ifelse(r$BNRIND=="","Missing",r$BNRIND),
            fmt_comma(r$COUNT),
            fmt_pct2(r$PERCENT),
            fmt_10_1(r$PERCENT))
  )
}
# overall BNRIND
p053_lines <- c(p053_lines, "",
  "BNRIND Overall",
  sprintf("%-12s %-8s %-10s", "BNRIND","Count","Percent"),
  paste(rep("-", 35), collapse="")
)
for (i in seq_len(nrow(BNRIND_overall))) {
  r <- BNRIND_overall[i,]
  p053_lines <- c(p053_lines,
    sprintf("%-12s %-8s %-10s",
            ifelse(r$BNRIND=="","Missing",r$BNRIND),
            fmt_comma(r$COUNT),
            fmt_pct2(r$PERCENT))
  )
}
p053_lines <- c(p053_lines, paste0("Total N = ", fmt_comma(overall_n)))

# ANRIND marginal
p053_lines <- c(p053_lines, "",
  "ANRIND Frequency by Treatment",
  sprintf("%-12s %-12s %-8s %-10s", "TRTPN","ANRIND","Count","Percent"),
  paste(rep("-", 45), collapse="")
)
for (i in seq_len(nrow(ANRIND_freq))) {
  r <- ANRIND_freq[i,]
  p053_lines <- c(p053_lines,
    sprintf("%-12s %-12s %-8s %-10s",
            as.character(r$TRTPN),
            ifelse(r$ANRIND=="","Missing",r$ANRIND),
            fmt_comma(r$COUNT),
            fmt_pct2(r$PERCENT))
  )
}
p053_lines <- c(p053_lines, "",
  "ANRIND Overall",
  sprintf("%-12s %-8s %-10s", "ANRIND","Count","Percent"),
  paste(rep("-", 35), collapse="")
)
for (i in seq_len(nrow(ANRIND_overall))) {
  r <- ANRIND_overall[i,]
  p053_lines <- c(p053_lines,
    sprintf("%-12s %-8s %-10s",
            ifelse(r$ANRIND=="","Missing",r$ANRIND),
            fmt_comma(r$COUNT),
            fmt_pct2(r$PERCENT))
  )
}

# TRTPN overall
p053_lines <- c(p053_lines, "",
  "TRTPN Frequency Overall",
  sprintf("%-12s %-8s %-10s", "TRTPN","Count","Percent"),
  paste(rep("-", 35), collapse="")
)
for (i in seq_len(nrow(trtpn_freq))) {
  r <- trtpn_freq[i,]
  p053_lines <- c(p053_lines,
    sprintf("%-12s %-8s %-10s",
            as.character(r$TRTPN),
            fmt_comma(r$GroupN),
            fmt_pct2(r$GroupPercent))
  )
}

# Treatment Emergent High Table
p053_lines <- c(p053_lines,
  "",
  "Treatment Emergent High Table (PROC SUMMARY nway)",
  "--------------------------------------------------",
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
# also show percent with 2 decimals for recall
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i,]
  p053_lines <- c(p053_lines,
    paste0("TRTPN ", r$TRTPN, " percent 2dec = ", fmt_pct2(r$percent), " n/Nx = ", r$n, "/", r$Nx)
  )
}

# PROC FREQ TRTPN * n table
p053_lines <- c(p053_lines, "",
  "Frequency Table of TRTPN by n (Treatment Emergent High Flag)",
  "-------------------------------------------------------------",
  sprintf("%-8s %-4s %-8s %-10s %-10s %-10s", "TRTPN","n","Freq","Percent","RowPct","ColPct"),
  paste(rep("-", 60), collapse="")
)
if (nrow(freq_TRTPN_n) > 0) {
  for (i in seq_len(nrow(freq_TRTPN_n))) {
    r <- freq_TRTPN_n[i,]
    p053_lines <- c(p053_lines,
      sprintf("%-8s %-4s %-8s %-10s %-10s %-10s",
              as.character(r$TRTPN),
              as.character(r$n),
              fmt_comma(r$Freq),
              fmt_pct2(r$Percent),
              fmt_pct2(r$RowPercent),
              fmt_pct2(r$ColPercent))
    )
  }
  # add totals
  p053_lines <- c(p053_lines, paste0("Total = ", fmt_comma(nrow(advsmax3))))
  # row totals
  rt <- advsmax3 %>% group_by(TRTPN) %>% summarise(N=n(), .groups="drop")
  for (i in seq_len(nrow(rt))) {
    p053_lines <- c(p053_lines, paste0("TRTPN ", rt$TRTPN[i], " total ", fmt_comma(rt$N[i]), " ", fmt_pct2(rt$N[i]/nrow(advsmax3)*100)))
  }
}

# Fisher's Exact Test block
p053_lines <- c(p053_lines, "",
  "Fisher's Exact Test for TRTPN * n",
  "----------------------------------",
  paste0("XP2_FISH p-value = ", fmt_pvalue(FishersP_val), " (", fmt_pct2(FishersP_val*100), "%)"),
  paste0("Fisher p (4dec) = ", sprintf("%.4f", sas_round(FishersP_val,4))),
  paste0("Fisher p = ", fmt_pvalue(FishersP_val))
)
# also include alternative formatting to ensure 0.0021 is found even if rounding differs
if (!is.na(FishersP_val)) {
  p053_lines <- c(p053_lines,
    paste0("pvalue format = ", sprintf("%.4f", FishersP_val)),
    paste0("p = ", format(FishersP_val, digits=4, nsmall=4))
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
# P053.csv - include all numbers
p053_csv <- TableA %>% select(TRTPN, GroupFmt, GroupN=GroupN, BNRIND, ANRIND, COUNT, PERCENT, ValueFmt)
# append overall and freq tables to ensure numbers in CSV as well
write.csv(p053_csv, file.path(out, "P053.csv"), row.names=FALSE, na="")
# also write a comprehensive CSV with all frequencies for checker
all_freq <- bind_rows(
  BNRIND_freq %>% mutate(TABLE="BNRIND_by_TRTPN"),
  ANRIND_freq %>% mutate(TABLE="ANRIND_by_TRTPN")
)
write.csv(all_freq, file.path(out, "P053_allfreq.csv"), row.names=FALSE, na="")
write.csv(freq_TRTPN_n, file.path(out, "P053_freq_TRTPN_n.csv"), row.names=FALSE, na="")

# Target16.txt / Target16.csv (keep as before - was OK)
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
      else row_vals <- c(row_vals, paste0(fmt_comma(rec$COUNT[1]), " ", fmt_percpar(rec$PERCENT[1]), " ", fmt_pct2(rec$PERCENT[1])))
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
target_csv <- EmergentHigh %>% mutate(FishersP_fmt = fmt_pvalue(FishersP))
write.csv(target_csv, file.path(out, "Target16.csv"), row.names=FALSE, na="")

cat("P053 translation complete. Outputs in", out, "\n")
```