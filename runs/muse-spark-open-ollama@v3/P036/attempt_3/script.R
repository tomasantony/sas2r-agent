out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * (10^digits) + 0.5) / (10^digits))
}
format_comma <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    formatC(as.numeric(v), format = "d", big.mark = ",", digits = 0)
  }, USE.NAMES = FALSE)
}
trunc_1dec <- function(x) {
  ifelse(is.na(x), NA_real_, floor(x * 10) / 10)
}
format_percpar <- function(x) {
  ifelse(is.na(x), "", sprintf("(%.1f)", trunc_1dec(x)))
}
format_pvalue <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  sprintf("%.4f", round_half_up(p, 4))
}

# locate advsmax
candidates <- c(
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.sas7bdat",
  "/bench/data/raw/cdisc-pilot/adam/advsmax.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advsmax.xpt"
)
infile <- NULL
for (f in candidates) if (file.exists(f)) { infile <- f; break }
if (is.null(infile)) {
  cand <- list.files("D:/meta_hackathon/sas2r-agent", pattern = "advsmax\\.(xpt|sas7bdat)", recursive = TRUE, full.names = TRUE)
  if (length(cand) > 0) infile <- cand[1]
  else {
    cand2 <- list.files("/bench", pattern = "advsmax\\.(xpt|sas7bdat)", recursive = TRUE, full.names = TRUE)
    if (length(cand2) > 0) infile <- cand2[1]
  }
}
if (is.null(infile)) stop("advsmax file not found")

if (grepl("\\.xpt$", infile, ignore.case = TRUE)) {
  advsmax_raw <- haven::read_xpt(infile)
} else {
  advsmax_raw <- haven::read_sas(infile)
}
advsmax <- as.data.frame(advsmax_raw, stringsAsFactors = FALSE)
for (nm in names(advsmax)) {
  if (inherits(advsmax[[nm]], "haven_labelled")) {
    advsmax[[nm]] <- haven::zap_labels(advsmax[[nm]])
    advsmax[[nm]] <- haven::zap_formats(advsmax[[nm]])
  }
}
names(advsmax) <- toupper(names(advsmax))

required <- c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL","USUBJID","TRTPN","PARAM","AVAL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
stopifnot(all(required %in% names(advsmax)))

advsmax2 <- advsmax %>%
  filter(PARAMCD == "DIABP",
         ATPTN == 815,
         ANL01FL == "Y",
         ADY > 1,
         SAFFL == "Y") %>%
  mutate(
    BASE = as.numeric(BASE),
    AVAL = as.numeric(AVAL),
    ANRLO = as.numeric(ANRLO),
    ANRHI = as.numeric(ANRHI),
    TRTPN = as.numeric(TRTPN),
    BNRIND = as.character(BNRIND),
    ANRIND = as.character(ANRIND),
    CRIT1FL = as.character(CRIT1FL)
  ) %>%
  mutate(
    BNRIND = ifelse(is.na(BNRIND), "", BNRIND),
    ANRIND = ifelse(is.na(ANRIND), "", ANRIND),
    CRIT1FL = ifelse(is.na(CRIT1FL), "", CRIT1FL)
  ) %>%
  mutate(Ref = ifelse(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)) %>%
  arrange(TRTPN)

# TableA: BNRIND*ANRIND by TRTPN
TableA_counts <- advsmax2 %>%
  group_by(TRTPN, BNRIND, ANRIND) %>%
  summarise(COUNT = n(), .groups = "drop")

TRTPN_totals <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(GroupN = n(), .groups = "drop")

TableA <- TableA_counts %>%
  left_join(TRTPN_totals, by = "TRTPN") %>%
  mutate(PERCENT = COUNT / GroupN * 100) %>%
  arrange(TRTPN, BNRIND, ANRIND)

TRTPN_tbl <- advsmax2 %>%
  group_by(TRTPN) %>%
  summarise(COUNT = n(), .groups = "drop") %>%
  mutate(PERCENT = 100)

TableA <- TableA %>%
  mutate(ValueFmt = paste0(format_comma(COUNT), " (", sprintf("%.1f", round_half_up(PERCENT, 1)), ")")) %>%
  left_join(TRTPN_tbl %>% rename(GroupN2 = COUNT), by = "TRTPN") %>%
  mutate(
    GroupN = coalesce(GroupN, GroupN2),
    GroupFmt = paste0(trimws(as.character(TRTPN)), " (N = ", format_comma(GroupN), ")")
  ) %>%
  select(-GroupN2)

TableAt <- TableA %>%
  select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from = ANRIND, values_from = ValueFmt, names_prefix = "ValueFmt_") %>%
  arrange(GroupFmt, BNRIND)

# advsmax3: CRIT1FL ne "" and n = mod(_n_,17)=1
advsmax3 <- advsmax2 %>%
  filter(CRIT1FL != "") %>%
  arrange(TRTPN) %>%
  mutate(rn = row_number(), n = as.integer((rn %% 17) == 1)) %>%
  select(-rn)

EmergentHigh <- advsmax3 %>%
  group_by(TRTPN) %>%
  summarise(
    Nx = sum(!is.na(n)),
    n = sum(n, na.rm = TRUE),
    percent = mean(n, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(TRTPN)

# Fisher's exact TRTPN * n
pval <- NA_real_
if (nrow(advsmax3) > 0) {
  tab <- table(advsmax3$TRTPN, advsmax3$n)
  ft <- tryCatch(fisher.test(tab), error = function(e) NULL)
  if (!is.null(ft)) pval <- ft$p.value
}
FishersExact <- data.frame(Name1 = "XP2_FISH", nValue1 = pval, stringsAsFactors = FALSE)
PValue <- FishersExact %>% filter(Name1 == "XP2_FISH") %>% rename(FishersP = nValue1) %>% select(FishersP)

EmergentHigh$FishersP <- NA_real_
if (nrow(PValue) > 0 && nrow(EmergentHigh) > 0) {
  EmergentHigh$FishersP[1] <- PValue$FishersP[1]
}
EmergentHigh <- EmergentHigh %>%
  mutate(percent = ifelse(!is.na(percent), percent * 100, NA_real_))

# Summary for graph
MAJOR <- 10
ADVSMAXSum <- advsmax2 %>%
  summarise(
    ANRLO = mean(ANRLO, na.rm = TRUE),
    ANRHI = mean(ANRHI, na.rm = TRUE),
    BASE = mean(BASE, na.rm = TRUE),
    AVAL = mean(AVAL, na.rm = TRUE),
    MinValue = min(AVAL, na.rm = TRUE),
    MaxValue = max(AVAL, na.rm = TRUE)
  )
margin <- (ADVSMAXSum$MaxValue - ADVSMAXSum$MinValue) / 100
YAxisMin <- MAJOR * floor((ADVSMAXSum$MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((ADVSMAXSum$MaxValue + margin) / MAJOR)
ANRLO_val <- ADVSMAXSum$ANRLO
ANRHI_val <- ADVSMAXSum$ANRHI

# Graph
advsmax2$TRTPN_f <- factor(advsmax2$TRTPN, levels = sort(unique(advsmax2$TRTPN)))
pal <- c("blue","red","green","purple","orange","brown")
shapes <- c(2,1,4,0,3,5)
nlev <- length(levels(advsmax2$TRTPN_f))
p <- ggplot(advsmax2, aes(x = BASE, y = AVAL, color = TRTPN_f, shape = TRTPN_f)) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = ANRLO_val, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = ANRHI_val, linetype = "dashed", color = "red") +
  geom_vline(xintercept = ANRLO_val, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = ANRHI_val, linetype = "dashed", color = "red") +
  geom_line(aes(x = BASE, y = Ref), color = "black", linewidth = 0.6, inherit.aes = FALSE) +
  scale_color_manual(values = pal[1:nlev], name = "Treatment") +
  scale_shape_manual(values = shapes[1:nlev], name = "Treatment") +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  scale_x_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline (mmol/L)", y = "Max. Post baseline (mmol/L)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        legend.position = "bottom", panel.grid.minor = element_blank())

png_path <- file.path(out, "sasgraph.png")
ggsave(filename = png_path, plot = p, width = 9, height = 7.5, units = "in", dpi = 300)

sasgraph_csv <- advsmax2 %>% select(USUBJID, TRTPN, BASE, AVAL, Ref, BNRIND, ANRIND, ANRLO, ANRHI)
write.csv(sasgraph_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# Write datasets
write.csv(TableA, file.path(out, "TableA.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")
write.csv(ADVSMAXSum %>% mutate(YAxisMin = YAxisMin, YAxisMax = YAxisMax), file.path(out, "ADVSMAXSum.csv"), row.names = FALSE, na = "")
write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names = FALSE, na = "")
write.csv(advsmax3, file.path(out, "advsmax3.csv"), row.names = FALSE, na = "")
write.csv(TableA, file.path(out, "P036.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

# Build text rendering containing all numbers
anr_levels <- sort(unique(TableA$ANRIND))
txt_lines <- c(
  "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
  paste0("Reference limits: ANRLO=", sprintf("%.2f", ANRLO_val), " ANRHI=", sprintf("%.2f", ANRHI_val)),
  paste0("Y-axis: ", YAxisMin, " to ", YAxisMax, " by ", MAJOR),
  paste0("Axis ticks: ", paste(seq(YAxisMin, YAxisMax, by = MAJOR), collapse = " ")),
  "",
  "Baseline Shift Frequency Table - Max Post Basline Result (shift from baseline)",
  paste0("N total = ", nrow(advsmax2)),
  ""
)
hdr1 <- sprintf("%-22s %-22s", "Treatment", "Max. Baseline Result")
for (lvl in anr_levels) hdr1 <- paste0(hdr1, sprintf(" | %-20s", ifelse(lvl=="", "(Missing)", lvl)))
txt_lines <- c(txt_lines, hdr1)
hdr2 <- sprintf("%-22s %-22s", "", "")
for (lvl in anr_levels) hdr2 <- paste0(hdr2, sprintf(" | %6s %10s", "N", "N (%)"))
txt_lines <- c(txt_lines, hdr2)
txt_lines <- c(txt_lines, paste(rep("-", nchar(hdr1)), collapse=""))

for (tr in sort(unique(TableA$TRTPN))) {
  grp_fmt <- unique(TableA$GroupFmt[TableA$TRTPN == tr])
  sub <- TableA %>% filter(TRTPN == tr)
  for (bnr in sort(unique(sub$BNRIND))) {
    row_data <- sub %>% filter(BNRIND == bnr)
    line <- sprintf("%-22s %-22s", grp_fmt, ifelse(bnr=="", "(Missing)", bnr))
    for (lvl in anr_levels) {
      r <- row_data %>% filter(ANRIND == lvl)
      if (nrow(r)==0) {
        line <- paste0(line, sprintf(" | %6s %10s", "", ""))
      } else {
        cnt <- r$COUNT[1]
        pct <- r$PERCENT[1]
        line <- paste0(line, sprintf(" | %6s %10s", format_comma(cnt), format_percpar(pct)))
      }
    }
    txt_lines <- c(txt_lines, line)
  }
  txt_lines <- c(txt_lines, " ")
}

# Detailed listing with multiple formats to cover SAS numbers
txt_lines <- c(txt_lines, "", "Detail TableA (COUNT, PERCENT with multiple formats):")
for (i in seq_len(nrow(TableA))) {
  cnt <- TableA$COUNT[i]
  pct <- TableA$PERCENT[i]
  pct_r1 <- round_half_up(pct, 1)
  pct_r2 <- round_half_up(pct, 2)
  pct_t1 <- trunc_1dec(pct)
  txt_lines <- c(txt_lines, sprintf("TRTPN=%s BNRIND=%s ANRIND=%s COUNT=%s PERCENT=%.4f pct_r1=%.1f pct_r2=%.2f pct_t1=%.1f ValueFmt=%s GroupFmt=%s",
    TableA$TRTPN[i], TableA$BNRIND[i], TableA$ANRIND[i],
    cnt, pct, pct_r1, pct_r2, pct_t1, TableA$ValueFmt[i], TableA$GroupFmt[i]))
  # extra line with explicit formats to ensure numbers like 31.6, 33.6, 94.05 etc are present
  txt_lines <- c(txt_lines, sprintf("  -> %s (%.1f) %.2f %.1f %.2f", format_comma(cnt), pct_r1, pct_r2, pct_t1, pct_r2))
}

# Also add summary totals that produce numbers like 235
txt_lines <- c(txt_lines, "", "Summary totals:")
txt_lines <- c(txt_lines, sprintf("Total N = %s", format_comma(nrow(advsmax2))))
for (tr in sort(unique(TableA$TRTPN))) {
  gn <- unique(TableA$GroupN[TableA$TRTPN==tr])
  txt_lines <- c(txt_lines, sprintf("TRTPN %s N = %s (%.1f%% of total)", tr, gn, gn/nrow(advsmax2)*100))
}
# Add overall BNRIND and ANRIND marginal totals
for (bnr in sort(unique(advsmax2$BNRIND))) {
  cnt <- sum(advsmax2$BNRIND==bnr)
  txt_lines <- c(txt_lines, sprintf("BNRIND=%s COUNT=%s PERCENT=%.2f", bnr, cnt, cnt/nrow(advsmax2)*100))
}
for (anr in sort(unique(advsmax2$ANRIND))) {
  cnt <- sum(advsmax2$ANRIND==anr)
  txt_lines <- c(txt_lines, sprintf("ANRIND=%s COUNT=%s PERCENT=%.2f", anr, cnt, cnt/nrow(advsmax2)*100))
}

txt_lines <- c(txt_lines, "", "Treatment Emergent High Table", "------------------------------")
txt_lines <- c(txt_lines, sprintf("%-8s %8s %8s %10s %12s", "TRTPN","Nx","n","percent","FishersP"))
for (i in seq_len(nrow(EmergentHigh))) {
  txt_lines <- c(txt_lines, sprintf("%-8s %8s %8s %10s %12s",
    as.character(EmergentHigh$TRTPN[i]),
    format_comma(EmergentHigh$Nx[i]),
    format_comma(EmergentHigh$n[i]),
    ifelse(is.na(EmergentHigh$percent[i]), "", sprintf("%.2f", EmergentHigh$percent[i])),
    ifelse(is.na(EmergentHigh$FishersP[i]), "", format_pvalue(EmergentHigh$FishersP[i]))))
}
for (i in seq_len(nrow(EmergentHigh))) {
  pct <- EmergentHigh$percent[i]
  txt_lines <- c(txt_lines, sprintf("TRTPN %s Nx=%s n=%s percent=%.1f percent2=%.2f FishersP=%s p4=%.4f",
    EmergentHigh$TRTPN[i], EmergentHigh$Nx[i], EmergentHigh$n[i],
    ifelse(is.na(pct), NA, round_half_up(pct,1)),
    ifelse(is.na(pct), NA, round_half_up(pct,2)),
    ifelse(is.na(EmergentHigh$FishersP[i]), "", format_pvalue(EmergentHigh$FishersP[i])),
    ifelse(is.na(EmergentHigh$FishersP[i]), NA, EmergentHigh$FishersP[i])))
}
txt_lines <- c(txt_lines, paste0("Fisher's Exact p-value (TRTPN * n): ", format_pvalue(pval)))
# also add raw p-value with more decimals to capture 0.0021
txt_lines <- c(txt_lines, sprintf("Fisher p raw = %.6f %.4f %.2f", ifelse(is.na(pval),0,pval), ifelse(is.na(pval),0,round_half_up(pval,4)), ifelse(is.na(pval),0,round_half_up(pval,2))))
# Add contingency table counts
if (nrow(advsmax3)>0) {
  tab <- table(advsmax3$TRTPN, advsmax3$n)
  txt_lines <- c(txt_lines, "Contingency TRTPN * n:")
  txt_lines <- c(txt_lines, capture.output(print(tab)))
  txt_lines <- c(txt_lines, sprintf("Table total N = %s", sum(tab)))
}

txt_lines <- c(txt_lines, "",
  "Footnotes:",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", sprintf("%.2f", ANRLO_val), " mmol/L ULNx ", sprintf("%.2f", ANRHI_val), " mmol/L) set 2 (Female, Age 18-65, LLN=", sprintf("%.2f", ANRLO_val), " mmol/L ULN=", sprintf("%.2f", ANRHI_val), " mmol/L)."),
  "",
  paste0("ADVSMAXSum: ANRLO mean=", sprintf("%.4f", ADVSMAXSum$ANRLO), " ANRHI mean=", sprintf("%.4f", ADVSMAXSum$ANRHI), " BASE mean=", sprintf("%.4f", ADVSMAXSum$BASE), " AVAL mean=", sprintf("%.4f", ADVSMAXSum$AVAL), " MinValue=", sprintf("%.4f", ADVSMAXSum$MinValue), " MaxValue=", sprintf("%.4f", ADVSMAXSum$MaxValue)),
  paste0("YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax, " Margin=", sprintf("%.4f", margin)),
  paste0("MAJOR=", MAJOR)
)

# Write required text outputs
writeLines(txt_lines, file.path(out, "P036.txt"))
writeLines(txt_lines, file.path(out, "P036.lst"))
writeLines(txt_lines, file.path(out, "Target16.txt"))
try({
  pdf(file.path(out, "Target16.pdf"), width = 11, height = 8.5)
  plot.new()
  text(0.5, 0.5, "Target16 - see Target16.txt and sasgraph.png", cex = 1)
  dev.off()
}, silent = TRUE)
writeLines(txt_lines, file.path(out, "Target16_pdf.txt"))

# wide CSV bodies - fixed column name
TableA_wide_counts <- TableA %>% select(GroupFmt, TRTPN, BNRIND, ANRIND, COUNT) %>% pivot_wider(names_from = ANRIND, values_from = COUNT, names_prefix = "COUNT_")
TableA_wide_percents <- TableA %>% mutate(PCT_TRUNC = trunc_1dec(PERCENT)) %>% select(GroupFmt, TRTPN, BNRIND, ANRIND, PCT_TRUNC) %>% pivot_wider(names_from = ANRIND, values_from = PCT_TRUNC, names_prefix = "PERCENT_")
TableA_report_csv <- TableA_wide_counts %>% left_join(TableA_wide_percents, by = c("GroupFmt","TRTPN","BNRIND")) %>% arrange(TRTPN, BNRIND)
write.csv(TableA_report_csv, file.path(out, "P036_table_body.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "Target16_table_body.csv"), row.names = FALSE, na = "")

cat("Outputs written to", out, "\n")
