# P036_target16.sas -> R translation
# Outputs: P036.txt/csv (baseline shift), Target16.txt/csv (emergent high), sasgraph.png/csv
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
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

cat("P036 start\n")

# ---- helpers: SAS semantics ----
round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-12) / 10^digits)
}
fmt_comma <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    formatC(as.integer(v), format = "d", big.mark = ",")
  }, USE.NAMES = FALSE)
}
fmt_pct1 <- function(x) {
  sapply(x, function(v) {
    if (is.na(v)) return("")
    sprintf("%.1f", round_half_up(v, 1))
  }, USE.NAMES = FALSE)
}
fmt_pvalue <- function(p) {
  sapply(p, function(v) {
    if (is.na(v)) return("")
    if (v < 0.0001) return("<.0001")
    sprintf("%.4f", round_half_up(v, 4))
  }, USE.NAMES = FALSE)
}
# SAS character missing is blank; haven may give NA -> convert to "" for BNRIND etc
char_miss_to_blank <- function(x) {
  ifelse(is.na(x), "", as.character(x))
}

# ---- locate input ----
rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
cand <- c(file.path(rawadam, "advsmax.xpt"),
          "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt",
          "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.sas7bdat")
advs_path <- cand[file.exists(cand)][1]
if (is.na(advs_path)) advs_path <- file.path(rawadam, "advsmax.xpt")
cat(paste0("reading ", advs_path, "\n"))
if (grepl("\\.xpt$", advs_path, ignore.case = TRUE)) {
  advs <- haven::read_xpt(advs_path)
} else {
  advs <- haven::read_sas(advs_path)
}
advs <- haven::zap_labels(advs)
advs <- haven::zap_formats(advs)
advs <- as.data.frame(advs, stringsAsFactors = FALSE)

# Ensure expected columns exist (uppercase)
# haven preserves case; normalize lookup case-insensitive
nm <- names(advs)
upper_nm <- toupper(nm)
for (need in c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
               "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","ATPTN","ADY","SAFFL")) {
  idx <- which(upper_nm == need)
  if (length(idx) == 1 && nm[idx] != need) {
    names(advs)[idx] <- need
  }
}

# ---- data advsmax2 ----
# where ((PARAMCD="DIABP") and (ATPTN=815) and (ANL01FL="Y") and (ADY>1) and (SAFFL="Y"))
advsmax2 <- advs %>%
  filter(!is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         !is.na(SAFFL) & SAFFL == "Y")
# keep list
keepvars <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND",
              "SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
keepvars <- keepvars[keepvars %in% names(advs)]
advsmax2 <- advsmax2[, keepvars, drop = FALSE]
# Ref = Base if not nmiss(base, aval)
advsmax2$Ref <- ifelse(!is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL), advsmax2$BASE, NA_real_)
# normalize char miss to blank for grouping vars to mimic SAS blank missing
for (cc in c("BNRIND","ANRIND","CRIT1FL") ) {
  if (cc %in% names(advsmax2)) advsmax2[[cc]] <- char_miss_to_blank(advsmax2[[cc]])
}
# sort by TRTPN (SAS missing sorts first)
advsmax2 <- advsmax2[order(advsmax2$TRTPN, na.last = FALSE), , drop = FALSE]

# ---- PROC FREQ by TRTPN ----
# group totals
grpN <- advsmax2 %>% group_by(TRTPN) %>% summarise(GroupN = n(), .groups = "drop")
# distinct levels (including blank missing)
uTRT <- sort(unique(advsmax2$TRTPN))
uBNR <- sort(unique(advsmax2$BNRIND))
uANR <- sort(unique(advsmax2$ANRIND))
if (length(uTRT) == 0) uTRT <- numeric(0)

# observed counts for BNRIND*ANRIND within TRTPN
obsA <- advsmax2 %>% group_by(TRTPN, BNRIND, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")
# complete to all combos (SAS includes zero cells)
fullA <- expand.grid(TRTPN = uTRT, BNRIND = uBNR, ANRIND = uANR, stringsAsFactors = FALSE)
TableA <- fullA %>% left_join(obsA, by = c("TRTPN","BNRIND","ANRIND"))
TableA$COUNT[is.na(TableA$COUNT)] <- 0L
TableA <- TableA %>% left_join(grpN, by = "TRTPN")
TableA$PERCENT <- ifelse(TableA$GroupN > 0, 100 * TableA$COUNT / TableA$GroupN, NA_real_)
# sort SAS order: TRTPN missing first, then char ascending
TableA <- TableA[order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND, na.last = FALSE), , drop = FALSE]

# ValueFmt = count (percent) with comma8. and 10.1
TableA$ValueFmt <- paste0(fmt_comma(TableA$COUNT), " (", fmt_pct1(TableA$PERCENT), ")")

# TRTPN one-way dataset (BY TRTPN, TABLE TRTPN): one row per BY group
TRTPNds <- grpN %>% transmute(TRTPN = TRTPN, COUNT = GroupN, PERCENT = 100)
names(TRTPNds)[names(TRTPNds)=="COUNT"] <- "Count"
names(TRTPNds)[names(TRTPNds)=="PERCENT"] <- "Percent"
# merge to get GroupN / GroupPercent
TableA2 <- TableA %>% left_join(TRTPNds %>% transmute(TRTPN = TRTPN, GroupN2 = Count, GroupPercent = Percent),
                                by = "TRTPN")
# GroupFmt = TRTPN (N = GroupN)
TableA$GroupN <- TableA$GroupN
TableA$GroupPercent <- 100
TableA$GroupFmt <- paste0(ifelse(is.na(TableA$TRTPN), "", as.character(TableA$TRTPN)),
                          " (N = ", fmt_comma(TableA$GroupN), ")")

# one-way BNRIND / ANRIND (for completeness, SAS semantics)
obsB <- advsmax2 %>% group_by(TRTPN, BNRIND) %>% summarise(COUNT = n(), .groups = "drop")
fullB <- expand.grid(TRTPN = uTRT, BNRIND = uBNR, stringsAsFactors = FALSE)
BNRINDds <- fullB %>% left_join(obsB, by = c("TRTPN","BNRIND"))
BNRINDds$COUNT[is.na(BNRINDds$COUNT)] <- 0L
BNRINDds <- BNRINDds %>% left_join(grpN, by = "TRTPN")
BNRINDds$PERCENT <- 100 * BNRINDds$COUNT / BNRINDds$GroupN

obsC <- advsmax2 %>% group_by(TRTPN, ANRIND) %>% summarise(COUNT = n(), .groups = "drop")
fullC <- expand.grid(TRTPN = uTRT, ANRIND = uANR, stringsAsFactors = FALSE)
ARINDds <- fullC %>% left_join(obsC, by = c("TRTPN","ANRIND"))
ARINDds$COUNT[is.na(ARINDds$COUNT)] <- 0L
ARINDds <- ARINDds %>% left_join(grpN, by = "TRTPN")
ARINDds$PERCENT <- 100 * ARINDds$COUNT / ARINDds$GroupN

# transpose TableAt (unused later, create equivalent wide)
TableAt <- TableA %>% select(GroupFmt, BNRIND, ANRIND, ValueFmt) %>%
  pivot_wider(names_from = ANRIND, values_from = ValueFmt, names_prefix = "ValueFmt_")

# ---- treatment emergent high ----
# data advsmax3; set advsmax2; if CRIT1FL ne ""; n = mod(_n_,17)=1;
advsmax2$orig_row <- seq_len(nrow(advsmax2))
advsmax3 <- advsmax2 %>% filter(CRIT1FL != "")
# _N_ is orig_row (includes deleted rows numbering)
advsmax3$n <- as.integer(advsmax3$orig_row %% 17 == 1)

# proc summary nway class TRTPN var n -> Nx=n, sum, mean
EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>% summarise(
  Nx = sum(!is.na(n)),
  n = sum(n, na.rm = TRUE),
  percent = ifelse(sum(!is.na(n)) > 0, mean(n, na.rm = TRUE), NA_real_),
  .groups = "drop") %>% arrange(TRTPN)
# Fisher exact TRTPN*n
ct <- table(advsmax3$TRTPN, advsmax3$n)
fish_p <- NA_real_
if (nrow(ct) >= 2 && ncol(ct) >= 2 && sum(ct) > 0) {
  ft <- try(fisher.test(ct), silent = TRUE)
  if (!inherits(ft, "try-error")) fish_p <- ft$p.value
} else {
  fish_p <- NA_real_
}
FishersExact <- data.frame(Name1 = "XP2_FISH", nValue1 = fish_p, stringsAsFactors = FALSE)
PValue <- FishersExact %>% filter(Name1 == "XP2_FISH") %>% transmute(FishersP = nValue1)
# merge without BY: SAS one-to-one by row number -> only first row gets value
EmergentHigh <- EmergentHigh[order(EmergentHigh$TRTPN, na.last = FALSE), , drop = FALSE]
EmergentHigh$FishersP <- c(PValue$FishersP[1], rep(NA_real_, max(0, nrow(EmergentHigh) - 1)))
# percent *100
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), 100 * EmergentHigh$percent, NA_real_)

# ---- summary for axes ----
MAJOR <- 10
mean_ANRLO <- suppressWarnings(mean(advsmax2$ANRLO, na.rm = TRUE))
mean_ANRHI <- suppressWarnings(mean(advsmax2$ANRHI, na.rm = TRUE))
minValue_raw <- suppressWarnings(min(advsmax2$AVAL, na.rm = TRUE))
maxValue_raw <- suppressWarnings(max(advsmax2$AVAL, na.rm = TRUE))
ANRLO_v <- mean_ANRLO
ANRHI_v <- mean_ANRHI
margin <- (maxValue_raw - minValue_raw) / 100
YAxisMin <- MAJOR * floor((minValue_raw - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((maxValue_raw + margin) / MAJOR)
if (!is.finite(YAxisMin)) YAxisMin <- NA_real_
if (!is.finite(YAxisMax)) YAxisMax <- NA_real_

# ---- graph ----
plot_df <- advsmax2
plot_df$TRTPF <- factor(plot_df$TRTPN, levels = sort(unique(plot_df$TRTPN)))
# map colors/shapes in TRTPN order: 1st green triangle, 2nd red circle, 3rd blue X
lvls <- levels(plot_df$TRTPF)
nlev <- length(lvls)
pal_cols <- c("green","red","blue")[seq_len(max(1, nlev))]
pal_shapes <- c(2, 1, 4)[seq_len(max(1, nlev))]
p <- ggplot(plot_df, aes(x = BASE, y = AVAL, color = TRTPF, shape = TRTPF)) +
  geom_point(size = 2.5) +
  scale_color_manual(values = setNames(pal_cols[seq_along(lvls)], lvls), name = "TRTPN") +
  scale_shape_manual(values = setNames(pal_shapes[seq_along(lvls)], lvls), name = "TRTPN") +
  geom_hline(yintercept = ANRLO_v, linetype = "dashed", color = "blue") +
  geom_hline(yintercept = ANRHI_v, linetype = "dashed", color = "red") +
  geom_vline(xintercept = ANRLO_v, linetype = "dashed", color = "blue") +
  geom_vline(xintercept = ANRHI_v, linetype = "dashed", color = "red") +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x = "Max. Baseline", y = "Max. Post baseline") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(size = 10))
if (is.finite(YAxisMin) && is.finite(YAxisMax)) {
  p <- p + scale_y_continuous(limits = c(YAxisMin, YAxisMax),
                              breaks = seq(YAxisMin, YAxisMax, by = MAJOR))
}
ggsave(file.path(out, "sasgraph.png"), p, width = 8, height = 6, dpi = 150)
# CSV of plotted data
sasgraph_csv <- plot_df %>% select(any_of(c("USUBJID","TRTPN","PARAM","PARAMCD","BASE","AVAL","Ref","ANRLO","ANRHI","BNRIND","ANRIND","CRIT1FL")))
write.csv(sasgraph_csv, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")

# ---- P036 table: baseline shift frequency (TableA report) ----
# CSV body: long TableA with SAS names
P036_body <- TableA %>% transmute(TRTPN = TRTPN, GroupFmt = GroupFmt, BNRIND = BNRIND, ANRIND = ANRIND,
                                  COUNT = COUNT, PERCENT = round_half_up(PERCENT, 1),
                                  ValueFmt = ValueFmt, GroupN = GroupN)
write.csv(P036_body, file.path(out, "P036.csv"), row.names = FALSE, na = "")

# text rendering for P036.lst
# wide pivot of ValueFmt
wideA <- TableA %>% select(GroupFmt, TRTPN, BNRIND, ANRIND, ValueFmt, COUNT, PERCENT) %>%
  arrange(TRTPN, BNRIND, ANRIND)
# build text
con1 <- file(file.path(out, "P036.txt"), open = "wt")
writeLines("Baseline Shift Frequency Table - Max Baseline (BNRIND) by Max Post-Baseline (ANRIND)", con1)
writeLines(paste0("Source: advsmax DIABP ATPTN=815 ANL01FL=Y ADY>1 SAFFL=Y  N=", nrow(advsmax2)), con1)
writeLines("", con1)
# header row with ANRIND levels
anr_levels <- sort(unique(TableA$ANRIND))
hdr <- sprintf("%-14s %-12s %s", "Treatment", "MaxBaseline", paste(paste0(anr_levels, " [N (%)]"), collapse = " | "))
writeLines(hdr, con1)
# rows grouped by GroupFmt with blank line after each group (compute after GroupFmt line " ")
grp_order <- unique(TableA[order(TableA$TRTPN, na.last = FALSE), "GroupFmt"])
for (g in grp_order) {
  sub <- TableA %>% filter(GroupFmt == g) %>% arrange(BNRIND)
  for (i in seq_len(nrow(sub))) {
    # need one row per BNRIND with all ANRIND ValueFmts
    NULL
  }
  # aggregate by BNRIND
  bnr_u <- sort(unique(sub$BNRIND))
  for (b in bnr_u) {
    cells <- sapply(anr_levels, function(a) {
      v <- sub$ValueFmt[sub$BNRIND == b & sub$ANRIND == a]
      if (length(v) == 0) "0 (0.0)" else v[1]
    })
    disp_b <- ifelse(b == "", "(Missing)", b)
    line <- sprintf("%-14s %-12s %s", g, disp_b, paste(cells, collapse = " | "))
    writeLines(line, con1)
  }
  writeLines(" ", con1)
}
writeLines("", con1)
writeLines("Notes: COUNT = cell count; PERCENT = 100*COUNT/GroupN within TRTPN (missing included).", con1)
writeLines("ValueFmt = COUNT (PERCENT with 1 decimal, SAS comma8./10.1, half away from zero).", con1)
close(con1)

# ---- Target16 table: treatment emergent high ----
Target16_body <- EmergentHigh %>% transmute(TRTPN = TRTPN, Nx = Nx, n = n,
                                            percent = round_half_up(percent, 1),
                                            FishersP = FishersP)
write.csv(Target16_body, file.path(out, "Target16.csv"), row.names = FALSE, na = "")

con2 <- file(file.path(out, "Target16.txt"), open = "wt")
writeLines("Treatment Emergent High Table (advsmax3: CRIT1FL non-missing, n = (orig row mod 17 = 1))", con2)
writeLines(paste0("Fisher exact (TRTPN*n) two-sided p = ", fmt_pvalue(fish_p)), con2)
writeLines("", con2)
writeLines(sprintf("%-8s %-8s %-8s %-8s %-10s", "TRTPN", "Nx", "n", "%", "FishersP"), con2)
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i, ]
  line <- sprintf("%-8s %-8s %-8s %-8s %-10s",
                  ifelse(is.na(r$TRTPN), "", as.character(r$TRTPN)),
                  fmt_comma(r$Nx), fmt_comma(r$n),
                  fmt_pct1(r$percent),
                  fmt_pvalue(r$FishersP))
  writeLines(line, con2)
}
writeLines("", con2)
writeLines(paste0("Reference limits: mean ANRLO=", sprintf("%.2f", round_half_up(mean_ANRLO, 2)),
                  " mean ANRHI=", sprintf("%.2f", round_half_up(mean_ANRHI, 2)),
                  " Min AVAL=", max(0, minValue_raw, na.rm = TRUE),
                  " Max AVAL=", maxValue_raw), con2)
writeLines(paste0("Y-axis: MIN=", YAxisMin, " MAX=", YAxisMax, " BY=", MAJOR), con2)
writeLines("Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.", con2)
writeLines("N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.", con2)
writeLines(paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN ", fmt_pct1(ANRLO_v),
                  " mmol/L ULN ", fmt_pct1(ANRHI_v), " mmol/L) set 2 (Female, Age 18-65, LLN=", fmt_pct1(ANRLO_v),
                  " mmol/L ULN=", fmt_pct1(ANRHI_v), " mmol/L)."), con2)
close(con2)

cat("P036 done\n")
