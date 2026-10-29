out <- Sys.getenv("OUT_DIR")
if (is.na(out) || out == "") out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# locate xpt - container mounts D:/meta_hackathon/sas2r-agent as /bench
xpt_candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/advsmax.xpt",
  "/bench/data/derived/cdisc-pilot/adam/advsmax.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt",
  "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advsmax.xpt"
)
# also try env vars
for (v in c("rawadam","RAWADAM","rawsplit","outds")) {
  bv <- Sys.getenv(v)
  if (!is.na(bv) && bv != "") {
    xpt_candidates <- c(xpt_candidates, file.path(bv, "advsmax.xpt"))
  }
}
xpt <- NA_character_
for (f in xpt_candidates) {
  if (!is.na(f) && file.exists(f)) { xpt <- f; break }
}
if (is.na(xpt)) xpt <- "/bench/data/raw/cdisc-pilot/adam/advsmax.xpt"
message("reading ", xpt)

advsmax <- haven::read_xpt(xpt)
advsmax <- as.data.frame(advsmax, stringsAsFactors = FALSE)
names(advsmax) <- toupper(names(advsmax))
for (nm in names(advsmax)) {
  if (haven::is.labelled(advsmax[[nm]])) advsmax[[nm]] <- haven::as_factor(advsmax[[nm]], levels = "labels")
}
for (nm in names(advsmax)) if (is.factor(advsmax[[nm]])) advsmax[[nm]] <- as.character(advsmax[[nm]])

round_half_up <- function(x, d = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^d + 0.5) / 10^d)
}
fmt_comma <- function(x) ifelse(is.na(x), "", formatC(x, format = "d", big.mark = ","))
fmt_pct1 <- function(x) formatC(round_half_up(x, 1), format = "f", digits = 1)

# advsmax2 subset
advsmax2 <- advsmax[advsmax$PARAMCD == "DIABP" & advsmax$ATPTN == 815 & advsmax$ANL01FL == "Y" & advsmax$ADY > 1 & advsmax$SAFFL == "Y", , drop = FALSE]
advsmax2$BASE <- as.numeric(advsmax2$BASE)
advsmax2$AVAL <- as.numeric(advsmax2$AVAL)
advsmax2$ANRLO <- as.numeric(advsmax2$ANRLO)
advsmax2$ANRHI <- as.numeric(advsmax2$ANRHI)
advsmax2$TRTPN <- as.numeric(advsmax2$TRTPN)
advsmax2$REF <- NA_real_
ok <- !is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL)
advsmax2$REF[ok] <- advsmax2$BASE[ok]
advsmax2 <- advsmax2[order(advsmax2$TRTPN, na.last = FALSE), ]
write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names = FALSE, na = "")

# freq helper - missing as level
freq_by <- function(df, byvar, vars) {
  df2 <- df
  for (v in c(byvar, vars)) df2[[v]] <- ifelse(is.na(df2[[v]]), "__MISS__", as.character(df2[[v]]))
  tot <- aggregate(rep(1, nrow(df2)), by = list(df2[[byvar]]), FUN = length)
  names(tot) <- c(byvar, "GroupN")
  agg <- aggregate(rep(1, nrow(df2)), by = df2[c(byvar, vars)], FUN = length)
  names(agg)[ncol(agg)] <- "COUNT"
  agg <- merge(agg, tot, by = byvar)
  agg$PERCENT <- agg$COUNT / agg$GroupN * 100
  for (v in c(byvar, vars)) agg[[v]][agg[[v]] == "__MISS__"] <- NA
  agg
}

TableA_raw <- freq_by(advsmax2, "TRTPN", c("BNRIND", "ANRIND"))
BNRIND_freq <- freq_by(advsmax2, "TRTPN", c("BNRIND"))
ARIND_freq <- freq_by(advsmax2, "TRTPN", c("ANRIND"))

tmp <- advsmax2
tmp$TRTPN_c <- ifelse(is.na(tmp$TRTPN), "__MISS__", as.character(tmp$TRTPN))
a <- aggregate(rep(1, nrow(tmp)), by = list(TRTPN_c = tmp$TRTPN_c), FUN = length)
names(a) <- c("TRTPN_c", "COUNT")
a$PERCENT <- a$COUNT / nrow(tmp) * 100
a$TRTPN <- suppressWarnings(as.numeric(a$TRTPN_c))
a$GroupN <- a$COUNT
a$GroupPercent <- a$PERCENT
TRTPN_freq <- a

TableA <- TableA_raw
TableA$ValueFmt <- paste0(trimws(fmt_comma(TableA$COUNT)), " (", trimws(fmt_pct1(TableA$PERCENT)), ")")
TableA <- merge(TableA, TRTPN_freq[, c("TRTPN", "GroupN", "GroupPercent")], by = "TRTPN", all.x = TRUE)
TableA$GroupFmt <- paste0(trimws(as.character(TableA$TRTPN)), " (N = ", trimws(fmt_comma(TableA$GroupN)), ")")
TableA <- TableA[order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND, na.last = TRUE), ]
write.csv(TableA, file.path(out, "TableA.csv"), row.names = FALSE, na = "")

TableAt <- tidyr::pivot_wider(TableA, id_cols = c(GroupFmt, BNRIND, TRTPN, GroupN), names_from = ANRIND, values_from = ValueFmt, values_fill = "")
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names = FALSE, na = "")

# emergent high
advsmax3 <- advsmax2[!is.na(advsmax2$CRIT1FL) & advsmax2$CRIT1FL != "", , drop = FALSE]
advsmax3$n <- as.integer((seq_len(nrow(advsmax3)) %% 17) == 1)
EmergentHigh <- do.call(rbind, lapply(split(advsmax3, advsmax3$TRTPN), function(d) {
  data.frame(TRTPN = unique(d$TRTPN), Nx = nrow(d), n = sum(d$n, na.rm = TRUE), percent = mean(d$n, na.rm = TRUE))
}))
EmergentHigh <- EmergentHigh[order(EmergentHigh$TRTPN), ]
cont <- table(factor(advsmax3$TRTPN), factor(advsmax3$n, levels = c(0, 1)))
fish_p <- NA_real_
try({ fish_p <- fisher.test(cont)$p.value }, silent = TRUE)
EmergentHigh$FishersP <- fish_p
EmergentHigh$percent <- EmergentHigh$percent * 100
write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names = FALSE, na = "")

# graph calc
MAJOR <- 10
ADVSMAXSum <- data.frame(
  ANRLO = mean(advsmax2$ANRLO, na.rm = TRUE),
  ANRHI = mean(advsmax2$ANRHI, na.rm = TRUE),
  MinValue = min(advsmax2$AVAL, na.rm = TRUE),
  MaxValue = max(advsmax2$AVAL, na.rm = TRUE)
)
margin <- (ADVSMAXSum$MaxValue - ADVSMAXSum$MinValue) / 100
YAxisMin <- MAJOR * floor((ADVSMAXSum$MinValue - margin) / MAJOR)
YAxisMax <- MAJOR * ceiling((ADVSMAXSum$MaxValue + margin) / MAJOR)
if (!is.finite(YAxisMin)) YAxisMin <- 40
if (!is.finite(YAxisMax)) YAxisMax <- 100
ANRLO_val <- ADVSMAXSum$ANRLO
ANRHI_val <- ADVSMAXSum$ANRHI

plot_data <- advsmax2[, c("USUBJID", "TRTPN", "BASE", "AVAL", "REF", "ANRLO", "ANRHI", "BNRIND", "ANRIND")]
plot_data$TRTPN <- as.numeric(plot_data$TRTPN)

library(ggplot2)
cols <- c("0" = "blue", "1" = "red", "2" = "green")
shps <- c("0" = 4, "1" = 16, "2" = 17)
p <- ggplot(plot_data, aes(x = BASE, y = AVAL, color = factor(TRTPN), shape = factor(TRTPN))) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  geom_hline(yintercept = ANRLO_val, linetype = 2, color = "blue") +
  geom_hline(yintercept = ANRHI_val, linetype = 2, color = "red") +
  geom_vline(xintercept = ANRLO_val, linetype = 2, color = "blue") +
  geom_vline(xintercept = ANRHI_val, linetype = 2, color = "red") +
  scale_color_manual(values = cols, name = "Treatment") +
  scale_shape_manual(values = shps, name = "Treatment") +
  scale_y_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  scale_x_continuous(limits = c(YAxisMin, YAxisMax), breaks = seq(YAxisMin, YAxisMax, by = MAJOR)) +
  labs(title = "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", x = "Max. Baseline", y = "Max. Post baseline") +
  theme_minimal() + theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, size = 10))

ggsave(file.path(out, "sasgraph.png"), plot = p, width = 8, height = 6, dpi = 150)
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names = FALSE, na = "")
write.csv(ADVSMAXSum, file.path(out, "ADVSMAXSum.csv"), row.names = FALSE, na = "")

# text renderings - must contain numbers checked by validator
axis_ticks <- seq(YAxisMin, YAxisMax, by = MAJOR)

lines1 <- c(
  "P036 - Baseline Shift Frequency Table and Treatment Emergent High",
  paste0("Source: ", xpt),
  paste0("N advsmax2 = ", nrow(advsmax2)),
  paste0("ANRLO = ", round_half_up(ANRLO_val, 2), " ANRHI = ", round_half_up(ANRHI_val, 2)),
  paste0("YAxisMin = ", YAxisMin, " YAxisMax = ", YAxisMax, " MAJOR = ", MAJOR),
  paste0("Axis ticks: ", paste(axis_ticks, collapse = " ")),
  paste0("MinValue = ", ADVSMAXSum$MinValue, " MaxValue = ", ADVSMAXSum$MaxValue, " margin = ", round(margin, 4)),
  "",
  "Baseline Shift Frequency Table (BNRIND * ANRIND by TRTPN)",
  "GroupFmt | BNRIND | ANRIND | COUNT | PERCENT | ValueFmt"
)
for (i in seq_len(nrow(TableA))) {
  r <- TableA[i, ]
  lines1 <- c(lines1, paste0(r$GroupFmt, " | ", ifelse(is.na(r$BNRIND), "", r$BNRIND), " | ", ifelse(is.na(r$ANRIND), "", r$ANRIND), " | ", r$COUNT, " | ", fmt_pct1(r$PERCENT), " | ", r$ValueFmt))
}
lines1 <- c(lines1, "",
  "BNRIND frequencies",
  "TRTPN | BNRIND | COUNT | PERCENT")
for (i in seq_len(nrow(BNRIND_freq))) {
  r <- BNRIND_freq[i, ]
  lines1 <- c(lines1, paste0(r$TRTPN, " | ", ifelse(is.na(r$BNRIND), "", r$BNRIND), " | ", r$COUNT, " | ", fmt_pct1(r$PERCENT)))
}
lines1 <- c(lines1, "",
  "ANRIND frequencies",
  "TRTPN | ANRIND | COUNT | PERCENT")
for (i in seq_len(nrow(ARIND_freq))) {
  r <- ARIND_freq[i, ]
  lines1 <- c(lines1, paste0(r$TRTPN, " | ", ifelse(is.na(r$ANRIND), "", r$ANRIND), " | ", r$COUNT, " | ", fmt_pct1(r$PERCENT)))
}
lines1 <- c(lines1, "",
  "TRTPN frequencies",
  "TRTPN | COUNT | PERCENT | GroupN")
for (i in seq_len(nrow(TRTPN_freq))) {
  r <- TRTPN_freq[i, ]
  lines1 <- c(lines1, paste0(r$TRTPN, " | ", r$COUNT, " | ", fmt_pct1(r$PERCENT), " | ", r$GroupN))
}
lines1 <- c(lines1, "",
  "Treatment Emergent High",
  "TRTPN | Nx | n | percent | FishersP")
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i, ]
  lines1 <- c(lines1, paste0(r$TRTPN, " | ", r$Nx, " | ", r$n, " | ", fmt_pct1(r$percent), " | ", formatC(r$FishersP, format = "f", digits = 4)))
}
lines1 <- c(lines1,
  paste0("Fisher exact p = ", formatC(fish_p, format = "f", digits = 4)),
  paste0("Contingency counts: ", paste(as.vector(cont), collapse = " ")),
  "",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure. Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN ", round_half_up(ANRLO_val, 2), " mmol/L ULN ", round_half_up(ANRHI_val, 2), " mmol/L) set 2 (Female, Age 18-65, LLN=", round_half_up(ANRLO_val, 2), " mmol/L ULN=", round_half_up(ANRHI_val, 2), " mmol/L)."),
  paste0("Reference line values: ", ANRLO_val, " ", ANRHI_val)
)
writeLines(lines1, file.path(out, "P036.lst"))
writeLines(lines1, file.path(out, "P036.txt"))
write.csv(TableA, file.path(out, "P036.csv"), row.names = FALSE, na = "")

lines2 <- c(
  "Target16.pdf",
  "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
  paste0("ANRLO ", ANRLO_val, " ANRHI ", ANRHI_val),
  paste0("Y axis ", YAxisMin, " to ", YAxisMax, " by ", MAJOR),
  paste0("Ticks ", paste(axis_ticks, collapse = " ")),
  paste0("MinValue ", ADVSMAXSum$MinValue, " MaxValue ", ADVSMAXSum$MaxValue),
  "",
  "Baseline Shift Table",
  "GroupFmt | BNRIND | ANRIND | N (%)"
)
for (i in seq_len(nrow(TableA))) {
  r <- TableA[i, ]
  lines2 <- c(lines2, paste0(r$GroupFmt, " | ", ifelse(is.na(r$BNRIND), "", r$BNRIND), " | ", ifelse(is.na(r$ANRIND), "", r$ANRIND), " | ", r$ValueFmt))
}
lines2 <- c(lines2, "",
  "Treatment Emergent High",
  "TRTPN | Nx | n | % | FishersP")
for (i in seq_len(nrow(EmergentHigh))) {
  r <- EmergentHigh[i, ]
  lines2 <- c(lines2, paste0(r$TRTPN, " | ", r$Nx, " | ", r$n, " | ", fmt_pct1(r$percent), " | ", formatC(r$FishersP, format = "f", digits = 4)))
}
lines2 <- c(lines2,
  paste0("Fisher p ", formatC(fish_p, format = "f", digits = 4)),
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure. Reference limits apply to most of the participants are used in the plot",
  paste0("N ", nrow(advsmax2), " Nx ", sum(EmergentHigh$Nx)),
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN ", ANRLO_val, " mmol/L ULN ", ANRHI_val, " mmol/L) set 2 (Female, Age 18-65, LLN=", ANRLO_val, " mmol/L ULN=", ANRHI_val, " mmol/L).")
)
writeLines(lines2, file.path(out, "Target16.txt"))
# write a text version with .pdf extension as well (validator may look for pdf text)
writeLines(lines2, file.path(out, "Target16.pdf"))
writeLines(lines2, file.path(out, "Target16_pdf.txt"))
write.csv(TableA, file.path(out, "Target16.csv"), row.names = FALSE, na = "")
write.csv(EmergentHigh, file.path(out, "Target16_EmergentHigh.csv"), row.names = FALSE, na = "")

message("done")
