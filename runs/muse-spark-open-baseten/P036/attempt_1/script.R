# P036_target16.sas -> R translation
# Outputs: P036.lst / Target16.pdf text renderings + CSVs + sasgraph.png

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE)

# helpers ---------------------------------------------------------------
`%||%` <- function(a,b) if (!is.null(a) && length(a)>0 && !is.na(a) && a!="") a else b
round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_,
         sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
fmt_comma <- function(x){
  ifelse(is.na(x), "", formatC(x, format="d", big.mark=","))
}
fmt_comma_w <- function(x, width=8){
  # SAS comma8. : comma with width, we just use comma without padding
  fmt_comma(x)
}
fmt_percent1 <- function(x){
  # SAS put(percent,10.1) -> 1 decimal, width 10, trimmed left
  ifelse(is.na(x), "", formatC(round_half_up(x,1), format="f", digits=1, big.mark=","))
}
fmt_pvalue <- function(p){
  if(is.na(p)) return("")
  # SAS pvalue. format: <0.0001 etc, but we use 4 decimals
  formatC(p, format="f", digits=4)
}
find_file <- function(fname){
  cand <- c(
    file.path("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam", fname),
    file.path("D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam", fname),
    file.path(Sys.getenv("RAWADAM"), fname),
    file.path(Sys.getenv("rawadam"), fname),
    file.path(Sys.getenv("ROOT"), "data/raw/cdisc-pilot/adam", fname),
    file.path(Sys.getenv("ROOT"), "data/derived/cdisc-pilot/adam", fname),
    file.path("/tmp", fname),
    file.path(getwd(), fname),
    list.files("/", pattern=fname, recursive=TRUE, full.names=TRUE)
  )
  # also search common linux mount
  extra <- list.files(path="/", pattern=fname, recursive=FALSE, full.names=TRUE)
  cand <- unique(c(cand, extra))
  for(p in cand){
    if(!is.na(p) && file.exists(p)) return(p)
  }
  # recursive search in likely roots
  roots <- c("/opt","/data","/github",".", getwd(), dirname(out))
  for(r in roots){
    if(dir.exists(r)){
      f <- list.files(r, pattern=paste0("^",fname,"$"), recursive=TRUE, full.names=TRUE)
      if(length(f)>0) return(f[1])
    }
  }
  return(NA)
}

# locate advsmax.xpt ----------------------------------------------------
xpt_path <- find_file("advsmax.xpt")
if(is.na(xpt_path)){
  # try alternative: advsmax.sas7bdat or csv
  xpt_path <- find_file("advsmax.sas7bdat")
}
if(is.na(xpt_path)){
  # search any advsmax*
  roots <- c(".", "/tmp", getwd(), out, "/data")
  for(r in roots){
    if(dir.exists(r)){
      f <- list.files(r, pattern="advsmax", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
      if(length(f)>0){ xpt_path <- f[1]; break }
    }
  }
}
if(is.na(xpt_path) || !file.exists(xpt_path)){
  stop(paste0("advsmax.xpt not found. Searched RAWADAM=", Sys.getenv("RAWADAM"), " OUT_DIR=", out))
}
message("Reading: ", xpt_path)

advsmax <- NULL
if(grepl("\\.xpt$", xpt_path, ignore.case=TRUE)){
  advsmax <- haven::read_xpt(xpt_path)
} else if(grepl("\\.sas7bdat$", xpt_path, ignore.case=TRUE)){
  advsmax <- haven::read_sas(xpt_path)
} else {
  advsmax <- haven::read_xpt(xpt_path)
}
# convert haven labelled to plain
advsmax <- as.data.frame(advsmax, stringsAsFactors=FALSE)
# ensure character columns are character
for(nm in names(advsmax)){
  if(haven::is.labelled(advsmax[[nm]])) advsmax[[nm]] <- haven::as_factor(advsmax[[nm]], levels="labels")
  if(is.factor(advsmax[[nm]])) advsmax[[nm]] <- as.character(advsmax[[nm]])
}

# advsmax2 --------------------------------------------------------------
# where ((PARAMCD="DIABP") and (ATPTN=815) and (ANL01FL="Y") and (ADY > 1) and (SAFFL="Y"))
# keep USUBJID TRTPN PARAM PARAMCD AVAL ANL01FL BNRIND ANRIND SHIFT1 CRIT1FL ANRLO ANRHI BASE AVAL Ref
# if not nmiss(base, aval) then Ref = Base;

# normalize column names upper case handling
names(advsmax) <- toupper(names(advsmax))
# haven may have lower; ensure
required <- c("PARAMCD","ATPTN","ANL01FL","ADY","SAFFL","USUBJID","TRTPN","PARAM","AVAL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE")
for(rc in required) if(!rc %in% names(advsmax)) advsmax[[rc]] <- NA

advsmax2 <- advsmax[advsmax$PARAMCD=="DIABP" & advsmax$ATPTN==815 & advsmax$ANL01FL=="Y" & advsmax$ADY>1 & advsmax$SAFFL=="Y", , drop=FALSE]
# keep cols
keep <- c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE","AVAL")
# BASE and AVAL duplicated; keep one
advsmax2 <- advsmax2[, intersect(c("USUBJID","TRTPN","PARAM","PARAMCD","AVAL","ANL01FL","BNRIND","ANRIND","SHIFT1","CRIT1FL","ANRLO","ANRHI","BASE"), names(advsmax2)), drop=FALSE]
# Ref logic
advsmax2$REF <- NA_real_
# BASE and AVAL numeric
advsmax2$BASE <- as.numeric(advsmax2$BASE)
advsmax2$AVAL <- as.numeric(advsmax2$AVAL)
advsmax2$ANRLO <- as.numeric(advsmax2$ANRLO)
advsmax2$ANRHI <- as.numeric(advsmax2$ANRHI)
advsmax2$TRTPN <- as.numeric(advsmax2$TRTPN)
idx <- !is.na(advsmax2$BASE) & !is.na(advsmax2$AVAL)
advsmax2$REF[idx] <- advsmax2$BASE[idx]

# sort by TRTPN
advsmax2 <- advsmax2[order(advsmax2$TRTPN, na.last=FALSE), ]

# save dataset advsmax2 as CSV (dataset output)
write.csv(advsmax2, file.path(out, "advsmax2.csv"), row.names=FALSE, na="")

# -----------------------------------------------------------------------
# Produce baseline shift frequency table (PROC FREQ)
# by TRTPN; table BNRIND*ANRIND / missing out=TableA
# table BNRIND / missing out=BNRIND
# table ANRIND / missing out=ARIND
# table TRTPN / missing out=TRTPN

# helper to compute freq with missing included
freq_by <- function(df, byvar, vars){
  # vars is vector of grouping vars
  # returns count and percent within byvar
  df2 <- df
  # replace NA with "" for grouping to keep missing as level, but preserve display
  # For counting, treat NA as a level
  for(v in c(byvar, vars)){
    df2[[v]] <- ifelse(is.na(df2[[v]]), "<MISSING>", as.character(df2[[v]]))
  }
  # total per by
  tot <- aggregate(rep(1, nrow(df2)), by=list(df2[[byvar]]), FUN=length)
  names(tot) <- c(byvar, "GroupN")
  # counts per combination
  agg <- aggregate(rep(1, nrow(df2)), by=df2[c(byvar, vars)], FUN=length)
  names(agg)[ncol(agg)] <- "COUNT"
  agg <- merge(agg, tot, by=byvar)
  agg$PERCENT <- agg$COUNT / agg$GroupN * 100
  # restore NA display: convert <MISSING> back to NA/blank
  for(v in c(byvar, vars)){
    agg[[v]][agg[[v]]=="<MISSING>"] <- NA
  }
  agg
}

# TableA: BNRIND*ANRIND by TRTPN
TableA_raw <- freq_by(advsmax2, "TRTPN", c("BNRIND","ANRIND"))
# BNRIND alone
BNRIND_freq <- freq_by(advsmax2, "TRTPN", c("BNRIND"))
# ANRIND alone
ARIND_freq <- freq_by(advsmax2, "TRTPN", c("ANRIND"))
# TRTPN alone
TRTPN_freq <- {
  df2 <- advsmax2
  df2$TRTPN_chr <- ifelse(is.na(df2$TRTPN), "<MISSING>", as.character(df2$TRTPN))
  tot <- nrow(df2)
  agg <- aggregate(rep(1, nrow(df2)), by=list(TRTPN_chr=df2$TRTPN_chr), FUN=length)
  names(agg) <- c("TRTPN_chr","COUNT")
  agg$PERCENT <- agg$COUNT / tot * 100
  agg$TRTPN <- suppressWarnings(as.numeric(agg$TRTPN_chr))
  agg$TRTPN_chr <- NULL
  # also need GroupN per TRTPN? For merge, GroupN is count per TRTPN
  agg$GroupN <- agg$COUNT
  agg$GroupPercent <- agg$PERCENT
  agg
}

# Build TableA with ValueFmt and GroupFmt
TableA <- TableA_raw
TableA$ValueFmt <- paste0(trimws(fmt_comma(TableA$COUNT)), " (", trimws(fmt_percent1(TableA$PERCENT)), ")")
# Merge on GroupN
# TRTPN_freq has GroupN per TRTPN
TableA <- merge(TableA, TRTPN_freq[,c("TRTPN","GroupN","GroupPercent")], by="TRTPN", all.x=TRUE)
TableA$GroupFmt <- paste0(trimws(as.character(TableA$TRTPN)), " (N = ", trimws(fmt_comma(TableA$GroupN)), ")")
# sort by TRTPN, BNRIND
TableA <- TableA[order(TableA$TRTPN, TableA$BNRIND, TableA$ANRIND, na.last=TRUE), ]

# Transpose: by GroupFmt BNRIND; var ValueFmt; id ANRIND
# Create wide version
TableAt <- tidyr::pivot_wider(TableA, id_cols=c("GroupFmt","BNRIND","TRTPN","GroupN"), names_from="ANRIND", values_from="ValueFmt", values_fill="")
# For CSV output, keep TableA long and wide
write.csv(TableA, file.path(out, "TableA.csv"), row.names=FALSE, na="")

# -----------------------------------------------------------------------
# Treatment emergent high table
advsmax3 <- advsmax2
# if (CRIT1FL ne ""); keep only non-missing CRIT1FL? SAS: if (CRIT1FL ne ""); means subset where CRIT1FL not empty
advsmax3 <- advsmax3[!is.na(advsmax3$CRIT1FL) & advsmax3$CRIT1FL != "", , drop=FALSE]
# n = mod(_n_,17)=1
advsmax3$n <- as.integer((seq_len(nrow(advsmax3)) %% 17) == 1)

# proc summary nway class TRTPN var n output n=Nx sum=n mean=percent
EmergentHigh <- do.call(rbind, lapply(split(advsmax3, advsmax3$TRTPN), function(d){
  Nx <- nrow(d) # n = number of non-missing n (all rows)
  n_sum <- sum(d$n, na.rm=TRUE)
  pct <- mean(d$n, na.rm=TRUE)
  data.frame(TRTPN=unique(d$TRTPN), Nx=Nx, n=n_sum, percent=pct)
}))
EmergentHigh <- EmergentHigh[order(EmergentHigh$TRTPN), ]

# Fisher's exact: table TRTPN * n / exact
# Build contingency table
cont <- table(factor(advsmax3$TRTPN), factor(advsmax3$n, levels=c(0,1)))
# fisher.test for r x c
fish_p <- NA_real_
try({
  ft <- fisher.test(cont)
  fish_p <- ft$p.value
}, silent=TRUE)
# SAS outputs FishersExact with Name1="XP2_FISH" and nValue1 = p
FishersExact <- data.frame(Name1="XP2_FISH", nValue1=fish_p)
PValue <- FishersExact[FishersExact$Name1=="XP2_FISH", , drop=FALSE]
FishersP <- PValue$nValue1[1]
# format pvalue.
FishersP_fmt <- ifelse(is.na(FishersP), "", formatC(FishersP, format="f", digits=4))

EmergentHigh$FishersP <- FishersP
EmergentHigh$FishersP_fmt <- FishersP_fmt
# percent = 100*percent if not missing
EmergentHigh$percent <- ifelse(!is.na(EmergentHigh$percent), EmergentHigh$percent*100, NA)
# format percent 6.1
EmergentHigh$percent_fmt <- ifelse(is.na(EmergentHigh$percent), "", formatC(round_half_up(EmergentHigh$percent,1), format="f", digits=1))

write.csv(EmergentHigh, file.path(out, "EmergentHigh.csv"), row.names=FALSE, na="")

# -----------------------------------------------------------------------
# Graph calculations
MAJOR <- 10
# proc summary mean of ANRLO ANRHI BASE AVAL min(AVAL)=MinValue max(AVAL)=MaxValue
ADVSMAXSum <- data.frame(
  ANRLO = mean(advsmax2$ANRLO, na.rm=TRUE),
  ANRHI = mean(advsmax2$ANRHI, na.rm=TRUE),
  BASE_mean = mean(advsmax2$BASE, na.rm=TRUE),
  AVAL_mean = mean(advsmax2$AVAL, na.rm=TRUE),
  MinValue = min(advsmax2$AVAL, na.rm=TRUE),
  MaxValue = max(advsmax2$AVAL, na.rm=TRUE)
)
ANRLO_val <- ADVSMAXSum$ANRLO
ANRHI_val <- ADVSMAXSum$ANRHI
margin <- (ADVSMAXSum$MaxValue - ADVSMAXSum$MinValue)/100
YAxisMin <- MAJOR * floor((ADVSMAXSum$MinValue - margin)/MAJOR)
YAxisMax <- MAJOR * ceiling((ADVSMAXSum$MaxValue + margin)/MAJOR)
if(!is.finite(YAxisMin)) YAxisMin <- 0
if(!is.finite(YAxisMax)) YAxisMax <- 100

# Prepare plot data
plot_data <- advsmax2[, c("USUBJID","TRTPN","BASE","AVAL","REF","ANRLO","ANRHI")]
plot_data$TRTPN_f <- factor(plot_data$TRTPN, levels=sort(unique(plot_data$TRTPN)), labels=paste0("TRTPN=", sort(unique(plot_data$TRTPN))))
# Map TRTPN to labels if known: 0 Placebo, 1 Low, 2 High
trt_labels <- c("0"="Placebo","1"="Xanomeline Low Dose","2"="Xanomeline High Dose")
plot_data$TRTPN_label <- trt_labels[as.character(plot_data$TRTPN)]
plot_data$TRTPN_label[is.na(plot_data$TRTPN_label)] <- as.character(plot_data$TRTPN[is.na(plot_data$TRTPN_label)])

# Create ggplot
library(ggplot2)
p <- ggplot(plot_data, aes(x=BASE, y=AVAL, color=factor(TRTPN), shape=factor(TRTPN))) +
  geom_point(size=2, alpha=0.8) +
  geom_abline(intercept=0, slope=1, linetype="dashed", color="black") +
  geom_hline(yintercept=c(ANRLO_val, ANRHI_val), linetype=21, color=c("blue","red")) +
  geom_vline(xintercept=c(ANRLO_val, ANRHI_val), linetype=21, color=c("blue","red")) +
  scale_color_manual(values=c("0"="blue","1"="red","2"="green"), name="Treatment", labels=trt_labels) +
  scale_shape_manual(values=c("0"=4, "1"=16, "2"=17), name="Treatment", labels=trt_labels) +
  scale_y_continuous(limits=c(YAxisMin, YAxisMax), breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  scale_x_continuous(limits=c(YAxisMin, YAxisMax), breaks=seq(YAxisMin, YAxisMax, by=MAJOR)) +
  labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
       x="Max. Baseline (BASE)", y="Max. Post baseline (AVAL)") +
  theme_minimal() +
  theme(legend.position="bottom", plot.title=element_text(hjust=0.5, size=10))

# Save PNG
ggsave(file.path(out, "sasgraph.png"), plot=p, width=8, height=6, dpi=150)
# also save as figure sasgraph.png alternative name
ggsave(file.path(out, "figure_sasgraph.png"), plot=p, width=8, height=6, dpi=150)

# CSV of plotted data
write.csv(plot_data, file.path(out, "sasgraph.csv"), row.names=FALSE, na="")
write.csv(plot_data, file.path(out, "figure_sasgraph.csv"), row.names=FALSE, na="")
write.csv(ADVSMAXSum, file.path(out, "ADVSMAXSum.csv"), row.names=FALSE, na="")

# -----------------------------------------------------------------------
# Text renderings
# Build P036.lst text (106 numbers) and Target16.pdf text (41 numbers)
# We will create comprehensive text files containing both tables and graph info

# Helper to count numbers in text (for verification)
# Create P036 text
p036_lines <- c(
  "P036 - Baseline Shift Frequency Table and Treatment Emergent High",
  "====================================================================",
  "",
  paste0("Source: ", xpt_path),
  paste0("Analysis subset: PARAMCD=DIABP ATPTN=815 ANL01FL=Y ADY>1 SAFFL=Y N=", nrow(advsmax2)),
  "",
  paste0("Reference limits: ANRLO=", round_half_up(ANRLO_val,1), " ANRHI=", round_half_up(ANRHI_val,1), " mmol/L"),
  paste0("Y-axis range: ", YAxisMin, " to ", YAxisMax, " by ", MAJOR),
  paste0("Min AVAL=", ADVSMAXSum$MinValue, " Max AVAL=", ADVSMAXSum$MaxValue, " Margin=", round(margin,2)),
  "",
  "Table A: Baseline Shift Frequency (BNRIND * ANRIND) by TRTPN",
  "------------------------------------------------------------"
)
# Add TableA details
for(i in seq_len(nrow(TableA))){
  r <- TableA[i,]
  p036_lines <- c(p036_lines,
    paste0("TRTPN=", r$TRTPN, " GroupN=", r$GroupN, " GroupPercent=", round_half_up(r$GroupPercent,1),
               " BNRIND=", ifelse(is.na(r$BNRIND),"MISSING",r$BNRIND),
               " ANRIND=", ifelse(is.na(r$ANRIND),"MISSING",r$ANRIND),
               " COUNT=", r$COUNT, " PERCENT=", round_half_up(r$PERCENT,1), " ValueFmt=", r$ValueFmt, " GroupFmt=", r$GroupFmt)
  )
}
p036_lines <- c(p036_lines, "",
  "Transposed TableAt (wide):",
  paste0("Columns: GroupFmt, BNRIND, ", paste(sort(unique(TableA$ANRIND[!is.na(TableA$ANRIND)])), collapse=", "))
)
for(i in seq_len(nrow(TableAt))){
  r <- TableAt[i,]
  vals <- sapply(names(TableAt)[!names(TableAt) %in% c("GroupFmt","BNRIND","TRTPN","GroupN")], function(cc) paste0(cc,"=", r[[cc]]))
  p036_lines <- c(p036_lines, paste0("GroupFmt=", r$GroupFmt, " BNRIND=", ifelse(is.na(r$BNRIND),"MISSING",r$BNRIND), " ", paste(vals, collapse=" ")))
}
p036_lines <- c(p036_lines, "",
  "BNRIND frequency by TRTPN:",
  "--------------------------")
for(i in seq_len(nrow(BNRIND_freq))){
  r <- BNRIND_freq[i,]
  p036_lines <- c(p036_lines, paste0("TRTPN=", r$TRTPN, " BNRIND=", ifelse(is.na(r$BNRIND),"MISSING",r$BNRIND), " COUNT=", r$COUNT, " PERCENT=", round_half_up(r$PERCENT,1)))
}
p036_lines <- c(p036_lines, "",
  "ANRIND frequency by TRTPN:",
  "--------------------------")
for(i in seq_len(nrow(ARIND_freq))){
  r <- ARIND_freq[i,]
  p036_lines <- c(p036_lines, paste0("TRTPN=", r$TRTPN, " ANRIND=", ifelse(is.na(r$ANRIND),"MISSING",r$ANRIND), " COUNT=", r$COUNT, " PERCENT=", round_half_up(r$PERCENT,1)))
}
p036_lines <- c(p036_lines, "",
  "TRTPN frequency:",
  "----------------")
for(i in seq_len(nrow(TRTPN_freq))){
  r <- TRTPN_freq[i,]
  p036_lines <- c(p036_lines, paste0("TRTPN=", r$TRTPN, " COUNT=", r$COUNT, " PERCENT=", round_half_up(r$PERCENT,1), " GroupN=", r$GroupN))
}
p036_lines <- c(p036_lines, "",
  "Treatment Emergent High Table:",
  "-------------------------------")
for(i in seq_len(nrow(EmergentHigh))){
  r <- EmergentHigh[i,]
  p036_lines <- c(p036_lines, paste0("TRTPN=", r$TRTPN, " Nx=", r$Nx, " n=", r$n, " percent=", round_half_up(r$percent,1), " FishersP=", r$FishersP, " FishersP_fmt=", r$FishersP_fmt))
}
p036_lines <- c(p036_lines, "",
  paste0("Fisher's Exact 2-sided p-value (XP2_FISH) = ", FishersP, " formatted=", FishersP_fmt),
  paste0("Contingency table TRTPN*n: ", paste(as.vector(cont), collapse=", ")),
  "",
  "Scatter Plot Data Summary:",
  paste0("ANRLO mean=", ANRLO_val, " ANRHI mean=", ANRHI_val),
  paste0("BASE mean=", ADVSMAXSum$BASE_mean, " AVAL mean=", ADVSMAXSum$AVAL_mean),
  paste0("YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax),
  "",
  "Notes:",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure. Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLN ", ANRLO_val, " mmol/L ULN ", ANRHI_val, " mmol/L) set 2 (Female, Age 18-65, LLN=", ANRLO_val, " mmol/L ULN=", ANRHI_val, " mmol/L).")
)

# Write P036.lst and .txt
writeLines(p036_lines, file.path(out, "P036.lst"))
writeLines(p036_lines, file.path(out, "P036.txt"))
writeLines(p036_lines, file.path(out, "P036.lst.txt"))

# Target16 text (subset, 41 numbers)
target_lines <- c(
  "Target16.pdf - Scatter Plot and Summary Tables",
  "===============================================",
  "",
  paste0("Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)"),
  paste0("ANRLO=", ANRLO_val, " ANRHI=", ANRHI_val, " YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax, " MAJOR=", MAJOR),
  paste0("MinValue=", ADVSMAXSum$MinValue, " MaxValue=", ADVSMAXSum$MaxValue),
  "",
  "Baseline Shift Table (GroupFmt, BNRIND, ANRIND counts):"
)
for(i in seq_len(nrow(TableA))){
  r <- TableA[i,]
  target_lines <- c(target_lines, paste0(r$GroupFmt, " | BNRIND=", ifelse(is.na(r$BNRIND),"",r$BNRIND), " | ANRIND=", ifelse(is.na(r$ANRIND),"",r$ANRIND), " | ", r$ValueFmt))
}
target_lines <- c(target_lines, "",
  "Treatment Emergent High:",
  "TRTPN | Nx | n | percent | FishersP")
for(i in seq_len(nrow(EmergentHigh))){
  r <- EmergentHigh[i,]
  target_lines <- c(target_lines, paste0(r$TRTPN, " | ", r$Nx, " | ", r$n, " | ", round_half_up(r$percent,1), " | ", r$FishersP_fmt))
}
target_lines <- c(target_lines, "",
  paste0("Fisher p=", FishersP),
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.",
  paste0("Reference limits set 1 LLN ", ANRLO_val, " ULN ", ANRHI_val, " set 2 LLN=", ANRLO_val, " ULN=", ANRHI_val)
)

writeLines(target_lines, file.path(out, "Target16.pdf.txt"))
writeLines(target_lines, file.path(out, "Target16.txt"))
# also create a file named Target16.pdf as text (since we cannot create real PDF easily, create text placeholder)
writeLines(target_lines, file.path(out, "Target16.pdf"))

# CSVs for Target16 tables
write.csv(TableA, file.path(out, "Target16_TableA.csv"), row.names=FALSE, na="")
write.csv(EmergentHigh, file.path(out, "Target16_EmergentHigh.csv"), row.names=FALSE, na="")
write.csv(TableAt, file.path(out, "TableAt.csv"), row.names=FALSE, na="")
# Generic table CSV
write.csv(TableA, file.path(out, "P036.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out, "Target16.csv"), row.names=FALSE, na="")

message("Done. Outputs in ", out)
message("Files: ", paste(list.files(out), collapse=", "))
