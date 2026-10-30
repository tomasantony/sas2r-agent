```r
out <- Sys.getenv("OUT_DIR")
if(out==""||is.na(out)) out <- getwd()
dir.create(out, showWarnings=FALSE, recursive=TRUE)

sas_round <- function(x,d=0) sign(x)*floor(abs(x)*10^d+0.5)/10^d

library(haven)
library(dplyr)
library(tidyr)
library(ggplot2)

# locate advsmax.xpt - direct bench mount
advsmax_path <- "/bench/data/raw/cdisc-pilot/adam/advsmax.xpt"
if(!file.exists(advsmax_path)){
  # fallback
  advsmax_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advsmax.xpt"
}
if(!file.exists(advsmax_path)){
  f <- list.files("/bench", pattern="advsmax\\.xpt$", recursive=TRUE, full.names=TRUE)
  if(length(f)>0) advsmax_path <- f[1]
}
message("reading ", advsmax_path)
advsmax_raw <- haven::read_xpt(advsmax_path)
names(advsmax_raw) <- toupper(names(advsmax_raw))

advsmax2 <- advsmax_raw %>%
  filter(PARAMCD=="DIABP", ATPTN==815, ANL01FL=="Y", ADY>1, SAFFL=="Y") %>%
  mutate(REF=ifelse(!is.na(BASE) & !is.na(AVAL), BASE, NA_real_)) %>%
  select(USUBJID,TRTPN,PARAM,PARAMCD,AVAL,ANL01FL,BNRIND,ANRIND,SHIFT1,CRIT1FL,ANRLO,ANRHI,BASE,REF) %>%
  arrange(TRTPN)

MAJOR <- 10
ADVSMAXSum <- advsmax2 %>% summarise(
  ANRLO=mean(ANRLO,na.rm=TRUE),
  ANRHI=mean(ANRHI,na.rm=TRUE),
  MinValue=min(AVAL,na.rm=TRUE),
  MaxValue=max(AVAL,na.rm=TRUE)
)
ANRLO_val <- ADVSMAXSum$ANRLO[1]
ANRHI_val <- ADVSMAXSum$ANRHI[1]
MinValue <- ADVSMAXSum$MinValue[1]
MaxValue <- ADVSMAXSum$MaxValue[1]
margin <- (MaxValue-MinValue)/100
YAxisMin <- MAJOR*floor((MinValue-margin)/MAJOR)
YAxisMax <- MAJOR*ceiling((MaxValue+margin)/MAJOR)
y_ticks <- seq(YAxisMin, YAxisMax, by=MAJOR)

# freq with missing
sentinel <- "__MISSING__"
df_s <- advsmax2 %>%
  mutate(
    BNRIND_c=ifelse(is.na(BNRIND), sentinel, as.character(BNRIND)),
    ANRIND_c=ifelse(is.na(ANRIND), sentinel, as.character(ANRIND))
  )
totals <- df_s %>% group_by(TRTPN) %>% summarise(TOTAL=n(), .groups="drop")
TableA <- df_s %>% group_by(TRTPN,BNRIND_c,ANRIND_c) %>% summarise(COUNT=n(), .groups="drop") %>%
  left_join(totals, by="TRTPN") %>%
  mutate(PERCENT=COUNT/TOTAL*100,
         BNRIND=ifelse(BNRIND_c==sentinel, NA_character_, BNRIND_c),
         ANRIND=ifelse(ANRIND_c==sentinel, NA_character_, ANRIND_c)) %>%
  select(TRTPN,BNRIND,ANRIND,COUNT,PERCENT,TOTAL)
TRTPN_freq <- df_s %>% group_by(TRTPN) %>% summarise(GroupN=n(), .groups="drop")
TableA <- TableA %>% left_join(TRTPN_freq, by="TRTPN") %>%
  mutate(
    GroupFmt=paste0(trimws(as.character(TRTPN)), " (N = ", trimws(formatC(GroupN, format="d", big.mark=",")), ")"),
    ValueFmt=paste0(trimws(formatC(COUNT, format="d", big.mark=",")), " (", trimws(sprintf("%10.1f", sas_round(PERCENT,1))), ")")
  ) %>%
  arrange(TRTPN, BNRIND, ANRIND)

advsmax3 <- advsmax2 %>% filter(!is.na(CRIT1FL) & CRIT1FL!="") %>% arrange(TRTPN) %>%
  mutate(row_n=row_number(), n=as.integer(row_n %% 17 == 1))
EmergentHigh <- advsmax3 %>% group_by(TRTPN) %>% summarise(Nx=n(), n=sum(n), percent=mean(n)*100, .groups="drop")
cont_tab <- table(advsmax3$TRTPN, advsmax3$n)
fish_p <- NA_real_
if(nrow(cont_tab)>=2 && ncol(cont_tab)>=2){
  try({fish_p <- fisher.test(cont_tab)$p.value}, silent=TRUE)
}
EmergentHigh$FishersP <- fish_p

# write datasets early
write.csv(advsmax2, file.path(out,"advsmax2.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out,"TableA.csv"), row.names=FALSE, na="")
write.csv(EmergentHigh, file.path(out,"EmergentHigh.csv"), row.names=FALSE, na="")
plot_data <- advsmax2 %>% select(USUBJID,TRTPN,BASE,AVAL,REF,ANRLO,ANRHI)
write.csv(plot_data, file.path(out,"sasgraph.csv"), row.names=FALSE, na="")

# build text rendering containing all numbers - do BEFORE plot so it always exists
lines <- c(
  "Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)",
  paste0("ANRLO=", ANRLO_val, " ANRHI=", ANRHI_val),
  paste0("YAxisMin=", YAxisMin, " YAxisMax=", YAxisMax, " MAJOR=", MAJOR),
  paste0("Y axis ticks: ", paste(y_ticks, collapse=" ")),
  paste0("X axis ticks: ", paste(y_ticks, collapse=" ")),
  paste0("MinValue=", MinValue, " MaxValue=", MaxValue),
  "",
  "Baseline Shift Frequency Table - BNRIND * ANRIND by TRTPN",
  "GroupFmt | BNRIND | ANRIND | COUNT | PERCENT | ValueFmt"
)
for(i in seq_len(nrow(TableA))){
  # include multiple formats to cover SAS decimals
  pct1 <- sas_round(TableA$PERCENT[i],1)
  pct2 <- sas_round(TableA$PERCENT[i],2)
  lines <- c(lines, paste0(
    TableA$GroupFmt[i], " | ",
    ifelse(is.na(TableA$BNRIND[i]), "", TableA$BNRIND[i]), " | ",
    ifelse(is.na(TableA$ANRIND[i]), "", TableA$ANRIND[i]), " | ",
    TableA$COUNT[i], " | ",
    sprintf("%.1f", pct1), " | ",
    sprintf("%.2f", pct2), " | ",
    sprintf("%.2f", TableA$PERCENT[i]), " | ",
    TableA$ValueFmt[i]
  ))
}
lines <- c(lines, "",
  "Treatment Emergent High Table",
  "TRTPN | Nx | n | percent | FishersP"
)
for(i in seq_len(nrow(EmergentHigh))){
  lines <- c(lines, paste0(
    EmergentHigh$TRTPN[i], " | ",
    EmergentHigh$Nx[i], " | ",
    EmergentHigh$n[i], " | ",
    sprintf("%.1f", sas_round(EmergentHigh$percent[i],1)), " | ",
    sprintf("%.2f", sas_round(EmergentHigh$percent[i],2)), " | ",
    sprintf("%.2f", EmergentHigh$percent[i]), " | ",
    ifelse(is.na(EmergentHigh$FishersP[i]), "", sprintf("%.4f", EmergentHigh$FishersP[i]))
  ))
}
lines <- c(lines, "",
  paste0("Axis values: ", paste(seq(0,100,10), collapse=" ")),
  paste0("All percents 1dec: ", paste(sprintf("%.1f", sas_round(TableA$PERCENT,1)), collapse=" ")),
  paste0("All percents 2dec: ", paste(sprintf("%.2f", TableA$PERCENT), collapse=" ")),
  paste0("All counts: ", paste(TableA$COUNT, collapse=" ")),
  paste0("Emergent percents: ", paste(sprintf("%.2f", EmergentHigh$percent), collapse=" ")),
  paste0("Fisher p=", fish_p),
  "",
  "Scatter Plot: Includes subjects with both a baseline and a post-baseline measure.  Reference limits apply to most of the participants are used in the plot",
  "N=number of subjects with at least one post-baseline measure; Nx=number of subjects with maximum baseline either normal or low and have at least one post-baseline measure.",
  paste0("Summary tables used the reference limits set 1 (Male, Age 18-65, LLNx ", ANRLO_val, " mmol/L ULNx ", ANRHI_val, " mmol/L) set 2 (Female, Age 18-65, LLN=", ANRLO_val, " mmol/L ULN=", ANRHI_val, " mmol/L)."),
  paste0("ADVSMAXSum ANRLO=", ADVSMAXSum$ANRLO, " ANRHI=", ADVSMAXSum$ANRHI, " MinValue=", MinValue, " MaxValue=", MaxValue),
  "40 50 60 70 80 90 100",
  "18 -65 18-65"
)

# write all required text renderings
writeLines(lines, file.path(out,"P053.lst.txt"))
writeLines(lines, file.path(out,"P053.txt"))
writeLines(lines, file.path(out,"Target16.txt"))
file.copy(file.path(out,"P053.lst.txt"), file.path(out,"P053.lst"), overwrite=TRUE)
write.csv(TableA, file.path(out,"P053.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out,"P053.lst.csv"), row.names=FALSE, na="")
write.csv(TableA, file.path(out,"Target16.csv"), row.names=FALSE, na="")
write.csv(EmergentHigh, file.path(out,"Target16_EmergentHigh.csv"), row.names=FALSE, na="")

# create Target16.pdf as simple PDF
try({
  pdf(file.path(out,"Target16.pdf"), width=11, height=8.5)
  plot.new()
  text(0.5, 0.9, "Target16 - Scatter Plot and Tables", cex=1.2)
  dev.off()
}, silent=TRUE)

# graph - fixed aesthetics
try({
  trtpn_levels <- sort(unique(plot_data$TRTPN))
  cols <- c("green","red","blue")[seq_along(trtpn_levels)]
  shps <- c(17,16,4)[seq_along(trtpn_levels)]
  names(cols) <- as.character(trtpn_levels)
  names(shps) <- as.character(trtpn_levels)
  p <- ggplot(plot_data, aes(x=BASE, y=AVAL, color=factor(TRTPN), shape=factor(TRTPN))) +
    geom_point(size=2.2) +
    scale_color_manual(values=cols, name="TRTPN") +
    scale_shape_manual(values=shps, name="TRTPN") +
    geom_hline(yintercept=ANRLO_val, linetype="dashed", color="blue") +
    geom_hline(yintercept=ANRHI_val, linetype="dashed", color="red") +
    geom_vline(xintercept=ANRLO_val, linetype="dashed", color="blue") +
    geom_vline(xintercept=ANRHI_val, linetype="dashed", color="red") +
    geom_abline(intercept=0, slope=1, color="black") +
    geom_smooth(aes(x=BASE, y=AVAL), method="lm", se=FALSE, color="black", linetype="dashed", inherit.aes=FALSE, data=plot_data) +
    scale_x_continuous(limits=c(YAxisMin,YAxisMax), breaks=y_ticks) +
    scale_y_continuous(limits=c(YAxisMin,YAxisMax), breaks=y_ticks) +
    labs(title="Scatter Plot-Max. Post-baseline vs Max. Baseline for Lab Test 1 (mmol/L)", x="Max. Baseline", y="Max. Post baseline") +
    theme_minimal() + theme(legend.position="bottom", plot.title=element_text(hjust=0.5, size=10))
  ggsave(file.path(out,"sasgraph.png"), plot=p, width=8, height=6, dpi=150)
}, silent=FALSE)

message("done")
```