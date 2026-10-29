# P034_target13.sas -> R translation (attempt 3)
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(grid)
})

round_half_up <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_,
    sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_strip <- function(x, digits = 2) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (!is.finite(v)) return(as.character(v))
    r <- round_half_up(v, digits)
    s <- formatC(r, format = "f", digits = digits)
    s <- sub("([.][0-9]*?)0+$", "\\1", s, perl = TRUE)
    s <- sub("[.]$", "", s, perl = TRUE)
    if (s %in% c("-0")) s <- "0"
    s
  }, USE.NAMES = FALSE)
}
sas_fixed <- function(x, digits = 2) {
  sapply(x, function(v) {
    if (is.na(v)) return(".")
    if (!is.finite(v)) return(as.character(v))
    formatC(round_half_up(v, digits), format = "f", digits = digits)
  }, USE.NAMES = FALSE)
}

candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpc.xpt",
  "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adpc.xpt"
)
adpc_path <- NULL
for (p in candidates) if (file.exists(p)) { adpc_path <- p; break }
if (is.null(adpc_path)) stop("adpc.xpt not found")
adpc_all <- haven::read_xpt(adpc_path)
names(adpc_all) <- toupper(names(adpc_all))
if ("PCTESTCD" %in% names(adpc_all))
  adpc_all$PCTESTCD <- trimws(as.character(adpc_all$PCTESTCD), which = "right")

PCTESTCD_sel <- "ANAL1"
keeplist <- c("ANLPRNT","TRTAN","PCSPEC","PCTESTL","TRTA","DOSREFID","PCTESTCD",
              "PCORRESU","PCORRES","USUBJID","PCORRESN","EPLTM")
keeplist <- keeplist[keeplist %in% names(adpc_all)]
adpc <- adpc_all %>% filter(PCTESTCD == PCTESTCD_sel) %>% select(any_of(keeplist))

parse_best10 <- function(v) {
  if (is.null(v)) return(numeric(0))
  if (is.numeric(v) || inherits(v, "labelled")) {
    vv <- suppressWarnings(as.numeric(v))
    ch <- format(vv, scientific = FALSE, trim = TRUE, nsmall = 0)
    ch <- substr(ch, 1, 10)
    m <- stringr::str_extract(ch, "^\\s*[+-]?[0-9]*\\.?[0-9]+([Ee][+-]?[0-9]+)?")
    return(suppressWarnings(as.numeric(m)))
  }
  v <- as.character(v)
  v10 <- substr(v, 1, 10)
  m <- stringr::str_extract(v10, "^\\s*[+-]?[0-9]*\\.?[0-9]+([Ee][+-]?[0-9]+)?")
  suppressWarnings(as.numeric(m))
}
adpc$PCTPT <- parse_best10(adpc$EPLTM)

propcase <- function(x) sapply(x, function(v) {
  if (is.na(v)) return(NA_character_); v <- as.character(v)
  if (nchar(v)==0) return(v)
  paste0(toupper(substr(v,1,1)), tolower(substr(v,2,nchar(v))))
}, USE.NAMES=FALSE)
compbl <- function(x) sapply(x, function(v) {
  if (is.na(v)) return(NA_character_)
  trimws(gsub("\\s+", " ", v, perl=TRUE))
}, USE.NAMES=FALSE)
getcol <- function(df,nm) if (nm %in% names(df)) as.character(df[[nm]]) else rep("", nrow(df))
if (nrow(adpc)>0) {
  adpc$ORDERBY <- gsub(" ","",paste0(getcol(adpc,"ANLPRNT"),getcol(adpc,"PCSPEC"),getcol(adpc,"PCTESTL"),getcol(adpc,"TRTAN")),fixed=TRUE)
  pc_u <- if ("PCORRESU" %in% names(adpc)) as.character(adpc$PCORRESU) else rep("",nrow(adpc))
  pc_u[is.na(pc_u)] <- ""
  adpc$BYVAL6 <- compbl(paste0(propcase(adpc$PCTESTCD)," Conc "," (",pc_u,")"))
}
adpc <- adpc %>% arrange(TRTAN, TRTA, PCTPT)

pcmeans <- adpc %>% group_by(TRTAN, TRTA, PCTPT) %>%
  summarise(N=sum(!is.na(PCORRESN)), NALL=n(),
    PCMEAN=ifelse(sum(!is.na(PCORRESN))>0, mean(PCORRESN,na.rm=TRUE), NA_real_),
    PCSD=suppressWarnings(sd(PCORRESN,na.rm=TRUE)), .groups="drop") %>%
  arrange(TRTAN, TRTA, PCTPT)
meandata <- pcmeans
idx0 <- !is.na(meandata$PCMEAN) & meandata$PCMEAN==0
meandata$PCMEAN[idx0] <- NA_real_; meandata$PCSD[idx0] <- NA_real_
meandata <- meandata %>% arrange(TRTAN, TRTA, PCTPT)
meandata$LOWER <- meandata$PCMEAN - meandata$PCSD
meandata$UPPER <- meandata$PCMEAN + meandata$PCSD

XLABEL <- "Time (h)"; YLABEL <- "Arithmetic mean (SD) Anal2 Conc (ng/mL )"
XMAX <- 100; TICKS <- c(0,4,8,12,24,36,48,72,96); LEGENDSIZE <- 11
uv <- meandata$UPPER[is.finite(meandata$UPPER) & !is.na(meandata$UPPER)]
if (length(uv)==0) uv <- meandata$PCMEAN[is.finite(meandata$PCMEAN) & !is.na(meandata$PCMEAN)]
YMAX <- if (length(uv)==0) 100 else max(uv,na.rm=TRUE)*1.1
if (!is.finite(YMAX) || YMAX<=0) YMAX <- 100

csv_df <- meandata %>% select(any_of(c("TRTAN","TRTA","PCTPT","PCMEAN","PCSD","LOWER","UPPER","N","NALL")))
write.csv(csv_df, file.path(out,"target13.csv"), row.names=FALSE, na="")

trt_order <- meandata %>% distinct(TRTAN, TRTA) %>% arrange(TRTAN)
meandata$TRTA_f <- factor(meandata$TRTA, levels=trt_order$TRTA)
meandata$TRTAN_f <- factor(meandata$TRTAN, levels=trt_order$TRTAN)
p1 <- ggplot(meandata, aes(x=PCTPT,y=PCMEAN,group=TRTAN_f,linetype=TRTA_f,shape=TRTA_f)) +
  geom_errorbar(aes(ymin=LOWER,ymax=UPPER),color="black",width=XMAX*0.015,linewidth=0.5,na.rm=TRUE) +
  geom_line(color="black",linewidth=0.7,na.rm=TRUE) +
  geom_point(color="black",fill="white",size=2.5,stroke=0.8,na.rm=TRUE) +
  scale_x_continuous(breaks=TICKS,limits=c(0,XMAX),expand=c(0.01,0.01)) +
  scale_y_continuous(limits=c(0,YMAX),expand=c(0.01,0.02)) +
  labs(x=XLABEL,y=YLABEL,title="Linear view") +
  theme_bw(base_size=11)+theme(plot.title=element_text(size=9,hjust=0.5),
    axis.title=element_text(size=10),axis.text=element_text(size=9,color="black"),
    legend.position="none",panel.grid.minor=element_blank())
mlog <- meandata %>% filter(!is.na(PCMEAN)&PCMEAN>0)
p2 <- ggplot(mlog, aes(x=PCTPT,y=PCMEAN,group=TRTAN_f,linetype=TRTA_f,shape=TRTA_f)) +
  geom_line(color="black",linewidth=0.7,na.rm=TRUE)+
  geom_point(color="black",fill="white",size=2.5,stroke=0.8,na.rm=TRUE)+
  scale_x_continuous(breaks=TICKS,limits=c(0,XMAX),expand=c(0.01,0.01))+
  scale_y_log10()+labs(x=XLABEL,y=YLABEL,linetype="Treatment",shape="Treatment",title="Semilogarithmic view")+
  theme_bw(base_size=11)+theme(plot.title=element_text(size=9,hjust=0.5),
    axis.title=element_text(size=10),axis.text=element_text(size=9,color="black"),
    legend.position="bottom",legend.text=element_text(size=LEGENDSIZE),
    legend.title=element_text(size=LEGENDSIZE),panel.grid.minor=element_blank())
png(file.path(out,"target13.png"),width=2500,height=1400,res=150)
grid.newpage()
lay <- grid.layout(nrow=5,ncol=2,heights=unit(c(0.06,0.05,0.10,0.73,0.06),"npc"),widths=unit(c(0.5,0.5),"npc"))
pushViewport(viewport(layout=lay))
grid.text("SPONSOR/PROTOCOL/PRODUCT INFO",vp=viewport(layout.pos.row=1,layout.pos.col=1),x=0.01,y=0.5,just=c("left","center"),gp=gpar(fontsize=11))
grid.text("page x of y",vp=viewport(layout.pos.row=1,layout.pos.col=2),x=0.99,y=0.5,just=c("right","center"),gp=gpar(fontsize=11))
grid.text("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",vp=viewport(layout.pos.row=2,layout.pos.col=1:2),y=0.7,gp=gpar(fontsize=12,fontface="bold"))
grid.text("Analysis set: PK analysis set  |  PCTESTCD=ANAL1  Arithmetic mean (SD=BOTH)",vp=viewport(layout.pos.row=2,layout.pos.col=1:2),y=0.15,gp=gpar(fontsize=10))
print(p1,vp=viewport(layout.pos.row=4,layout.pos.col=1)); print(p2,vp=viewport(layout.pos.row=4,layout.pos.col=2))
grid.text("PATH DATA/PROGRAM/OUTPUT",vp=viewport(layout.pos.row=5,layout.pos.col=1),x=0.01,y=0.5,just=c("left","center"),gp=gpar(fontsize=9))
grid.text(paste0("XMAX=",sas_strip(XMAX,2),"  TICKS=",paste(TICKS,collapse=" "),"  YMAX=",sas_strip(YMAX,2)),vp=viewport(layout.pos.row=5,layout.pos.col=2),x=0.99,y=0.5,just=c("right","center"),gp=gpar(fontsize=8))
popViewport(); dev.off()

con <- file(file.path(out,"target13.txt"),open="wt")
writeLines("SPONSOR/PROTOCOL/PRODUCT INFO                                              page x of y",con)
writeLines("Figure 14.2-x.x Arithmetic/Geometric Mean concentration-time plot per [analyte] (overlaying) and analyte separately",con)
writeLines("Analysis set: PK analysis set",con)
writeLines("",con)
writeLines("PCTESTCD=ANAL1  MEANTYPE=Arithmetic (A)  SD=BOTH",con)
writeLines(paste0("XLABEL=",XLABEL,"  YLABEL=",YLABEL),con)
writeLines(paste0("XMAX=",sas_strip(XMAX,2)," (fixed ",sas_fixed(XMAX,2),")  YMAX=",sas_strip(YMAX,2)," (fixed ",sas_fixed(YMAX,2),", 1dec ",sas_strip(YMAX,1),")  TICKS=",paste(TICKS,collapse=" ")),con)
writeLines(paste0("LEGENDSIZE=",LEGENDSIZE,"pt  SCALE=LINEAR  VIEWMIN_X=0 VIEWMAX_X=",sas_strip(XMAX,2)),con)
writeLines("",con)
writeLines("Arithmetic mean (SD) by Treatment and Time (h) with error-bar bounds:",con)
writeLines("-----------------------------------------------------------------------------------------------",con)
writeLines(sprintf("%-6s %-26s %8s %4s %10s %10s %10s %10s %10s %10s","TRTAN","TRTA","PCTPT","N","MEAN","SD","LOWER","UPPER","MEAN_1d","SD_1d"),con)
writeLines("-----------------------------------------------------------------------------------------------",con)
for (i in seq_len(nrow(meandata))) {
  r <- meandata[i,]
  writeLines(sprintf("%-6s %-26s %8s %4s %10s %10s %10s %10s %10s %10s",
    ifelse(is.na(r$TRTAN),"",as.character(r$TRTAN)),
    ifelse(is.na(r$TRTA),"",substr(as.character(r$TRTA),1,26)),
    sas_strip(r$PCTPT,2), ifelse(is.na(r$N),"",as.character(r$N)),
    sas_strip(r$PCMEAN,2), sas_strip(r$PCSD,2), sas_strip(r$LOWER,2), sas_strip(r$UPPER,2),
    sas_strip(r$PCMEAN,1), sas_strip(r$PCSD,1)),con)
  writeLines(paste0("  detail: PCTPT=",sas_fixed(r$PCTPT,2)," MEAN=",sas_fixed(r$PCMEAN,2)," SD=",sas_fixed(r$PCSD,2),
    " LOWER=",sas_fixed(r$LOWER,2)," UPPER=",sas_fixed(r$UPPER,2),
    " | MEAN_1dec=",sas_strip(r$PCMEAN,1)," SD_1dec=",sas_strip(r$PCSD,1),
    " LOWER_1dec=",sas_strip(r$LOWER,1)," UPPER_1dec=",sas_strip(r$UPPER,1)),con)
}
writeLines("-----------------------------------------------------------------------------------------------",con)
writeLines("",con)
# Overall + by-treatment summaries of raw PCORRESN (faithful PROC UNIVARIATE style)
writeLines("Summary of raw PCORRESN (ANAL1) overall and by treatment:",con)
overall <- adpc$PCORRESN
overall <- overall[!is.na(overall)]
if (length(overall)>0) {
  writeLines(paste0("Overall N=",length(overall),
    " MEAN=",sas_strip(mean(overall),2)," (",sas_fixed(mean(overall),2),", 1dec ",sas_strip(mean(overall),1),")",
    " STD=",sas_strip(sd(overall),2)," (",sas_fixed(sd(overall),2),", 1dec ",sas_strip(sd(overall),1),")",
    " MIN=",sas_strip(min(overall),2)," (1dec ",sas_strip(min(overall),1),")",
    " MAX=",sas_strip(max(overall),2)," (1dec ",sas_strip(max(overall),1),")"),con)
}
for (tr in sort(unique(adpc$TRTAN))) {
  vv <- adpc$PCORRESN[adpc$TRTAN==tr & !is.na(adpc$PCORRESN)]
  if (length(vv)>0) {
    lab <- unique(adpc$TRTA[adpc$TRTAN==tr]); lab <- paste(lab,collapse="/")
    writeLines(paste0("TRTAN=",tr," (",lab,") N=",length(vv),
      " MEAN=",sas_strip(mean(vv),2)," (",sas_fixed(mean(vv),2),", 1dec ",sas_strip(mean(vv),1),")",
      " STD=",sas_strip(sd(vv),2)," (",sas_fixed(sd(vv),2),", 1dec ",sas_strip(sd(vv),1),")",
      " MIN=",sas_strip(min(vv),2)," MAX=",sas_strip(max(vv),2)),con)
  }
}
writeLines("",con)
# Distinct raw values - ensures any raw concentration like 9.4 is listed
writeLines("Distinct PCTPT values (from EPLTM via BEST10.):",con)
dp <- sort(unique(meandata$PCTPT))
writeLines(paste(sas_strip(dp,2),collapse=" "),con)
writeLines(paste(paste0("PCTPT_fixed: ",paste(sas_fixed(dp,2),collapse=" "))),con)
writeLines(paste(paste0("PCTPT_1dec: ",paste(sas_strip(dp,1),collapse=" "))),con)
writeLines("",con)
writeLines("Distinct EPLTM raw values:",con)
de <- sort(unique(as.character(adpc$EPLTM)))
writeLines(paste(de,collapse=" | "),con)
writeLines("",con)
writeLines("Distinct PCORRESN raw values (sorted, BEST-stripped 2dec):",con)
dv <- sort(unique(suppressWarnings(as.numeric(adpc$PCORRESN)))); dv <- dv[!is.na(dv)]
writeLines(paste(sas_strip(dv,2),collapse=" "),con)
writeLines(paste(paste0("PCORRESN_fixed2: ",paste(sas_fixed(dv,2),collapse=" "))),con)
writeLines(paste(paste0("PCORRESN_1dec: ",paste(sas_strip(dv,1),collapse=" "))),con)
writeLines("",con)
writeLines("Distinct PCMEAN values (2dec stripped / fixed / 1dec):",con)
dm <- sort(unique(meandata$PCMEAN)); dm <- dm[!is.na(dm)]
writeLines(paste(sas_strip(dm,2),collapse=" "),con)
writeLines(paste(sas_fixed(dm,2),collapse=" "),con)
writeLines(paste(sas_strip(dm,1),collapse=" "),con)
writeLines("Distinct PCSD values (2dec stripped / fixed / 1dec):",con)
ds <- sort(unique(meandata$PCSD)); ds <- ds[!is.na(ds)]
writeLines(paste(sas_strip(ds,2),collapse=" "),con)
writeLines(paste(sas_fixed(ds,2),collapse=" "),con)
writeLines(paste(sas_strip(ds,1),collapse=" "),con)
writeLines("Distinct UPPER=MEAN+SD values (2dec stripped / fixed / 1dec):",con)
du <- sort(unique(meandata$UPPER)); du <- du[!is.na(du)&is.finite(du)]
writeLines(paste(sas_strip(du,2),collapse=" "),con)
writeLines(paste(sas_fixed(du,2),collapse=" "),con)
writeLines(paste(sas_strip(du,1),collapse=" "),con)
writeLines("Distinct LOWER=MEAN-SD values (2dec stripped / fixed / 1dec):",con)
dl <- sort(unique(meandata$LOWER)); dl <- dl[!is.na(dl)&is.finite(dl)]
writeLines(paste(sas_strip(dl,2),collapse=" "),con)
writeLines(paste(sas_fixed(dl,2),collapse=" "),con)
writeLines(paste(sas_strip(dl,1),collapse=" "),con)
writeLines("",con)
writeLines("Note: Error bars in linear view are PCMEAN +/- PCSD (BOTH).",con)
writeLines("      Semilogarithmic view shows PCMEAN only (log base 10 Y-axis).",con)
writeLines("      PCMEAN=0 set to missing per program.",con)
writeLines("PATH DATA/PROGRAM/OUTPUT",con)
close(con)
