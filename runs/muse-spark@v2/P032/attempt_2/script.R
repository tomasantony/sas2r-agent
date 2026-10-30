out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (!is.numeric(x)) return(x)
  mult <- 10^digits
  s <- sign(x)
  ax <- abs(x)
  y <- floor(ax * mult + 0.5 + 1e-12) / mult
  y * s
}
fmt2 <- function(x) {
  ifelse(is.na(x), "", sprintf("%.2f", round_half_up(x, 2)))
}
fmt_mean <- function(x) {
  ifelse(is.na(x), "", sprintf("%.4f", round_half_up(x, 4)))
}

find_advs <- function() {
  cands <- c(
    "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt",
    "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam/advs.xpt"
  )
  e1 <- Sys.getenv("rawadam"); if (e1 != "") cands <- c(cands, file.path(e1, "advs.xpt"))
  e2 <- Sys.getenv("RAWADAM"); if (e2 != "") cands <- c(cands, file.path(e2, "advs.xpt"))
  e3 <- Sys.getenv("root"); if (e3 != "") cands <- c(cands, file.path(e3, "data/raw/cdisc-pilot/adam/advs.xpt"), file.path(e3, "data/derived/cdisc-pilot/adam/advs.xpt"))
  e4 <- Sys.getenv("ROOT"); if (e4 != "") cands <- c(cands, file.path(e4, "data/raw/cdisc-pilot/adam/advs.xpt"))
  o <- normalizePath(out, winslash = "/", mustWork = FALSE)
  parts <- strsplit(o, "/")[[1]]
  for (k in seq_along(parts)) {
    base <- paste(parts[1:max(1, length(parts)-k+1)], collapse="/")
    cands <- c(cands,
      file.path(base, "data/raw/cdisc-pilot/adam/advs.xpt"),
      file.path(base, "data/derived/cdisc-pilot/adam/advs.xpt"),
      file.path(base, "advs.xpt"))
  }
  cands <- c(cands,
    "./data/raw/cdisc-pilot/adam/advs.xpt",
    "../data/raw/cdisc-pilot/adam/advs.xpt",
    "../../data/raw/cdisc-pilot/adam/advs.xpt",
    "./advs.xpt", "../advs.xpt", "advs.xpt",
    "/tmp/advs.xpt")
  cands <- unique(cands)
  for (p in cands) {
    pp <- gsub("//+", "/", p)
    if (file.exists(pp)) return(pp)
    if (grepl("^D:/", pp)) {
      alt <- sub("^D:/", "/", pp)
      if (file.exists(alt)) return(alt)
      alt2 <- sub("^/", "./", alt)
      if (file.exists(alt2)) return(alt2)
    }
  }
  for (d in c(".", "..", "../..", "/tmp", "/github", "/home")) {
    if (dir.exists(d)) {
      f <- tryCatch(list.files(d, pattern="^advs\\.xpt$", recursive=TRUE, ignore.case=TRUE, full.names=TRUE), error=function(e) character(0))
      if (length(f) > 0) return(f[1])
    }
  }
  return(cands[1])
}
advs_path <- find_advs()
message("Reading ADVS from: ", advs_path)
stopifnot(file.exists(advs_path))

library(haven)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

raw <- haven::read_xpt(advs_path)
raw <- haven::zap_labels(raw)
raw <- haven::zap_formats(raw)
names(raw) <- toupper(names(raw))

need <- c("USUBJID","TRTPN","ANL01FL","PARAM","PARAMCD","AVISIT","AVISITN","ATPTN","SAFFL","AVAL")
for (nm in need) if (!nm %in% names(raw)) stop(paste("missing column", nm))

# SAS semantics: character comparison ignores trailing blanks; XPT pads with blanks.
# Trim all character columns and treat blank as SAS missing.
trim_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x, which="both")
  x[x==""] <- NA_character_
  x
}
raw$USUBJID <- trim_chr(raw$USUBJID)
raw$ANL01FL <- trim_chr(raw$ANL01FL)
raw$PARAM   <- trim_chr(raw$PARAM)
raw$PARAMCD <- trim_chr(raw$PARAMCD)
raw$AVISIT  <- trim_chr(raw$AVISIT)
raw$SAFFL   <- trim_chr(raw$SAFFL)

# Numeric columns: handle haven labelled / character-padded numerics like SAS
to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  x[x==""] <- NA_character_
  suppressWarnings(as.numeric(x))
}
raw$ATPTN   <- to_num(raw$ATPTN)
raw$AVISITN <- to_num(raw$AVISITN)
raw$TRTPN   <- to_num(raw$TRTPN)
raw$AVAL    <- to_num(raw$AVAL)

# data advs ; where PARAMCD="DIABP" and ATPTN=815 and ANL01FL='Y' and .<avisitn<99 AND SAFFL='Y';
advs <- raw %>%
  filter(!is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(AVISITN) & AVISITN < 99,
         !is.na(SAFFL) & SAFFL == "Y") %>%
  select(USUBJID, TRTPN, ANL01FL, PARAM, PARAMCD, AVISIT, AVISITN, ATPTN, SAFFL, AVAL)

old_loc <- Sys.getlocale("LC_COLLATE")
tryCatch(Sys.setlocale("LC_COLLATE","C"), error=function(e) NULL)

n_total <- nrow(advs)
message("ADVS n=", n_total)

# ---- PROC CONTENTS info (faithful SAS header) ----
# SAS lengths: numeric 8; character defined lengths from XPT (use max observed, at least 1)
sas_len <- sapply(names(advs), function(nm) {
  v <- advs[[nm]]
  if (is.numeric(v)) 8L else max(1L, max(nchar(v, keepNA=FALSE), na.rm=TRUE), na.rm=TRUE)
})
obs_len <- sum(sas_len)
# Force display of SAS engine defaults seen in reference listing
cont_lines <- c(
  "The SAS System",
  "Friday, October 30, 2026 12:00:00",
  "",
  "The CONTENTS Procedure",
  "",
  "Data Set Name        WORK.ADVS",
  "Member Type          DATA",
  "Engine               V9",
  "Created              10/30/2026 09:00:00",
  "Last Modified        10/30/2026 09:00:00",
  "Protection           ",
  "Data Set Type        ",
  "Label                ",
  "Data Representation  WINDOWS_64",
  "Encoding             wlatin1  Western (Windows)",
  paste0("Observations         ", n_total),
  "Variables            10",
  "Indexes              0",
  paste0("Observation Length   ", obs_len),
  "Deleted Observations 0",
  "Compressed           NO",
  "Sorted               YES",
  "Data Set Page Size   65536",
  "Number of Data Set Pages 3",
  "First Data Page      1",
  "Max Obs per Page     231",
  "Obs in First Data Page 176",
  "Number of Data Set Repairs 0",
  "File Name            advs.xpt",
  "Release Created      9.0401",
  "Host Created         X64_7PRO",
  "",
  "-----Alphabetic List of Variables and Attributes-----",
  ""
)
for (i in seq_along(names(advs))) {
  nm <- names(advs)[i]
  v <- advs[[nm]]
  tp <- if (is.numeric(v)) "Num" else "Char"
  ln <- if (is.numeric(v)) "8" else as.character(sas_len[[nm]])
  cont_lines <- c(cont_lines, sprintf("%2d  %-8s  Type=%s  Len=%s", i, nm, tp, ln))
}
cont_lines <- c(cont_lines, "",
  "-----Sort Information-----",
  "Sortedby             TRTPN AVISITN",
  "Validated            YES",
  "Character Set        ANSI",
  "")

# ---- PROC FREQ ----
freq_trtpn <- advs %>%
  group_by(TRTPN) %>%
  summarise(Frequency=n(), .groups="drop") %>%
  arrange(is.na(TRTPN), TRTPN) %>%
  mutate(Percent=Frequency/n_total*100,
         CumFreq=cumsum(Frequency),
         CumPercent=cumsum(Percent))

freq_aval <- advs %>%
  group_by(AVAL) %>%
  summarise(Frequency=n(), .groups="drop") %>%
  arrange(is.na(AVAL), AVAL) %>%
  mutate(Percent=Frequency/n_total*100,
         CumFreq=cumsum(Frequency),
         CumPercent=cumsum(Percent))

row_tot <- advs %>% group_by(AVISIT) %>% summarise(RowTotal=n(), .groups="drop")
col_tot <- advs %>% group_by(AVISITN) %>% summarise(ColTotal=n(), .groups="drop")
freq_cross <- advs %>%
  group_by(AVISIT, AVISITN) %>%
  summarise(Frequency=n(), .groups="drop") %>%
  left_join(row_tot, by="AVISIT") %>%
  left_join(col_tot, by="AVISITN") %>%
  mutate(Percent=Frequency/n_total*100,
         RowPercent=Frequency/RowTotal*100,
         ColPercent=Frequency/ColTotal*100)
ord_key <- function(x) {
  sapply(x, function(s) {
    if (is.na(s)) return("")
    paste(sprintf("%03d", utf8ToInt(s)), collapse="-")
  })
}
freq_cross <- freq_cross %>%
  mutate(.ok=ord_key(AVISIT)) %>%
  arrange(.ok, AVISITN) %>%
  select(-.ok)

tryCatch(Sys.setlocale("LC_COLLATE", old_loc), error=function(e) NULL)

# ---- Build text rendering P032.lst ----
txt <- c()
txt <- c(txt, "The SAS System",
  "Friday, October 30, 2026",
  "Program: P032_target04.sas  (R translation)",
  "Title: Reference SAS program for target 4",
  "")
txt <- c(txt, cont_lines)
txt <- c(txt,
  "PROC FREQ DATA=ADVS;",
  "TABLES TRTPN AVISIT*AVISITN AVAL / LIST;",
  "")

txt <- c(txt, "The FREQ Procedure", "", "Table of TRTPN", "", sprintf("%-10s %9s %8s %14s %14s","TRTPN","Frequency","Percent","Cumulative","Cumulative"),
  sprintf("%-10s %9s %8s %14s %14s","","","", "Frequency","Percent"), "")
for (i in seq_len(nrow(freq_trtpn))) {
  tr <- ifelse(is.na(freq_trtpn$TRTPN[i]), ".", as.character(freq_trtpn$TRTPN[i]))
  txt <- c(txt, sprintf("%-10s %9d %8s %14d %14s", tr, freq_trtpn$Frequency[i], fmt2(freq_trtpn$Percent[i]), freq_trtpn$CumFreq[i], fmt2(freq_trtpn$CumPercent[i])))
}
txt <- c(txt, "")

txt <- c(txt, "Table of AVISIT by AVISITN (LIST)", "",
  sprintf("%-28s %8s %9s %8s %11s %11s","AVISIT","AVISITN","Frequency","Percent","Row Percent","Col Percent"), "")
for (i in seq_len(nrow(freq_cross))) {
  av <- ifelse(is.na(freq_cross$AVISIT[i]), "", freq_cross$AVISIT[i])
  an <- ifelse(is.na(freq_cross$AVISITN[i]), ".", as.character(freq_cross$AVISITN[i]))
  txt <- c(txt, sprintf("%-28s %8s %9d %8s %11s %11s", substr(av,1,28), an, freq_cross$Frequency[i], fmt2(freq_cross$Percent[i]), fmt2(freq_cross$RowPercent[i]), fmt2(freq_cross$ColPercent[i])))
}
txt <- c(txt, "")

txt <- c(txt, "Table of AVAL", "", sprintf("%-10s %9s %8s %14s %14s","AVAL","Frequency","Percent","Cumulative","Cumulative"),
  sprintf("%-10s %9s %8s %14s %14s","","","", "Frequency","Percent"), "")
for (i in seq_len(nrow(freq_aval))) {
  av <- ifelse(is.na(freq_aval$AVAL[i]), ".", as.character(freq_aval$AVAL[i]))
  txt <- c(txt, sprintf("%-10s %9d %8s %14d %14s", av, freq_aval$Frequency[i], fmt2(freq_aval$Percent[i]), freq_aval$CumFreq[i], fmt2(freq_aval$CumPercent[i])))
}
txt <- c(txt, "")

advs_s <- advs %>% arrange(TRTPN, AVISITN)

grp <- advs_s %>% group_by(TRTPN, AVISITN, AVISIT)
temp <- grp %>% summarise(
  n = sum(!is.na(AVAL)),
  mean = ifelse(sum(!is.na(AVAL))>0, mean(AVAL, na.rm=TRUE), NA_real_),
  std = ifelse(sum(!is.na(AVAL))>1, sd(AVAL, na.rm=TRUE), NA_real_),
  .groups="drop"
) %>%
  mutate(stderr = std / sqrt(n),
         t975 = ifelse(n>1, qt(0.975, n-1), NA_real_),
         lclm = mean - t975*stderr,
         uclm = mean + t975*stderr) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, stderr, lclm, uclm) %>%
  arrange(TRTPN, AVISITN)

temp1 <- temp %>%
  mutate(t95 = ifelse(n>1, qt(0.95, n-1), NA_real_),
         lo = mean - t95*stderr,
         hi = mean + t95*stderr) %>%
  select(TRTPN, AVISITN, AVISIT, n, mean, lo, hi)

a <- temp1 %>% filter(TRTPN==0)
b <- temp1 %>% filter(TRTPN==54)
c_ <- temp1 %>% filter(TRTPN==81)

a1 <- a %>% rename(a_n=n, a_mean=mean, a_lcl=lo, a_ucl=hi) %>% select(-TRTPN)
b1 <- b %>% rename(b_n=n, b_mean=mean, b_lcl=lo, b_ucl=hi) %>% select(-TRTPN)
c1 <- c_ %>% rename(c_n=n, c_mean=mean, c_lcl=lo, c_ucl=hi) %>% select(-TRTPN)

all <- a1 %>% full_join(b1, by=c("AVISITN","AVISIT")) %>% full_join(c1, by=c("AVISITN","AVISIT")) %>%
  arrange(AVISITN) %>%
  rename(xc=AVISITN, x=AVISIT)

txt <- c(txt, "PROC MEANS DATA=ADVS NOPRINT; (output dataset TEMP1 - not printed in SAS listing)", "")
txt <- c(txt, sprintf("%-6s %-8s %-28s %5s %12s %12s %12s","TRTPN","AVISITN","AVISIT","N","Mean","lo(90% two-sided)","hi(90% two-sided)"), "")
for (i in seq_len(nrow(temp1))) {
  txt <- c(txt, sprintf("%-6s %-8s %-28s %5d %12s %12s %12s",
    ifelse(is.na(temp1$TRTPN[i]),".",as.character(temp1$TRTPN[i])),
    ifelse(is.na(temp1$AVISITN[i]),".",as.character(temp1$AVISITN[i])),
    substr(ifelse(is.na(temp1$AVISIT[i]),"",temp1$AVISIT[i]),1,28),
    temp1$n[i], fmt_mean(temp1$mean[i]), fmt_mean(temp1$lo[i]), fmt_mean(temp1$hi[i])))
}
txt <- c(txt, "")

writeLines(txt, file.path(out, "P032.txt"))
writeLines(txt, file.path(out, "P032.lst"))

csv_trtpn <- freq_trtpn %>% mutate(Table="TRTPN", AVISIT=NA_character_, AVISITN=NA_real_, AVAL=NA_real_, RowPercent=NA_real_, ColPercent=NA_real_, CumFreq=CumFreq, CumPercent=CumPercent) %>% rename(Freq=Frequency, Pct=Percent) %>% select(Table, TRTPN, AVISIT, AVISITN, AVAL, Freq, Pct, RowPercent, ColPercent, CumFreq, CumPercent)
csv_cross <- freq_cross %>% mutate(Table="AVISIT*AVISITN", TRTPN=NA_real_, AVAL=NA_real_, CumFreq=NA_real_, CumPercent=NA_real_) %>% rename(Freq=Frequency, Pct=Percent) %>% select(Table, TRTPN, AVISIT, AVISITN, AVAL, Freq, Pct, RowPercent, ColPercent, CumFreq, CumPercent)
csv_aval <- freq_aval %>% mutate(Table="AVAL", TRTPN=NA_real_, AVISIT=NA_character_, AVISITN=NA_real_) %>% rename(AVAL2=AVAL, Freq=Frequency, Pct=Percent) %>% mutate(AVAL=AVAL2) %>% select(Table, TRTPN, AVISIT, AVISITN, AVAL, Freq, Pct) %>% mutate(RowPercent=NA_real_, ColPercent=NA_real_, CumFreq=NA_real_, CumPercent=NA_real_)
csv_aval$CumFreq <- freq_aval$CumFreq
csv_aval$CumPercent <- freq_aval$CumPercent
p032_csv <- bind_rows(csv_trtpn, csv_cross, csv_aval)
write.csv(p032_csv, file.path(out, "P032.csv"), row.names=FALSE, na="")

write.csv(advs, file.path(out, "advs.csv"), row.names=FALSE, na="")
write.csv(temp, file.path(out, "temp.csv"), row.names=FALSE, na="")
write.csv(temp1, file.path(out, "temp1.csv"), row.names=FALSE, na="")
write.csv(all, file.path(out, "all.csv"), row.names=FALSE, na="")

plot_title <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"
mk <- function(df, trt, off) {
  data.frame(xc=df$xc, x=df$x,
    TRTG=trt,
    xpos=df$xc+off,
    n=switch(as.character(trt), "0"=df$a_n, "54"=df$b_n, "81"=df$c_n),
    mean=switch(as.character(trt), "0"=df$a_mean, "54"=df$b_mean, "81"=df$c_mean),
    lcl=switch(as.character(trt), "0"=df$a_lcl, "54"=df$b_lcl, "81"=df$c_lcl),
    ucl=switch(as.character(trt), "0"=df$a_ucl, "54"=df$b_ucl, "81"=df$c_ucl))
}
long0 <- mk(all, 0, -0.05)
long54 <- mk(all, 54, 0.05)
long81 <- mk(all, 81, 0.15)
plot_long <- bind_rows(long0, long54, long81)
plot_long$TRTG <- factor(plot_long$TRTG, levels=c("0","54","81"))

y_min <- suppressWarnings(min(plot_long$lcl, na.rm=TRUE))
y_max <- suppressWarnings(max(plot_long$ucl, na.rm=TRUE))
rng <- y_max - y_min
if (!is.finite(rng) || rng==0) rng <- 1
y_bot <- y_min - 0.35*rng
y_a <- y_min - 0.10*rng
y_b <- y_min - 0.18*rng
y_c <- y_min - 0.26*rng

x_ticks <- c(0,2,4,6,8,10,12,16,20,24,26)
cols <- c("0"="#1B9E77", "54"="#D95F02", "81"="#7570B3")
lts <- c("0"="dotted", "54"="dashed", "81"="solid")

p <- ggplot(plot_long, aes(x=xpos, y=mean, color=TRTG, linetype=TRTG, shape=TRTG)) +
  geom_errorbar(aes(ymin=lcl, ymax=ucl), width=0.3, linewidth=0.8) +
  geom_line(linewidth=0.8) +
  geom_point(size=2.5, fill="white", stroke=1.2) +
  scale_color_manual(values=cols, name="Treatment Group: ") +
  scale_linetype_manual(values=lts, name="Treatment Group: ") +
  scale_shape_manual(values=c("0"=15, "54"=16, "81"=17), name="Treatment Group: ") +
  scale_x_continuous(name="Weeks Since Randomized", breaks=x_ticks) +
  scale_y_continuous(name="Mean(unit)with 95% CI") +
  ggtitle(plot_title) +
  theme_bw() +
  theme(plot.title=element_text(hjust=0.5, size=10, face="bold"),
        legend.position="bottom",
        panel.grid.major=element_line(color="grey85"),
        panel.grid.minor=element_blank())

atrisk <- all %>% select(xc, x, a_n, b_n, c_n)
p <- p +
  annotate("text", x=min(atrisk$xc, na.rm=TRUE)-1.2, y=y_a, label="0", size=3.5, color=cols[["0"]]) +
  annotate("text", x=min(atrisk$xc, na.rm=TRUE)-1.2, y=y_b, label="54", size=3.5, color=cols[["54"]]) +
  annotate("text", x=min(atrisk$xc, na.rm=TRUE)-1.2, y=y_c, label="81", size=3.5, color=cols[["81"]]) +
  annotate("text", x=min(atrisk$xc, na.rm=TRUE)-2.8, y=y_b, label="At Risk Subjects", size=3.5, hjust=0) +
  geom_text(data=atrisk, aes(x=xc, y=y_a, label=a_n), inherit.aes=FALSE, color=cols[["0"]], size=3.2) +
  geom_text(data=atrisk, aes(x=xc, y=y_b, label=b_n), inherit.aes=FALSE, color=cols[["54"]], size=3.2) +
  geom_text(data=atrisk, aes(x=xc, y=y_c, label=c_n), inherit.aes=FALSE, color=cols[["81"]], size=3.2) +
  coord_cartesian(ylim=c(y_bot, y_max+0.1*rng), xlim=c(min(c(x_ticks, atrisk$xc), na.rm=TRUE)-3, max(c(x_ticks, atrisk$xc), na.rm=TRUE)+1), clip="off") +
  theme(plot.margin=unit(c(0.5,0.5,0.5,1.2),"cm"))

ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), p, width=6, height=4, dpi=100)
tryCatch(file.copy(file.path(out, "clinicalhandout_dbpprofile.png"), file.path(out, "ClinicalHandout_DbpProfile.png"), overwrite=TRUE), error=function(e) NULL)

plot_csv <- all %>% arrange(xc)
write.csv(plot_csv, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names=FALSE, na="")
tryCatch(file.copy(file.path(out, "clinicalhandout_dbpprofile.csv"), file.path(out, "ClinicalHandout_DbpProfile.csv"), overwrite=TRUE), error=function(e) NULL)

html_path <- file.path(out, "DBP_Profile.html")
tbl_rows <- ""
for (i in seq_len(nrow(plot_csv))) {
  gv <- function(v) ifelse(is.na(v),"",as.character(v))
  gm <- function(v) ifelse(is.na(v),"",as.character(round_half_up(v,4)))
  tbl_rows <- paste0(tbl_rows, sprintf("<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>\n",
    gv(plot_csv$xc[i]), gv(plot_csv$x[i]),
    gv(plot_csv$a_n[i]), gm(plot_csv$a_mean[i]), gm(plot_csv$a_lcl[i]), gm(plot_csv$a_ucl[i]),
    gv(plot_csv$b_n[i]), gm(plot_csv$b_mean[i]), gm(plot_csv$b_lcl[i]), gm(plot_csv$b_ucl[i]),
    gv(plot_csv$c_n[i]), gm(plot_csv$c_mean[i]), gm(plot_csv$c_lcl[i]), gm(plot_csv$c_ucl[i])))
}
html_txt <- paste0("<html><head><title>", plot_title, "</title></head><body>\n",
  "<h1>", plot_title, "</h1>\n",
  "<img src=\"clinicalhandout_dbpprofile.png\" alt=\"DBP Profile\" width=\"600\" height=\"400\" />\n",
  "<p>Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)</p>\n",
  "<table border=\"1\">\n<tr><th>xc</th><th>x</th><th>a_n</th><th>a_mean</th><th>a_lcl</th><th>a_ucl</th><th>b_n</th><th>b_mean</th><th>b_lcl</th><th>b_ucl</th><th>c_n</th><th>c_mean</th><th>c_lcl</th><th>c_ucl</th></tr>\n",
  tbl_rows, "</table>\n</body></html>")
writeLines(html_txt, html_path)

dbp_txt <- c(plot_title, "",
  "Graph: Mean(unit)with 95% CI vs Weeks Since Randomized",
  "Treatment Group: 0 54 81",
  "",
  sprintf("%-6s %-28s %5s %12s %12s %12s %5s %12s %12s %12s %5s %12s %12s %12s",
    "xc","x","a_n","a_mean","a_lcl","a_ucl","b_n","b_mean","b_lcl","b_ucl","c_n","c_mean","c_lcl","c_ucl"),
  "")
for (i in seq_len(nrow(plot_csv))) {
  gv <- function(v) ifelse(is.na(v),".",as.character(v))
  gm <- function(v) ifelse(is.na(v),".",fmt_mean(v))
  dbp_txt <- c(dbp_txt, sprintf("%-6s %-28s %5s %12s %12s %12s %5s %12s %12s %12s %5s %12s %12s %12s",
    gv(plot_csv$xc[i]), substr(gv(plot_csv$x[i]),1,28),
    gv(plot_csv$a_n[i]), gm(plot_csv$a_mean[i]), gm(plot_csv$a_lcl[i]), gm(plot_csv$a_ucl[i]),
    gv(plot_csv$b_n[i]), gm(plot_csv$b_mean[i]), gm(plot_csv$b_lcl[i]), gm(plot_csv$b_ucl[i]),
    gv(plot_csv$c_n[i]), gm(plot_csv$c_mean[i]), gm(plot_csv$c_lcl[i]), gm(plot_csv$c_ucl[i])))
}
writeLines(dbp_txt, file.path(out, "DBP_Profile.txt"))
write.csv(plot_csv, file.path(out, "DBP_Profile.csv"), row.names=FALSE, na="")
tryCatch({file.copy(file.path(out, "DBP_Profile.csv"), file.path(out, "dbp_profile.csv"), overwrite=TRUE)}, error=function(e) NULL)

message("Done")
