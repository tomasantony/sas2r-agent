```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

round_half_up <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  posneg * z / 10^digits
}
fmt2 <- function(x) sprintf("%.2f", round_half_up(x, 2))
fmt4 <- function(x) sprintf("%.4f", round_half_up(x, 4))

## ---------------- Demo data (SAS streaminit(123) + RAND Normal) ----------------
## SAS calls RAND only in Day<35 and Day>=68 blocks; mimic call count
set.seed(123)
norms <- rnorm(67)
Demo <- data.frame(Day = 1:100)
Demo$Block <- ifelse(Demo$Day < 35, "Concave Up:/New Cases/Increasing  ",
              ifelse(Demo$Day < 68, "Linear:/New Cases/Constant        ",
                                          "Concave Down:/New Cases/Decreasing"))
n1 <- 0L; n2 <- 0L
NewCases_raw <- numeric(100)
for (i in 1:100) {
  Day <- Demo$Day[i]
  if (Day < 35) {
    n1 <- n1 + 1L
    NewCases_raw[i] <- Day + norms[n1]
  } else if (Day < 68) {
    NewCases_raw[i] <- 30 + (Day %% 2)
  } else {
    n2 <- n2 + 1L
    NewCases_raw[i] <- 100 - Day + norms[34 + n2]
  }
}
NewCases2 <- floor(0.49 * NewCases_raw)
NewCases2[NewCases2 < 0] <- 0
Demo$NewCases <- NewCases2
Demo$Cumul <- cumsum(Demo$NewCases)

## ---------------- Datalines ----------------
Day <- 1:100
Uni_New <- c(9,5,8,8,5,13,11,12,9,3,13,5,12,14,11,11,11,10,12,14,11,11,13,12,14,13,15,11,12,11,5,12,5,9,10,12,11,11,11,8,15,9,9,16,13,8,13,9,5,12,11,16,9,7,9,7,12,7,7,12,11,9,5,9,12,6,14,15,9,7,10,7,9,8,12,10,7,10,11,12,4,12,12,16,13,12,7,6,14,12,7,7,5,11,4,12,6,9,11,8)
Uni_Cum <- c(9,14,22,30,35,48,59,71,80,83,96,101,113,127,138,149,160,170,182,196,207,218,231,243,257,270,285,296,308,319,324,336,341,350,360,372,383,394,405,413,428,437,446,462,475,483,496,505,510,522,533,549,558,565,574,581,593,600,607,619,630,639,644,653,665,671,685,700,709,716,726,733,742,750,762,772,779,789,800,812,816,828,840,856,869,881,888,894,908,920,927,934,939,950,954,966,972,981,992,1000)
Peak_New <- c(8,13,18,24,24,28,31,31,35,37,37,37,33,37,32,32,33,33,29,27,26,27,23,24,20,20,21,13,16,17,16,14,12,13,15,10,11,11,9,7,7,7,7,8,9,5,2,3,3,4,3,5,3,3,4,0,3,0,2,0,0,4,2,0,0,0,0,0,0,0,1,0,0,1,0,0,1,2,0,0,0,0,0,0,0,1,1,1,0,0,0,0,1,1,1,0,0,1,0,0,0)
Peak_Cum <- c(8,21,39,63,87,115,146,177,212,249,286,323,356,393,425,457,490,523,552,579,605,632,655,679,699,719,740,753,769,786,802,816,828,841,856,866,877,888,897,904,911,918,925,933,942,947,949,952,955,959,962,967,970,973,977,977,980,980,982,982,982,986,988,988,988,988,988,988,988,988,989,989,989,990,990,990,991,993,993,993,993,993,993,993,993,994,995,996,996,996,996,996,997,998,999,999,999,1000,1000,1000,1000)
Flat_New <- c(3,5,7,11,10,13,16,16,20,23,24,25,21,27,22,23,26,26,24,23,23,24,21,23,20,21,22,15,19,20,19,17,16,17,20,14,16,16,14,12,12,12,12,13,14,10,7,8,8,9,10,13,11,10,8,4,10,3,6,0,3,7,7,3,4,0,2,2,3,3,7,2,0,3,0,3,4,5,2,0,0,0,0,0,0,3,3,4,0,0,1,0,4,3,2,0,0,2,1,2,1)
Flat_Cum <- c(3,8,15,26,36,49,65,81,101,124,148,173,194,221,243,266,292,318,342,365,388,412,433,456,476,497,519,534,553,573,592,609,625,642,662,676,692,708,722,734,746,758,770,783,797,807,814,822,830,839,849,862,873,883,891,895,905,908,914,914,917,924,931,934,938,938,940,942,945,948,955,957,957,960,960,963,967,972,974,974,974,974,974,974,977,980,984,984,984,985,985,989,992,994,994,994,996,997,999,1000)
Out_New <- c(4,5,5,8,6,9,12,13,17,20,22,25,23,29,27,29,33,34,32,32,32,33,30,32,28,27,27,19,21,21,19,16,14,13,14,9,9,8,6,4,3,3,3,3,5,1,0,0,0,1,2,5,3,3,1,0,4,0,2,0,1,6,8,7,9,7,11,13,15,15,19,13,8,13,6,8,10,10,3,1,0,0,0,0,0,2,1,2,0,0,0,0,2,1,1,0,0,1,0,1,0)
Out_Cum <- c(4,9,14,22,28,37,49,62,79,99,121,146,169,198,225,254,287,321,353,385,417,450,480,512,540,567,594,613,634,655,674,690,704,717,731,740,749,757,763,767,770,773,776,779,784,785,785,785,785,786,788,793,796,799,800,800,804,804,806,806,807,813,821,828,837,844,855,868,883,898,917,930,938,951,957,965,975,985,988,989,989,989,989,989,989,991,992,994,994,994,994,994,996,997,998,998,998,999,999,1000,1000)
Exp_New <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,1,0,1,0,0,1,0,0,1,1,0,0,1,2,0,1,1,1,1,1,1,2,2,3,2,1,1,2,2,3,4,4,4,3,2,5,3,4,2,3,5,5,4,5,4,6,6,7,8,11,10,9,12,11,13,16,17,16,17,17,19,19,22,25,26,28,29,31,34,34,40,42,45,46,50,55,58,63,66)
Exp_Cum <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,2,3,3,3,3,4,4,5,5,5,6,6,6,7,8,8,8,9,11,11,12,13,14,15,16,17,19,21,24,26,27,28,30,32,35,39,43,47,50,52,57,60,64,66,69,74,79,83,88,92,98,104,111,119,130,140,149,161,172,185,201,218,234,251,268,287,306,328,353,379,407,436,467,501,535,575,617,662,708,758,813,871,934,1000)

Uni <- data.frame(Day=Day, NewCases=Uni_New, Cumul=Uni_Cum, Type="Constant", stringsAsFactors=FALSE)
Peak <- data.frame(Day=Day, NewCases=Peak_New, Cumul=Peak_Cum, Type="Early Peak", stringsAsFactors=FALSE)
Flatten <- data.frame(Day=Day, NewCases=Flat_New, Cumul=Flat_Cum, Type="Flattened", stringsAsFactors=FALSE)
Outbreak2 <- data.frame(Day=Day, NewCases=Out_New, Cumul=Out_Cum, Type="Second Outbreak", stringsAsFactors=FALSE)
Exp <- data.frame(Day=Day, NewCases=Exp_New, Cumul=Exp_Cum, Type="Exponential", stringsAsFactors=FALSE)

type_order <- c("Constant","Early Peak","Flattened","Second Outbreak","Exponential")
Infection <- bind_rows(Uni, Peak, Flatten, Outbreak2, Exp)
Infection$Type <- factor(Infection$Type, levels=type_order)
stopifnot(all(c("Type","NewCases","Cumul","Day") %in% names(Infection)))
stopifnot(nrow(Infection)==500)

## ---------------- PROC MEANS ----------------
means_df <- Infection %>%
  group_by(Type) %>%
  summarise(N=sum(!is.na(NewCases)),
            Mean=mean(NewCases, na.rm=TRUE),
            StdDev=sd(NewCases, na.rm=TRUE),
            Min=min(NewCases, na.rm=TRUE),
            Max=max(NewCases, na.rm=TRUE),
            Sum=sum(NewCases, na.rm=TRUE), .groups="drop") %>%
  arrange(match(as.character(Type), type_order))
means_df$Type <- as.character(means_df$Type)

write.csv(means_df, file.path(out, "P099.csv"), row.names=FALSE, na="")

txt_path <- file.path(out, "P099.txt")
lst_path <- file.path(out, "P099.lst")
con <- file(txt_path, open="wt")
writeLines("Total Cases", con)
writeLines("", con)
writeLines("The MEANS Procedure", con)
writeLines("", con)
hdr <- sprintf("%-16s %8s %12s %12s %8s %8s %8s", "Type", "N", "Mean", "Std Dev", "Minimum", "Maximum", "Sum")
writeLines(hdr, con)
for (i in seq_len(nrow(means_df))) {
  r <- means_df[i,]
  line <- sprintf("%-16s %8d %12s %12s %8d %8d %8d",
                  r$Type, r$N, fmt2(r$Mean), fmt2(r$StdDev), r$Min, r$Max, r$Sum)
  writeLines(line, con)
}
writeLines("", con)
writeLines(sprintf("N total rows: %d", nrow(Infection)), con)
close(con)
file.copy(txt_path, lst_path, overwrite=TRUE)

## ---------------- Figures ----------------
## SGPlot.png : Demo cumulative with blocks
p_demo <- ggplot(Demo, aes(x=Day, y=Cumul)) +
  annotate("rect", xmin=1, xmax=34.5, ymin=-Inf, ymax=Inf, fill="#FF9999", alpha=0.2) +
  annotate("rect", xmin=34.5, xmax=67.5, ymin=-Inf, ymax=Inf, fill="#FFFF99", alpha=0.2) +
  annotate("rect", xmin=67.5, xmax=100, ymin=-Inf, ymax=Inf, fill="#99FF99", alpha=0.2) +
  annotate("text", x=17, y=max(Demo$Cumul)*0.95, label="Concave Up:\nNew Cases\nIncreasing", size=4) +
  annotate("text", x=51, y=max(Demo$Cumul)*0.95, label="Linear:\nNew Cases\nConstant", size=4) +
  annotate("text", x=84, y=max(Demo$Cumul)*0.95, label="Concave Down:\nNew Cases\nDecreasing", size=4) +
  geom_line(linewidth=1.2, color="black") +
  labs(title="Cumulative Cases versus Time", y="Cumulative Cases", x="Day") +
  theme_bw() + theme(plot.title=element_text(hjust=0.5))
ggsave(file.path(out, "sgplot.png"), p_demo, width=6.4, height=4.8, dpi=100)
write.csv(Demo[,c("Day","Block","NewCases","Cumul")], file.path(out, "sgplot.csv"), row.names=FALSE, na="")

## SGPlot1.png : Outbreak2 needle New Cases
p_needle <- ggplot(Outbreak2, aes(x=Day, y=NewCases)) +
  geom_segment(aes(xend=Day, yend=0), linewidth=0.6) +
  labs(title="New Cases", x="Day", y=NULL) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggsave(file.path(out, "sgplot1.png"), p_needle, width=3, height=2, dpi=100)
write.csv(Outbreak2[,c("Day","NewCases")], file.path(out, "sgplot1.csv"), row.names=FALSE, na="")

## SGPlot2.png : Outbreak2 series Total Cases
p_total <- ggplot(Outbreak2, aes(x=Day, y=Cumul)) +
  geom_line() +
  labs(title="Total Cases", x="Day", y=NULL) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5), axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggsave(file.path(out, "sgplot2.png"), p_total, width=3, height=2, dpi=100)
write.csv(Outbreak2[,c("Day","Cumul")], file.path(out, "sgplot2.csv"), row.names=FALSE, na="")

## SGPanel.png : New Cases versus Time needle panel
p_panel <- ggplot(Infection, aes(x=Day, y=NewCases)) +
  geom_segment(aes(xend=Day, yend=0), linewidth=0.4) +
  facet_grid(Type ~ ., scales="free_y") +
  labs(title="New Cases versus Time", x="Day", y="New Cases") +
  scale_x_continuous(breaks=seq(0,100,10), limits=c(0,100)) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5), panel.grid.major=element_line())
ggsave(file.path(out, "sgpanel.png"), p_panel, width=5, height=6, dpi=100)
write.csv(Infection[,c("Type","Day","NewCases")], file.path(out, "sgpanel.csv"), row.names=FALSE, na="")

## SGPanel1.png : Cumulative Cases versus Time series panel
p_panel1 <- ggplot(Infection, aes(x=Day, y=Cumul)) +
  geom_line() +
  facet_grid(Type ~ ., scales="free_y") +
  labs(title="Cumulative Cases versus Time", x="Day", y="Cumulative Cases") +
  scale_x_continuous(breaks=seq(0,100,10), limits=c(0,100)) +
  scale_y_continuous(limits=c(0,NA)) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5), panel.grid.major=element_line(), axis.title.x=element_blank())
ggsave(file.path(out, "sgpanel1.png"), p_panel1, width=5, height=6, dpi=100)
write.csv(Infection[,c("Type","Day","Cumul")], file.path(out, "sgpanel1.csv"), row.names=FALSE, na="")

## SGPanel2.png : log(Cumulative Cases) versus Time
InfPos <- Infection %>% filter(Cumul > 0)
p_panel2 <- ggplot(InfPos, aes(x=Day, y=Cumul)) +
  geom_line() +
  facet_grid(Type ~ ., scales="free_y") +
  scale_y_log10() +
  labs(title="log(Cumulative Cases) versus Time", x="Day", y="Cumulative Cases") +
  scale_x_continuous(breaks=seq(0,100,10), limits=c(0,100)) +
  theme_bw() + theme(plot.title=element_text(hjust=0.5), panel.grid.major=element_line(), axis.title.x=element_blank())
ggsave(file.path(out, "sgpanel2.png"), p_panel2, width=5, height=6, dpi=100)
write.csv(InfPos[,c("Type","Day","Cumul")], file.path(out, "sgpanel2.csv"), row.names=FALSE, na="")

## SGRender* : two-row lattice per Type (base graphics)
render_one <- function(df, title, pngfile, csvfile) {
  png(pngfile, width=500, height=360)
  par(mfrow=c(2,1), mar=c(2.5,4.5,2.5,1), oma=c(2,0,0,0))
  plot(df$Day, df$Cumul, type="l", xlim=c(0,100), xaxt="n",
       ylab="Cumulative Cases", xlab="", main=title, cex.main=0.9)
  axis(1, at=seq(0,100,10)); grid()
  plot(df$Day, df$NewCases, type="h", xlim=c(0,100), ylim=c(0,66),
       ylab="New Cases", xlab="Day")
  axis(1, at=seq(0,100,10)); grid()
  dev.off()
  write.csv(df[,c("Day","NewCases","Cumul")], csvfile, row.names=FALSE, na="")
}
render_one(Uni, "New Cases at a Constant Rate", file.path(out,"sgrender.png"), file.path(out,"sgrender.csv"))
render_one(Peak, "Rapid Growth; Gradual Decline", file.path(out,"sgrender1.png"), file.path(out,"sgrender1.csv"))
render_one(Flatten, "Gradual Growth; Gradual Decline", file.path(out,"sgrender2.png"), file.path(out,"sgrender2.csv"))
render_one(Outbreak2, "Secondary Outbreak", file.path(out,"sgrender3.png"), file.path(out,"sgrender3.csv"))
render_one(Exp, "Exponential Growth in New Cases", file.path(out,"sgrender4.png"), file.path(out,"sgrender4.csv"))
```