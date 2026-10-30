```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

rhu2 <- function(x, d=2) {
  # round half away from zero and format
  fmt <- paste0("%.", d, "f")
  # use sprintf (C rounding half away)
  sprintf(fmt, x)
}

## ---------- Demo data (SAS streaminit 123 + rand Normal) ----------
set.seed(123)
Day_v <- 1:100
Block_v <- character(100)
NewCases_v <- numeric(100)
Cumul_v <- numeric(100)
cum <- 0
for (d in 1:100) {
  if (d < 35) {
    Block_v[d] <- "Concave Up:/New Cases/Increasing  "
    nc <- d + rnorm(1)
  } else if (d < 68) {
    Block_v[d] <- "Linear:/New Cases/Constant        "
    nc <- 30 + (d %% 2)
  } else {
    Block_v[d] <- "Concave Down:/New Cases/Decreasing"
    nc <- 100 - d + rnorm(1)
  }
  nc <- floor(0.49 * nc)
  if (nc < 0) nc <- 0
  NewCases_v[d] <- nc
  cum <- cum + nc
  Cumul_v[d] <- cum
}
Demo <- data.frame(Day=Day_v, Block=Block_v, NewCases=NewCases_v, Cumul=Cumul_v, stringsAsFactors=FALSE)

## ---------- Datalines ----------
uni_txt <- "1 9 9
2 5 14
3 8 22
4 8 30
5 5 35
6 13 48
7 11 59
8 12 71
9 9 80
10 3 83
11 13 96
12 5 101
13 12 113
14 14 127
15 11 138
16 11 149
17 11 160
18 10 170
19 12 182
20 14 196
21 11 207
22 11 218
23 13 231
24 12 243
25 14 257
26 13 270
27 15 285
28 11 296
29 12 308
30 11 319
31 5 324
32 12 336
33 5 341
34 9 350
35 10 360
36 12 372
37 11 383
38 11 394
39 11 405
40 8 413
41 15 428
42 9 437
43 9 446
44 16 462
45 13 475
46 8 483
47 13 496
48 9 505
49 5 510
50 12 522
51 11 533
52 16 549
53 9 558
54 7 565
55 9 574
56 7 581
57 12 593
58 7 600
59 7 607
60 12 619
61 11 630
62 9 639
63 5 644
64 9 653
65 12 665
66 6 671
67 14 685
68 15 700
69 9 709
70 7 716
71 10 726
72 7 733
73 9 742
74 8 750
75 12 762
76 10 772
77 7 779
78 10 789
79 11 800
80 12 812
81 4 816
82 12 828
83 12 840
84 16 856
85 13 869
86 12 881
87 7 888
88 6 894
89 14 908
90 12 920
91 7 927
92 7 934
93 5 939
94 11 950
95 4 954
96 12 966
97 6 972
98 9 981
99 11 992
100 8 1000"

peak_txt <- "1 8 8
2 13 21
3 18 39
4 24 63
5 24 87
6 28 115
7 31 146
8 31 177
9 35 212
10 37 249
11 37 286
12 37 323
13 33 356
14 37 393
15 32 425
16 32 457
17 33 490
18 33 523
19 29 552
20 27 579
21 26 605
22 27 632
23 23 655
24 24 679
25 20 699
26 20 719
27 21 740
28 13 753
29 16 769
30 17 786
31 16 802
32 14 816
33 12 828
34 13 841
35 15 856
36 10 866
37 11 877
38 11 888
39 9 897
40 7 904
41 7 911
42 7 918
43 7 925
44 8 933
45 9 942
46 5 947
47 2 949
48 3 952
49 3 955
50 4 959
51 3 962
52 5 967
53 3 970
54 3 973
55 4 977
56 0 977
57 3 980
58 0 980
59 2 982
60 0 982
61 0 982
62 4 986
63 2 988
64 0 988
65 0 988
66 0 988
67 0 988
68 0 988
69 0 988
70 0 988
71 1 989
72 0 989
73 0 989
74 1 990
75 0 990
76 0 990
77 1 991
78 2 993
79 0 993
80 0 993
81 0 993
82 0 993
83 0 993
84 0 993
85 1 994
86 1 995
87 1 996
88 0 996
89 0 996
90 0 996
91 0 996
92 1 997
93 1 998
94 1 999
95 0 999
96 0 999
97 1 1000
98 0 1000
99 0 1000
100 0 1000"

flatten_txt <- "1 3 3
2 5 8
3 7 15
4 11 26
5 10 36
6 13 49
7 16 65
8 16 81
9 20 101
10 23 124
11 24 148
12 25 173
13 21 194
14 27 221
15 22 243
16 23 266
17 26 292
18 26 318
19 24 342
20 23 365
21 23 388
22 24 412
23 21 433
24 23 456
25 20 476
26 21 497
27 22 519
28 15 534
29 19 553
30 20 573
31 19 592
32 17 609
33 16 625
34 17 642
35 20 662
36 14 676
37 16 692
38 16 708
39 14 722
40 12 734
41 12 746
42 12 758
43 12 770
44 13 783
45 14 797
46 10 807
47 7 814
48 8 822
49 8 830
50 9 839
51 10 849
52 13 862
53 11 873
54 10 883
55 8 891
56 4 895
57 10 905
58 3 908
59 6 914
60 0 914
61 3 917
62 7 924
63 7 931
64 3 934
65 4 938
66 0 938
67 2 940
68 2 942
69 3 945
70 3 948
71 7 955
72 2 957
73 0 957
74 3 960
75 0 960
76 3 963
77 4 967
78 5 972
79 2 974
80 0 974
81 0 974
82 0 974
83 0 974
84 0 974
85 3 977
86 3 980
87 4 984
88 0 984
89 0 984
90 1 985
91 0 985
92 4 989
93 3 992
94 2 994
95 0 994
96 0 994
97 2 996
98 1 997
99 2 999
100 1 1000"

outbreak2_txt <- "1 4 4
2 5 9
3 5 14
4 8 22
5 6 28
6 9 37
7 12 49
8 13 62
9 17 79
10 20 99
11 22 121
12 25 146
13 23 169
14 29 198
15 27 225
16 29 254
17 33 287
18 34 321
19 32 353
20 32 385
21 32 417
22 33 450
23 30 480
24 32 512
25 28 540
26 27 567
27 27 594
28 19 613
29 21 634
30 21 655
31 19 674
32 16 690
33 14 704
34 13 717
35 14 731
36 9 740
37 9 749
38 8 757
39 6 763
40 4 767
41 3 770
42 3 773
43 3 776
44 3 779
45 5 784
46 1 785
47 0 785
48 0 785
49 0 785
50 1 786
51 2 788
52 5 793
53 3 796
54 3 799
55 1 800
56 0 800
57 4 804
58 0 804
59 2 806
60 0 806
61 1 807
62 6 813
63 8 821
64 7 828
65 9 837
66 7 844
67 11 855
68 13 868
69 15 883
70 15 898
71 19 917
72 13 930
73 8 938
74 13 951
75 6 957
76 8 965
77 10 975
78 10 985
79 3 988
80 1 989
81 0 989
82 0 989
83 0 989
84 0 989
85 2 991
86 1 992
87 2 994
88 0 994
89 0 994
90 0 994
91 0 994
92 2 996
93 1 997
94 1 998
95 0 998
96 0 998
97 1 999
98 0 999
99 1 1000
100 0 1000"

exp_txt <- "1 0 0
2 0 0
3 0 0
4 0 0
5 0 0
6 0 0
7 0 0
8 0 0
9 0 0
10 0 0
11 0 0
12 0 0
13 0 0
14 1 1
15 0 1
16 0 1
17 1 2
18 1 3
19 0 3
20 0 3
21 0 3
22 1 4
23 0 4
24 1 5
25 0 5
26 0 5
27 1 6
28 0 6
29 0 6
30 1 7
31 1 8
32 0 8
33 0 8
34 1 9
35 2 11
36 0 11
37 1 12
38 1 13
39 1 14
40 1 15
41 1 16
42 1 17
43 2 19
44 2 21
45 3 24
46 2 26
47 1 27
48 1 28
49 2 30
50 2 32
51 3 35
52 4 39
53 4 43
54 4 47
55 3 50
56 2 52
57 5 57
58 3 60
59 4 64
60 2 66
61 3 69
62 5 74
63 5 79
64 4 83
65 5 88
66 4 92
67 6 98
68 6 104
69 7 111
70 8 119
71 11 130
72 10 140
73 9 149
74 12 161
75 11 172
76 13 185
77 16 201
78 17 218
79 16 234
80 17 251
81 17 268
82 19 287
83 19 306
84 22 328
85 25 353
86 26 379
87 28 407
88 29 436
89 31 467
90 34 501
91 34 535
92 40 575
93 42 617
94 45 662
95 46 708
96 50 758
97 55 813
98 58 871
99 63 934
100 66 1000"

read_tri <- function(txt, type) {
  df <- read.table(text=txt, col.names=c("Day","NewCases","Cumul"))
  df$Type <- type
  df[, c("Type","Day","NewCases","Cumul")]
}
Uni <- read_tri(uni_txt, "Constant")
Peak <- read_tri(peak_txt, "Early Peak")
Flatten <- read_tri(flatten_txt, "Flattened")
Outbreak2 <- read_tri(outbreak2_txt, "Second Outbreak")
Exp <- read_tri(exp_txt, "Exponential")

Infection <- rbind(Uni, Peak, Flatten, Outbreak2, Exp)
type_order <- c("Constant","Early Peak","Flattened","Second Outbreak","Exponential")
Infection$Type <- factor(Infection$Type, levels=type_order)

## ---------- PROC MEANS: Mean StdDev Min Max Sum (+N) by Type order=data ----------
means_list <- lapply(type_order, function(t) {
  v <- Infection$NewCases[Infection$Type==t]
  data.frame(Type=t, N=length(v), Mean=mean(v),
             StdDev=ifelse(length(v)>1, sd(v), NA_real_),
             Min=min(v), Max=max(v), Sum=sum(v), stringsAsFactors=FALSE)
})
means_df <- do.call(rbind, means_list)

# CSV of table body (numeric unrounded)
write.csv(means_df, file.path(out, "P099.csv"), row.names=FALSE, na="")

# Text rendering P099.lst / P099.txt (30 numbers)
# SAS MAXDEC=2 formatting
hdr <- sprintf("%-15s %5s %10s %10s %10s %10s %10s", "Type", "N", "Mean", "StdDev", "Min", "Max", "Sum")
lines <- c(
  "Total Cases",
  "The MEANS Procedure",
  "",
  "Analysis Variable : NewCases  New Cases",
  "",
  hdr
)
for (i in seq_len(nrow(means_df))) {
  r <- means_df[i,]
  lines <- c(lines, sprintf("%-15s %5d %10s %10s %10s %10s %10s",
    r$Type, r$N, rhu2(r$Mean,2), rhu2(r$StdDev,2), rhu2(r$Min,2), rhu2(r$Max,2), rhu2(r$Sum,2)))
}
lines <- c(lines, "",
  "N = 100 for each Type; Mean StdDev Min Max Sum as in PROC MEANS with CLASS Type / ORDER=DATA")
writeLines(lines, file.path(out, "P099.lst"))
writeLines(lines, file.path(out, "P099.txt"))

## ---------- Figures ----------
# sgplot.png : Demo cumulative with blocks, title Cumulative Cases versus Time
maxC <- max(Demo$Cumul)
ymax <- maxC * 1.22
p_demo <- ggplot(Demo, aes(x=Day, y=Cumul)) +
  annotate("rect", xmin=0.5, xmax=34.5, ymin=-Inf, ymax=Inf, fill="mistyrose", alpha=0.6) +
  annotate("rect", xmin=34.5, xmax=67.5, ymin=-Inf, ymax=Inf, fill="lightyellow", alpha=0.6) +
  annotate("rect", xmin=67.5, xmax=100.5, ymin=-Inf, ymax=Inf, fill="lightgreen", alpha=0.6) +
  geom_line(linewidth=1.2, color="black") +
  annotate("text", x=17.5, y=ymax*0.97, label="Concave Up:\nNew Cases\nIncreasing", size=4) +
  annotate("text", x=51, y=ymax*0.97, label="Linear:\nNew Cases\nConstant", size=4) +
  annotate("text", x=84, y=ymax*0.97, label="Concave Down:\nNew Cases\nDecreasing", size=4) +
  labs(title="Cumulative Cases versus Time", x="Day", y="Cumulative Cases") +
  coord_cartesian(ylim=c(0, ymax)) +
  theme_bw()
ggsave(file.path(out, "sgplot.png"), p_demo, width=8, height=6, dpi=100)
write.csv(Demo[, c("Day","Block","NewCases","Cumul")], file.path(out, "sgplot.csv"), row.names=FALSE, na="")

# sgplot1.png : New Cases needle for Outbreak2 (300x200)
p_needle_small <- ggplot(Outbreak2, aes(x=Day, y=NewCases)) +
  geom_segment(aes(xend=Day, yend=0), color="darkblue") +
  labs(title="New Cases", x="Day", y=NULL) +
  scale_x_continuous(limits=c(1,100)) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggsave(file.path(out, "sgplot1.png"), p_needle_small, width=3, height=2, dpi=100)
write.csv(Outbreak2[, c("Day","NewCases")], file.path(out, "sgplot1.csv"), row.names=FALSE, na="")

# sgplot2.png : Total Cases series for Outbreak2
p_series_small <- ggplot(Outbreak2, aes(x=Day, y=Cumul)) +
  geom_line(color="darkblue") +
  labs(title="Total Cases", x="Day", y=NULL) +
  scale_x_continuous(limits=c(1,100)) +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())
ggsave(file.path(out, "sgplot2.png"), p_series_small, width=3, height=2, dpi=100)
write.csv(Outbreak2[, c("Day","Cumul")], file.path(out, "sgplot2.csv"), row.names=FALSE, na="")

# sgpanel.png : New Cases versus Time panelby Type needle
p_panel_new <- ggplot(Infection, aes(x=Day, y=NewCases)) +
  geom_segment(aes(xend=Day, yend=0), color="darkblue") +
  facet_grid(Type ~ ., scales="fixed") +
  labs(title="New Cases versus Time", x="Day", y="New Cases") +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  theme_bw() + theme(panel.grid.major=element_line(color="grey80"))
ggsave(file.path(out, "sgpanel.png"), p_panel_new, width=5, height=6, dpi=100)
write.csv(Infection[, c("Type","Day","NewCases")], file.path(out, "sgpanel.csv"), row.names=FALSE, na="")

# sgpanel1.png : Cumulative Cases versus Time
p_panel_cum <- ggplot(Infection, aes(x=Day, y=Cumul)) +
  geom_line(color="darkblue") +
  facet_grid(Type ~ ., scales="fixed") +
  labs(title="Cumulative Cases versus Time", x=NULL, y="Cumulative Cases") +
  scale_y_continuous(limits=c(0, NA)) +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  theme_bw() + theme(panel.grid.major=element_line(color="grey80"))
ggsave(file.path(out, "sgpanel1.png"), p_panel_cum, width=5, height=6, dpi=100)
write.csv(Infection[, c("Type","Day","Cumul")], file.path(out, "sgpanel1.csv"), row.names=FALSE, na="")

# sgpanel2.png : log(Cumulative) versus Time where cumul>0
InfPos <- Infection[Infection$Cumul > 0, ]
p_panel_log <- ggplot(InfPos, aes(x=Day, y=Cumul)) +
  geom_line(color="darkblue") +
  facet_grid(Type ~ ., scales="fixed") +
  labs(title="log(Cumulative Cases) versus Time", x=NULL, y="Cumulative Cases (log scale)") +
  scale_y_log10() +
  scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
  theme_bw() + theme(panel.grid.major=element_line(color="grey80"))
ggsave(file.path(out, "sgpanel2.png"), p_panel_log, width=5, height=6, dpi=100)
write.csv(InfPos[, c("Type","Day","Cumul")], file.path(out, "sgpanel2.csv"), row.names=FALSE, na="")

# sgrender figures: Epidemic template (2 rows: cumulative series + needle)
make_epidemic <- function(df, ttl) {
  cum_df <- data.frame(Day=df$Day, y=df$Cumul, Panel="Cumulative Cases")
  new_df <- data.frame(Day=df$Day, y=df$NewCases, Panel="New Cases")
  # force New Cases viewmax 66
  blank_df <- data.frame(Day=c(0,100), y=c(0,66), Panel="New Cases")
  cum_blank <- data.frame(Day=c(0), y=c(0), Panel="Cumulative Cases")
  p <- ggplot() +
    geom_line(data=cum_df, aes(x=Day, y=y), color="darkblue") +
    geom_segment(data=new_df, aes(x=Day, y=y, xend=Day, yend=0), color="darkblue") +
    geom_blank(data=blank_df, aes(x=Day, y=y)) +
    geom_blank(data=cum_blank, aes(x=Day, y=y)) +
    facet_grid(Panel ~ ., scales="free_y") +
    labs(title=ttl, x="Day", y=NULL) +
    scale_x_continuous(limits=c(0,100), breaks=seq(0,100,10)) +
    theme_bw() + theme(panel.grid.major=element_line(color="grey80"))
  p
}
render_specs <- list(
  list(df=Uni, title="New Cases at a Constant Rate", file="sgrender.png", csv="sgrender.csv"),
  list(df=Peak, title="Rapid Growth; Gradual Decline", file="sgrender1.png", csv="sgrender1.csv"),
  list(df=Flatten, title="Gradual Growth; Gradual Decline", file="sgrender2.png", csv="sgrender2.csv"),
  list(df=Outbreak2, title="Secondary Outbreak", file="sgrender3.png", csv="sgrender3.csv"),
  list(df=Exp, title="Exponential Growth in New Cases", file="sgrender4.png", csv="sgrender4.csv")
)
for (sp in render_specs) {
  pp <- make_epidemic(sp$df, sp$title)
  ggsave(file.path(out, sp$file), pp, width=5, height=3.6, dpi=100)
  write.csv(sp$df[, c("Day","NewCases","Cumul")], file.path(out, sp$csv), row.names=FALSE, na="")
}
```