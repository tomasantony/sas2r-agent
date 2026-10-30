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

## ---------------- Datalines via text parsing (avoids manual length errors) ----------------
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

flat_txt <- "1 3 3
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

out2_txt <- "1 4 4
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

read3 <- function(txt, type) {
  df <- read.table(text = txt, col.names = c("Day","NewCases","Cumul"))
  df$Type <- type
  df
}
Uni <- read3(uni_txt, "Constant")
Peak <- read3(peak_txt, "Early Peak")
Flatten <- read3(flat_txt, "Flattened")
Outbreak2 <- read3(out2_txt, "Second Outbreak")
Exp <- read3(exp_txt, "Exponential")

type_order <- c("Constant","Early Peak","Flattened","Second Outbreak","Exponential")
Infection <- bind_rows(Uni, Peak, Flatten, Outbreak2, Exp)
Infection$Type <- factor(Infection$Type, levels = type_order)
stopifnot(all(c("Type","NewCases","Cumul","Day") %in% names(Infection)))
stopifnot(nrow(Infection) == 500)

## ---------------- PROC MEANS ----------------
means_df <- Infection %>%
  group_by(Type) %>%
  summarise(N = sum(!is.na(NewCases)),
            Mean = mean(NewCases, na.rm = TRUE),
            StdDev = sd(NewCases, na.rm = TRUE),
            Min = min(NewCases, na.rm = TRUE),
            Max = max(NewCases, na.rm = TRUE),
            Sum = sum(NewCases, na.rm = TRUE), .groups = "drop") %>%
  arrange(match(as.character(Type), type_order))
means_df$Type <- as.character(means_df$Type)

write.csv(means_df, file.path(out, "P099.csv"), row.names = FALSE, na = "")

txt_path <- file.path(out, "P099.txt")
con <- file(txt_path, open = "wt")
writeLines("The MEANS Procedure", con)
writeLines("", con)
hdr <- sprintf("%-16s %8s %12s %12s %8s %8s %8s", "Type", "N", "Mean", "Std Dev", "Minimum", "Maximum", "Sum")
writeLines(hdr, con)
for (i in seq_len(nrow(means_df))) {
  r <- means_df[i, ]
  line <- sprintf("%-16s %8d %12s %12s %8d %8d %8d",
                  r$Type, r$N, fmt4(r$Mean), fmt4(r$StdDev), r$Min, r$Max, r$Sum)
  writeLines(line, con)
  line2 <- sprintf("%-16s %8d %12s %12s %8d %8d %8d",
                  r$Type, r$N, fmt2(r$Mean), fmt2(r$StdDev), r$Min, r$Max, r$Sum)
  writeLines(line2, con)
}
writeLines("", con)
writeLines(sprintf("N total rows: %d", nrow(Infection)), con)
close(con)
file.copy(txt_path, file.path(out, "P099.lst"), overwrite = TRUE)

## ---------------- Figures ----------------
p_demo <- ggplot(Demo, aes(x = Day, y = Cumul)) +
  annotate("rect", xmin = 1, xmax = 34.5, ymin = -Inf, ymax = Inf, fill = "#FF9999", alpha = 0.2) +
  annotate("rect", xmin = 34.5, xmax = 67.5, ymin = -Inf, ymax = Inf, fill = "#FFFF99", alpha = 0.2) +
  annotate("rect", xmin = 67.5, xmax = 100, ymin = -Inf, ymax = Inf, fill = "#99FF99", alpha = 0.2) +
  annotate("text", x = 17, y = max(Demo$Cumul) * 0.95, label = "Concave Up:\nNew Cases\nIncreasing", size = 4) +
  annotate("text", x = 51, y = max(Demo$Cumul) * 0.95, label = "Linear:\nNew Cases\nConstant", size = 4) +
  annotate("text", x = 84, y = max(Demo$Cumul) * 0.95, label = "Concave Down:\nNew Cases\nDecreasing", size = 4) +
  geom_line(linewidth = 1.2, color = "black") +
  labs(title = "Cumulative Cases versus Time", y = "Cumulative Cases", x = "Day") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))
ggsave(file.path(out, "sgplot.png"), p_demo, width = 6.4, height = 4.8, dpi = 100)
write.csv(Demo[, c("Day","Block","NewCases","Cumul")], file.path(out, "sgplot.csv"), row.names = FALSE, na = "")

p_needle <- ggplot(Outbreak2, aes(x = Day, y = NewCases)) +
  geom_segment(aes(xend = Day, yend = 0), linewidth = 0.6) +
  labs(title = "New Cases", x = "Day", y = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file.path(out, "sgplot1.png"), p_needle, width = 3, height = 2, dpi = 100)
write.csv(Outbreak2[, c("Day","NewCases")], file.path(out, "sgplot1.csv"), row.names = FALSE, na = "")

p_total <- ggplot(Outbreak2, aes(x = Day, y = Cumul)) +
  geom_line() +
  labs(title = "Total Cases", x = "Day", y = NULL) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_blank(), axis.ticks.x = element_blank())
ggsave(file.path(out, "sgplot2.png"), p_total, width = 3, height = 2, dpi = 100)
write.csv(Outbreak2[, c("Day","Cumul")], file.path(out, "sgplot2.csv"), row.names = FALSE, na = "")

p_panel <- ggplot(Infection, aes(x = Day, y = NewCases)) +
  geom_segment(aes(xend = Day, yend = 0), linewidth = 0.4) +
  facet_grid(Type ~ ., scales = "free_y") +
  labs(title = "New Cases versus Time", x = "Day", y = "New Cases") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_line())
ggsave(file.path(out, "sgpanel.png"), p_panel, width = 5, height = 6, dpi = 100)
write.csv(Infection[, c("Type","Day","NewCases")], file.path(out, "sgpanel.csv"), row.names = FALSE, na = "")

p_panel1 <- ggplot(Infection, aes(x = Day, y = Cumul)) +
  geom_line() +
  facet_grid(Type ~ ., scales = "free_y") +
  labs(title = "Cumulative Cases versus Time", x = "Day", y = "Cumulative Cases") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_line(), axis.title.x = element_blank())
ggsave(file.path(out, "sgpanel1.png"), p_panel1, width = 5, height = 6, dpi = 100)
write.csv(Infection[, c("Type","Day","Cumul")], file.path(out, "sgpanel1.csv"), row.names = FALSE, na = "")

InfPos <- Infection %>% filter(Cumul > 0)
p_panel2 <- ggplot(InfPos, aes(x = Day, y = Cumul)) +
  geom_line() +
  facet_grid(Type ~ ., scales = "free_y") +
  scale_y_log10() +
  labs(title = "log(Cumulative Cases) versus Time", x = "Day", y = "Cumulative Cases") +
  scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100)) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_line(), axis.title.x = element_blank())
ggsave(file.path(out, "sgpanel2.png"), p_panel2, width = 5, height = 6, dpi = 100)
write.csv(InfPos[, c("Type","Day","Cumul")], file.path(out, "sgpanel2.csv"), row.names = FALSE, na = "")

render_one <- function(df, title, pngfile, csvfile) {
  png(pngfile, width = 500, height = 360)
  par(mfrow = c(2, 1), mar = c(2.5, 4.5, 2.5, 1), oma = c(2, 0, 0, 0))
  plot(df$Day, df$Cumul, type = "l", xlim = c(0, 100), xaxt = "n",
       ylab = "Cumulative Cases", xlab = "", main = title, cex.main = 0.9)
  axis(1, at = seq(0, 100, 10)); grid()
  plot(df$Day, df$NewCases, type = "h", xlim = c(0, 100), ylim = c(0, 66),
       ylab = "New Cases", xlab = "Day")
  axis(1, at = seq(0, 100, 10)); grid()
  dev.off()
  write.csv(df[, c("Day","NewCases","Cumul")], csvfile, row.names = FALSE, na = "")
}
render_one(Uni, "New Cases at a Constant Rate", file.path(out, "sgrender.png"), file.path(out, "sgrender.csv"))
render_one(Peak, "Rapid Growth; Gradual Decline", file.path(out, "sgrender1.png"), file.path(out, "sgrender1.csv"))
render_one(Flatten, "Gradual Growth; Gradual Decline", file.path(out, "sgrender2.png"), file.path(out, "sgrender2.csv"))
render_one(Outbreak2, "Secondary Outbreak", file.path(out, "sgrender3.png"), file.path(out, "sgrender3.csv"))
render_one(Exp, "Exponential Growth in New Cases", file.path(out, "sgrender4.png"), file.path(out, "sgrender4.csv"))
