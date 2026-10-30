```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

# ---- test dataset (unused downstream, as in SAS) ----
test <- data.frame(
  USUBJID = as.character(1:9),
  TRT  = c("PL","PL","PL","T1","T1","T1","T2","T2","T2"),
  Base = c(10,30,40,60,30,22,39,49,12),
  Post = c(30,20,30,50,22,12,38,44,60),
  ULN = 55, LLN = 15, stringsAsFactors = FALSE
)

# ---- read ADLBC from adam-split ----
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split"
f_sas <- file.path(adam_dir, "adlbc.sas7bdat")
f_xpt <- file.path(adam_dir, "adlbc.xpt")
if (file.exists(f_sas)) {
  adlbc <- read_sas(f_sas)
} else if (file.exists(f_xpt)) {
  adlbc <- read_xpt(f_xpt)
} else {
  cand <- list.files(adam_dir, pattern = "^adlbc\\.(sas7bdat|xpt)$",
                     ignore.case = TRUE, full.names = TRUE)
  if (length(cand) == 0) stop("ADLBC not found in ", adam_dir)
  adlbc <- if (grepl("xpt$", cand[1], ignore.case = TRUE)) read_xpt(cand[1]) else read_sas(cand[1])
}
names(adlbc) <- toupper(names(adlbc))
cat("ADLBC read:", nrow(adlbc), "rows\n")

adlbc <- adlbc %>% filter(PARAMCD == "SODIUM")

start <- adlbc %>%
  filter(AENTMTFL == "Y", is.na(AVISITN) | AVISITN != 99) %>%
  select(any_of(c("USUBJID","TRTAN","TRTA","PARAMCD","PARAM","AVAL","BASE","A1LO","A1HI")))
start <- as.data.frame(lapply(start, function(x) { attributes(x) <- NULL; x }),
                       stringsAsFactors = FALSE)

# ---- PROC FREQ + sort descending count + first.paramcd ----
pick_mode <- function(df, var) {
  fr <- df %>% filter(!is.na(.data[[var]]), !is.na(PARAMCD)) %>%
    count(PARAMCD, val = .data[[var]], name = "COUNT") %>%
    arrange(PARAMCD, val)                       # PROC FREQ output order
  fr <- fr[order(fr$PARAMCD, -fr$COUNT, method = "radix"), ]  # stable sort
  fr <- fr[!duplicated(fr$PARAMCD), ]
  fr$val[nrow(fr)]                              # symputx: last value wins
}
LLN <- pick_mode(start, "A1LO")
ULN <- pick_mode(start, "A1HI")
cat("LLN:", LLN, "\n")
cat("ULN:", ULN, "\n")

# ---- plot data ----
plotdat <- start %>% select(USUBJID, TRTA, BASE, AVAL) %>%
  mutate(LLN = LLN, ULN = ULN)
grp_levels <- unique(plotdat$TRTA[!is.na(plotdat$TRTA)])
plotdat$TRTA_F <- factor(plotdat$TRTA, levels = grp_levels)

write.csv(start, file.path(out, "start.csv"), row.names = FALSE, na = "")
write.csv(plotdat %>% select(-TRTA_F), file.path(out, "sgrender.csv"),
          row.names = FALSE, na = "")

# axis ranges (data + reference lines), padded; diagonal drawn wall corner to corner
pad <- function(r) { d <- diff(r); if (d == 0) d <- 1; c(r[1] - 0.05 * d, r[2] + 0.05 * d) }
xr <- pad(range(c(plotdat$BASE, LLN, ULN), na.rm = TRUE))
yr <- pad(range(c(plotdat$AVAL, LLN, ULN), na.rm = TRUE))

cols <- c("#FFA53D", "#F7DF54", "#EF6B48", "#61A6E7", "#CB79C8", "#85CE79",
          "#967CD0", "#DD6475", "#9DAA2D", "#6C69D9", "#2EA64A", "#C17D35")
shps <- rep(c(3, 16, 17), length.out = length(grp_levels))
names(shps) <- grp_levels
colv <- rep(cols, length.out = length(grp_levels)); names(colv) <- grp_levels

p <- ggplot(plotdat %>% filter(!is.na(BASE), !is.na(AVAL)),
            aes(x = BASE, y = AVAL, colour = TRTA_F, shape = TRTA_F)) +
  annotate("segment", x = xr[1], y = yr[1], xend = xr[2], yend = yr[2],
           colour = "grey40") +
  geom_hline(yintercept = ULN, colour = "#0000FF", linetype = "dashed") +
  geom_hline(yintercept = LLN, colour = "#FF0000", linetype = "dashed") +
  geom_vline(xintercept = LLN, colour = "#FF0000", linetype = "dashed") +
  geom_vline(xintercept = ULN, colour = "#0000FF", linetype = "dashed") +
  annotate("text", x = xr[1], y = ULN, label = "ULN", hjust = 0, vjust = -0.3, colour = "#0000FF") +
  annotate("text", x = xr[1], y = LLN, label = "LLN", hjust = 0, vjust = -0.3, colour = "#FF0000") +
  annotate("text", x = LLN, y = yr[1], label = "LLN", hjust = -0.1, vjust = 0, colour = "#FF0000") +
  annotate("text", x = ULN, y = yr[1], label = "ULN", hjust = -0.1, vjust = 0, colour = "#0000FF") +
  geom_point(size = 2) +
  scale_colour_manual(values = colv, name = "Treatment") +
  scale_shape_manual(values = shps, name = "Treatment") +
  coord_cartesian(xlim = xr, ylim = yr, expand = FALSE) +
  labs(title = "Figure 11.1 Scatterplot", caption = "LLN ULN ....",
       x = "Min. Baseline", y = "Min. Post Baseline") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        panel.background = element_rect(fill = "white"))

ggsave(file.path(out, "sgrender.png"), p, width = 951 / 96, height = 689 / 96, dpi = 96)
cat("Done.\n")
```