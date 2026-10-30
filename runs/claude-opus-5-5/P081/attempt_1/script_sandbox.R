# P081: Figure 14.2.4 - Mean Change from Baseline Over Time
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adam_dir <- "/bench/data/raw/own-synthetic/adam"

read_ds <- function(dir, name) {
  f <- list.files(dir, full.names = TRUE)
  b <- tolower(basename(f))
  hit <- f[b == paste0(name, ".sas7bdat")]
  if (length(hit)) return(haven::read_sas(hit[1]))
  hit <- f[b == paste0(name, ".xpt")]
  if (length(hit)) return(haven::read_xpt(hit[1]))
  stop("Dataset not found: ", name)
}

adeff <- read_ds(adam_dir, "adeff")
adsl  <- read_ds(adam_dir, "adsl")
names(adeff) <- toupper(names(adeff))
names(adsl)  <- toupper(names(adsl))

eff_param <- "ACTOT"
y_label   <- "Change from Baseline in ADAS-Cog Total Score"

# helper: SAS treats "" as missing for character
chr <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }

# ---- Step 1: analysis data (PROC SQL inner join) ----
adsl_e <- adsl %>% filter(chr(EFFFL) == "Y") %>% select(USUBJID)
eff <- adeff %>%
  inner_join(adsl_e, by = "USUBJID") %>%
  filter(chr(PARAMCD) == eff_param, chr(ANL01FL) == "Y", !is.na(CHG)) %>%
  select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, AVISIT, AVISITN,
         AVAL, BASE, CHG, ANL01FL) %>%
  mutate(TRTA = chr(TRTA), AVISIT = chr(AVISIT))
eff <- eff[order(eff$TRTAN, eff$AVISITN, method = "radix"), ]
cat("eff rows:", nrow(eff), "\n")

# ---- Step 2: PROC MEANS NWAY (missing class values excluded) ----
eff_c <- eff %>% filter(TRTA != "", !is.na(TRTAN), AVISIT != "", !is.na(AVISITN))
stats <- eff_c %>%
  group_by(TRTA, TRTAN, AVISIT, AVISITN) %>%
  summarise(n = sum(!is.na(CHG)),
            mean = mean(CHG),
            sd = if (n() > 1) sd(CHG) else NA_real_,
            .groups = "drop") %>%
  mutate(se  = sd / sqrt(n),
         tq  = ifelse(n > 1, qt(0.975, n - 1), NA_real_),
         lcl = mean - tq * se,
         ucl = mean + tq * se) %>%
  select(TRTA, TRTAN, AVISIT, AVISITN, n, mean, se, lcl, ucl) %>%
  as.data.frame()
stats <- stats[order(stats$TRTA, stats$TRTAN, stats$AVISIT, stats$AVISITN,
                     method = "radix"), ]

# ---- Step 3: x offset ----
plot_data <- stats %>%
  mutate(xpos = case_when(TRTAN == 1 ~ AVISITN - 0.15,
                          TRTAN == 2 ~ AVISITN,
                          TRTAN == 3 ~ AVISITN + 0.15,
                          TRUE ~ AVISITN))
write.csv(plot_data, file.path(out, "f_mean_cfb.csv"), row.names = FALSE, na = "")

# ---- Step 4: figure ----
trt_levels <- unique(plot_data$TRTA[order(plot_data$TRTA, method = "radix")])
pd <- plot_data %>% mutate(TRTA = factor(TRTA, levels = trt_levels))
pd_line <- pd[order(pd$TRTA, pd$xpos, method = "radix"), ]

p <- ggplot(pd_line, aes(x = xpos, colour = TRTA, group = TRTA)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "gray", linewidth = 0.4) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.05, linewidth = 0.5, na.rm = TRUE) +
  geom_line(aes(y = mean), linewidth = 0.8) +
  geom_point(aes(y = mean), size = 2.5) +
  scale_x_continuous(breaks = function(l) unique(floor(pretty(l)))) +
  labs(x = "Visit (Week)", y = y_label, colour = NULL,
       title = "Figure 14.2.4\nMean Change from Baseline Over Time\nEfficacy Analysis Set",
       caption = paste("Data cutoff: [DDMONYYYY]",
                       "Error bars represent 95% confidence intervals.",
                       "n = number of subjects with non-missing change from baseline.",
                       sep = "\n")) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0),
        panel.grid.major.y = element_line(linetype = "dotted", colour = "lightgray"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) +
  guides(colour = guide_legend(nrow = 1))
ggsave(file.path(out, "f_mean_cfb.png"), p, width = 10, height = 6.5, dpi = 150)

# ---- Step 5: supporting n table (PROC TABULATE) ----
ntab <- plot_data %>%
  group_by(TRTA, AVISIT) %>%
  summarise(n_Sum = sum(n), .groups = "drop") %>%
  as.data.frame()
ntab <- ntab[order(ntab$TRTA, ntab$AVISIT, method = "radix"), ]
write.csv(ntab, file.path(out, "f_mean_cfb_ntable.csv"), row.names = FALSE, na = "")

lines <- c("n per visit", "")
for (t in unique(ntab$TRTA)) {
  sub <- ntab[ntab$TRTA == t, ]
  lines <- c(lines, paste0("TRTA: ", t),
             paste(sprintf("%-20s", sub$AVISIT), sprintf("%10.2f", sub$n_Sum)), "")
}
writeLines(lines, file.path(out, "f_mean_cfb.txt"))

cat("NOTE: f_mean_cfb completed successfully.\n")
