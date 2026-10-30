```r
# P082 - Figure 14.2.3 Waterfall Plot of Best Percentage Change from Baseline
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

adampath <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_ds <- function(name) {
  cands <- c(file.path(adampath, paste0(name, ".sas7bdat")),
             file.path(adampath, paste0(toupper(name), ".sas7bdat")),
             file.path(adampath, paste0(name, ".xpt")),
             file.path(adampath, paste0(toupper(name), ".xpt")))
  f <- cands[file.exists(cands)][1]
  if (is.na(f)) stop("Dataset not found: ", name)
  d <- if (grepl("\\.xpt$", f, ignore.case = TRUE)) read_xpt(f) else read_sas(f)
  names(d) <- toupper(names(d))
  d <- zap_labels(zap_formats(d))
  d
}

cat("Reading ADTR / ADSL\n")
adtr <- read_ds("adtr")
adsl <- read_ds("adsl")

# Step 1: PROC SQL inner join, where EFFFL='Y' and PARAMCD='BPCHG', order by PCHG
a <- adtr %>% select(USUBJID, TRTA, TRTAN, PARAMCD, PARAM, PCHG, AVALC) %>%
  mutate(.ra = row_number())
b <- adsl %>% select(USUBJID, EFFFL)
tumor <- a %>%
  inner_join(b, by = "USUBJID", relationship = "many-to-many") %>%
  filter(!is.na(EFFFL) & EFFFL == "Y", !is.na(PARAMCD) & PARAMCD == "BPCHG") %>%
  # SAS: missing values sort first
  arrange(!is.na(PCHG), PCHG, .ra) %>%
  select(-.ra)

# Step 2: display order, colors, capping
waterfall <- tumor %>%
  mutate(
    bar_order = row_number(),
    bar_color = case_when(
      toupper(AVALC) == "CR" ~ "darkgreen",
      toupper(AVALC) == "PR" ~ "green",
      toupper(AVALC) == "SD" ~ "gold",
      toupper(AVALC) == "PD" ~ "red",
      TRUE ~ "gray"),
    PCHG_display = case_when(
      !is.na(PCHG) & PCHG > 100 ~ 100,
      !is.na(PCHG) & PCHG < -100 ~ -100,   # SAS: missing < -100 is TRUE
      is.na(PCHG) ~ -100,
      TRUE ~ PCHG)
  )
# SAS comparison: missing < -100 evaluates true, so missing PCHG -> -100 (kept as above)

write.csv(waterfall, file.path(out, "f_waterfall.csv"), row.names = FALSE, na = "")

# Step 3: plot
pd <- waterfall %>% mutate(AVALC = ifelse(is.na(AVALC) | AVALC == "", " ", AVALC),
                           bar_order = factor(bar_order))
p <- ggplot(pd, aes(x = bar_order, y = PCHG_display, fill = AVALC)) +
  geom_col(width = 1, colour = "black", linewidth = 0.2) +
  geom_hline(yintercept = -30, linetype = "dashed", colour = "blue") +
  geom_hline(yintercept = 20, linetype = "dashed", colour = "red") +
  geom_hline(yintercept = 0, colour = "black") +
  annotate("text", x = Inf, y = -30, label = "-30% (PR threshold)", hjust = 1.05, vjust = -0.4, size = 2.5) +
  annotate("text", x = Inf, y = 20, label = "+20% (PD threshold)", hjust = 1.05, vjust = -0.4, size = 2.5) +
  scale_y_continuous(breaks = seq(-100, 100, 20), limits = c(-100, 100)) +
  labs(title = "Figure 14.2.3",
       subtitle = "Waterfall Plot of Best Percentage Change from Baseline in Sum of Target Lesions\nEfficacy Analysis Set",
       x = NULL, y = "Best Percentage Change from Baseline (%)",
       fill = "Best Overall Response",
       caption = paste("Data cutoff: [DDMONYYYY]",
                       "Each bar represents one subject.",
                       "CR=Complete Response, PR=Partial Response, SD=Stable Disease, PD=Progressive Disease",
                       "Dashed lines: RECIST 1.1 thresholds for PR (-30%) and PD (+20%)", sep = "\n")) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted", colour = "lightgray"),
        legend.position = "bottom", plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0)) +
  guides(fill = guide_legend(nrow = 1))

ggsave(file.path(out, "f_waterfall.png"), p, width = 10, height = 6.5, dpi = 150)
cat("NOTE: f_waterfall completed successfully. N bars =", nrow(waterfall), "\n")
```