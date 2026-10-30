```r
# P052 - outliers scatter (max post-baseline vs max baseline), translated from SAS
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# ---- read data ----
adsl <- haven::read_xpt(file.path(rawadam, "adsl.xpt"))
advsmax <- haven::read_xpt(file.path(rawadam, "advsmax.xpt"))
advsmax <- haven::zap_labels(haven::zap_formats(advsmax))

# ---- proc sort by studyid (stable) ----
advsmax <- advsmax[order(advsmax$STUDYID, method = "radix"), , drop = FALSE]

# ---- data advsmax_final: WHERE + first.studyid labels ----
advsmax_final <- advsmax %>%
  filter(!is.na(PARAMCD) & PARAMCD == "DIABP",
         !is.na(ATPTN) & ATPTN == 815,
         !is.na(ANL01FL) & ANL01FL == "Y",
         !is.na(ADY) & ADY > 1,
         !is.na(SAFFL) & SAFFL == "Y")

first_study <- !duplicated(advsmax_final$STUDYID)
advsmax_final$LLN_label <- ifelse(first_study, "LLN", "")
advsmax_final$ULN_label <- ifelse(first_study, "ULN", "")

# ---- ranges ----
smin <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
smax <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

range_df <- data.frame(
  min_base = smin(advsmax_final$BASE),
  max_base = smax(advsmax_final$BASE),
  min_aval = smin(advsmax_final$AVAL),
  max_aval = smax(advsmax_final$AVAL)
)
range2 <- range_df
range2$min <- smin(c(range2$min_base, range2$min_aval))
range2$max <- smax(c(range2$max_base, range2$max_aval))

min_v <- smin(range2$min)
max_v <- smax(range2$max)

# ---- count distinct studyid (remerge) ----
advsmax_final2 <- advsmax_final
advsmax_final2$count_study <- length(unique(advsmax_final$STUDYID[!is.na(advsmax_final$STUDYID) &
                                                                    advsmax_final$STUDYID != ""]))

advsmax_final3 <- advsmax_final2
advsmax_final3$column_numbers <- NA_real_
advsmax_final3$row_numbers <- NA_real_
advsmax_final3$column_numbers[advsmax_final3$count_study <= 2] <- 2
advsmax_final3$row_numbers[advsmax_final3$count_study <= 2] <- 1
advsmax_final3$column_numbers[advsmax_final3$count_study > 2] <- 2
advsmax_final3$row_numbers[advsmax_final3$count_study > 2] <- 2

cr <- unique(advsmax_final3[, c("column_numbers", "row_numbers")])
column_numbers <- cr$column_numbers[1]
row_numbers <- cr$row_numbers[1]

# ---- datasets ----
write.csv(advsmax_final, file.path(out, "advsmax_final.csv"), row.names = FALSE, na = "")
write.csv(range_df, file.path(out, "range.csv"), row.names = FALSE, na = "")
write.csv(range2, file.path(out, "range2.csv"), row.names = FALSE, na = "")
write.csv(advsmax_final3, file.path(out, "advsmax_final3.csv"), row.names = FALSE, na = "")

# ---- listing (P052.lst equivalent) ----
fmt <- function(x) ifelse(is.na(x), ".", format(x, trim = TRUE))
lst_body <- data.frame(
  query = c("min(min)", "max(max)", "column_numbers", "row_numbers"),
  value = c(min_v, max_v, column_numbers, row_numbers)
)
write.csv(lst_body, file.path(out, "P052.csv"), row.names = FALSE, na = "")
txt <- c(
  "The SAS System",
  "",
  sprintf("%10s  %10s", "", ""),
  sprintf("%10s  %10s", "----------", "----------"),
  sprintf("%10s  %10s", fmt(min_v), fmt(max_v)),
  "",
  sprintf("%14s  %11s", "column_numbers", "row_numbers"),
  sprintf("%14s  %11s", "--------------", "-----------"),
  sprintf("%14s  %11s", fmt(column_numbers), fmt(row_numbers))
)
writeLines(txt, file.path(out, "P052.txt"))

# ---- figure ----
plotdat <- advsmax_final3 %>%
  select(STUDYID, USUBJID, TRTA, BASE, AVAL, ANRLO, ANRHI, LLN_label, ULN_label)
write.csv(plotdat, file.path(out, "sgrender.csv"), row.names = FALSE, na = "")

refs <- plotdat %>% distinct(STUDYID, ANRLO, ANRHI)
lbls <- plotdat %>% filter(LLN_label != "") %>% distinct(STUDYID, ANRLO, ANRHI)

p <- ggplot(plotdat, aes(x = BASE, y = AVAL)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_vline(data = refs, aes(xintercept = ANRLO), linetype = "dashed") +
  geom_vline(data = refs, aes(xintercept = ANRHI), linetype = "dashed") +
  geom_hline(data = refs, aes(yintercept = ANRLO), linetype = "dashed") +
  geom_hline(data = refs, aes(yintercept = ANRHI), linetype = "dashed") +
  geom_point(aes(colour = TRTA)) +
  facet_wrap(~STUDYID, nrow = row_numbers, ncol = column_numbers) +
  labs(x = "Maximum Baseline Measurment", y = "Maximum Post-baseline Measurment",
       colour = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")
if (!is.na(min_v) && !is.na(max_v)) {
  p <- p + coord_cartesian(xlim = c(min_v, max_v), ylim = c(min_v, max_v))
  if (nrow(lbls) > 0) {
    p <- p +
      geom_text(data = lbls, aes(x = ANRLO, y = min_v, label = "LLN"), vjust = 1, size = 3) +
      geom_text(data = lbls, aes(x = ANRHI, y = min_v, label = "ULN"), vjust = 1, size = 3) +
      geom_text(data = lbls, aes(x = min_v, y = ANRLO, label = "LLN"), hjust = 0, size = 3) +
      geom_text(data = lbls, aes(x = min_v, y = ANRHI, label = "ULN"), hjust = 0, size = 3)
  }
}
ggsave(file.path(out, "sgrender.png"), p, width = 8, height = 6, dpi = 96)

cat("P052 done\n")
```