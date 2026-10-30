```r
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2); library(tidyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# SAS-style rounding: halves go away from zero
round_half_up <- function(x, d = 0) {
  m <- 10^d
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m
}

## ---- read ADVS ----
src <- read_xpt(file.path(rawadam, "advs.xpt"))
advs <- src %>%
  filter(PARAMCD == "DIABP", ATPTN == 815, ANL01FL == "Y",
         !is.na(AVISITN), AVISITN < 99, SAFFL == "Y") %>%
  select(any_of(c("USUBJID", "TRTPN", "ANL01FL", "PARAM", "PARAMCD", "AVISIT",
                  "AVISITN", "ATPTN", "SAFFL", "AVAL")))
cat("ADVS rows:", nrow(advs), "\n")

## ---- PROC CONTENTS (text) ----
lst <- character()
lst <- c(lst, "The CONTENTS Procedure", "",
         sprintf("Data Set Name  WORK.ADVS   Observations  %d   Variables  %d",
                 nrow(advs), ncol(advs)), "",
         "Alphabetic List of Variables and Attributes", "",
         sprintf("%-4s %-10s %-6s %-4s %s", "#", "Variable", "Type", "Len", "Label"))
vn <- names(advs)
ord <- order(toupper(vn))
for (i in ord) {
  v <- advs[[vn[i]]]
  typ <- if (is.numeric(v)) "Num" else "Char"
  len <- if (is.numeric(v)) 8 else max(1, max(nchar(as.character(v)), na.rm = TRUE))
  lab <- attr(v, "label"); if (is.null(lab)) lab <- ""
  lst <- c(lst, sprintf("%-4d %-10s %-6s %-4d %s", i, vn[i], typ, len, lab))
}
lst <- c(lst, "")

## ---- PROC FREQ ----
freq_tab <- function(df, vars) {
  d <- df[, vars, drop = FALSE]
  nmiss <- sum(!complete.cases(d))
  d <- d[complete.cases(d), , drop = FALSE]
  t <- d %>% count(across(all_of(vars)), name = "Frequency")
  t <- t[do.call(order, lapply(vars, function(v) t[[v]])), , drop = FALSE]
  N <- sum(t$Frequency)
  t$Percent <- round_half_up(100 * t$Frequency / N, 2)
  t$CumFrequency <- cumsum(t$Frequency)
  t$CumPercent <- round_half_up(100 * t$CumFrequency / N, 2)
  list(tab = as.data.frame(t), nmiss = nmiss)
}
freq_txt <- function(ft, vars) {
  t <- ft$tab
  hdr <- paste(c(sprintf("%-12s", vars), sprintf("%10s %8s %12s %10s",
               "Frequency", "Percent", "Cumulative", "Cumulative")), collapse = " ")
  hdr2 <- paste(c(sprintf("%-12s", rep("", length(vars))),
                  sprintf("%10s %8s %12s %10s", "", "", "Frequency", "Percent")), collapse = " ")
  rows <- apply(t, 1, function(r) {
    paste(c(sprintf("%-12s", trimws(as.character(r[vars]))),
            sprintf("%10d %8.2f %12d %10.2f", as.integer(r["Frequency"]),
                    as.numeric(r["Percent"]), as.integer(r["CumFrequency"]),
                    as.numeric(r["CumPercent"]))), collapse = " ")
  })
  res <- c("The FREQ Procedure", "", hdr, hdr2, rows)
  if (ft$nmiss > 0) res <- c(res, sprintf("Frequency Missing = %d", ft$nmiss))
  c(res, "")
}

f1 <- freq_tab(advs, "TRTPN")
f2 <- freq_tab(advs, c("AVISIT", "AVISITN"))
f3 <- freq_tab(advs, "AVAL")
lst <- c(lst, freq_txt(f1, "TRTPN"), freq_txt(f2, c("AVISIT", "AVISITN")),
         freq_txt(f3, "AVAL"))
writeLines(lst, file.path(out, "P032.txt"))

body <- bind_rows(
  f1$tab %>% mutate(Table = "TRTPN", Level = as.character(TRTPN)) %>% select(-TRTPN),
  f2$tab %>% mutate(Table = "AVISIT*AVISITN",
                    Level = paste(AVISIT, AVISITN, sep = " | ")) %>% select(-AVISIT, -AVISITN),
  f3$tab %>% mutate(Table = "AVAL", Level = as.character(AVAL)) %>% select(-AVAL)
) %>% select(Table, Level, Frequency, Percent, CumFrequency, CumPercent)
write.csv(body, file.path(out, "P032.csv"), row.names = FALSE, na = "")

## ---- PROC MEANS ----
advs <- advs %>% arrange(TRTPN, AVISITN)
temp <- advs %>%
  filter(!is.na(AVAL) | TRUE) %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(n = sum(!is.na(AVAL)),
            mean = mean(AVAL, na.rm = TRUE),
            stderr = sd(AVAL, na.rm = TRUE) / sqrt(sum(!is.na(AVAL))),
            .groups = "drop") %>%
  arrange(TRTPN, AVISITN, AVISIT)

temp1 <- temp %>%
  mutate(lo = mean - qt(0.95, n - 1) * stderr,
         hi = mean + qt(0.95, n - 1) * stderr) %>%
  select(-stderr)
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")

mk <- function(tr, p) {
  temp1 %>% filter(TRTPN == tr) %>% select(-TRTPN) %>%
    rename(!!paste0(p, "_n") := n, !!paste0(p, "_mean") := mean,
           !!paste0(p, "_lcl") := lo, !!paste0(p, "_ucl") := hi)
}
all <- mk(0, "a") %>%
  full_join(mk(54, "b"), by = c("AVISITN", "AVISIT")) %>%
  full_join(mk(81, "c"), by = c("AVISITN", "AVISIT")) %>%
  arrange(AVISITN, AVISIT) %>%
  rename(xc = AVISITN, x = AVISIT)
write.csv(all, file.path(out, "all.csv"), row.names = FALSE, na = "")
write.csv(advs, file.path(out, "advs.csv"), row.names = FALSE, na = "")

## ---- Figure ----
pd <- bind_rows(
  all %>% transmute(xc, x, trt = "0",  xp = xc - 0.05, mean = a_mean, lcl = a_lcl, ucl = a_ucl, n = a_n),
  all %>% transmute(xc, x, trt = "54", xp = xc + 0.05, mean = b_mean, lcl = b_lcl, ucl = b_ucl, n = b_n),
  all %>% transmute(xc, x, trt = "81", xp = xc + 0.15, mean = c_mean, lcl = c_lcl, ucl = c_ucl, n = c_n)
)
write.csv(pd, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")

ymin <- min(pd$lcl, na.rm = TRUE); ymax <- max(pd$ucl, na.rm = TRUE)
rng <- ymax - ymin
risk <- pd %>% filter(!is.na(n)) %>%
  mutate(yr = ymin - rng * c("81" = 0.12, "54" = 0.20, "0" = 0.28)[trt])
cols <- c("0" = "#A23A2E", "54" = "#01665E", "81" = "#543005")
ltys <- c("0" = "dotted", "54" = "dashed", "81" = "longdash")

p <- ggplot(pd %>% filter(!is.na(mean)), aes(xp, mean, colour = trt)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.3, linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = trt), linewidth = 0.8) +
  geom_text(data = risk, aes(x = xc, y = yr, label = n), size = 2.5, show.legend = FALSE) +
  annotate("text", x = -0.5, y = ymin - rng * 0.04, label = "At Risk Subjects",
           hjust = 0, size = 3) +
  annotate("text", x = -1.2, y = ymin - rng * c(0.12, 0.20, 0.28),
           label = c("81", "54", "0"), colour = cols[c("81", "54", "0")], size = 2.5) +
  scale_colour_manual(values = cols, name = "Treatment Group: ") +
  scale_linetype_manual(values = ltys, name = "Treatment Group: ") +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 26)) +
  labs(x = "Weeks Since Randomized", y = "Mean(unit)with 95% CI",
       title = "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)") +
  theme_bw() +
  theme(legend.position = "bottom", plot.title = element_text(size = 9))
ggsave(file.path(out, "clinicalhandout_dbpprofile.png"), p, width = 6, height = 4, dpi = 100)

## ---- DBP_Profile.html counterpart (graph-only page) ----
writeLines(c("Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
             "[Graph: clinicalhandout_dbpprofile.png]"),
           file.path(out, "DBP_Profile.txt"))
write.csv(data.frame(title = "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)",
                     image = "clinicalhandout_dbpprofile.png"),
          file.path(out, "DBP_Profile.csv"), row.names = FALSE, na = "")

cat("Done.\n")
```