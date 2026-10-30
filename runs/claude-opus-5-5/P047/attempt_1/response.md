```r
# P047 - Mean DBP profile over time (translation of P047_mean_time.sas)
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(ggplot2); library(grid)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"

# SAS-style rounding (half away from zero)
round_half_up <- function(x, digits = 0) {
  p <- 10^digits
  sign(x) * floor(abs(x) * p + 0.5 + 1e-9) / p
}

# ---------------- read ADVS ----------------
cat("Reading ADVS\n")
src <- read_xpt(file.path(rawadam, "advs.xpt"))
names(src) <- toupper(names(src))

advs <- src %>%
  filter(PARAMCD == "DIABP", !is.na(ATPTN), ATPTN == 815, ANL01FL == "Y",
         !is.na(AVISITN), AVISITN < 99, SAFFL == "Y") %>%
  select(any_of(c("USUBJID", "TRTPN", "ANL01FL", "PARAM", "PARAMCD", "AVISIT",
                  "AVISITN", "ATPTN", "SAFFL", "AVAL")))

lst <- character(0)
add <- function(...) lst <<- c(lst, ...)

# ---------------- PROC CONTENTS ----------------
add("The CONTENTS Procedure", "",
    sprintf("Data Set Name        WORK.ADVS        Observations          %d", nrow(advs)),
    sprintf("Member Type          DATA             Variables             %d", ncol(advs)),
    "", "Alphabetic List of Variables and Attributes", "",
    sprintf("%-4s %-10s %-6s %5s  %s", "#", "Variable", "Type", "Len", "Label"))
vinfo <- lapply(seq_along(advs), function(i) {
  v <- advs[[i]]
  typ <- if (is.character(v)) "Char" else "Num"
  len <- if (is.character(v)) {
    w <- attr(v, "width"); if (is.null(w)) w <- max(c(1, nchar(v)), na.rm = TRUE); w
  } else 8
  lab <- attr(v, "label"); if (is.null(lab)) lab <- ""
  data.frame(num = i, var = names(advs)[i], type = typ, len = len, label = lab,
             stringsAsFactors = FALSE)
})
vinfo <- bind_rows(vinfo)
vinfo <- vinfo[order(toupper(vinfo$var)), ]
for (i in seq_len(nrow(vinfo)))
  add(sprintf("%-4d %-10s %-6s %5d  %s", vinfo$num[i], vinfo$var[i], vinfo$type[i],
              as.integer(vinfo$len[i]), vinfo$label[i]))
add("", "")

# ---------------- PROC FREQ ----------------
freq_tab <- function(df, vars) {
  d <- df[, vars, drop = FALSE]
  miss <- rowSums(is.na(d) | (sapply(d, is.character) & d == "")) > 0
  nmiss <- sum(miss)
  d <- d[!miss, , drop = FALSE]
  tab <- d %>% group_by(across(all_of(vars))) %>% summarise(Frequency = n(), .groups = "drop")
  ord <- do.call(order, lapply(vars, function(v) {
    x <- tab[[v]]; if (is.character(x)) xtfrm(enc2native(x)) else x }))
  tab <- tab[ord, ]
  tot <- sum(tab$Frequency)
  tab$Percent <- 100 * tab$Frequency / tot
  tab$CumFrequency <- cumsum(tab$Frequency)
  tab$CumPercent <- 100 * tab$CumFrequency / tot
  attr(tab, "nmiss") <- nmiss
  tab
}
old_loc <- Sys.getlocale("LC_COLLATE"); invisible(Sys.setlocale("LC_COLLATE", "C"))
freq_csv <- list()
add("The FREQ Procedure", "")
for (vs in list("TRTPN", c("AVISIT", "AVISITN"), "AVAL")) {
  tab <- freq_tab(advs, vs)
  hdr <- paste(sprintf("%-20s", vs), collapse = "")
  add(paste0(hdr, sprintf("%10s %10s %12s %12s", "Frequency", "Percent", "Cumulative", "Cumulative")),
      paste0(strrep(" ", nchar(hdr)), sprintf("%10s %10s %12s %12s", "", "", "Frequency", "Percent")),
      strrep("-", nchar(hdr) + 47))
  for (i in seq_len(nrow(tab))) {
    lab <- paste(sapply(vs, function(v) {
      x <- tab[[v]][i]; sprintf("%-20s", if (is.numeric(x)) format(x) else x) }), collapse = "")
    add(paste0(lab, sprintf("%10d %10.2f %12d %12.2f", tab$Frequency[i],
                            round_half_up(tab$Percent[i], 2), tab$CumFrequency[i],
                            round_half_up(tab$CumPercent[i], 2))))
  }
  if (attr(tab, "nmiss") > 0) add("", sprintf("Frequency Missing = %d", attr(tab, "nmiss")))
  add("")
  tc <- as.data.frame(tab)
  tc$table <- paste(vs, collapse = "*")
  for (v in c("TRTPN", "AVISIT", "AVISITN", "AVAL")) if (!v %in% names(tc)) tc[[v]] <- NA
  freq_csv[[length(freq_csv) + 1]] <- tc[, c("table", "TRTPN", "AVISIT", "AVISITN", "AVAL",
                                            "Frequency", "Percent", "CumFrequency", "CumPercent")]
}
invisible(Sys.setlocale("LC_COLLATE", old_loc))
writeLines(lst, file.path(out, "P047.txt"))
write.csv(bind_rows(freq_csv), file.path(out, "P047.csv"), row.names = FALSE, na = "")
write.csv(vinfo, file.path(out, "P047_contents.csv"), row.names = FALSE, na = "")

# ---------------- PROC MEANS ----------------
advs <- advs %>% arrange(TRTPN, AVISITN)
temp <- advs %>% filter(!is.na(AVAL) | TRUE) %>%
  group_by(TRTPN, AVISITN, AVISIT) %>%
  summarise(`_TYPE_` = 0, `_FREQ_` = n(),
            n = sum(!is.na(AVAL)),
            mean = mean(AVAL, na.rm = TRUE),
            stderr = sd(AVAL, na.rm = TRUE) / sqrt(sum(!is.na(AVAL))),
            .groups = "drop") %>%
  mutate(lclm = mean - qt(0.975, n - 1) * stderr,
         uclm = mean + qt(0.975, n - 1) * stderr) %>%
  arrange(TRTPN, AVISITN)

temp1 <- temp %>%
  mutate(lo = mean - qt(0.95, n - 1) * stderr,
         hi = mean + qt(0.95, n - 1) * stderr) %>%
  select(-`_TYPE_`, -`_FREQ_`, -lclm, -uclm, -stderr)

mk <- function(tp, pre) {
  temp1 %>% filter(TRTPN == tp) %>% select(-TRTPN) %>%
    rename(!!paste0(pre, "_n") := n, !!paste0(pre, "_mean") := mean,
           !!paste0(pre, "_lcl") := lo, !!paste0(pre, "_ucl") := hi)
}
a1 <- mk(0, "a"); b1 <- mk(54, "b"); c1 <- mk(81, "c")
all <- a1 %>% full_join(b1, by = c("AVISITN", "AVISIT")) %>%
  full_join(c1, by = c("AVISITN", "AVISIT")) %>%
  arrange(AVISITN, AVISIT) %>%
  rename(xc = AVISITN, x = AVISIT) %>%
  select(x, xc, a_n, a_mean, a_lcl, a_ucl, b_n, b_mean, b_lcl, b_ucl, c_n, c_mean, c_lcl, c_ucl)

write.csv(temp, file.path(out, "temp.csv"), row.names = FALSE, na = "")
write.csv(temp1, file.path(out, "temp1.csv"), row.names = FALSE, na = "")
write.csv(all, file.path(out, "all.csv"), row.names = FALSE, na = "")

# ---------------- Figure ----------------
title <- "Mean of DBP Measures by Treatment: Profile Over Time (Weeks Since Randomized)"
pd <- bind_rows(
  data.frame(trt = "0", xpos = all$xc - 0.05, xc = all$xc, n = all$a_n, mean = all$a_mean, lcl = all$a_lcl, ucl = all$a_ucl),
  data.frame(trt = "54", xpos = all$xc + 0.05, xc = all$xc, n = all$b_n, mean = all$b_mean, lcl = all$b_lcl, ucl = all$b_ucl),
  data.frame(trt = "81", xpos = all$xc + 0.15, xc = all$xc, n = all$c_n, mean = all$c_mean, lcl = all$c_lcl, ucl = all$c_ucl))
write.csv(pd, file.path(out, "clinicalhandout_dbpprofile.csv"), row.names = FALSE, na = "")

cols <- c("0" = "#A23A2E", "54" = "#01665E", "81" = "#543005")
ltys <- c("0" = "dotted", "54" = "dashed", "81" = "longdash")
xr <- range(c(pd$xpos, 0, 26), na.rm = TRUE); pad <- 0.05 * diff(xr)
brks <- c(0, 2, 4, 6, 8, 10, 12, 16, 20, 24, 26)
pdm <- pd[!is.na(pd$mean), ]
p1 <- ggplot(pdm, aes(xpos, mean, colour = trt)) +
  geom_errorbar(aes(ymin = lcl, ymax = ucl), width = 0.2, linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_line(aes(linetype = trt), linewidth = 0.8) +
  scale_colour_manual(values = cols, name = "Treatment Group: ") +
  scale_linetype_manual(values = ltys, name = "Treatment Group: ") +
  scale_x_continuous(breaks = brks, limits = c(xr[1] - pad, xr[2] + pad)) +
  labs(title = title, x = "Weeks Since Randomized", y = "Mean(unit)with 95% CI") +
  theme_bw() + theme(legend.position = "bottom", plot.title = element_text(size = 9, hjust = 0.5))
rk <- pd[!is.na(pd$n), ]
rk$trt <- factor(rk$trt, levels = c("0", "54", "81"))
p2 <- ggplot(rk, aes(xc, trt, label = n, colour = trt)) +
  geom_text(size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = cols) +
  scale_x_continuous(breaks = brks, limits = c(xr[1] - pad, xr[2] + pad)) +
  labs(title = "At Risk Subjects", x = NULL, y = NULL) +
  theme_minimal() + theme(axis.text.x = element_blank(), panel.grid = element_blank(),
                          plot.title = element_text(size = 8))
png(file.path(out, "clinicalhandout_dbpprofile.png"), width = 600, height = 400)
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1, heights = unit(c(0.75, 0.25), "npc"))))
print(p1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
dev.off()

# ---------------- DBP_Profile.html counterpart ----------------
writeLines(c(title, "", "(Graph: ClinicalHandout_DbpProfile)"), file.path(out, "dbp_profile.txt"))
write.csv(data.frame(title = title, image = "clinicalhandout_dbpprofile.png"),
          file.path(out, "dbp_profile.csv"), row.names = FALSE, na = "")

cat("Done\n")
```