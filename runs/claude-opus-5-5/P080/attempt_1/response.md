```r
# P080 - Figure 14.2.2 Forest Plot of Subgroup Analyses (translation of f_forest.sas)
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(survival); library(ggplot2)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
adam_dir <- "D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam"

read_adam <- function(name) {
  f1 <- file.path(adam_dir, paste0(name, ".sas7bdat"))
  f2 <- file.path(adam_dir, paste0(name, ".xpt"))
  if (file.exists(f1)) d <- haven::read_sas(f1) else d <- haven::read_xpt(f2)
  names(d) <- toupper(names(d))
  d <- haven::zap_labels(d)
  d <- as.data.frame(d, stringsAsFactors = FALSE)
  for (v in names(d)) if (is.character(d[[v]])) d[[v]][is.na(d[[v]])] <- ""
  d
}

trt1lbl <- "Placebo"

# SAS round half away from zero
rhu <- function(x, d = 0) sign(x) * floor(abs(x) * 10^d + 0.5 + 1e-9) / 10^d

# SAS put(x, w.d) (with decimal reduction when too wide)
put_wd <- function(x, w = 5, d = 2) {
  sapply(x, function(v) {
    if (is.na(v) || !is.finite(v)) return(".")
    for (dd in d:0) {
      s <- formatC(rhu(v, dd), format = "f", digits = dd)
      if (nchar(s) <= w) return(s)
    }
    s <- formatC(v, format = "E", digits = max(w - 6, 0))
    s
  }, USE.NAMES = FALSE)
}

cat("Reading data...\n")
adtte <- read_adam("adtte")
adsl  <- read_adam("adsl")

# Step 2: prepare data
a <- adtte[adtte$PARAMCD == "OS", c("USUBJID", "TRTA", "TRTAN", "AVAL", "CNSR")]
b <- adsl[adsl$EFFFL == "Y", c("USUBJID", "SEX", "AGEGR1", "RACE", "REGION1", "ECOGBL")]
eff <- inner_join(a, b, by = "USUBJID")

# Cox model mimicking PROC PHREG (Breslow ties, class ref=Placebo)
fit_cox <- function(d) {
  d <- d[!is.na(d$AVAL) & !is.na(d$CNSR) & d$TRTA != "", , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  lv <- sort(unique(d$TRTA), method = "radix")
  if (!(trt1lbl %in% lv) || length(lv) < 2) return(NULL)
  lv <- c(trt1lbl, setdiff(lv, trt1lbl))
  d$TRTA_F <- factor(d$TRTA, levels = lv)
  d$EVENT <- as.integer(d$CNSR != 1)
  if (sum(d$EVENT) == 0) return(NULL)
  fit <- tryCatch(suppressWarnings(coxph(Surv(AVAL, EVENT) ~ TRTA_F, data = d, ties = "breslow")),
                  error = function(e) NULL)
  if (is.null(fit)) return(NULL)
  est <- coef(fit); se <- sqrt(diag(vcov(fit)))
  chi <- (est / se)^2
  data.frame(ClassVal0 = lv[-1], Estimate = unname(est), StdErr = unname(se),
             ProbChiSq = unname(pchisq(chi, 1, lower.tail = FALSE)), stringsAsFactors = FALSE)
}

format_res <- function(pe, subgroup, level, ord) {
  if (is.null(pe) || nrow(pe) == 0) return(NULL)
  HR <- exp(pe$Estimate)
  L <- exp(pe$Estimate - 1.96 * pe$StdErr)
  U <- exp(pe$Estimate + 1.96 * pe$StdErr)
  data.frame(ProbChiSq = pe$ProbChiSq, subgroup = subgroup, subgroup_level = level,
             subgrp_ord = ord, HR = HR, HR_LCL = L, HR_UCL = U,
             hr_display = put_wd(HR),
             ci_display = paste0("(", put_wd(L), ", ", put_wd(U), ")"),
             stringsAsFactors = FALSE)
}

subgroup_analysis <- function(data, subgrp, subgrp_label, subgrp_ord) {
  vals <- data[[subgrp]]
  levs <- if (is.numeric(vals)) sort(unique(vals), na.last = FALSE) else sort(unique(vals), method = "radix")
  res <- list()
  for (lv in levs) {
    d <- if (is.na(lv)) data[is.na(vals), ] else data[!is.na(vals) & vals == lv, ]
    lab <- if (is.na(lv)) "." else trimws(as.character(lv))
    res[[length(res) + 1]] <- format_res(fit_cox(d), subgrp_label, lab, subgrp_ord)
  }
  do.call(rbind, res)
}

cat("Running subgroup analyses...\n")
sg1 <- subgroup_analysis(eff, "SEX", "Sex", 1)
sg2 <- subgroup_analysis(eff, "AGEGR1", "Age Group", 2)
sg3 <- subgroup_analysis(eff, "RACE", "Race", 3)
sg4 <- subgroup_analysis(eff, "REGION1", "Region", 4)

# Step 4: overall
sg0 <- format_res(fit_cox(eff), "Overall", "All Subjects", 0)

# Step 5: combine
forest_data <- rbind(sg0, sg1, sg2, sg3, sg4)
forest_data$row <- seq_len(nrow(forest_data))
forest_data$yval <- -forest_data$row

write.csv(forest_data, file.path(out, "forest_data.csv"), row.names = FALSE, na = "")
write.csv(forest_data, file.path(out, "f_forest.csv"), row.names = FALSE, na = "")

# Text rendering
txt <- c("Figure 14.2.2",
         "Forest Plot of Subgroup Analyses - Overall Survival",
         "Efficacy Analysis Set", "",
         sprintf("%-12s %-30s %-8s %-18s %s", "Subgroup", "Level", "HR", "95% CI", "p-value"))
for (i in seq_len(nrow(forest_data))) {
  txt <- c(txt, sprintf("%-12s %-30s %-8s %-18s %s", forest_data$subgroup[i],
                        forest_data$subgroup_level[i], forest_data$hr_display[i],
                        forest_data$ci_display[i], formatC(forest_data$ProbChiSq[i], format = "f", digits = 4)))
}
txt <- c(txt, "", "Hazard Ratios from Cox proportional hazards model.")
writeLines(txt, file.path(out, "f_forest.txt"))

# Step 6: forest plot
cat("Drawing plot...\n")
n <- nrow(forest_data)
p <- ggplot(forest_data, aes(y = yval)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "gray") +
  geom_errorbarh(aes(xmin = HR_LCL, xmax = HR_UCL), height = 0, colour = "darkblue", linewidth = 1) +
  geom_point(aes(x = HR), shape = 18, size = 4, colour = "darkblue") +
  scale_x_continuous(trans = "log2", breaks = c(0.1, 0.25, 0.5, 1, 2, 4),
                     labels = c("0.1", "0.25", "0.5", "1", "2", "4")) +
  scale_y_continuous(breaks = -(1:n), labels = as.character(-(1:n))) +
  labs(x = "Hazard Ratio (95% CI)", y = NULL,
       title = "Figure 14.2.2\nForest Plot of Subgroup Analyses - Overall Survival\nEfficacy Analysis Set",
       caption = "Hazard Ratios from Cox proportional hazards model.") +
  annotate("text", x = -Inf, y = -Inf, label = "Favors [Drug] Low Dose", hjust = -0.05, vjust = -0.5, size = 3) +
  annotate("text", x = Inf, y = -Inf, label = "Favors Placebo", hjust = 1.05, vjust = -0.5, size = 3) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5), plot.caption = element_text(hjust = 0))
ggsave(file.path(out, "f_forest.png"), p, width = 10, height = 7, dpi = 100)

cat("NOTE: f_forest completed successfully.\n")
```