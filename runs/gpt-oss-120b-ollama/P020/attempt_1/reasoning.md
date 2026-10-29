We need to translate SAS program that creates a Kaplan-Meier plot using PROC LIFETEST on ADTTE dataset, stratified by TRT01A, with censoring variable CNSR (1 = censored). Need to output figure (PNG) and a table/listing (P020.lst) which presumably is the survival table output from PROC LIFETEST (including number at risk, survival estimates, etc). We need to reproduce SAS outputs exactly: counts, percentages, etc. But we don't have the exact expected numbers. We need to generate analogous outputs: survival curve plot and a table of survival estimates (maybe the default PROC LIFETEST output). We need to produce a text rendering and CSV of the same table.

We need to read ADTTE dataset from adam library path: &rawadam? Actually macro variable rawadam points to raw data, but library adam is set to &project_path./data/adam. The dataset is adam.adtte. So path: D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adtte.sas7bdat (or .xpt). We'll read using haven::read_sas.

We need to produce Kaplan-Meier using survival::survfit. Use Surv(AVAL, CNSR==0?) In SAS, time AVAL * CNSR(1) means event occurs when CNSR not equal 1? Actually in SAS, the syntax is time variable * censor(1) where censor variable = 1 indicates censored. So event indicator = (CNSR ne 1). So event = (CNSR != 1). Usually CNSR = 0 for event, 1 for censored. So we can define status = ifelse(CNSR == 1, 0, 1). Or status = 1 - CNSR (if only 0/1). We'll compute.

Strata TRT01A: factor.

We need to generate survival curve with at-risk numbers (atrisk). In ggplot2 we can use ggsurvplot from survminer, but we need to output PNG. Use survminer::ggsurvplot with risk table. However risk table is separate; we can just plot survival curves and maybe add risk table as annotation? The SAS ODS graphics produce a plot with at risk table below. We can replicate using ggsurvplot which includes risk table.

We need to write CSV of plotted data: maybe the data used for the plot (time, surv, lower, upper, strata). We'll create a data frame.

We also need to produce a table listing: PROC LIFETEST default output includes "Number of Subjects", "Number of Events", "Median Survival Time", "Survival Estimates at each time point", etc. Hard to replicate exactly. But we can produce a summary table: for each stratum, number of subjects, number of events, median survival, and maybe survival at each time point. We'll produce a table with columns: Stratum, N, Events, Median_Surv, Median_CI_Lower, Median_CI_Upper. Also maybe survival at each distinct time point: we can produce a long table with time, n_risk, n_event, survival, lower, upper. That matches typical PROC LIFETEST "Survival Estimates" table.

We'll output as CSV and also as text rendering with formatted numbers (2 decimals for percentages, maybe). We'll format numbers similar to SAS default: survival probabilities as 0.123 (maybe 3 decimals). We'll use format with 3 decimal places.

Implementation steps:

- Set out <- Sys.getenv("OUT_DIR")
- Read adtte: adtte_path <- file.path(project_path, "data", "adam", "adtte.sas7bdat")? Actually project_path variable is not defined in R; we can get from environment variable? Not provided. But we can reconstruct using macro variable values: root = D:/meta_hackathon/sas2r-agent, project_path = &root./data/derived/c2c. So we can set project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c". Use that.

- Read dataset: adtte <- haven::read_sas(file.path(project_path, "data", "adam", "adtte.sas7bdat"))

- Ensure variable names uppercase: haven reads as lower? It preserves case. We'll convert names to uppercase: names(adtte) <- toupper(names(adtte))

- Compute status: adtte$EVENT <- ifelse(adtte$CNSR == 1, 0, 1) (assuming CNSR numeric). Also ensure AVAL numeric.

- Convert TRT01A to factor with levels sorted as SAS would (character ordering, uppercase before lowercase). Use sort(unique(...), method = "radix")? R's default sort is lexical, case-sensitive, uppercase before lowercase, which matches SAS. So factor with levels = sort(unique(...), decreasing = FALSE).

- Fit survfit: fit <- survfit(Surv(AVAL, EVENT) ~ TRT01A, data = adtte, se = TRUE)

- Generate plot: use survminer::ggsurvplot(fit, data=adtte, risk.table=TRUE, conf.int=TRUE, palette = "Dark2", ggtheme = theme_minimal(), xlab = "Time", ylab = "Event-Free Survival Probability", title = "Figure 1: Kaplan-Meier Event-Free Survival by Treatment", legend.title = "Treatment", legend.labs = levels(adtte$TRT01A))

- Save PNG: ggsave(filename = file.path(out, "P020.png"), plot = ggsurv, width = 7, height = 5.5, units = "in", dpi = 300)

- Save plotted data: create data frame from summary of survfit: use tidy(fit) from broom? broom::tidy.survfit returns data with time, n.risk, n.event, estimate, std.error, conf.low, conf.high, strata. We'll use broom::tidy.

- Write CSV: write.csv(plot_data, file.path(out, "P020_plot_data.csv"), row.names = FALSE, na = "")

- Create table listing: Use summary of survfit to get survival estimates at each event time. Use broom::tidy again. We'll format numbers: n.risk integer, n.event integer, estimate with 3 decimal, conf.low/high with 3 decimal.

- Write CSV: write.csv(table_df, file.path(out, "P020.csv"), row.names = FALSE, na = "")

- Write text rendering: create a character vector lines: header, then formatted rows. Write to .txt.

- Ensure rounding half away from zero: Use custom round_half_up function: function(x, digits) { sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits }

- Apply to numeric columns before formatting.

- Use date formatting not needed.

- At end, no console prints except maybe message.

Let's craft script.

Edge: need to load packages: library(survival), library(survminer), library(broom), library(dplyr), library(readr), library(stringr), library(lubridate) maybe not needed.

Make sure to set options(stringsAsFactors = FALSE).

Implement round_half_up.

When formatting for text, we need to align columns maybe using sprintf.

We'll produce text with columns: Stratum, Time, N_Risk, N_Event, Survival, Lower_CI, Upper_CI.

We'll separate by spaces.

Implementation details:

table_df <- broom::tidy(fit) %>% mutate(across(where(is.numeric), ~ round_half_up(., 3))) # 3 decimals

But need to keep integer columns as integer.

We'll separate by strata: the 'strata' column includes "TRT01A=Placebo" etc. We'll extract stratum name after "=".

Add column Stratum = sub(".*=", "", strata)

Then order by Stratum then time.

Write CSV.

For text: create header line: sprintf("%-12s %8s %8s %8s %10s %10s %10s", "Stratum","Time","N_Risk","N_Event","Survival","Lower_CI","Upper_CI")

Then each row: sprintf("%-12s %8.0f %8d %8d %10.3f %10.3f %10.3f", ...)

Write lines to file.

Now produce final script.

