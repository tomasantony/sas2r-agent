We need to produce a single R script that reproduces outputs. Must read raw CSV, create datasets, compute summary tables, frequencies, survival analysis, generate plots, write outputs as CSV, text, PNG, and also produce PDF? The spec says write PNG and CSV of plotted data. Also need to produce text rendering and CSV of tables/listings: P061.lst, P061_t_oncology_report.pdf (but they are tables/listings text). Actually they are PDF in SAS but we need to write text rendering and CSV. So we need to generate same tables as in SAS.

We need to mimic SAS PROC MEANS, PROC FREQ, PROC LIFETEST outputs.

Simplify: produce tables using base R summarise and format numbers with same decimals as SAS default (PROC MEANS default prints with 8.2? Actually default prints with default format, but we can approximate. However requirement: "contains the same numbers the SAS output shows (counts, percentages with the same decimals, statistics)". We need to replicate exactly. Hard without seeing actual data. We'll approximate using typical formatting: mean, std, min, max with 2 decimals? SAS default for numeric is BEST12. but for PROC MEANS default prints with 8.2? Actually PROC MEANS default prints with default format of variable, which for numeric is BEST12. That may show many decimals. Hard.

Given constraints, we can produce tables with raw numbers (full precision) and write to CSV. For text rendering, we can create simple formatted table using knitr::kable? Not allowed packages. Use base write.table with formatted numbers.

Probably acceptable.

Implement:

- Set out <- Sys.getenv("OUT_DIR")
- Read raw CSV: readr::read_csv(file.path(Sys.getenv("rawdir"), "sumit-oncology", "Breast Cancer METABRIC.csv"))
But macro variable rawdir is D:/meta_hackathon/sas2r-agent/data/raw. Use Sys.getenv("rawdir")? Not set. Only OUT_DIR is set. So we need to use absolute paths given in macro variables. They are not environment variables. We can embed them directly as strings.

Thus define rawdir <- "D:/meta_hackathon/sas2r-agent/data/raw"
rawfile <- file.path(rawdir, "sumit-oncology", "Breast Cancer METABRIC.csv")

Read.

Create sdtm_dm: add STUDYID, DOMAIN, USUBJID, AGE.

adam_adsl: copy sdtm_dm, add TRT01P, SAFFL.

adam_adtte: copy adam_adsl, AVAL = Overall Survival (Months) column, CNSR based on Overall Survival Status.

Assume column names: patient_id, age_at_diagnosis, sex, Tumor Stage, Tumor Size, Overall Survival (Months), Overall Survival Status, Chemotherapy, ER Status, HER2 Status.

Need to handle spaces and special characters. Use backticks.

Now produce tables:

- Demographic Summary - Age: PROC MEANS n mean std min max for Age variable. Compute n = sum(!is.na(AGE)), mean, sd, min, max.

- Gender Distribution: PROC FREQ for sex.

- Tumor Stage Distribution: PROC FREQ for Tumor Stage.

- Tumor Size Summary: PROC MEANS for Tumor Size.

- Treatment Analysis: PROC FREQ for Chemotherapy.

- ER / HER2 Status: PROC FREQ for ER Status and HER2 Status (two-way? SAS PROC FREQ tables 'ER Status'n 'HER2 Status'n; that produces separate one-way tables). We'll produce separate freq tables.

- In ODS PDF sections: same tables plus survival analysis.

Survival analysis: Kaplan-Meier using survival::survfit(Surv(AVAL, CNSR==0)?) In SAS time AVAL*CNSR(1) means event indicator = CNSR where 1 indicates censored? Actually in SAS, CENSOR=1 indicates censored. They used CNSR where 0=deceased, 1=alive. So event = CNSR(1) means censoring variable = 1 indicates censored. So survival time variable AVAL, censor variable CNSR (1=censored). So in R, Surv(time = AVAL, event = CNSR==0). Use survfit.

Plot survival curve using ggplot2 with ggsurvplot from survminer (allowed). Save PNG.

Also other plots: vbar Tumor Stage (bar chart), histogram Age with density, scatter Tumor Size vs Overall Survival (Months), vbox Overall Survival by ER Status, vbar Chemotherapy.

Save each plot PNG and also CSV of plotted data (the data used for each plot). For bar and histogram, we can write the data frame used for plot (e.g., counts). For scatter, write the raw data.

Naming: figure files: sasgraph.png, sgplot.png, sgplot1.png, sgplot2.png, sgplot3.png, sgplot4.png, sgplot5.png, sgplot6.png. Need to map each figure to a name.

From SAS: first Kaplan-Meier Survival Graph (Figure 1) - maybe sasgraph.png.

Then Tumor Stage Bar Graph - sgplot.png

Age Distribution Histogram - sgplot1.png

Tumor Size vs Survival Scatter Plot - sgplot2.png

ER Status vs Survival Box Plot - sgplot3.png

Treatment Analysis Graph (Chemotherapy bar) - sgplot4.png

Later in PDF export: histogram again (sgplot5.png), vbar Tumor Stage (sgplot6.png), Kaplan-Meier again (maybe not needed). We'll follow order.

Implement.

Write CSV of each dataset: sdtm_dm.csv, adam_adsl.csv, adam_adtte.csv.

Write listing P061.lst: maybe combine all tables? We'll create a text file concatenating all tables with titles.

Similarly for PDF equivalents: produce same as text.

Implement helper function round_half_up: function(x, digits) { sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits }

Use for formatting numbers to 2 decimals.

Write text tables using sprintf.

Implement.

Now produce final R script.