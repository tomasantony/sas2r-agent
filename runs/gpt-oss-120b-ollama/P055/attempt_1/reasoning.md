We need to produce R script that replicates SAS program. Steps:

- Read advs.xpt from rawadam path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt

- Filter: SAFFL="Y", PARAMCD="DIABP", ATPT="AFTER LYING DOWN FOR 5 MINUTES", AVISITN >=0 and <99, ANL01FL="Y". Keep USUBJID TRTA ADY AVAL.

- Sort by TRTA ADY.

- Compute means, n, lclm, uclm per TRTA ADY using PROC MEANS with default confidence level 95%? SAS default for LCLM/UCLM is mean +/- t*SE. We'll replicate using t distribution. Use t quantile with df = n-1. lclm = mean - t*SE, uclm = mean + t*SE. Need to match SAS rounding? We'll output as is.

- Create datasets mean (trta ady mean), num (trta ady num), ci (trta ady lclm uclm) with adjusted ADY offsets: for Low Dose keep ady; High Dose ady+0.3; Placebo ady+0.6.

- Sort ci by trta ady.

- Create anno dataset: for each row in ci where lclm and uclm not missing, produce multiple rows for drawing lines. This is complex; but for final plot we can use ggplot with error bars and annotate counts. However SAS uses annotate to draw custom lines and markers. We can simplify: produce plot using ggplot with mean points and error bars, and add text annotations for n.

- The required outputs: listing P055.lst (table/listing text, 1242 numbers) – likely the PROC PRINT of ci? Actually they printed ci and maybe final? We'll need to produce a table that matches SAS listing. Probably they want the listing of ci (with adjusted ADY). We'll output that as CSV and text.

- Figure: produce PNG similar to SAS gplot: x=ady (adjusted), y=mean, color by trta, point shapes as per symbol mapping: Placebo blue dot, High Dose red triangle, Low Dose green star. Add error bars using lclm/uclm. Add annotation of n below x-axis (y negative). We'll use ggplot2.

- Also need CSV of plotted data (maybe final dataset used for plot). We'll output final dataset (mean, num, adjusted ady) as CSV.

- Write outputs to OUT_DIR env var.

Implement rounding half away from zero: define function round_half_up.

But for numeric values we can keep as is.

Implement reading with haven::read_xpt.

Implement writing.

Now produce script.

