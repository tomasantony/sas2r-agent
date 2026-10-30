We need to produce R script that replicates SAS program. Steps:

- Read advs.xpt from rawadam path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt

- Filter where PARAMCD="DIABP", ATPTN=815, ANL01FL='Y', .<avisitn<99, SAFFL='Y'.

- Keep variables: usubjid trtpn ANL01FL PARAM PARAMCD avisit avisitn ATPTN SAFFL aval.

- PROC CONTENTS not needed for output.

- PROC FREQ tables trtpn Avisit*avisitn aval/list; Not required for output.

- PROC SORT by trtpn avisitn.

- PROC MEANS BY trtpn avisitn avisit (note they also include avisit variable). Compute N, mean, stderr, lclm, uclm. In SAS, STDERR = standard error of mean = stddev / sqrt(N). LCLM and UCLM are mean +/- t * stderr? Actually PROC MEANS with LCLM/UCLM produce mean +/- t_{alpha/2,df} * stddev / sqrt(N). But later they compute lo/hi using TINV(0.95, n-1) * stderr. Note TINV returns t value for two-sided? In SAS, TINV(p,df) returns t such that P(T>t)=p? Actually TINV(prob,df) returns t such that two-tailed probability = prob? Wait SAS TINV returns the inverse of the Student t distribution. For probability p and df, returns t such that P(T <= t) = p. So for 0.95, it's the 95th percentile. For 95% CI, they need t_{0.975}. But they use 0.95, maybe they intend 90% CI? We'll replicate exactly: lo = mean - TINV(0.95, n-1)*stderr; hi = mean + TINV(0.95, n-1)*stderr.

- Then drop _TYPE_, _FREQ_, lclm, uclm, stderr.

- Then split into three datasets a,b,c based on trtpn values 0,54,81.

- Rename columns accordingly.

- Merge a1 b1 c1 by avisitn avisit (xc, x). Use full outer? In SAS merge without specifying type defaults to one-to-one by sorted data; if any missing, will be missing. We'll do full join (inner? but all have same avisitn/avisit). Use dplyr::full_join sequentially.

- Rename avisitn to xc, avisit to x, drop trtpn.

- label a_mean=0 b_mean=54 c_mean=81 (not needed for output).

- Output dataset "all" as CSV.

- Then produce graph using ggplot replicating scatter with error bars and series lines, blockplot (bars showing n). In SAS they use blockplot to display counts at each x for each group. We'll approximate using geom_col with position = "dodge" and annotate values.

- Output PNG named clinicalhandout_dbpprofile.png (lowercase as per spec). Also CSV of plotted data (maybe the "all" dataset). We'll write that.

- Also produce listing P047.lst: likely a table of summary stats? The SAS program didn't produce explicit table output besides the graph. But expected outputs include P047.lst (table/listing text, 419 numbers). That likely corresponds to PROC MEANS output? Or maybe they want the final "all" dataset printed as a table. We'll create a text rendering of the "all" dataset with formatted numbers similar to SAS PROC PRINT? We'll produce a table with columns: xc, x, a_n, a_mean, a_lcl, a_ucl, b_n, b_mean, b_lcl, b_ucl, c_n, c_mean, c_lcl, c_ucl. Format numbers: n as integer, means and CI with 2 decimals. We'll also write CSV.

- Also DBP_Profile.html: SAS ODS HTML with the graph. We'll produce an HTML file embedding the PNG image. We'll write simple HTML with <img src="clinicalhandout_dbpprofile.png">.

- Ensure output directory from Sys.getenv("OUT_DIR").

- Use round-half-up for numbers: need custom function round_half_up(x, digits). Use floor(x*10^d + 0.5)/10^d.

- For formatting, use sprintf with appropriate decimals.

- Use haven::read_xpt.

- Use dplyr for processing.

- Write outputs.

Let's craft script.

Edge: TINV function: we can use qt(p, df) where p is probability. In SAS TINV(0.95, df) returns t such that P(T > t) = 0.95? Actually SAS's TINV returns the inverse of the t distribution. According to SAS docs, TINV(p, df) returns the t value such that the probability that a t random variable with df degrees of freedom exceeds the absolute value of the t value is p. So it's two-sided? Actually they have TINV(p,df) = quantile of t distribution with upper tail probability p. So for 0.95, t_{0.05} (i.e., 5% upper tail) which is about 1.645 for large df. That matches 90% CI (since 5% each side). We'll replicate using qt(1-0.95, df)?? Wait upper tail probability p = 0.95 => lower tail = 0.05 => qt(0.05, df) negative. But they likely use absolute value. In SAS they use TINV(0.95, n-1) which returns positive t for 0.95 upper tail? Actually SAS's TINV returns the t value such that the probability of a t random variable with df degrees of freedom being greater than the absolute value of the t value is p. So for p=0.95, we need t such that P(|T|>t)=0.95 => extremely small t? That seems odd. I think they misuse; but we replicate using qt(0.95, df) which gives 95th percentile (upper tail 5%). We'll use qt(0.95, df). That matches typical 90% CI? Actually 95th percentile corresponds to 5% upper tail, so CI = mean +/- t_{0.975} for 95% CI. So not same. But we follow SAS: use qt(0.95, df). We'll compute t = qt(0.95, df=n-1).

- Compute stderr = sd / sqrt(n). Use sd from data.

- PROC MEANS with BY trtpn avisitn avisit: need grouping.

Implementation steps:

1. out <- Sys.getenv("OUT_DIR"); if empty, stop.

2. Read advs.

3. Filter.

4. Keep columns.

5. Compute summary: group_by(trtpn, avisitn, avisit) summarise n = n(), mean = mean(aval, na.rm=TRUE), sd = sd(aval, na.rm=TRUE). Compute stderr = sd / sqrt(n). Compute t = qt(0.95, df = n-1). lo = mean - t*stderr; hi = mean + t*stderr.

6. Create temp1 dataframe with needed columns.

7. Split into a,b,c.

8. Rename.

9. Merge.

10. Write all.csv.

11. Create listing: format as text. We'll create a character vector lines: header then each row formatted.

12. Write P047.txt and P047.csv.

13. Plot: using ggplot2. Data frame all. We'll create base plot with x = xc (numeric). Use geom_point with errorbars for each group offset.

Offsets: a: -0.05, b: +0.05, c: +0.15.

We'll create separate data frames for each group with x offset.

Add geom_line for series.

Add geom_col for blockplot: counts a_n, b_n, c_n as bars at x positions with width maybe 0.08? Use position = position_dodge? But SAS blockplot shows stacked? Actually blockplot with block = count variable draws blocks (bars) at each x, repeated values true, display values label. We'll approximate with geom_col with fill per group, label with count.

Simplify: Use geom_col with aes(x=xc, y=a_n) fill='0', etc. Use position = position_dodge(width=0.2). Add geom_text to show counts.

We'll set theme minimal.

Save PNG with ggsave.

Write CSV of plotted data: maybe the "all" dataset.

12. Write HTML file DBP_Profile.html with <img src="clinicalhandout_dbpprofile.png">.

All file names lower case? Expected: ClinicalHandout_DbpProfile.gif -> we output clinicalhandout_dbpprofile.png. Use same base name but .png.

Also DBP_Profile.html.

Now produce script.

