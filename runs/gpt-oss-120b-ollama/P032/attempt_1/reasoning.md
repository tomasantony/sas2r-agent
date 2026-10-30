We need to produce R script that replicates SAS program. Steps:

- Read advs.xpt from rawadam path: D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/advs.xpt

- Filter where PARAMCD="DIABP", ATPTN=815, ANL01FL='Y', .<avisitn<99, SAFFL='Y'.

- Keep variables: usubjid trtpn ANL01FL PARAM PARAMCD avisit avisitn ATPTN SAFFL aval.

- PROC CONTENTS not needed for output.

- PROC FREQ tables trtpn Avisit*avisitn aval/list; Not required for output.

- PROC SORT by trtpn avisitn.

- PROC MEANS BY trtpn avisitn avisit (note they include avisit variable). Compute N, mean, stderr, LCLM, UCLM. In SAS, STDERR = standard error of mean = stddev / sqrt(N). LCLM and UCLM are mean +/- t * stderr? Actually PROC MEANS with LCLM/UCLM produce confidence limits based on t distribution (default 95%). We'll compute same.

- Then compute lo and hi using TINV(0.95, n-1) * stderr. Note TINV returns t critical for two-sided? In SAS, TINV(p,df) returns t such that P(T>t)=p. For 95% CI, they use TINV(0.95, n-1) which is t_{0.05,df} (upper tail). So CI = mean +/- t * stderr. We'll replicate.

- Then split dataset into three groups based on trtpn values 0,54,81 into a,b,c.

- Rename columns accordingly.

- Merge a1,b1,c1 by avisitn, avisit (xc, x). In SAS merge without by will be by default all common variables; they have by avisitn avisit. So we need full outer join? In SAS merge with by will produce rows where key present in any dataset, missing others. We'll use full_join sequentially.

- After merge, rename avisitn to xc, avisit to x, drop trtpn, label a_mean=0 b_mean=54 c_mean=81 (labels not needed for output). Output dataset "all" as CSV.

- Then produce plot similar to SAS graph: scatter with error bars and series lines for each treatment group, plus blockplot (bars) showing counts (n). In R, we can use ggplot2.

- Need to output PNG named clinicalhandout_dbpprofile.png (lowercase as per spec). Also CSV of plotted data (maybe the "all" dataset). We'll write both.

- Also need to produce listing P032.lst: seems to be a table/listing text with 419 numbers. Likely the PROC FREQ or PROC MEANS output? Not sure. But we need to produce a text rendering and CSV of same table. Which table? Probably the "all" dataset? But they also have DBP_Profile.html (listing with 0 numbers). The .lst maybe from PROC PRINT? Not present. Could be from PROC FREQ? The spec says P032.lst (table/listing text, 419 numbers). We'll need to generate a table summarizing the data maybe the "temp1" stats? Let's assume they want the PROC MEANS output table (N, mean, stderr, lclm, uclm). We'll produce a table with columns trtpn, avisitn, avisit, n, mean, stderr, lclm, uclm, lo, hi. That's many numbers.

We'll create a data frame "summary_table" with those stats (including lo, hi). Write to CSV and also to text file with formatted numbers: N as integer, mean, stderr, lclm, uclm, lo, hi with 2 decimals maybe. We'll mimic SAS default: PROC MEANS default prints mean, stddev, etc with 8.2? Not sure. We'll just format with 2 decimals.

Also need DBP_Profile.html: SAS ODS HTML with the graph. In R we can generate an HTML file embedding the PNG image. We'll create simple HTML with <img src="clinicalhandout_dbpprofile.png">. Write to file.

Implement round-half-up: SAS rounds half away from zero. Use custom function: round_half_up <- function(x, digits=0){ sign(x)*floor(abs(x)*10^digits + 0.5)/10^digits }.

But for mean etc we may not need rounding; we will output raw values.

Implement TINV: use qt(p, df, lower.tail=FALSE). Since SAS TINV(p,df) returns t such that P(T>t)=p, so qt(p, df, lower.tail=FALSE). So tcrit = qt(0.95, df=n-1, lower.tail=FALSE).

Now produce script.

Need to get OUT_DIR from env var.

Write CSVs: advs.csv? Not required. Only outputs: P032.lst, clinicalhandout_dbpprofile.png, clinicalhandout_dbpprofile.csv (maybe same as all), DBP_Profile.html.

We'll write summary_table to P032.lst.txt and P032.lst.csv.

Also write "all" dataset to all.csv maybe not required but for completeness.

Implement.

Now produce final R script inside ```r``` block only.