Need to understand mismatch: 249/308 numbers found, missing numbers like 0.0465 etc which are likely standard errors. The R script used surv_std_err = std.err * surv but SAS uses Greenwood formula. Need to check. Also missing 68 etc.

The SAS PROC LIFETEST default: survival estimates with Greenwood SE. In R, summary(fit)$std.err is SE of cumulative hazard? Actually survfit gives std.err as SE of survival? Let's recall: survfit summary std.err is standard error of the survival estimate? Check docs: std.err is standard error of the cumulative hazard or survival? For survfit, std.err is SE of survival? Actually for Kaplan-Meier, std.err is SE of the survival estimate using Greenwood? Let's verify.

But numbers missing: 0.0465, 0.0641 etc are likely Greenwood SE values. Our calculation std.err * surv may be wrong. Should use sfit_summary$std.err directly? Let's see typical values: For first event, survival 0.9524, SE should be about 0.0465. If std.err * surv = 0.0465, then std.err ~0.0488. Could be.

We need to inspect SAS output to know expected numbers. We don't have SAS lst directly but we can infer. The missing numbers include 0.0465 etc which are SEs. Our current SE values maybe slightly different due to calculation.

Also missing 68 etc maybe time values? Our breaks_atrisk maybe different from SAS.

We need to reproduce SAS PROC LIFETEST exactly: It outputs Product-Limit Survival Estimates table with columns: Time, Survival, Failure, Survival Standard Error, Number Failed, Number Left. The SE is Greenwood.

In R, survfit with default conf.type = "log" gives CI via log transform. SAS default is log-log? Need to check.

But the main mismatch is SE values. Let's look at what R's summary gives: sfit_summary$std.err is actually standard error of the survival estimate? Let's check documentation: For survfit, summary returns std.err = standard error of the survival estimate. Actually ?summary.survfit says std.err is standard error of the survival estimate. So we should not multiply.

Our code did surv_std_err = std.err * surv which would be wrong if std.err already is SE of survival. That would produce smaller values? Let's test: If std.err is 0.0465, then std.err*surv = 0.0443, not matching. So missing numbers are 0.0465 etc, meaning our output had 0.0443 etc, not matching.

So fix: use std.err directly.

Also need to ensure rounding matches SAS: SAS rounds half away from zero, we do. Need to format to 4 decimals.

Also need to check other missing numbers: 68 etc maybe median? Our median 69 vs SAS 68? Let's see: quartile estimates: we have 32,69 for Active Drug, 17,40 for Placebo. SAS maybe has 68? The missing 68 suggests SAS median is 68 not 69. Could be due to different handling of censored times.

We need to investigate ADTTE data: 47 rows, 21 active, 26 placebo. Need to compute median via SAS method: PROC LIFETEST median is defined as smallest time where survival <=0.5. R's quantile may give slightly different due to interpolation. Need to replicate SAS definition.

Also need to ensure we output all numbers SAS outputs: The SAS output includes 308 numbers. Our text currently has 83 lines, but SAS likely has many more lines including all survival estimates (27 events + censored times?). Our surv_df only includes event times where n.event>0, but SAS includes every distinct time where either event or censor occurs? PROC LIFETEST by default outputs every event time, but also includes censored times with survival unchanged? Let's check: In SAS, Product-Limit table includes rows for each unique AVAL where at least one event or censor occurs, with survival staying same for censored times but SE unchanged. Our surv_df from summary(fit) only includes event times (where survival drops). Censored times are not included as separate rows, but SAS includes them? Actually summary(fit) includes only event times; but SAS includes censored times as well with same survival but updated n.risk, number left etc. The missing numbers like 0.1125 etc could be SE at censored times.

We need to generate full table including censored times.

How to get full table? Use survfit summary with censored=TRUE? Or manually construct.

Simpler: Build product-limit table manually by iterating over sorted unique times.

We need to replicate SAS PROC LIFETEST Product-Limit estimates: For each distinct AVAL sorted ascending, compute n.risk, n.event, n.censor, survival, SE.

We can compute manually using Greenwood formula.

Let's implement manual KM to include all times.

Also need to ensure we output numbers with correct formatting to match SAS numbers: e.g., 0.9524 etc.

We should also check log-rank numbers: we have 0.6133 etc, need to ensure matches.

We need to adjust script to generate full table.

Let's design manual KM:

For each TRT01A group:
- sort by AVAL ascending
- get unique times
- for each time t:
  n.risk = number with AVAL >= t
  n.event = number with AVAL == t & EVENT==1
  n.censor = number with AVAL == t & EVENT==0
  survival = previous survival * (1 - n.event/n.risk) if n.event>0 else previous survival
  Greenwood variance: var = sum over event times <=t of n.event / (n.risk*(n.risk - n.event))
  SE = survival * sqrt(var)

This will produce SE values matching SAS.

We need to test with data to see if SE matches 0.0465 etc.

For first event: n.risk=21 for Active Drug? Actually total 21. First event at time maybe 13? Let's compute: For Active Drug, first event survival = 1*(1-1/21)=0.9524, var =1/(21*20)=0.002381, sqrt=0.0488, SE=0.9524*0.0488=0.0465 matches missing number! So our earlier std.err*surv gave 0.0465? Wait we computed std.err*surv =0.0465, but if std.err is sqrt(var)=0.0488, then product is 0.0465. So our earlier method would give correct if std.err is sqrt(var). But summary(fit)$std.err is actually sqrt(var) ? Let's see: summary(fit)$std.err for first event should be 0.0488, then *surv =0.0465. So our earlier code did that and should match. But we got mismatch, meaning summary(fit)$std.err maybe already is SE of survival (0.0465), then *surv would give 0.0443, mismatch. So we need to determine which.

We can test by printing both.

But manual calculation will guarantee correct.

Also need to include censored times: For censored time, survival unchanged, SE unchanged, but n.risk, number left etc change.

SAS table includes rows for censored times as well, with survival same as previous, SE same, but Number Failed cumulative, Number Left updated.

Our current surv_df only has event times, so we miss censored rows. That explains missing numbers: many SE values repeated at censored times (0.0465 etc repeated), and times like 68 etc are censored times.

We need to generate full table including all distinct AVAL values.

Let's implement manual.

Also need to handle median: SAS median is smallest t where survival <=0.5. For Active Drug, survival at 69 maybe 0.476? Let's see. Our quantile gave 69, SAS maybe 68? Could be due to censored time 68 being considered. If survival drops to 0.5 at 68 vs 69, difference.

We should compute median manually from our full table.

Let's craft corrected script.

Also need to ensure text file contains all numbers: we must output full table with all times.

We also need to ensure we output numbers like 0.9524 etc with 4 decimals, and times as integers.

We should also ensure we output summary numbers: Total, Failed, Censored etc.

We need to produce 308 numbers. Let's estimate: 47 rows * ~8 numbers per row = 376, but SAS outputs 308 numbers. So our full table should have maybe 47 rows? Let's see: 47 subjects, but distinct times maybe ~30? Each row has ~8 numbers, 30*8=240 plus summary etc =308.

So we need to generate ~30 rows per group? Actually 47 total distinct times across both groups maybe ~35.

We should implement manual and then output.

Also need to fix at-risk table: SAS at-risk table maybe at specific times (0,20,40,60,80,100...). Our breaks_atrisk using pretty may not match SAS. But at-risk numbers are not necessarily part of 308? Could be.

We should keep at-risk as before but ensure text includes it.

We need to ensure we write P020.lst with all numbers.

Let's produce corrected R script with manual KM.

