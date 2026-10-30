Need to fix script to match SAS numbers. Missing numbers include 0,17,10,8,21,233,11.206,4.0033,94,-4.0033,-94,4135.35 etc. Need to understand SAS lifetest output details.

We need to reproduce SAS PROC LIFETEST output exactly. The previous script attempted but missed many numbers. Need to look at expected SAS output? We don't have PDF but can infer.

SAS lifetest with plots=survival (atrisk=0 to 330 by 30) and strata arm, time day*censor(1). It prints:

- Product-limit table per stratum with columns: day, survival, failure, survival SE, number failed, number left. With 4 decimals.

- Quartile estimates with 3 decimals.

- Mean and SE with 3 decimals.

- Summary of censored.

- Rank statistics, covariance matrix, test of equality.

We need to ensure numbers match SAS exactly. The missing numbers suggest our calculations differ.

We need to replicate SAS formulas precisely.

Key differences: SAS uses Greenwood formula for SE, but also uses specific handling for ties? Our km_table seems correct but need to verify.

Also need to compute mean and SE with SAS formula: SE = sqrt( m/(m-1) * sum A_i^2 * d_i / (n_i*(n_i - d_i)) ) where A_i is area from t_i to largest event time. We implemented but maybe off.

Also need to compute rank statistics: SAS log-rank and Wilcoxon. Our survdiff may not match SAS exactly due to handling of ties and weights. Need to implement SAS formulas manually.

SAS PROC LIFETEST rank tests:

For each distinct event time t_j, let d_j = total events at t_j, n_j = total at risk, d_{kj} = events in stratum k, n_{kj} = at risk in stratum k.

Expected e_{kj} = n_{kj} * d_j / n_j

Variance etc.

Log-rank weight w_j =1

Wilcoxon weight w_j = n_j

Then for each stratum k, statistic U_k = sum_j w_j * (d_{kj} - e_{kj})

Covariance matrix V_{kl} = sum_j w_j^2 * (n_{kj}/n_j)*(delta_{kl} - n_{lj}/n_j) * (n_j - d_j)/(n_j -1) * d_j   ??? Need formula.

For 2 groups, V = sum w_j^2 * n1j*n2j * d_j*(n_j - d_j) / (n_j^2*(n_j-1))

Then test chi-square = U' V^{-} U

For 2 groups, chi-square = U1^2 / V11

We need to compute exactly to match SAS.

Also need to compute -2Log(LR) test: SAS computes likelihood ratio test for homogeneity? Might be different.

Also need to ensure product-limit table includes all times including censored times with *.

Our km_table currently creates one row per distinct time, but SAS creates one row per distinct event/censor time? For times with both events and censored, SAS may create separate rows? Need to check.

Also need to ensure survival SE formatting 4 decimals, and numbers like 0,17 etc appear. The missing numbers include 0,17,10,8,21,233 etc which are likely number failed / number left values.

Our km_all has 30 rows (distinct times). But SAS may have 38 rows? Let's count: Arm1 has 15 events +2 censored =17 observations, but distinct times: 143,164,188,190,192,206,213,216,220,227.23,234,244,246,265,304 =15 distinct (188 duplicate, 216 has event+censored). So 15 rows. Arm2 has 19 events +2 censored =21 observations, distinct times: 142,156,163,198,204,205,232,233,239,240,261,280,296,323,344 =15 distinct (232 duplicate, 233 x4, 280 x2, 296 x2). So total 30 rows. That matches our 30. So why missing numbers 0,17 etc? Maybe SAS includes time 0 row with survival 1, failure 0, SE 0, number failed 0, number left 17 etc. Indeed SAS product-limit table includes a row at time 0 with survival 1. That would add numbers 0,17,21 etc. Our table starts at first event time, missing time 0.

SAS PROC LIFETEST prints at time 0: survival 1, failure 0, SE 0, number failed 0, number left = total at risk (17 for Arm1, 21 for Arm2). That explains missing 0,17,21.

Also need to include censored rows marked with *.

Also need to ensure quartile estimates: SAS prints 75,50,25 percentiles with point estimate and CI. Our calculation may be off.

Mean: SAS prints mean and SE with 3 decimals. Our mean calculation may be off due to integration method.

We need to implement exact SAS mean: restricted mean up to largest event time (304 for Arm1, 323 for Arm2). Area under curve.

Let's compute manually to verify.

We should also ensure we output all numbers in text file with correct formatting so that recall matches.

The feedback says 328 of 373 numbers found, missing 45 numbers. Likely missing time 0 rows (adds ~10 numbers), plus covariance matrix details, plus rank statistics details, plus mean etc.

We need to reconstruct full SAS output text structure.

We should look at typical SAS PROC LIFETEST output to replicate titles, footnotes, etc.

We need to ensure we output numbers with same decimals as SAS: survival 4 decimals, failure 4 decimals, SE 4 decimals, quartile 3 decimals, mean 3 decimals, percent censored 2 decimals, chi-square 4 decimals, etc.

Also need to ensure we output "Statistics for Table of A by B" not needed here.

We need to fix script minimally but add missing blocks.

We should implement manual rank test calculation to match SAS exactly, rather than using survdiff which may differ slightly.

Let's implement manual calculation for log-rank and Wilcoxon.

Steps:

- Get all distinct event times across both arms where at least one event occurs (d_j >0). For each time t_j, compute n_j = total at risk (day >= t_j), n1j, n2j, d_j, d1j, d2j.

- For each t_j, compute e1j = n1j * d_j / n_j, e2j = n2j * d_j / n_j

- For log-rank: w=1, U1 = sum (d1j - e1j), U2 = -U1

- For Wilcoxon: w = n_j, U1_w = sum n_j*(d1j - e1j) = sum (n_j*d1j - n1j*d_j) ??? Actually w*(d - e) = n_j*(d1j - n1j*d_j/n_j) = n_j*d1j - n1j*d_j

- Variance: For each j, var_j = w_j^2 * n1j*n2j * d_j*(n_j - d_j) / (n_j^2*(n_j -1))  (if n_j>1). For n_j=1, var=0.

- Total V = sum var_j

- Chi-square = U1^2 / V

- For Wilcoxon, V_w = sum w_j^2 * ... = sum n_j^2 * n1j*n2j * d_j*(n_j - d_j) / (n_j^2*(n_j-1)) = sum n1j*n2j * d_j*(n_j - d_j)/(n_j -1)

We need to compute these and compare to SAS.

Also need to compute covariance matrix: For 2 groups, matrix = [[V, -V],[-V, V]]

SAS prints covariance matrix with values like 4.0033, 94 etc? The missing numbers include 4.0033, 11.206, 94, 4135.35 etc. Those are likely variances.

Let's attempt to compute quickly via mental or we can approximate but need to code correctly.

Also need to compute -2Log(LR) test: SAS computes likelihood ratio test based on exponential? Actually PROC LIFETEST with no covariates, the -2Log(LR) is based on comparing survival curves via likelihood? Might be computed as 2* sum (observed * log(observed/expected))? Need to check SAS documentation: Test of Equality over Strata includes Log-Rank, Wilcoxon, and -2Log(LR). The -2Log(LR) is the likelihood ratio test assuming exponential survival? Let's see SAS docs: The -2Log(LR) test is based on the exponential likelihood. It may be computed differently.

We can try to compute -2Log(LR) as: For each stratum, estimate hazard lambda = events / total time at risk (person-time). Then LR test compares.

But we can also approximate using coxph LR as we did, but SAS value may differ.

We need to find SAS formula for -2Log(LR) in PROC LIFETEST: It is -2 * log likelihood ratio test for homogeneity, where likelihood is based on exponential distribution. The test statistic = 2 * sum_{k} (d_k * log(d_k / e_k)) where e_k is expected under null? Not sure.

Alternative: We can search memory: SAS PROC LIFETEST -2Log(LR) test is computed as: For each stratum, compute observed events O_k and expected? Actually for exponential, the test is based on total time.

Let's look up quickly (from knowledge): For exponential, the likelihood ratio test statistic is 2 * sum O_k * log( O_k / E_k ) where E_k = O * (T_k / T) where T_k is total time at risk in stratum k, T total. Something like that.

We need to implement to match SAS numbers. The missing numbers include 4135.35 which could be total time? Let's see: total time at risk for Arm1: sum of days? For Arm1: events 143+164+188+188+190+192+206+213+216+220+227.23+234+246+265+304 + censored 216+244 = 3740.23? Let's compute: sum events = 143+164=307, +188=495, +188=683, +190=873, +192=1065, +206=1271, +213=1484, +216=1700, +220=1920, +227.23=2147.23, +234=2381.23, +246=2627.23, +265=2892.23, +304=3196.23, +216=3412.23, +244=3656.23. So total time Arm1 =3656.23. Arm2: events 142+156+163+198+205+232+232+233+233+233+233+239+240+261+280+280+296+296+323 = let's sum: 142+156=298, +163=461, +198=659, +205=864, +232=1096, +232=1328, +233=1561, +233=1794, +233=2027, +233=2260, +239=2499, +240=2739, +261=3000, +280=3280, +280=3560, +296=3856, +296=4152, +323=4475, +204=4679, +344=5023. So total time Arm2=5023. Total =8679.23. Not 4135.35.

Maybe 4135 is something else.

We need to ensure we output all numbers that SAS outputs, even if our calculation differs, we need to match SAS numbers. The feedback says missing numbers include 4135.35, 11.206, 4.0033, 94, -4.0033, -94 etc. Those are likely from covariance matrices and rank statistics.

We need to compute rank statistics correctly to get those numbers.

Let's attempt to implement manual calculation in R and see what values we get, then adjust to match SAS.

We should write a quick mental script but we can't run here. We need to reason.

We should implement function to compute U and V exactly as SAS does, including handling of ties where d_j >1.

SAS uses formula for variance with (n_j - d_j)/(n_j -1) factor.

We have that.

Let's code manual calculation and output with 4 decimals.

Also need to include time 0 row.

Also need to ensure we output product-limit table with correct columns: SAS prints columns: Time, Survival, Failure, Survival Standard Error, Number Failed, Number Left. With 4 decimals for survival etc.

We need to ensure we output numbers with correct formatting so that text contains them.

The previous script's text had survival etc but missing time 0.

We should add time 0 row.

Also need to ensure we output "Quartile Estimates" with correct values. SAS prints for each stratum: Percent, Point Estimate, 95% Confidence Interval Transform, Lower, Upper. With 3 decimals.

We need to compute CI for quartiles using log-log transform as SAS does.

Our previous method for quartile CI used survival CI, which is correct (Brookmeyer-Crowley). Need to ensure we compute correctly.

Also need to output "Mean and Standard Error" with 3 decimals.

We should also output "Summary of the Number of Censored and Uncensored Values" with percent censored 2 decimals.

We need to ensure all numbers appear in text file as plain numbers without extra formatting that would hide them. The checker searches for numbers as strings in text files. So we need to ensure numbers appear exactly as SAS prints them, e.g., "11.206" not "11.2060". Need to match decimals.

We should format numbers with correct decimals: survival 4 decimals, mean 3 decimals, chi-square 4 decimals, etc.

We need to produce a corrected script that adds time 0 row and correct rank calculations.

We should also ensure we output covariance matrices with full square (2x2) including negative off-diagonals.

Our previous script did that but values may be wrong due to using survdiff var.

We should replace with manual calculation.

We also need to ensure we output "Rank Statistics" block with per stratum observed, expected etc.

Let's craft final script with manual rank test.

We also need to handle -2Log(LR) - we can compute using exponential LR formula: For each stratum k, let O_k = number of events, T_k = total time at risk (sum of observed times). Under null, expected hazard = O / T, expected events E_k = T_k * O / T. Then LR statistic = 2 * sum O_k * log(O_k / E_k) . Let's test: O1=15, O2=19, O=34, T1=3656.23, T2=5023, T=8679.23, E1=3656.23*34/8679.23=14.32, E2=19.68, Then LR =2*(15*log(15/14.32)+19*log(19/19.68))=2*(15*0.0464 +19*(-0.0352))=2*(0.696 -0.669)=0.054. Not large. So not matching expected chi-square ~2.3.

Maybe -2Log(LR) is different.

Alternative formula for -2Log(LR) in lifetest is based on product-limit likelihood? Could be 2* sum (O_k * log(O_k/E_k)) where E_k is from log-rank expected? That would be similar to above but with E from log-rank (which is sum e_{kj}). Let's compute E from log-rank: E1 = sum n1j*d_j/n_j. Need to compute. Could be around 17? Not sure.

We need to find SAS documentation for -2Log(LR) test: It is the likelihood ratio test derived from the Cox model? Actually SAS says: "-2Log(LR) test is based on the exponential distribution". So our exponential formula should be correct, but our computed LR is small (0.05) not matching log-rank 2.37. Could be SAS prints -2Log(LR) close to log-rank, maybe around 2.5? So our exponential LR should be similar magnitude, but we got 0.05, so maybe T_k is not sum of times but something else (like total follow-up time including censored? We did that). Let's compute more accurately: T1=3656, T2=5023, O1=15, O2=19, E1=14.32, E2=19.68, LR=0.054, very small. So not matching.

Maybe T_k is sum of event times only? Then T1=3196, T2=4475, T=7671, E1=3196*34/7671=14.16, E2=19.84, LR=2*(15*log(15/14.16)+19*log(19/19.84))=2*(15*0.0576+19*(-0.0432))=2*(0.864-0.821)=0.086. Still small.

So not.

Maybe -2Log(LR) is based on discrete likelihood: product over times of binomial likelihood? Then statistic = 2* sum [ d_{kj} log(d_{kj}/e_{kj}) + (n_{kj}-d_{kj}) log((n_{kj}-d_{kj})/(n_{kj}-e_{kj})) ] summed over j,k. That would be larger.

We need to implement that to get plausible value.

We can attempt to compute -2Log(LR) as likelihood ratio for homogeneity of survival curves using the hypergeometric likelihood? Actually the likelihood for each 2x2 table at each event time is binomial.

The LR test for homogeneity across strata at each time point: For each time j, the likelihood under null is binomial with p = d_j/n_j, under alternative p_k = d_{kj}/n_{kj}. Then LR_j = 2* sum_k [ d_{kj} log(d_{kj}/e_{kj}) + (n_{kj}-d_{kj}) log((n_{kj}-d_{kj})/(n_{kj}-e_{kj})) ] where e_{kj}= n_{kj}*d_j/n_j. Then total LR = sum_j LR_j.

This is the "Likelihood Ratio" test for 2x2 tables, which SAS might use for -2Log(LR).

Let's test approximate magnitude: For each time, if d_j=1, then term is small. Summing over ~30 times, total could be ~2-3, plausible.

We should implement this formula.

We need to ensure we handle cases where d_{kj}=0 then term 0*log(0) =0.

We should implement this and see if we get value that matches SAS missing numbers? The missing numbers include 4135.35 which is not a chi-square, maybe it's total time? Could be something else.

We need to ensure we output all numbers that SAS outputs, even if we can't perfectly match -2Log(LR), we should at least output a value with correct format so that numbers are found. But the checker checks for presence of SAS numbers in R text, not exact match of all numbers? It checks recall: 328 of 373 numbers found. So we need to increase recall to 1.0 (or close). The missing numbers are those not found, so we need to ensure our text contains those numbers. If our -2Log(LR) value is wrong, the SAS number for that test will be missing.

We need to try to compute -2Log(LR) correctly to match SAS.

We can search memory: SAS PROC LIFETEST -2Log(LR) test statistic formula: It is computed as 2 * sum_{i} (O_i * log(O_i/E_i)) where O_i is observed events in stratum i, E_i is expected events under null assuming exponential? Hmm.

Let's look up quickly from internet memory: In SAS documentation, the three tests are: Log-Rank, Wilcoxon, and Likelihood Ratio. The Likelihood Ratio test is based on the exponential distribution and is computed as: Chi-square = 2 * sum_{k} O_k * log( O_k / E_k ) where E_k = (T_k / T) * O, where T_k is total time at risk in stratum k. That's what we did and got 0.05, but maybe T_k is not sum of times but sum of something else like "exposure" = sum of survival times? We did that.

Why would SAS get larger value? Let's compute T_k differently: Maybe T_k is sum of all observed times (including censored) but we did that. Still small.

Let's compute O_k/E_k ratio: 15/14.32=1.047, 19/19.68=0.965, log ratios small, so LR small. So exponential LR will always be small if total times are similar to event counts ratio. So maybe SAS -2Log(LR) is not exponential but based on discrete likelihood as described, which yields larger value.

We should implement discrete LR as described, which will give larger value similar to log-rank.

We will implement both and see which yields plausible.

We need to produce final script that computes -2Log(LR) via discrete likelihood sum.

We should also ensure we output covariance matrices correctly.

Let's craft final script with manual calculations.

We need to add time 0 row to km tables.

We need to ensure we output all blocks with correct titles.

We should also ensure we output "Example K-M Plot" title.

We should ensure we write text file with all numbers formatted to match SAS.

We will produce final R script.

