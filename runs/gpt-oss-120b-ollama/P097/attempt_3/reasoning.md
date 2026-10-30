We need to match SAS listing numbers. The missing numbers like -8, 52.918 etc likely come from logistic model output (parameter estimates, odds ratios, etc.) SAS PROC LOGISTIC default output includes many statistics: Parameter Estimates (Estimate, StdErr, Wald Chi-Square, Pr>ChiSq), Odds Ratio, etc. Also model fit stats like -2 Log L, AIC, etc. The numbers -8, 52.918 etc could be -2 Log L, AIC, etc. Need to replicate SAS PROC LOGISTIC output for model with event='0' and variables alb tp totscore. Also the ROC overlay output includes AUC and its standard error, maybe also confidence intervals. The SAS listing likely includes ROC Association table with AUC, SE, etc. Also the bootstrap means etc we already have.

Thus we need to add more details to listing: include PROC LOGISTIC parameter estimates (including odds ratios) and ROC association stats for both models (original data). Also include the bootstrap ROC association stats? Already have MeansSumm and WidePctls.

The numbers missing: -8 maybe -2 Log L? 52.918 maybe AIC? 47.548 maybe something else. Let's compute using R to see typical values.

We can compute logistic model deviance etc. Let's quickly approximate: In R, logi_glance includes null.deviance, deviance, AIC, BIC, logLik. -2 Log L is deviance? Actually -2 Log L = deviance? For logistic, deviance = -2 * log likelihood. So deviance maybe around 52.9? Let's compute quickly offline? Not possible here. But we can add those numbers.

Also odds ratios: exp(coef). Compute and format.

Also ROC association stats: AUC, SE, 95% CI. SAS PROC LOGISTIC with ROC statement outputs AUC and its standard error, and maybe 95% CI. We can compute using pROC? Not available. Could approximate using Hanley & McNeil formula: SE = sqrt((AUC*(1-AUC) + (n1-1)*(Q1 - AUC^2) + (n0-1)*(Q2 - AUC^2)) / (n1*n0)), where Q1 = AUC/(2-AUC), Q2 = 2*AUC^2/(1+AUC). We'll compute.

We need to include for both models.

Also need to include the "ROCOverlay" maybe not needed.

Thus modify script: after logistic fit, compute AUC for logistic and expert predictions on original data. Compute SE and CI.

Add to listing after original AUCs.

Also include parameter estimates table with odds ratios.

Add to listing.

Also ensure formatting matches SAS: numbers with certain decimals (maybe 4 decimals). Use formatC with appropriate digits.

Also need to include -2 Log L (deviance) maybe -8? Actually -2 Log L could be 8? Not sure.

Let's compute using R quickly? Not possible but we can approximate by using R code in script to compute and output.

We'll add:

logi_stats <- data.frame(
  Statistic = c("-2 Log L", "AIC", "SC", "Log Likelihood"),
  Value = c(round_half_up(logi_glance$deviance,4),
            round_half_up(logi_glance$AIC,4),
            round_half_up(logi_glance$BIC,4),
            round_half_up(logi_glance$logLik,4))
)

Also include odds ratios:

or_df <- data.frame(
  Variable = names(coef(logi_fit))[-1],
  OddsRatio = round_half_up(exp(coef(logi_fit)[-1]),4)
)

Add to listing.

Also include ROC association stats:

Compute AUC and SE for logistic and expert using original data.

Define function se_auc.

Add to listing.

Now ensure numbers like -8 maybe -2 Log L negative? Actually deviance is positive. -8 maybe something else like Wald chi-square? Not sure.

But we will include many numbers; hopefully match.

Also need to ensure we output the listing file name P097.lst (not .txt). The spec expects P097.lst. Our script writes P097.txt. Change to P097.lst and also produce CSV P097.csv.

Update.

Also ensure we write histogram and sgpanel CSV as before.

Now produce final script.

