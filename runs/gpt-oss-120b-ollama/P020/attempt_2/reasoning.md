We need to match SAS PROC LIFETEST output. The SAS output likely includes number at risk, number of events, survival probability at each distinct time, plus maybe cumulative hazard? The numbers missing include 3, 0.9524 etc. Our rounding may be off: SAS default prints survival with 4 decimal? The example numbers: 0.9524, 0.0476, 0.0465, 0.9048, 0.0952, 0.0641, 0.8571, 0.1429, 0.0764, 0.8095. Those are fractions like 20/21 etc. So need more precision (4 decimal). Also need to include at each time point maybe cumulative survival after each event. Our rounding to 3 decimals lost those. Also need to include number censored? The SAS output may show "Number of Censored" column. Also maybe "Survival (Std Error)" etc.

We need to replicate PROC LIFETEST default listing. Let's recall default PROC LIFETEST output: It prints "Product-limit survival estimate" table with columns: Time, Number at risk, Number of events, Survival Estimate, Std Error, 95% Confidence Limits (Lower, Upper). Also for each stratum. The default prints survival with 4 decimal places, std error with 4, CI with 4. So we need to include Std Error column.

Our tidy output from broom includes std.error? Actually broom::tidy(survfit) returns columns: time, n.risk, n.event, n.censor, estimate, std.error, conf.low, conf.high, strata. So we need to include std.error.

Also need to include number censored column maybe. SAS default includes "Number Censored". So we need to include n.censor.

Thus listing should have columns: Treatment, Time, N_Risk, N_Event, N_Censor, Survival, Std_Err, Lower_CI, Upper_CI.

Numbers like 3 maybe number at risk or events.

Also need to ensure rounding half-up to 4 decimal for survival, std error, CI.

Also need to ensure ordering same as SAS: time ascending, missing first? Not needed.

Also need to ensure formatting of text file matches SAS: maybe columns separated by spaces, with parentheses for percentages? Not needed for survival.

Our text file currently only includes limited columns.

We need to produce text file with all numbers.

Let's adjust.

Also need to ensure that we include overall stratum? SAS with STRATA TRT01A will produce separate tables per treatment, not overall. So we should not include overall. In broom tidy, overall appears as NA strata? Actually when using strata, each level gets separate rows, no overall. So fine.

We need to format each row similar to SAS: maybe "Treatment   Time   N_Risk   N_Event   N_Censor   Survival   Std_Err   Lower_CI   Upper_CI". Use fixed width.

Implement rounding half-up to 4 decimals for survival, std error, CI.

Also need to ensure numbers like 0.9524 appear (4 decimals). So rounding to 4.

Implement.

Also need to ensure that we include N_Censor column.

Update listing_tbl accordingly.

Also need to adjust text rendering to include those columns.

Also need to ensure that we write CSV with all columns.

Let's modify script.

Also need to ensure that factor levels ordering matches SAS: uppercase before lowercase, missing first. Already done.

Now implement.

