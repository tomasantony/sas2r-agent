We need to match SAS PROC LIFETEST output. The numbers missing likely because SAS includes survival estimates at each distinct event time per stratum, but also includes cumulative number at risk etc. Our listing may be missing some rows (maybe SAS includes time 0 row). Also rounding digits: SAS default prints 4 decimal places? The missing numbers like 0.0476 etc correspond to survival probabilities at certain times maybe with 4 decimal places (0.0476). Our rounding to 4 decimals may be okay but maybe need 4 decimal places for survival and 4 for CI, but also need to include "Std_Err"? SAS LIFETEST default listing includes "Survival", "Std Err", "Lower 95% CI", "Upper 95% CI". The numbers missing like 0.0476 could be survival at time 3? Actually 0.0476 = 2/42? Not sure.

We need to replicate exactly SAS output. Let's think: PROC LIFETEST with data=adam.adtte, time AVAL * CNSR(1); strata TRT01A; plots=survival(atrisk). The default listing includes:

- Number of subjects, events, censored per stratum.
- Survival table: Time, N (number at risk), N_censor, Survival, Std Err, Lower 95% CI, Upper 95% CI.

But the numbers missing like 3, 15, 4 etc could be times (Time values). Our listing includes Time values from plot_data which are event times (unique times). However SAS includes also times where there are censored observations? Actually survival table includes each distinct time where an event occurs (not censored). So maybe we are missing some times because we used tidy(km_fit) which includes times for each step (including censored? I think tidy includes times where events happen, not censored). But SAS includes also times where there are censored? Not sure.

Also SAS prints survival at each distinct time, plus maybe at time 0 (Survival=1). Our listing missing time 0 row. That could explain missing numbers like 0.9524 etc (maybe survival after first event). Also numbers like 0.0476 (1 - 0.9524). So we need to include time 0 row with N_Risk = total per stratum, N_Event = 0, N_Censor = 0, Survival = 1.0, Std_Err = 0, Lower CI = 1, Upper CI = 1.

Also need to include rows for censored times? SAS includes them as separate rows? I think LIFETEST listing includes rows for each distinct event time, and also includes a row for censored times? Actually the "At Risk" table in plot includes risk numbers at each time point (including censored). But the listing may include "Censor" column.

Our tidy output includes N_Censor column, which we included. Good.

Missing numbers like 3, 4, 15 etc could be times where events occurred (e.g., day 3). So we need to ensure we capture all event times. tidy(km_fit) includes times where events happen, but also includes times where there are censored? I think it includes all times where the survival curve changes, which includes events and censored? Actually survfit updates at each event time; censored times do not change survival estimate, but they affect n.risk and n.censor. The summary includes times where there are events, but also includes times where there are censored if there are no events? I think summary includes all distinct times where either event or censor occurs, but survival estimate unchanged for censored only times. In R, summary(survfit) includes times where there are events, but also includes times where there are censored if there are no events? Not sure.

But missing numbers like 0.0476 (survival after first event maybe). So maybe we need to compute survival table manually to match SAS.

Simpler: Use summary(km_fit, times = NULL, extend = TRUE) which returns a list with time, n.risk, n.event, n.censor, surv, std.err, lower, upper. That includes all times (including censored). Let's use summary(km_fit, extend = TRUE) which includes times where there are censored but no event? Actually extend = TRUE adds a row at the end with time = max time and survival = 0? Not needed.

We'll generate a data frame from summary(km_fit) for each stratum.

Implementation: Use summary(km_fit, times = NULL, extend = FALSE) returns a list with components: time, n.risk, n.event, n.censor, surv, std.err, upper, lower. For each stratum, there is a separate component (if multiple strata). summary(km_fit)$time is a vector concatenated across strata? Actually summary returns a list with elements: time, n.risk, n.event, n.censor, surv, std.err, upper, lower, strata (if multiple). The vectors are concatenated in order of strata.

We can convert to data frame:

s <- summary(km_fit, extend = FALSE)
df <- data.frame(
  Time = s$time,
  N_Risk = s$n.risk,
  N_Event = s$n.event,
  N_Censor = s$n.censor,
  Survival = s$surv,
  Std_Err = s$std.err,
  Lower_CI = s$lower,
  Upper_CI = s$upper,
  Treatment = if (!is.null(s$strata)) s$strata else "Overall",
  stringsAsFactors = FALSE
)

But need to ensure ordering matches SAS: time ascending within each stratum, include time 0 row? summary does not include time 0. We can add row manually for each stratum: Time = 0, N_Risk = number of subjects in that stratum, N_Event = 0, N_Censor = 0, Survival = 1, Std_Err = 0, Lower_CI = 1, Upper_CI = 1.

Compute N_Risk0 per stratum: count rows in adtte per TRT01A.

Add row per stratum at top.

Also need to ensure rounding: SAS default prints 4 decimal places for survival and CI, and maybe 4 for Std_Err. Use round_half_up to 4.

Also need to ensure numeric formatting in text file matches SAS: numbers separated by spaces, maybe parentheses for percentages not needed.

Our text file currently prints columns with formatting using sprintf. Need to ensure width matches but not critical as long as numbers present.

Missing numbers like 0.0476 (which is 1 - 0.9524). That would be survival at time after first event maybe 0.9524. So we need to include those values.

Let's adjust script:

- Use summary(km_fit) to get full table.
- Add time 0 rows.
- Combine and round.

Also ensure ordering of Treatment factor levels same as earlier.

Implement.

Also need to write CSV of listing (P020.csv) with same columns as SAS listing: maybe includes columns: TRT, TIME, N_RISK, N_EVENT, N_CENSOR, SURVIVAL, STD_ERR, LOWER, UPPER. We'll keep same as we produce.

Now produce corrected script.

