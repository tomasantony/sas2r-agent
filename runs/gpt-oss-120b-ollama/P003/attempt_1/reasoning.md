We need to translate SAS program to R script that reproduces outputs. The declared output type: table, P003.lst (listing text, 104 numbers). Likely the final listing is a concatenated table of results? The SAS program creates many tables: DENOM, AEPTD, AEPTA, RESULTPT, AESOCD, AESOCA, RESULTSOC, AESUBD, AESUBA, RESULTSUB, BACKBONE, HADEVENT, FISHER, F1PT, F2PT, BACKBONE1, HADEVENT1, FISHER1, F1SOC, F2SOC, BACKBONE2, HADEVENT2, FISHER2, F1SUB, F2SUB. The final output likely is the listing of all these tables? But the declared output is a table/listing with 104 numbers. Could be the final table RESULTPT? Not sure.

Simplify: produce all tables as CSV and also produce a combined listing text file P003.lst containing all tables? The spec: each output dataset as CSV. each table or listing additionally as plain-text rendering (txt) that contains same numbers the SAS output shows (counts, percentages...). So we need to output each table as CSV and also a txt rendering. The listing P003.lst maybe is a concatenated report of the final results (maybe RESULTPT, RESULTSOC, RESULTSUB). We'll create a combined table that stacks them with appropriate columns.

Given limited time, produce R script that reads ADSL and ADAE from haven (xpt files). The path: rawadam/adam.sas7bdat? Actually earlier they loaded xpt files: sdtmnew/ae.xpt, suppae.xpt, adamnew/adsl.xpt, adae.xpt. So the data files are .xpt in raw directories. We'll read using haven::read_xpt.

Paths: rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam ; rawsdtm = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm

Thus ADSL at rawadam/adsl.xpt ; ADAE at rawadam/adae.xpt.

Implement steps using dplyr.

Need round-half-up for percentages: SAS format 8.1 means one decimal, rounding half away from zero. Use custom function round_half_up(x, digits). Use base::round with .5 upward? R's round uses "round to even". We'll implement: function(x) sign(x)*floor(abs(x)*10^digits + 0.5)/10^digits.

Compute percentages: (count/denom)*100.

Construct RESULT string: paste0(count, " (", sprintf("%.1f", percent), "%) [", events, "]").

Make sure count and events are integer.

Create tables as per SAS.

For PROC FREQ with exact Fisher test: need to compute Fisher's exact test for each AEDECOD (or AEBODSYS) and each comparison (high dose vs placebo: TRTAN 0 vs 2; low dose vs placebo: 0 vs 1). SAS outputs dataset FishersExact with columns: Name1, Name2, etc. We need to replicate. In SAS, ODS OUTPUT FishersExact=F1PT(where=(Name1='XP2_FISH')). The output dataset includes columns: Name1, Name2, DF, Prob, etc? Actually Fisher's exact test output includes columns: Name1 (test name), Name2 (maybe variable), DF, Prob, etc. But replicating exactly may be complex. Maybe we can skip? But output expected is a table listing numbers (104 numbers). Likely includes p-values from Fisher tests.

Simplify: compute fisher.test for each AEDECOD and each dose comparison, extract p-value, and create a data frame with columns: AEDECOD, TRTAN_GROUP (e.g., "0_vs_2"), P_VALUE. Similarly for SOC level and subject level.

We'll output these as CSV and txt.

Implement loops.

Finally write all outputs to OUT_DIR.

Implement helper write_table function to write csv and txt (using write.table with row.names=FALSE, col.names=TRUE, sep="\t").

For txt rendering, we can use format similar to SAS: maybe just write.table with format.

Implement.

Now produce final R script.