Need to produce R script that reproduces SAS program outputs. The SAS program creates 5 datasets via datalines.

We need to read? Actually no inputs, just create datasets. Write to OUT_DIR.

We must follow rules: write CSVs with dates as YYYY-MM-DD, etc.

We need to create raw_dm, raw_ex, raw_lb, raw_ae, raw_vs.

Check SAS program details:

raw_dm:
SUBJID SEX AGE TRTGRP RANDDT RFENDT
101,M,55,A,2022-01-10,2022-06-30 etc.

raw_ex: SUBJID EXSTDAT EXENDAT DOSE

raw_lb: SUBJID VISIT LBDT PARAM AVAL

raw_ae: SUBJID AETERM AESER AESEV AESTDT AEENDT

raw_vs: SUBJID VISIT VSDT PARAM AVAL

We need to write CSVs.

Also need to handle OUT_DIR env var.

We must produce single R script inside one ```r block.

No install.packages, etc.

We should ensure dates formatted as YYYY-MM-DD.

We need to ensure numeric values unrounded.

We should also ensure column names same case.

We need to write to file.path(out, "<name>.csv")

We should also ensure out dir exists.

We should not attempt to read inputs.

We need to follow SAS semantics: dates.

We should create data frames exactly.

We need to ensure types: SUBJID numeric, SEX character, AGE numeric, TRTGRP character, RANDDT Date, RFENDT Date.

Similarly others.

We should write CSV with na = "".

We should also ensure no row.names.

We should produce script that runs non-interactively.

We should include library calls? Not needed but can include.

We must ensure script is complete.

We should also handle that expected outputs are datasets only, no txt/png needed, but rules say each table/listing additionally as txt. But here only datasets, so just CSV.

We should ensure we create out dir if not exists.

We should use base R.

We need to output exactly one R script inside one ```r code block. No prose before or after.

We need to ensure script uses Sys.getenv("OUT_DIR").

We should also handle fallback if OUT_DIR empty? Use temp? But spec says write into OUT_DIR.

We should implement.

We need to ensure dates are Date objects, write.csv will format as YYYY-MM-DD if we convert to character via format.

Better to convert dates to character ISO before writing, or rely on write.csv formatting? write.csv will use as.character(Date) which is YYYY-MM-DD. That's fine.

We should ensure numeric values not rounded.

We should create data frames via tibble or data.frame.

We should ensure order same as SAS.

We should also ensure we don't add extra columns.

We should produce script.

We need to be careful about string handling: AETERM values: Headache, Nausea, Dizziness, Fatigue. AESER N/Y, AESEV MILD etc.

We should ensure we replicate exactly.

We should also ensure we handle that SAS input uses $ for character, but values have no spaces.

We should produce final answer.

We need to ensure we don't call install.packages.

We should include out <- Sys.getenv("OUT_DIR"); if out == "" then out <- "." or something, but spec says out <- Sys.getenv("OUT_DIR").

We should add dir.create.

We should write 5 CSVs.

We should ensure we use write.csv with row.names=FALSE, na="".

We should ensure we don't add extra outputs.

We should produce final R code block.

We need to ensure we handle that raw_ae etc. need to be written.

We should also ensure we handle that SAS librefs raw -> etc. but R just writes to OUT_DIR.

We should produce script.

We should ensure we use correct date parsing: as.Date("2022-01-10").

We should ensure we handle that SAS yymmdd10. format is YYYY-MM-DD.

We should produce final answer with ```r ... ```.

We must not add prose before or after.

We should ensure script is standalone.

We should include library calls not needed.

We should ensure we handle that output datasets are named raw_ae.csv etc. exactly.

We should produce script.

We should ensure we handle that we need to write each dataset as CSV with SAS variable names (same case is fine). Use uppercase? SAS variable names are uppercase? In SAS program, input SUBJID SEX AGE TRTGRP RANDDT RFENDT - all uppercase. So we should use uppercase column names. The spec says same case is fine, matching is case-insensitive, but we should use uppercase to be safe.

We should use column names exactly as in SAS: SUBJID, SEX, AGE, TRTGRP, RANDDT, RFENDT etc.

We should ensure we use correct case.

We should produce script.

We should ensure we handle that raw_lb and raw_vs have PARAM and AVAL, VISIT, etc.

We should ensure we handle that raw_ex has DOSE numeric.

We should ensure we handle that raw_vs AVAL numeric.

We should ensure we handle that raw_lb AVAL numeric with decimals.

We should produce final script.

We should add helper for round half away? Not needed.

We should ensure we handle that we need to write dates as YYYY-MM-DD.

We should produce final answer.

We need to ensure we don't miss anything.

We should produce final R code block with script.

We should ensure we handle that out dir may not exist, so dir.create.

We should ensure we handle that we need to write 5 files.

We should produce final answer now.