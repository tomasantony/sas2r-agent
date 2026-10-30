The error again due to comment line with non-ASCII dash? Actually the error "unexpected symbol in: ... _dsdt_raw" indicates that line continuation broken due to comment line with special character (en dash) maybe causing parsing issue? In R, comments start with #, but the line "# (SAS date value = days since 1960‑01‑01)" contains a non-ASCII hyphen (U+2011?). That shouldn't cause syntax error. The error occurs because the line before comment didn't end with a comma? Let's examine mutate block:

mutate(
    STUDYID = "CDISC-01",
    DOMAIN  = "DS",
    USUBJID = str_c(STUDYID, "-", str_trim(SUBJ_ID)),
    DSSEQ   = 1L,
    DSCAT   = "DISPOSITION EVENT",
    DSSCAT  = "STUDY PARTICIPATION",
    DSTERM  = str_to_upper(str_trim(DISPOSITION_REASON)),
    DSDECOD = DSTERM,
    EPOCH   = if_else(
                str_to_upper(str_trim(DISPOSITION)) == "SCREEN FAILURE",
                "SCREENING",
                "FOLLOW-UP"
              ),
    # ----------------------------------------------------------------
    # Handle STUDY_END_DAT which may be character (date string) or numeric
    # (SAS date value = days since 1960‑01‑01)
    _dsdt_raw = STUDY_END_DAT,
    _dsdt = case_when(
      is.character(_dsdt_raw) ~ parse_any_date(_dsdt_raw),
      is.numeric(_dsdt_raw)   ~ as_date(_dsdt_raw, origin = "1960-01-01"),
      TRUE                     ~ as_date(NA)
    ),
    DSSTDTC = if_else(!is.na(_dsdt),
                      format(_dsdt, "%Y-%m-%d"),
                      "")
  ) 

Potential issue: In case_when, the RHS as_date(NA) is not valid; need as_date(NA) but as_date expects numeric? Could use as_date(NA_real_). But syntax error earlier.

The error "unexpected symbol in: ... _dsdt_raw" suggests that R parser sees something before that line not closed, maybe missing comma after previous line. The previous line is comment "# (SAS date value = days since 1960‑01‑01)" which is fine. The line before comment is "# Handle STUDY_END_DAT which may be character (date string) or numeric". That's fine.

But maybe the comment line contains a non-ASCII hyphen causing encoding issues? Usually fine.

But the error points to line with "_dsdt_raw". Could be that we used a variable name starting with underscore inside mutate; that's allowed. However, maybe the previous line "# (SAS date value = days since 1960‑01‑01)" contains a non-ASCII hyphen that R treats as part of comment? Still comment.

Maybe the issue is that we used a line continuation with a stray backtick? Not.

Let's replicate minimal: In R script, after comment lines, we have "_dsdt_raw = STUDY_END_DAT," which is fine.

But the error "unexpected symbol in: "    # (SAS date value = days since 1960‑01‑01)
    _dsdt_raw"" suggests that the parser thinks the comment line is not a comment, maybe because the dash is not a hyphen but a Unicode "non-breaking hyphen"? Actually comment starts with "#", everything after is comment, regardless of characters. So not.

Maybe the issue is that we used a non-ASCII character earlier in the script causing file encoding issues leading to parse error. The dash in "1960‑01‑01" is a non-breaking hyphen (U+2011). That could cause the parser to think the comment line ends earlier? No, comment includes everything after #, so still comment.

But the error occurs at line start of "_dsdt_raw". Could be that we have a stray invisible character before underscore, like a zero-width space, causing syntax error? Possibly.

Simplify: Remove comment lines with special characters, and rename variable without leading underscore to avoid confusion. Use dsdt instead of _dsdt_raw.

Let's rewrite mutate block without leading underscores and simplify comments.

Also need to ensure case when numeric date column is read as Date already (readr parsed as date). In raw_ds, STUDY_END_DAT column is date type (as shown in console). So is.numeric will be FALSE, is.character FALSE, case_when will go to TRUE -> as_date(NA). But we need to keep date if it's already Date. So we can just use if_else(is.na(STUDY_END_DAT) etc. Actually we can treat _dsdt = as_date(STUDY_END_DAT) if it's Date, else parse.

Simplify: dsdt <- if (inherits(STUDY_END_DAT, "Date")) STUDY_END_DAT else parse_any_date(as.character(STUDY_END_DAT))

Implement using if_else with is.na.

We'll avoid leading underscores.

Also need to ensure output listing text includes numbers as in SAS. The SAS listing prints first 12 rows of ds with default PROC PRINT formatting (character values as is). Our text file currently writes header and rows separated by two spaces. That should contain numbers like DSSEQ = 1 etc. Should be fine.

But earlier the script didn't produce any files because it errored before writing.

Now fix script.

Also ensure we write ds.csv correctly.

Let's produce final script.

