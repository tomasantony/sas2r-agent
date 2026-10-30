We need to fix errors. The arrange usage was wrong. We can simply use arrange directly: lb_qc <- lb %>% arrange(USUBJID, PARAM, LBDTC, .by_group = FALSE) but need missing first. dplyr arrange puts NA last by default. To mimic SAS missing first, we can use arrange with desc(is.na(...))? Actually SAS sorts missing first, so we need to order NA first. In dplyr, we can use arrange with `desc(is.na(USUBJID))`? Wait: is.na returns TRUE for missing; we want those first, so we can use `desc(!is.na(USUBJID))`? Simpler: use base order: lb_qc <- lb[order(lb$USUBJID, lb$PARAM, lb$LBDTC, na.last = FALSE), ].

Thus replace arrange with base ordering.

Also compute baseline: SAS logic: retain BASE; if first.PARAM then BASE = .; if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL; if lbdt < rfstdt and AVAL = BASE then ABLFL = "Y"; else ABLFL = ""; if not missing(BASE) then CHG = AVAL - BASE;

Our compute_baseline currently sets BASE as last AVAL before rfstdt? Actually SAS sets BASE to AVAL for each observation where lbdt < rfstdt and not missing AVAL, overwriting previous. So for each row, BASE is the most recent pre-baseline AVAL (including current). That's what our loop does: if condition true, set cur_base to AVAL, then assign BASE = cur_base. That matches.

But we need to reset BASE at first.PARAM. In our loop we didn't reset at first.PARAM. We need to detect first row per group (by PARAM) and set cur_base <- NA before loop. Since we group_by USUBJID, PARAM, the first row of each group is first.PARAM. So before loop, set cur_base <- NA. That's fine.

But we also need to ensure that for rows where condition not met, BASE remains previous value (including possibly NA). That's done.

Now need to ensure ABLFL logic: if lbdt < rfstdt and AVAL = BASE then "Y" else "". Our code uses if not missing lbdt/rfstdt and condition and AVAL == BASE. That's fine.

Now need to ensure date parsing: LBDTC and RFSTDTC are character strings in yymmdd10 format. Use ymd from lubridate; but ymd expects "YYYY-MM-DD". For "20200101"? Actually yymmdd10 format is "YYYY-MM-DD"? Wait SAS yymmdd10. expects "yyyy-mm-dd". So LBDTC likely "2020-01-01". ymd can parse that. Use ymd(LBDTC). Good.

Now fix sorting: Use base order.

Also need to ensure we keep variable order same as SAS output. SAS dataset adlb_qc will have all variables from lb plus RFSTDTC, TRT01P, plus new variables ABLFL, CHG, BASE? Actually SAS code: merge lb_qc and adsl_qc (keep USUBJID RFSTDTC TRT01P). So resulting dataset includes all variables from lb plus those three, plus new variables created (BASE, ABLFL, CHG). Then drop lbdt rfstdt. So final variables: all original lb variables (including LBDTC, AVAL, etc), plus RFSTDTC, TRT01P, BASE, ABLFL, CHG. Order: variables from lb first (in original order), then RFSTDTC, TRT01P, then BASE, ABLFL, CHG. Our select currently drops lbdt rfstdt but retains all others. The order may be changed due to dplyr operations. We can reorder columns to match original order: get names of lb, then add RFSTDTC, TRT01P, then BASE, ABLFL, CHG.

Implement: final_df <- adlb_qc %>% select(all_of(c(names(lb), "RFSTDTC", "TRT01P", "BASE", "ABLFL", "CHG")))

But note that after merge, there may be duplicate columns (e.g., USUBJID appears in both). Keep one.

Now write CSV.

Also need to ensure missing values written as empty strings.

Now produce corrected script.

