We need to fix SRCDOM values. In PFS and TTR we set SRCDOM blank, but SAS had values: For PFS rows, SRCDOM is either 'RS' or 'ADSL' depending on source. In SAS code: In adtte_pfs, they set SRCDOM = 'RS' when event is progression, SRCDOM = 'ADSL' when death, and also SRCDOM = 'ADSL' when censored. So we need to replicate that logic.

Similarly for adtte_os, SRCDOM is always 'ADSL' (already set). For adtte_ttr, SAS code didn't set SRCDOM; they didn't include SRCDOM variable in that data step. In SAS code for adtte_ttr, they didn't define SRCDOM, so it will be missing (blank). But the mismatch shows SRCDOM missing (nan) vs SAS maybe blank? Actually SAS dataset adtte_ttr likely has SRCDOM missing (blank). The mismatch shows only adtte_pfs and adtte (overall) have SRCDOM missing. So we need to set SRCDOM correctly for adtte_pfs and overall adtte (which combines). Also for adtte_os we already set SRCDOM = "ADSL". Good.

Implement SRCDOM logic in adtte_pfs: Determine source domain based on which event used.

We have PROGDT and DTHDT. In SAS: if both present, compare dates; if PROGDT <= DTHDT then ADT=PROGDT, EVNTDESC='Disease Progression', SRCDOM='RS'; else ADT=DTHDT, EVNTDESC='Death', SRCDOM='ADSL'. If only PROGDT, SRCDOM='RS'. If only DTHDT, SRCDOM='ADSL'. If censored, SRCDOM='ADSL'.

So modify adtte_pfs mutate accordingly.

Also for adtte (combined) we need SRCDOM values preserved.

Implement in adtte_pfs:

mutate SRCDOM = case_when(
  !is.na(PROGDT) & !is.na(DTHDT) & PROGDT <= DTHDT ~ "RS",
  !is.na(PROGDT) & !is.na(DTHDT) & PROGDT > DTHDT  ~ "ADSL",
  !is.na(PROGDT)                                   ~ "RS",
  !is.na(DTHDT)                                    ~ "ADSL",
  TRUE                                            ~ "ADSL"
)

Also ensure SRCDOM column is character, not factor.

For adtte_ttr, keep SRCDOM blank (empty string). Already set "".

Now also need to ensure ASEQ starts at 1 per subject, but SAS code: they set ASEQ = 0 at first USUBJID then ASEQ+1 each row, so first row per subject gets 1? Let's check: In SAS: if first.USUBJID then ASEQ = 0; ASEQ + 1; So for first row, ASEQ becomes 1. For second row, ASEQ increments to 2, etc. So our row_number() gives 1,2,... correct.

Now listing: need to match formatting numbers (e.g., decimals). The listing mismatches many numbers; likely due to formatting of numeric columns (e.g., AVAL, CNSR). SAS PROC PRINT default prints numeric with default width maybe 12? But the mismatch numbers like '22', '9', '10' etc. Might be because we omitted some columns in listing? The listing includes many columns; we need to match exactly SAS PROC PRINT output. The SAS PROC PRINT noobs prints all variables in dataset order. The dataset adtte has columns: STUDYID USUBJID TRTP TRTPN AGE AGEGR1 SEX RACE SAFFL ITTFL PARAMCD PARAM PARAMN STARTDT ADT AVAL CNSR EVNTDESC CNSDTDSC SRCDOM ASEQ (maybe also AVAL_M?). In SAS they didn't include AVAL_M in PROC PRINT (since after adding label they didn't drop). Actually after adding AVAL_M, they didn't drop; PROC PRINT will include AVAL_M as last variable. So listing should include AVAL_M column too. Our listing omitted AVAL_M. That explains missing numbers. Also order may differ.

We need to include AVAL_M in listing, after ASEQ maybe. Also ensure numeric formatting matches SAS default (no extra decimals). SAS prints numeric with default format (BEST12.) maybe. Our write.table will output numbers as is, which may include many decimals (e.g., 9.0401). The mismatched numbers like 9.0401 suggests AVAL_M values printed with many decimals. SAS likely printed with default format (maybe 9.0401 appears? Actually SAS may have printed AVAL_M with default BEST12., which could show many decimals). But mismatch includes numbers not found: maybe because we omitted AVAL_M, causing shift.

Let's include AVAL_M column.

Also ensure missing values are blank not "NA". write.table with na = "" already.

Update listing_df to include AVAL_M.

Also ensure column order matches dataset order. Let's get column order from adtte after creation: we can use colnames(adtte). We'll just select in that order.

So modify listing_df:

listing_df <- adtte %>%
  mutate(
    STARTDT = fmt_sas_date(STARTDT),
    ADT = fmt_sas_date(ADT)
  ) %>%
  select(all_of(colnames(adtte)))   # preserve order

Now write.table.

Also ensure SRCDOM values are character not factor; dplyr will keep as character.

Now adjust adtte_pfs SRCDOM logic.

Implement.

Also ensure adtte_os SRCDOM already set.

Now produce final script.