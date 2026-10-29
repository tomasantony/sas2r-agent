The error: SAFFL not found in adlb. In SAS they join adlb a with adsl b on USUBJID, then filter b.SAFFL='Y'. In our code we inner_join adlb with adsl to bring SAFFL, but we used inner_join(adlb, adsl %>% select(USUBJID, SAFFL)). However adsl variable may be named SAFFL (character). The error occurs because after join, SAFFL column may be named SAFFL.x or something? Actually inner_join will add SAFFL from adsl, but if adlb also has SAFFL column? In adlb maybe not. But error says SAFFL not found in the pipeline before filter. That means the join didn't happen because we used inner_join before filter? In code: lb <- adlb %>% inner_join(adsl %>% select(USUBJID, SAFFL), by = "USUBJID") %>% filter(toupper(as.character(SAFFL)) == "Y", PARAMCD %in% param_list) ... The error occurs earlier: the pipeline before filter includes arrange(TRTAN...). Actually we call arrange after filter, but error occurs at filter step. So SAFFL column not present after join. Maybe adsl variable is named SAFFL (uppercase) but haven reads as factor? Actually haven reads character as character. Should be there. Could be that adsl dataset doesn't have variable SAFFL (maybe named SAFFL?). In SAS they used adsl with SAFFL flag. So should exist.

But error persists, maybe because adsl variable is named SAFFL (character) but after select, the column name becomes "SAFFL". Should be fine.

Maybe the issue is that adlb also has a variable named SAFFL, causing duplicate column names, and inner_join will suffix .x/.y. If adlb has SAFFL, then after join we get SAFFL.x and SAFFL.y. Then filter referencing SAFFL ambiguous leads to error. Let's check: In ADaM, adlb may have SAFFL variable as well. So we need to explicitly use SAFFL from adsl (e.g., SAFFL.y). Simpler: after join, rename the SAFFL column to SAFFL_adsl.

Implement: adsl_small <- adsl %>% select(USUBJID, SAFFL) %>% rename(SAFFL_adsl = SAFFL)

Then join and filter on SAFFL_adsl.

Also need to ensure that ABLFL, ATOXGR, BTOXGR are character; treat missing as "".

Also need to ensure that worst_grade_n conversion works when ATOXGR may be character with spaces.

Also need to ensure that when worst_grade_n is NA after conversion, we exclude.

Also need to ensure that we write both CSV and TXT; SAS expects .rtf but we produce .txt and .csv.

Now adjust script.

Also need to ensure that we use toupper on SAFFL_adsl.

Implement changes.

Also ensure that we use as.character for ABLFL etc.

Potential other errors: In step 4, we used filter(ABLFL == "Y") but ABLFL may be character; fine.

Now produce corrected script.