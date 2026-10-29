Need to fix error: names(adpp_sum_t)[names(adpp_sum_t) == "_PARAM"] <- "_PARAM" attempt to set attribute on NULL. Means adpp_sum_t is NULL? Actually adpp_sum_t_list maybe empty? Or adpp_sum_t is NULL because do.call(rbind, ...) with data.frames that have check.names=FALSE but column name _PARAM maybe converted to X_PARAM? In R, data.frame with check.names=FALSE still? But we used `_PARAM` as column name via data.frame(..., `_PARAM`=param, ...) but R may mangle? Actually data.frame with check.names=FALSE should keep _PARAM. But we used names(adpp_sum_t)[names(adpp_sum_t)=="_PARAM"] <- "_PARAM" which is redundant. The error indicates names(adpp_sum_t) is NULL, meaning adpp_sum_t is NULL. That would happen if adpp_sum_t_list is empty or do.call returns NULL. Why would adpp_sum_t_list be empty? Possibly adpp_sum has 0 rows due to filtering? Let's check filtering: PKFN==1, ANL01FN !=1, PARAMCD in ..., PPSTRESN not NA, compress(PPCATL)=='ANAL2'. Maybe PPCATL values have spaces? compress removes spaces. But maybe PPCATL is "ANAL2" already. Could be case sensitivity? Also PPSTRESN vs AVAL? In SAS they filter PPSTRESN ne . but we used PPSTRESN. Might be missing column? Also PARAMCD values maybe lowercase? Check.

Also adpp_raw names toupper, but values remain as original case. PARAMCD values likely "AUCINFO" etc uppercase. Should be fine.

But why adpp_sum_t is NULL? Let's see code: adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), ...) If nrow(adpp_sum)==0, then seq_len(0) is integer(0)? Actually seq_len(0) returns integer(0) in R, so lapply over integer(0) returns list(). Then do.call(rbind, list()) returns NULL. Then names(NULL) is NULL, error.

So adpp_sum is empty. Need to debug filtering.

Potential issues: PKFN, ANL01FN are numeric but haven labelled maybe character? We converted labelled to character via as.character, which would make PKFN character "1". Then filter PKFN ==1 would compare character "1" ==1 -> TRUE? In R, "1"==1 is TRUE due to coercion? Might be TRUE. But ANL01FN !=1 with character? Might still work.

But we did mutate across where haven::is.labelled, haven::zap_labels then mutate across where haven::is.labelled, as.character . Actually after zap_labels, is.labelled is FALSE, so second mutate does nothing. So numeric stays numeric. Good.

But we also did names toupper, but haven::read_xpt may produce lowercase names? Actually haven preserves original case, which is uppercase. So fine.

Check PPSTRESN: maybe column is PPSTRESN but we filter !is.na(PPSTRESN). Could be that PPSTRESN is character? No.

Another possibility: compress(PPCATL)='ANAL2' - PPCATL may have trailing spaces, but gsub(" ", "", PPCATL) == "ANAL2" should work. However PPCATL might be "ANAL2 " with maybe other characters? Should still match.

But maybe ANL01FN ne 1 filter excludes all? In SAS, ANL01FN ne 1 means not equal 1, but maybe ANL01FN is missing for some? In R, NA !=1 is NA, filter will drop NA rows (since filter requires TRUE). SAS where ANL01FN ne 1 would include missing? In SAS, missing is considered less than 1, so missing !=1 is true. In R, NA !=1 is NA, filtered out. Could cause difference but not empty.

Let's check actual data: Need to inspect adpp.xpt quickly mental? Could be that PKFN is 1 for all? ANL01FN maybe 1 for some? But should have some rows.

Alternative: paramcd filter: we used PARAMCD %in% c('AUCINFO',...) but original SAS paramcd in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL') - note SAS is case-insensitive? But values are uppercase. Should match.

Maybe PPSTRESN column is named PPSTRESN but we filtered on PPSTRESN, but SAS also has AVAL? In adpp, AVAL is derived from PPSTRESN? But they filter PPSTRESN ne . and later use AVAL. Could be PPSTRESN is numeric but AVAL is also numeric. Filtering on PPSTRESN not missing should be similar to AVAL not missing.

Could be that adpp_path not found and we used wrong file? But we found file.

We need to make script robust: handle empty case, handle NA in ANL01FN, handle column name case, handle PPCATL compress.

Also fix the names assignment error: don't do that line, just ensure column name is correct. Use tibble or data.frame with check.names=FALSE properly.

Also need to ensure adpp_sum_t creation handles empty.

We should also handle reading: haven::read_xpt may produce tibble with lower case names? Let's not toupper names blindly; instead normalize to upper via rename.

Better to inspect actual file existence: The error log shows script failed at that line, not earlier, so adpp_path was found, adpp_raw read, adpp filtered to 0 rows.

We need to debug filtering step: Let's add fallback: if nrow(adpp)==0, try less strict filtering to see.

We should implement filtering that mimics SAS exactly: SAS where PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2'

In SAS, compress removes spaces by default, but also maybe other? We used gsub(" ", "", PPCATL). That's equivalent to compress with default (removes blanks). Good.

But PPCATL may be "ANAL2" with no spaces, so gsub does nothing.

Potential issue: PPCATL column may be named PPCAT? No.

Let's look at ADPP structure: In CDISC pilot ADPP, variables include USUBJID, PPTESTCD, PPCAT, PPCATL? Actually PPCATL maybe? Let's assume exists.

We should make filtering case-insensitive for PPCATL compress: toupper(gsub(...)) == "ANAL2"

Also handle ANL01FN: SAS where ANL01FN ne 1 includes missing. In R, we should do is.na(ANL01FN) | ANL01FN !=1

Also PKFN: maybe PKFN is character "1" or numeric 1. Use as.numeric.

Also PPSTRESN: maybe named PPSTRESN but could be PPSTRESN is character? Use !is.na(PPSTRESN) & PPSTRESN != "".

Also PARAMCD: ensure uppercase.

We should add debugging: if adpp_sum empty, try to log unique values.

But for final script, we need to ensure it produces output even if filtering yields 0 rows? But expected output has 132 numbers, so filtering must yield rows.

We need to fix filtering to be more permissive: maybe PPCATL compress equals ANAL2 but PPCATL has value "Analyte 2"? No.

Let's search memory: In phuse-scripts, ADPP dataset for P035: where PKFN=1 and ANL01FN ne 1 and paramcd in (...) and PPSTRESN ne . and compress(PPCATL)='ANAL2' . PPCATL maybe contains "ANAL2" exactly.

Could be that PPCATL is actually "ANAL2" but with leading/trailing spaces, gsub(" ", "", ...) works.

Another possibility: ANL01FN ne 1 - ANL01FN is flag for period 1? Maybe all rows have ANL01FN=1? Then filter would exclude all? But SAS output exists, so not.

We should implement filtering step with explicit handling of NA and type.

Also need to handle PPTESTCD vs PARAMCD: In ADPP, PARAMCD is derived from PPTESTCD? The SAS code uses paramcd in (...) but also uses PPTESTCD for _PARAM. So PARAMCD should be same as PPTESTCD? Might be.

We should also check that adpp_raw column names after haven::read_xpt are uppercase but haven may convert to lower? Let's check: haven::read_xpt preserves names as in file, which are uppercase. So toupper is okay but if already uppercase, fine. However we did names(adpp_raw) <- toupper(names(adpp_raw)) which will make them uppercase even if they were lower. Good.

But we also need to handle that haven may import numeric columns as double, character as character.

We should rewrite filtering to be robust:

adpp <- adpp_raw %>%
  mutate(
    PKFN_num = suppressWarnings(as.numeric(PKFN)),
    ANL01FN_num = suppressWarnings(as.numeric(ANL01FN)),
    PARAMCD_up = toupper(as.character(PARAMCD)),
    PPCATL_comp = toupper(gsub("[[:space:]]", "", as.character(PPCATL))),
    PPSTRESN_num = suppressWarnings(as.numeric(PPSTRESN))
  ) %>%
  filter(
    !is.na(PKFN_num) & PKFN_num == 1,
    is.na(ANL01FN_num) | ANL01FN_num != 1,
    PARAMCD_up %in% c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL'),
    !is.na(PPSTRESN_num),
    PPCATL_comp == "ANAL2"
  )

Also need to handle PPSTRESN vs AVAL: Use PPSTRESN_num not NA.

Also need to ensure AVAL exists: AVAL may be numeric, use as.numeric.

We also need to handle _PARAM creation: use PPTESTCD and PPORRESU. PPORRESU is unit.

We should also ensure AVAL_log computed correctly: log(AVAL) where AVAL is numeric.

We also need to fix adpp_sum_t creation: Use tibble with proper column names, avoid data.frame check.names issue. Use tibble or data.frame with check.names=FALSE but ensure names are correct.

Simplify: Instead of lapply with data.frame, use vectorized approach or use bind_rows.

We should also handle case where adpp_sum is empty: log error but still produce empty output? But we need to ensure not empty.

We should also fix the names assignment line that caused error: remove it.

We need to produce text output with 132 numbers. The expected numbers include 179086 etc. Those are likely AUC values.

We should ensure sas_put formatting matches SAS exactly: SAS put with w.d right aligned. Our sas_put uses sprintf which right aligns. Good.

But we need to ensure we handle NA for std etc: SAS put of missing is blanks (w spaces). Our sas_put returns w spaces for NA, which when concatenated with " (" etc will produce something like "   123 (     )" ? In SAS, if std is missing, put(std,5.) would be 5 blanks, so value would be "   123 (     )". That's okay.

We should ensure we handle cv1 etc NA similarly.

We also need to ensure we handle geo-mean CV: sqrt(exp(std2^2)-1)*100 . If std2 is NA, then geo_cv is NA, sas_put returns blanks.

We should rewrite adpp_sum_t generation to avoid error.

Let's craft final script with robust handling.

Also need to ensure OUT_DIR handling and writing files: The comparison expects text P035.lst: MISSING. It expects OUT_DIR/<any>.txt or .csv rendering. Our previous script wrote P035.txt, P035.lst.txt, P035.lst etc but maybe OUT_DIR was not correctly set? The log shows OUT_DIR=/work/out, we wrote to out variable. That should be fine. But the error prevented any files from being written. So fixing error will allow files to be written.

We should ensure we write at least one .txt file that contains numbers. The comparison looks for numbers in text files. So we need to ensure text file contains numbers like 179086 etc.

We should ensure we write P035.lst.txt and P035.txt etc.

We should also ensure we handle sorting and grouping correctly.

Let's produce final corrected R script.

