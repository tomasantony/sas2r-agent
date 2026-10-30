Need to fix error: object ANLPRNT not found. Means adpp_sum_t column names issue. In make_rows we used transmute but maybe column names with _PARAM cause issues. Also adpp_sum_t creation: we used split and map_dfr but column names may be mangled.

Also need to ensure outputs: P059.lst text rendering with 124 numbers.

We need to debug: adpp_sum_t arrange fails because ANLPRNT not found. Means adpp_sum_t doesn't have that column. Why? Possibly because adpp_sum has no rows? Or column names lowercased? Let's check adpp_raw names uppercased, but maybe ANLPRNT not present? In ADPP dataset, variables: ANLPRNT etc. Should exist.

But error indicates adpp_sum_t missing ANLPRNT. Let's inspect make_rows: we used df %>% transmute(ANLPRNT, ...) but df is one row tibble with columns ANLPRNT etc. That should work.

But maybe adpp_sum is empty due to filter? Then adpp_sum_t empty, then arrange fails? Actually arrange on empty tibble with no columns would fail. Let's see filter: PKFN=1 and ANL01FN ne 1 and paramcd in ... and PPSTRESN ne . and compress(PPCATL)='ANAL2'. If no rows match, adpp_sum empty.

Check ADPP data: need to inspect actual values. Could be that PPCATL values are like "ANALYTE 2" ? But compress removes spaces, so "ANAL2" matches "ANAL 2"? Let's check typical CDISC pilot ADPP: PPCATL maybe "ANALYTE"? Need to check.

Alternative: The SAS where clause: compress(PPCATL)='ANAL2' . In SAS compress removes spaces by default. So if PPCATL is "ANAL2" it matches. If it's "ANALYTE 2" compress would be "ANALYTE2" not match. So maybe data has "ANAL2".

But our filter may be too strict: ANL01FN ne 1 . In R, ANL01FN !=1 but if ANL01FN is NA, SAS "ne 1" would be true? In SAS, missing ne 1 is true. In R we filtered !is.na(ANL01FN) & ANL01FN !=1 which excludes missing. Should include missing as true. So we incorrectly excluded.

Similarly PKFN=1: if PKFN missing, SAS false. So need PKFN==1 with NA false.

PPSTRESN ne . : SAS missing is ., so ne . means not missing. In R !is.na(PPSTRESN) correct.

So fix ANL01FN filter: (is.na(ANL01FN) | ANL01FN !=1)

Also need to check paramcd values: PARAMCD in ('AUCINFO' 'AUCLAST' 'CMAX' 'TMAX' 'MRTLAST' 'LAMZHL') . Note SAS paramcd vs PPTESTCD. In ADPP, PARAMCD may be same as PPTESTCD? Let's check.

Also need to handle case sensitivity.

Also need to ensure adpp_sum_t creation robust even if empty.

Also need to handle column names with underscore: _PARAM is not valid R name without backticks. Our arrange uses `_PARAM` inside arrange, but we used backticks. Should work.

But error says ANLPRNT not found, suggests adpp_sum_t has zero columns? Let's trace: adpp_sum_t <- adpp_sum %>% split(...) %>% map_dfr(make_rows). If adpp_sum is empty, split returns list of 0? Actually nrow 0 => split returns list() => map_dfr returns empty tibble with 0 rows and 0 cols. Then arrange fails.

So need to ensure adpp_sum not empty. Fix filter.

Also need to handle reading adpp.xpt path: previous script found path but maybe not. Need robust.

Also need to ensure output files: P059.lst expected. We wrote P059.lst but also need .txt rendering. The evaluator looks for any .txt or .csv rendering. We wrote P059.txt and P059.lst etc. Should be okay but need to ensure numbers match SAS.

We need to reproduce SAS numbers: The feedback lists numbers like 1,14.2,2,0,28,179086 etc. Those are likely PK parameter values.

We need to ensure formatting matches SAS: put(mn1,6.) etc. SAS 6. format rounds to integer, width 6. Our sas_put does that.

But need to ensure we handle NA std etc.

Also need to ensure we handle _PARAM creation: compbl(put(PPTESTCD,$pk.)||' ('||strip(PPORRESU)||')') . So _PARAM is like "AUC (h*ng/mL)" etc. Need to map PPTESTCD via $pk format.

We did that.

Potential issue: PPORRESU may have different values per param, need to keep.

Also need to handle TRTAN, PPSDY grouping.

We should rewrite script minimally but fix filter and empty handling.

Also need to ensure we use haven read_xpt correctly, and handle variable names case.

Let's craft corrected script with robust handling, and ensure adpp_sum_t arrange works even if empty (add check).

Also need to ensure we write output files to OUT_DIR.

We should also add fallback to search for adpp.xpt in multiple locations.

Let's produce final script.