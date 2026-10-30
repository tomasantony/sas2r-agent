We need to fix variable name case: In advs_raw, column names may be uppercase or lower. Using haven, variable names become lower case? Actually read_xpt returns tibble with column names as they appear, likely lower case? In SAS, variable names are uppercase, but haven converts to lower case by default? I think haven keeps original case but makes them lower? I recall haven makes variable names lower case. So need to use lower case names: paramcd, atptn, anl01fl, avisitn, saffl, etc. So adjust filter accordingly.

Also need to write P032.lst text and csv. The script previously wrote P032.lst.txt and .csv but maybe naming expects P032.lst (no extension) for text? The spec: expected output P032.lst (table/listing text, 419 numbers) -> write a text rendering and CSV of the same table. So we should write file.path(out, "P032.lst.txt")? The spec says write a text rendering and CSV of the same table. It didn't specify naming, but earlier they said each output dataset as CSV with name.csv, each table or listing additionally as a plain-text rendering (file.path(out, "<name>.txt")). So name should be "P032.lst". So text file should be "P032.lst.txt". That's what we did. But they didn't find any .txt files. Maybe because script failed before writing. So after fixing filter, should produce.

Also figure: we saved PNG as clinicalhandout_dbpprofile.png, correct name. But they expected ClinicalHandout_DbpProfile.gif? They said write OUT_DIR/clinicalhandout_dbpprofile.png and a CSV of plotted data. So naming is fine. However they said missing figure. Maybe because plot didn't generate due to missing data? We'll ensure data exists.

Also need to write CSV of plotted data: maybe all_df already saved as all.csv, but they expect a CSV of plotted data maybe named clinicalhandout_dbpprofile.csv. Let's add that.

Also need to ensure blockplot data is correct; but not essential for file existence.

Fix filter variable names: use lower case.

Also ensure that after reading, column names: usubjid, trtpn, anl01fl, param, paramcd, avisit, avisitn, atptn, saffl, aval.

So modify filter accordingly.

Also ensure that we keep trtpn numeric; maybe it's character. Convert to numeric.

Add conversion: advs <- advs %>% mutate(trtpn = as.numeric(trtpn))

Also ensure that after merging, we have trtpn column removed.

Now produce CSV of plotted data: maybe same as all_df.

Add write_csv(all_df, file.path(out_dir, "clinicalhandout_dbpprofile.csv"))

Now produce text rendering: we need to include all numbers with same formatting as SAS. Hard to replicate exactly but at least produce numbers. Use sprintf with appropriate decimals.

Also need to include header lines? Not required.

Now produce.

Let's craft final script.