We need to inspect why DM dataset columns not found. The raw DM Excel file likely has column names not uppercase, maybe "USUBJID" and "RFSTDTC" but after rename_with(toupper) they become uppercase, so they should exist. However maybe the column names have leading/trailing spaces or different names like "USUBJID " or "RFSTDTC ". Let's inspect typical CDISC DM dataset: columns include USUBJID, RFSTDTC, etc. Should be there.

But error indicates they are missing. Perhaps the Excel file has sheet name not default first sheet? read_excel reads first sheet; maybe the data is in a sheet named "DM". Could need to specify sheet = "DM". However we cannot inspect file. Safer: after reading, we can look for columns containing "USUBJID" ignoring case and spaces. Use grep.

Implement: find column that matches regex "^\\s*USUBJID\\s*$" case-insensitive. Similarly for RFSTDTC.

We'll rename columns to trimmed uppercase without spaces.

Implementation:

dm_raw <- read_excel(..., .name_repair = "minimal") then rename columns: names(dm_raw) <- toupper(str_trim(names(dm_raw)))

Similarly for ae_raw.

Then select.

Also ensure that if columns still missing, we stop with informative.

Let's adjust script.

Also need to ensure that output listing includes numbers as in SAS. The previous script writes tab-separated text; but SAS PROC PRINT would produce space separated maybe. However the evaluation expects numbers like 1, -2026 etc. Those numbers likely are from dataset values (e.g., STUDYID "ONCO-2026" includes 2026). Our output should contain those numbers. The script writes the dataset; should be fine.

But earlier failure: no files written because script stopped early. So fix column detection.

Implement robust column renaming.

Also ensure that we write listing as .txt and .csv as required.

Let's produce final script.

