Need to fix haven::is_labelled not exported. Use haven::is.labelled? Actually haven exports is_labelled? In older version maybe not. Use inherits or check. Use haven::is_labelled alternative: use haven::is_tagged_na? Simpler: check if inherits labelled. Or use haven::is_labelled via haven:::is_labelled. Better to just check class.

Also need to ensure outputs written even if error. Need to handle reading.

Also need to ensure OUT_DIR handling.

We need to produce adtte.csv and P007.lst text with numbers.

The error was is_labelled not exported. Fix by using is.null(attr(...)) or use haven::is_labelled via checking.

Also need to ensure script writes files.

We should also handle reading correctly.

We need to produce text with numbers matching SAS. The SAS listing is PROC PRINT obs=15 var USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT. Need to ensure numbers appear.

We need to ensure adtte.csv has correct columns.

Fix script.

Also need to handle haven::zap_labels etc.

We should avoid using haven::is_labelled. Use function that checks inherits(x, "haven_labelled").

Also need to ensure format_for_csv handles correctly.

We should also ensure we handle missing.

We need to produce complete R script.

We should test logic for reading.

We should also ensure we use correct paths: /bench/data/derived/c2c/data/adam etc. The script uses D:/... but in docker it's /bench. However find_dataset will handle if adam_path not exists fallback. But we set adam_path to D:/... which won't exist in linux. So need to handle both. Use multiple candidate paths.

Better to define candidate paths list and find first existing.

We should implement robust path resolution.

Also need to ensure we write OUT_DIR files.

We should also ensure we don't fail on missing columns.

We need to produce text file with numbers: The SAS numbers include 1, -1, -701 etc. Those are AVAL values? Need to ensure AVAL computed correctly.

We should ensure we compute AVAL as (EVENTDT - TRTSDT)+1 etc.

We need to ensure we handle date conversion correctly. haven reads SAS dates as Date already.

We should ensure we handle TRTSDT missing.

We should produce listing text that contains those numbers.

We should also ensure we write adtte.csv with correct rows.

We need to fix is_labelled issue.

We should also avoid using haven::is_labelled elsewhere.

We should also ensure we handle haven::zap_formats etc.

We should produce final script.

We need to ensure we use only allowed packages.

We should implement read_dataset without is_labelled.

We should also ensure we handle haven_labelled conversion via as_factor? Use haven::zap_labels.

We should test.

We need to output one R code block.

We should include out dir creation.

We should ensure we write P007.lst.txt etc.

We should ensure we handle errors gracefully.

We should also ensure we handle case where adsl.sas7bdat reading fails due to haven version.

We should add fallback.

We should ensure we handle USUBJID length.

We should produce final script.

