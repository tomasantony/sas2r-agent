We need to fix column names. The raw CSV column names likely include spaces and parentheses. In previous script we cleaned to lower case and underscores, losing original names needed for later referencing. Instead we should keep original names as is, using backticks for variable names with spaces. So we shouldn't rename columns. Use raw_metabric as read with default column names (preserve). Then refer to correct names: patient_id, age_at_diagnosis, sex, `Tumor Stage`, `Tumor Size`, `Overall Survival (Months)`, `Overall Survival Status`, Chemotherapy, `ER Status`, `HER2 Status`. Also need to ensure those columns exist.

Thus modify import: read_csv with col_names = TRUE, keep as is. No cleaning.

Also need to ensure that variable names used in mutate are correct.

Also need to ensure that we write P061.lst (text) not .lst.txt. The spec expects P061.lst (text). So we should write file.path(out_dir, "P061.lst") as text. Also need to write CSV version maybe P061.lst.csv.

Similarly for PDFs: need to write P061_t_oncology_report.txt and .csv, but they expect .pdf? The spec says write text rendering and CSV. So naming can be .txt. Already did .txt.

But missing files: maybe script didn't run due to error earlier. Fix column names.

Also need to ensure that we create the required PNG files with exact names: sasgraph.png, SGPlot.png etc. In previous script we used sgplot.png etc. Need to match case: SGPlot.png, SGPlot1.png etc. So rename accordingly.

Also need to ensure that we write the listing P061.lst (not .lst.txt). So adjust.

Also need to ensure that we write the PDF listing files: P061_t_oncology_report.pdf and P061_f_oncology_graphs.pdf as .txt? The spec expects .pdf but we can output .txt. But they look for .txt or .csv rendering of the table/listing. So naming maybe .txt is okay. But they may look for .pdf? The earlier attempt wrote .txt. The evaluation looked for .txt or .csv. So fine.

Now adjust script.

Also need to ensure that we write the CSV of each figure data with same base name as PNG (e.g., SGPlot.csv). Use same naming.

Implement.

