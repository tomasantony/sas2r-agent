Need to fix sas_put_wd to be vectorized. Also need to ensure outputs written to OUT_DIR. Previous script failed before writing files, so missing. Fix helper.

Also need to ensure text rendering contains numbers. The previous script's sas_put_wd used if(is.na(x)) which fails vectorized. Need to make vectorized.

Also need to ensure OUT_DIR handling.

Also need to ensure reading files works.

We should fix sas_put_wd and sas_put_cnt to be vectorized.

Also need to ensure VALUE creation works.

Also need to ensure report generation.

We should provide complete corrected script.

We need to keep minimal edits but provide full script.

We should also ensure that the text file contains numbers like 14.3 etc. The previous txt_lines included headers but not numbers? It did include values. But failure prevented writing.

Fix helpers:

sas_put_wd <- function(x, w, d) {
  sapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    r <- sas_round(v, d)
    fmt <- formatC(r, format="f", digits=d, flag="")
    trimws(fmt)
  })
}

Similarly sas_put_cnt vectorized.

Also sas_round already vectorized.

Also need to handle VALUE creation: use ifelse with vectorized functions.

Also need to ensure haven reading works.

We should also ensure that the script writes t_ae_sum.txt and csv.

We should test logic for grade filter etc.

We should provide final script.

We need to ensure out dir creation.

We should also ensure that the script doesn't fail on missing columns.

We should include library calls.

We should ensure that the script writes files even if error.

We should provide complete script in one r code block.

We need to ensure that the script uses correct paths.

We should also ensure that the script handles TRTA values with spaces.

We should ensure that the transpose works.

We should ensure that the text file contains numbers.

We should also ensure that the script writes t_ae_sum.csv and t_ae_sum.txt.

We should ensure that the script handles case where adsl/adam files are .sas7bdat.

We should ensure that the script handles haven labelled.

We should ensure that the script handles missing values.

We should provide final answer.

