Need to fix error: all_stats_wide[is.na(all_stats_wide)] <- "" fails because ord is numeric. Need to handle only character columns or replace NA properly.

Also need to ensure outputs written to OUT_DIR. Previous script failed before writing, so no files.

Fix: replace NA handling: for each column, if numeric, keep NA? But for display we want "" for missing col values. Better to do:

all_stats_wide <- all_stats_wide %>% mutate(across(everything(), ~replace_na(as.character(.x), ""))) but ord numeric would become character. Instead handle separately: keep ord numeric, only replace NA in col1-3 and maybe stat/category.

Simpler: all_stats_wide[is.na(all_stats_wide)] <- "" fails due to type. Use dplyr mutate.

Also need to ensure text file contains numbers. The previous script's text rendering should contain numbers but failed before.

We need to fix minimal.

Also need to ensure OUT_DIR handling and write files.

We should also ensure we produce t_dm.txt and t_dm.rtf and csv.

Fix the line:

all_stats_wide[is.na(all_stats_wide)] <- ""

Replace with:

for (cc in names(all_stats_wide)) {
  if (is.character(all_stats_wide[[cc]])) all_stats_wide[[cc]][is.na(all_stats_wide[[cc]])] <- ""
}

Or use tidyr replace_na.

Also need to ensure report handling similar.

Also need to ensure we don't convert ord to character prematurely.

We should provide complete corrected script.

Also need to ensure we handle haven reading correctly.

We should keep previous logic but fix bug.

Also need to ensure we write files even if error elsewhere.

Provide full script in one r code block.

