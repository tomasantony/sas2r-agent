Need to fix script: EACTM is character with "" values causing discrete scale error. Need to convert to numeric. Also need to handle missing.

We should parse EACTM as numeric, handling empty strings to NA.

Also need to ensure plot works.

We should edit previous script minimally.

We need to produce complete corrected script.

We need to handle EACTM conversion: as.numeric, with "" -> NA.

Also need to ensure sorting.

Also need to handle that EACTM may be character like "96.0333333".

We should convert EACTM to numeric for plotting.

Also need to ensure AVAL numeric.

We should add step: plot_data$EACTM_num = suppressWarnings(as.numeric(plot_data$EACTM))

Then use that for x.

Also need to handle axis.

We should keep everything else.

We need to output one R script.

We must ensure OUT_DIR handling.

We should fix scale_x_continuous error.

Also need to ensure we write CSV with original EACTM? But plotted data CSV should have numeric.

We should ensure plot uses numeric.

We should also handle NA removal.

We should produce final script.

We need to include all previous logic but fix EACTM.

We should also ensure we don't break other parts.

We need to provide complete script in ```r block.

We should keep round_sas helper etc.

We should ensure we handle EACTM conversion before arrange.

We should also ensure we handle that EACTM may have missing "".

We should filter or keep NA? For plot, NA will be dropped.

We should add conversion.

We should also ensure we handle that EACTM column is character.

We should implement:

adpc$EACTM <- suppressWarnings(as.numeric(trimws(as.character(adpc$EACTM))))
But need to handle "" -> NA.

We should do that.

Also need to ensure sort uses numeric.

We should update script.

We should also ensure we handle that plot_data EACTM is numeric.

We should provide final script.

We need to ensure we don't call install.packages.

We need to ensure we write outputs.

We should produce final answer.