Need to translate SAS program P052.

We need to read inputs: &rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
Files: adsl.xpt and advsmax.xpt

But also libref inlib -> derived adam. However program uses rawadam.

We need to reproduce outputs:
- P052.lst (table/listing text, 4 numbers) -> write text rendering and CSV
- figure SGRender.png -> write OUT_DIR/sgrender.png and CSV of plotted data

We need to understand what SAS does.

Steps:
- read adsl, advsmax
- sort advsmax by studyid
- advsmax_final: filter WHERE PARAMCD="DIABP" and ATPTN=815 and ANL01FL="Y" and ADY>1 and SAFFL="Y"; also create LLN_label, ULN_label for first.studyid per studyid.
- range: min(base), max(base), min(aval), max(aval) from advsmax_final
- range2: min = min(min_base, min_aval); max = max(max_base, max_aval)
- select min(min), max(max) into :min, :max
- advsmax_final2: count distinct studyid as count_study
- advsmax_final3: if count_study <=2 then column_numbers=2 row_numbers=1 else 2,2
- select distinct column_numbers, row_numbers into macro vars
- template shiftplot: datapanel classvars=(studyid) order rowmajor rows=row_numbers columns=column_numbers
  rowaxisopts label "Maximum Post-baseline Measurment" viewmin=min viewmax=max
  columnaxisopts label "Maximum Baseline Measurment" viewmin=min viewmax=max
  scatterplot x=base y=aval group=trta
  lineparm x=0 y=0 slope=1
  referenceline x=ANRLO, x=ANRHI, y=ANRLO, y=ANRHI with labels LLN/ULN

- sgrender data=advsmax_final3 template=shiftplot

Expected outputs: P052.lst (table/listing text, 4 numbers) -> likely the range table? Or maybe SAS log listing? Need to check what SAS output would be. The program has no PROC PRINT, but maybe ODS LISTING? However expected outputs says P052.lst (table/listing text, 4 numbers) -> write text rendering and CSV of same table. And figure SGRender.png.

We need to infer what 4 numbers are. Likely the range table: min_base, max_base, min_aval, max_aval. That's 4 numbers. So P052.lst probably contains that.

Alternatively could be min and max? But says 4 numbers.

We should produce a text file P052.lst containing the range values.

Also need to produce dataset outputs? Declared output types: graph;dataset . But expected outputs lists only lst and figure. Maybe dataset is advsmax_final3? But we should output datasets as CSV anyway.

We need to create R script that:
- reads advsmax.xpt
- does filtering
- computes range
- creates plot

We need to handle OUT_DIR env var.

We need to produce:
- OUT_DIR/P052.csv ? Or maybe advsmax_final3.csv
- OUT_DIR/sgrender.png and sgrender.csv
- OUT_DIR/P052.lst.txt and P052.csv? The instruction says each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) that contains same numbers and as CSV of table body. Each figure as PNG via ggsave plus CSV of plotted data.

So we need to map expected outputs to files:
- P052.lst -> P052.txt and P052.csv (or P052.lst.csv?) The spec says write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) ... each figure as PNG via ggsave plus CSV of plotted data.

So for P052.lst, we should create P052.csv and P052.txt (or P052.lst.csv/txt). Safer to create both naming variants.

For figure SGRender.png -> sgrender.png and sgrender.csv

We should also output advsmax_final3 dataset.

Let's look at data to understand.

We need to write R script that does all.

We need to handle haven reading.

We need to create plot similar to SAS template: datapanel by studyid, scatterplot base vs aval colored by trta, diagonal line, reference lines at ANRLO/ANRHI.

We need to determine ANRLO/ANRHI values. Likely constant per param.

We need to compute min/max for axis limits.

We need to create faceted ggplot.

We need to handle LLN_label etc not needed for plot but we can annotate.

We should produce sgrender.png with facet_wrap(~studyid, nrow=row_numbers, ncol=column_numbers)

We need to ensure script runs.

We should also produce text rendering of range table.

Let's craft R script.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "."
- rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
- read adsl.xpt, advsmax.xpt via haven::read_xpt
- sort advsmax by studyid
- filter: PARAMCD=="DIABP" & ATPTN==815 & ANL01FL=="Y" & ADY>1 & SAFFL=="Y"
- Need to handle haven labelled vectors: convert to character/numeric appropriately. Use haven::as_factor? Better to use zap_labels? But we need to compare strings. haven reads as labelled with attributes. We can use haven::as_factor or just as.character. For numeric, as.numeric.

Simplify: after reading, convert all haven_labelled to appropriate via haven::zap_labels? Let's use dplyr mutate across where haven::is.labelled -> as.character if character type else as.numeric.

Alternatively use haven::read_xpt with col_select? We'll just handle.

- Create LLN_label, ULN_label: for first row per studyid, set to "LLN"/"ULN", else NA. In SAS, they set only for first.studyid then retain? Actually they do if first.studyid then do; LLN_label="LLN"; ULN_label="ULN"; end; This means for non-first, those variables remain missing (since not retained? In SAS data step without retain, they would be missing for other rows). So we set NA for others.

- Compute range: min_base = min(base, na.rm=TRUE), etc.

- Compute min_val = min(min_base, min_aval), max_val = max(max_base, max_aval)

- Compute count_study = n_distinct(studyid)

- Create column_numbers, row_numbers based on count_study

- Create advsmax_final3 with those columns

- Write CSVs

- Create plot

We need to handle ANRLO, ANRHI: they are reference lines. In SAS they use referenceline x=ANRLO etc. That means per observation? But ANRLO is constant per param. In ggplot we can add geom_vline/hline at unique ANRLO/ANRHI values.

We should extract unique ANRLO, ANRHI from data.

We also need to add diagonal line y=x.

We need to set axis limits to min_val, max_val.

We need to create ggplot:

ggplot(advsmax_final3, aes(x=base, y=aval, color=trta)) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  geom_vline(xintercept=unique ANRLO) linetype=dashed +
  geom_vline(xintercept=unique ANRHI) +
  geom_hline similarly +
  facet_wrap(~studyid, nrow=row_numbers, ncol=column_numbers) +
  labs(x="Maximum Baseline Measurment", y="Maximum Post-baseline Measurment") +
  xlim(min_val, max_val) + ylim(min_val, max_val) + coord_equal() + theme_bw()

Add annotations for LLN/ULN labels at min position.

We need to produce sgrender.png via ggsave.

Also produce sgrender.csv containing plotted data (advsmax_final3 relevant columns)

Also produce P052.txt and P052.csv for range table.

We should also produce advsmax_final3.csv as dataset output.

We need to ensure script writes to OUT_DIR.

We need to include round-half-up helper (though not needed for this program, but include).

We need to handle missing values.

We should also handle case where advsmax.xpt not found at rawadam but maybe at derived? Check both.

We should implement robust reading: try rawadam path, if not exists try inlib path.

We need to output exactly one R script inside one ```r code block.

We must not call install.packages.

We need to ensure script creates OUT_DIR if not exists.

We need to produce text rendering: simple table with 4 numbers.

Let's craft final R script.

