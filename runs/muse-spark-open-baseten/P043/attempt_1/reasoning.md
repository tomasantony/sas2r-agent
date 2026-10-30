Need to translate SAS program P043 Figure 11.1.

We have test data creation but then overwritten? Actually SAS creates data test with 9 rows, but then uses ADLBC from adam-split. So final figure uses ADLBC where PARAMCD=SODIUM, AENTMTFL=Y and AVISITN !=99. Keep USUBJID TRTAN TRTA PARAMCD PARAM AVAL BASE A1LO A1HI.

Then PROC FREQ to get LLN and ULN as most frequent A1LO/A1HI per PARAMCD.

Then proc template defines graph: scatterplot x=BASE y=AVAL grouped by TRTA, with reference lines at LLN and ULN both x and y, plus 45-degree line.

Need to produce sgrender.png and CSV of plotted data.

We need to read ADLBC from adam-split. Path: &rawsplit = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split . That's directory with .sas7bdat? Or .xpt? Let's check typical. In phuse, adam-split contains sas7bdat files. So libname adam ".../adam-split" means each dataset is a file. So ADLBC is file ADLBC.sas7bdat or .xpt. Need to handle both.

We need to locate files. Use Sys.getenv? But we have absolute paths given. However for portability, we should try to find files via OUT_DIR? The instruction says read inputs from absolute paths given in context. So we should use those paths. But also handle if not found, try alternative.

We need to produce OUT_DIR/sgrender.png and CSV.

Also need to handle macro variables LLN ULN.

We need to implement SAS semantics: PROC FREQ counts, then sort by PARAMCD descending COUNT, take first per PARAMCD. That gives most frequent A1LO/A1HI. For SODIUM, likely single value.

We need to create figure similar to SAS template: designwidth 951 designheight 689. DataSymbols, DataColors. Title "Figure 11.1 Scatterplot", footnote "LLN ULN ....", x label "Min. Baseline", y label "Min. Post Baseline" (though SAS says Min. Baseline vs Min. Post Baseline, but data is BASE vs AVAL). Add 45-degree line, reference lines.

We need to produce PNG via ggsave.

Also need CSV of plotted data: should contain columns used for plot: USUBJID, TRTA, PARAMCD, PARAM, AVAL, BASE, A1LO, A1HI maybe.

We also need to handle test dataset? The SAS creates test but not used. So ignore.

We need to write R script that does all.

We must ensure OUT_DIR exists.

We need to handle reading ADLBC: try haven::read_sas, haven::read_xpt.

Paths: rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam ; rawsplit = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split ; root = D:/meta_hackathon/sas2r-agent

We should attempt to list files in rawsplit.

We need to produce sgrender.png with ggplot.

Steps in R:

- out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- Define helper round half up? Not needed but include.

- Read ADLBC: try file.path(rawsplit, "adlbc.sas7bdat"), "ADLBC.sas7bdat", "adlbc.xpt", etc. Use haven.

- Filter: PARAMCD == "SODIUM", AENTMTFL == "Y", AVISITN != 99 (handle NA).

- Keep columns.

- Compute LOW and HIGH via dplyr count.

- Determine LLN, ULN.

- Create plot.

We need to ensure treatment variable TRTA is used.

We need to replicate colors: DataColors=(CXFFA53D CXF7DF54 CXEF6B48 CX61A6E7 CXCB79C8 CX85CE79 CX967CD0 CXDD6475 CX9DAA2D CX6C69D9 CX2EA64A CXC17D35) -> hex colors #FFA53D etc.

Symbols: PLUS, circlefilled, trianglefilled -> map to shapes.

We need to add reference lines: y=ULN blue dashed, y=LLN red dashed, x=LLN red, x=ULN blue.

Add 45-degree line from 0,0 to 100,100 (wallpercent). Use geom_abline.

Add legend title Treatment.

Add grid.

Add title and footnote via labs + caption.

We need to save PNG with width 951 height 689 pixels. ggsave uses inches/dpi. So width=951/96 ~9.9 inches at 96 dpi, or use dpi=100. Simpler: width=951, height=689 units px via ggsave with dpi.

We also need to write CSV of plotted data: file.path(out, "sgrender.csv")

Also need to ensure we write sgrender.png exactly lower case as expected: "sgrender.png"

The expected output says figure SGRender.png -> write OUT_DIR/sgrender.png and CSV of plotted data. So we need both.

We should also handle if ADLBC not found, fallback to creating test data as in SAS? But ADLBC should exist.

We need to produce complete R script inside one ```r block.

We must not call install.packages.

We need to include library calls.

We need to handle Windows paths with forward slashes.

We should implement robust file finding: list.files.

We should also handle haven reading with col_select? Keep all.

We need to ensure dates formatting not needed.

We need to write CSV with na="".

We need to ensure script runs non-interactively.

We should add progress messages via cat.

We need to handle missing TRTA values.

We should also output LLN ULN to console.

We need to create plot data CSV with same columns as START.

We should also create figure with ggplot2.

Let's craft R script.

Potential pitfalls: ADLBC variable names may be uppercase. haven preserves case. So filter using case-insensitive? Use names to lower? Better to handle both.

We should normalize column names to uppercase for processing, then keep original.

Simpler: after reading, convert names to uppercase via rename_with(toupper).

Then filter.

We need to handle AENTMTFL, AVISITN, PARAMCD, etc.

We need to handle TRTA vs TRTAN.

We need to compute LOW/HIGH: group by PARAMCD, A1LO count, then sort descending COUNT.

We need to pick first per PARAMCD.

We need to handle if multiple PARAMCD values (only SODIUM).

We need to set LLN, ULN as numeric.

We need to handle if A1LO/A1HI are character? Probably numeric.

We need to create plot.

We need to ensure we handle NA.

We need to write CSV.

We need to ensure OUT_DIR exists: dir.create.

We need to handle ggsave.

We should also handle if no data, create empty plot.

We need to include all required libraries.

We should produce final script.

We need to ensure we use only allowed packages.

We should include haven, dplyr, ggplot2, etc.

We need to ensure we don't open graphics windows.

We need to produce PNG.

We should add theme.

Let's write script.