We need to produce an R script that reproduces the SAS program's outputs: a figure SGPlot.png and a CSV of plotted data. Also any datasets? The declared output types: graph;dataset. The SAS program creates many intermediate datasets but final output is the SGPlot graph. Possibly also need to output the final dataset tcount3 as CSV? The spec says each output dataset as CSV. The expected outputs list only figure SGPlot.png. But we should output all datasets created? The spec: "Write every output into the directory in the environment variable OUT_DIR: each output dataset as CSV". So we need to write each dataset that would be considered output. Which ones are output? Probably the final dataset tcount3 is the dataset used for graph. Also maybe countae, nsubj, count, tcount, maxpercen2, tcount2, AEgrid, tcount3. But to be safe, we can output all created tables that are not intermediate? The spec says each output dataset as CSV. The SAS program didn't have explicit PROC EXPORT; but the benchmark expects certain outputs. The expected outputs list only figure. So maybe only need to output the figure and its data. However to follow spec, we can output tcount3 as CSV and also the plotted data (which is same as tcount3 filtered). We'll output tcount3.csv and sgplot.png and sgplot_data.csv.

Implement steps:

- Set out <- Sys.getenv("OUT_DIR")
- Read adsl.xpt and adae.xpt using haven::read_xpt.

- Keep variables in adae: USUBJID, TRTAN, AEDECOD, TRTEMFL, AOCCPFL, SAFFL.

- Sort adae by AEDECOD (not needed for later merges but okay).

- Create adae1: filter where TRTEMFL=="Y" & AOCCPFL=="Y" & SAFFL=="Y".

- countae: distinct count(usubjid) as numAE per trtan, aedecod. In SAS: select distinct count(usubjid) as numAE, aedecod, trtan from adae1 group by trtan, aedecod; The distinct is redundant. So we compute counts.

- nsubj: distinct count(distinct usubjid) as numusubjid, TRT01AN from adsl group by TRT01AN. Note adsl variable TRT01AN? Not kept earlier but present in adsl dataset. Use rename later.

- Merge countae with nsubj by trtan (where nsubj.trt01an renamed to trtan). In SAS they rename trt01an=trtan. So we need to left join countae with nsubj (by trtan). If not found, abort cancel (i.e., stop with error). We'll implement check: if any rows in countae have no matching nsubj, stop with error.

- Compute AEpercent = numAE/numusubjid else missing.

- Sort by aedecod.

- tcount: merge count with count(where trtan_1=0 rename ...) Wait: In SAS they have count where (trtan_1=0) but count dataset doesn't have trtan_1 variable. Actually they likely meant trtan=0? In SAS they rename AEpercent=AEpercent_1 trtan=trtan_1. So they take rows where trtan=0 (placebo?) and rename. So we need to create a copy of count where trtan==0, rename columns, then merge with count by aedecod.

- After merge, fill missing AEpercent and AEpercent_1 with 0. Compute RelRisk = AEpercent - AEpercent_1. aeseqno+1 (incremental). In SAS aeseqno is retained across rows after sort by aedecod? Actually they didn't sort before aeseqno, but after merge they have by aedecod, so aeseqno increments within each aedecod? In SAS, aeseqno is retained and increments each observation in the data step order (which is sorted by aedecod). So aeseqno is sequential across rows after sort by aedecod. We'll compute aeseqno = row_number() after sorting by aedecod.

- maxpercen2: select distinct -max(AEpercent) as minusmaxAEpercent, aedecod from tcount group by aedecod order by minusmaxAEpercent. So for each aedecod compute minusmaxAEpercent = -max(AEpercent). Then sort by minusmaxAEpercent (ascending because negative). Then add dispseqno+1 (sequential). So dispseqno is row number after sorting.

- Merge tcount with maxpercen2 by aedecod to get tcount2.

- AEgrid: create table with all combinations of distinct trtan from tcount2 and distinct aedecod, dispseqno from tcount2. In SAS they use select * from (select distinct trtan from tcount2), (select distinct aedecod, dispseqno from tcount2) order by aedecod, trtan; This creates Cartesian product. So we need to expand grid.

- tcount3: merge tcount2 with AEgrid by aedecod trtan. Fill missing AEpercent with 0. Sort by dispseqno.

- Plot: filter dispseqno <=20, scatter x=AEpercent y=dispseqno, group=trtan. Use ggplot2: geom_point or geom_line? SAS scatter. We'll use geom_point. Add xaxis grid, yaxis grid with breaks 1:20. Label axes: x = "AEpercent", y = "dispseqno". Also label trtan as "Treatment", aeseqno as "AE ordered in descending order". Title "Common Treatment-Emergent Adverse Events". Save PNG.

- Also need to output plotted data: the data used for plot (tcount3 filtered). Save as CSV.

- Also output tcount3 dataset as CSV.

- Use round-half-up? Not needed for percentages maybe. But we may need to format numbers for table outputs, but not required.

Implement helper for rounding half away from zero if needed later.

Write script with library calls.

Edge Cases: variable names case. Use same names.

Implement abort if any count rows missing nsubj: stop("Missing nsubj for some trtan").

Write CSV with na = "".

Implement.

