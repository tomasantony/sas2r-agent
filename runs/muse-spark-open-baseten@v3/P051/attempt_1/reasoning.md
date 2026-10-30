Need to translate SAS program P051 medication listing.

We need to read ADCM.xpt from rawadam path. But libref inlib is derived adam. However program uses source libname xport from &rawadam./adcm.xpt . So read that.

Steps:
- sort by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD where SAFFL='Y'
- Data ADCM2: length StartStop $25 Doseunit $50; Period='Study Period'; if CMSTDTC='' then 'NK'; same CMENDTC; StartStop=compbl(CMSTDTC||'/'||CMENDTC); If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE); if AENRF='Ongoing' then Ongoing='Y' else 'N';

Note: cmunit variable? In ADCM dataset, likely CMDOSE numeric? CMDOSU? But code uses cmunit. Might be CMDOSU? Check typical ADCM: variables include CMDOSE, CMDOSU, CMTRT etc. The code uses cmunit - maybe typo but dataset has CMUNIT? Let's inspect actual file.

We need to produce outputs: Target36.rtf -> text rendering and CSV.

We need to explore data.

We must write R script that reads ADCM.xpt, does transformations, outputs CSV and txt.

We need to handle OUT_DIR env var.

We need to produce Target36.csv and Target36.txt (and maybe dataset). The expected output types: table;dataset . But listed expected outputs only Target36.rtf . So we need to produce dataset CSV as well? The rules say each output dataset as CSV. But here listing is table. We'll produce Target36.csv for table body and Target36.txt rendering.

Also need to handle titles: title1 "Safety Population", title2 "Study Period", title3 "Treatment: #byval1" per by group.

PROC REPORT with by TRTA, columns: TRTA USUBJID CMTRT cmdecod cmclas DoseUnit CMINDC StartStop ADURN Ongoing

We need to reproduce listing.

We need to check actual ADCM columns.

Let's craft R script that does exploration but final script must be standalone.

We need to implement SAS semantics: compbl = compress multiple blanks to single, trim leading/trailing? In SAS compbl removes multiple blanks. We'll implement via gsub("\\s+", " ", trimws(...))

CMSTDTC and CMENDTC are character ISO dates? Need to handle missing as "" -> "NK". In R, haven reads missing as NA. So convert NA to "" then check.

DoseUnit: If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')'); else DoseUnit = compbl(CMDOSE);

But CMDOSE numeric? In SAS, CMDOSE || '(' will do automatic numeric to character conversion using BEST12. format. So need to replicate. In R, if CMDOSE is numeric, convert to character without trailing spaces. Use as.character(CMDOSE) but need to handle SAS formatting: BEST12. would produce e.g., "  200" ? Actually BEST. strips leading spaces? In SAS, put numeric to string via || does conversion with BEST12. which right aligns. But compbl will compress blanks. So effectively trim.

Also cmunit variable: check if exists. Might be CMDOSU. Let's assume cmunit is CMDOSU or CMUNIT. We'll handle both: look for column named CMUNIT, CMDOSU, CMUNIT etc case-insensitive. If not found, fallback to CMDOSU.

Also need to handle CMTRT column: not mentioned in keep but used in report.

ADURN numeric duration.

Ongoing Y/N.

Sort order: by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD

We need to produce text file with titles and table.

We should inspect data to ensure correct.

We need to write R script that does all.

We must not call install.packages.

We need to handle OUT_DIR.

We need to produce CSV with same columns as report.

We need to produce PNG? No figure.

We need to ensure dates as YYYY-MM-DD etc but here dates are strings.

We need to implement round-half-up helper (not needed).

We need to produce text rendering that contains numbers.

Let's craft script.

Steps in R script:

- out <- Sys.getenv("OUT_DIR"); if out=="" out <- "."
- root etc paths: use "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adcm.xpt" as source. But to be robust, try both rawadam and derived. Use file.exists check.

- Read with haven::read_xpt

- Convert all character columns: haven labelled -> as.character? Use haven::as_factor? Better to use haven::read_xpt then convert labelled to character via haven::as_factor or as.character. Use dplyr mutate across where haven::is.labelled -> as.character(as_factor).

- Ensure column names uppercase for consistency but keep original.

- Filter SAFFL == 'Y'

- Sort.

- Create ADCM2.

Need to handle CMSTDTC, CMENDTC missing: if is.na or "" then "NK"

StartStop = paste0(CMSTDTC_filled, "/", CMENDTC_filled) then compbl.

DoseUnit: if !is.na(cmunit) and cmunit != "" then paste0(CMDOSE, " (", cmunit, ")") else as.character(CMDOSE) . Need to handle CMDOSE missing? Then DoseUnit maybe "".

Compbl.

Ongoing.

Period.

Then output.

For text rendering: we need to produce a plain text file that mimics PROC REPORT. Include titles per by group.

We can use simple fixed-width table via format.

We need to produce CSV of table body: write.csv(ADCM2_selected, file.path(out, "Target36.csv"), row.names=FALSE, na="")

And txt: include header lines and table.

We should also produce dataset CSV maybe ADCM2.csv? But expected output is Target36. We'll produce Target36.csv and Target36.txt.

We need to ensure 46030 numbers? That's count of numbers in table. We'll just produce table.

Implement text rendering: for each TRTA by group, print titles and then table rows.

Use width definitions: but we can just produce tab-separated.

Simpler: use capture.output and print data.frame.

We need to ensure titles verbatim: "Safety Population", "Study Period", "Treatment: #byval1" where #byval1 is TRTA value.

We should include column headers: USUBJID, CMTRT, CMDECOD, CMCLAS, Dose (unit), CMINDC, Start Date / Stop Date, Dur (Days), Ongoing

We need to handle TRTA noprint but by group.

We should write txt file with these.

Let's craft final R script.

We need to handle cmunit variable name detection.

Also need to handle CMTRT vs CMTRT? Check.

We should include library calls.

We need to ensure script runs non-interactively.

Let's produce final answer.