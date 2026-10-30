We need to produce a single R script that reproduces the SAS program's outputs: a listing of protocol deviations. The SAS program reads sdtm.dv and adam.adsl datasets, filters for randomized subjects (RANDFL='Y'), joins, orders, then uses PROC REPORT to produce a listing with specific column widths, titles, footnotes, and break after TRTA (page break). Output is an RTF file l_dv.rtf, but we need to produce a text rendering and CSV of the same table. According to instructions: each output dataset as CSV, each table/listing additionally as plain-text rendering (txt) containing same numbers the SAS output shows, and as CSV of the table body. So we need to output the listing table as CSV (maybe same as dataset dv_list) and also a txt rendering replicating the PROC REPORT output formatting (including column headers, maybe rows). We also need to write titles and footnotes? The spec says "write a text rendering and CSV of the same table". So we need to produce l_dv.txt (plain-text rendering) and l_dv.csv (the table). Also maybe l_dv.rtf is not required; we produce txt and csv.

We need to read from the librefs: sdtm.dv and adam.adsl. Paths: sdtm lib points to D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm. So file path: D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/sdtm/dv.sas7bdat (or .sas7bdat). Use haven::read_sas. Similarly adam.adsl: D:/.../adam/adsl.sas7bdat.

We need to output to OUT_DIR environment variable. Use out <- Sys.getenv("OUT_DIR"). Write files there.

Implement rounding half away from zero? Not needed as no numeric rounding.

Need to sort order: by TRTA, SITEID, USUBJID, DVSTDTC. In SAS, character sorting is case-sensitive, uppercase before lowercase. R's default ordering is based on locale; we can use dplyr::arrange with factor? We'll just use arrange with strings; should be fine.

Date variables: DVSTDTC and DVENDTC are character strings in ISO? In SDTM, they are datetime strings? We'll keep as character; but output as same format.

Implement titles: we can create a vector of title lines and footnote lines and write them to txt before table.

Break after TRTA: In listing, page break means separate pages; in txt we could insert a form feed or blank line with a separator. We'll just insert a line of dashes between groups.

Implement column widths: not needed for txt.

Implement writing CSV of dv_list: write.csv.

Implement txt rendering: We'll create a formatted table using format strings aligning columns based on widths (in inches). Convert widths to characters approximations: maybe 0.8in ~ 12 chars, etc. We'll just use sprintf with left-justified width.

Define column widths in characters: approximate: 0.8in ~ 12, 0.5in ~ 8, 0.8in ~12, 1.2in ~18, 1.0in ~15, 2.0in ~25, 0.7in ~10, 0.7in ~10, 0.8in ~12, 0.6in ~9.

We'll create header line with column names as defined in PROC REPORT (display labels). Use same labels with line breaks replaced by space.

Define header labels: Treatment, Site, Subject ID, Deviation Category, Sub- category, Description, Start Date, End Date, Epoch, Severity.

We'll produce header line with those labels.

Then for each row, format each field left-justified within width.

Insert a blank line after each TRTA group (page break). Also maybe include titles and footnotes at top and bottom.

Implement titles: Use macro set_titles for L_DV. Titles: title4 "Listing 16.2.2", title5 "Listing of Protocol Deviations", title6 "All Subjects". Also title1 sponsor left, title2 protocol, title3 blank, footnotes: footnote1 Data cutoff: &cutdate., footnote2 Source: ADaM datasets, footnote3 Program: &sysin. | Run: date time. We don't have those macro variables; we can set placeholder values from environment? Not provided. We'll just leave blank or use placeholder. But to match SAS output, we need actual values. The macro variables were set earlier: sponsor, protocol, cutdate, sysin (program path). In the environment, they are macro variables but not passed to R. We could read from environment variables? Not set. We could just not include them; but the spec expects the same table as SAS output, maybe titles not needed for table content. The txt rendering likely includes only the table, not titles. The spec says "write a text rendering and CSV of the same table". So we can just output the table.

Thus script steps:

- Load libraries.
- Set out dir.
- Read datasets.
- Filter and join.
- Select and rename columns as per SAS.
- Arrange.
- Write CSV dv_list.csv.
- Create txt rendering: write titles lines (maybe just column headers) and rows.

Implement using base writeLines.

Edge Cases: missing values: write empty strings.

Implement.

Now produce final R script inside ```r``` block.