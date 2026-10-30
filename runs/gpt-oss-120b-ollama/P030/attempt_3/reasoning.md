We need to fix error in VSDTC creation: format(parse_date11(VSDT), "%Y-%m-%d") where parse_date11 returns Date, but if parse_date11 returns NA_Date_, format(NA, ...) returns "NA"? Actually format(NA_Date_, "%Y-%m-%d") returns "NA". The error arises because VSDT may be numeric? In edc_vs, VSDT column likely is character date string like "01JAN2020". parse_date11 expects character. But if VSDT is numeric (maybe SAS date numeric), then dmy will error. Also if VSDT is already Date? The error about prettyNum trim argument suggests format got something not appropriate, maybe VSDT is already Date and format expects numeric? Actually format.default for Date uses prettyNum? Not sure.

Simplify: Instead of using ifelse with format, we can use case_when and as.character with ymd? Actually we need VSDTC as character in yymmdd10 format (YYYY-MM-DD). If VSDT is already a date (maybe numeric days since 1960?), we can convert using as.Date with origin = "1960-01-01". Let's inspect typical EDC VS dataset: columns likely include VSDT (character date). Safer: treat VSDT as character; if it's numeric, convert.

Implement VSDTC = ifelse(is.na(VSDT) | VSDT == "", NA_character_, {
   dt <- if(is.numeric(VSDT)) as.Date(VSDT, origin = "1960-01-01") else parse_date11(VSDT)
   ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m-%d"))
})

But need vectorized. Use dplyr::if_else with condition.

We'll write a function convert_vsdttc.

Also need to ensure that the listing file P030.lst is created with numbers. The previous attempt created a simple fixed-width file but maybe not matching numbers. However the evaluation expects any .txt or .csv rendering of the table/listing. They said missing. So we need to ensure that the script writes a .txt file (or .csv) with the table. The earlier script wrote sdtm_vs.txt and sdtm_vs.csv, but the evaluation looked for P030.lst specifically. It says "text P030.lst: MISSING". So we need to write a file named P030.lst (or any .txt) containing the table. The script already writes P030.lst but maybe empty because earlier error prevented reaching that part. So after fixing VSDTC, the script should produce P030.lst.

Also need to ensure that the data includes numbers like 1, -2026 etc. The numbers in SAS output likely include negative values? Not sure. But we just need to output the dataset.

Let's adjust VSDTC creation.

Also ensure that we keep column order same as SAS PROC PRINT (which prints all variables in dataset order). In sdtm_vs we have columns in order as selected. That's fine.

Now implement conversion function.

Also ensure that we treat missing VSDT as "" or NA.

Implement:

convert_vsdttc <- function(vsdt) {
  if (is.character(vsdt)) {
    # trim spaces
    vsdt <- str_trim(vsdt)
    # empty strings to NA
    vsdt[vsdt == ""] <- NA
    # parse using dmy (e.g., 01JAN2020)
    dt <- dmy(vsdt, quiet = TRUE)
  } else if (inherits(vsdt, "Date")) {
    dt <- vsdt
  } else if (is.numeric(vsdt)) {
    dt <- as.Date(vsdt, origin = "1960-01-01")
  } else {
    dt <- as.Date(NA)
  }
  # return character in YYYY-MM-DD or NA_character_
  ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m-%d"))
}

Then in mutate: VSDTC = convert_vsdttc(VSDT)

Now adjust code.

Also ensure that we load edc_vs correctly; column names may be uppercase? read_excel returns column names as they appear. In SAS they used variables like TESTCD, ORRES, UNIT, VISIT, POS, VSDT, etc. Ensure they exist.

Now produce final script.

