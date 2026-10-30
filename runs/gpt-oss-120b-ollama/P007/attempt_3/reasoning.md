We need to improve listing text to match SAS PROC PRINT output. The SAS PROC PRINT prints variables: USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT. It prints default formatting: numeric without decimals (probably integer). Missing values appear as blank. Dates printed in default SAS date format (DATE9.) e.g., 01JAN2020. Also column widths? The mismatch numbers likely due to rounding or missing spaces. The numbers missing: 2,5,6,7,8,10,11,14 etc maybe row numbers? Actually SAS PROC PRINT includes observation numbers? By default PROC PRINT prints Obs column (observation number) unless NOOBS option. In code they didn't specify NOOBS, so default includes Obs column as first column. So our listing missing Obs column. Also SAS prints variable names as column headers with maybe spacing. Need to include Obs column with numbers 1-15. Also need to ensure numeric values printed as integer without decimal, and missing as blank. Also need to ensure CNSR printed as 0/1 (no decimal). AVAL integer. Also need to ensure dates printed as DATE9 (e.g., 01JAN2020). Already using sas_date9.

Thus modify listing_fmt to include Obs column. Also ensure CNSR and AVAL are printed as integer (no decimal). Use as.character of round_half_up with 0 decimals but no trailing .0.

Also need to ensure header includes Obs as first column.

Also need to ensure spacing matches SAS? The evaluation likely just checks numbers present, not spacing. So adding Obs column should capture missing numbers (like 1-15). The numbers missing earlier include 2,5,6,7,8,10,11,14 which are observation numbers. So add Obs.

Implement: listing_df with row_number() as Obs.

Also ensure ordering of columns: Obs USUBJID TRT01A PARAM CNSR AVAL EVENTDT EOSDT.

Implement.

Also ensure title line maybe not needed? SAS listing includes title line separate; they may compare only table body? But numbers missing are obs numbers, so include.

Thus modify script.

