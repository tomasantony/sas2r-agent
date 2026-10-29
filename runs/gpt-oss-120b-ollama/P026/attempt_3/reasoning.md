We need to fix error: LBDT column likely vector, convert_LBDT expects scalar. In mutate, we need vectorized function. Modify convert_LBDT to handle vector via sapply or vectorize. Also ensure column names: In edc_lb, column names may be uppercase? Need to check. The mismatches earlier indicated swapped rows: ALT vs HGB etc. Likely because ordering of rows after sort differs due to character sorting case sensitivity. SAS sorts by USUBJID (character) with uppercase before lowercase. Our USUBJID all uppercase, fine. But the mismatched rows indicate that rows 4 and 5 swapped test codes and categories. That suggests original data order after sort maybe different due to original order of edc_lb before sort? In SAS, after creating lb_prep, they sort by USUBJID. Then merge with dm_for_lb (by USUBJID). Since both sorted, merge retains order of lb_prep (by USUBJID). Our code does left_join which may reorder rows (by USUBJID but not guarantee stable order). We need to preserve order of lb_prep after sort. So after left_join, we should arrange by USUBJID (already sorted) and maybe by original row order within same USUBJID (none). Use left_join then arrange(USUBJID) again.

Also need to ensure LBDT column name: In edc_lb, maybe "LBDT" is numeric? Actually raw data likely has LBDT as character date string like "01JAN2020". We'll treat as character.

Our convert_LBDT should handle vector: use ifelse.

Implement:

convert_LBDT_vec <- function(x) {
  sapply(x, function(v){
    if (is.na(v) || str_trim(v) == "") return("")
    dt <- as.Date(toupper(str_trim(v)), format = "%d%b%Y")
    if (!is.na(dt)) return(format(dt, "%Y-%m-%d"))
    dt2 <- ymd(v, quiet = TRUE)
    if (!is.na(dt2)) return(format(dt2, "%Y-%m-%d"))
    dt3 <- as.Date(v, format = "%Y%m%d")
    if (!is.na(dt3)) return(format(dt3, "%Y-%m-%d"))
    ""
  }, USE.NAMES = FALSE)
}

Then in mutate LBDTC = convert_LBDT_vec(LBDT)

Also need to ensure LBORRES numeric conversion: In SAS, LBORRES is character (original ORRES). In our dataset, ORRES may be numeric or character. Keep as character. Our earlier conversion of LBORRES = ORRES (character). Good.

But mismatched categories: LBCAT values swapped. That likely due to original data having CAT column values; maybe we need to keep original order after sort. The swap may be due to our left_join reordering rows differently (maybe alphabetical order of USUBJID same, but within same USUBJID there are multiple rows with same USUBJID? Possibly multiple tests per subject. The order of rows for same USUBJID may be by original order of lb_prep, which after sort is by USUBJID only, not by any other variable, so original order among same USUBJID is preserved (stable sort). In R, arrange is stable? dplyr's arrange is stable (preserves order of equal keys). So after arrange, order of rows with same USUBJID is original order in lb_prep. Our left_join may reorder rows by the order of the right data (dm_for_lb) which has one row per USUBJID, so join may produce rows in order of left (lb_prep) but I think left_join keeps order of x (lb_prep). Should be fine. However earlier mismatch suggests that rows 4 and 5 swapped test codes: maybe because our mapping of test names uses case_when but we didn't preserve original order of rows after sort? Let's just ensure after all processing we sort by USUBJID and LBDTC (as SAS later). That may reorder rows differently than SAS's PROC PRINT which prints in dataset order (which after step 5 is sdtm_lb after deriving LBSEQ). In SAS, after deriving LBSEQ, they didn't sort again; they kept order from sdtm_lb1 (which was sorted by USUBJID LBDTC). Then they added LBSEQ by first. So final order is by USUBJID then LBDTC. Our code does arrange(USUBJID, LBDTC) before adding LBSEQ, which matches. So why swap? Possibly LBDTC values missing causing order differences. If LBDTC missing, order may be by NA (missing first). In SAS, missing values sort first. In R, arrange puts NA last by default. Need to replicate SAS sorting: missing first. Use arrange with .by_group? Use order with is.na. Use arrange with desc(is.na(LBDTC))? Actually we want NA first, then non-NA ascending. In dplyr, arrange puts NA last. To mimic SAS, we can use arrange(is.na(LBDTC), LBDTC). That will put TRUE (missing) first? is.na returns TRUE for missing; we want missing first, so arrange(is.na(LBDTC), LBDTC). That will order FALSE (non-missing) before TRUE? Actually FALSE < TRUE, so non-missing first. We need missing first, so arrange(desc(is.na(LBDTC)), LBDTC). Or use arrange(is.na(LBDTC), LBDTC) and then set decreasing? Let's test: Suppose vector c(NA, "2020-01-01"): is.na = c(TRUE, FALSE). arrange sorts by TRUE then FALSE? In R, FALSE < TRUE, so FALSE first. So missing (TRUE) would be after. So we need desc(is.na(...)). So arrange(desc(is.na(LBDTC)), LBDTC). That puts missing first.

Thus modify sorting before LBSEQ: arrange(USUBJID, desc(is.na(LBDTC)), LBDTC)

Also need to ensure LBDTC is character; sorting works lexicographically same as SAS date string? SAS sorts by character order of date string "YYYY-MM-DD". That's fine.

Now ensure we write outputs to OUT_DIR. The previous error prevented writing. Also need to ensure we create out_dir if not exists.

Add dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

Now produce listing text: need to match numbers formatting. Our print may not format numeric with same decimals. SAS PROC PRINT default prints numeric with best12. Might show decimals as needed. Our print may show many decimals. To mimic, we can format numeric columns to remove trailing .0? In SAS output, numbers like 30.0 appear as 30.0 (one decimal). We can format using format with trim = TRUE? Simpler: use write.table with sep = "\t"? But requirement: produce plain-text rendering containing same numbers. Could use write.table with row.names = FALSE, col.names = TRUE, sep = " ". That may be acceptable.

Let's produce listing_txt using write.table to a temporary connection and capture.

listing_txt <- capture.output(
  write.table(sdtm_lb, file = "", sep = " ", row.names = FALSE, col.names = TRUE, quote = FALSE)
)

That will produce space-separated values.

Now ensure numeric formatting: use format with trim = TRUE, nsmall = 0? Use format(sdtm_lb, scientific = FALSE, trim = TRUE). But write.table will coerce to character using as.character which may produce scientific for large numbers. Probably fine.

Now implement final script.

