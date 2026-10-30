We need to fix ordering issue: The mismatches show that MHTERM values are misaligned with dates. Likely sorting by MHSTDTC as character string not date, causing order differences when dates are missing? Actually SAS sorts by USUBJID then MHSTDTC (character variable). In SAS, MHSTDTC is a character string of ISO date (YYYY-MM-DD). Missing values sort first. In R we used arrange(USUBJID, MHSTDTC) where MHSTDTC is character, which should sort same as SAS (character). However mismatches show swapping of MHTERM within same USUBJID? Possibly because we didn't retain original order within same USUBJID when MHSTDTC missing? In SAS, after first data step, they keep only those columns, then sort by USUBJID MHSTDTC. If MHSTDTC missing, they sort missing first. Within same USUBJID and same MHSTDTC (including missing), SAS retains original order (stable sort?). In R, arrange is stable? dplyr's arrange is stable (preserves order of ties). So why mismatch? Maybe the date conversion produced NA for some rows where SAS had a date, causing different ordering. The mismatched rows show many swaps, likely due to date parsing differences: SAS ANYDTDTE. may interpret dates differently than our parse_anydtdte. We used parse_date_time with many orders, but maybe SAS interprets ambiguous dates as day-month-year vs month-day-year based on locale? We need to mimic SAS ANYDTDTE. SAS uses US locale? Actually ANYDTDTE. reads many formats, but ambiguous dates like 01/02/2020 interpreted as 01FEB2020? Not sure. The mismatches are systematic: pairs swapped, suggests that for a given USUBJID, two records have dates that are swapped because our parsing interpreted date order opposite. So need to ensure we parse dates exactly as SAS ANYDTDTE. Could use haven::as_sas_date? Not. Maybe the raw data has dates in format "DD-MMM-YYYY" etc. Let's inspect typical raw_mh.csv? Not available. But we can approximate: Use readr::parse_date_time with orders that prioritize day-month-year? SAS ANYDTDTE uses DMY if ambiguous? Actually SAS ANYDTDTE. reads dates in many forms, but if ambiguous, it uses the DATESTYLE system option (default MDY). In US, default is MDY. So ambiguous like 01/02/2020 => Jan 2 2020 (MDY). Our orders include both MDY and DMY; parse_date_time may pick first matching order, which is "Ymd", then "Y-m-d", etc, then "mdY", then "dmY". Since we have both mdY and dmY, ambiguous "01/02/2020" could match mdY (Jan 2) or dmY (Feb 1). parse_date_time will try orders sequentially and pick first that works. For "01/02/2020", both mdY and dmY parse, but which is first? mdY is before dmY, so it will interpret as Jan 2 (MDY). That's same as SAS default MDY, so should match. But mismatches show swapping, maybe dates are in format "02/01/2020" (Feb 1) and SAS interprets as Feb 1 (MDY) but our parse may interpret as Jan 2? Actually "02/01/2020": MDY => Feb 1, DMY => Jan 2. Since mdY is before dmY, we get Feb 1, matches SAS. So maybe other format like "2020-02-01"? That's fine.

Maybe the issue is that we treat DIAGNOSIS_DATE as character when it's numeric but stored as character in CSV? In SAS, PROC IMPORT may infer numeric SAS date (days). In R, read_csv guessed numeric? If column is numeric, we treat as numeric and convert via sas_num_to_date. That's fine.

But mismatches show many swaps across rows, maybe because we didn't sort by MHSTDTC as character but as date? Actually we kept MHSTDTC as character after formatting date. That's fine.

But maybe SAS sorts missing dates first, and our NA handling leads to NA (character NA) which sorts after? In arrange, NA values are placed at the end by default (unless na.last = FALSE). In dplyr, arrange puts NA at the end. SAS sorts missing values first. So rows with missing MHSTDTC will be placed after those with dates, causing order shift. That would cause many swaps. Indeed mismatches show many rows swapped, likely those with missing dates. So need to sort with missing first. In dplyr, we can use arrange with desc(is.na(MHSTDTC))? Actually we want missing first, so we can arrange(USUBJID, is.na(MHSTDTC), MHSTDTC). Since FALSE < TRUE, is.na FALSE (not missing) will be first; we need missing first, so is.na TRUE first => we can arrange(USUBJID, desc(is.na(MHSTDTC)), MHSTDTC). Or use factor with levels.

Implement: mh <- mh %>% arrange(USUBJID, desc(is.na(MHSTDTC)), MHSTDTC)

But need to ensure that within missing, original order preserved. Since we lose original order after mutate, we can add a row index before sorting.

Add raw_mh %>% mutate(_orig_order = row_number()) then after sorting, arrange(USUBJID, desc(is.na(MHSTDTC)), MHSTDTC, _orig_order). Then drop _orig_order.

Let's modify script accordingly.

Also ensure that MHSTDTC is character; missing values are NA_character_, arrange will treat NA as missing; but we need to sort missing first, then by date string.

Implement.

Also ensure that case_when warning resolved: we can replace with if_else using ifelse? Simpler: use if (is.character) else.

We'll just compute mhdt using ifelse with vectorized: ifelse(is.character(...), parse_anydtdte(...), sas_num_to_date(...)). But parse_anydtdte expects character; if numeric, will coerce to character causing parse errors. Better: use ifelse with is.character, but both branches need same length. Use dplyr::if_else which is type-stable. We'll do:

mhdt <- if_else(is.character(mhdt_raw),
                parse_anydtdte(str_trim(mhdt_raw)),
                sas_num_to_date(mhdt_raw))

But if mhdt_raw is numeric, is.character returns FALSE, then sas_num_to_date works. However if mhdt_raw is character but numeric strings, parse_anydtdte will parse.

Implement.

Let's produce final script.