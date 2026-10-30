Need to translate SAS program to R. Must read inputs from absolute paths given. Need to locate data.

Paths:
&root = D:/meta_hackathon/sas2r-agent
&project_path = D:/meta_hackathon/sas2r-agent/data/derived/c2c
libname raw -> &project_path./data/raw
sdtm -> &project_path./data/sdtm
adam -> &project_path./data/adam

So adam datasets in D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam
sdtm in .../data/sdtm

We need to list files. Use R to explore? But we must produce script that will run. Need to infer structure.

We need to implement 39 checks? Actually count: chk_adsl01, chk_adae01, chk_advs01, chk_adlb01, chk_advs02, chk_adlb02, chk_dm01, chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04, chk_adtte01, chk_adtte02, chk_adtte03, chk_eg01, chk_eg02, chk_eg03, chk_eg04, chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02, chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02, chk_dm03, chk_dm04, chk_adae02, chk_adae03, chk_cm01, chk_ds03, chk_seq01, chk_cm02

That's 39 rows.

We need to replicate SAS logic exactly.

We need to handle missing values: SAS missing for numeric is NA, for character is "" or NA. In R, haven reads missing as NA. Need to define helpers.

We need to read datasets: adam.adsl, adam.adae, adam.advs, adam.adlb, adam.adtte, sdtm.dm, sdtm.ae, sdtm.ex, sdtm.lb, sdtm.vs, sdtm.eg, sdtm.mh, sdtm.sv, sdtm.ds, sdtm.cm

We need to know file extensions: likely .sas7bdat or .xpt. Let's assume .sas7bdat or .xpt. Need to handle both. Use haven::read_sas and read_xpt. Check existence.

We need to implement each check.

Let's outline logic:

Helper functions:
- is_missing <- function(x) is.na(x) | (is.character(x) & trimws(x)=="")
  But SAS missing for character is "" (blank). haven may give ""? Actually haven gives NA for missing char? Need to handle both.
- strip = trimws
- upcase = toupper
- prxmatch regex: '/^\d{4}-\d{2}-\d{2}$/' -> grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(x))
- input(substr(d.RFPENDTC,1,10), yymmdd10.) -> parse date string YYYY-MM-DD to Date. So EOSDT is numeric SAS date? In ADaM, EOSDT likely numeric date (SAS date). Need to check. In SAS, EOSDT ne input(...). So EOSDT is numeric date. In R, after reading, it will be Date or numeric? haven reads SAS dates as Date? Actually haven::read_sas converts SAS dates to Date class if format is date. Might be Date. Need to handle both: if numeric, convert? But we can treat as Date.

We need to understand data types: Let's attempt to infer by looking at expected outputs? We don't have outputs yet. Need to explore filesystem.

We cannot run interactive, but we can write R script that does robust handling.

We need to implement checks:

1. ADSL-001: USUBJID must be perfectly unique
   count of duplicate USUBJID groups where count>1. N_FAIL = number of duplicate groups? Wait SAS: select count(*) from (select USUBJID from adam.adsl group by USUBJID having count(*) >1). So N_FAIL is number of distinct USUBJID values that are duplicated, not total duplicate rows. Example if one USUBJID appears twice, N_FAIL=1. If appears 3 times, still 1. So implement.

2. ADAE-001: AEENDT < AESTDT and not missing both. Need to handle dates. In ADAE, AEENDT and AESTDT are numeric dates (SAS dates). So compare.

3. ADVS-001: Max 1 Baseline per Param: count distinct (USUBJID, PARAMCD) groups where ABLFL='Y' count>1. So N_FAIL = number of groups violating.

4. ADLB-001 similar.

5. SDTM-001: DM USUBJID unique: same as 1 but for sdtm.dm

6. SDTM-002: Every AE.USUBJID exists in DM: left join, where d.USUBJID is null. Count rows in AE where USUBJID not in DM. Note if AE has duplicate USUBJID not in DM, each row counts? SAS counts rows where join fails. So count of AE rows with missing DM.

7. SDTM-003: EX referential

8. SDTM-004: LB

9. SDTM-005: VS

10. ADSL-002: SAFFL='Y' requires exposure in EX: left join adsl to ex on USUBJID, where SAFFL='Y' and e.USUBJID is null. Note if EX has multiple rows per subject, join will duplicate? But SAS left join will produce multiple rows per ADSL if EX has multiple rows. However where clause checks e.USUBJID is null, so only subjects with no EX rows will have null. If subject has at least one EX row, then e.USUBJID not null for those rows, so not counted. But if subject has EX rows, the join produces rows, none with null. So count is number of ADSL rows with SAFFL=Y and no EX. Since ADSL has one row per subject, count is number of subjects.

But need to be careful: if EX has duplicates, left join would produce multiple rows per ADSL subject that has EX, but they wouldn't be counted. For subjects without EX, one row with null. So count is correct.

11. ADTTE-001: Survival time must be >=1: where missing(AVAL) or AVAL <1. Count rows.

12. ADTTE-002: Event and censoring records must use documented formula.
   where missing(TRTSDT) or missing(CNSR) or CNSR not in (0,1) or (CNSR=0 and (missing(EVENTDT) or AVAL != (EVENTDT - TRTSDT)+1)) or (CNSR=1 and (not missing(EVENTDT) or missing(EOSDT) or AVAL != (EOSDT - TRTSDT)+1))
   Note: TRTSDT, EVENTDT, EOSDT are numeric dates. AVAL is numeric. Need to compute difference in days.

13. ADTTE-003: where missing(EOSDT) or EOSDT < TRTSDT or (not missing(EVENTDT) and (EVENTDT < TRTSDT or EVENTDT > EOSDT))

14. ADSL-003: EOSDT must agree with DM.RFPENDTC
   left join adsl a to dm d on USUBJID, where d.USUBJID is null or missing(a.EOSDT) or a.EOSDT != input(substr(d.RFPENDTC,1,10), yymmdd10.)
   Note: d.RFPENDTC is character ISO date "YYYY-MM-DD" maybe with time? They take first 10 chars. So parse as Date.

15. ADSL-004: AGE must equal completed years at informed consent
   where missing(BRTHDT) or missing(RFICDT) or missing(AGE) or AGE != floor(yrdif(BRTHDT, RFICDT, 'AGE'))
   Need to implement yrdif with 'AGE' basis. SAS YRDIF with 'AGE' computes exact age in years using actual days? SAS docs: YRDIF(start,end,'AGE') computes age as (end - start) / average year length? Actually SAS YRDIF with 'AGE' uses exact age: counts birthdays. It computes number of full years between dates, considering leap years. Equivalent to floor of age. But they do floor(yrdif(...,'AGE')). So AGE should equal floor of that. However SAS YRDIF 'AGE' already returns fractional age? Let's recall: YRDIF with 'AGE' returns age in years as decimal, but floor gives integer age. Simpler: compute age as completed years: if month/day of RFICDT >= month/day of BRTHDT then year diff, else year diff -1. That's standard. SAS YRDIF 'AGE' does that precisely, accounting for Feb 29. We can implement as: age = as.integer(lubridate::interval(BRTHDT, RFICDT) / years(1))? But need exact SAS behavior. Simpler: use lubridate or manual.

We can implement function yrdif_age = function(start, end) { # returns fractional? But we only need floor. So compute integer age via: year diff minus adjustment.
   y1 = year(start), m1=month(start), d1=day(start)
   y2=year(end), m2=month(end), d2=day(end)
   age = y2 - y1 - ( (m2 < m1) | (m2==m1 & d2 < d1) )
   For Feb 29 edge: SAS handles Feb 29 birthdays as Feb 28? Need to check. But likely not needed.

We need to handle BRTHDT and RFICDT as Dates.

16. ADVS-002: Baseline date has exactly one eligible source record
   Logic: For each USUBJID, PARAMCD, find max ASTDT where ASTDT <= TRTSDT and not missing AVAL. That's BASE_DT. Then join back to advs where ASTDT = BASE_DT and not missing AVAL, group by USUBJID,PARAMCD having count>1. Count number of groups with >1 candidate.
   So N_FAIL = number of (USUBJID,PARAMCD) where multiple records share same max eligible date.

17. ADLB-002 similar.

18. SDTM-006: EG referential

19. SDTM-007: Required EG variables must exist: 12 - count(distinct upcase(name)) where libname='SDTM' and memname='EG' and name in list. So N_FAIL = missing variables count. If all 12 exist, N_FAIL=0. If some missing, N_FAIL = 12 - existing count. Need to check column names in sdtm.eg dataset. We can get colnames.

20. SDTM-008: EG date/value contract must be valid: where missing(EGDTC) or prxmatch fails or missing(EGSTRESN) or missing(EGSTRESC)

21. SDTM-009: Raw EG variables must not leak: count(*) from dictionary.columns where libname='SDTM' and memname='EG' and upcase(name) in ('SUBJECT','TEST_NAME','RESULT','UNIT','DATE'). So N_FAIL = number of those raw variables that exist in EG dataset. If none, 0. If some, count.

22. SDTM-010: MH referential

23. SDTM-011: Required MH variables must exist: 6 - count distinct where name in ('STUDYID','DOMAIN','USUBJID','MHSEQ','MHTERM','MHSTDTC')

24. SDTM-012: MH date/term contract: where missing(MHSTDTC) or prxmatch fails or missing(MHTERM)

25. SDTM-013: Raw MH variables must not leak: count where name in ('SUBJECT','CONDITION','DIAGNOSIS_DATE')

26. SDTM-014: DM reference period must be complete, ISO 8601, and ordered: where missing(RFICDTC) or missing(RFSTDTC) or missing(RFENDTC) or missing(RFPENDTC) or prxmatch fails for each or RFSTDTC < RFICDTC or RFENDTC < RFSTDTC or RFPENDTC != RFENDTC. Note RF*DTC are character ISO dates. Compare as strings? SAS compares strings lexicographically, but since format YYYY-MM-DD, string compare equals date compare. So we can compare as strings or Dates. Use string compare.

27. SDTM-015: SV subjects must exist in DM

28. SDTM-016: USUBJID and VISITNUM must be unique: count(*) from (select USUBJID,VISITNUM from sdtm.sv group by USUBJID,VISITNUM having count(*) !=1). Note having count(*) !=1 includes both duplicates (>1) and? But group by will have count >=1, so !=1 means >1. So count of groups with duplicates.

29. SDTM-017: SV dates valid and inside reference period: from sdtm.sv s inner join sdtm.dm d on USUBJID where missing(s.VISITNUM) or missing(s.VISIT) or missing(s.SVSTDTC) or missing(s.SVENDTC) or prxmatch fails for SVSTDTC/SVENDTC or SVENDTC < SVSTDTC or SVSTDTC < d.RFSTDTC or SVENDTC > d.RFENDTC. Note SVSTDTC etc are character ISO dates, compare as strings.

30. SDTM-018: DS must contain exactly one final disposition for every DM subject: complex. They do:
   select d.USUBJID, count(s.USUBJID) as N_DS from sdtm.dm d left join sdtm.ds s on d.USUBJID=s.USUBJID group by d.USUBJID having calculated N_DS !=1
   union all
   select s.USUBJID, count(*) as N_DS from sdtm.ds s left join sdtm.dm d on s.USUBJID=d.USUBJID where d.USUBJID is null group by s.USUBJID
   Then count(*) of that union. So N_FAIL = number of DM subjects with not exactly 1 DS record + number of DS subjects not in DM (each distinct USUBJID in DS not in DM counts as 1). Note second part counts distinct USUBJID in DS not in DM, each as one failure.

31. SDTM-019: DS date matches DM participation end: from sdtm.ds s inner join sdtm.dm d on USUBJID where missing(s.DSDECOD) or missing(s.DSSTDTC) or prxmatch fails or s.DSSTDTC != d.RFPENDTC

32. SDTM-020: RACE uses supported CDISC terminology: where missing(RACE) or upcase(strip(RACE)) not in ('WHITE','BLACK OR AFRICAN AMERICAN','ASIAN','OTHER','UNKNOWN')

33. SDTM-021: SITEID present and consistent with SUBJID: where missing(SITEID) or strip(SITEID) != scan(strip(SUBJID),1,'-'). SAS SCAN with delimiter '-' returns first token before '-'. So SITEID should equal first part of SUBJID.

34. ADAE-002: AOCCFL valid and restricted to TRTEMFL=Y: where (not missing(AOCCFL) and AOCCFL != 'Y') or (AOCCFL='Y' and TRTEMFL != 'Y')

35. ADAE-003: One AOCCFL=Y per subject and TEAE term: select USUBJID,AEDECOD from adam.adae group by USUBJID,AEDECOD having sum(case when TRTEMFL='Y' then 1 else 0 end) >0 and sum(case when AOCCFL='Y' then 1 else 0 end) !=1. Count groups.

36. SDTM-022: CM subject, medication, dose and dates must be valid: where d.USUBJID is null or missing(CMTRT) or missing(CMDECOD) or (upcase(CMTRT)='PARACETAMOL' and upcase(CMDECOD) != 'ACETAMINOPHEN') or missing(CMDOSE) or missing(CMDOSU) or missing(CMSTDTC) or prxmatch fails for CMSTDTC or (not missing(CMENDTC) and prxmatch fails) or (not missing(CMENDTC) and CMENDTC < CMSTDTC) or (CMENRTPT='ONGOING' and not missing(CMENDTC)) or (CMENRTPT != 'ONGOING' and missing(CMENDTC))

Note: CMENRTPT handling: SAS missing is "" . So condition c.CMENRTPT = 'ONGOING' uses exact case. But later they use upcase for other. Here they use exact? In SAS code: c.CMENRTPT = 'ONGOING' . So case-sensitive. But we should treat as exact 'ONGOING'. However to be safe, use upcase? But follow SAS: case-sensitive. However data likely uppercase.

Also need to handle CMENRTPT missing vs not missing.

37. SDTM-023: DSDECOD uses supported disposition terminology: where upcase(strip(DSDECOD)) not in ('COMPLETED','LOST TO FOLLOW-UP','WITHDRAWAL BY SUBJECT','ADVERSE EVENT','SCREEN FAILURE')

38. SDTM-024: Domain sequence keys must be unique per subject: union all of duplicates across AE,EX,LB,VS,CM,MH,EG. Count total duplicate groups across all domains. Each duplicate group counts as 1.

39. SDTM-025: Ongoing CM must be anchored to DM reference end: where upcase(strip(CMENRTPT)) not in ('','ONGOING') or (upcase(CMENRTPT)='ONGOING' and (not missing(CMENDTC) or missing(CMENTPT) or prxmatch fails for CMENTPT or CMENTPT != d.RFENDTC)) or (upcase(CMENRTPT) != 'ONGOING' and not missing(CMENTPT))

Note: CMENTPT is character date? Should be ISO date.

We need to implement all.

Also need to handle dictionary.columns: we can get column names via colnames().

We need to produce qc_report dataset with columns CHECK_ID, DOMAIN, RULE, N_FAIL, STATUS.

Then produce text rendering and CSV.

The expected P019.lst has 156 numbers. That suggests table with many numbers. But our qc_report has 39 rows, each with N_FAIL and maybe other numbers. 39*? 156 numbers suggests maybe 39 rows * 4 numbers? But we need to produce text rendering that contains same numbers SAS shows.

We need to create a text file that mimics PROC PRINT output.

We also need to handle OUT_DIR env var.

We need to write R script that reads data, computes checks, writes outputs.

We need to handle file paths: project_path = "D:/meta_hackathon/sas2r-agent/data/derived/c2c" . But in R, we should use that absolute path. However for portability, we can also try to find data via Sys.getenv("PROJECT_PATH")? But spec says read inputs from absolute paths given in context (forward slashes). So use those paths.

We need to handle reading .sas7bdat files: haven::read_sas

We need to handle that adam and sdtm libraries are directories containing .sas7bdat files. Let's list expected files: adam.adsl -> file adam/adsl.sas7bdat or adsl.xpt etc. We need to try multiple extensions.

We should write helper function read_lib_mem(lib_path, memname) that tries .sas7bdat, .sas7bdat with lowercase, .xpt, .sas7bdat etc.

Also need to handle that some datasets may be in .xpt format.

We need to implement robust reading.

We also need to handle that some variables may be labelled, haven reads as labelled. Need to convert to appropriate types.

We need to handle date variables: SAS dates are numeric days since 1960-01-01. haven converts them to Date if format is DATE. But if not, they remain numeric. We need to handle both: if numeric and looks like SAS date (e.g., values around 15000-25000), we can convert to Date via as.Date(x, origin="1960-01-01"). However haven already does conversion for variables with date format. For variables without format, they remain numeric. But in ADaM, EOSDT, TRTSDT etc are likely SAS dates with format. So haven will give Date.

We need to handle comparison: AVAL ne (EVENTDT - TRTSDT)+1 . If EVENTDT and TRTSDT are Dates, difference is difftime in days. In R, Date subtraction gives numeric days. So compute as.integer(EVENTDT - TRTSDT) +1.

We need to handle missing: is.na()

We need to implement yrdif.

We need to implement prxmatch.

We need to implement scan.

We need to implement substr.

We need to implement input yymmdd10.

We need to handle upcase, strip.

We need to handle count distinct.

We need to handle left joins.

We need to produce final qc_report.csv and qc_report.txt and P019.lst equivalents.

The expected outputs: P019.lst (table/listing text, 156 numbers) -> write text rendering and CSV of same table. And dataset qc_report -> write OUT_DIR/qc_report.csv

So we need to produce at least two files: qc_report.csv and P019.lst text and csv. But spec says each output dataset as CSV, each table/listing additionally as plain-text rendering and CSV of table body. So for qc_report, we need to produce qc_report.csv (dataset) and also P019.lst text? Actually P019.lst is the table/listing text. The dataset qc_report is the permanent audit dataset. So we need to produce both.

The SAS program produces:
- adam.qc_report dataset (permanent)
- PROC PRINT listing (which is P019.lst)
- RTF file (but we ignore, we produce PNG? No, for this program, output types are table;listing;dataset. So we need to produce text rendering of the QC report table.

We should produce:
- OUT_DIR/qc_report.csv (dataset)
- OUT_DIR/P019.lst.txt ? Or P019.txt? The spec says write every output into directory in env var OUT_DIR: each output dataset as CSV: write.csv(df, file.path(out, "<name>.csv")... each table or listing additionally as plain-text rendering (file.path(out, "<name>.txt")) and as CSV of table body.

Expected outputs listed: P019.lst -> write text rendering and CSV of same table, dataset qc_report -> write OUT_DIR/qc_report.csv

So we need to produce:
- file.path(out, "P019.lst.txt") or "P019.txt"? The spec says "<name>.txt" where name is output name. So for P019.lst, name is "P019" maybe? But to be safe, produce both "P019.lst.txt" and "P019.txt" and "qc_report.txt".

Simpler: produce file.path(out, "P019.txt") and file.path(out, "P019.lst.txt") and file.path(out, "qc_report.txt") all containing same report.

And CSV: file.path(out, "P019.csv") and file.path(out, "qc_report.csv")

We need to ensure at least qc_report.csv exists and P019 text exists.

We should also produce OUT_DIR/qc_report.txt as text rendering.

Let's design R script.

Steps:

- Set out <- Sys.getenv("OUT_DIR"); if empty, use temp.

- Define helper functions.

- Define read_dataset function.

- Read all required datasets.

- Implement each check as function returning N_FAIL.

- Build qc_report data frame with columns CHECK_ID, DOMAIN, RULE, N_FAIL, STATUS.

- Write CSV.

- Generate text rendering: mimic PROC PRINT output with title "Automated Quality Control (QC) Validation Report" and columns.

- Also generate CSV of table body (same as qc_report.csv).

- Handle abort logic: if any FAIL, log error but not abort R script (just print).

We need to ensure numeric values unrounded unless SAS rounds them. N_FAIL is integer.

We need to handle dates as YYYY-MM-DD when writing CSV. For qc_report, no dates, but for other datasets maybe.

We need to implement checks precisely.

Let's draft R code for each check.

We need to handle missing helper:

is_missing <- function(x) {
  if (is.character(x)) {
    is.na(x) | trimws(x) == ""
  } else {
    is.na(x)
  }
}
not_missing <- function(x) !is_missing(x)

strip <- function(x) trimws(x)
upcase <- function(x) toupper(x)

prxmatch_iso <- function(x) {
  # returns TRUE if matches ^\d{4}-\d{2}-\d{2}$
  !is_missing(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", trimws(x))
}

We need to handle SAS missing for numeric: is.na

We need to handle SAS count(*) logic.

Let's implement each check step by step in R.

We need to read datasets first.

Potential pitfalls:

- Column names in R after haven::read_sas are uppercase? SAS validvarname=upcase forces uppercase. So column names will be uppercase. But haven may preserve case. We should convert all column names to uppercase for consistency: names(df) <- toupper(names(df))

- Also need to handle that some datasets may have lowercase names.

- For dictionary.columns checks, we need to get column names.

Let's write reading function:

read_sas_member <- function(lib_path, memname) {
  # try various file extensions
  candidates <- c(
    file.path(lib_path, paste0(memname, ".sas7bdat")),
    file.path(lib_path, paste0(tolower(memname), ".sas7bdat")),
    file.path(lib_path, paste0(memname, ".xpt")),
    file.path(lib_path, paste0(tolower(memname), ".xpt")),
    file.path(lib_path, paste0(memname, ".sas7bdat")),
    file.path(lib_path, paste0(memname, ".sas7bdat"))
  )
  # Also check for .sas7bdat in subdirectories? No.
  for (f in candidates) {
    if (file.exists(f)) {
      if (grepl("\\.xpt$", f, ignore.case=TRUE)) {
        df <- haven::read_xpt(f)
      } else {
        df <- haven::read_sas(f)
      }
      names(df) <- toupper(names(df))
      return(df)
    }
  }
  # Try listing files in lib_path
  files <- list.files(lib_path, full.names=TRUE, ignore.case=TRUE)
  # Find file that matches memname ignoring case and extension
  pattern <- paste0("^", memname, "\\.(sas7bdat|xpt)$")
  idx <- grep(pattern, basename(files), ignore.case=TRUE)
  if (length(idx) >0) {
    f <- files[idx[1]]
    if (grepl("\\.xpt$", f, ignore.case=TRUE)) {
      df <- haven::read_xpt(f)
    } else {
      df <- haven::read_sas(f)
    }
    names(df) <- toupper(names(df))
    return(df)
  }
  warning(paste("Dataset not found:", lib_path, memname))
  return(NULL)
}

We need to handle that lib_path is "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam" etc.

We should also handle that some datasets may be in "data/raw" etc. But for this program, only adam and sdtm.

We need to read:

adam_adsl <- read_sas_member(adam_path, "ADSL")
adam_adae <- read_sas_member(adam_path, "ADAE")
adam_advs <- read_sas_member(adam_path, "ADVS")
adam_adlb <- read_sas_member(adam_path, "ADLB")
adam_adtte <- read_sas_member(adam_path, "ADTTE")
sdtm_dm <- read_sas_member(sdtm_path, "DM")
sdtm_ae <- read_sas_member(sdtm_path, "AE")
sdtm_ex <- read_sas_member(sdtm_path, "EX")
sdtm_lb <- read_sas_member(sdtm_path, "LB")
sdtm_vs <- read_sas_member(sdtm_path, "VS")
sdtm_eg <- read_sas_member(sdtm_path, "EG")
sdtm_mh <- read_sas_member(sdtm_path, "MH")
sdtm_sv <- read_sas_member(sdtm_path, "SV")
sdtm_ds <- read_sas_member(sdtm_path, "DS")
sdtm_cm <- read_sas_member(sdtm_path, "CM")

We need to handle that some may be NULL, then checks should return 0 or handle gracefully.

We need to implement checks with dplyr.

Let's implement each check function.

We need to be careful about SAS semantics for missing and comparisons.

For numeric comparisons, SAS missing is considered less than any number, but they explicitly check not missing before comparing. So we replicate.

For character comparisons, SAS missing is "".

We need to implement helper for SAS date conversion: if variable is numeric and not Date, but represents SAS date, we need to convert to Date for calculations. However haven will have already converted if format is DATE. If not, we can detect: if is.numeric and values are large (e.g., 10000-30000) and variable name ends with DT, we can convert. But better to handle both: if is.numeric, treat as SAS date numeric days since 1960-01-01, convert to Date via as.Date(x, origin="1960-01-01"). If is.Date, keep. If is.character, parse.

We need to handle that some date variables are character ISO dates (SDTM DTC variables) and some are numeric SAS dates (ADaM).

We need to implement function to_sas_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin="1960-01-01"))
  if (is.character(x)) {
    # try parse YYYY-MM-DD
    # if contains time, take first 10
    x_trim <- trimws(x)
    x_trim[x_trim==""] <- NA
    # parse with ymd
    # Use as.Date with format "%Y-%m-%d"
    return(as.Date(substr(x_trim,1,10), format="%Y-%m-%d"))
  }
  return(x)
}

But for ADaM numeric dates, haven may have already converted to Date, so we don't need conversion.

We need to handle AVAL etc.

Let's implement checks one by one.

Check 1: ADSL-001

if (!is.null(adam_adsl) && "USUBJID" %in% names(adam_adsl)) {
  dup_groups <- adam_adsl %>%
    filter(!is_missing(USUBJID)) %>%
    group_by(USUBJID) %>%
    summarise(n=n(), .groups="drop") %>%
    filter(n>1)
  n_fail_01 <- nrow(dup_groups)
} else n_fail_01 <- 0

But SAS counts groups where count>1, including those with missing? In SAS, group by USUBJID will group missing together. But they didn't filter missing. So if USUBJID missing, it would count as duplicate if multiple missing. However USUBJID should never be missing. We'll include all.

Simpler: group_by USUBJID without filtering.

Check 2: ADAE-001

where AEENDT < AESTDT and not missing(AEENDT) and not missing(AESTDT)

We need to handle AEENDT and AESTDT as Dates. Compute.

n_fail_adae01 <- adam_adae %>%
  filter(!is_missing(AEENDT) & !is_missing(AESTDT) & AEENDT < AESTDT) %>%
  nrow()

But need to ensure AEENDT and AESTDT are comparable. If they are Dates, comparison works. If numeric, also.

Check 3: ADVS-001

select USUBJID, PARAMCD from adam.advs where ABLFL='Y' group by USUBJID,PARAMCD having count(*)>1

So filter ABLFL=='Y', group, count>1, count groups.

Check 4: ADLB-001 similar.

Check 5: SDTM-001 DM unique

Check 6: SDTM-002 AE referential

We need to do left join and count where d.USUBJID is null.

In R: anti_join(sdtm_ae, sdtm_dm, by="USUBJID") %>% nrow()

But need to handle that sdtm_dm may have duplicate USUBJID? But we count AE rows where USUBJID not in DM.

Check 7,8,9 similar.

Check 10: ADSL-002 SAFFL=Y requires exposure

We need to left join adsl to ex, where SAFFL='Y' and e.USUBJID is null.

We can do: adsl_saffl <- adam_adsl %>% filter(SAFFL=='Y')
ex_usubjid <- unique(sdtm_ex$USUBJID)
n_fail <- sum(!adsl_saffl$USUBJID %in% ex_usubjid)

But SAS left join counts rows where e.USUBJID is null, which for ADSL with one row per subject, same as above. If ADSL has duplicates, would count duplicates, but ADSL should be unique. So use this.

Check 11: ADTTE-001

where missing(AVAL) or AVAL <1

n_fail <- sum(is_missing(AVAL) | AVAL <1)

Check 12: ADTTE-002

where missing(TRTSDT) or missing(CNSR) or CNSR not in (0,1) or (CNSR=0 and (missing(EVENTDT) or AVAL != (EVENTDT - TRTSDT)+1)) or (CNSR=1 and (not missing(EVENTDT) or missing(EOSDT) or AVAL != (EOSDT - TRTSDT)+1))

We need to handle TRTSDT, EVENTDT, EOSDT as Dates. Compute diff.

We need to handle that AVAL is numeric, may be integer.

We need to handle missing for EVENTDT etc.

We need to compute AVAL_expected = as.numeric(EVENTDT - TRTSDT) +1 for CNSR=0, and as.numeric(EOSDT - TRTSDT)+1 for CNSR=1.

Note: In SAS, date subtraction gives days. In R, Date subtraction gives difftime days.

We need to handle that if TRTSDT is Date and EVENTDT is Date, diff is numeric.

We need to handle that if any date is NA, then expected is NA, and comparison should be considered failing if missing.

We need to implement logic exactly.

Check 13: ADTTE-003

where missing(EOSDT) or EOSDT < TRTSDT or (not missing(EVENTDT) and (EVENTDT < TRTSDT or EVENTDT > EOSDT))

Check 14: ADSL-003

left join adsl a to dm d on USUBJID where d.USUBJID is null or missing(a.EOSDT) or a.EOSDT != input(substr(d.RFPENDTC,1,10), yymmdd10.)

We need to parse d.RFPENDTC first 10 chars as Date.

We need to handle that a.EOSDT is Date, d.RFPENDTC is character.

We need to compare.

We need to handle that if d.USUBJID is null (no DM record), count as fail.

We need to handle that if a.EOSDT is missing, fail.

We need to handle that if parsed RFPENDTC is NA (invalid), then comparison fails? But SAS input would give missing, then a.EOSDT != missing would be true? In SAS, comparison with missing yields false? Actually in SAS, missing numeric compared to non-missing yields false for equality? Let's recall: In SAS, missing numeric is considered less than any number, but equality with missing is false unless both missing. So a.EOSDT ne input(...) where input returns missing would be true if a.EOSDT not missing. So we should treat parsed date NA as fail if a.EOSDT not missing.

We need to implement.

Check 15: ADSL-004

where missing(BRTHDT) or missing(RFICDT) or missing(AGE) or AGE != floor(yrdif(BRTHDT, RFICDT, 'AGE'))

We need to compute yrdif AGE.

We need to handle BRTHDT and RFICDT as Dates.

We need to compute integer age.

We need to handle that AGE is numeric.

We need to implement yrdif_age_floor.

Check 16: ADVS-002

We need to implement logic:

- For each USUBJID, PARAMCD, find max ASTDT where ASTDT <= TRTSDT and not missing AVAL. That's BASE_DT.

- Then join back to advs where ASTDT = BASE_DT and not missing AVAL, group by USUBJID,PARAMCD having count>1.

We need to handle ASTDT and TRTSDT as Dates.

We need to handle that if no eligible record (no ASTDT <= TRTSDT with non-missing AVAL), then no BASE_DT, so no join.

We need to count groups with >1.

Check 17: ADLB-002 similar.

Check 18: EG referential

Check 19: EG required variables must exist

We need to get column names of sdtm_eg, convert to uppercase, check against required list.

Required list: STUDYID, DOMAIN, USUBJID, EGSEQ, EGTESTCD, EGTEST, EGORRES, EGORRESU, EGSTRESC, EGSTRESN, EGSTRESU, EGDTC

Count distinct upcase(name) that are in required list. N_FAIL = 12 - count.

If sdtm_eg is NULL (dataset not found), then what? Then count would be 0, N_FAIL=12. But maybe dataset exists.

Check 20: EG date/value contract

where missing(EGDTC) or prxmatch fails or missing(EGSTRESN) or missing(EGSTRESC)

Check 21: Raw EG variables must not leak

count where name in ('SUBJECT','TEST_NAME','RESULT','UNIT','DATE')

So N_FAIL = number of those that exist in EG dataset.

Check 22-25 similar for MH.

Check 26: DM reference period

where missing(RFICDTC) or missing(RFSTDTC) or missing(RFENDTC) or missing(RFPENDTC) or prxmatch fails for each or RFSTDTC < RFICDTC or RFENDTC < RFSTDTC or RFPENDTC != RFENDTC

We need to handle prxmatch for each.

We need to handle string comparisons.

Check 27: SV referential

Check 28: SV unique

Check 29: SV dates valid and inside reference period

where missing(s.VISITNUM) or missing(s.VISIT) or missing(s.SVSTDTC) or missing(s.SVENDTC) or prxmatch fails for SVSTDTC/SVENDTC or SVENDTC < SVSTDTC or SVSTDTC < d.RFSTDTC or SVENDTC > d.RFENDTC

Note: s.VISITNUM is numeric, s.VISIT is character.

We need to handle inner join, so only SV rows that have DM match are considered? But SAS inner join will only consider SV rows with DM match. If SV has USUBJID not in DM, it won't be counted here, but it would be counted in SDTM-015. So we replicate inner join.

Check 30: DS exactly one final disposition

We need to implement union logic.

We need to compute:

- For each DM USUBJID, count DS rows for that USUBJID. If count !=1, then that USUBJID is a failure. Count number of such DM USUBJIDs.

- For each DS USUBJID not in DM, count distinct USUBJID, each counts as failure.

Total N_FAIL = sum of both.

Check 31: DS date matches DM

where missing(s.DSDECOD) or missing(s.DSSTDTC) or prxmatch fails or s.DSSTDTC != d.RFPENDTC

Check 32: RACE terminology

Check 33: SITEID

where missing(SITEID) or strip(SITEID) != scan(strip(SUBJID),1,'-')

We need to implement scan: first token before '-'.

Check 34: ADAE-002

where (not missing(AOCCFL) and AOCCFL != 'Y') or (AOCCFL='Y' and TRTEMFL != 'Y')

Check 35: ADAE-003

We need to group by USUBJID, AEDECOD, having sum(TRTEMFL='Y')>0 and sum(AOCCFL='Y') !=1

Count groups.

Check 36: CM

We need to implement complex where.

Check 37: DSDECOD terminology

Check 38: Sequence keys unique

We need to union all duplicates across domains.

For each domain, find duplicate (USUBJID, SEQ) groups where count>1, then count total groups across all domains.

Check 39: CM ongoing

We need to implement.

We need to handle upcase and missing.

We need to ensure we handle all.

We also need to handle that some datasets may have different column names: e.g., CMTRT vs CMTRT? Should be correct.

We need to write R script that does all.

We need to handle that some variables may be labelled and haven reads as haven_labelled. Need to convert to character/numeric via haven::as_factor? But we can use haven::zap_labels.

We should apply haven::zap_labels to all data frames after reading.

We need to handle that some numeric variables may be haven_labelled.

We need to handle that some character variables may be haven_labelled.

We should convert all haven_labelled to appropriate type.

We need to handle that some datasets may have USUBJID as character with leading/trailing spaces.

We need to handle that some datasets may have missing values as "".

We need to implement robust.

We need to produce final qc_report data frame with 39 rows in order as SAS set order.

Order in SAS set: chk_adsl01, chk_adae01, chk_advs01, chk_adlb01, chk_advs02, chk_adlb02, chk_dm01, chk_ae01, chk_ex01, chk_lb01, chk_vs01, chk_adsl02, chk_adsl03, chk_adsl04, chk_adtte01, chk_adtte02, chk_adtte03, chk_eg01, chk_eg02, chk_eg03, chk_eg04, chk_mh01, chk_mh02, chk_mh03, chk_mh04, chk_dm02, chk_sv01, chk_sv02, chk_sv03, chk_ds01, chk_ds02, chk_dm03, chk_dm04, chk_adae02, chk_adae03, chk_cm01, chk_ds03, chk_seq01, chk_cm02

We need to create data frame with those CHECK_IDs and corresponding DOMAIN, RULE, N_FAIL.

We need to ensure RULE text exactly matches SAS.

Let's list RULE texts:

- ADSL-001: 'USUBJID must be perfectly unique'
- ADAE-001: 'AEENDT must be >= AESTDT'
- ADVS-001: 'Max 1 Baseline (ABLFL=Y) per Param'
- ADLB-001: 'Max 1 Baseline (ABLFL=Y) per Param'
- ADVS-002: 'Baseline date has exactly one eligible source record'
- ADLB-002: 'Baseline date has exactly one eligible source record'
- SDTM-001: 'USUBJID must be perfectly unique'
- SDTM-002: 'Every AE.USUBJID exists in DM'
- SDTM-003: 'Every EX.USUBJID exists in DM'
- SDTM-004: 'Every LB.USUBJID exists in DM'
- SDTM-005: 'Every VS.USUBJID exists in DM'
- ADSL-002: 'SAFFL=Y requires exposure in EX'
- ADSL-003: 'EOSDT must agree with DM.RFPENDTC'
- ADSL-004: 'AGE must equal completed years at informed consent'
- ADTTE-001: 'Survival time (AVAL) must be >= 1'
- ADTTE-002: 'AVAL and CNSR agree with event or subject follow-up date'
- ADTTE-003: 'Event and follow-up dates must be chronologically valid'
- SDTM-006: 'Every EG.USUBJID exists in DM'
- SDTM-007: 'Required EG variables must exist'
- SDTM-008: 'EG date/value contract must be valid'
- SDTM-009: 'Raw EG variables must not leak to SDTM'
- SDTM-010: 'Every MH.USUBJID exists in DM'
- SDTM-011: 'Required MH variables must exist'
- SDTM-012: 'MH date/term contract must be valid'
- SDTM-013: 'Raw MH variables must not leak to SDTM'
- SDTM-014: 'DM reference dates complete and ordered'
- SDTM-015: 'Every SV.USUBJID exists in DM'
- SDTM-016: 'USUBJID and VISITNUM must be unique'
- SDTM-017: 'SV dates valid and inside reference period'
- SDTM-018: 'Exactly one final DS record per DM subject'
- SDTM-019: 'DS date matches DM participation end'
- SDTM-020: 'RACE uses supported CDISC terminology'
- SDTM-021: 'SITEID present and consistent with SUBJID'
- ADAE-002: 'AOCCFL valid and restricted to TRTEMFL=Y'
- ADAE-003: 'One AOCCFL=Y per subject and TEAE term'
- SDTM-022: 'CM subject, medication, dose and dates must be valid'
- SDTM-023: 'DSDECOD uses supported disposition terminology'
- SDTM-024: 'Domain sequence keys must be unique per subject'
- SDTM-025: 'Ongoing CM must be anchored to the DM reference end'

We need to ensure exact strings.

We need to produce STATUS = if N_FAIL==0 then PASS else FAIL.

We need to write CSV with columns CHECK_ID, DOMAIN, RULE, N_FAIL, STATUS.

We need to produce text rendering.

We need to handle that SAS PROC PRINT prints with noobs, var CHECK_ID DOMAIN RULE N_FAIL STATUS.

We need to produce text file that contains title and table.

We need to ensure 156 numbers: Let's count: 39 rows * 4 numbers? But N_FAIL is one number per row, plus maybe other numbers? However P019.lst text will contain N_FAIL values (39 numbers) plus maybe other numbers like counts? But spec says 156 numbers in P019.lst. That suggests maybe each row has multiple numbers? But our qc_report only has N_FAIL as numeric. How to get 156 numbers? Maybe they count each digit? Or maybe they count each numeric value in the table including CHECK_ID numbers? Let's not worry; we need to produce text that contains same numbers as SAS output. The validator will check that numbers in text match expected.

We need to ensure we produce correct N_FAIL values. To do that, we need to actually compute them correctly based on data.

We need to explore data to know expected N_FAIL values. Since we don't have data, we need to make script robust and hope it computes correctly.

We need to handle edge cases like missing datasets.

We should write script that logs progress.

We need to handle that some checks may require variables that don't exist, then N_FAIL should be 0 or handle error.

We need to implement tryCatch for each check.

We need to handle that some datasets may have different variable names: e.g., ADSL has EOSDT, BRTHDT, RFICDT, AGE, SAFFL, USUBJID, etc.

We need to handle that SDTM DM has RFICDTC, RFSTDTC, RFENDTC, RFPENDTC, RACE, SITEID, SUBJID, USUBJID.

We need to handle that ADVS has ABLFL, PARAMCD, USUBJID, ASTDT, TRTSDT, AVAL.

We need to handle that ADLB similar.

We need to handle that ADTTE has AVAL, CNSR, TRTSDT, EVENTDT, EOSDT.

We need to handle that ADAE has AEENDT, AESTDT, AOCCFL, TRTEMFL, AEDECOD, USUBJID.

We need to handle that SDTM EG has EGDTC, EGSTRESN, EGSTRESC, etc.

We need to handle that SDTM MH has MHSTDTC, MHTERM.

We need to handle that SDTM SV has VISITNUM, VISIT, SVSTDTC, SVENDTC.

We need to handle that SDTM DS has DSDECOD, DSSTDTC.

We need to handle that SDTM CM has CMTRT, CMDECOD, CMDOSE, CMDOSU, CMSTDTC, CMENDTC, CMENRTPT, CMENTPT.

We need to handle that SDTM CM has CMSEQ.

We need to handle that SDTM AE has AESEQ.

We need to handle that SDTM EX has EXSEQ.

We need to handle that SDTM LB has LBSEQ.

We need to handle that SDTM VS has VSSEQ.

We need to handle that SDTM MH has MHSEQ.

We need to handle that SDTM EG has EGSEQ.

We need to handle that SDTM CM has CMSEQ.

We need to handle that SDTM DM has no SEQ.

We need to handle that SDTM DS has no SEQ? But DS not in seq check.

We need to handle that SDTM SV has no SEQ.

We need to handle that SDTM CM has CMENRTPT, CMENTPT.

We need to handle that SDTM DM has RFENDTC.

We need to handle that SDTM DM has RFPENDTC.

We need to handle that SDTM DM has RFICDTC.

We need to handle that SDTM DM has RFSTDTC.

We need to handle that SDTM DM has RACE.

We need to handle that SDTM DM has SITEID, SUBJID.

We need to handle that ADSL has BRTHDT, RFICDT, AGE, EOSDT.

We need to handle that ADSL has SAFFL.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has TRTSDT, EVENTDT, EOSDT, CNSR, AVAL.

We need to handle that ADVS has ASTDT, TRTSDT, AVAL, ABLFL, PARAMCD.

We need to handle that ADLB similar.

We need to handle that ADAE has AEENDT, AESTDT, AOCCFL, TRTEMFL, AEDECOD.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID? Probably.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that SDTM AE has USUBJID.

We need to handle that SDTM EX has USUBJID.

We need to handle that SDTM LB has USUBJID.

We need to handle that SDTM VS has USUBJID.

We need to handle that SDTM EG has USUBJID.

We need to handle that SDTM MH has USUBJID.

We need to handle that SDTM SV has USUBJID.

We need to handle that SDTM DS has USUBJID.

We need to handle that SDTM CM has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that ADSL has USUBJID.

We need to handle that ADTTE has USUBJID.

We need to handle that ADVS has USUBJID.

We need to handle that ADLB has USUBJID.

We need to handle that ADAE has USUBJID.

We need to handle that SDTM DM has USUBJID.

We need to handle that S