Need to translate SAS program P051_medication_listing.sas to R.

We have inputs: ADCM.xpt in &rawadam = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
Also libname inlib -> derived adam but program uses source.ADCM from rawadam.

We need to produce Target36.rtf -> text rendering and CSV.

We need to understand expected outputs: Target36.rtf (table/listing text, 46030 numbers) -> write text rendering and CSV of same table.

We need to replicate PROC REPORT with BY TRTA.

Steps:

- Read ADCM.xpt via haven::read_xpt
- Filter SAFFL='Y'
- Sort by TRTA STUDYID USUBJID CMSTDTC CMENDTC CMDECOD
- Create ADCM2:
  length StartStop $25 Doseunit $50;
  Period ='Study Period';
  if CMSTDTC='' then CMSTDTC='NK';
  if CMENDTC='' then CMENDTC='NK';
  StartStop=compbl(CMSTDTC||'/'||CMENDTC);
  If cmunit ne . then DoseUnit = compbl(CMDOSE || '('|| cmunit || ')');
    else DoseUnit = compbl(CMDOSE);
  if AENRF='Ongoing' then Ongoing='Y'; else Ongoing='N';

Note: SAS code uses cmunit but comment says CMDOSU. Need to check actual variable names in ADCM. Let's inspect typical CDISC pilot ADCM. Variables: STUDYID, USUBJID, TRTA, TRTAN, SAFFL, CMCLAS, CMDECOD, CMDOSE, CMDOSU, CMINDC, CMSTDTC, CMENDTC, ADURN, AENRF, etc. But code uses cmunit. Might be CMDOSU or CMUNIT? Need to handle both. In SAS, if cmunit ne . then ... else ... So cmunit is numeric? But CMDOSU is character. Could be that ADCM has CMDOSE numeric and CMDOSU char, but code mistakenly uses cmunit. Let's check actual dataset.

We need to explore file system.