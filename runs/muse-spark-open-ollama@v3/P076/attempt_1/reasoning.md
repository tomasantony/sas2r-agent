Need to translate SAS program P076_l_ae.sas to R.

We need to read inputs from absolute paths given. Librefs: adam -> D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam, sdtm -> similar. Need to list files.

We need to produce outputs under OUT_DIR: l_ae.rtf -> write text rendering and CSV of same table. Expected outputs: l_ae.rtf (table/listing text, 2771 numbers) -> write text rendering and CSV.

We need to reproduce SAS logic:

Step1: proc sql create table ae_list as select a.USUBJID, b.TRTA, b.AGE, b.SEX, b.RACE, a.AEBODSYS as SOC, a.AEDECOD as PT, a.AETERM as VERBATIM, a.AESTDTC as START_DATE, a.AEENDTC as END_DATE, a.AESEV as SEVERITY, a.AEREL as RELATIONSHIP, a.AESER as SERIOUS, a.AEACN as ACTION_TAKEN, a.AEOUT as OUTCOME, a.AETOXGR as CTCAE_GRADE, a.TRTEMFL from adam.adae a inner join adam.adsl b on a.USUBJID = b.USUBJID where b.SAFFL='Y' and a.TRTEMFL='Y' order by b.TRTA, a.USUBJID, a.AESTDTC, a.AEBODSYS, a.AEDECOD

Step2: data ae_report; set ae_list; length severity_d $10 rel_d $15 serious_d $3 action_d $30 outcome_d $30;
 severity_d = propcase(SEVERITY);
 rel_d = propcase(RELATIONSHIP);
 serious_d = ifc(SERIOUS='Y','Yes','No');
 select(upcase(ACTION_TAKEN));
   when('DRUG WITHDRAWN') action_d='Drug Withdrawn';
   when('DOSE REDUCED') action_d='Dose Reduced';
   when('DRUG INTERRUPTED') action_d='Drug Interrupted';
   when('DOSE NOT CHANGED') action_d='Dose Not Changed';
   when('NOT APPLICABLE') action_d='Not Applicable';
   otherwise action_d=propcase(ACTION_TAKEN);
 end;
 select(upcase(OUTCOME));
   when('RECOVERED/RESOLVED') outcome_d='Resolved';
   when('RECOVERING/RESOLVING') outcome_d='Resolving';
   when('NOT RECOVERED/NOT RESOLVED') outcome_d='Not Resolved';
   when('RECOVERED/RESOLVED WITH SEQUELAE') outcome_d='Resolved w/ Sequelae';
   when('FATAL') outcome_d='Fatal';
   otherwise outcome_d=propcase(OUTCOME);
 end;

Step3: proc report with columns TRTA USUBJID SOC PT VERBATIM START_DATE END_DATE CTCAE_GRADE severity_d serious_d rel_d action_d outcome_d; define TRTA / order; USUBJID / order; break after TRTA / page;

Titles: via set_titles macro output=L_AE, population=Safety Analysis Set
 title1 j=l "&sponsor." j=r "Page ^{pageof}";
 title2 j=l "Protocol: &protocol.";
 title3 j=c " ";
 title4 j=c "Listing 16.2.7.1";
 title5 j=c "Listing of Adverse Events";
 title6 j=c "Safety Analysis Set";
 footnotes:
 footnote1 j=l "Data cutoff: &cutdate.";
 footnote2 j=l "Source: ADaM datasets";
 footnote3 j=l "Program: &sysin. | Run: %sysfunc(date(), date9.) %sysfunc(time(), time5.)";

But sponsor, protocol, cutdate are placeholders: [Sponsor Name], [Protocol Number], [DDMONYYYY]. So we can use those literal strings.

We need to produce text rendering and CSV.

We need to explore data files to understand structure.

We need to write R script that reads adam.adae and adam.adsl from D:/meta_hackathon/sas2r-agent/data/raw/own-synthetic/adam

List files there.