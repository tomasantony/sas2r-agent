Translate the SAS program below to R.

Program id: P023   file: P023_dm.sas   source: faijalanalytics-jpg/oncology-clinical-sas-project   family: faijal
Declared output types: listing

Macro variables set by the benchmark before the program ran:
  &root. = D:/meta_hackathon/sas2r-agent
  &pid. = P023
  &rawadam. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam
  &rawsdtm. = D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/sdtm
  &rawsplit. = D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam-split
  &rawdir. = D:/meta_hackathon/sas2r-agent/data/raw
  &outtlf. = D:/meta_hackathon/sas2r-agent/output/sas/tlf
  &outlog. = D:/meta_hackathon/sas2r-agent/output/sas/logs
  &outds. = D:/meta_hackathon/sas2r-agent/output/sas/datasets/P023

WORK datasets pre-loaded by the environment from these Excel files (dataset name -> file):
  edc_ae -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ae.xlsx
  edc_death -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_death.xlsx
  edc_dm -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_dm.xlsx
  edc_ds -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ds.xlsx
  edc_ex -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_ex.xlsx
  edc_lb -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_lb.xlsx
  edc_rs -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_rs.xlsx
  edc_tr -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tr.xlsx
  edc_tu -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_tu.xlsx
  edc_vs -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/edc_vs.xlsx
  ivrs_rand -> D:/meta_hackathon/sas2r-agent/data/raw/faijal-oncology/raw_data/ivrs_rand.xlsx

Expected outputs (produced by SAS; the R script must produce the counterparts under OUT_DIR):
  P023.lst (table/listing text, 142 numbers)  -> write a text rendering and CSV of the same table

--- family environment (faijal) that ran before the program ---
/* faijal programs expect edc_<domain> (and ivrs_rand) datasets in WORK: import every raw xlsx */
filename _edcdir "&rawdir.\faijal-oncology\raw_data";
data _edc; length f $200 ds $32; did=dopen('_edcdir'); do i=1 to dnum(did); f=dread(did,i); if lowcase(scan(f,-1,'.'))='xlsx' then do; ds=scan(f,1,'.'); output; end; end; rc=dclose(did); run;
data _null_; set _edc; length cmd $500;
  cmd = 'proc import datafile="' || "&rawdir.\faijal-oncology\raw_data\" || strip(f) || '" out=work.' || strip(ds) || ' dbms=xlsx replace; run;';
  call execute(cmd); run;


--- SAS program (D:/meta_hackathon/sas2r-agent/programs/sas_run/P023_dm.sas) ---
/********************************************************************/
/* STUDY: ONCO-2026                                                 */
/* DOMAIN: DM (Demographics)                                        */
/* PURPOSE: Create SDTM DM Dataset from Raw Source Systems          */
/* AUTHOR: Faijal Qureshi                                           */
/********************************************************************/

/********************************************************************/
/* STEP 1: PRE-PROCESSING SOURCE DATA                               */
/********************************************************************/

/*------------------------------------------------------------------*/
/* PURPOSE: Derive Enrollment Date from IVRS System                 */
/*------------------------------------------------------------------*/

proc sql;

    create table ds_enrol as

    select 
        SUBJID,

        /* Convert character randomization date to SAS date */
        input(RAND_DT, date11.) as enr_dt format=yymmdd10.

    from ivrs_rand;

quit;


/*------------------------------------------------------------------*/
/* PURPOSE: Derive First and Last Dose Dates from Exposure Domain   */
/*------------------------------------------------------------------*/

proc sql;

    create table ex_dates as

    select 
        SUBJID,

        /* First dose date */
        min(input(STDT, date11.)) as f_dose format=yymmdd10.,

        /* Last dose date */
        max(input(ENDT, date11.)) as l_dose format=yymmdd10.

    from edc_ex

    group by SUBJID;

quit;


/*------------------------------------------------------------------*/
/* PURPOSE: Derive Study End Date and Death Flag                    */
/*------------------------------------------------------------------*/

proc sql;

    create table ds_final as

    select 
        SUBJID,

        /* Study completion/discontinuation date */
        input(DS_DT, date11.) as end_dt format=yymmdd10.,

        /* Death Flag */
        case 
            when DS_TERM = 'DEATH' then 'Y'
            else 'N'
        end as dth_fl

    from edc_ds;

quit;


/********************************************************************/
/* STEP 2: SORT DATASETS BEFORE MERGE                               */
/********************************************************************/

/* SAS requires sorting before BY-group merge */

proc sort data=edc_dm;
    by SUBJID;
run;

proc sort data=ivrs_rand;
    by SUBJID;
run;

proc sort data=ds_enrol;
    by SUBJID;
run;

proc sort data=ex_dates;
    by SUBJID;
run;

proc sort data=ds_final;
    by SUBJID;
run;

proc sort data=edc_death;
    by SUBJID;
run;


/********************************************************************/
/* STEP 3: CREATE SDTM DM DOMAIN                                    */
/********************************************************************/

data sdtm_dm;

    /***************************************************************/
    /* DEFINE VARIABLE LENGTHS                                     */
    /***************************************************************/

    length
        STUDYID   $10
        DOMAIN    $2
        USUBJID   $25
        SUBJID    $10
        SITEID    $10

        RFSTDTC   $20
        RFENDTC   $20
        RFXSTDTC  $20
        RFXENDTC  $20
        RFICDTC   $20
        RFPENDTC  $20

        BRTHDTC   $20
        AGE       8
        AGEU      $10
        SEX       $1
        RACE      $40
        ETHNIC    $40

        ARMCD     $10
        ARM       $40
        ACTARMCD  $10
        ACTARM    $40

        COUNTRY   $3

        DTHFL     $1
        DTHDTC    $20
    ;

    /***************************************************************/
    /* MERGE SOURCE DATASETS                                       */
    /***************************************************************/

    merge

        edc_dm(in=a)

        ivrs_rand(
            keep=SUBJID TRT_CODE
        )

        ds_enrol

        ex_dates

        ds_final

        edc_death(
            rename=(DTH_DT=death_date_raw)
        )

    ;

    by SUBJID;

    /* Keep only subjects present in DM */
    if a;


    /***************************************************************/
    /* IDENTIFIER VARIABLES                                        */
    /***************************************************************/

    STUDYID = "ONCO-2026";

    DOMAIN = "DM";

    /* Create Unique Subject Identifier */
    USUBJID = catx("-", STUDYID, SUBJID);

    SITEID = SITE;


    /***************************************************************/
    /* DEMOGRAPHIC VARIABLES                                       */
    /***************************************************************/

    /* Convert Birth Date */
    BRTHDTC = put(input(BRTHDT, date11.), yymmdd10.);

    /* Convert Informed Consent Date */
    RFICDTC = put(input(RFICDT, date11.), yymmdd10.);

    /* Convert MALE/FEMALE to M/F */
    SEX = substr(GENDER, 1, 1);

    /* Calculate Age */
    AGE = intck(
                'year',
                input(BRTHDTC, yymmdd10.),
                input(RFICDTC, yymmdd10.),
                'c'
          );

    AGEU = "YEARS";

    COUNTRY = "IND";


    /***************************************************************/
    /* TIMING VARIABLES                                            */
    /***************************************************************/

    /* Enrollment Date */
    RFSTDTC = put(enr_dt, yymmdd10.);

    /* First Exposure Date */
    RFXSTDTC = put(f_dose, yymmdd10.);

    /* Last Exposure Date */
    RFXENDTC = put(l_dose, yymmdd10.);

    /* Study Completion Date */
    RFENDTC = put(end_dt, yymmdd10.);

    /* End of Participation Date */
    RFPENDTC = RFENDTC;


    /***************************************************************/
    /* TREATMENT VARIABLES                                         */
    /***************************************************************/

    ARMCD = TRT_CODE;

    if ARMCD = "DRUGA" then
        ARM = "Experimental Drug A";

    else if ARMCD = "PBO" then
        ARM = "Placebo";


    /* Assume Actual Treatment = Planned Treatment */

    ACTARMCD = ARMCD;

    ACTARM = ARM;


    /***************************************************************/
    /* DEATH VARIABLES                                             */
    /***************************************************************/

    DTHFL = dth_fl;

    if death_date_raw ne "" then
        DTHDTC = put(
                        input(death_date_raw, date11.),
                        yymmdd10.
                     );


    /***************************************************************/
    /* KEEP FINAL SDTM VARIABLES                                   */
    /***************************************************************/

    keep

        STUDYID
        DOMAIN
        USUBJID
        SUBJID
        SITEID

        RFSTDTC
        RFENDTC
        RFXSTDTC
        RFXENDTC
        RFICDTC
        RFPENDTC

        BRTHDTC
        AGE
        AGEU
        SEX
        RACE
        ETHNIC

        ARMCD
        ARM
        ACTARMCD
        ACTARM

        COUNTRY

        DTHFL
        DTHDTC
    ;

run;


/********************************************************************/
/* STEP 4: VIEW FINAL SDTM DM DATASET                               */
/********************************************************************/

proc print data=sdtm_dm;
run;

--- end of SAS program ---

Write the complete R script now.