Need to translate SAS program P029_tu.sas to R.

We have inputs: edc_tu, sdtm_dm? Wait family environment imports edc_* and ivrs_rand. But program uses sdtm_dm. Where does sdtm_dm come from? Possibly not preloaded? Need to check rawadam, rawsdtm paths. The program references sdtm_dm. In faijal family, maybe sdtm_dm is derived earlier? But we have to derive it from raw inputs using standard CDISC definition if not among listed inputs. The listed WORK datasets pre-loaded are edc_* and ivrs_rand. No sdtm_dm. So we need to derive sdtm_dm from raw inputs.

We need to understand sdtm_dm structure: It has USUBJID, RFSTDTC. RFSTDTC is reference start date (randomization or first exposure). We have edc_dm, edc_ex, ivrs_rand.

We need to inspect files to understand.

We need to produce R script that reads inputs from absolute paths given in context (forward slashes). SAS transport files (.xpt) and SAS datasets (.sas7bdat) are read with haven::read_xpt / haven::read_sas; Excel with readxl::read_excel; CSV with readr or base read.csv.

We need to write outputs into OUT_DIR: dataset tu -> OUT_DIR/tu.csv, and P029.lst -> text rendering and CSV.

We need to explore data.

We should look at directory structure.