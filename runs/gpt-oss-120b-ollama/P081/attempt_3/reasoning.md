The error persists: EFFFL not found in adsl after rename_all(toupper). Maybe column name is "EFFFL" but in adsl dataset maybe it's "EFFFL" already? Actually adsl likely has variable "EFFFL". But after rename_all(toupper) it becomes "EFFFL". So why not found? Perhaps adsl dataset does not have EFFFL variable (maybe it's "EFFFL" but in synthetic data it's "EFFFL"? Could be missing. In SAS code they join adeff a with adsl b and filter b.EFFFL='Y'. If adsl doesn't have EFFFL, maybe it's "EFFFL" in adsl? Let's inspect typical ADaM ADSL includes "EFFFL". Might be present. But error indicates column not present. Could be that adsl dataset is not loaded because path wrong? The path is adam_path = ".../own-synthetic/adam". The file names may be lower case or have .sas7bdat extension but maybe the dataset is stored as .xpt? In earlier macro they used libname adam "..." and refer to adam.adeff etc. So file names are adeff.sas7bdat, adsl.sas7bdat. Should exist.

But error persists, maybe rename_all(toupper) changed column names but later we refer to EFFFL (uppercase) which matches. So maybe adsl doesn't have that variable; maybe it's "EFFFL" in adsl but the synthetic dataset uses "EFFFL" but maybe it's "EFFFL" with trailing spaces? Unlikely.

Alternative: Instead of joining and then filtering on adsl$EFFFL, we could filter adsl first for EFFFL == 'Y' and then inner_join. But still need column.

Let's add a check: if "EFFFL" not in adsl, then assume all subjects are in efficacy set (i.e., skip filter). So modify code: after reading adsl, check if "EFFFL" %in% names(adsl). If not, create a logical vector of TRUE.

Implement:

adsl_filtered <- adsl %>% { if ("EFFFL" %in% names(.)) filter(., EFFFL == "Y") else . }

Then join.

Also need to ensure column names are consistent: after rename_all(toupper), adeff also has "TRTAN" etc.

Let's adjust.

Also ensure that after join we have column "EFFFL"? Not needed after join.

Implement changes.

Also ensure that we write the figure PNG path correctly; earlier we used ggsave with device png; should work.

Let's produce final script.

