Need to fix TRTAN not found. Likely ADSL/ADLB don't have TRTAN column, need to derive. Check synthetic data. ADSL has TRTA, maybe TRTAN missing. Need to handle missing TRTAN by creating mapping.

We need to inspect typical CDISC pilot: ADSL has TRT01P etc. But synthetic may have TRTA and TRTAN. Error says TRTAN not found in shift_full arrange. That means shift_full doesn't have TRTAN. That comes from shift_cnt distinct. shift_cnt derived from shift which comes from bl_grade and worst_grade2. bl_grade has TRTAN from lb. lb has TRTAN from adlb. If adlb lacks TRTAN, then lb lacks it.

We need to robustly handle missing TRTAN: create TRTAN from TRTA mapping.

We should add logic: if TRTAN not in adsl/adlb, create it.

Also need to ensure output text is written even if error earlier. Need to make script robust.

We need to create TRTAN mapping: Placebo=1, Low Dose=2, High Dose=3 etc. Use factor ordering.

Also need to handle PARAM etc.

We should add code to derive TRTAN if missing.

Also need to ensure OUT_DIR writing happens even if earlier fails? But fix root cause.

Let's implement helper to ensure TRTAN exists.

Also need to handle case where adlb has TRTA but not TRTAN.

We should add after reading adsl/adlb:

if (!"TRTAN" %in% names(adsl)) { adsl$TRTAN <- case_when(... ) }

Similarly for adlb.

We need to infer mapping: use unique TRTA values sorted? SAS order by TRTA? In m_counts, order by TRTA (character). But report orders by TRTAN. So TRTAN numeric order should correspond to alphabetical TRTA? Let's check typical: Placebo, Xanomeline High Dose, Xanomeline Low Dose -> alphabetical would be Placebo, High, Low. But TRTAN 1=Placebo, 2=Low, 3=High per u_setup. So not alphabetical. So we need to map based on known labels.

We can define mapping: if TRTA contains "Placebo" ->1, "Low" ->2, "High" ->3. Else assign sequential by factor.

Implement generic: create mapping from adsl distinct TRTA/TRTAN if TRTAN exists, else create.

Also need to handle PARAMCD etc.

Also need to ensure shift_full arrange doesn't fail if TRTAN missing: we can add check.

Simplify: after reading, ensure both have TRTAN.

Also need to ensure report generation doesn't fail due to missing columns.

We also need to ensure output files are written even if data empty.

We should wrap main logic in tryCatch and still write dummy output? But better fix.

Let's craft final script with robust TRTAN derivation.

Also need to handle BTOXGR/ATOXGR maybe named differently? But assume exists.

Add fallback: if TRTAN missing, create.

Implementation steps:

- function ensure_trtan(df):
 if "TRTAN" %in% names(df) return df
 else {
   # create mapping based on TRTA
   trta_vals <- unique(df$TRTA)
   # define known mapping
   map <- tibble(TRTA=trta_vals)
   map$TRTAN <- sapply(map$TRTA, function(x) {
     xl <- tolower(x)
     if (grepl("placebo", xl)) 1
     else if (grepl("low", xl)) 2
     else if (grepl("high", xl)) 3
     else if (grepl("total", xl)) 99
     else NA
   })
   # for any NA, assign sequential after max
   if (any(is.na(map$TRTAN))) {
     maxv <- max(map$TRTAN, na.rm=TRUE)
     if (!is.finite(maxv)) maxv <- 0
     for (i in which(is.na(map$TRTAN))) {
       maxv <- maxv+1
       map$TRTAN[i] <- maxv
     }
   }
   df <- left_join(df, map, by="TRTA")
   return(df)
 }

Apply to adsl and adlb.

Also need to handle case where adlb has TRTAN but adsl not, etc.

Also need to handle shift_full arrange: if TRTAN not in shift_full, skip.

But after ensuring, it will be there.

Also need to handle bign_df creation: if TRTAN missing, we already ensured.

Also need to handle report creation: ensure wg columns.

We should also add defensive: if shift_cnt empty, create empty report but still write text.

We need to ensure script writes files even if error: wrap in tryCatch and on error write minimal text with numbers? But better just fix.

We should produce complete script.

Also need to ensure library loading doesn't fail.

We should output t_lb_shift.txt and .rtf and .csv.

Let's craft final R script.