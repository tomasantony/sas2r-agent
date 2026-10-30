We need to produce R script that reproduces SAS program. Steps:

- Read sdtm.lb dataset from path: &root.\data\derived\wxzhai\sdtm\lb (likely .sas7bdat). Use haven::read_sas. Path: file.path(root, "data/derived/wxzhai/sdtm", "lb.sas7bdat")? Actually libref sdtm points to that directory, dataset lb. So file path: D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/sdtm/lb.sas7bdat . Use forward slashes.

- Read adam.adsl dataset: path D:/meta_hackathon/sas2r-agent/data/derived/wxzhai/adam/adsl.sas7bdat.

- Sort lb by USUBJID, PARAM, LBDTC using SAS ordering: character case-sensitive, missing first. In R, we can use arrange with dplyr, but need to mimic SAS sort: missing values first, then character order (uppercase before lowercase). dplyr's arrange uses default order which puts NA last. So we need to handle. Could use order with na.last = FALSE. For character, default order is based on locale; uppercase before lowercase? Usually "A" < "a". So okay. We'll use base::order.

- Similarly sort adsl by USUBJID.

- Merge (by USUBJID) with left join? SAS merge with both datasets sorted and by USUBJID, and condition if a and b (both present). So inner join on USUBJID, keeping all variables from both, but only those rows where both present. In SAS, if multiple rows per USUBJID in lb, they will repeat adsl variables (one per USUBJID). So we need to replicate that: join lb_qc with adsl_qc (selected columns) using left join of lb to adsl (since adsl has one row per USUBJID). Then filter rows where adsl exists (i.e., not NA). Equivalent to inner join.

- Keep only USUBJID, RFSTDTC, TRT01P from adsl.

- Compute lbdt = input(LBDTC, yymmdd10.) ; LBDTC is character date string like "2020-01-01"? SAS yymmdd10. expects "YYYY-MM-DD". So we can parse with lubridate::ymd.

- rfstdt similarly.

- retain BASE variable across rows within each PARAM group? In SAS code: retain BASE; if first.PARAM then BASE = .; So BASE is reset at start of each new PARAM value (within each USUBJID? Actually BY USUBJID only, not by PARAM. Wait they have BY USUBJID only in merge. So first.PARAM refers to first observation for each PARAM within each BY group? In SAS, after merge BY USUBJID, the BY statement only defines grouping for BY USUBJID. However first.PARAM is a variable that indicates first occurrence of PARAM within the BY group? In SAS, FIRST.variable is set when variable appears in BY statement. Since only USUBJID is in BY, FIRST.PARAM is not defined. Actually SAS automatically creates FIRST.variable for each variable in BY statement. Since PARAM is not in BY, FIRST.PARAM is not defined, will be missing. But SAS still allows referencing FIRST.PARAM? I think SAS will treat it as missing (0). But they used first.PARAM; maybe they intended BY USUBJID PARAM? Wait earlier they sorted lb by USUBJID PARAM LBDTC. But merge BY USUBJID only. So first.PARAM will be missing for all rows, causing BASE never reset? That seems odd. However typical QC code would reset BASE at first observation of each PARAM within each subject. They would need BY USUBJID PARAM. But they didn't. Might be oversight but we must replicate SAS behavior. In SAS, referencing FIRST.PARAM when PARAM not in BY will cause error: "Variable PARAM not in BY statement". Actually SAS will give error: "Variable PARAM not in BY statement". So program would fail. But maybe SAS automatically sets FIRST. for any variable in BY? No. So maybe they omitted BY PARAM in merge but SAS still allows? I think SAS requires variable to be in BY list to use FIRST./LAST. So program would error. But we assume program runs, maybe they used BY USUBJID PARAM LBDTC earlier? No, merge statement: merge lb_qc (in=a) adsl_qc (in=b keep=USUBJID RFSTDTC TRT01P); by USUBJID; So only USUBJID. So referencing FIRST.PARAM would cause error. However maybe they rely on automatic variable FIRST.PARAM from previous sort? Not.

Given this, maybe they intended to use BY USUBJID PARAM. We'll replicate likely intended behavior: reset BASE at first observation of each PARAM within each USUBJID. So we will group_by USUBJID, PARAM and arrange by LBDTC, then compute BASE.

Implementation: after merging, arrange by USUBJID, PARAM, LBDTC (already sorted). Then within each group, compute BASE: initialize NA, then for each row, if lbdt < rfstdt and not missing(AVAL) then set BASE = AVAL (but only if earlier condition). Actually SAS code: if first.PARAM then BASE = .; then if lbdt < rfstdt and not missing(AVAL) then BASE = AVAL; So for each row, if lbdt < rfstdt and not missing AVAL, then set BASE to AVAL (overwrites). So BASE becomes most recent pre-baseline AVAL? Actually they set BASE to AVAL when lbdt < rfstdt, not missing. So after loop, BASE will be last pre-baseline AVAL. Then later they compute ABLFL = "Y" if lbdt < rfstdt and AVAL = BASE else "" . So baseline flag for rows where AVAL equals baseline value (i.e., the last pre-baseline measurement). CHG = AVAL - BASE if BASE not missing.

Thus we need to implement this logic.

Implementation steps:

- After merge, compute lbdt, rfstdt.

- Arrange by USUBJID, PARAM, LBDTC.

- Use dplyr::group_by(USUBJID, PARAM) and mutate:

   BASE = NA_real_
   Then we need to compute cumulative baseline: for each row, if lbdt < rfstdt & !is.na(AVAL) then assign BASE = AVAL else keep previous BASE. This is like fill forward but only when condition true. Use cummax? Not exactly; we need last value where condition true. Use accumulate.

Simpler: within group, compute a vector baseline = NA; then loop.

We'll use dplyr::mutate with custom function using purrr::accumulate.

Pseudo:

df <- df %>%
  group_by(USUBJID, PARAM) %>%
  arrange(LBDTC, .by_group = TRUE) %>%
  mutate(
    BASE = {
      vals <- AVAL
      lbdt_vec <- lbdt
      cond <- lbdt_vec < rfstdt & !is.na(vals)
      base_vec <- NA_real_
      cur <- NA_real_
      for(i in seq_along(vals)){
        if(cond[i]) cur <- vals[i]
        base_vec[i] <- cur
      }
      base_vec
    },
    ABLFL = ifelse(lbdt < rfstdt & AVAL == BASE, "Y", ""),
    CHG = ifelse(!is.na(BASE), AVAL - BASE, NA_real_)
  ) %>% ungroup()

- Drop lbdt, rfstdt.

- Output dataset adlb_qc.csv.

Need to write to OUT_DIR environment variable.

Also need to ensure variable names same as SAS: keep all original variables from lb plus RFSTDTC, TRT01P, plus new variables ABLFL, CHG, BASE? In SAS they retain BASE variable (kept). They also drop lbdt rfstdt. So output includes BASE, ABLFL, CHG.

Thus we will output all columns.

Write CSV with na = "".

Now produce R script.

Edge Cases: date parsing: LBDTC may be missing; use ymd with na.

Now produce script.

