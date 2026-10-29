We need to fix EFFFL missing values (NaN) where should be "N". In our script, after left_join with eff_tbl, EFFFL column may be NA for subjects not in eff_tbl. Then mutate sets EFFFL = if_else(SAFFL == "Y" & EFFFL == "Y", "Y", "N") . However if EFFFL is NA, the condition SAFFL == "Y" & EFFFL == "Y" yields NA (since NA & something = NA). if_else expects condition vector logical, but NA leads to NA, then returns "N"? Actually if_else with NA condition returns NA (since condition is NA). So we get NA (converted to NaN in CSV). Need to treat missing as FALSE. Use replace_na(EFFFL, "N") before condition, or modify condition: if_else(SAFFL == "Y" & (!is.na(EFFFL) & EFFFL == "Y"), "Y", "N").

Also need to parse datetime strings correctly. The warning indicates all formats failed for ymd_hms. The SAS input uses e8601dt19. which expects format like "2020-01-02T13:45:00". ymd_hms can parse "2020-01-02 13:45:00" but not with "T". Use ymd_hms with "T" allowed: lubridate's ymd_hms can parse "2020-01-02T13:45:00". Actually it should parse. But maybe the strings have timezone offset or missing seconds. Safer to use parse_date_time with orders = "Ymd HMS". But earlier we had error due to complex regex. Simpler: use as.POSIXct(RFXSTDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") and fallback to NA if fails. Use lubridate::ymd_hms with truncated = 3? Actually ymd_hms can parse with "T". Let's test conceptually: ymd_hms("2020-01-02T13:45:00") works. So maybe some strings have only date part (no time). In SAS they used input(..., ?? e8601dt19.) which returns missing if cannot parse. So we can attempt ymd_hms, if fails returns NA. Use parse_date_time2? Simpler: use lubridate::ymd_hms with quiet = TRUE (returns NA on failure). So:

TRTSDTM = if_else(!is.na(RFXSTDTC), ymd_hms(RFXSTDTC, tz = "UTC"), as.POSIXct(NA))

But ymd_hms returns POSIXct, but if_else expects same type; okay.

Similarly for TRTEDTM.

Now ensure EFFFL correct.

Also ensure that after left_join, EFFFL column may be NA; we set to "N". Use mutate(EFFFL = if_else(SAFFL == "Y" & EFFFL == "Y", "Y", "N")) after replace_na(EFFFL, "N").

Implement:

adsl_eff <- adsl_trt01 %>%
  left_join(eff_tbl, by = "USUBJID") %>%
  mutate(EFFFL = replace_na(EFFFL, "N")) %>%
  mutate(EFFFL = if_else(SAFFL == "Y" & EFFFL == "Y", "Y", "N"))

Now adjust datetime parsing.

Also ensure we keep original character columns for later steps (they are needed for output). In final select we include RFSTDTC etc. Those are still present.

Now produce final script.

