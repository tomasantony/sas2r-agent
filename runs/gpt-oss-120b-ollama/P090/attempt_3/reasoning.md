The error persists: unexpected symbol in mutate(_trt_end. Likely due to line continuation issue: we used mutate( _trt_end = ifelse(... ) ) but preceding line ended with a comma? Let's check code: after deriving ONTRTFL we have:

mutate(
    ONTRTFL = case_when(
      !is.na(ASTDT) & is.na(AENDT) & ASTDT <= _trt_end ~ "Y",
      !is.na(ASTDT) & !is.na(AENDT) & ASTDT <= _trt_end & AENDT >= TRTSDT ~ "Y",
      TRUE ~ "N"
    )
  ) %>%
  # Concomitant flag

But earlier we defined _trt_end in a mutate before that. However the syntax error occurs earlier: after previous mutate we have:

mutate(
    _trt_end = ifelse(is.na(TRTEDT), Sys.Date(), TRTEDT)
  ) %>%
  mutate(
    ONTRTFL = case_when(
      !is.na(ASTDT) & is.na(AENDT) & ASTDT <= _trt_end ~ "Y",
...

But the error shows unexpected symbol at "_trt_end". Possibly because we used a line break after mutate( without closing parentheses? Let's re-express more cleanly: combine into one mutate call to avoid line break issues.

Also earlier we used relocate with .before = 1 but that may cause error? Not likely.

The main syntax error is likely due to stray backticks or missing comma before mutate. Let's reconstruct the pipeline.

We'll build stepwise:

adcm <- cm %>%
 left_join(adsl, ...) %>%
 mutate(ASTDT = ymd(CMSTDTC), AENDT = ymd(CMENDTC)) %>%
 mutate(ASTDY = ..., AENDY = ...) %>%
 mutate(CMDURN = ...) %>%
 mutate(PREFL = ...) %>%
 mutate(_trt_end = ifelse(is.na(TRTEDT), Sys.Date(), TRTEDT)) %>%
 mutate(ONTRTFL = case_when(...)) %>%
 mutate(CONCOMFL = ifelse(...)) %>%
 mutate(ADTEFFL = ..., ADTEFN = ...) %>%
 mutate(CMDECOD_UP = toupper(CMDECOD), CMCLAS_UP = toupper(CMCLAS)) %>%
 mutate(ATCCD1 = ..., ATC1 = ...) %>%
 select(-_trt_end) %>%
 relocate(...)

Make sure each mutate closed.

Also need to ensure ymd works for dates like "2023-06-15". That's fine.

Now also need to ensure that the listing file is written. The earlier error prevented script from running at all, so no outputs.

Let's produce corrected script.

