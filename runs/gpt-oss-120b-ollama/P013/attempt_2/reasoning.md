We need to fix syntax error. In earlier script, we used mutate with case_when and braces causing syntax error. In R, case_when expects vectors, not block with braces. Need to write proper code.

Specifically:

_mutate(
      _egdt = case_when(
        is.character(DATE) ~ {
          # parse character dates; anydtdte. equivalent -> lubridate::parse_date_time
          parsed <- parse_date_time(DATE, orders = "any", exact = FALSE)
          as.Date(parsed)
        },
        !is.na(DATE) ~ {
          # numeric SAS date (days since 1960‑01‑01)
          as.Date(as.numeric(DATE), origin = "1960-01-01")
        },
        TRUE ~ as.Date(NA)                           # missing
      )
    ) 

But case_when cannot have braces; need to compute parsed outside or use if_else with vectorized functions. Simpler: use ifelse with vectorized.

We can do:

raw_eg <- raw_eg %>%
  mutate(
    _egdt = ifelse(
      is.character(DATE),
      as.Date(parse_date_time(DATE, orders = "any", exact = FALSE)),
      ifelse(!is.na(DATE),
        as.Date(as.numeric(DATE), origin = "1960-01-01"),
        as.Date(NA)
      )
    )
  ) %>%
  mutate(EGDTC = if_else(!is.na(_egdt), format(_egdt, "%Y-%m-%d"), NA_character_))

But ifelse will coerce to character; better use dplyr::case_when with vectorized expressions without braces:

case_when(
  is.character(DATE) ~ as.Date(parse_date_time(DATE, orders = "any", exact = FALSE)),
  !is.na(DATE) ~ as.Date(as.numeric(DATE), origin = "1960-01-01"),
  TRUE ~ as.Date(NA)
)

That works.

Also earlier we used strip function defined; need to ensure strip works on vectors.

strip <- function(x) ifelse(is.character(x), trimws(x), x)

But we used strip inside mutate for many columns; need vectorized.

Also we used EGORRES = strip(as.character(RESULT_NUM)) but we wrote stray parentheses.

Let's rewrite script cleanly.

Steps:

1. read_csv raw_ecg_path.

2. Define strip function.

3. Convert RESULT to numeric: RESULT_NUM.

4. Date handling: create _egdt.

5. Create EGDTC.

6. Build eg_step1 with transmute.

Need to ensure column names from raw file: we need to know actual column names: SUBJECT, VISIT_NAM, VISIT_NUM, TEST_NAME, RESULT, UNIT, DATE. Use same case as CSV.

Assume they exist.

Implement transmute with proper vectorized strip.

EGORRES = strip(as.character(RESULT_NUM))

EGSTRESC = strip(as.character(RESULT_NUM))

EGORRESU = strip(UNIT) etc.

Also need to keep VISITNUM as numeric maybe.

Sorting: SAS sorts missing dates first, then by date ascending, then EGTESTCD. In SAS PROC SORT default ascending, missing values first. In R arrange, missing values are placed last by default. To mimic missing first, we can use `is.na(EGDTC)` descending? Actually we want missing first, then non-missing ascending. So we can arrange(USUBJID, desc(is.na(EGDTC)), EGDTC, EGTESTCD). That works because desc(TRUE) = FALSE? Let's test: is.na returns TRUE for missing; desc(TRUE) = FALSE, so missing rows get FALSE, non-missing get TRUE, so order: FALSE then TRUE => missing first. Good.

But need to ensure EGDTC is character; missing is NA_character_. arrange will treat character NA as NA; using is.na works.

Add EGSEQ per subject using row_number.

Write CSV.

Also need to ensure output directory exists; create if not.

Add dir.create(out_dir, recursive=TRUE, showWarnings=FALSE).

Now produce final script.

