Need to fix error: fmt_age_best not vectorized. Also need to ensure outputs match SAS reference. Need to produce l_death.rtf text rendering with 46 numbers.

Previous script had bug in mutate DEMOG = paste(fmt_age_best(AGE)...). fmt_age_best expects scalar. Need vectorized version.

Also need to ensure OUT_DIR handling, reading data, etc.

We need to reproduce SAS logic exactly.

We should fix fmt_age_best to be vectorized.

Also DEMOG logic: catx('/', strip(put(AGE,best.)), strip(SEX), strip(substr(RACE,1,1)))

So need to handle NA.

Also days_display logic.

Also need to ensure text file contains numbers: 16.2, 7.3 etc. Those are from titles? Listing 16.2.7.3 contains numbers 16.2 and 7.3. So need to include titles in text file.

Previous script wrote l_death.txt and l_death.rtf but error prevented creation. Need to ensure files created.

Also need to ensure OUT_DIR/<any>.txt contains numbers. The comparison expects text l_death.rtf: MISSING but they look for any .txt or .csv rendering. So we need to write l_death.txt and also ensure l_death.rtf exists? The expected output is l_death.rtf -> write text rendering and CSV. So we should write both l_death.txt and l_death.rtf? The rules say each table/listing additionally as plain-text rendering (file.path(out, "<name>.txt")) . So we need l_death.txt.

Also need to ensure numbers: examples not found: '-1066', '59', etc. Those are likely DAYS_LASTDOSE_DEATH values? Let's check synthetic data. Need to ensure days calculation correct.

Also need to handle TRTA ordering.

Fix script.

We should make fmt_age_best vectorized:

fmt_age_best <- function(age) {
  sapply(age, function(a) {
    if (is.na(a)) return("")
    s <- format(a, scientific=FALSE, trim=TRUE, nsmall=0, digits=12)
    s <- sub("\\.0+$","", trimws(s))
    trimws(s)
  })
}

Similarly fmt_date9 vectorized.

Also DEMOG: use vectorized.

Also need to handle sas_to_date vectorized.

Simplify.

Also need to ensure death_final creation doesn't error.

We should rewrite script cleanly, fixing mutate.

Also need to ensure we write CSV correctly.

We should also ensure we handle haven labelled.

We need to produce complete standalone R script.

We should test logic mentally.

We should also ensure we handle case where adam_path not found fallback.

We should keep same structure but fix bug.

Also need to ensure we don't use if(is.na(age)) inside vectorized.

We should replace DEMOG mutate with:

DEMOG = paste(... ) vectorized via mapply or sapply.

Simpler: use rowwise or sapply.

We had second line that overwrote DEMOG with sapply, but first line caused error before second. So remove first DEMOG line.

We should do:

DEMOG = sapply(seq_len(n()), function(i) { ... })

But inside mutate, n() not available outside? Use seq_along(AGE).

Better to do mutate then separate.

We should do:

death_final <- death_report %>%
  mutate(
    DEMOG = sapply(seq_along(AGE), function(i) {
      parts <- c(fmt_age_best(AGE[i]), trimws(SEX[i]), substr(trimws(RACE[i]),1,1))
      parts <- parts[parts != "" & !is.na(parts)]
      paste(parts, collapse="/")
    }),
    DAYS_DISPLAY = ifelse(!is.na(DAYS_LASTDOSE_DEATH), trimws(fmt_days8(DAYS_LASTDOSE_DEATH)), "N/A")
  )

But fmt_days8 also needs vectorization.

Define fmt_days8 vectorized similarly.

Also need to ensure DAYS_LASTDOSE_DEATH calculation correct: SAS does DTHDT - TRTEDT (both SAS dates numeric). In R we do as.numeric(Date diff). That's days.

Similarly DAYS_FIRSTDOSE_DEATH = DTHDT - TRTSDT +1

We did that.

Need to ensure we handle NA correctly.

Also need to ensure we order by TRTA, USUBJID as SAS.

We should also ensure we handle TRTA labels: Placebo etc.

We should produce text file with all numbers.

We should ensure we write l_death.txt with titles containing 16.2.7.3

We should also ensure we write l_death.csv

We should ensure OUT_DIR exists.

We should provide final script.

We need to output exactly one R code block.

We should include all helpers vectorized.

We should ensure we don't call install.packages.

We should ensure we use haven::read_sas etc.

We should ensure we handle both synthetic and pilot paths.

We should ensure we write files to out.

We should ensure we handle errors gracefully.

Let's craft final script.