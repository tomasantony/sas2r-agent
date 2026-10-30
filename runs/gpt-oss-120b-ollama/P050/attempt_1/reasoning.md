We need to translate SAS program to R. Need to produce dataset disp.csv under OUT_DIR. Also need to follow SAS semantics: reading adsl.xpt, keep variables, filter ittfl="Y". Then create adsl1 with extra total row (trt01an=999, trt01a="Total") and output both original rows and total row.

Then compute tot table: for each trt01an, count distinct usubjid as tot.

Also need macro variables ntrt, trt1-... etc but not needed for final dataset.

Then dummy dataset not used.

Macro disp creates tables comp, disc, all with counts per treatment group.

The macro uses SQL: select count(distinct usubjid) as cnt, (cnt*100)/tot as per, &var as col1, a.trt01an, trt01a length=50 from adsl1 as a, tot as b where &where and a.trt01an = b.trt01an group by a.trt01an, trt01a, tot, col1;

So for each treatment group, compute count of distinct usubjid meeting condition, compute percent of total for that group (using tot from tot table). col1 is a constant string (or variable). For "all", var is dcreascd (the original value). So for all, col1 will be the value of dcreascd variable (so each row per distinct dcreascd? Actually grouping includes col1, which is dcreascd variable, so each distinct dcreascd value per treatment group will be a row.

Thus we need to replicate.

Then combine comp, disc, all into disp dataset. The SAS data step sets by trt01an trt01a; if a then col1 = "  " || trim(left(col1)); So for rows from 'all' (where a flag true), prepend two spaces to col1.

Then nper = put(cnt, 3.) || "(" || put(per, 5.1) || ")"; This creates string with count formatted width 3 (no leading zeros) and per with 5.1 (one decimal). In SAS, put(cnt,3.) will right-align with spaces; but concatenated with string, spaces may be kept. We'll replicate as sprintf("%3d(%5.1f)", cnt, per). However SAS format 5.1 includes leading spaces for width 5, one decimal. We'll produce same as sprintf("%3d(%5.1f)", cnt, per). That yields spaces maybe. We'll keep as character.

Need to output dataset disp.csv with variable names as in SAS: likely columns: cnt, per, col1, trt01an, trt01a, nper? Actually nper is new variable; SAS dataset outds.disp includes all variables from comp/disc/all plus nper. The order: after set comp disc all(in=a); by trt01an trt01a; if a then col1 = "  " || trim(left(col1)); nper = ...; So final dataset has columns: cnt, per, col1, trt01an, trt01a, nper. Also maybe other variables from all (like dcreascd) but not included because all dataset includes col1 as dcreascd value, not separate variable. So final columns as above.

We need to write CSV with row.names=FALSE, na="". Dates formatting not needed.

Implement using dplyr.

Steps:

- out <- Sys.getenv("OUT_DIR")
- rawadam <- Sys.getenv("rawadam")? Not given; but macro variable rawadam path is known but we can just use environment variable? Not set. We can construct path using given macro values? The script should use absolute paths given in context. So we can set rawadam_path <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam". Use that.

- Read adsl.xpt: haven::read_xpt(file.path(rawadam_path, "adsl.xpt"))

- Keep variables: usubjid, trt01an, trt01a, dcreascd, ittfl. Filter ittfl == "Y". Keep only those columns.

- adsl1: bind rows of adsl and a tibble with trt01an=999, trt01a="Total", other columns? In SAS they output original rows then set trt01an=999, trt01a="Total", output; other variables remain as from current observation? In SAS they set trt01an=999; trt01a="Total"; output; Since other variables (usubjid, dcreascd, ittfl) remain as from the current observation? Actually they are still from the observation being processed (the same as previous row). In SAS, after output; then they change trt01an and trt01a for the same observation and output again. So the total row duplicates the same usubjid etc as the original row but with trt01an=999 and trt01a="Total". That seems odd but that's what code does. So for each original row, there will be a duplicate row with total group. So adsl1 will have twice number of rows. We'll replicate that.

Implementation: adsl1 <- bind_rows(adsl, adsl %>% mutate(trt01an=999, trt01a="Total"))

- tot: for each trt01an, count distinct usubjid. Use distinct.

tot <- adsl1 %>% group_by(trt01an) %>% summarise(tot = n_distinct(usubjid), .groups='drop')

- Macro disp: function that takes where expression (as R expression) and var (string). For comp: where upcase(dcreascd) == "COMPLETED". In R, toupper(dcreascd) == "COMPLETED". For disc: toupper(dcreascd) != "COMPLETED". For all: toupper(dcreascd) != "COMPLETED"? Actually same condition as disc but var = dcreascd (original). So for all, we need to group by dcreascd value.

Implementation:

disp_func <- function(where_expr, var_expr, var_is_constant=TRUE) {
   # filter adsl1 according to where_expr (as expression)
   filtered <- adsl1 %>% filter(!!rlang::parse_expr(where_expr))
   # join with tot to get tot per trt01an
   filtered <- filtered %>% left_join(tot, by="trt01an")
   if (var_is_constant) {
        # var_expr is a quoted string constant, e.g., "Completed the study"
        col1_val <- gsub('^"|"$', '', var_expr) # remove quotes
        result <- filtered %>%
            group_by(trt01an, trt01a, tot) %>%
            summarise(cnt = n_distinct(usubjid),
                      per = (cnt*100)/tot,
                      col1 = col1_val,
                      .groups='drop')
   } else {
        # var_expr is column name, e.g., dcreascd
        result <- filtered %>%
            group_by(trt01an, trt01a, tot, !!rlang::sym(var_expr)) %>%
            summarise(cnt = n_distinct(usubjid),
                      per = (cnt*100)/tot,
                      col1 = !!rlang::sym(var_expr),
                      .groups='drop')
   }
   # rename col1 already set
   result
}

But need to ensure ordering same as SAS: order by trt01an, trt01a, col1? SAS group by a.trt01an, trt01a, tot, col1; default order is by group variables order. We'll arrange by trt01an, trt01a, col1.

Now compute comp, disc, all.

comp <- disp_func('toupper(dcreascd) == "COMPLETED"', '"Completed the study"', TRUE)

disc <- disp_func('toupper(dcreascd) != "COMPLETED"', '"Discontinued"', TRUE)

all <- disp_func('toupper(dcreascd) != "COMPLETED"', 'dcreascd', FALSE)

Now combine:

disp <- bind_rows(comp, disc, all, .id = "source")? In SAS they set all(in=a) to flag rows from all. So we need to know which rows are from all to prepend spaces. We'll add a flag.

comp$source <- "comp"
disc$source <- "disc"
all$source <- "all"

combined <- bind_rows(comp, disc, all) %>% arrange(trt01an, trt01a, col1)

Now modify col1 for rows where source=="all": prepend two spaces.

combined <- combined %>% mutate(col1 = if_else(source=="all", paste0("  ", str_trim(col1)), col1))

Now compute nper string: sprintf("%3d(%5.1f)", cnt, per). Need to ensure per numeric maybe with rounding half away from zero? SAS uses round-half-up for formatting? The per is computed as (cnt*100)/tot, then formatted with 5.1 which rounds half away from zero. R's sprintf uses round half away? It uses round to nearest even? Actually formatting with sprintf uses round to nearest, ties to even? Not sure. We'll implement custom rounding: per_rounded <- round_half_up(per, 1). We'll define function.

round_half_up <- function(x, digits=0) {
  posneg <- sign(x)
  absx <- abs(x)
  factor <- 10^digits
  floor(absx*factor + 0.5)/factor * posneg
}

Then per_formatted <- sprintf("%5.1f", round_half_up(per,1))

cnt_formatted <- sprintf("%3d", cnt)

nper <- paste0(cnt_formatted, "(", per_formatted, ")")

Add column nper.

Finally select columns: cnt, per, col1, trt01an, trt01a, nper (maybe keep order). Remove source column.

Write CSV to file.path(out, "disp.csv").

Also need to write plain-text rendering and CSV of table body? The spec says each table or listing additionally as plain-text rendering and CSV of table body. But declared output types: dataset only. So maybe only dataset needed. We'll just output dataset.

Implement.

Need to load packages: haven, dplyr, stringr, rlang.

Make sure to set stringsAsFactors=FALSE.

Write script.

