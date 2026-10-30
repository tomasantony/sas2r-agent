We need to debug. The error: column usubjid doesn't exist in adsl_raw after read_xpt. Let's inspect the dataset: maybe variable names are uppercase? In SAS, variable names are case-insensitive, but in R they become lower case? Haven reads variable names as they appear, preserving case. In XPT, variable names likely are USUBJID, TRT01AN, TRT01A, DCREASCD, ITTFL. So they are uppercase. In R, they will be uppercase. The script used lower case names. Need to use same case as dataset. So change select to use uppercase names. Also filter condition: ittfl == "Y". variable name ITTFL. Use ITTFL.

Also later code uses trt01an etc. Should use uppercase.

Thus adjust throughout: use USUBJID, TRT01AN, TRT01A, DCREASCD, ITTFL.

Also later functions referencing trt01an etc need to be updated.

Also make_disp uses parse_expr with toupper(dcreascd) etc. Should use DCREASCD.

Also variable names in mutate: trt01an etc.

Let's rewrite.

Also need to ensure that tot join works: tot has TRT01AN column.

Make_disp: filter_expr string uses uppercase.

var_expr handling: for constant, we can just set const_val.

When var_expr is column name, we need to use column DCREASCD.

Also after combine, arrange by TRT01AN, TRT01A, col1.

Select final columns: cnt, per, col1, TRT01AN, TRT01A, nper.

Write CSV.

Also need to ensure that per rounding uses round_half_up.

Potential other issues: In SAS, per is formatted with 5.1 (width 5, 1 decimal). Our sprintf("%5.1f") will produce leading spaces; that's fine.

cnt formatted with 3. width.

Now implement.

Also need to ensure that the macro where condition uses upcase(dcreascd) = "COMPLETED". In R, we can use toupper(DCREASCD) == "COMPLETED". Use filter.

Make_disp: filter_expr string passed to parse_expr; we can use rlang::parse_expr.

But we need to ensure that the expression uses column names as symbols. Use parse_expr.

Now adjust.

Also need to ensure that the constant var_expr is quoted string; we can just pass the string without quotes.

Simplify: define function make_disp(filter_expr, const_label = NULL, col_name = NULL). If const_label not null, use that constant; else use column.

Implement.

Let's craft final script.

