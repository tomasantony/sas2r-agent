We need to fix syntax error: const_label = "Completed the study" inside function call, but we used named argument const_label = "Completed the study" but function signature is make_disp(filter_expr, const_label = NULL, col_name = NULL). That's fine. However error shows unexpected symbol, likely due to line break without comma? Let's view code snippet:

comp <- make_disp(
  filter_expr = 'toupper(DCREASCD) == "COMPLETED"',
  const_label = "Completed the study"
) %>% mutate(_src = "comp")

That is fine. But maybe stray characters? The error shows at " const_label = "Completed the study" ) %>% mutate(_src". Could be because we used double quotes inside double quotes? Actually filter_expr string contains double quotes inside single quotes, fine. const_label string is double quotes, fine.

But maybe the R parser got confused because we used line continuation without comma after const_label? We have a comma after filter_expr line, then const_label line, then closing parenthesis. That's correct.

But maybe the function definition earlier used const_label = NULL, col_name = NULL, but we passed const_label argument before col_name, okay.

The error "unexpected symbol in: " const_label = "Completed the study"" suggests maybe there is an invisible character or missing comma after const_label line? Let's reformat to single line to avoid.

Also need to ensure column names are case-insensitive; we used uppercase names from dataset. In adsl_raw, column names may be lower case? Haven reads variable names as they appear, likely lower case. In SAS, variable names are uppercase. In R, they become lower case? Actually haven converts to lower case? I think read_xpt keeps original case but as lower case? Haven usually makes variable names lower case. So our select(USUBJID, ...) will fail because column names are lower case. In earlier attempt we used uppercase and got error earlier about usubjid missing. So we need to use lower case names: usubjid, trt01an, trt01a, dcreascd, ittfl.

Thus we should use lower case throughout.

Let's adjust: read_xpt, then rename to lower case? Already lower case. So use select(usubjid, trt01an, trt01a, dcreascd, ittfl). Then mutate accordingly.

Also in later steps, column names should be lower case.

Thus modify code accordingly.

Also adjust make_disp function to use lower case column names.

Also adjust tot grouping.

Also adjust final select column names: keep same as SAS: cnt, per, col1, trt01an, trt01a, nper.

Let's rewrite script.

