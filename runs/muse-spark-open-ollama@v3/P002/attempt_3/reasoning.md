We need to fix error: can only subtract from Date objects. The issue is ae_merged$ASTDT and TRTSDT are Date, but subtraction should work. However error says can only subtract from Date objects - maybe one is character? Let's see: ae_merged$ASTDT is Date, TRTSDT is Date, but idx_ge subset returns Date, should work. But error indicates maybe TRTSDT is not Date? We converted temp$TRTSDT via convert_sas_date, but ae_merged$TRTSDT after merge might be numeric? Let's check ADSL TRTSDT type: haven read_xpt may give Date. But we converted. Should be Date.

But error: `-.Date`(ae_merged$ASTDT[idx_ge], ae_merged$TRTSDT[idx_ge]) : can only subtract from "Date" objects

This suggests ae_merged$ASTDT[idx_ge] is Date, but ae_merged$TRTSDT[idx_ge] is not Date? Actually subtraction method is dispatched on first argument. If first is Date, it expects second to be Date or numeric? But error says can only subtract from Date objects - maybe second is character?

Wait: ae_merged$TRTSDT might be character after some operation? No.

Alternatively, the issue is that ae_merged$ASTDT is Date, but ae_merged$TRTSDT is also Date, subtraction should work: Date - Date gives difftime numeric.

But error occurs: maybe ae_merged$TRTSDT is POSIXct? No.

Let's look at code: we did ae_merged$ASTDT <- parse_yymmdd10(...) which returns Date (as.Date). Good.

temp$TRTSDT <- convert_sas_date(temp$TRTSDT) which returns Date.

Then merge: ae_merged <- merge(ae, temp, by="USUBJID", ...) - this should preserve Date class.

But then we do idx_impute handling: ae_merged$ASTDT[idx_impute] <- parsed where parsed is Date. That's fine.

Then we compute idx_both_ast <- !is.na(ae_merged$ASTDT) & !is.na(ae_merged$TRTSDT) - works.

Then idx_ge <- idx_both_ast & ae_merged$ASTDT >= ae_merged$TRTSDT - comparison of Dates works.

Then ae_merged$ASTDY[idx_ge] <- as.integer(ae_merged$ASTDT[idx_ge] - ae_merged$TRTSDT[idx_ge] + 1L)

The error says can only subtract from Date objects. This happens when trying to subtract Date from Date? Actually Date - Date should work. Let's test: as.Date("2020-01-02") - as.Date("2020-01-01") = 1. Works.

Why error? Possibly one of them is not Date but character? Let's check ADSL TRTSDT raw: In ADSL xpt, TRTSDT might be character? Let's see typical CDISC pilot ADSL: TRTSDT is numeric SAS date with format? Haven might read as numeric? But we converted numeric to Date via as.Date(x, origin="1960-01-01"). That should be Date.

But what about ae_merged$ASTDT? It's Date.

Wait error message: `-.Date`(ae_merged$ASTDT[idx_ge], ae_merged$TRTSDT[idx_ge]) : can only subtract from "Date" objects

This error is from Ops.Date: it checks that e2 is not Date? Actually Date subtraction: Date - Date is allowed, Date - numeric is allowed? Let's recall R's Date Ops: `-.Date` checks if e1 is Date and e2 is Date or numeric? The error "can only subtract from Date objects" occurs when you try to do numeric - Date? No.

Let's look at R source: `-.Date` function:

```
`-.Date` <- function (e1, e2) 
{
    if (!inherits(e1, "Date")) 
        stop("can only subtract from \"Date\" objects")
    ...
}
```

So it checks e1 is Date, which it is. Then why error? Wait the error message says "can only subtract from Date objects" - that is the check for e1. So e1 is not Date? But we thought ae_merged$ASTDT[idx_ge] is Date. Could it be that subsetting a Date vector with logical index returns numeric? No, subsetting Date preserves class.

Unless ae_merged$ASTDT is not Date but character? Let's see: we set ae$ASTDT <- parse_yymmdd10(...) which returns Date, but parse_yymmdd10 returns vector of class Date? We did res <- rep(NA, length(x)) which is logical NA, then res[idx] <- as.Date(...). But rep(NA, ...) is logical, not Date. Then assigning Date into logical vector may coerce to numeric? Actually res is logical, then res[idx] <- as.Date(...) will coerce Date to numeric? Let's see: rep(NA, 5) is logical. Then res[1] <- as.Date("2020-01-01") will convert Date to numeric (days since 1970) but lose class? Let's test mentally: In R, c(NA, as.Date("2020-01-01")) will be numeric? Actually Date is numeric with class. Assigning Date into logical vector may strip class.

We did: res <- rep(NA, length(x)) then res[idx] <- as.Date(x[idx], format="%Y-%m-%d") . The res vector initially is logical, but after assignment, it becomes numeric (since Date is numeric). But does it retain Date class? No, because res was not Date. So res will be numeric, not Date. Then later ae$ASTDT is numeric, not Date. Then ae_merged$ASTDT is numeric, not Date. Then subtraction fails because e1 is numeric, not Date, but the method dispatched is `-.Date`? Wait if e1 is numeric, it wouldn't dispatch to `-.Date`. Hmm.

Actually if ae_merged$ASTDT is numeric (without class), then ae_merged$ASTDT[idx_ge] - ae_merged$TRTSDT[idx_ge] would be numeric - Date? That would dispatch? Let's see: numeric - Date: e1 is numeric, not Date, so it wouldn't call `-.Date`. But error shows `-.Date` was called, meaning e1 is Date. So maybe ae_merged$ASTDT is Date, but ae_merged$TRTSDT is not Date? Wait e1 is Date, so it passes first check, then it checks e2? Let's look at full `-.Date` code:

```
`-.Date` <- function (e1, e2) 
{
    if (!inherits(e1, "Date")) 
        stop("can only subtract from \"Date\" objects")
    if (nargs() == 1) 
        stop("unary '-' is not defined for \"Date\" objects")
    if (inherits(e2, "Date")) 
        return(difftime(e1, e2, units = "days"))
    if (is.numeric(e2)) 
        return(.Date(unclass(e1) - e2))
    stop("can only subtract from \"Date\" objects")
}
```

Ah, the last stop is also "can only subtract from Date objects" but it's for e2 not being Date or numeric. So error could be e2 is not Date nor numeric, e.g., character. So ae_merged$TRTSDT[idx_ge] might be character.

Why would TRTSDT be character? Let's see convert_sas_date: if x is character, it returns parse_yymmdd10(x) which returns Date? But parse_yymmdd10 has same bug: res <- rep(NA, length(x)) then res[idx] <- as.Date(...). So it also returns numeric without class if initial res is logical.

So both ASTDT and TRTSDT might be numeric without Date class, but then subtraction would be numeric - numeric, not Date. But error shows Date dispatch, so one is Date, other is character.

Wait, temp$TRTSDT conversion: if adsl_raw$TRTSDT is numeric (SAS date), then convert_sas_date returns as.Date(x, origin="1960-01-01") which is Date correctly (since as.Date.numeric with origin returns Date). So temp$TRTSDT is Date.

But ae$ASTDT is numeric (due to bug), so ae_merged$ASTDT is numeric, ae_merged$TRTSDT is Date. Then ae_merged$ASTDT[idx_ge] is numeric, ae_merged$TRTSDT[idx_ge] is Date. Then expression ae_merged$ASTDT[idx_ge] - ae_merged$TRTSDT[idx_ge] would be numeric - Date. That would dispatch to `-.Date`? No, e1 is numeric, not Date, so it would not dispatch to `-.Date`, it would be numeric - Date, which would try to dispatch based on e1? Actually S3 dispatch for Ops: if either is Date, it will dispatch? Let's see: Ops.Date is for Date objects. If e1 is numeric and e2 is Date, then e1 is not Date, but e2 is Date, does it still dispatch? In R, Ops dispatch checks both? For binary ops, if either is Date, it may dispatch. The error message shows `-.Date` was called with e1 = ae_merged$ASTDT[idx_ge] (which is numeric? but it says can only subtract from Date objects, meaning e1 is not Date). Wait the error says can only subtract from Date objects, which is the check for e1 not being Date. So e1 is not Date. That matches: ae_merged$ASTDT is numeric (not Date), so e1 is numeric, fails first check.

So root cause is parse_yymmdd10 returns numeric without Date class due to rep(NA, ...) initialization.

Fix: initialize res as as.Date(rep(NA, length(x))) or rep(as.Date(NA), length(x)).

Also need to ensure convert_sas_date handles character correctly with same fix.

We need to fix parse_yymmdd10 to return Date properly.

Also need to handle other date conversions.

Let's fix script minimally: correct parse_yymmdd10 and convert_sas_date.

Also need to ensure ADUR calculation uses Date diff correctly: as.integer(AENDT - ASTDT +1) where both are Date, diff is difftime, need as.numeric.

We should fix those.

We should also ensure that after fixing, the rest works.

We need to output complete corrected script.

We should also ensure OUT_DIR handling and file writing.

We need to keep everything else same, just fix the bug.

Let's craft final script with corrected parse_yymmdd10.

Also need to ensure that ae$ASTDT and ae$AENDT are Date.

Implement:

parse_yymmdd10 <- function(x) {
  x <- trimws(as.character(x))
  res <- as.Date(rep(NA, length(x)))
  idx <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  if (any(idx)) {
    res[idx] <- as.Date(x[idx], format="%Y-%m-%d")
  }
  res
}

Similarly for convert_sas_date character branch.

Also need to handle ADUR: if both Date, then as.integer(as.numeric(AENDT - ASTDT) +1) or as.integer(AENDT - ASTDT +1) where difftime to numeric.

Better to use as.integer(as.numeric(AENDT - ASTDT) +1)

But original code used as.integer(AENDT - ASTDT +1) where AENDT and ASTDT are Date, subtraction gives difftime, adding 1 gives difftime, as.integer on difftime may not work. Need to convert to numeric.

We should fix ADUR and ASTDY etc to use as.numeric.

Let's check original script: ae$ADUR <- ifelse(!is.na(ae$ASTDT) & !is.na(ae$AENDT), as.integer(ae$AENDT - ae$ASTDT + 1L), NA_integer_)

If ASTDT and AENDT are Date, then ae$AENDT - ae$ASTDT returns difftime, +1L still difftime, as.integer(difftime) may give integer days? Actually as.integer(difftime) returns integer days? Let's test: as.integer(as.Date("2020-01-02") - as.Date("2020-01-01") +1) => as.integer(difftime) ? In R, difftime + numeric gives difftime, as.integer(difftime) works? It may return integer days. But safer to use as.numeric.

We should change to as.integer(as.numeric(ae$AENDT - ae$ASTDT) +1)

Similarly for ASTDY etc.

We need to ensure we fix all date arithmetic to use as.numeric.

Let's produce final script with these fixes, keeping rest identical.

