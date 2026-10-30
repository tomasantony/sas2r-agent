We need to fix errors. The error arises in left_join where trtvar variable name mismatch. In earlier code, we used trtvar = "trtpn". In full we have column named trtpn (lowercase). In allcat we used distinct(param, paramn, !!sym(trtvar)). That yields column trtpn. In full we used expand_grid with !!sym(trtvar) := cur[[trtvar]] but inside rowwise, cur[[trtvar]] may not exist because cur has column trtpn but we used variable trtvar = "trtpn". Should work. However later left_join uses by = c("param","paramn",trtvar = "trtpn", "bnrindn","anrindn") but trtvar variable is "trtpn". So we wrote trtvar = "trtpn" which is same name, but left_join expects column named "trtpn" in both. In full, column name is trtpn. In fixtot, column name is trtpn as well. However we used fixtot earlier defined from counts, which had column trtpn. So join should be fine. The error says Unknown or uninitialised column: `denom`. Actually earlier we had denom variable from fixtot$denom but we incorrectly assigned denom <- fixtot$denom (should be from earlier list). In code: denom   <- fixtot$denom (but fixtot is a data frame, not list). We earlier created list with fixtot and denom, but then we overwrote fixtot variable with that list's fixtot, and denom variable with that list's denom? Let's check: earlier we had:

fixtot <- counts %>% ... %>% { denom <- filter(...); fixtot <- filter(...); list(fixtot = fixtot, denom = denom) }

fixtot <- fixtot$fixtot
denom   <- fixtot$denom

But after that, fixtot is a data frame, so fixtot$denom is NULL. So denom becomes NULL, causing later join missing denom column.

We need to capture both correctly. Let's restructure: after the block, assign both objects.

We can do:

tmp <- counts %>% ... { denom <- ...; fixtot <- ...; list(fixtot = fixtot, denom = denom) }

fixtot <- tmp$fixtot
denom   <- tmp$denom

Fix that.

Also later left_join for allrecs: we used left_join(fixtot, by = c("param","paramn",trtvar = "trtpn", "bnrindn","anrindn")). Since trtvar is "trtpn", we can just use trtpn. Simpler: by = c("param","paramn","trtpn","bnrindn","anrindn").

Also later left_join allrecs with denom: by = c("param","paramn","trtpn").

Also need to ensure that denom column exists after join.

Also need to ensure that the text file is written; earlier we wrote P054.txt but expected P054.lst maybe. The spec says output P054.lst (table/listing text). So we should write file named "P054.lst". Also need to write CSV of same table. Already wrote P054.csv.

Also need to ensure numbers formatting matches SAS: SAS uses n (integer) and pct with parentheses and one decimal, width maybe 5.1. Our formatting uses sprintf("%5.1f", v) which yields leading spaces. That's fine.

But need to ensure that the text file includes all rows, not empty. In earlier script, we used pwalk on trans select columns, but we didn't include line breaks correctly? Should be fine.

But we need to ensure that we include treatment grouping break lines? Not required.

Also need to ensure that we write the text file before closing.

Let's rewrite script with corrections.

