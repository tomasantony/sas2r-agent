The error: after left_join(ae, dm, by="USUBJID"), the resulting column names: USUBJID (from ae) and USUBJID (from dm) get suffix .x and .y? Actually default suffixes are .x and .y. In our code we used left_join(dm, by="USUBJID") which adds dm columns with suffix .y? Since both have USUBJID same name, they are not duplicated; only one column USUBJID remains. So after left_join, there is no USUBJID.y. The SAS code used a.USUBJID = d.USUBJID and then where d.USUBJID is null. In R we need to detect rows where dm does not have matching USUBJID. Use anti_join instead. So replace left_join + filter(is.na(USUBJID.y)) with anti_join(ae, dm, by="USUBJID").

Similarly for other referential integrity checks.

Also for other joins where we used left_join and then filter(is.na(USUBJID.y)), need to adjust.

Let's modify:

chk_ae01 <- anti_join(ae, dm, by="USUBJID") %>% summarise(N_FAIL=n()) ...

Similarly for ex, lb, vs, eg, mh, sv, etc.

Also for chk_adsl02 we used left_join(ex) and filter(a.SAFFL='Y' and e.USUBJID is null). Use anti_join.

Also for chk_cm01 we used left_join(dm) and filter(d.USUBJID is null). Use anti_join.

Also for chk_cm02 we need inner join, fine.

Also for chk_seq01 we used bind_rows with select(SOURCE = "AE"...). That is wrong: we attempted to set SOURCE constant but using select with literal not allowed. Should use mutate.

Better: ae %>% mutate(SOURCE="AE") %>% select(SOURCE, USUBJID, SEQ=AESEQ). Similarly for others.

Also for chk_cm02 we used mutate(CMENRTPT_up = toupper(str_trim(CMENRTPT))) but extra parentheses. Fix.

Also for chk_cm02 we need to ensure date columns are character; they may be dates already. Use as.character.

Also for chk_cm02 condition: (CMENRTPT_up != "ONGOING" & !is_missing(CMENTPT)) should be flagged as fail.

Now also need to ensure that date columns are character strings for regex checks; if they are Date class, str_trim works but regex expects string. Convert to character.

Also for chk_eg04 raw variable detection: we used sum(toupper(names(eg)) %in% raw_eg_vars). That's fine.

Now ensure that all objects exist.

Also need to ensure that we write .txt file not .lst? The requirement: write a text rendering and CSV of the table. The name can be any .txt. In previous attempt we wrote P019.lst. That's okay as .lst is a text file. But they said .txt or .csv. Use .txt.

Let's write P019.txt.

Now implement changes.

