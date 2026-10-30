```r
suppressPackageStartupMessages({
  library(haven); library(dplyr); library(tidyr)
})

out <- Sys.getenv("OUT_DIR")
if (out == "") out <- "."
dir.create(out, showWarnings = FALSE, recursive = TRUE)

round_half_up <- function(x, unit = 1) {
  sign(x) * floor(abs(x) / unit + 0.5 + 1e-9) * unit
}
nz <- function(x) { x <- as.character(x); x[is.na(x)] <- ""; x }
sas_order <- function(...) order(..., method = "radix")

cat("Reading ADSL\n")
adsl <- read_xpt("D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam/adsl.xpt")
names(adsl) <- toupper(names(adsl))
for (v in c("STUDYID","TRT01P","DISCONFL","DCREASCD","DCDECOD","DSRAEFL","DTHFL")) {
  if (!v %in% names(adsl)) adsl[[v]] <- ""
  adsl[[v]] <- nz(adsl[[v]])
}

adsl <- adsl %>% filter(TRT01P != "") %>%
  mutate(TRT01PN = case_when(TRT01PN == 0 ~ 3, TRT01PN == 54 ~ 1,
                             TRT01PN == 81 ~ 2, TRUE ~ as.numeric(TRT01PN)))

# PROC FREQ-like count (missing levels excluded), sorted by the table vars
freq <- function(d, vars) {
  for (v in vars) {
    if (is.character(d[[v]])) d <- d[d[[v]] != "", ] else d <- d[!is.na(d[[v]]), ]
  }
  r <- d %>% group_by(across(all_of(vars))) %>% summarise(COUNT = n(), .groups = "drop") %>%
    as.data.frame()
  r[do.call(sas_order, unname(as.list(r[vars]))), , drop = FALSE]
}

grp <- c("STUDYID","TRT01PN","TRT01P")
topline <- freq(adsl, c(grp, "DISCONFL"))

d1 <- adsl %>% filter(DSRAEFL == "Y" | DTHFL == "Y")
midline1 <- freq(d1, c(grp, "DCREASCD")); sortord1 <- freq(d1, c("STUDYID","DCREASCD"))
d2 <- adsl %>% filter(DCDECOD == "LACK OF EFFICACY")
midline2 <- freq(d2, c(grp, "DCREASCD")); sortord2 <- freq(d2, c("STUDYID","DCREASCD"))
d3 <- adsl %>% filter(DCDECOD != "LACK OF EFFICACY" & DSRAEFL != "Y" & DTHFL != "Y") %>%
  filter(DISCONFL == "Y")
midline3 <- freq(d3, c(grp, "DCREASCD")); sortord3 <- freq(d3, c("STUDYID","DCREASCD"))

# data all (interleave by studyid trt01pn trt01p)
all <- bind_rows(
  topline  %>% mutate(src = 1, ORD1 = 1, ORD2 = 1),
  midline1 %>% mutate(src = 2, ORD1 = 2, ORD2 = 1),
  midline2 %>% mutate(src = 3, ORD1 = 3, ORD2 = 2),
  midline3 %>% mutate(src = 4, ORD1 = 4, ORD2 = 3))
all$rn <- seq_len(nrow(all))
all <- all[sas_order(all$STUDYID, all$TRT01PN, all$TRT01P, all$src, all$rn), ]

alls <- bind_rows(
  sortord1 %>% mutate(ORD1 = 2, ORD2 = 1),
  sortord2 %>% mutate(ORD1 = 3, ORD2 = 2),
  sortord3 %>% mutate(ORD1 = 4, ORD2 = 3))
alls <- alls[sas_order(alls$ORD1, alls$ORD2, -alls$COUNT), ]
alls2 <- alls %>% group_by(ORD1, ORD2) %>% mutate(ORD3 = row_number()) %>% ungroup() %>%
  as.data.frame()

pop <- freq(adsl, grp) %>% rename(TOT = COUNT)

# allp
allp <- full_join(all, pop, by = grp)
allp$countc <- ifelse(!is.na(allp$COUNT),
  paste0(formatC(allp$COUNT, width = 4, format = "d"), " (",
         sprintf("%5.1f", round_half_up(allp$COUNT / allp$TOT, 0.1)), ")"), NA)
allp$DCREASCD <- nz(allp$DCREASCD)
allp$DISCONFL <- nz(allp$DISCONFL)
i1 <- !is.na(allp$ORD1) & allp$ORD1 == 1
allp$DCREASCD[i1] <- ifelse(allp$DISCONFL[i1] == "Y", "Discontinued", "Completed the Study")

allp <- allp[sas_order(allp$STUDYID, allp$ORD1, allp$ORD2, allp$DCREASCD, allp$TRT01PN), ]

# transpose
allp$idn <- paste0("trt", allp$TRT01PN)
allpt <- allp %>% select(STUDYID, ORD1, ORD2, DCREASCD, idn, countc) %>%
  pivot_wider(names_from = idn, values_from = countc) %>% as.data.frame()

maxt <- nrow(pop)
for (i in seq_len(maxt)) {
  v <- paste0("trt", i)
  if (!v %in% names(allpt)) allpt[[v]] <- NA_character_
  x <- allpt[[v]]
  allpt[[v]] <- ifelse(is.na(x) | x == "", "###0", gsub(" ", "#", x))
}
allpt2 <- allpt

# merge allpt2 alls2 by studyid ord1 ord2 (SAS many-to-many semantics)
keys <- unique(rbind(allpt2[, c("STUDYID","ORD1","ORD2")], alls2[, c("STUDYID","ORD1","ORD2")]))
keys <- keys[sas_order(keys$STUDYID, keys$ORD1, keys$ORD2), ]
trtv <- paste0("trt", seq_len(maxt))
res <- list()
for (k in seq_len(nrow(keys))) {
  a <- allpt2[allpt2$STUDYID == keys$STUDYID[k] & allpt2$ORD1 %in% keys$ORD1[k] &
                allpt2$ORD2 %in% keys$ORD2[k], , drop = FALSE]
  b <- alls2[alls2$STUDYID == keys$STUDYID[k] & alls2$ORD1 %in% keys$ORD1[k] &
               alls2$ORD2 %in% keys$ORD2[k], , drop = FALSE]
  na <- nrow(a); nb <- nrow(b)
  for (i in seq_len(max(na, nb))) {
    row <- data.frame(STUDYID = keys$STUDYID[k], ORD1 = keys$ORD1[k], ORD2 = keys$ORD2[k],
                      stringsAsFactors = FALSE)
    for (v in trtv) row[[v]] <- if (na > 0) a[[v]][min(i, na)] else NA_character_
    row$ORD3 <- if (nb > 0) b$ORD3[min(i, nb)] else NA
    if (nb > 0 && i <= nb) row$DCREASCD <- b$DCREASCD[i]
    else if (na > 0) row$DCREASCD <- a$DCREASCD[min(i, na)]
    else row$DCREASCD <- if (nb > 0) b$DCREASCD[nb] else ""
    res[[length(res) + 1]] <- row
  }
}
allpt3 <- bind_rows(res)
o1 <- allpt3$ORD1 == 1
allpt3$ORD3[o1 & toupper(allpt3$DCREASCD) == "DISCONTINUED"] <- 2
allpt3$ORD3[o1 & toupper(allpt3$DCREASCD) != "DISCONTINUED"] <- 1
allpt3 <- allpt3[sas_order(allpt3$STUDYID, allpt3$ORD1, allpt3$ORD2, allpt3$ORD3), ]

# final
final <- allpt3
final$PAGEX <- 1
final$COL1 <- ""
final$COL1[final$ORD1 == 1] <- trimws(final$DCREASCD[final$ORD1 == 1])
final$COL1[final$ORD1 == 3] <- paste0("####", trimws(final$DCREASCD[final$ORD1 == 3]))
final <- final[, c("COL1","STUDYID","DCREASCD","ORD1","ORD2","ORD3", trtv, "PAGEX")]
write.csv(final, file.path(out, "final.csv"), row.names = FALSE, na = "")

# Report rendering
pop <- pop[order(pop$TRT01PN), ]
trtc <- trimws(pop$TRT01P)
hdr1 <- paste0(trtc, " ")
hdr2 <- paste0("(N=", trtc, ")")
w1 <- 40; wc <- max(26, nchar(hdr2) + 2)
padr <- function(x, w) formatC(x, width = -w, flag = "-")
lines <- c("Table X: Dispostion Table", "##", "Subject Disposition", "",
  paste0(padr("", w1), paste0(padr(hdr1, wc), collapse = "")),
  paste0(padr("", w1), paste0(padr(hdr2, wc), collapse = "")),
  paste0(padr("Subject Disposition", w1), paste0(padr(rep("  n   %", maxt), wc), collapse = "")),
  strrep("-", w1 + wc * maxt))
prev_ord2 <- NA
for (i in seq_len(nrow(final))) {
  if (!identical(final$ORD2[i], prev_ord2) && i > 1) lines <- c(lines, "")
  prev_ord2 <- final$ORD2[i]
  lines <- c(lines, paste0(padr(final$COL1[i], w1),
                           paste0(padr(unlist(final[i, trtv]), wc), collapse = "")))
}
lines <- c(lines, "",
  "Abbreviations: N=total number of subjects in population.",
  "###############n=number of subjects in the specified category",
  "###############%=Percentage of patients with N as denomiator",
  "##", "##", "Program Location: blah lah blah")

tab <- final[, c("COL1", trtv)]
names(tab) <- c("Subject Disposition", paste0(trtc, " (N=", trtc, ") n %"))
for (nm in c("P048", "sasrtf")) {
  writeLines(lines, file.path(out, paste0(nm, ".txt")))
  write.csv(tab, file.path(out, paste0(nm, ".csv")), row.names = FALSE, na = "")
}
cat("Done\n")
```