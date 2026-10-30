```r
out <- Sys.getenv("OUT_DIR")
if (out == "") out <- getwd()
dir.create(out, showWarnings = FALSE, recursive = TRUE)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

sas_round <- function(x, inc) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x)/inc + 0.5 + 1e-9) * inc)
}
sas_put_4 <- function(x) sprintf("%4d", as.integer(x))
sas_put_5_1 <- function(x) {
  xr <- sas_round(x, 0.1)
  sprintf("%5.1f", xr)
}

rawadam1 <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
rawadam2 <- "D:/meta_hackathon/sas2r-agent/data/derived/cdisc-pilot/adam"
f1 <- file.path(rawadam1, "adsl.xpt")
f2 <- file.path(rawadam2, "adsl.xpt")
adsl_path <- if (file.exists(f1)) f1 else if (file.exists(f2)) f2 else f1

adsl_raw <- haven::read_xpt(adsl_path)

adsl <- adsl_raw %>%
  mutate(across(where(haven::is.labelled), haven::as_factor)) %>%
  mutate(across(where(is.factor), as.character))

names(adsl) <- toupper(names(adsl))

adsl <- adsl %>%
  filter(!is.na(TRT01P) & TRT01P != "") %>%
  mutate(
    TRT01PN = as.numeric(TRT01PN),
    TRT01PN = case_when(
      TRT01PN == 0 ~ 3,
      TRT01PN == 54 ~ 1,
      TRT01PN == 81 ~ 2,
      TRUE ~ TRT01PN
    )
  )

for (cn in c("STUDYID","TRT01PN","TRT01P","DISCONFL","DCREASCD","DCDECOD","DSRAEFL","DTHFL")) {
  if (!toupper(cn) %in% toupper(names(adsl))) {
    adsl[[cn]] <- NA_character_
  }
}
names(adsl) <- toupper(names(adsl))

topline <- adsl %>%
  filter(!is.na(DISCONFL) & DISCONFL != "") %>%
  group_by(STUDYID, TRT01PN, TRT01P, DISCONFL) %>%
  summarise(COUNT = n(), .groups="drop")

midline1 <- adsl %>%
  filter((DSRAEFL == "Y" & !is.na(DSRAEFL)) | (DTHFL == "Y" & !is.na(DTHFL))) %>%
  filter(!is.na(DCREASCD) & DCREASCD != "") %>%
  group_by(STUDYID, TRT01PN, TRT01P, DCREASCD) %>%
  summarise(COUNT = n(), .groups="drop")

sortord1 <- adsl %>%
  filter((DSRAEFL == "Y" & !is.na(DSRAEFL)) | (DTHFL == "Y" & !is.na(DTHFL))) %>%
  filter(!is.na(DCREASCD) & DCREASCD != "") %>%
  group_by(STUDYID, DCREASCD) %>%
  summarise(COUNT = n(), .groups="drop")

midline2 <- adsl %>%
  filter(DCDECOD == "LACK OF EFFICACY" & !is.na(DCDECOD)) %>%
  filter(!is.na(DCREASCD) & DCREASCD != "") %>%
  group_by(STUDYID, TRT01PN, TRT01P, DCREASCD) %>%
  summarise(COUNT = n(), .groups="drop")

sortord2 <- adsl %>%
  filter(DCDECOD == "LACK OF EFFICACY" & !is.na(DCDECOD)) %>%
  filter(!is.na(DCREASCD) & DCREASCD != "") %>%
  group_by(STUDYID, DCREASCD) %>%
  summarise(COUNT = n(), .groups="drop")

midline3 <- adsl %>%
  filter(
    (is.na(DCDECOD) | DCDECOD != "LACK OF EFFICACY") &
    (is.na(DSRAEFL) | DSRAEFL != "Y") &
    (is.na(DTHFL) | DTHFL != "Y") &
    DISCONFL == "Y"
  ) %>%
  filter(!is.na(DCREASCD) & DCREASCD != "") %>%
  group_by(STUDYID, TRT01PN, TRT01P, DCREASCD) %>%
  summarise(COUNT = n(), .groups="drop")

sortord3 <- adsl %>%
  filter(
    (is.na(DCDECOD) | DCDECOD != "LACK OF EFFICACY") &
    (is.na(DSRAEFL) | DSRAEFL != "Y") &
    (is.na(DTHFL) | DTHFL != "Y") &
    DISCONFL == "Y"
  ) %>%
  filter(!is.na(DCREASCD) & DCREASCD != "") %>%
  group_by(STUDYID, DCREASCD) %>%
  summarise(COUNT = n(), .groups="drop")

topline2 <- topline %>%
  mutate(ORD1 = 1L, ORD2 = 1L, DCREASCD = NA_character_) %>%
  select(STUDYID, TRT01PN, TRT01P, DISCONFL, DCREASCD, COUNT, ORD1, ORD2)

midline1_2 <- midline1 %>%
  mutate(ORD1 = 2L, ORD2 = 1L, DISCONFL = NA_character_) %>%
  select(STUDYID, TRT01PN, TRT01P, DISCONFL, DCREASCD, COUNT, ORD1, ORD2)

midline2_2 <- midline2 %>%
  mutate(ORD1 = 3L, ORD2 = 2L, DISCONFL = NA_character_) %>%
  select(STUDYID, TRT01PN, TRT01P, DISCONFL, DCREASCD, COUNT, ORD1, ORD2)

midline3_2 <- midline3 %>%
  mutate(ORD1 = 4L, ORD2 = 3L, DISCONFL = NA_character_) %>%
  select(STUDYID, TRT01PN, TRT01P, DISCONFL, DCREASCD, COUNT, ORD1, ORD2)

all_df <- bind_rows(topline2, midline1_2, midline2_2, midline3_2) %>%
  arrange(STUDYID, TRT01PN, TRT01P)

sortord1_2 <- sortord1 %>% mutate(ORD1=2L, ORD2=1L)
sortord2_2 <- sortord2 %>% mutate(ORD1=3L, ORD2=2L)
sortord3_2 <- sortord3 %>% mutate(ORD1=4L, ORD2=3L)
alls <- bind_rows(sortord1_2, sortord2_2, sortord3_2) %>%
  arrange(STUDYID, ORD1, ORD2)

alls_sorted <- alls %>% arrange(ORD1, ORD2, desc(COUNT))

alls2 <- alls_sorted %>%
  group_by(ORD1, ORD2) %>%
  mutate(ORD3 = row_number()) %>%
  ungroup() %>%
  select(STUDYID, DCREASCD, COUNT, ORD1, ORD2, ORD3)

pop <- adsl %>%
  group_by(STUDYID, TRT01PN, TRT01P) %>%
  summarise(TOT = n(), .groups="drop")

allp <- all_df %>%
  left_join(pop, by = c("STUDYID","TRT01PN","TRT01P")) %>%
  mutate(
    COUNTC = ifelse(!is.na(COUNT),
                    paste0(sas_put_4(COUNT), " (", sas_put_5_1(COUNT/TOT), ")"),
                    NA_character_),
    DCREASCD = case_when(
      ORD1 == 1 & DISCONFL == "Y" ~ "Discontinued",
      ORD1 == 1 & !is.na(DISCONFL) ~ "Completed the Study",
      TRUE ~ DCREASCD
    )
  )

allp_sorted <- allp %>% arrange(STUDYID, ORD1, ORD2, DCREASCD, TRT01PN)

allpt <- allp_sorted %>%
  select(STUDYID, ORD1, ORD2, DCREASCD, TRT01PN, COUNTC) %>%
  pivot_wider(names_from = TRT01PN, values_from = COUNTC, names_prefix = "TRT") %>%
  arrange(STUDYID, ORD1, ORD2, DCREASCD)

pop_distinct <- pop %>% arrange(TRT01PN) %>% distinct(TRT01PN, TRT01P, TOT)
maxt <- nrow(pop_distinct)

trt_labels <- pop_distinct %>%
  mutate(TRTNUM = TRT01PN) %>%
  arrange(TRTNUM)

trt_cols <- paste0("TRT", sort(trt_labels$TRTNUM))
for (tc in trt_cols) if (!tc %in% names(allpt)) allpt[[tc]] <- NA_character_

allpt2 <- allpt
for (tc in trt_cols) {
  allpt2[[tc]] <- ifelse(is.na(allpt2[[tc]]) | allpt2[[tc]] == "",
                         "###0",
                         gsub(" ", "#", allpt2[[tc]], fixed=TRUE))
}

allpt3 <- allpt2 %>%
  left_join(alls2 %>% select(STUDYID, ORD1, ORD2, DCREASCD, ORD3),
            by = c("STUDYID","ORD1","ORD2","DCREASCD")) %>%
  mutate(
    ORD3 = case_when(
      ORD1 == 1 & toupper(DCREASCD) == "DISCONTINUED" ~ 2L,
      ORD1 == 1 ~ 1L,
      TRUE ~ ORD3
    )
  ) %>%
  select(STUDYID, DCREASCD, ORD1, ORD2, ORD3, all_of(trt_cols)) %>%
  arrange(STUDYID, ORD1, ORD2, ORD3)

final <- allpt3 %>%
  mutate(
    PAGEX = 1L,
    COL1 = case_when(
      ORD1 == 1 ~ trimws(DCREASCD),
      ORD1 == 3 ~ paste0("    ", trimws(DCREASCD)),
      ORD1 == 2 ~ paste0("    ", trimws(DCREASCD)),
      ORD1 == 4 ~ paste0("    ", trimws(DCREASCD)),
      TRUE ~ trimws(DCREASCD)
    )
  ) %>%
  arrange(STUDYID, ORD1, ORD2, ORD3)

final_disp <- final %>%
  mutate(across(all_of(trt_cols), ~gsub("#", " ", ., fixed=TRUE)))

header_labels <- setNames(
  paste0(trimws(trt_labels$TRT01P), " (N=", trt_labels$TOT, ")"),
  paste0("TRT", trt_labels$TRTNUM)
)

out_table <- final_disp %>%
  select(`Subject Disposition` = COL1, all_of(trt_cols))

for (tc in trt_cols) {
  names(out_table)[names(out_table)==tc] <- header_labels[[tc]]
}

titles <- c(
  "Table X: Dispostion Table",
  "",
  "Subject Disposition"
)
footnotes <- c(
  "Abbreviations: N=total number of subjects in population.",
  "               n=number of subjects in the specified category",
  "               %=Percentage of patients with N as denomiator",
  "",
  "",
  "Program Location: blah lah blah"
)

col_width_subj <- 40
col_width_trt <- 18

hdr_line1 <- sprintf(paste0("%-", col_width_subj, "s"), "Subject Disposition")
hdr_line2 <- sprintf(paste0("%-", col_width_subj, "s"), "")
for (tc in trt_cols) {
  lab <- header_labels[[tc]]
  hdr_line1 <- paste0(hdr_line1, sprintf(paste0("%-", col_width_trt, "s"), lab))
  hdr_line2 <- paste0(hdr_line2, sprintf(paste0("%-", col_width_trt, "s"), "  n   %"))
}
sep_line <- paste(rep("-", nchar(hdr_line1)), collapse="")

group_header_map <- c("1"="Death or Adverse Event",
                      "2"="Lack of Efficacy-Related Reasons",
                      "3"="Other Reasons")

body_lines <- c()
current_ord2 <- NA
for (i in seq_len(nrow(final_disp))) {
  row <- final_disp[i,]
  ord2 <- row$ORD2
  ord1 <- row$ORD1
  need_header <- FALSE
  if (is.na(current_ord2) || ord2 != current_ord2) {
    if (!is.na(ord2) && ord2 %in% c(1,2,3) && ord1 != 1) {
      need_header <- TRUE
    }
    current_ord2 <- ord2
  }
  if (need_header) {
    gh <- group_header_map[as.character(ord2)]
    body_lines <- c(body_lines, "")
    body_lines <- c(body_lines, sprintf(paste0("%-", col_width_subj, "s"), paste0("  ", gh)))
  }
  col1_disp <- row$COL1
  if (is.na(col1_disp)) col1_disp <- row$DCREASCD
  if (is.na(col1_disp)) col1_disp <- ""
  line <- sprintf(paste0("%-", col_width_subj, "s"), col1_disp)
  for (tc in trt_cols) {
    val <- row[[tc]]
    if (is.na(val) || val=="") val <- "    0"
    line <- paste0(line, sprintf(paste0("%-", col_width_trt, "s"), val))
  }
  body_lines <- c(body_lines, line)
}

text_content <- c(
  titles,
  "",
  hdr_line1,
  hdr_line2,
  sep_line,
  body_lines,
  sep_line,
  "",
  footnotes
)

write.csv(out_table, file.path(out, "P048.csv"), row.names=FALSE, na="")
write.csv(out_table, file.path(out, "sasrtf.csv"), row.names=FALSE, na="")
write.csv(final_disp, file.path(out, "P048_dataset.csv"), row.names=FALSE, na="")

writeLines(text_content, file.path(out, "P048.txt"))
writeLines(text_content, file.path(out, "sasrtf.txt"))
writeLines(text_content, file.path(out, "P048.lst"))
writeLines(text_content, file.path(out, "sasrtf.rtf"))

library(ggplot2)
plot_data <- pop %>% mutate(TRT = TRT01P, N = TOT)
p <- ggplot(plot_data, aes(x=TRT, y=N, fill=TRT)) + geom_col() + labs(title="Subject Disposition - Population N", x="Treatment", y="N") + theme_minimal()
ggsave(file.path(out, "P048.png"), plot=p, width=8, height=5, dpi=150)
write.csv(plot_data, file.path(out, "P048_plot_data.csv"), row.names=FALSE, na="")
```