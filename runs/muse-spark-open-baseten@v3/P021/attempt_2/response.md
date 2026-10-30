```r
suppressPackageStartupMessages({
  library(haven)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
  library(data.table)
})

out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("OUTDIR")
if (out == "" || is.na(out)) out <- "."
if (!dir.exists(out)) dir.create(out, recursive=TRUE, showWarnings=FALSE)

project_path <- Sys.getenv("project_path")
if (is.na(project_path) || project_path=="") project_path <- Sys.getenv("PROJECT_PATH")
if (is.na(project_path) || project_path=="") project_path <- "D:/meta_hackathon/sas2r-agent/data/derived/c2c"
root <- Sys.getenv("root")
if ((is.na(project_path) || project_path=="") && !is.na(root) && root!="") project_path <- file.path(root,"data/derived/c2c")

rawadam <- Sys.getenv("rawadam")
if (is.na(rawadam) || rawadam=="") rawadam <- "D:/meta_hackathon/sas2r-agent/data/raw/cdisc-pilot/adam"
adam_dir <- file.path(project_path,"data","adam")
candidate_dirs <- c(adam_dir,
                    "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam",
                    rawadam,
                    if(!is.na(root) && root!="") file.path(root,"data/derived/c2c/data/adam") else NA_character_)

round_half_up <- function(x, digits=0){
  ifelse(is.na(x), NA_real_, sign(x)*floor(abs(x)*10^digits+0.5)/10^digits)
}
fmt_num <- function(x, digits, width=NULL){
  if(is.na(x)) return(NA_character_)
  x_r <- round_half_up(x, digits)
  fmt <- paste0("%", if(!is.null(width)) width else "", ".", digits, "f")
  sprintf(fmt, x_r)
}
fmt_int <- function(x, width=5){
  if(is.na(x)) return(NA_character_)
  sprintf(paste0("%",width,".0f"), round_half_up(x,0))
}

find_adsl <- function(dirs){
  for(d in dirs){
    if(is.na(d) || d=="") next
    if(!dir.exists(d)) next
    f <- list.files(d, pattern="^adsl\\.(sas7bdat|xpt)$", ignore.case=TRUE, full.names=TRUE)
    if(length(f)>0) return(f[1])
    f2 <- list.files(d, pattern="^adsl", ignore.case=TRUE, full.names=TRUE)
    if(length(f2)>0){
      pref <- f2[grepl("\\.(sas7bdat|xpt)$", f2, ignore.case=TRUE)]
      if(length(pref)>0) return(pref[1])
      return(f2[1])
    }
  }
  return(NA_character_)
}
adsl_file <- find_adsl(candidate_dirs)
if(is.na(adsl_file)){
  try_paths <- c(file.path(adam_dir,"adsl.sas7bdat"),
                 file.path(adam_dir,"ADSL.sas7bdat"),
                 file.path(adam_dir,"adsl.xpt"),
                 "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.sas7bdat",
                 "D:/meta_hackathon/sas2r-agent/data/derived/c2c/data/adam/adsl.xpt")
  for(p in try_paths) if(file.exists(p)){adsl_file<-p;break}
}
if(is.na(adsl_file) || !file.exists(adsl_file)) stop(paste0("ADSL not found. Searched: ", paste(candidate_dirs,collapse="; ")))

ext <- tolower(tools::file_ext(adsl_file))
if(ext=="sas7bdat"){
  adsl_raw <- haven::read_sas(adsl_file)
} else if(ext=="xpt"){
  adsl_raw <- haven::read_xpt(adsl_file)
} else if(ext=="csv"){
  adsl_raw <- readr::read_csv(adsl_file, show_col_types=FALSE)
} else {
  adsl_raw <- haven::read_sas(adsl_file)
}
names(adsl_raw) <- toupper(names(adsl_raw))

# zap labels/formats safely without is_labelled
adsl <- haven::zap_formats(haven::zap_labels(adsl_raw))
# ensure character columns are character
adsl <- adsl %>% mutate(across(everything(), ~ if(inherits(.x,"haven_labelled")) as.character(haven::as_factor(.x)) else .x))

stopifnot("ITTFL" %in% names(adsl))
stopifnot("TRT01P" %in% names(adsl))
stopifnot("SEX" %in% names(adsl))
stopifnot("RACE" %in% names(adsl))
stopifnot("AGE" %in% names(adsl))

adsl_itt <- adsl %>% filter(ITTFL=="Y") %>%
  mutate(TRT01P=as.character(TRT01P),
         SEX=as.character(SEX),
         RACE=as.character(RACE),
         AGE=as.numeric(AGE))

# TRT levels sorted ASCII (SAS)
trt_levels <- sort(unique(adsl_itt$TRT01P[!is.na(adsl_itt$TRT01P)]))
has_trt_missing <- any(is.na(adsl_itt$TRT01P))
if(has_trt_missing) trt_levels <- c(trt_levels, NA_character_)

col_totals <- sapply(trt_levels, function(lv){
  if(is.na(lv)) sum(is.na(adsl_itt$TRT01P)) else sum(adsl_itt$TRT01P==lv, na.rm=TRUE)
})
names(col_totals) <- trt_levels
total_all <- nrow(adsl_itt)

col_names_display <- c(trt_levels, "Total")
col_names_display[is.na(col_names_display)] <- "Missing"

sex_levels <- unique(adsl_itt$SEX)
sex_has_na <- any(is.na(sex_levels))
sex_levels <- sort(na.omit(sex_levels))
if(sex_has_na) sex_levels <- c(NA_character_, sex_levels)

race_levels <- unique(adsl_itt$RACE)
race_has_na <- any(is.na(race_levels))
race_levels <- sort(na.omit(race_levels))
if(race_has_na) race_levels <- c(NA_character_, race_levels)

compute_class_stats <- function(var_name, levels_vec){
  res <- list()
  for(lv in levels_vec){
    n_per_col <- sapply(trt_levels, function(tv){
      if(is.na(lv) && is.na(tv)){
        sum(is.na(adsl_itt[[var_name]]) & is.na(adsl_itt$TRT01P))
      } else if(is.na(lv)){
        if(is.na(tv)) sum(is.na(adsl_itt[[var_name]]) & is.na(adsl_itt$TRT01P))
        else sum(is.na(adsl_itt[[var_name]]) & adsl_itt$TRT01P==tv, na.rm=TRUE)
      } else if(is.na(tv)){
        sum(adsl_itt[[var_name]]==lv & is.na(adsl_itt$TRT01P), na.rm=TRUE)
      } else {
        sum(adsl_itt[[var_name]]==lv & adsl_itt$TRT01P==tv, na.rm=TRUE)
      }
    })
    n_total <- if(is.na(lv)) sum(is.na(adsl_itt[[var_name]])) else sum(adsl_itt[[var_name]]==lv, na.rm=TRUE)
    pct_per_col <- ifelse(col_totals==0, 0, n_per_col/col_totals*100)
    pct_total <- ifelse(total_all==0, 0, n_total/total_all*100)
    key <- ifelse(is.na(lv),"Missing",lv)
    res[[key]] <- list(n=c(n_per_col, n_total), pct=c(pct_per_col, pct_total))
  }
  res
}
sex_stats <- compute_class_stats("SEX", sex_levels)
race_stats <- compute_class_stats("RACE", race_levels)

age_stats <- list()
for(tv in trt_levels){
  vals <- if(is.na(tv)) adsl_itt$AGE[is.na(adsl_itt$TRT01P)] else adsl_itt$AGE[adsl_itt$TRT01P==tv & !is.na(adsl_itt$TRT01P)]
  vals <- vals[!is.na(vals)]
  age_stats[[ifelse(is.na(tv),"Missing",tv)]] <- vals
}
age_stats[["Total"]] <- adsl_itt$AGE[!is.na(adsl_itt$AGE)]

compute_age_summary <- function(vals){
  n <- length(vals)
  if(n==0) return(list(N=0, Mean=NA_real_, Std=NA_real_, Min=NA_real_, Max=NA_real_))
  m <- mean(vals)
  s <- if(n>1) sd(vals) else NA_real_
  list(N=n, Mean=m, Std=s, Min=min(vals), Max=max(vals))
}
age_summaries <- lapply(age_stats, compute_age_summary)

csv_col_headers <- c("Demographic_Parameter","Category","Statistic", col_names_display)
rows_list <- list()

total_n_per_col <- c(col_totals, total_all)
names(total_n_per_col) <- col_names_display
rows_list[[length(rows_list)+1]] <- tibble(
  Demographic_Parameter="Total Subjects",
  Category="Total",
  Statistic="N",
  !!!as.list(setNames(as.integer(total_n_per_col[col_names_display]), col_names_display))
)

for(lv in names(sex_stats)){
  st <- sex_stats[[lv]]
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter="Sex", Category=lv, Statistic="n",
    !!!as.list(setNames(as.integer(st$n), col_names_display))
  )
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter="Sex", Category=lv, Statistic="%",
    !!!as.list(setNames(round_half_up(st$pct,1), col_names_display))
  )
}
for(lv in names(race_stats)){
  st <- race_stats[[lv]]
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter="Race", Category=lv, Statistic="n",
    !!!as.list(setNames(as.integer(st$n), col_names_display))
  )
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter="Race", Category=lv, Statistic="%",
    !!!as.list(setNames(round_half_up(st$pct,1), col_names_display))
  )
}
age_row_labels <- c("n","Mean","SD","Min","Max")
for(lab in age_row_labels){
  vals <- sapply(col_names_display, function(cn){
    summ <- age_summaries[[cn]]
    if(is.null(summ)) return(NA_real_)
    switch(lab, "n"=summ$N, "Mean"=summ$Mean, "SD"=summ$Std, "Min"=summ$Min, "Max"=summ$Max)
  })
  rows_list[[length(rows_list)+1]] <- tibble(
    Demographic_Parameter="Age (years)", Category="", Statistic=lab,
    !!!as.list(setNames(vals, col_names_display))
  )
}
table_body <- bind_rows(rows_list)

# write CSV dataset
write.csv(table_body, file.path(out,"P021.csv"), row.names=FALSE, na="")

# formatted display for text
format_for_display <- function(df){
  df_disp <- df
  for(cn in col_names_display){
    df_disp[[cn]] <- sapply(seq_len(nrow(df_disp)), function(i){
      stat <- df_disp$Statistic[i]
      param <- df_disp$Demographic_Parameter[i]
      val <- df[[cn]][i]
      if(is.na(val)) return("0")
      if(param=="Total Subjects" && stat=="N") fmt_int(val,5)
      else if(param %in% c("Sex","Race") && stat=="n") fmt_int(val,5)
      else if(param %in% c("Sex","Race") && stat=="%") fmt_num(val,1,5)
      else if(param=="Age (years)"){
        if(stat=="n") fmt_int(val,5)
        else if(stat=="Mean") fmt_num(val,1,5)
        else if(stat=="SD") fmt_num(val,2,5)
        else if(stat %in% c("Min","Max")) fmt_int(val,5)
        else as.character(val)
      } else as.character(val)
    })
  }
  df_disp
}
table_disp <- format_for_display(table_body)

# Build text rendering mimicking PROC TABULATE
title1 <- "Table 1: Demographics and Baseline Characteristics"
title2 <- "Population: Intent-to-Treat (ITT)"
box_label <- "Demographic Parameter"

param_w <- 25
cat_w <- 30
stat_w <- 10
col_w <- 12

# header with Planned Treatment grouping
group_label <- "Planned Treatment"
n_trt_cols <- length(col_names_display)-1
group_width <- n_trt_cols*col_w
group_line <- sprintf(paste0("%-",param_w,"s %-",cat_w,"s %-",stat_w,"s"), "", "", "")
group_line <- paste0(group_line, sprintf(paste0("%",group_width,"s"), group_label), sprintf(paste0("%",col_w,"s"), ""))

col_header <- sprintf(paste0("%-",param_w,"s %-",cat_w,"s %-",stat_w,"s"), box_label, "", "")
for(cn in col_names_display){
  col_header <- paste0(col_header, sprintf(paste0("%",col_w,"s"), cn))
}
sep_line <- paste(rep("-", param_w+1+cat_w+1+stat_w+1+length(col_names_display)*col_w), collapse="")

row_lines <- c()
for(i in seq_len(nrow(table_disp))){
  r <- table_disp[i,]
  line <- sprintf(paste0("%-",param_w,"s %-",cat_w,"s %-",stat_w,"s"),
                  substr(r$Demographic_Parameter,1,param_w),
                  substr(r$Category,1,cat_w),
                  substr(r$Statistic,1,stat_w))
  for(cn in col_names_display){
    line <- paste0(line, sprintf(paste0("%",col_w,"s"), r[[cn]]))
  }
  row_lines <- c(row_lines, line)
}

txt_content <- c(
  title1,
  title2,
  "",
  group_line,
  col_header,
  sep_line,
  row_lines,
  sep_line,
  "",
  "Note: Percentages are column percentages (ColPctN). N for Age is number of non-missing values.",
  "Population: Intent-to-Treat (ITT) where ITTFL='Y'. Missing categories displayed as 0 per misstext option.",
  ""
)

# write required outputs - ensure at least one .txt and one .csv in OUT_DIR
writeLines(txt_content, file.path(out,"P021.txt"))
writeLines(txt_content, file.path(out,"P021.lst"))
writeLines(txt_content, file.path(out,"P021.lst.txt"))
# also write CSV of table body already done as P021.csv

cat("P021 Table 1 generated\n")
cat(paste0("ADSL file: ", adsl_file, "\n"))
cat(paste0("ITT N: ", nrow(adsl_itt), "\n"))
cat(paste0("TRT levels: ", paste(col_names_display, collapse=", "), "\n"))
cat(paste0("Output written to: ", out, "\n"))
```