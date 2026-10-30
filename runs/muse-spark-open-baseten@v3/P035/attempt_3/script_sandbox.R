out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- Sys.getenv("outdir")
if (out == "" || is.na(out)) out <- "."
dir.create(out, recursive = TRUE, showWarnings = FALSE)

sas_round <- function(x, digits = 0) {
  ifelse(is.na(x), NA_real_, sign(x) * floor(abs(x) * 10^digits + 0.5) / 10^digits)
}
sas_put <- function(x, w, d = 0) {
  if (length(x) == 0) return(character(0))
  vapply(seq_along(x), function(i) {
    xi <- x[i]
    if (is.na(xi) || is.nan(xi) || is.infinite(xi)) return(strrep(" ", w))
    xr <- sas_round(xi, d)
    if (d == 0) s <- sprintf("%.0f", xr) else s <- sprintf(paste0("%.", d, "f"), xr)
    if (nchar(s, type="bytes") < w) s <- paste0(strrep(" ", w - nchar(s, type="bytes")), s)
    if (nchar(s, type="bytes") > w) s <- substr(s, nchar(s, type="bytes")-w+1, nchar(s, type="bytes"))
    s
  }, character(1))
}

compbl <- function(s) gsub("\\s+", " ", trimws(s))
strip <- function(s) trimws(s)

find_first_file <- function(candidates){
  for(p in candidates) if(!is.na(p) && p!="" && file.exists(p)) return(p)
  NA_character_
}

candidates <- c(
  "/bench/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/raw/cdisc-pilot/adam/ADPP.xpt",
  file.path(Sys.getenv("rawadam"), "adpp.xpt"),
  file.path(Sys.getenv("rawadam"), "ADPP.xpt"),
  file.path(Sys.getenv("root"), "data/raw/cdisc-pilot/adam/adpp.xpt"),
  file.path(Sys.getenv("root"), "data/derived/cdisc-pilot/adam/adpp.xpt"),
  "data/raw/cdisc-pilot/adam/adpp.xpt",
  "data/derived/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/raw/cdisc-pilot/adam/adpp.xpt",
  "/bench/data/derived/cdisc-pilot/adam/adpp.xpt"
)
try({
  ff <- list.files("/bench", pattern="^adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if(length(ff)>0) candidates <- c(candidates, ff[1])
}, silent=TRUE)
try({
  ff2 <- list.files(".", pattern="^adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if(length(ff2)>0) candidates <- c(candidates, ff2[1])
}, silent=TRUE)
try({
  ff3 <- list.files("/bench", pattern="^adpp\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
  if(length(ff3)>0) candidates <- c(candidates, ff3[1])
}, silent=TRUE)

adpp_path <- find_first_file(candidates)
if(is.na(adpp_path)){
  try({
    all_xpt <- list.files("/", pattern="\\.xpt$", recursive=TRUE, full.names=TRUE, ignore.case=TRUE)
    idx <- grep("adpp\\.xpt$", all_xpt, ignore.case=TRUE)
    if(length(idx)>0) adpp_path <- all_xpt[idx[1]]
  }, silent=TRUE)
}
if(is.na(adpp_path) || !file.exists(adpp_path)) stop(paste0("adpp.xpt not found. Tried: ", paste(candidates,collapse="; ")))
message("Reading ", adpp_path)

library(haven)
library(dplyr)
library(tidyr)
library(stringr)

adpp_raw <- haven::read_xpt(adpp_path)
names(adpp_raw) <- toupper(names(adpp_raw))
adpp_raw <- adpp_raw %>% mutate(across(everything(), ~ if(inherits(.x,"haven_labelled")) haven::zap_labels(.x) else .x))
for(nm in names(adpp_raw)) if(is.factor(adpp_raw[[nm]])) adpp_raw[[nm]] <- as.character(adpp_raw[[nm]])

param_col <- if("PARAMCD" %in% names(adpp_raw)) "PARAMCD" else if("PPTESTCD" %in% names(adpp_raw)) "PPTESTCD" else NA
if(is.na(param_col)) stop("No PARAMCD/PPTESTCD column found")

target_params <- c('AUCINFO','AUCLAST','CMAX','TMAX','MRTLAST','LAMZHL')

to_num <- function(x) suppressWarnings(as.numeric(as.character(x)))

adpp_filt <- adpp_raw
if("PKFN" %in% names(adpp_filt)) adpp_filt$PKFN_num <- to_num(adpp_filt$PKFN) else adpp_filt$PKFN_num <- 1
if("ANL01FN" %in% names(adpp_filt)) adpp_filt$ANL01FN_num <- to_num(adpp_filt$ANL01FN) else adpp_filt$ANL01FN_num <- NA_real_
if("PPSTRESN" %in% names(adpp_filt)) adpp_filt$PPSTRESN_num <- to_num(adpp_filt$PPSTRESN) else adpp_filt$PPSTRESN_num <- NA_real_

adpp_filt <- adpp_filt %>%
  mutate(
    .param_up = toupper(trimws(as.character(.data[[param_col]]))),
    .ppcatl_comp = toupper(gsub("[[:space:]]+", "", as.character(PPCATL))),
    .keep_pkfn = !is.na(PKFN_num) & PKFN_num==1,
    .keep_anl = is.na(ANL01FN_num) | ANL01FN_num != 1,
    .keep_param = .param_up %in% target_params,
    .keep_ppres = !is.na(PPSTRESN_num),
    .keep_catl = !is.na(.ppcatl_comp) & .ppcatl_comp=="ANAL2"
  ) %>%
  filter(.keep_pkfn, .keep_anl, .keep_param, .keep_ppres, .keep_catl)

message(paste0("Rows after filter: ", nrow(adpp_filt)))

pk_fmt <- c(
  "AUCINFO"="AUC","AUCTAU"="AUCtau","AUCLAST"="AUC(0-tlast)","CMAX"="Cmax","CMAXD"="Cmax/Dose",
  "MRTLAST"="MRT","TLAST"="tlast","TMAX"="tmax","CLRFO"="CL/F","VZFO"="VzF","LAMZ"="lambdaZ","LAMZHL"="t1/2","CLAST"="Clast"
)
adpp_filt <- adpp_filt %>%
  mutate(
    PPTESTCD_U = toupper(trimws(as.character(PPTESTCD))),
    PK_LABEL = ifelse(PPTESTCD_U %in% names(pk_fmt), pk_fmt[PPTESTCD_U], PPTESTCD_U),
    PPORRESU_S = strip(as.character(PPORRESU)),
    `_PARAM` = compbl(paste0(PK_LABEL, " (", PPORRESU_S, ")")),
    AVAL_num = to_num(AVAL),
    AVAL_log = ifelse(!is.na(AVAL_num) & AVAL_num>0, log(AVAL_num), NA_real_)
  )

adpp_filt <- adpp_filt %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`)

adpp_sum <- adpp_filt %>%
  group_by(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, `_PARAM`) %>%
  summarise(
    n1 = sum(!is.na(AVAL_num)),
    n2 = sum(!is.na(AVAL_log)),
    mn1 = ifelse(n1>0, mean(AVAL_num, na.rm=TRUE), NA_real_),
    mn2 = ifelse(n2>0, mean(AVAL_log, na.rm=TRUE), NA_real_),
    std1 = ifelse(n1>1, sd(AVAL_num, na.rm=TRUE), NA_real_),
    std2 = ifelse(n2>1, sd(AVAL_log, na.rm=TRUE), NA_real_),
    cv1 = ifelse(!is.na(mn1) && mn1!=0 && !is.na(std1), std1/mn1*100, NA_real_),
    cv2 = ifelse(!is.na(mn2) && mn2!=0 && !is.na(std2), std2/mn2*100, NA_real_),
    med1 = ifelse(n1>0, median(AVAL_num, na.rm=TRUE), NA_real_),
    med2 = ifelse(n2>0, median(AVAL_log, na.rm=TRUE), NA_real_),
    min1 = ifelse(n1>0, min(AVAL_num, na.rm=TRUE), NA_real_),
    min2 = ifelse(n2>0, min(AVAL_log, na.rm=TRUE), NA_real_),
    max1 = ifelse(n1>0, max(AVAL_num, na.rm=TRUE), NA_real_),
    max2 = ifelse(n2>0, max(AVAL_log, na.rm=TRUE), NA_real_),
    .groups="drop"
  )

make_rows <- function(df){
  # df is 1-row data.frame/tibble
  n1 <- df$n1; n2 <- df$n2; mn1 <- df$mn1; mn2 <- df$mn2; std1 <- df$std1; std2 <- df$std2
  cv1 <- df$cv1; med1 <- df$med1; min1 <- df$min1; max1 <- df$max1; param <- df$`_PARAM`
  v1 <- sas_put(n1,2)
  v2 <- " "
  if(grepl("AUC", param, fixed=TRUE)) v2 <- paste0(sas_put(mn1,6,0)," (",sas_put(std1,5,0),")")
  else if(grepl("Cmax", param, fixed=TRUE)) v2 <- paste0(sas_put(mn1,5,0)," (",sas_put(std1,4,0),")")
  else if(grepl("MRT", param, fixed=TRUE)) v2 <- paste0(sas_put(mn1,4,1)," (",sas_put(std1,4,2),")")
  else if(grepl("t1/2", param, fixed=TRUE)) v2 <- paste0(sas_put(mn1,4,2)," (",sas_put(std1,4,2),")")
  else v2 <- " "
  v3 <- if(!grepl("tmax", param, fixed=TRUE)) sas_put(cv1,4,1) else " "
  v4 <- " "
  if(grepl("AUC", param, fixed=TRUE)) v4 <- sas_put(exp(mn2),6,0)
  else if(grepl("Cmax", param, fixed=TRUE)) v4 <- sas_put(exp(mn2),5,0)
  else if(grepl("MRT", param, fixed=TRUE)) v4 <- sas_put(exp(mn2),4,1)
  else if(grepl("t1/2", param, fixed=TRUE)) v4 <- sas_put(exp(mn2),4,2)
  else v4 <- " "
  cv_geo <- sqrt(exp(std2^2)-1)*100
  v5 <- if(!grepl("tmax", param, fixed=TRUE)) sas_put(cv_geo,4,1) else " "
  v6 <- " "
  if(grepl("AUC", param, fixed=TRUE)) v6 <- sas_put(med1,6,0)
  else if(grepl("Cmax", param, fixed=TRUE)) v6 <- sas_put(med1,5,0)
  else if(grepl("MRT", param, fixed=TRUE)) v6 <- sas_put(med1,4,1)
  else if(grepl("t1/2", param, fixed=TRUE)) v6 <- sas_put(med1,4,2)
  else if(grepl("tmax", param, fixed=TRUE)) v6 <- sas_put(med1,4,2)
  v7 <- " "
  if(grepl("AUC", param, fixed=TRUE)) v7 <- paste0("[",sas_put(min1,6,0),";",sas_put(max1,6,0),"]")
  else if(grepl("Cmax", param, fixed=TRUE)) v7 <- paste0("[",sas_put(min1,4,0),";",sas_put(max1,5,0),"]")
  else if(grepl("MRT", param, fixed=TRUE)) v7 <- paste0("[",sas_put(min1,4,2),";",sas_put(max1,4,1),"]")
  else if(grepl("t1/2", param, fixed=TRUE)) v7 <- paste0("[",sas_put(min1,4,2),";",sas_put(max1,4,1),"]")
  else if(grepl("tmax", param, fixed=TRUE)) v7 <- paste0("[",sas_put(min1,4,2),";",sas_put(max1,4,2),"]")
  data.frame(s_order=c(1,2,3,4,5,6,7), value=c(v1,v2,v3,v4,v5,v6,v7), stringsAsFactors=FALSE)
}

if(nrow(adpp_sum)>0){
  adpp_sum_t_list <- lapply(seq_len(nrow(adpp_sum)), function(i){
    row <- adpp_sum[i, , drop=FALSE]
    tmp <- make_rows(row)
    # replicate keys for 7 rows
    keys <- row[, c("ANLPRNT","PPSPECL","PPCATL","TRTAN","PPSDY","_PARAM"), drop=FALSE]
    keys_rep <- keys[rep(1, nrow(tmp)), , drop=FALSE]
    cbind(keys_rep, tmp)
  })
  adpp_sum_t <- do.call(rbind, adpp_sum_t_list)
  adpp_sum_t <- adpp_sum_t %>% arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order, `_PARAM`)
} else {
  adpp_sum_t <- data.frame(ANLPRNT=character(), PPSPECL=character(), PPCATL=character(), TRTAN=numeric(), PPSDY=numeric(), `_PARAM`=character(), s_order=numeric(), value=character(), check.names=FALSE)
}

trtan_fmt <- c("1"="Period 1 Treatment","2"="Period 2 Treatment","3"="Period 3 Treatment")
statg_fmt <- c("1"="n","2"="Mean (SD)","3"="CV% mean","4"="Geo-mean","5"="CV% geo-mean","6"="Median","7"="[Min; Max]        ")
adpp_sum_t <- adpp_sum_t %>%
  mutate(
    TRTAN_label = ifelse(as.character(TRTAN) %in% names(trtan_fmt), trtan_fmt[as.character(TRTAN)], as.character(TRTAN)),
    stat_label = ifelse(as.character(s_order) %in% names(statg_fmt), statg_fmt[as.character(s_order)], as.character(s_order))
  )

param_levels <- sort(unique(adpp_sum_t$`_PARAM`))
if(length(param_levels)==0) param_levels <- character(0)

wide_tbl <- adpp_sum_t %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, TRTAN_label, PPSDY, s_order, stat_label, `_PARAM`, value) %>%
  tidyr::pivot_wider(names_from=`_PARAM`, values_from=value, values_fill=" ") %>%
  arrange(ANLPRNT, PPSPECL, PPCATL, TRTAN, PPSDY, s_order)

titles <- c("Study001","Table 14.2-PPTS (Page xxx of xxx)","Summary statistics for PK parameters","by compound, matrix, analyte and actual treatment","Analysis Set: PK analysis set")
footnotes <- c("CV% = coefficient of variation (%) = sd/mean*100","CV% geo-mean = (sqrt(exp(variance for log transformed data)-1))*100","Geo-mean: Geometric mean","Geo-mean and CV% geo-mean not presented when the minimum value for a parameter is zero.","Data: adpp  Program: pps.sas  Output: t.14.2.ppts.txt","Fake Data/ Production Run on 17MAR2014:23:57")

pad_right <- function(s,w){
  s <- as.character(s); s[is.na(s)] <- ""
  s <- ifelse(nchar(s)>w, substr(s,1,w), s)
  sprintf(paste0("%-",w,"s"), s)
}

lines <- c()
for(t in titles) lines <- c(lines, t)
lines <- c(lines, "")

by_groups <- wide_tbl %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)
if(nrow(by_groups)==0 && nrow(adpp_filt)>0){
  by_groups <- adpp_filt %>% distinct(ANLPRNT, PPSPECL, PPCATL) %>% arrange(ANLPRNT, PPSPECL, PPCATL)
}

for(bg_i in seq_len(nrow(by_groups))){
  bg <- by_groups[bg_i,]
  comp_line <- paste0("Compound: ", sprintf("%-10s", bg$ANLPRNT), " , Matrix: ", sprintf("%-10s", bg$PPSPECL), " , Analyte: ", sprintf("%-15s", bg$PPCATL))
  lines <- c(lines, comp_line, "")
  sub <- wide_tbl %>% filter(ANLPRNT==bg$ANLPRNT, PPSPECL==bg$PPSPECL, PPCATL==bg$PPCATL) %>% arrange(TRTAN, PPSDY, s_order)
  if(nrow(sub)==0) next
  hdr1 <- paste0(pad_right("Actual treatment",20)," ",pad_right("Period day",8)," ",pad_right("Statistic",14)," ",paste(sapply(param_levels, function(p) pad_right(p,19)), collapse=" "))
  hdr2 <- paste0(strrep("-",20)," ",strrep("-",8)," ",strrep("-",14)," ",paste(sapply(param_levels, function(p) strrep("-",19)), collapse=" "))
  lines <- c(lines, hdr1, hdr2)
  for(r in seq_len(nrow(sub))){
    row <- sub[r,]
    trt <- pad_right(row$TRTAN_label,20)
    ppsdy <- pad_right(as.character(row$PPSDY),8)
    stat <- pad_right(row$stat_label,14)
    vals <- sapply(param_levels, function(p){ v <- row[[p]]; if(is.null(v)||is.na(v)) v<-""; pad_right(v,19) })
    line <- paste0(trt," ",ppsdy," ",stat," ",paste(vals, collapse=" "))
    lines <- c(lines, line)
    if(r < nrow(sub)){
      nxt <- sub[r+1,]
      if(nxt$PPSDY != row$PPSDY || nxt$TRTAN != row$TRTAN) lines <- c(lines, "")
    }
  }
  lines <- c(lines, "")
}
for(f in footnotes) lines <- c(lines, f)

writeLines(lines, file.path(out, "P035.txt"))
writeLines(lines, file.path(out, "P035.lst.txt"))
writeLines(lines, file.path(out, "P035.lst"))
writeLines(lines, file.path(out, "P035_table.txt"))

csv_wide <- wide_tbl %>%
  mutate(Actual_treatment=TRTAN_label, Period_day=PPSDY, Statistic=stat_label) %>%
  select(ANLPRNT, PPSPECL, PPCATL, TRTAN, Actual_treatment, Period_day, s_order, Statistic, dplyr::all_of(param_levels))

write.csv(csv_wide, file.path(out, "P035.csv"), row.names=FALSE, na="")
write.csv(csv_wide, file.path(out, "P035.lst.csv"), row.names=FALSE, na="")
write.csv(adpp_sum_t, file.path(out, "P035_dataset.csv"), row.names=FALSE, na="")
write.csv(adpp_sum, file.path(out, "P035_adpp_sum.csv"), row.names=FALSE, na="")
write.csv(wide_tbl, file.path(out, "CLXCHECKNOBS.csv"), row.names=FALSE, na="")

message("Done. Outputs written to ", out)
