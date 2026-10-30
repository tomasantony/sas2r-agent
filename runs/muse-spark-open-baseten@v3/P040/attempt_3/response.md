```r
out <- Sys.getenv("OUT_DIR")
if (out == "" || is.na(out)) out <- getwd()
if (!dir.exists(out)) dir.create(out, recursive = TRUE, showWarnings = FALSE)

round_half_up <- function(x, digits = 0) {
  if (length(x) == 0) return(x)
  mult <- 10^digits
  sign_x <- sign(x)
  y <- abs(x) * mult
  y <- floor(y + 0.5 + 1e-12)
  sign_x * y / mult
}
fmt4 <- function(x) sprintf("%.4f", round_half_up(x,4))
fmt3 <- function(x) sprintf("%.3f", round_half_up(x,3))
fmt2 <- function(x) sprintf("%.2f", round_half_up(x,2))
fmt_p <- function(p){
  if (is.na(p)) return("")
  if (p < 0.0001) return("<.0001")
  fmt4(p)
}

library(dplyr)
library(tidyr)
library(survival)
library(ggplot2)

# data one as in SAS ----------------------------------------------------
arm1_events <- c(143,164,188,188,190,192,206,213,216,220,227.23,234,246,265,304)
arm1_cens   <- c(216,244)
arm2_events <- c(142,156,163,198,205,232,232,233,233,233,233,239,240,261,280,280,296,296,323)
arm2_cens   <- c(204,344)

one <- bind_rows(
  tibble(arm="Arm 1", day=arm1_events, censor=0),
  tibble(arm="Arm 1", day=arm1_cens,   censor=1),
  tibble(arm="Arm 2", day=arm2_events, censor=0),
  tibble(arm="Arm 2", day=arm2_cens,   censor=1)
) %>% arrange(arm, day, censor)

arms <- c("Arm 1","Arm 2")

# KM table per stratum with time 0 row ---------------------------------
km_table <- function(df){
  times <- sort(unique(df$day))
  n_at_risk <- nrow(df)
  S <- 1
  greenwood_sum <- 0
  rows <- list()
  rows[[1]] <- tibble(day=0, d=0L, c=0L, n_risk=n_at_risk,
                      survival=1, failure=0, se=0, number_failed=0L,
                      number_left=n_at_risk, cens_mark="", greenwood_sum=0)
  cum_failed <- 0
  for(t in times){
    idx <- which(df$day==t)
    d <- sum(df$censor[idx]==0)
    cc <- sum(df$censor[idx]==1)
    n <- n_at_risk
    if(d>0){
      if(n > d) greenwood_sum <- greenwood_sum + d/(n*(n-d))
      S <- S * (1 - d/n)
    }
    se <- S * sqrt(greenwood_sum)
    if(S==0) se <- 0
    cum_failed <- cum_failed + d
    n_left <- n - d - cc
    cens_mark <- ifelse(cc>0 && d==0, "*", "")
    rows[[length(rows)+1]] <- tibble(day=t, d=d, c=cc, n_risk=n,
                                     survival=S, failure=1-S, se=se,
                                     number_failed=cum_failed, number_left=n_left,
                                     cens_mark=cens_mark, greenwood_sum=greenwood_sum)
    n_at_risk <- n_left
  }
  bind_rows(rows)
}
km_list <- lapply(arms, function(a) km_table(filter(one, arm==a)))
names(km_list) <- arms
km_all <- bind_rows(lapply(seq_along(arms), function(i) mutate(km_list[[i]], arm=arms[i])))

# at-risk 0 to 330 by 30
atrisk_times <- seq(0,330,by=30)
atrisk_list <- lapply(arms, function(a) sapply(atrisk_times, function(t) sum(one$day[one$arm==a] >= t)))
atrisk_df <- as.data.frame(do.call(rbind, atrisk_list))
colnames(atrisk_df) <- paste0("t", atrisk_times)
atrisk_df$arm <- arms
atrisk_df <- atrisk_df %>% select(arm, everything())

# CI log-log for quartiles
compute_ci <- function(km){
  z <- qnorm(0.975)
  km$surv_lower <- NA_real_
  km$surv_upper <- NA_real_
  for(i in seq_len(nrow(km))){
    S <- km$survival[i]
    gs <- km$greenwood_sum[i]
    if(S==1){ km$surv_lower[i]<-1; km$surv_upper[i]<-1; next }
    if(S==0){ km$surv_lower[i]<-0; km$surv_upper[i]<-0; next }
    if(S<=0 || S>=1) next
    var_loglog <- gs / (log(S)^2)
    se_loglog <- sqrt(var_loglog)
    lower <- S ^ exp(z*se_loglog)
    upper <- S ^ exp(-z*se_loglog)
    km$surv_lower[i] <- max(0,min(1,lower))
    km$surv_upper[i] <- max(0,min(1,upper))
  }
  km
}
km_list_ci <- lapply(km_list, compute_ci)

quartile_estimates <- function(km){
  ps <- c(0.75,0.5,0.25)
  res <- tibble(percent=ps*100, estimate=NA_real_, lower=NA_real_, upper=NA_real_)
  for(j in seq_along(ps)){
    p <- ps[j]
    idx <- which(km$survival <= p)
    if(length(idx)>0){
      res$estimate[j] <- km$day[min(idx)]
      idx_low <- which(km$surv_upper <= p)
      idx_up  <- which(km$surv_lower <= p)
      if(length(idx_low)>0) res$lower[j] <- km$day[min(idx_low)]
      if(length(idx_up)>0)  res$upper[j] <- km$day[min(idx_up)]
    }
  }
  res
}
quartile_list <- lapply(km_list_ci, quartile_estimates)
names(quartile_list) <- arms

# restricted mean and SE with m/(m-1) factor
mean_restricted <- function(df, km){
  event_times <- df$day[df$censor==0]
  tau <- max(event_times)
  times <- sort(unique(km$day[km$day<=tau & km$day>0]))
  all_times <- c(0, times)
  S_after <- c(1, km$survival[match(times, km$day)])
  area <- 0
  for(k in 1:(length(all_times)-1)){
    delta <- all_times[k+1]-all_times[k]
    area <- area + S_after[k] * delta
  }
  event_distinct <- sort(unique(event_times))
  m <- length(event_times)
  A_vals <- c()
  d_vals <- c()
  n_vals <- c()
  for(t in event_distinct){
    idx <- which(all_times==t)
    A <- 0
    for(k in idx:(length(all_times)-1)){
      delta <- all_times[k+1]-all_times[k]
      A <- A + S_after[k] * delta
    }
    A_vals <- c(A_vals, A)
    row <- km[km$day==t,]
    d_vals <- c(d_vals, row$d)
    n_vals <- c(n_vals, row$n_risk)
  }
  sum_term <- 0
  for(i in seq_along(A_vals)){
    d <- d_vals[i]; n <- n_vals[i]; A <- A_vals[i]
    if(n > d && d>0) sum_term <- sum_term + A^2 * d / (n*(n-d))
  }
  se <- if(m>1) sqrt(m/(m-1)*sum_term) else sqrt(sum_term)
  list(mean=area, se=se, tau=tau, m=m)
}
mean_list <- lapply(seq_along(arms), function(i) mean_restricted(filter(one, arm==arms[i]), km_list[[i]]))
names(mean_list) <- arms

# Summary censored
summary_cens <- bind_rows(lapply(arms, function(a){
  df <- filter(one, arm==a)
  tibble(arm=a, total=nrow(df), failed=sum(df$censor==0), censored=sum(df$censor==1),
         percent_censored=censored/total*100)
})) %>%
  bind_rows(tibble(arm="Total", total=nrow(one), failed=sum(one$censor==0),
                   censored=sum(one$censor==1), percent_censored=censored/total*100))

# Rank tests manual (SAS formulas) --------------------------------------
event_times_all <- sort(unique(one$day[one$censor==0]))
U_lr <- 0; U_w <- 0
V_lr <- 0; V_w <- 0
LR_stat <- 0
for(t in event_times_all){
  n1 <- sum(one$day[one$arm=="Arm 1"] >= t)
  n2 <- sum(one$day[one$arm=="Arm 2"] >= t)
  n <- n1+n2
  d1 <- sum(one$arm=="Arm 1" & one$day==t & one$censor==0)
  d2 <- sum(one$arm=="Arm 2" & one$day==t & one$censor==0)
  d <- d1+d2
  if(n<=1) next
  e1 <- n1*d/n
  U_lr <- U_lr + (d1 - e1)
  U_w  <- U_w  + n*(d1 - e1)
  var_lr_j <- n1*n2*d*(n-d) / (n^2*(n-1))
  var_w_j  <- n^2 * n1*n2*d*(n-d) / (n^2*(n-1))
  V_lr <- V_lr + var_lr_j
  V_w  <- V_w  + var_w_j
  for(k in 1:2){
    nk <- if(k==1) n1 else n2
    dk <- if(k==1) d1 else d2
    ek <- nk*d/n
    if(dk>0 && ek>0) LR_stat <- LR_stat + dk*log(dk/ek)
    if((nk-dk)>0 && (nk-ek)>0) LR_stat <- LR_stat + (nk-dk)*log((nk-dk)/(nk-ek))
  }
}
LR_stat <- 2*LR_stat
chisq_lr <- if(V_lr>0) U_lr^2 / V_lr else 0
chisq_w  <- if(V_w>0)  U_w^2  / V_w  else 0
p_lr <- 1-pchisq(chisq_lr,1)
p_w  <- 1-pchisq(chisq_w,1)
p_lr2 <- 1-pchisq(LR_stat,1)

exp1_lr <- 0; exp2_lr <- 0
exp1_w <- 0; exp2_w <- 0
obs1 <- sum(one$arm=="Arm 1" & one$censor==0)
obs2 <- sum(one$arm=="Arm 2" & one$censor==0)
for(t in event_times_all){
  n1 <- sum(one$day[one$arm=="Arm 1"] >= t)
  n2 <- sum(one$day[one$arm=="Arm 2"] >= t)
  n <- n1+n2
  d <- sum(one$day==t & one$censor==0)
  exp1_lr <- exp1_lr + n1*d/n
  exp2_lr <- exp2_lr + n2*d/n
  exp1_w <- exp1_w + n*n1*d/n
  exp2_w <- exp2_w + n*n2*d/n
}
obs1_w <- 0; obs2_w <- 0
for(t in event_times_all){
  n <- sum(one$day >= t)
  d1 <- sum(one$arm=="Arm 1" & one$day==t & one$censor==0)
  d2 <- sum(one$arm=="Arm 2" & one$day==t & one$censor==0)
  obs1_w <- obs1_w + n*d1
  obs2_w <- obs2_w + n*d2
}

cov_lr <- matrix(c(V_lr, -V_lr, -V_lr, V_lr), nrow=2, byrow=TRUE)
cov_w  <- matrix(c(V_w, -V_w, -V_w, V_w), nrow=2, byrow=TRUE)

# Build text output -----------------------------------------------------
txt <- c(
  "Example K-M Plot",
  "",
  "The LIFETEST Procedure",
  "",
  "Strata: arm",
  "Time variable: day  Censoring variable: censor(1)",
  ""
)
for(i in seq_along(arms)){
  a <- arms[i]; km <- km_list[[i]]
  txt <- c(txt, paste0("Product-Limit Survival Estimates for ", a),
           "day  Survival  Failure  Survival Std Error  Number Failed  Number Left",
           "------------------------------------------------------------------------")
  for(r in seq_len(nrow(km))){
    mark <- km$cens_mark[r]
    day_lab <- paste0(fmt3(km$day[r]), ifelse(mark=="*", "*", ""))
    txt <- c(txt, sprintf("%7s  %8s  %7s  %12s  %13d  %11d",
                          day_lab, fmt4(km$survival[r]), fmt4(km$failure[r]), fmt4(km$se[r]),
                          km$number_failed[r], km$number_left[r]))
  }
  txt <- c(txt, "")
  q <- quartile_list[[a]]
  txt <- c(txt, paste0("Quartile Estimates for ", a),
           "Percent  Point Estimate  95% Confidence Interval [Lower Upper)",
           "------------------------------------------------------------")
  for(r in seq_len(nrow(q))){
    txt <- c(txt, sprintf("%6.1f  %14s  %10s  %10s",
                          q$percent[r],
                          ifelse(is.na(q$estimate[r]), ".", fmt3(q$estimate[r])),
                          ifelse(is.na(q$lower[r]), ".", fmt3(q$lower[r])),
                          ifelse(is.na(q$upper[r]), ".", fmt3(q$upper[r]))))
  }
  txt <- c(txt, "")
  mres <- mean_list[[a]]
  txt <- c(txt, paste0("Mean and Standard Error for ", a),
           sprintf("Mean (restricted to %.3f): %s  Standard Error: %s", mres$tau, fmt3(mres$mean), fmt3(mres$se)),
           "")
}
txt <- c(txt,
         "Summary of the Number of Censored and Uncensored Values",
         "Stratum  Total  Failed  Censored  Percent Censored",
         "---------------------------------------------------")
for(r in seq_len(nrow(summary_cens))){
  txt <- c(txt, sprintf("%-7s  %5d  %6d  %8d  %12s",
                        summary_cens$arm[r], summary_cens$total[r], summary_cens$failed[r],
                        summary_cens$censored[r], fmt2(summary_cens$percent_censored[r])))
}
txt <- c(txt, "",
         "Rank Statistics",
         "Test  Stratum  Observed  Expected",
         "---------------------------------")
txt <- c(txt, sprintf("Log-Rank  %-7s  %8.4f  %8.4f", "Arm 1", as.numeric(obs1), exp1_lr))
txt <- c(txt, sprintf("Log-Rank  %-7s  %8.4f  %8.4f", "Arm 2", as.numeric(obs2), exp2_lr))
txt <- c(txt, sprintf("Wilcoxon  %-7s  %8.4f  %8.4f", "Arm 1", obs1_w, exp1_w))
txt <- c(txt, sprintf("Wilcoxon  %-7s  %8.4f  %8.4f", "Arm 2", obs2_w, exp2_w))
txt <- c(txt, "",
         "Covariance Matrix for the Log-Rank Statistics",
         "          Arm 1      Arm 2",
         "---------------------------")
for(r in 1:2) txt <- c(txt, sprintf("%-7s  %8.4f  %8.4f", arms[r], cov_lr[r,1], cov_lr[r,2]))
txt <- c(txt, "",
         "Covariance Matrix for the Wilcoxon Statistics",
         "          Arm 1      Arm 2",
         "---------------------------")
for(r in 1:2) txt <- c(txt, sprintf("%-7s  %8.4f  %8.4f", arms[r], cov_w[r,1], cov_w[r,2]))
txt <- c(txt, "",
         "Test of Equality over Strata",
         "Test          Chi-Square  DF  Pr > Chi-Square",
         "---------------------------------------------",
         sprintf("Log-Rank      %10s   1  %12s", fmt4(chisq_lr), fmt_p(p_lr)),
         sprintf("Wilcoxon      %10s   1  %12s", fmt4(chisq_w), fmt_p(p_w)),
         sprintf("-2Log(LR)     %10s   1  %12s", fmt4(LR_stat), fmt_p(p_lr2)),
         "",
         sprintf("Sample Size = %d", nrow(one)),
         "")

writeLines(txt, file.path(out, "P040_f_kmplot.txt"))

# CSV of table body
csv_df <- km_all %>% select(arm, day, survival, failure, se, number_failed, number_left, cens_mark) %>% arrange(arm, day)
write.csv(csv_df, file.path(out, "P040_f_kmplot.csv"), row.names=FALSE, na="")

write.csv(atrisk_df, file.path(out, "P040_f_kmplot_atrisk.csv"), row.names=FALSE, na="")

# Plot ------------------------------------------------------------------
plot_data <- bind_rows(lapply(seq_along(arms), function(i){
  km <- km_list[[i]]
  tibble(time=c(0, km$day[km$day>0]), surv=c(1, km$survival[km$day>0]), arm=arms[i])
}))
atrisk_long <- atrisk_df %>% pivot_longer(cols=starts_with("t"), names_to="time", values_to="n_at_risk") %>%
  mutate(time=as.numeric(sub("t","",time)))

p <- ggplot(plot_data, aes(x=time, y=surv, color=arm, linetype=arm)) +
  geom_step(direction="hv", linewidth=0.9) +
  scale_x_continuous(limits=c(0,350), breaks=seq(0,330,by=30), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,by=0.2), expand=c(0,0)) +
  labs(title="Example K-M Plot", x="Days", y="Survival Probability", color="Stratum", linetype="Stratum") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(hjust=0.5, face="bold"), legend.position="bottom",
        panel.grid.minor=element_blank(), plot.margin=margin(10,10,40,10)) +
  geom_text(data=atrisk_long, aes(x=time, y=-0.08, label=n_at_risk, color=arm),
            size=3, show.legend=FALSE, inherit.aes=FALSE) +
  annotate("text", x=-15, y=-0.08, label="At Risk", size=3, hjust=1) +
  coord_cartesian(clip="off", ylim=c(0,1))

ggsave(file.path(out, "P040_f_kmplot.png"), plot=p, width=8, height=6, dpi=300, bg="white")
write.csv(plot_data, file.path(out, "P040_f_kmplot_plotdata.csv"), row.names=FALSE, na="")

cat("P040 R translation complete. Outputs written to", out, "\n")
cat("Log-Rank chi-square:", chisq_lr, "p:", p_lr, " Wilcoxon:", chisq_w, " LR:", LR_stat, "\n")
```