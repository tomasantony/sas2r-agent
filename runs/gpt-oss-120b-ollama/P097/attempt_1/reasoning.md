We need to produce R script that replicates SAS program. Steps:

- Read data: the data is defined inline in SAS; we need to create same data frames manually in R.

- Data roc: columns alb, tp, totscore, popind. Input lines: each line has 4 numbers: alb tp totscore popind. Then they compute totscore = 10 - totscore. So we need to read raw values then transform.

- Then logistic regression using PROC LOGISTIC with model popind(event='0') = alb tp totscore; output predicted values (LogiPred). In SAS, event='0' means modeling probability of popind=0 (i.e., treat 0 as event). In R, we can fit glm with binomial family, using response as factor with reference level 0? Actually glm default models probability of the first level of factor (or of 1 if numeric). If we have numeric 0/1, glm models probability of 1. To model probability of 0, we can model with response = 1 - popind, or set family = binomial(link='logit') and use as.numeric(popind==0). Simpler: create response = ifelse(popind==0,1,0) and fit.

- Get predicted probabilities (probability of event=0). In SAS they output predicted=LogiPred (predicted probability of event). So we need predicted values.

- Then ExpertPred dataset: a single column ExpertPred with values listed. Need to read as vector.

- Merge LogiOut and ExpertPred: they just combine side by side (same number of rows). In SAS, merge without by merges by order (assuming same order). So we can cbind.

- Then logistic on Survival with nofit (just compute ROC). In SAS they use PROC LOGISTIC with ROC statements to compute ROC curves and association. In R we can compute ROC using pROC or ROCR, but we need to produce same outputs: tables and plots.

- They produce ROCEst: bootstrap distribution of Area statistic for each model (Expert and Logistic). In SAS they use PROC SURVEYSELECT to generate bootstrap samples (with replacement) of the Survival dataset, then for each replicate they compute ROC association (Area) for each model.

- Then PROC MEANS to compute summary stats of Area by ROCModel.

- Then PROC UNIVARIATE to compute percentiles and histogram.

- Then PROC PRINT to display table with BootMean, BootStdErr, 95% Lower CL, Upper CL.

- Then PROC SGPANEL to plot histograms of Area for each model.

- Also earlier they compute a listing P097.lst: includes many numbers (maybe from PROC PRINT of Prob dataset and other outputs). We need to produce a text rendering and CSV of the same table. The listing likely includes the printed tables: PROC PRINT of Prob (x, xbar, s, z, p) and maybe the PROC PRINT of WidePctls. Also maybe the PROC MEANS output. But spec says listing text with 121 numbers. Hard to replicate exactly. However we need to produce a listing file containing the same numbers as SAS output. This is complex.

Given constraints, we can approximate: produce a listing that includes the tables we generate: the summary stats from PROC MEANS, the percentiles table, and the Prob table. Provide as plain text with formatted numbers similar to SAS.

We need to write CSV versions of each table: e.g., means table, percentiles table, prob table.

Also need to produce histogram data CSV for the histogram of Area (maybe from univariate histogram) and for SGPanel.

Simplify: produce CSV of ROCEst (bootstrap results) and use ggplot2 to plot histogram of Area for each ROCModel (like SGPanel). Also produce a histogram of Area overall? The earlier PROC UNIVARIATE histogram of Area (no panel) maybe not needed for output.

Outputs required: P097.lst (listing), histogram.png, sgpanel.png.

Thus we need to generate two figures: histogram of Area (maybe overall) and SGPanel (panel by ROCModel). The SAS code: first PROC UNIVARIATE histogram Area; output out=WidePctls ...; That histogram is not saved as figure; but later they produce SGPanel with histograms per model. The expected outputs list includes "figure Histogram.png" and "figure SGPanel.png". So we need to produce a histogram of Area (overall) and the panel histogram.

Implement:

- After computing ROCEst (bootstrap Area per model), create histogram data: use ggplot2 to plot histogram of Area (overall) and save as histogram.png. Also save the data used for histogram: maybe the ROCEst data.

- For SGPanel: use ggplot2 facet_wrap or facet_grid to mimic panelby rows=2 layout. Use facet_wrap(~ROCModel, nrow=2). Save as sgpanel.png.

- Also write CSV of plotted data: for histogram, we can write the ROCEst data (same as used). For sgpanel, same.

Now produce listing: combine:

1. Prob table: x, xbar, s, z, p.

2. Means summary: class ROCModel, mean, std, P5, P95, P10, P90, P25, P75.

3. WidePctls table: BootMean, BootStdErr, 95% Lower CL, 95% Upper CL.

We need to format numbers similar to SAS: Means with ndec=4 (4 decimal places). Percentiles with format 6.4.

Implement rounding half away from zero: define custom round_half_up function.

For formatting, we can use sprintf with appropriate decimals.

Write listing as plain text: maybe create a character vector lines and writeLines.

Also write CSV for each table: e.g., prob.csv, means.csv, widepctls.csv.

But spec says each output dataset as CSV with same name as SAS dataset. The listing is a table/listing text; also CSV of same table. Which table? Probably the listing includes multiple tables; but we can produce a combined CSV? Might be ambiguous.

Simplify: produce a CSV for each table with appropriate name: prob.csv, means.csv, widepctls.csv.

But the listing file name is P097.lst. We'll write a text file containing all tables formatted.

Now implement script.

Need to set out <- Sys.getenv("OUT_DIR").

Now code steps.

Edge: logistic regression predictions: use glm with binomial, response = ifelse(popind==0,1,0). Predict type="response". That gives probability of event (popind=0). Save as LogiPred.

Now create Survival data: combine LogiOut (including original variables and LogiPred) with ExpertPred column.

Now bootstrap: Use data.table or dplyr to sample with replacement for each replicate. Number of rows N = nrow(Survival). For each replicate (1:NumSamples), sample indices with replacement, compute ROC AUC for each model.

Compute AUC: use pROC::roc? Not allowed packages. Not in allowed list. Could compute AUC manually: using trapezoidal rule on ROC curve. But we can compute using survival::survfit? Not appropriate.

We can compute AUC using the formula: AUC = mean of predictions for positive class vs negative? For binary outcome, AUC = (rank of predictions for positive - n_pos*(n_pos+1)/2) / (n_pos * n_neg). Use rank method.

Implement function auc(y, pred): where y is binary (0/1) where event is 0? In SAS they compute Area for model predicting probability of event=0. So we need to compute AUC for predicting 0 vs 1. Use y_event = ifelse(popind==0,1,0). Then compute AUC.

Implement:

auc <- function(y, pred) {
  # y: 1 for event (popind=0), 0 otherwise
  # pred: predicted probability of event
  # Use Mann-Whitney U
  pos <- which(y==1)
  neg <- which(y==0)
  n_pos <- length(pos)
  n_neg <- length(neg)
  if (n_pos==0 || n_neg==0) return(NA)
  rank_pred <- rank(pred, ties.method="average")
  U <- sum(rank_pred[pos]) - n_pos*(n_pos+1)/2
  auc_val <- U / (n_pos * n_neg)
  return(auc_val)
}

Now for each bootstrap sample, compute AUC for Logistic (LogiPred) and Expert (ExpertPred). Store in data frame with columns Replicate, ROCModel (character "Logistic" or "Expert"), Area.

Implementation: loop over replicates (5000) maybe heavy but okay.

We can use vectorized approach: for each replicate, sample indices, compute.

Will be time-consuming but okay.

Now compute summary stats: using dplyr summarise.

Means: mean, sd, quantile 5%,95%,10%,90%,25%,75% with rounding to 4 decimals.

WidePctls: compute percentiles 2.5 and 97.5, mean, sd.

Now produce plots.

Now write outputs.

Now produce listing text: combine formatted tables.

Implement formatting: for each table, create header lines similar to SAS output.

Simplify: just write CSV and also write a text file with tables separated by blank lines.

Now produce script.

