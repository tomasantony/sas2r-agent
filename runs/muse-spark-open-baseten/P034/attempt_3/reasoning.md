Need to fix geom_errorbar usage. In ggplot2, geom_errorbar requires xmin/xmax for horizontal, or ymin/ymax with x. Actually geom_errorbar requires x, ymin, ymax. But error says requires xmin xmax - maybe using wrong orientation? Wait geom_errorbar vertical needs x, ymin, ymax. That should be fine. But error says requires xmin xmax suggests maybe using geom_errorbarh? Let's check code: geom_errorbar(data=MeanData, aes(ymin=..., ymax=...), ...) but missing x mapping? Inherit.aes=FALSE so need to provide x and y? Actually geom_errorbar needs x, ymin, ymax. We provided ymin, ymax but not x. Need to include x=PCTPT. Also y not needed. So fix.

Also need to handle missing values.

Also need to ensure target13.png is created even if error.

We should simplify: use geom_errorbar with aes(x=PCTPT, ymin=..., ymax=...)

Also need to handle SD_opt logic: currently using { if(...) geom_errorbar } inside ggplot chain may cause issues when condition false returns NULL? Better to build plot incrementally.

Also need to ensure no error when SD_opt == BOTH vs UPPER.

We should rewrite p_linear construction with conditional addition.

Also need to ensure MeanData has no missing PCTPT? There are 6 rows missing values outside scale range - likely PCTPT NA? Need to filter NA PCTPT.

Also need to ensure YMAX handling.

We must produce complete script.

We should also ensure OUT_DIR/target13.png exists.

Let's craft final script with robust plotting.

We need to keep previous logic but fix errorbar.

We should also avoid using size aesthetic deprecated.

We should ensure we handle case where MeanData has NA PCMEAN.

We should also ensure we use linewidth not size.

We should produce simple combined plot using grid or patchwork? Use grid as before but fix errorbar.

Let's produce final R script.

We need to include all required outputs: target13.png and CSV of plotted data. Already writes target13.csv etc.

We need to ensure script runs without error.

We should implement p_linear as:

p_linear <- ggplot(MeanData, aes(x=PCTPT, y=PCMEAN, group=TRTA_f)) + geom_line(...) + geom_point(...)

Then if SD_opt == "BOTH" add geom_errorbar(aes(x=PCTPT, ymin=PCMEAN-PCSD, ymax=PCMEAN+PCSD), ...)

Similarly UPPER.

We need to ensure we don't use inherit.aes issues.

Also need to handle that geom_errorbar requires data with non-NA.

We should filter MeanData for errorbar: MeanData_eb <- MeanData %>% filter(!is.na(PCMEAN), !is.na(PCSD))

We should also ensure we handle log scale.

We should also ensure we handle legend.

We should keep round_sas fix already done.

We should ensure we close png device even on error? Use tryCatch.

Simplify: use ggsave for both panels combined via gridExtra? But we can keep png device.

We should ensure we use correct viewport printing.

We should test logic for p_linear creation without conditional inline.

We should produce final script with all steps.

We must output single R code block.

